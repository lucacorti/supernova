defmodule Supernova.Protocol do
  @moduledoc """
  Ankh Ranch protocol implementation
  """

  use GenServer

  alias Ankh.HTTP
  alias Ankh.HTTP.{Request, Response}

  require Logger

  @behaviour :ranch_protocol

  @impl :ranch_protocol
  def start_link(ref, _socket, transport, options) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [[ref, transport, options]])}
  end

  @impl GenServer
  def init([ref, _transport, _options]) do
    address = "https://localhost:8000"

    with uri <- URI.parse(address),
         {:ok, socket} <- :ranch.handshake(ref),
         {:ok, protocol} <- HTTP.accept(uri, socket) do
      :gen_server.enter_loop(__MODULE__, [], %{protocol: protocol, requests: %{}, uri: uri})
    end
  end

  @impl GenServer
  def handle_info(msg, %{protocol: protocol, requests: requests} = state) do
    with {:ok, protocol, responses} <- HTTP.stream(protocol, msg),
         {:ok, protocol, requests} <- process_responses(protocol, requests, responses) do
      {:noreply, %{state | protocol: protocol, requests: requests}}
    else
      {:other, msg} ->
        Logger.warn(fn -> "unknown msg #{inspect(msg)}" end)
        {:noreply, %{state | protocol: protocol}}

      {:error, :protocol_error} ->
        Logger.error(fn -> "Protocol error, closing" end)
        HTTP.error(protocol)
        {:noreply, %{state | protocol: protocol}}

      {:error, reason} ->
        Logger.error(fn -> "Received error #{inspect(reason)}" end)
        {:noreply, %{state | protocol: protocol}}
    end
  end

  defp process_responses(protocol, requests, []), do: {:ok, protocol, requests}

  defp process_responses(protocol, requests, [response | rest]) do
    case process_response(protocol, requests, response) do
      {:ok, protocol, requests} ->
        process_responses(protocol, requests, rest)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp process_response(protocol, requests, {:headers, reference, headers, true = _complete}) do
    request = Map.get(requests, reference, %Request{})

    with {:ok, request} <- validate_headers(request, headers),
         :ok <- Logger.debug(fn -> "#{inspect(reference)} RECV HEADERS #{inspect(headers)}" end),
         {:ok, protocol} <- send_response(protocol, request, reference) do
      {:ok, protocol, Map.delete(requests, reference)}
    end
  end

  defp process_response(protocol, requests, {:headers, reference, headers, false = _complete}) do
    request = Map.get(requests, reference, %Request{})

    with {:ok, request} <- validate_headers(request, headers),
         :ok <- Logger.debug(fn -> "#{inspect(reference)} RECV HEADERS #{inspect(headers)}" end) do
        {:ok, protocol, Map.put(requests, reference, request)}
    end
  end

  defp process_response(protocol, requests, {:data, reference, data, true = _complete}) do
    request = Map.get(requests, reference, %Request{})
    data = if is_nil(request.body), do: [data], else: [data | request.body]

    Logger.debug(fn -> "#{inspect(reference)} RECV DATA #{inspect(data)}" end)

    request = Request.set_body(request, data)

    with {:ok, request} <- validate_data(request, data),
         {:ok, protocol} <- send_response(protocol, request, reference) do
      {:ok, protocol, Map.delete(requests, reference)}
    end
  end

  defp process_response(protocol, requests, {:data, reference, data, false = _complete}) do
    request = Map.get(requests, reference, %Request{})
    data = if is_nil(request.body), do: [data], else: [data | request.body]

    Logger.debug(fn -> "#{inspect(reference)} RECV DATA #{inspect(data)}" end)

    request = Request.set_body(request, data)

    {:ok, protocol, Map.put(requests, reference, request)}
  end

  defp process_response(protocol, requests, {:error, reference, reason, _complete}) do
    Logger.error(fn -> "#{inspect(reference)} ERROR #{inspect(reason)}" end)
    {:ok, protocol, Map.delete(requests, reference)}
  end

  defp send_response(protocol, %Request{}, reference) do
    Logger.debug(fn -> "#{inspect(reference)} RECVD REQUEST" end)
    response =
      Response.new()
      |> Response.set_body("data\n")

    with {:ok, protocol} <- HTTP.respond(protocol, reference, response) do
      Logger.debug(fn -> "#{inspect(reference)} SENT RESPONSE" end)
      {:ok, protocol}
    end
  end

  defp validate_headers(request, headers) do
    stats = %{method: false, scheme: false, authority: false, path: false}
    do_validate_headers(request, headers, stats, false)
  end

  defp do_validate_headers(_request, [], %{method: method, scheme: scheme, authority: authority, path: path}, _end_pseudo)
    when not method or not path or not scheme or not authority, do: {:error, :protocol_error}

  defp do_validate_headers(request, [], _stats, _end_pseudo), do: {:ok, request}

  defp do_validate_headers(request, [{":" <> pseudo_header = header, value} | rest], stats, false)
  when pseudo_header in ["authority", "method", "path", "scheme"] do
    atom = String.to_atom(pseudo_header)

    if value == "" or Map.get(stats, atom, false) do
      {:error, :protocol_error}
    else
      request = Request.put_header(request, header, value)
      stats = Map.put(stats, atom, true)
      do_validate_headers(request, rest, stats, false)
    end
  end

  defp do_validate_headers(_request, [{":" <> _pseaudo_header, _value} | _rest], _stats, _end_pseudo),
    do: {:error, :protocol_error}

  defp do_validate_headers(_request, [{"te", value} | _rest], _stats, _end_pseudo) when value != "trailers",
    do: {:error, :protocol_error}

  defp do_validate_headers(_request, [{"connection", _value} | _rest], _stats, _end_pseudo),
    do: {:error, :protocol_error}

  defp do_validate_headers(request, [{header, value} | rest], stats, _end_pseudo) do
    if header_name_valid?(header) do
      request = Request.put_header(request, header, value)
      do_validate_headers(request, rest, stats, true)
    else
      {:error, :protocol_error}
    end
  end

  defp header_name_valid?(name), do: name =~ ~r(^[[:lower:][:digit:]\!\#\$\%\&\'\*\+\-\.\^\_\`\|\~]+$)

  defp validate_data(%{headers: headers} = request, data) do
    with content_length when not is_nil(content_length) <- Enum.reduce(headers, nil, fn
      {"content-length", length}, _acc -> length
      _header, acc -> acc
    end),
    data_length when data_length != content_length <- IO.iodata_length(data) |> Integer.to_string() do
      {:error, :protocol_error}
    else
      _ ->
        {:ok, Request.set_body(request, data)}
    end
  end
end
