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

      {:error, :closed} ->
        {:noreply, %{state | protocol: protocol}}

      {:error, reason} ->
        Logger.error(fn -> "Received error #{inspect(reason)}, closing" end)
        :ok = HTTP.close(protocol)
        {:noreply, %{state | protocol: protocol}}
    end
  end

  defp process_responses(protocol, requests, []), do: {:ok, protocol, requests}
  defp process_responses(protocol, requests, [response | rest]) do
    case handle_response(protocol, requests, response) do
      {:ok, protocol, requests} ->
        process_responses(protocol, requests, rest)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_response(protocol, requests, {:headers, reference, headers, true = _complete}) do
    request = Map.get(requests, reference, %Request{})

    with {:ok, request} <- validate_headers(request, headers),
         {:ok, protocol} <- send_response(protocol, request, reference) do
      Logger.info(fn -> "#{inspect(reference)} RECV HEADERS #{inspect(headers)} COMPLETE" end)
      {:ok, protocol, Map.delete(requests, reference)}
    end
  end

  defp handle_response(protocol, requests, {:headers, reference, headers, false = _complete}) do
    request = Map.get(requests, reference, %Request{})

    with {:ok, request} <- validate_headers(request, headers) do
        Logger.debug(fn -> "#{inspect(reference)} RECV HEADERS #{inspect(headers)}" end)
        {:ok, protocol, Map.put(requests, reference, request)}
    end
  end

  defp handle_response(protocol, requests, {:data, reference, data, true = _complete}) do
    Logger.debug(fn -> "#{inspect(reference)} RECV DATA #{inspect(data)} COMPLETE" end)

    request = Map.get(requests, reference, %Request{})

    with {:ok, protocol} <- send_response(protocol, request, reference) do
      {:ok, protocol, Map.delete(requests, reference)}
    end
  end

  defp handle_response(protocol, requests, {:data, reference, data, false = _complete}) do
    Logger.info(fn -> "#{inspect(reference)} RECV DATA #{inspect(data)}" end)

    request = Map.get(requests, reference, %Request{})
    data = if request.body, do: [data | request.body], else: [data]
    request = Request.set_body(request, data)

    {:ok, protocol, Map.put(requests, reference, request)}
  end

  defp handle_response(protocol, requests, {:error, reference, reason, _complete}) do
    Logger.debug(fn -> "#{inspect(reference)} ERROR #{inspect(reason)}" end)
    {:ok, protocol, Map.delete(requests, reference)}
  end

  defp send_response(protocol, %Request{path: path}, reference) do
    response =
      Response.new()
      |> Response.set_path(path)
      |> Response.set_body("data\n")

    with {:ok, protocol} <- HTTP.respond(protocol, reference, response) do
      Logger.info(fn -> "#{inspect(reference)} SENT RESPONSE" end)
      {:ok, protocol}
    end
  end

  defp validate_headers(%{body: body} = request, headers) do
    stats = %{method: false, scheme: false, authority: false, path: false}
    do_validate_headers(request, headers, stats, not is_nil(body))
  end

  defp do_validate_headers(_request, [], %{method: method, scheme: scheme, authority: authority, path: path}, _end_pseudo)
    when not method or not path or not scheme or not authority, do: {:error, :protocol_error}

  defp do_validate_headers(request, [], _stats, _end_pseudo), do: {:ok, request}

  defp do_validate_headers(request, [{":" <> pseudo_header = header, value} | rest], stats, false = _end_pseudo)
  when pseudo_header in ["authority", "method", "path", "scheme"] do
    atom = String.to_atom(pseudo_header)

    if Map.get(stats, atom, false) do
      {:error, :protocol_error}
    else
      do_validate_headers(Request.put_header(request, header, value), rest, Map.put(stats, atom, true), false)
    end
  end

  defp do_validate_headers(_request, [{":" <> _pseaudo_header} | _rest], _stats, _end_pseudo),
    do: {:error, :protocol_error}

  defp do_validate_headers(_request, [{"te", value} | _rest], _stats, _end_pseudo) when value != "trailers",
    do: {:error, :protocol_error}

  defp do_validate_headers(_request, [{"connection", _value} | _rest], _stats, _end_pseudo),
    do: {:error, :protocol_error}

  defp do_validate_headers(request, [{header, value} | rest], stats, _end_pseudo) do
    if header =~ ~r(^[[:lower:][:digit:]\!\#\$\%\&\'\*\+\-\.\^\_\`\|\~]+$) do
      do_validate_headers(Request.put_header(request, header, value), rest, stats, true)
    else
      {:error, :protocol_error}
    end
  end
end
