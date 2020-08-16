defmodule Supernova.Protocol do
  @moduledoc """
  Ankh Ranch protocol implementation
  """

  @behaviour :ranch_protocol

  use GenServer

  alias Ankh.HTTP
  alias Ankh.HTTP.{Request, Response}

  require Logger

  @impl :ranch_protocol
  def start_link(ref, transport, options),
    do: {:ok, :proc_lib.spawn_link(__MODULE__, :init, [[ref, transport, options]])}

  @impl GenServer
  def init([ref, _transport, options]) do
    {address, options} = Keyword.pop(options, :address, "localhost")
    port = Keyword.get(options, :port, 8_000)
    timeout = Keyword.get(options, :timeout, 5_000)
    address = "https://#{address}#{if port, do: ":" <> Integer.to_string(port), else: ""}"

    with {:ok, socket} <- :ranch.handshake(ref) do
      Process.send(self(), {:accept, address, socket}, [])

      :gen_server.enter_loop(
        __MODULE__,
        [],
        %{protocol: nil, ref: make_ref(), requests: %{}, timeout: timeout},
        timeout
      )
    end
  end

  @impl GenServer
  def handle_info({:accept, address, socket}, %{timeout: timeout} = state) do
    address
    |> URI.parse()
    |> HTTP.accept(socket)
    |> case do
      {:ok, protocol} ->
        {:noreply, %{state | protocol: protocol}, timeout}

      {:error, reason} ->
        {:stop, {:shutdown, reason}, state}
    end
  end

  def handle_info(:timeout, state), do: {:stop, {:shutdown, :timeout}, state}

  def handle_info(
        msg,
        %{protocol: protocol, ref: ref, requests: requests, timeout: timeout} = state
      ) do
    with {:ok, protocol, responses} <- HTTP.stream(protocol, msg),
         {:ok, protocol, requests} <- process_responses(ref, protocol, requests, responses) do
      {:noreply, %{state | protocol: protocol, requests: requests}, timeout}
    else
      {:other, msg} ->
        Logger.warn(fn -> "unknown msg #{inspect(msg)}" end)
        {:noreply, %{state | protocol: protocol}, timeout}

      {:error, reason} ->
        {:stop, {:shutdown, reason}, %{state | protocol: protocol}}
    end
  end

  @impl GenServer
  def terminate(reason, %{protocol: nil, ref: ref}) do
    Logger.warn(fn -> "#{inspect(ref)} Closing connection: #{inspect(reason)}" end)
  end

  def terminate(reason, %{protocol: protocol, ref: ref}) do
    Logger.warn(fn -> "#{inspect(ref)} Closing connection: #{inspect(reason)}" end)
    HTTP.close(protocol)
  end

  defp process_responses(_ref, protocol, requests, []), do: {:ok, protocol, requests}

  defp process_responses(ref, protocol, requests, [response | rest]) do
    case process_response(ref, protocol, requests, response) do
      {:ok, protocol, requests} ->
        process_responses(ref, protocol, requests, rest)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp process_response(ref, protocol, requests, {:headers, reference, headers, true = complete}) do
    request = Map.get(requests, reference, %Request{})

    with {:ok, request} <- validate_headers(request, headers),
         :ok <-
           Logger.info(fn ->
             "#{inspect(ref)} #{inspect(reference)} RECV HEADERS #{inspect(headers)} COMPLETE #{
               complete
             }"
           end),
         {:ok, protocol} <- send_response(ref, protocol, request, reference) do
      {:ok, protocol, Map.delete(requests, reference)}
    end
  end

  defp process_response(ref, protocol, requests, {:headers, reference, headers, false = complete}) do
    request = Map.get(requests, reference, %Request{})

    with {:ok, request} <- validate_headers(request, headers),
         :ok <-
           Logger.info(fn ->
             "#{inspect(ref)} #{inspect(reference)} RECV HEADERS #{inspect(headers)} COMPLETE #{
               complete
             }"
           end) do
      {:ok, protocol, Map.put(requests, reference, request)}
    end
  end

  defp process_response(ref, protocol, requests, {:data, reference, data, true = complete}) do
    request = Map.get(requests, reference, %Request{})
    data = if is_nil(request.body), do: [data], else: [data | request.body]

    Logger.info(fn ->
      "#{inspect(ref)} #{inspect(reference)} RECV DATA #{inspect(data)} COMPLETE #{complete}"
    end)

    request = Request.set_body(request, data)

    with {:ok, request} <- validate_data(request, data),
         {:ok, protocol} <- send_response(ref, protocol, request, reference) do
      {:ok, protocol, Map.delete(requests, reference)}
    end
  end

  defp process_response(ref, protocol, requests, {:data, reference, data, false = complete}) do
    request = Map.get(requests, reference, %Request{})
    data = if is_nil(request.body), do: [data], else: [data | request.body]

    Logger.info(fn ->
      "#{inspect(ref)} #{inspect(reference)} RECV DATA #{inspect(data)} COMPLETE #{complete}"
    end)

    request = Request.set_body(request, data)

    {:ok, protocol, Map.put(requests, reference, request)}
  end

  defp process_response(ref, protocol, requests, {:error, reference, reason, _complete}) do
    Logger.error(fn -> "#{inspect(ref)} #{inspect(reference)} ERROR #{inspect(reason)}" end)
    {:ok, protocol, Map.delete(requests, reference)}
  end

  defp send_response(ref, protocol, _request, reference) do
    response =
      Response.new()
      |> Response.set_body("1234567890 test response\n")

    with {:ok, protocol} <- HTTP.respond(protocol, reference, response) do
      Logger.info(fn ->
        "#{inspect(ref)} #{inspect(reference)} SENT RESPONSE #{inspect(response)}"
      end)

      {:ok, protocol}
    end
  end

  defp validate_headers(%{headers: []} = request, headers) do
    stats = %{method: false, scheme: false, authority: false, path: false}
    do_validate_headers(request, headers, stats, false)
  end

  defp validate_headers(request, headers), do: validate_trailers(request, headers)

  defp do_validate_headers(
         _request,
         [],
         %{method: method, scheme: scheme, authority: authority, path: path},
         _end_pseudo
       )
       when not method or not path or not scheme or not authority,
       do: {:error, :protocol_error}

  defp do_validate_headers(request, [], _stats, _end_pseudo), do: {:ok, request}

  defp do_validate_headers(request, [{":" <> pseudo_header = header, value} | rest], stats, false)
       when pseudo_header in ["authority", "method", "path", "scheme"] do
    pseudo = String.to_existing_atom(pseudo_header)

    if value == "" or Map.get(stats, pseudo, false) do
      {:error, :protocol_error}
    else
      request = Request.put_header(request, header, value)
      stats = Map.put(stats, pseudo, true)
      do_validate_headers(request, rest, stats, false)
    end
  end

  defp do_validate_headers(
         _request,
         [{":" <> _pseaudo_header, _value} | _rest],
         _stats,
         _end_pseudo
       ),
       do: {:error, :protocol_error}

  defp do_validate_headers(_request, [{"te", value} | _rest], _stats, _end_pseudo)
       when value != "trailers",
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

  defp validate_trailers(request, []), do: {:ok, request}

  defp validate_trailers(_request, [{":" <> _trailer, _value} | _rest]),
    do: {:error, :protocol_error}

  defp validate_trailers(request, [{trailer, value} | rest]) do
    if header_name_valid?(trailer) do
      request = Request.put_trailer(request, trailer, value)
      validate_trailers(request, rest)
    else
      {:error, :protocol_error}
    end
  end

  defp header_name_valid?(name),
    do: name =~ ~r(^[[:lower:][:digit:]\!\#\$\%\&\'\*\+\-\.\^\_\`\|\~]+$)

  defp validate_data(request, data) do
    with content_length when not is_nil(content_length) <-
           request
           |> Request.fetch_header_values("content-length")
           |> List.first(),
         data_length when data_length != content_length <-
           data
           |> IO.iodata_length()
           |> Integer.to_string() do
      {:error, :protocol_error}
    else
      _ ->
        {:ok, Request.set_body(request, data)}
    end
  end
end
