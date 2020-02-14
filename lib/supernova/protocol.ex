defmodule Supernova.Protocol do
  use GenServer

  alias Ankh.HTTP
  alias Ankh.HTTP.{Request, Response}

  require Logger

  @behaviour :ranch_protocol

  def start_link(ref, socket, transport, options) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [[ref, socket, transport, options]])
    {:ok, pid}
  end

  def init([ref, _socket, _transport, _options]) do
    address = "https://localhost:8000"

    with uri <- URI.parse(address),
         {:ok, socket} <- :ranch.handshake(ref),
         {:ok, protocol} <- HTTP.accept(uri, socket) do
      :gen_server.enter_loop(__MODULE__, [], %{protocol: protocol, requests: %{}, uri: uri})
    end
  end

  def handle_response(protocol, requests, {:headers, reference, headers, true = _end_stream}) do
    Logger.debug(fn ->
      "#{inspect(reference)} RECV HEADERS #{inspect(headers)} END_STREAM"
    end)

    requests
    |> Map.get(reference, %Request{})
    |> Request.set_headers(headers)
    |> send_response(protocol, reference)

    {:ok, protocol, Map.delete(requests, reference)}
  end

  def handle_response(protocol, requests, {:headers, reference, headers, false = _end_stream}) do
    Logger.debug(fn -> "#{inspect(reference)} RECV HEADERS #{inspect(headers)}" end)

    request =
      requests
      |> Map.get(reference, %Request{})
      |> Request.set_headers(headers)

    {:ok, protocol, Map.put(requests, reference, request)}
  end

  def handle_response(protocol, requests, {:data, reference, data, true = _end_stream}) do
    Logger.debug(fn -> "#{inspect(reference)} RECV DATA #{inspect(data)} END_STREAM" end)

    request =
      requests
      |> Map.get(reference, %Request{})

    with {:ok, protocol} <- send_response(request, protocol, reference) do
      {:ok, protocol, Map.delete(requests, reference)}
    end
  end

  def handle_response(protocol, requests, {:data, reference, data, false = _end_stream}) do
    Logger.debug(fn -> "#{inspect(reference)} RECV DATA #{inspect(data)}" end)

    request =
      requests
      |> Map.get(reference, %Request{})

    request =
      request
      |> Request.set_body([data | request.body])

    {:ok, protocol, Map.put(requests, reference, request)}
  end

  def handle_response(protocol, requests, {:error, reference, reason, _end_stream}) do
    Logger.debug(fn -> "#{inspect(reference)} ERROR #{inspect(reason)}" end)
    {:ok, protocol, Map.delete(requests, reference)}
  end

  def handle_response(protocol, requests, response) do
    Logger.warn(fn -> "Unhandled response #{inspect(response)}" end)
    {:ok, protocol, requests}
  end

  def handle_info(msg, %{protocol: protocol, requests: requests} = state) do
    with {:ok, protocol, responses} <- HTTP.stream(protocol, msg),
         {:ok, protocol, requests} <- Enum.reduce(responses, {:ok, protocol, requests}, fn
          response, {:ok, protocol, requests} ->
            handle_response(protocol, requests, response)
        end) do
      {:noreply, %{state | protocol: protocol, requests: requests}}
    else
      {:other, msg} ->
        Logger.warn(fn -> "unknown msg #{inspect(msg)}" end)
        {:noreply, %{state | protocol: protocol}}

      {:error, reason} ->
        Logger.error(fn -> "Received error #{inspect(reason)}, closing" end)
        :ok = HTTP.close(protocol)
        {:noreply, %{state | protocol: %{protocol | socket: nil}}}
    end
  end

  defp send_response(%Request{path: path}, protocol, reference) do
    response = Response.new()
      |> Response.set_path(path)
      |> Response.set_body("data\n")

    with {:ok, protocol} <- HTTP.respond(protocol, reference, response) do
      Logger.debug(fn -> "#{inspect(reference)} SENT RESPONSE" end)
      {:ok, protocol}
    end
  end
end
