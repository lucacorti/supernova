defmodule Supernova.Handler do
  @moduledoc """
  Ankh Ranch protocol implementation
  """

  alias Ankh.HTTP
  alias Ankh.HTTP.{Request, Response}
  alias ThousandIsland.{Handler, Socket}

  require Logger

  use Handler

  @impl Handler
  def handle_connection(%Socket{socket: socket}, state) do
    {address, options} = Keyword.pop(state, :address, "http://localhost")
    timeout = Keyword.get(options, :timeout, 5_000)

    case HTTP.accept(URI.parse(address), socket) do
      {:ok, protocol} ->
        {
          :continue,
          %{protocol: protocol, ref: make_ref(), requests: %{}, timeout: timeout},
          timeout
        }

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  @impl Handler
  def handle_data(
        data,
        _socket,
        %{protocol: protocol, ref: ref, requests: requests, timeout: timeout} = state
      ) do
    case HTTP.stream_data(protocol, data) do
      {:ok, protocol, responses} ->
        case process_responses(ref, protocol, requests, responses) do
          {:ok, protocol, requests} ->
            {:continue, %{state | protocol: protocol, requests: requests}, timeout}

          {:error, reason} ->
            {:error, reason, %{state | protocol: protocol}}
        end

      {:error, reason} ->
        {:error, reason, %{state | protocol: protocol}}
    end
  end

  @impl Handler
  def handle_timeout(_socket, state), do: {:close, state}

  @impl Handler
  def handle_shutdown(_socket, %{protocol: nil, ref: ref}) do
    Logger.info(fn -> "#{inspect(ref)} Closing connection" end)
  end

  def handle_shutdown(_socket, %{protocol: protocol, ref: ref}) do
    Logger.info(fn -> "#{inspect(ref)} Closing connection" end)
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
    request =
      case Map.get(requests, reference, %Request{}) do
        %Request{headers: []} = request ->
          HTTP.put_headers(request, headers)

        request ->
          HTTP.put_trailers(request, headers)
      end

    Logger.info(fn ->
      "#{inspect(ref)} #{inspect(reference)} RECV HEADERS #{inspect(headers)} COMPLETE #{complete}"
    end)

    with {:ok, protocol} <- send_response(ref, protocol, request, reference) do
      {:ok, protocol, Map.delete(requests, reference)}
    end
  end

  defp process_response(ref, protocol, requests, {:headers, reference, headers, false = complete}) do
    Logger.info(fn ->
      "#{inspect(ref)} #{inspect(reference)} RECV HEADERS #{inspect(headers)} COMPLETE #{complete}"
    end)

    request =
      case Map.get(requests, reference, %Request{}) do
        %Request{headers: []} = request ->
          HTTP.put_headers(request, headers)

        request ->
          HTTP.put_trailers(request, headers)
      end

    {:ok, protocol, Map.put(requests, reference, request)}
  end

  defp process_response(ref, protocol, requests, {:data, reference, data, true = complete}) do
    %Request{body: body} = request = requests[reference] || %Request{}

    Logger.info(fn ->
      "#{inspect(ref)} #{inspect(reference)} RECV DATA #{inspect(data)} COMPLETE #{complete}"
    end)

    request = Request.set_body(request, Enum.reverse([data | body]))

    with {:ok, request} <- HTTP.validate_body(request),
         {:ok, protocol} <- send_response(ref, protocol, request, reference) do
      {:ok, protocol, Map.delete(requests, reference)}
    else
      :error ->
        {:error, :protocol_error}

      {:error, _reason} = error ->
        error
    end
  end

  defp process_response(ref, protocol, requests, {:data, reference, data, false = complete}) do
    %Request{body: body} = request = requests[reference] || %Request{}

    Logger.info(fn ->
      "#{inspect(ref)} #{inspect(reference)} RECV DATA #{inspect(data)} COMPLETE #{complete}"
    end)

    request = Request.set_body(request, [data | body])

    {:ok, protocol, Map.put(requests, reference, request)}
  end

  defp process_response(ref, protocol, requests, {:error, reference, reason, _complete}) do
    Logger.error(fn -> "#{inspect(ref)} #{inspect(reference)} ERROR #{inspect(reason)}" end)
    {:ok, protocol, Map.delete(requests, reference)}
  end

  defp send_response(ref, %{} = protocol, _request, reference) do
    response = Response.set_body(Response.new(), ["1234567890 test response\n"])

    with {:ok, protocol} <- HTTP.respond(protocol, reference, response) do
      Logger.info(fn ->
        "#{inspect(ref)} #{inspect(reference)} SENT RESPONSE #{inspect(response)}"
      end)

      {:ok, protocol}
    end
  end
end
