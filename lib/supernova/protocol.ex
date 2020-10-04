defmodule Supernova.Protocol do
  @moduledoc """
  Ankh Ranch protocol implementation
  """

  @behaviour :ranch_protocol

  use GenServer

  alias Ankh.HTTP
  alias HTTP.{Request, Response}

  require Logger

  @impl :ranch_protocol
  def start_link(ref, _transport, options),
    do: {:ok, :proc_lib.spawn_link(__MODULE__, :init, [[ref, options]])}

  @impl GenServer
  def init([ref, options]) do
    {address, options} = Keyword.pop(options, :address, "http://localhost")
    timeout = Keyword.get(options, :timeout, 5_000)

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
    request =
      requests
      |> Map.get(reference, %Request{})
      |> case do
        %Request{headers: []} = request ->
          Request.put_headers(request, headers)

        request ->
          Request.put_trailers(request, headers)
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
      requests
      |> Map.get(reference, %Request{})
      |> case do
        %Request{headers: []} = request ->
          Request.put_headers(request, headers)

        request ->
          Request.put_trailers(request, headers)
      end

    {:ok, protocol, Map.put(requests, reference, request)}
  end

  defp process_response(ref, protocol, requests, {:data, reference, data, true = complete}) do
    %Request{body: body} = request = Map.get(requests, reference, %Request{})

    Logger.info(fn ->
      "#{inspect(ref)} #{inspect(reference)} RECV DATA #{inspect(data)} COMPLETE #{complete}"
    end)

    request = Request.set_body(request, Enum.reverse([data | body]))

    with {:ok, request} <- Request.validate_body(request),
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
    %Request{body: body} = request = Map.get(requests, reference, %Request{})

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
    response =
      Response.new()
      |> Response.set_body(["1234567890 test response\n"])

    with {:ok, protocol} <- HTTP.respond(protocol, reference, response) do
      Logger.info(fn ->
        "#{inspect(ref)} #{inspect(reference)} SENT RESPONSE #{inspect(response)}"
      end)

      {:ok, protocol}
    end
  end
end
