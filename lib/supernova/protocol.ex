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
         {:ok, protocol, requests} <-
           Enum.reduce_while(responses, {:ok, protocol, requests}, fn
             response, {:ok, protocol, requests} ->
               case handle_response(protocol, requests, response) do
                  {:ok, _protocol, _requests} = acc ->
                    {:cont, acc}

                  {:error, reason} ->
                    {:halt, {:error, reason}}
               end
           end) do
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

  defp validate_headers(%{body: nil} = request, headers) do
    case Enum.reduce_while(headers, {:ok, request, %{headers: false}}, fn
      {":" <> pseudo_header, value}, {:ok, request, %{headers: false} = stats}
      when byte_size(value) > 0 and pseudo_header in ["method", "scheme", "authority", "path"] ->
        atom = String.to_existing_atom(pseudo_header)

        if Map.get(stats, atom, false) do
          {:halt, :error}
        else
          {:cont, {:ok, Request.put_header(request, ":" <> pseudo_header, value), Map.put(stats, atom, true)}}
        end

      {":" <> _unknown_pseudo_header, _value}, _acc ->
        {:halt, :error}

      {"te", value}, _acc when value not in ["trailers"] ->
        {:halt, :error}

      {"connection", _value}, _acc ->
        {:halt, :error}

      {header, value}, {:ok, request, stats} ->
        if header =~ ~r(^[[:lower:][:digit:]!#$%&'\*\+\-\.\^_`\|~]+$) do
          {:cont, {:ok, Request.put_header(request, header, value), %{stats | headers: true}}}
        else
          {:halt, :error}
        end
    end) do
      {:ok, request, %{method: true, scheme: true, path: true}} ->
        {:ok, request}

      _ ->
        {:error, :protocol_error}
    end
  end

  defp validate_headers(request, headers) do
    Enum.reduce_while(headers, {:ok, request}, fn
      {":" <> _pseudo_header, _value}, _acc ->
        {:halt, {:error, :protocol_error}}

      _, acc ->
        {:cont, acc}
    end)
  end
end
