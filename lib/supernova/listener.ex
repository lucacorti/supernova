defmodule Supernova.Listener do
  @moduledoc """
  Supernova Ranch Listener implementation
  """
  use GenServer

  alias Supernova.Cert

  {key, {_, cert}} = Cert.generate()

  @options []

  @tcp_options []

  {:ok, options} = Plug.SSL.configure([cipher_suite: :strong, key: key, cert: cert])
  @tls_options options
              |> Keyword.take([:versions, :ciphers, :eccs, :key, :cert])
              |> Keyword.merge(
                alpn_preferred_protocols: ["h2", "http/1.1"],
                next_protocols_advertised: ["h2", "http/1.1"]
              )
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl GenServer
  def init(options) do
    {transport_options, options} =
      options
      |> Keyword.merge(@options)
      |> Keyword.pop(:transport_options, [])

    port = Keyword.get(options, :port, 8_000)
    transport_options = Keyword.merge([port: port], transport_options)

    %URI{scheme: scheme} =
      options
      |> Keyword.get(:address)
      |> URI.parse()

    {transport, transport_options} =
      case scheme do
        "https" ->
          {:ranch_ssl, Keyword.merge(transport_options, @tls_options)}

        "http" ->
          {:ranch_tcp, Keyword.merge(transport_options, @tcp_options)}

        scheme ->
          raise "Unsupported scheme #{scheme}"
      end

    :ranch.start_listener(Supernova, transport, transport_options, Supernova.Protocol, options)
  end
end
