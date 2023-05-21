defmodule Supernova.Supervisor do
  @moduledoc false

  alias Supernova.Cert
  alias ThousandIsland.Transports.{SSL, TCP}

  {key, {_, cert}} = Cert.generate()

  @options []

  @tcp_options []

  {:ok, options} = Plug.SSL.configure(cipher_suite: :strong, key: key, cert: cert)

  @tls_options options
               |> Keyword.take([:versions, :ciphers, :eccs, :key, :cert])
               |> Keyword.merge(
                 alpn_preferred_protocols: ["h2", "http/1.1"],
                 next_protocols_advertised: ["h2", "http/1.1"]
               )

  def start_link(options) do
    Supervisor.start_link(__MODULE__, options, name: __MODULE__)
  end

  def init(options) do
    {transport_options, options} =
      options
      |> Keyword.merge(@options)
      |> Keyword.pop(:transport_options, [])

    port = Keyword.get(options, :port, 8_000)
    transport_options = Keyword.merge([port: port], transport_options)

    %URI{scheme: scheme} =
      options
      |> Keyword.get(:address, "https://localhost")
      |> URI.parse()

    {transport, transport_options} =
      case scheme do
        "https" ->
          {SSL, Keyword.merge(transport_options, @tls_options)}

        "http" ->
          {TCP, Keyword.merge(transport_options, @tcp_options)}

        scheme ->
          raise "Unsupported scheme #{scheme}"
      end

    Supervisor.init(
      [
        {
          ThousandIsland,
          port: port,
          transport_module: transport,
          transport_options: transport_options,
          handler_module: Supernova.Handler,
          handler_options: [address: "https://localhost"]
        }
      ],
      strategy: :one_for_one
    )
  end
end
