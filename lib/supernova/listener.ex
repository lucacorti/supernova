defmodule Supernova.Listener do
  @moduledoc """
  Supernova Ranch Listener implementation
  """
  use GenServer

  alias Supernova.Cert

  {key, {_, cert}} = Cert.generate()

  @transport_options [
    port: 8_000,
    cert: cert,
    key: key,
    versions: [:"tlsv1.2"],
    alpn_preferred_protocols: ["h2"],
    next_protocols_advertised: ["h2"],
    ciphers:
      :ssl.cipher_suites(:default, :"tlsv1.2")
      |> :ssl.filter_cipher_suites(
        key_exchange: &(&1 == :ecdhe_rsa or &1 == :ecdhe_ecdsa),
        mac: &(&1 == :aead)
      )
  ]

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl GenServer
  def init(options) do
    {transport_options, options} = Keyword.pop(options, :transport_options, [])
    transport_options = Keyword.merge(transport_options, @transport_options)

    :ranch.start_listener(Supernova, :ranch_ssl, transport_options, Supernova.Protocol, options)
  end
end
