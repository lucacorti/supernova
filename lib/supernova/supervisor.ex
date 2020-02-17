defmodule Supernova.Supervisor do
  @moduledoc false

  @accept_options [
    port: 8_000,
    certfile: "cert.pem",
    keyfile: "key.pem",
    versions: [:"tlsv1.2"],
    alpn_preferred_protocols: ["h2"],
    next_protocols_advertised: ["h2"]
  ]

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    [
      {Supernova.Listener, @accept_options}
    ]
    |> Supervisor.init(strategy: :one_for_one)
  end
end
