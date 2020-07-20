defmodule Supernova.Supervisor do
  @moduledoc false

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    [
      {Supernova.Listener, [address: "localhost", port: 8_000]}
    ]
    |> Supervisor.init(strategy: :one_for_one)
  end
end
