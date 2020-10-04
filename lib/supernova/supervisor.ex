defmodule Supernova.Supervisor do
  @moduledoc false

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    [
      {Supernova.Listener, [address: "https://localhost"]}
    ]
    |> Supervisor.init(strategy: :one_for_one)
  end
end
