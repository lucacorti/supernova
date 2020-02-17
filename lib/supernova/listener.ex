defmodule Supernova.Listener do
  @moduledoc """
  Supernova Ranch Listener implementation
  """
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl GenServer
  def init(options) do
    :ranch.start_listener(Supernova, 100, :ranch_ssl, options, Supernova.Protocol, reuseaddr: true)
  end
end
