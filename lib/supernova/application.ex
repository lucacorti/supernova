defmodule Supernova.Application do
  @moduledoc false

  use Application

  def start(_type, _args), do: Supernova.Supervisor.start_link([])
end
