defmodule ExMCP.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start Horde cluster for distributed services
      {Horde.Registry, keys: :unique, name: ExMCP.ServiceRegistry, members: :auto},
      {Horde.DynamicSupervisor,
       strategy: :one_for_one, name: ExMCP.ServiceSupervisor, members: :auto},
      # Dynamic supervisor for runtime components
      {DynamicSupervisor, strategy: :one_for_one, name: ExMCP.DynamicSupervisor}
    ]

    opts = [strategy: :one_for_one, name: ExMCP.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
