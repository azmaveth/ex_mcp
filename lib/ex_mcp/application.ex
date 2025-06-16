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
      {DynamicSupervisor, strategy: :one_for_one, name: ExMCP.DynamicSupervisor},
      # Start observability service for metrics and monitoring
      ExMCP.Transport.Beam.Observability,
      # Start zero-copy manager for large payload optimization
      ExMCP.Transport.Beam.ZeroCopy
    ]

    # Optionally start security supervisor if configured
    children = maybe_add_security_supervisor(children)

    opts = [strategy: :one_for_one, name: ExMCP.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp maybe_add_security_supervisor(children) do
    if Application.get_env(:ex_mcp, :enable_security, false) do
      security_config = Application.get_env(:ex_mcp, :security, [])
      children ++ [{ExMCP.Security.Supervisor, security_config}]
    else
      children
    end
  end
end
