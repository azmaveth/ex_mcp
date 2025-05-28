defmodule ExMCP.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start any global services here
      # For example, a registry for MCP connections:
      {Registry, keys: :unique, name: ExMCP.Registry},
      # Dynamic supervisor for runtime components
      {DynamicSupervisor, strategy: :one_for_one, name: ExMCP.DynamicSupervisor}
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
