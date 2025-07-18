defmodule ExMCP.Application do
  @moduledoc false

  use Application

  alias ExMCP.Internal.StdioLoggerConfig

  @impl true
  def start(_type, _args) do
    # Check if STDIO transport is being used and configure logging appropriately
    # This MUST happen before defining children to prevent Horde from logging
    if Application.get_env(:ex_mcp, :stdio_mode, false) do
      configure_stdio_logging()
    end

    # Now define children - Horde won't log during startup
    children = [
      # Start Horde cluster for distributed services
      {Horde.Registry, keys: :unique, name: ExMCP.ServiceRegistry, members: :auto},
      {Horde.DynamicSupervisor,
       strategy: :one_for_one, name: ExMCP.ServiceSupervisor, members: :auto},
      # Dynamic supervisor for runtime components
      {DynamicSupervisor, strategy: :one_for_one, name: ExMCP.DynamicSupervisor},
      # Start the Consent Cache for security features
      ExMCP.Internal.ConsentCache,
      # Start the Session Manager for streamable HTTP sessions
      ExMCP.SessionManager,
      # Start the Progress Tracker for 2025-06-18 progress notifications
      ExMCP.ProgressTracker,
      # Start the Reliability Supervisor for circuit breakers and health checks
      {ExMCP.Reliability.Supervisor, name: ExMCP.Reliability.Supervisor}
    ]

    opts = [strategy: :one_for_one, name: ExMCP.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Configure logging for STDIO transport to prevent stdout contamination
  defp configure_stdio_logging do
    StdioLoggerConfig.configure()
  end
end
