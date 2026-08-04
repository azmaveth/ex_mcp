defmodule ExMCP.Application do
  @moduledoc false

  use Application

  alias ExMCP.Internal.StdioLoggerConfig

  @impl true
  def start(_type, _args) do
    # Check if STDIO transport is being used and configure logging appropriately.
    if Application.get_env(:ex_mcp, :stdio_mode, false) do
      configure_stdio_logging()
    end

    children =
      [
        # Dynamic supervisor for runtime components
        {DynamicSupervisor, strategy: :one_for_one, name: ExMCP.DynamicSupervisor},
        # Start the Consent Cache for security features
        ExMCP.Internal.ConsentCache,
        # Remembers protocol-era observations across individual client processes
        ExMCP.Client.EraCache,
        # Optional node-local single-use enforcement for resumed MRTR requests
        ExMCP.Server.ReplayCache.ETS,
        # Owns the ETS table mapping SSE session ids to handler pids for
        # ExMCP.HttpPlug (must outlive individual HTTP request processes)
        ExMCP.HttpPlug.SessionRegistry,
        # Owns the ETS indexes for streamable-HTTP resource subscriptions
        ExMCP.SubscriptionRegistry,
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
