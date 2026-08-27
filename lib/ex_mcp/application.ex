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
        # Atomically consumes OAuth state and authorization codes
        {ExMCP.Authorization.OAuthTransactionStore,
         Application.get_env(:ex_mcp, ExMCP.Authorization.OAuthTransactionStore, [])},
        # Optional node-local single-use enforcement for resumed MRTR requests
        ExMCP.Server.ReplayCache.ETS,
        # Atomically retains modern task handles across client connections
        {ExMCP.Tasks.Store.ETS, Application.get_env(:ex_mcp, ExMCP.Tasks.Store.ETS, [])},
        # Coordinates bounded MCP 2026-07-28 subscription listeners
        {ExMCP.Server.Subscriptions,
         Application.get_env(:ex_mcp, ExMCP.Server.Subscriptions, [])},
        # Owns the ETS table mapping SSE session ids to handler pids for
        # ExMCP.HttpPlug (must outlive individual HTTP request processes)
        ExMCP.HttpPlug.SessionRegistry,
        # Owns the ETS table of cancelled request ids so handlers can call
        # Context.cancelled?/0 without waiting on the server GenServer
        ExMCP.Server.Cancellation,
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
