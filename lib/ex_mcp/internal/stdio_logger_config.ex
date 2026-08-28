defmodule ExMCP.Internal.StdioLoggerConfig do
  @moduledoc """
  Configures logging for the MCP stdio transport so stdout stays JSON-RPC only.

  `configure/0` mutates VM-global Logger, Application, and OTP logger
  behavior: it sets `:ex_mcp` `:stdio_mode`, the Elixir `Logger` level, the
  `:logger` application env, and the OTP primary logger level to
  `:emergency`. The change is process-wide for the BEAM VM, not scoped to
  the stdio connection. Unrelated host-application logging is suppressed.

  1.x keeps this global behavior so stdio protocol output stays
  uncontaminated. 2.0 may replace it with a dedicated IO device and stderr
  logging.

  This module is internal. Do not call it from application code unless you
  intend to apply the same VM-global configuration.
  """

  @doc """
  Configures logging for STDIO transport to prevent stdout contamination.

  The MCP STDIO transport requires that ONLY JSON-RPC messages appear on stdout.
  This function suppresses all logging to ensure clean protocol communication
  by mutating VM-global Logger/Application/OTP logger settings.
  """
  def configure do
    # Set stdio mode flag
    Application.put_env(:ex_mcp, :stdio_mode, true)

    # Configure Logger
    Logger.configure(level: :emergency)

    # Configure application-level logging
    Application.put_env(:logger, :level, :emergency)

    # Configure OTP logger
    :logger.set_primary_config(:level, :emergency)

    :ok
  end
end
