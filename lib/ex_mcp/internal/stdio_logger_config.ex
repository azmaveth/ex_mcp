defmodule ExMCP.Internal.StdioLoggerConfig do
  @moduledoc false
  
  # Internal module for centralizing STDIO logging configuration
  # to prevent stdout contamination in STDIO transport

  @doc """
  Configures logging for STDIO transport to prevent stdout contamination.
  
  The MCP STDIO transport requires that ONLY JSON-RPC messages appear on stdout.
  This function suppresses all logging to ensure clean protocol communication.
  """
  def configure do
    # Set stdio mode flag
    Application.put_env(:ex_mcp, :stdio_mode, true)
    
    # Configure Logger
    Logger.configure(level: :emergency)
    
    # Configure application-level logging
    Application.put_env(:logger, :level, :emergency)
    Application.put_env(:horde, :log_level, :emergency)
    
    # Configure OTP logger
    :logger.set_primary_config(:level, :emergency)
    
    # Try to configure console backend to use stderr
    try do
      Logger.configure_backend(:console, device: :stderr, level: :emergency)
    rescue
      _ -> :ok  # Backend might not be available
    end
  end
end