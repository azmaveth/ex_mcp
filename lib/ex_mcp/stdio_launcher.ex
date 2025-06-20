defmodule ExMCP.StdioLauncher do
  @moduledoc """
  Launcher module for STDIO servers that handles Mix.install output.

  This module provides a clean way to start STDIO servers that need
  to use Mix.install by handling the startup output problem.

  ## Important Note

  While this module minimizes output contamination, Mix.install may still
  produce some stdout output during dependency resolution that cannot be
  completely suppressed. The ExMCP STDIO server is designed to gracefully
  handle and ignore such non-JSON lines during startup.

  For production use, consider using pre-compiled releases instead of Mix.install
  to eliminate all startup output.

  ## Usage

  Instead of using Mix.install directly in your script, use:

      #!/usr/bin/env elixir

      ExMCP.StdioLauncher.start(MyServer, [
        {:ex_mcp, "~> 0.1"},
        {:jason, "~> 1.4"}
      ])

  This will:
  1. Install dependencies with minimal output
  2. Configure logging appropriately for STDIO transport
  3. Start your server with STDIO transport
  """

  alias ExMCP.Internal.StdioLoggerConfig

  @doc """
  Starts a STDIO server with proper dependency installation.

  ## Options

  * `:deps` - List of dependencies for Mix.install (required)
  * `:mix_install_opts` - Options to pass to Mix.install
  * `:server_opts` - Options to pass to server start_link
  """
  def start(server_module, deps, opts \\ []) do
    # Note: We can't redirect stdout as that's needed for JSON-RPC
    # Mix.install output will go to stdout, but our improved STDIO
    # server will gracefully ignore non-JSON lines

    # Configure logging before Mix.install
    configure_stdio_environment()

    # Install dependencies with minimal output
    mix_opts = Keyword.get(opts, :mix_install_opts, [])
    Mix.install(deps, Keyword.put(mix_opts, :verbose, false))

    # Ensure logging is still suppressed after Mix.install
    configure_stdio_environment()

    # Start the server
    server_opts = Keyword.get(opts, :server_opts, [])
    server_opts = Keyword.put(server_opts, :transport, :stdio)

    case server_module.start_link(server_opts) do
      {:ok, pid} ->
        # Keep the process running
        Process.sleep(:infinity)
        {:ok, pid}

      error ->
        IO.puts(:stderr, "Failed to start server: #{inspect(error)}")
        System.halt(1)
    end
  end

  defp configure_stdio_environment do
    # Configure startup delay
    Application.put_env(:ex_mcp, :stdio_startup_delay, 200)

    # Use centralized STDIO logging configuration
    StdioLoggerConfig.configure()
  end
end
