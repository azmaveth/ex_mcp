defmodule ExMCP.Server.Transport do
  @moduledoc """
  Transport configuration and lifecycle management for ExMCP servers.

  This module provides unified transport startup and configuration for MCP servers,
  supporting stdio, HTTP, and Server-Sent Events (SSE) transports.

  ## Usage

      # Start with HTTP transport
      {:ok, _pid} = ExMCP.Server.Transport.start_server(MyServer, server_info, tools, transport: :http, port: 4000)

      # Start with stdio transport
      {:ok, _pid} = ExMCP.Server.Transport.start_server(MyServer, server_info, tools, transport: :stdio)

      # Start with SSE-enabled HTTP transport
      {:ok, _pid} = ExMCP.Server.Transport.start_server(MyServer, server_info, tools, transport: :sse, port: 8080)
  """

  require Logger

  alias ExMCP.Internal.StdioLoggerConfig
  alias ExMCP.Server.StdioServer

  @doc """
  Starts a server with the specified transport configuration.

  ## Options

  * `:transport` - The transport type (`:stdio`, `:http`, `:sse`, `:native`)
  * `:port` - Port number for HTTP/SSE transports (default: 4000)
  * `:host` - Host for HTTP/SSE transports (default: "localhost")
  * `:cors_enabled` - Enable CORS for HTTP transports (default: true)
  * `:sse_enabled` - Enable SSE for HTTP transports (default: false, true for :sse transport)

  ## Examples

      # HTTP server
      ExMCP.Server.Transport.start_server(MyServer, %{name: "my-server", version: "1.0.0"}, [],
        transport: :http, port: 4000)

      # Stdio server
      ExMCP.Server.Transport.start_server(MyServer, %{name: "my-server", version: "1.0.0"}, [],
        transport: :stdio)
  """
  @spec start_server(module(), map(), list(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def start_server(module, server_info, tools, opts \\ []) do
    transport = Keyword.get(opts, :transport, :http)

    case transport do
      :stdio ->
        start_stdio_server(module, server_info, tools, opts)

      :http ->
        start_http_server(module, server_info, tools, opts)

      :sse ->
        start_http_server(module, server_info, tools, Keyword.put(opts, :sse_enabled, true))

      :native ->
        start_native_server(module, server_info, tools, opts)

      _ ->
        {:error, {:unsupported_transport, transport}}
    end
  end

  @doc """
  Starts a stdio-based MCP server.

  The stdio transport communicates via standard input/output, making it suitable
  for command-line tools and scripting environments.
  """
  @spec start_stdio_server(module(), map(), list(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def start_stdio_server(module, _server_info, _tools, opts) do
    # CRITICAL: Configure logging for STDIO transport before starting server
    configure_stdio_logging()

    # Use ExMCP v1 StdioServer for now - this provides stdio transport
    # In the future, this could be replaced with a version-specific implementation
    case Code.ensure_loaded(StdioServer) do
      {:module, StdioServer} ->
        StdioServer.start_link([module: module] ++ opts)

      {:error, _} ->
        Logger.warning("StdioServer not available, starting basic GenServer")
        # Fallback to basic server startup
        module.start_link(opts)
    end
  end

  @doc """
  Starts an HTTP-based MCP server using Cowboy.

  The HTTP transport allows integration with web applications and provides
  REST-like access to MCP functionality.
  """
  @spec start_http_server(module(), map(), list(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def start_http_server(module, server_info, tools, opts) do
    port = Keyword.get(opts, :port, 4000)
    host = Keyword.get(opts, :host, "localhost")
    # Check both :sse_enabled and :use_sse for compatibility
    sse_enabled = Keyword.get(opts, :sse_enabled, false) || Keyword.get(opts, :use_sse, false)
    cors_enabled = Keyword.get(opts, :cors_enabled, true)

    # Configure the HTTP Plug
    plug_opts = [
      handler: module,
      server_info: server_info,
      tools: tools,
      sse_enabled: sse_enabled,
      cors_enabled: cors_enabled
    ]

    # Configure Cowboy
    cowboy_opts = [
      port: port,
      ip: parse_host(host)
    ]

    Logger.info("Starting MCP HTTP server on #{host}:#{port} (SSE: #{sse_enabled})")

    case Plug.Cowboy.http(ExMCP.HttpPlug, plug_opts, cowboy_opts) do
      {:ok, pid} ->
        Logger.info("MCP HTTP server started successfully")
        {:ok, pid}

      {:error, {:already_started, pid}} ->
        Logger.info("MCP HTTP server already running")
        {:ok, pid}

      {:error, reason} ->
        Logger.error("Failed to start MCP HTTP server: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Starts a native Erlang process-based MCP server.

  The native transport uses Erlang message passing for high-performance
  local communication between processes.
  """
  @spec start_native_server(module(), map(), list(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def start_native_server(module, _server_info, _tools, opts) do
    Logger.info("Starting MCP native server: #{module}")

    # Start the server module directly as a GenServer
    case module.start_link(opts) do
      {:ok, pid} ->
        Logger.info("MCP native server started successfully")
        {:ok, pid}

      {:error, {:already_started, pid}} ->
        Logger.info("MCP native server already running")
        {:ok, pid}

      {:error, reason} ->
        Logger.error("Failed to start MCP native server: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Stops a running MCP server.
  """
  @spec stop_server(pid() | atom()) :: :ok
  def stop_server(server) when is_pid(server) do
    GenServer.stop(server)
  end

  def stop_server(server) when is_atom(server) do
    case Process.whereis(server) do
      nil -> :ok
      pid -> GenServer.stop(pid)
    end
  end

  @doc """
  Gets information about a running server.
  """
  @spec server_info(pid() | atom()) :: {:ok, map()} | {:error, term()}
  def server_info(server) do
    case GenServer.call(server, :get_server_info, 5000) do
      info when is_map(info) -> {:ok, info}
      _ -> {:error, :no_server_info}
    end
  rescue
    e -> {:error, e}
  catch
    :exit, reason -> {:error, reason}
  end

  @doc """
  Lists all available transports and their status.
  """
  @spec list_transports() :: map()
  def list_transports do
    %{
      stdio: %{
        available: Code.ensure_loaded?(StdioServer),
        description: "Standard input/output transport for CLI tools"
      },
      http: %{
        available: Code.ensure_loaded?(Plug.Cowboy),
        description: "HTTP transport with REST-like API"
      },
      sse: %{
        available: Code.ensure_loaded?(Plug.Cowboy),
        description: "Server-Sent Events over HTTP for real-time communication"
      },
      native: %{
        available: true,
        description: "Native Erlang process communication"
      }
    }
  end

  # Configure logging for STDIO transport to prevent stdout contamination
  defp configure_stdio_logging do
    StdioLoggerConfig.configure()
  end

  # Parse host string to IP tuple
  defp parse_host(host) when is_binary(host) do
    case :inet.parse_address(String.to_charlist(host)) do
      {:ok, ip} ->
        ip

      {:error, :einval} ->
        # Try resolving hostname
        case :inet.gethostbyname(String.to_charlist(host)) do
          {:ok, {:hostent, _, _, _, _, [ip | _]}} -> ip
          # Default to localhost
          _ -> {127, 0, 0, 1}
        end
    end
  end

  defp parse_host(host) when is_tuple(host), do: host
  defp parse_host(_), do: {127, 0, 0, 1}
end
