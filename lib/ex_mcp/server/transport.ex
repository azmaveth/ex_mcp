defmodule ExMCP.Server.Transport do
  @moduledoc """
  Transport configuration and lifecycle management for ExMCP servers.

  This module provides unified transport startup and configuration for MCP servers,
  supporting stdio, HTTP, BEAM-local, and test transports.

  ## Usage

      # Start with HTTP transport
      {:ok, _pid} = ExMCP.Server.Transport.start_server(MyServer, server_info, tools, transport: :http, port: 4000)

      # Start with stdio transport
      {:ok, _pid} = ExMCP.Server.Transport.start_server(MyServer, server_info, tools, transport: :stdio)

      # Explicitly retain the deprecated 2024-11-05 HTTP+SSE transport
      {:ok, _pid} = ExMCP.Server.Transport.start_server(MyServer, server_info, tools,
        transport: :http,
        legacy_http_sse: true,
        port: 8080
      )
  """

  require Logger

  alias ExMCP.Internal.StdioLoggerConfig
  alias ExMCP.Server.StdioServer

  @doc """
  Starts a server with the specified transport configuration.

  ## Options

  * `:transport` - The transport type (`:stdio`, `:http`, `:beam`, `:test`)
  * `:adapter` - HTTP server adapter (`:auto`, `:bandit`, or `:cowboy`).
    `:auto` (the default) prefers Bandit when it is loaded, then Cowboy.
    Override with this option or `config :ex_mcp, :http_adapter, ...`.
    Cowboy ships with ExMCP. Add `{:bandit, "~> 1.0"}` to use Bandit.
  * `:port` - Port number for HTTP transports (default: 4000)
  * `:host` - Host for HTTP transports (default: "localhost")
  * `:cors_enabled` - Enable CORS for HTTP transports (default: `false`, the
    same default `ExMCP.HttpPlug` uses)
  * `:legacy_http_sse` - Enable the deprecated MCP 2024-11-05 HTTP+SSE
    transport (default: `false`). Retained throughout ExMCP 1.x
  * `:sse_enabled` - Deprecated rc.5 alias for `:legacy_http_sse`
  * `:allowed_hosts` - Host-header allow-list passed to `ExMCP.HttpPlug`.
    Defaults to the localhost names when binding to a localhost address
    (DNS rebinding protection), otherwise `:any`
  * `:allowed_origins` - Origin allow-list passed to `ExMCP.HttpPlug`.
    Defaults to localhost origins for the bound port when binding to a
    localhost address, otherwise `[]` (reject all cross-origin browsers)
  * `:ranch_ref` - Cowboy-only Ranch listener name. Omitted by default so
    Plug.Cowboy registers the named `ExMCP.HttpPlug.HTTP` singleton. Bandit
    ignores this option.

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

      :beam ->
        start_beam_server(module, server_info, tools, opts)

      :test ->
        start_test_server(module, server_info, tools, opts)

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
  Starts an HTTP-based MCP server using Bandit or Cowboy.

  Phoenix applications should mount `ExMCP.HttpPlug` in the endpoint instead
  of calling this function. Cowboy is included with ExMCP. Add
  `{:bandit, "~> 1.0"}` to start Bandit instead.

  `:adapter` selects the server (`:auto`, `:bandit`, or `:cowboy`). `:auto`
  prefers Bandit when it is loaded. `:ranch_ref` is Cowboy-only and selects
  the Ranch listener name. The default is the named `ExMCP.HttpPlug.HTTP`
  singleton. Bandit ignores `:ranch_ref`.
  """
  @spec start_http_server(module(), map(), list(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def start_http_server(module, server_info, _tools, opts) do
    with {:ok, adapter} <- resolve_http_adapter(opts) do
      port = Keyword.get(opts, :port, 4000)
      host = Keyword.get(opts, :host, "localhost")
      # Preserve the rc.5 server option aliases throughout 1.x, but never enable
      # the deprecated standalone SSE transport on a new server by default.
      legacy_http_sse =
        Keyword.get(
          opts,
          :legacy_http_sse,
          Keyword.get(opts, :sse_enabled, false) || Keyword.get(opts, :use_sse, false)
        )

      # Matches ExMCP.HttpPlug's own default; CORS must be opted into (audit L10).
      cors_enabled = Keyword.get(opts, :cors_enabled, false)
      ranch_ref = Keyword.get(opts, :ranch_ref)

      # Localhost-bound servers are the prime target for DNS rebinding, so
      # they get a Host allow-list (and matching localhost Origin allow-list)
      # by default. Explicit :allowed_hosts / :allowed_origins always win.
      allowed_hosts = Keyword.get(opts, :allowed_hosts, default_allowed_hosts(host))
      allowed_origins = Keyword.get(opts, :allowed_origins, default_allowed_origins(host, port))

      # Configure the HTTP Plug. Tools are read from the handler module, so the
      # `tools` argument is not forwarded (ExMCP.HttpPlug.init/1 ignores it).
      plug_opts =
        [
          handler: module,
          server_info: server_info,
          legacy_http_sse: legacy_http_sse,
          cors_enabled: cors_enabled,
          allowed_hosts: allowed_hosts,
          allowed_origins: allowed_origins
        ] ++
          Keyword.take(opts, [
            :request_state,
            :mrtr,
            :path,
            :legacy_http_sse_path,
            :legacy_http_sse_post_path,
            :protocol_mode,
            :instructions,
            :server_capabilities,
            :handler_call_timeout,
            :max_input_requests,
            :max_mrtr_bytes,
            :replay_cache,
            :require_replay_protection
          ])

      if legacy_http_sse do
        Logger.warning(
          "The MCP 2024-11-05 HTTP+SSE transport is deprecated; migrate clients to Streamable HTTP"
        )
      end

      Logger.info(
        "Starting MCP HTTP server (#{adapter}) on #{host}:#{port} " <>
          "(deprecated HTTP+SSE: #{legacy_http_sse})"
      )

      start_http_listener(adapter, plug_opts, port, parse_host(host), ranch_ref)
    end
  end

  @doc """
  Stops a Bandit listener started by `start_http_server/4`.

  Cowboy listeners use Ranch refs. Stop them with `Plug.Cowboy.shutdown/1`
  and the `:ranch_ref` you passed, or `ExMCP.HttpPlug.HTTP` for the default
  named singleton.
  """
  @spec stop_http_server(pid()) :: :ok
  def stop_http_server(pid) when is_pid(pid) do
    stop_supervisor(pid)
  end

  @doc """
  Resolves the HTTP adapter from options and application env.

  `:adapter` on the start options wins, then `config :ex_mcp, :http_adapter`,
  then `:auto`. `:auto` prefers Bandit when it is loaded.
  """
  @spec resolve_http_adapter(keyword()) ::
          {:ok, :bandit | :cowboy}
          | {:error, {:missing_http_adapter, String.t()}}
          | {:error, {:invalid_http_adapter, term()}}
  def resolve_http_adapter(opts \\ []) do
    requested =
      Keyword.get(opts, :adapter, Application.get_env(:ex_mcp, :http_adapter, :auto))

    case requested do
      :auto ->
        cond do
          http_adapter_loaded?(:bandit) -> {:ok, :bandit}
          http_adapter_loaded?(:cowboy) -> {:ok, :cowboy}
          true -> {:error, {:missing_http_adapter, missing_http_adapter_message(:auto)}}
        end

      :bandit ->
        if http_adapter_loaded?(:bandit) do
          {:ok, :bandit}
        else
          {:error, {:missing_http_adapter, missing_http_adapter_message(:bandit)}}
        end

      :cowboy ->
        if http_adapter_loaded?(:cowboy) do
          {:ok, :cowboy}
        else
          {:error, {:missing_http_adapter, missing_http_adapter_message(:cowboy)}}
        end

      other ->
        {:error, {:invalid_http_adapter, other}}
    end
  end

  @doc false
  @spec http_adapter_loaded?(:bandit | :cowboy) :: boolean()
  def http_adapter_loaded?(:bandit), do: Code.ensure_loaded?(Bandit)
  def http_adapter_loaded?(:cowboy), do: Code.ensure_loaded?(Plug.Cowboy)

  @doc """
  Starts a BEAM-local MCP server.

  The BEAM transport uses Erlang message passing for high-performance local
  communication between processes while preserving MCP-shaped messages.
  """
  @spec start_beam_server(module(), map(), list(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def start_beam_server(module, _server_info, _tools, opts) do
    Logger.info("Starting MCP BEAM server: #{module}")

    # Start the server module directly as a GenServer
    case module.start_link(opts) do
      {:ok, pid} ->
        Logger.info("MCP BEAM server started successfully")
        {:ok, pid}

      {:error, {:already_started, pid}} ->
        Logger.info("MCP BEAM server already running")
        {:ok, pid}

      {:error, reason} ->
        Logger.error("Failed to start MCP BEAM server: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Starts a test transport-based MCP server.

  The test transport uses in-memory communication for efficient
  testing without external processes or network connections.
  """
  @spec start_test_server(module(), map(), list(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def start_test_server(module, _server_info, _tools, opts) do
    Logger.debug("Starting MCP test server: #{module}")

    # Start the server module directly as a GenServer with test transport
    case module.start_link(opts) do
      {:ok, pid} ->
        Logger.debug("MCP test server started successfully")
        {:ok, pid}

      {:error, {:already_started, pid}} ->
        Logger.debug("MCP test server already running")
        {:ok, pid}

      {:error, reason} ->
        Logger.error("Failed to start MCP test server: #{inspect(reason)}")
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
        available: http_adapter_loaded?(:bandit) or http_adapter_loaded?(:cowboy),
        description: "HTTP transport with REST-like API"
      },
      beam: %{
        available: true,
        description: "BEAM-local MCP transport"
      },
      test: %{
        available: true,
        description: "In-memory transport for testing"
      }
    }
  end

  defp start_http_listener(:cowboy, plug_opts, port, ip, ranch_ref) do
    _ = Application.ensure_all_started(:plug_cowboy)

    cowboy_opts =
      [port: port, ip: ip]
      |> then(fn opts ->
        if ranch_ref, do: Keyword.put(opts, :ref, ranch_ref), else: opts
      end)

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

  defp start_http_listener(:bandit, plug_opts, port, ip, _ranch_ref) do
    _ = Application.ensure_all_started(:bandit)

    spec =
      Supervisor.child_spec(
        {Bandit, plug: {ExMCP.HttpPlug, plug_opts}, scheme: :http, port: port, ip: ip},
        id: {:ex_mcp_http_bandit, System.unique_integer([:positive])},
        restart: :temporary
      )

    case DynamicSupervisor.start_child(ExMCP.DynamicSupervisor, spec) do
      {:ok, pid} ->
        Logger.info("MCP HTTP server started successfully")
        {:ok, pid}

      {:error, reason} ->
        Logger.error("Failed to start MCP HTTP server: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp stop_supervisor(pid) do
    if Process.alive?(pid) do
      try do
        Supervisor.stop(pid, :normal, 5_000)
      catch
        :exit, _ -> :ok
      end
    else
      :ok
    end
  end

  defp missing_http_adapter_message(:bandit) do
    "HTTP transport adapter :bandit is not available. Add {:bandit, \"~> 1.0\"} to your mix.exs dependencies."
  end

  defp missing_http_adapter_message(:cowboy) do
    "HTTP transport adapter :cowboy is not available. Add {:plug_cowboy, \"~> 2.7\"} to your mix.exs dependencies."
  end

  defp missing_http_adapter_message(:auto) do
    "HTTP transport requires an adapter. Cowboy is included with ExMCP. Add {:bandit, \"~> 1.0\"} to use Bandit."
  end

  # Configure logging for STDIO transport to prevent stdout contamination
  defp configure_stdio_logging do
    StdioLoggerConfig.configure()
  end

  @localhost_hosts ["localhost", "127.0.0.1", "::1", "[::1]"]

  defp localhost_bind?(host) do
    host in @localhost_hosts or host == {127, 0, 0, 1} or host == {0, 0, 0, 0, 0, 0, 0, 1}
  end

  # Host allow-list for ExMCP.HttpPlug: localhost binds get DNS rebinding
  # protection by default; other binds keep :any for backwards compatibility.
  defp default_allowed_hosts(host) do
    if localhost_bind?(host) do
      ["localhost", "127.0.0.1", "[::1]", "::1"]
    else
      :any
    end
  end

  # Origin allow-list for ExMCP.HttpPlug. HttpPlug no longer has a
  # same-origin fallback (Host is attacker-controlled under DNS rebinding),
  # and ExMCP's own HTTP client sends an Origin derived from the server URL,
  # so localhost binds explicitly allow localhost origins for the bound port.
  # This is rebinding-safe: a rebinding attack presents the attacker page's
  # real (non-localhost) origin.
  defp default_allowed_origins(host, port) do
    if localhost_bind?(host) do
      for h <- ["localhost", "127.0.0.1", "[::1]"],
          origin <- ["http://#{h}", "http://#{h}:#{port}"] do
        origin
      end
    else
      []
    end
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
