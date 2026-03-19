defmodule ExMCP.HttpPlug do
  @moduledoc """
  HTTP Plug for MCP (Model Context Protocol) requests.
  Compatible with Phoenix and Cowboy servers.

  This plug provides HTTP transport for MCP servers, allowing integration
  with standard Elixir web applications. It supports both regular POST
  requests for RPC calls and Server-Sent Events (SSE) for real-time
  communication.

  ## Usage

      # With Cowboy
      {:ok, _} = Plug.Cowboy.http(ExMCP.HttpPlug, [
        handler: MyApp.MCPServer,
        server_info: %{name: "my-app", version: "1.0.0"}
      ], port: 4000)

      # With Phoenix
      plug ExMCP.HttpPlug,
        handler: MyApp.MCPServer,
        server_info: %{name: "my-app", version: "1.0.0"}

  ## OAuth 2.1 Integration

  To enable OAuth 2.1 bearer token validation:

      plug ExMCP.HttpPlug,
        handler: MyApp.MCPServer,
        server_info: %{name: "my-app"},
        oauth_enabled: true,
        auth_config: %{
          introspection_endpoint: "https://auth.example.com/introspect",
          realm: "my-mcp-server" # Optional, defaults to server_info.name
        }
  """

  @behaviour Plug

  import Plug.Conn
  require Logger

  alias ExMCP.Authorization.AuthorizationServerMetadata
  alias ExMCP.Authorization.ScopeValidator
  alias ExMCP.Authorization.ServerGuard
  alias ExMCP.FeatureFlags
  alias ExMCP.HttpPlug.SSEHandler
  alias ExMCP.Internal.VersionRegistry
  alias ExMCP.Protocol.ErrorCodes

  # Simple session registry using ETS
  @ets_table :http_plug_sessions

  def start_link(_opts \\ []) do
    # Create ETS table for session storage if it doesn't exist
    :ets.new(@ets_table, [:named_table, :public, :set])
    {:ok, self()}
  rescue
    # Table already exists
    ArgumentError -> {:ok, self()}
  end

  @doc """
  Initializes the plug with configuration options.
  """
  @impl Plug
  def init(opts) do
    %{
      handler: Keyword.get(opts, :handler),
      server_info: Keyword.get(opts, :server_info, %{name: "ex_mcp_server", version: "1.0.0"}),
      session_manager: Keyword.get(opts, :session_manager, ExMCP.SessionManager),
      sse_enabled: Keyword.get(opts, :sse_enabled, true),
      cors_enabled: Keyword.get(opts, :cors_enabled, true),
      oauth_enabled: Keyword.get(opts, :oauth_enabled, false),
      auth_config: Keyword.get(opts, :auth_config, %{})
    }
  end

  @doc """
  Processes HTTP connections for MCP protocol.
  """
  @impl Plug
  def call(%Plug.Conn{method: "OPTIONS"} = conn, opts) do
    Logger.debug("HttpPlug: OPTIONS request")

    if opts.cors_enabled do
      handle_cors_preflight(conn)
    else
      send_resp(conn, 405, "Method not allowed")
    end
  end

  def call(
        %Plug.Conn{method: "GET", path_info: [".well-known", "oauth-protected-resource"]} = conn,
        opts
      ) do
    if opts.oauth_enabled do
      handle_well_known_resource(conn, opts)
    else
      send_resp(conn, 404, "Not Found")
    end
  end

  def call(
        %Plug.Conn{method: "GET", path_info: [".well-known", "oauth-authorization-server"]} =
          conn,
        opts
      ) do
    if opts.oauth_enabled do
      handle_authorization_server_metadata(conn, opts)
    else
      send_resp(conn, 404, "Not Found")
    end
  end

  def call(%Plug.Conn{method: "GET", path_info: ["sse"]} = conn, opts) do
    if opts.sse_enabled do
      handle_sse_connection(conn, opts)
    else
      send_resp(conn, 404, "SSE not enabled")
    end
  end

  def call(%Plug.Conn{method: "GET", path_info: ["mcp", "v1", "sse"]} = conn, opts) do
    if opts.sse_enabled do
      handle_sse_connection(conn, opts)
    else
      send_resp(conn, 404, "SSE not enabled")
    end
  end

  # Handle POST to OAuth endpoints - these should return 404
  def call(
        %Plug.Conn{method: "POST", path_info: [".well-known", "oauth-authorization-server"]} =
          conn,
        _opts
      ) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end

  def call(
        %Plug.Conn{method: "POST", path_info: [".well-known", "oauth-protected-resource"]} = conn,
        _opts
      ) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end

  def call(%Plug.Conn{method: "POST"} = conn, opts) do
    Logger.debug("HttpPlug: POST request to #{conn.request_path}")
    handle_mcp_request(conn, opts)
  end

  def call(%Plug.Conn{method: "DELETE", path_info: ["sse", session_id]} = conn, opts) do
    handle_session_delete(conn, session_id, opts)
  end

  def call(%Plug.Conn{method: "DELETE", path_info: ["mcp", "v1", "sse", session_id]} = conn, opts) do
    handle_session_delete(conn, session_id, opts)
  end

  # Per MCP spec, DELETE to the MCP endpoint with Mcp-Session-Id header terminates the session.
  def call(%Plug.Conn{method: "DELETE"} = conn, opts) do
    case get_req_header(conn, "mcp-session-id") do
      [session_id | _] ->
        handle_session_delete(conn, session_id, opts)

      [] ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(%{error: "Missing Mcp-Session-Id header"}))
    end
  end

  # Per MCP spec, SSE GET uses the same endpoint as POST.
  # Handle GET requests with Accept: text/event-stream on any path.
  def call(%Plug.Conn{method: "GET"} = conn, opts) do
    accepts_sse =
      conn
      |> get_req_header("accept")
      |> Enum.any?(&String.contains?(&1, "text/event-stream"))

    if opts.sse_enabled and accepts_sse do
      handle_sse_connection(conn, opts)
    else
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(404, Jason.encode!(%{error: "Not found"}))
    end
  end

  def call(conn, _opts) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end

  # CORS preflight handling
  defp handle_cors_preflight(conn) do
    conn
    |> put_resp_header("access-control-allow-origin", "*")
    |> put_resp_header("access-control-allow-methods", "GET, POST, OPTIONS")
    |> put_resp_header(
      "access-control-allow-headers",
      "content-type, authorization, mcp-protocol-version"
    )
    |> put_resp_header("access-control-max-age", "86400")
    |> send_resp(200, "")
  end

  # Handle regular MCP JSON-RPC requests
  defp handle_mcp_request(conn, opts) do
    Logger.debug("Handling MCP request, SSE enabled: #{opts.sse_enabled}")

    # Per MCP spec, get or generate session ID for this request.
    # The server provides session IDs to clients via response headers.
    session_id = get_or_create_session_id(conn)

    with {:ok, conn} <- validate_protocol_version(conn),
         {:ok, body, conn} <- read_body(conn),
         {:ok, request} <- parse_json(body),
         {:ok, _token_info} <- authorize_request(conn, request, opts),
         result <- process_mcp_request(request, opts) do
      Logger.debug("MCP request processed, result: #{inspect(result)}")

      case result do
        {:ok, response} ->
          # Per MCP spec, POST responses MUST contain the result in the body,
          # even when SSE is enabled. The SSE stream is for server-initiated
          # messages only (notifications, progress updates).
          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> put_resp_header("mcp-session-id", session_id)
          |> put_resp_content_type("application/json")
          |> send_resp(200, Jason.encode!(response))

        {:notification, _} ->
          # Notifications get 202 Accepted with no body
          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> put_resp_header("mcp-session-id", session_id)
          |> send_resp(202, "")

        {:error, :no_response} ->
          Logger.error("Handler did not provide a response for request: #{inspect(request)}")

          error_response = %{
            "jsonrpc" => "2.0",
            "error" => %{
              "code" => ErrorCodes.internal_error(),
              "message" => "Internal error: no response from handler"
            },
            "id" => Map.get(request, "id")
          }

          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> put_resp_header("mcp-session-id", session_id)
          |> put_resp_content_type("application/json")
          |> send_resp(500, Jason.encode!(error_response))

        {:error, reason} ->
          Logger.error("Request processing error: #{inspect(reason)}")

          error_response = %{
            "jsonrpc" => "2.0",
            "error" => %{
              "code" => ErrorCodes.internal_error(),
              "message" => "Internal error"
            },
            "id" => Map.get(request, "id")
          }

          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> put_resp_header("mcp-session-id", session_id)
          |> put_resp_content_type("application/json")
          |> send_resp(500, Jason.encode!(error_response))
      end
    else
      {:error, {:protocol_version_mismatch, message}} ->
        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            # Invalid Request
            "code" => ErrorCodes.invalid_request(),
            "message" => message,
            "data" => %{"expectedVersion" => VersionRegistry.latest_version()}
          },
          "id" => nil
        }

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> put_resp_header("mcp-session-id", session_id)
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))

      {:error, {:auth_error, {status, www_auth_header, body}}} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_header("www-authenticate", www_auth_header)
        |> send_resp(status, body)

      {:error, :parse_error} ->
        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => ErrorCodes.parse_error(),
            "message" => "Parse error"
          },
          "id" => nil
        }

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> put_resp_header("mcp-session-id", session_id)
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))

      {:error, reason} ->
        Logger.error("MCP request processing failed: #{inspect(reason)}")

        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => ErrorCodes.internal_error(),
            "message" => "Internal error"
          },
          "id" => nil
        }

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> put_resp_header("mcp-session-id", session_id)
        |> put_resp_content_type("application/json")
        |> send_resp(500, Jason.encode!(error_response))
    end
  end

  # Parse JSON and handle decode errors
  defp parse_json(body) do
    case Jason.decode(body) do
      {:ok, json} -> {:ok, json}
      {:error, _} -> {:error, :parse_error}
    end
  end

  # Handle session termination via DELETE request
  defp handle_session_delete(conn, session_id, opts) do
    case authorize_request(conn, %{}, opts) do
      {:ok, _token_info} ->
        # Terminate the session
        :ok = ExMCP.SessionManager.terminate_session(session_id)

        # Try to stop the SSE handler if it exists
        case lookup_sse_handler(session_id) do
          {:ok, handler_pid} ->
            if Process.alive?(handler_pid) do
              SSEHandler.close(handler_pid)
            end

            cleanup_sse_handler(session_id)

          {:error, _} ->
            # Session not found in ETS, but that's OK
            :ok
        end

        conn
        |> maybe_add_cors_headers(opts)
        |> send_resp(204, "")

      {:error, {:auth_error, {status, www_auth_header, body}}} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_header("www-authenticate", www_auth_header)
        |> send_resp(status, body)
    end
  end

  # Handle Server-Sent Events connections
  defp handle_sse_connection(conn, opts) do
    case authorize_request(conn, %{}, opts) do
      {:ok, _token_info} ->
        _original_session_id = get_session_id(conn)

        conn =
          conn
          |> maybe_add_cors_headers(opts)
          |> put_resp_header("content-type", "text/event-stream")
          |> put_resp_header("cache-control", "no-cache")
          |> put_resp_header("connection", "keep-alive")
          |> send_chunked(200)

        # Extract client information for session
        client_info = %{
          user_agent: get_req_header(conn, "user-agent") |> List.first(),
          origin: get_req_header(conn, "origin") |> List.first(),
          referer: get_req_header(conn, "referer") |> List.first(),
          remote_ip: get_peer_data(conn).address |> :inet.ntoa() |> to_string()
        }

        # Create or get existing session from SessionManager
        existing_session_id =
          case get_req_header(conn, "mcp-session-id") do
            [existing_id] -> existing_id
            [] -> nil
          end

        final_session_id =
          if existing_session_id do
            # Check if session exists and is valid
            case ExMCP.SessionManager.get_session(existing_session_id) do
              {:ok, session} when session.status == :active ->
                # Update session activity
                ExMCP.SessionManager.update_session(existing_session_id, %{
                  client_info: client_info,
                  transport: :sse
                })

                existing_session_id

              _ ->
                # Session doesn't exist or is terminated, create new one
                ExMCP.SessionManager.create_session(%{
                  transport: :sse,
                  client_info: client_info
                })
            end
          else
            # Create new session
            ExMCP.SessionManager.create_session(%{
              transport: :sse,
              client_info: client_info
            })
          end

        # Check if we're in test mode via application environment
        if Application.get_env(:ex_mcp, :test_mode, false) do
          # Send a simple connection message and return for testing
          {:ok, conn} =
            chunk(conn, "event: connected\ndata: {\"session_id\": \"#{final_session_id}\"}\n\n")

          conn
        else
          # Use the new SSE handler with backpressure control
          {:ok, handler} = SSEHandler.start_link(conn, final_session_id, opts)

          # Register with session manager
          ExMCP.SessionManager.update_session(final_session_id, %{handler_pid: handler})

          # Also register in our simple ETS registry
          register_sse_handler(final_session_id, handler)

          # Block until handler exits
          ref = Process.monitor(handler)

          receive do
            {:DOWN, ^ref, :process, ^handler, reason} ->
              # Clean up the session registry when handler exits
              cleanup_sse_handler(final_session_id)

              # Terminate session in SessionManager if it was a clean shutdown
              if reason == :normal do
                ExMCP.SessionManager.terminate_session(final_session_id)
              end

              conn
          end
        end

      {:error, {:auth_error, {status, www_auth_header, body}}} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_header("www-authenticate", www_auth_header)
        |> send_resp(status, body)
    end
  end

  # Process MCP request using the configured handler
  defp process_mcp_request(request, opts) do
    handler = opts.handler
    server_info = opts.server_info

    case handler do
      nil ->
        {:error, :no_handler_configured}

      handler_module when is_atom(handler_module) ->
        # Use ExMCP.MessageProcessor to process the request
        conn = ExMCP.MessageProcessor.new(request, transport: :http)

        # Create a simple processor that delegates to the handler
        processed_conn =
          ExMCP.MessageProcessor.process(conn, %{
            handler: handler_module,
            server_info: server_info
          })

        case processed_conn.response do
          nil ->
            # Check if this was a notification (no id field)
            if Map.get(request, "id") == nil do
              # Notifications don't get responses - return special marker
              {:notification, nil}
            else
              {:error, :no_response}
            end

          %{"jsonrpc" => "2.0", "error" => _} = response ->
            # JSON-RPC error responses are still valid HTTP responses
            {:ok, response}

          response ->
            {:ok, response}
        end

      handler_fun when is_function(handler_fun, 1) ->
        # Direct function handler
        case handler_fun.(request) do
          {:ok, response} -> {:ok, response}
          {:error, reason} -> {:error, reason}
          response when is_map(response) -> {:ok, response}
        end
    end
  end

  # Add CORS headers if enabled
  defp maybe_add_cors_headers(conn, %{cors_enabled: true}) do
    conn
    |> put_resp_header("access-control-allow-origin", "*")
    |> put_resp_header("access-control-allow-methods", "GET, POST, OPTIONS")
    |> put_resp_header(
      "access-control-allow-headers",
      "content-type, authorization, mcp-protocol-version"
    )
  end

  defp maybe_add_cors_headers(conn, _opts), do: conn

  # Extract session ID from request or generate a new one.
  # Per MCP spec, the server provides the session ID — the client's first
  # request should not include one, and the server generates it.
  defp get_or_create_session_id(conn) do
    case get_req_header(conn, "mcp-session-id") do
      [session_id] -> session_id
      [] -> generate_session_id()
    end
  end

  # Extract or generate session ID (legacy, also checks x-session-id)
  defp get_session_id(conn) do
    # Try multiple possible session header names for compatibility
    case get_req_header(conn, "mcp-session-id") do
      [session_id] ->
        session_id

      [] ->
        case get_req_header(conn, "x-session-id") do
          [session_id] -> session_id
          [] -> generate_session_id()
        end
    end
  end

  # Register SSE handler for a session
  defp register_sse_handler(session_id, handler_pid) do
    :ets.insert(@ets_table, {session_id, handler_pid})

    # Also register with SessionManager if available
    if function_exported?(ExMCP.SessionManager, :update_session, 2) do
      ExMCP.SessionManager.update_session(session_id, %{handler_pid: handler_pid})
    end
  rescue
    ArgumentError ->
      # Table doesn't exist, create it
      :ets.new(@ets_table, [:named_table, :public, :set])
      :ets.insert(@ets_table, {session_id, handler_pid})

      # Also register with SessionManager if available
      if function_exported?(ExMCP.SessionManager, :update_session, 2) do
        ExMCP.SessionManager.update_session(session_id, %{handler_pid: handler_pid})
      end
  end

  # Look up SSE handler for a session
  defp lookup_sse_handler(session_id) do
    case :ets.lookup(@ets_table, session_id) do
      [{^session_id, handler_pid}] -> {:ok, handler_pid}
      [] -> {:error, :not_found}
    end
  rescue
    ArgumentError -> {:error, :table_not_found}
  end

  # Clean up SSE handler registration
  defp cleanup_sse_handler(session_id) do
    :ets.delete(@ets_table, session_id)
  rescue
    # Table doesn't exist, nothing to clean up
    ArgumentError -> :ok
  end

  # Generate a simple session ID
  defp generate_session_id do
    "sse_" <>
      (:crypto.strong_rand_bytes(16)
       |> Base.encode16(case: :lower))
  end

  # --- New Helper Functions ---

  defp validate_protocol_version(conn) do
    if FeatureFlags.enabled?(:protocol_version_header) do
      supported = VersionRegistry.supported_versions()
      latest = VersionRegistry.latest_version()

      case get_req_header(conn, "mcp-protocol-version") do
        [version] when is_binary(version) ->
          if version in supported do
            {:ok, conn}
          else
            message = "Unsupported MCP-Protocol-Version: #{version}. Server supports #{latest}."
            {:error, {:protocol_version_mismatch, message}}
          end

        [] ->
          message = "Missing MCP-Protocol-Version header. Server requires version #{latest}."
          {:error, {:protocol_version_mismatch, message}}
      end
    else
      {:ok, conn}
    end
  end

  defp authorize_request(conn, request, opts) do
    if opts.oauth_enabled do
      required_scopes = ScopeValidator.get_required_scopes(request)
      # Set default realm if not provided in config
      auth_config =
        if Map.has_key?(opts.auth_config, :realm) do
          opts.auth_config
        else
          Map.put(opts.auth_config, :realm, opts.server_info.name)
        end

      case ServerGuard.authorize(conn.req_headers, required_scopes, auth_config) do
        {:ok, token_info} ->
          {:ok, token_info}

        {:error, error_response} ->
          {:error, {:auth_error, error_response}}

        :ok ->
          # This case happens if ServerGuard's own feature flag (:oauth2_auth) is disabled.
          # We treat this as a successful authorization because the plug's `oauth_enabled`
          # is the master switch for this instance.
          {:ok, nil}
      end
    else
      {:ok, nil}
    end
  end

  defp handle_well_known_resource(conn, opts) do
    metadata = %{
      "resource" => opts.server_info.name,
      "scopes_supported" => get_supported_scopes(),
      "bearer_token_types_supported" => ["bearer"]
    }

    conn
    |> maybe_add_cors_headers(opts)
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(metadata))
  end

  defp handle_authorization_server_metadata(conn, opts) do
    metadata = AuthorizationServerMetadata.build_metadata()

    conn
    |> maybe_add_cors_headers(opts)
    |> put_resp_content_type("application/json")
    |> put_resp_header("cache-control", "public, max-age=3600")
    |> send_resp(200, Jason.encode!(metadata))
  rescue
    e in ArgumentError ->
      Logger.error(
        "OAuth authorization server metadata configuration error: #{Exception.message(e)}"
      )

      error_response = %{
        "error" => "server_error",
        "error_description" => "Authorization server metadata is not properly configured"
      }

      conn
      |> maybe_add_cors_headers(opts)
      |> put_resp_content_type("application/json")
      |> send_resp(500, Jason.encode!(error_response))
  end

  defp get_supported_scopes do
    ScopeValidator.get_all_static_scopes()
  end

  # Per MCP spec, all responses MUST include the mcp-protocol-version header.
  defp add_protocol_version_header(conn) do
    put_resp_header(conn, "mcp-protocol-version", VersionRegistry.latest_version())
  end
end
