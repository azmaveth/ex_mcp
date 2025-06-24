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

  alias ExMCP.HttpPlug.SSEHandler
  alias ExMCP.FeatureFlags
  alias ExMCP.Authorization.ServerGuard
  alias ExMCP.Authorization.ScopeValidator

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

  def call(%Plug.Conn{method: "POST"} = conn, opts) do
    Logger.debug("HttpPlug: POST request to #{conn.request_path}")
    handle_mcp_request(conn, opts)
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

    with {:ok, conn} <- validate_protocol_version(conn),
         {:ok, body, conn} <- read_body(conn),
         {:ok, request} <- parse_json(body),
         {:ok, _token_info} <- authorize_request(conn, request, opts),
         result <- process_mcp_request(request, opts) do
      Logger.debug("MCP request processed, result: #{inspect(result)}")

      case result do
        {:ok, response} ->
          # If SSE is enabled, send response via SSE instead of HTTP response
          if opts.sse_enabled do
            Logger.debug("SSE mode: sending response via SSE: #{inspect(response)}")
            send_response_via_sse(response, conn, opts)
            # Return 202 Accepted for the POST request
            conn
            |> maybe_add_cors_headers(opts)
            |> send_resp(202, "")
          else
            # Non-SSE mode: send response in HTTP body
            conn
            |> maybe_add_cors_headers(opts)
            |> put_resp_content_type("application/json")
            |> send_resp(200, Jason.encode!(response))
          end

        {:notification, _} ->
          # Notifications get 202 Accepted with no body
          conn
          |> maybe_add_cors_headers(opts)
          |> send_resp(202, "")

        {:error, :no_response} ->
          Logger.error("Handler did not provide a response for request: #{inspect(request)}")

          error_response = %{
            "jsonrpc" => "2.0",
            "error" => %{
              "code" => -32603,
              "message" => "Internal error: no response from handler"
            },
            "id" => Map.get(request, "id")
          }

          conn
          |> maybe_add_cors_headers(opts)
          |> put_resp_content_type("application/json")
          |> send_resp(500, Jason.encode!(error_response))

        {:error, reason} ->
          Logger.error("Request processing error: #{inspect(reason)}")

          error_response = %{
            "jsonrpc" => "2.0",
            "error" => %{
              "code" => -32603,
              "message" => "Internal error"
            },
            "id" => Map.get(request, "id")
          }

          conn
          |> maybe_add_cors_headers(opts)
          |> put_resp_content_type("application/json")
          |> send_resp(500, Jason.encode!(error_response))
      end
    else
      {:error, {:protocol_version_mismatch, message}} ->
        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            # Invalid Request
            "code" => -32600,
            "message" => message,
            "data" => %{"expectedVersion" => "2025-06-18"}
          },
          "id" => nil
        }

        conn
        |> maybe_add_cors_headers(opts)
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
            "code" => -32700,
            "message" => "Parse error"
          },
          "id" => nil
        }

        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))

      {:error, reason} ->
        Logger.error("MCP request processing failed: #{inspect(reason)}")

        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32603,
            "message" => "Internal error"
          },
          "id" => nil
        }

        conn
        |> maybe_add_cors_headers(opts)
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

  # Handle Server-Sent Events connections
  defp handle_sse_connection(conn, opts) do
    with {:ok, _token_info} <- authorize_request(conn, %{}, opts) do
      session_id = get_session_id(conn)

      conn =
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_header("content-type", "text/event-stream")
        |> put_resp_header("cache-control", "no-cache")
        |> put_resp_header("connection", "keep-alive")
        |> send_chunked(200)

      # Check if we're in test mode via application environment
      if Application.get_env(:ex_mcp, :test_mode, false) do
        # Send a simple connection message and return for testing
        {:ok, conn} =
          chunk(conn, "event: connected\ndata: {\"session_id\": \"#{session_id}\"}\n\n")

        conn
      else
        # Use the new SSE handler with backpressure control
        {:ok, handler} = SSEHandler.start_link(conn, session_id, opts)

        # Register with session manager if available
        if function_exported?(opts.session_manager, :register_sse_handler, 2) do
          opts.session_manager.register_sse_handler(session_id, handler)
        end

        # Also register in our simple ETS registry
        register_sse_handler(session_id, handler)

        # Block until handler exits
        ref = Process.monitor(handler)

        receive do
          {:DOWN, ^ref, :process, ^handler, _reason} ->
            # Clean up the session registry when handler exits
            cleanup_sse_handler(session_id)
            conn
        end
      end
    else
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

  # Extract or generate session ID
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
  rescue
    ArgumentError ->
      # Table doesn't exist, create it
      :ets.new(@ets_table, [:named_table, :public, :set])
      :ets.insert(@ets_table, {session_id, handler_pid})
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

  # Send MCP response via SSE to connected clients
  defp send_response_via_sse(response, conn, opts) do
    session_id = get_session_id(conn)
    Logger.debug("Sending SSE response for session #{session_id}")

    # Try to send via session manager first if available
    if function_exported?(opts.session_manager, :send_sse_message, 2) do
      json_response = Jason.encode!(response)
      Logger.debug("Using session manager to send SSE message")
      opts.session_manager.send_sse_message(session_id, json_response)
    else
      # Try to send via our ETS registry
      case lookup_sse_handler(session_id) do
        {:ok, handler_pid} ->
          Logger.debug("Found SSE handler #{inspect(handler_pid)}, sending event")
          # Send the response as an SSE event (SSEHandler will encode it)
          SSEHandler.send_event(handler_pid, "message", response)

        {:error, reason} ->
          Logger.warning(
            "Could not find SSE handler for session #{session_id}: #{inspect(reason)}"
          )
      end
    end
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
      case get_req_header(conn, "mcp-protocol-version") do
        ["2025-06-18"] ->
          {:ok, conn}

        [other] ->
          message = "Unsupported MCP-Protocol-Version: #{other}. Server supports 2025-06-18."
          {:error, {:protocol_version_mismatch, message}}

        [] ->
          message = "Missing MCP-Protocol-Version header. Server requires version 2025-06-18."
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

  defp get_supported_scopes do
    ScopeValidator.get_all_static_scopes()
  end
end
