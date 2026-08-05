defmodule ExMCP.HttpPlug do
  @moduledoc """
  HTTP Plug for MCP (Model Context Protocol) requests.
  Compatible with Phoenix and Cowboy servers.

  This plug provides HTTP transport for MCP servers, allowing integration
  with standard Elixir web applications. It supports both regular POST
  requests for RPC calls and Server-Sent Events (SSE) for real-time
  communication.

  ## Handler options

  `:handler_opts` configures the argument passed to a handler module's
  `init/1`. It may be a static term, a one-arity function called with the
  `Plug.Conn`, a two-arity function called with the `Plug.Conn` and decoded
  JSON-RPC request, or an `{module, function, extra_args}` tuple. MFA handlers
  are called as `apply(module, function, [conn, request | extra_args])`.

  `:handler_call_timeout` is the server-side deadline, in milliseconds, for
  each call from the plug into a Handler process (default: `10_000`). It is
  independent of client request and stream timeouts.

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

  ## Security

  ### Origin validation (`:validate_origin`, `:allowed_origins`)

  With `validate_origin: true` (the default), any request carrying an
  `Origin` header is rejected with `403` unless the origin is listed in
  `:allowed_origins` (or `:allowed_origins` is `:any`). There is no
  "same origin as the Host header" fallback: in a DNS rebinding attack the
  Host header is attacker-controlled, so such a comparison would always pass.

  Requests **without** an `Origin` header are allowed. Non-browser clients
  (CLIs, SDKs, server-to-server callers) do not send the header; use
  `:allowed_hosts` to protect them against DNS rebinding.

  ### Host validation (`:allowed_hosts`)

  `:allowed_hosts` is either `:any` (default, no restriction) or a list of
  hostnames. When a list is given, requests whose `Host` header does not
  match an entry are rejected with `421` before any processing. Ports are
  ignored and IPv6 hosts match with or without brackets, so
  `allowed_hosts: ["localhost", "127.0.0.1", "[::1]", "::1"]` accepts
  `localhost:4000` and `[::1]:8080`. Servers started via
  `ExMCP.Server.Transport` with a localhost bind get this allow-list by
  default.

  ### Server-Sent Events (`:sse_mode`)

  `:sse_mode` is `:stream` (default) or `:oneshot`. `:stream` starts an
  `ExMCP.HttpPlug.SSEHandler` and holds the request open for the lifetime of
  the stream; `:oneshot` writes a single `connected` event and returns, which
  suits test harnesses and health checks.

  MCP 2026-07-28 does not use that GET stream. A modern
  `subscriptions/listen` POST owns its SSE response directly. The response
  process closes its registry entry when the client disconnects and emits SSE
  comment keepalives every `:subscription_keepalive_interval_ms` milliseconds
  (default: `15_000`; set `:infinity` to disable).

  ### Session ids

  Client-supplied `mcp-session-id` (and legacy `x-session-id`) header values
  are validated before use: at most 128 bytes from the character set
  `A-Z a-z 0-9 . _ ~ + / = -` (covering UUIDs and base64/base64url tokens).
  Invalid values are rejected with a `400` JSON-RPC error and are never
  echoed back.
  """

  @behaviour Plug

  import Plug.Conn
  require Logger

  alias ExMCP.Authorization.AuthorizationServerMetadata
  alias ExMCP.Authorization.ScopeValidator
  alias ExMCP.Authorization.ServerGuard
  alias ExMCP.FeatureFlags
  alias ExMCP.HttpPlug.Core
  alias ExMCP.HttpPlug.ModernStream
  alias ExMCP.HttpPlug.SessionRegistry
  alias ExMCP.HttpPlug.SSEHandler
  alias ExMCP.Internal.{JSONRPC, VersionRegistry}
  alias ExMCP.Protocol.{ErrorCodes, Methods}
  alias ExMCP.Server.{RequestContext, Subscriptions}
  alias ExMCP.Transport.HTTP.RequestHeaders

  @session_id_max_bytes 128

  @deprecated "The session table is owned by ExMCP.HttpPlug.SessionRegistry, started with the :ex_mcp application"
  def start_link(opts \\ []) do
    case SessionRegistry.start_link(opts) do
      {:ok, pid} -> {:ok, pid}
      {:error, {:already_started, pid}} -> {:ok, pid}
      other -> other
    end
  end

  @doc """
  Initializes the plug with configuration options.
  """
  @impl Plug
  def init(opts) do
    validate_mrtr_configuration!(opts)

    %{
      handler: Keyword.get(opts, :handler),
      handler_opts: Keyword.get(opts, :handler_opts, []),
      handler_call_timeout: Keyword.get(opts, :handler_call_timeout, 10_000),
      server_info: Keyword.get(opts, :server_info, %{name: "ex_mcp_server", version: "1.0.0"}),
      server_capabilities: Keyword.get(opts, :server_capabilities),
      protocol_mode: Keyword.get(opts, :protocol_mode),
      instructions: Keyword.get(opts, :instructions),
      request_state: Keyword.get(opts, :request_state),
      endpoint: Keyword.get(opts, :path, "/mcp"),
      max_input_requests: Keyword.get(opts, :max_input_requests, 16),
      max_mrtr_bytes: Keyword.get(opts, :max_mrtr_bytes, 1_048_576),
      replay_cache: Keyword.get(opts, :replay_cache),
      require_replay_protection: Keyword.get(opts, :require_replay_protection, false),
      principal_id: Keyword.get(opts, :principal_id),
      tenant_id: Keyword.get(opts, :tenant_id),
      subscription_registry: Keyword.get(opts, :subscription_registry),
      authorize_subscription_filter: Keyword.get(opts, :authorize_subscription_filter),
      authorize_subscription_publication: Keyword.get(opts, :authorize_subscription_publication),
      subscription_max_queue: Keyword.get(opts, :subscription_max_queue),
      subscription_max_lifetime_ms: Keyword.get(opts, :subscription_max_lifetime_ms),
      subscription_keepalive_interval_ms:
        subscription_keepalive_interval!(
          Keyword.get(opts, :subscription_keepalive_interval_ms, 15_000)
        ),
      session_manager: Keyword.get(opts, :session_manager, ExMCP.SessionManager),
      sse_enabled: Keyword.get(opts, :sse_enabled, true),
      sse_mode: Keyword.get(opts, :sse_mode, default_sse_mode()),
      cors_enabled: Keyword.get(opts, :cors_enabled, false),
      allowed_origins: Keyword.get(opts, :allowed_origins, []),
      allowed_hosts: Keyword.get(opts, :allowed_hosts, :any),
      validate_origin: Keyword.get(opts, :validate_origin, true),
      body_limit: Keyword.get(opts, :body_limit, 1_000_000),
      oauth_enabled: Keyword.get(opts, :oauth_enabled, false),
      auth_config: Keyword.get(opts, :auth_config, %{})
    }
  end

  defp validate_mrtr_configuration!(opts) do
    if Keyword.get(opts, :mrtr, false) do
      case ExMCP.Server.RequestState.validate_configuration(
             request_state: Keyword.get(opts, :request_state)
           ) do
        :ok ->
          :ok

        {:error, reason} ->
          raise ArgumentError, "invalid MRTR requestState configuration: #{reason}"
      end
    end
  end

  @doc """
  Processes HTTP connections for MCP protocol.

  Host validation (`:allowed_hosts`) runs before any routing so that DNS
  rebinding attempts are rejected before request processing.
  """
  @impl Plug
  def call(conn, opts) do
    if request_host_allowed?(conn, opts) do
      dispatch(conn, opts)
    else
      reject_disallowed_host(conn, opts)
    end
  end

  defp dispatch(conn, opts) do
    if modern_only_disallowed_method?(conn, opts) do
      conn
      |> put_resp_header("allow", "POST")
      |> put_resp_content_type("application/json")
      |> send_resp(405, Jason.encode!(%{"error" => "Method not allowed"}))
    else
      do_dispatch(conn.method, conn.path_info, conn, opts)
    end
  end

  defp modern_only_disallowed_method?(conn, %{protocol_mode: :modern_only} = opts) do
    conn.method in ["GET", "DELETE"] and conn.request_path == opts.endpoint
  end

  defp modern_only_disallowed_method?(_conn, _opts), do: false

  defp do_dispatch("OPTIONS", _path, conn, opts) do
    Logger.debug("HttpPlug: OPTIONS request")

    if opts.cors_enabled do
      handle_cors_preflight(conn, opts)
    else
      send_resp(conn, 405, "Method not allowed")
    end
  end

  defp do_dispatch("GET", [".well-known", "oauth-protected-resource"], conn, opts) do
    if opts.oauth_enabled do
      handle_well_known_resource(conn, opts)
    else
      send_resp(conn, 404, "Not Found")
    end
  end

  defp do_dispatch("GET", [".well-known", "oauth-authorization-server"], conn, opts) do
    if opts.oauth_enabled do
      handle_authorization_server_metadata(conn, opts)
    else
      send_resp(conn, 404, "Not Found")
    end
  end

  defp do_dispatch("GET", ["sse"], conn, opts) do
    if opts.sse_enabled do
      handle_sse_connection(conn, opts)
    else
      send_resp(conn, 404, "SSE not enabled")
    end
  end

  defp do_dispatch("GET", ["mcp", "v1", "sse"], conn, opts) do
    if opts.sse_enabled do
      handle_sse_connection(conn, opts)
    else
      send_resp(conn, 404, "SSE not enabled")
    end
  end

  # Handle POST to OAuth endpoints - these should return 404
  defp do_dispatch("POST", [".well-known", "oauth-authorization-server"], conn, _opts) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end

  defp do_dispatch("POST", [".well-known", "oauth-protected-resource"], conn, _opts) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end

  defp do_dispatch("POST", _path, conn, opts) do
    Logger.debug("HttpPlug: POST request to #{conn.request_path}")

    :telemetry.execute(
      [:ex_mcp, :server, :http, :request],
      %{},
      %{method: conn.method, path: conn.request_path}
    )

    handle_mcp_request(conn, opts)
  end

  defp do_dispatch("DELETE", ["sse", session_id], conn, opts) do
    handle_session_delete(conn, session_id, opts)
  end

  defp do_dispatch("DELETE", ["mcp", "v1", "sse", session_id], conn, opts) do
    handle_session_delete(conn, session_id, opts)
  end

  # Per MCP spec, DELETE to the MCP endpoint with Mcp-Session-Id header terminates the session.
  defp do_dispatch("DELETE", _path, conn, opts) do
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
  defp do_dispatch("GET", _path, conn, opts) do
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

  defp do_dispatch(_method, _path, conn, _opts) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end

  # CORS preflight handling
  defp handle_cors_preflight(conn, opts) do
    if origin_allowed?(conn, opts) do
      conn
      |> maybe_add_cors_headers(opts)
      |> put_resp_header("access-control-allow-methods", "GET, POST, DELETE, OPTIONS")
      |> put_resp_header(
        "access-control-allow-headers",
        "content-type, authorization, mcp-protocol-version, mcp-session-id"
      )
      |> put_resp_header("access-control-max-age", "86400")
      |> send_resp(200, "")
    else
      send_resp(conn, 403, "Origin not allowed")
    end
  end

  # Handle regular MCP JSON-RPC requests
  defp handle_mcp_request(conn, opts) do
    case validate_request_origin(conn, opts) do
      {:ok, conn} ->
        select_mcp_era(conn, opts)

      {:error, :origin_not_allowed} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> send_resp(403, "Origin not allowed")
    end
  end

  defp select_mcp_era(conn, opts) do
    Logger.debug("Handling MCP request, SSE enabled: #{opts.sse_enabled}")

    # Era selection happens before session allocation. Modern requests are
    # stateless and ignore legacy session/resumption headers; legacy requests
    # retain the existing session behavior.
    case read_or_cached_body(conn, opts) do
      {:ok, body, conn} ->
        conn = assign(conn, :raw_body, body)

        case parse_json(body) do
          {:ok, request} ->
            if modern_http_request?(conn, request) do
              do_handle_mcp_request(conn, opts, nil)
            else
              handle_legacy_mcp_request(conn, opts)
            end

          {:error, _reason} ->
            # Preserve the legacy error/session shape when no modern envelope
            # can be identified from an invalid body.
            handle_legacy_mcp_request(conn, opts)
        end

      {:error, :body_too_large} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> send_resp(413, "Request body too large")

      {:error, reason} ->
        Logger.error("Failed to read MCP request body: #{inspect(reason)}")
        send_resp(conn, 400, "Invalid request body")
    end
  end

  defp handle_legacy_mcp_request(conn, opts) do
    case get_or_create_session_id(conn) do
      {:ok, session_id} -> do_handle_mcp_request(conn, opts, session_id)
      {:error, :invalid_session_id} -> reject_invalid_session_id(conn, opts)
    end
  end

  defp do_handle_mcp_request(conn, opts, session_id) do
    with {:ok, conn} <- validate_request_origin(conn, opts),
         {:ok, body, conn} <- read_or_cached_body(conn, opts),
         {:ok, request} <- parse_json(body),
         conn = assign_request_protocol_version(conn, request),
         {:ok, conn} <- validate_protocol_version(conn, request),
         :ok <- validate_modern_method(request),
         {:ok, token_info} <- authorize_request(conn, request, opts),
         {:ok, opts} <- resolve_handler_opts(conn, request, opts),
         {:ok, opts} <- resolve_mrtr_identity(conn, request, token_info, opts),
         :ok <-
           maybe_ensure_session(
             Map.get(opts, :session_manager, ExMCP.SessionManager),
             session_id,
             %{transport: :http}
           ),
         result <-
           process_mcp_request(
             request,
             opts
             |> Map.put(:session_id, session_id)
             |> Map.put(:request_headers, conn.req_headers)
           ) do
      Logger.debug("MCP request processed, result: #{inspect(result)}")

      case result do
        {:subscription, entry} ->
          :telemetry.execute(
            [:ex_mcp, :server, :http, :response],
            %{},
            %{status: 200, streaming: true}
          )

          subscription_options = subscription_options(opts)

          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> put_resp_header("content-type", "text/event-stream")
          |> put_resp_header("x-accel-buffering", "no")
          |> put_resp_header("cache-control", "no-cache")
          |> send_chunked(200)
          |> ModernStream.serve(entry, opts, subscription_options)

        {:ok, response} ->
          # Per MCP spec, POST responses MUST contain the result in the body,
          # even when SSE is enabled. The SSE stream is for server-initiated
          # messages only (notifications, progress updates).
          :telemetry.execute(
            [:ex_mcp, :server, :http, :response],
            %{},
            %{status: 200}
          )

          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> maybe_put_session_header(session_id)
          |> put_resp_content_type("application/json")
          |> send_resp(200, Jason.encode!(response))

        {:notification, _} ->
          # Notifications get 202 Accepted with no body
          :telemetry.execute(
            [:ex_mcp, :server, :http, :response],
            %{},
            %{status: 202}
          )

          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> maybe_put_session_header(session_id)
          |> send_resp(202, "")

        {:http_error, status, response} ->
          :telemetry.execute(
            [:ex_mcp, :server, :http, :response],
            %{},
            %{status: status}
          )

          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> maybe_put_session_header(session_id)
          |> put_resp_content_type("application/json")
          |> send_resp(status, Jason.encode!(response))

        {:error, :no_response} ->
          :telemetry.execute(
            [:ex_mcp, :server, :http, :response],
            %{},
            %{status: 500}
          )

          Logger.error("Handler did not provide a response for request: #{inspect(request)}")

          error_response =
            JSONRPC.error(
              Map.get(request, "id"),
              ErrorCodes.internal_error(),
              "Internal error: no response from handler"
            )

          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> maybe_put_session_header(session_id)
          |> put_resp_content_type("application/json")
          |> send_resp(500, Jason.encode!(error_response))

        {:error, reason} ->
          :telemetry.execute(
            [:ex_mcp, :server, :http, :response],
            %{},
            %{status: 500}
          )

          Logger.error("Request processing error: #{inspect(reason)}")

          error_response =
            JSONRPC.error(
              Map.get(request, "id"),
              ErrorCodes.internal_error(),
              "Internal error"
            )

          conn
          |> maybe_add_cors_headers(opts)
          |> add_protocol_version_header()
          |> maybe_put_session_header(session_id)
          |> put_resp_content_type("application/json")
          |> send_resp(500, Jason.encode!(error_response))
      end
    else
      {:error, {:header_mismatch, request_id, message}} ->
        error_response =
          JSONRPC.error(request_id, ErrorCodes.header_mismatch(), message)

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))

      {:error, {:unsupported_protocol_version, request_id, version}} ->
        supported = VersionRegistry.known_versions()

        error_response =
          JSONRPC.error(
            request_id,
            ErrorCodes.unsupported_protocol_version(),
            "Unsupported MCP protocol version",
            %{"requested" => version, "supported" => supported}
          )

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))

      {:error, {:modern_method_not_found, request_id}} ->
        error_response =
          JSONRPC.error(request_id, ErrorCodes.method_not_found(), "Method not found")

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> put_resp_content_type("application/json")
        |> send_resp(404, Jason.encode!(error_response))

      {:error, {:protocol_version_mismatch, message}} ->
        error_response =
          JSONRPC.error(
            nil,
            ErrorCodes.invalid_request(),
            message,
            %{"expectedVersion" => VersionRegistry.latest_version()}
          )

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> maybe_put_session_header(session_id)
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))

      {:error, {:auth_error, {status, www_auth_header, body}}} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_header("www-authenticate", www_auth_header)
        |> send_resp(status, body)

      {:error, :oauth_guard_disabled} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_content_type("application/json")
        |> send_resp(500, Jason.encode!(Core.oauth_guard_disabled_error()))

      {:error, :origin_not_allowed} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> send_resp(403, "Origin not allowed")

      {:error, :parse_error} ->
        error_response = JSONRPC.error(nil, ErrorCodes.parse_error(), "Parse error")

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> maybe_put_session_header(session_id)
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))

      {:error, :invalid_json_rpc_envelope} ->
        error_response = JSONRPC.error(nil, ErrorCodes.invalid_request(), "Invalid Request")

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> maybe_put_session_header(session_id)
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))

      {:error, :body_too_large} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> send_resp(413, "Request body too large")

      {:error, reason} ->
        Logger.error("MCP request processing failed: #{inspect(reason)}")

        error_response = JSONRPC.error(nil, ErrorCodes.internal_error(), "Internal error")

        conn
        |> maybe_add_cors_headers(opts)
        |> add_protocol_version_header()
        |> maybe_put_session_header(session_id)
        |> put_resp_content_type("application/json")
        |> send_resp(500, Jason.encode!(error_response))
    end
  end

  # Parse JSON and handle decode errors
  defp parse_json(body) do
    Core.parse_json(body)
  end

  defp modern_http_request?(conn, request) do
    modern_protocol_header?(conn) or modern_request_metadata?(request)
  end

  defp modern_protocol_header?(conn) do
    case get_req_header(conn, "mcp-protocol-version") do
      [version] -> VersionRegistry.modern?(version)
      _other -> false
    end
  end

  defp modern_request_metadata?(%{"params" => %{"_meta" => meta}}) when is_map(meta) do
    Map.has_key?(meta, "io.modelcontextprotocol/protocolVersion") or
      Map.has_key?(meta, "io.modelcontextprotocol/clientCapabilities")
  end

  defp modern_request_metadata?(_request), do: false

  # Allow upstream plugs (e.g., signature-verification auth pipelines) to
  # pre-read the request body and stash it in `conn.assigns[:raw_body]`.
  # When present, we use it instead of calling `read_body/1`, which would
  # otherwise return an empty body since the underlying adapter has already
  # been consumed. Falls back to normal `read_body/1` when no cached body
  # is present, preserving existing behaviour for callers that don't pre-read.
  defp read_or_cached_body(%Plug.Conn{assigns: %{raw_body: body}} = conn, opts)
       when is_binary(body) do
    body_limit = Map.get(opts, :body_limit, 1_000_000)

    if byte_size(body) <= body_limit do
      {:ok, body, conn}
    else
      {:error, :body_too_large}
    end
  end

  defp read_or_cached_body(conn, opts) do
    body_limit = Map.get(opts, :body_limit, 1_000_000)

    case read_body(conn, length: body_limit, read_length: body_limit) do
      {:ok, body, conn} -> {:ok, body, conn}
      {:more, _partial, _conn} -> {:error, :body_too_large}
      {:error, reason} -> {:error, reason}
    end
  end

  defp resolve_handler_opts(conn, request, opts) do
    handler_opts = Map.get(opts, :handler_opts, [])

    resolved =
      case handler_opts do
        fun when is_function(fun, 2) ->
          fun.(conn, request)

        fun when is_function(fun, 1) ->
          fun.(conn)

        {module, function, args} when is_atom(module) and is_atom(function) and is_list(args) ->
          apply(module, function, [conn, request | args])

        other ->
          other
      end

    {:ok, Map.put(opts, :handler_opts, resolved)}
  rescue
    exception ->
      Logger.error("Failed to resolve MCP handler_opts: #{Exception.message(exception)}")
      {:error, :handler_opts_failed}
  end

  defp resolve_mrtr_identity(conn, request, token_info, opts) do
    principal =
      resolve_identity_value(
        Map.get(opts, :principal_id),
        conn,
        request,
        token_info,
        token_claim(token_info, ["sub", :sub, "principal_id", :principal_id])
      )

    tenant =
      resolve_identity_value(
        Map.get(opts, :tenant_id),
        conn,
        request,
        token_info,
        token_claim(token_info, ["tenant_id", :tenant_id, "tenant", :tenant])
      )

    with true <- is_nil(principal) or is_binary(principal),
         true <- is_nil(tenant) or is_binary(tenant) do
      {:ok, opts |> Map.put(:principal_id, principal) |> Map.put(:tenant_id, tenant)}
    else
      false -> {:error, :invalid_mrtr_identity}
    end
  rescue
    exception ->
      Logger.error("Failed to resolve MRTR identity: #{Exception.message(exception)}")
      {:error, :invalid_mrtr_identity}
  end

  defp resolve_identity_value(nil, _conn, _request, _token_info, fallback), do: fallback

  defp resolve_identity_value(fun, conn, request, token_info, _fallback)
       when is_function(fun, 3),
       do: fun.(conn, request, token_info)

  defp resolve_identity_value(fun, conn, request, _token_info, _fallback)
       when is_function(fun, 2),
       do: fun.(conn, request)

  defp resolve_identity_value(fun, conn, _request, _token_info, _fallback)
       when is_function(fun, 1),
       do: fun.(conn)

  defp resolve_identity_value({module, function, args}, conn, request, token_info, _fallback)
       when is_atom(module) and is_atom(function) and is_list(args),
       do: apply(module, function, [conn, request, token_info | args])

  defp resolve_identity_value(value, _conn, _request, _token_info, _fallback), do: value

  defp token_claim(token_info, keys) when is_map(token_info) do
    Enum.find_value(keys, &Map.get(token_info, &1))
  end

  defp token_claim(_token_info, _keys), do: nil

  # Handle session termination via DELETE request
  defp handle_session_delete(conn, session_id, opts) do
    with :ok <- validate_session_id_value(session_id),
         {:ok, conn} <- validate_request_origin(conn, opts),
         {:ok, _token_info} <- authorize_request(conn, %{"method" => "session/delete"}, opts),
         {:ok, session_manager} <- ensure_session_manager(opts.session_manager) do
      # Terminate the session. Deletion is idempotent, so log (rather than
      # crash on) failures and still acknowledge the request.
      case session_manager.terminate_session(session_id) do
        :ok -> :ok
        error -> Logger.error("Failed to terminate MCP session: #{inspect(error)}")
      end

      ExMCP.SubscriptionRegistry.remove_session(session_id)

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
    else
      {:error, :invalid_session_id} ->
        reject_invalid_session_id(conn, opts)

      {:error, :session_manager_unavailable} ->
        session_manager_unavailable_response(conn, opts)

      {:error, {:auth_error, {status, www_auth_header, body}}} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_header("www-authenticate", www_auth_header)
        |> send_resp(status, body)

      {:error, :oauth_guard_disabled} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_content_type("application/json")
        |> send_resp(500, Jason.encode!(Core.oauth_guard_disabled_error()))

      {:error, :origin_not_allowed} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> send_resp(403, "Origin not allowed")
    end
  end

  # Handle Server-Sent Events connections
  defp handle_sse_connection(conn, opts) do
    with :ok <- validate_session_id_headers(conn),
         {:ok, conn} <- validate_request_origin(conn, opts),
         {:ok, _token_info} <- authorize_request(conn, %{}, opts),
         {:ok, session_manager} <- ensure_session_manager(opts.session_manager) do
      conn =
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_header("content-type", "text/event-stream")
        |> put_resp_header("x-accel-buffering", "no")
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
          case session_manager.get_session(existing_session_id) do
            {:ok, session} when session.status == :active ->
              # Update session activity
              session_manager.update_session(existing_session_id, %{
                client_info: client_info,
                transport: :sse
              })

              existing_session_id

            _ ->
              # Session doesn't exist or is terminated, create new one
              session_manager.create_session(%{
                transport: :sse,
                client_info: client_info
              })
          end
        else
          # Create new session
          session_manager.create_session(%{
            transport: :sse,
            client_info: client_info
          })
        end

      serve_sse(conn, final_session_id, session_manager, opts)
    else
      {:error, :invalid_session_id} ->
        reject_invalid_session_id(conn, opts)

      {:error, {:auth_error, {status, www_auth_header, body}}} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> put_resp_header("www-authenticate", www_auth_header)
        |> send_resp(status, body)

      {:error, :session_manager_unavailable} ->
        session_manager_unavailable_response(conn, opts)

      {:error, :origin_not_allowed} ->
        conn
        |> maybe_add_cors_headers(opts)
        |> send_resp(403, "Origin not allowed")
    end
  end

  # `:sse_mode` decides how a GET SSE request is served:
  #
  #   * `:stream` (default) - start an `ExMCP.HttpPlug.SSEHandler` and keep the
  #     request process alive for the lifetime of the stream
  #   * `:oneshot` - write a single `connected` event and return, for callers
  #     (harnesses, health checks) that only need the handshake
  #
  # The mode is resolved in `init/1`, so the request path below has no
  # environment lookups or test-only branches (audit L6). Opts maps that were
  # hand-built rather than produced by `init/1` have no `:sse_mode` key, so the
  # default is resolved once here for them.
  defp serve_sse(conn, session_id, session_manager, opts)
       when not is_map_key(opts, :sse_mode) do
    serve_sse(conn, session_id, session_manager, Map.put(opts, :sse_mode, default_sse_mode()))
  end

  defp serve_sse(conn, session_id, _session_manager, %{sse_mode: :oneshot}) do
    {:ok, conn} = chunk(conn, "event: connected\ndata: {\"session_id\": \"#{session_id}\"}\n\n")

    conn
  end

  defp serve_sse(conn, session_id, session_manager, opts) do
    # Use the SSE handler with backpressure control
    {:ok, handler} = SSEHandler.start_link(conn, session_id, opts)

    # Register with session manager
    session_manager.update_session(session_id, %{handler_pid: handler})

    # Also register in our simple ETS registry
    register_sse_handler(session_id, handler, session_manager)

    # Block until handler exits
    ref = Process.monitor(handler)

    receive do
      {:DOWN, ^ref, :process, ^handler, reason} ->
        # Clean up the session registry when handler exits
        cleanup_sse_handler(session_id)

        # Terminate session in SessionManager if it was a clean shutdown
        if reason == :normal do
          session_manager.terminate_session(session_id)
        end

        conn
    end
  end

  # Default SSE mode. `:ex_mcp, :sse_mode` selects it explicitly; the legacy
  # `:ex_mcp, :test_mode` flag is still honoured here (and only here) so
  # existing harnesses keep working.
  defp default_sse_mode do
    case Application.get_env(:ex_mcp, :sse_mode) do
      mode when mode in [:stream, :oneshot] ->
        mode

      _unset ->
        if Application.get_env(:ex_mcp, :test_mode, false), do: :oneshot, else: :stream
    end
  end

  # The default session manager is supervised by the :ex_mcp application
  # (see ExMCP.Application). If it is not running, fail fast instead of
  # lazily starting an unsupervised copy linked to the HTTP request process.
  defp ensure_session_manager(ExMCP.SessionManager) do
    if Process.whereis(ExMCP.SessionManager) do
      {:ok, ExMCP.SessionManager}
    else
      Logger.error(
        "ExMCP.SessionManager is not running. Start the :ex_mcp application " <>
          "(or add ExMCP.SessionManager to your supervision tree) before " <>
          "serving MCP session requests."
      )

      {:error, :session_manager_unavailable}
    end
  end

  defp ensure_session_manager(session_manager), do: {:ok, session_manager}

  defp maybe_ensure_session(_session_manager, nil, _metadata), do: :ok

  defp maybe_ensure_session(session_manager, session_id, metadata) do
    if function_exported?(session_manager, :ensure_session, 2) do
      session_manager.ensure_session(session_id, metadata)
    else
      :ok
    end
  end

  defp maybe_put_session_header(conn, nil), do: conn

  defp maybe_put_session_header(conn, session_id) do
    put_resp_header(conn, "mcp-session-id", session_id)
  end

  defp session_manager_unavailable_response(conn, opts) do
    error_response =
      JSONRPC.error(nil, ErrorCodes.internal_error(), "Service unavailable", %{
        "type" => "session_manager_unavailable"
      })

    conn
    |> maybe_add_cors_headers(opts)
    |> put_resp_content_type("application/json")
    |> send_resp(503, Jason.encode!(error_response))
  end

  # Process MCP request using the configured handler
  defp process_mcp_request(%{"method" => "subscriptions/listen"} = request, opts) do
    open_http_subscription(request, opts)
  end

  defp process_mcp_request(request, opts) do
    handler = opts.handler
    handler_opts = Map.get(opts, :handler_opts, [])
    server_info = opts.server_info

    case handler do
      nil ->
        {:error, :no_handler_configured}

      handler_module when is_atom(handler_module) ->
        # Use ExMCP.MessageProcessor to process the request
        conn =
          ExMCP.MessageProcessor.new(request,
            transport: :http,
            session_id: Map.get(opts, :session_id)
          )

        # Create a simple processor that delegates to the handler
        processed_conn =
          ExMCP.MessageProcessor.process(conn, %{
            handler: handler_module,
            handler_opts: handler_opts,
            handler_call_timeout: Map.get(opts, :handler_call_timeout, 10_000),
            server_info: server_info,
            server_capabilities: Map.get(opts, :server_capabilities),
            protocol_mode: Map.get(opts, :protocol_mode),
            instructions: Map.get(opts, :instructions),
            request_state: Map.get(opts, :request_state),
            endpoint: Map.get(opts, :endpoint, "/mcp"),
            max_input_requests: Map.get(opts, :max_input_requests, 16),
            max_mrtr_bytes: Map.get(opts, :max_mrtr_bytes, 1_048_576),
            replay_cache: Map.get(opts, :replay_cache),
            require_replay_protection: Map.get(opts, :require_replay_protection, false),
            principal_id: Map.get(opts, :principal_id),
            tenant_id: Map.get(opts, :tenant_id),
            request_headers: Map.get(opts, :request_headers, [])
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

          response ->
            processed_response(response, processed_conn.assigns)
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

  defp processed_response(
         %{"jsonrpc" => "2.0", "error" => _} = response,
         %{http_status: status}
       ) do
    {:http_error, status, response}
  end

  defp processed_response(response, _assigns), do: {:ok, response}

  defp open_http_subscription(request, opts) do
    id = Map.get(request, "id")
    params = Map.get(request, "params") || %{}

    with {:ok, context} <- RequestContext.from_message(request),
         :ok <- RequestContext.validate_protocol_mode(context, Map.get(opts, :protocol_mode)),
         :ok <- RequestContext.validate_method(context),
         :modern <- context.era,
         subscription_options = subscription_options(opts),
         {:ok, entry} <-
           Subscriptions.listen(
             id,
             Map.get(params, "notifications"),
             self(),
             subscription_options
           ) do
      {:subscription, entry}
    else
      {:error, reason} -> {:ok, subscription_error(id, reason)}
      _other -> {:ok, subscription_error(id, :modern_protocol_required)}
    end
  end

  defp subscription_options(opts) do
    opts
    |> Map.to_list()
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Subscriptions.runtime_options()
  end

  defp subscription_error(id, reason) do
    JSONRPC.error(id, ErrorCodes.invalid_params(), "Subscription request rejected", %{
      "reason" => subscription_reason(reason)
    })
  end

  defp subscription_reason(reason) when is_atom(reason), do: Atom.to_string(reason)
  defp subscription_reason(reason), do: inspect(reason)

  defp subscription_keepalive_interval!(:infinity), do: :infinity

  defp subscription_keepalive_interval!(interval)
       when is_integer(interval) and interval > 0,
       do: interval

  defp subscription_keepalive_interval!(interval) do
    raise ArgumentError,
          ":subscription_keepalive_interval_ms must be a positive integer or :infinity, " <>
            "got: #{inspect(interval)}"
  end

  # Add CORS headers if enabled
  defp maybe_add_cors_headers(conn, %{cors_enabled: true} = opts) do
    case cors_response_origin(conn, opts) do
      nil ->
        conn

      origin ->
        conn
        |> put_resp_header("access-control-allow-origin", origin)
        |> put_resp_header("access-control-allow-methods", "GET, POST, DELETE, OPTIONS")
        |> put_resp_header(
          "access-control-allow-headers",
          "content-type, authorization, mcp-protocol-version, mcp-session-id"
        )
    end
  end

  defp maybe_add_cors_headers(conn, _opts), do: conn

  defp validate_request_origin(conn, %{validate_origin: false}), do: {:ok, conn}

  defp validate_request_origin(conn, opts) do
    if origin_allowed?(conn, opts), do: {:ok, conn}, else: {:error, :origin_not_allowed}
  end

  # Host-header allow-list (DNS rebinding protection). Prefers the raw Host
  # header — the literal value the client sent — and falls back to the
  # adapter-parsed conn.host when the header is absent (e.g. HTTP/2
  # :authority).
  defp request_host_allowed?(conn, opts) do
    Core.host_allowed?(request_host(conn), Map.get(opts, :allowed_hosts, :any))
  end

  defp request_host(conn) do
    case get_req_header(conn, "host") do
      [host | _] -> host
      [] -> conn.host
    end
  end

  # 421 Misdirected Request: the Host header does not match this server.
  defp reject_disallowed_host(conn, opts) do
    error_response = JSONRPC.error(nil, ErrorCodes.invalid_request(), "Host header not allowed")

    conn
    |> maybe_add_cors_headers(opts)
    |> put_resp_content_type("application/json")
    |> send_resp(421, Jason.encode!(error_response))
  end

  defp origin_allowed?(conn, opts) do
    conn
    |> origin_context()
    |> Core.origin_allowed?(opts)
  end

  defp cors_response_origin(conn, %{allowed_origins: :any}) do
    conn
    |> origin_context()
    |> Core.cors_response_origin(%{allowed_origins: :any})
  end

  defp cors_response_origin(conn, opts) do
    conn
    |> origin_context()
    |> Core.cors_response_origin(opts)
  end

  defp request_origin(conn), do: get_req_header(conn, "origin") |> List.first()

  defp origin_context(conn) do
    %{
      origin: request_origin(conn),
      scheme: Atom.to_string(conn.scheme),
      host: conn.host,
      port: conn.port
    }
  end

  # Extract session ID from request or generate a new one.
  # Per MCP spec, the server provides the session ID — the client's first
  # request should not include one, and the server generates it.
  # Both supported session headers are validated even though only
  # mcp-session-id is used on this path.
  defp get_or_create_session_id(conn) do
    with {:ok, session_id} <- fetch_session_id_header(conn, "mcp-session-id"),
         {:ok, _legacy} <- fetch_session_id_header(conn, "x-session-id") do
      {:ok, session_id || generate_session_id()}
    end
  end

  # Validates both supported session id headers (mcp-session-id plus the
  # legacy x-session-id) without selecting either.
  defp validate_session_id_headers(conn) do
    with {:ok, _} <- fetch_session_id_header(conn, "mcp-session-id"),
         {:ok, _} <- fetch_session_id_header(conn, "x-session-id") do
      :ok
    end
  end

  defp fetch_session_id_header(conn, header) do
    case get_req_header(conn, header) do
      [] ->
        {:ok, nil}

      [value] ->
        if valid_session_id?(value) do
          {:ok, value}
        else
          {:error, :invalid_session_id}
        end

      _multiple ->
        {:error, :invalid_session_id}
    end
  end

  defp validate_session_id_value(session_id) do
    if valid_session_id?(session_id) do
      :ok
    else
      {:error, :invalid_session_id}
    end
  end

  # Client-supplied session ids are echoed back in response headers and used
  # as registry keys, so they are strictly validated: bounded length and the
  # printable token charset [A-Za-z0-9._~+/=-], covering UUID, hex, and
  # base64/base64url shapes.
  defp valid_session_id?(session_id) when is_binary(session_id) do
    byte_size(session_id) in 1..@session_id_max_bytes and
      valid_session_id_chars?(session_id)
  end

  defp valid_session_id_chars?(<<>>), do: true

  defp valid_session_id_chars?(<<c, rest::binary>>)
       when c in ?A..?Z or c in ?a..?z or c in ?0..?9 or c in [?., ?_, ?~, ?+, ?/, ?=, ?-] do
    valid_session_id_chars?(rest)
  end

  defp valid_session_id_chars?(_other), do: false

  # 400 response for malformed session ids. Deliberately does not echo the
  # offending value or set a session header.
  defp reject_invalid_session_id(conn, opts) do
    error_response = JSONRPC.error(nil, ErrorCodes.invalid_request(), "Invalid session ID")

    conn
    |> maybe_add_cors_headers(opts)
    |> add_protocol_version_header()
    |> put_resp_content_type("application/json")
    |> send_resp(400, Jason.encode!(error_response))
  end

  # Register SSE handler for a session
  defp register_sse_handler(session_id, handler_pid, session_manager) do
    case SessionRegistry.register(session_id, handler_pid) do
      :ok ->
        :ok

      {:error, :registry_not_started} ->
        Logger.error(
          "ExMCP.HttpPlug.SessionRegistry is not running; SSE handler for session " <>
            "#{session_id} cannot be registered. Ensure the :ex_mcp application is started."
        )
    end

    # Also register with the configured session manager if available
    if function_exported?(session_manager, :update_session, 2) do
      session_manager.update_session(session_id, %{handler_pid: handler_pid})
    end
  end

  # Look up SSE handler for a session
  defp lookup_sse_handler(session_id) do
    SessionRegistry.lookup(session_id)
  end

  @doc """
  Broadcasts a resource update to each live SSE client subscribed to `uri`.

  Subscription lookup is performed directly against ETS, and delivery uses
  independent tasks so backpressure from one client does not block the rest.
  Sessions without a live SSE connection remain subscribed for reconnection
  and are removed by `ExMCP.SessionManager` when they expire.
  """
  @spec broadcast_resource_update(String.t()) :: %{
          subscribers: non_neg_integer(),
          delivered: non_neg_integer()
        }
  def broadcast_resource_update(uri) when is_binary(uri) do
    session_ids = ExMCP.SubscriptionRegistry.sessions(uri)

    delivered =
      session_ids
      |> Task.async_stream(&deliver_resource_update(&1, uri),
        ordered: false,
        timeout: 5_000,
        on_timeout: :kill_task
      )
      |> Enum.count(&match?({:ok, :ok}, &1))

    %{subscribers: length(session_ids), delivered: delivered}
  end

  defp deliver_resource_update(session_id, uri) do
    notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/resources/updated",
      "params" => %{"uri" => uri}
    }

    with {:ok, handler} <- lookup_sse_handler(session_id),
         true <- Process.alive?(handler),
         :ok <- SSEHandler.request_send(handler) do
      SSEHandler.send_event(handler, "message", notification)
    else
      _not_connected -> :not_delivered
    end
  end

  # Clean up SSE handler registration
  defp cleanup_sse_handler(session_id) do
    SessionRegistry.unregister(session_id)
  end

  # Generate a simple session ID
  defp generate_session_id do
    "sse_" <>
      (:crypto.strong_rand_bytes(16)
       |> Base.encode16(case: :lower))
  end

  # --- New Helper Functions ---

  defp validate_protocol_version(conn, request) do
    if modern_http_request?(conn, request) do
      request_id = Map.get(request, "id")

      case RequestHeaders.protocol_version(request) do
        version when is_binary(version) ->
          with :ok <- RequestHeaders.validate(conn.req_headers, request),
               true <- VersionRegistry.known?(version) do
            {:ok, conn}
          else
            {:error, message} -> {:error, {:header_mismatch, request_id, message}}
            false -> {:error, {:unsupported_protocol_version, request_id, version}}
          end

        _missing_version ->
          {:error, {:header_mismatch, request_id, "Protocol version metadata is missing"}}
      end
    else
      validate_legacy_protocol_version(conn)
    end
  end

  defp validate_legacy_protocol_version(conn) do
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

  defp validate_modern_method(request) do
    version = RequestHeaders.protocol_version(request)
    method = Map.get(request, "method")

    if VersionRegistry.modern?(version) do
      known? =
        Enum.any?(Methods.rows(), fn {known, _min, _max, _kind, _handlers} -> known == method end)

      if known? and Methods.available?(method, version),
        do: :ok,
        else: {:error, {:modern_method_not_found, Map.get(request, "id")}}
    else
      :ok
    end
  end

  defp assign_request_protocol_version(conn, request) do
    case RequestHeaders.protocol_version(request) do
      version when is_binary(version) -> assign(conn, :request_protocol_version, version)
      _missing -> conn
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
          # ServerGuard returns :ok only when the global OAuth feature flag is
          # disabled. If this plug opted into OAuth, fail closed instead of
          # silently allowing unauthenticated MCP requests.
          {:error, :oauth_guard_disabled}
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
    version = conn.assigns[:request_protocol_version] || VersionRegistry.latest_version()
    put_resp_header(conn, "mcp-protocol-version", version)
  end
end
