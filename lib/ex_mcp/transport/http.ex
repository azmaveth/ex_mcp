defmodule ExMCP.Transport.HTTP do
  @moduledoc """
  This module implements the standard MCP specification.

  Streamable HTTP transport for MCP with enhanced SSE support.

  This transport uses HTTP POST and GET requests with optional Server-Sent Events (SSE)
  for streaming server-to-client messages. This is one of the two
  official MCP transports defined in the specification.

  ## Features

  - **Auto-reconnection**: Automatic reconnection with exponential backoff
  - **Keep-alive**: Built-in heartbeat mechanism for connection health
  - **Event resumption**: Supports Last-Event-ID for event replay
  - **Session management**: Automatic session ID generation and tracking
  - **Configurable endpoint**: Customize the MCP endpoint path
  - **Single response mode**: Option to use HTTP responses instead of SSE
  - **Protocol Versioning**: Sends `mcp-protocol-version` header.

  ## Security Features

  The Streamable HTTP transport supports comprehensive security features:

  - **Authentication**: Bearer tokens, API keys, basic auth
  - **Origin Validation**: Prevent DNS rebinding attacks (recommended to enable)
  - **CORS Headers**: Cross-origin resource sharing
  - **Security Headers**: XSS protection, frame options, etc.
  - **TLS/SSL**: Secure connections with certificate validation

  ## Example with Security

      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        endpoint: "/mcp/v1",  # Configurable endpoint
        protocol_version: "2025-06-18", # Specify protocol version
        use_sse: true,         # Use SSE for responses (default: true)
        session_id: "existing-session",  # Resume existing session
        security: %{
          auth: {:bearer, "your-token"},
          validate_origin: true,
          allowed_origins: ["https://app.example.com"],
          cors: %{
            allowed_methods: ["GET", "POST"],
            allow_credentials: true
          }
        }
      )

  ## Session Management

  The HTTP transport automatically manages sessions using the `Mcp-Session-Id` header.
  Sessions enable:
  - Request/response correlation
  - Resumability after connection loss
  - Server-side state management

  ## Non-SSE Mode

  For simpler deployments, the HTTP transport can operate without SSE:

      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        use_sse: false  # Responses come in HTTP response body
      )

  > #### Security Best Practices {: .warning}
  >
  > 1. **Always use HTTPS** in production
  > 2. **Enable origin validation** to prevent DNS rebinding attacks
  > 3. **Bind to localhost** when possible for local servers
  > 4. **Implement proper authentication** (bearer tokens, API keys, etc.)
  > 5. **Set restrictive CORS policies** for cross-origin requests
  """

  @behaviour ExMCP.Transport
  require Logger

  alias ExMCP.Authorization.FullOAuthFlow
  alias ExMCP.Internal.{Security, SecurityConfig}
  alias ExMCP.Protocol.VersionNegotiator
  alias ExMCP.Transport.{SecurityGuard, SSEClient}

  defstruct [
    :base_url,
    :headers,
    :http_client,
    :sse_pid,
    :endpoint,
    :security,
    :origin,
    :session_id,
    :use_sse,
    :last_event_id,
    :last_response,
    :timeouts,
    :protocol_version,
    :auth_config,
    :access_token,
    :retry_delay,
    :max_retry_delay,
    :auth_provider,
    :auth_provider_state,
    sse_deferred_attempted: false,
    auth_completed: false
  ]

  @type t :: %__MODULE__{
          base_url: String.t(),
          headers: [{String.t(), String.t()}],
          http_client: module(),
          sse_pid: pid() | nil,
          endpoint: String.t(),
          security: ExMCP.Security.Validation.security_config() | nil,
          origin: String.t() | nil,
          session_id: String.t() | nil,
          use_sse: boolean(),
          last_event_id: String.t() | nil,
          last_response: map() | nil,
          timeouts: map(),
          protocol_version: String.t(),
          sse_deferred_attempted: boolean(),
          auth_provider: module() | nil,
          auth_provider_state: any()
        }

  @default_endpoint "/mcp/v1"
  @session_header "Mcp-Session-Id"
  @protocol_version_header "mcp-protocol-version"

  @impl true
  def connect(config) do
    raw_url = Keyword.fetch!(config, :url)

    # If no explicit endpoint is provided, extract path from URL.
    # e.g., "http://localhost:3000/mcp" → base_url: "http://localhost:3000", endpoint: "/mcp"
    {base_url, default_ep} = split_url_path(raw_url)

    headers = Keyword.get(config, :headers, [])
    http_client = Keyword.get(config, :http_client, :httpc)
    endpoint = Keyword.get(config, :endpoint, default_ep)
    security = Keyword.get(config, :security)
    use_sse = Keyword.get(config, :use_sse, true)
    session_id = Keyword.get(config, :session_id)

    protocol_version =
      Keyword.get(config, :protocol_version) ||
        Application.get_env(:ex_mcp, :protocol_version) ||
        VersionNegotiator.latest_version()

    # Extract timeout configurations with backwards compatibility
    connect_timeout = Keyword.get(config, :timeout, 5_000)
    request_timeout = Keyword.get(config, :request_timeout, 30_000)
    stream_handshake_timeout = Keyword.get(config, :stream_handshake_timeout, 15_000)
    stream_idle_timeout = Keyword.get(config, :stream_idle_timeout, 60_000)

    Logger.debug("HTTP transport connecting with use_sse: #{use_sse}, endpoint: #{endpoint}")

    # Extract origin if provided
    origin =
      case security do
        %{origin: origin} -> origin
        _ -> extract_origin_from_url(base_url)
      end

    # Build security headers
    security_headers =
      if security do
        Security.build_security_headers(security)
      else
        []
      end

    # Merge all headers
    all_headers = Enum.uniq_by(headers ++ security_headers, fn {name, _} -> name end)

    # Create timeout configuration
    timeouts = %{
      connect: connect_timeout,
      request: request_timeout,
      stream_handshake: stream_handshake_timeout,
      stream_idle: stream_idle_timeout
    }

    # OAuth config for automatic 401 → discover → token → retry flow.
    # Pass auth: %{client_id: "...", client_secret: "...", ...} to enable.
    auth_config = Keyword.get(config, :auth)

    # Reconnection options (passed through to SSEClient)
    max_retry_delay = Keyword.get(config, :max_retry_delay, 60_000)

    # Auth provider: explicit provider tuple, or auto-create from :auth config
    {auth_provider, auth_provider_state} =
      init_auth_provider(Keyword.get(config, :auth_provider), auth_config, base_url, endpoint)

    state = %__MODULE__{
      base_url: base_url,
      headers: all_headers,
      http_client: http_client,
      endpoint: endpoint,
      security: security,
      origin: origin,
      use_sse: use_sse,
      session_id: session_id,
      timeouts: timeouts,
      protocol_version: protocol_version,
      auth_config: auth_config,
      max_retry_delay: max_retry_delay,
      auth_provider: auth_provider,
      auth_provider_state: auth_provider_state
    }

    # Validate security configuration
    # Per MCP spec, session ID comes from the server after initialization.
    # SSE is deferred until after the first successful POST (which provides
    # the session ID needed for the GET SSE stream).
    with :ok <- validate_security(state) do
      if use_sse do
        Logger.debug("HTTP transport configured with SSE (deferred until after initialization)")
      else
        Logger.debug("SSE disabled, using synchronous HTTP responses")
      end

      {:ok, state}
    end
  end

  @impl true
  def send_message(message, %__MODULE__{use_sse: true, sse_pid: nil} = state) do
    # SSE configured but not yet started (pre-initialization).
    # Handle synchronously like non-SSE mode, then start SSE if we get a session ID.
    case perform_and_maybe_auth(message, state) do
      {:ok, response} ->
        case handle_http_response(response, state) do
          {:ok, new_state, response_data} ->
            new_state = maybe_start_deferred_sse(new_state)
            {:ok, new_state, response_data}

          {:ok, new_state} ->
            new_state = maybe_start_deferred_sse(new_state)
            {:ok, new_state}

          error ->
            error
        end

      {:ok, response, new_state} ->
        case handle_http_response(response, new_state) do
          {:ok, new_state2, response_data} ->
            new_state2 = maybe_start_deferred_sse(new_state2)
            {:ok, new_state2, response_data}

          {:ok, new_state2} ->
            new_state2 = maybe_start_deferred_sse(new_state2)
            {:ok, new_state2}

          error ->
            error
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def send_message(message, %__MODULE__{use_sse: false} = state) do
    # For non-SSE mode, we handle the request-response synchronously
    case perform_and_maybe_auth(message, state) do
      {:ok, response} ->
        handle_http_response(response, state)

      {:ok, response, new_state} ->
        handle_http_response(response, new_state)

      {:error, reason} ->
        {:error, reason}
    end
  end

  def send_message(message, %__MODULE__{} = state) do
    # SSE mode with active connection — responses arrive via SSE stream
    case perform_and_maybe_auth(message, state) do
      {:ok, response} -> handle_http_response(response, state)
      {:ok, response, new_state} -> handle_http_response(response, new_state)
      {:error, reason} -> {:error, reason}
    end
  end

  # Perform HTTP request with automatic OAuth retry via auth provider.
  defp perform_and_maybe_auth(body, %{auth_provider: provider} = state)
       when not is_nil(provider) do
    state = apply_provider_token(state)

    case perform_http_request(body, state) do
      {:ok, response} ->
        handle_provider_auth_challenge(response, body, state)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Legacy inline OAuth path (when no auth provider configured)
  defp perform_and_maybe_auth(body, state) do
    result = perform_http_request(body, state)

    case result do
      {:ok, response} ->
        case handle_auth_challenge(response, body, state) do
          :no_challenge ->
            {:ok, response}

          {:ok, retry_response, new_state} ->
            # Check if retry got another 401 (scope step-up)
            case handle_auth_challenge(retry_response, body, new_state) do
              :no_challenge -> {:ok, retry_response, new_state}
              {:ok, r, s} -> {:ok, r, s}
              {:error, reason} -> {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Route provider auth challenge based on response status.
  defp handle_provider_auth_challenge(response, body, state) do
    case extract_auth_status(response) do
      {:unauthorized, www_auth} ->
        provider_authenticate(:handle_unauthorized, www_auth, body, state)

      {:forbidden, www_auth} ->
        provider_authenticate(:handle_forbidden, www_auth, body, state)

      :ok ->
        {:ok, response}
    end
  end

  # Authenticate via provider, retry request, handle scope step-up on retry.
  defp provider_authenticate(callback, www_auth, body, state) do
    provider = state.auth_provider
    scopes = extract_scope_from_www_auth(www_auth)

    case apply(provider, callback, [www_auth, scopes, state.auth_provider_state]) do
      {:ok, token, new_ps} ->
        new_state = state |> Map.put(:auth_provider_state, new_ps) |> apply_token_to_state(token)
        provider_retry_request(body, new_state)

      {:error, reason, _ps} ->
        {:error, reason}
    end
  end

  # Retry request after auth; if 403 scope step-up, try once more.
  defp provider_retry_request(body, state) do
    case perform_http_request(body, state) do
      {:ok, retry_resp} ->
        case extract_auth_status(retry_resp) do
          {:forbidden, www_auth} ->
            provider_authenticate(:handle_forbidden, www_auth, body, state)

          _ ->
            {:ok, retry_resp, state}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp extract_auth_status({status_line, headers, _body}) do
    case status_line do
      {_, 401, _} -> {:unauthorized, find_header(headers, "www-authenticate")}
      {_, 403, _} -> {:forbidden, find_header(headers, "www-authenticate")}
      _ -> :ok
    end
  end

  defp extract_auth_status(_), do: :ok

  defp apply_provider_token(%{auth_provider: provider, auth_provider_state: ps} = state) do
    case provider.get_token(ps) do
      {:ok, nil, new_ps} ->
        %{state | auth_provider_state: new_ps}

      {:ok, token, new_ps} ->
        %{state | auth_provider_state: new_ps, headers: put_bearer_header(state.headers, token)}

      {:error, _} ->
        state
    end
  end

  defp apply_token_to_state(state, token) do
    %{state | access_token: token, headers: put_bearer_header(state.headers, token)}
  end

  defp init_auth_provider({mod, provider_config}, _auth_config, _base_url, _endpoint) do
    case mod.init(provider_config) do
      {:ok, ps} -> {mod, ps}
      {:error, _} -> {nil, nil}
    end
  end

  defp init_auth_provider(nil, auth_config, base_url, endpoint) when is_map(auth_config) do
    provider_config = Map.put(auth_config, :resource_url, base_url <> (endpoint || ""))
    {:ok, ps} = ExMCP.Authorization.Provider.OAuth.init(provider_config)
    {ExMCP.Authorization.Provider.OAuth, ps}
  end

  defp init_auth_provider(nil, _auth_config, _base_url, _endpoint), do: {nil, nil}

  # Check if the response is a 401 that we can handle with OAuth
  defp handle_auth_challenge({status_line, headers, _body}, original_body, state) do
    case status_line do
      {_, 401, _} ->
        maybe_oauth_retry(headers, original_body, state)

      {_, 403, _} ->
        # 403 with insufficient_scope → scope step-up
        maybe_oauth_retry(headers, original_body, state)

      _ ->
        :no_challenge
    end
  end

  defp handle_auth_challenge(_, _, _), do: :no_challenge

  # Attempt OAuth discovery and retry if auth_config is available
  defp maybe_oauth_retry(headers, original_body, %{auth_config: nil} = state) do
    # No explicit auth config — try full OAuth flow if this looks like an OAuth challenge
    www_auth = find_header(headers, "www-authenticate")

    if www_auth && String.starts_with?(www_auth, "Bearer") do
      run_full_oauth_flow(www_auth, original_body, state)
    else
      :no_challenge
    end
  end

  defp maybe_oauth_retry(headers, original_body, %{access_token: token} = state)
       when is_binary(token) and byte_size(token) > 0 do
    # Already have a token but got 401/403 — check if this is a scope step-up
    www_auth = find_header(headers, "www-authenticate") || ""

    if String.contains?(www_auth, "insufficient_scope") do
      # Scope step-up: clear token and re-auth with new scope requirements
      Logger.info("Scope step-up required, re-authorizing")
      new_state = %{state | access_token: nil, auth_completed: false}
      maybe_oauth_retry(headers, original_body, new_state)
    else
      # Token rejected for other reason — auth loop protection
      Logger.warning("Auth failed after successful OAuth flow, not retrying")
      :no_challenge
    end
  end

  defp maybe_oauth_retry(headers, original_body, state) do
    # Have auth_config with credentials — use FullOAuthFlow which handles
    # PRM discovery with header fallback and pre-existing credentials
    www_auth = find_header(headers, "www-authenticate")
    resource_url = build_url(state, "")

    config =
      (state.auth_config || %{})
      |> Map.put(:resource_url, resource_url)
      |> Map.put(:www_authenticate, www_auth)

    Logger.info("Received 401, attempting OAuth flow with credentials")

    case FullOAuthFlow.execute(config) do
      {:ok, token_result} ->
        access_token = token_result[:access_token] || token_result["access_token"]
        Logger.info("OAuth token obtained, retrying request")

        # Update state with token and add Authorization header
        new_state = %{
          state
          | access_token: access_token,
            auth_completed: true,
            headers: put_bearer_header(state.headers, access_token)
        }

        # Retry the original request with auth headers
        case perform_http_request(original_body, new_state) do
          {:ok, response} -> {:ok, response, new_state}
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} ->
        Logger.warning("OAuth discovery flow failed: #{inspect(reason)}")
        {:error, {:oauth_failed, reason}}
    end
  end

  defp run_full_oauth_flow(www_auth, original_body, state) do
    Logger.info("Attempting full OAuth flow (authorization code + PKCE)")
    resource_url = build_url(state, "")

    # Extract scope from WWW-Authenticate header if present
    scopes = extract_scope_from_www_auth(www_auth)

    config = %{
      resource_url: resource_url,
      www_authenticate: www_auth,
      scopes: scopes
    }

    case FullOAuthFlow.execute(config) do
      {:ok, token_result} ->
        access_token = token_result[:access_token] || token_result["access_token"]
        Logger.info("Full OAuth flow succeeded, retrying request")

        new_state = %{
          state
          | access_token: access_token,
            auth_completed: true,
            headers: put_bearer_header(state.headers, access_token)
        }

        case perform_http_request(original_body, new_state) do
          {:ok, response} -> {:ok, response, new_state}
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} ->
        Logger.warning("Full OAuth flow failed: #{inspect(reason)}")
        {:error, {:oauth_failed, reason}}
    end
  end

  defp extract_scope_from_www_auth(nil), do: []

  defp extract_scope_from_www_auth(www_auth) when is_binary(www_auth) do
    case Regex.run(~r/scope="([^"]+)"/, www_auth) do
      [_, scope_str] -> String.split(scope_str, " ", trim: true)
      _ -> []
    end
  end

  defp put_bearer_header(headers, token) do
    headers
    |> Enum.reject(fn {k, _} -> String.downcase(k) == "authorization" end)
    |> List.insert_at(0, {"Authorization", "Bearer #{token}"})
  end

  defp perform_http_request(body, state) do
    # According to MCP spec, POST directly to the MCP endpoint, not /messages
    url = build_url(state, "")
    Logger.debug("HTTP request to URL: #{url}")
    headers = build_request_headers(state)

    # Security validation before making external request
    case validate_http_request(url, headers, state) do
      {:ok, sanitized_headers} ->
        make_http_request(url, sanitized_headers, body, state)

      {:error, security_error} ->
        Logger.warning("HTTP request blocked by security policy",
          url: url,
          error: security_error
        )

        {:error, {:security_violation, security_error}}
    end
  end

  defp validate_http_request(url, headers, state) do
    # Build SecurityGuard request
    security_request = %{
      url: url,
      headers: headers,
      method: "POST",
      transport: :http,
      user_id: extract_user_id(state)
    }

    # Get transport-specific security configuration
    config = SecurityConfig.get_transport_config(:http)

    case SecurityGuard.validate_request(security_request, config) do
      {:ok, sanitized_request} ->
        {:ok, sanitized_request.headers}

      {:error, security_error} ->
        {:error, security_error}
    end
  end

  defp make_http_request(url, headers, body, state) do
    request = {
      String.to_charlist(url),
      Enum.map(headers, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end),
      String.to_charlist("application/json"),
      body
    }

    # Add request timeout to HTTP options
    base_http_opts = [{:timeout, state.timeouts.request}]

    http_opts =
      case URI.parse(url).scheme do
        "https" -> build_ssl_options_from_state(state) ++ base_http_opts
        _ -> base_http_opts
      end

    :httpc.request(:post, request, http_opts, [])
  end

  defp extract_user_id(state) do
    config = SecurityConfig.get_transport_config(:http)

    case Map.get(config, :user_id_resolver) do
      nil ->
        # Default behavior if no resolver is configured
        cond do
          # Check if user_id is explicitly set in state (struct is a map)
          Map.has_key?(state, :user_id) ->
            Map.get(state, :user_id)

          # Check security context for user information
          is_map(state.security) ->
            Map.get(state.security, :user_id, "http_anonymous")

          # Check session ID as fallback
          state.session_id ->
            "session_#{state.session_id}"

          # Default fallback
          true ->
            "http_anonymous"
        end

      resolver ->
        # Use the configured resolver. The resolver is expected to be a function
        # that takes the transport state and returns a user ID string.
        resolver.(state)
    end
  end

  defp handle_http_response({status_line, headers, body}, state) do
    # Convert charlist to binary if needed (httpc returns charlists by default)
    body_binary = if is_list(body), do: List.to_string(body), else: body

    # Extract session ID from server response headers (per MCP spec)
    state = maybe_update_session_id(headers, state)

    case status_line do
      {_, 200, _} ->
        # Always parse the response body — per MCP spec, POST responses
        # contain the result even in SSE mode. The SSE stream is for
        # server-initiated messages (notifications, progress updates).
        handle_non_sse_response(body_binary, headers, state)

      {_, 202, _} ->
        # 202 Accepted - notification was accepted, no response body expected
        {:ok, state}

      {_, 401, _} ->
        # Extract WWW-Authenticate header for OAuth discovery hints
        www_auth = find_header(headers, "www-authenticate")
        {:error, {:unauthorized, 401, body_binary, www_auth}}

      {_, status, _} ->
        {:error, {:http_error, status, body_binary}}
    end
  end

  defp handle_http_response({:error, reason}, _state) do
    {:error, reason}
  end

  defp handle_non_sse_response(body, headers, state) do
    content_type = find_header(headers, "content-type") || ""

    if String.contains?(content_type, "text/event-stream") do
      handle_sse_post_response(body, state)
    else
      case Jason.decode(body) do
        {:ok, response} ->
          {:ok, %{state | last_response: response}, Jason.encode!(response)}

        {:error, reason} ->
          {:error, {:json_decode_error, reason}}
      end
    end
  end

  # Process SSE-formatted POST response.
  # Extracts JSON data, retry fields, and event IDs.
  # If no JSON result is found (just priming events), the response
  # will come via GET SSE stream — start/ensure SSE is connected.
  defp handle_sse_post_response(body, state) do
    events = parse_sse_events(body)

    # Extract retry field and last event ID from events
    state =
      Enum.reduce(events, state, fn event, acc ->
        acc =
          case event[:retry] do
            nil -> acc
            ms -> %{acc | retry_delay: ms}
          end

        case event[:id] do
          nil -> acc
          id -> %{acc | last_event_id: id}
        end
      end)

    # Find the JSON data event (if any)
    json_data =
      events
      |> Enum.flat_map(fn event ->
        case event[:data] do
          nil -> []
          "" -> []
          data -> [data]
        end
      end)
      |> Enum.join("")

    if json_data != "" do
      case Jason.decode(json_data) do
        {:ok, response} ->
          {:ok, %{state | last_response: response}, Jason.encode!(response)}

        {:error, _} ->
          # SSE had data but it wasn't valid JSON — treat as no response.
          # Close existing GET stream so it reconnects with retry delay.
          trigger_sse_reconnect(state)
          state = maybe_start_deferred_sse(state)
          {:ok, state}
      end
    else
      # No JSON data in SSE response (just priming events).
      # Result will arrive via GET SSE stream.
      # Close existing GET stream to trigger reconnection with retry delay.
      trigger_sse_reconnect(state)
      state = maybe_start_deferred_sse(state)
      {:ok, state}
    end
  end

  # Parse raw SSE text into a list of event maps
  defp parse_sse_events(body) do
    body
    |> String.split("\n\n")
    |> Enum.map(fn block ->
      block
      |> String.split("\n")
      |> Enum.reduce(%{}, fn line, acc ->
        cond do
          String.starts_with?(line, "data: ") ->
            data = String.trim_leading(line, "data: ")
            Map.update(acc, :data, data, fn existing -> existing <> data end)

          String.starts_with?(line, "data:") ->
            data = String.trim_leading(line, "data:")
            Map.update(acc, :data, data, fn existing -> existing <> data end)

          String.starts_with?(line, "id: ") ->
            Map.put(acc, :id, String.trim_leading(line, "id: "))

          String.starts_with?(line, "retry: ") ->
            case Integer.parse(String.trim_leading(line, "retry: ")) do
              {ms, _} -> Map.put(acc, :retry, ms)
              _ -> acc
            end

          String.starts_with?(line, "event: ") ->
            Map.put(acc, :event, String.trim_leading(line, "event: "))

          true ->
            acc
        end
      end)
    end)
    |> Enum.reject(&(&1 == %{}))
  end

  @impl true
  def receive_message(%__MODULE__{use_sse: true, sse_pid: sse_pid} = state)
      when is_pid(sse_pid) do
    # Ensure SSEClient sends events to the calling process (may be the receiver task,
    # not the process that originally started the SSE connection)
    send(sse_pid, {:change_parent, self()})

    receive do
      {:sse_event, ^sse_pid, %{data: data} = event} ->
        # The enhanced SSE client sends structured events
        event_id = Map.get(event, :id)
        new_state = if event_id, do: %{state | last_event_id: event_id}, else: state

        case Jason.decode(data) do
          {:ok, %{"type" => "keep-alive"}} ->
            # Ignore keep-alive messages and continue receiving
            receive_message(new_state)

          {:ok, message} ->
            {:ok, message, new_state}

          {:error, reason} ->
            {:error, {:json_decode_error, reason}}
        end

      {:sse_error, ^sse_pid, reason} ->
        {:error, {:sse_error, reason}}

      {:sse_closed, ^sse_pid} ->
        {:error, :connection_closed}
    end
  end

  def receive_message(%__MODULE__{last_response: response} = state)
      when response != nil do
    # Return stored response (from POST) and clear it
    {:ok, response, %{state | last_response: nil}}
  end

  def receive_message(%__MODULE__{use_sse: true, sse_pid: nil} = state) do
    # SSE enabled but no stream started yet (no session ID from server).
    # Wait for SSE to connect, or a 405/error indicating SSE not supported.
    receive do
      {:sse_connected, pid} ->
        receive_message(%{state | sse_pid: pid})

      {:sse_not_supported, _pid} ->
        # Server doesn't support SSE — fall back to sync mode
        Logger.info("SSE not supported, falling back to sync mode")
        {:error, :not_supported_in_sync_mode}
    after
      500 ->
        {:error, :waiting_for_session}
    end
  end

  def receive_message(%__MODULE__{use_sse: false} = _state) do
    # Non-SSE mode — responses returned directly from send_message
    {:error, :not_supported_in_sync_mode}
  end

  def receive_message(%__MODULE__{} = _state) do
    {:error, :not_connected}
  end

  @doc """
  Terminates the server-side session by sending DELETE to the endpoint.

  Per the MCP spec, clients SHOULD send a DELETE request with the session ID
  to allow the server to clean up session state. This is best-effort — errors
  are logged but don't prevent client shutdown.

  Returns `:ok` regardless of server response (fire-and-forget).
  """
  @spec terminate_session(t()) :: :ok
  def terminate_session(%__MODULE__{session_id: nil}), do: :ok

  def terminate_session(%__MODULE__{session_id: session_id} = state) when is_binary(session_id) do
    url = state.base_url <> state.endpoint

    headers = [
      {@session_header, session_id},
      {@protocol_version_header, state.protocol_version}
      | state.headers
    ]

    charlist_url = String.to_charlist(url)

    charlist_headers =
      Enum.map(headers, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end)

    http_opts = [{:timeout, state.timeouts.connect}]

    case :httpc.request(:delete, {charlist_url, charlist_headers}, http_opts, []) do
      {:ok, {{_, status, _}, _, _}} when status in [200, 202, 204] ->
        Logger.debug("Session #{session_id} terminated (HTTP #{status})")
        :ok

      {:ok, {{_, status, _}, _, _}} ->
        Logger.debug("Session termination returned HTTP #{status} (non-fatal)")
        :ok

      {:error, reason} ->
        Logger.debug("Session termination failed: #{inspect(reason)} (non-fatal)")
        :ok
    end
  end

  @impl true
  def close(%__MODULE__{} = state) do
    # Best-effort session termination before closing
    terminate_session(state)

    # Stop SSE connection if active
    if is_pid(state.sse_pid) do
      Process.exit(state.sse_pid, :normal)
    end

    :ok
  end

  # NOTE: HTTP transport does NOT implement subscribe/2 (push model) because
  # bidirectional flows (elicitation, sampling) require the receiver task to
  # process SSE events while the GenServer is blocked on a synchronous POST.
  # The push model would deadlock: GenServer blocked in handle_call doing POST,
  # can't process {:transport_event, elicitation} in handle_info.
  # The receiver task (separate process) avoids this by forwarding SSE events
  # as {:transport_message, msg} which queue in the GenServer mailbox.
  # When :httpc is replaced with an async HTTP client, push mode can be enabled.

  # Private functions

  defp maybe_update_session_id(headers, state) do
    case find_header(headers, @session_header) do
      nil -> state
      session_id -> %{state | session_id: session_id}
    end
  end

  defp maybe_start_deferred_sse(
         %{use_sse: true, sse_pid: nil, session_id: session_id, sse_deferred_attempted: false} =
           state
       )
       when is_binary(session_id) do
    Logger.debug("Starting deferred SSE connection with session ID: #{session_id}")
    state = %{state | sse_deferred_attempted: true}

    case start_sse(state) do
      {:ok, sse_pid} ->
        handshake_timeout = state.timeouts.stream_handshake

        receive do
          {:sse_connected, ^sse_pid} ->
            Logger.debug("Deferred SSE connection established")
            %{state | sse_pid: sse_pid}

          {:sse_error, ^sse_pid, reason} ->
            Logger.debug("Deferred SSE connection failed: #{inspect(reason)}, falling back")
            state
        after
          handshake_timeout ->
            Logger.debug("Deferred SSE connection timeout, falling back to non-SSE")
            state
        end

      {:error, reason} ->
        Logger.debug("Failed to start deferred SSE: #{inspect(reason)}, falling back")
        state
    end
  end

  defp maybe_start_deferred_sse(state), do: state

  # Trigger SSE reconnection when POST response closes without a result.
  # Sends a message to SSEClient to close current connection and reconnect
  # with the server-specified retry delay.
  defp trigger_sse_reconnect(%{
         sse_pid: sse_pid,
         retry_delay: retry_delay,
         last_event_id: last_id
       })
       when is_pid(sse_pid) do
    Logger.info("Triggering SSE reconnection (POST response had no result)")
    if retry_delay, do: send(sse_pid, {:update_retry_delay, retry_delay})
    if last_id, do: send(sse_pid, {:update_last_event_id, last_id})
    send(sse_pid, :force_reconnect)
  end

  defp trigger_sse_reconnect(%{sse_pid: sse_pid}) when is_pid(sse_pid) do
    send(sse_pid, :force_reconnect)
  end

  defp trigger_sse_reconnect(_state), do: :ok

  defp start_sse(state) do
    url = build_url(state, "")
    Logger.debug("Starting SSE connection to: #{url}")
    ssl_opts = build_ssl_options_from_state(state)

    # Build headers including session
    sse_headers = [
      {@session_header, state.session_id} | state.headers
    ]

    # Add Last-Event-ID if we have one for resumability
    sse_headers =
      if state.last_event_id do
        [{"Last-Event-ID", state.last_event_id} | sse_headers]
      else
        sse_headers
      end

    # Use the enhanced SSE client with keep-alive and reconnection.
    # Pass retry_delay from POST SSE response if available.
    opts = [
      url: url,
      headers: sse_headers,
      initial_retry_delay: state.retry_delay,
      max_retry_delay: state.max_retry_delay,
      ssl_opts: ssl_opts,
      parent: self(),
      connect_timeout: state.timeouts.connect,
      idle_timeout: state.timeouts.stream_idle
    ]

    case SSEClient.start_link(opts) do
      {:ok, sse_pid} ->
        # Return immediately, connection happens asynchronously
        {:ok, sse_pid}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_url(%__MODULE__{base_url: base_url, endpoint: endpoint}, path) do
    # Normalize endpoint - ensure it starts with / and doesn't end with /
    normalized_endpoint = normalize_endpoint(endpoint)

    base_url
    |> URI.parse()
    |> Map.put(:path, normalized_endpoint <> path)
    |> URI.to_string()
  end

  defp normalize_endpoint(""), do: ""

  defp normalize_endpoint(endpoint) do
    endpoint
    |> ensure_leading_slash()
    |> String.trim_trailing("/")
  end

  defp ensure_leading_slash("/" <> _ = endpoint), do: endpoint
  defp ensure_leading_slash(endpoint), do: "/" <> endpoint

  # Security-related helper functions

  defp validate_security(%{security: nil}), do: :ok

  defp validate_security(%{security: security}) do
    Security.validate_config(security)
  end

  # Split a full URL into {base_url, path} so callers can pass a single URL.
  # "http://localhost:3000/mcp" → {"http://localhost:3000", "/mcp"}
  # "http://localhost:3000" → {"http://localhost:3000", "/mcp/v1"} (default)
  defp split_url_path(url) do
    uri = URI.parse(url)

    case uri.path do
      nil ->
        {url, @default_endpoint}

      "/" ->
        {url, @default_endpoint}

      "" ->
        {url, @default_endpoint}

      path ->
        base = "#{uri.scheme}://#{uri.host}#{if uri.port, do: ":#{uri.port}", else: ""}"
        {base, path}
    end
  end

  defp extract_origin_from_url(url) do
    uri = URI.parse(url)

    if uri.scheme && uri.host do
      "#{uri.scheme}://#{uri.host}#{if uri.port && uri.port != default_port(uri.scheme), do: ":#{uri.port}", else: ""}"
    else
      nil
    end
  end

  defp default_port("http"), do: 80
  defp default_port("https"), do: 443
  defp default_port(_), do: nil

  defp build_request_headers(%{
         headers: headers,
         security: security,
         origin: origin,
         session_id: session_id,
         last_event_id: last_event_id,
         protocol_version: protocol_version
       }) do
    base_headers = [
      {"content-type", "application/json"},
      {"accept", "application/json, text/event-stream"},
      {@protocol_version_header, protocol_version}
      | headers
    ]

    # Add session header only if we have a session ID (per MCP spec,
    # the first request should not include a session ID — the server provides one)
    headers_with_session =
      if session_id do
        [{@session_header, session_id} | base_headers]
      else
        base_headers
      end

    # Add Last-Event-ID for resumability if available
    headers_with_event_id =
      if last_event_id do
        [{"Last-Event-ID", last_event_id} | headers_with_session]
      else
        headers_with_session
      end

    # Add Origin header if we have one
    headers_with_origin =
      if origin do
        [{"Origin", origin} | headers_with_event_id]
      else
        headers_with_event_id
      end

    # Add security headers if configured
    if security && Map.get(security, :include_security_headers, false) do
      headers_with_origin ++ Security.build_standard_security_headers()
    else
      headers_with_origin
    end
  end

  @doc """
  Builds SSL options from TLS configuration.

  ## Examples

      tls_config = %{
        verify: :verify_peer,
        versions: [:"tlsv1.2", :"tlsv1.3"],
        cert: "client.pem",
        key: "client.key"
      }

      ssl_opts = ExMCP.Transport.HTTP.build_ssl_options(tls_config)
  """
  def build_ssl_options(tls_config) when is_map(tls_config) do
    base_ssl_opts = [
      verify: Map.get(tls_config, :verify, :verify_peer),
      cacerts: Map.get(tls_config, :cacerts, :public_key.cacerts_get()),
      versions: Map.get(tls_config, :versions, [:"tlsv1.2", :"tlsv1.3"])
    ]

    # Add client certificate if provided
    ssl_opts =
      case Map.get(tls_config, :cert) do
        nil -> base_ssl_opts
        cert -> Keyword.put(base_ssl_opts, :cert, cert)
      end

    # Add private key if provided
    ssl_opts =
      case Map.get(tls_config, :key) do
        nil -> ssl_opts
        key -> Keyword.put(ssl_opts, :key, key)
      end

    # Add cipher suites if provided
    ssl_opts =
      case Map.get(tls_config, :ciphers) do
        nil -> ssl_opts
        ciphers -> Keyword.put(ssl_opts, :ciphers, ciphers)
      end

    # Add verify function if provided
    ssl_opts =
      case Map.get(tls_config, :verify_fun) do
        nil -> ssl_opts
        verify_fun -> Keyword.put(ssl_opts, :verify_fun, verify_fun)
      end

    [ssl: ssl_opts]
  end

  def build_ssl_options(_) do
    # Default secure SSL options
    [
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    ]
  end

  defp build_ssl_options_from_state(%{security: %{tls: tls_config}}) when is_map(tls_config) do
    build_ssl_options(tls_config)
  end

  defp build_ssl_options_from_state(_state) do
    build_ssl_options(%{})
  end

  defp find_header(headers, name) do
    name_lower = String.downcase(name)

    Enum.find_value(headers, fn
      {key, value} when is_list(key) ->
        if String.downcase(to_string(key)) == name_lower do
          to_string(value)
        end

      {key, value} when is_binary(key) ->
        if String.downcase(key) == name_lower do
          value
        end

      _ ->
        nil
    end)
  end
end
