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

  alias ExMCP.Internal.Security
  alias ExMCP.Transport.SSEClient

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
    :timeouts
  ]

  @type t :: %__MODULE__{
          base_url: String.t(),
          headers: [{String.t(), String.t()}],
          http_client: module(),
          sse_pid: pid() | nil,
          endpoint: String.t(),
          security: Security.security_config() | nil,
          origin: String.t() | nil,
          session_id: String.t() | nil,
          use_sse: boolean(),
          last_event_id: String.t() | nil,
          last_response: map() | nil,
          timeouts: map()
        }

  @default_endpoint "/mcp/v1"
  @session_header "Mcp-Session-Id"

  @impl true
  def connect(config) do
    base_url = Keyword.fetch!(config, :url)
    headers = Keyword.get(config, :headers, [])
    http_client = Keyword.get(config, :http_client, :httpc)
    endpoint = Keyword.get(config, :endpoint, @default_endpoint)
    security = Keyword.get(config, :security)
    use_sse = Keyword.get(config, :use_sse, true)
    session_id = Keyword.get(config, :session_id)
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

    state = %__MODULE__{
      base_url: base_url,
      headers: all_headers,
      http_client: http_client,
      endpoint: endpoint,
      security: security,
      origin: origin,
      use_sse: use_sse,
      session_id: session_id || generate_session_id(),
      timeouts: timeouts
    }

    # Validate security configuration
    with :ok <- validate_security(state) do
      if use_sse do
        Logger.debug("Starting SSE for HTTP transport")

        case start_sse(state) do
          {:ok, sse_pid} ->
            Logger.debug("SSE started successfully with pid: #{inspect(sse_pid)}")
            # Wait for SSE connection to be fully established using configurable timeout
            handshake_timeout = state.timeouts.stream_handshake
            Logger.debug("Waiting for SSE handshake, timeout: #{handshake_timeout}ms")

            receive do
              {:sse_connected, ^sse_pid} ->
                Logger.debug("SSE connection fully established")
                {:ok, %{state | sse_pid: sse_pid}}

              {:sse_error, ^sse_pid, reason} ->
                Logger.debug("SSE connection failed: #{inspect(reason)}")
                {:error, {:sse_connection_failed, reason}}
            after
              handshake_timeout ->
                Logger.debug("SSE connection timeout after #{handshake_timeout}ms")
                {:error, :sse_connection_timeout}
            end

          error ->
            Logger.debug("SSE start failed: #{inspect(error)}")
            error
        end
      else
        Logger.debug("SSE disabled, using synchronous HTTP responses")
        {:ok, state}
      end
    end
  end

  @impl true
  def send_message(message, %__MODULE__{use_sse: false} = state) do
    # For non-SSE mode, we handle the request-response synchronously
    body = message
    result = perform_http_request(body, state)

    case result do
      {:ok, response} ->
        case handle_http_response(response, state) do
          {:ok, new_state} ->
            # Return both the response data and the updated state for immediate consumption
            case new_state.last_response do
              # No response body (e.g., 202 notifications)
              nil ->
                {:ok, new_state}

              response_data ->
                # Return the response data and clear it from state to prevent reuse
                {:ok, %{new_state | last_response: nil}, response_data}
            end

          error ->
            error
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def send_message(message, %__MODULE__{} = state) do
    # Message is already JSON encoded by the client
    body = message
    result = perform_http_request(body, state)

    case result do
      {:ok, response} -> handle_http_response(response, state)
      {:error, reason} -> {:error, reason}
    end
  end

  defp perform_http_request(body, state) do
    # According to MCP spec, POST directly to the MCP endpoint, not /messages
    url = build_url(state, "")
    Logger.debug("HTTP request to URL: #{url}")
    headers = build_request_headers(state)

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

  defp handle_http_response({status_line, headers, body}, state) do
    # Convert charlist to binary if needed (httpc returns charlists by default)
    body_binary = if is_list(body), do: List.to_string(body), else: body

    case status_line do
      {_, 200, _} ->
        if state.use_sse do
          # SSE mode - responses come via SSE
          case validate_cors_response(headers, state) do
            :ok -> {:ok, state}
            error -> error
          end
        else
          # Non-SSE mode - response in HTTP body
          handle_non_sse_response(body_binary, state)
        end

      {_, 202, _} ->
        # 202 Accepted - notification was accepted, no response body expected
        if state.use_sse do
          {:ok, state}
        else
          # For non-SSE mode, we still need to signal that the request completed
          # even though there's no JSON response to parse
          {:ok, state}
        end

      {_, status, _} ->
        {:error, {:http_error, status, body_binary}}
    end
  end

  defp handle_http_response({:error, reason}, _state) do
    {:error, reason}
  end

  defp handle_non_sse_response(body, state) do
    case Jason.decode(body) do
      {:ok, response} ->
        # In non-SSE mode, store response in transport state for immediate access
        {:ok, %{state | last_response: response}}

      {:error, reason} ->
        {:error, {:json_decode_error, reason}}
    end
  end

  @impl true
  def receive_message(%__MODULE__{use_sse: true, sse_pid: sse_pid} = state)
      when is_pid(sse_pid) do
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

  def receive_message(%__MODULE__{use_sse: false, last_response: response} = state)
      when response != nil do
    # Non-SSE mode - return the stored response and clear it
    {:ok, response, %{state | last_response: nil}}
  end

  def receive_message(%__MODULE__{use_sse: false} = _state) do
    # Non-SSE mode with no response available
    {:error, :no_response}
  end

  def receive_message(%__MODULE__{} = _state) do
    {:error, :not_connected}
  end

  @impl true
  def close(%__MODULE__{sse_pid: sse_pid}) when is_pid(sse_pid) do
    Process.exit(sse_pid, :normal)
    :ok
  end

  def close(%__MODULE__{use_sse: false}) do
    # Non-SSE mode - no special cleanup needed
    :ok
  end

  def close(%__MODULE__{}), do: :ok

  # Private functions

  defp start_sse(state) do
    url = build_url(state, "/sse")
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

    # Use the enhanced SSE client with keep-alive and reconnection
    opts = [
      url: url,
      headers: sse_headers,
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

  defp generate_session_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
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
         last_event_id: last_event_id
       }) do
    base_headers = [{"content-type", "application/json"} | headers]

    # Add session header
    headers_with_session = [{@session_header, session_id} | base_headers]

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

  defp validate_cors_response(
         response_headers,
         %{security: %{validate_origin: true, allowed_origins: allowed}} = state
       ) do
    origin_header = find_header(response_headers, "access-control-allow-origin")

    case origin_header do
      nil ->
        {:error, :missing_cors_header}

      "*" when allowed != :any ->
        {:error, :wildcard_origin_not_allowed}

      origin ->
        if origin == state.origin || origin in (allowed || []) do
          :ok
        else
          {:error, {:origin_not_allowed, origin}}
        end
    end
  end

  defp validate_cors_response(_headers, _state), do: :ok

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
