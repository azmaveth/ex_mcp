defmodule ExMCP.Transport.HTTP do
  @moduledoc """
  @mcp_spec

  Streamable HTTP transport for MCP with enhanced SSE support.

  This transport uses HTTP POST and GET requests with Server-Sent Events (SSE)
  for streaming server-to-client messages. This is one of the two
  official MCP transports defined in the specification.

  ## Features

  - **Auto-reconnection**: Automatic reconnection with exponential backoff
  - **Keep-alive**: Built-in heartbeat mechanism for connection health
  - **Event resumption**: Supports Last-Event-ID for event replay
  - **Configurable endpoint**: Customize the MCP endpoint path

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

  > #### Security Best Practices {: .warning}
  > 
  > 1. **Always use HTTPS** in production
  > 2. **Enable origin validation** to prevent DNS rebinding attacks
  > 3. **Bind to localhost** when possible for local servers
  > 4. **Implement proper authentication** (bearer tokens, API keys, etc.)
  > 5. **Set restrictive CORS policies** for cross-origin requests
  """

  @behaviour ExMCP.Transport

  alias ExMCP.Security
  alias ExMCP.Transport.SSEClient

  defstruct [
    :base_url,
    :headers,
    :http_client,
    :sse_pid,
    :endpoint,
    :security,
    :origin
  ]

  @type t :: %__MODULE__{
          base_url: String.t(),
          headers: [{String.t(), String.t()}],
          http_client: module(),
          sse_pid: pid() | nil,
          endpoint: String.t(),
          security: Security.security_config() | nil,
          origin: String.t() | nil
        }

  @default_endpoint "/mcp/v1"

  @impl true
  def connect(config) do
    base_url = Keyword.fetch!(config, :url)
    headers = Keyword.get(config, :headers, [])
    http_client = Keyword.get(config, :http_client, :httpc)
    endpoint = Keyword.get(config, :endpoint, @default_endpoint)
    security = Keyword.get(config, :security)

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

    state = %__MODULE__{
      base_url: base_url,
      headers: all_headers,
      http_client: http_client,
      endpoint: endpoint,
      security: security,
      origin: origin
    }

    # Validate security configuration
    with :ok <- validate_security(state),
         {:ok, sse_pid} <- start_sse(state) do
      {:ok, %{state | sse_pid: sse_pid}}
    end
  end

  @impl true
  def send_message(message, %__MODULE__{} = state) do
    url = build_url(state, "/messages")

    # Build headers with security
    headers = build_request_headers(state)

    case Jason.encode(message) do
      {:ok, body} ->
        request = {
          String.to_charlist(url),
          Enum.map(headers, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end),
          String.to_charlist("application/json"),
          body
        }

        # Add SSL options if using HTTPS
        http_opts =
          case URI.parse(url).scheme do
            "https" -> build_ssl_options(state)
            _ -> []
          end

        case :httpc.request(:post, request, http_opts, []) do
          {:ok, {{_, 200, _}, response_headers, _response_body}} ->
            # Validate CORS if configured
            case validate_cors_response(response_headers, state) do
              :ok -> {:ok, state}
              error -> error
            end

          {:ok, {{_, status, _}, _, body}} ->
            {:error, {:http_error, status, body}}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, {:encoding_error, reason}}
    end
  end

  @impl true
  def receive_message(%__MODULE__{sse_pid: sse_pid} = state) do
    receive do
      {:sse_event, ^sse_pid, %{data: data}} ->
        # The enhanced SSE client sends structured events
        case Jason.decode(data) do
          {:ok, message} ->
            {:ok, message, state}

          {:error, reason} ->
            {:error, {:json_decode_error, reason}}
        end

      {:sse_error, ^sse_pid, reason} ->
        {:error, {:sse_error, reason}}

      {:sse_closed, ^sse_pid} ->
        {:error, :connection_closed}
    end
  end

  @impl true
  def close(%__MODULE__{sse_pid: sse_pid}) when is_pid(sse_pid) do
    Process.exit(sse_pid, :normal)
    :ok
  end

  def close(%__MODULE__{}), do: :ok

  # Private functions

  defp start_sse(state) do
    url = build_url(state, "/sse")
    ssl_opts = build_ssl_options(state)

    # Use the enhanced SSE client with keep-alive and reconnection
    opts = [
      url: url,
      headers: state.headers,
      ssl_opts: ssl_opts,
      parent: self()
    ]

    case SSEClient.start_link(opts) do
      {:ok, sse_pid} ->
        # Wait for connection confirmation
        receive do
          {:sse_connected, ^sse_pid} ->
            {:ok, sse_pid}

          {:sse_error, ^sse_pid, reason} ->
            {:error, reason}
        after
          10_000 ->
            Process.exit(sse_pid, :kill)
            {:error, :connection_timeout}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_url(%__MODULE__{base_url: base_url, endpoint: endpoint}, path) do
    base_url
    |> URI.parse()
    |> Map.put(:path, endpoint <> path)
    |> URI.to_string()
  end

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

  defp build_request_headers(%{headers: headers, security: security, origin: origin}) do
    base_headers = [{"content-type", "application/json"} | headers]

    # Add Origin header if we have one
    headers_with_origin =
      if origin do
        [{"Origin", origin} | base_headers]
      else
        base_headers
      end

    # Add security headers if configured
    if security && Map.get(security, :include_security_headers, false) do
      headers_with_origin ++ Security.build_standard_security_headers()
    else
      headers_with_origin
    end
  end

  defp build_ssl_options(%{security: %{tls: tls_config}}) when is_map(tls_config) do
    ssl_opts = [
      ssl: [
        verify: Map.get(tls_config, :verify, :verify_peer),
        cacerts: Map.get(tls_config, :cacerts, :public_key.cacerts_get()),
        versions: Map.get(tls_config, :versions, [:"tlsv1.2", :"tlsv1.3"])
      ]
    ]

    # Add client cert if provided
    ssl_opts =
      case Map.get(tls_config, :cert) do
        nil -> ssl_opts
        cert -> put_in(ssl_opts, [:ssl, :cert], cert)
      end

    case Map.get(tls_config, :key) do
      nil -> ssl_opts
      key -> put_in(ssl_opts, [:ssl, :key], key)
    end
  end

  defp build_ssl_options(_state) do
    # Default SSL options
    [
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    ]
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
