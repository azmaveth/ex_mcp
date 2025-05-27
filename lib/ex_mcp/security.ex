defmodule ExMCP.Security do
  @moduledoc """
  Security configuration and utilities for MCP transports.

  Provides a unified API for configuring authentication, authorization,
  and security headers across all MCP transports.

  ## Security Configuration

  Security can be configured at the transport level:

      # Bearer token authentication
      {:ok, client} = ExMCP.Client.start_link(
        transport: :sse,
        url: "https://api.example.com",
        security: %{
          auth: {:bearer, "your-token-here"},
          validate_origin: true,
          allowed_origins: ["https://trusted.example.com"]
        }
      )

      # API key authentication
      {:ok, client} = ExMCP.Client.start_link(
        transport: :websocket,
        url: "wss://api.example.com",
        security: %{
          auth: {:api_key, "your-api-key", header: "X-API-Key"}
        }
      )

      # Custom headers
      {:ok, client} = ExMCP.Client.start_link(
        transport: :sse,
        url: "https://api.example.com",
        security: %{
          headers: [
            {"X-Custom-Header", "value"},
            {"X-Request-ID", "uuid"}
          ]
        }
      )

  ## Security Features by Transport

  | Feature | SSE | WebSocket | BEAM | stdio |
  |---------|-----|-----------|------|-------|
  | Bearer Auth | ✓ | ✓ | ✓ | - |
  | API Key | ✓ | ✓ | ✓ | - |
  | Custom Headers | ✓ | ✓ | - | - |
  | Origin Validation | ✓ | ✓ | - | - |
  | CORS Headers | ✓ | - | - | - |
  | TLS/SSL | ✓ | ✓ | ✓* | - |
  | Mutual TLS | ✓ | ✓ | - | - |

  *BEAM transport uses Erlang distribution security
  """

  @type auth_method ::
          {:bearer, token :: String.t()}
          | {:api_key, key :: String.t(), opts :: keyword()}
          | {:basic, username :: String.t(), password :: String.t()}
          | {:custom, headers :: [{String.t(), String.t()}]}

  @type security_config :: %{
          optional(:auth) => auth_method(),
          optional(:headers) => [{String.t(), String.t()}],
          optional(:validate_origin) => boolean(),
          optional(:allowed_origins) => [String.t()],
          optional(:cors) => cors_config(),
          optional(:tls) => tls_config()
        }

  @type cors_config :: %{
          optional(:allowed_origins) => [String.t()] | :any,
          optional(:allowed_methods) => [String.t()],
          optional(:allowed_headers) => [String.t()],
          optional(:expose_headers) => [String.t()],
          optional(:max_age) => integer(),
          optional(:allow_credentials) => boolean()
        }

  @type tls_config :: %{
          optional(:verify) => :verify_peer | :verify_none,
          optional(:cacerts) => [binary()],
          optional(:cert) => binary(),
          optional(:key) => binary(),
          optional(:versions) => [atom()],
          optional(:ciphers) => [String.t()]
        }

  @doc """
  Builds authentication headers from security configuration.

  ## Examples

      iex> ExMCP.Security.build_auth_headers(%{auth: {:bearer, "token123"}})
      [{"Authorization", "Bearer token123"}]

      iex> ExMCP.Security.build_auth_headers(%{auth: {:api_key, "key123", header: "X-API-Key"}})
      [{"X-API-Key", "key123"}]
  """
  @spec build_auth_headers(security_config()) :: [{String.t(), String.t()}]
  def build_auth_headers(%{auth: auth_method} = _config) do
    case auth_method do
      {:bearer, token} ->
        [{"Authorization", "Bearer #{token}"}]

      {:api_key, key, opts} ->
        header_name = Keyword.get(opts, :header, "X-API-Key")
        [{header_name, key}]

      {:basic, username, password} ->
        credentials = Base.encode64("#{username}:#{password}")
        [{"Authorization", "Basic #{credentials}"}]

      {:custom, headers} ->
        headers

      _ ->
        []
    end
  end

  def build_auth_headers(_config), do: []

  @doc """
  Builds all security headers including auth and custom headers.
  """
  @spec build_security_headers(security_config()) :: [{String.t(), String.t()}]
  def build_security_headers(config) do
    auth_headers = build_auth_headers(config)
    custom_headers = Map.get(config, :headers, [])

    Enum.uniq_by(auth_headers ++ custom_headers, fn {name, _} -> name end)
  end

  @doc """
  Validates origin header against allowed origins.

  ## Examples

      iex> ExMCP.Security.validate_origin("https://example.com", ["https://example.com"])
      :ok

      iex> ExMCP.Security.validate_origin("https://evil.com", ["https://example.com"])
      {:error, :origin_not_allowed}
  """
  @spec validate_origin(String.t() | nil, [String.t()] | :any) ::
          :ok | {:error, :origin_not_allowed}
  def validate_origin(_origin, :any), do: :ok
  def validate_origin(nil, _allowed), do: {:error, :origin_not_allowed}

  def validate_origin(origin, allowed_origins) when is_list(allowed_origins) do
    if origin in allowed_origins do
      :ok
    else
      {:error, :origin_not_allowed}
    end
  end

  @doc """
  Builds CORS headers based on configuration.
  """
  @spec build_cors_headers(cors_config(), String.t() | nil) :: [{String.t(), String.t()}]
  def build_cors_headers(cors_config, origin \\ nil) do
    []
    |> add_origin_header(cors_config, origin)
    |> add_cors_header(
      cors_config,
      "Access-Control-Allow-Methods",
      :allowed_methods,
      &Enum.join(&1, ", ")
    )
    |> add_cors_header(
      cors_config,
      "Access-Control-Allow-Headers",
      :allowed_headers,
      &Enum.join(&1, ", ")
    )
    |> add_cors_header(
      cors_config,
      "Access-Control-Expose-Headers",
      :expose_headers,
      &Enum.join(&1, ", ")
    )
    |> add_cors_header(cors_config, "Access-Control-Max-Age", :max_age, &to_string/1)
    |> add_credentials_header(cors_config)
    |> Enum.reverse()
  end

  defp add_origin_header(headers, cors_config, origin) do
    case Map.get(cors_config, :allowed_origins, :any) do
      :any ->
        [{"Access-Control-Allow-Origin", "*"} | headers]

      origins when is_list(origins) ->
        if origin && origin in origins do
          [{"Access-Control-Allow-Origin", origin} | headers]
        else
          headers
        end

      _ ->
        headers
    end
  end

  defp add_cors_header(headers, cors_config, header_name, key, formatter)
       when is_map(cors_config) do
    case Map.get(cors_config, key) do
      nil -> headers
      value -> [{header_name, formatter.(value)} | headers]
    end
  end

  defp add_credentials_header(headers, cors_config) do
    if Map.get(cors_config, :allow_credentials, false) do
      [{"Access-Control-Allow-Credentials", "true"} | headers]
    else
      headers
    end
  end

  @doc """
  Builds standard security headers for HTTP-based transports.
  """
  @spec build_standard_security_headers() :: [{String.t(), String.t()}]
  def build_standard_security_headers do
    [
      {"X-Content-Type-Options", "nosniff"},
      {"X-Frame-Options", "DENY"},
      {"X-XSS-Protection", "1; mode=block"},
      {"Strict-Transport-Security", "max-age=31536000; includeSubDomains"}
    ]
  end

  @doc """
  Validates security configuration.
  """
  @spec validate_config(security_config()) :: :ok | {:error, term()}
  def validate_config(config) do
    with :ok <- validate_auth(Map.get(config, :auth)),
         :ok <- validate_cors(Map.get(config, :cors)) do
      validate_tls(Map.get(config, :tls))
    end
  end

  defp validate_auth(nil), do: :ok
  defp validate_auth({:bearer, token}) when is_binary(token), do: :ok
  defp validate_auth({:api_key, key, _opts}) when is_binary(key), do: :ok
  defp validate_auth({:basic, user, pass}) when is_binary(user) and is_binary(pass), do: :ok
  defp validate_auth({:custom, headers}) when is_list(headers), do: :ok
  defp validate_auth({:node_cookie, cookie}) when is_atom(cookie), do: :ok
  defp validate_auth(_), do: {:error, :invalid_auth_config}

  defp validate_cors(nil), do: :ok
  defp validate_cors(%{} = _cors), do: :ok
  defp validate_cors(_), do: {:error, :invalid_cors_config}

  defp validate_tls(nil), do: :ok
  defp validate_tls(%{} = _tls), do: :ok
  defp validate_tls(_), do: {:error, :invalid_tls_config}

  @doc """
  Applies security configuration to transport options.

  This function merges security-specific options into transport configuration.
  """
  @spec apply_security(keyword(), security_config()) :: keyword()
  def apply_security(transport_opts, security_config) do
    headers = build_security_headers(security_config)

    transport_opts
    |> Keyword.update(:headers, headers, &(&1 ++ headers))
    |> maybe_add_tls_options(Map.get(security_config, :tls))
  end

  defp maybe_add_tls_options(opts, nil), do: opts

  defp maybe_add_tls_options(opts, tls_config) do
    ssl_opts =
      [
        verify: Map.get(tls_config, :verify, :verify_peer),
        cacerts: Map.get(tls_config, :cacerts),
        cert: Map.get(tls_config, :cert),
        key: Map.get(tls_config, :key),
        versions: Map.get(tls_config, :versions),
        ciphers: Map.get(tls_config, :ciphers)
      ]
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)

    Keyword.put(opts, :ssl_options, ssl_opts)
  end
end
