defmodule ExMCP.Security do
  @moduledoc """
  Security configuration and utilities for MCP transports.

  Provides a unified API for configuring authentication, authorization,
  and security headers across all MCP transports.

  ## Security Configuration

  Security can be configured at the transport level:

      # Bearer token authentication
      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        security: %{
          auth: {:bearer, "your-token-here"},
          validate_origin: true,
          allowed_origins: ["https://trusted.example.com"]
        }
      )

      # API key authentication
      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        security: %{
          auth: {:api_key, "your-api-key", header: "X-API-Key"}
        }
      )

      # OAuth 2.1 authentication
      {:ok, token_response} = ExMCP.Authorization.client_credentials_flow(%{
        client_id: "my-client",
        client_secret: "my-secret",
        token_endpoint: "https://auth.example.com/token"
      })
      
      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        security: %{
          auth: {:oauth2, token_response}
        }
      )

      # Custom headers
      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        security: %{
          headers: [
            {"X-Custom-Header", "value"},
            {"X-Request-ID", "uuid"}
          ]
        }
      )

  ## Security Features by Transport

  | Feature | HTTP | BEAM | stdio |
  |---------|------|------|-------|
  | Bearer Auth | ✓ | ✓ | - |
  | OAuth 2.1 | ✓ | ✓ | - |
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
          | {:oauth2, ExMCP.Authorization.token_response()}

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

      {:oauth2, %{access_token: token, token_type: token_type}} ->
        [{"Authorization", "#{token_type} #{token}"}]

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
  Validates that a server binding is localhost-only for security.

  ## Examples

      iex> ExMCP.Security.validate_localhost_binding(%{binding: "127.0.0.1"})
      :ok
      
      iex> ExMCP.Security.validate_localhost_binding(%{binding: "localhost"})
      :ok
      
      iex> ExMCP.Security.validate_localhost_binding(%{binding: "0.0.0.0"})
      {:error, :public_binding_requires_security}
  """
  @spec validate_localhost_binding(map()) :: :ok | {:error, :public_binding_requires_security}
  def validate_localhost_binding(%{binding: binding}) when is_binary(binding) do
    case binding do
      "127.0.0.1" -> :ok
      "localhost" -> :ok
      "::1" -> :ok
      _ -> {:error, :public_binding_requires_security}
    end
  end

  def validate_localhost_binding(_), do: :ok

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
      {"Strict-Transport-Security", "max-age=31536000; includeSubDomains"},
      {"Referrer-Policy", "strict-origin-when-cross-origin"},
      {"X-Permitted-Cross-Domain-Policies", "none"}
    ]
  end

  @doc """
  Validates an HTTP request for security compliance.

  This function implements comprehensive security validation including:
  - Origin header validation (DNS rebinding protection)
  - Required security headers validation
  - HTTPS enforcement for non-localhost

  ## Examples

      headers = [{"origin", "https://example.com"}, {"host", "api.example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}
      
      :ok = ExMCP.Security.validate_request(headers, config)
  """
  @spec validate_request([{String.t(), String.t()}], security_config()) ::
          :ok | {:error, atom()}
  def validate_request(headers, config \\ %{}) do
    with :ok <- validate_request_origin_header(headers, config),
         :ok <- validate_host_header(headers, config) do
      validate_https_requirement(headers, config)
    end
  end

  defp validate_request_origin_header(headers, %{validate_origin: true} = config) do
    case find_header(headers, "origin") do
      nil ->
        # Origin header is required when origin validation is enabled
        {:error, :origin_header_required}

      origin ->
        allowed = Map.get(config, :allowed_origins, [])
        validate_origin(origin, allowed)
    end
  end

  defp validate_request_origin_header(_headers, _config), do: :ok

  defp validate_host_header(headers, config) do
    case find_header(headers, "host") do
      nil ->
        {:error, :host_header_required}

      host ->
        validate_host_against_policy(host, config)
    end
  end

  defp validate_host_against_policy(host, config) do
    # Basic validation - could be enhanced based on security policy
    if String.contains?(host, ["localhost", "127.0.0.1", "[::1]"]) do
      :ok
    else
      # For non-localhost hosts, ensure they match expected patterns
      allowed_hosts = Map.get(config, :allowed_hosts, [])

      if allowed_hosts == [] or host in allowed_hosts do
        :ok
      else
        {:error, :host_not_allowed}
      end
    end
  end

  defp validate_https_requirement(headers, %{enforce_https: true}) do
    # Check if this is an HTTPS request
    case find_header(headers, "x-forwarded-proto") do
      "https" ->
        :ok

      nil ->
        # If no x-forwarded-proto, check if this is localhost
        case find_header(headers, "host") do
          host when host in ["localhost", "127.0.0.1", "[::1]"] -> :ok
          _ -> {:error, :https_required}
        end

      _ ->
        {:error, :https_required}
    end
  end

  defp validate_https_requirement(_headers, _config), do: :ok

  defp find_header(headers, name) do
    name_lower = String.downcase(name)

    Enum.find_value(headers, fn
      {key, value} when is_binary(key) ->
        if String.downcase(key) == name_lower do
          value
        end

      {key, value} when is_list(key) ->
        if String.downcase(to_string(key)) == name_lower do
          to_string(value)
        end

      _ ->
        nil
    end)
  end

  @doc """
  Validates TLS/SSL configuration.

  ## Examples

      config = %{
        verify: :verify_peer,
        versions: [:"tlsv1.2", :"tlsv1.3"],
        ciphers: ["ECDHE-RSA-AES256-GCM-SHA384"]
      }
      
      :ok = ExMCP.Security.validate_tls_config(config)
  """
  @spec validate_tls_config(map()) :: :ok | {:error, atom()}
  def validate_tls_config(config) when is_map(config) do
    with :ok <- validate_verify_mode(Map.get(config, :verify)),
         :ok <- validate_tls_versions(Map.get(config, :versions)),
         :ok <- validate_cipher_suites(Map.get(config, :ciphers)) do
      validate_certificates(config)
    end
  end

  def validate_tls_config(_), do: {:error, :invalid_tls_config}

  defp validate_verify_mode(nil), do: :ok
  defp validate_verify_mode(:verify_peer), do: :ok
  defp validate_verify_mode(:verify_none), do: :ok
  defp validate_verify_mode(_), do: {:error, :invalid_verify_mode}

  defp validate_tls_versions(nil), do: :ok

  defp validate_tls_versions(versions) when is_list(versions) do
    insecure_versions = [:"tlsv1.0", :"tlsv1.1", :sslv3, :sslv2]

    if Enum.any?(versions, &(&1 in insecure_versions)) do
      {:error, :insecure_tls_versions}
    else
      :ok
    end
  end

  defp validate_tls_versions(_), do: {:error, :invalid_tls_versions}

  @doc """
  Validates cipher suite configuration.
  """
  @spec validate_cipher_suites(map() | nil) :: :ok | {:error, atom()}
  def validate_cipher_suites(nil), do: :ok
  def validate_cipher_suites(%{ciphers: ciphers}), do: validate_cipher_suites(ciphers)

  def validate_cipher_suites(ciphers) when is_list(ciphers) do
    weak_ciphers = [
      # 3DES
      "DES-CBC3-SHA",
      # RC4
      "RC4-SHA",
      # NULL encryption
      "NULL-SHA",
      # Anonymous DH
      "ADH-",
      # Anonymous ECDH
      "AECDH-",
      # MD5 hash
      "MD5"
    ]

    has_weak =
      Enum.any?(ciphers, fn cipher ->
        Enum.any?(weak_ciphers, &String.contains?(cipher, &1))
      end)

    if has_weak do
      {:error, :weak_cipher_suites}
    else
      :ok
    end
  end

  def validate_cipher_suites(_), do: {:error, :invalid_cipher_config}

  defp validate_certificates(config) do
    # Basic validation - could be enhanced
    case {Map.get(config, :cert), Map.get(config, :key)} do
      {nil, nil} -> :ok
      {cert, key} when is_binary(cert) and is_binary(key) -> :ok
      {cert, nil} when is_binary(cert) -> {:error, :cert_without_key}
      {nil, key} when is_binary(key) -> {:error, :key_without_cert}
      _ -> {:error, :invalid_certificate_config}
    end
  end

  @doc """
  Validates mutual TLS configuration.
  """
  @spec validate_mtls_config(map()) :: :ok | {:error, atom()}
  def validate_mtls_config(config) do
    required_fields = [:cert, :key, :cacerts]

    missing_fields = Enum.reject(required_fields, &Map.has_key?(config, &1))

    if missing_fields == [] do
      :ok
    else
      {:error, :incomplete_mtls_config}
    end
  end

  @doc """
  Validates certificate pinning configuration.
  """
  @spec validate_certificate_pinning_config(map()) :: :ok | {:error, atom()}
  def validate_certificate_pinning_config(%{certificate_pinning: pins}) when is_list(pins) do
    valid_pins =
      Enum.all?(pins, fn pin ->
        String.starts_with?(pin, "sha256:") and byte_size(pin) > 7
      end)

    if valid_pins do
      :ok
    else
      {:error, :invalid_certificate_pins}
    end
  end

  def validate_certificate_pinning_config(_), do: :ok

  @doc """
  Returns the preferred TLS version from a list.
  """
  @spec preferred_tls_version([atom()]) :: atom()
  def preferred_tls_version(versions) do
    cond do
      :"tlsv1.3" in versions -> :"tlsv1.3"
      :"tlsv1.2" in versions -> :"tlsv1.2"
      true -> hd(versions)
    end
  end

  @doc """
  Returns recommended cipher suites in order of preference.
  """
  @spec recommended_cipher_suites() :: [String.t()]
  def recommended_cipher_suites do
    [
      # TLS 1.3 cipher suites (AEAD only)
      "TLS_AES_256_GCM_SHA384",
      "TLS_AES_128_GCM_SHA256",
      "TLS_CHACHA20_POLY1305_SHA256",

      # TLS 1.2 ECDHE cipher suites (forward secrecy)
      "ECDHE-RSA-AES256-GCM-SHA384",
      "ECDHE-RSA-AES128-GCM-SHA256",
      "ECDHE-RSA-CHACHA20-POLY1305",
      "ECDHE-RSA-AES256-SHA384",
      "ECDHE-RSA-AES128-SHA256"
    ]
  end

  @doc """
  Builds mutual TLS options.
  """
  @spec build_mtls_options(map()) :: keyword()
  def build_mtls_options(config) do
    base_opts = [
      verify: Map.get(config, :verify, :verify_peer),
      cert: Map.get(config, :cert),
      key: Map.get(config, :key),
      cacerts: Map.get(config, :cacerts),
      versions: Map.get(config, :versions, [:"tlsv1.2", :"tlsv1.3"])
    ]

    # Add fail_if_no_peer_cert for server-side mTLS
    if Map.get(config, :fail_if_no_peer_cert) do
      Keyword.put(base_opts, :fail_if_no_peer_cert, true)
    else
      base_opts
    end
  end

  @doc """
  Validates hostname verification function.
  """
  def verify_hostname(_cert, :valid_peer, _hostname) do
    # This would implement proper hostname verification
    # For now, just return valid
    :valid_peer
  end

  def verify_hostname(_cert, {:bad_cert, reason}, _hostname) do
    {:fail, reason}
  end

  def verify_hostname(_cert, {:extension, _ext}, _hostname) do
    :unknown
  end

  @doc """
  Validates transport security configuration.
  """
  @spec validate_transport_security(map()) :: :ok | {:error, atom()}
  def validate_transport_security(config) do
    url = Map.get(config, :url, "")
    security = Map.get(config, :security, %{})

    with :ok <- validate_url_security(url, security) do
      validate_tls_config(Map.get(security, :tls, %{}))
    end
  end

  defp validate_url_security(url, %{enforce_https: true}) do
    enforce_https_requirement(url)
  end

  defp validate_url_security(_url, _security), do: :ok

  @doc """
  Validates security configuration.
  """
  @spec validate_config(security_config()) :: :ok | {:error, term()}
  def validate_config(config) do
    with :ok <- validate_auth(Map.get(config, :auth)),
         :ok <- validate_cors(Map.get(config, :cors)),
         :ok <- validate_tls(Map.get(config, :tls)) do
      validate_security_requirements(config)
    end
  end

  defp validate_auth(nil), do: :ok
  defp validate_auth({:bearer, token}) when is_binary(token), do: :ok
  defp validate_auth({:api_key, key, _opts}) when is_binary(key), do: :ok
  defp validate_auth({:basic, user, pass}) when is_binary(user) and is_binary(pass), do: :ok
  defp validate_auth({:custom, headers}) when is_list(headers), do: :ok
  defp validate_auth({:node_cookie, cookie}) when is_atom(cookie), do: :ok
  defp validate_auth({:oauth2, %{access_token: token}}) when is_binary(token), do: :ok
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

  # MCP Specification Security Requirements

  @doc """
  Validates that security configuration meets MCP specification requirements.
  """
  @spec validate_security_requirements(security_config()) :: :ok | {:error, term()}
  def validate_security_requirements(config) do
    with :ok <- validate_origin_requirements(config) do
      validate_localhost_binding(config)
    end
  end

  defp validate_origin_requirements(%{validate_origin: true, allowed_origins: origins})
       when is_list(origins) and length(origins) > 0 do
    :ok
  end

  defp validate_origin_requirements(%{validate_origin: true}) do
    {:error, :allowed_origins_required_when_origin_validation_enabled}
  end

  defp validate_origin_requirements(_), do: :ok

  @doc """
  Enforces HTTPS requirement for non-localhost URLs.
  """
  @spec enforce_https_requirement(String.t()) :: :ok | {:error, :https_required}
  def enforce_https_requirement(url) do
    uri = URI.parse(url)

    case {uri.scheme, uri.host} do
      {"http", "localhost"} -> :ok
      {"http", "127.0.0.1"} -> :ok
      # IPv6 localhost without brackets
      {"http", "::1"} -> :ok
      # IPv6 localhost with brackets
      {"http", "[::1]"} -> :ok
      {"http", _} -> {:error, :https_required}
      {"https", _} -> :ok
      # Other schemes like ws/wss handled elsewhere
      _ -> :ok
    end
  end

  @doc """
  Validates request origin against security policy.

  This implements DNS rebinding attack protection as required by the MCP spec.
  """
  @spec validate_request_origin(String.t() | nil, security_config()) ::
          :ok | {:error, :origin_validation_failed}
  def validate_request_origin(origin, %{validate_origin: true} = config) do
    allowed = Map.get(config, :allowed_origins, [])
    validate_origin(origin, allowed)
  end

  def validate_request_origin(_, _), do: :ok

  @doc """
  Builds secure default configuration based on deployment context.
  """
  @spec secure_defaults(String.t()) :: security_config()
  def secure_defaults(url) do
    uri = URI.parse(url)

    base_config = %{
      validate_origin: true,
      tls: %{
        verify: :verify_peer,
        versions: [:"tlsv1.2", :"tlsv1.3"]
      }
    }

    # Add localhost-specific relaxations
    if uri.host in ["localhost", "127.0.0.1", "[::1]"] do
      Map.put(base_config, :allowed_origins, ["http://localhost", "https://localhost"])
    else
      base_config
    end
  end
end
