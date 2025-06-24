defmodule ExMCP.Security.Validation do
  @moduledoc """
  Handles request and response validation, including transport layer security configurations.

  This module is responsible for various validation tasks to ensure that
  incoming requests and security configurations meet the required policies.
  """

  @doc """
  Validates that a server binding is localhost-only for security.

  ## Examples

      iex> ExMCP.Security.Validation.validate_localhost_binding(%{binding: "127.0.0.1"})
      :ok

      iex> ExMCP.Security.Validation.validate_localhost_binding(%{binding: "localhost"})
      :ok

      iex> ExMCP.Security.Validation.validate_localhost_binding(%{binding: "0.0.0.0"})
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

      iex> ExMCP.Security.Validation.validate_origin("https://example.com", ["https://example.com"])
      :ok

      iex> ExMCP.Security.Validation.validate_origin("https://evil.com", ["https://example.com"])
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
  Validates an HTTP request for security compliance.

  This function implements comprehensive security validation including:
  - Origin header validation (DNS rebinding protection)
  - Required security headers validation
  - HTTPS enforcement for non-localhost

  ## Examples

      headers = [{"origin", "https://example.com"}, {"host", "api.example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      :ok = ExMCP.Security.Validation.validate_request(headers, config)
  """
  @spec validate_request([{String.t(), String.t()}], ExMCP.Internal.Security.security_config()) ::
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

      :ok = ExMCP.Security.Validation.validate_tls_config(config)
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
  @spec validate_config(ExMCP.Internal.Security.security_config()) :: :ok | {:error, term()}
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

  # MCP Specification Security Requirements

  @doc """
  Validates that security configuration meets MCP specification requirements.
  """
  @spec validate_security_requirements(ExMCP.Internal.Security.security_config()) ::
          :ok | {:error, term()}
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
end
