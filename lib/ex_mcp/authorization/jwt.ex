defmodule ExMCP.Authorization.JWT do
  @moduledoc """
  General-purpose JWT module wrapping JOSE for MCP authorization.

  Provides key loading, JWT signing, verification, and claims validation
  used by OAuth client assertions, ID-JAG tokens, and JWT bearer grants.
  """

  @doc """
  Loads a JWK from a PEM string, JWK map, or file path.

  ## Examples

      {:ok, jwk} = JWT.load_key(%{"kty" => "RSA", ...})
      {:ok, jwk} = JWT.load_key("-----BEGIN RSA PRIVATE KEY-----\\n...")
      {:ok, jwk} = JWT.load_key({:pem_file, "/path/to/key.pem"})
  """
  @spec load_key(map() | String.t() | {:pem_file, String.t()}) ::
          {:ok, JOSE.JWK.t()} | {:error, term()}
  def load_key(%{"kty" => _} = jwk_map) do
    {:ok, JOSE.JWK.from_map(jwk_map)}
  rescue
    e -> {:error, {:invalid_jwk, Exception.message(e)}}
  end

  def load_key(%{kty: _} = jwk_map) do
    string_map = Map.new(jwk_map, fn {k, v} -> {to_string(k), v} end)
    load_key(string_map)
  end

  def load_key(pem) when is_binary(pem) do
    if String.starts_with?(String.trim(pem), "-----BEGIN") do
      {:ok, JOSE.JWK.from_pem(pem)}
    else
      {:error, :invalid_key_format}
    end
  rescue
    e -> {:error, {:invalid_pem, Exception.message(e)}}
  end

  def load_key({:pem_file, path}) when is_binary(path) do
    case File.read(path) do
      {:ok, pem} -> load_key(pem)
      {:error, reason} -> {:error, {:file_read_error, reason}}
    end
  end

  def load_key(_), do: {:error, :invalid_key_format}

  @doc """
  Generates an RSA key pair for development/testing.

  ## Options
    - `:size` - Key size in bits (default: 2048)
  """
  @spec generate_rsa_key(keyword()) :: JOSE.JWK.t()
  def generate_rsa_key(opts \\ []) do
    size = Keyword.get(opts, :size, 2048)
    JOSE.JWK.generate_key({:rsa, size})
  end

  @doc """
  Generates an EC key pair for development/testing.

  ## Options
    - `:curve` - EC curve name (default: "P-256")
  """
  @spec generate_ec_key(keyword()) :: JOSE.JWK.t()
  def generate_ec_key(opts \\ []) do
    curve = Keyword.get(opts, :curve, "P-256")
    JOSE.JWK.generate_key({:ec, curve})
  end

  @doc """
  Extracts the public key from a JWK.
  """
  @spec to_public_key(JOSE.JWK.t()) :: JOSE.JWK.t()
  def to_public_key(jwk) do
    JOSE.JWK.to_public(jwk)
  end

  @doc """
  Converts a JWK to a map representation (for JWKS publishing).
  """
  @spec to_map(JOSE.JWK.t()) :: map()
  def to_map(jwk) do
    {_modules, map} = JOSE.JWK.to_map(jwk)
    map
  end

  @doc """
  Signs a claims map into a compact JWS string.

  ## Options
    - `:alg` - Signing algorithm (default: "RS256")
    - `:kid` - Key ID to include in header
    - `:typ` - Token type header (default: "JWT")
  """
  @spec sign(map(), JOSE.JWK.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def sign(claims, jwk, opts \\ []) do
    alg = Keyword.get(opts, :alg, "RS256")
    kid = Keyword.get(opts, :kid)
    typ = Keyword.get(opts, :typ, "JWT")

    header = %{"alg" => alg, "typ" => typ}
    header = if kid, do: Map.put(header, "kid", kid), else: header

    jws = JOSE.JWS.from_map(header)
    payload = Jason.encode!(claims)

    {_modules, signed} = JOSE.JWK.sign(payload, jws, jwk)
    {_modules, compact} = JOSE.JWS.compact(signed)
    {:ok, compact}
  rescue
    e -> {:error, {:signing_error, Exception.message(e)}}
  end

  @doc """
  Verifies a JWS string and returns decoded claims.

  Accepts a single JWK or a list of JWKs (JWKS).
  """
  @spec verify(String.t(), JOSE.JWK.t() | [JOSE.JWK.t()]) ::
          {:ok, map()} | {:error, term()}
  def verify(token, jwk_or_jwks) when is_binary(token) do
    jwks = List.wrap(jwk_or_jwks)

    result =
      Enum.find_value(jwks, {:error, :verification_failed}, fn jwk ->
        case JOSE.JWK.verify_strict(token, allowed_algorithms(), jwk) do
          {true, payload, _jws} ->
            case Jason.decode(payload) do
              {:ok, claims} -> {:ok, claims}
              {:error, _} -> nil
            end

          {false, _payload, _jws} ->
            nil
        end
      end)

    result
  rescue
    e -> {:error, {:verification_error, Exception.message(e)}}
  end

  @doc """
  Verifies a JWS string and validates claims against expected values.

  ## Expected Claims Options
    - `:iss` - Expected issuer
    - `:aud` - Expected audience (string or list)
    - `:sub` - Expected subject
    - `:max_age` - Maximum token age in seconds
    - `:required` - List of required claim keys
  """
  @spec verify_and_validate(String.t(), JOSE.JWK.t() | [JOSE.JWK.t()], keyword()) ::
          {:ok, map()} | {:error, term()}
  def verify_and_validate(token, jwk_or_jwks, expected \\ []) do
    with {:ok, claims} <- verify(token, jwk_or_jwks) do
      validate_claims(claims, expected)
    end
  end

  @doc """
  Validates standard JWT claims against expected values.

  ## Options
    - `:iss` - Expected issuer
    - `:aud` - Expected audience (string or list)
    - `:sub` - Expected subject
    - `:max_age` - Maximum token age in seconds
    - `:required` - List of required claim keys (as strings)
  """
  @spec validate_claims(map(), keyword()) :: {:ok, map()} | {:error, term()}
  def validate_claims(claims, expected \\ []) do
    now = System.system_time(:second)

    validations = [
      fn -> validate_exp(claims, now) end,
      fn -> validate_iat(claims, now) end,
      fn -> validate_nbf(claims, now) end,
      fn -> validate_iss(claims, expected[:iss]) end,
      fn -> validate_aud(claims, expected[:aud]) end,
      fn -> validate_sub(claims, expected[:sub]) end,
      fn -> validate_max_age(claims, now, expected[:max_age]) end,
      fn -> validate_required(claims, expected[:required]) end
    ]

    case Enum.find_value(validations, fn v -> error_or_nil(v.()) end) do
      nil -> {:ok, claims}
      error -> error
    end
  end

  @doc """
  Generates a unique JWT ID (jti).
  """
  @spec generate_jti() :: String.t()
  def generate_jti do
    :crypto.strong_rand_bytes(16) |> Base.url_encode64(padding: false)
  end

  @doc """
  Fetches a JWKS (JSON Web Key Set) from a URL.
  """
  @spec fetch_jwks(String.t()) :: {:ok, [JOSE.JWK.t()]} | {:error, term()}
  def fetch_jwks(url) when is_binary(url) do
    ssl_opts = [
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    ]

    case :httpc.request(:get, {String.to_charlist(url), []}, ssl_opts, []) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        case Jason.decode(to_string(body)) do
          {:ok, %{"keys" => keys}} when is_list(keys) ->
            jwks = Enum.map(keys, &JOSE.JWK.from_map/1)
            {:ok, jwks}

          {:ok, _} ->
            {:error, :invalid_jwks_format}

          {:error, reason} ->
            {:error, {:json_decode_error, reason}}
        end

      {:ok, {{_, status, _}, _headers, _body}} ->
        {:error, {:http_error, status}}

      {:error, reason} ->
        {:error, {:request_failed, reason}}
    end
  end

  @doc """
  Reads the unverified header from a JWS token (for typ checking).
  """
  @spec peek_header(String.t()) :: {:ok, map()} | {:error, term()}
  def peek_header(token) when is_binary(token) do
    case String.split(token, ".") do
      [header_b64 | _] ->
        case Base.url_decode64(header_b64, padding: false) do
          {:ok, header_json} ->
            Jason.decode(header_json)

          :error ->
            {:error, :invalid_token_format}
        end

      _ ->
        {:error, :invalid_token_format}
    end
  end

  # Private helpers

  defp allowed_algorithms do
    ["RS256", "RS384", "RS512", "ES256", "ES384", "ES512", "PS256", "PS384", "PS512"]
  end

  defp error_or_nil(:ok), do: nil
  defp error_or_nil({:error, _} = err), do: err

  defp validate_exp(%{"exp" => exp}, now) when is_number(exp) do
    # Allow 30 seconds of clock skew
    if now <= exp + 30, do: :ok, else: {:error, :token_expired}
  end

  defp validate_exp(_, _), do: :ok

  defp validate_iat(%{"iat" => iat}, now) when is_number(iat) do
    # iat should not be in the future (with 30s skew)
    if iat <= now + 30, do: :ok, else: {:error, :invalid_iat}
  end

  defp validate_iat(_, _), do: :ok

  defp validate_nbf(%{"nbf" => nbf}, now) when is_number(nbf) do
    # Allow 30 seconds of clock skew
    if now >= nbf - 30, do: :ok, else: {:error, :token_not_yet_valid}
  end

  defp validate_nbf(_, _), do: :ok

  defp validate_iss(_, nil), do: :ok

  defp validate_iss(%{"iss" => iss}, expected_iss) do
    if iss == expected_iss, do: :ok, else: {:error, {:invalid_issuer, iss}}
  end

  defp validate_iss(_, _expected), do: {:error, :missing_issuer}

  defp validate_aud(_, nil), do: :ok

  defp validate_aud(%{"aud" => aud}, expected_aud) when is_binary(expected_aud) do
    aud_list = List.wrap(aud)
    if expected_aud in aud_list, do: :ok, else: {:error, {:invalid_audience, aud}}
  end

  defp validate_aud(%{"aud" => aud}, expected_auds) when is_list(expected_auds) do
    aud_list = List.wrap(aud)

    if Enum.any?(expected_auds, &(&1 in aud_list)),
      do: :ok,
      else: {:error, {:invalid_audience, aud}}
  end

  defp validate_aud(_, _expected), do: {:error, :missing_audience}

  defp validate_sub(_, nil), do: :ok

  defp validate_sub(%{"sub" => sub}, expected_sub) do
    if sub == expected_sub, do: :ok, else: {:error, {:invalid_subject, sub}}
  end

  defp validate_sub(_, _expected), do: {:error, :missing_subject}

  defp validate_max_age(_, _, nil), do: :ok

  defp validate_max_age(%{"iat" => iat}, now, max_age) when is_number(iat) do
    if now - iat <= max_age, do: :ok, else: {:error, :token_too_old}
  end

  defp validate_max_age(_, _, _max_age), do: {:error, :missing_iat_for_max_age}

  defp validate_required(_, nil), do: :ok

  defp validate_required(claims, required) when is_list(required) do
    missing = Enum.filter(required, fn key -> not Map.has_key?(claims, key) end)

    if Enum.empty?(missing),
      do: :ok,
      else: {:error, {:missing_required_claims, missing}}
  end
end
