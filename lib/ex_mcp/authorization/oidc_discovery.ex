defmodule ExMCP.Authorization.OIDCDiscovery do
  @moduledoc """
  OpenID Connect Discovery support for MCP authorization.

  Implements OIDC Discovery (OpenID Connect Discovery 1.0) which allows
  fetching and parsing `.well-known/openid-configuration` documents.

  This extends the OAuth 2.1 authorization server metadata discovery
  with OIDC-specific fields like `userinfo_endpoint` and
  `id_token_signing_alg_values_supported`.

  Available in protocol version 2025-11-25.
  """

  alias ExMCP.Authorization.AuthorizationServerMetadata

  @type oidc_metadata :: %{String.t() => term()}

  @oidc_well_known_path "/.well-known/openid-configuration"
  @oauth_well_known_path "/.well-known/oauth-authorization-server"

  @doc """
  Discovers authorization server metadata using OIDC Discovery with
  fallback to OAuth 2.0 Authorization Server Metadata (RFC 8414).

  Tries `.well-known/openid-configuration` first, then falls back to
  `.well-known/oauth-authorization-server`.

  ## Parameters
  - `issuer` - The issuer URL to discover metadata for
  - `opts` - Options including `:http_client` for custom HTTP client

  ## Returns
  - `{:ok, metadata}` - Successfully fetched metadata
  - `{:error, reason}` - Failed to fetch metadata
  """
  @spec discover(String.t(), keyword()) :: {:ok, oidc_metadata()} | {:error, term()}
  def discover(issuer, opts \\ []) do
    http_client = Keyword.get(opts, :http_client)

    # Build candidate URLs in priority order:
    # 1. OIDC: {issuer}/.well-known/openid-configuration
    # 2. OAuth (OIDC style): {issuer}/.well-known/oauth-authorization-server
    # 3. OAuth (RFC 8414 style): {base}/.well-known/oauth-authorization-server{path}
    # 4. OIDC (RFC 8414 style): {base}/.well-known/openid-configuration{path}
    urls = build_discovery_urls(issuer)

    try_urls(urls, http_client)
  end

  defp build_discovery_urls(issuer) do
    trimmed = String.trim_trailing(issuer, "/")
    uri = URI.parse(trimmed)
    path = uri.path || ""

    base =
      "#{uri.scheme}://#{uri.host}#{if uri.port, do: ":#{uri.port}", else: ""}"

    oidc_appended = trimmed <> @oidc_well_known_path
    oauth_appended = trimmed <> @oauth_well_known_path

    urls = [oidc_appended, oauth_appended]

    # Add RFC 8414 style if issuer has a path component
    if path != "" and path != "/" do
      oauth_rfc8414 = base <> @oauth_well_known_path <> path
      oidc_rfc8414 = base <> @oidc_well_known_path <> path
      urls ++ [oauth_rfc8414, oidc_rfc8414]
    else
      urls
    end
  end

  defp try_urls([], _http_client), do: {:error, :discovery_failed}

  defp try_urls([url | rest], http_client) do
    case fetch_metadata(url, http_client) do
      {:ok, metadata} -> {:ok, metadata}
      {:error, _} -> try_urls(rest, http_client)
    end
  end

  @doc """
  Validates that the discovered metadata contains required OIDC fields.

  ## Required Fields
  - `issuer` - Must match the expected issuer
  - `authorization_endpoint` - URL of the authorization endpoint
  - `token_endpoint` - URL of the token endpoint

  ## OIDC-specific Fields (optional but recommended)
  - `userinfo_endpoint`
  - `jwks_uri`
  - `id_token_signing_alg_values_supported`
  - `subject_types_supported`
  """
  @spec validate_metadata(oidc_metadata(), String.t()) :: :ok | {:error, term()}
  def validate_metadata(metadata, expected_issuer) do
    with :ok <- validate_issuer(metadata, expected_issuer) do
      validate_required_endpoints(metadata)
    end
  end

  @doc """
  Checks if the metadata is OIDC-compliant (vs plain OAuth 2.0).

  Returns true if the metadata contains OIDC-specific fields.
  """
  @spec oidc_compliant?(oidc_metadata()) :: boolean()
  def oidc_compliant?(metadata) do
    Map.has_key?(metadata, "userinfo_endpoint") or
      Map.has_key?(metadata, "id_token_signing_alg_values_supported") or
      Map.has_key?(metadata, "subject_types_supported")
  end

  @doc """
  Builds local OIDC-compatible metadata from application configuration.

  Extends the base OAuth metadata from `AuthorizationServerMetadata.build_metadata/0`
  with OIDC-specific fields.
  """
  @spec build_metadata() :: oidc_metadata()
  def build_metadata do
    base = AuthorizationServerMetadata.build_metadata()
    config = Application.get_env(:ex_mcp, :oidc_discovery, [])

    oidc_fields =
      [
        :userinfo_endpoint,
        :jwks_uri,
        :id_token_signing_alg_values_supported,
        :subject_types_supported,
        :claims_supported,
        :scopes_supported
      ]
      |> Enum.map(fn field ->
        case Keyword.get(config, field) do
          nil -> nil
          value -> {to_string(field), value}
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Map.new()

    Map.merge(base, oidc_fields)
  end

  # Private helpers

  defp fetch_metadata(url, nil) do
    # No HTTP client provided — use :httpc directly
    case :httpc.request(:get, {String.to_charlist(url), []}, [{:timeout, 10_000}], []) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        body_str = if is_list(body), do: List.to_string(body), else: body

        case Jason.decode(body_str) do
          {:ok, metadata} when is_map(metadata) -> {:ok, metadata}
          _ -> {:error, :invalid_json}
        end

      {:ok, {{_, status, _}, _headers, _body}} ->
        {:error, {:http_error, status}}

      {:error, reason} ->
        {:error, {:request_failed, reason}}
    end
  end

  defp fetch_metadata(url, http_client) do
    case http_client.get(url) do
      {:ok, %{status: 200, body: body}} when is_binary(body) ->
        case Jason.decode(body) do
          {:ok, metadata} when is_map(metadata) -> {:ok, metadata}
          _ -> {:error, :invalid_json}
        end

      {:ok, %{status: status}} ->
        {:error, {:http_error, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp validate_issuer(metadata, expected_issuer) do
    case Map.get(metadata, "issuer") do
      ^expected_issuer -> :ok
      nil -> {:error, :missing_issuer}
      actual -> {:error, {:issuer_mismatch, expected: expected_issuer, actual: actual}}
    end
  end

  defp validate_required_endpoints(metadata) do
    required = ["authorization_endpoint", "token_endpoint"]

    missing = Enum.reject(required, &Map.has_key?(metadata, &1))

    case missing do
      [] -> :ok
      [field | _] -> {:error, {:missing_required_field, field}}
    end
  end
end
