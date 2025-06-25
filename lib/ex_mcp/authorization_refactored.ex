defmodule ExMCP.AuthorizationRefactored do
  @moduledoc """
  Refactored MCP Authorization support for OAuth 2.1 with PKCE.

  This is a cleaned-up version of the Authorization module that delegates
  to focused, single-responsibility modules:

  - `ExMCP.Authorization.OAuthFlow` - OAuth flow implementations
  - `ExMCP.Authorization.PKCE` - PKCE security implementation
  - `ExMCP.Authorization.HTTPClient` - HTTP communication
  - `ExMCP.Authorization.Validator` - Parameter validation

  This module serves as a facade, maintaining the same public API while
  leveraging the decomposed architecture for better maintainability.
  """

  alias ExMCP.Authorization.{HTTPClient, OAuthFlow, PKCE, Validator}

  @type authorization_config :: %{
          client_id: String.t(),
          client_secret: String.t() | nil,
          authorization_endpoint: String.t(),
          token_endpoint: String.t(),
          redirect_uri: String.t(),
          scopes: [String.t()],
          additional_params: map() | nil,
          resource: String.t() | [String.t()] | nil
        }

  @type token_response :: %{
          access_token: String.t(),
          token_type: String.t(),
          expires_in: integer() | nil,
          refresh_token: String.t() | nil,
          scope: String.t() | nil
        }

  @type server_metadata :: %{
          authorization_endpoint: String.t(),
          token_endpoint: String.t(),
          registration_endpoint: String.t() | nil,
          scopes_supported: [String.t()],
          response_types_supported: [String.t()],
          grant_types_supported: [String.t()],
          code_challenge_methods_supported: [String.t()]
        }

  @doc """
  Starts an OAuth 2.1 authorization code flow with PKCE.

  Delegates to `ExMCP.Authorization.OAuthFlow.start_authorization_flow/1`
  with the same interface and behavior.
  """
  @spec start_authorization_flow(authorization_config()) ::
          {:ok, String.t(), map()} | {:error, term()}
  def start_authorization_flow(config) do
    # Convert to the format expected by OAuthFlow
    auth_params = %{
      client_id: config.client_id,
      redirect_uri: config.redirect_uri,
      authorization_endpoint: config.authorization_endpoint,
      scopes: config.scopes
    }

    auth_params =
      auth_params
      |> maybe_add_resource(config)
      |> maybe_add_state(config)

    OAuthFlow.start_authorization_flow(auth_params)
  end

  @doc """
  Exchanges an authorization code for an access token using PKCE.

  Delegates to `ExMCP.Authorization.OAuthFlow.exchange_code_for_token/1`
  """
  @spec exchange_code_for_token(map()) :: {:ok, token_response()} | {:error, term()}
  def exchange_code_for_token(params) do
    # Convert to the format expected by OAuthFlow
    token_params = %{
      code: params.code,
      code_verifier: params.code_verifier,
      client_id: params.client_id,
      redirect_uri: params.redirect_uri,
      token_endpoint: params.token_endpoint
    }

    token_params =
      token_params
      |> maybe_add_client_secret(params)
      |> maybe_add_resource(params)

    OAuthFlow.exchange_code_for_token(token_params)
  end

  @doc """
  Performs OAuth 2.1 client credentials flow.

  Delegates to `ExMCP.Authorization.OAuthFlow.client_credentials_flow/1`
  """
  @spec client_credentials_flow(map()) :: {:ok, token_response()} | {:error, term()}
  def client_credentials_flow(params) do
    # Convert to the format expected by OAuthFlow
    cred_params = %{
      client_id: params.client_id,
      client_secret: params.client_secret,
      token_endpoint: params.token_endpoint
    }

    cred_params =
      cred_params
      |> maybe_add_scopes(params)
      |> maybe_add_resource(params)

    OAuthFlow.client_credentials_flow(cred_params)
  end

  @doc """
  Refreshes an access token using a refresh token.

  Delegates to `ExMCP.Authorization.OAuthFlow.refresh_token/4`
  """
  @spec refresh_token(String.t(), String.t(), String.t(), String.t() | nil) ::
          {:ok, token_response()} | {:error, term()}
  def refresh_token(refresh_token, client_id, token_endpoint, client_secret \\ nil) do
    OAuthFlow.refresh_token(refresh_token, client_id, token_endpoint, client_secret)
  end

  @doc """
  Discovers server metadata from the authorization server.

  Uses HTTPClient for the actual HTTP request and metadata parsing.
  """
  @spec discover_server_metadata(String.t()) :: {:ok, server_metadata()} | {:error, term()}
  def discover_server_metadata(issuer_url) do
    with :ok <- Validator.validate_https_endpoint(issuer_url) do
      metadata_url = build_metadata_url(issuer_url)
      HTTPClient.fetch_server_metadata(metadata_url)
    end
  end

  @doc """
  Validates an access token with the authorization server.

  Uses HTTPClient for the introspection request.
  """
  @spec validate_token(String.t(), String.t()) :: {:ok, map()} | {:error, term()}
  def validate_token(token, introspection_endpoint) do
    with :ok <- Validator.validate_https_endpoint(introspection_endpoint) do
      request_body = %{
        token: token,
        token_type_hint: "access_token"
      }

      case HTTPClient.make_introspection_request(introspection_endpoint, request_body) do
        {:ok, %{active: true} = response} ->
          {:ok, response}

        {:ok, %{active: false}} ->
          {:error, :token_inactive}

        error ->
          error
      end
    end
  end

  @doc """
  Generates PKCE code challenge parameters.

  Delegates to the PKCE module.
  """
  @spec generate_pkce_challenge() :: {:ok, String.t(), String.t()} | {:error, term()}
  def generate_pkce_challenge do
    code_verifier = PKCE.generate_code_verifier()
    code_challenge = PKCE.generate_code_challenge(code_verifier)
    {:ok, code_verifier, code_challenge}
  end

  @doc """
  Verifies a PKCE code challenge.

  Delegates to the PKCE module.
  """
  @spec verify_pkce_challenge(String.t(), String.t()) :: :ok | {:error, term()}
  def verify_pkce_challenge(code_verifier, expected_challenge) do
    if PKCE.validate_challenge(code_verifier, expected_challenge) do
      :ok
    else
      {:error, :invalid_pkce_challenge}
    end
  end

  @doc """
  Makes a token request to the authorization server.

  Used internally by TokenManager for refresh operations.
  Delegates to HTTPClient for the actual request.
  """
  @spec token_request(map()) :: {:ok, map()} | {:error, any()}
  def token_request(config) do
    endpoint = config[:token_endpoint] || raise "Missing token_endpoint"

    with :ok <- Validator.validate_resource_parameters(config) do
      # Build request body based on grant type
      body = build_refresh_token_request_body(config)
      HTTPClient.make_token_request(endpoint, body)
    end
  end

  # Private helper functions

  defp build_refresh_token_request_body(config) do
    base_params = [
      client_id: config[:client_id]
    ]

    # Add grant-specific parameters
    grant_params =
      cond do
        config[:refresh_token] ->
          params = [
            grant_type: "refresh_token",
            refresh_token: config[:refresh_token]
          ]

          # Add client_secret if provided (for confidential clients)
          if config[:client_secret] do
            params ++ [client_secret: config[:client_secret]]
          else
            params
          end

        config[:code] ->
          [
            grant_type: "authorization_code",
            code: config[:code],
            redirect_uri: config[:redirect_uri],
            code_verifier: config[:code_verifier]
          ]

        config[:client_secret] ->
          [
            grant_type: "client_credentials",
            client_secret: config[:client_secret],
            scope: config[:scope] || ""
          ]

        true ->
          raise "Invalid token request configuration"
      end

    resource_params = build_resource_params(config)
    base_params ++ grant_params ++ resource_params
  end

  defp build_metadata_url(issuer_url) do
    issuer_url
    |> String.trim_trailing("/")
    |> Kernel.<>("/.well-known/oauth-authorization-server")
  end

  defp build_resource_params(config) do
    case Map.get(config, :resource) do
      nil ->
        []

      uri when is_binary(uri) ->
        [resource: uri]

      uris when is_list(uris) ->
        Enum.map(uris, fn uri -> {:resource, uri} end)
    end
  end

  defp maybe_add_resource(params, config) do
    case Map.get(config, :resource) do
      nil -> params
      resource -> Map.put(params, :resource, resource)
    end
  end

  defp maybe_add_state(params, config) do
    case Map.get(config, :additional_params) do
      %{state: state} -> Map.put(params, :state, state)
      _ -> params
    end
  end

  defp maybe_add_client_secret(params, config) do
    case Map.get(config, :client_secret) do
      nil -> params
      secret -> Map.put(params, :client_secret, secret)
    end
  end

  defp maybe_add_scopes(params, config) do
    case Map.get(config, :scopes) do
      nil -> params
      scopes -> Map.put(params, :scopes, scopes)
    end
  end
end
