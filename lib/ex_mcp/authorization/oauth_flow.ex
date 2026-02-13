defmodule ExMCP.Authorization.OAuthFlow do
  @moduledoc """
  OAuth 2.1 flow implementations for MCP authorization.

  This module handles the core OAuth flows:
  - Authorization Code Flow with PKCE
  - Client Credentials Flow
  - Token refresh flow
  """

  alias ExMCP.Authorization.{ClientAssertion, HTTPClient, PKCE, Validator}

  @type auth_params :: %{
          optional(:state) => String.t(),
          optional(:resource) => String.t() | [String.t()],
          client_id: String.t(),
          redirect_uri: String.t(),
          authorization_endpoint: String.t(),
          scopes: [String.t()]
        }

  @type token_params :: %{
          optional(:client_secret) => String.t(),
          optional(:resource) => String.t() | [String.t()],
          code: String.t(),
          code_verifier: String.t(),
          client_id: String.t(),
          redirect_uri: String.t(),
          token_endpoint: String.t()
        }

  @type client_credentials_params :: %{
          optional(:scopes) => [String.t()],
          optional(:resource) => String.t() | [String.t()],
          client_id: String.t(),
          client_secret: String.t(),
          token_endpoint: String.t()
        }

  @type token_response :: %{
          access_token: String.t(),
          token_type: String.t(),
          expires_in: non_neg_integer() | nil,
          refresh_token: String.t() | nil,
          scope: String.t() | nil
        }

  @doc """
  Starts the OAuth 2.1 authorization code flow with PKCE.

  ## Example

      {:ok, auth_url, state} = OAuthFlow.start_authorization_flow(%{
        client_id: "my-client",
        redirect_uri: "http://localhost:8080/callback",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        scopes: ["mcp:read", "mcp:write"]
      })
  """
  @spec start_authorization_flow(auth_params()) ::
          {:ok, String.t(), map()} | {:error, term()}
  def start_authorization_flow(
        %{
          client_id: _client_id,
          redirect_uri: redirect_uri,
          authorization_endpoint: auth_endpoint,
          scopes: _scopes
        } = params
      ) do
    with :ok <- Validator.validate_redirect_uri(redirect_uri),
         :ok <- Validator.validate_https_endpoint(auth_endpoint),
         :ok <- Validator.validate_resource_parameters(params) do
      # Generate PKCE challenge
      code_verifier = PKCE.generate_code_verifier()
      code_challenge = PKCE.generate_code_challenge(code_verifier)

      # Generate state for CSRF protection
      state = Map.get(params, :state, generate_state())

      # Build authorization URL
      query_params = build_auth_query_params(params, code_challenge, state)
      auth_url = build_url(auth_endpoint, query_params)

      state_data = %{
        code_verifier: code_verifier,
        state: state,
        redirect_uri: redirect_uri,
        initiated_at: DateTime.utc_now()
      }

      {:ok, auth_url, state_data}
    end
  end

  @doc """
  Exchanges an authorization code for tokens.
  """
  @spec exchange_code_for_token(token_params()) ::
          {:ok, token_response()} | {:error, term()}
  def exchange_code_for_token(
        %{
          code: _code,
          code_verifier: _code_verifier,
          client_id: _client_id,
          redirect_uri: _redirect_uri,
          token_endpoint: token_endpoint
        } = params
      ) do
    with :ok <- Validator.validate_https_endpoint(token_endpoint),
         :ok <- Validator.validate_resource_parameters(params) do
      request_body = build_token_request_body(params)
      HTTPClient.make_token_request(token_endpoint, request_body)
    end
  end

  @doc """
  Performs OAuth 2.1 client credentials flow.
  """
  @spec client_credentials_flow(client_credentials_params()) ::
          {:ok, token_response()} | {:error, term()}
  def client_credentials_flow(
        %{
          client_id: client_id,
          client_secret: client_secret,
          token_endpoint: token_endpoint
        } = params
      ) do
    with :ok <- Validator.validate_https_endpoint(token_endpoint),
         :ok <- Validator.validate_resource_parameters(params) do
      request_body = %{
        grant_type: "client_credentials",
        client_id: client_id,
        client_secret: client_secret
      }

      # Add optional parameters
      request_body =
        request_body
        |> maybe_add_scopes(params)
        |> Map.to_list()
        |> maybe_add_resource_params(params)

      HTTPClient.make_token_request(token_endpoint, request_body)
    end
  end

  @type jwt_credentials_params :: %{
          optional(:scopes) => [String.t()],
          optional(:resource) => String.t() | [String.t()],
          optional(:alg) => String.t(),
          optional(:kid) => String.t(),
          client_id: String.t(),
          private_key: JOSE.JWK.t(),
          token_endpoint: String.t()
        }

  @doc """
  Performs OAuth 2.1 client credentials flow with JWT client authentication (private_key_jwt).

  Uses RFC 7523 Section 2.2 client assertions instead of a client secret.
  """
  @spec client_credentials_jwt_flow(jwt_credentials_params()) ::
          {:ok, token_response()} | {:error, term()}
  def client_credentials_jwt_flow(
        %{
          client_id: client_id,
          private_key: private_key,
          token_endpoint: token_endpoint
        } = params
      ) do
    with :ok <- Validator.validate_https_endpoint(token_endpoint),
         :ok <- Validator.validate_resource_parameters(params),
         {:ok, assertion_params} <-
           ClientAssertion.build_assertion_params(
             client_id: client_id,
             token_endpoint: token_endpoint,
             private_key: private_key,
             alg: Map.get(params, :alg, "RS256"),
             kid: Map.get(params, :kid)
           ) do
      request_body =
        [{"grant_type", "client_credentials"}] ++
          assertion_params

      # Add optional parameters
      request_body =
        request_body
        |> maybe_add_scope_param(params)
        |> maybe_add_resource_params(params)

      HTTPClient.make_token_request(token_endpoint, request_body)
    end
  end

  @doc """
  Refreshes an access token using a refresh token.

  ## Options

  - `client_secret` - Client secret for confidential clients (default: nil)
  - `scope` - Space-separated scope string to request expanded scopes during refresh.
    Used for incremental scope upgrades (2025-11-25). If the authorization server
    supports it, the new token will have the expanded scope set.
  """
  @spec refresh_token(String.t(), String.t(), String.t(), keyword() | String.t() | nil) ::
          {:ok, token_response()} | {:error, term()}
  def refresh_token(refresh_token, client_id, token_endpoint, opts \\ nil)

  # Backwards-compatible: opts is a string (client_secret) or nil
  def refresh_token(refresh_token, client_id, token_endpoint, client_secret)
      when is_binary(client_secret) or is_nil(client_secret) do
    refresh_token(refresh_token, client_id, token_endpoint, client_secret: client_secret)
  end

  # New: opts is a keyword list
  def refresh_token(refresh_token, client_id, token_endpoint, opts) when is_list(opts) do
    client_secret = Keyword.get(opts, :client_secret)
    scope = Keyword.get(opts, :scope)

    with :ok <- Validator.validate_https_endpoint(token_endpoint) do
      request_body = %{
        grant_type: "refresh_token",
        refresh_token: refresh_token,
        client_id: client_id
      }

      request_body =
        if client_secret do
          Map.put(request_body, :client_secret, client_secret)
        else
          request_body
        end

      request_body =
        if scope do
          Map.put(request_body, :scope, scope)
        else
          request_body
        end

      HTTPClient.make_token_request(token_endpoint, Map.to_list(request_body))
    end
  end

  @doc """
  Initiates a full re-authorization flow with an expanded scope set.

  Used when a refresh token is not available or the server does not support
  scope upgrades via refresh. This starts a new authorization code flow
  with the combined current + additional scopes.
  """
  @spec reauthorize_with_scopes(auth_params(), [String.t()]) ::
          {:ok, String.t(), map()} | {:error, term()}
  def reauthorize_with_scopes(params, additional_scopes) do
    current_scopes = Map.get(params, :scopes, [])
    combined_scopes = Enum.uniq(current_scopes ++ additional_scopes)
    start_authorization_flow(%{params | scopes: combined_scopes})
  end

  # Private helpers

  defp build_auth_query_params(params, code_challenge, state) do
    base_params = [
      {"response_type", "code"},
      {"client_id", params.client_id},
      {"redirect_uri", params.redirect_uri},
      {"code_challenge", code_challenge},
      {"code_challenge_method", "S256"},
      {"state", state}
    ]

    # Add scope if present
    base_params =
      if params[:scopes] && length(params.scopes) > 0 do
        base_params ++ [{"scope", Enum.join(params.scopes, " ")}]
      else
        base_params
      end

    # Add resource parameters
    maybe_add_resource_params(base_params, params)
  end

  defp build_token_request_body(params) do
    base_body = %{
      grant_type: "authorization_code",
      code: params.code,
      redirect_uri: params.redirect_uri,
      client_id: params.client_id,
      code_verifier: params.code_verifier
    }

    # Add client secret for confidential clients
    base_body =
      if params[:client_secret] do
        Map.put(base_body, :client_secret, params.client_secret)
      else
        base_body
      end

    base_body
    |> Map.to_list()
    |> maybe_add_resource_params(params)
  end

  defp maybe_add_scopes(request_body, %{scopes: scopes}) when is_list(scopes) do
    Map.put(request_body, :scope, Enum.join(scopes, " "))
  end

  defp maybe_add_scopes(request_body, _), do: request_body

  defp maybe_add_scope_param(params_list, %{scopes: scopes})
       when is_list(scopes) and scopes != [] do
    params_list ++ [{"scope", Enum.join(scopes, " ")}]
  end

  defp maybe_add_scope_param(params_list, _), do: params_list

  defp maybe_add_resource_params(params_list, %{resource: resource}) when is_binary(resource) do
    params_list ++ [{"resource", resource}]
  end

  defp maybe_add_resource_params(params_list, %{resource: resources}) when is_list(resources) do
    params_list ++ Enum.map(resources, &{"resource", &1})
  end

  defp maybe_add_resource_params(params_list, _), do: params_list

  defp generate_state do
    :crypto.strong_rand_bytes(32) |> Base.url_encode64(padding: false)
  end

  defp build_url(base_url, query_params) do
    query_string = URI.encode_query(query_params)
    "#{base_url}?#{query_string}"
  end
end
