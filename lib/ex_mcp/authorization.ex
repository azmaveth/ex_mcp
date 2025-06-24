defmodule ExMCP.Authorization do
  @moduledoc """
  MCP Authorization support for OAuth 2.1 with PKCE.

  This module provides OAuth 2.1 authorization flows as specified in the MCP
  Authorization specification. It supports both authorization code flow (for user-based
  interactions) and client credentials flow (for application-to-application communication).

  Authorization is **optional** for MCP implementations. When supported:

  - HTTP-based transports should conform to this specification
  - STDIO transports should retrieve credentials from the environment
  - Other transports must follow established security best practices

  ## Security Features

  - **PKCE (Proof Key for Code Exchange)** - Required for all authorization code flows
  - **HTTPS enforcement** - All authorization endpoints must use HTTPS (except localhost)
  - **Secure redirect URI validation** - Prevents open redirect vulnerabilities
  - **Dynamic client registration** - Automatic client ID acquisition
  - **Server metadata discovery** - RFC 8414 OAuth 2.0 Authorization Server Metadata

  ## Quick Start

  ### Authorization Code Flow (User-based)

      # Start authorization flow
      {:ok, auth_url, state} = ExMCP.Authorization.start_authorization_flow(%{
        client_id: "my-client",
        redirect_uri: "http://localhost:8080/callback",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        scopes: ["mcp:read", "mcp:write"]
      })

      # User visits auth_url and authorizes
      # Server redirects to redirect_uri with authorization code

      # Exchange code for token
      {:ok, token_response} = ExMCP.Authorization.exchange_code_for_token(%{
        code: "auth_code_from_callback",
        code_verifier: state.code_verifier,
        client_id: "my-client",
        redirect_uri: "http://localhost:8080/callback",
        token_endpoint: "https://auth.example.com/oauth/token"
      })

  ### Client Credentials Flow (Application-to-application)

      {:ok, token_response} = ExMCP.Authorization.client_credentials_flow(%{
        client_id: "service-client",
        client_secret: "client-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:admin"]
      })

  ### Server Metadata Discovery

      # Discover endpoints automatically
      {:ok, metadata} = ExMCP.Authorization.discover_server_metadata(
        "https://auth.example.com"
      )

  ### Client Configuration

  For automatic authorization handling in MCP clients:

      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        auth_config: %{
          client_id: "my-client",
          client_secret: "secret",  # Optional for public clients
          token_endpoint: "https://auth.example.com/token",
          authorization_endpoint: "https://auth.example.com/authorize",
          redirect_uri: "http://localhost:8080/callback",
          scopes: ["mcp:read", "mcp:write"],
          initial_token: %{
            "access_token" => "...",
            "refresh_token" => "...",
            "expires_in" => 3600
          }
        }
      )

  The client will automatically:
  - Add Authorization headers to all requests
  - Refresh tokens before expiration
  - Handle 401/403 responses appropriately

  ## Grant Types

  OAuth specifies different flows for different use cases:

  1. **Authorization Code** - User-based interactions where the client acts on behalf of a human
     - Example: Agent calls MCP tool implemented by a SaaS system
     - Requires user consent and PKCE for security

  2. **Client Credentials** - Application-to-application communication (no user)
     - Example: Agent calls secure MCP tool to check inventory
     - Uses client ID/secret for authentication

  ## Security Considerations

  - All authorization endpoints **MUST** use HTTPS (except localhost for development)
  - PKCE is **REQUIRED** for all authorization code flows
  - Redirect URIs **MUST** be validated to prevent open redirect attacks
  - Client secrets **MUST** be stored securely
  - Access tokens **MUST NOT** be included in URI query strings
  - Token expiration and rotation **SHOULD** be implemented

  ## Error Handling

  Common error responses:

  - `401 Unauthorized` - Authorization required or token invalid
  - `403 Forbidden` - Invalid scopes or insufficient permissions
  - `400 Bad Request` - Malformed authorization request

  > #### MCP Specification {: .info}
  > This module implements the MCP Authorization specification (2025-06-18 revision).
  > See [MCP Authorization docs](https://modelcontextprotocol.io/docs/specification/server/authorization)
  > for the complete specification.
  """

  alias ExMCP.Internal.Authorization.PKCE

  @type authorization_config :: %{
          client_id: String.t(),
          client_secret: String.t() | nil,
          authorization_endpoint: String.t(),
          token_endpoint: String.t(),
          redirect_uri: String.t(),
          scopes: [String.t()],
          additional_params: map() | nil
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

  This function initiates the authorization code flow for user-based authentication.
  It generates secure PKCE parameters and returns the authorization URL that the
  user should visit to grant consent.

  ## Parameters

  - `config` - Authorization configuration map containing:
    - `:client_id` - OAuth client identifier
    - `:redirect_uri` - Callback URI after authorization
    - `:authorization_endpoint` - Authorization server's authorize endpoint
    - `:scopes` - List of requested scopes
    - `:additional_params` - Optional additional query parameters

  ## Return Value

  Returns `{:ok, authorization_url, state}` where:
  - `authorization_url` - URL for user to visit and authorize
  - `state` - Map containing code_verifier and state parameter (store securely)

  ## Security

  - All endpoints must use HTTPS (except localhost for development)
  - PKCE code verifier is generated securely and must be stored
  - State parameter prevents CSRF attacks

  ## Example

      {:ok, auth_url, state} = ExMCP.Authorization.start_authorization_flow(%{
        client_id: "my-mcp-client",
        redirect_uri: "http://localhost:8080/callback",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        scopes: ["mcp:read", "mcp:write"]
      })

      # Store state securely (contains code_verifier)
      # Redirect user to auth_url
  """
  @spec start_authorization_flow(authorization_config()) ::
          {:ok, String.t(), map()} | {:error, term()}
  def start_authorization_flow(config) do
    with :ok <- validate_https_endpoint(config.authorization_endpoint),
         :ok <- validate_redirect_uri(config.redirect_uri),
         {:ok, code_verifier, code_challenge} <- PKCE.generate_challenge() do
      state = %{
        code_verifier: code_verifier,
        state_param: generate_state()
      }

      params = %{
        response_type: "code",
        client_id: config.client_id,
        redirect_uri: config.redirect_uri,
        scope: Enum.join(config.scopes, " "),
        state: state.state_param,
        code_challenge: code_challenge,
        code_challenge_method: "S256"
      }

      additional_params = Map.get(config, :additional_params, %{})
      all_params = Map.merge(params, additional_params)

      auth_url = build_authorization_url(config.authorization_endpoint, all_params)

      {:ok, auth_url, state}
    end
  end

  @doc """
  Exchanges an authorization code for an access token using PKCE.

  This function completes the authorization code flow by exchanging the
  authorization code received from the callback for an access token.

  ## Parameters

  Map containing:
  - `:code` - Authorization code from callback
  - `:code_verifier` - PKCE code verifier from authorization state
  - `:client_id` - OAuth client identifier
  - `:redirect_uri` - Same redirect URI used in authorization request
  - `:token_endpoint` - Authorization server's token endpoint
  - `:client_secret` - Client secret (optional, for confidential clients)

  ## Return Value

  Returns `{:ok, token_response}` with:
  - `:access_token` - Bearer token for API requests
  - `:token_type` - Usually "Bearer"
  - `:expires_in` - Token lifetime in seconds (optional)
  - `:refresh_token` - Token for refreshing access (optional)
  - `:scope` - Granted scopes (optional)

  ## Example

      {:ok, tokens} = ExMCP.Authorization.exchange_code_for_token(%{
        code: "auth_code_from_callback",
        code_verifier: state.code_verifier,
        client_id: "my-mcp-client",
        redirect_uri: "http://localhost:8080/callback",
        token_endpoint: "https://auth.example.com/oauth/token"
      })

      access_token = tokens.access_token
  """
  @spec exchange_code_for_token(map()) :: {:ok, token_response()} | {:error, term()}
  def exchange_code_for_token(
        %{
          code: code,
          code_verifier: code_verifier,
          client_id: client_id,
          redirect_uri: redirect_uri,
          token_endpoint: token_endpoint
        } = params
      ) do
    with :ok <- validate_https_endpoint(token_endpoint) do
      request_body = %{
        grant_type: "authorization_code",
        code: code,
        redirect_uri: redirect_uri,
        client_id: client_id,
        code_verifier: code_verifier
      }

      # Add client_secret if provided
      request_body =
        case Map.get(params, :client_secret) do
          nil -> request_body
          secret -> Map.put(request_body, :client_secret, secret)
        end

      make_token_request(token_endpoint, request_body)
    end
  end

  @doc """
  Performs OAuth 2.1 client credentials flow.

  This function implements the client credentials grant type for application-to-application
  authentication where no user interaction is required.

  ## Parameters

  Map containing:
  - `:client_id` - OAuth client identifier
  - `:client_secret` - Client secret
  - `:token_endpoint` - Authorization server's token endpoint
  - `:scopes` - List of requested scopes (optional)

  ## Return Value

  Returns `{:ok, token_response}` with access token information.

  ## Example

      {:ok, tokens} = ExMCP.Authorization.client_credentials_flow(%{
        client_id: "service-client",
        client_secret: "client-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:admin"]
      })

      access_token = tokens.access_token
  """
  @spec client_credentials_flow(map()) :: {:ok, token_response()} | {:error, term()}
  def client_credentials_flow(
        %{
          client_id: client_id,
          client_secret: client_secret,
          token_endpoint: token_endpoint
        } = params
      ) do
    with :ok <- validate_https_endpoint(token_endpoint) do
      request_body = %{
        grant_type: "client_credentials",
        client_id: client_id,
        client_secret: client_secret
      }

      # Add scopes if provided
      request_body =
        case Map.get(params, :scopes) do
          nil -> request_body
          scopes -> Map.put(request_body, :scope, Enum.join(scopes, " "))
        end

      make_token_request(token_endpoint, request_body)
    end
  end

  @doc """
  Discovers server metadata from the authorization server.

  This function implements RFC 8414 (OAuth 2.0 Authorization Server Metadata)
  to automatically discover authorization endpoints and capabilities.

  ## Parameters

  - `issuer_url` - Base URL of the authorization server

  ## Return Value

  Returns `{:ok, server_metadata}` with discovered endpoints and capabilities:
  - `:authorization_endpoint` - Authorization endpoint URL
  - `:token_endpoint` - Token endpoint URL
  - `:registration_endpoint` - Dynamic registration endpoint (optional)
  - `:scopes_supported` - Supported scopes
  - `:response_types_supported` - Supported response types
  - `:grant_types_supported` - Supported grant types
  - `:code_challenge_methods_supported` - Supported PKCE methods

  ## Example

      {:ok, metadata} = ExMCP.Authorization.discover_server_metadata(
        "https://auth.example.com"
      )

      auth_endpoint = metadata.authorization_endpoint
      token_endpoint = metadata.token_endpoint
  """
  @spec discover_server_metadata(String.t()) :: {:ok, server_metadata()} | {:error, term()}
  def discover_server_metadata(issuer_url) do
    with :ok <- validate_https_endpoint(issuer_url) do
      metadata_url = build_metadata_url(issuer_url)

      case make_http_request(:get, metadata_url, [], "") do
        {:ok, {{_, 200, _}, _headers, body}} ->
          case Jason.decode(body) do
            {:ok, metadata} ->
              {:ok, parse_server_metadata(metadata)}

            {:error, reason} ->
              {:error, {:json_decode_error, reason}}
          end

        {:ok, {{_, status, _}, _headers, body}} ->
          {:error, {:http_error, status, body}}

        {:error, reason} ->
          {:error, {:request_failed, reason}}
      end
    end
  end

  @doc """
  Validates an access token with the authorization server.

  This function uses OAuth 2.0 token introspection to validate whether
  an access token is still active and get information about it.

  ## Parameters

  - `token` - Access token to validate
  - `introspection_endpoint` - Token introspection endpoint URL

  ## Return Value

  Returns `{:ok, introspection_response}` if token is active, or
  `{:error, :token_inactive}` if token is inactive.

  ## Example

      case ExMCP.Authorization.validate_token(token, introspection_endpoint) do
        {:ok, info} ->
          # Token is valid, info contains claims
          :ok
        {:error, :token_inactive} ->
          # Token expired or revoked
          :error
      end
  """
  @spec validate_token(String.t(), String.t()) :: {:ok, map()} | {:error, term()}
  def validate_token(token, introspection_endpoint) do
    with :ok <- validate_https_endpoint(introspection_endpoint) do
      request_body = %{
        token: token,
        token_type_hint: "access_token"
      }

      case make_introspection_request(introspection_endpoint, request_body) do
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

  This function generates the PKCE (Proof Key for Code Exchange) parameters
  required for secure authorization code flows.

  ## Return Value

  Returns `{:ok, code_verifier, code_challenge}` where:
  - `code_verifier` - Random string stored securely by client
  - `code_challenge` - SHA256 hash of code_verifier for authorization request

  ## Example

      {:ok, verifier, challenge} = ExMCP.Authorization.generate_pkce_challenge()

      # Use challenge in authorization URL
      # Store verifier securely for token exchange
  """
  @spec generate_pkce_challenge() :: {:ok, String.t(), String.t()} | {:error, term()}
  defdelegate generate_pkce_challenge(),
    to: ExMCP.Internal.Authorization.PKCE,
    as: :generate_challenge

  @doc """
  Verifies a PKCE code challenge.

  This function verifies that a code verifier matches the expected code challenge.
  Used by authorization servers to validate PKCE flows.

  ## Parameters

  - `code_verifier` - The code verifier from token exchange
  - `expected_challenge` - The code challenge from authorization request

  ## Return Value

  Returns `:ok` if verification succeeds, `{:error, reason}` otherwise.

  ## Example

      case ExMCP.Authorization.verify_pkce_challenge(verifier, challenge) do
        :ok -> # Valid
        {:error, _reason} -> # Invalid
      end
  """
  @spec verify_pkce_challenge(String.t(), String.t()) :: :ok | {:error, term()}
  defdelegate verify_pkce_challenge(code_verifier, expected_challenge),
    to: ExMCP.Internal.Authorization.PKCE,
    as: :verify_challenge

  @doc """
  Makes a token request to the authorization server.

  Used internally by TokenManager for refresh operations.
  """
  @spec token_request(map()) :: {:ok, map()} | {:error, any()}
  def token_request(config) do
    endpoint = config[:token_endpoint] || raise "Missing token_endpoint"

    # Build request body based on grant type
    body = build_refresh_token_request_body(config)

    # Make the request
    make_token_request(endpoint, body)
  end

  # Private helper functions

  defp build_refresh_token_request_body(config) do
    base_params = %{
      "client_id" => config[:client_id]
    }

    # Add grant-specific parameters
    cond do
      config[:refresh_token] ->
        params =
          Map.merge(base_params, %{
            "grant_type" => "refresh_token",
            "refresh_token" => config[:refresh_token]
          })

        # Add client_secret if provided (for confidential clients)
        if config[:client_secret] do
          Map.put(params, "client_secret", config[:client_secret])
        else
          params
        end

      config[:code] ->
        Map.merge(base_params, %{
          "grant_type" => "authorization_code",
          "code" => config[:code],
          "redirect_uri" => config[:redirect_uri],
          "code_verifier" => config[:code_verifier]
        })

      config[:client_secret] ->
        Map.merge(base_params, %{
          "grant_type" => "client_credentials",
          "client_secret" => config[:client_secret],
          "scope" => config[:scope] || ""
        })

      true ->
        raise "Invalid token request configuration"
    end
  end

  defp validate_https_endpoint(url) do
    case URI.parse(url) do
      %URI{scheme: "https"} -> :ok
      %URI{scheme: "http", host: host} when host in ["localhost", "127.0.0.1"] -> :ok
      _ -> {:error, :https_required}
    end
  end

  defp validate_redirect_uri(uri) do
    case URI.parse(uri) do
      %URI{scheme: "https"} -> :ok
      %URI{scheme: "http", host: host} when host in ["localhost", "127.0.0.1"] -> :ok
      _ -> {:error, :invalid_redirect_uri}
    end
  end

  defp generate_state do
    :crypto.strong_rand_bytes(32) |> Base.url_encode64(padding: false)
  end

  defp build_authorization_url(base_url, params) do
    query_string = URI.encode_query(params)
    "#{base_url}?#{query_string}"
  end

  defp build_metadata_url(issuer_url) do
    issuer_url
    |> String.trim_trailing("/")
    |> Kernel.<>("/.well-known/oauth-authorization-server")
  end

  defp make_introspection_request(endpoint, body) do
    headers = [
      {"content-type", "application/x-www-form-urlencoded"},
      {"accept", "application/json"}
    ]

    encoded_body = URI.encode_query(body)

    case make_http_request(:post, endpoint, headers, encoded_body) do
      {:ok, {{_, 200, _}, _response_headers, response_body}} ->
        case Jason.decode(response_body) do
          {:ok, introspection_data} ->
            {:ok, parse_introspection_response(introspection_data)}

          {:error, reason} ->
            {:error, {:json_decode_error, reason}}
        end

      {:ok, {{_, status, _}, _headers, error_body}} ->
        case Jason.decode(error_body) do
          {:ok, error_data} ->
            {:error, {:oauth_error, status, error_data}}

          {:error, _} ->
            {:error, {:http_error, status, error_body}}
        end

      {:error, reason} ->
        {:error, {:request_failed, reason}}
    end
  end

  defp make_token_request(endpoint, body) do
    headers = [
      {"content-type", "application/x-www-form-urlencoded"},
      {"accept", "application/json"}
    ]

    encoded_body = URI.encode_query(body)

    case make_http_request(:post, endpoint, headers, encoded_body) do
      {:ok, {{_, 200, _}, _response_headers, response_body}} ->
        case Jason.decode(response_body) do
          {:ok, token_data} ->
            {:ok, parse_token_response(token_data)}

          {:error, reason} ->
            {:error, {:json_decode_error, reason}}
        end

      {:ok, {{_, status, _}, _headers, error_body}} ->
        case Jason.decode(error_body) do
          {:ok, error_data} ->
            {:error, {:oauth_error, status, error_data}}

          {:error, _} ->
            {:error, {:http_error, status, error_body}}
        end

      {:error, reason} ->
        {:error, {:request_failed, reason}}
    end
  end

  defp make_http_request(method, url, headers, body) do
    # Convert headers to charlist format for httpc
    httpc_headers =
      Enum.map(headers, fn {k, v} ->
        {String.to_charlist(k), String.to_charlist(v)}
      end)

    request =
      case method do
        :get ->
          {String.to_charlist(url), httpc_headers}

        :post ->
          {String.to_charlist(url), httpc_headers, ~c"application/x-www-form-urlencoded",
           String.to_charlist(body)}
      end

    # SSL options for HTTPS
    ssl_opts = [
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    ]

    :httpc.request(method, request, ssl_opts, [])
  end

  defp parse_token_response(data) do
    %{
      access_token: Map.fetch!(data, "access_token"),
      token_type: Map.get(data, "token_type", "Bearer"),
      expires_in: Map.get(data, "expires_in"),
      refresh_token: Map.get(data, "refresh_token"),
      scope: Map.get(data, "scope")
    }
  end

  defp parse_introspection_response(data) do
    %{
      active: Map.get(data, "active", false),
      scope: Map.get(data, "scope"),
      client_id: Map.get(data, "client_id"),
      username: Map.get(data, "username"),
      token_type: Map.get(data, "token_type"),
      exp: Map.get(data, "exp"),
      iat: Map.get(data, "iat"),
      nbf: Map.get(data, "nbf"),
      sub: Map.get(data, "sub"),
      aud: Map.get(data, "aud"),
      iss: Map.get(data, "iss"),
      jti: Map.get(data, "jti")
    }
  end

  defp parse_server_metadata(data) do
    %{
      authorization_endpoint: Map.fetch!(data, "authorization_endpoint"),
      token_endpoint: Map.fetch!(data, "token_endpoint"),
      registration_endpoint: Map.get(data, "registration_endpoint"),
      scopes_supported: Map.get(data, "scopes_supported", []),
      response_types_supported: Map.get(data, "response_types_supported", ["code"]),
      grant_types_supported:
        Map.get(data, "grant_types_supported", ["authorization_code", "client_credentials"]),
      code_challenge_methods_supported:
        Map.get(data, "code_challenge_methods_supported", ["S256"])
    }
  end
end
