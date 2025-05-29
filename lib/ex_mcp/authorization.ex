defmodule ExMCP.Authorization do
  @moduledoc """
  This module implements the standard MCP specification.

  MCP Authorization implementation supporting OAuth 2.1.

  This module implements the MCP authorization specification, providing
  OAuth 2.1 authorization flows with PKCE support for secure client
  authentication.

  ## Supported Grant Types

  - **Authorization Code**: For user-based interactions (PKCE required)
  - **Client Credentials**: For application-to-application communication

  ## Security Features

  - PKCE (Proof Key for Code Exchange) support - required for all clients
  - HTTPS enforcement for all authorization endpoints  
  - Secure redirect URI validation
  - Dynamic client registration
  - Server metadata discovery

  ## Example: Authorization Code Flow

      # Start authorization flow
      {:ok, auth_url, state} = ExMCP.Authorization.start_authorization_flow(%{
        client_id: "my-client",
        redirect_uri: "https://localhost:8080/callback",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        scopes: ["mcp:read", "mcp:write"]
      })

      # User visits auth_url and authorizes
      # Server redirects to redirect_uri with code

      # Exchange code for token
      {:ok, token_response} = ExMCP.Authorization.exchange_code_for_token(%{
        code: "auth_code_from_callback",
        code_verifier: state.code_verifier,
        client_id: "my-client",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "https://auth.example.com/oauth/token"
      })

  ## Example: Client Credentials Flow

      {:ok, token_response} = ExMCP.Authorization.client_credentials_flow(%{
        client_id: "service-client",
        client_secret: "client-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:admin"]
      })

  ## Server Metadata Discovery

      {:ok, metadata} = ExMCP.Authorization.discover_server_metadata(
        "https://auth.example.com"
      )

  > #### Security Warning {: .warning}
  > Always use HTTPS for authorization endpoints. PKCE is required for
  > all authorization code flows. Validate all redirect URIs.
  """

  alias ExMCP.Authorization.PKCE

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

  Returns the authorization URL and state containing the code verifier.
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

  This implements RFC 8414 (OAuth 2.0 Authorization Server Metadata).
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

  # Private functions

  defp build_refresh_token_request_body(config) do
    base_params = %{
      "client_id" => config[:client_id]
    }

    # Add grant-specific parameters
    cond do
      config[:refresh_token] ->
        Map.merge(base_params, %{
          "grant_type" => "refresh_token",
          "refresh_token" => config[:refresh_token]
        })

      config[:code] ->
        Map.merge(base_params, %{
          "grant_type" => "authorization_code",
          "code" => config[:code],
          "redirect_uri" => config[:redirect_uri],
          "code_verifier" => config[:code_verifier]
        })

      config[:client_secret] && !config[:refresh_token] ->
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
