defmodule ExMCP.Authorization.ClientRegistration do
  @moduledoc """
  This module implements the standard MCP specification.

  Dynamic Client Registration for OAuth 2.1.

  Implements RFC 7591 (OAuth 2.0 Dynamic Client Registration Protocol)
  to allow MCP clients to register themselves with authorization servers
  at runtime.

  ## Example

      # Register a new client
      {:ok, client_info} = ExMCP.Authorization.ClientRegistration.register_client(%{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "My MCP Client",
        redirect_uris: ["https://localhost:8080/callback"],
        grant_types: ["authorization_code"],
        response_types: ["code"],
        scope: "mcp:read mcp:write"
      })

      # Use the returned client_id and client_secret for authorization flows
  """

  @type registration_request :: %{
          registration_endpoint: String.t(),
          client_name: String.t(),
          redirect_uris: [String.t()],
          grant_types: [String.t()],
          response_types: [String.t()],
          scope: String.t(),
          client_uri: String.t() | nil,
          logo_uri: String.t() | nil,
          contacts: [String.t()] | nil,
          tos_uri: String.t() | nil,
          policy_uri: String.t() | nil,
          software_id: String.t() | nil,
          software_version: String.t() | nil
        }

  @type client_information :: %{
          client_id: String.t(),
          client_secret: String.t() | nil,
          client_secret_expires_at: integer() | nil,
          registration_access_token: String.t() | nil,
          registration_client_uri: String.t() | nil,
          client_name: String.t(),
          redirect_uris: [String.t()],
          grant_types: [String.t()],
          response_types: [String.t()],
          scope: String.t()
        }

  @doc """
  Registers a new client with the authorization server.

  This implements the client registration flow from RFC 7591,
  sending client metadata to the registration endpoint and
  receiving client credentials in response.
  """
  @spec register_client(registration_request()) ::
          {:ok, client_information()} | {:error, term()}
  def register_client(request) do
    with :ok <- validate_registration_request(request),
         :ok <- validate_https_endpoint(request.registration_endpoint) do
      registration_body = build_registration_body(request)

      case make_registration_request(request.registration_endpoint, registration_body) do
        {:ok, response} ->
          {:ok, parse_client_information(response)}

        error ->
          error
      end
    end
  end

  @doc """
  Retrieves client information using a registration access token.

  This allows clients to read their current registration information
  from the authorization server.
  """
  @spec get_client_information(String.t(), String.t()) ::
          {:ok, client_information()} | {:error, term()}
  def get_client_information(registration_client_uri, registration_access_token) do
    with :ok <- validate_https_endpoint(registration_client_uri) do
      headers = [
        {"authorization", "Bearer #{registration_access_token}"},
        {"accept", "application/json"}
      ]

      case make_http_request(:get, registration_client_uri, headers, "") do
        {:ok, {{_, 200, _}, _headers, body}} ->
          case Jason.decode(body) do
            {:ok, client_data} ->
              {:ok, parse_client_information(client_data)}

            {:error, reason} ->
              {:error, {:json_decode_error, reason}}
          end

        {:ok, {{_, status, _}, _headers, error_body}} ->
          {:error, {:http_error, status, error_body}}

        {:error, reason} ->
          {:error, {:request_failed, reason}}
      end
    end
  end

  @doc """
  Updates client information using a registration access token.

  This allows clients to modify their registration information
  at the authorization server.
  """
  @spec update_client_information(String.t(), String.t(), map()) ::
          {:ok, client_information()} | {:error, term()}
  def update_client_information(registration_client_uri, registration_access_token, updates) do
    with :ok <- validate_https_endpoint(registration_client_uri) do
      headers = [
        {"authorization", "Bearer #{registration_access_token}"},
        {"content-type", "application/json"},
        {"accept", "application/json"}
      ]

      case Jason.encode(updates) do
        {:ok, body} ->
          case make_http_request(:put, registration_client_uri, headers, body) do
            {:ok, {{_, 200, _}, _headers, response_body}} ->
              case Jason.decode(response_body) do
                {:ok, client_data} ->
                  {:ok, parse_client_information(client_data)}

                {:error, reason} ->
                  {:error, {:json_decode_error, reason}}
              end

            {:ok, {{_, status, _}, _headers, error_body}} ->
              {:error, {:http_error, status, error_body}}

            {:error, reason} ->
              {:error, {:request_failed, reason}}
          end

        {:error, reason} ->
          {:error, {:json_encode_error, reason}}
      end
    end
  end

  # Private functions

  defp validate_registration_request(request) do
    required_fields = [:registration_endpoint, :client_name, :redirect_uris]

    missing_fields =
      Enum.filter(required_fields, fn field ->
        not Map.has_key?(request, field) or is_nil(Map.get(request, field))
      end)

    case missing_fields do
      [] -> validate_redirect_uris(request.redirect_uris)
      fields -> {:error, {:missing_required_fields, fields}}
    end
  end

  defp validate_redirect_uris(uris) when is_list(uris) do
    invalid_uris =
      Enum.filter(uris, fn uri ->
        case URI.parse(uri) do
          %URI{scheme: "https"} -> false
          %URI{scheme: "http", host: host} when host in ["localhost", "127.0.0.1"] -> false
          _ -> true
        end
      end)

    case invalid_uris do
      [] -> :ok
      uris -> {:error, {:invalid_redirect_uris, uris}}
    end
  end

  defp validate_redirect_uris(_), do: {:error, :redirect_uris_must_be_list}

  defp validate_https_endpoint(url) do
    case URI.parse(url) do
      %URI{scheme: "https"} -> :ok
      %URI{scheme: "http", host: host} when host in ["localhost", "127.0.0.1"] -> :ok
      _ -> {:error, :https_required}
    end
  end

  defp build_registration_body(request) do
    base_body = %{
      client_name: request.client_name,
      redirect_uris: request.redirect_uris,
      grant_types: Map.get(request, :grant_types, ["authorization_code"]),
      response_types: Map.get(request, :response_types, ["code"]),
      scope: Map.get(request, :scope, ""),
      # PKCE clients don't need client authentication
      token_endpoint_auth_method: "none"
    }

    # Add optional fields if present
    optional_fields = [
      :client_uri,
      :logo_uri,
      :contacts,
      :tos_uri,
      :policy_uri,
      :software_id,
      :software_version
    ]

    Enum.reduce(optional_fields, base_body, fn field, acc ->
      case Map.get(request, field) do
        nil -> acc
        value -> Map.put(acc, field, value)
      end
    end)
  end

  defp make_registration_request(endpoint, body) do
    headers = [
      {"content-type", "application/json"},
      {"accept", "application/json"}
    ]

    case Jason.encode(body) do
      {:ok, encoded_body} ->
        case make_http_request(:post, endpoint, headers, encoded_body) do
          {:ok, {{_, 201, _}, _headers, response_body}} ->
            case Jason.decode(response_body) do
              {:ok, client_data} ->
                {:ok, client_data}

              {:error, reason} ->
                {:error, {:json_decode_error, reason}}
            end

          {:ok, {{_, status, _}, _headers, error_body}} ->
            case Jason.decode(error_body) do
              {:ok, error_data} ->
                {:error, {:registration_error, status, error_data}}

              {:error, _} ->
                {:error, {:http_error, status, error_body}}
            end

          {:error, reason} ->
            {:error, {:request_failed, reason}}
        end

      {:error, reason} ->
        {:error, {:json_encode_error, reason}}
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

        method when method in [:post, :put] ->
          content_type =
            if String.contains?(body, "{"),
              do: ~c"application/json",
              else: ~c"application/x-www-form-urlencoded"

          {String.to_charlist(url), httpc_headers, content_type, String.to_charlist(body)}
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

  defp parse_client_information(data) do
    %{
      client_id: Map.fetch!(data, "client_id"),
      client_secret: Map.get(data, "client_secret"),
      client_secret_expires_at: Map.get(data, "client_secret_expires_at"),
      registration_access_token: Map.get(data, "registration_access_token"),
      registration_client_uri: Map.get(data, "registration_client_uri"),
      client_name: Map.get(data, "client_name"),
      redirect_uris: Map.get(data, "redirect_uris", []),
      grant_types: Map.get(data, "grant_types", ["authorization_code"]),
      response_types: Map.get(data, "response_types", ["code"]),
      scope: Map.get(data, "scope", "")
    }
  end
end
