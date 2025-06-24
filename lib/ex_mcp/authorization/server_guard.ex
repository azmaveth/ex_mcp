defmodule ExMCP.Authorization.ServerGuard do
  @moduledoc """
  OAuth 2.1 Resource Server guard for validating bearer tokens.

  This module provides functionality for an MCP server acting as an OAuth 2.1
  Resource Server to validate incoming requests containing bearer tokens. It
  implements the validation logic as specified in RFC 6750.

  ## Features

  - Extracts bearer tokens from the `Authorization` header.
  - Validates tokens using OAuth 2.0 Token Introspection (RFC 7662).
  - Performs scope-based access control.
  - Generates appropriate `WWW-Authenticate` error responses.
  - Integrates with `ExMCP.FeatureFlags` to be enabled/disabled.

  ## Usage

  This module is typically used in an MCP server's request processing pipeline,
  for example, in a Plug or before a message handler.

      def handle_request(conn, required_scopes) do
        auth_config = %{
          introspection_endpoint: "https://auth.example.com/introspect",
          realm: "mcp-server"
        }

        case ExMCP.Authorization.ServerGuard.authorize(conn.req_headers, required_scopes, auth_config) do
          {:ok, token_info} ->
            # Authorization successful, proceed with processing
            # token_info contains claims about the token
            process_authorized_request(conn, token_info)

          {:error, {status, www_auth_header, body}} ->
            # Authorization failed, send error response
            conn
            |> put_resp_header("www-authenticate", www_auth_header)
            |> send_resp(status, body)
        end
      end
  """

  alias ExMCP.Authorization
  alias ExMCP.FeatureFlags

  @type auth_config :: %{
          required(:introspection_endpoint) => String.t(),
          optional(:realm) => String.t()
        }

  @type token_info :: map()
  @type error_response :: {integer(), String.t(), String.t()}

  @doc """
  Authorizes a request by validating the bearer token and checking scopes.

  This is the main entry point for the guard. It performs the following steps:
  1. Checks if OAuth 2.1 authorization is enabled via feature flags.
  2. Extracts the bearer token from the `Authorization` header.
  3. Validates the token using the introspection endpoint.
  4. Verifies that the token's scopes include all required scopes.

  ## Parameters
  - `headers`: A map or list of request headers.
  - `required_scopes`: A list of scope strings required for the operation.
  - `config`: Authorization configuration containing `:introspection_endpoint` and optional `:realm`.

  ## Return Value
  - `{:ok, token_info}`: If authorization is successful. `token_info` is the map
    returned from the introspection endpoint.
  - `{:error, error_response}`: If authorization fails. `error_response` is a tuple
    `{status_code, www_authenticate_header, body}`.
  - `:ok`: If authorization is disabled via feature flags.
  """
  @spec authorize(map() | list(), [String.t()], auth_config()) ::
          {:ok, token_info()} | {:error, error_response()} | :ok
  def authorize(headers, required_scopes, config) do
    if FeatureFlags.enabled?(:oauth2_auth) do
      do_authorize(headers, required_scopes, config)
    else
      # Auth is not enabled, so we allow the request.
      :ok
    end
  end

  defp do_authorize(headers, required_scopes, config) do
    with :ok <- validate_config(config),
         {:ok, token} <- extract_bearer_token(headers),
         {:ok, token_info} <- validate_token(token, config),
         :ok <- check_scopes(token_info, required_scopes) do
      {:ok, token_info}
    else
      {:error, :missing_token} ->
        {:error,
         build_error_response(
           401,
           "invalid_request",
           "Authorization header is missing or malformed.",
           Map.get(config, :realm),
           nil
         )}

      {:error, :invalid_token, reason} ->
        {:error, build_error_response(401, "invalid_token", reason, Map.get(config, :realm), nil)}

      {:error, :insufficient_scope} ->
        scope_str = Enum.join(required_scopes, " ")

        {:error,
         build_error_response(
           403,
           "insufficient_scope",
           "The request requires higher privileges.",
           Map.get(config, :realm),
           scope_str
         )}

      {:error, reason} ->
        # Handle other errors, like config validation or unexpected validation results
        {:error,
         build_error_response(
           500,
           "server_error",
           "Authorization check failed: #{inspect(reason)}",
           Map.get(config, :realm),
           nil
         )}
    end
  end

  defp validate_config(%{introspection_endpoint: endpoint}) when is_binary(endpoint) do
    case URI.parse(endpoint) do
      %URI{scheme: "https"} -> :ok
      %URI{scheme: "http", host: host} when host in ["localhost", "127.0.0.1"] -> :ok
      _ -> {:error, :https_required_for_introspection}
    end
  end

  defp validate_config(_) do
    {:error, :invalid_auth_config}
  end

  @doc """
  Extracts a bearer token from the `Authorization` header.
  """
  @spec extract_bearer_token(map() | list()) :: {:ok, String.t()} | {:error, :missing_token}
  def extract_bearer_token(headers) do
    auth_header = find_header(headers, "authorization")

    case auth_header do
      "Bearer " <> token ->
        if String.length(token) > 0, do: {:ok, token}, else: {:error, :missing_token}

      _ ->
        {:error, :missing_token}
    end
  end

  defp find_header(headers, key) when is_list(headers) do
    # Case-insensitive header lookup
    key_lower = String.downcase(key)

    Enum.find_value(headers, nil, fn
      {header_key, value} when is_binary(header_key) ->
        if String.downcase(header_key) == key_lower, do: value, else: nil

      _ ->
        nil
    end)
  end

  defp find_header(headers, key) when is_map(headers) do
    # Case-insensitive header lookup for maps
    key_lower = String.downcase(key)

    Enum.find_value(headers, nil, fn
      {header_key, value} when is_binary(header_key) ->
        if String.downcase(header_key) == key_lower, do: value, else: nil

      {header_key, value} when is_atom(header_key) ->
        if String.downcase(Atom.to_string(header_key)) == key_lower, do: value, else: nil

      _ ->
        nil
    end)
  end

  defp validate_token(token, config) do
    case Authorization.validate_token(token, config.introspection_endpoint) do
      {:ok, token_info} ->
        {:ok, token_info}

      {:error, :token_inactive} ->
        {:error, :invalid_token, "The access token is expired, revoked, or malformed."}

      {:error, {:oauth_error, _status, %{"error" => "invalid_token"}}} ->
        {:error, :invalid_token, "The access token is invalid."}

      {:error, reason} ->
        {:error, :token_validation_failed, inspect(reason)}
    end
  end

  defp check_scopes(token_info, required_scopes) do
    # Per RFC 6749, scope is a space-delimited string.
    token_scopes_str = Map.get(token_info, :scope, "") || ""
    token_scopes = String.split(token_scopes_str, " ") |> MapSet.new()

    required_set = MapSet.new(required_scopes)

    if MapSet.subset?(required_set, token_scopes) do
      :ok
    else
      {:error, :insufficient_scope}
    end
  end

  defp build_error_response(status, error_code, description, realm, scope) do
    parts =
      [
        realm && ~s(realm="#{realm}"),
        ~s(error="#{error_code}"),
        description && ~s(error_description="#{description}"),
        scope && ~s(scope="#{scope}")
      ]
      |> Enum.reject(&is_nil/1)

    www_auth_header = "Bearer " <> Enum.join(parts, ", ")
    body = Jason.encode!(%{error: error_code, error_description: description})

    {status, www_auth_header, body}
  end
end
