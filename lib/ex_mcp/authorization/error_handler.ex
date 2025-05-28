defmodule ExMCP.Authorization.ErrorHandler do
  @moduledoc """
  Handles authorization-related errors and triggers appropriate OAuth flows.

  This module implements the MCP specification requirements for handling
  401 Unauthorized and 403 Forbidden responses, automatically initiating
  OAuth flows when needed.
  """

  require Logger

  @doc """
  Handles HTTP authorization errors according to MCP specification.

  - 401 Unauthorized: Token is invalid or required
  - 403 Forbidden: Insufficient permissions

  Returns {:error, reason} or {:retry, auth_params} to trigger OAuth flow.
  """
  @spec handle_auth_error(integer(), list(), binary(), map()) ::
          {:error, atom() | {atom(), any()}}
          | {:retry, map()}
  def handle_auth_error(401, headers, _body, state) do
    Logger.debug("Received 401 Unauthorized response")

    # Extract WWW-Authenticate header for OAuth discovery
    case extract_www_authenticate(headers) do
      {:ok, auth_info} ->
        handle_unauthorized(auth_info, state)

      :error ->
        {:error, :unauthorized_no_auth_info}
    end
  end

  def handle_auth_error(403, _headers, body, _state) do
    Logger.warning("Received 403 Forbidden response")

    # Parse error details if available
    error_details = parse_error_body(body)

    {:error, {:forbidden, error_details}}
  end

  def handle_auth_error(status, _headers, _body, _state) when status in 400..499 do
    {:error, {:auth_error, status}}
  end

  def handle_auth_error(_status, _headers, _body, _state) do
    :ok
  end

  @doc """
  Processes an OAuth error response according to RFC 6749.
  """
  @spec handle_oauth_error(map()) :: {:error, {atom(), String.t()}}
  def handle_oauth_error(%{"error" => error} = response) do
    description = Map.get(response, "error_description", "")
    uri = Map.get(response, "error_uri")

    Logger.error("OAuth error: #{error} - #{description}")

    if uri do
      Logger.debug("Error details: #{uri}")
    end

    error_atom = String.to_atom(error)
    {:error, {error_atom, description}}
  end

  def handle_oauth_error(response) do
    {:error, {:invalid_oauth_response, response}}
  end

  # Private functions

  defp handle_unauthorized(auth_info, state) do
    cond do
      # Token might be expired, try refresh
      state[:refresh_token] && auth_info[:error] == "invalid_token" ->
        {:retry, %{action: :refresh_token, refresh_token: state.refresh_token}}

      # Need new authorization
      auth_info[:realm] ->
        case discover_auth_server(auth_info[:realm]) do
          {:ok, metadata} ->
            {:retry,
             %{
               action: :authorize,
               auth_server: metadata,
               scope: auth_info[:scope]
             }}

          {:error, reason} ->
            {:error, {:discovery_failed, reason}}
        end

      # Can't determine how to authenticate
      true ->
        {:error, :unauthorized_no_realm}
    end
  end

  defp extract_www_authenticate(headers) do
    case List.keyfind(headers, "www-authenticate", 0) do
      {_, value} ->
        parse_www_authenticate(value)

      nil ->
        :error
    end
  end

  defp parse_www_authenticate(value) do
    # Parse Bearer realm="...", error="...", scope="..."
    case Regex.scan(~r/(\w+)="([^"]+)"/, value) do
      matches when matches != [] ->
        params =
          Enum.map(matches, fn [_, key, val] -> {String.to_atom(key), val} end)
          |> Enum.into(%{})

        {:ok, params}

      _ ->
        :error
    end
  end

  defp discover_auth_server(realm) do
    # Use the discovery module to find authorization server metadata
    # In a full implementation, this would use the Discovery module
    # For now, return a simple structure
    if String.starts_with?(realm, "http") do
      {:ok,
       %{
         authorization_endpoint: "#{realm}/authorize",
         token_endpoint: "#{realm}/token"
       }}
    else
      {:error, :invalid_realm}
    end
  end

  defp parse_error_body(body) when is_binary(body) do
    case Jason.decode(body) do
      {:ok, json} ->
        %{
          error: Map.get(json, "error"),
          description: Map.get(json, "error_description"),
          details: Map.get(json, "details")
        }

      _ ->
        %{raw: body}
    end
  end

  defp parse_error_body(body), do: %{raw: inspect(body)}
end
