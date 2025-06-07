defmodule ExMCP.Security.TokenValidator do
  @moduledoc """
  Token validation for MCP security best practices.

  This module implements strict token validation to prevent:
  - Confused deputy attacks
  - Security control circumvention
  - Unauthorized token reuse
  - Trust boundary violations

  Following MCP specification requirement:
  "MUST NOT accept any tokens that were not explicitly issued for the MCP server"
  """

  require Logger

  @doc """
  Validates that a token was explicitly issued for this MCP server.

  Checks:
  - Token audience (aud) matches server identifier
  - Token issuer (iss) is trusted
  - Token is not expired
  - Token has required scopes for the operation

  ## Options
  - `:server_id` - The MCP server identifier (required)
  - `:trusted_issuers` - List of trusted token issuers
  - `:required_scopes` - Scopes required for the operation
  - `:introspection_endpoint` - Token introspection endpoint
  """
  @spec validate_token_for_server(String.t(), keyword()) ::
          {:ok, map()} | {:error, atom() | {atom(), any()}}
  def validate_token_for_server(token, opts) do
    server_id = Keyword.fetch!(opts, :server_id)

    with {:ok, token_info} <- introspect_token(token, opts),
         :ok <- validate_audience(token_info, server_id),
         :ok <- validate_issuer(token_info, opts),
         :ok <- validate_expiration(token_info),
         :ok <- validate_scopes(token_info, opts) do
      {:ok, token_info}
    end
  end

  @doc """
  Validates client authorization for dynamic registration.

  Per MCP spec: "MCP proxy servers... MUST obtain user consent for each 
  dynamically registered client before forwarding to third-party authorization servers"
  """
  @spec validate_dynamic_client_consent(map(), pid() | nil) ::
          {:ok, :approved} | {:error, :consent_required | :consent_denied}
  def validate_dynamic_client_consent(client_metadata, approval_handler \\ nil) do
    cond do
      # Static clients don't need consent
      client_metadata[:registration_type] == :static ->
        {:ok, :approved}

      # Dynamic clients require consent
      approval_handler != nil ->
        request_user_consent(client_metadata, approval_handler)

      # No handler configured, deny by default
      true ->
        Logger.warning("Dynamic client registration attempted without approval handler")
        {:error, :consent_required}
    end
  end

  @doc """
  Creates a security audit trail entry for accountability.

  Following best practice for maintaining request traceability.
  """
  @spec audit_token_usage(String.t(), map(), map()) :: :ok
  def audit_token_usage(token_jti, client_info, request_info) do
    audit_entry = %{
      timestamp: DateTime.utc_now(),
      token_id: token_jti,
      client_id: client_info[:client_id],
      client_name: client_info[:name],
      request_method: request_info[:method],
      request_params: sanitize_params(request_info[:params]),
      source_ip: request_info[:source_ip]
    }

    Logger.info("Security audit: #{inspect(audit_entry)}",
      tag: :security_audit,
      audit: audit_entry
    )

    :ok
  end

  @doc """
  Validates token audience separation for trust boundary management.

  Ensures tokens are properly scoped to prevent cross-service token reuse.
  """
  @spec validate_token_boundary(map(), String.t()) :: :ok | {:error, :invalid_boundary}
  def validate_token_boundary(token_info, expected_boundary) do
    # Check if token has boundary-specific claims
    case token_info do
      %{mcp_boundary: boundary} when boundary == expected_boundary ->
        :ok

      %{aud: audiences} when is_list(audiences) ->
        if expected_boundary in audiences do
          :ok
        else
          {:error, :invalid_boundary}
        end

      _ ->
        # No boundary information, consider invalid
        {:error, :invalid_boundary}
    end
  end

  # Private functions

  defp introspect_token(token, opts) do
    case Keyword.get(opts, :introspection_endpoint) do
      nil ->
        # No introspection endpoint, try to decode locally
        decode_and_validate_jwt(token)

      endpoint ->
        # Use OAuth introspection
        ExMCP.Authorization.validate_token(token, endpoint)
    end
  end

  @doc false
  def validate_audience(%{aud: audience}, server_id) when is_binary(audience) do
    if audience == server_id do
      :ok
    else
      Logger.warning("Token audience mismatch: expected #{server_id}, got #{audience}")
      {:error, :invalid_audience}
    end
  end

  def validate_audience(%{aud: audiences}, server_id) when is_list(audiences) do
    if server_id in audiences do
      :ok
    else
      Logger.warning("Token audience mismatch: server #{server_id} not in #{inspect(audiences)}")
      {:error, :invalid_audience}
    end
  end

  def validate_audience(_, _) do
    Logger.warning("Token missing audience claim")
    {:error, :missing_audience}
  end

  defp validate_issuer(token_info, opts) do
    trusted_issuers = Keyword.get(opts, :trusted_issuers, [])

    case token_info do
      %{iss: _issuer} when trusted_issuers == [] ->
        # No issuer restrictions
        :ok

      %{iss: issuer} when is_list(trusted_issuers) and length(trusted_issuers) > 0 ->
        if issuer in trusted_issuers do
          :ok
        else
          {:error, :untrusted_issuer}
        end

      %{} ->
        {:error, :missing_issuer}
    end
  end

  defp validate_expiration(%{exp: exp}) when is_integer(exp) do
    now = System.system_time(:second)

    if exp > now do
      :ok
    else
      {:error, :token_expired}
    end
  end

  defp validate_expiration(%{active: false}), do: {:error, :token_inactive}
  defp validate_expiration(%{active: true}), do: :ok
  defp validate_expiration(_), do: :ok

  defp validate_scopes(token_info, opts) do
    required_scopes = Keyword.get(opts, :required_scopes, [])

    if required_scopes == [] do
      :ok
    else
      token_scopes = parse_scopes(token_info[:scope])

      if MapSet.subset?(MapSet.new(required_scopes), MapSet.new(token_scopes)) do
        :ok
      else
        {:error, :insufficient_scope}
      end
    end
  end

  defp parse_scopes(nil), do: []
  defp parse_scopes(scope) when is_binary(scope), do: String.split(scope, " ")
  defp parse_scopes(scopes) when is_list(scopes), do: scopes

  defp decode_and_validate_jwt(_token) do
    # In production, use a proper JWT library
    # This is a placeholder
    {:error, :jwt_validation_not_implemented}
  end

  defp request_user_consent(client_metadata, approval_handler) do
    approval_data = %{
      client_id: client_metadata[:client_id],
      client_name: client_metadata[:client_name],
      redirect_uris: client_metadata[:redirect_uris],
      scopes: client_metadata[:scope],
      metadata: client_metadata
    }

    case approval_handler.request_approval(:dynamic_client_registration, approval_data, []) do
      {:approved, _} ->
        {:ok, :approved}

      {:denied, reason} ->
        Logger.info("User denied dynamic client registration: #{reason}")
        {:error, :consent_denied}

      {:modified, _} ->
        # Could support modified consent in future
        {:error, :consent_modified}
    end
  end

  defp sanitize_params(params) when is_map(params) do
    # Remove sensitive data from audit logs
    params
    |> Map.drop(["password", "secret", "token", "api_key"])
    |> Map.new(fn {k, v} ->
      {k, if(String.contains?(k, ["password", "secret", "token"]), do: "[REDACTED]", else: v)}
    end)
  end

  defp sanitize_params(params), do: params
end
