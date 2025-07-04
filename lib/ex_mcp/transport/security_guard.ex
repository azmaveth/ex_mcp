defmodule ExMCP.Transport.SecurityGuard do
  @moduledoc """
  Transport-layer security interceptor that enforces MCP security policies.

  This module provides consistent security enforcement across all transports
  by intercepting outbound requests and applying token passthrough prevention
  and user consent validation.
  """

  alias ExMCP.Internal.Security
  alias ExMCP.Transport.SecurityError

  require Logger

  @type request :: %{
          url: String.t(),
          headers: list({String.t(), String.t()}),
          method: String.t(),
          transport: atom(),
          user_id: String.t()
        }

  @type security_result ::
          {:ok, sanitized_request :: map()}
          | {:error, security_violation :: map()}

  @doc """
  Validates a request against security policies.

  This function enforces both token passthrough prevention and user consent
  validation for external resource access.

  ## Parameters

  - `request` - Standardized request structure
  - `config` - Security configuration (optional, uses defaults if not provided)

  ## Returns

  - `{:ok, sanitized_request}` - Request is allowed with potentially sanitized headers
  - `{:error, security_violation}` - Request blocked by security policy

  ## Examples

      request = %{
        url: "https://api.example.com/data",
        headers: [{"Authorization", "Bearer token"}],
        method: "GET",
        transport: :http,
        user_id: "user123"
      }

      case SecurityGuard.validate_request(request, config) do
        {:ok, sanitized_request} ->
          # Proceed with sanitized request
          perform_request(sanitized_request)

        {:error, violation} ->
          # Handle security violation
          {:error, violation}
      end
  """
  @spec validate_request(request(), map()) :: security_result()
  def validate_request(request, config \\ %{}) do
    Logger.debug("SecurityGuard validating request",
      url: request.url,
      transport: request.transport,
      user_id: request.user_id
    )

    with {:ok, headers_after_token_check} <- check_token_passthrough(request, config),
         {:ok, :consent_granted} <- check_user_consent(request, config) do
      sanitized_request = %{request | headers: headers_after_token_check}
      Logger.debug("SecurityGuard: Request approved", url: request.url)
      {:ok, sanitized_request}
    else
      {:error, :consent_required} ->
        error =
          SecurityError.new(
            :consent_required,
            "User consent required for external resource access",
            %{url: request.url, user_id: request.user_id, transport: request.transport}
          )

        Logger.info("SecurityGuard: Consent required",
          url: request.url,
          user_id: request.user_id
        )

        {:error, error}

      {:error, :consent_denied} ->
        error =
          SecurityError.new(
            :consent_denied,
            "User denied consent for external resource access",
            %{url: request.url, user_id: request.user_id, transport: request.transport}
          )

        Logger.warning("SecurityGuard: Consent denied",
          url: request.url,
          user_id: request.user_id
        )

        {:error, error}

      {:error, :consent_error} ->
        error =
          SecurityError.new(
            :consent_error,
            "Error processing user consent for external resource access",
            %{url: request.url, user_id: request.user_id, transport: request.transport}
          )

        Logger.error("SecurityGuard: Consent processing error",
          url: request.url,
          user_id: request.user_id
        )

        {:error, error}
    end
  end

  @doc """
  Gets the security configuration, merging provided config with defaults.
  """
  @spec get_security_config(map()) :: map()
  def get_security_config(config \\ %{}) do
    default_config = %{
      trusted_origins: ["localhost", "127.0.0.1", "::1"],
      consent_handler: ExMCP.ConsentHandler.Deny,
      log_security_actions: true
    }

    Map.merge(default_config, config)
  end

  # Private helper functions

  defp check_token_passthrough(request, config) do
    security_config = get_security_config(config)
    Security.check_token_passthrough(request.url, request.headers, security_config)
  end

  defp check_user_consent(request, config) do
    security_config = get_security_config(config)
    consent_handler = Map.get(security_config, :consent_handler, ExMCP.ConsentHandler.Deny)

    result =
      Security.ensure_user_consent(
        request.user_id,
        request.url,
        request.transport,
        consent_handler,
        security_config
      )

    case result do
      :ok ->
        # `ensure_user_consent` returns `:ok` for internal URLs where consent is not needed.
        # We map this to `{:ok, :consent_granted}` to satisfy the `with` clause in `validate_request`.
        {:ok, :consent_granted}

      {:error, :consent_denied} = error ->
        error

      {:error, :consent_required} = error ->
        error

      {:error, :consent_error} = error ->
        error

      other ->
        # Log unexpected values for debugging and treat as consent error
        # This is a defensive pattern for robustness against malformed consent handlers
        Logger.warning("SecurityGuard: Unexpected consent result: #{inspect(other)}",
          url: request.url,
          user_id: request.user_id
        )

        {:error, :consent_error}
    end
  end
end
