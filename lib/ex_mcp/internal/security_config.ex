defmodule ExMCP.Internal.SecurityConfig do
  @moduledoc """
  Centralized security configuration management with validation.

  This module provides secure defaults and configuration validation
  for the ExMCP security system.
  """

  require Logger

  @default_config %{
    # Token passthrough prevention
    trusted_origins: ["localhost", "127.0.0.1", "::1"],
    additional_sensitive_headers: [],

    # Consent management
    consent_handler: ExMCP.ConsentHandler.Deny,
    consent_ttl: :timer.hours(24),
    consent_cache_cleanup_interval: :timer.minutes(5),

    # User identification resolvers for different transports
    user_id_resolvers: %{
      http: &__MODULE__.default_http_user_resolver/1,
      stdio: &__MODULE__.default_stdio_user_resolver/1,
      beam: &__MODULE__.default_beam_user_resolver/1
    },

    # Security logging
    log_security_actions: true,
    audit_log_level: :info,

    # Feature flags
    enable_token_passthrough_prevention: true,
    enable_user_consent_validation: true
  }

  @doc """
  Gets the current security configuration.

  Merges application configuration with secure defaults and validates the result.
  """
  @spec get_security_config() :: map()
  def get_security_config do
    app_config = Application.get_env(:ex_mcp, :security, %{})
    # Convert keyword list to map if needed
    app_config_map = if is_list(app_config), do: Enum.into(app_config, %{}), else: app_config
    config = Map.merge(@default_config, app_config_map)

    case validate_config(config) do
      {:ok, validated_config} ->
        validated_config

      {:error, reason} ->
        Logger.error("Invalid security configuration: #{reason}")
        Logger.error("Falling back to secure defaults")
        @default_config
    end
  end

  @doc """
  Validates security configuration.

  Ensures all required fields are present and have valid values.
  """
  @spec validate_config(map()) :: {:ok, map()} | {:error, String.t()}
  def validate_config(config) do
    with :ok <- validate_trusted_origins(config.trusted_origins),
         :ok <- validate_consent_handler(config.consent_handler),
         :ok <- validate_ttl_values(config),
         :ok <- validate_user_resolvers(config.user_id_resolvers) do
      {:ok, config}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Gets security configuration for a specific transport.

  Includes transport-specific settings and user ID resolution.
  """
  @spec get_transport_config(atom(), map()) :: map()
  def get_transport_config(transport, base_config \\ %{}) do
    security_config = get_security_config()
    transport_config = Map.merge(security_config, base_config)

    # Add transport-specific user ID resolver
    user_resolver = get_in(transport_config, [:user_id_resolvers, transport])
    Map.put(transport_config, :user_id_resolver, user_resolver)
  end

  # Default user ID resolvers for different transports

  @doc """
  Default user ID resolver for HTTP transport.

  Extracts user ID from request context, headers, or session.
  """
  def default_http_user_resolver(request_context) do
    Map.get(request_context, :user_id, "anonymous")
  end

  @doc """
  Default user ID resolver for stdio transport.

  Uses system user or process-based identification.
  """
  def default_stdio_user_resolver(_request_context) do
    System.get_env("USER") || "stdio_user"
  end

  @doc """
  Default user ID resolver for BEAM transport.

  Uses node-based identification for distributed systems.
  """
  def default_beam_user_resolver(_request_context) do
    "#{node()}_beam_user"
  end

  # Private validation functions

  defp validate_trusted_origins(origins) when is_list(origins) do
    if Enum.all?(origins, &is_binary/1) do
      :ok
    else
      {:error, "trusted_origins must be a list of strings"}
    end
  end

  defp validate_trusted_origins(_), do: {:error, "trusted_origins must be a list"}

  defp validate_consent_handler(handler) when is_atom(handler) do
    if Code.ensure_loaded?(handler) and function_exported?(handler, :request_consent, 3) do
      :ok
    else
      {:error, "consent_handler must implement ExMCP.ConsentHandler behavior"}
    end
  end

  defp validate_consent_handler(_), do: {:error, "consent_handler must be a module"}

  defp validate_ttl_values(config) do
    if is_integer(config.consent_ttl) and config.consent_ttl > 0 do
      :ok
    else
      {:error, "consent_ttl must be a positive integer (milliseconds)"}
    end
  end

  defp validate_user_resolvers(resolvers) when is_map(resolvers) do
    required_transports = [:http, :stdio, :beam]

    if Enum.all?(required_transports, &Map.has_key?(resolvers, &1)) do
      :ok
    else
      {:error, "user_id_resolvers must include resolvers for http, stdio, and beam transports"}
    end
  end

  defp validate_user_resolvers(_), do: {:error, "user_id_resolvers must be a map"}
end
