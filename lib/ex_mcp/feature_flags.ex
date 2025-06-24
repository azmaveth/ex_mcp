defmodule ExMCP.FeatureFlags do
  @moduledoc """
  Feature flag system for controlling rollout of new MCP features.

  This module provides a centralized way to check if specific features
  are enabled, allowing for gradual rollout and easy rollback.
  """

  @doc """
  Check if a specific feature is enabled.

  ## Features

  * `:protocol_version_header` - Enforce MCP-Protocol-Version header validation
  * `:structured_output` - Enable structured tool output with schema validation
  * `:oauth2_auth` - Enable OAuth 2.1 authorization

  ## Examples

      iex> ExMCP.FeatureFlags.enabled?(:protocol_version_header)
      false

      iex> Application.put_env(:ex_mcp, :protocol_version_required, true)
      iex> ExMCP.FeatureFlags.enabled?(:protocol_version_header)
      true
  """
  @spec enabled?(atom()) :: boolean()
  def enabled?(:protocol_version_header) do
    Application.get_env(:ex_mcp, :protocol_version_required, false)
  end

  def enabled?(:structured_output) do
    Application.get_env(:ex_mcp, :structured_output_enabled, false)
  end

  def enabled?(:oauth2_auth) do
    Application.get_env(:ex_mcp, :oauth2_enabled, false)
  end

  def enabled?(_unknown_feature), do: false

  @doc """
  Get all feature flags and their current status.

  ## Examples

      iex> ExMCP.FeatureFlags.all()
      %{
        protocol_version_header: false,
        structured_output: false,
        oauth2_auth: false
      }
  """
  @spec all() :: map()
  def all do
    %{
      protocol_version_header: enabled?(:protocol_version_header),
      structured_output: enabled?(:structured_output),
      oauth2_auth: enabled?(:oauth2_auth)
    }
  end
end
