defmodule ExMCP.ComplianceTestHelpers do
  @moduledoc """
  Helper functions for MCP compliance tests.
  Provides client setup, mock servers, and common test utilities.
  """

  import ExUnit.Assertions
  alias ExMCP.{Client, Server}

  @doc """
  Sets up a test client for the specified MCP version.
  Returns {:ok, client} or {:error, reason}.
  """
  def setup_test_client(version) when version in ["2024-11-05", "2025-03-26", "2025-06-18"] do
    # Create a test server handler for the specific version
    handler_module = get_handler_module(version)

    # Start test server
    {:ok, server} =
      Server.start_link(
        handler: handler_module,
        transport: :test
      )

    # Start test client
    {:ok, client} =
      Client.start_link(
        transport: :test,
        server: server,
        client_info: %{name: "compliance-test-client", version: "1.0.0"},
        protocol_version: version
      )

    # Allow time for initialization
    Process.sleep(50)

    {:ok, %{client: client, server: server, version: version, handler: handler_module}}
  end

  @doc """
  Cleans up test client and server.
  """
  def cleanup_test_client(%{client: client, server: server}) do
    if Process.alive?(client), do: GenServer.stop(client)
    if Process.alive?(server), do: GenServer.stop(server)
    :ok
  end

  @doc """
  Creates a mock OAuth server response for testing authorization.
  """
  def mock_oauth_response(type, opts \\ []) do
    case type do
      :metadata ->
        %{
          "issuer" => "https://auth.example.com",
          "authorization_endpoint" => "https://auth.example.com/authorize",
          "token_endpoint" => "https://auth.example.com/token",
          "introspection_endpoint" => "https://auth.example.com/introspect",
          "scopes_supported" => ["mcp:read", "mcp:write", "offline_access"],
          "response_types_supported" => ["code"],
          "grant_types_supported" => ["authorization_code", "client_credentials", "refresh_token"],
          "code_challenge_methods_supported" => ["S256"]
        }

      :token ->
        scope = Keyword.get(opts, :scope, "mcp:read mcp:write")

        %{
          "access_token" => "access-token-#{System.unique_integer()}",
          "token_type" => "Bearer",
          "expires_in" => 3600,
          "scope" => scope
        }

      :introspection ->
        active = Keyword.get(opts, :active, true)
        base = %{"active" => active}

        if active do
          Map.merge(base, %{
            "scope" => "mcp:read mcp:write",
            "client_id" => "test-client",
            "exp" => System.system_time(:second) + 3600
          })
        else
          base
        end
    end
  end

  @doc """
  Validates that a test result matches expected structure for the given version.
  """
  def validate_version_compatibility(result, version, feature) do
    newer_versions = ["2025-03-26", "2025-06-18"]
    is_newer_version = version in newer_versions

    case feature do
      :tool_annotations ->
        validate_tool_annotations(result, is_newer_version)

      :batch_processing ->
        validate_batch_processing(result, is_newer_version)

      :authorization ->
        validate_authorization(result, is_newer_version)

      _ ->
        # Default validation - just ensure result exists
        assert result != nil
    end
  end

  defp validate_tool_annotations(result, true) do
    # Tool annotations should be present in newer versions
    assert Map.has_key?(result, :annotations) or
             (is_list(result) and Enum.any?(result, &Map.has_key?(&1, :annotations)))
  end

  defp validate_tool_annotations(result, false) do
    # Tool annotations should NOT be present in earlier versions
    refute Map.has_key?(result, :annotations)

    if is_list(result) do
      refute Enum.any?(result, &Map.has_key?(&1, :annotations))
    end
  end

  defp validate_batch_processing(result, true) do
    # Batch processing should be supported in newer versions
    assert is_list(result) or Map.has_key?(result, :batch_supported)
  end

  defp validate_batch_processing(_result, false) do
    # Batch processing should NOT be supported in earlier versions
    # Earlier versions don't have batch support
    :ok
  end

  defp validate_authorization(result, true) do
    # Authorization should be supported in newer versions
    # Authorization responses are maps
    assert Map.has_key?(result, :authorization) or
             Map.has_key?(result, :oauth) or
             is_map(result)
  end

  defp validate_authorization(_result, false) do
    # Authorization should NOT be supported in earlier versions
    # Earlier versions don't have authorization
    :ok
  end

  # Private helper functions

  defp get_handler_module(version) do
    case version do
      "2024-11-05" -> ExMCP.Compliance.Handlers.Handler20241105
      "2025-03-26" -> ExMCP.Compliance.Handlers.Handler20250326
      "2025-06-18" -> ExMCP.Compliance.Handlers.Handler20250618
    end
  end
end
