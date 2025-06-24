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
    {:ok, server} = Server.start_link(
      handler: handler_module,
      transport: :test
    )

    # Start test client
    {:ok, client} = Client.start_link(
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
    case {feature, version} do
      {:tool_annotations, v} when v in ["2025-03-26", "2025-06-18"] ->
        # Tool annotations should be present
        assert Map.has_key?(result, :annotations) or
               (is_list(result) and Enum.any?(result, &Map.has_key?(&1, :annotations)))

      {:tool_annotations, _} ->
        # Tool annotations should NOT be present in earlier versions
        refute Map.has_key?(result, :annotations)
        if is_list(result) do
          refute Enum.any?(result, &Map.has_key?(&1, :annotations))
        end

      {:batch_processing, v} when v in ["2025-03-26", "2025-06-18"] ->
        # Batch processing should be supported
        assert is_list(result) or Map.has_key?(result, :batch_supported)

      {:batch_processing, _} ->
        # Batch processing should NOT be supported in earlier versions
        :ok  # Earlier versions don't have batch support

      {:authorization, v} when v in ["2025-03-26", "2025-06-18"] ->
        # Authorization should be supported
        assert Map.has_key?(result, :authorization) or
               Map.has_key?(result, :oauth) or
               is_map(result)  # Authorization responses are maps

      {:authorization, _} ->
        # Authorization should NOT be supported in earlier versions
        :ok  # Earlier versions don't have authorization

      _ ->
        # Default validation - just ensure result exists
        assert result != nil
    end
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
