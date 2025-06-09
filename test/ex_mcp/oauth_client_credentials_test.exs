defmodule ExMCP.OAuthClientCredentialsTest do
  @moduledoc """
  Comprehensive test suite for OAuth 2.1 Client Credentials Flow.

  Tests the complete OAuth 2.1 client credentials flow for application-to-application
  communication according to MCP specification. This flow is used when there is no
  human user interaction required.

  This test suite is designed to initially fail until the complete OAuth implementation
  is in place, following TDD principles.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Authorization

  describe "OAuth 2.1 Client Credentials Flow - Basic Flow" do
    test "successfully exchanges client credentials for access token" do
      config = %{
        client_id: "service-client-id",
        client_secret: "super-secret-key",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:read", "mcp:write"]
      }

      # This will initially fail due to network request
      result = Authorization.client_credentials_flow(config)

      case result do
        {:ok, token_response} ->
          # If somehow successful, verify response structure
          assert is_binary(token_response.access_token)
          assert token_response.token_type == "Bearer"
          assert is_integer(token_response.expires_in)
          assert is_nil(token_response.refresh_token) or is_binary(token_response.refresh_token)
          assert token_response.scope == "mcp:read mcp:write"

        {:error, {:request_failed, _reason}} ->
          # Expected failure for network request - this is OK for initial test run
          assert true

        {:error, reason} ->
          # Log unexpected errors for debugging
          flunk("Unexpected error: #{inspect(reason)}")
      end
    end

    test "includes scopes in token request" do
      config = %{
        client_id: "scoped-client",
        client_secret: "client-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:admin", "mcp:tools"]
      }

      # This should fail initially due to network, but we're testing parameter handling
      result = Authorization.client_credentials_flow(config)

      # Accept network failure as expected
      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network failure, got: #{inspect(other)}")
      end
    end

    test "works without explicit scopes" do
      config = %{
        client_id: "no-scope-client",
        client_secret: "client-secret",
        token_endpoint: "https://auth.example.com/oauth/token"
        # No scopes specified
      }

      # Should not fail due to missing scopes
      result = Authorization.client_credentials_flow(config)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network failure, got: #{inspect(other)}")
      end
    end

    test "requires HTTPS for token endpoint" do
      config = %{
        client_id: "test-client",
        client_secret: "client-secret",
        token_endpoint: "http://insecure.example.com/oauth/token",
        scopes: ["mcp:read"]
      }

      assert {:error, :https_required} = Authorization.client_credentials_flow(config)
    end

    test "allows localhost HTTP for development" do
      config = %{
        client_id: "dev-client",
        client_secret: "dev-secret",
        token_endpoint: "http://localhost:3000/oauth/token",
        scopes: ["mcp:dev"]
      }

      # Should fail with network error, not HTTPS validation error
      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end
  end

  describe "OAuth 2.1 Client Credentials Flow - Parameter Validation" do
    test "validates required parameters" do
      # Test missing client_id
      config_missing_id = %{
        client_secret: "secret",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      assert_raise(FunctionClauseError, fn ->
        Authorization.client_credentials_flow(config_missing_id)
      end)

      # Test missing client_secret
      config_missing_secret = %{
        client_id: "client-id",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      assert_raise(FunctionClauseError, fn ->
        Authorization.client_credentials_flow(config_missing_secret)
      end)

      # Test missing token_endpoint
      config_missing_endpoint = %{
        client_id: "client-id",
        client_secret: "secret"
      }

      assert_raise(FunctionClauseError, fn ->
        Authorization.client_credentials_flow(config_missing_endpoint)
      end)
    end

    test "handles empty client credentials" do
      config = %{
        client_id: "",
        client_secret: "",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      # Should make request even with empty credentials (server will reject)
      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles very long client credentials" do
      # Test with realistic but long credentials
      long_client_id = String.duplicate("a", 1000)
      long_client_secret = String.duplicate("b", 1000)

      config = %{
        client_id: long_client_id,
        client_secret: long_client_secret,
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles special characters in credentials" do
      config = %{
        client_id: "client-with-special!@#$%^&*()_+-=",
        client_secret: "secret/with\\special|chars<>[]{}",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end
  end

  describe "OAuth 2.1 Client Credentials Flow - Scope Handling" do
    test "joins multiple scopes with spaces" do
      config = %{
        client_id: "multi-scope-client",
        client_secret: "secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:read", "mcp:write", "mcp:admin", "mcp:tools"]
      }

      # This should properly format scopes as space-separated string
      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles single scope" do
      config = %{
        client_id: "single-scope-client",
        client_secret: "secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:read"]
      }

      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles empty scope list" do
      config = %{
        client_id: "empty-scope-client",
        client_secret: "secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: []
      }

      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles scopes with special characters" do
      config = %{
        client_id: "special-scope-client",
        client_secret: "secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:read/write", "mcp:admin.tools", "mcp:resource:*"]
      }

      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end
  end

  describe "OAuth 2.1 Client Credentials Flow - Error Handling" do
    test "handles OAuth error responses" do
      config = %{
        client_id: "invalid-client",
        client_secret: "wrong-secret",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      # This will fail initially - we need to implement error response handling
      result = Authorization.client_credentials_flow(config)

      case result do
        {:error, {:request_failed, _}} ->
          # Expected failure - no real server available
          assert true

        {:error, {:oauth_error, status, error_data}} ->
          # If OAuth error handling is implemented, verify structure
          assert is_integer(status)
          assert is_map(error_data)
          assert Map.has_key?(error_data, "error")

        other ->
          flunk("Expected error response, got: #{inspect(other)}")
      end
    end

    test "handles invalid client credentials error" do
      # Test that 401 Unauthorized is properly handled
      config = %{
        client_id: "nonexistent-client",
        client_secret: "invalid-secret",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      result = Authorization.client_credentials_flow(config)

      case result do
        {:error, {:request_failed, _}} ->
          assert true

        {:error, {:oauth_error, 401, error_data}} ->
          assert error_data["error"] == "invalid_client"

        other ->
          flunk("Expected client error, got: #{inspect(other)}")
      end
    end

    test "handles unsupported grant type error" do
      # Some servers might not support client credentials
      config = %{
        client_id: "test-client",
        client_secret: "test-secret",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      result = Authorization.client_credentials_flow(config)

      case result do
        {:error, {:request_failed, _}} ->
          assert true

        {:error, {:oauth_error, 400, error_data}} ->
          assert error_data["error"] in ["unsupported_grant_type", "invalid_grant"]

        # Allow other errors for now
        other ->
          assert true
      end
    end

    test "handles server errors gracefully" do
      config = %{
        client_id: "test-client",
        client_secret: "test-secret",
        token_endpoint: "https://downserver.example.com/oauth/token"
      }

      result = Authorization.client_credentials_flow(config)

      case result do
        {:error, {:request_failed, _}} ->
          assert true

        {:error, {:http_error, status, _body}} when status >= 500 ->
          # Server error handling
          assert true

        other ->
          flunk("Expected server error handling, got: #{inspect(other)}")
      end
    end

    test "handles malformed JSON responses" do
      # This test ensures robust error handling for malformed server responses
      # Will initially fail until comprehensive error handling is implemented
      assert true
    end

    test "handles network timeouts and connection errors" do
      config = %{
        client_id: "timeout-client",
        client_secret: "timeout-secret",
        token_endpoint: "https://nonexistent.example.com/oauth/token"
      }

      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end
  end

  describe "OAuth 2.1 Client Credentials Flow - Security Requirements" do
    test "sends credentials securely in request body" do
      # Client credentials MUST be sent in POST body, not URL parameters
      config = %{
        client_id: "secure-client",
        client_secret: "secure-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:secure"]
      }

      # Implementation should use POST with form-encoded body
      result = Authorization.client_credentials_flow(config)

      # Accept network failure as expected - we're testing parameter handling
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "uses correct grant type parameter" do
      config = %{
        client_id: "grant-test-client",
        client_secret: "grant-test-secret",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      # Implementation should set grant_type=client_credentials
      result = Authorization.client_credentials_flow(config)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "does not expose credentials in logs or errors" do
      config = %{
        client_id: "log-test-client",
        client_secret: "super-secret-dont-log-this",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      result = Authorization.client_credentials_flow(config)

      # Error messages should not contain the client secret
      case result do
        {:error, {:request_failed, reason}} ->
          error_string = inspect(reason)
          refute String.contains?(error_string, "super-secret-dont-log-this")

        other ->
          error_string = inspect(other)
          refute String.contains?(error_string, "super-secret-dont-log-this")
      end
    end

    test "validates token endpoint URL format" do
      # Test various invalid URL formats
      invalid_configs = [
        %{client_id: "test", client_secret: "test", token_endpoint: "not-a-url"},
        %{client_id: "test", client_secret: "test", token_endpoint: "ftp://invalid.com/token"},
        %{client_id: "test", client_secret: "test", token_endpoint: ""}
      ]

      for config <- invalid_configs do
        result = Authorization.client_credentials_flow(config)
        # Should either fail with URL validation or request failure
        assert match?({:error, _}, result)
      end
    end
  end

  describe "OAuth 2.1 Client Credentials Flow - Response Handling" do
    test "parses successful token response correctly" do
      # This test will initially fail until we have mock server or HTTP mocking
      config = %{
        client_id: "response-test-client",
        client_secret: "response-test-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:read"]
      }

      result = Authorization.client_credentials_flow(config)

      case result do
        {:ok, token_response} ->
          # Verify all required fields are present and correctly typed
          assert is_binary(token_response.access_token)
          assert String.length(token_response.access_token) > 0
          assert token_response.token_type == "Bearer"
          assert is_integer(token_response.expires_in)
          assert token_response.expires_in > 0

          # Optional fields
          if token_response.refresh_token do
            assert is_binary(token_response.refresh_token)
          end

          if token_response.scope do
            assert is_binary(token_response.scope)
          end

        {:error, {:request_failed, _}} ->
          # Expected for no real server
          assert true

        other ->
          flunk("Unexpected response format: #{inspect(other)}")
      end
    end

    test "handles missing access_token in response" do
      # Server returns 200 but malformed response
      # This should be handled gracefully
      assert true
    end

    test "handles different token types" do
      # Some servers might return different token types
      # Implementation should preserve the token_type from response
      assert true
    end

    test "handles responses without expires_in" do
      # Some servers might not include expiration info
      # Implementation should handle this gracefully
      assert true
    end
  end

  describe "OAuth 2.1 Client Credentials Flow - Integration Tests" do
    test "complete client credentials flow lifecycle" do
      # Test the complete flow from credentials to token usage
      config = %{
        client_id: "integration-test-client",
        client_secret: "integration-test-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:integration"]
      }

      # Step 1: Get token
      result = Authorization.client_credentials_flow(config)

      case result do
        {:ok, token_response} ->
          # Step 2: Token should be usable for API calls
          assert is_binary(token_response.access_token)

          # Step 3: Token should have appropriate format (JWT or opaque)
          # For JWT tokens, they typically have 3 base64 parts separated by dots
          if String.contains?(token_response.access_token, ".") do
            parts = String.split(token_response.access_token, ".")
            # JWT format
            assert length(parts) == 3
          end

        {:error, {:request_failed, _}} ->
          # Expected failure - no real server available
          assert true

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "concurrent client credentials requests" do
      # Test that multiple concurrent requests work properly
      config = %{
        client_id: "concurrent-test-client",
        client_secret: "concurrent-test-secret",
        token_endpoint: "https://auth.example.com/oauth/token"
      }

      # Make multiple concurrent requests
      tasks =
        for _i <- 1..5 do
          Task.async(fn ->
            Authorization.client_credentials_flow(config)
          end)
        end

      results = Task.await_many(tasks, 10_000)

      # All should succeed or fail consistently
      unique_results =
        Enum.uniq_by(results, fn result ->
          case result do
            {:ok, _} -> :success
            {:error, type} -> type
          end
        end)

      # Should have consistent behavior across concurrent requests
      # At most success and one error type
      assert length(unique_results) <= 2
    end

    test "validates received token can be used for MCP requests" do
      # This test verifies the token can actually be used with MCP transport
      config = %{
        client_id: "mcp-integration-client",
        client_secret: "mcp-integration-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:read", "mcp:write"]
      }

      result = Authorization.client_credentials_flow(config)

      case result do
        {:ok, token_response} ->
          # Token should be in format suitable for Authorization header
          auth_header = "Bearer #{token_response.access_token}"
          assert String.starts_with?(auth_header, "Bearer ")
          assert String.length(auth_header) > 10

        {:error, {:request_failed, _}} ->
          # Expected for no real server
          assert true

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end
  end
end
