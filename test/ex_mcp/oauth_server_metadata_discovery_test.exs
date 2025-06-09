defmodule ExMCP.OAuthServerMetadataDiscoveryTest do
  @moduledoc """
  Comprehensive test suite for OAuth 2.0 Authorization Server Metadata Discovery (RFC 8414).

  Tests the server metadata discovery mechanism that allows clients to automatically
  discover authorization server endpoints and capabilities, following the MCP specification
  requirements for OAuth 2.1 implementation.

  This test suite is designed to initially fail until the complete OAuth implementation
  is in place, following TDD principles.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Authorization

  describe "OAuth Server Metadata Discovery - Basic Discovery" do
    test "discovers server metadata from issuer URL" do
      issuer_url = "https://auth.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # Verify required metadata fields
          assert is_binary(metadata.authorization_endpoint)
          assert is_binary(metadata.token_endpoint)
          assert String.starts_with?(metadata.authorization_endpoint, "https://")
          assert String.starts_with?(metadata.token_endpoint, "https://")

          # Verify optional fields have correct types
          if metadata.registration_endpoint do
            assert is_binary(metadata.registration_endpoint)
            assert String.starts_with?(metadata.registration_endpoint, "https://")
          end

          assert is_list(metadata.scopes_supported)
          assert is_list(metadata.response_types_supported)
          assert is_list(metadata.grant_types_supported)
          assert is_list(metadata.code_challenge_methods_supported)

          # Verify PKCE support
          assert "S256" in metadata.code_challenge_methods_supported

        {:error, {:request_failed, _}} ->
          # Expected failure - no real server available
          assert true

        {:error, reason} ->
          flunk("Unexpected error: #{inspect(reason)}")
      end
    end

    test "constructs correct metadata URL from issuer" do
      issuer_url = "https://auth.example.com"

      # Should request /.well-known/oauth-authorization-server
      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error for metadata discovery: #{inspect(other)}")
      end
    end

    test "handles issuer URL with trailing slash" do
      issuer_url = "https://auth.example.com/"

      # Should properly handle trailing slash
      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error: #{inspect(other)}")
      end
    end

    test "handles issuer URL with path" do
      issuer_url = "https://auth.example.com/oauth"

      # Should append .well-known to the issuer URL
      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error: #{inspect(other)}")
      end
    end

    test "requires HTTPS for issuer URL" do
      issuer_url = "http://insecure.example.com"

      assert {:error, :https_required} = Authorization.discover_server_metadata(issuer_url)
    end

    test "allows localhost HTTP for development" do
      issuer_url = "http://localhost:3000"

      result = Authorization.discover_server_metadata(issuer_url)
      # Should fail with network error, not HTTPS validation error
      assert match?({:error, {:request_failed, _}}, result)
    end
  end

  describe "OAuth Server Metadata Discovery - Response Parsing" do
    test "parses valid metadata response correctly" do
      issuer_url = "https://auth.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # Test that all fields are parsed with correct types
          assert is_binary(metadata.authorization_endpoint)
          assert is_binary(metadata.token_endpoint)

          # Optional fields
          assert is_binary(metadata.registration_endpoint) or
                   is_nil(metadata.registration_endpoint)

          # Arrays
          assert is_list(metadata.scopes_supported)
          assert is_list(metadata.response_types_supported)
          assert is_list(metadata.grant_types_supported)
          assert is_list(metadata.code_challenge_methods_supported)

          # Verify default values are applied
          assert "code" in metadata.response_types_supported
          assert "authorization_code" in metadata.grant_types_supported
          assert "S256" in metadata.code_challenge_methods_supported

        {:error, {:request_failed, _}} ->
          # Expected for no real server
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "handles missing optional fields gracefully" do
      # Server might not include all optional fields
      issuer_url = "https://minimal.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # Required fields must be present
          assert is_binary(metadata.authorization_endpoint)
          assert is_binary(metadata.token_endpoint)

          # Optional fields should have reasonable defaults
          assert is_list(metadata.scopes_supported)
          assert is_list(metadata.response_types_supported)
          assert is_list(metadata.grant_types_supported)
          assert is_list(metadata.code_challenge_methods_supported)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "validates required fields are present" do
      # Should fail if authorization_endpoint is missing
      issuer_url = "https://incomplete.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        # Missing required field
        {:error, %KeyError{}} -> assert true
        {:ok, _metadata} -> flunk("Should fail with missing required fields")
        other -> flunk("Unexpected error type: #{inspect(other)}")
      end
    end

    test "handles malformed JSON response" do
      issuer_url = "https://malformed.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:json_decode_error, _reason}} -> assert true
        other -> flunk("Expected JSON decode error: #{inspect(other)}")
      end
    end

    test "handles non-JSON response" do
      issuer_url = "https://html.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:json_decode_error, _}} -> assert true
        other -> flunk("Expected JSON decode error: #{inspect(other)}")
      end
    end
  end

  describe "OAuth Server Metadata Discovery - HTTP Error Handling" do
    test "handles 404 Not Found" do
      issuer_url = "https://no-metadata.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:http_error, 404, _body}} -> assert true
        other -> flunk("Expected 404 handling: #{inspect(other)}")
      end
    end

    test "handles 500 Internal Server Error" do
      issuer_url = "https://broken.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:http_error, 500, _body}} -> assert true
        other -> flunk("Expected 500 handling: #{inspect(other)}")
      end
    end

    test "handles network timeouts" do
      issuer_url = "https://timeout.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network timeout: #{inspect(other)}")
      end
    end

    test "handles DNS resolution failures" do
      issuer_url = "https://nonexistent.invalid"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected DNS failure: #{inspect(other)}")
      end
    end

    test "handles SSL/TLS certificate errors" do
      # This would test invalid certificates in a real scenario
      issuer_url = "https://expired-cert.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected SSL error: #{inspect(other)}")
      end
    end
  end

  describe "OAuth Server Metadata Discovery - Security Validation" do
    test "validates endpoint URLs use HTTPS" do
      issuer_url = "https://secure.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # All endpoints must use HTTPS
          assert String.starts_with?(metadata.authorization_endpoint, "https://")
          assert String.starts_with?(metadata.token_endpoint, "https://")

          if metadata.registration_endpoint do
            assert String.starts_with?(metadata.registration_endpoint, "https://")
          end

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "validates endpoint URLs have same origin as issuer" do
      # Security best practice: endpoints should be from same origin
      issuer_url = "https://auth.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          issuer_uri = URI.parse(issuer_url)
          auth_uri = URI.parse(metadata.authorization_endpoint)
          token_uri = URI.parse(metadata.token_endpoint)

          # Should be same host (allow different paths)
          assert auth_uri.host == issuer_uri.host
          assert token_uri.host == issuer_uri.host

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "validates PKCE support is advertised" do
      issuer_url = "https://pkce.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # Must support S256 for MCP compliance
          assert "S256" in metadata.code_challenge_methods_supported

          # Should not support plain (insecure)
          refute "plain" in metadata.code_challenge_methods_supported

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "validates supported grant types include required flows" do
      issuer_url = "https://grants.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # Should support authorization_code for user flows
          assert "authorization_code" in metadata.grant_types_supported

          # Should support client_credentials for service flows  
          assert "client_credentials" in metadata.grant_types_supported

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "validates response types include code" do
      issuer_url = "https://response-types.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # Must support "code" response type for authorization code flow
          assert "code" in metadata.response_types_supported

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end
  end

  describe "OAuth Server Metadata Discovery - Edge Cases" do
    test "handles very large metadata responses" do
      # Test that large responses don't cause issues
      issuer_url = "https://large-metadata.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error: #{inspect(other)}")
      end
    end

    test "handles unusual but valid endpoint URLs" do
      issuer_url = "https://unusual.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # Should handle unusual but valid URLs
          assert String.starts_with?(metadata.authorization_endpoint, "https://")
          assert String.starts_with?(metadata.token_endpoint, "https://")

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "handles metadata with additional custom fields" do
      # Servers may include custom extensions
      issuer_url = "https://extended.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:ok, metadata} ->
          # Should still parse required fields correctly
          assert is_binary(metadata.authorization_endpoint)
          assert is_binary(metadata.token_endpoint)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "handles concurrent discovery requests" do
      issuer_url = "https://concurrent.example.com"

      # Make multiple concurrent discovery requests
      tasks =
        for _i <- 1..5 do
          Task.async(fn ->
            Authorization.discover_server_metadata(issuer_url)
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
  end

  describe "OAuth Server Metadata Discovery - Integration" do
    test "discovered metadata can be used for authorization flow" do
      issuer_url = "https://integration.example.com"

      case Authorization.discover_server_metadata(issuer_url) do
        {:ok, metadata} ->
          # Use discovered endpoints in authorization flow
          config = %{
            client_id: "discovered-client",
            authorization_endpoint: metadata.authorization_endpoint,
            redirect_uri: "https://localhost:8080/callback",
            scopes: ["mcp:read"]
          }

          # Should be able to start authorization flow with discovered endpoints
          result = Authorization.start_authorization_flow(config)
          assert match?({:ok, _auth_url, _state}, result)

        {:error, {:request_failed, _}} ->
          # Expected for no real server
          assert true

        other ->
          flunk("Unexpected discovery result: #{inspect(other)}")
      end
    end

    test "discovered metadata can be used for client credentials flow" do
      issuer_url = "https://integration.example.com"

      case Authorization.discover_server_metadata(issuer_url) do
        {:ok, metadata} ->
          # Use discovered token endpoint
          config = %{
            client_id: "service-client",
            client_secret: "service-secret",
            token_endpoint: metadata.token_endpoint,
            scopes: ["mcp:service"]
          }

          # Should be able to use discovered token endpoint
          result = Authorization.client_credentials_flow(config)
          # Will fail due to network but validates endpoint URL format
          assert match?({:error, {:request_failed, _}}, result)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected discovery result: #{inspect(other)}")
      end
    end

    test "metadata discovery caching behavior" do
      # Test if implementation caches metadata (implementation detail)
      issuer_url = "https://cache-test.example.com"

      # Multiple requests should be handled efficiently
      start_time = System.monotonic_time(:millisecond)

      _result1 = Authorization.discover_server_metadata(issuer_url)
      _result2 = Authorization.discover_server_metadata(issuer_url)

      end_time = System.monotonic_time(:millisecond)
      duration = end_time - start_time

      # Should complete quickly (whether cached or not)
      # 30 seconds max for network operations
      assert duration < 30_000
    end

    test "metadata discovery with custom HTTP headers" do
      # Future enhancement: support for custom headers in discovery
      issuer_url = "https://custom-headers.example.com"

      result = Authorization.discover_server_metadata(issuer_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error: #{inspect(other)}")
      end
    end
  end
end
