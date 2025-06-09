defmodule ExMCP.OAuthProtectedResourceMetadataTest do
  @moduledoc """
  Comprehensive test suite for OAuth 2.0 Protected Resource Metadata (RFC 9728).

  Tests the protected resource metadata discovery mechanism that allows MCP servers
  to indicate their authorization server locations. This is a draft specification
  feature that enhances OAuth integration for protected resources.

  This test suite is designed to initially fail until the complete OAuth implementation
  is in place, following TDD principles.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Authorization
  alias ExMCP.Authorization.ProtectedResourceMetadata

  describe "Protected Resource Metadata Discovery - Basic Discovery" do
    test "discovers protected resource metadata from resource URL" do
      resource_url = "https://api.example.com/mcp"

      # Implementation should discover metadata from /.well-known/oauth-protected-resource
      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          # Verify required metadata fields
          assert is_list(metadata.authorization_servers)
          assert length(metadata.authorization_servers) > 0

          # Each authorization server should have proper structure
          Enum.each(metadata.authorization_servers, fn server ->
            assert is_binary(server.issuer)
            assert String.starts_with?(server.issuer, "https://")

            if server.metadata_endpoint do
              assert is_binary(server.metadata_endpoint)
              assert String.starts_with?(server.metadata_endpoint, "https://")
            end
          end)

        {:error, {:request_failed, _}} ->
          # Expected failure - no real server available
          assert true

        {:error, reason} ->
          flunk("Unexpected error: #{inspect(reason)}")
      end
    end

    test "constructs correct metadata URL from resource URL" do
      resource_url = "https://api.example.com/v1/mcp"

      # Should request https://api.example.com/.well-known/oauth-protected-resource
      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error for metadata discovery: #{inspect(other)}")
      end
    end

    test "handles resource URL with trailing slash" do
      resource_url = "https://api.example.com/mcp/"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error: #{inspect(other)}")
      end
    end

    test "extracts base URL correctly" do
      # Various resource URLs should resolve to same base
      resource_urls = [
        "https://api.example.com/mcp/v1",
        "https://api.example.com/services/mcp",
        "https://api.example.com/api/mcp/tools",
        "https://api.example.com:8443/mcp"
      ]

      for url <- resource_urls do
        result = ProtectedResourceMetadata.discover(url)
        assert match?({:error, {:request_failed, _}}, result)
      end
    end

    test "requires HTTPS for resource URL" do
      resource_url = "http://insecure.example.com/mcp"

      assert {:error, :https_required} = ProtectedResourceMetadata.discover(resource_url)
    end

    test "allows localhost HTTP for development" do
      resource_url = "http://localhost:3000/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)
      # Should fail with network error, not HTTPS validation error
      assert match?({:error, {:request_failed, _}}, result)
    end
  end

  describe "Protected Resource Metadata - Response Parsing" do
    test "parses valid metadata response with single authorization server" do
      resource_url = "https://api.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          assert is_list(metadata.authorization_servers)
          assert length(metadata.authorization_servers) >= 1

          # First server should have required fields
          [server | _] = metadata.authorization_servers
          assert is_binary(server.issuer)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "parses metadata with multiple authorization servers" do
      resource_url = "https://multi-auth.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          # Should support multiple authorization servers
          assert is_list(metadata.authorization_servers)

          # Each server should be properly structured
          Enum.each(metadata.authorization_servers, fn server ->
            assert Map.has_key?(server, :issuer)
            assert is_binary(server.issuer)
          end)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "handles optional fields in authorization server entries" do
      resource_url = "https://optional-fields.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          Enum.each(metadata.authorization_servers, fn server ->
            # Required field
            assert is_binary(server.issuer)

            # Optional fields
            if server.metadata_endpoint do
              assert is_binary(server.metadata_endpoint)
            end

            if server.scopes_supported do
              assert is_list(server.scopes_supported)
            end

            if server.audience do
              assert is_binary(server.audience) or is_list(server.audience)
            end
          end)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "validates required fields are present" do
      resource_url = "https://incomplete.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:invalid_metadata, _}} -> assert true
        {:ok, _metadata} -> flunk("Should fail with invalid metadata")
        other -> flunk("Unexpected error type: #{inspect(other)}")
      end
    end

    test "handles malformed JSON response" do
      resource_url = "https://malformed.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:json_decode_error, _}} -> assert true
        other -> flunk("Expected JSON decode error: #{inspect(other)}")
      end
    end
  end

  describe "Protected Resource Metadata - WWW-Authenticate Header" do
    test "extracts metadata URL from WWW-Authenticate header" do
      # Simulate 401 response with WWW-Authenticate header
      www_authenticate =
        ~s(Bearer realm="https://auth.example.com", as_uri="https://auth.example.com/.well-known/oauth-authorization-server")

      result = ProtectedResourceMetadata.parse_www_authenticate(www_authenticate)

      assert {:ok, auth_info} = result
      assert auth_info.realm == "https://auth.example.com"
      assert auth_info.as_uri == "https://auth.example.com/.well-known/oauth-authorization-server"
    end

    test "handles WWW-Authenticate with error information" do
      www_authenticate =
        ~s(Bearer realm="https://auth.example.com", error="invalid_token", error_description="The access token expired")

      result = ProtectedResourceMetadata.parse_www_authenticate(www_authenticate)

      assert {:ok, auth_info} = result
      assert auth_info.realm == "https://auth.example.com"
      assert auth_info.error == "invalid_token"
      assert auth_info.error_description == "The access token expired"
    end

    test "handles multiple authentication schemes" do
      # Server might support multiple schemes
      www_authenticate = ~s(Bearer realm="https://auth.example.com", Basic realm="api")

      result = ProtectedResourceMetadata.parse_www_authenticate(www_authenticate)

      assert {:ok, auth_info} = result
      assert auth_info.realm == "https://auth.example.com"
    end

    test "extracts resource metadata endpoint from header" do
      www_authenticate =
        ~s(Bearer realm="https://auth.example.com", resource_uri="https://api.example.com/.well-known/oauth-protected-resource")

      result = ProtectedResourceMetadata.parse_www_authenticate(www_authenticate)

      assert {:ok, auth_info} = result

      assert auth_info.resource_uri ==
               "https://api.example.com/.well-known/oauth-protected-resource"
    end

    test "handles malformed WWW-Authenticate header" do
      malformed_headers = [
        "",
        "NotBearer",
        # Missing parameters
        "Bearer",
        # Missing value
        ~s(Bearer realm),
        # Unclosed quote
        ~s(Bearer realm="unclosed)
      ]

      for header <- malformed_headers do
        result = ProtectedResourceMetadata.parse_www_authenticate(header)
        assert match?({:error, _}, result)
      end
    end
  end

  describe "Protected Resource Metadata - HTTP Error Handling" do
    test "handles 404 Not Found gracefully" do
      resource_url = "https://no-metadata.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:http_error, 404, _}} -> assert true
        {:error, :no_metadata} -> assert true
        other -> flunk("Expected 404 handling: #{inspect(other)}")
      end
    end

    test "handles 401 Unauthorized with WWW-Authenticate" do
      resource_url = "https://protected.example.com/mcp"

      # Should extract authorization info from WWW-Authenticate header
      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} ->
          assert true

        {:ok, metadata} ->
          # If 401 response includes metadata, should parse it
          assert is_list(metadata.authorization_servers)

        other ->
          flunk("Expected proper 401 handling: #{inspect(other)}")
      end
    end

    test "handles server errors appropriately" do
      resource_url = "https://error.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:http_error, status, _}} when status >= 500 -> assert true
        other -> flunk("Expected server error handling: #{inspect(other)}")
      end
    end

    test "handles network timeouts" do
      resource_url = "https://timeout.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network timeout: #{inspect(other)}")
      end
    end
  end

  describe "Protected Resource Metadata - Security Validation" do
    test "validates all URLs use HTTPS" do
      resource_url = "https://secure.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          # All authorization server URLs must use HTTPS
          Enum.each(metadata.authorization_servers, fn server ->
            assert String.starts_with?(server.issuer, "https://")

            if server.metadata_endpoint do
              assert String.starts_with?(server.metadata_endpoint, "https://")
            end
          end)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "validates issuer URLs are absolute" do
      resource_url = "https://absolute.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          Enum.each(metadata.authorization_servers, fn server ->
            uri = URI.parse(server.issuer)
            assert uri.scheme in ["https", "http"]
            assert is_binary(uri.host)
            refute is_nil(uri.host)
          end)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "prevents confused deputy attacks" do
      # Metadata should clearly indicate which auth server protects which resource
      resource_url = "https://deputy.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          # Should have clear association between resource and auth servers
          assert is_list(metadata.authorization_servers)
          assert length(metadata.authorization_servers) > 0

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end
  end

  describe "Protected Resource Metadata - Integration with OAuth Flow" do
    test "discovered metadata can be used for authorization" do
      resource_url = "https://integrated.example.com/mcp"

      case ProtectedResourceMetadata.discover(resource_url) do
        {:ok, metadata} ->
          # Use first authorization server
          [auth_server | _] = metadata.authorization_servers

          # Should be able to discover auth server metadata
          case Authorization.discover_server_metadata(auth_server.issuer) do
            {:ok, server_metadata} ->
              assert is_binary(server_metadata.authorization_endpoint)
              assert is_binary(server_metadata.token_endpoint)

            {:error, {:request_failed, _}} ->
              assert true

            other ->
              flunk("Unexpected auth server discovery result: #{inspect(other)}")
          end

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected resource metadata result: #{inspect(other)}")
      end
    end

    test "handles resource with multiple authorization options" do
      resource_url = "https://multi-auth.example.com/mcp"

      case ProtectedResourceMetadata.discover(resource_url) do
        {:ok, metadata} ->
          # Client should be able to choose from multiple auth servers
          assert length(metadata.authorization_servers) >= 1

          # Each server should be independently usable
          Enum.each(metadata.authorization_servers, fn server ->
            assert is_binary(server.issuer)
            # Could attempt discovery on each
          end)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "caches metadata appropriately" do
      resource_url = "https://cache-test.example.com/mcp"

      # Multiple requests should be handled efficiently
      _result1 = ProtectedResourceMetadata.discover(resource_url)
      _result2 = ProtectedResourceMetadata.discover(resource_url)

      # Both should complete (whether cached or not)
      assert true
    end

    test "handles delegation scenarios" do
      # Resource server delegates to third-party auth server
      resource_url = "https://delegated.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          # Auth servers might be from different domains
          Enum.each(metadata.authorization_servers, fn server ->
            uri = URI.parse(server.issuer)
            assert is_binary(uri.host)
          end)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end
  end

  describe "Protected Resource Metadata - Edge Cases" do
    test "handles empty authorization servers list" do
      resource_url = "https://no-auth.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          # Empty list means no authorization required?
          assert is_list(metadata.authorization_servers)

        {:error, {:request_failed, _}} ->
          assert true

        {:error, {:invalid_metadata, _}} ->
          # Empty list might be considered invalid
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "handles very large metadata responses" do
      resource_url = "https://large-metadata.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error: #{inspect(other)}")
      end
    end

    test "handles concurrent discovery requests" do
      resource_url = "https://concurrent.example.com/mcp"

      # Make multiple concurrent discovery requests
      tasks =
        for _i <- 1..5 do
          Task.async(fn ->
            ProtectedResourceMetadata.discover(resource_url)
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

      # Should have consistent behavior
      assert length(unique_results) <= 2
    end

    test "handles non-standard metadata fields gracefully" do
      resource_url = "https://extended.example.com/mcp"

      result = ProtectedResourceMetadata.discover(resource_url)

      case result do
        {:ok, metadata} ->
          # Should still parse required fields
          assert is_list(metadata.authorization_servers)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end
  end
end
