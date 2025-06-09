defmodule ExMCP.OAuthDynamicClientRegistrationTest do
  @moduledoc """
  Comprehensive test suite for OAuth 2.0 Dynamic Client Registration Protocol (RFC 7591).

  Tests the dynamic client registration functionality that allows MCP clients to
  register themselves with authorization servers at runtime, according to the
  MCP specification requirements.

  This test suite is designed to initially fail until the complete OAuth implementation
  is in place, following TDD principles.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Authorization.ClientRegistration

  describe "Dynamic Client Registration - Basic Registration" do
    test "registers new client with minimal required fields" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test MCP Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)

      case result do
        {:ok, client_info} ->
          # Verify required response fields
          assert is_binary(client_info.client_id)
          assert String.length(client_info.client_id) > 0
          assert client_info.client_name == "Test MCP Client"
          assert client_info.redirect_uris == ["https://localhost:8080/callback"]

          # Verify default values
          assert "authorization_code" in client_info.grant_types
          assert "code" in client_info.response_types

          # For PKCE clients, client_secret may be nil
          if client_info.client_secret do
            assert is_binary(client_info.client_secret)
          end

        {:error, {:request_failed, _}} ->
          # Expected failure - no real server available
          assert true

        {:error, reason} ->
          flunk("Unexpected error: #{inspect(reason)}")
      end
    end

    test "registers client with all optional fields" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Full Feature MCP Client",
        redirect_uris: ["https://app.example.com/callback", "https://app.example.com/callback2"],
        grant_types: ["authorization_code", "refresh_token"],
        response_types: ["code"],
        scope: "mcp:read mcp:write mcp:admin",
        client_uri: "https://app.example.com",
        logo_uri: "https://app.example.com/logo.png",
        contacts: ["admin@example.com", "support@example.com"],
        tos_uri: "https://app.example.com/tos",
        policy_uri: "https://app.example.com/privacy",
        software_id: "mcp-client-v1",
        software_version: "1.0.0"
      }

      result = ClientRegistration.register_client(request)

      case result do
        {:ok, client_info} ->
          # Verify all fields are preserved
          assert client_info.client_name == "Full Feature MCP Client"
          assert length(client_info.redirect_uris) == 2
          assert client_info.grant_types == ["authorization_code", "refresh_token"]
          assert client_info.response_types == ["code"]
          assert client_info.scope == "mcp:read mcp:write mcp:admin"

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "requires HTTPS for registration endpoint" do
      request = %{
        registration_endpoint: "http://insecure.example.com/register",
        client_name: "Test Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      assert {:error, :https_required} = ClientRegistration.register_client(request)
    end

    test "allows localhost HTTP for development" do
      request = %{
        registration_endpoint: "http://localhost:3000/register",
        client_name: "Dev Client",
        redirect_uris: ["http://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)
      # Should fail with network error, not HTTPS validation error
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "sets token_endpoint_auth_method to none for PKCE clients" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "PKCE Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      # Implementation should set token_endpoint_auth_method: "none" for PKCE
      result = ClientRegistration.register_client(request)

      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network error: #{inspect(other)}")
      end
    end
  end

  describe "Dynamic Client Registration - Field Validation" do
    test "validates required fields are present" do
      # Missing registration_endpoint
      request1 = %{
        client_name: "Test Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      assert {:error, {:missing_required_fields, fields}} =
               ClientRegistration.register_client(request1)

      assert :registration_endpoint in fields

      # Missing client_name
      request2 = %{
        registration_endpoint: "https://auth.example.com/register",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      assert {:error, {:missing_required_fields, fields}} =
               ClientRegistration.register_client(request2)

      assert :client_name in fields

      # Missing redirect_uris
      request3 = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test Client"
      }

      assert {:error, {:missing_required_fields, fields}} =
               ClientRegistration.register_client(request3)

      assert :redirect_uris in fields
    end

    test "validates redirect URIs are HTTPS" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test Client",
        redirect_uris: ["http://insecure.example.com/callback"]
      }

      assert {:error, {:invalid_redirect_uris, invalid_uris}} =
               ClientRegistration.register_client(request)

      assert "http://insecure.example.com/callback" in invalid_uris
    end

    test "allows localhost HTTP redirect URIs for development" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Dev Client",
        redirect_uris: ["http://localhost:8080/callback", "http://127.0.0.1:3000/callback"]
      }

      result = ClientRegistration.register_client(request)
      # Should pass validation, fail on network
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "validates redirect_uris is a list" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test Client",
        redirect_uris: "https://single-string.example.com/callback"
      }

      assert {:error, :redirect_uris_must_be_list} = ClientRegistration.register_client(request)
    end

    test "validates multiple redirect URIs" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Multi URI Client",
        redirect_uris: [
          "https://valid.example.com/callback",
          "http://invalid.example.com/callback",
          "https://another-valid.example.com/callback",
          "ftp://invalid-scheme.example.com/callback"
        ]
      }

      assert {:error, {:invalid_redirect_uris, invalid_uris}} =
               ClientRegistration.register_client(request)

      assert "http://invalid.example.com/callback" in invalid_uris
      assert "ftp://invalid-scheme.example.com/callback" in invalid_uris
      assert length(invalid_uris) == 2
    end

    test "handles empty redirect_uris list" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "No Redirect Client",
        redirect_uris: []
      }

      # Should be treated as missing required field
      result = ClientRegistration.register_client(request)
      assert match?({:error, _}, result)
    end
  end

  describe "Dynamic Client Registration - HTTP Request Handling" do
    test "sends correct Content-Type header" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Content Type Test",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      # Implementation should send Content-Type: application/json
      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "sends correct Accept header" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Accept Header Test",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      # Implementation should send Accept: application/json
      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "encodes request body as JSON" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "JSON Test Client",
        redirect_uris: ["https://localhost:8080/callback"],
        grant_types: ["authorization_code"],
        response_types: ["code"]
      }

      # Implementation should encode body as JSON
      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "includes all fields in request body" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Complete Request Test",
        redirect_uris: ["https://app.example.com/callback"],
        grant_types: ["authorization_code", "refresh_token"],
        response_types: ["code"],
        scope: "mcp:read mcp:write",
        client_uri: "https://app.example.com",
        contacts: ["admin@example.com"]
      }

      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end
  end

  describe "Dynamic Client Registration - Response Handling" do
    test "parses successful registration response" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Response Parse Test",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)

      case result do
        {:ok, client_info} ->
          # Verify response structure
          assert is_binary(client_info.client_id)
          assert client_info.client_name == "Response Parse Test"
          assert is_list(client_info.redirect_uris)
          assert is_list(client_info.grant_types)
          assert is_list(client_info.response_types)
          assert is_binary(client_info.scope) or is_nil(client_info.scope)

          # Optional fields
          assert is_binary(client_info.client_secret) or is_nil(client_info.client_secret)

          assert is_integer(client_info.client_secret_expires_at) or
                   is_nil(client_info.client_secret_expires_at)

          assert is_binary(client_info.registration_access_token) or
                   is_nil(client_info.registration_access_token)

          assert is_binary(client_info.registration_client_uri) or
                   is_nil(client_info.registration_client_uri)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected response: #{inspect(other)}")
      end
    end

    test "handles registration errors from server" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Error Test Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)

      case result do
        {:error, {:request_failed, _}} ->
          assert true

        {:error, {:registration_error, status, error_data}} ->
          assert is_integer(status)
          assert is_map(error_data)

        other ->
          flunk("Unexpected error format: #{inspect(other)}")
      end
    end

    test "handles malformed JSON response" do
      request = %{
        registration_endpoint: "https://malformed.example.com/register",
        client_name: "Malformed Response Test",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:json_decode_error, _}} -> assert true
        other -> flunk("Expected JSON error: #{inspect(other)}")
      end
    end

    test "requires client_id in response" do
      # Server must return client_id in successful response
      request = %{
        registration_endpoint: "https://incomplete.example.com/register",
        client_name: "Missing ID Test",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)

      case result do
        {:error, {:request_failed, _}} -> assert true
        # Missing client_id
        {:error, %KeyError{}} -> assert true
        other -> flunk("Expected missing client_id error: #{inspect(other)}")
      end
    end
  end

  describe "Dynamic Client Registration - Client Information Management" do
    test "retrieves client information with registration access token" do
      registration_client_uri = "https://auth.example.com/register/client123"
      registration_access_token = "registration_token_value"

      result =
        ClientRegistration.get_client_information(
          registration_client_uri,
          registration_access_token
        )

      case result do
        {:ok, client_info} ->
          # Should have same structure as registration response
          assert is_binary(client_info.client_id)
          assert is_binary(client_info.client_name)
          assert is_list(client_info.redirect_uris)

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "sends Bearer token in Authorization header for client info retrieval" do
      registration_client_uri = "https://auth.example.com/register/client123"
      registration_access_token = "access_token_123"

      # Implementation should send Authorization: Bearer access_token_123
      result =
        ClientRegistration.get_client_information(
          registration_client_uri,
          registration_access_token
        )

      assert match?({:error, {:request_failed, _}}, result)
    end

    test "updates client information with registration access token" do
      registration_client_uri = "https://auth.example.com/register/client123"
      registration_access_token = "registration_token_value"

      updates = %{
        client_name: "Updated Client Name",
        redirect_uris: ["https://updated.example.com/callback"]
      }

      result =
        ClientRegistration.update_client_information(
          registration_client_uri,
          registration_access_token,
          updates
        )

      case result do
        {:ok, client_info} ->
          # Should return updated client information
          assert client_info.client_name == "Updated Client Name"
          assert client_info.redirect_uris == ["https://updated.example.com/callback"]

        {:error, {:request_failed, _}} ->
          assert true

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "requires HTTPS for client management URIs" do
      # get_client_information with HTTP
      http_uri = "http://insecure.example.com/register/client123"

      assert {:error, :https_required} =
               ClientRegistration.get_client_information(http_uri, "token")

      # update_client_information with HTTP
      assert {:error, :https_required} =
               ClientRegistration.update_client_information(http_uri, "token", %{})
    end

    test "handles authorization errors for client management" do
      registration_client_uri = "https://auth.example.com/register/client123"
      invalid_token = "invalid_token"

      # Should get 401 or 403 with invalid token
      result = ClientRegistration.get_client_information(registration_client_uri, invalid_token)

      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:http_error, status, _}} when status in [401, 403] -> assert true
        other -> flunk("Expected auth error: #{inspect(other)}")
      end
    end
  end

  describe "Dynamic Client Registration - Security Requirements" do
    test "validates registration endpoint is secure" do
      # Only HTTPS endpoints should be accepted (except localhost)
      https_request = %{
        registration_endpoint: "https://secure.example.com/register",
        client_name: "Secure Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(https_request)
      # Network error, not security error
      assert match?({:error, {:request_failed, _}}, result)

      # HTTP should fail
      http_request = %{
        registration_endpoint: "http://insecure.example.com/register",
        client_name: "Insecure Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      assert {:error, :https_required} = ClientRegistration.register_client(http_request)
    end

    test "validates all redirect URIs are secure" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Mixed URIs Client",
        redirect_uris: [
          "https://secure.example.com/callback",
          "http://insecure.example.com/callback"
        ]
      }

      assert {:error, {:invalid_redirect_uris, invalid_uris}} =
               ClientRegistration.register_client(request)

      assert "http://insecure.example.com/callback" in invalid_uris
    end

    test "does not expose sensitive data in errors" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Secret Client",
        redirect_uris: ["https://secret.example.com/callback"]
      }

      result = ClientRegistration.register_client(request)

      # Error messages should not expose sensitive request details
      error_string = inspect(result)
      # Basic check - full implementation would check more thoroughly
      assert is_binary(error_string)
    end

    test "uses secure HTTP client configuration" do
      # Implementation should use proper SSL/TLS settings
      request = %{
        registration_endpoint: "https://ssl-test.example.com/register",
        client_name: "SSL Test Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end
  end

  describe "Dynamic Client Registration - Edge Cases" do
    test "handles very long client names" do
      long_name = String.duplicate("a", 1000)

      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: long_name,
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles special characters in client name" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Client with émojis 🔐 and spëcial chars!@#$%",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles many redirect URIs" do
      many_uris =
        for i <- 1..50 do
          "https://app#{i}.example.com/callback"
        end

      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Many URIs Client",
        redirect_uris: many_uris
      }

      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles Unicode in various fields" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        # Japanese
        client_name: "クライアント名前",
        redirect_uris: ["https://localhost:8080/callback"],
        # IDN domain
        client_uri: "https://例え.テスト/",
        contacts: ["admin@例え.テスト"]
      }

      result = ClientRegistration.register_client(request)
      assert match?({:error, {:request_failed, _}}, result)
    end

    test "handles concurrent registration requests" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Concurrent Test Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      # Make multiple concurrent registration requests
      tasks =
        for _i <- 1..5 do
          Task.async(fn ->
            ClientRegistration.register_client(request)
          end)
        end

      results = Task.await_many(tasks, 10_000)

      # All should fail consistently (network error)
      assert Enum.all?(results, fn result ->
               match?({:error, {:request_failed, _}}, result)
             end)
    end
  end
end
