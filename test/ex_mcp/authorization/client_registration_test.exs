defmodule ExMCP.Authorization.ClientRegistrationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Authorization.ClientRegistration

  describe "client registration validation" do
    test "validates required fields" do
      # Missing client_name
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      assert {:error, {:missing_required_fields, [:client_name]}} =
               ClientRegistration.register_client(request)
    end

    test "validates redirect URIs" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test Client",
        # Invalid
        redirect_uris: ["http://evil.com/callback"]
      }

      assert {:error, {:invalid_redirect_uris, ["http://evil.com/callback"]}} =
               ClientRegistration.register_client(request)
    end

    test "allows valid HTTPS redirect URIs" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test Client",
        redirect_uris: ["https://app.example.com/callback"]
      }

      # Would make actual HTTP request in real scenario
      case ClientRegistration.register_client(request) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, :https_required} -> flunk("Should allow HTTPS")
        _ -> :ok
      end
    end

    test "allows localhost redirect URIs" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test Client",
        redirect_uris: ["http://localhost:8080/callback", "http://127.0.0.1:3000/auth"]
      }

      # Should pass validation for localhost URIs
      case ClientRegistration.register_client(request) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, {:invalid_redirect_uris, _}} -> flunk("Should allow localhost")
        _ -> :ok
      end
    end

    test "validates HTTPS registration endpoint" do
      request = %{
        # HTTP not allowed
        registration_endpoint: "http://auth.example.com/register",
        client_name: "Test Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      assert {:error, :https_required} = ClientRegistration.register_client(request)
    end

    test "allows localhost registration endpoint" do
      request = %{
        registration_endpoint: "http://localhost:3000/register",
        client_name: "Test Client",
        redirect_uris: ["https://localhost:8080/callback"]
      }

      # Should pass HTTPS validation for localhost
      case ClientRegistration.register_client(request) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, :https_required} -> flunk("Should allow localhost")
        _ -> :ok
      end
    end

    test "validates redirect_uris must be a list" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test Client",
        redirect_uris: "not_a_list"
      }

      assert {:error, :redirect_uris_must_be_list} = ClientRegistration.register_client(request)
    end
  end

  describe "client information operations" do
    test "validates HTTPS for client information retrieval" do
      assert {:error, :https_required} =
               ClientRegistration.get_client_information(
                 "http://auth.example.com/clients/123",
                 "access_token"
               )
    end

    test "validates HTTPS for client information updates" do
      assert {:error, :https_required} =
               ClientRegistration.update_client_information(
                 "http://auth.example.com/clients/123",
                 "access_token",
                 %{client_name: "Updated Name"}
               )
    end

    test "allows localhost for client operations" do
      # get_client_information
      case ClientRegistration.get_client_information(
             "http://localhost:3000/clients/123",
             "access_token"
           ) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, :https_required} -> flunk("Should allow localhost")
        _ -> :ok
      end

      # update_client_information
      case ClientRegistration.update_client_information(
             "http://localhost:3000/clients/123",
             "access_token",
             %{client_name: "Updated"}
           ) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, :https_required} -> flunk("Should allow localhost")
        _ -> :ok
      end
    end
  end

  describe "registration body building" do
    # We can't easily test the private function, but we can test through the public interface
    test "builds proper registration request with minimal fields" do
      request = %{
        registration_endpoint: "http://localhost:3000/register",
        client_name: "Minimal Client",
        redirect_uris: ["http://localhost:8080/callback"]
      }

      # The function should not fail on request building
      case ClientRegistration.register_client(request) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, {:json_encode_error, _}} -> flunk("Should build valid JSON")
        _ -> :ok
      end
    end

    test "builds registration request with all optional fields" do
      request = %{
        registration_endpoint: "http://localhost:3000/register",
        client_name: "Full Client",
        redirect_uris: ["http://localhost:8080/callback"],
        grant_types: ["authorization_code", "refresh_token"],
        response_types: ["code"],
        scope: "mcp:read mcp:write",
        client_uri: "https://example.com",
        logo_uri: "https://example.com/logo.png",
        contacts: ["admin@example.com"],
        tos_uri: "https://example.com/tos",
        policy_uri: "https://example.com/privacy",
        software_id: "my-mcp-client",
        software_version: "1.0.0"
      }

      # Should build valid request body
      case ClientRegistration.register_client(request) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, {:json_encode_error, _}} -> flunk("Should build valid JSON")
        _ -> :ok
      end
    end
  end
end
