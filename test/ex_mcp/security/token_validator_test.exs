defmodule ExMCP.Security.TokenValidatorTest do
  use ExUnit.Case, async: true

  alias ExMCP.Security.TokenValidator

  describe "validate_token_for_server/2" do
    test "validates token with correct audience" do
      # Mock token info with correct audience
      token_info = %{
        aud: "mcp-server-123",
        iss: "https://auth.example.com",
        exp: System.system_time(:second) + 3600,
        scope: "mcp:read mcp:write"
      }

      # Since we can't easily mock introspection, we'll test the validation logic
      # In real tests, you'd mock the introspection endpoint
      assert :ok = TokenValidator.validate_audience(token_info, "mcp-server-123")
    end

    test "rejects token with wrong audience" do
      token_info = %{
        aud: "different-server",
        iss: "https://auth.example.com",
        exp: System.system_time(:second) + 3600
      }

      # Test internal validation function
      assert {:error, :invalid_audience} =
               TokenValidator.validate_audience(token_info, "mcp-server-123")
    end

    test "handles multiple audiences" do
      token_info = %{
        aud: ["service-a", "mcp-server-123", "service-b"],
        iss: "https://auth.example.com",
        exp: System.system_time(:second) + 3600
      }

      assert :ok = TokenValidator.validate_audience(token_info, "mcp-server-123")
    end
  end

  describe "validate_dynamic_client_consent/2" do
    test "allows static clients without consent" do
      client_metadata = %{
        client_id: "static-client",
        registration_type: :static
      }

      assert {:ok, :approved} =
               TokenValidator.validate_dynamic_client_consent(client_metadata)
    end

    test "requires consent for dynamic clients" do
      client_metadata = %{
        client_id: "dynamic-client",
        registration_type: :dynamic
      }

      assert {:error, :consent_required} =
               TokenValidator.validate_dynamic_client_consent(client_metadata)
    end
  end

  describe "audit_token_usage/3" do
    @tag :capture_log
    test "creates audit trail entry" do
      token_jti = "token-123"
      client_info = %{client_id: "client-abc", name: "Test Client"}

      request_info = %{
        method: "tools/call",
        params: %{"tool" => "test", "password" => "secret"},
        source_ip: "192.168.1.1"
      }

      assert :ok = TokenValidator.audit_token_usage(token_jti, client_info, request_info)

      # Verify sensitive data was sanitized
      # In real implementation, check logs don't contain "secret"
    end
  end

  describe "validate_token_boundary/2" do
    test "validates matching trust boundary" do
      token_info = %{mcp_boundary: "production"}

      assert :ok = TokenValidator.validate_token_boundary(token_info, "production")
    end

    test "rejects mismatched trust boundary" do
      token_info = %{mcp_boundary: "staging"}

      assert {:error, :invalid_boundary} =
               TokenValidator.validate_token_boundary(token_info, "production")
    end

    test "checks audience list for boundary" do
      token_info = %{aud: ["production", "monitoring"]}

      assert :ok = TokenValidator.validate_token_boundary(token_info, "production")
    end
  end
end
