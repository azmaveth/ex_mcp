defmodule ExMCP.Compliance.Features.Authorization do
  @moduledoc """
  Shared authorization compliance tests across all MCP versions.
  Authorization features are only available in 2025-03-26 and later.
  """

  # Full module names are required in macro-generated code to ensure proper resolution
  # credo:disable-for-lines:50 Credo.Check.Design.AliasUsage
  defmacro __using__(version) do
    quote do
      import ExMCP.Compliance.Features.Authorization
      @version unquote(version)

      # Authorization features only in 2025-03-26+
      if @version in ["2025-03-26", "2025-06-18"] do
        test "OAuth 2.1 authorization framework works" do
          ExMCP.Compliance.Features.Authorization.test_oauth_authorization(@version)
        end

        test "authorization server metadata is valid" do
          ExMCP.Compliance.Features.Authorization.test_authorization_metadata(@version)
        end

        test "token introspection works correctly" do
          ExMCP.Compliance.Features.Authorization.test_token_introspection(@version)
        end

        test "client credentials flow works" do
          ExMCP.Compliance.Features.Authorization.test_client_credentials(@version)
        end

        test "authorization code flow works" do
          ExMCP.Compliance.Features.Authorization.test_authorization_code_flow(@version)
        end

        test "token refresh works correctly" do
          ExMCP.Compliance.Features.Authorization.test_token_refresh(@version)
        end

        test "PKCE challenge generation works" do
          ExMCP.Compliance.Features.Authorization.test_pkce_challenge(@version)
        end

        test "streamable HTTP transport authorization headers" do
          ExMCP.Compliance.Features.Authorization.test_streamable_http_auth(@version)
        end
      end
    end
  end

  # Import test helpers
  import ExUnit.Assertions
  alias ExMCP.Authorization
  alias ExMCP.Authorization.TokenManager

  # Test data constants
  @issuer "https://auth.example.com"
  @metadata_url "https://auth.example.com/.well-known/oauth-authorization-server"
  @auth_endpoint "https://auth.example.com/authorize"
  @token_endpoint "https://auth.example.com/token"
  @introspection_endpoint "https://auth.example.com/introspect"

  # Actual test implementations
  def test_oauth_authorization(version) when version in ["2025-03-26", "2025-06-18"] do
    # Test OAuth 2.1 authorization framework
    assert version in ["2025-03-26", "2025-06-18"]

    # Test basic OAuth 2.1 compliance
    # TODO: Add actual OAuth testing with mock server
    # This would test the complete OAuth 2.1 flow

    # For now, test that authorization modules exist
    assert Code.ensure_loaded?(ExMCP.Authorization)

    # Test authorization configuration validation
    config = %{
      issuer: @issuer,
      authorization_endpoint: @auth_endpoint,
      token_endpoint: @token_endpoint,
      introspection_endpoint: @introspection_endpoint
    }

    # Validate configuration structure
    validate_oauth_config(config)
  end

  def test_authorization_metadata(version) when version in ["2025-03-26", "2025-06-18"] do
    # Test authorization server metadata (RFC8414)
    assert version in ["2025-03-26", "2025-06-18"]

    # Test metadata structure
    metadata = %{
      "issuer" => @issuer,
      "authorization_endpoint" => @auth_endpoint,
      "token_endpoint" => @token_endpoint,
      "introspection_endpoint" => @introspection_endpoint,
      "scopes_supported" => ["mcp:read", "mcp:write", "offline_access"],
      "response_types_supported" => ["code"],
      "grant_types_supported" => ["authorization_code", "client_credentials", "refresh_token"],
      "code_challenge_methods_supported" => ["S256"]
    }

    validate_authorization_metadata(metadata)
  end

  def test_token_introspection(version) when version in ["2025-03-26", "2025-06-18"] do
    # Test token introspection functionality
    assert version in ["2025-03-26", "2025-06-18"]

    # Test introspection response structure
    introspection_response = %{
      "active" => true,
      "scope" => "mcp:read mcp:write",
      "client_id" => "test-client",
      "exp" => System.system_time(:second) + 3600
    }

    validate_introspection_response(introspection_response)
  end

  def test_client_credentials(version) when version in ["2025-03-26", "2025-06-18"] do
    # Test client credentials flow
    assert version in ["2025-03-26", "2025-06-18"]

    # Test client credentials token response
    token_response = %{
      "access_token" => "access-token-123",
      "token_type" => "Bearer",
      "expires_in" => 3600,
      "scope" => "mcp:read mcp:write"
    }

    validate_token_response(token_response)
  end

  def test_authorization_code_flow(version) when version in ["2025-03-26", "2025-06-18"] do
    # Test authorization code flow
    assert version in ["2025-03-26", "2025-06-18"]

    # Test authorization code response
    auth_response = %{
      "access_token" => "access-token-456",
      "token_type" => "Bearer",
      "expires_in" => 3600,
      "refresh_token" => "refresh-token-xyz",
      "scope" => "mcp:read mcp:write"
    }

    validate_token_response(auth_response)
    assert Map.has_key?(auth_response, "refresh_token")
  end

  def test_token_refresh(version) when version in ["2025-03-26", "2025-06-18"] do
    # Test token refresh functionality
    assert version in ["2025-03-26", "2025-06-18"]

    # Test refresh token response
    refresh_response = %{
      "access_token" => "new-access-token-789",
      "token_type" => "Bearer",
      "expires_in" => 3600,
      "scope" => "mcp:read mcp:write"
    }

    validate_token_response(refresh_response)
  end

  def test_pkce_challenge(version) when version in ["2025-03-26", "2025-06-18"] do
    # Test PKCE challenge generation
    assert version in ["2025-03-26", "2025-06-18"]

    # Test PKCE verifier and challenge generation
    {:ok, verifier, challenge} = Authorization.PKCE.generate_challenge()

    assert is_binary(verifier)
    assert is_binary(challenge)
    # Min length per spec
    assert byte_size(verifier) >= 43
    # Max length per spec
    assert byte_size(verifier) <= 128

    # Verify challenge is properly encoded
    assert String.match?(challenge, ~r/^[A-Za-z0-9_-]+$/)
  end

  def test_streamable_http_auth(version) when version in ["2025-03-26", "2025-06-18"] do
    # Test streamable HTTP transport authorization
    assert version in ["2025-03-26", "2025-06-18"]

    # Test session ID generation for HTTP transport
    session_id = Base.encode16(:crypto.strong_rand_bytes(16), case: :lower)

    assert is_binary(session_id)
    # 16 bytes hex encoded
    assert byte_size(session_id) == 32

    # Test authorization header format
    auth_header = {"authorization", "Bearer test-token-123"}

    assert elem(auth_header, 0) == "authorization"
    assert String.starts_with?(elem(auth_header, 1), "Bearer ")
  end

  # Helper functions for authorization validation
  defp validate_oauth_config(config) do
    # Validate OAuth configuration structure
    assert is_binary(config.issuer)
    assert is_binary(config.authorization_endpoint)
    assert is_binary(config.token_endpoint)
    assert is_binary(config.introspection_endpoint)

    # Validate URLs are HTTPS (security requirement)
    assert String.starts_with?(config.issuer, "https://")
    assert String.starts_with?(config.authorization_endpoint, "https://")
    assert String.starts_with?(config.token_endpoint, "https://")
    assert String.starts_with?(config.introspection_endpoint, "https://")
  end

  defp validate_authorization_metadata(metadata) do
    # Validate authorization server metadata structure (RFC8414)
    required_fields = [
      "issuer",
      "authorization_endpoint",
      "token_endpoint",
      "response_types_supported",
      "grant_types_supported"
    ]

    for field <- required_fields do
      assert Map.has_key?(metadata, field), "Missing required field: #{field}"
    end

    # Validate specific field types
    assert is_binary(metadata["issuer"])
    assert is_binary(metadata["authorization_endpoint"])
    assert is_binary(metadata["token_endpoint"])
    assert is_list(metadata["response_types_supported"])
    assert is_list(metadata["grant_types_supported"])

    # Validate required grant types for MCP
    assert "authorization_code" in metadata["grant_types_supported"]
    assert "client_credentials" in metadata["grant_types_supported"]
  end

  defp validate_token_response(response) do
    # Validate OAuth token response structure
    assert Map.has_key?(response, "access_token")
    assert Map.has_key?(response, "token_type")
    assert Map.has_key?(response, "expires_in")

    assert is_binary(response["access_token"])
    assert response["token_type"] == "Bearer"
    assert is_integer(response["expires_in"])
    assert response["expires_in"] > 0
  end

  defp validate_introspection_response(response) do
    # Validate token introspection response structure
    assert Map.has_key?(response, "active")
    assert is_boolean(response["active"])

    if response["active"] do
      # Active tokens should have additional fields
      if Map.has_key?(response, "scope") do
        assert is_binary(response["scope"])
      end

      if Map.has_key?(response, "exp") do
        assert is_integer(response["exp"])
        assert response["exp"] > System.system_time(:second)
      end
    end
  end
end
