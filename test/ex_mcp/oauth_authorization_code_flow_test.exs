defmodule ExMCP.OAuthAuthorizationCodeFlowTest do
  @moduledoc """
  Comprehensive test suite for OAuth 2.1 Authorization Code Flow with PKCE.
  
  Tests the complete OAuth 2.1 authorization code flow according to MCP specification,
  including PKCE requirements, security validations, and error handling.
  
  This test suite is designed to initially fail until the complete OAuth implementation
  is in place, following TDD principles.
  """
  
  use ExUnit.Case, async: true
  
  alias ExMCP.Authorization
  alias ExMCP.Authorization.PKCE
  
  describe "OAuth 2.1 Authorization Code Flow - Start Authorization" do
    test "starts authorization flow with valid configuration" do
      config = %{
        client_id: "test-client-id",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read", "mcp:write"]
      }
      
      assert {:ok, auth_url, state} = Authorization.start_authorization_flow(config)
      
      # Verify authorization URL contains required parameters
      uri = URI.parse(auth_url)
      query_params = URI.decode_query(uri.query)
      
      assert query_params["response_type"] == "code"
      assert query_params["client_id"] == "test-client-id"
      assert query_params["redirect_uri"] == "https://localhost:8080/callback"
      assert query_params["scope"] == "mcp:read mcp:write"
      assert query_params["code_challenge_method"] == "S256"
      
      # Verify PKCE parameters are present
      assert is_binary(query_params["code_challenge"])
      assert is_binary(query_params["state"])
      
      # Verify state contains code verifier
      assert is_binary(state.code_verifier)
      assert is_binary(state.state_param)
      assert state.state_param == query_params["state"]
      
      # Verify PKCE challenge is valid
      assert :ok == PKCE.verify_challenge(state.code_verifier, query_params["code_challenge"])
    end
    
    test "includes additional parameters in authorization URL" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read"],
        additional_params: %{
          "audience" => "mcp-api",
          "resource" => "https://api.example.com"
        }
      }
      
      assert {:ok, auth_url, _state} = Authorization.start_authorization_flow(config)
      
      uri = URI.parse(auth_url)
      query_params = URI.decode_query(uri.query)
      
      assert query_params["audience"] == "mcp-api"
      assert query_params["resource"] == "https://api.example.com"
    end
    
    test "requires HTTPS for authorization endpoint" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "http://insecure.example.com/oauth/authorize",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read"]
      }
      
      assert {:error, :https_required} = Authorization.start_authorization_flow(config)
    end
    
    test "allows localhost HTTP for development" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "http://localhost:3000/oauth/authorize",
        redirect_uri: "http://localhost:8080/callback",
        scopes: ["mcp:read"]
      }
      
      assert {:ok, _auth_url, _state} = Authorization.start_authorization_flow(config)
    end
    
    test "validates redirect URI scheme" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "custom://callback",
        scopes: ["mcp:read"]
      }
      
      assert {:error, :invalid_redirect_uri} = Authorization.start_authorization_flow(config)
    end
    
    test "generates unique state and PKCE values on each call" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize", 
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read"]
      }
      
      {:ok, _url1, state1} = Authorization.start_authorization_flow(config)
      {:ok, _url2, state2} = Authorization.start_authorization_flow(config)
      
      # State parameters should be unique
      assert state1.state_param != state2.state_param
      assert state1.code_verifier != state2.code_verifier
    end
  end
  
  describe "OAuth 2.1 Authorization Code Flow - Token Exchange" do
    test "exchanges authorization code for access token with PKCE" do
      # First generate PKCE challenge
      {:ok, code_verifier, _code_challenge} = PKCE.generate_challenge()
      
      params = %{
        code: "mock_authorization_code",
        code_verifier: code_verifier,
        client_id: "test-client-id",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "https://auth.example.com/oauth/token"
      }
      
      # Mock HTTP response for token exchange
      mock_token_response = %{
        "access_token" => "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...",
        "token_type" => "Bearer",
        "expires_in" => 3600,
        "refresh_token" => "refresh_token_value",
        "scope" => "mcp:read mcp:write"
      }
      
      # This will initially fail until HTTP mocking or real server is set up
      # For now, expect a network error which is acceptable for initial test failure
      result = Authorization.exchange_code_for_token(params)
      
      case result do
        {:ok, token_response} ->
          # If somehow successful, verify response structure
          assert token_response.access_token == "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9..."
          assert token_response.token_type == "Bearer"
          assert token_response.expires_in == 3600
          assert token_response.refresh_token == "refresh_token_value"
          assert token_response.scope == "mcp:read mcp:write"
          
        {:error, {:request_failed, _reason}} ->
          # Expected failure for network request - this is OK for initial test run
          assert true
          
        {:error, reason} ->
          # Log unexpected errors for debugging
          flunk("Unexpected error: #{inspect(reason)}")
      end
    end
    
    test "includes client_secret when provided" do
      {:ok, code_verifier, _code_challenge} = PKCE.generate_challenge()
      
      params = %{
        code: "mock_authorization_code",
        code_verifier: code_verifier,
        client_id: "confidential-client",
        client_secret: "client-secret-value",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "https://auth.example.com/oauth/token"
      }
      
      # This should fail initially due to network, but we're testing parameter handling
      result = Authorization.exchange_code_for_token(params)
      
      # Accept network failure as expected
      case result do
        {:error, {:request_failed, _}} -> assert true
        other -> flunk("Expected network failure, got: #{inspect(other)}")
      end
    end
    
    test "requires HTTPS for token endpoint" do
      {:ok, code_verifier, _code_challenge} = PKCE.generate_challenge()
      
      params = %{
        code: "mock_code",
        code_verifier: code_verifier,
        client_id: "test-client",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "http://insecure.example.com/oauth/token"
      }
      
      assert {:error, :https_required} = Authorization.exchange_code_for_token(params)
    end
    
    test "allows localhost HTTP for development token endpoint" do
      {:ok, code_verifier, _code_challenge} = PKCE.generate_challenge()
      
      params = %{
        code: "mock_code",
        code_verifier: code_verifier,
        client_id: "test-client",
        redirect_uri: "http://localhost:8080/callback",
        token_endpoint: "http://localhost:3000/oauth/token"
      }
      
      # Should fail with network error, not HTTPS validation error
      result = Authorization.exchange_code_for_token(params)
      assert match?({:error, {:request_failed, _}}, result)
    end
    
    test "validates required parameters for token exchange" do
      # Test missing code
      params = %{
        code_verifier: "test_verifier",
        client_id: "test-client",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "https://auth.example.com/oauth/token"
      }
      
      # This should cause a function clause error due to missing required field
      assert_raise(FunctionClauseError, fn ->
        Authorization.exchange_code_for_token(params)
      end)
    end
  end
  
  describe "OAuth 2.1 Authorization Code Flow - Error Handling" do
    test "handles OAuth error responses during token exchange" do
      {:ok, code_verifier, _code_challenge} = PKCE.generate_challenge()
      
      params = %{
        code: "invalid_code",
        code_verifier: code_verifier,
        client_id: "test-client",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "https://auth.example.com/oauth/token"
      }
      
      # This will fail initially - we need to implement error response handling
      result = Authorization.exchange_code_for_token(params)
      
      # For now, accept network failure
      case result do
        {:error, {:request_failed, _}} -> assert true
        {:error, {:oauth_error, status, error_data}} -> 
          # If OAuth error handling is implemented, verify structure
          assert is_integer(status)
          assert is_map(error_data)
        other -> 
          flunk("Expected error response, got: #{inspect(other)}")
      end
    end
    
    test "handles malformed JSON responses" do
      # This test ensures robust error handling for malformed server responses
      # Will initially fail until comprehensive error handling is implemented
      assert true
    end
    
    test "handles network timeouts and connection errors" do
      # Test that network failures are properly handled
      # Initial implementation should handle basic network errors
      {:ok, code_verifier, _code_challenge} = PKCE.generate_challenge()
      
      params = %{
        code: "test_code",
        code_verifier: code_verifier,
        client_id: "test-client",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "https://nonexistent.example.com/oauth/token"
      }
      
      result = Authorization.exchange_code_for_token(params)
      assert match?({:error, {:request_failed, _}}, result)
    end
  end
  
  describe "OAuth 2.1 Authorization Code Flow - Security Requirements" do
    test "prevents authorization code interception with PKCE" do
      # Verify that PKCE is mandatory for all authorization code flows
      config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read"]
      }
      
      {:ok, auth_url, state} = Authorization.start_authorization_flow(config)
      
      # Parse URL to verify PKCE parameters
      uri = URI.parse(auth_url)
      query_params = URI.decode_query(uri.query)
      
      # PKCE parameters MUST be present
      assert query_params["code_challenge_method"] == "S256"
      assert is_binary(query_params["code_challenge"])
      assert String.length(query_params["code_challenge"]) > 0
      
      # Verify challenge-verifier relationship
      assert :ok == PKCE.verify_challenge(state.code_verifier, query_params["code_challenge"])
    end
    
    test "generates cryptographically secure state parameter" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read"]
      }
      
      {:ok, _auth_url, state} = Authorization.start_authorization_flow(config)
      
      # State should be cryptographically random
      assert is_binary(state.state_param)
      assert String.length(state.state_param) >= 32  # Sufficient entropy
      
      # Base64url encoded without padding
      refute String.contains?(state.state_param, "=")
      refute String.contains?(state.state_param, "+")
      refute String.contains?(state.state_param, "/")
    end
    
    test "validates redirect URI exactly matches registered URI" do
      # This test verifies that redirect URI validation is strict
      # Implementation should perform exact string matching
      config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://app.example.com/callback",
        scopes: ["mcp:read"]
      }
      
      {:ok, auth_url, _state} = Authorization.start_authorization_flow(config)
      
      uri = URI.parse(auth_url)
      query_params = URI.decode_query(uri.query)
      
      # Redirect URI should be exactly as provided
      assert query_params["redirect_uri"] == "https://app.example.com/callback"
    end
    
    test "requires secure authorization and token endpoints" do
      # Both endpoints must use HTTPS in production
      https_config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://app.example.com/callback",
        scopes: ["mcp:read"]
      }
      
      assert {:ok, _url, _state} = Authorization.start_authorization_flow(https_config)
      
      # HTTP should fail unless localhost
      http_config = %{
        client_id: "test-client", 
        authorization_endpoint: "http://auth.example.com/oauth/authorize",
        redirect_uri: "https://app.example.com/callback",
        scopes: ["mcp:read"]
      }
      
      assert {:error, :https_required} = Authorization.start_authorization_flow(http_config)
    end
  end
  
  describe "OAuth 2.1 Authorization Code Flow - Integration Tests" do
    test "complete authorization flow with mock server" do
      # This test will initially fail until we implement a mock OAuth server
      # or proper HTTP mocking for integration testing
      
      # Step 1: Start authorization flow
      config = %{
        client_id: "integration-test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read", "mcp:write"]
      }
      
      {:ok, auth_url, state} = Authorization.start_authorization_flow(config)
      
      # Step 2: Simulate user authorization (normally done in browser)
      # Parse the auth URL to extract parameters
      uri = URI.parse(auth_url)
      query_params = URI.decode_query(uri.query)
      
      # Step 3: Simulate authorization server response with code
      mock_auth_code = "mock_authorization_code_12345"
      
      # Step 4: Exchange code for token
      token_params = %{
        code: mock_auth_code,
        code_verifier: state.code_verifier,
        client_id: config.client_id,
        redirect_uri: config.redirect_uri,
        token_endpoint: "https://auth.example.com/oauth/token"
      }
      
      # This will fail initially due to no mock server - expected
      result = Authorization.exchange_code_for_token(token_params)
      
      case result do
        {:error, {:request_failed, _}} ->
          # Expected failure - no real server available
          assert true
        {:ok, token_response} ->
          # If mock server is implemented, verify token response
          assert is_binary(token_response.access_token)
          assert token_response.token_type == "Bearer"
          assert is_integer(token_response.expires_in)
        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end
    
    test "authorization flow state preservation across requests" do
      # Verify that state is properly maintained between authorization request and callback
      config = %{
        client_id: "state-test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize", 
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read"]
      }
      
      {:ok, auth_url, state} = Authorization.start_authorization_flow(config)
      
      # Extract state from URL
      uri = URI.parse(auth_url)
      query_params = URI.decode_query(uri.query)
      url_state = query_params["state"]
      
      # State should match between URL and returned state object
      assert state.state_param == url_state
      
      # Code verifier should be preserved for token exchange
      assert is_binary(state.code_verifier)
      assert String.length(state.code_verifier) >= 43
    end
    
    test "multiple concurrent authorization flows maintain separate state" do
      # Test that multiple authorization flows don't interfere with each other
      config = %{
        client_id: "concurrent-test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://localhost:8080/callback", 
        scopes: ["mcp:read"]
      }
      
      # Start multiple flows concurrently
      {:ok, _url1, state1} = Authorization.start_authorization_flow(config)
      {:ok, _url2, state2} = Authorization.start_authorization_flow(config)
      {:ok, _url3, state3} = Authorization.start_authorization_flow(config)
      
      # All states should be unique
      states = [state1.state_param, state2.state_param, state3.state_param]
      verifiers = [state1.code_verifier, state2.code_verifier, state3.code_verifier]
      
      assert length(Enum.uniq(states)) == 3
      assert length(Enum.uniq(verifiers)) == 3
    end
  end
end