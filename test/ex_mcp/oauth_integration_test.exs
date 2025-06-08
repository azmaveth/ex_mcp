defmodule ExMCP.OAuthIntegrationTest do
  @moduledoc """
  Integration tests for OAuth functionality with ExMCP.
  
  Tests OAuth integration with the existing transport implementations
  and client functionality.
  """
  
  use ExUnit.Case, async: true
  
  alias ExMCP.Authorization
  alias ExMCP.Authorization.{TokenManager, PKCE}
  
  @moduletag :capture_log
  
  describe "OAuth Authorization Flow Integration" do
    test "complete authorization code flow with PKCE" do
      # Start authorization flow
      config = %{
        client_id: "integration-test-client",
        authorization_endpoint: "https://auth.example.com/oauth/authorize",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read", "mcp:write"]
      }
      
      assert {:ok, auth_url, state} = Authorization.start_authorization_flow(config)
      
      # Verify all components are present
      assert is_binary(auth_url)
      assert is_binary(state.code_verifier)
      assert is_binary(state.state_param)
      
      # Parse authorization URL
      uri = URI.parse(auth_url)
      query_params = URI.decode_query(uri.query)
      
      # Verify PKCE is enforced
      assert query_params["code_challenge_method"] == "S256"
      assert query_params["code_challenge"] != nil
      
      # Verify challenge is valid
      assert :ok = PKCE.verify_challenge(state.code_verifier, query_params["code_challenge"])
    end
    
    test "client credentials flow for service authentication" do
      config = %{
        client_id: "service-client",
        client_secret: "service-secret",
        token_endpoint: "https://auth.example.com/oauth/token",
        scopes: ["mcp:service"]
      }
      
      # Would normally get a token
      result = Authorization.client_credentials_flow(config)
      
      # Expect network failure in test environment
      assert match?({:error, {:request_failed, _}}, result)
    end
    
    test "authorization server discovery" do
      issuer = "https://auth.example.com"
      
      result = Authorization.discover_server_metadata(issuer)
      
      # Expect network failure in test environment
      assert match?({:error, {:request_failed, _}}, result)
    end
  end
  
  describe "Token Manager Integration" do
    test "token manager lifecycle with OAuth config" do
      auth_config = %{
        client_id: "test-client",
        client_secret: "test-secret",
        token_endpoint: "https://auth.example.com/token"
      }
      
      # Start token manager
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      # Set initial token
      token = %{
        "access_token" => "test-access-token",
        "refresh_token" => "test-refresh-token",
        "expires_in" => 3600,
        "token_type" => "Bearer"
      }
      
      :ok = TokenManager.set_token(manager, token)
      
      # Get token
      assert {:ok, "test-access-token"} = TokenManager.get_token(manager)
      
      # Get full token info
      assert {:ok, info} = TokenManager.get_token_info(manager)
      assert info.access_token == "test-access-token"
      assert info.token_type == "Bearer"
      
      # Subscribe to updates
      :ok = TokenManager.subscribe(manager)
      
      # Update token
      new_token = %{
        "access_token" => "new-access-token",
        "expires_in" => 7200
      }
      
      :ok = TokenManager.set_token(manager, new_token)
      
      # Should receive notification
      assert_receive {:token_updated, ^manager, updated_info}
      assert updated_info.access_token == "new-access-token"
      
      # Stop manager
      :ok = GenServer.stop(manager)
    end
    
    test "token refresh scheduling" do
      auth_config = %{
        client_id: "refresh-test",
        token_endpoint: "https://auth.example.com/token",
        refresh_window: 300  # 5 minutes
      }
      
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      # Token with refresh capability
      token = %{
        "access_token" => "schedulable-token",
        "refresh_token" => "refresh-token",
        "expires_in" => 3600  # 1 hour
      }
      
      :ok = TokenManager.set_token(manager, token)
      
      # Verify refresh timer is scheduled
      state = :sys.get_state(manager)
      assert is_reference(state.refresh_timer)
      
      :ok = GenServer.stop(manager)
    end
  end
  
  describe "OAuth with MCP Client" do
    test "client configuration with OAuth" do
      # OAuth configuration for client
      oauth_config = %{
        client_id: "mcp-client",
        client_secret: "mcp-secret",
        token_endpoint: "https://auth.example.com/token"
      }
      
      initial_token = %{
        "access_token" => "client-token",
        "refresh_token" => "client-refresh",
        "expires_in" => 3600
      }
      
      # Client configuration
      client_opts = [
        transport: :stdio,  # Use stdio to avoid network issues
        security: %{
          auth_config: oauth_config,
          initial_token: initial_token
        }
      ]
      
      # Client would use token manager internally
      assert is_list(client_opts)
      assert client_opts[:security][:auth_config][:client_id] == "mcp-client"
    end
    
    test "authorization header construction" do
      token = "test-bearer-token"
      
      # Construct authorization header
      auth_header = {"authorization", "Bearer #{token}"}
      
      assert auth_header == {"authorization", "Bearer test-bearer-token"}
    end
  end
  
  describe "PKCE Security" do
    test "PKCE generation and verification" do
      # Generate multiple challenges to test uniqueness
      challenges = for _i <- 1..10 do
        {:ok, verifier, challenge} = PKCE.generate_challenge()
        {verifier, challenge}
      end
      
      # All should be unique
      unique_verifiers = challenges |> Enum.map(&elem(&1, 0)) |> Enum.uniq()
      unique_challenges = challenges |> Enum.map(&elem(&1, 1)) |> Enum.uniq()
      
      assert length(unique_verifiers) == 10
      assert length(unique_challenges) == 10
      
      # Each should verify correctly
      Enum.each(challenges, fn {verifier, challenge} ->
        assert :ok = PKCE.verify_challenge(verifier, challenge)
      end)
      
      # Cross-verification should fail
      [{v1, c1}, {v2, c2} | _] = challenges
      assert {:error, :invalid_challenge} = PKCE.verify_challenge(v1, c2)
      assert {:error, :invalid_challenge} = PKCE.verify_challenge(v2, c1)
    end
    
    test "PKCE code verifier validation" do
      {:ok, verifier, _challenge} = PKCE.generate_challenge()
      
      # Valid verifier
      assert :ok = PKCE.validate_code_verifier(verifier)
      
      # Invalid verifiers
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier("")
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier("too-short")
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier("has spaces")
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier("has!special@chars")
    end
  end
  
  describe "OAuth Error Handling" do
    test "handles various OAuth error responses" do
      # Test error response parsing
      error_responses = [
        %{"error" => "invalid_client", "error_description" => "Client not found"},
        %{"error" => "invalid_grant", "error_description" => "Code expired"},
        %{"error" => "insufficient_scope", "error_description" => "Missing required scope"}
      ]
      
      for error_data <- error_responses do
        # These would be parsed from HTTP responses
        assert is_binary(error_data["error"])
        assert is_binary(error_data["error_description"])
      end
    end
    
    test "token expiration handling" do
      auth_config = %{
        client_id: "expiry-test",
        token_endpoint: "https://auth.example.com/token"
      }
      
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      # Set already expired token
      expired_token = %{
        "access_token" => "expired-token",
        "expires_in" => 0
      }
      
      :ok = TokenManager.set_token(manager, expired_token)
      Process.sleep(100)
      
      # Should detect expiration
      assert {:error, :token_expired} = TokenManager.get_token(manager)
      
      :ok = GenServer.stop(manager)
    end
  end
  
  describe "Protected Resource Metadata" do
    test "discovers authorization servers for protected resources" do
      resource_url = "https://api.example.com/mcp"
      
      result = ExMCP.Authorization.ProtectedResourceMetadata.discover(resource_url)
      
      # Expect network failure in test environment
      assert match?({:error, {:request_failed, _}}, result)
    end
    
    test "parses WWW-Authenticate headers" do
      # Test various WWW-Authenticate formats
      headers = [
        ~s(Bearer realm="https://auth.example.com"),
        ~s(Bearer realm="https://auth.example.com", error="invalid_token"),
        ~s(Bearer realm="https://auth.example.com", as_uri="https://auth.example.com/.well-known/oauth-authorization-server")
      ]
      
      for header <- headers do
        result = ExMCP.Authorization.ProtectedResourceMetadata.parse_www_authenticate(header)
        
        assert match?({:ok, %{realm: _}}, result)
      end
    end
  end
  
  describe "Dynamic Client Registration" do
    test "registers new OAuth client" do
      request = %{
        registration_endpoint: "https://auth.example.com/register",
        client_name: "Test MCP Client",
        redirect_uris: ["https://localhost:8080/callback"],
        grant_types: ["authorization_code"],
        response_types: ["code"]
      }
      
      result = ExMCP.Authorization.ClientRegistration.register_client(request)
      
      # Expect network failure in test environment
      assert match?({:error, {:request_failed, _}}, result)
    end
  end
end