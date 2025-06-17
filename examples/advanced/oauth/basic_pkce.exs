# OAuth 2.1 Authorization Example
#
# This example demonstrates how to use the OAuth 2.1 authorization
# features in ExMCP according to the MCP specification.
#
# Run with: elixir -r lib/ex_mcp.ex examples/oauth_authorization.exs

alias ExMCP.Authorization
alias ExMCP.Authorization.{PKCE, ClientRegistration}

IO.puts("🔐 ExMCP OAuth 2.1 Authorization Example")
IO.puts("=" <> String.duplicate("=", 39))

# Example 1: PKCE Generation
IO.puts("\n1. PKCE (Proof Key for Code Exchange)")
IO.puts("   Required for all authorization code flows")

case PKCE.generate_challenge() do
  {:ok, verifier, challenge} ->
    IO.puts("   ✓ Code Verifier: #{String.slice(verifier, 0, 20)}...")
    IO.puts("   ✓ Code Challenge: #{String.slice(challenge, 0, 20)}...")
    
    # Verify the challenge
    case PKCE.verify_challenge(verifier, challenge) do
      :ok ->
        IO.puts("   ✓ Challenge verification: SUCCESS")
      {:error, reason} ->
        IO.puts("   ✗ Challenge verification failed: #{reason}")
    end
    
  {:error, reason} ->
    IO.puts("   ✗ PKCE generation failed: #{inspect(reason)}")
end

# Example 2: Authorization Code Flow Setup
IO.puts("\n2. Authorization Code Flow")
IO.puts("   User-based authorization with PKCE")

auth_config = %{
  client_id: "example-mcp-client",
  authorization_endpoint: "https://auth.example.com/oauth/authorize",
  redirect_uri: "https://localhost:8080/callback",
  scopes: ["mcp:read", "mcp:write", "mcp:admin"],
  additional_params: %{
    "audience" => "mcp-api"
  }
}

case Authorization.start_authorization_flow(auth_config) do
  {:ok, auth_url, state} ->
    IO.puts("   ✓ Authorization URL generated")
    IO.puts("   → User should visit: #{String.slice(auth_url, 0, 60)}...")
    IO.puts("   → State parameter: #{String.slice(state.state_param, 0, 20)}...")
    IO.puts("   → Code verifier stored for token exchange")
    
  {:error, :https_required} ->
    IO.puts("   ⚠ HTTPS required for authorization endpoints")
    IO.puts("   → For development, use localhost URLs")
    
  {:error, reason} ->
    IO.puts("   ✗ Authorization flow setup failed: #{inspect(reason)}")
end

# Example 3: Client Credentials Flow
IO.puts("\n3. Client Credentials Flow")
IO.puts("   Application-to-application authorization")

client_creds_config = %{
  client_id: "service-client",
  client_secret: "super-secret-key",
  token_endpoint: "https://auth.example.com/oauth/token",
  scopes: ["mcp:service"]
}

case Authorization.client_credentials_flow(client_creds_config) do
  {:ok, token_response} ->
    IO.puts("   ✓ Access token obtained")
    IO.puts("   → Token type: #{token_response.token_type}")
    IO.puts("   → Expires in: #{token_response.expires_in || "never"} seconds")
    IO.puts("   → Scope: #{token_response.scope || "default"}")
    
  {:error, {:request_failed, _}} ->
    IO.puts("   ⚠ Cannot connect to authorization server")
    IO.puts("   → This is expected in the example environment")
    
  {:error, reason} ->
    IO.puts("   ✗ Client credentials flow failed: #{inspect(reason)}")
end

# Example 4: Server Metadata Discovery
IO.puts("\n4. Server Metadata Discovery")
IO.puts("   Discover OAuth server capabilities")

case Authorization.discover_server_metadata("https://auth.example.com") do
  {:ok, metadata} ->
    IO.puts("   ✓ Server metadata discovered")
    IO.puts("   → Authorization endpoint: #{metadata.authorization_endpoint}")
    IO.puts("   → Token endpoint: #{metadata.token_endpoint}")
    IO.puts("   → Supported scopes: #{Enum.join(metadata.scopes_supported, ", ")}")
    IO.puts("   → PKCE methods: #{Enum.join(metadata.code_challenge_methods_supported, ", ")}")
    
  {:error, {:request_failed, _}} ->
    IO.puts("   ⚠ Cannot connect to authorization server")
    IO.puts("   → Discovery would work with a real OAuth server")
    
  {:error, reason} ->
    IO.puts("   ✗ Server metadata discovery failed: #{inspect(reason)}")
end

# Example 5: Dynamic Client Registration
IO.puts("\n5. Dynamic Client Registration")
IO.puts("   Register clients at runtime")

registration_request = %{
  registration_endpoint: "https://auth.example.com/register",
  client_name: "ExMCP Demo Client",
  redirect_uris: ["https://localhost:8080/callback"],
  grant_types: ["authorization_code", "refresh_token"],
  response_types: ["code"],
  scope: "mcp:read mcp:write",
  client_uri: "https://example.com/mcp-client",
  contacts: ["admin@example.com"]
}

case ClientRegistration.register_client(registration_request) do
  {:ok, client_info} ->
    IO.puts("   ✓ Client registered successfully")
    IO.puts("   → Client ID: #{client_info.client_id}")
    IO.puts("   → Client Name: #{client_info.client_name}")
    IO.puts("   → Redirect URIs: #{Enum.join(client_info.redirect_uris, ", ")}")
    
  {:error, {:request_failed, _}} ->
    IO.puts("   ⚠ Cannot connect to registration endpoint")
    IO.puts("   → Registration would work with a real OAuth server")
    
  {:error, reason} ->
    IO.puts("   ✗ Client registration failed: #{inspect(reason)}")
end

# Example 6: Using OAuth with ExMCP Client
IO.puts("\n6. Using OAuth with ExMCP Client")
IO.puts("   Integration with MCP transport")

# Simulate getting a token (in real scenario, this would come from auth flow)
mock_token_response = %{
  access_token: "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...",
  token_type: "Bearer",
  expires_in: 3600,
  scope: "mcp:read mcp:write"
}

security_config = %{
  auth: {:oauth2, mock_token_response}
}

IO.puts("   ✓ OAuth token configured for transport")
IO.puts("   → Token type: #{mock_token_response.token_type}")
IO.puts("   → Scope: #{mock_token_response.scope}")
IO.puts("   → Ready for use with ExMCP.Client")

IO.puts("\n   Example client configuration:")
IO.puts("""
   {:ok, client} = ExMCP.Client.start_link(
     transport: :http,
     url: "https://mcp-server.example.com",
     security: security_config
   )
""")

IO.puts("\n✅ OAuth 2.1 Authorization Example Complete!")
IO.puts("\nKey Security Features Demonstrated:")
IO.puts("• PKCE for authorization code flows (required)")
IO.puts("• HTTPS enforcement (with localhost exception)")
IO.puts("• Comprehensive token validation")
IO.puts("• Dynamic client registration")
IO.puts("• Server metadata discovery")
IO.puts("• Seamless integration with MCP transports")