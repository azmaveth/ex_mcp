# ExMCP Security Guide

This guide covers security features and best practices for the ExMCP library.

## Overview

ExMCP provides comprehensive security features to ensure secure communication between MCP clients and servers. The security model is designed to be flexible while maintaining strong defaults.

## Security Features by Component

### Transport Security

| Feature | Streamable HTTP | stdio | Native Service Dispatcher |
|---------|-----------------|-------|---------------------------|
| Bearer Authentication | ✅ | ❌ | Via process validation |
| API Key Authentication | ✅ | ❌ | Via process validation |
| Basic Authentication | ✅ | ❌ | ❌ |
| Custom Headers | ✅ | ❌ | ❌ |
| Origin Validation | ✅ | ❌ | N/A |
| CORS Headers | ✅ | ❌ | N/A |
| TLS/SSL | ✅ | ❌ | Via Erlang distribution* |
| Mutual TLS | ✅ | ❌ | ❌ |
| OAuth 2.1 | ✅ | ❌ | Via process validation |

*Native Service Dispatcher uses Erlang distribution security between nodes

## Authentication Methods

### Bearer Token Authentication

Used for OAuth2 and JWT tokens:

```elixir
security = %{
  auth: {:bearer, "your-bearer-token"}
}

{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  security: security
)
```

### API Key Authentication

For API key-based authentication:

```elixir
# Default header (X-API-Key)
security = %{
  auth: {:api_key, "your-api-key", []}
}

# Custom header
security = %{
  auth: {:api_key, "your-api-key", header: "X-Custom-API-Key"}
}
```

### Basic Authentication

HTTP Basic Authentication:

```elixir
security = %{
  auth: {:basic, "username", "password"}
}
```

### Custom Headers

For custom authentication schemes:

```elixir
security = %{
  auth: {:custom, [
    {"X-Auth-Token", "token"},
    {"X-Request-ID", "request-id"}
  ]}
}
```

### OAuth 2.1 Authentication

For OAuth 2.1 authentication flows:

```elixir
# First, obtain tokens via OAuth flow
{:ok, token_response} = ExMCP.Authorization.client_credentials_flow(%{
  client_id: "my-client",
  client_secret: "my-secret",
  token_endpoint: "https://auth.example.com/token"
})

# Then use the token for authentication
security = %{
  auth: {:oauth2, token_response}
}
```

## Transport-Specific Security

### Streamable HTTP Transport Security

The Streamable HTTP transport (with Server-Sent Events) supports the most comprehensive security features:

```elixir
security = %{
  # Authentication
  auth: {:bearer, "token"},
  
  # Origin validation
  validate_origin: true,
  allowed_origins: ["https://app.example.com"],
  
  # CORS configuration
  cors: %{
    allowed_origins: ["https://app.example.com"],
    allowed_methods: ["GET", "POST"],
    allowed_headers: ["Content-Type", "Authorization"],
    allow_credentials: true,
    max_age: 3600
  },
  
  # Custom headers
  headers: [
    {"X-Client-Version", "1.0.0"},
    {"X-Request-Source", "mobile-app"}
  ],
  
  # Include standard security headers
  include_security_headers: true,
  
  # TLS configuration
  tls: %{
    verify: :verify_peer,
    cacerts: :public_key.cacerts_get(),
    versions: [:"tlsv1.2", :"tlsv1.3"],
    cert: cert_data,  # For mutual TLS
    key: key_data     # For mutual TLS
  }
}
```


### Native Service Dispatcher Security

The Native Service Dispatcher (ExMCP.Native) provides high-performance communication between Elixir services within the same cluster. Security is handled at the Erlang distribution and process levels:

```elixir
# Services can implement their own authentication
defmodule MySecureService do
  use ExMCP.Service, name: :secure_service
  
  def init(args) do
    # Initialize with expected tokens
    {:ok, %{authorized_tokens: MapSet.new(args[:tokens] || [])}}
  end
  
  def handle_mcp_request(method, params, state) do
    # Validate authentication token from metadata
    case Map.get(params, "_meta", %{}) |> Map.get("auth_token") do
      nil -> 
        {:error, %{"code" => -32001, "message" => "Authentication required"}, state}
      
      token ->
        if MapSet.member?(state.authorized_tokens, token) do
          # Process authenticated request
          handle_authenticated_request(method, params, state)
        else
          {:error, %{"code" => -32002, "message" => "Invalid token"}, state}
        end
    end
  end
end

# Client usage with authentication
{:ok, result} = ExMCP.Native.call(
  :secure_service,
  "method_name",
  %{"data" => "value"},
  meta: %{"auth_token" => "secret-token"}
)
```

#### Erlang Distribution Security

For production deployments, secure the Erlang distribution layer:

```elixir
# 1. Set a strong cookie in your release configuration
# config/runtime.exs
config :my_app,
  erlang_cookie: System.fetch_env!("ERLANG_COOKIE")

# 2. Use TLS for distribution (in vm.args)
# -proto_dist inet_tls
# -ssl_dist_optfile /path/to/ssl_dist.conf

# 3. Configure node names to use fully qualified domain names
# -name myapp@secure.internal.network
```

#### Native Service Security Best Practices

1. **Node Security**: Use strong Erlang cookies and consider TLS distribution
2. **Process Isolation**: Each service runs in its own supervised process
3. **Network Isolation**: Deploy clusters on private networks
4. **Authentication**: Implement service-level authentication for sensitive operations
5. **Monitoring**: Use OTP supervision and monitoring for security events

## Security Best Practices

### 1. Always Use TLS in Production

```elixir
# Good - uses HTTPS/WSS
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  security: %{auth: {:bearer, token}}
)

# Bad - unencrypted HTTP
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://api.example.com",  # Insecure!
  security: %{auth: {:bearer, token}}
)
```

### 2. Validate Origins for Web-Based Clients

```elixir
security = %{
  validate_origin: true,
  allowed_origins: [
    "https://app.mycompany.com",
    "https://staging.mycompany.com"
  ]
}
```

### 3. Use Strong Authentication

```elixir
# Good - strong token
security = %{
  auth: {:bearer, "eyJhbGciOiJIUzI1NiIs..."}  # JWT or strong token
}

# Bad - weak token
security = %{
  auth: {:bearer, "simple-password"}  # Avoid simple passwords
}
```

### 4. Configure CORS Properly

```elixir
cors = %{
  # Specific origins instead of wildcard
  allowed_origins: ["https://app.example.com"],
  
  # Only required methods
  allowed_methods: ["GET", "POST"],
  
  # Only required headers
  allowed_headers: ["Content-Type", "Authorization"],
  
  # Enable credentials only if needed
  allow_credentials: true
}
```

### 5. Use Certificate Validation

```elixir
tls = %{
  # Always verify certificates in production
  verify: :verify_peer,
  
  # Use system CA certificates
  cacerts: :public_key.cacerts_get(),
  
  # Use modern TLS versions
  versions: [:"tlsv1.2", :"tlsv1.3"]
}
```

## Error Handling

Security-related errors are returned with descriptive error tuples:

```elixir
case ExMCP.Client.start_link(transport: :http, url: url, security: security) do
  {:ok, client} ->
    # Success
    client
    
  {:error, :invalid_auth_config} ->
    # Invalid authentication configuration
    handle_auth_error()
    
  {:error, :origin_not_allowed} ->
    # Origin validation failed
    handle_origin_error()
    
  {:error, :connection_refused} ->
    # Server refused connection (possibly auth failure)
    handle_connection_error()
    
  {:error, reason} ->
    # Other errors
    handle_generic_error(reason)
end
```

## Testing Security Configuration

ExMCP provides utilities for testing security configurations:

```elixir
# Validate security configuration
case ExMCP.Security.validate_config(security_config) do
  :ok -> 
    # Configuration is valid
    proceed_with_connection()
    
  {:error, reason} ->
    # Configuration is invalid
    fix_configuration(reason)
end

# Test authentication headers
headers = ExMCP.Security.build_auth_headers(security_config)
assert {"Authorization", "Bearer token"} in headers
```

## Security Considerations

### 1. Token Management

- Store tokens securely (encrypted at rest)
- Rotate tokens regularly
- Use short-lived tokens when possible
- Implement token refresh mechanisms

### 2. Network Security

- Always use TLS for production traffic
- Consider using mutual TLS for high-security environments
- Implement rate limiting at the network level
- Use VPNs or private networks when possible

### 3. Process Security (BEAM Transport)

- Use Erlang node cookies for basic node authentication
- Consider network-level isolation for distributed nodes
- Monitor process connections and authentication attempts

### 4. Error Handling

- Don't leak sensitive information in error messages
- Log authentication failures for monitoring
- Implement exponential backoff for failed auth attempts

### 5. Monitoring and Auditing

- Log all authentication attempts
- Monitor for unusual connection patterns
- Track API usage and rate limits
- Set up alerts for security events

## Example: Complete Secure Setup

Here's a complete example of a secure MCP client setup:

```elixir
defmodule SecureMCPClient do
  def start_secure_client do
    # Load configuration securely
    token = System.get_env("MCP_AUTH_TOKEN") || load_from_secure_store()
    server_url = System.get_env("MCP_SERVER_URL") || "https://api.company.com"
    
    security = %{
      # Strong authentication
      auth: {:bearer, token},
      
      # Origin validation
      validate_origin: true,
      allowed_origins: ["https://app.company.com"],
      
      # Secure headers
      headers: [
        {"X-Client-Version", Application.spec(:my_app, :vsn)},
        {"X-Request-ID", generate_request_id()}
      ],
      
      # TLS configuration
      tls: %{
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.3"]
      },
      
      # CORS for web clients
      cors: %{
        allowed_origins: ["https://app.company.com"],
        allowed_methods: ["GET", "POST"],
        allow_credentials: true
      }
    }
    
    # Validate configuration
    case ExMCP.Security.validate_config(security) do
      :ok ->
        ExMCP.Client.start_link(
          transport: :http,
          url: server_url,
          security: security
        )
        
      {:error, reason} ->
        {:error, {:security_config_invalid, reason}}
    end
  end
  
  defp load_from_secure_store do
    # Implement secure token loading
    # e.g., from encrypted file, HSM, or vault
  end
  
  defp generate_request_id do
    # Generate unique request ID for tracing
    :crypto.strong_rand_bytes(16) |> Base.encode64()
  end
end
```

## Troubleshooting

### Common Security Issues

1. **Authentication Failures**
   - Verify token format and validity
   - Check server authentication requirements
   - Ensure headers are properly formatted

2. **Origin Validation Errors**
   - Verify allowed origins configuration
   - Check client origin header
   - Ensure proper CORS setup

3. **TLS/Certificate Issues**
   - Verify certificate validity
   - Check CA certificate configuration
   - Ensure proper hostname verification

4. **Connection Refused**
   - Check authentication credentials
   - Verify server security requirements
   - Check network connectivity and firewall rules

### Debugging Security Issues

Enable debug logging to troubleshoot security issues:

```elixir
# Enable debug logging
Logger.configure(level: :debug)

# Check security configuration
IO.inspect(ExMCP.Security.validate_config(security))

# Check generated headers
IO.inspect(ExMCP.Security.build_security_headers(security))
```

## Migration Guide

### Upgrading from Unsecured Connections

1. **Add authentication**:
   ```elixir
   # Before
   {:ok, client} = ExMCP.Client.start_link(transport: :http, url: url)
   
   # After
   {:ok, client} = ExMCP.Client.start_link(
     transport: :http, 
     url: url, 
     security: %{auth: {:bearer, token}}
   )
   ```

2. **Enable TLS**:
   ```elixir
   # Change HTTP to HTTPS
   url = "https://api.example.com"  # was "http://..."
   ```

3. **Add origin validation**:
   ```elixir
   security = %{
     auth: {:bearer, token},
     validate_origin: true,
     allowed_origins: ["https://yourapp.com"]
   }
   ```

## Summary

ExMCP provides comprehensive security features across all transports:

- **Streamable HTTP Transport**: Full authentication, TLS, CORS, and origin validation support
- **stdio Transport**: Limited security (process isolation only)
- **Native Service Dispatcher**: Process-level security with Erlang distribution protection

Choose the appropriate transport based on your security requirements. For maximum security, use the Streamable HTTP transport with TLS and proper authentication.
