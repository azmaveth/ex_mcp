# ExMCP Security Guide

This guide covers security features and best practices for the ExMCP library.

## Overview

ExMCP provides comprehensive security features across all transports to ensure secure communication between MCP clients and servers. The security model is designed to be flexible while maintaining strong defaults.

## Security Features by Transport

| Feature | SSE | WebSocket | BEAM | stdio |
|---------|-----|-----------|------|-------|
| Bearer Authentication | ✅ | ✅ | ✅ | ❌ |
| API Key Authentication | ✅ | ✅ | ✅ | ❌ |
| Basic Authentication | ✅ | ✅ | ❌ | ❌ |
| Custom Headers | ✅ | ✅ | ❌ | ❌ |
| Origin Validation | ✅ | ✅ | ❌ | ❌ |
| CORS Headers | ✅ | ❌ | ❌ | ❌ |
| TLS/SSL | ✅ | ✅ | ✅* | ❌ |
| Mutual TLS | ✅ | ✅ | ❌ | ❌ |
| Node Cookie Auth | ❌ | ❌ | ✅ | ❌ |

*BEAM transport uses Erlang distribution security

## Authentication Methods

### Bearer Token Authentication

Used for OAuth2 and JWT tokens:

```elixir
security = %{
  auth: {:bearer, "your-bearer-token"}
}

{:ok, client} = ExMCP.Client.start_link(
  transport: :sse,
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

### Node Cookie Authentication (BEAM only)

For Erlang distribution security:

```elixir
security = %{
  auth: {:node_cookie, :secret_cookie}
}
```

## Transport-Specific Security

### SSE Transport Security

The SSE transport supports the most comprehensive security features:

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

### WebSocket Transport Security

WebSocket transport supports authentication and TLS:

```elixir
security = %{
  # Authentication
  auth: {:bearer, "ws-token"},
  
  # Custom headers for handshake
  headers: [{"X-Client-ID", "client123"}],
  
  # TLS for WSS connections
  tls: %{
    verify: :verify_peer,
    versions: [:"tlsv1.3"]
  }
}

{:ok, client} = ExMCP.Client.start_link(
  transport: :websocket,
  url: "wss://secure.example.com/mcp",
  security: security
)
```

### BEAM Transport Security

BEAM transport provides process-level security:

```elixir
# Client security
security = %{
  auth: {:bearer, "beam-token"}
}

{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: {:secure_server, :node@hostname},
  security: security
)

# Server security
{:ok, server} = ExMCP.Server.start_link(
  transport: :beam,
  name: :secure_server,
  handler: MyHandler,
  security: %{
    auth: {:bearer, "expected-token"}
  }
)
```

## Security Best Practices

### 1. Always Use TLS in Production

```elixir
# Good - uses HTTPS/WSS
{:ok, client} = ExMCP.Client.start_link(
  transport: :sse,
  url: "https://api.example.com",
  security: %{auth: {:bearer, token}}
)

# Bad - unencrypted HTTP
{:ok, client} = ExMCP.Client.start_link(
  transport: :sse,
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
case ExMCP.Client.start_link(transport: :sse, url: url, security: security) do
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
          transport: :sse,
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
   {:ok, client} = ExMCP.Client.start_link(transport: :sse, url: url)
   
   # After
   {:ok, client} = ExMCP.Client.start_link(
     transport: :sse, 
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

This completes the security implementation for ExMCP. The library now provides comprehensive security features across all transports with proper documentation and testing.