# OAuth 2.1 Resource Server Implementation Plan

## Executive Summary

ExMCP requires server-side OAuth 2.1 Resource Server implementation to achieve full MCP 2025-06-18 specification compliance. While the library has excellent client-side OAuth infrastructure, critical server-side components are missing that prevent classification as an OAuth 2.1 Resource Server.

**Current Compliance Status**: ~54% for MCP 2025-06-18  
**Target Compliance Status**: 90%+ for MCP 2025-06-18  
**Estimated Implementation Time**: 2 weeks

## Current OAuth 2.1 Implementation State

### Strengths (Client-Side)

ExMCP has comprehensive OAuth 2.1 client implementation:

- **Authorization Flows**: Complete PKCE authorization code flow and client credentials flow (`Authorization.ex`)
- **Token Management**: Full lifecycle management with automatic refresh (`TokenManager.ex`) 
- **Dynamic Registration**: RFC 7591 client registration support (`ClientRegistration.ex`)
- **Metadata Discovery**: RFC 8414 server metadata discovery (`ProtectedResourceMetadata.ex`)
- **Request Interceptor**: Automatic authorization header injection (`Interceptor.ex`)
- **Error Handling**: Proper OAuth error response handling (`ErrorHandler.ex`)
- **Security**: HTTPS enforcement, PKCE validation, secure state generation

### Critical Gaps (Server-Side)

**Missing OAuth 2.1 Resource Server Classification Components:**

1. **Authorization Middleware**: No bearer token validation for incoming MCP requests
2. **Resource Metadata Endpoint**: Missing `/.well-known/oauth-protected-resource` discovery endpoint
3. **Protocol Header Validation**: No `MCP-Protocol-Version` header validation in HTTP transport
4. **Capability Advertisement**: Server capabilities don't advertise OAuth support
5. **Scope Validation Infrastructure**: No systematic scope checking for MCP operations
6. **Bearer Token Processing**: `HttpPlug.ex` lacks authorization header extraction and validation

## Implementation Strategy

### Phase 1: Core Authorization Infrastructure (Week 1)

#### 1.1 Server Authorization Guard
**File**: `lib/ex_mcp/authorization/server_guard.ex`
**Purpose**: Bearer token validation middleware for all MCP requests

```elixir
defmodule ExMCP.Authorization.ServerGuard do
  @moduledoc """
  OAuth 2.1 Resource Server authorization middleware.
  
  Validates bearer tokens and enforces scope-based access control
  for MCP requests according to 2025-06-18 specification.
  """
  
  @doc """
  Validates authorization header and extracts token claims.
  """
  def validate_request(conn, auth_config)
  
  @doc """
  Checks if token has required scopes for MCP operation.
  """
  def authorize_scope(claims, required_scopes)
end
```

**Key Features:**
- Bearer token extraction from Authorization header
- Token introspection with authorization server
- Scope validation for MCP operations
- Integration with existing `TokenManager` infrastructure

#### 1.2 HTTP Plug Enhancement
**File**: `lib/ex_mcp/http_plug.ex` (modifications)
**Purpose**: Integrate authorization middleware into request processing

**Changes Required:**
- Add `MCP-Protocol-Version` header validation for 2025-06-18 compliance
- Integrate `ServerGuard` for bearer token validation
- Add authorization configuration options
- Implement proper 401/403 error responses with WWW-Authenticate headers

#### 1.3 Resource Metadata Endpoint
**File**: `lib/ex_mcp/http_plug.ex` (additions)
**Purpose**: Implement `/.well-known/oauth-protected-resource` endpoint

```elixir
def call(%Plug.Conn{method: "GET", path_info: [".well-known", "oauth-protected-resource"]} = conn, opts) do
  metadata = %{
    "authorization_servers" => [
      %{
        "issuer" => opts.authorization_server,
        "scopes_supported" => ["mcp:read", "mcp:write", "mcp:admin"]
      }
    ]
  }
  
  conn
  |> put_resp_content_type("application/json")
  |> send_resp(200, Jason.encode!(metadata))
end
```

### Phase 2: Integration and Configuration (Week 1-2)

#### 2.1 Server Capability Advertisement
**File**: `lib/ex_mcp/protocol/version_negotiator.ex` (modifications)
**Purpose**: Advertise OAuth support in server capabilities

```elixir
def build_capabilities(negotiated_version, opts) do
  base_capabilities = %{
    experimental: %{protocolVersionHeader: true}
  }
  
  # Add OAuth capability for 2025-06-18
  if negotiated_version == "2025-06-18" and FeatureFlags.enabled?(:oauth2_auth) do
    put_in(base_capabilities, [:experimental, :oauth2], %{
      resourceServer: true,
      scopesSupported: ["mcp:read", "mcp:write", "mcp:admin"]
    })
  else
    base_capabilities
  end
end
```

#### 2.2 Authorization Configuration
**File**: `lib/ex_mcp/authorization/server_config.ex` (new)
**Purpose**: Centralized OAuth server configuration

```elixir
defmodule ExMCP.Authorization.ServerConfig do
  @moduledoc """
  OAuth 2.1 Resource Server configuration management.
  """
  
  defstruct [
    :authorization_server,
    :introspection_endpoint,
    :required_scopes,
    :token_cache_ttl,
    :enforce_https
  ]
  
  def from_application_env()
  def validate_config(config)
end
```

#### 2.3 Scope-Based Access Control
**File**: `lib/ex_mcp/authorization/scope_validator.ex` (new)
**Purpose**: Map MCP operations to required OAuth scopes

```elixir
defmodule ExMCP.Authorization.ScopeValidator do
  @doc """
  Returns required scopes for MCP method.
  """
  def required_scopes("tools/call"), do: ["mcp:write"]
  def required_scopes("tools/list"), do: ["mcp:read"]
  def required_scopes("resources/read"), do: ["mcp:read"]
  def required_scopes("resources/subscribe"), do: ["mcp:write"]
  def required_scopes(_method), do: ["mcp:read"]
end
```

### Phase 3: Testing and Validation (Week 2)

#### 3.1 Comprehensive Test Suite
**Files**: 
- `test/ex_mcp/authorization/server_guard_test.exs`
- `test/ex_mcp/authorization/scope_validator_test.exs`
- `test/ex_mcp/http_plug_oauth_test.exs`

**Test Coverage:**
- Bearer token validation (valid, invalid, expired tokens)
- Scope enforcement for different MCP operations
- Protocol header validation
- Resource metadata endpoint discovery
- Integration with existing client-side OAuth flows
- Error response formats (401, 403 with proper WWW-Authenticate headers)

#### 3.2 Compliance Validation
**File**: `test/ex_mcp/compliance/oauth_compliance_test.exs`
**Purpose**: Ensure full OAuth 2.1 Resource Server compliance

```elixir
defmodule ExMCP.Compliance.OAuthComplianceTest do
  @moduledoc """
  Tests OAuth 2.1 Resource Server compliance for MCP 2025-06-18.
  """
  
  test "server advertises OAuth capability in initialization"
  test "/.well-known/oauth-protected-resource endpoint returns valid metadata" 
  test "bearer token validation follows RFC 6750"
  test "scope enforcement prevents unauthorized access"
  test "MCP-Protocol-Version header validation for 2025-06-18"
end
```

## Integration Points

### Existing Infrastructure Leverage

**TokenManager Integration:**
- Reuse token introspection logic from `TokenManager.validate_token/2`
- Leverage existing HTTP client infrastructure for authorization server communication
- Use existing HTTPS validation and security patterns

**Feature Flag Integration:**
- Use existing `:oauth2_auth` feature flag in `FeatureFlags.ex`
- Enable gradual rollout with backward compatibility
- Allow development/testing without OAuth requirements

**Transport Integration:**
- Build on existing `HttpPlug` architecture
- Maintain compatibility with SSE and other transports
- Preserve existing CORS and error handling patterns

### Configuration Integration

**Application Environment:**
```elixir
config :ex_mcp,
  oauth2_enabled: true,
  authorization_server: "https://auth.example.com",
  introspection_endpoint: "https://auth.example.com/introspect",
  required_scopes: %{
    "tools/call" => ["mcp:write"],
    "tools/list" => ["mcp:read"],
    "resources/read" => ["mcp:read"]
  }
```

## Security Considerations

### Token Security
- **Bearer Token Handling**: Secure extraction, validation, and caching
- **HTTPS Enforcement**: All OAuth endpoints must use HTTPS (except localhost)
- **Token Introspection**: Cache introspection results with appropriate TTL
- **Scope Validation**: Strict enforcement of least-privilege access

### Error Handling
- **Information Disclosure**: Prevent token details leakage in error responses
- **WWW-Authenticate Headers**: Proper OAuth error responses per RFC 6750
- **Audit Logging**: Log authorization attempts for security monitoring

## Performance Considerations

### Token Validation Optimization
- **Introspection Caching**: Cache valid token introspection results (5-15 minutes TTL)
- **Connection Pooling**: Reuse HTTP connections to authorization server
- **Async Validation**: Non-blocking token validation where possible
- **Circuit Breaker**: Fail-fast for unavailable authorization server

### Scalability
- **Stateless Design**: No server-side session storage required
- **Horizontal Scaling**: OAuth validation works across multiple server instances
- **Load Balancing**: Compatible with load-balanced deployments

## Migration Strategy

### Backward Compatibility
- **Feature Flag Control**: OAuth enforcement disabled by default
- **Gradual Rollout**: Enable OAuth per-environment or per-deployment
- **Fallback Behavior**: Graceful degradation when OAuth unavailable
- **Documentation**: Clear migration guide for existing deployments

### Development Experience
- **Local Development**: Allow bypass for localhost/development environments
- **Testing Support**: Mock authorization server for integration tests
- **Configuration Validation**: Clear error messages for misconfiguration
- **Debug Logging**: Detailed OAuth flow logging for troubleshooting

## Success Metrics

### Compliance Targets
- **MCP 2025-06-18 Compliance**: Achieve 90%+ compliance score
- **OAuth 2.1 Conformance**: Pass OAuth 2.1 Resource Server compliance tests
- **Security Validation**: Zero critical security issues in OAuth implementation

### Quality Metrics
- **Test Coverage**: 90%+ coverage for OAuth-related code
- **Performance**: <50ms overhead for token validation
- **Documentation**: Complete OAuth setup and configuration guide

### Integration Metrics
- **Client Compatibility**: Support for all major OAuth 2.1 clients
- **Transport Compatibility**: OAuth works with HTTP, SSE transports
- **Error Handling**: Proper OAuth error responses in all scenarios

## Risk Mitigation

### Implementation Risks
- **Breaking Changes**: OAuth requirements may break existing clients
  - *Mitigation*: Feature flag control, backward compatibility mode
- **Performance Impact**: Token validation adds request latency  
  - *Mitigation*: Caching, connection pooling, circuit breaker
- **Security Vulnerabilities**: OAuth implementation bugs create attack vectors
  - *Mitigation*: Comprehensive testing, security audit, established patterns

### Operational Risks
- **Authorization Server Dependency**: OAuth server outage breaks MCP access
  - *Mitigation*: Circuit breaker, graceful degradation, monitoring
- **Configuration Complexity**: OAuth setup complexity reduces adoption
  - *Mitigation*: Clear documentation, validation helpers, examples
- **Token Management**: Token expiration handling complexity
  - *Mitigation*: Leverage existing TokenManager patterns, proper error handling

## Timeline

### Week 1
- **Days 1-2**: Implement `ServerGuard` and basic token validation
- **Days 3-4**: Enhance `HttpPlug` with OAuth integration and protocol header validation
- **Day 5**: Implement resource metadata endpoint and capability advertisement

### Week 2  
- **Days 1-2**: Add scope validation and configuration management
- **Days 3-4**: Comprehensive testing and compliance validation
- **Day 5**: Documentation, examples, and migration guide

### Deliverables
- **Code**: All OAuth 2.1 Resource Server components implemented
- **Tests**: 90%+ test coverage with compliance validation
- **Documentation**: Setup guide, configuration reference, migration instructions
- **Compliance**: 90%+ MCP 2025-06-18 compliance score

## Conclusion

This implementation plan leverages ExMCP's excellent existing OAuth 2.1 client infrastructure to add the missing server-side components required for MCP 2025-06-18 compliance. The modular design and feature flag system enable gradual rollout while maintaining backward compatibility.

The focus on security-first implementation, comprehensive testing, and clear documentation ensures production-ready OAuth 2.1 Resource Server classification that integrates seamlessly with the existing ExMCP architecture.