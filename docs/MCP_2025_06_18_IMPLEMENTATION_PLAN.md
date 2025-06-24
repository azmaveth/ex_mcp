# ExMCP 2025-06-18 Implementation Plan

## Executive Summary

This document outlines the implementation strategy for three critical MCP 2025-06-18 specification features:
- HTTP MCP-Protocol-Version Header (Low complexity)
- Structured Tool Output (Medium complexity)  
- OAuth 2.1 Authorization (High complexity)

The approach prioritizes security, backwards compatibility, and phased rollout to minimize risk.

## Feature Overview

### 1. HTTP MCP-Protocol-Version Header
- **Purpose**: Ensure protocol version clarity in all HTTP requests
- **Complexity**: Very Low (days)
- **Key Requirements**: 
  - Validate header on all requests post-initialization
  - Default to "2025-06-18" if missing
  - Return 400 for unsupported versions

### 2. Structured Tool Output
- **Purpose**: Enable typed, validated tool responses
- **Complexity**: Low (1-2 weeks)
- **Key Requirements**:
  - Add `structuredContent` field alongside `content`
  - Implement `outputSchema` validation
  - Maintain backwards compatibility

### 3. OAuth 2.1 Authorization
- **Purpose**: Secure, delegated authorization
- **Complexity**: High (4-6 weeks)
- **Key Requirements**:
  - Resource server metadata endpoints
  - PKCE implementation
  - Dynamic client registration
  - Resource indicators (RFC 8707)

## Implementation Phases

### Phase 1: Foundation (Week 1)

#### Module Structure
```
lib/ex_mcp/
├── feature_flags.ex          # Feature toggle system
├── plugs/
│   └── protocol_version.ex   # Header validation
└── protocol/
    └── version_negotiator.ex # Version negotiation
```

#### Feature Flag System
```elixir
defmodule ExMCP.FeatureFlags do
  def enabled?(:protocol_version_header), do: Application.get_env(:ex_mcp, :protocol_version_required, false)
  def enabled?(:structured_output), do: Application.get_env(:ex_mcp, :structured_output_enabled, false)
  def enabled?(:oauth2_auth), do: Application.get_env(:ex_mcp, :oauth2_enabled, false)
end
```

#### Protocol Version Plug
```elixir
defmodule ExMCP.Plugs.ProtocolVersion do
  import Plug.Conn
  
  @supported_versions ["2025-06-18", "2025-03-26"]
  
  def init(opts), do: opts
  
  def call(conn, _opts) do
    case get_req_header(conn, "mcp-protocol-version") do
      [] -> assign(conn, :mcp_version, "2025-06-18") # Default per spec
      [version] when version in @supported_versions ->
        assign(conn, :mcp_version, version)
      [invalid] ->
        conn
        |> send_resp(400, Jason.encode!(%{error: "Unsupported protocol version: #{invalid}"}))
        |> halt()
    end
  end
end
```

### Phase 2: Structured Output (Weeks 2-3)

#### Module Structure
```
lib/ex_mcp/
├── tools/
│   ├── response_builder.ex   # Dual-format responses
│   └── schema_validator.ex   # JSON Schema validation
└── types/
    └── tool_response.ex      # Updated types
```

#### Key Features
- Dual-format responses (content + structuredContent)
- JSON Schema validation using ex_json_schema
- Backwards compatibility guaranteed
- Schema caching for performance

### Phase 3: OAuth 2.1 Core (Weeks 4-7)

#### Module Structure
```
lib/ex_mcp/
├── authorization/
│   ├── oauth_client.ex       # OAuth client implementation
│   ├── token_manager.ex      # Token storage and validation
│   ├── pkce.ex              # PKCE implementation
│   └── resource_metadata.ex  # Resource server metadata
├── plugs/
│   └── oauth_validator.ex    # Request authentication
```

#### PKCE Implementation
```elixir
defmodule ExMCP.Authorization.PKCE do
  @spec generate_verifier() :: String.t()
  def generate_verifier do
    :crypto.strong_rand_bytes(32) |> Base.url_encode64(padding: false)
  end
  
  @spec generate_challenge(String.t()) :: String.t()
  def generate_challenge(verifier) do
    :crypto.hash(:sha256, verifier) |> Base.url_encode64(padding: false)
  end
end
```

#### Resource Server Metadata
```json
{
  "resource": "https://mcp.example.com",
  "authorization_servers": ["https://auth.example.com"],
  "bearer_methods_supported": ["header"],
  "resource_documentation": "https://docs.example.com/mcp"
}
```

### Phase 4: OAuth 2.1 Advanced (Weeks 8-9)
- Dynamic client registration
- Resource indicators implementation
- Token refresh and revocation
- Security audit preparation

### Phase 5: Integration & Migration (Week 10)
- Full integration testing
- Migration guide and tooling
- Performance optimization
- Documentation completion

## Security Considerations

### OAuth 2.1 Security Measures
1. **Token Validation**:
   - ETS-based cache with TTL
   - Constant-time comparison
   - Background introspection

2. **Rate Limiting**:
   - 10 requests/second per client
   - Circuit breaker for auth server

3. **Audit & Monitoring**:
   - All auth events logged
   - Failed attempt spike detection
   - Token validation metrics

4. **OWASP Compliance**:
   - PKCE mandatory
   - State parameter validation
   - Secure token storage

## Migration Strategy

### Compatibility Layer
```elixir
defmodule ExMCP.Compatibility do
  def handle_request(conn) do
    version = conn.assigns[:mcp_version] || detect_legacy_client(conn)
    
    case version do
      "2025-03-26" -> handle_legacy_request(conn)
      "2025-06-18" -> handle_current_request(conn)
      _ -> {:error, :unsupported_version}
    end
  end
  
  defp detect_legacy_client(conn) do
    # Check for batch requests, missing headers, etc.
    cond do
      is_batch_request?(conn) -> "2025-03-26"
      true -> "2025-06-18"
    end
  end
end
```

### Phased Rollout
1. **Phase 1** (Weeks 1-2): Deploy with all features disabled
2. **Phase 2** (Week 3): Enable protocol version header (log-only)
3. **Phase 3** (Week 4): Enable structured output for opt-in
4. **Phase 4** (Weeks 5-8): OAuth 2.1 in parallel mode
5. **Phase 5** (Week 9): Enforce new features for new clients
6. **Phase 6** (Week 12): Deprecation warnings for legacy

### Migration Tools
- Batch-to-sequential request converter
- OAuth migration wizard
- Automated compatibility checker
- Performance comparison dashboard

## Testing Strategy

### Test Coverage by Feature

#### Protocol Version Tests
- Unit: Plug validation logic
- Integration: Full request cycle
- Edge cases: Missing/invalid headers
- Property tests: Random version strings

#### Structured Output Tests
- Unit: Schema validation
- Integration: Tool call responses
- Compatibility: Dual-format support
- Fuzz testing: Invalid schemas

#### OAuth 2.1 Test Suite
- Unit: PKCE, token validation
- Integration: Full auth flow
- Security: Replay attacks, CSRF
- Load: High concurrency validation

### Security Testing
- Static analysis (Sobelow)
- OWASP checklist compliance
- Third-party security audit
- Penetration testing

## Success Metrics

### Technical KPIs
- Zero OAuth security vulnerabilities
- <50ms token validation latency (P99)
- 100% backwards compatibility (3 months)
- Zero unplanned migration downtime
- >95% schema validation success rate

### Business KPIs
- 50% protocol adoption (6 months)
- 80% OAuth usage (new integrations)
- <5% migration support tickets

## Monitoring & Observability

### Required Dashboards
1. Protocol version distribution
2. OAuth flow success rates
3. Token validation performance
4. Structured output usage
5. Deprecation warning tracking

### Alert Configuration
- Auth failure spike (>10x baseline)
- Token validation >100ms
- Schema validation failures >10%
- Unknown protocol versions

## Deliverables

### Documentation
1. Complete API reference (2025-06-18)
2. OAuth 2.1 integration guide
3. Migration cookbook with examples
4. Security best practices

### Tools & Libraries
1. Updated ExMCP client SDK
2. Migration CLI tool
3. OAuth testing sandbox
4. Performance benchmark suite

### Training Materials
1. Video walkthrough series
2. Interactive OAuth demo
3. Workshop materials

## Risk Mitigation

### High-Risk Areas
1. **OAuth Security**: External audit + pen testing
2. **Breaking Changes**: Extensive compatibility layer
3. **Performance**: Continuous benchmarking
4. **Migration**: Phased rollout + support

## Implementation Checklist

### Week 1 Tasks
- [ ] Implement feature flag system
- [ ] Create protocol version plug
- [ ] Set up version negotiation
- [ ] Write comprehensive tests
- [ ] Deploy to staging environment

### Immediate Actions
1. Configure feature flag system
2. Create protocol version plug skeleton
3. Research OAuth libraries (POC)
4. Set up monitoring infrastructure

## Dependencies

### Libraries to Evaluate
- `ex_json_schema` - JSON Schema validation
- `oauth2` - OAuth 2.1 client
- `ueberauth` - Authentication framework
- `nimble_options` - Configuration validation

### External Requirements
- Security audit service
- Load testing infrastructure
- Documentation platform
- Training video hosting

## Notes for Implementation

### Batch Request Removal
The 2025-06-18 spec removes batch request support. Implementation must:
- Detect batch requests from legacy clients
- Return appropriate error with migration guidance
- Provide batch-to-sequential converter tool
- Track batch request usage for deprecation

### Security First Approach
All OAuth implementation decisions should prioritize security:
- Use well-vetted libraries
- Implement defense in depth
- Plan for security audit from day one
- Consider hiring OAuth expert consultant

### Performance Considerations
- Token validation must be fast (<50ms)
- Use ETS for token caching
- Implement circuit breakers
- Monitor all critical paths

## Conclusion

This plan provides a comprehensive roadmap for implementing the 2025-06-18 MCP features while maintaining security, performance, and backwards compatibility. The phased approach minimizes risk while enabling gradual adoption of new capabilities.