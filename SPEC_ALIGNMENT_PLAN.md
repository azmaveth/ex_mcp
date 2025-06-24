# ExMCP Specification Alignment Plan

## Executive Summary

ExMCP demonstrates excellent architectural foundations with a modular design, sophisticated transport abstraction, and strong performance characteristics. However, it suffers from **critical specification drift** where the implementation has not kept pace with the evolving MCP standard, particularly the 2025-06-18 version. This creates significant compliance gaps that undermine the core promise of a standardized protocol.

**Current Compliance Status:**
- 2024-11-05: ~63% coverage
- 2025-03-26: ~61% coverage  
- 2025-06-18: ~54% coverage (concerning decline)

## Critical Issues Requiring Immediate Action

### 1. Missing Message Validation Layer (CRITICAL)

**Issue**: No comprehensive JSON-RPC/MCP message validation exists in the codebase.

**Specification Violations**:
- **Null ID Validation**: MCP spec states IDs `MUST NOT` be `null`, but no validation exists
- **Request ID Uniqueness**: No session-based tracking to prevent ID reuse (spec violation)
- **Response Format**: Missing validation that responses have either `result` OR `error`, never both

**Current Implementation Gap**:
```elixir
# Current implementation in lib/ex_mcp/internal/protocol.ex
def parse_message(%{"jsonrpc" => "2.0", "method" => method, "params" => params, "id" => id}) do
  {:request, method, params, id}  # No validation of id
end
```

**Required Implementation**:
```elixir
defmodule ExMCP.Internal.MessageValidator do
  @moduledoc """
  Validates MCP messages according to specification requirements.
  """

  def validate_request(%{"id" => nil}) do
    {:error, %{code: -32600, message: "Request ID must not be null"}}
  end

  def validate_request(%{"id" => id}) when not is_binary(id) and not is_integer(id) do
    {:error, %{code: -32600, message: "Request ID must be string or integer"}}
  end

  def validate_request(%{"jsonrpc" => "2.0"} = request) do
    # Additional validations...
    {:ok, request}
  end

  def validate_response(%{"result" => _, "error" => _}) do
    {:error, %{code: -32603, message: "Response cannot contain both result and error"}}
  end

  def validate_response(%{"result" => result, "id" => id}) do
    {:ok, %{"result" => result, "id" => id}}
  end

  def validate_response(%{"error" => error, "id" => id}) do
    {:ok, %{"error" => error, "id" => id}}
  end
end
```

### 2. 2025-06-18 Specification Lag (HIGH PRIORITY)

**Issue**: Implementation significantly behind latest MCP specification.

**Missing Features**:

#### Structured Tool Output
- **Current**: Partial implementation in tests
- **Required**: Full `structuredContent` field validation
- **Specification**: Tools can return structured data alongside text content

#### Elicitation Support
- **Current**: Basic encoding exists but no full server-side implementation
- **Required**: Complete server-initiated user request capability
- **Specification**: Servers can request additional information from users during interactions

#### OAuth 2.1 Resource Server Classification
- **Current**: Minimal testing, missing metadata discovery
- **Required**: Full OAuth 2.1 Resource Server implementation
- **Specification**: MCP servers classified as OAuth Resource Servers with metadata discovery

#### HTTP Protocol Version Header
- **Current**: No validation of `MCP-Protocol-Version` header
- **Required**: Mandatory header validation for HTTP transport
- **Specification**: All HTTP requests must include negotiated protocol version

### 3. Batch Request Specification Conflict (MEDIUM PRIORITY)

**Issue**: ExMCP supports batching despite 2025-06-18 removing it.

**Evidence**:
- `docs/mcp-specs/2025-06-18/KeyChanges.md`: "Remove support for JSON-RPC batching"
- `lib/ex_mcp/internal/protocol.ex` still contains `encode_batch/1` function
- Tests still validate batch functionality against newer specs

**Impact**: Direct interoperability risk with compliant 2025-06-18 implementations.

## Implementation Roadmap

### Phase 1: Critical Compliance (Weeks 1-2)

#### Week 1: Message Validation Foundation
- [ ] **Create MessageValidator Module**
  - Implement `ExMCP.Internal.MessageValidator`
  - Add to message processing pipeline as first step
  - Validate request ID format and uniqueness
  - Validate response format (result XOR error)

- [ ] **Session-based ID Tracking**
  - Add ID tracking to transport layer state
  - Implement per-session uniqueness validation
  - Add cleanup for closed sessions

- [ ] **Update Message Processor**
  - Integrate validator into `ExMCP.MessageProcessor`
  - Ensure validation occurs before handler dispatch
  - Add proper error responses for validation failures

#### Week 2: Test Coverage and Validation
- [ ] **Implement Skipped Tests**
  - Remove `@tag :skip` from message validation tests
  - Implement all missing validation test cases
  - Update `MCP_COVERAGE_MATRIX.md` to reflect actual state

- [ ] **Batch Request Resolution**
  - Decision: Remove batching for 2025-06-18 compliance
  - Alternative: Implement version-specific behavior
  - Update tests and documentation accordingly

### Phase 2: 2025-06-18 Feature Parity (Weeks 3-6)

#### Week 3: Structured Tool Output
- [ ] **Complete Implementation**
  - Add `structuredContent` field validation
  - Implement schema validation for structured output
  - Add resource links in tool results

- [ ] **Update DSL Support**
  - Extend tool DSL to support structured output
  - Add validation for output schemas
  - Update examples and documentation

#### Week 4: HTTP Protocol Version Header
- [ ] **Header Validation**
  - Add `MCP-Protocol-Version` header requirement
  - Implement version validation in HTTP transport
  - Add proper error responses for missing/invalid headers

- [ ] **Transport Updates**
  - Update `ExMCP.HttpPlug` to validate headers
  - Add header injection for client requests
  - Update HTTP transport tests

#### Week 5: OAuth 2.1 Resource Server
- [ ] **Metadata Discovery**
  - Implement authorization server metadata endpoint
  - Add resource server classification
  - Implement scope validation

- [ ] **Enhanced Security**
  - Add comprehensive OAuth 2.1 testing
  - Implement token validation
  - Add security best practices validation

#### Week 6: Elicitation Support
- [ ] **Server-side Implementation**
  - Complete elicitation request handling
  - Add client-side elicitation support
  - Implement user interaction flows

- [ ] **Integration Testing**
  - Add comprehensive elicitation tests
  - Test client-server elicitation flows
  - Update compliance matrix

### Phase 3: Security Hardening (Weeks 7-8)

#### Week 7: HITL Implementation
- [ ] **User Consent Flows**
  - Implement Human-in-the-Loop approval mechanisms
  - Add consent UI abstractions
  - Create approval handler interfaces

- [ ] **Security Best Practices**
  - Complete HTTPS enforcement
  - Add comprehensive security validation
  - Implement security audit logging

#### Week 8: Final Validation
- [ ] **Comprehensive Testing**
  - Run full compliance test suite
  - Validate against all MCP specification versions
  - Update coverage matrix to 90%+ for 2025-06-18

- [ ] **Documentation Updates**
  - Update README with compliance status
  - Add migration guide for breaking changes
  - Update API documentation

## Architectural Strengths to Preserve

### Excellent Foundation
1. **Modular Design**: Clean separation between protocol, transport, and application layers
2. **Transport Abstraction**: Superior support for multiple transports (stdio, HTTP, native BEAM)
3. **Performance**: Native BEAM transport provides exceptional performance (~15μs local calls)
4. **Version Management**: Sophisticated version registry system for protocol negotiation
5. **Test Organization**: Well-structured compliance tests with clear tagging strategy

### DSL Migration Strategy
Continue the move from handler-based to DSL-based server implementation:
```elixir
# Preferred DSL approach
defmodule MyServer do
  use ExMCP.Server
  
  deftool "calculate" do
    meta do
      description("Performs calculations")
    end
    
    input_schema(%{
      type: "object",
      properties: %{
        expression: %{type: "string"}
      }
    })
    
    # 2025-06-18 feature: structured output
    output_schema(%{
      type: "object",
      properties: %{
        result: %{type: "number"},
        explanation: %{type: "string"}
      }
    })
  end
end
```

## Testing Strategy

### Compliance Test Enhancement
- [ ] **Remove Skip Tags**: Implement all `:skip` marked tests
- [ ] **Add Edge Cases**: Comprehensive edge case testing for each feature
- [ ] **Transport-Specific**: Complete transport requirement testing
- [ ] **Security Testing**: Add comprehensive security validation tests

### Automated Compliance Checking
- [ ] **Spec Validation**: Add automated spec compliance checking
- [ ] **Version Testing**: Ensure all protocol versions work correctly
- [ ] **Regression Prevention**: Add tests to prevent specification drift

## Success Metrics

### Compliance Targets
- **2024-11-05**: Maintain 90%+ coverage
- **2025-03-26**: Achieve 90%+ coverage  
- **2025-06-18**: Achieve 90%+ coverage (from current 54%)

### Quality Metrics
- [ ] Zero `:skip` tagged compliance tests
- [ ] All `MUST` requirements implemented
- [ ] All `MUST NOT` requirements enforced
- [ ] Comprehensive security validation

### Performance Targets
- [ ] Maintain current native BEAM transport performance
- [ ] HTTP transport latency < 5ms for local requests
- [ ] Memory usage stable under load

## Risk Mitigation

### Breaking Changes
- **Message Validation**: May reject previously accepted invalid messages
- **Batch Requests**: Removing batching is a breaking change
- **HTTP Headers**: New header requirements may break existing clients

### Mitigation Strategies
- [ ] **Gradual Rollout**: Implement validation warnings before errors
- [ ] **Version Negotiation**: Use protocol version to enable/disable features
- [ ] **Migration Guide**: Provide clear upgrade path for users
- [ ] **Backward Compatibility**: Maintain support for older protocol versions

## Compatibility Assessment for Ash Framework

Based on this analysis, ExMCP's current architecture would require significant modifications to support Ash's extension pattern:

**Current Limitations**:
- Monolithic handler approach vs. Ash's composable tool modules
- No opts-first architecture pattern
- Missing dynamic extension system

**Recommendation**: Use ExMCP as a solid MCP protocol foundation while building Ash's tool composition pattern on top, rather than trying to modify ExMCP's core architecture.

**Integration Strategy**:
```elixir
defmodule AshAI.MCP.Server do
  use ExMCP.Server.Handler
  
  # Use ExMCP for protocol handling
  # Implement Ash's tool pattern in the handler
  
  @impl true
  def handle_list_tools(state) do
    tools = state.ash_tools
    |> Enum.map(fn {tool_module, opts} ->
      tool_state = tool_module.init(opts)
      %{
        name: tool_module.name(tool_state),
        description: tool_module.description(tool_state),
        input_schema: tool_module.input_schema(tool_state)
      }
    end)
    
    {:ok, tools, state}
  end
end
```

## Conclusion

ExMCP has excellent architectural foundations and is well-positioned for improvement due to its modular design and comprehensive test structure. However, critical compliance gaps prevent it from being fully MCP-compliant. The library needs focused effort on message validation, 2025-06-18 feature completion, and security hardening to achieve full specification compliance.

**Priority Order**:
1. **Critical**: Message validation layer (prevents protocol violations)
2. **High**: 2025-06-18 feature completion (ensures modern compliance)
3. **Medium**: Security hardening (enables production deployment)

The good news is that the architectural foundation is solid, making these improvements achievable with focused development effort over the next 8 weeks.
