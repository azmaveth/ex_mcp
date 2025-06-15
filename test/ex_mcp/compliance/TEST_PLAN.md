# MCP Compliance Test Plan

This document outlines the test plan for achieving 100% MCP specification compliance.

## Phase 1: Critical Security & Protocol Validation (High Priority)

### 1.1 Message Format Validation Tests
**File**: `test/ex_mcp/compliance/message_validation_test.exs`

- [ ] Request ID uniqueness validation
  - Test that IDs cannot be reused within a session
  - Test proper error when duplicate ID is used
- [ ] Null ID rejection
  - Test that null IDs are rejected (unlike base JSON-RPC)
  - Test proper error response
- [ ] Response format validation
  - Test that responses have either result OR error, never both
  - Test missing result/error rejection
- [ ] Error code validation
  - Test standard JSON-RPC error codes (-32700 to -32603)
  - Test custom error code ranges

### 1.2 Security Vulnerability Tests
**File**: `test/ex_mcp/compliance/security_test.exs`

- [ ] Token Passthrough Prevention
  - Test rejection of tokens not issued for MCP server
  - Test audience claim validation
- [ ] Confused Deputy Attack Prevention
  - Test consent requirement before third-party auth
  - Test proper token scope validation
- [ ] User Consent Validation
  - Test explicit consent before tool invocation
  - Test consent for data access operations
- [ ] HTTPS Enforcement
  - Test rejection of non-HTTPS external connections
  - Test localhost exception handling

## Phase 2: Transport-Specific Requirements (Medium Priority)

### 2.1 stdio Transport Tests
**File**: `test/ex_mcp/compliance/stdio_transport_test.exs`

- [ ] stdout/stdin isolation
  - Test server writes ONLY valid MCP messages to stdout
  - Test client writes ONLY valid MCP messages to stdin
- [ ] Newline handling
  - Test rejection of embedded newlines in messages
  - Test proper line delimiter handling

### 2.2 Streamable HTTP Transport Tests
**File**: `test/ex_mcp/compliance/streamable_http_test.exs`

- [ ] Session Management
  - Test 400 response for missing Mcp-Session-Id
  - Test 404 response for invalid/expired session
  - Test automatic re-initialization on 404
  - Test session termination via DELETE
  - Test session ID persistence across requests
- [ ] Connection Resumability
  - Test Last-Event-ID header handling
  - Test message replay for disconnected streams
  - Test isolation between different streams
  - Test event ID tracking and resumption

## Phase 3: Advanced Protocol Features (Medium Priority)

### 3.1 Batch Request Edge Cases
**File**: `test/ex_mcp/compliance/batch_edge_cases_test.exs`

- [ ] Empty batch array handling
- [ ] Notification-only batch (202 Accepted, no body)
- [ ] Mixed batch (requests + notifications)
- [ ] Response-only batch handling
- [ ] Initialize request in batch rejection

### 3.2 Progress & Cancellation
**File**: `test/ex_mcp/compliance/progress_cancellation_test.exs`

- [ ] Progress percentage validation (0-100)
- [ ] Multiple progress updates for single operation
- [ ] Progress message field (2025-03-26+)
- [ ] Cancellation reason propagation
- [ ] In-flight request cancellation

### 3.3 OAuth 2.1 Advanced Features
**File**: `test/ex_mcp/compliance/oauth_advanced_test.exs`

- [ ] PKCE challenge length validation (43-128 bytes)
- [ ] Token type validation (MUST be "Bearer")
- [ ] Scope validation and enforcement
- [ ] Well-known metadata discovery
- [ ] Dynamic client registration flow
- [ ] Token refresh error handling

## Phase 4: Draft-Specific Features (Lower Priority)

### 4.1 Elicitation Tests
**File**: `test/ex_mcp/compliance/elicitation_test.exs`

- [ ] Schema validation for all primitive types
- [ ] String format constraints
- [ ] Number constraints (min/max)
- [ ] Enum validation
- [ ] Action types (accept/decline/cancel)
- [ ] Rate limiting enforcement

### 4.2 Structured Tool Output
**File**: `test/ex_mcp/compliance/structured_output_test.exs`

- [ ] isError flag handling
- [ ] structuredContent validation against outputSchema
- [ ] Error details with structured output
- [ ] Schema mismatch handling

### 4.3 Resource Metadata
**File**: `test/ex_mcp/compliance/resource_metadata_test.exs`

- [ ] Protected resource metadata in init response
- [ ] Authorization server discovery
- [ ] Scope requirements in annotations

## Phase 5: Backwards Compatibility (Lower Priority)

### 5.1 Version Migration Tests
**File**: `test/ex_mcp/compliance/version_migration_test.exs`

- [ ] Server supporting both old and new transports
- [ ] Client discovery mechanism (POST -> GET fallback)
- [ ] Protocol version negotiation
- [ ] Feature availability by version

## Implementation Guidelines

### Test Structure
Each compliance test file should:
1. Use `@moduletag :compliance`
2. Define version-specific handler modules
3. Test both success and error cases
4. Include descriptive test names
5. Reference specific spec sections in comments

### Example Test Pattern
```elixir
defmodule ExMCP.Compliance.MessageValidationTest do
  use ExUnit.Case, async: true
  
  @moduletag :compliance
  
  describe "Request ID Validation (MCP Spec Section X.Y)" do
    test "rejects duplicate request IDs within session" do
      # Test implementation
    end
    
    test "rejects null request IDs" do
      # Test implementation
    end
  end
end
```

### Priority Order
1. **Week 1-2**: Phase 1 (Security & Validation)
2. **Week 3-4**: Phase 2 (Transport Requirements)
3. **Week 5-6**: Phase 3 (Advanced Features)
4. **Week 7-8**: Phase 4 & 5 (Draft & Compatibility)

### Success Metrics
- All tests passing with `mix test --only compliance`
- 100% coverage of MCP specification requirements
- Clear documentation linking tests to spec sections
- No security vulnerabilities in implementation