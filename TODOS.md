# ExMCP Development TODOs

## Executive Summary

This document tracks the remaining work needed to achieve full MCP specification compliance. Recent implementation work has addressed critical message validation gaps, moving ExMCP from basic JSON-RPC parsing to comprehensive MCP specification validation. The focus now shifts to completing security validation, removing deprecated features, and achieving full 2025-06-18 specification compliance.

## Current Status

### ✅ Recently Completed
- **Message Validation Layer**: Comprehensive `ExMCP.Internal.MessageValidator` module implemented
- **Protocol Integration**: Updated `ExMCP.Internal.Protocol` with validation support
- **Core Compliance Tests**: Removed multiple `@tag :skip` markers and implemented actual validation tests
- **Session Management**: Request ID uniqueness tracking per session
- **Backward Compatibility**: Maintained existing API while adding validation
- **Security Infrastructure (Phase 1 & 2)**: Complete security implementation with ConsentHandler behavior, SecurityGuard interceptor, and transport integration infrastructure
- **Security Test Implementation (Phase 3)**: Comprehensive test suite with SecurityTestHelpers, Test ConsentHandler, removed @tag :skip markers, and performance validation (<100μs target achieved at 21μs)

### 📊 Compliance Status Improvement
- **Message Validation**: 0% → 80% implemented
- **Security Implementation**: 0% → 100% implemented (Phases 1-3 complete)
- **2024-11-05**: ~63% → ~85% coverage (estimated)
- **2025-03-26**: ~61% → ~80% coverage (estimated)  
- **2025-06-18**: ~54% → ~75% coverage (estimated)

## High Priority TODOs (Next 1-2 Weeks)

### 1. Complete Message Validation Tests
**Priority**: CRITICAL
**Files**: `test/ex_mcp/compliance/message_validation_compliance_test.exs`

- [ ] **Error Code Validation Tests**
  - Remove `@tag :skip` from "server uses appropriate standard error codes" test
  - Implement test for standard JSON-RPC error codes (-32700 to -32603)
  - Test custom error code range validation (-32099 to -32000)
  - Verify error code enforcement in actual server responses

- [ ] **Message Structure Validation Tests**
  - Remove `@tag :skip` from "requests include all required fields" test
  - Test validation of required JSON-RPC fields (jsonrpc, method, id)
  - Test rejection of malformed requests
  - Verify proper error responses for missing fields

- [ ] **Batch Request Validation Tests**
  - Remove `@tag :skip` from "empty batch array is rejected" test
  - Remove `@tag :skip` from "notification-only batch returns 202" test
  - **NOTE**: Consider removing these entirely if batch support is deprecated

### 2. Update Coverage Documentation
**Priority**: HIGH
**Files**: `test/ex_mcp/compliance/MCP_COVERAGE_MATRIX.md`

- [ ] **Update Coverage Matrix**
  - Change "Request ID uniqueness" from ❌ to ✅
  - Change "Null ID rejection" from ❌ to ✅
  - Change "Result XOR error" from ❌ to ✅
  - Update coverage percentages to reflect actual implementation
  - Add notes about newly implemented validation features

### 3. Security Compliance Implementation
**Priority**: ✅ COMPLETED
**Files**: `test/ex_mcp/compliance/security_compliance_test.exs`

- [x] **Token Passthrough Prevention**
  - Removed `@tag :skip` from "prevents token passthrough to external resources" test
  - Implemented comprehensive test scenarios for various token types (Bearer, API keys, OAuth, Cookie)
  - Added test cases for different URL patterns (external vs internal)
  - Verified proper error responses when token passthrough is attempted

- [x] **User Consent Validation**
  - Removed `@tag :skip` from "validates user consent before accessing external resources" test
  - Implemented Human-in-the-Loop (HITL) approval mechanisms with Test ConsentHandler
  - Added consent UI abstractions and approval handler interfaces
  - Tested consent flow for external resource access across different transports

- [x] **SecurityGuard Configuration Improvements** ⚠️ CRITICAL
  - SecurityGuard uses centralized SecurityConfig for configuration management
  - Feature flags are respected (enable_token_passthrough_prevention, enable_user_consent_validation)
  - Updated SecurityGuard.validate_request/2 to fetch validated configuration from SecurityConfig
  - Implemented proper transport-specific configuration handling

- [x] **ConsentHandler Configuration Enhancement** ⚠️ HIGH
  - ConsentHandler behavior accepts config parameter for TTL and other settings
  - All ConsentHandler implementations (Deny, CLI, Web, Test) accept config parameter
  - Test ConsentHandler implemented for automated test scenarios
  - Updated ensure_user_consent/5 to pass config to consent handlers

- [x] **Comprehensive Test Implementation**
  - Created SecurityTestHelpers with test utilities for security scenarios
  - Created Test ConsentHandler for automated consent scenarios
  - Implemented integration tests across HTTP, stdio, and BEAM transports
  - Added performance tests validating <100μs target (achieved 21μs performance)
  - Comprehensive test coverage for token types, URL patterns, consent flows, edge cases

## Medium Priority TODOs (Next Month)

### 4. Specification Drift Resolution
**Priority**: HIGH
**Files**: `lib/ex_mcp/internal/protocol.ex`, `test/ex_mcp/compliance/batch_edge_cases_test.exs`

- [ ] **Remove Deprecated Batch Support**
  - Remove `encode_batch/1` function from `ExMCP.Internal.Protocol`
  - Remove `parse_batch_response/1` function
  - Delete `test/ex_mcp/compliance/batch_edge_cases_test.exs` entirely
  - Update documentation to reflect batch removal
  - **Rationale**: 2025-06-18 specification explicitly removes JSON-RPC batching

- [ ] **HTTP Protocol Version Header Validation**
  - Add `MCP-Protocol-Version` header requirement to HTTP transport
  - Implement version validation in `ExMCP.HttpPlug`
  - Add proper error responses for missing/invalid headers
  - Update HTTP transport tests to include header validation

### 5. 2025-06-18 Feature Implementation
**Priority**: MEDIUM
**Files**: Various server and client modules

- [ ] **Structured Tool Output**
  - Complete `structuredContent` field validation
  - Implement schema validation for structured output
  - Add resource links in tool results
  - Update DSL to support structured output definitions

- [ ] **Elicitation Support**
  - Complete server-side elicitation implementation
  - Add client-side elicitation handling
  - Implement user interaction flows
  - Add comprehensive elicitation tests

- [ ] **OAuth 2.1 Resource Server Classification**
  - Implement authorization server metadata endpoint
  - Add resource server classification
  - Implement scope validation
  - Add comprehensive OAuth 2.1 testing

### 6. Batch Edge Cases Cleanup
**Priority**: LOW (if batch support is removed)
**Files**: `test/ex_mcp/compliance/batch_edge_cases_test.exs`

- [ ] **Remove All Batch Tests** (if batch support is deprecated)
  - Delete entire `batch_edge_cases_test.exs` file
  - Remove batch-related test cases from other compliance tests
  - Update test suite configuration to exclude batch tests

**Alternative**: If batch support is maintained for backward compatibility:
- [ ] **Complete Batch Edge Cases**
  - Remove all `@tag :skip` markers
  - Implement empty batch handling tests
  - Implement notification-only batch tests
  - Implement mixed batch handling tests
  - Implement batch error handling tests
  - Implement initialize-in-batch validation tests
  - Implement large batch handling tests

## Long-term Strategic TODOs (Next Quarter)

### 7. Integration and End-to-End Testing
**Priority**: MEDIUM
**Files**: New integration test files

- [ ] **Server Integration Tests**
  - Test validator integration with actual server implementations
  - Verify session state management across different transports
  - Test error handling in real client-server scenarios
  - Performance testing with validation enabled

- [ ] **Client Integration Tests**
  - Test client behavior with validated server responses
  - Verify proper error handling for validation failures
  - Test session management from client perspective
  - Cross-transport compatibility testing

### 8. Performance and Optimization
**Priority**: LOW
**Files**: `lib/ex_mcp/internal/message_validator.ex`

- [ ] **Validation Performance Optimization**
  - Profile validation performance impact
  - Optimize session state management for high-throughput scenarios
  - Consider caching for repeated validations
  - Benchmark against unvalidated parsing

- [ ] **Memory Usage Optimization**
  - Monitor session state memory usage
  - Implement session cleanup for closed connections
  - Optimize MapSet operations for ID tracking
  - Add configurable limits for session state size

### 9. Developer Experience Improvements
**Priority**: MEDIUM
**Files**: Documentation and examples

- [ ] **Enhanced Error Messages**
  - Improve validation error message clarity
  - Add suggestions for fixing common validation errors
  - Include examples in error responses
  - Add debugging helpers for validation failures

- [ ] **Documentation Updates**
  - Update README with compliance status
  - Add migration guide for validation changes
  - Update API documentation with validation details
  - Create troubleshooting guide for validation errors

### 10. Automated Compliance Checking
**Priority**: LOW
**Files**: New tooling and CI integration

- [ ] **Specification Compliance Automation**
  - Generate compliance tests from TypeScript schema files
  - Automate coverage matrix updates
  - Add CI checks for specification drift
  - Create compliance reporting dashboard

- [ ] **Regression Prevention**
  - Add tests to prevent specification drift
  - Implement compliance gates in CI/CD
  - Create specification change detection
  - Add automated compliance reporting

## Architecture and Code Quality TODOs

### 11. DSL Migration Completion
**Priority**: MEDIUM
**Files**: Various server modules

- [ ] **Complete Handler to DSL Migration**
  - Migrate remaining handler-based patterns to DSL
  - Update examples to use DSL patterns
  - Deprecate old handler-based APIs
  - Create migration guide for users

### 12. Security Hardening
**Priority**: HIGH
**Files**: `lib/ex_mcp/internal/security.ex`

- [ ] **Comprehensive Security Review**
  - Security audit of `ExMCP.Internal.Security` module
  - Implement security best practices from MCP specification
  - Add security validation tests
  - Create security documentation

- [ ] **HTTPS Enforcement**
  - Complete HTTPS enforcement implementation
  - Add comprehensive HTTPS testing
  - Implement certificate validation
  - Add TLS configuration options

## Testing Strategy TODOs

### 13. Test Coverage Enhancement
**Priority**: MEDIUM
**Files**: All test files

- [ ] **Achieve 90%+ Compliance Coverage**
  - Target 90%+ coverage for all MCP specification versions
  - Add comprehensive edge case testing
  - Improve transport-specific requirement testing
  - Add property-based testing for validation logic

- [ ] **Test Organization Improvements**
  - Consolidate related test cases
  - Improve test naming and documentation
  - Add test categories for better organization
  - Create test utilities for common patterns

## Risk Mitigation TODOs

### 14. Breaking Changes Management
**Priority**: HIGH
**Files**: Documentation and migration guides

- [ ] **Migration Strategy**
  - Create comprehensive migration guide
  - Implement gradual rollout strategy for validation
  - Add compatibility warnings for deprecated features
  - Provide clear upgrade path for users

- [ ] **Backward Compatibility**
  - Maintain support for older protocol versions during transition
  - Add feature flags for validation behavior
  - Implement graceful degradation for unsupported features
  - Create compatibility testing suite

## Success Metrics and Tracking

### 15. Compliance Metrics
**Target Metrics**:
- [ ] **2024-11-05**: Achieve 90%+ coverage (from current ~75%)
- [ ] **2025-03-26**: Achieve 90%+ coverage (from current ~70%)
- [ ] **2025-06-18**: Achieve 90%+ coverage (from current ~65%)
- [ ] **Zero `@tag :skip`**: Remove all skipped compliance tests
- [ ] **Security Coverage**: 100% of security requirements tested

### 16. Quality Metrics
**Target Metrics**:
- [ ] **Performance**: Maintain <5ms HTTP transport latency
- [ ] **Memory**: Stable memory usage under load
- [ ] **Reliability**: Zero validation-related crashes
- [ ] **Compatibility**: 100% backward compatibility maintained

## Notes and Considerations

### Implementation Guidelines
- **Validation First**: All new features must include comprehensive validation
- **Test-Driven**: Remove `@tag :skip` only after implementing actual test logic
- **Backward Compatibility**: Maintain existing APIs during transition
- **Performance**: Monitor validation performance impact
- **Documentation**: Update documentation with each change

### Decision Points
- **Batch Support**: Decide whether to remove entirely or maintain for compatibility
- **Protocol Versions**: Define support policy for older MCP versions
- **Security Features**: Prioritize security implementation over feature additions
- **Breaking Changes**: Plan breaking changes carefully with migration support

### Dependencies
- Some TODOs depend on MCP specification clarifications
- Security features may require external authentication libraries
- Performance optimization may require profiling tools
- Integration testing requires test infrastructure setup

---

**Last Updated**: Based on compliance analysis and implementation work completed
**Next Review**: After completing high-priority validation tests
**Owner**: ExMCP Development Team
