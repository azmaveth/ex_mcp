# ExMCP Security Implementation Plan

**Version**: 1.0 | **Created**: 2025-01-22 | **Priority**: CRITICAL

## Overview

Implementation plan for two critical security vulnerabilities:
1. **Token Passthrough Prevention** - Prevents confused deputy attacks
2. **User Consent Validation** - Ensures user approval for external resource access

**Expert Consensus**: Transport-layer interceptor approach (OpenAI o3-pro + Google Gemini-2.5-pro)

## Architecture

```
Transport Layer (HTTP/Stdio/BEAM)
    ↓
SecurityGuard (Interceptor)
    ├── Token Passthrough Check
    └── User Consent Validation
    ↓
ExMCP.Internal.Security
    ├── URL Classification & Header Stripping
    └── ConsentHandler Behavior + ConsentCache
```

## Implementation Phases

### Phase 1: Core Security Infrastructure (CRITICAL)

**Files to Create:**
- `lib/ex_mcp/consent_handler.ex` - Behavior definition
- `lib/ex_mcp/consent_handler/deny.ex` - Production-safe default
- `lib/ex_mcp/consent_handler/cli.ex` - Interactive CLI prompts
- `lib/ex_mcp/consent_handler/web.ex` - Web integration
- `lib/ex_mcp/internal/consent_cache.ex` - ETS-based cache

**Files to Modify:**
- `lib/ex_mcp/internal/security.ex` - Add token passthrough functions
- `lib/ex_mcp/application.ex` - Add ConsentCache to supervision tree

### Phase 2: Transport Integration (HIGH)

**Files to Create:**
- `lib/ex_mcp/transport/security_guard.ex` - Central interceptor
- `lib/ex_mcp/transport/security_error.ex` - Error handling
- `lib/ex_mcp/internal/security_config.ex` - Configuration management

**Files to Modify:**
- `lib/ex_mcp/transport/http.ex` - Add SecurityGuard calls
- `lib/ex_mcp/transport/stdio.ex` - Add SecurityGuard calls
- `lib/ex_mcp/transport/beam.ex` - Add SecurityGuard calls
- `config/config.exs` - Add security defaults

### Phase 3: Test Implementation (HIGH)

**Files to Create:**
- `test/support/security_test_helpers.ex` - Test utilities
- `lib/ex_mcp/consent_handler/test.ex` - Test handler
- `test/ex_mcp/transport/security_integration_test.exs` - Integration tests
- `test/ex_mcp/performance/security_performance_test.exs` - Performance tests

**Files to Modify:**
- `test/ex_mcp/compliance/security_compliance_test.exs` - Remove @tag :skip, implement tests

## Aider Instructions

### Phase 1: Core Infrastructure
```
1. Create ConsentHandler behavior with callbacks:
   - request_consent/3, check_existing_consent/2, revoke_consent/2

2. Implement consent handlers:
   - Deny (production-safe), CLI (interactive), Web (async)

3. Create ConsentCache GenServer with ETS storage and TTL

4. Enhance Security module with:
   - check_token_passthrough/3, classify_url/2, strip_sensitive_headers/2
   - ensure_user_consent/5, extract_origin/1

Focus: Security-first, deny-by-default behavior
```

### Phase 2: Transport Integration
```
1. Create SecurityGuard with validate_request/2 function

2. Create SecurityConfig with secure defaults:
   - trusted_origins: ["localhost", "127.0.0.1", "::1"]
   - consent_handler: ExMCP.ConsentHandler.Deny

3. Integrate SecurityGuard into all transports:
   - HTTP: modify send_request functions
   - Stdio: modify resource access functions  
   - BEAM: modify external service calls

4. Create SecurityError for standardized error handling

Focus: Consistent enforcement across transports
```

### Phase 3: Test Implementation
```
1. Create SecurityTestHelpers with test utilities

2. Remove @tag :skip from security compliance tests:
   - "prevents token passthrough to external resources"
   - "validates user consent before accessing external resources"

3. Implement comprehensive test scenarios:
   - Token types, URL patterns, consent flows, edge cases

4. Create integration and performance tests

5. Create Test consent handler for automated scenarios

Focus: 100% test coverage, <100μs performance target
```

## Configuration Examples

```elixir
# Development
config :ex_mcp, :security,
  consent_handler: ExMCP.ConsentHandler.CLI,
  trusted_origins: ["localhost", "*.local"],
  audit_log_level: :debug

# Production  
config :ex_mcp, :security,
  consent_handler: ExMCP.ConsentHandler.Deny,
  trusted_origins: ["api.company.com"],
  consent_ttl: :timer.hours(1)

# Testing
config :ex_mcp, :security,
  consent_handler: ExMCP.ConsentHandler.Test,
  log_security_actions: false
```

## Success Criteria

**Validation Checklist:**
- [ ] Security tests pass without @tag :skip
- [ ] No token leakage to external URLs
- [ ] Consent required for external resources
- [ ] <5ms performance impact
- [ ] Backward compatibility maintained
- [ ] Configuration validation works

**Key Metrics:**
- Zero @tag :skip in security tests
- 100% security function test coverage
- <100μs security validation time
- <10μs consent cache lookup time

## Security Features

**Token Passthrough Prevention:**
- Strips sensitive headers for external URLs
- Protected headers: Authorization, Cookie, X-API-Key, X-Auth-Token, etc.
- URL classification: internal vs external

**User Consent Validation:**
- Pluggable consent handlers for different environments
- Session-based consent caching with TTL
- Per-user, per-origin granular control

## Risk Mitigation

**Risks & Mitigations:**
1. Breaking changes → Feature flags + thorough testing
2. Performance impact → Benchmarking + optimization
3. Transport complexity → Phased implementation
4. UX issues → Multiple consent handler options
5. Config errors → Validation + secure defaults

**Rollback Plan:**
- Independent feature disabling via config
- Secure defaults (deny external access)
- Preserved existing functionality
- Clear troubleshooting documentation

---

**Next Steps**: Begin with Phase 1 implementation, validate each phase before proceeding.
