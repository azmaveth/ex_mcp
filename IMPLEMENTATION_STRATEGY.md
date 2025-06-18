# ExMCP Implementation Strategy

## Overview

This document outlines the implementation strategy for transforming ExMCP from a low-level transport library to a high-level, developer-friendly framework. The approach uses a **clean slate with strategic preservation** methodology optimized for AI coding agents.

## Problem Statement

- Current codebase has existing code that might confuse AI agents
- Design represents significant architectural shift (Phoenix-inspired layered architecture + DSL)
- Need to avoid mixing old and new code, including tests
- Want to preserve working transport implementations while rebuilding higher-level APIs

## Solution: Dual Directory Structure

### Directory Structure

```
lib/
├── ex_mcp/                    # Legacy (keep during transition)
│   ├── transport/             # ✅ Keep - these work well
│   │   ├── stdio.ex
│   │   ├── http.ex
│   │   ├── native.ex
│   │   └── sse.ex
│   ├── protocol.ex           # ✅ Keep - solid foundation
│   ├── types.ex              # ✅ Keep - good type definitions
│   ├── client.ex             # ❌ Will be replaced
│   └── server.ex             # ❌ Will be replaced
│
└── ex_mcp_v2/                # New implementation
    ├── plug.ex               # Phase 1: Foundation
    ├── registry.ex           # Phase 1: Tool/Resource registry
    ├── server.ex             # Phase 2: Magic Layer DSL
    ├── client.ex             # Phase 3: Hardened client
    ├── content/              # Phase 5: Content system
    │   ├── protocol.ex       # Content protocol definition
    │   ├── text.ex           # Text content implementation
    │   ├── image.ex          # Image content implementation
    │   ├── audio.ex          # Audio content implementation
    │   └── resource.ex       # Embedded resource content
    ├── dsl/                  # Phase 5: DSL macros
    │   ├── tool.ex           # deftool macro and schema compilation
    │   ├── resource.ex       # defresource macro
    │   └── prompt.ex         # defprompt macro
    ├── middleware/           # Advanced features
    │   ├── rate_limit.ex     # Rate limiting middleware
    │   ├── telemetry.ex      # Telemetry middleware
    │   └── auth.ex           # Authentication middleware
    └── testing/              # Testing support
        ├── case.ex           # ExMCP.TestCase
        ├── assertions.ex     # Custom assertions
        └── builders.ex       # Test data builders

test/
├── ex_mcp/                   # Legacy tests (minimal maintenance)
└── ex_mcp_v2/               # New comprehensive test suite
    ├── integration/          # Full client-server integration tests
    ├── unit/                 # Unit tests for individual modules
    ├── property/             # Property-based tests
    ├── performance/          # Performance benchmarks
    └── support/              # Test support files

examples/
├── legacy/                   # Move current examples here
└── v2/                      # New framework examples
    ├── getting_started/
    ├── advanced/
    └── migration_guide/
```

## Implementation Phases

### Phase 1: Foundation Setup
**Goal**: Create clean implementation space and basic foundation

**Tasks**:
1. Create `lib/ex_mcp_v2/` directory structure
2. Implement `ExMCP.Plug` - core message processing abstraction
3. Implement `ExMCP.Registry` - tool/resource registry
4. Create symlinks to preserve working transport code
5. Basic test infrastructure

**Files to create**:
- `lib/ex_mcp_v2/plug.ex`
- `lib/ex_mcp_v2/registry.ex` 
- `lib/ex_mcp_v2/application.ex`
- `test/ex_mcp_v2/plug_test.exs`

### Phase 2: Magic Layer (DSL)
**Goal**: Implement the developer-friendly DSL

**Tasks**:
1. Create DSL macros (`deftool`, `defresource`, `defprompt`)
2. JSON Schema compilation from Elixir-native DSL
3. Capability auto-detection
4. Basic server implementation using DSL

**Files to create**:
- `lib/ex_mcp_v2/server.ex`
- `lib/ex_mcp_v2/dsl/tool.ex`
- `lib/ex_mcp_v2/dsl/resource.ex`
- `lib/ex_mcp_v2/dsl/prompt.ex`

### Phase 3: Client Hardening
**Goal**: Implement synchronous, reliable client

**Tasks**:
1. Synchronous `start_link` with proper initialization
2. Robust error handling and reconnection
3. Transport abstraction and fallback

**Files to create**:
- `lib/ex_mcp_v2/client.ex`
- `test/ex_mcp_v2/client_test.exs`

### Phase 4: DX Enhancements
**Goal**: Developer experience improvements

**Tasks**:
1. Convenience functions
2. Response normalization
3. Better error messages

### Phase 5: Content System & Advanced DSL
**Goal**: Type-safe content system and full DSL features

**Tasks**:
1. Content protocol and struct implementations
2. Smart constructors and builder patterns
3. Full DSL feature set (annotations, middleware, etc.)

### Phase 6: Testing & Reliability
**Goal**: Comprehensive testing framework

**Tasks**:
1. Custom test case templates
2. Property-based testing
3. Performance benchmarks
4. Integration test suite

## Migration Strategy

### Step 1: Preserve Working Code
```bash
# Symlink existing working implementations
ln -s ../ex_mcp/transport lib/ex_mcp_v2/transport
ln -s ../ex_mcp/protocol.ex lib/ex_mcp_v2/protocol.ex
ln -s ../ex_mcp/types.ex lib/ex_mcp_v2/types.ex
```

### Step 2: Update mix.exs
```elixir
def application do
  [
    extra_applications: [:logger],
    mod: {ExMCP.Application, []},  # Legacy - will switch to ExMCPV2.Application
  ]
end

# Add compilation paths
def project do
  [
    # ... existing config
    elixirc_paths: elixirc_paths(Mix.env()),
  ]
end

defp elixirc_paths(:test), do: ["lib", "test/support"]
defp elixirc_paths(_), do: ["lib"]
```

### Step 3: Gradual Examples Migration
1. Move current examples to `examples/legacy/`
2. Create new examples in `examples/v2/` using new API
3. Maintain both during transition period

### Step 4: Documentation Strategy
- Keep current docs for legacy API
- Create new documentation for v2 API
- Clear migration guide from v1 to v2

## Benefits for AI Agents

1. **Clear Boundaries**: `lib/ex_mcp_v2/` vs `lib/ex_mcp/` - no confusion
2. **Clean Slate**: No legacy patterns to work around in new implementation
3. **Focused Scope**: Each phase has isolated, well-defined scope
4. **Testability**: New tests match new architecture patterns
5. **Rollback Safety**: Legacy implementation remains functional

## AI Agent Instructions Template

When working with AI agents, use focused instructions like:

```
Implement Phase 1 of DESIGN_PLAN.md. 

RULES:
- Work ONLY in lib/ex_mcp_v2/ directory
- IGNORE everything in lib/ex_mcp/ - that's legacy code
- Focus on building ExMCP.Plug from scratch per the specification
- Create corresponding tests in test/ex_mcp_v2/
- Follow existing Elixir/OTP patterns (GenServer, Supervisor, etc.)

GOAL: Build the foundation layer that other phases will build upon.
```

## Risk Mitigation

1. **Legacy Preservation**: Old API continues working during development
2. **Incremental Testing**: Each phase can be tested independently
3. **Clear Rollback**: Can abandon v2 and continue with v1 if needed
4. **Gradual Migration**: Users can migrate examples/code at their own pace

## Success Metrics

- [x] Phase 1: New foundation compiles and basic tests pass
- [x] Phase 2: Simple DSL example works end-to-end
- [x] Phase 3: Client can connect and communicate reliably
- [x] Phase 4: Developer experience measurably improved
- [x] Phase 5: Full DSL feature parity with design
- [ ] Phase 6: >80% test coverage, performance benchmarks pass

## Implementation Status

### ✅ Phase 1: Foundation Setup (COMPLETED)

**Completed Tasks**:
1. ✅ Created directory structure (`lib/ex_mcp_v2/`, `test/ex_mcp_v2/`, `examples/v2/`)
2. ✅ Moved legacy examples to `examples/legacy/`
3. ✅ Implemented `ExMCP.Plug` - core message processing abstraction
4. ✅ Implemented `ExMCP.Registry` - capability registry with GenServer
5. ✅ Implemented `ExMCPV2.Application` - application supervisor
6. ✅ Created comprehensive tests for all Phase 1 modules
7. ✅ Verified compilation and all tests pass

**Phase 1 Results**:
- Clean foundation established with no legacy code conflicts
- 11 tests passing for core infrastructure
- Ready for Phase 2 (DSL implementation)

### ✅ Phase 2: Magic Layer (DSL) (COMPLETED)

**Completed Tasks**:

1. ✅ Implemented DSL macros (`deftool`, `defresource`, `defprompt`)
   - `lib/ex_mcp_v2/dsl/tool.ex` - Tool DSL with JSON Schema compilation
   - `lib/ex_mcp_v2/dsl/resource.ex` - Resource DSL
   - `lib/ex_mcp_v2/dsl/prompt.ex` - Prompt DSL
   - `lib/ex_mcp_v2/dsl/advanced.ex` - Advanced DSL features
2. ✅ JSON Schema compilation from Elixir-native DSL
3. ✅ Capability auto-detection
4. ✅ Server implementation with DSL support (`lib/ex_mcp_v2/server_v2.ex`)

### ✅ Phase 3: Client Enhancement (COMPLETED)

**Completed Tasks**:

1. ✅ Implemented hardened client (`lib/ex_mcp_v2/client.ex`)
2. ✅ Automatic connection management
3. ✅ Built-in retry logic and error handling
4. ✅ Convenience client with high-level API (`lib/ex_mcp_v2/convenience_client.ex`)
5. ✅ Simple client for basic use cases (`lib/ex_mcp_v2/simple_client.ex`)

### ✅ Phase 4: Transport & Reliability (COMPLETED)

**Completed Tasks**:

1. ✅ Transport manager with automatic failover (`lib/ex_mcp_v2/transport_manager.ex`)
2. ✅ Reliability features:
   - Circuit breaker (`lib/ex_mcp_v2/reliability/circuit_breaker.ex`)
   - Health checks (`lib/ex_mcp_v2/reliability/health_check.ex`)
   - Retry logic (`lib/ex_mcp_v2/reliability/retry.ex`)
3. ✅ Helper utilities (`lib/ex_mcp_v2/helpers.ex`)

### ✅ Phase 5: Content System & Testing (COMPLETED)

**Completed Tasks**:

1. ✅ Content protocol implementation:
   - `lib/ex_mcp_v2/content/protocol.ex` - Core content protocol
   - `lib/ex_mcp_v2/content/builders.ex` - Content builders
   - `lib/ex_mcp_v2/content/validation.ex` - Content validation
2. ✅ Testing infrastructure:
   - `lib/ex_mcp_v2/testing/case.ex` - Test case helpers
   - `lib/ex_mcp_v2/testing/assertions.ex` - Custom assertions
   - `lib/ex_mcp_v2/testing/builders.ex` - Test data builders
   - `lib/ex_mcp_v2/testing/mock_server.ex` - Mock server for testing
3. ✅ Comprehensive test suite (24 test files)

### 🔄 Current Status: Phase 6 - Polish & Performance

**Remaining Tasks**:

1. ⏳ Achieve >80% test coverage
2. ⏳ Performance benchmarks
3. ⏳ Documentation improvements
4. ⏳ Migration guide completion

**Current Stats**:

- 27 implementation files in `lib/ex_mcp_v2/`
- 24 test files in `test/ex_mcp_v2/`
- Full v2 implementation appears to be feature-complete
- Need to verify test coverage and performance metrics

---

This strategy provides a clear, AI-agent-friendly path to implementing the new ExMCP architecture while preserving what works and minimizing risks.
