# ExMCP Cleanup Tracker

**Created:** June 21, 2025  
**Target Completion:** v0.8.0  
**Total Estimated Reduction:** 7,225 lines (19% of codebase)

## Overview

This document tracks the cleanup work identified in CLEANUP.md and the Phase 2 analysis. Each item includes status, assignee, and verification steps.

## Phase 1: Critical Cleanup (COMPLETED ✅)

### 1.1 Dead Code Removal ✅
- [x] Remove `zero_copy.ex` (489 lines)
- [x] Remove `hot_reload.ex` (436 lines)
- **Lines Removed:** 925
- **Completed:** June 21, 2025

### 1.2 Dependency Cleanup ✅
- [x] Remove Ranch dependency from mix.exs
- **Completed:** June 21, 2025

### 1.3 Legacy File Removal ✅
- [x] Remove `client_legacy.ex` (61KB)
- [x] Remove `server_legacy.ex` (44KB)
- **Lines Removed:** ~2,800
- **Completed:** June 21, 2025

### 1.4 Test Consolidation ✅
- [x] Remove 6 duplicate coverage test files
- **Lines Removed:** 2,644
- **Completed:** June 21, 2025

### 1.5 Repository Cleanup ✅
- [x] Remove erl_crash.dump (6MB)
- **Note:** Still needs removal from git history
- **Completed:** June 21, 2025

**Phase 1 Total:** 6,369 lines removed ✅

## Phase 2: Architecture Simplification (IN PROGRESS 🔄)

### 2.1 Client Consolidation 🔄
**Target:** Consolidate 3 clients into 1  
**Est. Reduction:** 1,262 lines  
**Risk:** MEDIUM  
**Status:** IN PROGRESS (60% complete)

#### Tasks:
- [x] Create migration guide documenting API changes
- [x] Extract best features from each client:
  - [x] SimpleClient's TransportManager pattern
  - [x] ConvenienceClient's URL parsing and helpers
  - [x] Client's comprehensive feature set
- [x] Create unified client implementation (client_unified.ex)
- [x] Test unified client API
- [ ] Replace existing client.ex with unified implementation
- [ ] Update all 15 files using Client.ex
- [ ] Update all 8 files using SimpleClient
- [ ] Update 2 files using ConvenienceClient
- [ ] Add deprecation warnings to old clients
- [ ] Update documentation
- [ ] Run full test suite

#### Verification:
- [ ] All tests pass
- [ ] Migration guide tested with example code
- [ ] No references to old client modules remain

### 2.2 DSL Simplification 🔄
**Target:** Extract minimal deftool macro  
**Est. Reduction:** 1,600 lines  
**Risk:** LOW  
**Status:** NOT STARTED

#### Tasks:
- [ ] Create new `lib/ex_mcp/dsl.ex` with minimal implementation
- [ ] Extract only essential deftool macro (~200-300 lines)
- [ ] Update 8 files using deftool
- [ ] Remove complex DSL modules:
  - [ ] dsl/advanced.ex (17KB)
  - [ ] dsl/meta.ex (4KB)
  - [ ] dsl/resource.ex (10KB)
  - [ ] dsl/prompt.ex (7KB)
  - [ ] dsl/handler.ex (2KB)
- [ ] Update documentation
- [ ] Ensure backward compatibility

#### Verification:
- [ ] All existing deftool usage still works
- [ ] Tests pass
- [ ] Documentation reflects simplified DSL

### 2.3 Circuit Breaker Consolidation 🔄
**Target:** Remove duplicate implementation  
**Est. Reduction:** 363 lines  
**Risk:** LOW  
**Status:** NOT STARTED

#### Tasks:
- [ ] Audit both implementations for feature parity
- [ ] Update BEAM transport to use reliability version
- [ ] Remove transport/beam/circuit_breaker.ex
- [ ] Update any documentation references
- [ ] Run BEAM transport tests

#### Verification:
- [ ] BEAM transport tests pass
- [ ] No references to beam/circuit_breaker remain
- [ ] Single circuit breaker implementation works correctly

### 2.4 BEAM Transport Separation 🔄
**Target:** Move to separate package  
**Est. Reduction:** ~4,000 lines  
**Risk:** LOW  
**Status:** NOT STARTED

#### Tasks:
- [ ] Create new mix project: ex_mcp_beam
- [ ] Move transport/beam/* modules (10 files)
- [ ] Update dependencies in main project
- [ ] Add ex_mcp_beam as optional dependency
- [ ] Update documentation
- [ ] Create separate README for BEAM transport
- [ ] Update mix.exs groups_for_modules

#### Verification:
- [ ] Core tests pass without BEAM transport
- [ ] BEAM transport tests pass in new package
- [ ] Optional dependency works correctly
- [ ] Documentation updated

**Phase 2 Total:** 7,225 lines (target)

## Progress Summary

| Phase | Status | Lines Removed | Target | Completion |
|-------|--------|---------------|--------|------------|
| Phase 1 | ✅ COMPLETED | 6,369 | 6,369 | 100% |
| Phase 2 | 🔄 IN PROGRESS | 0 | 7,225 | 0% |
| **Total** | **🔄** | **6,369** | **13,594** | **47%** |

## Migration Scripts

### Client Migration (2.1)
```bash
# Script to help migrate from SimpleClient to Client
# Location: scripts/migrate_to_unified_client.sh
# TODO: Create after client consolidation design is finalized
```

### DSL Migration (2.2)
```bash
# No migration needed - backward compatible
```

## Rollback Plan

Each phase can be rolled back independently:
1. Git revert the specific commits
2. Restore removed files from git history if needed
3. Re-add removed dependencies to mix.exs

## Success Metrics

- [ ] Codebase reduced by target amount
- [ ] All tests passing
- [ ] No performance regressions
- [ ] Documentation updated
- [ ] Migration guides validated
- [ ] Community feedback positive

## Notes

- Keep backward compatibility where possible
- Provide clear migration paths
- Update CHANGELOG.md after each phase
- Consider beta release after Phase 2 completion