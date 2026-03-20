# Refactored Module Adoption

**Priority:** Medium
**Status:** Planned (attempted swap — API differences found)

## Context

4 refactored modules exist alongside their originals. Each is architecturally superior
(better delegation, less duplication, SOLID principles) but has API differences that
prevent a simple file swap.

## Modules

### 1. authorization_refactored.ex (304 vs 822 lines, 62% reduction)
- Delegates to HTTPClient, OAuthFlow, PKCE, Validator sub-modules
- Sub-modules exist and work
- **Gap:** Different parameter transformation patterns in facade

### 2. message_processor_refactored.ex (309 vs 1344 lines, 77% reduction)
- Unified Dispatcher pattern replaces 3 separate process_with_* functions
- **Gap:** `Conn` struct was nested defmodule in original, needs extraction to separate file
- Dispatcher.dispatch/4 must match original's handler modes

### 3. content/validation_refactored.ex (324 vs 995 lines, 67% reduction)
- Delegates to SchemaValidator, Sanitizer, Transformer, SecurityScanner, ValidatorRegistry
- **Gap:** Other modules call `Content.ValidationRefactored` by name (not `Content.Validation`)
- **Gap:** `validate_batch/3` lost Task.async_stream parallelism — must restore
- **Gap:** Return type changed from list to map in validate_batch

### 4. server/tools_refactored.ex (471 vs 709 lines, 34% reduction)
- Builder pattern replaces nested quote blocks
- **Gap:** Missing `validate_with_schema/2` and `compile_schema/1` public functions
- Other modules call these directly

## Approach

For each module:
1. Find all callers of the original module's public functions
2. Verify the refactored version exports the same API (or add delegations)
3. Fix any callers that reference `*Refactored` by name
4. Extract nested types (like Conn) into separate files
5. Swap, compile, test

## Decision

These are post-1.0 improvements. The originals work fine and are well-tested.
The refactored versions are valuable for maintainability but the migration
carries risk that isn't justified before a stable release.

For 1.0: delete the `_refactored.ex` files (they're dead code that confuses the codebase).
For 1.1: do the proper migration with full API compatibility verification.
