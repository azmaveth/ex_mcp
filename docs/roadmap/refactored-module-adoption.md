# Refactored Module Adoption

**Priority:** Medium
**Status:** In Progress — Authorization and Content.Validation done, MessageProcessor.Conn extracted

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

## Progress (2026-03-19)

### Completed
- **Authorization** — Fresh swap, 822→304 lines. Fixed state_param key, additional_params passthrough.
- **Content.Validation** — Fresh extraction from original (not swap). 995→293 facade + 490 lines in 6 sub-modules. All 82 tests pass.
- **MessageProcessor.Conn** — Extracted nested defmodule to separate file.

### Remaining
- **MessageProcessor handler groups** — 3 duplicated handler modes (~800 lines). Need unified Dispatcher with actual implementations (existing Handlers module is stubs).
- **Server.Tools** — Refactored version missing `validate_with_schema/2` and `compile_schema/1`. Need to add delegations before swap.
- Delete `message_processor_refactored.ex` and `server/tools_refactored.ex` after proper migration.
