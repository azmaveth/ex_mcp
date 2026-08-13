# Post-1.0 Maintenance Plan

- **Status:** Proposed and tracked; execute in small, independently reviewable changes
- **Baseline:** ExMCP `1.0.0-rc.7`
- **Scope:** ACP adapter modularization and Hex source-package cleanup
- **Last updated:** 2026-08-13

This is a repository-maintenance document, not user-facing package
documentation. It records cleanup that is valuable but too invasive to mix
into the final 1.0 release-candidate cycle.

## Goals and constraints

- Keep every documented public module, callback, option, and return shape
  available throughout 1.x.
- Preserve ACP JSON-RPC and native CLI wire output byte-for-byte unless a
  separately documented bug fix requires a change.
- Keep the root adapter modules as the public behaviour implementations; move
  cohesive private responsibilities behind them.
- Prefer a few substantial boundaries over many tiny helper modules.
- Do not create a shared Codex/ZCode abstraction merely because private
  functions have similar names. Share behavior only after golden tests prove
  that its inputs, outputs, errors, ordering, and lifecycle are identical.

The rc.8 internal-dedup work establishes reusable subprocess-environment,
positive-option, and workspace-containment helpers. The larger adapter changes
below remain deferred until after stable 1.0.

## Codex adapter restructuring

At the rc.7 baseline, `ExMCP.ACP.Adapters.Codex` is approximately 3,470 lines,
in addition to the existing `Codex.Config` and `Codex.Events` modules. The root
module currently owns process protocol, ACP lifecycle, request tracking,
permissions, prompt conversion, session state, and MCP configuration.

### Characterization gate

Before moving production code, add golden tests for:

- every app-server request emitted by initialize and session lifecycle calls;
- prompt content conversion for text, images, resources, and resource links;
- permission request options and every accepted/rejected response shape;
- session update ordering for text, reasoning, tool calls, plans, and usage;
- MCP stdio/HTTP/SSE conversion and authorization failures;
- cancellation, timeout, late-response, and subprocess-exit behavior; and
- model and mode catalog normalization across supported Codex CLI versions.

### Proposed boundaries

1. **`Codex.Protocol`** — native app-server envelope builders, method names,
   response classification, and request-id correlation shapes. It must remain a
   pure module with no process ownership.
2. **`Codex.Sessions`** — session lookup/update helpers and lifecycle state
   transitions. The root adapter retains orchestration and subprocess ownership.
3. **`Codex.Permissions`** — approval option construction, structured-decision
   encoding/decoding, and fail-closed fallback responses.
4. **`Codex.Content`** — ACP prompt/resource conversion plus native item and
   tool-result mapping.
5. **`Codex.MCP`** — MCP server normalization and native configuration output;
   authorization policy remains explicit at the adapter boundary.

Extract one boundary per commit. A boundary should generally remove at least
100 lines or eliminate a repeated semantic decision; otherwise leaving the
code local is clearer.

### Codex completion criteria

- The public `ExMCP.ACP.Adapters.Codex` API and state behavior are unchanged.
- The root module primarily coordinates lifecycle, state, and subprocess I/O.
- Unit, official ACP SDK interop, and real Codex CLI lifecycle tests pass.
- Golden native-wire fixtures are unchanged.
- Any helper proposed for ZCode reuse has explicit cross-adapter contract tests.

## Pi adapter restructuring

At the rc.7 baseline, `ExMCP.ACP.Adapters.Pi` is approximately 2,357 lines.
`Pi.SessionStore`, `Pi.Settings`, `Pi.SlashCommands`, `Pi.Startup`, `Pi.Tools`,
and `Pi.Version` already provide useful boundaries, but the root module still
combines RPC control flow, ACP lifecycle, streaming events, prompt scheduling,
and configuration translation.

### Characterization gate

Before moving production code, add golden tests for:

- RPC messages for new, load, resume, fork, close, delete, and prompt flows;
- control-group completion and failure ordering;
- assistant/thinking/tool/usage stream-event conversion;
- prompt queue, steering, follow-up, cancellation, and subprocess-exit behavior;
- model, thinking-level, and boolean configuration updates;
- slash-command expansion and available-command notifications; and
- session-map and backing JSONL safety rules.

### Proposed boundaries

1. **`Pi.RPC`** — RPC envelope construction, correlation ids, and response
   classification. It should not own adapter state or a Port.
2. **`Pi.Sessions`** — ACP lifecycle translation and Pi session-switch/new-session
   state transitions, building on `Pi.SessionStore`.
3. **`Pi.Events`** — inbound stream-event folding into ACP notifications and
   prompt results.
4. **`Pi.PromptFlow`** — active/queued prompt transitions, steering/follow-up,
   cancellation, and terminal completion.
5. **`Pi.Config`** — model catalogs, thinking levels, mode/config option
   construction, and config-update translation.

Keep `Pi.Settings`, `Pi.Startup`, `Pi.SlashCommands`, `Pi.Tools`, and
`Pi.Version` separate unless an extraction exposes a concrete duplicate. Do
not merge modules solely to reduce the file count.

### Pi completion criteria

- The public `ExMCP.ACP.Adapters.Pi` API and startup options are unchanged.
- The root module primarily coordinates state and native process I/O.
- Pi unit tests and the credential-free real CLI lifecycle test pass.
- Golden RPC fixtures and ACP event ordering are unchanged.
- No test reads the developer's real Pi settings, prompts, models, or sessions.

## Hex source-package documentation cleanup

The rc.7 `package.files` list ships approximately 195 KB of raw internal
planning, audit, coverage, and release-candidate history:

- `docs/API_DIFF_RC5_TO_1_0.md`
- `docs/MCP_2026_07_28_MIGRATION_PLAN.md`
- `docs/MCP_COVERAGE_MATRIX.md`
- `docs/RELEASE_1_0_0_RC_6.md`
- `docs/RELEASE_1_0_0_RC_7.md`
- `docs/SECURITY_AUDIT_2026-08-12.md`
- `docs/PRE_2_0_TECH_DEBT_PLAN.md`
- `docs/V2_ROADMAP.md`

These files should remain in Git history and the repository. They need not be
installed in every consumer's dependency tree or presented as normal library
guides on HexDocs.

### Packaging change checklist

- [ ] Confirm the stable user migration guide contains any still-relevant
      upgrade instructions from the RC-specific documents.
- [ ] Keep `README.md`, `CHANGELOG.md`, `docs/SECURITY.md`, architecture,
      configuration, transport, troubleshooting, ACP, DSL, and getting-started
      guides in the package.
- [ ] Remove the internal files above from `package.files`.
- [ ] Remove the same files from ExDoc `extras` and their documentation group in
      the same commit so `mix docs` works from an unpacked Hex package.
- [ ] Preserve repository links from release notes or contributor documentation
      where historical context remains useful.
- [ ] Run `mix hex.build`, inspect the tarball file list, and record compressed
      size before and after.
- [ ] Run `mix docs` with warnings as errors and verify that no retained guide
      links to an omitted local file.

This cleanup is packaging-only and can ship in rc.8 if the migration-content
check is complete. Otherwise, remove RC-specific documents immediately after
stable 1.0 rather than risking the loss of useful upgrade guidance.

## Execution order

1. Land rc.8's behavior-preserving internal helper deduplication.
2. Decide and, if safe, apply the Hex documentation cleanup separately.
3. Release stable 1.0 with no adapter decomposition mixed into the release diff.
4. Modularize Codex one characterized boundary at a time.
5. Modularize Pi one characterized boundary at a time.
6. Re-evaluate shared app-server pieces while preparing the post-1.0 ZCode
   adapter; keep vendor-specific protocol semantics separate by default.
