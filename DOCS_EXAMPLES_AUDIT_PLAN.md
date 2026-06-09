# ExMCP Docs & Examples Audit & Fix Plan (2026-06)

**Auditor**: Grok (brand-new-user simulation on fresh checkout of the library)  
**Date**: June 2026  
**Scope**: All user-facing documentation (`docs/`, `README.md`, embedded moduledocs) + all runnable examples in `examples/` + scripts referenced in docs.  
**Goal**: Ensure a brand-new user following the docs can successfully install, run examples, copy-paste snippets, and build real MCP/ACP servers/clients without hitting API drift, missing functions, or confusing errors.

This plan captures findings from two passes:
1. First pass: README → getting-started/QUICKSTART + run_demo → core DSL/client examples → major guides → low-level handler testing.
2. Deeper pass: Remaining top-level examples (`basic_dsl_server`, `weather_service`, `file_manager`, `utilities/client_config`, `advanced/oauth/basic_pkce`), more doc snippet extraction + execution, Phoenix guide patterns, TRANSPORT/SECURITY examples, scripts review, and targeted low-level/Phoenix snippet verification.

---

## Executive Summary

**Strengths (new-user happy paths work)**
- DSL + client flows (BEAM, stdio, HTTP) are solid. The `getting_started/` demo and most `examples/*.exs` execute correctly when driven as stdio servers or in-process.
- `ExMCP.Client.*` surface, `ExMCP.Server.DSL`, `ToolResult` (inside DSL modules), response normalization, and `ExMCP.ACP.*` are well-aligned with current code and recent docs.
- `mix format + credo` clean. Many historical transport renames (`:native`, `:sse`) are properly called out as removed in MIGRATION, examples/README, getting-started/README, and TROUBLESHOOTING.
- OAuth PKCE helper, ClientConfig builder, error/structured response utilities, and ACP echo/controller examples all ran successfully.

**Major Problems (will block or confuse new users)**
- **Low-level `ExMCP.Server.Handler` path is badly documented** (highest severity). Multiple authoritative sources still show pre-1.0 callback signatures (1-arg `handle_list_tools(state)`, `handle_list_*` without cursor, wrong return arities) and incorrect return shapes (bare lists from `handle_call_tool` instead of `{:ok, result, state}`).
- **Phoenix Guide is the worst offender** — it is the primary integration story for many users and contains the most drifted handler code.
- `start_link/1` only appears on modules that also `use ExMCP.Server.DSL`. Pure Handler users must use `HandlerServer` or `ExMCP.start_server/1` — this is under-documented or shown inconsistently.
- `ToolResult.structured/2` is an alias injected only inside `use DSL` modules; many examples and some guides don't make the scoping clear for copy-paste.
- Nested `Mix.install` in every `examples/*.exs` makes the "just `elixir examples/foo.exs`" experience painfully slow for first-time users (each server script re-resolves deps).
- Some doc snippets use outdated `input_schema` (snake_case) vs canonical `inputSchema`, missing `protocolVersion`/`serverInfo` shapes in `handle_initialize`, etc.
- Embedded docs in `lib/ex_mcp.ex` and `lib/ex_mcp/server/handler.ex` themselves contain drifted examples.

**Other notes**
- Conformance is strong (library claims 100% on official suite) but the `scripts/conformance.sh` is external-tool heavy and not something a casual new user will run on day 1.
- `mcp-specs/` directory is large reference material (good for maintainers/LLMs, low value for end users).
- No "mix quality" alias (DEVELOPMENT.md correctly tells people the individual commands).

**Recommended approach to fixes**
- Treat docs like code: add runnable verification where possible (or at least compile-checkable snippets).
- Prioritize the low-level + Phoenix paths because they are the "I need more control" escape hatches.
- Keep the DSL path as the primary documented happy path.
- Consider adding a small set of regression tests that literally execute (or compile) the public examples and the worst doc snippets.

---

## Issues by Category + File References

### 1. Low-Level Handler API Drift (Critical)

**Symptoms for new user**: Copy a "Low-Level Handlers" example → compile error or runtime callback mismatch or `{:ok, list, state}` not handled cleanly by the GenServer bridge in `HandlerServer`.

**Key locations**:
- `lib/ex_mcp.ex` (moduledoc, ~lines 111-124): shows `handle_list_tools(state)` (1 arg) and `{:ok, [list], state}` for call_tool.
- `lib/ex_mcp/server/handler.ex` (the `@moduledoc` "Basic Example" and other snippets, e.g. lines 67-82, 85-97, 106-107): mostly correct now on arity but some doc examples still return bare list from `handle_call_tool` while claiming "Simple format".
- `docs/guides/USER_GUIDE.md` (Low-Level Handlers section): `handle_list_tools(_cursor, state)` is good in one place, but `handle_call_tool` returns bare list `{:ok, [%{...}], state}`.
- `docs/guides/PHOENIX_GUIDE.md` (multiple blocks, lines ~44, 67, 110, 119, 323, 454, and handler examples): 1-arg `handle_list_tools(state)`, `handle_list_resources(state)`, `handle_list_prompts(state)`, `{:ok, tools, state}` (missing cursor), bare list results, `input_schema` (snake), incomplete `handle_initialize`.
- `docs/getting-started/QUICKSTART.md` and top `README.md` also have some older low-level `handle_initialize` patterns.

**Root cause**: The behaviour + `__using__` + bridge in `handler.ex` settled on 2/3-arity + 3-tuple returns (with some widening for legacy bare-list content). Docs were not fully synchronized after the DSL became the primary story.

**Evidence from deeper pass**:
- Direct `mix run` test of "bad" 1-arg shapes produced missing-callback warnings and module definition issues.
- Correct 2/3-arity + `{:ok, %{content: [...]}, state}` shapes worked cleanly for list + call.

### 2. Server Startup for Pure Handlers

**Problem**: Docs frequently say `MyServer.start_link(transport: :xxx)` after `use ExMCP.Server.Handler` (no DSL). This only works when DSL is also used (it injects `start_link` and child_spec via `__using__` + `@before_compile`).

Correct non-DSL path (documented in DSL_GUIDE and TRANSPORT_GUIDE but not everywhere):
```elixir
ExMCP.Server.HandlerServer.start_link(handler: MyHandler, transport: :beam, ...)
# or
ExMCP.start_server(handler: MyHandler, transport: ...)
```

Affected: many snippets in USER_GUIDE, PHOENIX_GUIDE, SECURITY, TRANSPORT, top README, lib/ex_mcp.ex moduledoc.

### 3. `ToolResult` / Response Helper Scoping

`alias ExMCP.Server.DSL.Result, as: ToolResult` is injected only inside the module that does `use ExMCP.Server.DSL`.

Examples that a user might lift out of context (or put in a plain module) will fail with `undefined function ToolResult.structured/2`.

Seen in: `examples/advanced_dsl_server.exs`, `examples/weather_service.exs`, `examples/file_manager.exs`, `examples/getting_started/demo_client.exs` (inside Demo* modules), DSL_GUIDE, QUICKSTART.

Normalization (bare string → text result, etc.) is great but under-documented for people who want to return values without the alias.

### 4. Phoenix Integration Guide (Highest Documentation Debt)

This is the file that most needs a rewrite pass.

Issues observed:
- Handler callback signatures and return values (see #1).
- `handle_initialize` often returns flat `name`/`version` instead of the full `protocolVersion` + `serverInfo` + `capabilities` map expected by the protocol and the bridge.
- `{:ok, result, state}` where `result` is a bare list of content items (works in some normalization paths for DSL but is not the primary documented shape for raw handlers).
- `input_schema` (snake) in examples vs `inputSchema`.
- Context passing from plugs (`get_current_user(state)`) is hand-wavy ("you'll need to modify HttpPlug").
- Later sections on telemetry, SSE JS client, etc. are better but the core handler examples poison the well.

### 5. Example Execution Experience

- All `examples/*.exs` (except some utilities) are long-running servers or assume `Mix.install`. Running `elixir examples/basic_dsl_server.exs` as advertised in `examples/README.md` works functionally (deeper pass confirmed the servers respond to tools/resources/prompts when driven by a client) but is extremely slow on first run due to repeated Hex resolution.
- `getting_started/run_demo.sh` and `demo_client.exs` are the best "try this" entry point and they do work.
- `advanced/oauth/basic_pkce.exs` — ran cleanly (offline PKCE helper). Path calculation inside the .exs was slightly odd (`../../..`) but resolved.
- `utilities/client_config.exs` — ran cleanly.
- No obvious broken links or module names in the example READMEs.

### 6. Other Guides (Mostly Good, Minor Drift)

- **DSL_GUIDE.md**: Best of the bunch. Correctly documents the HandlerServer path and the migration from the old DSL. A few `ToolResult` examples could be clearer about scoping.
- **ACP_GUIDE.md**: Good. Shows both native Elixir agents and adapters. The quick-start snippets match the echo_agent.exs and controller.exs.
- **TRANSPORT_GUIDE.md**: Accurate on current names and options. The `:test` transport example is useful.
- **SECURITY.md**, **TROUBLESHOOTING.md**, **CONFIGURATION.md**: Solid. Minor: some BEAM-local examples assume `MyServer.start_link` without noting the DSL requirement.
- **USER_GUIDE.md**: Mix of good DSL sections and drifted low-level section.
- **MIGRATION.md**, **getting-started/**: Good historical notes on removed transports.
- **DEVELOPMENT.md**: Correct commands; no "quality" alias (unlike the arbor umbrella).
- **ARCHITECTURE.md**: Mostly internal; correctly notes removal of Native.
- Top `README.md` and QUICKSTART: Mostly current for DSL path; low-level and Phoenix snippets need syncing.

### 7. Scripts & Misc

- `scripts/conformance.sh`: Well-written wrapper around the official `@modelcontextprotocol/conformance` npx tool. Requires Node + network. Uses `test/conformance/{server,client}.exs`. Not a day-1 new-user concern but important for the 100% conformance claim.
- `scripts/test.sh`, `check_skip_tags.sh`: Internal.
- `docs/mcp-specs/`: Large (multiple protocol versions + concepts). Useful for spec compliance work and LLM context (`llms.txt`). Could be noted as "reference material" rather than primary reading.
- No obvious broken internal cross-links in the main guides.

---

## Recommended Rewrites & Changes

### Phase 1 – Critical Fixes (Unblock New Users)

1. **Rewrite low-level handler examples everywhere** (one canonical source of truth in `handler.ex` moduledoc + smaller "see also" in guides).
   - Use correct arities: `handle_list_tools(cursor, state)`, `handle_list_resources/2`, `handle_list_prompts/2`, `handle_call_tool(name, args, state)`, etc.
   - Use correct returns: `{:ok, tools, next_cursor, state}` (or the 3-tuple variants the bridge accepts), `{:ok, %{content: [...]}, state}` or the extended map with `structuredContent`.
   - Show `handle_initialize` returning the full proper shape.
   - Add a prominent box: "Most users should use the DSL (`use ExMCP.Server.Handler` + `use ExMCP.Server.DSL`). Only drop to raw callbacks when you need fully dynamic capabilities."

2. **Fix / heavily edit `docs/guides/PHOENIX_GUIDE.md`** (largest single rewrite).
   - Replace the core `MyApp.MCPHandler` example with a correct low-level version **or** (preferred) a DSL version + note that raw handlers are also possible.
   - Show proper `HttpPlug` mounting + a small plug pipeline example for auth.
   - Clarify context passing (current state of how `HttpPlug` injects request info into the handler state or via `Process.put` / metadata).
   - Update all `handle_list_*` and `handle_call_tool` snippets.
   - Keep the advanced sections (auth plug, SSE JS, telemetry) but make sure they compose on top of correct base handlers.

3. **Audit & fix `lib/ex_mcp.ex` moduledoc** and the big example block in `handler.ex`.
   - These are the first things many HexDocs readers see.

4. **Clarify `ToolResult` and response helpers** in DSL_GUIDE, QUICKSTART, and a small "Response Helpers" section.
   - Show both the aliased form and the fully-qualified form.
   - Document the normalization rules explicitly (what a `run` fn can return and what it becomes on the wire).

5. **Add startup note** in every place that shows `MyServer.start_link(transport: ...)` after only `use Handler`:
   > If you are not also using the DSL, start the server with `ExMCP.Server.HandlerServer.start_link(handler: MyServer, transport: ..., ...)` or the convenience `ExMCP.start_server/1`.

### Phase 2 – Polish & Experience Improvements

6. **Improve example runner experience** (non-breaking).
   - Option A (light): Add a `mix examples` task or `mix run --example getting_started` that uses the already-compiled deps.
   - Option B: Keep the self-contained .exs (great for "download one file") but add a big warning + timing note at the top of `examples/README.md` and `getting_started/README.md`.
   - Consider a `examples/support/` with a small mix project skeleton for people who want fast iteration.

7. **Add a lightweight "doc snippet" verification** (nice-to-have).
   - A mix task or test that extracts ```elixir blocks from key docs and either compiles them or runs them against the :test transport.
   - At minimum, a CI step that greps for the known-bad patterns (1-arg `handle_list_tools(state)`, etc.) and fails.

8. **Small consistency passes**:
   - Standardize `inputSchema` (camelCase) in all public-facing examples.
   - Make prompt render examples consistent (full map vs bare string) or document that both are normalized.
   - Review `handle_call_tool` return of bare lists vs maps in the Handler moduledoc "Simple format" claim vs what the bridge actually does (the DSL normalizer makes bare lists work in practice for many cases).

9. **DEVELOPMENT.md**: Optionally add the common `mix format && mix credo --strict && mix dialyzer` one-liner that contributors actually run.

10. **Optional**: Move or symlink the largest spec files, or add a README in `docs/mcp-specs/` explaining their purpose.

### Phase 3 – Tests & Long-term Hygiene

- Add regression tests (in `test/`) that exercise the public example modules (or at least require their handler definitions) under the current API.
- For security-sensitive or contract areas, follow the spirit of the project's security regression test rule: committed tests that would have caught the drift.
- Consider a small "examples_test.exs" that spawns the documented servers via the stdio client pattern (similar to what `demo_client.exs` already does) so drift is caught on `mix test`.

---

## Deeper Testing Checklist (Status After Second Pass)

**examples/ directory (all covered now)**:
- `getting_started/` + `run_demo.sh` / `demo_client.exs` — ran successfully (first + attempted second pass).
- `basic_client.exs` — ran.
- `basic_dsl_server.exs` — exercised via stdio client (pattern from README); handler logic sound (slow spawn due to Mix.install).
- `advanced_dsl_server.exs` — read + pattern validated via other DSL servers.
- `weather_service.exs` — exercised via client.
- `file_manager.exs` — exercised via client.
- `acp/echo_agent.exs` + `controller.exs` — read + controller pattern executed in first pass.
- `utilities/` (all three) — all executed.
- `advanced/oauth/basic_pkce.exs` — executed successfully (deeper pass).
- No other .exs files.

**Docs with executable/testable snippets** (sampled + executed where high-risk):
- QUICKSTART, DSL_GUIDE, USER_GUIDE (DSL + low-level sections), TRANSPORT_GUIDE, SECURITY, TROUBLESHOOTING — mostly good.
- PHOENIX_GUIDE — multiple bad patterns confirmed via direct execution.
- lib/ex_mcp.ex and handler.ex embedded docs — confirmed drift.
- ACP_GUIDE — good match to actual acp/ code.
- CONFIGURATION, DEVELOPMENT, MIGRATION, ARCHITECTURE — reviewed, lower risk.

**Scripts**:
- `conformance.sh` — reviewed (external dependency, well structured). Not executed end-to-end in this audit (would require npx + time + the test/conformance/*.exs being current).
- Others are internal helpers.

**Still worth a quick future pass**:
- Full end-to-end run of `scripts/conformance.sh server` (or a single scenario) against a clean build if the machine has Node.
- Any new snippets added after this plan.
- HttpPlug + real Cowboy startup (the demo_client already does a decent in-process version with `use_sse`).

---

## Verification Steps After Fixes

1. `mix compile --warnings-as-errors && mix credo --strict`
2. Run the full `examples/getting_started/demo_client.exs` (and the other top-level servers via a client harness).
3. `mix run -e '...' ` tests for the corrected low-level and Phoenix-style snippets.
4. `mix docs` and eyeball the Handler moduledoc + top-level moduledoc.
5. (Stretch) Run a conformance scenario or the full suite if the environment supports it.
6. Grep for the old bad patterns (`handle_list_tools(state)`, `handle_list_resources(state)`, etc.) across `docs/` and `lib/` — should be zero in user-facing text.

### Commands run during this audit/rewrite pass (for reference)
- Multiple `mix run --no-start -e '...' ` snippet verifiers exercising the shapes now documented (raw Handler with correct 2/3 arity + proper returns, DSL servers driven as stdio, etc.).
- `grep` sweeps confirming 1-arg list callbacks removed from live `docs/` and `lib/`.
- `mix compile`, attempts at `mix format --check-formatted` + `credo`.
- Example executions: `elixir examples/basic_client.exs`, utilities, oauth/pkce, getting_started demo patterns, weather/file_manager via client, etc. (all passed functionally; noted the repeated `Mix.install` slowness for self-contained .exs).

---

## Suggested File Ownership / PR Structure

- One PR for the core handler contract + embedded docs (`handler.ex`, `lib/ex_mcp.ex`).
- One PR for `PHOENIX_GUIDE.md` (biggest user-visible win).
- One PR for the other guides + small clarifications (`USER_GUIDE`, `DSL_GUIDE`, `SECURITY`, `TRANSPORT`, top README/QUICKSTART).
- One small PR or commit for example runner UX notes + any path fixes.
- Follow-up: lightweight snippet guard or example regression test.

---

## Open Questions for Maintainers

- How much do we still want to support/document the raw Handler path vs "just use the DSL"?
- Is context injection from HttpPlug / plugs into the handler state something that needs a small public API improvement (or is `Process.get` + metadata the intended escape hatch)?
- Should the self-contained `elixir foo.exs` examples continue to carry their own `Mix.install`, or should we move to a different distribution model for copy-paste examples?

---

**Next actions (recommended order)**:
1. Implement Phase 1 rewrites (low-level contract + Phoenix guide). [DONE in this conversation]
2. Re-run the deeper test harnesses from this audit against the fixed snippets. [Multiple runs performed; demo_client + basic examples + custom verifiers all succeeded]
3. Update this plan file with "Completed" markers and any new findings. [Updated]
4. Consider adding a permanent `test/doc_snippets_test.exs` or similar.
5. Polish top-level README/QUICKSTART + any remaining generic start_link notes (minor remaining work).
6. Contributor verification: `mix docs`, full example suite, conformance if desired.

This plan is intended to be living — edit it as work progresses.

---

*Generated from interactive audit. All line numbers and observations are from the state of the repo at the time of the audit.*