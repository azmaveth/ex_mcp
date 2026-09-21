# Post-1.0 Maintenance Plan

- **Status:** Stable 1.0 packaging and the focused contract cleanup are
  complete; the Codex characterization gate is met; adapter modularization
  and functional-core extraction remain proposed and tracked
- **Baseline:** ExMCP `1.0.0`
- **Scope:** behavior-preserving modularization, functional-core extraction,
  dependency cleanup, and Hex source-package cleanup
- **Last updated:** 2026-09-20

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
- Separate deterministic decisions from side effects where doing so creates a
  testable semantic boundary. Pass clocks, identifiers, resolved configuration,
  and working directories into pure code rather than reading process-global
  state there.
- Keep GenServers, Ports, ETS, HTTP clients, `Plug.Conn`, telemetry, and logging
  at orchestration edges. Pure cores may return tagged actions for those shells
  to execute; they must not pretend to be pure while calling `System`, `File`,
  `Application`, or process APIs internally.
- Do not create a shared Codex/ZCode abstraction merely because private
  functions have similar names. Share behavior only after golden tests prove
  that its inputs, outputs, errors, ordering, and lifecycle are identical.

The rc.8 work is limited to credential-free real-CLI lifecycle tests for the
Claude SDK, Codex, and Pi adapters; Pi configuration normalization/isolation;
reusable subprocess-environment, positive-option, and workspace-containment
helpers; and the Hex documentation cleanup below. The CLI tests do not send
prompts or call an LLM. The larger adapter changes below remain
deferred until after stable 1.0.

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

### Status as of 2026-09-05 (characterization gate)

The Codex characterization gate above is met on `master` by a golden-transcript
suite under `test/ex_mcp/acp/adapters/codex/characterization/` driven by
`ExMCP.Test.CodexGolden` (`test/support/acp/codex_golden.ex`), with one fixture
per scenario under `test/fixtures/acp/codex/<area>/`:

| Gate bullet | File | Scenarios |
|---|---|---|
| initialize and session lifecycle requests | `lifecycle_golden_test.exs` | 114 |
| prompt content conversion | `prompt_content_golden_test.exs` | 96 |
| permission options and response shapes | `permissions_golden_test.exs` | 83 |
| session update ordering | `session_updates_golden_test.exs` | 92 |
| MCP conversion and authorization failures | `mcp_config_golden_test.exs` | 110 |
| cancellation, late responses, fenced sessions | `faults_golden_test.exs` | 46 |
| model and mode catalog normalization | `catalog_golden_test.exs` | 83 |

Each scenario reaches its preconditions through the adapter's public
callbacks only (`init/1`, `post_connect/1`, `translate_outbound/2`,
`translate_inbound/2`), so the fixtures pin wire behavior rather than state
layout and must stay byte-identical across the boundary extractions below.
Every area was mutation-tested by an independent reviewer (a single-edit
behavior change to `codex.ex`, `config.ex`, `events.ex`, or `sessions.ex` must
fail at least one scenario); the misses that remained are wire-equivalent
resets (`accumulated_text`, `accumulated_thinking`) and are recorded in the
area moduledocs. Client-side timeouts and subprocess exit are owned by
`ExMCP.ACP.Client` and `AdapterBridge`, not by the adapter, and are covered by
their own tests rather than by this gate. See `docs/DEVELOPMENT.md` for the
regeneration workflow.

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

### Status as of 2026-09-20 (boundary extractions)

Three of the proposed boundaries are extracted on
`refactor/codex-modularization`, one commit each, with every golden fixture
byte-identical:

- `Codex.Permissions` (857 lines): approval tool-call and option
  construction, command decision options, permission request metadata, the
  user-input form schema and answer decoding, structured-decision decoding for
  every approval method, and the fail-closed cancel/late/closed-session
  responses. The root keeps pending-request state and the elicitation
  capability checks.
- `Codex.Content` (418 lines): ACP prompt block conversion, native
  `item/started` and `item/completed` mapping for every stateless item type,
  history replay, and streamed-text reconciliation. Functions return plain
  message lists; the root wraps them and keeps the stateful `agent_message`
  completion, which folds deltas into the session accumulators.
- `Codex.MCP` (226 lines): MCP server transport defaulting and validation,
  the native `mcp_servers` entries, and native config assembly (gateway
  providers, trusted projects, sandbox writable roots). Authorization stays in
  the root: `session_config/3` still authorizes the workspace, the additional
  directories, and each normalized server in list order, and passes the
  `trust_authorized_workspaces` policy flag in explicitly.

The root module is 3,284 lines (4,645 at the start of the branch). It still
owns the model catalog mapping, auth helpers, turn-failure and rate-limit
classification, session config authorization, and notification dispatch;
those become boundaries only if they clear the 100-line or repeated-decision
bar above.

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

### Status as of 2026-09-20 (characterization gate)

The Pi characterization gate above is met by a golden-transcript suite under
`test/ex_mcp/acp/adapters/pi/characterization/` driven by
`ExMCP.Test.PiGolden` (`test/support/acp/pi_golden.ex`, with the shared step
builders in `ExMCP.Test.PiGolden.Flows`), with one fixture per scenario under
`test/fixtures/acp/pi/<area>/`:

| Gate bullet | File | Scenarios |
|---|---|---|
| RPC messages for new, load, resume, fork, close, delete, prompt | `rpc_golden_test.exs` | 24 |
| control-group completion and failure ordering | `control_groups_golden_test.exs` | 16 |
| assistant/thinking/tool/usage stream-event conversion | `stream_events_golden_test.exs` | 19 |
| prompt queue, steering, follow-up, cancellation, subprocess exit | `prompt_flow_golden_test.exs` | 18 |
| model, thinking-level, and boolean configuration updates | `config_golden_test.exs` | 16 |
| slash-command expansion and available-command notifications | `slash_commands_golden_test.exs` | 15 |
| session-map and backing JSONL safety rules | `session_safety_golden_test.exs` | 15 |

Each scenario reaches its preconditions through the adapter's public
callbacks only (`init/1`, `translate_outbound/2`, `translate_inbound/2`,
`handle_adapter_message/2`, `list_sessions/2`, `shutdown/1`) inside a
per-run sandbox that holds the agent directory, session directory, session
map, working directory and a fake echoing `pi` executable, so the fixtures
pin wire behavior rather than state layout and no test reads the developer's
real Pi settings, prompts, models, or sessions. The fake executable also
makes managed-mode port writes and real subprocess exits observable.
Sandbox paths, minted `pi-N` / `tool-N` ids and near-now timestamps are
normalized (see the harness moduledoc), and the fixtures are byte-stable
across runs.

Every area was mutation-tested (a single-edit behavior change to `pi.ex` or
`pi/slash_commands.ex` must fail at least one scenario); the edit and the
scenario that catches it are recorded in each area's moduledoc so the check
can be repeated. Pi's startup banner (`Startup.build/3`, which inventories
`~/.pi` and `~/.agents`) and the final `File.cwd!/0` fallback for a missing
`cwd` are not characterized because they depend on the developer's machine.
See `docs/DEVELOPMENT.md` for the regeneration workflow.

Building the gate surfaced three agent-controlled payloads that made the
adapter raise instead of producing a transcript: a streamed tool-call event
carrying the call under `partial.content[contentIndex]` (`get_in/2` with an
integer index on a list, in the clause written to support exactly that
shape), a `get_available_models` payload whose `models` is not a list, and
catalog entries that are not maps. All three are fixed in 1.5.0 and pinned
by golden scenarios in the stream_events and config areas; each scenario
reproduces the original crash when the fix is reverted.

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

## Functional-core and effect-boundary follow-up

These extractions are candidates for the supported 1.x line after stable 1.0,
not a requirement to perform all of them. Each must land behind characterization
tests and remain independently revertible. If an extraction changes process
ownership, callback identity, restart behavior, cancellation, ordering, or a
public return shape, it belongs in the 2.0 roadmap instead.

The canonical 1.x compatibility gate is
[`V2_ROADMAP.md` section 8.1](./V2_ROADMAP.md#81-required-backport-tests); this
document's candidate lists do not weaken or replace it.

The preferred shape is a reducer such as
`transition(state, event, now) -> {new_state, actions}`. Actions can describe
effects such as `{:send, message}`, `{:reply, caller, result}`,
`{:schedule, deadline}`, or `{:emit, event}`. The owning process executes those
actions and feeds outcomes back as later events. This makes state transitions
exhaustively testable without weakening OTP ownership.

Before extracting a reducer, characterize action ordering, effect-failure
feedback, request correlation/idempotency, duplicate and late events, and timer
or cancellation races. Reducer events/actions are private implementation
contracts unless a separate public design explicitly says otherwise.

### Priority candidates

1. **Client request lifecycle** — extract request planning, correlation,
   timeout/cancellation decisions, and response reduction from
   `ExMCP.Client` and `ExMCP.Client.RequestHandler`. Keep transport calls,
   `GenServer.reply/2`, timers, and telemetry in the client process.
2. **Session lifecycle** — extract identity binding, initialization claims,
   replay ordering, retention, and expiry decisions from
   `ExMCP.SessionManager`. Keep ETS, monitors, clocks, logging, and subscription
   cleanup in the owner.
3. **HTTP client state** — extract option normalization plus Mint response/SSE
   event reduction from `ExMCP.Transport.HTTP`. Keep sockets, OAuth callbacks,
   process messages, and telemetry at the edge.
4. **HTTP server routing** — expand the existing `ExMCP.HttpPlug.Core` pattern
   to cover protocol-era, route, session, and response planning from plain data.
   Keep `Plug.Conn`, request-body reads, stores, and SSE streaming in the Plug.
5. **OAuth decisions** — extract redirect policy, callback parsing, discovery
   choices, and token-request construction from
   `ExMCP.Authorization.FullOAuthFlow`. Keep browser, listener socket, HTTP,
   credential-store, and transaction-store operations in the flow shell.
6. **ACP adapters** — use the Codex and Pi boundaries above as pure protocol,
   content, permission, configuration, event, and prompt-flow cores. The root
   adapters continue to own subprocesses and ACP lifecycle orchestration.
7. **ACP pending requests** — either promote `ExMCP.ACP.PendingRequests` into a
   real request-lifecycle core with explicit entry, resolve, cancel, expire, and
   late-response transitions, or remove the shallow map wrapper. Do not retain
   an abstraction that owns neither policy nor invariants.

### Shared HTTP framing

`ExMCP.Internal.PinnedHTTPClient`,
`ExMCP.Authorization.PinnedHTTPClient`, and
`ExMCP.Transport.HTTP.BoundedClient` contain overlapping Mint response
accumulation and bounded-body decisions. Extract one small pure HTTP event
reducer and contract suite while keeping DNS, target, TLS, redirect, OAuth, and
authorization policies in their current owners. Do not merge the policy layers
merely because all three use Mint.

#### Status as of 2026-09-20

Extracted. `ExMCP.Internal.HTTPResponseReducer` (`@moduledoc false`) owns the
pure mechanics only: `reduce(events, request_ref, acc, limits)` returning
`{:cont, acc} | {:done, acc} | {:error, reason}`, the empty accumulator,
`body/1`, `remaining_ms/2`, header normalization, the lenient
(`content_length_too_large?/2`) and strict
(`invalid_or_oversized_content_length?/2`) content-length checks,
`compressed?/1`, `conflicting_framing?/1`, `request_target/1`,
`default_port/1`, `address_family_options/1`, and `method_name/1`. The
reducer applies a caller-supplied `:validate_headers` function to every
`:headers` event, passing both the normalized batch and the accumulated list,
and returns the caller's error term untouched. Events for a foreign request
ref and unknown event shapes are skipped; the first `:done` ends the batch.

Each owner kept its policy, its socket and clock handling, and its shapes:

- `ExMCP.Internal.PinnedHTTPClient`: GET only, lenient content-length check
  on each header batch, no compression check, `{:ok, %{status, headers,
  body}}`, and `:fetch_failed` for every transport or Mint failure.
- `ExMCP.Authorization.PinnedHTTPClient`: httpc-style tuple with the status
  reason, `:compressed_response`, strict content-length on each batch,
  `:invalid_response` for a `:done` without a status, `:request_failed` for
  receive failures, Mint connect errors passed through, request-tuple parsing
  with `content-type` defaulting, and the TLS and `send_timeout` socket
  options.
- `ExMCP.Transport.HTTP.BoundedClient`: httpc-style tuple, `TargetPolicy`
  resolution, the request-size limit, `host` stripping plus forced
  `content-type` and `accept-encoding: identity`, `:compressed_response`,
  `:invalid_response_framing`, strict content-length over the accumulated
  header list (trailers included), and the `{:http_request_failed, _}`,
  `{:http_receive_failed, _}`, and `{:http_client_error, _}` error shapes.

Characterization suites live in
`test/ex_mcp/internal/pinned_http_client_test.exs`,
`test/ex_mcp/authorization/pinned_http_client_test.exs`, and
`test/ex_mcp/transport/http_bounded_client_test.exs`, driven through
`ExMCP.Test.RawHTTPServer`; the reducer has its own table-driven suite in
`test/ex_mcp/internal/http_response_reducer_test.exs`. Request-header
editing (`put_header/3`, `delete_header/2`, `put_header_if_missing/3`) stayed
in the owners because the two clients that have it disagree on
replace-versus-keep semantics.

## Focused correctness and contract cleanup

Resolve these as separate fixes, with the documented behavior and release lane
chosen explicitly before changing code:

| Area | Current mismatch or risk | Follow-up | Release lane |
|---|---|---|---|
| Circuit breaker clocks | `ExMCP.Reliability.CircuitBreaker.Core` calls `System.system_time/1`, despite presenting itself as a pure core. Wall time can also move backwards during duration calculations. | Pass `now_ms` from the process shell and use monotonic time for elapsed durations. Audit session expiry for the same distinction between wall-clock timestamps and elapsed time. | Eligible for 1.x as a characterized correctness fix; preserve timeout and telemetry behavior. |
| Session storage option | `ExMCP.SessionManager` documents `storage_backend: :persistent_term`, but its runtime always creates ETS state. | Specify the store contract and either implement the backend or deprecate the no-op option while continuing to accept it throughout 1.x. Do not leave a durability setting that silently does nothing. | Contract/backend may be additive in a later 1.x minor; option removal is 2.0-only. |
| Client fallback | `ExMCP.connect/2` documents a transport list as fallback, while the implementation selects only `List.first/1`. | Specify ordered errors, ownership, and cleanup before implementing fallback. If those semantics are not accepted, correct the docs and deprecate the list form while preserving 1.x acceptance. | A fully characterized spec-correctness fix may qualify for a 1.x minor; otherwise defer behavior change/removal to 2.0. |
| Stdio logging | `ExMCP.Internal.StdioLoggerConfig.configure/0` mutates VM-global Logger/Application/OTP logger behavior. | Route protocol output through a dedicated IO device and logs to stderr without changing unrelated host-application logging. | Document the hazard in 1.x; replace the global behavior in 2.0 unless compatibility evidence proves a safe 1.x path. |
| Client capability detection | Resource operations inspect the process dictionary's `$initial_call` to infer a modern client. | Replace the heuristic with an explicit internal connection-info or capability query. | Eligible for 1.x only with identical results for all supported client entry points. |
| Ambient inputs | Several paths read application/system environment, current directory, time, or generate IDs inside decision code. | Normalize configuration once at startup and pass resolved values into cores. | Internal injection is eligible for 1.x if precedence and generated wire values remain identical; precedence changes are 2.0-only. |

### Status as of 2026-09-02

Each row above was resolved on `master` after 1.1.1 and ships in 1.2.0 unless
noted:

- **Circuit breaker clocks:** resolved. `CircuitBreaker.Core` receives an
  injected monotonic `now_ms` from the process shell and no longer reads wall
  time for elapsed durations.
- **Session storage option:** resolved as a contract plus adapter. A standalone
  1.x store-adapter ADR and ETS-only contract suite were accepted,
  `SessionManager` dispatches through an internal `SessionStore` seam, an
  opt-in DETS backend (`storage_backend: :dets` with `:storage_path`) is
  available, and `:persistent_term` remains accepted with a warning that it
  uses ETS. ETS is still the default and its restart-empty behavior is
  unchanged.
- **Client fallback:** resolved as a documentation correction. `ExMCP.connect/2`
  now documents that only the first spec of a list is used; the list form stays
  accepted throughout 1.x.
- **Stdio logging:** the VM-global hazard is documented for 1.x. Replacing the
  behavior remains a 2.0 item.
- **Client capability detection:** resolved. Resource subscribe/unsubscribe
  decide modern versus legacy from `Client.get_status` protocol version behind
  an explicit Client process marker; `$initial_call` is no longer inspected.
- **Ambient inputs:** open. The clock injection above is the first instance;
  remaining reads are handled case by case under the same 1.x rule.

### Status as of 2026-09-05

The 1.3.0 line adds ACP client and adapter work that was driven by a
downstream consumer (Jido Harness) and is classified in
[`V2_ROADMAP.md` section 8.2](./V2_ROADMAP.md#82-current-classifications):

- **Handler message context:** `ExMCP.ACP.Client.Handler` gained optional
  `handle_session_update/4` and `handle_permission_request/5` variants that
  receive the decoded JSON-RPC message the client received. Both arities of
  each pair are optional; `HandlerRunner` refuses to start a handler that
  exports neither. Retained message data counts toward the handler update
  queue byte limit.
- **Adapter metadata namespace:** all adapter extension data lives under nested
  `_meta.ex_mcp.<adapter>`, matching the documented shape. `AdapterBridge`
  can tag adapter-derived messages with `_meta.ex_mcp.native` (adapter name,
  per-connection sequence, optional decoded native event) behind the
  `:native_events` option, which defaults to `:off` under the 1.x backport
  rule. Adapters implement the optional `name/0` callback.
- **Codex adapter correctness:** failed turns now fail the active prompt with
  a classified error, and streamed agent text is tracked per item so neither
  duplicate nor dropped messages reach chunk consumers. The second fix was
  found by review on the first; the per-turn accumulator had silently become
  ambiguous in a module that has grown to about 4,519 lines.
- **Dependency security:** mint 1.10.0. Cowlib 2.20.0 and Cowboy 2.19.0 were
  published on 2026-09-08 and locked here on 2026-09-16. State of the three
  Cowlib advisories, verified against the `2.20.0` tag and `master`:
  - `EEF-CVE-2026-43971` (`cow_link:link/1`): fixed. Commit `89da27ee` is an
    ancestor of the `2.20.0` tag, and EEF updated the advisory with
    `fixed: 2.20.0` on 2026-09-16, so `mix hex.audit` no longer reports it.
    The exception is removed.
  - `EEF-CVE-2026-43966` (`cow_http_struct_hd:escape_string/2`) and
    `EEF-CVE-2026-43969` (`cow_cookie:cookie/1`): not fixed, and not going to
    be. Both functions are byte-for-byte unchanged on Cowlib `master`. The
    maintainer closed every validating PR (ninenines/cowlib #154, #163, #164,
    #166, #169) and stated in #152 that the CVE "will likely remain as won't
    fix": Cowlib encoders expect RFC-valid input, and Cowboy 2.16+ and Gun
    2.4+ reject CR/LF at their own layer. The advisory metadata is accurate,
    so there is nothing to report to EEF. The exceptions have no review date;
    they stay for as long as ExMCP requires Cowboy, and
    `dependency_advisory_mitigation_test.exs` keeps locking the assumptions
    behind them.
  - Consequence: the only way to stop carrying audit exceptions for code
    ExMCP never calls is to stop requiring Cowboy. Bandit depends on
    `thousand_island`, `hpax`, `plug`, `websock`, and `telemetry` only, with
    no Cowlib in its tree. Making the HTTP server dependency optional (Cowboy
    optional, Bandit supported) was reserved for 2.0 because it is a breaking
    change for `transport: :http` consumers; it is now an accepted 2.0 item
    in `V2_ROADMAP.md` and a reason to bring 2.0 forward rather than wait for
    the rest of the 2.0 scope. PR #21 is the existing draft. GitHub #18 stays
    open until a downstream `mix hex.audit` passes without exceptions.

Related maintenance figures at this baseline: `ExMCP.ACP.Adapters.Codex` is
about 4,519 lines and `ExMCP.ACP.Adapters.Pi` about 2,553, up from the rc.7
figures quoted above, so the modularization sections below are more pressing,
not less. The existing `Codex.Sessions` helper covers session lookup and
update only; the lifecycle-transition boundary in the Codex plan remains.
Dialyzer reported 27 unnecessary entries in `.dialyzer_ignore.exs` on the CI
dialyzer version; 15 were pruned in 1.5.0 and the file now carries 26 entries.
The count is environment-dependent and that is the trap: the files under
`test/` are compiled only in the test environment, so a `MIX_ENV=dev` run
reports every test entry as unused even though the test run needs them.
Only the intersection was removed, and it was verified on both the current
toolchain (Elixir 1.19.5 / OTP 28.3.1) and the pinned CI dialyzer toolchain
(Elixir 1.17.3 / OTP 27.0), in both environments. `MIX_ENV=test` now reports
zero unnecessary skips; a `MIX_ENV=dev` run still reports the twelve
test-environment entries, which is expected and not a signal to remove them.
Record the toolchain and both environments with any future prune.

## Stdio byte-mode framing and internationalization

Tracked in GitHub #52. Diagnosed by deepfates in #41, which was withdrawn
before review; the diagnosis was correct and the unified fix sketched in that
thread is the shape adopted here.

### The defect

Both stdio transports let the VM's device encoding translate protocol frames.
That encoding is chosen by the process locale at VM start, and it has exactly
two values: `unicode` under a UTF-8 locale, `latin1` under anything else,
including no locale at all. Eleven locales were checked on 2026-09-20 (unset,
C, POSIX, ISO-8859-1, EUC-JP, Shift_JIS, GB18030, KOI8-R, and three UTF-8
variants); there is no third mode, so "other locales" is not a dimension the
fix has to generalize over.

| Transport | Device mode | Read | Write |
|---|---|---|---|
| `ExMCP.Server.StdioServer` (`IO.read`, `IO.puts`) | latin1 (launchd, systemd, minimal MCP hosts) | UTF-8 input is re-encoded byte by byte; `café` reaches the handler as `cafÃ©` (35 bytes for 11) | codepoints above U+00FF become `\x{65E5}` escapes inside the JSON string; the frame is not valid JSON |
| `ExMCP.ACP.Agent.Transport.Stdio` (`IO.binread(_, 1)`, `IO.puts`) | unicode (any UTF-8 developer shell) | each one-byte read returns the decoded codepoint as a latin1 byte, so `é` arrives as `0xE9` and anything above U+00FF fails with `no_translation` | unaffected |
| same | latin1 | unaffected | same corruption as the MCP server |

An echo tool hides the MCP case completely, because writing the double-encoded
text back through the same device reverses the damage exactly. The interop
lanes only ever send ASCII. Both facts explain why this shipped.

Two related findings from the same investigation:

- `IO.binwrite` on a unicode-mode device double-encodes. Switching the write
  calls without owning the device mode would move the corruption from one
  locale to the other.
- A byte-order mark before the first frame makes it undecodable, and the
  server silently drops undecodable lines by design (Mix.install noise), so a
  BOM-emitting host hangs at `initialize`.

Not affected: the client-side stdio transports move bytes through
binary-mode Ports, and the isolated child environment already passes `LANG`
and every `LC_*` variable through. The JSON layer round-trips astral
characters, surrogate-pair escapes, U+2028/U+2029, and decomposed sequences
unchanged, and rejects invalid UTF-8 on both encode and decode.

### The fix

1. **One owner for the rule.** An internal `ExMCP.Internal.StdioFraming`
   that asks a device whether it is a character (unicode) or byte (latin1)
   device and reads and writes it the matching way, which is byte-exact for
   valid UTF-8 in both cases, plus BOM stripping. The mode is consulted on
   every read and write, never cached and never changed: OTP 27 cannot
   switch a unicode-mode stdin after VM start (reads fail with
   `no_translation`), and OTP 27 and 28 both flip a unicode-mode stdio to
   latin1 for good when it meets input it cannot decode, so one bad line from
   a peer must not corrupt every frame after it. A read with the wrong view
   is not recoverable, so the view is never probed. `StdioServer` and the
   ACP transport both go through it; the ACP transport consults the mode
   once per frame and counts its limit in bytes, since a unicode read returns
   a whole character.
2. **Strip a leading BOM once** at stream start, in the same module.
3. **Docs.** `TRANSPORT_GUIDE.md`: the stdio transports own their device's
   mode; stdout is protocol-only, so nothing else may write to it, which was
   already the contract. Deployment note: Linux releases whose resource
   handlers touch non-ASCII filenames need `+fnu` in `vm.args`, because the
   VM's filename encoding is locale-driven on Linux (always UTF-8 on macOS);
   that is an application concern, not a transport one.
4. **Changelog** under Fixed, crediting deepfates and #41.

Nothing is pinned VM-wide. Known limitation: on OTP 27 under a UTF-8
locale, input already buffered when the io server meets an undecodable byte
is left half decoded and half raw while the device reports latin1 for all of
it, and the session ends; OTP 28 and newer drop the line and continue. A raw
fd port would bypass the io server but steals the descriptor from the tty
driver and cannot write, so it is not a library-default option.

### The test tier

One payload corpus, reused across transports, with the locale matrix applied
only where locale enters:

- **Corpus** (`test/support/i18n_corpus.ex`): Latin-1 (`café`), CJK, astral
  emoji, a joiner sequence, combining marks in decomposed form, right-to-left
  text with bidi controls, U+2028, an astral character delivered as a
  surrogate-pair `\u` escape (as other SDKs emit), and a multibyte payload at
  the frame-size limit so byte accounting is exercised rather than grapheme
  accounting.
- **Positive round trips, byte-exact:** MCP stdio server as a subprocess; ACP
  stdio transport through devices opened in unicode mode and in latin1 mode;
  HTTP client to `HttpPlug`; the in-process test transport.
- **Locale matrix, subprocess test only:** unset, `C`, `en_US.UTF-8`,
  `ja_JP.eucJP`, each with a tool that generates its own non-ASCII text, not
  an echo.
- **Negative cases:** BOM-prefixed first frame (accepted), CRLF-terminated
  frames (accepted), invalid UTF-8 (dropped without crashing on OTP 28 and
  newer; the stdio subprocess test sends it only there, and the choice not to
  answer `-32700` is documented in the test).

### Out of scope, tracked separately

- Boot-time logger output reaching stdout before `StdioLoggerConfig` runs.
  Resolved for ExMCP's own logs: `SessionManager` was the only boot-path
  module logging at `info` and now logs at `debug`, and a subprocess test
  boots the application's supervision tree under the default logger and
  asserts stdout stays empty. Other applications in the same VM remain the
  deployment's responsibility; the configuration guide now documents
  `stdio_mode: true` plus a stderr default handler as the stdio deployment
  setting. Replacing the VM-global suppression itself stays a 2.0 item under
  the "Stdio logging" row above.
- A Windows console CI lane. Byte mode is the right answer there too, but it
  has not been proven.

### Release lane and acceptance

A characterized correctness fix eligible for a 1.x patch or the next minor: no
wire change, no API change, identical behavior for ASCII payloads and for
properly configured devices. Done when the acceptance list in #52 passes:
the subprocess test under all four locales, the corpus byte-exact through
every transport, the three negative cases asserted, and the ASCII-only
interop lanes still green.

## Dependency-direction cleanup

At commit `4591af6`, `mix xref graph --format stats` reported eight dependency
cycles. Under Elixir 1.17.3 / OTP 27 the same command reports 22 cycles at both
the `v1.2.0` tag and the 1.3.0 baseline, none of them touching `lib/ex_mcp/acp`;
they sit in the transport, client, internal, and content modules. Treat 22 as
the current baseline and record the toolchain with any future count, since the
difference from the earlier figure is a measurement change rather than a
regression. Break the cycles through narrow dependency inversion rather than
moving code between large modules:

- move concrete `get_transport/1` selection out of the `ExMCP.Transport`
  behaviour and into a registry or factory;
- introduce a small revision catalog so version data does not cycle through
  `VersionRegistry`, `Protocol.Methods`, error codes, and generated types;
- have client operation modules call an internal request-executor contract
  instead of depending back on the public `ExMCP.Client` facade;
- replace the `MessageProcessor`/`MethodHandlers` mutual call with a one-way
  invocation boundary;
- separate content-validation rules and schema-policy resolution into acyclic
  decision modules; and
- move TLS option construction out of `ExMCP.Transport.HTTP` into a neutral
  security module so `ExMCP.Internal.Security` does not depend back on the HTTP
  transport that consumes it.

Record the cycle count in each cleanup PR and add an xref regression threshold
once the existing cycles are eliminated. Cycle removal is eligible for 1.x only
when runtime and compile-time characterization remains unchanged.

Reproduce the baseline with `mix xref graph --format stats` and inspect the
specific strongly connected components with
`mix xref graph --format cycles`. Update the commit anchor when this plan is
rebased onto a different maintenance baseline.

**Status as of 2026-09-20.** On Elixir 1.19.5 / OTP 28.3.1, `master` at
`94ba1bf` reported 9 cycles; after the `refactor(xref)` series the same
toolchain reports 2 (`mix xref graph --format stats`: 313 tracked files, 10
compile, 37 export, and 657 runtime edges, `Cycles: 2`). Seven small cycles
were broken, one commit each, with no public API, runtime, or compile-time
semantic change and the full unit suite (4679 tests, 0 failures) plus
`mix test.suite compliance` (591 tests, 0 failures) green after each step:

- `MessageProcessor` <-> `MessageProcessor.MethodHandlers`: the `assign/3`
  struct primitive moved down to `MessageProcessor.Conn` (`@doc false`);
  the public `MessageProcessor.assign/3` delegates to it and the handlers
  call `Conn.assign/3`, so dispatch is a one-way invocation boundary.
- `Content.Validation` <-> `Content.Validation.Rules`: `Validation` injects
  its custom-validator lookup into `Rules.apply_rule/4`; the persistent_term
  key and registered-validator semantics are unchanged.
- `Content.SchemaPolicy` <-> `Content.SchemaRemoteResolver`: the resolver
  takes the policy preflight function as an explicit argument
  (`resolve/3`), passed by `SchemaPolicy`, its only caller.
- `ExMCP` <-> `ClientConfig`: `ClientConfig` reads the library version from
  the existing `ExMCP.Internal.VersionInfo` instead of the `ExMCP` facade.
- `Server.Subscriptions` <-> `Tasks`: the store-invocation primitive behind
  `Tasks.get/2` lives in the new `@moduledoc false` `ExMCP.Tasks.StoreCall`;
  `Tasks` delegates to it and `Subscriptions` authorizes `taskIds` through it
  with the same owner map, so only `Tasks -> Subscriptions` remains.
- `Internal.SessionStore` <-> `SessionStore.DETS` <-> `SessionStore.ETS`:
  the behaviour's default-selecting `open/1` moved, unchanged, into the new
  `@moduledoc false` `ExMCP.Internal.SessionStore.Factory`, which
  `SessionManager` now calls.
- `Internal.VersionRegistry` <-> `Protocol.ErrorCodes` <-> `Protocol.Methods`
  (compile) <-> `Types`: the revision catalog suggested above now exists as
  the pure `@moduledoc false` `ExMCP.Internal.RevisionCatalog`. The registry
  sources its revision attributes from it and delegates `era_for/1`;
  `Methods`, `ErrorCodes`, and `Types` read the catalog instead of the
  registry. `VersionRegistry` remains the canonical registry for enablement,
  preference ordering, and version-specific behaviour.

Remaining (deliberately untouched; they are a separate decision because they
require the request-executor contract and the transport-registry/TLS moves
described above rather than a narrow inversion): the 13-module client cycle
through `lib/ex_mcp/client.ex` (its operations modules, connection manager,
era cache, notification listener, request handler, subscription, health
check, and reliability wrapper) and the 10-module transport cycle through
`lib/ex_mcp/transport.ex` (the HTTP transport and its header/SSE helpers,
local, stdio, test, security guard, and `Internal.Security`). No cycle in
the small set was skipped. Add the xref regression threshold once those two
are eliminated.

## Hex source-package documentation cleanup

The rc.7 `package.files` list ships 204,602 bytes (approximately 200 KB) of raw internal
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

- [x] Confirm the stable user migration guide contains any still-relevant
      upgrade instructions from the RC-specific documents.
- [x] Keep `README.md`, `CHANGELOG.md`, `docs/SECURITY.md`, architecture,
      configuration, transport, troubleshooting, ACP, DSL, and getting-started
      guides in the package.
- [x] Remove the internal files above from `package.files`.
- [x] Remove the same files from ExDoc `extras` and their documentation group in
      the same commit so `mix docs` works from an unpacked Hex package.
- [x] Preserve repository links from release notes or contributor documentation
      where historical context remains useful.
- [x] Run `mix hex.build`, inspect the tarball file list, and record compressed
      size before and after. The compressed package contents decreased from
      798,062 to 728,416 bytes; the outer Hex archive decreased from 819,200 to
      749,568 bytes.
- [x] Run `mix docs` with warnings as errors and verify that no retained guide
      links to an omitted local file. An unpacked-package link scan found no
      missing relative Markdown targets.

This packaging-only cleanup is complete for rc.8. The files remain available
in the repository, and packaged references to them use repository URLs.

## MCP conformance harness tracking

Keep release CI deterministic by pinning the reviewed modern conformance
harness in `scripts/conformance.sh`. Separately, the weekly `MCP conformance
upstream` workflow resolves the highest published
`@modelcontextprotocol/conformance` version and runs both complete 2026-07-28
suites. A manual dispatch can select an exact version for prerelease review.

The scheduled lane is intentionally advisory and never rewrites the pin. It
records the selected package version and uploads the complete client, server,
and runner logs even when the harness exposes a failure. For each upstream
failure, review the conformance release diff, determine whether the change is a
new protocol assertion or a harness regression, add focused local coverage for
newly required behavior, and advance the release pin only after the full suite
passes.

## ACP ecosystem and reference-adapter tracking

Post-1.0 ACP compatibility must cover both protocol conformance and differences
between real agent implementations. The repository therefore maintains a
reviewed manifest at `test/interop/acp_compatibility.json` with three distinct
inputs:

- membership of the public ACP agents page;
- IDs and versions from the machine-readable ACP Registry; and
- exact upstream revisions for `claude-agent-acp`, `codex-acp`, `pi-acp`, and
  `ZCode`, whose behavior informed ExMCP's Claude, Codex, Pi, and ZCode
  adapters.

`mix acp.compat.check` reports additions, removals, registry releases, and
reference-repository commits without installing or running remote catalog
content. A separate reviewed matrix runs credential-free initialization against
version-pinned native ACP commands in isolated scratch environments. It starts
with Claude Agent ACP, Codex ACP, Gemini CLI, and Pi ACP; expand it toward every
documented agent as installation, platform, licensing, and authentication
requirements are characterized.

When reference-adapter drift appears, review the compare link for protocol
mapping, capability, event-ordering, security, and lifecycle changes before
advancing the pinned commit. Port relevant behavior behind characterization
tests; a pin update alone is not evidence that ExMCP remains behaviorally
aligned.

### 2026-08-22 reference sync

The first scheduled-review baseline now pins Claude Agent ACP
`996d488589b8db7a0f9af3dfc7b886d9d47ebae9`, Codex ACP
`ba5bcc3d7759250dde9d4d2286a1bec11b363208`, and Pi ACP
`d1cffc047ab37a096ee70ca39cfc1de463db8d12`. The review produced characterized
adapter fixes rather than a pin-only update:

- shared ACP form/URL elicitation, explicit per-mode capability negotiation,
  URL completion, and validation;
- Claude `AskUserQuestion`, truthful durable permissions, Exit Plan effects,
  dynamic modes, the SDK marker update, and background-subagent settlement;
- Codex close/delete fencing, structured non-secret user input, MCP URL
  completion, and request-scoped device authentication;
- Pi's `agent_settled` completion boundary and select/confirm extension UI
  response bridge.

Follow-up reviews should promote these cases into live CLI or deterministic
fixture tiers when the upstream CLIs expose a credential-free trigger. The
current real-CLI lifecycle suite deliberately avoids prompts and therefore
cannot exercise LLM-originated permission, elicitation, or background-task
events; the adapter unit tests are the executable evidence for those paths.

### 2026-08-25 pending reference drift

Codex ACP development moved from `zed-industries/codex-acp` to
`agentclientprotocol/codex-acp`; the manifest now follows the canonical
repository while retaining the last behaviorally reviewed commit. Do not
advance the Claude or Codex reference pins until the following post-baseline
changes have focused ExMCP parity decisions and tests:

- Codex `8ff9e67f79335345ce53b3157b3d690c191ea027` adds permission presentation,
  provider decision preservation, and permission lifecycle isolation;
- Codex `50f69e57ca761ccafd2ca29de7fb591068277516` changes mode presentation and
  adds `_meta.kind` semantics; and
- Claude `caf609b56c91f677ffe82b6e9d11d9e9dfd99d45` advertises a stable mode
  catalog, adds `_meta.kind`, and falls unsupported Auto mode back to Accept
  edits with a client-visible warning.

These are genuine unreleased behavior changes rather than repository-move
noise. Keeping the reviewed commits pinned makes the scheduled drift check
continue to report the work until parity is deliberately accepted or ported.

### 2026-09-01 pin refresh and remaining parity decisions

The manifest was subsequently advanced to Claude Agent ACP
`7c6610835f26f18cd162b78dff74a7b7cd74497a` and Codex ACP
`4823131475b3b0d996ccc305e49dcf9fdaa6ee52`; Pi ACP is unchanged. The Codex
1.7.0 permission and mode parity work (`approvalsReviewer`, mode `_meta.kind`,
and TypeScript-style permission presentation) is ported with tests and covers
the two Codex commits listed above. The pins now lead the reviewed behavior,
so the drift check no longer reports the following upstream changes; each
still needs an explicit parity decision and, where accepted, characterized
adapter work before it is claimed as supported:

- Claude `caf609b` stable mode catalog with `_meta.kind` and the Auto-mode
  fallback to Accept edits with a client-visible warning;
- Claude per-model token usage on prompt responses, deferred steering while
  user input is pending, native subagents and async tasks, and
  message-specific ACP session forks;
- Codex native ACP subagent sessions, ACP session forks, and AI session title
  generation with a `/rename` command.

Record the decision for each item here before the next pin refresh; a pin
that leads the reviewed behavior must not be advanced again until this list is
resolved.

### 2026-09-20 drift review and parity decisions

`mix acp.compat.check` reported Claude Agent ACP at
`d421f56a6c43cde16d9a7531d08a750a5ef2f04a` (0.79.0, 39 commits past the pin)
and Codex ACP at `d7b07c1b44a28890cdf3d5450f8974a812db5ae2` (1.12.0, 29
commits), plus two new registry agents and 23 registry version moves. Both
references still build on ACP SDK 1.4.0, the version ExMCP pins and the
newest on npm, so none of the new capabilities are schema changes; they are
extensions negotiated through `_meta`.

Ported, each behind fixture tests (see the 1.5.0 changelog):

- Claude per-session opt-out of `bypassPermissions` (claude-agent-acp#1129);
- Claude AskUserQuestion custom text kept beside the pick (#1031, #1131);
- Codex `request_user_input` form shapes (codex-acp#299); and
- Codex paginated history on `session/load` (codex-acp#481).

Deferred, pre-standard extensions not in SDK 1.4.0, to be revisited when the
weekly ecosystem workflow reports an SDK release that carries them: the
`authStatus` push notification (#1080, #467), `recommendedValue` model and
effort hints (#1111, #491), `asyncTasks` for background terminals (#460),
the tool-call `name` field from an RFD (#1128, #513), and the compaction
mechanisms (#991, #1134, #515), which fold into the existing compaction
decision above.

Not applicable: codex-acp#471 (standalone MCP elicitation finalization),
because ExMCP forwards MCP elicitations without a synthetic tool call, so
nothing dangles. Upstream-internal: CI, dependency and Codex CLI version
bumps, fork-loading performance, TaskList parsing, model display-name
cosmetics. Kept as ExMCP's own surface: the Claude main-thread agent config
option, removed upstream in #1112; ExMCP retains it through 1.x.

Still open from the 2026-09-01 list, deferred to a later minor: the Claude
stable mode catalog with `_meta.kind` and the Auto-mode fallback, per-model
token usage, deferred steering while input is pending, message-specific
forks, and Codex session titles with `/rename`. Native subagents and async
tasks on both sides are covered by the `asyncTasks` deferral above.

The manifest pins now advance to the reviewed heads. The pins lead the
open items above, so they must not advance again until those are decided.

### 2026-09-21 ZCode source baseline

ZCode's newly published source repository is tracked from its first public
`main` revision, `872ad960de7ec172591f7e1952f7849229f94521`. The weekly
ecosystem check now reports later `zai-org/ZCode` commits with a direct compare
link, alongside the Claude, Codex, and Pi reference adapters. ZCode Protocol v1
remains the adapter's production boundary in that revision; upstream also
contains an in-progress V4 wire used by its own clients. Treat V4 drift as a
separate migration signal rather than silently changing the adapter's wire
version.

## ACP v1 completion and v2 monitoring

The July 2026 stable ACP v1 additions are represented in the runtime and
adapter tests. Boolean session config options require an explicit v1 client
capability, so ExMCP provides `Capabilities.put/3` with
`:boolean_config_options` and exercises the opt-in in both directions against
the official TypeScript SDK. Do not auto-advertise this capability merely
because a generic event handler can decode the update; the integrating client
must be able to present and change the value correctly.

ACP protocol v2 is Draft and is not part of ExMCP's advertised production
surface. The pinned interop lane validates the reviewed v1 and v2 schemas,
while the scheduled ACP ecosystem workflow installs the newest SDK to detect
release or schema drift. Version downgrade and SDK dual-router tests protect
continued v1 operation. The versioned architecture, schema-review procedure,
and Preview and Stable adoption gates live in
[`ACP_V2_TRACKING.md`](./ACP_V2_TRACKING.md).

The SDK 1.4.0 unstable compaction experiment is tracked but deliberately not
advertised or implemented. Revisit it only after the capability and update
contract enter the specification; until then, vendor-native compaction events
remain adapter details rather than claims of protocol-level support. The
removed experimental `env_var` auth variant remains available only through the
existing, disabled-by-default Codex legacy compatibility option.

## Execution order

1. Land rc.8's credential-free ACP CLI lifecycle coverage, Pi isolation fix,
   behavior-preserving internal helper deduplication, and Hex documentation
   cleanup.
2. Qualify and publish rc.8, then run the fresh final-candidate soak. **Complete.**
3. Release stable 1.0 with no adapter decomposition mixed into the release diff. **Complete.**
4. Resolve the focused contract mismatches as small correctness or documentation
   changes. **Complete for the 1.x lane** (see the status list above); ambient
   input injection continues case by case.
5. Extract the shared HTTP reducer and the smallest high-value functional cores
   behind characterization tests.
6. Modularize Codex one characterized boundary at a time. `Codex.Protocol`,
   `Codex.Permissions`, `Codex.Content`, and `Codex.MCP` are extracted and
   `Codex.Sessions` holds lookup/update helpers; the lifecycle `Sessions`
   boundary remains, and the root still owns the model catalog, auth helpers,
   turn-failure classification, and session config authorization. The 1.3.0
   failed-turn and per-item streamed-text logic is a natural seed for the
   event-folding and prompt-flow cores. This is behavior-preserving internal
   work: it lands on `master` behind golden tests and ships with the next
   user-visible release rather than forcing a release of its own.
7. Modularize Pi one characterized boundary at a time. `Pi.RPC` is extracted;
   `Sessions`, `Events`, `PromptFlow`, and `Config` remain.
8. Reduce dependency cycles without changing public or lifecycle semantics.
9. Re-evaluate shared app-server pieces while preparing the post-1.0 ZCode
   adapter; keep vendor-specific protocol semantics separate by default.
10. Make any MCP/ACP package-topology change only through the 2.0 decision and
    migration process in `V2_ROADMAP.md`.
11. Expand the reviewed native ACP matrix and promote agents from initialization
    to session and mock-prompt tiers where their supported configuration permits
    credential-free testing.
12. Keep ACP v2 monitoring non-shipping until its Preview adoption gates are
    met; then implement separate v1/v2 protocol surfaces around shared session
    and effect cores.
13. Fix stdio byte-mode framing for both stdio transports and land the i18n
    payload tier (GitHub #52) as a 1.x correctness fix, ahead of the
    modularization items: it is small, it is user-visible data corruption in
    common deployments, and its subprocess test is the first locale-aware gate
    in CI.
