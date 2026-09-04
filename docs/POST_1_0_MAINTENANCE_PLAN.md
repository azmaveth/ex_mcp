# Post-1.0 Maintenance Plan

- **Status:** Stable 1.0 packaging and the focused contract cleanup are
  complete; adapter modularization and functional-core extraction remain
  proposed and tracked
- **Baseline:** ExMCP `1.0.0`
- **Scope:** behavior-preserving modularization, functional-core extraction,
  dependency cleanup, and Hex source-package cleanup
- **Last updated:** 2026-09-04

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

## Dependency-direction cleanup

At commit `4591af6`, `mix xref graph --format stats` reports eight dependency
cycles. Break them through narrow dependency inversion rather than moving code
between large modules:

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
- exact upstream revisions for `claude-agent-acp`, `codex-acp`, and `pi-acp`,
  whose behavior informed ExMCP's Claude, Codex, and Pi adapters.

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

## Cowlib CVE-2026-43971 tracking

GitHub [#18](https://github.com/azmaveth/ex_mcp/issues/18) and Linear
[AZM-5](https://linear.app/azmaveth/issue/AZM-5/ex-mcp-p0-track-cowlib-cve-2026-43971-github-18)
track the Hex advisory that currently fails `mix hex.audit` in downstream
root projects. Rechecked **2026-09-04**. Do not close those issues until a
patched Cowlib is on Hex and a fresh downstream resolution passes
`mix hex.audit` with the ExMCP exception removed.

| Item | Status on 2026-09-04 |
|---|---|
| Locked version | `cowlib` **2.19.0** in `mix.lock` (`6dc66e3135b229193ea4dcb14294e79520c923d391315c9c962ef0b4bea72356`) |
| How it enters the tree | Transitive only: `ex_mcp` → `plug_cowboy 2.9.0` → `cowboy 2.18.0` → `cowlib >= 2.19.0 and < 3.0.0`. ExMCP does not depend on or call cowlib APIs. |
| Latest Hex / GitHub tag | Still **2.19.0** (Hex `updated_at` 2026-07-28). No `2.19.1` / `2.20.0`. Cowboy latest is **2.18.0**; `plug_cowboy` latest is **2.9.0**. |
| Advisory | [GHSA-gg23-fwhr-prjh](https://github.com/advisories/GHSA-gg23-fwhr-prjh) / [EEF-CVE-2026-43971](https://osv.dev/vulnerability/EEF-CVE-2026-43971). Affects cowlib `>= 2.9.0` including **2.19.0**. No `first_patched_version`. |
| Upstream fix | Merged on cowlib `master` as [`89da27ee`](https://github.com/ninenines/cowlib/commit/89da27ee4c241f5d649ba7d9b7f2188918af6cea) (2026-08-18). `master` is 8 commits ahead of `2.19.0` and also requires OTP 27+ and changes HPACK, header parsing, cookies, and SSE. |
| Safe pin in this repo? | **No.** A git/path override does not publish on Hex and does not change downstream resolution. Pinning `master` would pull unrelated HTTP-stack changes and would not make `jido_mcp` (or any other root project) pass `mix hex.audit`. |

Local mitigation stays as already shipped: the exact `EEF-CVE-2026-43971`
name in `mix.exs` `hex.ignore_advisories`, plus
`test/ex_mcp/security/dependency_advisory_mitigation_test.exs` which asserts
that the ExMCP / Plug / Plug Cowboy / Cowboy server path does not import
`cow_link:link/1`. That exception is ExMCP-local. It does not suppress the
advisory for consumers. Do not add ignore rules, skip tags, or scanner
disables to paper over this CVE.

**Next action:** wait for a Hex Cowlib that contains `89da27ee` (or
equivalent). Then bump the lock, remove `EEF-CVE-2026-43971` from
`ignore_advisories`, keep the import regression test until the new version is
proven, and close #18 / AZM-5 only after a fresh downstream `mix hex.audit`
passes. The named-exception review deadline remains **2026-09-12**.

Downstream applications that need the patched encoder before Hex publishes
can override in *their* root `mix.exs`:

```elixir
{:cowlib, github: "ninenines/cowlib", override: true}
```

That is an application-level choice, not something ExMCP can ship.

The 2026-09-04 `mix hex.audit` quality job also started failing on Mint 1.9.3
(`EEF-CVE-2026-82728`, `EEF-CVE-2026-82729`), which Hex published the same
day. Those advisories are unrelated to #18. Mint **1.10.0** is on Hex, has
no advertised breaking changes, and remediates both, so the lock was bumped
instead of adding ignore rules. Do not treat that Mint bump as a Cowlib
workaround.

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
6. Modularize Codex one characterized boundary at a time. `Codex.Protocol` is
   extracted; `Sessions`, `Permissions`, `Content`, and `MCP` remain.
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
