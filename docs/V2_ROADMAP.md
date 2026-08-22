# ExMCP 2.0 Roadmap

- **Status:** Living roadmap — Phase 0 complete; Phase 1 contract design is next
- **Target:** ExMCP `2.0.0`, after stable `1.0.0` and the supported 1.x line
- **Last updated:** 2026-08-22
- **Related release work:** [`RELEASE_1_0_0.md`](./RELEASE_1_0_0.md),
  [`API_DIFF_RC5_TO_1_0.md`](./API_DIFF_RC5_TO_1_0.md),
  [`POST_1_0_MAINTENANCE_PLAN.md`](./POST_1_0_MAINTENANCE_PLAN.md),
  [`MCP_2026_07_28_MIGRATION_PLAN.md`](./MCP_2026_07_28_MIGRATION_PLAN.md)

---

## 1. Purpose

ExMCP 1.0 establishes a dual-era MCP implementation with a broad, compatible
public API. ExMCP 2.0 should make that implementation easier to operate,
extend, and reason about without turning the major release into an unrelated
rewrite.

This roadmap records:

- the changes intentionally reserved for a major release;
- which ideas from Anubis MCP and the external Grok review are being adopted;
- which ideas are useful only with constraints, and which are rejected;
- the order in which the runtime, dispatch, storage, and public API should
  evolve; and
- the rules for safely backporting selected work to 1.x.

It is the canonical ExMCP 2.0 planning document. The similarly named
[`PRE_2_0_TECH_DEBT_PLAN.md`](./PRE_2_0_TECH_DEBT_PLAN.md) is completed rc.5
release history, not the 2.0 roadmap.

## 2. Inputs are evidence, not specifications

The roadmap uses three inputs:

1. ExMCP's current architecture, tests, deprecations, and 1.0 release gates.
2. [Anubis MCP](https://github.com/zoedsoupe/anubis-mcp), especially its
   smaller public surface, component boundaries, supervision-oriented design,
   and developer ergonomics.
3. The [Grok design review](https://grok.com/share/bGVnYWN5LWNvcHk_ce15b5ae-1f64-406c-b65e-df2b934e53ba),
   which proposed improvements around dispatch, handler scheduling, result and
   schema helpers, persistence adapters, protocol boundaries, telemetry, and
   client ergonomics.

Neither external project defines ExMCP's compatibility contract. Ideas are
accepted only when they solve an observed ExMCP problem and fit OTP, MCP wire
compatibility, and the maintenance cost of a multi-transport library. ExMCP
will not copy another library's public API merely to look familiar.

## 3. Guiding constraints

### 3.1 Preserve protocol capability

The 2.0 package version and MCP protocol revisions are separate compatibility
boundaries. A public Elixir API redesign does not justify losing modern MCP
conformance or silently dropping legacy protocol support.

Any removal of a legacy MCP revision or deprecated protocol feature requires a
separate decision with usage evidence, notice, and migration guidance. It is
not an automatic consequence of releasing ExMCP 2.0.

### 3.2 Prefer one runtime owner

A started MCP server should own its registries, handler execution, session and
subscription state, replay store, and telemetry identity through one
supervision subtree. Process-global state makes multiple servers in one VM
hard to isolate and obscures restart behavior.

### 3.3 Keep concurrency bounded and explicit

Running every handler in an unlinked task would improve throughput while
weakening cancellation and state guarantees. ExMCP should use supervised,
bounded work and define state-commit semantics before enabling concurrent
callbacks.

Stateful handlers remain serialized by default. Concurrent execution is for
handlers that explicitly select a stateless, re-entrant, or otherwise isolated
state model.

### 3.4 Make side effects replaceable, not mandatory

The core should define small store contracts for state that must outlive a
connection. ETS remains a useful default. Durable or clustered implementations
should be replaceable adapters rather than database dependencies in the core
package.

### 3.5 Spend the breaking-change budget deliberately

The major release should remove deprecated APIs, clarify ownership, and
regularize callback/result contracts. Cosmetic renames, duplicate aliases, and
speculative abstraction consume migration effort without producing the same
value.

### 3.6 Use functional cores inside OTP-owned shells

Processes should own mutable state and effects, but they need not own every
decision. Model request, session, retry, dispatch, and protocol transitions as
pure reducers where that produces a coherent semantic boundary. Reducers
receive time, identifiers, and normalized configuration explicitly and return
new state plus tagged effects for the owning process to execute.

This is not a mandate to turn every helper into a module or to replace OTP with
an application framework. The objective is to make ordering, cancellation,
expiry, and error decisions testable without a running process while leaving
Ports, ETS, HTTP, `Plug.Conn`, telemetry, logging, and supervision at the edge.

## 4. Current 1.0 baseline

The 1.0 line already provides:

- dual-era MCP support through explicit protocol modes;
- a canonical method registry and shared server dispatch primitives;
- `ExMCP.Server.Handler` and `ExMCP.Server.DSL` as the preferred server APIs;
- modern request context, result envelopes, MRTR, subscriptions, and Tasks;
- stdio, Streamable HTTP, legacy HTTP+SSE, BEAM-local, and test transports;
- ACP client, agent, and adapter support; and
- conformance, characterization, property, security, and interoperability
  coverage.

Stable `v1.0.0` was published on 2026-08-22 after the final `1.0.0-rc.8`
candidate completed a one-week soak. The release includes dual-era MCP,
legacy SSE persistence end to end, the 2026-08-12 security hardening, and the
credential-free Claude SDK, Codex, and Pi CLI lifecycle suite. The stable tag
also contains the deterministic OAuth callback field-lookup fix described in
the release record; it does not change the rc.8 public or wire surface.

The public API census is recorded in
[`API_DIFF_RC5_TO_1_0.md`](./API_DIFF_RC5_TO_1_0.md), and committed protocol
fixtures pin capability and initialize behavior across supported revisions.
The mixed-version rollback drill, modern conformance suites, official SDK v2
interop, security checks, and performance/load gates provide the remaining
release evidence.

Current `master` contains post-1.0 additive work: the ZCode ACP adapter and a
four-adapter real-CLI lifecycle suite. Those changes remain part of the next
1.x baseline and must not be described retroactively as contents of the
`v1.0.0` artifact.

The remaining architectural pressure is concentrated in runtime ownership,
callback execution, storage contracts, duplicated public concepts, and the
size of the compatibility surface.

## 5. Decision register

| Idea | Source | Decision | Target | Rationale |
|---|---|---|---|---|
| Per-server runtime and scoped registries | Anubis comparison; ExMCP review | Adopt | 2.0 | Makes ownership, multi-server isolation, restart behavior, and testing explicit. |
| Pure transition cores with effectful OTP shells | ExMCP architecture review | Adopt with constraints | Post-1.0 foundation; complete in 2.0 | Extract cohesive state machines, inject ambient inputs, and preserve process ownership and observable lifecycle. |
| Validated configuration structs | ExMCP architecture review | Adopt | 2.0 | Resolve application/system environment and defaults once; stop deep code from rediscovering precedence. |
| Supervised handler scheduler | Grok review | Adopt with constraints | 2.0 | Use bounded `Task.Supervisor` work, cancellation propagation, and explicit state semantics; never fire-and-forget tasks. |
| One dispatch/context/result pipeline | Both reviews; existing ExMCP drift | Adopt | 2.0 foundation | All transports should observe the same authorization, timeout, telemetry, normalization, and error behavior. |
| Event and session store behaviours | Grok review; SSE implementation | Adopt | Design in 2.0; adapter may later backport | ETS remains the default; append/replay/TTL/delete semantics must be specified before adapters are public. |
| Small result-construction API | Anubis comparison; Grok review | Adopt | 2.0 | Consolidate result helpers and normalizers instead of adding another parallel facade in 1.x. |
| Richer DSL parameter constraints | Both reviews | Adopt selectively | 2.0 | Add high-value JSON Schema constraints without trying to reproduce the entire schema vocabulary as macros. |
| Client `with_connection` lifecycle helper | Grok review | Adopt | 2.0 | Useful ergonomics, but it should be designed with ownership, links, shutdown, and reconnect behavior together. |
| Component grouping and composition | Anubis comparison | Adopt with constraints | 2.0 | Prefer compile-time composition and scoped runtime registration; avoid a second DSL or unconstrained global component registry. |
| Central additive telemetry | Grok review | Adopt | 1.x-compatible subset, complete in 2.0 | Instrument the shared dispatch boundary; payload capture stays opt-in and bounded. |
| Public middleware/pipeline API | Grok review | Defer | Reconsider after internal pipeline lands | First prove stable phases and use cases internally; a premature public pipeline becomes another compatibility surface. |
| General protocol-dialect framework | Grok review | Defer | Only with two concrete consumers | Keep existing era/version modules unless a second protocol family demonstrates that a general dialect abstraction removes real duplication. |
| Separate MCP and ACP Hex packages | Package-footprint review | Investigate | Phase 1 decision | ACP is substantial and lightly coupled, but package/release topology and migration cost need a focused design. |
| Third shared runtime package | Package-footprint review | Defer pending split design | 2.0 only if justified | Centralize security-sensitive JSON-RPC/framing/process code only if both packages need a stable neutral contract; do not publish a grab-bag of tiny helpers. |
| Built-in distributed database/event sourcing | External review extrapolation | Reject for core | External adapters | ExMCP should define contracts, not require a database or event-source all runtime state. |
| Copy Anubis APIs or rewrite ExMCP around them | Comparison exercise | Reject | — | ExMCP has broader protocol, transport, authorization, ACP, and compatibility requirements. |
| Treat every additive API as a safe 1.x backport | Backport discussion | Reject | — | Additions create support obligations and can still change lifecycle, defaults, ordering, or wire behavior. |
| Remove every legacy MCP feature in 2.0 | Version-number assumption | Reject as a default | Separate decision | Package-major and protocol-version policies are independent. |

## 6. Target runtime shape

The target is one server-owned supervision subtree with a common dispatch
boundary. Transport-specific framing stays at the edges.

```mermaid
flowchart LR
  A["Transport adapters"] --> B["Server runtime"]
  B --> C["Request context and policy"]
  C --> D["Bounded request scheduler"]
  D --> E["Handler / DSL callback"]
  E --> F["Result and error normalizer"]
  F --> A
  B --> G["Scoped registries"]
  B --> H["Session / event / subscription stores"]
  C --> I["Telemetry boundary"]
  D --> I
  F --> I
```

The runtime reference, not a global registered name, should identify a server
instance. Convenience startup may still provide a default name, but core code
must accept an explicit runtime or server reference.

### Runtime-owned children

A server runtime should own, as applicable:

- the handler state owner;
- the bounded request task supervisor and scheduler;
- session and SSE-handler registration;
- event replay storage;
- subscription indexes and optional cluster fanout;
- request-state/replay caches;
- transport listeners; and
- a stable telemetry identity.

Failures must have a documented blast radius. Restarting a transport listener
must not silently discard an independently configured durable event store, and
a failed request task must not terminate unrelated requests or another server
instance.

## 7. Delivery phases

Phases are dependency ordered. A later phase may be prototyped early, but it
does not merge to the 2.0 release branch until its prerequisites pass.

### Phase 0 — Finish and freeze the 1.0 baseline

**Goal:** establish the exact behavior from which 2.0 migrates.

**Status: Complete (2026-08-22).**

- `v1.0.0` was tagged at `e1e7e7a` and published as a stable GitHub release
  and Hex package after the one-week rc.8 soak and final release gates passed.
- The stable release preserves rc.8's public API, wire behavior, and protocol
  defaults while adding only the characterized OAuth callback correctness fix.
- The API census, protocol capability and initialize fixtures, release record,
  conformance results, official-SDK interoperability, security evidence,
  performance/load gates, and mixed-version rollback drill form the durable
  1.0 comparison baseline.
- Post-release ZCode adapter and real-CLI coverage are explicitly tracked as
  additive 1.x work rather than being folded into the historical 1.0 artifact.

**Exit achieved:** stable `1.0.0` is published, and its public and wire
characterization evidence is committed as the 2.0 comparison baseline. Phase 1
should turn the existing API census into a reproducible machine-readable
manifest before proposing removals.

### Phase 1 — Specify the 2.0 contract before moving code

**Goal:** make breaking changes reviewable rather than emergent.

- Inventory documented modules, exports, callbacks, types, structs, options,
  process names, telemetry, and wire-visible defaults.
- Publish the proposed removal and replacement table.
- Write focused decision records for runtime ownership, handler state and
  concurrency, store contracts, and legacy-protocol support.
- Add characterization tests for callback process identity, links, timeout and
  cancellation behavior, state ordering, and per-server isolation.
- Define the migration path for every removal before deleting it.
- Decide package topology, namespaces, dependency direction, version policy,
  and whether existing `ExMCP.ACP.*` modules move or remain compatibility
  namespaces.
- Define validated configuration structs and preserve the 1.x precedence table
  as an explicit migration contract.
- Specify reducer/action ordering, effect-failure feedback, correlation and
  idempotency, late/duplicate event handling, and timer/cancellation races.
  Keep reducer action schemas private unless deliberately accepted as public.

**Exit:** maintainers can answer what breaks, why it breaks, and how users
migrate without referring to implementation diffs.

### Phase 2 — Introduce `ExMCP.Server.Runtime`

**Goal:** replace process-global ownership with one explicit server boundary.

- Add a runtime child specification and stable runtime reference.
- Move registries, sessions, subscriptions, and request caches under the
  server's supervisor.
- Allow multiple independent server runtimes in one VM without key collisions
  or shared cleanup.
- Define restart strategies and ownership of external adapter processes.
- Keep transports thin: they resolve a runtime and submit framed requests.

**Exit:** isolation tests can start, crash, restart, and stop two servers
independently; no request/session/subscription state crosses the boundary.

### Phase 3 — Unify dispatch and add bounded scheduling

**Goal:** make every transport use the same request semantics.

- Route stdio, HTTP, BEAM-local, and test requests through one dispatch entry.
- Express dispatch/request lifecycle as a pure transition core where practical;
  the runtime executes returned transport, reply, timer, and telemetry actions.
- Centralize request context, authorization outcome, method lookup, deadlines,
  telemetry, result normalization, and error mapping.
- Execute eligible callbacks through a runtime-owned `Task.Supervisor` with
  configurable bounds and queue pressure policy.
- Propagate disconnects, cancellation, deadline expiry, and supervisor shutdown
  to request work.
- Keep stateful callbacks serialized by default. Require an explicit execution
  mode before callbacks may run concurrently.
- Document ordering guarantees for replies, notifications, and state commits.

**Exit:** cross-transport golden tests produce equivalent results and errors;
stress tests prove bounded processes, mailboxes, queues, and cancellation time.

### Phase 4 — Define state and replay adapters

**Goal:** make connection-surviving state replaceable without leaking backend
details into transports.

Define narrow behaviours for the state that benefits from replacement. The
event-store contract must cover at least:

- atomic append returning a store-owned opaque event ID;
- ordered replay after an exact cursor;
- bounded retention and cursor-eviction behavior;
- session TTL and explicit deletion;
- idempotent overwrite/deduplication expectations;
- adapter ownership and restart behavior; and
- telemetry that excludes event payloads by default.

Ship supervised ETS implementations as the defaults. A filesystem, Mnesia,
PostgreSQL, or third-party clustered adapter can be supplied separately once it
passes the same contract suite.

**Exit:** the SSE reconnect suite passes unchanged against every supported
adapter, including disconnect-after-append and publish-during-gap races.

### Phase 5 — Consolidate the public server API

**Goal:** make the common path smaller while retaining low-level control.

- Consolidate result constructors and normalization behind one
  `ExMCP.Server.Result` contract; avoid parallel `DSL.Result`, response helper,
  and transport-specific result vocabularies.
- Add selected DSL constraints such as numeric/string bounds, patterns,
  defaults, enums, and nested array/object schemas where they produce valid,
  inspectable JSON Schema.
- Support compile-time component grouping without adding a second tool DSL.
- Add a `with_connection` client helper or an equivalent bracketed lifecycle
  helper with explicit ownership and shutdown behavior.
- Keep one-file client/server examples as acceptance tests for the public API.

**Exit:** the quick-start server, advanced server, and client lifecycle require
fewer concepts than 1.x, while low-level Handler users retain a complete path.

### Phase 6 — Remove deprecated and duplicated surface

**Goal:** spend the major-version compatibility budget on known debt.

Planned removals include:

- `ExMCP.Server.Tools`, `Tools.Simplified`, and their companion modules after
  the DSL migration path is verified;
- deprecated image transformation stubs that are outside MCP/ACP scope;
- retained aliases and compatibility functions that have a documented 2.0
  replacement; and
- global runtime entry points superseded by explicit server references.

Legacy HTTP+SSE, Roots, Sampling, Logging, legacy subscriptions, and older MCP
revisions are **not** on this list by default. Their disposition requires the
separate protocol-support decision from Phase 1.

**Exit:** no removal lacks a migration example, deprecation history, and API
diff entry.

### Phase 7 — Migration and release qualification

**Goal:** prove that 2.0 is a controlled migration, not only a green unit suite.

- Generate a 1.x-to-2.0 API diff for modules, exports, callbacks, types,
  structs, options, and documented process names.
- Publish a migration guide with before/after examples for every removal.
- Run all supported MCP conformance and official-SDK interoperability lanes.
- Run cross-transport equivalence, multi-runtime isolation, cancellation,
  pressure, persistence-adapter, security, and upgrade tests.
- Establish performance budgets against the final 1.x release on the same
  runner.
- Ship at least one 2.0 RC and require a soak appropriate to the runtime and
  persistence changes.

**Exit:** every release gate has an owner and durable evidence; stable 2.0 is
behavior-identical to its final RC except for release metadata.

## 8. The 1.x backport lane

The roadmap deliberately permits some 2.0-derived work in 1.x. “No removed
function” is not enough to qualify a backport.

### 8.1 Required backport tests

A 1.x backport must satisfy all applicable conditions:

1. It fixes documented/spec behavior, or is additive and opt-in/default-off.
2. Existing function signatures, callbacks, return shapes, structs, and types
   remain compatible.
3. Existing wire output, ordering, defaults, and negotiated behavior remain
   compatible except for an explicitly documented bug or security fix.
4. Process ownership, callback process identity, links, cancellation, restart
   behavior, and state ordering do not change unexpectedly.
5. Resource usage remains bounded and existing telemetry contracts do not
   change.
6. Characterization and end-to-end regression tests land with the change.

A failed condition sends the work to 2.0 unless maintainers explicitly approve
another 1.x RC or document a patch-level correctness/security exception.

### 8.2 Current classifications

| Change | 1.x decision | Notes |
|---|---|---|
| Legacy SSE persist-before-delivery, gap replay, and session retention | Released in `1.0.0` | Soaked through rc.8 and retained in the stable baseline. |
| Correct bounded replay-buffer retention | Released in `1.0.0` | Correctness fix with regression coverage. |
| Documentation, examples, diagnostics, and characterization tests | Backport | No runtime compatibility cost. |
| Additive dispatch telemetry | Eligible for a 1.x minor | Preserve existing events; bounded metadata only; payload capture opt-in. |
| Store adapter seam | Possible later 1.x minor | Only after a standalone store ADR and contract suite are accepted; the design need not wait for 2.0 runtime implementation. ETS must remain the default and current behavior unchanged. |
| Internal dispatch deduplication | Case by case | Backport only when golden tests prove identical wire, errors, ordering, and lifecycle. |
| Richer DSL constraints | Hold for 2.0 | Additive, but expands the stable public language before its design is settled. |
| Result facade and client lifecycle helpers | Hold for 2.0 | Technically additive, but would create parallel APIs and long-term support obligations. |
| Per-server runtime/scoped registries | 2.0 only | Changes ownership, naming, restart, and cleanup behavior. |
| Concurrent handler scheduler | 2.0 only | Changes callback process identity, state timing, cancellation, and failure behavior. |
| Callback/context/result contract cleanup | 2.0 only | Public semantic change even if compatibility shims could preserve arities. |
| Deprecated API removals | 2.0 only | Breaking by definition. |

### 8.3 SemVer interpretation

For ExMCP, observable compatibility includes more than exported functions:

- JSON-RPC and SSE wire shapes, event ordering, and opaque cursor behavior;
- callback invocation and returned state ordering;
- which process executes user code and what it is linked to;
- timeout, cancellation, retry, and disconnect behavior;
- supervision names, registry scope, and restart effects;
- defaults and configuration precedence;
- telemetry names and established metadata; and
- documented side effects such as session cleanup.

Opaque SSE event IDs may change representation because clients must not parse
them. Losing events, replaying duplicates, terminating a resumable session on
disconnect, or changing callback execution processes is observable behavior.

## 9. Explicit non-goals

ExMCP 2.0 is not intended to:

- replace OTP supervision with an application-level framework;
- require Phoenix, Ecto, a database, or a distributed registry;
- expose transport internals through the common server API;
- add an abstraction for every possible protocol or JSON Schema keyword;
- make stateful handlers concurrently mutable by default;
- remove legacy MCP support solely because the package major changed; or
- preserve deprecated APIs under new names indefinitely.

## 10. Open decisions

### 10.1 MCP/ACP package topology

ACP is large enough to justify evaluating a split: at commit `db8a998`, after
the post-1.0 ZCode adapter and CLI interop merge, the tree contains 50 ACP
source files and 22,591 of 92,812 library lines (about 24.3%). The coupling is
much smaller than the line count. At this 2026-08-22 baseline, xref reports 26
direct edges from ACP files to ten non-ACP files:

- `Internal.NameValue` and `Internal.WorkspacePath` are ACP-only despite their
  current location and can move into an ACP-owned namespace;
- six genuinely shared utility files total approximately 281 lines: JSON-RPC
  envelopes/parsing, bounded option normalization, map helpers, port-environment
  policy, redacted log summaries, and stdio logger configuration; and
- the remaining two edges are the 208-line MCP transport behaviour/factory and
  the 642-line child-process stdio transport. The behaviour currently selects
  concrete MCP transports, and the stdio implementation mixes reusable Port and
  NDJSON mechanics with MCP-specific validation and security policy.

ACP source directly uses Jason and telemetry but not Mint, Plug, JOSE, or the
JSON Schema dependency. A clean `ex_acp` package could therefore have a much
smaller runtime dependency set than `ex_mcp`; making `ex_acp` depend on
`ex_mcp` would preserve code sharing but largely defeat that benefit.

ACP's session lifecycle includes `mcpServers`, but that does not by itself
create an implementation dependency on an MCP library. These values are ACP
wire descriptors telling the agent how to launch or reach an MCP server. The
ACP package should own their ACP types, builders, normalization, validation,
and authorization as plain data; it need not own an MCP client/server runtime.
This is also the correct trust boundary because session-supplied commands,
environment variables, URLs, and headers are untrusted even when the target
protocol is MCP.

An Elixir ACP agent that wants ExMCP to connect to those descriptors can install
both packages and use an optional bridge. Keep conversion from ExMCP-specific
configuration structs out of the core ACP API so the dependency remains
one-way and optional. The existing ExMCP-specific BEAM MCP capability/descriptor
is an integration extension and should move to that bridge (or require both
packages), rather than forcing all ACP consumers to depend on ExMCP. An
integration module/package is distinct from the proposed neutral shared runtime:
the latter must not own either protocol's configuration schema.

The package-topology design must compare these options:

| Shape | Advantages | Costs and risks |
|---|---|---|
| Keep one `ex_mcp` package | One release train, no migration or cross-package compatibility matrix | MCP-only and ACP-only users compile and receive unrelated capabilities; adapter growth remains coupled to MCP releases. |
| `ex_acp` depends on `ex_mcp` | Smallest implementation change and no copied code | ACP-only users still install MCP, HTTP, OAuth, and schema dependencies; release coupling remains. |
| Independent `ex_mcp` and `ex_acp` with copied helpers | Two simple dependency graphs and independent releases | Security, framing, environment, and JSON-RPC fixes can drift. Copying those implementations is not acceptable. |
| `ex_mcp` and `ex_acp` depend on a small shared package | No duplicated security-sensitive code; independent protocol packages and dependency sets | Adds a third public app, versioning policy, release order, compatibility matrix, and another release/maintenance coordination surface. |

The preliminary direction is a same-repository, multi-package design spike,
not an immediate split. First move ACP-only helpers under ACP ownership and
separate the generic transport behaviour from concrete MCP transport selection.
Then factor the subprocess transport into a neutral bounded-NDJSON/Port core
with MCP- and ACP-specific validation wrappers and remeasure the residual shared
surface.

Create a third package only if the residual code forms a cohesive, stable
runtime contract used by both packages. Its scope should be limited to
mechanics such as JSON-RPC envelopes, bounded line framing, subprocess
lifecycle/environment policy, and payload-safe diagnostics. Protocol methods,
MCP resource policy, ACP session semantics, adapter mappings, and public client
APIs stay in their owning packages. Trivial map construction may be separately
owned rather than forcing a dependency on a miscellaneous helper package.

If a shared package is justified, keep all packages in one repository, release
the shared package first, use explicit compatible version ranges, and run a CI
matrix against the lowest and newest supported shared version. One contract
suite should execute against both protocol wrappers, and security/framing fixes
must update that suite before either consumer releases. The Phase 1 design must
also choose whether the existing `ExMCP.ACP.*` namespace remains in `ex_acp`,
migrates to `ExACP.*`, or is preserved temporarily by a compatibility package.

The design spike must record, for the monolith and each viable split:

- compressed Hex archive size, clean compile time, and runtime dependency/app
  count for an MCP-only and an ACP-only consumer;
- the residual shared modules and why each is a cohesive neutral contract rather
  than a coincidental helper;
- CI jobs, release ordering, supported-version matrix, and estimated ongoing
  maintenance cost;
- source/API/namespace migration cost for existing ACP users; and
- whether an intentionally broken shared-package version is caught by each
  consumer's lowest/newest-version contract jobs.

Choose a split only when those measured dependency and maintenance benefits
outweigh the added release surface; source-line reduction by itself is not an
exit criterion.

Reproduce the source measurements with:

```text
find lib/ex_mcp/acp -type f -name '*.ex' -print0 | xargs -0 wc -l
find lib -type f -name '*.ex' -print0 | xargs -0 wc -l
mix xref graph --format json --output xref-graph.json
```

For the edge count, select xref entries whose source begins with
`lib/ex_mcp/acp/` and whose target does not. Recompute all figures at the start
of the spike rather than treating this baseline as a target.

### 10.2 Other open decisions

These need focused design records during Phase 1:

1. **Handler state model:** whether 2.0 supports serialized stateful and
   concurrent stateless modes only, or also defines isolated/partitioned state.
2. **Runtime reference:** PID, registered name, supervisor reference, or an
   opaque struct, including how it crosses Plug configuration boundaries.
3. **Store split:** one session/replay behaviour or separate session, event,
   subscription, and request-state behaviours.
4. **Legacy protocol policy:** which deprecated protocol features remain for
   the complete 2.x line and what evidence could justify removal.
5. **Component composition:** compile-time-only composition versus constrained
   runtime registration and how capability-change notifications are emitted.
6. **Public pipeline:** whether concrete authorization/telemetry middleware use
   cases justify exposing the internal dispatch phases.
7. **Package topology:** whether measured compile/dependency/release benefits
   justify separate MCP and ACP packages and, if so, the shared-runtime and
   namespace strategy described above.

An open decision is not permission to let an implementation choose the public
contract accidentally.

## 11. Roadmap maintenance

- Update the decision register when a design is accepted, rejected, or moved
  between release lines.
- Mark completed phase items in their implementation PRs; do not use this file
  as an issue tracker for individual code changes.
- Link substantial design records from the relevant phase.
- Record every 1.x backport in the classification table and `CHANGELOG.md`.
- Revisit external comparison inputs for ideas, but evaluate them against the
  current ExMCP baseline rather than treating parity as a goal.
