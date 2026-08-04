# ExMCP → MCP 2026-07-28 Migration Plan

**Status:** Draft / not yet started
**Target release:** ExMCP `2.0.0`
**Spec revision:** [2026-07-28](https://modelcontextprotocol.io/specification/2026-07-28) ([changelog](https://modelcontextprotocol.io/specification/2026-07-28/changelog))
**Current ExMCP:** `1.0.0-rc.5`, implements MCP `2024-11-05` / `2025-03-26` / `2025-06-18` / `2025-11-25`
**Prerequisite:** [`PRE_2_0_TECH_DEBT_PLAN.md`](./PRE_2_0_TECH_DEBT_PLAN.md) — behavior-preserving cleanup shipping as `1.0.0-rc.5`
**Author:** planning document — no code changes implied by this file
**Last updated:** 2026-08-02

---

## 1. Executive summary

MCP `2026-07-28` is the largest breaking change in the protocol's history. It is not a
feature addition on top of `2025-11-25`; it is a re-founding of the protocol on a
**stateless, per-request** model:

- The `initialize` / `notifications/initialized` handshake is **gone**. Protocol version,
  client identity and client capabilities now ride in `_meta` on *every* request.
- Protocol-level sessions (`Mcp-Session-Id`) are **gone**.
- Server-initiated JSON-RPC requests are **gone**. Sampling, elicitation and roots are now
  returned *inside results* via the new **Multi Round-Trip Requests (MRTR)** pattern and
  fulfilled by the client **retrying the original request**.
- The HTTP `GET` SSE endpoint, SSE resumability (`Last-Event-ID`), `resources/subscribe`,
  `resources/unsubscribe`, `ping`, `logging/setLevel` and `notifications/roots/list_changed`
  are **gone**, replaced by a single long-lived `subscriptions/listen` request.
- Every result now carries a required `resultType` discriminator.
- Tasks moved out of core into the `io.modelcontextprotocol/tasks` **extension**, redesigned.

### Decisions taken

| Decision | Choice | Rationale |
|---|---|---|
| Compatibility | **Dual-era.** Keep `2024-11-05` … `2025-11-25` working; add `2026-07-28`. | ExMCP already supports every prior revision and users depend on it. The spec explicitly defines a dual-era model and a compatibility matrix. |
| Default version | **Prefer `2026-07-28`, fall back by probe.** | There is no handshake to negotiate in. Clients probe with `server/discover`; servers branch on the shape of the first request. |
| Scope | **Everything** — core, tasks extension, `x-mcp-header`, auth updates (RFC 9207 `iss`, CIMD, `application_type`, issuer-keyed credentials). | A 2.0 is the right place to absorb the whole delta at once. |
| Release | **`2.0.0`** | Breaking. Also the window to drop `ExMCP.Server.Tools` and other deprecated surface. |
| Deprecated-but-live features | **Keep** Roots / Sampling / Logging / HTTP+SSE. | The spec deprecates them with a ≥12-month window; MRTR still carries sampling/elicitation/roots payloads. |

### Effort shape

Rough order of magnitude, based on the current codebase (~66k LOC `lib/`, ~73k LOC `test/`,
of which ~17.6k LOC is ACP and out of scope):

| Phase | Area | Relative size |
|---|---|---|
| 0 | Foundations — **mostly split out to [`PRE_2_0_TECH_DEBT_PLAN.md`](./PRE_2_0_TECH_DEBT_PLAN.md) (rc.5)**; what remains is the `initialize` consolidation and the wire-visible fixes rc.5 couldn't take | S (was M) |
| 1 | Types & schema for 2026-07-28 | M |
| 2 | Per-request `_meta`, result envelope, `server/discover` | L |
| 3 | Era detection & dual-era dispatch | L |
| 4 | MRTR (client loop + server return path) | XL |
| 5 | `subscriptions/listen` | L |
| 6 | Streamable HTTP rework | XL |
| 7 | Caching, logging, ordering, JSON Schema, `x-mcp-header` | M |
| 8 | Tasks extension | L |
| 9 | Authorization updates | M |
| 10 | Deprecation removals, docs, conformance, release | M |

---

## 2. What actually changed

### 2.1 Method surface

The `ClientRequest` union in the 2026-07-28 `schema.ts` is now exactly:

```
server/discover · completion/complete · prompts/get · prompts/list
resources/list · resources/templates/list · resources/read
subscriptions/listen · tools/call · tools/list
```

`ClientNotification` is exactly `notifications/cancelled` (stdio only).

| Method | 2025-11-25 | 2026-07-28 |
|---|---|---|
| `initialize` | required | **removed** |
| `notifications/initialized` | required | **removed** |
| `server/discover` | — | **new, servers MUST implement** |
| `ping` | both directions | **removed** |
| `logging/setLevel` | server | **removed** → `_meta["io.modelcontextprotocol/logLevel"]` per request |
| `resources/subscribe` / `unsubscribe` | server | **removed** → `subscriptions/listen` |
| `subscriptions/listen` | — | **new** |
| `notifications/subscriptions/acknowledged` | — | **new** |
| `roots/list` (server→client) | request | **only inside `inputRequests`** |
| `sampling/createMessage` (server→client) | request | **only inside `inputRequests`** |
| `elicitation/create` (server→client) | request | **only inside `inputRequests`** |
| `notifications/roots/list_changed` | client→server | **removed** |
| `notifications/elicitation/complete` | server | **removed** (added in 2025-11-25, gone again) |
| `tasks/list`, `tasks/result` | core (experimental) | **removed** |
| `tasks/get`, `tasks/cancel` | core (experimental) | **moved to extension**, redesigned |
| `tasks/update` | — | **new** (extension) |
| `notifications/tasks` | — | **new** (extension, via `subscriptions/listen`) |

### 2.2 Per-request `_meta`

```jsonc
"params": {
  "_meta": {
    "io.modelcontextprotocol/protocolVersion": "2026-07-28",   // REQUIRED
    "io.modelcontextprotocol/clientCapabilities": { },          // REQUIRED (may be {})
    "io.modelcontextprotocol/clientInfo": { "name": "...", "version": "..." }, // SHOULD
    "io.modelcontextprotocol/logLevel": "info",                 // optional, opt-in to logs
    "progressToken": "...",                                     // optional (unchanged)
    "traceparent": "...", "tracestate": "...", "baggage": "..." // OTel, optional
  }
}
```

- Missing a required field ⇒ `-32602` (`Invalid params`); on HTTP, `400`.
- Server needs a capability the client didn't declare ⇒ `MissingRequiredClientCapabilityError`
  `-32021` with `data.requiredCapabilities`.
- Results **SHOULD** carry `_meta["io.modelcontextprotocol/serverInfo"]`.
- Notifications on a listen stream **MUST** carry `_meta["io.modelcontextprotocol/subscriptionId"]`.

### 2.3 Result envelope

Every result gains a required `resultType`:

- `"complete"` — ordinary result.
- `"input_required"` — MRTR interim result (`InputRequiredResult`).
- `"task"` — tasks extension `CreateTaskResult`.
- Extensions MAY add more; unrecognised values MUST be treated as invalid.
- **Absent ⇒ treat as `"complete"`** (backward compat with legacy servers).

`CacheableResult` adds `ttlMs` (integer ms, ≥ 0) and `cacheScope` (`"public"` | `"private"`),
**required** on `resultType: "complete"` results of `server/discover`, `tools/list`,
`prompts/list`, `resources/list`, `resources/templates/list`, `resources/read`.

### 2.4 Error codes

| Code | Name | Notes |
|---|---|---|
| `-32020` | `HeaderMismatch` | was `-32001` in draft |
| `-32021` | `MissingRequiredClientCapability` | was `-32003` in draft |
| `-32022` | `UnsupportedProtocolVersion` | was `-32004` in draft; `data: {supported, requested}` |
| `-32602` | resource not found | **was `-32002`**; clients SHOULD still accept `-32002` from legacy servers |
| `-32042` | (URL elicitation required) | **retired**, MUST NOT be emitted |

Allocation policy: `-32000..-32019` legacy/implementation-defined (do not allocate new);
`-32020..-32099` reserved for the spec.

### 2.5 Transport

**Streamable HTTP**: POST-only endpoint. No session header, no GET stream, no DELETE, no
`Last-Event-ID`. New required headers on every POST: `MCP-Protocol-Version`, `Mcp-Method`,
and `Mcp-Name` (for `tools/call` / `resources/read` / `prompts/get`). Optional
`Mcp-Param-{Name}` headers mirrored from tool params annotated with `x-mcp-header`. Servers
MUST validate header↔body agreement and reject mismatches with `400` + `-32020`. Unknown
method ⇒ `404` + `-32601`. Closing the SSE response stream *is* cancellation. `X-Accel-Buffering: no`
SHOULD be set; SSE comment keep-alives encouraged on listen streams.

**stdio**: unchanged framing. Server MUST NOT write JSON-RPC *requests* to stdout.
Cancellation via `notifications/cancelled`. Backward-compat probe = `server/discover`.

---

## 3. Target architecture for ExMCP

### 3.1 The "era" concept

Introduce a first-class notion of protocol **era**, threaded everywhere a version is
currently threaded:

```elixir
@type era :: :modern | :legacy
# :modern  => "2026-07-28" and later — per-request _meta, stateless
# :legacy  => "2025-11-25" and earlier — initialize handshake, sessions
```

`ExMCP.Internal.VersionRegistry` becomes the **single** source of truth for
`supported_versions/0`, `latest_version/0`, `era_for/1`, `modern?/1`, `capabilities_for_version/1`,
and `negotiate/2`. Today there are two independent supported-version *lists*
(`VersionRegistry` `@versions`, `VersionNegotiator` `@supported_versions`) plus two more
single-version scalars (`Types`, `config.exs`) — four places that must agree and don't (§4.1).
Collapsing them is Phase 0 and everything else depends on it.

### 3.2 Client shape

```
ExMCP.Client (GenServer)
├── ExMCP.Client.EraProbe          NEW  — server/discover probe + fallback + cache
├── ExMCP.Client.ConnectionManager  MOD — branches: modern (no handshake) | legacy (initialize)
├── ExMCP.Client.RequestHandler     MOD — injects _meta on every outbound request;
│                                          parses resultType; routes input_required
├── ExMCP.Client.MRTR              NEW  — fulfils inputRequests via ExMCP.Client.Handler
│                                          callbacks, then retries with a fresh id
├── ExMCP.Client.Subscription      NEW  — long-lived subscriptions/listen request
├── ExMCP.Client.Cache             NEW  — ttlMs / cacheScope honouring, list_changed invalidation
└── ExMCP.Client.Handler            —   behaviour UNCHANGED (big win, see §3.4)
```

`ExMCP.Client.start_link/1` stays synchronous. In the modern era `init/1` performs the era
probe (`server/discover`) instead of the handshake, and populates the existing
`:server_info`, `:server_capabilities`, `:protocol_version` fields from the `DiscoverResult`.
**Public accessors `server_info/1`, `server_capabilities/1`, `protocol_version/1` keep working
unchanged** — this is the main source-compatibility lever for downstream users.

### 3.3 Server shape

```
ExMCP.Server.Dispatch              MOD — era-aware method table; builds RequestContext
├── ExMCP.Server.RequestContext   NEW  — %{protocol_version, era, client_info,
│                                          client_capabilities, log_level, extensions,
│                                          progress_token, subscription_id}
├── ExMCP.Server.Discover         NEW  — server/discover result assembly
├── ExMCP.Server.RequestState     NEW  — AEAD sign/verify of MRTR requestState
├── ExMCP.Server.Subscriptions    NEW  — per-listen-request filter registry + fan-out
└── ExMCP.Server.ResultNormalizer  MOD — stamps resultType, serverInfo, ttlMs/cacheScope
```

`RequestContext` is the direct replacement for connection state. It is derived from `_meta`
on each request and passed to handlers. In the legacy era it is synthesised from the stored
session so that handler code is era-agnostic.

`ExMCP.Server.ResultNormalizer` is already the single shared result shaper across all four
dispatch paths (this consolidation landed in `[Unreleased]`). That makes stamping
`resultType`/`_meta.serverInfo`/`ttlMs`/`cacheScope` a **one-file change** rather than four.
Preserve that property.

### 3.4 MRTR maps onto the existing `ExMCP.Client.Handler` behaviour

This is the most valuable observation in this plan. The 2026-07-28 `inputRequests` values are
*exactly* `ElicitRequest` / `CreateMessageRequest` / `ListRootsRequest` — the same payloads
ExMCP already handles as server-initiated requests. So:

- `ExMCP.Client.Handler` keeps its `handle_list_roots/1`, `handle_create_message/2`,
  `handle_elicitation_create/3` callbacks **unchanged**.
- `ExMCP.Client.MRTR` iterates the `inputRequests` map, dispatches each entry through the
  same callbacks `ExMCP.Client.RequestHandler` uses today (`request_handler.ex` L431-441),
  collects `inputResponses`, and re-issues the original request with a **new JSON-RPC id**
  plus `inputResponses` and the echoed `requestState`.
- Existing user handler modules (`DefaultHandler`, `CallbackHandler`, `InteractiveHandler`,
  `ElicitationHandler`) continue to work in both eras with no changes.

Two guards are required:
1. **Round-trip cap** (`max_input_rounds`, default e.g. 8) to bound the retry loop, since the
   spec explicitly permits servers to return `input_required` repeatedly.
2. **Capability gate** — never dispatch an `inputRequest` type the client did not declare;
   a server doing so is non-conforming and should surface a protocol error.

Note: `Handler.handle_url_elicitation/3` was declared but never routed — `RequestHandler`
funnelled URL-mode elicitation into `handle_elicitation_create/3` and dropped the `url`. That is
fixed in rc.5 (Track G), so MRTR should **call the fixed dispatcher**, not re-implement routing.

### 3.5 Server-side MRTR return path

Handlers need a way to say "I need input". Proposed additional return tuples, additive to the
existing `{:ok, result, state}` / `{:error, reason, state}`:

```elixir
{:input_required, input_requests :: %{String.t() => map()}, state}
{:input_required, input_requests, request_state :: term(), state}
```

with `request_state` being an arbitrary Elixir term that `ExMCP.Server.RequestState`
serialises + AEAD-seals into the opaque wire string, and unseals + verifies on the retry
(binding principal, TTL, and a digest of method + salient params, per the spec's replay
guidance). Handlers receive the verified term back via `RequestContext`. Default key source:
application config, with a clear error at boot if MRTR is used without a configured key.

The DSL gets a matching affordance inside `run/1` / `handle/1` bodies, e.g. a
`ExMCP.Server.DSL.Result.input_required/2` builder alongside `text/1`, `error/1`, `structured/2`.

### 3.6 Subscriptions

`subscriptions/listen` is a *request whose response is an open stream*. Model it as:

- **Client**: `ExMCP.Client.Subscription` — a supervised process owning one long-lived
  request id, delivering notifications to a subscriber pid. `ExMCP.Client.subscribe_resource/2`
  and `unsubscribe_resource/2` are re-implemented as sugar that opens/updates a listen stream
  in the modern era and falls back to `resources/subscribe` in the legacy era.
- **Server**: `ExMCP.Server.Subscriptions` — ETS-backed registry of
  `{subscription_id, transport_ref, filter}`. Replaces `ExMCP.Server.SSESession`'s
  server→client request correlation (which MRTR makes unnecessary) with pure notification
  fan-out. Existing `Protocol.encode_tools_changed/0` et al. get a `subscriptionId` stamped in
  `_meta` on the way out.
- On HTTP the listen response is a chunked `text/event-stream` that stays open; on stdio all
  listen streams are multiplexed on stdout and demultiplexed by `subscriptionId`.
- Graceful closure: server sends the empty `SubscriptionsListenResult` (with
  `_meta.subscriptionId`) before closing.

### 3.7 What disappears from the codebase in the modern path

These stay for the legacy path but must be **bypassed entirely** when `era == :modern`:

- `ExMCP.SessionManager` event buffering / `replay_events_after/2,3`
- `ExMCP.HttpPlug.SessionRegistry`, `get_or_create_session_id/1`, `validate_session_id_value/1`
- `ExMCP.HttpPlug.SSEHandler` `Last-Event-ID` extraction + replay (L127/L152/L310)
- `ExMCP.Transport.HTTP` `:session_id`, `:last_event_id`, `terminate_session/1`,
  `maybe_update_session_id/2`
- `ExMCP.Server.SSESession` server→client request correlation
- `Protocol.encode_ping/0`, `encode_pong/1`, `encode_set_log_level/1`,
  `encode_subscribe_resource/1`, `encode_unsubscribe_resource/1`, `encode_roots_changed/0`,
  `encode_elicitation_complete_notification/1`

---

## 4. Gap analysis by subsystem

### 4.1 Version handling — **blocker for everything else**

| Location | Problem |
|---|---|
| `lib/ex_mcp/internal/version_registry.ex` L14-19 | Canonical `@versions`, but only one of two lists |
| `lib/ex_mcp/protocol/version_negotiator.ex` L13-14 | Independent duplicate `@supported_versions` / `@latest_version`, plus its own `build_capabilities/1` |
| `lib/ex_mcp/types.ex` L27 | Third copy (scalar): `@latest_protocol_version` |
| `config/config.exs` L8 | Fourth (scalar): `protocol_version: "2025-11-25"` |
| `lib/ex_mcp/internal/protocol.ex` L555-570 | Six method/version MapSets, incl. `@methods_draft_only` (L565-568) which already lists `"server/discover"` and `"subscriptions/listen"` gated to a phantom `"draft"` version. `method_available?/2` at L573-589 |
| `lib/ex_mcp/internal/message_validator.ex` L81 | Batch rejection hard-matches the literal `"2025-06-18"` |
| `lib/ex_mcp/transport/http_server.ex` L295 | Hardcoded `protocolVersion: "2025-03-26"` |
| `lib/ex_mcp/protocol/request_processor.ex` L119 | Defaults missing version to `"2025-06-18"` |
| `lib/ex_mcp/plugs/protocol_version.ex` | Gated behind `FeatureFlags.enabled?(:protocol_version_header)`, **off by default** — must be always-on for modern |

**Action:** collapse to `VersionRegistry` first. Delete `VersionNegotiator`'s duplicate list
and route `build_capabilities/1` through `VersionRegistry.capabilities_for_version/1`. Replace
version string comparisons with `VersionRegistry.era_for/1` / ordering helpers.

### 4.2 Initialize handshake — four implementations

| Path | Location |
|---|---|
| Handler servers / stdio | `ExMCP.Server.Dispatch.do_dispatch("initialize", …)` L115-117 |
| DSL servers | `ExMCP.Protocol.RequestProcessor.process_initialize/2` L96-144 |
| HTTP | `ExMCP.MessageProcessor.MethodHandlers.handle_initialize/5` L21-26 |
| HTTP transport helper | `ExMCP.Transport.HttpServer` L292-298 (canned, hardcoded version at L295) |

Plus `HandlerServer.process_mcp_request` L640, `StdioServer.handle_request` L174,
`Testing.MockServer` L344/L448. `notifications/initialized` is handled in `RequestProcessor`
L363-366 and `MockServer` L448 only; `Dispatch` silently ignores it.

**Action:** all four keep their legacy `initialize` branch, and all four gain a modern branch
that never sees `initialize`. `server/discover` should be implemented **once** in
`ExMCP.Server.Discover` and wired into all four tables.

### 4.3 Error codes — four definitions, one collision

`lib/ex_mcp/protocol/error_codes.ex` (canonical), `Internal.Protocol` L652-665,
`Types` L31-35, `MessageValidator` L26-29. `-32002` is **double-assigned** to both
`consent_required` and `resource_not_found`.

**Action:** single module; add `-32020/-32021/-32022`; move resource-not-found to `-32602`
for modern while still *accepting* `-32002` from legacy servers; move `consent_required` out
of the reserved sub-range (it is an ExMCP-local concern, so per spec it belongs **outside**
`-32768..-32000`); retire `-32042`.

### 4.4 Client

| Concern | Current | Needed |
|---|---|---|
| Handshake | `ConnectionManager.do_handshake/3` → `send_initialize_request` → `send_initialized` | Era probe via `server/discover`; no handshake in modern |
| Outbound `_meta` | `ExMCP.Internal.RequestParams` — `with_meta/2` L38, `with_non_empty_meta/2` L43, `with_opts_meta/2` L50, `with_progress_or_meta/2` L73-79. Only progress + user meta | Must inject protocolVersion + clientCapabilities on **every** request; add clientInfo, logLevel |
| Result parsing | No `resultType` awareness | Discriminate `complete` / `input_required` / `task`; absent ⇒ complete |
| Server→client requests | `RequestHandler` L431-441 dispatch table (`ping`, `roots/list`, `sampling/createMessage`, `elicitation/create`) | Keep for legacy; add MRTR path for modern |
| Health check | idle `ping`; interval `health_check_interval: 30_000` default at `client.ex` L743, scheduled L1410/L1459, sent by `RequestHandler.send_ping/1` L180-187 | `ping` is gone — use cached `server/discover` in modern, or make health checks era-conditional |
| Subscriptions | `Client.subscribe_resource/2` → `Operations.Resources` L79 | Re-implement over `subscriptions/listen` |
| Caching | none | `ttlMs`/`cacheScope` cache + `list_changed` invalidation |
| Auto-reconnect | exists, backoff + jitter | Must re-establish `subscriptions/listen` streams after reconnect (spec: server holds no state) |
| `x-mcp-header` | none | MUST mirror annotated params into `Mcp-Param-*`; MUST exclude tools with invalid annotations from `tools/list` output |

### 4.5 Server

| Concern | Current | Needed |
|---|---|---|
| Request context | Session-derived; `_meta` merged into tool args by `Dispatch.tool_arguments/1` L106-113 | Explicit `RequestContext` from `_meta`, passed to handlers |
| `server/discover` | Absent (name appears only in a phantom `"draft"` gate) | MUST implement |
| Result envelope | `ResultNormalizer` — no `resultType` | Stamp `resultType`, `_meta.serverInfo`, `ttlMs`, `cacheScope` |
| MRTR | Server→client requests via `HandlerServer` (L358 roots, L384-395 sampling) and `SSESession.send_request/3` | New `{:input_required, …}` handler return + `RequestState` sealing |
| Subscriptions | `Dispatch` L163-171 → `handle_subscribe_resource/2`; no subscriber registry (handler's job) | `subscriptions/listen` + `Subscriptions` registry + acknowledgment notification |
| Logging | `logging/setLevel` via `Dispatch.set_log_level/2` L244-264 (succeeds even without the callback) | Per-request `logLevel`; MUST NOT emit `notifications/message` when absent |
| Deterministic tool order | not guaranteed | SHOULD return `tools/list` in stable order |
| Capabilities | `VersionRegistry.capabilities_for_version/1` **disagrees with** `FeatureFlags` on `tasks` | Reconcile; add `extensions` map to both client and server capabilities |

### 4.6 Streamable HTTP

`lib/ex_mcp/transport/http.ex` (1382 LOC) and `lib/ex_mcp/http_plug.ex` (1081 LOC) are the two
largest single changes.

Client transport removals for modern: `@session_header` L149, `:session_id`, `:last_event_id`,
`:sse_deferred_attempted`, `maybe_update_session_id/2` L1086-1092,
`maybe_start_deferred_sse/1` L1093-1125, `Last-Event-ID` on the GET SSE start
(`start_sse/1` L1158-1163) **and on every POST** (`build_request_headers/1` L1254, header at
L1278-1283), `terminate_session/1` L1011-1045. Note `trigger_sse_reconnect/1` L1130-1146 does
not itself set `Last-Event-ID`. Additions: required headers, header/body validation on the
response side, `-32020` handling, `400`-body inspection for era detection, `404` + `-32601`
for unknown methods.

Server plug: `do_dispatch/4` L153-262 has 14 clauses, including `GET /sse`, `GET /mcp/v1/sse`,
`DELETE /sse/:id`, `DELETE /mcp/v1/sse/:id` (L224), `DELETE *`, and a `GET *` SSE catch-all.
In a modern-only configuration these must return `405`; in dual-era they stay for
legacy clients but a modern request must never mint or echo `mcp-session-id` (currently echoed
at L319, 334, 356, 379, 396, 423, 433, 450). `handle_session_delete/3` L514-567 and
`handle_sse_connection/2` L570-645 become legacy-only. New: `Mcp-Method` / `Mcp-Name` /
`Mcp-Param-*` validation including the `=?base64?…?=` sentinel decoding, and
`X-Accel-Buffering: no` on SSE responses.

`ExMCP.Plugs.ProtocolVersion` must become unconditional for modern requests (currently behind
a default-off feature flag).

### 4.7 Tasks

Existing: `tasks/get`, `tasks/list`, `tasks/result`, `tasks/cancel`,
`notifications/tasks/status`, `ExMCP.Tasks.Task` struct (`lib/ex_mcp/tasks/task.ex`), DSL
`execution` instruction, `taskSupport` in `Types.V20251125`. Notably the `tools/call`
task-augmentation path was **never implemented** — nothing reads a `task` field or returns a
`CreateTaskResult`.

New extension shape:

| Old | New |
|---|---|
| `tasks/list` | **removed** |
| `tasks/result` (blocking) | **removed** — poll `tasks/get` |
| `tasks/get` | kept; returns full task incl. `result` / `error` / `inputRequests` |
| `tasks/cancel` | kept; cooperative |
| — | `tasks/update` — client submits `inputResponses` for a task in `input_required` |
| `notifications/tasks/status` | `notifications/tasks`, opted into via `subscriptions/listen` |
| per-request opt-in (`taskSupport`, tool `execution`) | **removed** — server-directed; client opts in once via the extension capability |
| `ttl` / `poll_interval` (struct, L37-38 & L49-50) emitted as `"ttl"` / `"pollInterval"` (wire, `to_map/1` L172-173) | `ttl_ms` / `poll_interval_ms` emitted as `"ttlMs"` / `"pollIntervalMs"` — two renames each |
| — | `CreateTaskResult` with `resultType: "task"` |

The existing `ExMCP.Tasks.Task` state machine (`working` / `input_required` / `completed` /
`failed` / `cancelled`) matches the new spec exactly — keep it, rename the two TTL fields,
add `inputRequests` and `error`, and delete the `to_map/1` keys that no longer exist.

**Action:** remove the DSL `execution` instruction and `taskSupport` from the modern path
(keep for legacy `2025-11-25`), and finally implement the creation side.

### 4.8 Authorization

`lib/ex_mcp/authorization/` (6100 LOC, 28 files) needs:

1. **RFC 9207** — authorization servers SHOULD return `iss`; clients **MUST** validate a
   present `iss` against the recorded issuer before redeeming the code.
2. **Client ID Metadata Documents** — new preferred registration mechanism. Client hosts an
   HTTPS JSON doc whose `client_id` equals its own URL. Detect support via
   `client_id_metadata_document_supported` in AS metadata. Priority order:
   pre-registered → CIMD → DCR → prompt user.
3. **`application_type`** — MUST be sent during DCR; `"native"` for desktop/CLI/localhost,
   `"web"` otherwise. Handle registration failures from OIDC redirect-URI constraints.
4. **Issuer-keyed credential storage** — persisted credentials MUST be keyed by issuer, MUST
   NOT be reused across authorization servers, MUST trigger re-registration on AS change.
5. DCR itself is now **deprecated** — keep, mark, document.

### 4.9 Types & schema

`lib/ex_mcp/types.ex` is 702 LOC of hand-written types, plus four per-version modules totalling
706 LOC (`v20241105` 103, `v20250326` 114, `v20250618` 218, `v20251125` 271) that are
documentation-grade and not used for runtime validation.

**Action:** add `lib/ex_mcp/types/v20260728.ex`. Given the size of the delta, consider
generating it from the vendored `schema.json` rather than hand-writing — but that is a
nice-to-have, not a blocker. `mix mcp.sync_spec --version 2026-07-28` should land the spec
docs and `schema.ts` / `schema.json` under `docs/mcp-specs/2026-07-28/` on day one.

### 4.10 Testing

248 files / 73k LOC. The compliance suite has a structural problem for this migration:
`test/ex_mcp/compliance/version_generator.ex` has `@versions ["2024-11-05", "2025-03-26", "2025-06-18"]`
— `2025-11-25` was added as a standalone hand-written file instead. Adding `2026-07-28` needs a
decision: extend the generator (and backfill `2025-11-25` into it), or write another standalone.

**Recommendation:** extend the generator. The 14 `Features.*` modules are the right unit of
reuse, but several of them (roots, sampling, elicitation, logging) describe *server-initiated
request* flows that no longer exist in modern — they need era-conditional variants.

`test/conformance/server.exs` hardcodes `mcp-protocol-version: "2025-11-25"` at L522 and the
external harness (`@modelcontextprotocol/conformance@0.1.16`) currently passes 39/39 server +
226/226 client. `scripts/conformance.sh` **already has a non-gating `draft-alpha` mode**
(`run_draft_alpha/0` L212-224, pinned to `@modelcontextprotocol/conformance@0.2.0-alpha.9`
via `CONFORMANCE_ALPHA_VERSION`, usage L252-268, documented at
`test/ex_mcp/compliance/MCP_COVERAGE_MATRIX.md` L9/L11). That mode is the natural early
signal; a stable harness release supporting 2026-07-28 is the acceptance gate.

---

## 5. Phased plan

Dependencies are strict unless noted. Phases 7/8/9 can run in parallel with each other once
6 lands.

```
0 ──> 1 ──> 2 ──> 3 ──> 4 ──> 6 ──> 10
                   └──> 5 ──┘   ┌──> 7
                                ├──> 8
                                └──> 9
```

---

### Phase 0 — Foundations

> **Most of this phase has been split out into
> [`PRE_2_0_TECH_DEBT_PLAN.md`](./PRE_2_0_TECH_DEBT_PLAN.md), targeting `1.0.0-rc.5`.**
> It is pre-existing debt, not 2026-07-28 work, and it can ship without any wire change.
> **This phase assumes rc.5 has landed.** If it has not, the debt tracks become Phase 0
> here and the estimates below are wrong.

**Inherited from rc.5** (do not redo): single version registry, single method table
(`ExMCP.Protocol.Methods`), single error-code module with `-32020`/`-32021`/`-32022` defined
additively, `era_for/1` / `modern?/1` helpers, a version-derived compliance test generator,
and characterization tests pinning per-version wire output.

**Remaining Phase 0 work — the parts that genuinely require the breaking release:**

- [ ] `mix mcp.sync_spec --version 2026-07-28` → `docs/mcp-specs/2026-07-28/` (+ `schema.ts`, `schema.json`).
      Verify `dev/ex_mcp/spec_sync/file_mapper.ex` maps the new pages (`basic/patterns/*`,
      `basic/versioning`, `basic/transports/*`, `server/discover`, `server/utilities/caching`) —
      the 2026-07-28 doc tree was reorganised relative to 2025-11-25 and the mapper will need
      new entries.
- [ ] **Consolidate the five `initialize` implementations** behind one
      `build_initialize_result/2` — `request_processor.ex` L118-144, `message_processor.ex` L283,
      `server/handler.ex` L300-305 and L745, `server/dsl.ex` L661,
      `transport/http_server.ex` L292-298. They disagree on default version, capability source
      and key casing. **This is the natural first commit of the 2.0 branch** — all five have to
      be touched anyway to add the modern era. (Deferred from rc.5 as §5.8 there.)
- [ ] Register `"2026-07-28"` in `VersionRegistry` and have `era_for/1` return `:modern` for it.
- [ ] Fix `RequestProcessor` L119's `"2025-06-18"` default (wire-visible for clients that omit
      `protocolVersion`; rc.5 §5.4 added the pinning test, 2.0 changes the behavior).
- [ ] Fix or delete `Transport.HttpServer` L292-298's canned `initialize` response with its
      hardcoded `"2025-03-26"` (rc.5 §5.3 documented it as an example; 2.0 changes it).
- [ ] Resolve the `-32002` collision: move resource-not-found to `-32602` for modern while
      still *accepting* `-32002` from legacy servers, and move `consent_required` out of the
      reserved sub-range. Note `-32002` currently carries a **third** meaning in
      `error.ex` L384/L461 (`prompt_error`). Retire `-32042`. (rc.5 §5.2.)
- [ ] Reconcile `FeatureFlags` vs `VersionRegistry` on `tasks` — resolves itself via extension
      negotiation in Phase 8, but the 2025-11-25 path must keep advertising `tasks`
      unconditionally or it is a regression for existing users. (rc.5 §5.5.)
- [ ] Decide the fate of `VersionNegotiator.build_capabilities/1` — it and
      `VersionRegistry.capabilities_for_version/1` are two different capability vocabularies
      with zero overlapping experimental keys on 2025-06-18+. Only `VersionRegistry` reaches
      the wire; `build_capabilities/1` has zero lib callers but ~45 test assertions. (rc.5 §5.1.)
- [ ] ~~Route `Handler.handle_url_elicitation/3`~~ — **shipping in rc.5 as Track G.** Phase 4
      inherits a correct dispatcher; MRTR must reuse it rather than adding a second routing path.

**Exit:** one `initialize` builder; `"2026-07-28"` registered as `:modern`; full suite green.

---

### Phase 1 — Types

- [ ] `lib/ex_mcp/types/v20260728.ex`: `RequestMetaObject`, `NotificationMetaObject`,
      `ResultMetaObject`, `ResultType`, `CacheableResult`, `DiscoverResult`,
      `InputRequests` / `InputResponses` / `InputRequiredResult`, `SubscriptionFilter`,
      `SubscriptionsListenRequest`/`Result`, `HeaderMismatchError`,
      `UnsupportedProtocolVersionError`, `MissingRequiredClientCapabilityError`.
- [ ] Add `extensions` to `client_capabilities` / `server_capabilities` in `ExMCP.Types`.
- [ ] Loosen `inputSchema`/`outputSchema` types to any JSON Schema 2020-12, and
      `structuredContent` to any JSON value (currently narrower).
- [ ] `min`/`max`/`default` are `number`, not `integer` (upstream generator fix).

**Exit:** compiles; `mix dialyzer` clean.

---

### Phase 2 — Wire plumbing

- [ ] `ExMCP.Protocol.Meta` (new): build/parse the `io.modelcontextprotocol/*` `_meta` block;
      enforce the `_meta` key-naming rules (reserved `*.modelcontextprotocol` / `*.mcp` second
      labels); OTel passthrough for `traceparent`/`tracestate`/`baggage`.
- [ ] `ExMCP.Internal.RequestParams`: inject required `_meta` on every modern outbound request.
      Single choke point — do **not** scatter this across the ~30 `encode_*` functions.
- [ ] `ExMCP.Server.RequestContext` (new) + extraction in `Dispatch`, `RequestProcessor`,
      `MethodHandlers`. Validate required fields → `-32602` / HTTP `400`.
- [ ] `ExMCP.Server.ResultNormalizer`: stamp `resultType: "complete"` and
      `_meta["io.modelcontextprotocol/serverInfo"]` on modern results.
- [ ] Client: parse `resultType`; absent ⇒ `"complete"`; unknown ⇒ protocol error.
- [ ] `ExMCP.Server.Discover` (new) + wire `server/discover` into all four method tables.
- [ ] Client: `server/discover` request + `DiscoverResult` → populate `:server_info`,
      `:server_capabilities`, `:protocol_version`.
- [ ] `MissingRequiredClientCapabilityError` emission when a handler needs an undeclared
      capability.

**Exit:** a modern client can call `server/discover` + `tools/list` + `tools/call` against a
modern ExMCP server over stdio, with no `initialize`.

---

### Phase 3 — Era detection & dual-era dispatch

- [ ] `ExMCP.Client.EraProbe` (new):
      - stdio/local/test: send `server/discover` with the preferred modern version.
        `DiscoverResult` ⇒ modern. Recognised modern JSON-RPC error (e.g. `-32022`) ⇒ modern,
        retry with an advertised version. Any other error **or timeout** ⇒ legacy → `initialize`.
        The fallback MUST NOT be keyed to a specific error code.
      - HTTP: attempt a modern request; on `400`, inspect the body — recognised modern error ⇒
        modern; otherwise fall back to `initialize`, then optionally to HTTP+SSE.
      - Cache the era per server process (stdio) / origin (HTTP); allow persistence across
        restarts with re-probe on failure.
- [ ] `ExMCP.Client.ConnectionManager`: branch on era. Modern path skips
      `send_initialize_request`/`send_initialized` entirely.
- [ ] Server: dual-era selection — a request carrying modern `_meta` is served statelessly;
      an `initialize` request selects legacy semantics scoped to the process (stdio) or session
      (HTTP). Both MAY be served concurrently on one endpoint.
- [ ] Modern-only servers SHOULD name their supported versions in the error returned to a
      legacy `initialize` (legacy clients have no fall-forward).
- [ ] `UnsupportedProtocolVersionError` emission + client-side retry with a mutually supported
      version.
- [ ] Health checks: `ping` no longer exists in modern. Make `:health_check_interval`
      era-aware — use a cached `server/discover`, or disable in modern and rely on transport
      liveness. **Open question, see §8.**

**Exit:** the full 7-row compatibility matrix from the spec is covered by tests.

---

### Phase 4 — MRTR

- [ ] `ExMCP.Client.MRTR` (new): on `resultType: "input_required"`, fulfil each `inputRequests`
      entry via the existing `ExMCP.Client.Handler` callbacks, then re-issue the original
      request with a **new id**, `inputResponses`, and the echoed-verbatim `requestState`.
      Never inspect `requestState`. Omit it entirely if the server didn't send one.
- [ ] Round-trip cap + telemetry (`[:ex_mcp, :client, :mrtr, :round]`).
- [ ] Capability gate: refuse to fulfil an `inputRequest` type not declared in this client's
      capabilities; surface a protocol error.
- [ ] Reuse the URL-mode elicitation dispatcher fixed in rc.5 Track G — do **not** add a second
      routing path inside `MRTR`.
- [ ] Server: `{:input_required, input_requests, state}` / `{:input_required, input_requests,
      request_state, state}` handler returns, honoured on `tools/call`, `resources/read`,
      `prompts/get` **only** (spec forbids elsewhere).
- [ ] `ExMCP.Server.RequestState` (new): AEAD seal/unseal with principal binding, TTL, and a
      digest of method + salient params. Reject on verification failure. Document that
      single-use requires server-side enforcement.
- [ ] `ExMCP.Server.DSL.Result.input_required/2` builder.
- [ ] Legacy path (server-initiated requests via `HandlerServer` / `SSESession`) stays intact
      and is used only when `era == :legacy`.

**Exit:** an elicitation-driven tool call round-trips end-to-end on stdio and HTTP; tampered
`requestState` is rejected.

---

### Phase 5 — Subscriptions

- [ ] `ExMCP.Server.Subscriptions` (new): registry of `{subscription_id, filter, transport_ref}`.
- [ ] `subscriptions/listen` handler; emit `notifications/subscriptions/acknowledged` **first**,
      reflecting only the honoured subset of the filter.
- [ ] Stamp `_meta["io.modelcontextprotocol/subscriptionId"]` on every notification on the
      stream. Server MUST NOT send unrequested types.
- [ ] Request-scoped notifications (`notifications/progress`, `notifications/message`) continue
      to flow on the **originating request's** stream, never on the listen stream.
- [ ] Graceful closure: empty `SubscriptionsListenResult` with `_meta.subscriptionId`.
- [ ] Cancellation: HTTP ⇒ closing the SSE stream; stdio ⇒ `notifications/cancelled` on the
      listen request id.
- [ ] `ExMCP.Client.Subscription` (new) + re-implement `subscribe_resource/2` /
      `unsubscribe_resource/2` on top of it for modern; keep `resources/subscribe` for legacy.
- [ ] Auto-reconnect must re-send `subscriptions/listen` — the server holds no state across
      reconnects.
- [ ] SSE comment keep-alives (`:\r\n`) on long-lived listen streams; clients must ignore them.

**Exit:** list-changed and resource-updated notifications delivered over both transports with
correct subscription correlation; reconnect re-establishes streams.

---

### Phase 6 — Streamable HTTP rework

- [ ] Client transport: remove session/resumability state for modern (see §4.6 for the exact
      line references). Keep the code path for legacy.
- [ ] Client: emit `MCP-Protocol-Version`, `Mcp-Method`, `Mcp-Name` on every POST.
      Base64 sentinel (`=?base64?…?=`) encoding when a value isn't header-safe, **including**
      plain-ASCII values that happen to match the sentinel pattern.
- [ ] Client: `x-mcp-header` → `Mcp-Param-{Name}` mirroring; static-reachability validation;
      **exclude** tools with invalid annotations from `tools/list` results and log a warning.
      On `-32020` due to missing/mismatched `Mcp-Param-*`, re-fetch `tools/list` and retry once.
- [ ] Server plug: validate header↔body agreement (numeric compare for integers, Base64 decode
      before compare) → `400` + `-32020`. Missing required header ⇒ same.
- [ ] Server plug: `404` + `-32601` for unknown methods (distinguishes a modern server from a
      legacy HTTP+SSE `404`).
- [ ] Server plug: modern requests never mint/echo `Mcp-Session-Id`; ignore `Last-Event-ID`.
      `GET`/`DELETE` on the MCP endpoint ⇒ `405` in a modern-only configuration.
- [ ] `X-Accel-Buffering: no` on SSE responses.
- [ ] Closing the SSE response stream = cancellation; server stops work and sends nothing more.
- [ ] `ExMCP.Plugs.ProtocolVersion` unconditional for modern (drop the default-off flag).

**Exit:** `test/conformance/server.exs` + `client.exs` pass against the 2026-07-28 harness.

---

### Phase 7 — Caching, logging, ordering, schemas

- [ ] `ttlMs` + `cacheScope` on `server/discover`, `tools/list`, `prompts/list`,
      `resources/list`, `resources/templates/list`, `resources/read`. Never on
      `input_required` results.
- [ ] `ExMCP.Client.Cache` (new): freshness = `now < t_received + ttlMs`; `ttlMs` absent or
      negative ⇒ 0; no background polling; `list_changed` notification invalidates immediately;
      MRTR retries (carrying `inputResponses`/`requestState`) **MUST NOT** be cached; per-page
      caching for paginated lists; `private` scope keyed by authorization context.
- [ ] Server: per-request `logLevel` from `_meta`; **MUST NOT** emit `notifications/message` for
      requests without it. Remove `logging/setLevel` from the modern method table.
- [ ] Deterministic `tools/list` ordering.
- [ ] JSON Schema: `$ref` MUST NOT auto-dereference network URIs (opt-in only, off by default,
      host allowlist, reject loopback/link-local/private, timeouts, size limits, logging);
      bound composition-keyword depth / subschema count / validation time. Touches
      `ExMCP.Content.SchemaValidator` and `ExMCP.Content.Validation`.
- [ ] OTel `_meta` propagation conventions (`traceparent`, `tracestate`, `baggage`).

---

### Phase 8 — Tasks extension

- [ ] Extension identifier `io.modelcontextprotocol/tasks` in client + server `capabilities.extensions`.
- [ ] `CreateTaskResult` with `resultType: "task"`, returned **unsolicited** from `tools/call`
      (and other supported requests) when the client declared the extension. Server MUST check
      the client declared it first.
- [ ] `tasks/get` (poll), `tasks/update` (submit `inputResponses`), `tasks/cancel` (cooperative).
      Remove `tasks/list` and `tasks/result` from the modern table.
- [ ] `notifications/tasks` carrying full task state, opted into via `subscriptions/listen`.
- [ ] `ExMCP.Tasks.Task`: rename `ttl` → `ttlMs`, `pollInterval` → `pollIntervalMs`; add
      `inputRequests`, `error`; update `to_map/1`.
- [ ] Durable creation before responding; task IDs survive client restarts.
- [ ] Remove DSL `execution` instruction and `taskSupport` from the modern path (keep for
      `2025-11-25`).
- [ ] Retire the always-off `FeatureFlags` `:tasks` gate in favour of extension negotiation.

---

### Phase 9 — Authorization

- [ ] RFC 9207: validate a present `iss` against the recorded issuer before code redemption.
- [ ] Client ID Metadata Documents: HTTPS `client_id` URL with a path component; document
      `client_id` MUST equal the URL; MUST include `client_id`, `client_name`, `redirect_uris`;
      detect `client_id_metadata_document_supported`; optional `private_key_jwt`.
- [ ] Registration priority: pre-registered → CIMD → DCR → prompt.
- [ ] `application_type` on DCR (`"native"` for desktop/CLI/localhost, `"web"` otherwise);
      handle OIDC redirect-URI rejections with an actionable error and optional retry.
- [ ] Key persisted credentials by issuer; refuse cross-AS reuse; re-register on AS change;
      surface an error on mismatched pre-registered credentials.
- [ ] Mark DCR deprecated in docs and `@doc`.

---

### Phase 10 — Removals, docs, release

- [ ] Remove `ExMCP.Server.Tools` + `Simplified`/`builder`/`helpers`/`registry`/
      `response_normalizer`/`ast_validator` (~2138 LOC, scheduled for 1.1.0 — take it in 2.0.0).
- [ ] Decide HTTP+SSE (2024-11-05) transport fate — spec reclassifies it Deprecated, not
      Removed. Recommend: keep, mark clearly, exclude from new-server defaults.
- [ ] Keep Roots/Sampling/Logging; add deprecation notes pointing at the suggested migrations
      (tool params / resource URIs for roots; direct LLM APIs for sampling; stderr or OTel for
      logging).
- [ ] Docs: `docs/getting-started/MIGRATION.md` gets a 1.x → 2.0 section; `docs/ARCHITECTURE.md`
      gets the era model; `docs/TRANSPORT_GUIDE.md` gets the new HTTP shape; `CLAUDE.md` updated.
- [ ] `CHANGELOG.md` `[2.0.0]` with **BREAKING:** entries; `mix.exs` version bump.
- [ ] Green: `mix test.suite ci`, `mix credo`, `mix dialyzer`, `mix sobelow --skip`,
      `scripts/conformance.sh` against a 2026-07-28-aware harness.

---

## 6. Public API impact

Things downstream users will notice.

| Change | Severity | Mitigation |
|---|---|---|
| `ExMCP.Client.server_info/1`, `server_capabilities/1`, `protocol_version/1` | none | Populated from `DiscoverResult` in modern; same shape |
| `ExMCP.Client.Handler` behaviour | none | MRTR reuses the same callbacks |
| `ExMCP.Client.ping/2` | **breaking in modern** | Raise/`{:error, :not_supported}` when era is modern; keep for legacy |
| `ExMCP.Client.set_log_level/2` | **breaking in modern** | Becomes a per-request option; keep the function as a client-wide default that populates `_meta` |
| `ExMCP.Client.subscribe_resource/2` | source-compatible | Re-implemented over `subscriptions/listen` |
| Server handler return tuples | additive | New `{:input_required, …}` |
| Server handlers receiving `_meta` in tool args | **changed** | `RequestContext` becomes the supported way; keep merging `_meta` into args for one release with a deprecation warning |
| `ExMCP.Server.Tools` | **removed** | Migrate to `ExMCP.Server.DSL` (already the documented path) |
| DSL `execution` instruction | **removed in modern** | Server-directed tasks — no per-tool declaration needed |
| Tasks API (`tasks/list`, `tasks/result`) | **removed** | Poll `tasks/get` |
| `ExMCP.SessionManager` | modern: unused | Still exported for legacy; document as legacy-only |

---

## 7. Test strategy

1. **Extend the compliance generator.** Add `2025-11-25` and `2026-07-28` to
   `test/ex_mcp/compliance/version_generator.ex` `@versions` and write
   `handlers/handler20251125.ex` + `handler20260728.ex`. Make the `Features.*` modules
   era-conditional where the flow changed (roots, sampling, elicitation, logging, transport).
2. **New feature modules** for `Features.Discover`, `Features.MRTR`, `Features.Subscriptions`,
   `Features.Caching`, `Features.RequestHeaders`.
3. **Compatibility-matrix suite** — one test per row of the spec's 7-row matrix
   (modern/legacy/dual-era × modern/legacy), asserting the exact fallback behaviour including
   the *timeout* branch on stdio.
4. **Property tests** for: `_meta` key-name validation, Base64 sentinel round-tripping,
   `ttlMs` freshness arithmetic, MRTR round-trip convergence under a round cap.
5. **Security tests** for `requestState`: tamper detection, wrong principal, expired TTL,
   cross-request replay.
6. **No `Process.sleep` for synchronization** — per `CLAUDE.md`, use `assert_receive`,
   monitors, a `ping`-equivalent flush (note: in modern the flush must be a real request such
   as `tools/list`, since `ping` is gone), telemetry assertions, or `wait_until/2`.
7. **External conformance** — `scripts/conformance.sh` against a 2026-07-28-capable release of
   `@modelcontextprotocol/conformance`. Un-hardcode `mcp-protocol-version` at
   `test/conformance/server.exs` L522. Track any gaps in `expected-failures.yml` and drive to zero.
7b. **Bump the alpha harness.** `scripts/conformance.sh` `run_draft_alpha/0` is already wired
   up; track `CONFORMANCE_ALPHA_VERSION` forward through each phase and drive the draft
   results from "non-gating exploration" to gating by Phase 10.
8. **Interop** — `test/interop/` vendors the TypeScript SDK; bump it once an SDK release
   implements 2026-07-28 and run cross-implementation tests both directions.

---

## 8. Risks & open questions

| # | Item | Notes |
|---|---|---|
| R1 | **Ecosystem timing.** Most servers and clients in the wild are legacy. | Dual-era is exactly the hedge. Ship modern behind a config flag first, flip the default when the SDKs land. |
| R2 | **Health checks lose `ping`.** ExMCP's 30s idle ping is load-bearing for the auto-reconnect path. | Options: (a) `server/discover` as the probe — cacheable, cheap, always implemented; (b) disable health checks in modern and rely on transport liveness + `subscriptions/listen` keep-alives. **(a) is recommended**; needs a decision. |
| R3 | **`requestState` key management.** MRTR security depends on an AEAD key the operator must configure. | Fail loudly at boot if a handler returns `{:input_required, _, request_state, _}` with no key configured. Document key rotation. |
| R4 | **HTTP plug complexity.** `do_dispatch/4` already has 14 clauses (L153-262); dual-era adds more. | Consider splitting modern vs legacy into separate plug modules behind a router rather than growing `do_dispatch/4`. |
| R5 | **Stateless servers break existing user handlers** that relied on per-connection state. | The spec's answer is explicit server-minted handles as tool arguments (§"Stateful Tools"). Needs a documented migration recipe with an example. |
| R6 | **Conformance harness availability.** No *stable* 2026-07-28 harness release is confirmed; `scripts/conformance.sh` already tracks `0.2.0-alpha.9` non-gating. | Promote the existing `draft-alpha` mode to gating once a stable release lands; until then self-conformance tests + TS SDK interop are the gate. |
| R7 | **Spec churn.** `2026-07-28` is dated in the near past relative to this plan; errata are likely. | `mix mcp.sync_spec` has sha256/ETag change detection — run it in CI and alert on drift. |
| R8 | **Scope.** ACP (17.6k LOC) is untouched but shares `_meta` helpers. | Verify no shared-helper regressions when `RequestParams` changes. |
| Q1 | Should modern be opt-in (`config :ex_mcp, prefer_era: :legacy`) for the first 2.0 release? | Recommend: modern preferred by default with automatic fallback, since the fallback is well-specified. |
| Q2 | Generate `types/v20260728.ex` from `schema.json`, or hand-write? | Hand-writing is consistent with the existing four modules; generating is more accurate. Lean generate, with the generator living in `dev/`. |
| Q3 | Keep `ExMCP.SessionManager`'s event buffering at all? | Only legacy uses it. Keep, mark legacy-only, and skip supervising it when configured modern-only. |
| Q4 | Ship `1.0.0` stable on `2025-11-25` before starting 2.0? | **Decided: yes.** `1.0.0-rc.5` absorbs the pre-existing debt ([`PRE_2_0_TECH_DEBT_PLAN.md`](./PRE_2_0_TECH_DEBT_PLAN.md)), then `1.0.0` stable, then branch 2.0. |

---

## 9. Appendix A — modern method table

| Method | Direction | Cacheable | MRTR-capable |
|---|---|---|---|
| `server/discover` | C→S | yes | no |
| `tools/list` | C→S | yes | no |
| `tools/call` | C→S | no | **yes** |
| `prompts/list` | C→S | yes | no |
| `prompts/get` | C→S | no | **yes** |
| `resources/list` | C→S | yes | no |
| `resources/templates/list` | C→S | yes | no |
| `resources/read` | C→S | yes | **yes** |
| `completion/complete` | C→S | no | no |
| `subscriptions/listen` | C→S | no | no |
| `notifications/cancelled` | C→S | — | — (stdio only) |
| `notifications/progress` | S→C | — | — (request stream) |
| `notifications/message` | S→C | — | — (request stream, opt-in via `logLevel`) |
| `notifications/subscriptions/acknowledged` | S→C | — | — (listen stream) |
| `notifications/tools/list_changed` | S→C | — | — (listen stream) |
| `notifications/prompts/list_changed` | S→C | — | — (listen stream) |
| `notifications/resources/list_changed` | S→C | — | — (listen stream) |
| `notifications/resources/updated` | S→C | — | — (listen stream) |
| `tasks/get` · `tasks/update` · `tasks/cancel` | C→S | no | — (extension) |
| `notifications/tasks` | S→C | — | — (extension, listen stream) |

## 10. Appendix B — reference links

- Changelog: <https://modelcontextprotocol.io/specification/2026-07-28/changelog>
- Base protocol / `_meta` / error codes: <https://modelcontextprotocol.io/specification/2026-07-28/basic>
- Versioning & compatibility matrix: <https://modelcontextprotocol.io/specification/2026-07-28/basic/versioning>
- MRTR: <https://modelcontextprotocol.io/specification/2026-07-28/basic/patterns/mrtr>
- Subscriptions: <https://modelcontextprotocol.io/specification/2026-07-28/basic/patterns/subscriptions>
- Streamable HTTP: <https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http>
- stdio: <https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/stdio>
- `server/discover`: <https://modelcontextprotocol.io/specification/2026-07-28/server/discover>
- Caching: <https://modelcontextprotocol.io/specification/2026-07-28/server/utilities/caching>
- Tools (incl. `x-mcp-header`): <https://modelcontextprotocol.io/specification/2026-07-28/server/tools>
- Client registration (CIMD): <https://modelcontextprotocol.io/specification/2026-07-28/basic/authorization/client-registration>
- Tasks extension: <https://modelcontextprotocol.io/extensions/tasks/overview> · <https://github.com/modelcontextprotocol/ext-tasks>
- Deprecated features registry: <https://modelcontextprotocol.io/specification/2026-07-28/deprecated>
- Feature lifecycle policy: <https://modelcontextprotocol.io/community/feature-lifecycle>
- Schema: `schema/2026-07-28/schema.ts` in `modelcontextprotocol/specification`
