# ExMCP 1.x Store Adapter

- **Status:** Proposed — ADR and ETS contract suite only; no public
  behaviour and no second backend
- **Baseline:** ExMCP 1.x after `b62eca2` (the `storage_backend:
  :persistent_term` warning and wall-clock TTL notes)
- **Scope:** lock the 1.x event-store contract against current
  `ExMCP.SessionManager` behavior so a later seam can be additive
- **Last updated:** 2026-08-27
- **Related:** [`V2_ROADMAP.md`](./V2_ROADMAP.md) §8.2 and Phase 4;
  [`POST_1_0_MAINTENANCE_PLAN.md`](./POST_1_0_MAINTENANCE_PLAN.md)
  session-storage option

This is a repository design record, not user-facing Hex documentation.
It does not publish a behaviour, change SessionManager runtime behavior,
or implement a durable backend.

## 1. Purpose

[`V2_ROADMAP.md` §8.2](./V2_ROADMAP.md#82-current-classifications)
allows a store-adapter seam in a later 1.x minor **only after** a
standalone store ADR and contract suite are accepted in-tree. ETS must
remain the default and current behavior must stay unchanged. Phase 4
lists the event-store bullets this record pins.

This ADR:

- records what `ExMCP.SessionManager` actually does today;
- locks the 1.x decisions that a later seam must not break;
- proposes a future behaviour surface without adding it to the public
  API; and
- names the gaps that tests pin as current behavior rather than
  inventing new runtime semantics.

A second backend (durable `persistent_term`, Mnesia, Postgres,
filesystem, or a clustered adapter) is out of scope for this change.

## 2. Current 1.x owner

Legacy Streamable HTTP and HTTP+SSE session state live in
`ExMCP.SessionManager`, which the application supervisor starts as a
singleton. MCP 2026-07-28 HTTP is stateless and does not use this
manager, `Mcp-Session-Id`, `Last-Event-ID`, GET streams, or DELETE
termination.

The process always creates three **unnamed, process-owned ETS tables**
in `init/1`:

- sessions (`:set`)
- events (`:ordered_set`)
- claimed JSON-RPC request IDs (`:set`)

`storage_backend: :persistent_term` is accepted for 1.x compatibility
and still uses those ETS tables. `init/1` logs a warning that the
option is a no-op for durability. That warning landed in `b62eca2`.
There is no `persistent_term` write path.

`terminate/2` deletes the three tables. Restarting the GenServer starts
empty. Transports (`ExMCP.HttpPlug`, `HttpPlug.SSEHandler`) call the
SessionManager public functions (`append_event/3`, `store_event/2`,
`replay_events_after/2`, create/ensure/terminate). They do not select
tables, backends, or cursors beyond the opaque event ID they already
put on the wire.

Session TTL is wall-clock idle expiry on purpose. `created_at` and
`last_activity` are absolute `System.system_time(:microsecond)`
timestamps. Cleanup expires an active session when that wall clock is
more than `:session_ttl_seconds` after `last_activity`. A backwards
wall clock yields a negative difference and does not expire the
session. This is not monotonic elapsed time; changing it would change
the meaning of the public timestamps and of `:session_ttl_seconds`.

## 3. Decisions locked for 1.x

| Decision | Lock |
|---|---|
| Default store | ETS remains the only implemented backend and the default. |
| Public API | Current `ExMCP.SessionManager` function names, arities, return shapes, and error atoms stay unchanged. |
| Wire / SSE replay | Persist-before-delivery, `Last-Event-ID` exact-cursor replay, gap retention across disconnect, and opaque event IDs stay unchanged. |
| `storage_backend: :persistent_term` | Remains accepted. Still uses ETS (no-op durability) and still warns. Option removal is 2.0-only. A real second backend is out of scope here. |
| Seam timing | A replaceable adapter seam is allowed in a later 1.x minor only after this ADR and the ETS contract suite are accepted in-tree. The design does not wait for 2.0 runtime ownership. |
| Transport isolation | Transports must not learn backend details. They keep calling SessionManager (or a later facade with the same shapes). No table names, adapter modules, or cursor encodings leak into Plug/SSE. |
| Session TTL | Wall-clock idle expiry stays. Do not "fix" it to `System.monotonic_time/1` in 1.x. |
| Public behaviour | Do not publish a Hex-documented behaviour until this ADR is accepted in-tree **and** a later change introduces the seam. A test-only helper is not a public contract. |
| Second backend | Out of scope. Filesystem, Mnesia, Postgres, durable `persistent_term`, and clustered adapters must pass the same contract suite before they ship. |

These are compatibility locks, not implementation tasks. This change
adds documentation and tests only.

## 4. Event-store contract (current ETS behavior)

Phase 4 requires the event-store contract to cover the bullets below.
The accepted 1.x meaning of each bullet is **what SessionManager does
today**. The ETS-only suite in
`test/ex_mcp/session_store_contract_test.exs` pins that meaning.
Documented gaps are called out here and in the tests; they are not
silently implemented.

### 4.1 Atomic append returning a store-owned opaque event ID

`append_event/3` is the persist-before-delivery path. It assigns the
ID, retains the event, then returns `{:ok, event_data}` with that ID.
The caller does not choose the ID. Concurrent appends are serialized
by the GenServer, so each retained event has a unique store-owned ID.

Current ETS IDs look like `"#{sequence}-0"`. Clients and transports
must treat them as opaque (`V2_ROADMAP.md` §8.3). A later adapter may
use a different representation. Losing events, replaying duplicates, or
changing cursor identity is observable and is not allowed as a silent
1.x change.

`store_event/2` remains the caller-supplied-ID path for tests and older
custom managers. It is not the SSE persist-before-delivery contract.

### 4.2 Ordered replay after an exact cursor

`replay_events_after/2` is the `Last-Event-ID` contract.

- `nil` replays every retained event for the session.
- When the cursor equals a retained event ID, replay is the suffix
  after that exact event (the cursor event itself is not replayed).
- Managed `append_event/3` sessions replay in store-sequence order,
  not wall-clock timestamp order.
- `store_event/2` sessions replay by `{timestamp, sequence, id}` so
  existing caller-supplied IDs keep their historical order.

If the cursor is not retained (evicted, or a custom `store_event/2`
ID), replay falls back to `compare_event_ids/2`: parse
`"integer-integer"` when both sides match that shape, otherwise
string-compare. That fallback is current compatibility behavior, not
a new guarantee for adapters.

Replay of an unknown session returns `{:error, :session_not_found}`.
A terminated session that is still in the table replays its remaining
events (usually none after cleanup).

### 4.3 Bounded retention and cursor-eviction

Retention is fail-closed and newest-preserving:

- a single JSON-encoded event larger than
  `min(:max_event_bytes, :max_replay_bytes_per_session)` is rejected
  with `{:error, :event_too_large}`;
- non-JSON-encodable payloads return
  `{:error, :event_not_json_encodable}`;
- after a successful store, oldest events are discarded until both
  `:max_events_per_session` and `:max_replay_bytes_per_session` hold.

Replaying after an evicted cursor uses the §4.2 fallback. It does not
resurrect discarded events and must not invent replacements. Counts
and byte totals reset when a session is terminated or expires.

### 4.4 Session TTL and explicit deletion

- **TTL** is wall-clock idle expiry on `last_activity`, as locked in
  §3. Append, store, ensure, claim, and other activity refresh that
  timestamp. Cleanup is periodic (`:cleanup_interval_ms`).
- **Explicit deletion** is `terminate_session/1`. It marks the session
  `:terminated`, clears events and request IDs, releases an
  initialization claim, and asks `SubscriptionRegistry` to drop the
  session. A transient SSE disconnect does not terminate the session.
- Terminated rows remain until the next `create_session/1` prunes them
  to free capacity. `get_session/1` still returns the terminated
  record. Further `store_event/2` / `append_event/3` calls return
  `{:error, :session_not_found}`.
- Terminating an unknown session is `:ok`.

Do not change these shapes to make TTL monotonic or to drop terminated
rows immediately.

### 4.5 Idempotent overwrite and deduplication

There are two different stories. Do not collapse them.

| Path | Current expectation |
|---|---|
| `append_event/3` | Not idempotent. Each call allocates a new store-owned ID. |
| `store_event/2` with the same `{session_id, event.id}` | Overwrite in place. The existing store sequence is reused; the payload is replaced. |
| `claim_request_id/2` | The JSON-RPC dedup story. First claim wins (`:ok`). A second claim of the same ID in the same session returns `{:error, :duplicate_request_id}`. The per-session cap fails closed with `{:error, :request_id_limit_exceeded}`. Claims live until the session is terminated or expires. |

A later adapter must preserve these outcomes for the same public
calls. It must not treat `append_event/3` as content-addressed
dedup unless a separate, documented option is added.

### 4.6 Adapter ownership and restart

Today the store **is** the SessionManager process:

- tables are unnamed and owned by that process;
- `terminate/2` deletes them;
- a supervisor restart starts empty tables;
- `storage_backend: :persistent_term` does not change that.

This is the accepted 1.x restart behavior. The contract suite pins
"stop + start with the same registered name loses sessions and
events."

A future durable adapter, when one exists, must document its own
ownership and blast radius: restarting SessionManager or a transport
listener must not silently discard an independently configured durable
store, and two server runtimes must not share keys. That ownership
split is **not** implemented now. Inventing it here would be a
runtime change.

### 4.7 Telemetry that excludes event payloads by default

**Gap (pinned, not implemented):** SessionManager emits no
`:telemetry` events today. Logs use `LogSummary.fingerprint/1` for
session IDs and do not write event payloads.

Phase 4 wants store telemetry that excludes payloads by default, with
opt-in payload capture. That is an eligible 1.x-minor follow-up
(additive events, bounded metadata) once this ADR is accepted. It is
not part of this change. The contract suite attaches handlers for
plausible store prefixes and asserts they stay silent, and it asserts
that logs from append/store do not contain the event payload.

`get_stats/0` remains the public observation surface: session counts,
event count, and ETS memory words. It does not return payloads.

## 5. Proposed future behaviour (unpublished)

Do not add this module to `lib/` or HexDocs until the ADR is accepted
and a later 1.x change introduces the seam.

**Recommendation for the 1.x seam:** one unpublished behaviour,
working name `ExMCP.SessionStore`, covering the state SessionManager's
three ETS tables already own — sessions, replay events, and claimed
request IDs. Do not split session / event / request-id / subscription
behaviours until a second backend exists and the split removes real
duplication. Subscriptions stay on `ExMCP.SubscriptionRegistry`.

Initialization claims, identity binding, and protocol-version
immutability stay SessionManager (or later server-runtime) **policy**.
A store adapter stores the resulting session record; it does not
reimplement those rules.

Proposed callbacks, matching today's public shapes:

```text
create_session(metadata) ->
  session_id | {:error, :session_limit_exceeded}

get_session(session_id) ->
  {:ok, session_data} | {:error, :session_not_found}

terminate_session(session_id) -> :ok

append_event(session_id, type, data) ->
  {:ok, event_data}
  | {:error, :session_not_found | :event_too_large | :event_not_json_encodable}

store_event(session_id, event_data) ->
  :ok | {:error, :session_not_found | :event_too_large | :event_not_json_encodable}

replay_events_after(session_id, cursor) ->
  [event_data] | {:error, :session_not_found}

claim_request_id(session_id, request_id) ->
  :ok | {:error, :session_not_found | :duplicate_request_id | :request_id_limit_exceeded}

get_stats() ->
  %{total_sessions, active_sessions, total_events, memory_usage}
```

Not part of the store behaviour:

- `replay_events_after/3` (transport helper that casts to an SSE
  handler);
- `ensure_session/2`, `ensure_initialized_session/2`,
  `claim_initialization/1`, `complete_initialization/2`,
  `update_session/2` (policy on top of the session record);
- table names, ETS types, or backend atoms.

A later seam should inject the store into SessionManager (or a thin
facade) so HttpPlug keeps calling the same functions. The default
implementation remains the current ETS process. `:persistent_term`
stays accepted and remains a no-op until a real backend exists.

`V2_ROADMAP.md` §10.2 item 3 (one behaviour vs a split) is resolved
for 1.x as: one session/event/request-id behaviour. Revisit the split
in 2.0 if per-server runtimes need independently owned stores.

## 6. Gaps versus Phase 4 (follow-up vs 2.0)

| Gap | Today | Lane |
|---|---|---|
| Store telemetry | None. Logs omit payloads. | Additive 1.x minor after this ADR: bounded metadata, payload capture opt-in. |
| Durable second backend | `:persistent_term` accepted, ETS-only, warns. | Contract/backend may be additive in a later 1.x minor; option removal is 2.0-only. |
| Adapter ownership independent of SessionManager | Process-owned ETS; restart loses state. | Specify with the first durable adapter. Changing restart semantics of the default ETS store is 2.0 unless an opt-in adapter is used. |
| Per-server store isolation | One application-wide SessionManager. | 2.0 runtime ownership (`V2_ROADMAP.md` §8.2). |
| Public behaviour module | None. Test helper only. | Publish only with the 1.x seam, after this ADR is accepted. |

## 7. Out of scope

This change does not:

- implement a second backend or give `:persistent_term` durability;
- change SessionManager clocks, table types, or `storage_backend`
  behavior;
- publish `ExMCP.SessionStore` or any Hex-documented behaviour;
- alter SSE wire/replay behavior or transport modules;
- move session policy (identity, initialization) into a store;
- "fix" wall-clock TTL to monotonic time;
- bump `mix.exs`.

## 8. Acceptance

This ADR is accepted in-tree when:

1. this file is merged without a public behaviour module;
2. `test/ex_mcp/session_store_contract_test.exs` pins every Phase 4
   bullet against current ETS SessionManager, including the telemetry
   and restart gaps as current behavior; and
3. existing SessionManager tests remain green and are not weakened.

After that, a later 1.x minor may introduce the unpublished-then-public
seam behind the same suite. A second backend may land only when it
passes that suite without changing default ETS results.
