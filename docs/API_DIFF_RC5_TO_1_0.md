# Public API diff: v1.0.0-rc.5 to 1.0 candidate

This report records the public Elixir API compatibility gate for the MCP
2026-07-28 migration. The comparison is between:

- baseline: `v1.0.0-rc.5` (`a2464c3423ee48ba825402cc2d17be8042f1451a`);
- candidate: the `v1.0.0-rc.6` release commit.

The rc.6 package started the first modern-preferred soak. This report compares
the retained rc.5 API surface with the 1.0 candidate, not version strings.

> **Post-rc.6 note (2026-08-11):** Current `main` adds legacy SSE persistence
> and an additive `ExMCP.SessionManager.append_event/3` entry point. Because the
> accompanying disconnect-lifecycle behavior requires another RC, this rc.6
> snapshot remains historical evidence rather than the final stable audit. The
> API comparison must be regenerated for the next candidate.

## Method

The rc.5 tag and the candidate were compiled independently. As a conservative
package-level check, the audit included every BEAM module whose compile source
is under `lib/`, even when the module is hidden from HexDocs. It normalized and
compared:

- module names;
- exported functions and arities, excluding `module_info/*` and `__info__/1`;
- callbacks, via `Code.Typespec.fetch_callbacks/1` and
  `Code.Typespec.spec_to_quoted/2`;
- public and opaque types, via `Code.Typespec.fetch_types/1` and
  `Code.Typespec.type_to_quoted/1`;
- struct field names.

The snapshot totals were:

| Surface | rc.5 | 1.0 candidate | Removed |
|---|---:|---:|---:|
| Modules | 236 | 290 | 0 |
| Exported functions | 2,288 | 2,690 | 0 |
| Callbacks | 93 | 115 | 0 |
| Struct fields | 363 | 503 | 0 |
| Types | 485 | 588 | 0 named types |

The type count treats a changed definition as one removed textual row and one
added textual row. Those changes were inspected individually as described
below. Public compatibility characterization tests remain the runtime check for
return shapes and retained deprecated modules; a BEAM snapshot cannot prove
behavioral equivalence by itself.

## Result

No public module, exported function/arity, callback, named type, or struct field
present in rc.5 was removed.

Three existing handler callbacks were widened to admit MRTR input-required
results while preserving every rc.5 return:

- `ExMCP.Server.Handler.handle_call_tool/3`;
- `ExMCP.Server.Handler.handle_get_prompt/3`;
- `ExMCP.Server.Handler.handle_read_resource/2`.

Existing structs only gained fields. The additions on rc.5 structs are:

- `ExMCP.Response`: `resultType`, `ttlMs`, and `cacheScope`;
- `ExMCP.Client`: MRTR and subscription bookkeeping;
- `ExMCP.Tasks.Task`: `error` and `input_requests`;
- `ExMCP.Transport.HTTP`: `protocol_era`, `modern_streams`, and `tool_headers`;
- `ExMCP.Server.HandlerServer`: protocol-era, request-context, MRTR,
  replay-cache, and subscription state;
- `ExMCP.Authorization.Provider.OAuth`: authorization-server issuer, rejected-issuer,
  and granted-scope tracking used to bound migration and scope-step-up retries.

The changed typespec definitions are additive or corrective except for the
OAuth state hardening called out below:

- authorization configuration types add issuer, metadata-fetch, client
  registration, credential-store, protocol-version, and HTTP adapter options;
- capability types add the standard `extensions` map;
- JSON Schema types now admit all map-shaped schemas, including boolean
  schemas where the receiving API supports them;
- `tool_result/0` widens `structuredContent` from string-keyed maps to any JSON
  value, and result/task/transport structs describe their added fields;
- `ClientRegistration.registration_request/0` now declares
  `application_type`, which its rc.5 documentation and runtime validation
  already required.

`OAuthFlow.auth_params/0` no longer advertises caller-supplied `:state`.
`start_authorization_flow/1` now rejects it and generates a high-entropy,
single-use state bound to the transaction store. This is an intentional
security behavior change, not a removed function or return shape. Applications
that supplied `:state` must retain the returned transaction and use its
generated `state_param` instead.

## Additive public API

The candidate adds 54 modules and 402 exports. The main supported additions
are grouped here rather than listing internal plumbing:

- era selection and discovery: `ExMCP.Client.EraProbe`,
  `ExMCP.Client.EraCache`, `ExMCP.Server.Discover`, and
  `ExMCP.Protocol.Initialize`;
- modern request context and envelopes: `ExMCP.Server.RequestContext`,
  `ExMCP.Server.Context`, `ExMCP.Protocol.Meta`,
  `ExMCP.Protocol.ResultEnvelope`, and `ExMCP.Protocol.CacheableResult`;
- MRTR: `ExMCP.Client.MRTR`, `ExMCP.Client.InputDispatcher`,
  `ExMCP.Server.MRTR`, `ExMCP.Server.RequestState`, and replay-cache modules;
- subscriptions: `ExMCP.Client.Subscription`,
  `ExMCP.Server.Subscriptions`, the ETS adapter, and the optional PubSub fanout
  adapter;
- Tasks extension: `ExMCP.Tasks`, `ExMCP.Tasks.Extension`, task operations,
  server dispatch, and store behavior/ETS implementation;
- HTTP 2026 routing: request/tool header modules and modern request-stream
  clients;
- OAuth 2026 security: credential stores, issuer checking, metadata fetchers,
  registration policy, and transaction storage.

Existing modules gain additive entry points such as `ExMCP.Client.discover/2`,
`listen/3`, `get_task/3`, `update_task/4`, and `cancel_task/3`, plus schema
compilation, version-registry, version-aware error, stream-management, MRTR
result helpers, and option-aware OAuth metadata URL validation.

## Protocol-driven behavior changes

These are wire or security changes rather than public Elixir symbol removals:

- modern-preferred connections negotiate MCP 2026-07-28 and use
  `server/discover`, stateless Streamable HTTP routing headers, result
  envelopes, MRTR, and `subscriptions/listen`;
- legacy revisions and deprecated HTTP+SSE remain selectable throughout 1.x;
- Tasks are advertised only when the extension is configured and supported;
- result cache metadata is validated, but ExMCP 1.0 does not store or reuse
  responses;
- OAuth state is generated and consumed by ExMCP as described above.

The wire migration is intentionally landing before stable 1.0. These changes
would have required an ExMCP 2.0 release if 1.0 had already been published;
the retained rc.5 public surface lets existing Elixir applications migrate
without a simultaneous package-API rewrite.

## Release decision

The public-API gate is satisfied: there are no unresolved rc.5 symbol,
callback, named-type, struct-field, or return-shape removals. Stable 1.0 still
depends on the conformance, security, interoperability, load, rollback, and
minimum seven-day modern-preferred RC soak gates in the migration plan.
