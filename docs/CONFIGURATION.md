# ExMCP Configuration Guide

This guide covers the supported configuration surfaces for ExMCP 1.0.

## Dependency

```elixir
def deps do
  [
    {:ex_mcp, "~> 1.0.0-rc.5"}
  ]
end
```

## Protocol Eras and Modes

ExMCP 1.0 implements two wire-incompatible MCP eras:

- **Legacy:** `2024-11-05`, `2025-03-26`, `2025-06-18`, and `2025-11-25`.
- **Modern:** `2026-07-28`, with stateless discovery and per-request context.

`protocol_mode` is the compatibility policy. Set it in application
configuration for a deployment default:

```elixir
config :ex_mcp,
  protocol_mode: :prefer_modern,
  protocol_version: "2025-11-25"
```

The current RC defaults to `:legacy_only`; a later RC will soak
`:prefer_modern` before 1.0. Stable 1.0 will copy the final RC behavior without
another default change. Production deployments should set the mode explicitly
while the RC rollout is in progress.

| Mode | Enabled versions, in preference order | Client establishment | Server acceptance |
|---|---|---|---|
| `:legacy_only` | 2025-11-25 → older legacy | `initialize` only | Legacy only |
| `:prefer_legacy` | Legacy revisions → 2026-07-28 | `initialize`, then a modern probe only after an eligible protocol failure on a live transport | Both eras |
| `:prefer_modern` | 2026-07-28 → legacy revisions | `server/discover`, then legacy fallback only with positive compatibility evidence on a live transport | Both eras |
| `:modern_only` | 2026-07-28 | `server/discover` only | Modern only |

The two preference modes differ when used by a client. On a server both accept
either era; their ordering controls the versions advertised by
`server/discover`. A stdio or BEAM connection pins its first valid modern
request or legacy `initialize` and rejects mixed-era traffic afterward.

Configure one client or server independently when canarying:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://mcp.example.com/mcp",
    protocol_mode: :prefer_modern,
    era_probe_timeout: 2_000,
    era_cache_legacy_ttl: 300_000
  )

{:ok, server} =
  MyServer.start_link(
    transport: :stdio,
    protocol_mode: :prefer_legacy
  )

# Phoenix/Plug servers accept the same option.
forward "/mcp", ExMCP.HttpPlug,
  handler: MyApp.MCPServer,
  protocol_mode: :prefer_legacy
```

Client mode options:

- `:era_probe_timeout` bounds the side-effect-free `server/discover` probe;
  the default is `2_000` milliseconds.
- `:era_cache_legacy_ttl` controls how long a legacy observation is reused;
  the default is `300_000` milliseconds. Modern observations do not expire
  and cannot be replaced by automatic fallback.
- `:reset_era_cache` clears the observation for the exact transport identity
  before connecting. Use it after an intentional endpoint upgrade, not as an
  automatic retry strategy.
- `:era_cache_key` supplies a stable identity for a custom transport that
  cannot be identified from its connected state. Never include raw secrets;
  ExMCP hashes the configured identity.

Fallback is deliberately narrow. A modern timeout, transport failure,
recognized modern error, authentication error, or cached-modern probe failure
does not trigger `initialize`. Similarly, `:prefer_legacy` probes modern only
after a protocol-level legacy failure while the transport remains usable.
Strict modes never fall back.

`protocol_version` is a revision preference, not an era switch. The
application-level value and the compatibility helper
`ExMCP.protocol_version/0` retain their rc.5 legacy semantics during the soak;
use `protocol_mode` to enable modern negotiation. A per-client modern
`protocol_version` is honored only when its mode enables the modern era.

Use the public negotiator for legacy compatibility checks:

```elixir
ExMCP.Protocol.VersionNegotiator.supported?("2025-11-25")
```

See the [migration rollout](getting-started/MIGRATION.md#recommended-rollout)
and the [architecture era model](ARCHITECTURE.md#protocol-era-model) before
changing a production default.

## OAuth Client Registration

Modern authorization uses an explicit client registration strategy in the
HTTP transport's `:auth` map:

```elixir
# Credentials established with this authorization server. Resolve secrets at
# use time rather than embedding them in application configuration.
auth: %{
  client_registration:
    {:pre_registered, "client-id", {:env, "MCP_CLIENT_SECRET"}},
  credential_issuer: "https://auth.example.com"
}

# Portable, self-hosted Client ID Metadata Document.
auth: %{
  client_registration:
    {:cimd, "https://client.example/oauth/metadata.json"},
  private_key: signing_jwk,
  signing_algorithm: "ES256",
  key_id: "client-key-1"
}

# Automatic compatibility fallback. DCR is used only when advertised.
auth: %{
  client_registration: :auto,
  client_metadata_url: "https://client.example/oauth/metadata.json",
  application_type: :native,
  redirect_port: 8080
}
```

Registration priority is pre-registered credentials, a configured CIMD URL
when the authorization server sets
`client_id_metadata_document_supported: true`, deprecated DCR when it exposes
`registration_endpoint`, then an actionable error. `:auto` never fabricates a
metadata URL. Existing `client_id` / `client_secret` keys remain accepted as
1.x compatibility aliases.

A CIMD client ID must be an exact HTTPS URL with a non-root path. The JSON
document at that URL must repeat the same `client_id` byte-for-byte and include
non-empty `client_name` and `redirect_uris`. Use
`ExMCP.Authorization.ClientIdMetadata.build_metadata/1` and `validate/2` before
publishing it. For `private_key_jwt`, publish `jwks_uri` or inline `jwks` and
configure the matching private key locally; ExMCP will not downgrade to a
weaker token authentication method if assertion construction fails.

DCR requires an explicit `application_type: :native | :web` and stable local
`redirect_port`. Registration rejections retain the authorization server's
error response so redirect-policy failures are actionable. ExMCP does not
silently change the application type or redirect URI.

### OAuth metadata network policy

CIMD, Protected Resource Metadata, OIDC/RFC 8414 authorization-server
metadata, and JWKS retrieval use one fail-closed outbound fetch boundary.
Metadata URLs must use HTTPS, including in local development. Each hostname is
resolved on every redirect hop; a DNS answer containing any private, loopback,
link-local, reserved, documentation, or otherwise non-public IPv4/IPv6 address
is rejected. The connection is pinned to an approved address while the original
hostname remains the TLS SNI and certificate-validation name.

Defaults can be tightened globally:

```elixir
config :ex_mcp, :oauth_metadata_fetch,
  max_redirects: 3,
  max_response_bytes: 262_144,
  max_aggregate_bytes: 524_288,
  dns_timeout_ms: 1_000,
  connect_timeout_ms: 2_000,
  request_timeout_ms: 5_000,
  allowed_redirect_origins: []
```

Redirects remain on the current origin by default. If a provider deliberately
hosts metadata on another origin, list each destination as an exact HTTPS
origin such as `https://metadata.example.com`; wildcards and URL paths are not
accepted. Every allowed destination still receives fresh DNS/IP validation.

The metadata client sends only `Accept`, `Accept-Encoding: identity`, and a
non-secret user agent. It never inherits MCP transport headers, authorization,
cookies, client secrets, or proxy credentials. Compressed responses are
rejected and the default client enforces the byte limit while streaming.

The legacy custom metadata-client shapes `get(url)` and `get(url, headers)` are
no longer accepted because they can re-resolve DNS after validation. A custom
`:http_client` must implement:

```elixir
get(uri, approved_address, options)
```

It must connect directly to `approved_address`, preserve `uri.host` for TLS and
HTTP host validation, use only `options[:request_headers]`, enforce
`options[:connect_timeout_ms]`, `options[:request_timeout_ms]`, and
`options[:max_response_bytes]` while streaming, and return
`{:ok, %{status: integer, headers: list, body: binary}}`. Per-flow overrides go
under `metadata_fetch: [...]`; use them only for a tighter policy or an exact
provider redirect.

### Issuer-bound credential persistence

For MCP `2026-07-28`, pre-registered credentials require
`credential_issuer`. ExMCP compares this value byte-for-byte with the issuer in
the discovered authorization-server metadata before resolving or using the
secret. A trailing slash, path change, or any other textual difference is a
mismatch; issuer identifiers are not URL-normalized. During 1.x, only the
legacy `client_id` / `client_secret` aliases retain their old unbound behavior
for legacy protocol versions; the new explicit pre-registration strategy is
always issuer-bound.

Applications that persist DCR registrations or tokens can provide an encrypted
store or OS-keychain adapter implementing
`ExMCP.Authorization.CredentialStore`:

```elixir
auth: %{
  client_registration: :auto,
  application_type: :native,
  redirect_port: 8080,
  credential_store: {MyApp.MCPCredentialStore, store_state},
  credential_context: "desktop-installation-42"
}
```

`credential_context` is a stable, non-secret local index (the resource URL is
the default). The adapter still stores each registration under the exact
versioned issuer + client-ID key supplied to it. On an authorization-server
change, the new issuer partition misses and ExMCP performs registration again;
an adapter returning a credential from another issuer is rejected.

Tokens are partitioned by issuer, client ID, resource and/or audience,
subject or client identity, and normalized granted scopes. Access and refresh
tokens never appear in a storage key, and the credential structs redact secret
fields from `Inspect`. ExMCP intentionally provides no plaintext file adapter.

Old records without an issuer fail with
`{:credential_migration_required, :registration | :token}`. After verifying
the original authorization server out of band, migrate them explicitly with
`CredentialStore.bind_legacy_registration/2` or
`CredentialStore.bind_legacy_token/2`; never use the currently discovered
issuer as an implicit migration value.

### OAuth transaction retention

Every authorization-code flow started by ExMCP uses a random 256-bit `state`
and PKCE verifier. The returned transaction is registered in a supervised,
node-local single-use store before the authorization URL is returned. Callback
validation consumes state atomically, and code exchange atomically binds the
validated code to the exact redirect URI before making the token request. This
path is shared by legacy and `2026-07-28` MCP sessions.

The default store retains up to 10,000 transaction records for 10 minutes. Both
limits can be adjusted:

```elixir
config :ex_mcp, ExMCP.Authorization.OAuthTransactionStore,
  ttl_ms: 600_000,
  max_entries: 10_000
```

Do not shorten the TTL below the time a user may reasonably spend in the
browser. Capacity exhaustion fails new flows closed. The built-in loopback flow
is intentionally node-local; a distributed web callback must route back to the
originating node or implement its own strongly consistent end-to-end flow.

For direct use of `ExMCP.Authorization`, preserve the returned transaction and
pass it through validation and redemption:

```elixir
{:ok, authorization_url, transaction} =
  ExMCP.Authorization.start_authorization_flow(config)

{:ok, code} =
  ExMCP.Authorization.validate_authorization_response(callback, transaction)

ExMCP.Authorization.exchange_code_for_token(%{
  code: code,
  code_verifier: transaction.code_verifier,
  client_id: config.client_id,
  redirect_uri: transaction.redirect_uri,
  token_endpoint: config.token_endpoint,
  transaction: transaction
})
```

ExMCP does not accept caller-supplied state or reserved OAuth fields in
`additional_params`. If a token request has an ambiguous outcome, its code
remains redeemed; restart authorization instead of retrying the code.

## JSON Schema Resource Policy

Every JSON Schema compiled or validated by ExMCP passes through one bounded,
fail-closed policy. By default, only local fragment references (`#` and
`#/...`) are accepted. HTTP(S), file, and relative cross-document `$ref` values
are rejected before ExJsonSchema can resolve them, even if the host application
configured ExJsonSchema's global `:remote_schema_resolver`.

The defaults are suitable for protocol schemas and can be tightened or raised
for a trusted application workload:

```elixir
config :ex_mcp, :json_schema,
  max_schema_bytes: 262_144,
  max_schema_depth: 64,
  max_subschemas: 1_000,
  max_composition_depth: 16,
  resolve_timeout_ms: 1_000,
  validation_timeout_ms: 100
```

`max_subschemas` conservatively counts schema object nodes, including nested
property maps but excluding literal instance data in `const`, `default`, `enum`,
and `examples`. Composition depth counts nesting through `allOf`, `anyOf`,
`oneOf`, `not`, `if`, `then`, and `else`. A zero timeout or limit is valid and
can be used to disable the corresponding work. Invalid values fail closed.

`$schema` draft identifiers are metadata and remain accepted; bundled draft
meta-schemas do not require a network request. Boolean JSON Schemas (`true` and
`false`) are supported.

### Opt-in network references

Keep remote references disabled unless the schema publisher is part of the
application's trust boundary. To opt in, provide a non-empty host allowlist and
increase the outer resolution deadline enough to cover the bounded network
work:

```elixir
config :ex_mcp, :json_schema,
  resolve_timeout_ms: 10_000,
  network_refs: [
    enabled: true,
    allowed_hosts: ["schemas.example.com", "*.schemas.example.net"],
    trust_partition: "production-schema-publishers",
    allow_http: false,
    max_redirects: 3,
    max_documents: 16,
    max_reference_depth: 8,
    max_response_bytes: 262_144,
    max_decompressed_bytes: 262_144,
    max_aggregate_bytes: 1_048_576,
    dns_timeout_ms: 1_000,
    connect_timeout_ms: 2_000,
    request_timeout_ms: 3_000,
    proxy: :disabled
  ]
```

The allowlist contains hostnames, not URLs. `*.example.com` matches subdomains
but not `example.com` itself. HTTPS is required unless `allow_http: true` is set;
plain HTTP provides no publisher authentication or integrity and is not
recommended. Redirects from HTTPS to HTTP are rejected even when HTTP was
enabled for an explicitly HTTP reference.

Every request and redirect target is allowlisted, independently DNS-resolved,
checked for public-only IPv4/IPv6 addresses, and connected to an approved IP
while TLS verification and SNI use the original hostname. A mixed DNS answer
containing even one loopback, link-local, private, reserved, or documentation
address is rejected. URI userinfo, compressed responses, and proxies are
rejected. No cookies, authorization headers, or other credentials are sent.

Fetched documents exist only inside one compilation; ExMCP does not persist or
globally share a remote-schema cache. This is stronger than partitioning a
persistent cache and prevents one tenant or principal from warming another's
schema state. `trust_partition` is hashed in audit logs and establishes the
partition identity for any future cache implementation.

`:dns_resolver` and `:http_client` adapter overrides exist for controlled tests.
Do not replace them in production: doing so replaces the DNS revalidation,
IP-pinned connection, TLS, streaming limit, and deadline enforcement that make
the boundary safe.

## OpenTelemetry Metadata Policy

ExMCP can carry W3C trace-context values in the MCP `_meta` object without
taking a dependency on an OpenTelemetry SDK or mutating process-global tracing
state. `traceparent` and `tracestate` are validated at every client and server
metadata boundary. Baggage is validated and bounded before filtering, then only
explicitly allowlisted members are retained. The default baggage allowlist is
empty, so baggage is dropped unless the application opts in.

```elixir
config :ex_mcp, :otel_meta,
  baggage_allowlist: ["tenant.id", "request-id"],
  max_total_bytes: 9_216,
  max_baggage_bytes: 8_192,
  max_baggage_members: 64
```

The fixed `tracestate` limits are 512 bytes and 32 unique members. Configured
byte limits cannot exceed 65,536 bytes, and baggage member/allowlist counts
cannot exceed 64. Invalid configuration or malformed metadata fails closed.
ExMCP currently accepts the W3C version `00` `traceparent` wire format; values
must use lowercase hexadecimal and non-zero trace and parent identifiers.

Attach a connection-level context to all modern client requests:

```elixir
ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com/mcp",
  trace_context: %{
    traceparent: "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
    tracestate: "vendor=opaque",
    baggage: "tenant.id=acme,secret=discarded"
  }
)
```

Per-request values may also be supplied in the request's `_meta`; the explicit
client `:trace_context` wins when both sources contain the same field. On the
server, handlers receive the sanitized map as
`ExMCP.Server.RequestContext.trace_context`. Notification and result metadata
go through the same policy.

Allowlist only low-cardinality routing or correlation fields. Do not propagate
credentials, authorization tokens, personal data, or other secrets as baggage.

## Tasks Extension

Modern Tasks is an explicit extension, not the experimental `tasks` capability
from MCP 2025-11-25. A modern client opts in on every request by adding
`io.modelcontextprotocol/tasks` to its configured capabilities:

```elixir
task_capabilities =
  ExMCP.Tasks.Extension.put_capability(%{
    "elicitation" => %{"form" => %{}}
  })

ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com/mcp",
  capabilities: task_capabilities
)
```

After a `tools/call` returns `resultType: "task"`, use
`ExMCP.Client.get_task/3`, `update_task/4`, and `cancel_task/3`. The client
rejects task results unless the extension was configured, and validates the
task handle before returning it to application code.

A server must advertise the same extension from `server/discover` only when it
has configured an appropriate task store. The bundled node-local store is
enabled in a Handler with `tasks: :store`:

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler, tasks: :store

  def __server_capabilities__ do
    ExMCP.Tasks.Extension.put_capability(%{"tools" => %{}})
  end

  @impl ExMCP.Server.Handler
  def handle_call_tool("long_deploy", arguments, state) do
    ExMCP.Tasks.Server.create(
      "long_deploy",
      arguments,
      state,
      __task_store_options__()
    )
  end
end
```

The injected modern `handle_task_get/2`, `handle_task_update/3`, and
`handle_task_cancel/2` callbacks use `ExMCP.Tasks`. Existing callbacks remain
overridable, and legacy task methods are unchanged unless the application
implements them explicitly. `ExMCP.Tasks.Server.create/4` inserts the task
synchronously and returns a handle only after `tasks/get` can read it.

`ExMCP.Tasks.Store.ETS` is bounded and atomic on one node. It keeps tasks
through client reconnects, client restarts, request-process failures, and
worker failures, but not an ExMCP application or node restart. Production
deployments that need that stronger guarantee should implement the
`ExMCP.Tasks.Store` behaviour, supervise the backend in their application, and
configure it globally or on the Handler:

```elixir
# Optional limits for the bundled reference store:
config :ex_mcp, ExMCP.Tasks.Store.ETS,
  max_tasks: 10_000,
  max_ttl_ms: 2_592_000_000

config :ex_mcp, task_store: MyApp.Tasks.PostgresStore

# Or for one server module:
use ExMCP.Server.Handler,
  tasks: :store,
  task_store: MyApp.Tasks.PostgresStore,
  task_store_opts: [repo: MyApp.Repo]
```

The store binds each task to the current request's principal, tenant, and
endpoint. Workers running outside a request callback must retain that owner
without credentials and pass it back explicitly:

```elixir
owner = ExMCP.Tasks.owner()
{:ok, task} = ExMCP.Tasks.complete(task_id, result, owner: owner)
```

Successful creates and wire-visible transitions publish full
`notifications/tasks` state to matching `subscriptions/listen` streams. A
deployment using a non-default subscription registry should pass
`subscription_registry: registry` when a worker calls `ExMCP.Tasks.complete/3`,
`fail/3`, `require_input/3`, `mark_cancelled/2`, or `put_status_message/3`.
Set `notify: false` only when the host application deliberately owns
publication itself.

The host application still owns worker execution and recovery. Store adapters
own persistence, atomicity across serving nodes, authorization binding, and
expiry. Do not advertise the extension when the configured store cannot meet
the deployment's durability requirements.

`ExMCP.Tasks.Task.to_map/1` retains the legacy 2025-11-25 keys.
`to_map/2` with `:modern` or `"2026-07-28"` emits `ttlMs`,
`pollIntervalMs`, `inputRequests`, and `error` without removing the public 1.x
struct aliases. `tasks/list`, `tasks/result`, and
`notifications/tasks/status` remain legacy-only.

## Client Configuration

You can pass options directly to `ExMCP.Client.start_link/1`:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
    protocol_mode: :prefer_modern,
    use_sse: true,
    request_timeout: 30_000
  )
```

Or build a reusable config with `ExMCP.ClientConfig`:

```elixir
config =
  ExMCP.ClientConfig.new(:production)
  |> ExMCP.ClientConfig.put_transport(:http, url: "https://api.example.com/mcp")
  |> ExMCP.ClientConfig.put_auth(:bearer, token: System.fetch_env!("MCP_TOKEN"))
  |> ExMCP.ClientConfig.put_retry_policy(max_attempts: 3, base_interval: 500)

{:ok, client} = ExMCP.connect(config)
```

## stdio

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :stdio,
    command: ["node", "server.js"],
    protocol_mode: :prefer_modern,
    cd: "/path/to/project",
    env: [{"NODE_ENV", "production"}],
    timeout: 30_000
  )
```

Supported options:

- `:command`
- `:cd`
- `:env`
- `:timeout`

## Streamable HTTP

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
    protocol_mode: :prefer_modern,
    use_sse: true,
    headers: [{"Authorization", "Bearer #{token}"}],
    request_timeout: 30_000,
    stream_handshake_timeout: 15_000,
    stream_idle_timeout: 60_000,
    max_retry_delay: 60_000
  )
```

Supported options include:

- `:url`
- `:endpoint`
- `:headers`
- `:protocol_mode`
- `:use_sse`
- `:session_id`
- `:protocol_version`
- `:timeout`
- `:request_timeout`
- `:stream_handshake_timeout`
- `:stream_idle_timeout`
- `:max_retry_delay`
- `:security`
- `:auth`
- `:auth_provider`

`use_sse` controls the legacy standalone GET stream. It may remain `true` on a
dual-era client: once `server/discover` succeeds, ExMCP disables that stream,
clears legacy session state, and uses JSON or POST-owned SSE for each modern
request. `subscriptions/listen` opens its own POST response stream.

## BEAM-Local

```elixir
{:ok, server} = MyServer.start_link(transport: :beam)  # works when using DSL; otherwise use HandlerServer.start_link(handler: MyServer, ...)

{:ok, client} =
  ExMCP.Client.start_link(
    transport: :beam,
    server: server,
    timeout: 5_000
  )
```

`transport: :beam` is local to the current VM and requires a server PID. Keep any
pooling, service discovery, or process selection in your application layer.

## Server Configuration

Servers (DSL or raw handlers) can be started with:

```elixir
MyServer.start_link(transport: :beam, protocol_mode: :prefer_legacy)
MyServer.start_link(transport: :stdio, protocol_mode: :prefer_legacy)
MyServer.start_link(transport: :http, port: 4000, protocol_mode: :prefer_legacy)

# For a raw handler module (no DSL):
ExMCP.Server.HandlerServer.start_link(handler: MyHandler, transport: :beam)
# or the convenience:
ExMCP.start_server(handler: MyHandler, transport: :stdio)
```

Phoenix/Plug applications usually mount `ExMCP.HttpPlug`:

```elixir
forward "/mcp", ExMCP.HttpPlug,
  handler: MyApp.MCPServer,
  server_info: %{name: "my-app", version: "1.0.0"},
  protocol_mode: :prefer_legacy,
  handler_call_timeout: 10_000,
  cors_enabled: true
```

`:handler_call_timeout` is the server-side deadline for each call from
`ExMCP.HttpPlug` into the Handler process (default `10_000` milliseconds).
It is separate from client-side `:timeout`, `:request_timeout`,
`:stream_handshake_timeout`, and `:stream_idle_timeout` settings.

The MCP 2024-11-05 HTTP+SSE transport is deprecated and disabled by default.
Existing servers may retain it during ExMCP 1.x with
`legacy_http_sse: true`. `sse_enabled: true` remains an rc.5-compatible alias
until ExMCP 2.0. Optional `legacy_http_sse_path` and
`legacy_http_sse_post_path` settings default to `/sse` and `/message`.
Neither dual-era preference mode enables this transport. `:modern_only`
disables it even when the compatibility option or its rc.5 alias is present.

## Multi Round-Trip Requests (MCP 2026-07-28)

MRTR lets `tools/call`, `resources/read`, and `prompts/get` pause for client
elicitation, sampling, or roots input. Configure a runtime AES-256 key ring and
declare `mrtr: true` so server startup validates it:

```elixir
# runtime.exs — load the secret from your runtime secret manager/environment.
key = System.fetch_env!("MCP_REQUEST_STATE_KEY") |> Base.decode64!()

config :ex_mcp, :request_state,
  active_key_id: "2026-08",
  keys: %{"2026-08" => key},
  ttl_seconds: 300,
  max_ttl_seconds: 900,
  clock_skew_seconds: 30
```

For a rolling rotation, first distribute `%{"old" => old_key, "new" =>
new_key}` to every node with `active_key_id: "old"`; then roll only
`active_key_id` to `"new"`; finally remove `"old"` after the maximum token TTL
plus clock skew. Install each complete key-ring snapshot atomically. Use
`revoked_key_ids: ["old"]` for emergency invalidation, accepting that any
in-flight token sealed by that key must restart.

```elixir
MyServer.start_link(
  transport: :stdio,
  protocol_mode: :modern_only,
  mrtr: true
)
```

Handlers can return either MRTR tuple, or use the DSL builder:

```elixir
{:input_required, input_requests, state}
{:input_required, input_requests, application_request_state, state}

ToolResult.input_required(input_requests, %{"workflowStep" => 1})
```

On the retry, unchanged callback arities read verified data from
`ExMCP.Server.Context.input_responses/0` and
`ExMCP.Server.Context.request_state/0`. Application request state must be JSON
encodable and is size-bounded before encryption.

Client operation options default to 8 rounds, 16 input requests per round, and
1 MiB of serialized MRTR input/output. Override them with
`:max_mrtr_rounds`, `:max_input_requests`, and `:max_mrtr_bytes`. One overall
`:timeout` covers all rounds.

Input callbacks run sequentially in deterministic request-ID order by default.
A stateless client handler can explicitly opt into bounded parallel dispatch by
implementing `mrtr_input_concurrency/0` and returning an integer from 2 through
16. Every parallel callback receives the same handler state and must return it
unchanged; ExMCP rejects a parallel callback that attempts to update the state.

For resumptions that may cause side effects, enable atomic single-use
enforcement:

```elixir
MyServer.start_link(
  mrtr: true,
  replay_cache: ExMCP.Server.ReplayCache.ETS,
  require_replay_protection: true
)
```

The bundled cache is node-local. Clustered deployments must implement
`ExMCP.Server.ReplayCache` over a shared, strongly consistent store. Without a
replay cache, verified retry context explicitly reports
`delivery_semantics: :at_least_once`.

HTTP deployments may provide `:principal_id` and `:tenant_id` as strings or
resolver functions. OAuth token `sub` and `tenant_id` claims are used by
default when available; these identities are authenticated into the sealed
state without embedding bearer tokens.

## Modern subscriptions (MCP 2026-07-28)

Open an immutable notification stream with `ExMCP.Client.listen/3`. The call
returns only after `notifications/subscriptions/acknowledged`; events are sent
to the subscribing process with the acknowledged subscription reference:

```elixir
{:ok, subscription} =
  ExMCP.Client.listen(client, %{
    "toolsListChanged" => true,
    "resourceSubscriptions" => ["file:///project/config.json"],
    "taskIds" => [task_id]
  })

receive do
  {:ex_mcp_subscription, ^subscription, method, params} ->
    handle_notification(method, params)
end

:ok = ExMCP.Client.Subscription.cancel(subscription)
```

`taskIds` is defined by the `io.modelcontextprotocol/tasks` extension. The
client must declare that extension in its configured capabilities. Servers
using `tasks: :store` automatically authorize every requested ID against the
same principal, tenant, endpoint, and task store used by `tasks/get`; IDs that
are missing or not authorized are omitted from the acknowledged filter. A
server with a custom task backend must provide
`:authorize_subscription_filter` and must not acknowledge an ID until it has
performed the equivalent access check.

`subscribe_resource/3` and `unsubscribe_resource/3` retain their legacy RPC
behavior before 2026-07-28. On a modern connection they maintain one
ref-counted desired URI set. Changes open and acknowledge an immutable
replacement stream before cancelling the old stream; only the committed
subscription ID delivers compatibility events:

```elixir
{:ok, _subscription} = ExMCP.Client.subscribe_resource(client, uri)

receive do
  {:ex_mcp_resource_updated, ^uri, params} -> handle_update(params)
end
```

After reconnect, subscriptions are opened with fresh JSON-RPC IDs. ExMCP
refetches each affected list, resource, and task, then emits
`{:ex_mcp_subscription_resync, subscription, {:complete, snapshot}}` for a
generic subscription or `{:ex_mcp_resource_resync, subscription, snapshot}`
for the resource compatibility wrapper before releasing queued events.

Server listener defaults are 1,000 global registrations, 100 per principal,
500 per tenant, 100 queued events per listener, a one-hour maximum lifetime,
256 resource URIs, 256 task IDs, and a 64 KiB filter. Configure the registry
child or pass
the corresponding server options (`:subscription_max_queue`,
`:subscription_max_lifetime_ms`, `:authorize_subscription_filter`, and
`:authorize_subscription_publication`). Publication authorization is checked
again for every event; denial gracefully closes the stream.

For clustered HTTP, start one named subscription registry per node after the
application's PubSub process and route every MCP server on that node to it:

```elixir
children = [
  {Phoenix.PubSub, name: MyApp.PubSub},
  {ExMCP.Server.Subscriptions,
   name: MyApp.MCPSubscriptions,
   adapter:
     {ExMCP.Server.Subscriptions.PubSub,
      pubsub_server: MyApp.PubSub,
      topic: "my_app:mcp:subscriptions:v1"}},
  {MyApp.MCPServer,
   subscription_registry: MyApp.MCPSubscriptions}
]
```

`ExMCP.Server.Subscriptions.PubSub` has no hard Phoenix dependency. Its
`:pubsub_module` defaults to `Phoenix.PubSub` and may be replaced by any module
implementing `subscribe/2` and `broadcast_from/4`. Registrations and listener
processes stay node-local; untargeted publications fan out and each receiving
listener rechecks authorization. Publications carrying a `:transport_ref`
stay on the owning node. `publish/3` counts describe synchronous work in the
called registry, not eventual work on peers.

The bundled ETS storage makes global/principal/tenant limits per-node. If a
deployment requires cluster-wide quotas, supply a storage adapter backed by a
shared, atomic data store via the PubSub adapter's `:storage_adapter` option.
That adapter must still return only entries whose listener processes belong to
the current registry; use the shared store for reservation/accounting rather
than attempting to call remote listener PIDs as local registrations.

Over modern Streamable HTTP, each `subscriptions/listen` call is a dedicated
POST response stream. Cancelling `ExMCP.Client.Subscription` closes that HTTP
response; it does not POST `notifications/cancelled`. An unexpected response
close opens a new listen request with a fresh JSON-RPC ID and runs the resync
flow described above. The server sends an SSE comment keepalive every 15
seconds by default so quiet disconnects are detected and intermediaries do not
expire an otherwise healthy stream:

```elixir
forward "/mcp", ExMCP.HttpPlug,
  handler: MyApp.MCPServer,
  protocol_mode: :modern_only,
  subscription_keepalive_interval_ms: 15_000,
  subscription_max_lifetime_ms: :timer.hours(1)
```

Set `:subscription_keepalive_interval_ms` to a positive integer or
`:infinity`. Disabling keepalives delays detection of a quiet peer disconnect
until the next notification or server-initiated closure.

## Modern Streamable HTTP headers (MCP 2026-07-28)

After a connection settles on MCP 2026-07-28, the HTTP client is stateless:
it neither sends nor retains `Mcp-Session-Id` or `Last-Event-ID`. Every POST
mirrors the body protocol version and method into `MCP-Protocol-Version` and
`Mcp-Method`; `tools/call`, `resources/read`, and `prompts/get` also send
`Mcp-Name`. Unsafe UTF-8, leading/trailing whitespace, control characters,
and values shaped like the Base64 sentinel are encoded automatically.

Tool input properties may opt into routing headers:

```elixir
%{
  "type" => "object",
  "properties" => %{
    "region" => %{
      "type" => "string",
      "x-mcp-header" => "Region"
    }
  }
}
```

After `tools/list`, a modern HTTP `tools/call` mirrors a present non-null
argument as `Mcp-Param-Region`. String, integer, and boolean properties are
supported, including nested property paths. Invalid, duplicate, unreachable,
or unsupported annotations cause the server to omit that tool from a modern
list response. On a `-32020` header mismatch the client refreshes `tools/list`
and retries the tool call exactly once inside the original timeout.

The ExMCP DSL returns its complete tool set and therefore needs no cursor
coordination. A raw handler that paginates a dynamic tool set must filter and
sort the full source collection before it slices the requested page:

```elixir
def handle_list_tools(cursor, state) do
  tools =
    state.dynamic_tools
    |> ExMCP.Server.ResultNormalizer.prepare_tools_list()

  {page, next_cursor} = MyApp.Cursor.page(tools, cursor)
  {:ok, page, next_cursor, state}
end
```

Result normalization repeats this validation as a defensive boundary, but it
cannot correct a handler-owned cursor calculated from invalid definitions.

The server validates standard and annotated headers against the body before
tool dispatch. Custom raw `Mcp-Method`, `Mcp-Name`, `Mcp-Session-Id`,
`Last-Event-ID`, and `Mcp-Param-*` values supplied through the client's
`:headers` option are removed and replaced by protocol-derived values on
modern requests.

Treat all `Mcp-Param-*` values as sensitive routing data. ExMCP does not attach
raw request headers to its Plug/client debug logs or telemetry. Configure
reverse proxies, load balancers, APM agents, and access-log middleware to
redact `Mcp-Param-*` just as they redact `Authorization` and cookies; those
systems observe headers before ExMCP can sanitize their logs.

### Modern result cache hints

MCP 2026-07-28 requires `ttlMs` and `cacheScope` on complete results from
`server/discover`, `tools/list`, `prompts/list`, `resources/list`,
`resources/templates/list`, and `resources/read`. ExMCP supplies conservative
defaults when a handler omits them:

```elixir
%{
  ttlMs: 0,
  cacheScope: :private
}
```

`ttlMs: 0` means immediately stale; `private` prevents reuse across
authorization contexts. A handler may return `ttl_ms` / `cache_scope` or the
wire keys `ttlMs` / `cacheScope` to override those defaults. TTL must be a
non-negative integer and scope must be `:public`, `:private`, `"public"`, or
`"private"`. Only use `public` when the result is safe to share across users,
including on authenticated endpoints. Each paginated response page carries
its own hints, and ExMCP removes cache hints from `input_required` results.

Modern clients reject missing or invalid required hints. With the default
`:struct` response format they are available as `response.ttlMs` and
`response.cacheScope`; `format: :map` preserves the wire keys. ExMCP currently
parses and validates these hints but does not store or reuse responses.

Pass request-local context into a handler with `:handler_opts`. The option can
be a static term, a one-arity function called with the `Plug.Conn`, a two-arity
function called with the `Plug.Conn` and decoded JSON-RPC request, or an MFA
tuple called as `apply(module, function, [conn, request | extra_args])`.

```elixir
forward "/mcp", ExMCP.HttpPlug,
  handler: MyApp.MCPServer,
  handler_opts: fn conn ->
    [current_user: conn.assigns[:current_user]]
  end,
  server_info: %{name: "my-app", version: "1.0.0"}
```

## Resilience

Retries:

```elixir
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
  retry_policy: [max_attempts: 3, initial_delay: 100, max_delay: 2_000]
)
```

Circuit breaker and health checks:

```elixir
ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com/mcp",
  reliability: [
    circuit_breaker: [failure_threshold: 5, reset_timeout: 30_000],
    health_check: [check_interval: 60_000]
  ]
)
```

## Logging

This section configures application/runtime logging. The MCP wire-level Logging
feature (`logging/setLevel`, per-request log levels, and
`notifications/message`) is deprecated as of MCP 2026-07-28 but remains
available throughout ExMCP 1.x. New observability integrations should use
stderr for stdio diagnostics or OpenTelemetry for structured telemetry.

For stdio servers, stdout must contain only JSON-RPC messages. ExMCP configures
stdio logging when stdio mode starts.
Send ad hoc diagnostics to stderr:

```elixir
IO.puts(:stderr, "debug")
```

For HTTP and BEAM-local development:

```elixir
Logger.configure(level: :debug)
```

## Security

HTTP clients can use headers:

```elixir
ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com/mcp",
  headers: [{"Authorization", "Bearer #{token}"}]
)
```

For server-side HTTP concerns, compose Plug/Phoenix pipelines before
`ExMCP.HttpPlug`.
