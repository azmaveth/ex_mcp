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

## Protocol Version

ExMCP supports:

- `2026-07-28` (modern stateless protocol; opt-in during the RC soak)
- `2024-11-05`
- `2025-03-26`
- `2025-06-18`
- `2025-11-25` (legacy default during the RC soak)

The latest supported version is returned by `ExMCP.protocol_version/0`.

```elixir
config :ex_mcp,
  protocol_mode: :prefer_modern
```

Modes are `:modern_only`, `:legacy_only`, `:prefer_modern`, and
`:prefer_legacy`. The current RC defaults to `:legacy_only`; a later RC will
soak `:prefer_modern` before 1.0.

Validate versions with the public negotiator:

```elixir
ExMCP.Protocol.VersionNegotiator.supported?("2025-11-25")
```

## JSON Schema Resource Policy

Every JSON Schema compiled or validated by ExMCP passes through one bounded,
fail-closed policy. Only local fragment references (`#` and `#/...`) are
accepted. HTTP(S), file, and relative cross-document `$ref` values are rejected
before ExJsonSchema can resolve them, even if the host application configured
ExJsonSchema's global `:remote_schema_resolver`.

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

Network schema fetching is not available in ExMCP 1.0. It will remain disabled
until an opt-in resolver can enforce host allowlists, DNS/IP checks on every
redirect, proxy policy, response and decompression limits, recursion/cycle
limits, deadlines, and trust-partitioned caching as one complete boundary.

## Client Configuration

You can pass options directly to `ExMCP.Client.start_link/1`:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
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

## HTTP/SSE

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
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
MyServer.start_link(transport: :beam)                    # DSL modules get this
MyServer.start_link(transport: :stdio)
MyServer.start_link(transport: :http, port: 4000, sse_enabled: true)

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
  handler_call_timeout: 10_000,
  sse_enabled: true,
  cors_enabled: true
```

`:handler_call_timeout` is the server-side deadline for each call from
`ExMCP.HttpPlug` into the Handler process (default `10_000` milliseconds).
It is separate from client-side `:timeout`, `:request_timeout`,
`:stream_handshake_timeout`, and `:stream_idle_timeout` settings.

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
    "resourceSubscriptions" => ["file:///project/config.json"]
  })

receive do
  {:ex_mcp_subscription, ^subscription, method, params} ->
    handle_notification(method, params)
end

:ok = ExMCP.Client.Subscription.cancel(subscription)
```

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
refetches each affected list/resource, then emits
`{:ex_mcp_subscription_resync, subscription, {:complete, snapshot}}` for a
generic subscription or `{:ex_mcp_resource_resync, subscription, snapshot}`
for the resource compatibility wrapper before releasing queued events.

Server listener defaults are 1,000 global registrations, 100 per principal,
500 per tenant, 100 queued events per listener, a one-hour maximum lifetime,
256 resource URIs, and a 64 KiB filter. Configure the registry child or pass
the corresponding server options (`:subscription_max_queue`,
`:subscription_max_lifetime_ms`, `:authorize_subscription_filter`, and
`:authorize_subscription_publication`). Publication authorization is checked
again for every event; denial gracefully closes the stream.

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
