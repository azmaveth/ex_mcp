# ExMCP Security Guide

Security is handled at the protocol edge: HTTP clients and Plug/Phoenix servers
use authentication, TLS, origin checks, and CORS; stdio relies on subprocess
isolation; BEAM-local relies on local process ownership and application-level
authorization.

Outbound requests additionally pass through a trust boundary that strips
credentials from, and requires consent for, origins the application has not
declared. Its defaults are fail-closed, so read
[Outbound Requests](#outbound-requests-trusted-origins-and-consent) before
pointing a client at a remote server.

## Security Matrix

| Feature | Streamable HTTP | stdio | BEAM-local (`:beam`) |
|---------|-----------------|-------|----------------------|
| Bearer/custom headers | Yes | No | App-level metadata/handler logic |
| OAuth 2.1 flows | Yes | No | No |
| TLS | Yes | No | Only through distributed Erlang if you add it |
| Origin/CORS checks | Yes | No | Not applicable |
| DNS rebinding protection | Origin + Host allow-lists (`:allowed_origins`, `:allowed_hosts`, `ExMCP.Plugs.DnsRebinding`) | Not applicable | Not applicable |
| Outbound origin trust (`SecurityGuard`) | Yes | Yes, for `resources/*` URIs | Not applicable |
| Process isolation | Server process | Subprocess | Local BEAM process |

## Outbound Requests: Trusted Origins and Consent

`ExMCP.Transport.SecurityGuard` runs on every outbound HTTP POST — the
JSON-RPC channel — and on the URIs of `resources/read` / `resources/list`
requests sent over stdio. It classifies the target URL against
`:trusted_origins` and, for anything that is **not** trusted:

1. removes credential headers (`authorization`, `cookie`, `x-api-key`,
   `x-auth-token`, `x-csrf-token`) so a token issued for one origin is never
   passed through to another, and
2. asks the configured `:consent_handler` to approve the origin, caching the
   decision until it expires.

**The defaults are fail-closed and this bites first-time users.**
`:trusted_origins` is loopback-only and `:consent_handler` is
`ExMCP.ConsentHandler.Deny`, so a client pointed at a server that is not on
localhost has its `Authorization` header stripped and the request denied with
`consent_denied`. Declare the servers your application talks to:

```elixir
config :ex_mcp, :security,
  trusted_origins: ["https://mcp.example.com"]
```

A trusted origin is exempt from *both* checks — it is never stripped and never
prompts for consent. Consent then applies only to origins your application did
not declare. Prefer this over disabling a control. The SecurityGuard logs the
exact setting to add whenever it strips credentials or blocks a request, so
this failure mode is loud rather than silent.

| Setting | Default | Effect |
|---------|---------|--------|
| `:trusted_origins` | `["localhost", "127.0.0.1", "::1"]` | Same security domain. `"*.example.com"` matches subdomains. |
| `:consent_handler` | `ExMCP.ConsentHandler.Deny` | Consulted for untrusted origins. `CLI` prompts; `Web` defers to an out-of-band flow. |
| `:consent_ttl` | 24 hours (milliseconds) | Lifetime of a cached consent decision. |
| `:enable_token_passthrough_prevention` | `true` | Set `false` to forward credentials to untrusted origins. |
| `:enable_user_consent_validation` | `true` | Set `false` to skip the consent handler entirely. |

Every consent decision path fails closed: a denial, an error, an
unrecognised handler return value, or an expiry that cannot be interpreted all
block the request.

### Writing a consent handler

`ExMCP.ConsentHandler` implementations return an expiry. Use one of the
explicit forms — `DateTime`, `{:ttl, seconds}`, `{:unix, seconds}`, or
`{:monotonic, seconds}`:

```elixir
def request_consent(_user_id, _origin, context) do
  {:ok, {:ttl, Map.get(context, :consent_ttl, 3600)}}
end
```

A bare integer is still read as `System.monotonic_time(:second)` for backwards
compatibility. Returning Unix epoch seconds as a bare integer is the easy
mistake — it would otherwise grant consent for decades — so implausible values
(already past, or more than 365 days out) are rejected and the request fails.

## HTTP Client Security

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
    headers: [{"Authorization", "Bearer #{token}"}],
    security: %{
      validate_origin: true,
      allowed_origins: ["https://app.example.com"],
      tls: %{verify: :verify_peer}
    }
  )
```

For OAuth flows, use the authorization modules or `:auth` / `:auth_provider`
options on the HTTP transport.

### TLS

HTTPS connections verify the peer against the OS trust store with TLS 1.2/1.3
and HTTPS hostname matching by default. `tls: %{verify: :verify_none}` is
accepted for local development against self-signed certificates, but it makes
the connection unauthenticated — encrypted, yet open to an active
man-in-the-middle — and ExMCP logs a warning whenever it is configured.

## HTTP Server Security

Use Plug/Phoenix pipelines for server-side concerns:

```elixir
pipeline :mcp do
  plug ExMCP.Plugs.DnsRebinding
  plug MyApp.VerifyRequestSignature
  plug MyApp.RequireMCPToken
end

scope "/mcp" do
  pipe_through :mcp

  forward "/", ExMCP.HttpPlug,
    handler: MyApp.MCPServer,
    server_info: %{name: "my-app", version: "1.0.0"},
    sse_enabled: true,
    cors_enabled: true
end
```

Keep request authentication and authorization at the HTTP edge. Keep
tool/resource authorization in handler code when it depends on the specific
tool, resource URI, user, tenant, or project.

### MRTR request state

Modern multi-round requests use an ExMCP-owned, versioned AES-256-GCM envelope.
The authenticated payload binds the immutable request digest, expected input
IDs, round, protocol version, endpoint, capability fingerprint, principal, and
tenant. It contains only bounded JSON application state—never bearer tokens or
runtime secrets.

Load the 32-byte key ring at runtime and retain decrypt-only old keys for at
least the maximum token TTL plus clock skew. `active_key_id` selects the only
encryption key; `revoked_key_ids` immediately prevents use of a compromised
key. Every node that may resume a request needs the same ring.

AEAD prevents tampering but not replay. Side-effecting resumptions should set
`require_replay_protection: true` and configure a shared replay-cache adapter.
`ExMCP.Server.ReplayCache.ETS` is atomic but node-local and is therefore only
appropriate for single-node deployments. Without an adapter, handlers must
treat `RequestContext.delivery_semantics == :at_least_once` accordingly.

### Modern subscription streams

Subscription registrations store stable principal and tenant identifiers,
never bearer tokens. Filter authorization runs before acknowledgment and
publication authorization runs again for every event. Configure a bounded
maximum stream lifetime so credentials are periodically re-evaluated, and use
a cluster-aware adapter with PubSub fan-out for multi-node HTTP deployments.
The bundled ETS adapter and listener supervisor are node-local. Modern HTTP
streams treat an unsuccessful response chunk as cancellation, immediately
remove the listener registration, and send no further events. Periodic SSE
comment keepalives make this cleanup happen even when no application events
are available to expose a disconnected peer.

### DNS rebinding protection

Protection is Host-allow-list based and is **on by default for localhost
servers**, which are the prime rebinding target. `ExMCP.HttpPlug` provides
three complementary controls:

- **Host allow-list** (`:allowed_hosts`): requests whose `Host` header is not
  listed are rejected with `421` before any routing or handler work. Ports are
  ignored and IPv6 hosts match with or without brackets (`[::1]:8080` matches
  `"[::1]"` and `"::1"`). Servers started through `ExMCP.Server.Transport`
  with a localhost bind get `["localhost", "127.0.0.1", "[::1]", "::1"]`
  automatically; an explicit `:allowed_hosts` always wins. When you mount
  `ExMCP.HttpPlug` yourself — in a Phoenix `forward`, say — set
  `:allowed_hosts` explicitly to the hostnames the server is reachable under
  rather than relying on the default.
- **Origin allow-list** (`:validate_origin`, default `true`, plus
  `:allowed_origins`): requests that carry an `Origin` header are rejected
  with `403` unless the origin is listed (or `:allowed_origins` is `:any`).
  Requests *without* an `Origin` header are allowed, because non-browser
  clients do not send one — so the Origin check alone is not rebinding
  protection, and the Host allow-list is what closes that gap. There is no
  "same origin as the Host header" fallback: under DNS rebinding the Host
  header is attacker-controlled, so such a comparison would always pass.
- **`ExMCP.Plugs.DnsRebinding`**: a standalone plug for Phoenix/Plug
  pipelines that enforces a Host allow-list (default: loopback names only)
  in front of any downstream plugs.

Session ids supplied via `mcp-session-id` / legacy `x-session-id` headers are
validated (max 128 bytes, `A-Za-z0-9._~+/=-`) and malformed values are
rejected with `400` without being echoed back.

## stdio Security

stdio is appropriate when the MCP server process is trusted by the application
that launches it.

Best practices:

- Use absolute commands or controlled PATHs for production.
- Set `cd` and `env` explicitly.
- Do not log to stdout; stdout is protocol traffic.
- Run subprocesses with the least privileges needed.
- Validate tool arguments before touching filesystem or network resources.

## BEAM-Local Security

`transport: :beam` is for trusted local Elixir processes:

```elixir
{:ok, server} = MyServer.start_link(transport: :beam)   # requires use of DSL, or use HandlerServer
{:ok, client} = ExMCP.Client.start_link(transport: :beam, server: server)
```

There is no wire-level authentication inside a single VM. Enforce access with
normal application structure: supervision boundaries, process ownership, handler
authorization, and explicit context in tool arguments or handler state.

If you cross Erlang nodes in your own application, secure distributed Erlang
separately with strong cookies, private networks, and TLS distribution where
appropriate.

## Validation

Use public validation helpers where available:

```elixir
ExMCP.Security.Validation.validate_config(security_config)
```

Use `ExMCP.Content.Validation` and handler-side schema checks for tool/resource
input validation.

### Verifying JWTs

`ExMCP.Authorization.JWT.verify/2` checks the **signature only** — an expired
token verifies fine. Use `verify_and_validate/3` (or `validate_claims/2`) for
anything that makes an authorization decision:

```elixir
{:ok, claims} =
  ExMCP.Authorization.JWT.verify_and_validate(token, jwks,
    iss: "https://auth.example.com",
    aud: "https://mcp.example.com"
  )
```

`exp` is required and must be numeric; `nbf` and `iat` must be numeric when
present. Time comparisons allow 30 seconds of clock skew, tunable with
`:leeway`. Pass `require_exp: false` only for a token profile that genuinely
has no expiry. Only asymmetric algorithms are accepted (RS/PS/ES) — `none` and
the HMAC family are rejected, so an attacker cannot swap the header's `alg`.

`iss` and `aud` are checked only when you supply the expected values; always
supply them when validating tokens from an identity provider.

### MCP routing-header confidentiality

Treat every `Mcp-Param-*` value as sensitive. These values mirror selected
tool arguments and may contain tenant, region, account, or routing data.
ExMCP does not include them in its HTTP debug logs or telemetry, but reverse
proxies, load balancers, APM agents, and access-log middleware may record
request headers independently. Configure those systems to redact
`Mcp-Param-*` with the same policy used for `Authorization` and cookies.

Modern HTTP servers compare protocol, method, name, and annotated parameter
headers with the JSON-RPC body before dispatch. Missing, duplicate, malformed,
oversized, or mismatched recognized headers return HTTP 400 with JSON-RPC
error `-32020`; the rejected header value is not copied into the response or
logs. Unsupported protocol-version errors may identify the requested version,
but `Mcp-Param-*` values are never echoed.

## Best Practices

- Use HTTPS in production.
- Prefer short-lived bearer tokens or OAuth flows.
- Do not send secrets through tool arguments unless the tool explicitly needs
  them.
- Validate resource URIs and file paths before access.
- Return safe error messages to clients; log sensitive details internally.
- Put coarse-grained HTTP checks in Plug pipelines and fine-grained MCP checks
  in handlers.
- Monitor telemetry for unusual request, auth, or transport failure patterns.

## Common Issues

**`{:security_violation, %ExMCP.Transport.SecurityError{type: :consent_denied}}`**

The server's origin is not in `:trusted_origins` and the default consent
handler denied it. Add the origin — see
[Outbound Requests](#outbound-requests-trusted-origins-and-consent).

**401/403 from HTTP server**

Check `headers`, `auth`, or `auth_provider` on the client and the server's Plug
auth pipeline. If the request never carried the credential at all, look for a
`SecurityGuard: removed credential headers` warning in the log: the target
origin is not trusted, so the `Authorization` header was stripped before the
request went out.

**CORS failure**

Configure the Phoenix/Plug pipeline or `ExMCP.HttpPlug` CORS options for the
browser origin.

**BEAM-local access control**

Pass explicit application context into the handler state or arguments and reject
unauthorized tool/resource calls inside the handler.
