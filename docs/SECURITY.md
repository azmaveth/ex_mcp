# ExMCP Security Guide

Security is handled at the protocol edge: HTTP clients and Plug/Phoenix servers
use authentication, TLS, origin checks, and CORS; stdio relies on subprocess
isolation; BEAM-local relies on local process ownership and application-level
authorization.

## Security Matrix

| Feature | Streamable HTTP | stdio | BEAM-local (`:beam`) |
|---------|-----------------|-------|----------------------|
| Bearer/custom headers | Yes | No | App-level metadata/handler logic |
| OAuth 2.1 flows | Yes | No | No |
| TLS | Yes | No | Only through distributed Erlang if you add it |
| Origin/CORS checks | Yes | No | Not applicable |
| DNS rebinding protection | Plug/client security config | No | Not applicable |
| Process isolation | Server process | Subprocess | Local BEAM process |

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
{:ok, server} = MyServer.start_link(transport: :beam)
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

**401/403 from HTTP server**

Check `headers`, `auth`, or `auth_provider` on the client and the server's Plug
auth pipeline.

**CORS failure**

Configure the Phoenix/Plug pipeline or `ExMCP.HttpPlug` CORS options for the
browser origin.

**BEAM-local access control**

Pass explicit application context into the handler state or arguments and reject
unauthorized tool/resource calls inside the handler.
