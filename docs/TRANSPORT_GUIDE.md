# ExMCP Transport Guide

ExMCP supports stdio, streamable HTTP/SSE, BEAM-local, and test transports.

## Overview

| Transport | Identifier | Best For |
|-----------|------------|----------|
| stdio | `:stdio` | Official MCP subprocess transport |
| Streamable HTTP/SSE | `:http` | Remote servers and Phoenix apps |
| BEAM-local | `:beam` | Local Elixir client/server pairs |
| Test | `:test` | In-memory tests |

## stdio

The stdio transport spawns an external MCP server process and exchanges
newline-delimited JSON-RPC.

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :stdio,
    command: ["node", "server.js"],
    cd: "/path/to/project",
    env: [{"NODE_ENV", "production"}]
  )
```

Supported options:

- `:command` - executable plus arguments as a list.
- `:cd` - subprocess working directory.
- `:env` - environment variables as `{"KEY", "VALUE"}` tuples.
- `:timeout` - client operation timeout.

## Streamable HTTP/SSE

The HTTP transport sends JSON-RPC over HTTP POST. With `use_sse: true`, it also
uses SSE for server-to-client streaming.

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
    use_sse: true,
    headers: [{"Authorization", "Bearer #{token}"}],
    request_timeout: 30_000,
    stream_handshake_timeout: 15_000,
    stream_idle_timeout: 60_000
  )
```

Supported client options include:

- `:url` - base URL or full MCP endpoint URL.
- `:endpoint` - endpoint path when it is not included in `url`.
- `:headers` - additional request headers.
- `:use_sse` - enable SSE response stream, defaults to `true`.
- `:session_id` - resume an existing streamable HTTP session.
- `:protocol_version` - requested MCP protocol version.
- `:timeout` - connect timeout.
- `:request_timeout` - single request timeout.
- `:stream_handshake_timeout` - wait for SSE stream startup.
- `:stream_idle_timeout` - allowed SSE idle time.
- `:max_retry_delay` - cap for SSE reconnect delay.
- `:security` - client-side security validation configuration.
- `:auth` / `:auth_provider` - OAuth/auth provider integration.

### Phoenix/Plug Server

```elixir
scope "/mcp" do
  pipe_through [:api, :mcp_auth]

  forward "/", ExMCP.HttpPlug,
    handler: MyApp.MCPServer,
    server_info: %{name: "my-app", version: "1.0.0"},
    sse_enabled: true,
    cors_enabled: true
end
```

Put HTTP concerns in Plug pipelines before `ExMCP.HttpPlug`: authentication,
request signing, rate limiting, CORS/origin decisions, and DNS rebinding checks.

## BEAM-Local

The BEAM-local transport carries MCP-shaped maps/lists as Elixir terms between
local processes. It does not JSON encode/decode in the transport, but it still
uses MCP initialize, request IDs, capabilities, and handler callbacks.

```elixir
{:ok, server} = MyServer.start_link(transport: :beam)  # DSL modules provide start_link/1; for raw handlers use HandlerServer or ExMCP.start_server

{:ok, client} =
  ExMCP.Client.start_link(
    transport: :beam,
    server: server
  )
```

Supported options:

- server side: `transport: :beam` on a DSL/handler server.
- client side: `transport: :beam`, `server: pid`, optional `timeout`.

**Tip:** For a fast local verification of these BEAM + DSL + Client patterns (no re-installs), run `mix examples.getting_started` from the project root after `mix compile`.

BEAM-local does not provide service discovery or distributed registry behavior.
If you need a pool or registry of server processes, keep that in your
application supervision layer and pass the selected server PID to the client.

## Test Transport

Use `:test` for in-memory tests where both endpoints are in the same process
tree:

```elixir
{:ok, server} =
  ExMCP.Server.HandlerServer.start_link(
    transport: :test,
    handler: MyServer
  )

{:ok, client} =
  ExMCP.Client.start_link(
    transport: :test,
    server: server
  )
```

## Reliability

Client retries:

```elixir
ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com/mcp",
  retry_policy: [max_attempts: 3, initial_delay: 100, max_delay: 2_000]
)
```

Transport wrapper:

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

## Telemetry

Transports emit connection and message telemetry. BEAM-local events use the
generic transport event names with `metadata.transport == :beam`:

- `[:ex_mcp, :transport, :connection, :opened]`
- `[:ex_mcp, :transport, :message, :sent]`
- `[:ex_mcp, :transport, :message, :received]`

## Selection Guide

- Use `:stdio` for official subprocess MCP servers.
- Use `:http` for network boundaries and Phoenix integrations.
- Use `:beam` for trusted local Elixir processes.
- Use `:test` for tests.
