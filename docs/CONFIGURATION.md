# ExMCP Configuration Guide

This guide covers the supported configuration surfaces for ExMCP 1.0.

## Dependency

```elixir
def deps do
  [
    {:ex_mcp, "~> 1.0.0-rc.4"}
  ]
end
```

## Protocol Version

ExMCP supports:

- `2024-11-05`
- `2025-03-26`
- `2025-06-18`
- `2025-11-25` (latest stable; default)

> **Note:** Upstream is drafting MCP **2026-07-28** (breaking). That version is not
> negotiated by ExMCP until a future release after the final specification ships.

The latest supported version is returned by `ExMCP.protocol_version/0`.

```elixir
config :ex_mcp,
  protocol_version: "2025-11-25"
```

Validate versions with the public negotiator:

```elixir
ExMCP.Protocol.VersionNegotiator.valid_version?("2025-11-25")
```

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
  sse_enabled: true,
  cors_enabled: true
```

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
