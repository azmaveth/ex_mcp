# ExMCP Troubleshooting Guide

## stdio

### Unexpected end of JSON input

The stdio transport requires stdout to contain only newline-delimited JSON-RPC.
Avoid `IO.puts/1`, normal Logger output, or noisy startup scripts on stdout.

Use stderr for diagnostics:

```elixir
IO.puts(:stderr, "debug")
```

For scripts with `Mix.install/2`, configure logging before installing deps:

```elixir
Application.put_env(:ex_mcp, :stdio_mode, true)
Application.put_env(:logger, :level, :emergency)

Mix.install([{:ex_mcp, "~> 1.0.0-rc.1"}], verbose: false)
```

### Server hangs after starting

Start stdio servers with:

```elixir
MyServer.start_link(transport: :stdio)
```

For clients, `command` must be a list:

```elixir
ExMCP.Client.start_link(transport: :stdio, command: ["node", "server.js"])
```

## HTTP

### Connection refused

Check the URL and endpoint path. If the path is included in `url`, ExMCP uses
that as the default endpoint:

```elixir
ExMCP.Client.start_link(transport: :http, url: "http://localhost:4000/mcp")
```

Or provide it explicitly:

```elixir
ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:4000",
  endpoint: "/mcp"
)
```

### CORS errors

For Phoenix/Plug servers, configure CORS in your Plug pipeline or pass
`cors_enabled: true` to `ExMCP.HttpPlug`.

### SSE stream does not start

Confirm the server has SSE enabled and the client uses `use_sse: true`. Increase
`stream_handshake_timeout` for slow deployments.

## BEAM-Local

### Client cannot connect

`transport: :beam` requires a live server PID:

```elixir
{:ok, server} = MyServer.start_link(transport: :beam)  # DSL provides start_link; raw handlers use HandlerServer
Process.alive?(server)

{:ok, client} = ExMCP.Client.start_link(transport: :beam, server: server)
```

Do not use `transport: :native`; it was removed in the 1.0 API cleanup.

## DSL

### Tools do not appear

Use `ExMCP.Server.Handler` and `ExMCP.Server.DSL` together, and make sure the
server starts through a supported transport:

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL

  tool "ping", "Health check" do
    run fn _args, state ->
      {:ok, %{content: [%{type: "text", text: "pong"}]}, state}
    end
  end
end
```

## Debugging

Enable debug logging for non-stdio transports:

```elixir
Logger.configure(level: :debug)
```

Inspect local server state when using BEAM-local tests:

```elixir
:sys.get_state(server)
```

Run focused tests:

```bash
mix test test/ex_mcp/client_beam_transport_test.exs
mix test test/ex_mcp/server/transport_test.exs
```
