# Getting Started With ExMCP

Start with:

- [QUICKSTART.md](QUICKSTART.md) for a minimal server and client
- [MIGRATION.md](MIGRATION.md) for breaking changes between versions
- [USER_GUIDE.md](../guides/USER_GUIDE.md) for the full MCP API

ExMCP supports MCP clients and servers over stdio, HTTP/SSE, and BEAM-local
transports, plus ACP controllers and agents.

## Current Server Shape

Use `ExMCP.Server.Handler` directly, optionally with the server DSL:

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "my-server", version: "1.0.0"

  tool "echo", "Echoes text" do
    param :message, :string, required: true
    run fn %{message: message}, state -> {:ok, message, state} end
  end
end
```

## Transports

- `:stdio` for subprocess JSON-RPC
- `:http` for Streamable HTTP, with `use_sse: true` for SSE
- `:beam` for local client/server processes in the same BEAM VM
- `:test` for in-memory tests

The old `ExMCP.Native` direct dispatcher and public `:native` transport alias
were removed before 1.0. Use `transport: :beam` with a server pid for BEAM-local
MCP.
