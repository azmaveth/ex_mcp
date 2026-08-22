# Getting Started With ExMCP

Start with:

- [QUICKSTART.md](QUICKSTART.md) for a minimal server and client
- [MIGRATION.md](MIGRATION.md) for breaking changes between versions
- [USER_GUIDE.md](../guides/USER_GUIDE.md) for the full MCP API

ExMCP supports MCP clients and servers over stdio, Streamable HTTP, and BEAM-local
transports, plus ACP controllers and agents.

MCP `2026-07-28` is the latest stable revision and is available through
`:prefer_modern` and `:modern_only` in `1.0.0`. Stable 1.0 defaults to
`:prefer_modern`; set `:legacy_only` to preserve the
legacy protocol era (not an exact rc.5 package rollback). See the
[Configuration Guide](../CONFIGURATION.md#protocol-eras-and-modes) before
deploying.

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
- `:http` for Streamable HTTP; modern POST-owned SSE streams need no server flag
- `:beam` for local client/server processes in the same BEAM VM
- `:test` for in-memory tests

The old `ExMCP.Native` direct dispatcher and public `:native` transport alias
were removed before 1.0. Use `transport: :beam` with a server pid for BEAM-local
MCP.
