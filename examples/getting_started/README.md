# Getting Started Examples

These examples show the supported ExMCP transports with the current
`ExMCP.Server.Handler` + `ExMCP.Server.DSL` server API.

## Files

- `01_stdio_server.exs` - subprocess stdio server with a `hello` tool
- `02_http_server.exs` - regular HTTP JSON-RPC server with resources
- `03_http_sse_server.exs` - HTTP JSON-RPC server with SSE enabled
- `04_beam_server.exs` - BEAM-local server for clients in the same VM
- `demo_client.exs` - starts demo servers and exercises all transport types

## Run The Demo

```bash
./run_demo.sh
```

or:

```bash
elixir demo_client.exs
```

## Start Servers Individually

```bash
elixir 01_stdio_server.exs
elixir 02_http_server.exs
elixir 03_http_sse_server.exs
elixir 04_beam_server.exs
```

## BEAM-Local Transport

The `:beam` transport is local to one BEAM VM. Start the server and pass its pid
to the client:

```elixir
{:ok, server} = MyServer.start_link(transport: :beam)
{:ok, client} = ExMCP.Client.start_link(transport: :beam, server: server)
```

It does not use the removed `ExMCP.Native` dispatcher and it does not discover
services through a registry.
