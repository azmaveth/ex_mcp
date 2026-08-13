# ExMCP Quick Start Guide

This guide shows the current 1.0 server, client, and BEAM-local patterns.

**Next Steps:** See the [User Guide](../guides/USER_GUIDE.md), [DSL Guide](../DSL_GUIDE.md), and [Configuration Guide](../CONFIGURATION.md).

## Installation

Install the modern-preferred rc.8 release candidate:

```elixir
def deps do
  [
    {:ex_mcp, "~> 1.0.0-rc.8"}
  ]
end
```

Run `mix deps.get` to install.

MCP `2026-07-28` is the latest stable revision. rc.8 tries it first while
retaining evidence-based legacy fallback. Pin the mode explicitly when rollout
policy must remain fixed:

```elixir
# config/config.exs
config :ex_mcp, protocol_mode: :prefer_modern
```

`:prefer_modern` is the rc.8 default. Use `:legacy_only` to preserve the
legacy protocol era. Exact rc.5 wire and session behavior still requires
package rollback to `1.0.0-rc.5`. See the
[Configuration Guide](../CONFIGURATION.md#protocol-eras-and-modes) for all four
modes.

`1.0.0-rc.8` is the current modern-preferred soak candidate and the
installation target for this tree.

## DSL Server

Define tools, resources, and prompts next to their handlers:

```elixir
defmodule MyMCPServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "my-server", version: "1.0.0"

  tool "echo", "Echoes back the input message" do
    param :message, :string, required: true

    # Plain strings and maps are normalized; ToolResult is aliased by the DSL
    run fn %{message: message}, state ->
      {:ok, %{content: [%{type: "text", text: "Echo: #{message}"}]}, state}
    end
  end

  resource "config://app/settings", "Current application configuration" do
    title "App Settings"
    mime_type "application/json"

    read fn _params, state ->
      {:ok, %{text: Jason.encode!(%{debug: false, log_level: "info"})}, state}
    end
  end
end

{:ok, server} = MyMCPServer.start_link(transport: :stdio)
```

The DSL generates the MCP list/read/call callbacks plus legacy initialization
and modern discovery metadata from your declarations. See the
[DSL Guide](../DSL_GUIDE.md) for param types (including `{:array, :string}`),
compile-time checks, and `ToolResult` helpers.

## Client Connections

Connect to a stdio server:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :stdio,
    command: ["node", "my-mcp-server.js"]
  )

{:ok, tools} = ExMCP.Client.list_tools(client)
{:ok, result} = ExMCP.Client.call_tool(client, "echo", %{"message" => "Hello"})
```

Connect to a streamable HTTP server:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "http://localhost:4000/mcp",
    use_sse: true
  )
```

## BEAM-Local MCP

Use `transport: :beam` when both the client and server are Elixir processes in the same VM:

```elixir
{:ok, server} =
  MyMCPServer.start_link(
    transport: :beam,
    protocol_mode: :prefer_modern
  )

{:ok, client} =
  ExMCP.Client.start_link(
    transport: :beam,
    server: server,
    protocol_mode: :prefer_modern
  )

{:ok, tools} = ExMCP.Client.list_tools(client)
{:ok, result} = ExMCP.Client.call_tool(client, "echo", %{"message" => "Hello"})
```

BEAM-local MCP uses the configured protocol mode. rc.8 defaults to
`:prefer_modern`, which uses MCP 2026-07-28 discovery and per-request context;
`:legacy_only` retains the initialize handshake. The transport simply passes
MCP-shaped maps/lists as Elixir terms between local processes.

> **Note for raw handlers:** If you are not using the DSL, start with `ExMCP.Server.HandlerServer.start_link(handler: YourHandler, transport: :beam)` (or `ExMCP.start_server/1`). DSL modules automatically provide `start_link/1`.

For a fast (compiled) run of the patterns in this guide, use `mix examples.getting_started` from the repo root.

## Choosing A Transport

| Transport | Best For |
|-----------|----------|
| `:stdio` | External MCP servers and subprocess tools |
| `:http` | Phoenix apps, remote clients, and Streamable HTTP |
| `:beam` | Trusted local Elixir client/server pairs |
| `:test` | Unit and integration tests |

## Resilience

Client connection retries are configured with `retry_policy`:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
    retry_policy: [max_attempts: 3, initial_delay: 100, max_delay: 2_000]
  )
```

Transport-level reliability can wrap supported transports with circuit breakers or health checks:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
    reliability: [
      circuit_breaker: [failure_threshold: 5, reset_timeout: 30_000],
      health_check: [check_interval: 60_000]
    ]
  )
```

For HTTP server-side pipelines, compose normal Plug/Phoenix plugs around `ExMCP.HttpPlug`.

## Next Steps

1. Read the [DSL Guide](../DSL_GUIDE.md)
2. Read the [User Guide](../guides/USER_GUIDE.md)
3. Review [Transport Guide](../TRANSPORT_GUIDE.md)
4. Review the [1.0 Migration Guide](MIGRATION.md) for the dual-era rollout
5. Explore [Examples](https://github.com/azmaveth/ex_mcp/tree/master/examples)
