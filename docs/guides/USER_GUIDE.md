# ExMCP User Guide

A practical guide to building MCP clients and servers with ExMCP.

## Table Of Contents

1. [Installation](#installation)
2. [Server DSL](#server-dsl)
3. [Low-Level Handlers](#low-level-handlers)
4. [BEAM-Local MCP](#beam-local-mcp)
5. [Clients](#clients)
6. [Transports](#transports)
7. [Resilience And Pipelines](#resilience-and-pipelines)
8. [Troubleshooting](#troubleshooting)

## Installation

```elixir
def deps do
  [
    {:ex_mcp, "~> 1.0.0-rc.4"}
  ]
end
```

## Server DSL

Use `ExMCP.Server.Handler` with `ExMCP.Server.DSL` for most servers:

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "my-server", version: "1.0.0"

  tool "echo", "Echoes the input message" do
    param :message, :string, required: true

    run fn %{message: message}, state ->
      {:ok, %{content: [%{type: "text", text: message}]}, state}
    end
  end

  resource "config://app", "Application configuration" do
    mime_type "application/json"

    read fn _params, state ->
      {:ok, %{text: Jason.encode!(%{debug: false})}, state}
    end
  end

  prompt "summarize", "Summarize text" do
    arg :text, required: true

    render fn %{text: text}, state ->
      {:ok,
       %{
         messages: [
           %{role: "user", content: %{type: "text", text: "Summarize: #{text}"}}
         ]
       }, state}
    end
  end
end
```

Start it with the transport you need:

```elixir
{:ok, server} = MyServer.start_link(transport: :beam)
```

## Low-Level Handlers

Use handwritten callbacks when capabilities are fully dynamic or you need
custom behavior. For nearly all cases, the DSL is simpler and recommended:

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "my-server", version: "1.0.0"

  tool "ping", "Health check" do
    run fn _args, state ->
      {:ok, %{content: [%{type: "text", text: "pong"}]}, state}
    end
  end
end

{:ok, server} = MyServer.start_link(transport: :beam)
```

### Raw Callback Example

```elixir
defmodule DynamicServer do
  use ExMCP.Server.Handler

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       protocolVersion: ExMCP.protocol_version(),
       serverInfo: %{name: "dynamic", version: "1.0.0"},
       capabilities: %{tools: %{}}
     }, state}
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "ping",
        description: "Health check",
        inputSchema: %{type: "object", properties: %{}}
      }
    ]

    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("ping", _args, state) do
    {:ok, %{content: [%{type: "text", text: "pong"}]}, state}
  end
end

# Start a raw handler (no DSL):
{:ok, server} =
  ExMCP.Server.HandlerServer.start_link(
    handler: DynamicServer,
    transport: :beam
  )
# Or the convenience:
# {:ok, server} = ExMCP.start_server(handler: DynamicServer, transport: :beam)
```

## BEAM-Local MCP

Use `transport: :beam` when both sides are Elixir processes in the same VM.
When using the DSL the server module gets a `start_link/1`:

```elixir
{:ok, server} = MyServer.start_link(transport: :beam)

{:ok, client} =
  ExMCP.Client.start_link(
    transport: :beam,
    server: server
  )

{:ok, tools} = ExMCP.Client.list_tools(client)
{:ok, result} = ExMCP.Client.call_tool(client, "echo", %{"message" => "hello"})
```

For a raw handler (no DSL) use `ExMCP.Server.HandlerServer.start_link(handler: MyHandler, ...)` (or `ExMCP.start_server/1`).

**Tip:** `mix examples.getting_started` (after `mix compile`) gives a fast local run of these DSL + Client patterns for quick verification.

BEAM-local MCP uses the normal initialize handshake, request IDs, capabilities,
and handler callbacks. The transport passes MCP-shaped maps/lists as Elixir
terms instead of JSON strings.

## Clients

Connect to stdio:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :stdio,
    command: ["node", "server.js"],
    cd: "/path/to/project",
    env: [{"NODE_ENV", "production"}]
  )
```

Connect to HTTP/SSE:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
    use_sse: true,
    headers: [{"Authorization", "Bearer #{token}"}]
  )
```

Call server features:

```elixir
{:ok, tools} = ExMCP.Client.list_tools(client)
{:ok, result} = ExMCP.Client.call_tool(client, "search", %{"query" => "Elixir"})
{:ok, resources} = ExMCP.Client.list_resources(client)
{:ok, content} = ExMCP.Client.read_resource(client, "file:///docs/readme.md")
{:ok, prompts} = ExMCP.Client.list_prompts(client)
```

## Transports

| Transport | Use When |
|-----------|----------|
| `:stdio` | Spawning an MCP subprocess |
| `:http` | Talking to a remote or Phoenix-hosted MCP server |
| `:beam` | Connecting local Elixir client/server processes |
| `:test` | Unit/integration tests |

## Resilience And Pipelines

Use client retries for transient connection/request failures:

```elixir
{:ok, client} =
  ExMCP.Client.start_link(
    transport: :http,
    url: "https://api.example.com/mcp",
    retry_policy: [max_attempts: 3, initial_delay: 100, max_delay: 2_000]
  )
```

Use transport reliability when a circuit breaker or health check belongs at the
connection boundary:

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

For HTTP servers, put side-effecting concerns such as authentication, request
signing, CORS, and DNS rebinding protection in the Plug/Phoenix pipeline before
`ExMCP.HttpPlug`.

## Troubleshooting

**BEAM-local client cannot connect**

```elixir
Process.alive?(server)
ExMCP.Client.start_link(transport: :beam, server: server)
```

**stdio server exits immediately**

Make sure `command` includes the executable and arguments as a list, and use
`cd`/`env` if the subprocess needs a specific working directory or environment.

**HTTP connection refused**

Verify the URL path matches the server endpoint. `ExMCP.Transport.HTTP` extracts
the path from `url` unless `endpoint:` is provided explicitly.

**Need HTTP auth or validation**

Use `headers`, `auth`, `auth_provider`, `security`, or Plug composition around
`ExMCP.HttpPlug` depending on whether the concern is client-side or server-side.
