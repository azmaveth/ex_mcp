# ExMCP Architecture Guide

ExMCP is organized around protocol boundaries: clients, servers, transports,
HTTP Plug integration, ACP, authorization, and internal protocol helpers. Public
APIs stay small; cross-cutting work is kept at transport or Plug boundaries.

## Public Layers

### MCP Client

`ExMCP.Client` owns the client process, initialization handshake, request IDs,
server capability state, retries, and request/response formatting.

Client operation modules under `lib/ex_mcp/client/operations/` keep tool,
resource, and prompt calls focused while `ExMCP.Client.ConnectionManager`
normalizes transport startup.

### MCP Server

Server implementations use `ExMCP.Server.Handler`. Most applications should add
`ExMCP.Server.DSL` for declarative tools, resources, and prompts:

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL

  tool "echo", "Echo input" do
    param :message, :string, required: true

    run fn %{message: message}, state ->
      {:ok, %{content: [%{type: "text", text: message}]}, state}
    end
  end
end
```

`ExMCP.Server.HandlerServer` is the transport-aware process for in-memory and
BEAM-local handler execution. HTTP and stdio servers are started through
`ExMCP.Server.Transport` or the DSL-generated `start_link/1`.

### Transports

Transport modules implement `ExMCP.Transport`:

- `ExMCP.Transport.Stdio` for newline-delimited JSON-RPC over subprocess stdio.
- `ExMCP.Transport.HTTP` for streamable HTTP with optional SSE.
- `ExMCP.Transport.Local` for BEAM-local MCP maps/lists passed as Elixir terms.
- `ExMCP.Transport.Test` for in-memory tests.

BEAM-local MCP is selected with `transport: :beam` and requires a server PID:

```elixir
{:ok, server} = MyServer.start_link(transport: :beam)  # or HandlerServer.start_link(handler: MyHandler, ...)
{:ok, client} = ExMCP.Client.start_link(transport: :beam, server: server)
```

The removed `:native` alias and direct dispatcher API are not part of the 1.0
public architecture.

### HTTP Plug

`ExMCP.HttpPlug` is the HTTP server boundary. Request parsing, session
resolution, CORS/origin handling, response shaping, and SSE handling are split
under `lib/ex_mcp/http_plug/`.

Use normal Phoenix/Plug composition for HTTP edge concerns:

```elixir
pipeline :mcp do
  plug ExMCP.Plugs.DnsRebinding
  plug MyApp.AuthenticateMCP
end

scope "/mcp" do
  pipe_through :mcp

  forward "/", ExMCP.HttpPlug,
    handler: MyApp.MCPServer,
    server_info: %{name: "my-app", version: "1.0.0"},
    sse_enabled: true
end
```

### ACP

ACP modules live under `lib/ex_mcp/acp/`. `ExMCP.ACP.Client` controls ACP
agents, `ExMCP.ACP.Agent` exposes native Elixir ACP agents, and adapter modules
bridge external CLIs such as Claude Code, Codex, and Pi.

ACP pooling is intentionally left to consumers. ExMCP provides the protocol,
transport, adapter, and native-agent building blocks.

## Internal Functional Cores

Pure transformation and validation logic is kept separate from process and I/O
boundaries:

- Internal protocol and version modules handle message construction, parsing,
  and version rules.
- The message processor modules provide a Plug-like processing pipeline for
  server request dispatch.
- `ExMCP.Content.*` modules normalize content, sanitize inputs, and validate
  schema-related data.
- ACP mapper/protocol/session modules keep adapter-specific decoding separate
  from subprocess ports.

This structure keeps side effects at the edges: GenServers, Ports, HTTP
requests, Plug connections, filesystem-backed session stores, and telemetry.

## Resilience And Pipelines

ExMCP currently has three pipeline-style boundaries:

- HTTP server requests: normal Plug/Phoenix pipelines around `ExMCP.HttpPlug`.
- Server message processing: `ExMCP.MessageProcessor.run/2` for internal
  Plug-like request processing.
- Transport reliability: `ExMCP.Transport.ReliabilityWrapper`, client
  `retry_policy`, and `ExMCP.Reliability.*` components.

HTTP client connection handling is transport-owned today. If ExMCP later adds a
public client middleware API, it should wrap request construction and transport
send/receive at the `ExMCP.Client` boundary rather than inside HTTP-specific
code, so stdio, HTTP, and BEAM-local can share the same cross-cutting behavior.

## Module Map

```text
lib/ex_mcp/
  acp/                 ACP protocol, client, native agent, adapters
  authorization/       OAuth 2.1 and auth provider flows
  client/              Client operations, handlers, state, connection setup
  content/             Content builders, validation, sanitization
  http_plug/           HTTP Plug functional core and SSE handling
  internal/            Private protocol, map, version, and security helpers
  message_processor/   Plug-like MCP request processing
  plugs/               Reusable Plug security/auth components
  protocol/            Public protocol utility modules
  reliability/         Retry, circuit breaker, health check supervisor
  server/              Handler behavior, DSL, transport startup
  transport/           Stdio, HTTP, BEAM-local, test transports
```

## Testing Architecture

The test suite covers unit, integration, interop, conformance, security, and
transport behavior. `ExMCP.Transport.Test` and `transport: :beam` keep local
server/client tests fast without starting subprocesses or network listeners.

External conformance scripts live in `scripts/` and should be run for each
supported MCP spec version before release.

## Design Rules

- Prefer `ExMCP.Server.Handler` plus `ExMCP.Server.DSL` for servers.
- Use `transport: :beam` for local BEAM MCP, not a separate service dispatcher.
- Put HTTP authorization, origin, and request-signing checks in Plug pipelines.
- Put transport failure handling in client retry/reliability options.
- Keep pure protocol transformations in functional modules and side effects in
  GenServer, Port, Plug, or filesystem boundaries.
