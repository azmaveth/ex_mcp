# ExMCP Examples

This directory contains runnable examples for the current ExMCP API.

## Getting Started

`getting_started/` demonstrates the supported transports:

- `01_stdio_server.exs` - stdio server with a `hello` tool
- `02_http_server.exs` - HTTP server with resources
- `03_http_sse_server.exs` - HTTP server with SSE enabled
- `04_beam_server.exs` - BEAM-local server
- `demo_client.exs` - self-contained client demo for stdio, HTTP, HTTP+SSE, and BEAM-local

```bash
cd examples/getting_started
./run_demo.sh
```

## Server Examples

All server examples use:

```elixir
use ExMCP.Server.Handler
use ExMCP.Server.DSL, name: "my-server", version: "1.0.0"
```

- `basic_dsl_server.exs` - minimal tool, resource, and prompt
- `advanced_dsl_server.exs` - typed parameters, structured output, templates, and metadata
- `weather_service.exs` - practical simulated weather tools and resources
- `file_manager.exs` - sandboxed file operations and file resources

Run a server directly:

```bash
elixir examples/basic_dsl_server.exs
```

Server examples use stdio by default unless their filename calls out another transport.

## Client Example

- `basic_client.exs` - starts a BEAM-local server in-process, connects a client, lists tools/resources, and calls a tool

```bash
elixir examples/basic_client.exs
```

## ACP Examples

- `acp/echo_agent.exs` - native Elixir ACP agent over stdio
- `acp/controller.exs` - ACP controller that starts the agent and streams a prompt result

```bash
mix run examples/acp/controller.exs
```

## OAuth And Utility Examples

- `advanced/oauth/basic_pkce.exs` - offline OAuth 2.1 PKCE helper flow
- `utilities/client_config.exs` - pipe-friendly client configuration
- `utilities/error_handling.exs` - response and error helpers
- `utilities/structured_responses.exs` - structured response helpers

## Transport Names

Current public transports are:

- `:stdio` for subprocess JSON-RPC
- `:http` for Streamable HTTP, with `use_sse: true` when SSE is needed
- `:beam` for BEAM-local client/server processes in the same VM
- `:test` for in-memory tests

The old public `:native` alias and direct dispatcher API were removed before 1.0.
