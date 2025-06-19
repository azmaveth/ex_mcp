# ExMCP v2 Examples

Well-organized examples demonstrating the ExMCP v2 API and DSL.

## Directory Structure

```
v2/
├── getting_started/      # Getting started examples and demos
│   ├── working_demo.exs          # ✅ Interactive v2 features demo
│   ├── hello_server_simple.exs   # ✅ Basic server with handler callbacks
│   ├── run_simple_demo.sh        # ✅ Working features demo script
│   ├── hello_server_stdio.exs    # ⚠️ STDIO server (needs debugging)
│   ├── hello_client_stdio.exs    # ⚠️ STDIO client (needs debugging)
│   ├── hello_server_native.exs   # ⚠️ Native BEAM transport (needs debugging)
│   ├── hello_server_http.exs     # ⚠️ HTTP server (needs debugging)
│   ├── hello_client_http.exs     # ⚠️ HTTP client (needs debugging)
│   ├── hello_server_sse.exs      # ⚠️ HTTP with SSE (needs debugging)
│   ├── hello_client_all.exs      # ⚠️ Universal client (needs debugging)
│   ├── run_demo.sh               # ⚠️ Transport demo script (experimental)
│   ├── run_demo.bat              # ⚠️ Windows demo script (experimental)
│   └── run_demo.exs              # ⚠️ Elixir demo script (experimental)
│
├── advanced/            # Complete, production-like examples
│   ├── complete_server.exs       # Full-featured server with all capabilities
│   ├── stateful_server.exs       # Server with state management
│   ├── error_handling_server.exs # Comprehensive error handling
│   └── weather_service.exs       # Production HTTP/SSE service
│
├── utilities/           # Utility examples for specific features
│   ├── structured_responses.exs  # Response and Error types
│   ├── error_handling.exs        # Error patterns
│   └── client_config.exs         # Configuration builder
│
├── migration_guide/     # v1 to v2 migration help
│   └── before_after.exs          # Side-by-side comparison
│
├── basic_client_v2.exs  # General purpose client example
├── simple_test_client.exs # API testing script
└── test_http_client.exs   # HTTP client testing
```

## Quick Start

1. **Start with v2 Features** (`getting_started/`)
   ```bash
   # Recommended: Working v2 features demo
   cd getting_started && ./run_simple_demo.sh
   
   # Individual examples:
   elixir getting_started/working_demo.exs
   ```

2. **Learn Core Concepts** (`utilities/`)
   ```bash
   elixir utilities/structured_responses.exs
   elixir utilities/client_config.exs
   ```

3. **Build Complete Servers** (`advanced/`)
   ```bash
   elixir advanced/complete_server.exs
   ```

## Transport Types

ExMCP v2 supports multiple transport types:

- **STDIO** - Subprocess communication (most common)
- **Native** - In-process BEAM communication
- **HTTP** - Standard HTTP with JSON-RPC
- **HTTP+SSE** - HTTP with Server-Sent Events for streaming

## Key v2 Features

### 1. DSL for Server Definition
```elixir
use ExMCP.ServerV2

deftool "greet" do
  description "Greets a user"
  input_schema %{
    type: "object",
    properties: %{name: %{type: "string"}},
    required: ["name"]
  }
  handler fn %{"name" => name} ->
    ExMCP.Response.text("Hello, #{name}!", "greet")
  end
end
```

### 2. Structured Responses
```elixir
ExMCP.Response.text("Hello!", "tool_name")
ExMCP.Response.json(%{data: "value"}, "tool_name")
ExMCP.Response.error("Something failed", "tool_name")
```

### 3. Configuration Builder
```elixir
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.put_transport(:stdio, command: ["server"])
         |> ExMCP.ClientConfig.put_timeout(connect: 5000)
```

### 4. Simplified Client API
```elixir
{:ok, client} = ExMCP.connect(config)
{:ok, response} = ExMCP.call_tool(client, "tool", %{args: "here"})
text = ExMCP.Response.text_content(response)
```

## Learning Path

1. **Beginners**: Start with `getting_started/hello_server_stdio.exs`
2. **Client Developers**: See `basic_client_v2.exs` and `utilities/client_config.exs`
3. **Server Developers**: Progress from `getting_started/` to `advanced/complete_server.exs`
4. **Migrating from v1**: Check `migration_guide/before_after.exs`

## Running Examples

All examples are self-contained with Mix.install:

```bash
elixir path/to/example.exs
```

No separate compilation or dependency installation needed!