# Getting Started with ExMCP

This directory contains simple examples to help you get started with ExMCP.

## Examples

### 1. `hello_world.exs` - All Transport Types Demo
The main demo that shows all three transport types in action:
- Native Service Dispatcher (Elixir-to-Elixir)
- stdio transport (subprocess communication)
- HTTP transport (network communication)

Run it:
```bash
elixir hello_world.exs
```

### 2. `stdio_server.exs` - Standard MCP Server
A standalone MCP server that communicates via stdin/stdout. This is the standard way MCP servers work and is compatible with any MCP client.

Run it standalone:
```bash
elixir stdio_server.exs
# Then send JSON-RPC messages to stdin
```

Or use it with the hello_world demo (it will be launched as a subprocess).

### 3. `simple_http_server.exs` - Simple HTTP MCP Server
A simple HTTP MCP server using Plug that responds to JSON-RPC requests.

Run it:
```bash
elixir simple_http_server.exs
# Server will start on http://localhost:8321
```

Then connect with any MCP client that supports HTTP transport.

### 4. `comprehensive_example.exs` - Full Feature Demo
A comprehensive example showing all MCP features including tools, resources, prompts, and more.

## Quick Start Guide

1. **For Elixir-only projects**: Use the Native Service Dispatcher
   ```elixir
   # Define a service
   defmodule MyService do
     use ExMCP.Service, name: :my_service
     # ... implement handle_mcp_request/3
   end
   
   # Call it directly
   {:ok, result} = ExMCP.Native.call(:my_service, "method", %{})
   ```

2. **For cross-language compatibility**: Use stdio or HTTP
   ```elixir
   # Start a server
   {:ok, server} = ExMCP.Server.start_link(
     handler: MyHandler,
     transport: :http,  # or :stdio
     transport_options: [port: 8080]
   )
   
   # Connect a client
   {:ok, client} = ExMCP.Client.start_link(
     transport: :http,
     transport_options: [base_url: "http://localhost:8080"]
   )
   ```

## Transport Comparison

| Transport | Use Case | Performance | Compatibility |
|-----------|----------|-------------|---------------|
| Native | Elixir-only systems | ~15μs latency | Elixir only |
| stdio | Subprocess communication | ~1-5ms latency | Universal |
| HTTP | Network services | ~5-20ms latency | Universal |

## Next Steps

- Explore transport-specific examples in `../transports/`
- See feature examples in `../features/`
- Check advanced patterns in `../advanced/`
- Try cross-language examples in `../interoperability/`