# Batch Request Examples

This directory contains examples of batch request handling in ExMCP.

## Examples

### stdio_batch.exs
Demonstrates batch requests using the standard stdio transport. This is the MCP-compliant way to send multiple requests as a batch to any MCP server.

```bash
elixir stdio_batch.exs
```

### native_batch.exs
Shows how to use ExMCP's Native Service Dispatcher for ultra-high performance batch operations between Elixir services.

```bash
elixir native_batch.exs
```

## When to Use Each

- **stdio_batch.exs**: Use when communicating with any MCP server (Elixir or non-Elixir) via standard transports
- **native_batch.exs**: Use for Elixir-to-Elixir communication when you need maximum performance (~15μs per call)