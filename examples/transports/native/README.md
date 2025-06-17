# Native Service Dispatcher Examples

This directory contains examples of using ExMCP's Native Service Dispatcher for ultra-high performance Elixir-to-Elixir communication.

## What is the Native Service Dispatcher?

The Native Service Dispatcher (`ExMCP.Native`) provides direct process communication between Elixir services without JSON serialization overhead. It offers:

- **Ultra-low latency**: ~15μs per call (vs 1-5ms for stdio, 5-20ms for HTTP)
- **Zero serialization**: Direct Elixir term passing
- **Distributed support**: Works across Elixir nodes
- **Service discovery**: Automatic service registration with Horde.Registry
- **Hot code reloading**: Update services without downtime

## Examples

### Calculator Examples
- `calculator_server.ex` - Basic calculator service using `ExMCP.Service` macro
- `calculator_client.ex` - Client demonstrating Native.call usage
- `clustering_example.ex` - Multiple service instances with load balancing
- `distributed_example.ex` - Cross-node service communication

### Advanced Examples
- `hot_reload_example.ex` - Dynamic service updates at runtime
- `supervisor_example.ex` - OTP supervision tree integration
- `audio_content_example.exs` - Binary data handling without serialization

## When to Use Native vs Standard Transports

### Use Native Service Dispatcher when:
- Building Elixir-only microservices
- Need maximum performance (μs latency)
- Working with large binary data (images, audio)
- Building distributed Elixir systems
- Hot code reloading is required

### Use Standard Transports (stdio/HTTP) when:
- Following MCP specification strictly
- Integrating with non-Elixir services
- Building cross-language systems
- Need standard JSON-RPC compatibility

## Quick Start

```elixir
# Define a service
defmodule MyService do
  use ExMCP.Service, name: :my_service
  
  def handle_mcp_request("tools/call", %{"name" => "greet", "arguments" => args}, state) do
    {:ok, %{"content" => [%{"text" => "Hello, #{args["name"]}!"}]}, state}
  end
end

# Start the service
{:ok, _pid} = MyService.start_link()

# Call from anywhere
{:ok, result} = ExMCP.Native.call(:my_service, "tools/call", %{
  "name" => "greet",
  "arguments" => %{"name" => "World"}
})
```

## Performance Comparison

| Transport | Latency | Use Case |
|-----------|---------|----------|
| Native | ~15μs | Elixir microservices |
| stdio | ~1-5ms | Local subprocess communication |
| HTTP | ~5-20ms | Network services |

The Native Service Dispatcher is ideal for building high-performance Elixir systems while maintaining MCP-compatible interfaces.