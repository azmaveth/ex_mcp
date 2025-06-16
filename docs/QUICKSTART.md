# ExMCP Quick Start Guide

Get up and running with ExMCP in just a few minutes! This guide shows you how to create MCP services using both traditional transports and ExMCP's ultra-fast Native BEAM dispatcher.

## Installation

Add ExMCP to your `mix.exs` dependencies:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.5.0"},
    {:horde, "~> 0.8"}  # Required for Native BEAM dispatcher
  ]
end
```

Run `mix deps.get` to install.

## Quick Examples

### 1. Traditional MCP Server (stdio transport)

Perfect for external tools that communicate via standard MCP protocol:

```elixir
defmodule MyMCPServer do
  use ExMCP.Server.Handler

  @impl true
  def init(_args), do: {:ok, %{}}

  @impl true  
  def handle_initialize(_params, state) do
    {:ok, %{
      protocolVersion: "2025-03-26",
      serverInfo: %{name: "my-server", version: "1.0.0"},
      capabilities: %{tools: %{listChanged: true}}
    }, state}
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        "name" => "echo",
        "description" => "Echoes back the input",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "message" => %{"type" => "string"}
          },
          "required" => ["message"]
        }
      }
    ]
    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("echo", %{"message" => message}, state) do
    content = [%{"type" => "text", "text" => "Echo: #{message}"}]
    {:ok, content, state}
  end
end

# Start the server
{:ok, server} = ExMCP.Server.start_link(
  transport: :stdio,
  handler: MyMCPServer
)
```

### 2. Native BEAM Service (ultra-fast)

Perfect for high-performance communication within Elixir clusters:

```elixir
defmodule CalculatorService do
  use ExMCP.Service, name: :calculator

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "add",
        "description" => "Adds two numbers",
        "inputSchema" => %{
          "type" => "object", 
          "properties" => %{
            "a" => %{"type" => "number"},
            "b" => %{"type" => "number"}
          },
          "required" => ["a", "b"]
        }
      }
    ]
    {:ok, %{"tools" => tools}, state}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "add", "arguments" => args}, state) do
    %{"a" => a, "b" => b} = args
    result = a + b
    
    content = [%{"type" => "text", "text" => "#{a} + #{b} = #{result}"}]
    {:ok, %{"content" => content}, state}
  end

  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

# Start the service (automatically registers with ExMCP.Native)
{:ok, _pid} = CalculatorService.start_link()

# Call the service directly
{:ok, tools} = ExMCP.Native.call(:calculator, "list_tools", %{})
{:ok, result} = ExMCP.Native.call(:calculator, "tools/call", %{
  "name" => "add", 
  "arguments" => %{"a" => 5, "b" => 3}
})
```

### 3. Client Connection

Connect to MCP servers:

```elixir
# Connect to stdio server
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  server: server_pid
)

# Initialize connection
{:ok, info} = ExMCP.Client.initialize(client, %{
  protocolVersion: "2025-03-26",
  clientInfo: %{name: "my-client", version: "1.0.0"},
  capabilities: %{}
})

# List available tools
{:ok, tools} = ExMCP.Client.list_tools(client)

# Call a tool
{:ok, result} = ExMCP.Client.call_tool(client, "echo", %{"message" => "Hello, MCP!"})
```

## Architecture Comparison

### Traditional MCP (Client/Server with Transport)

```
Client <--JSON-RPC--> Transport <--JSON-RPC--> Server
         (stdio/HTTP)             (stdio/HTTP)
```

**Use cases:**
- Cross-language communication
- External tools and AI models
- Standard MCP compliance
- Network communication

**Performance:** ~1-5ms per call

### Native BEAM Service Dispatcher

```
Service A <--Direct GenServer.call()--> Service B
          (via Horde.Registry lookup)
```

**Use cases:**
- Trusted Elixir services within clusters  
- High-performance internal communication
- Distributed Elixir applications
- Real-time systems

**Performance:** ~15μs local calls, ~50μs cross-node

## When to Use Which?

### Use Traditional MCP When:
- ✅ Communicating with external AI models
- ✅ Building language-agnostic tools
- ✅ Following strict MCP protocol compliance
- ✅ Network-based communication needed

### Use Native BEAM When:
- ⚡ High-performance internal communication
- ⚡ Trusted Elixir service clusters
- ⚡ Real-time or latency-sensitive applications
- ⚡ Large data processing (zero serialization)

## Advanced Features

### Resilience Patterns

Add retry logic and fallbacks for unreliable services:

```elixir
# Retry with exponential backoff
{:ok, result} = ExMCP.Resilience.call_with_retry(
  :unreliable_service,
  "process_data",
  %{"input" => data},
  max_attempts: 3,
  backoff: :exponential
)

# Fallback for service unavailability  
result = ExMCP.Resilience.call_with_fallback(
  :primary_service,
  "get_data", 
  %{},
  fallback: fn -> {:ok, %{"data" => "cached_result"}} end
)
```

### Cross-Node Communication

Services automatically work across Elixir cluster nodes:

```elixir
# Connect nodes
Node.connect(:"worker@cluster.local")

# Call service on remote node
{:ok, result} = ExMCP.Native.call(
  {:data_processor, :"worker@cluster.local"},
  "process_large_dataset",
  %{"dataset_id" => "abc123"}
)
```

### Service Discovery

```elixir
# List all services across the cluster
services = ExMCP.Native.list_services()
IO.inspect(services)
#=> [
#=>   {:calculator, #PID<0.123.0>, %{registered_at: ~U[...]}},
#=>   {:data_processor, #PID<0.124.0>, %{registered_at: ~U[...]}}
#=> ]

# Check specific service availability
if ExMCP.Native.service_available?(:calculator) do
  {:ok, result} = ExMCP.Native.call(:calculator, "add", %{"a" => 1, "b" => 2})
end
```

## Next Steps

1. **Read the [User Guide](USER_GUIDE.md)** - Comprehensive documentation
2. **Check the [API Reference](API_REFERENCE.md)** - Detailed API docs  
3. **Explore [Examples](../examples/)** - Real-world usage patterns
4. **Review [Transport Architecture](TRANSPORT_ARCHITECTURE.md)** - Deep dive into transports

## Performance Tips

### Native BEAM Optimization

1. **Use Direct Calls**: Skip Client/Server for internal services
2. **Batch Operations**: Combine multiple calls when possible  
3. **Leverage OTP**: Use supervision trees for fault tolerance
4. **Monitor Performance**: Use `:telemetry` for metrics

### Traditional MCP Optimization

1. **Connection Pooling**: Reuse connections when possible
2. **Batch Requests**: Use batch API for multiple operations
3. **Async Operations**: Use notifications for fire-and-forget
4. **Proper Cleanup**: Always close connections

## Troubleshooting

### Common Issues

**Service not found:**
```elixir
# Check if service is registered
ExMCP.Native.service_available?(:my_service)

# List all services
ExMCP.Native.list_services()
```

**Cross-node communication fails:**
```elixir
# Verify node connection
Node.list() |> IO.inspect()

# Check Horde cluster members
Horde.Cluster.members(ExMCP.ServiceRegistry)
```

**Performance issues:**
```elixir
# Measure call performance
:timer.tc(fn -> ExMCP.Native.call(:service, "method", %{}) end)
```

Ready to build fast, reliable MCP services with ExMCP! 🚀