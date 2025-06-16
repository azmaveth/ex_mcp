# Native BEAM Service Dispatcher

## Overview

The Native BEAM Service Dispatcher provides ultra-fast, direct process communication within Elixir clusters using `ExMCP.Native` and Horde.Registry. This is not a traditional MCP transport, but rather a high-performance service dispatcher that leverages OTP's built-in features for maximum performance and reliability.

## Key Benefits

- **Zero Serialization**: Direct GenServer.call() with no JSON encoding/decoding for local calls
- **Ultra-Low Latency**: ~15μs for local calls, ~50μs for cross-node calls
- **Horde Registry**: Distributed service discovery via gossip protocol
- **OTP Integration**: Full supervision tree and fault tolerance support
- **Cross-Node Distribution**: Automatic distributed communication
- **Memory Efficient**: Single Horde.Registry entry per service, data passed by reference
- **Security Model**: Leverages Erlang's proven security and process isolation

## Architecture

### Service Registration with ExMCP.Service

Services use the `ExMCP.Service` macro for automatic registration and lifecycle management:

```elixir
defmodule MyToolService do
  use ExMCP.Service, name: :my_tools

  @impl true
  def init(_args) do
    # Service automatically registers with ExMCP.Native on startup
    {:ok, %{}}
  end

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "ping",
        "description" => "Test tool",
        "inputSchema" => %{"type" => "object", "properties" => %{}}
      }
    ]
    {:ok, %{"tools" => tools}, state}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "ping", "arguments" => args}, state) do
    content = [%{"type" => "text", "text" => "Pong! #{inspect(args)}"}]
    {:ok, %{"content" => content}, state}
  end

  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end

  # Optional: Handle notifications (fire-and-forget)
  @impl true
  def handle_mcp_notification("clear_cache", _params, state) do
    {:noreply, %{state | cache: %{}}}
  end
end
```

### Service Discovery with Horde

The Native BEAM dispatcher provides distributed service discovery via Horde.Registry:

```elixir
# List all available services across the cluster
services = ExMCP.Native.list_services()
#=> [{:my_tools, #PID<0.123.0>, %{registered_at: ~U[...]}}, ...]

# Check if a service is available
available? = ExMCP.Native.service_available?(:my_tools)
#=> true

# Discover services across the cluster
for {service_name, _pid, _meta} <- services do
  IO.puts("Found service: #{service_name}")
end
```

### Direct Service Calls

Call services directly without any transport overhead:

```elixir
# Call a local service
{:ok, tools} = ExMCP.Native.call(:my_tools, "list_tools", %{})

# Call a tool with arguments
{:ok, result} = ExMCP.Native.call(:my_tools, "tools/call", %{
  "name" => "ping",
  "arguments" => %{"message" => "hello"}
})

# Call with options (timeout, progress token, metadata)
{:ok, result} = ExMCP.Native.call(
  :my_tools,
  "tools/call",
  %{"name" => "process_data", "arguments" => %{"dataset" => "large_data"}},
  timeout: 30_000,
  progress_token: "progress-123",
  meta: %{"trace_id" => "abc", "user_id" => "user1"}
)
```

### Cross-Node Communication

The Native BEAM dispatcher automatically supports distributed Elixir clusters via Horde:

```elixir
# Call a service on another node
{:ok, result} = ExMCP.Native.call(
  {:data_service, :"worker@cluster.local"},
  "tools/call",
  %{"name" => "process_dataset", "arguments" => %{"dataset_id" => "abc123"}}
)

# Works seamlessly with Elixir clustering
Node.connect(:"worker@cluster.local")
# Services on remote nodes are automatically discovered via Horde gossip
```

### Notifications

Send fire-and-forget notifications to services:

```elixir
# Send a notification (no response expected)
:ok = ExMCP.Native.notify(:event_service, "resource_updated", %{
  "uri" => "file:///config.json",
  "type" => "modified"
})

# Handle notifications in your service
@impl true
def handle_mcp_notification("resource_updated", params, state) do
  # Process the notification
  IO.puts("Resource updated: #{params["uri"]}")
  {:noreply, state}
end
```

## Performance Characteristics

### Latency

| Operation Type | Local Call | Cross-Node Call | HTTP Equivalent |
|----------------|------------|-----------------|-----------------|
| Service Call   | ~15μs      | ~50μs           | ~1-5ms          |
| Notification   | ~5μs       | ~20μs           | ~1-5ms          |
| Service Discovery | ~1μs    | ~10μs           | ~1-5ms          |

### Memory Usage

- **Horde.Registry Entry**: ~200 bytes per service
- **Process Overhead**: Standard GenServer memory (1-2KB)
- **Data Transfer**: Zero-copy for local calls (data passed by reference)
- **Cross-Node**: Standard Erlang distribution overhead

### Scalability

- **Services**: Limited only by Horde.Registry capacity (~millions)
- **Concurrent Calls**: Limited only by process scheduler
- **Nodes**: Standard Erlang distribution limits (~200+ nodes)

## Use Cases

### Ideal For

1. **Trusted Elixir Services**: Services within the same security boundary
2. **High-Performance Communication**: When latency matters most (<100μs)
3. **Real-Time Systems**: Time-sensitive operations requiring minimal overhead
4. **Large Data Processing**: Zero serialization for large datasets
5. **Distributed Systems**: Multi-node Elixir clusters
6. **OTP Integration**: Services that need supervision and fault tolerance

### Not Suitable For

1. **External Clients**: Non-Elixir clients (use HTTP transport)
2. **Untrusted Services**: Services outside your security boundary
3. **Public APIs**: Internet-facing services (use HTTP transport)
4. **Cross-Language**: Communication with non-BEAM languages
5. **MCP Protocol Compliance**: Use traditional transports for strict MCP compatibility

## Security Model

The Native BEAM dispatcher relies on Erlang's security model:

### Trust Boundary

- **Trusted**: All services within the Elixir cluster
- **Authentication**: Erlang cookie for node joining
- **Authorization**: Process-level permissions via OTP supervision
- **Isolation**: Process sandboxing and supervision trees

### Security Features

- **Process Isolation**: Each service runs in its own process
- **Supervision**: Automatic restart on crashes
- **No Network Exposure**: Direct process communication only
- **Cookie Authentication**: Node-level authentication required

### Security Considerations

For untrusted services or external clients, use traditional MCP transports instead:

```elixir
# For external/untrusted clients - use HTTP transport
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  port: 8080
)
```

## Integration Patterns

### With OTP Supervision

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_init_arg) do
    children = [
      {MyApp.CalculatorService, []},
      {MyApp.FileService, []},
      {MyApp.DataService, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

# All services automatically register with ExMCP.Native
{:ok, _} = MyApp.Supervisor.start_link([])
```

### With Application Startup

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      # Horde registry and supervisor are started by ExMCP.Application
      
      # Start your MCP services
      MyApp.Supervisor
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

### Inter-Service Communication

```elixir
defmodule DataProcessor do
  use ExMCP.Service, name: :data_processor

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "process_data", "arguments" => params}, state) do
    # Use other services to process data
    {:ok, calc_result} = ExMCP.Native.call(:calculator, "tools/call", %{
      "name" => "sum",
      "arguments" => params["numbers"]
    })

    {:ok, file_result} = ExMCP.Native.call(:file_service, "tools/call", %{
      "name" => "save_result",
      "arguments" => %{"data" => calc_result}
    })

    content = [%{"type" => "text", "text" => "Data processed successfully"}]
    {:ok, %{"content" => content}, state}
  end
end
```

## Resilience Patterns

Add optional resilience for unreliable services using `ExMCP.Resilience`:

```elixir
# Retry with exponential backoff
{:ok, result} = ExMCP.Resilience.call_with_retry(
  :flaky_service,
  "process_data",
  %{"input" => "data"},
  max_attempts: 3,
  backoff: :exponential
)

# Fallback for unavailable services
result = ExMCP.Resilience.call_with_fallback(
  :unreliable_service,
  "get_data",
  %{},
  fallback: fn -> {:ok, %{"data" => "cached_value"}} end
)

# Circuit breaker for failing services
{:ok, result} = ExMCP.Resilience.call_with_circuit_breaker(
  :unstable_service,
  "risky_operation",
  %{},
  failure_threshold: 5,
  timeout: 60_000
)
```

## Best Practices

### Service Registration

1. **Use ExMCP.Service**: Always use the service macro for automatic lifecycle management
2. **Meaningful Names**: Use descriptive atom names for services
3. **Early Registration**: Services register automatically on startup
4. **Cleanup**: Services automatically unregister when processes die

### Error Handling

1. **Handle Timeouts**: Use appropriate timeout values for long operations
2. **Process Errors**: Handle `{:error, reason}` responses appropriately
3. **Service Availability**: Check `service_available?/1` before critical calls
4. **Graceful Degradation**: Use resilience patterns for optional services

### Performance

1. **Local First**: Prefer local services for low-latency operations
2. **Batch Operations**: Group related calls when possible
3. **Progress Tokens**: Use for long-running operations
4. **Avoid Blocking**: Don't block in service handlers
5. **Zero-Copy**: Pass large data structures directly for local calls

### Development

1. **Use Supervision**: Always run services under supervisors
2. **Test Isolation**: Services can be tested independently
3. **Hot Reloading**: Services can be reloaded during development
4. **Monitor Performance**: Use `:timer.tc/1` to measure call latency

## Migration from Traditional MCP

If you're migrating from traditional MCP client/server patterns:

### Traditional MCP (stdio/HTTP)

```elixir
# OLD - Traditional MCP with transport overhead
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"]
)

{:ok, tools} = ExMCP.Client.list_tools(client)
{:ok, result} = ExMCP.Client.call_tool(client, "ping", %{})
```

### Native BEAM Service Dispatcher

```elixir
# NEW - Direct service calls with zero overhead
defmodule PingService do
  use ExMCP.Service, name: :ping_service

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [%{"name" => "ping", "description" => "Ping tool"}]
    {:ok, %{"tools" => tools}, state}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "ping"}, state) do
    content = [%{"type" => "text", "text" => "Pong!"}]
    {:ok, %{"content" => content}, state}
  end
end

# Start the service
{:ok, _} = PingService.start_link()

# Direct calls (15μs vs 1-5ms)
{:ok, tools} = ExMCP.Native.call(:ping_service, "list_tools", %{})
{:ok, result} = ExMCP.Native.call(:ping_service, "tools/call", %{"name" => "ping"})
```

## Examples

See the [examples/native_beam/](../examples/native_beam/) directory for complete working examples:

- **Calculator Service**: Basic MCP service with tools
- **Supervisor Example**: Multiple services under supervision
- **Distributed Example**: Cross-node communication
- **Inter-Service Communication**: Services calling other services
- **Resilience Patterns**: Retry, fallback, and circuit breaker examples

## Comparison with Traditional MCP

| Aspect | Native BEAM Dispatcher | Traditional MCP |
|--------|----------------------|----------------|
| **Latency** | ~15μs local, ~50μs cross-node | ~1-5ms |
| **Serialization** | Zero (local calls) | JSON encode/decode |
| **Use Case** | Trusted Elixir clusters | External clients, AI models |
| **Compliance** | ExMCP extension | Full MCP protocol |
| **Discovery** | Horde.Registry | Manual configuration |
| **Fault Tolerance** | OTP supervision | Transport reconnection |
| **Cross-Language** | Elixir only | Language agnostic |

## Conclusion

The Native BEAM Service Dispatcher provides the highest performance option for service communication within Elixir clusters. By leveraging direct GenServer.call() and Horde.Registry, it eliminates serialization overhead while maintaining all the benefits of Erlang's fault-tolerant architecture.

**Use Native BEAM When:**
- ⚡ Ultra-low latency required (<100μs)
- ⚡ High-throughput service communication
- ⚡ Large data processing (zero serialization)
- ⚡ Trusted Elixir service clusters

**Use Traditional MCP When:**
- 🌐 External clients (AI models, other languages)
- 🔒 Untrusted services or public APIs
- 📋 Strict MCP protocol compliance required
- 🔗 Network-based communication needed

For trusted, high-performance communication between Elixir services, the Native BEAM dispatcher is the ideal choice. For external clients or untrusted services, use traditional MCP transports instead.