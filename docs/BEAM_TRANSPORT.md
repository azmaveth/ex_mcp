# Native BEAM Transport

## Overview

The Native BEAM transport provides direct process-to-process communication within Elixir clusters, leveraging OTP's built-in features for maximum performance and reliability. This transport is designed for trusted Elixir services that need high-performance MCP communication with zero serialization overhead.

## Key Benefits

- **Zero Serialization**: Direct process communication with no JSON encoding/decoding for local calls
- **Ultra-Low Latency**: ~15μs for local calls, ~50μs for cross-node calls
- **Registry Discovery**: Built-in service discovery using Erlang Registry
- **OTP Integration**: Full supervision tree and fault tolerance support
- **Node Distribution**: Automatic cross-node communication support
- **Memory Efficient**: Single Registry entry per service, data passed by reference
- **Security Model**: Leverages Erlang's proven security and process isolation

## Architecture

### Service Registration

Services register themselves with the Native BEAM transport using the Registry:

```elixir
defmodule MyToolService do
  use GenServer

  def init(_) do
    # Register with the native transport
    ExMCP.Transport.Native.register_service(:my_tools)
    {:ok, %{}}
  end

  def handle_call({:mcp_request, %{"method" => "list_tools"}}, _from, state) do
    tools = [
      %{
        "name" => "ping",
        "description" => "Test tool",
        "inputSchema" => %{"type" => "object", "properties" => %{}}
      }
    ]
    {:reply, {:ok, %{"tools" => tools}}, state}
  end

  def handle_call({:mcp_request, %{"method" => "tools/call", "params" => params}}, _from, state) do
    # Handle tool calls
    {:reply, {:ok, %{"content" => [%{"type" => "text", "text" => "Result"}]}}, state}
  end
end
```

### Service Discovery

The Native BEAM transport provides built-in service discovery:

```elixir
# List all available services
services = ExMCP.Transport.Native.list_services()
#=> [{:my_tools, #PID<0.123.0>, %{registered_at: ~U[...]}}, ...]

# Check if a service is available
available? = ExMCP.Transport.Native.service_available?(:my_tools)
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
{:ok, tools} = ExMCP.Transport.Native.call(:my_tools, "list_tools", %{})

# Call a tool with arguments
{:ok, result} = ExMCP.Transport.Native.call(:my_tools, "tools/call", %{
  "name" => "ping",
  "arguments" => %{"message" => "hello"}
})

# Call with options (timeout, progress token, metadata)
{:ok, result} = ExMCP.Transport.Native.call(
  :my_tools,
  "process_data",
  %{"dataset" => "large_data"},
  timeout: 30_000,
  progress_token: "progress-123",
  meta: %{"trace_id" => "abc", "user_id" => "user1"}
)
```

### Cross-Node Communication

The Native BEAM transport automatically supports distributed Elixir clusters:

```elixir
# Call a service on another node
{:ok, result} = ExMCP.Transport.Native.call(
  {:data_service, :"worker@cluster.local"},
  "process_dataset",
  %{"dataset_id" => "abc123"}
)

# Works seamlessly with Elixir clustering
Node.connect(:"worker@cluster.local")
# Services on remote nodes are now available
```

### Notifications

Send fire-and-forget notifications to services:

```elixir
# Send a notification (no response expected)
:ok = ExMCP.Transport.Native.notify(:event_service, "resource_updated", %{
  "uri" => "file:///config.json",
  "type" => "modified"
})

# Handle notifications in your service
def handle_cast({:mcp_notification, %{"method" => "resource_updated", "params" => params}}, state) do
  # Process the notification
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

- **Registry Entry**: ~200 bytes per service
- **Process Overhead**: Standard GenServer memory (1-2KB)
- **Data Transfer**: Zero-copy for local calls (data passed by reference)
- **Cross-Node**: Standard Erlang distribution overhead

### Scalability

- **Services**: Limited only by Registry capacity (~millions)
- **Concurrent Calls**: Limited only by process scheduler
- **Nodes**: Standard Erlang distribution limits (~200+ nodes)

## Use Cases

### Ideal For

1. **Trusted Elixir Services**: Services within the same security boundary
2. **High-Performance Communication**: When latency matters most
3. **OTP Integration**: Services that need supervision and fault tolerance
4. **Distributed Systems**: Multi-node Elixir clusters
5. **Development/Testing**: Local service integration

### Not Suitable For

1. **External Clients**: Non-Elixir clients (use HTTP transport)
2. **Untrusted Services**: Services outside your security boundary
3. **Public APIs**: Internet-facing services (use HTTP transport)
4. **Cross-Language**: Communication with non-BEAM languages

## Security Model

The Native BEAM transport relies on Erlang's security model:

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

For untrusted services or external clients, use the HTTP transport instead:

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

# All services automatically register with Native BEAM transport
{:ok, _} = MyApp.Supervisor.start_link([])
```

### With Application Startup

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      # Start the Registry for Native BEAM transport
      {Registry, keys: :unique, name: ExMCP.Registry},
      
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
  use GenServer

  def init(_) do
    ExMCP.Transport.Native.register_service(:data_processor)
    {:ok, %{}}
  end

  def handle_call({:mcp_request, %{"method" => "process_data", "params" => params}}, _from, state) do
    # Use other services to process data
    {:ok, calc_result} = ExMCP.Transport.Native.call(:calculator, "tools/call", %{
      "name" => "sum",
      "arguments" => params["numbers"]
    })

    {:ok, file_result} = ExMCP.Transport.Native.call(:file_service, "tools/call", %{
      "name" => "save_result",
      "arguments" => %{"data" => calc_result}
    })

    {:reply, {:ok, %{"result" => "processed"}}, state}
  end
end
```

## Best Practices

### Service Registration

1. **Register Early**: Register services in `init/1` callbacks
2. **Meaningful Names**: Use descriptive atom names for services
3. **Cleanup**: Services automatically unregister when processes die

### Error Handling

1. **Handle Timeouts**: Use appropriate timeout values for long operations
2. **Process Errors**: Handle `{:error, reason}` responses appropriately
3. **Service Availability**: Check `service_available?/1` before critical calls

### Performance

1. **Local First**: Prefer local services for low-latency operations
2. **Batch Operations**: Group related calls when possible
3. **Progress Tokens**: Use for long-running operations
4. **Avoid Blocking**: Don't block in service handlers

### Development

1. **Use Supervision**: Always run services under supervisors
2. **Test Isolation**: Services can be tested independently
3. **Hot Reloading**: Services can be reloaded during development

## Migration from Old BEAM Transport

If you were using the previous TCP-based BEAM transport, here's how to migrate:

### Old API (Removed)

```elixir
# OLD - Don't use this anymore
{:ok, server} = ExMCP.Transport.Beam.Server.start_link([
  port: 9999,
  handler: MyHandler
])

{:ok, client} = ExMCP.Transport.Beam.Client.connect([
  host: "localhost",
  port: 9999
])
```

### New API (Native BEAM)

```elixir
# NEW - Use this instead
defmodule MyService do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_) do
    ExMCP.Transport.Native.register_service(:my_service)
    {:ok, %{}}
  end

  # Handle MCP requests directly
  def handle_call({:mcp_request, message}, _from, state) do
    # Process MCP message and return result
    {:reply, {:ok, result}, state}
  end
end

# Start the service
{:ok, _} = MyService.start_link([])

# Call the service directly
{:ok, result} = ExMCP.Transport.Native.call(:my_service, "method", %{})
```

## Examples

See the [examples/beam_transport/](../examples/beam_transport/) directory for complete working examples:

- **Calculator Service**: Basic MCP service with tools
- **Supervisor Example**: Multiple services under supervision
- **Distributed Example**: Cross-node communication
- **Inter-Service Communication**: Services calling other services

## Conclusion

The Native BEAM transport provides the highest performance option for MCP communication within Elixir clusters. By leveraging direct process communication and OTP's built-in features, it eliminates serialization overhead while maintaining all the benefits of Erlang's fault-tolerant architecture.

For trusted, high-performance communication between Elixir services, the Native BEAM transport is the ideal choice. For external clients or untrusted services, use the HTTP transport instead.