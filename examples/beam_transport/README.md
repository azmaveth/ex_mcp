# Native BEAM Transport Examples

This directory contains examples demonstrating the use of ExMCP's Native BEAM transport for building MCP servers and clients in Elixir. The Native BEAM transport provides direct process-to-process communication within Elixir clusters, leveraging OTP's built-in features for maximum performance and reliability.

## Examples

### 1. Calculator Server (`calculator_server.ex` & `calculator_client.ex`)

A basic example showing:
- Implementing an MCP server with multiple tools
- Error handling and state management
- Progress notifications for long operations
- Client-server communication

Run the example:
```elixir
iex> Examples.BeamTransport.CalculatorClient.demo()
```

### 2. Distributed Example (`distributed_example.ex`)

Demonstrates MCP communication across Erlang nodes:
- Starting a server on one node
- Connecting from another node
- Transparent cross-node communication

To run:

Terminal 1:
```bash
iex --name node1@localhost --cookie democookie -S mix
iex> Examples.BeamTransport.DistributedExample.start_server()
```

Terminal 2:
```bash
iex --name node2@localhost --cookie democookie -S mix
iex> Examples.BeamTransport.DistributedExample.start_client()
```

Or run a single-node demo:
```elixir
iex> Examples.BeamTransport.DistributedExample.demo_single_node()
```

### 3. Supervisor Example (`supervisor_example.ex`)

Shows integration with OTP supervisors:
- Starting MCP components under supervision
- Automatic restart on crashes
- Building fault-tolerant MCP services
- Managing multiple MCP connections

Run the example:
```elixir
iex> Examples.BeamTransport.SupervisorExample.demo()
```

## Key Concepts

### Native BEAM Transport Advantages

#### Core Features
1. **Zero Serialization**: Direct process-to-process communication with no JSON overhead for local calls
2. **Registry Discovery**: Built-in service discovery using Erlang Registry
3. **OTP Integration**: Full supervision tree and fault tolerance support
4. **Node Distribution**: Automatic cross-node communication support
5. **Performance**: ~15μs local calls, ~50μs cross-node calls
6. **Memory Efficiency**: Single Registry entry per service, data passed by reference
7. **Security Model**: Leverages Erlang's proven security and process isolation

### Service Patterns

#### Native BEAM Service

1. **Service Registration**: Register your GenServer as an MCP service
   ```elixir
   defmodule MyToolService do
     use GenServer

     def init(_) do
       # Register with the native transport
       ExMCP.Transport.Native.register_service(:my_tools)
       {:ok, %{}}
     end

     def handle_call({:mcp_request, message}, _from, state) do
       # Handle MCP requests with normal GenServer patterns
       {:reply, {:ok, result}, state}
     end
   end
   ```

2. **Direct Service Calls**: Call services directly without TCP overhead
   ```elixir
   # Call a local service
   {:ok, tools} = ExMCP.Transport.Native.call(:my_tools, "list_tools", %{})

   # Call a service on another node
   {:ok, result} = ExMCP.Transport.Native.call(
     {:data_service, :"worker@cluster.local"},
     "process_data",
     %{"dataset_id" => "abc123"}
   )
   ```

### Error Handling

The BEAM transport automatically handles:
- Process crashes (both client and server)
- Network partitions (for distributed nodes)
- Message buffering during temporary disconnections
- Automatic reconnection attempts

### Best Practices

#### General Practices
1. **Use Supervisors**: Always run MCP services under supervision
2. **Service Registration**: Register services with meaningful names for discovery
3. **Error Tuples**: Handle both `{:ok, result}` and `{:error, reason}`
4. **Progress Tokens**: Use progress notifications for long operations
5. **OTP Patterns**: Follow standard GenServer patterns for state management
6. **Service Discovery**: Use `list_services/0` and `service_available?/1` for discovery
7. **Cross-Node**: Leverage Elixir clustering for distributed services

#### Example Production Service
```elixir
# Production-ready service with supervision
defmodule MyApp.MCPServiceSupervisor do
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

# Start the service supervisor
{:ok, _} = MyApp.MCPServiceSupervisor.start_link([])

# Services are now available for direct calls
{:ok, tools} = ExMCP.Transport.Native.call(:calculator, "list_tools", %{})
```

## Running All Examples

To load all examples in IEx:

```elixir
iex> c "examples/beam_transport/calculator_server.ex"
iex> c "examples/beam_transport/calculator_client.ex"
iex> c "examples/beam_transport/distributed_example.ex"
iex> c "examples/beam_transport/supervisor_example.ex"
```

Then run any of the demos:
```elixir
iex> Examples.NativeBeam.CalculatorClient.demo()
iex> Examples.NativeBeam.DistributedExample.demo_single_node()
iex> Examples.NativeBeam.SupervisorExample.demo()
```