# BEAM Transport Examples

This directory contains examples demonstrating the use of ExMCP's BEAM transport for building MCP servers and clients in Elixir.

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

### BEAM Transport Advantages

1. **Native Integration**: Works seamlessly with Elixir/Erlang processes
2. **Fault Tolerance**: Automatic recovery through OTP supervision
3. **Distribution**: Easy communication across nodes
4. **Performance**: Direct process messaging without serialization overhead
5. **Bidirectional**: Full support for server-initiated notifications

### Connection Patterns

1. **Local Process**: Connect by registered name
   ```elixir
   {:ok, client} = ExMCP.Client.start_link(
     transport: :beam,
     server: :my_server
   )
   ```

2. **Remote Process**: Connect across nodes
   ```elixir
   {:ok, client} = ExMCP.Client.start_link(
     transport: :beam,
     server: {:my_server, :"node@host"}
   )
   ```

3. **Direct PID**: Connect to a specific process
   ```elixir
   {:ok, client} = ExMCP.Client.start_link(
     transport: :beam,
     server: server_pid
   )
   ```

### Error Handling

The BEAM transport automatically handles:
- Process crashes (both client and server)
- Network partitions (for distributed nodes)
- Message buffering during temporary disconnections
- Automatic reconnection attempts

### Best Practices

1. **Use Supervisors**: Always run MCP servers under supervision
2. **Name Registration**: Use registered names for easier discovery
3. **Error Tuples**: Handle both `{:ok, result}` and `{:error, reason}`
4. **Progress Tokens**: Use progress notifications for long operations
5. **Monitoring**: Implement health checks for production systems

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
iex> Examples.BeamTransport.CalculatorClient.demo()
iex> Examples.BeamTransport.DistributedExample.demo_single_node()
iex> Examples.BeamTransport.SupervisorExample.demo()
```