# ExMCP Examples

This directory contains comprehensive examples demonstrating various features and use cases of the ExMCP library.

## Example Categories

### 1. BEAM Transport Examples (`beam_transport/`)

Examples specific to the BEAM transport for Elixir-to-Elixir MCP communication:

- **Calculator Server**: Basic MCP server with tools, state management, and progress tracking
- **Distributed Example**: Cross-node communication using BEAM transport
- **Supervisor Integration**: Building fault-tolerant MCP services with OTP
- **Calculator Client**: Client usage patterns and best practices

[View BEAM Transport Examples](beam_transport/README.md)

### 2. Advanced Features Examples (`advanced_features/`)

Examples demonstrating the latest MCP protocol features:

- **Sampling/LLM Integration**: Implementing `createMessage` for AI model integration
- **Notifications**: All notification types (progress, resources, tools, prompts)
- **Dynamic Content**: Servers that change their capabilities at runtime

[View Advanced Features Examples](advanced_features/README.md)

## Quick Start

To run any example:

1. Start an IEx session:
   ```bash
   iex -S mix
   ```

2. Compile the example file:
   ```elixir
   iex> c "examples/beam_transport/calculator_server.ex"
   iex> c "examples/beam_transport/calculator_client.ex"
   ```

3. Run the demo:
   ```elixir
   iex> Examples.BeamTransport.CalculatorClient.demo()
   ```

## Common Patterns

### Server Implementation

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "my-server",
      version: "1.0.0",
      capabilities: %{
        tools: %{},
        resources: %{},
        prompts: %{},
        sampling: %{}
      }
    }, state}
  end
  
  # Implement other callbacks as needed
end
```

### Client Usage

```elixir
# Connect to server
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,  # or :stdio, :sse
  server: :my_server
)

# Use the server
{:ok, tools} = ExMCP.Client.list_tools(client)
{:ok, result} = ExMCP.Client.call_tool(client, "tool_name", %{})
```

### Progress Tracking

```elixir
# Server side
def handle_call_tool("process", params, state) do
  if token = params["_progressToken"] do
    Task.start(fn ->
      for i <- 1..100 do
        Process.sleep(100)
        ExMCP.Server.notify_progress(self(), token, i, 100)
      end
    end)
  end
  {:ok, [%{type: "text", text: "Processing..."}], state}
end

# Client side
{:ok, result} = ExMCP.Client.call_tool(
  client, 
  "process", 
  %{},
  progress_token: "task-123"
)
```

### Notifications

```elixir
# Server can send various notifications
ExMCP.Server.notify_resources_changed(server)
ExMCP.Server.notify_resource_updated(server, "file:///path")
ExMCP.Server.notify_tools_changed(server)
ExMCP.Server.notify_prompts_changed(server)
ExMCP.Server.notify_progress(server, token, current, total)
```

## Transport Options

ExMCP supports three transport types:

1. **BEAM**: Native Erlang/Elixir message passing
   - Best for Elixir-to-Elixir communication
   - Supports distributed nodes
   - No serialization overhead

2. **stdio**: Standard input/output
   - For external process communication
   - Compatible with any MCP server

3. **SSE**: Server-Sent Events
   - For HTTP-based communication
   - Good for web integrations

## Learn More

- [ExMCP Documentation](https://hexdocs.pm/ex_mcp)
- [Model Context Protocol Specification](https://modelcontextprotocol.io)
- [GitHub Repository](https://github.com/yourusername/ex_mcp)