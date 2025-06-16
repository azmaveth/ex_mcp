# ExMCP Examples

This directory contains comprehensive examples demonstrating various features and use cases of the ExMCP library.

## Example Categories

### 1. Native BEAM Transport Examples (`beam_transport/`)

Examples specific to the Native BEAM transport for direct Elixir service communication:

- **Calculator Service**: Basic MCP service with tools, state management, and progress tracking
- **Distributed Example**: Cross-node communication using Native BEAM transport
- **Supervisor Integration**: Building fault-tolerant MCP services with OTP supervision
- **Service Communication**: Direct service-to-service call patterns and best practices

[View Native BEAM Transport Examples](beam_transport/README.md)

### 2. Advanced Features Examples (`advanced_features/`)

Examples demonstrating the latest MCP protocol features:

- **Sampling/LLM Integration**: Implementing `createMessage` for AI model integration
- **Notifications**: All notification types (progress, resources, tools, prompts)
- **Dynamic Content**: Servers that change their capabilities at runtime

[View Advanced Features Examples](advanced_features/README.md)

### 3. Draft Specification Features (`draft_features_example.exs`)

⚠️ **Experimental**: Example demonstrating features from the draft MCP specification that are not part of the official 2025-03-26 release:

- **Structured Tool Output**: Tools with `outputSchema` and `structuredContent`
- **Logging Level Control**: Dynamic log level adjustment with `logging/setLevel`

Run with: `./examples/draft_features_example.exs`

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
# For HTTP transport
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8080"
)

# For stdio transport
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["node", "mcp-server.js"]
)

# For Native BEAM transport (direct service calls)
{:ok, tools} = ExMCP.Transport.Native.call(:my_service, "list_tools", %{})
{:ok, result} = ExMCP.Transport.Native.call(:my_service, "tools/call", %{
  "name" => "tool_name",
  "arguments" => %{}
})
```

### Progress Tracking

```elixir
# Server side
def handle_call_tool("process", params, state) do
  if token = get_in(params, ["_meta", "progressToken"]) do
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

3. **Streamable HTTP**: HTTP with optional Server-Sent Events
   - For HTTP-based communication
   - Good for web integrations
   - Use `:http` transport identifier

## Learn More

- [ExMCP Documentation](https://hexdocs.pm/ex_mcp)
- [Model Context Protocol Specification](https://modelcontextprotocol.io)
- [GitHub Repository](https://github.com/azmaveth/ex_mcp)