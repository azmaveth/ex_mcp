# ExMCP Quick Reference

One-page reference for common ExMCP operations. Perfect for developers who need quick answers.

## Installation

```elixir
# mix.exs
{:ex_mcp, "~> 0.6.0"}
```

## Transport Selection

Choose the right transport for your use case:

```
                    Performance  Use Case
    ┌─────────────────────────────────────────────┐
    │ Native BEAM  │ ~15μs      │ Internal services │
    │ HTTP         │ ~5-20ms    │ Network clients   │
    │ stdio        │ ~1-5ms     │ External tools    │
    └─────────────────────────────────────────────┘
```

**Decision Tree:**
- **Native BEAM**: Same Elixir application, ultra-high performance
- **HTTP**: Network communication, web clients, load balancing
- **stdio**: External tools, cross-language integration

## Basic Client Setup

### Native BEAM Client (Fastest)
```elixir
# For services in the same Elixir application
result = ExMCP.Native.call_service(:my_service, "tool_name", %{arg: "value"})
```

### HTTP Client
```elixir
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:4000/mcp/v1"
)

{:ok, tools} = ExMCP.Client.list_tools(client)
{:ok, result} = ExMCP.Client.call_tool(client, "tool_name", %{arg: "value"})
```

### stdio Client  
```elixir
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: "python my_mcp_server.py"
)

{:ok, tools} = ExMCP.Client.list_tools(client)
```

## Basic Server Setup

### Native BEAM Server (Service)
```elixir
defmodule MyService do
  use ExMCP.Service
  
  deftool "greet" do
    meta do
      description("Greets someone")
      input_schema(%{
        type: "object",
        properties: %{name: %{type: "string"}},
        required: ["name"]
      })
    end
  end
  
  @impl true
  def handle_tool_call("greet", %{"name" => name}, state) do
    {:ok, %{content: [text("Hello, #{name}!")]}, state}
  end
end
```

### HTTP Server
```elixir
defmodule MyServer do
  use ExMCP.Server
  
  deftool "calculate" do
    meta do
      description("Performs calculations")
    end
  end
  
  @impl true
  def handle_tool_call("calculate", args, state) do
    # Your implementation
    {:ok, %{content: [text("Result: 42")]}, state}
  end
end

# Start server
{:ok, _pid} = MyServer.start_link(transport: :http, port: 4000)
```

### stdio Server
```elixir
defmodule StdioServer do
  use ExMCP.Server
  
  # Tool definitions...
end

# Start server
{:ok, _pid} = StdioServer.start_link(transport: :stdio)
```

## Common Operations

### Tool Operations
```elixir
# List available tools
{:ok, tools} = ExMCP.Client.list_tools(client)

# Call a tool
{:ok, result} = ExMCP.Client.call_tool(client, "tool_name", %{
  arg1: "value1",
  arg2: "value2"
})
```

### Resource Operations
```elixir
# List resources
{:ok, resources} = ExMCP.Client.list_resources(client)

# Read a resource
{:ok, content} = ExMCP.Client.read_resource(client, "resource://uri")

# Subscribe to resource changes (if supported)
{:ok, _} = ExMCP.Client.subscribe_resource(client, "resource://uri")
```

### Prompt Operations
```elixir
# List prompts
{:ok, prompts} = ExMCP.Client.list_prompts(client)

# Get a prompt
{:ok, prompt} = ExMCP.Client.get_prompt(client, "prompt_name", %{
  style: "formal",
  language: "english"
})
```

## Content Helpers

Use these helpers in your server implementations:

```elixir
# Text content
text("Hello, world!")

# JSON content  
json(%{status: "success", data: [1, 2, 3]})

# Image content
image("base64data", "image/png")

# Resource reference
resource("file://path/to/resource")

# User/assistant messages (for prompts)
user("Please help me with this task")
assistant("I'll be happy to help!")
```

## Configuration

### Basic Configuration
```elixir
# config/config.exs
config :ex_mcp,
  protocol_version: "2025-06-18",
  structured_output_enabled: true
```

### OAuth 2.1 Configuration
```elixir
config :ex_mcp, :oauth2_server_config,
  introspection_endpoint: "https://auth.example.com/introspect",
  required_scopes: ["mcp:read"],
  token_cache_ttl: :timer.minutes(5)
```

### Phoenix Integration
```elixir
# In your Phoenix router
forward "/mcp", ExMCP.HttpPlug, server: MyServer
```

## Error Handling

```elixir
case ExMCP.Client.call_tool(client, "tool_name", args) do
  {:ok, result} ->
    # Success
    IO.inspect(result.content)
    
  {:error, %{code: -32601}} ->
    # Method not found
    IO.puts("Tool not found")
    
  {:error, %{code: -32602}} ->
    # Invalid params
    IO.puts("Invalid arguments")
    
  {:error, reason} ->
    # Other error
    IO.puts("Error: #{inspect(reason)}")
end
```

## Testing

### Unit Tests
```elixir
defmodule MyServerTest do
  use ExUnit.Case, async: true
  
  @moduletag :unit
  
  test "greet tool returns greeting" do
    {:ok, result, _state} = MyServer.handle_tool_call(
      "greet", 
      %{"name" => "Alice"}, 
      %{}
    )
    
    assert %{content: [%{"type" => "text", "text" => text}]} = result
    assert String.contains?(text, "Alice")
  end
end
```

### Integration Tests
```elixir
defmodule MyServerIntegrationTest do
  use ExUnit.Case, async: false
  
  @moduletag :integration
  
  setup do
    {:ok, client} = ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:4000/mcp/v1"
    )
    
    %{client: client}
  end
  
  test "can list and call tools", %{client: client} do
    {:ok, tools} = ExMCP.Client.list_tools(client)
    assert length(tools) > 0
    
    {:ok, result} = ExMCP.Client.call_tool(client, "greet", %{"name" => "Test"})
    assert %{content: [_]} = result
  end
end
```

## Performance Tips

1. **Use Native BEAM** for internal services (~15μs calls)
2. **Connection pooling** for HTTP clients
3. **Async operations** with `Task.async` for concurrent calls
4. **Structured output** to reduce parsing overhead
5. **Resource caching** for frequently accessed data

## Common Patterns

### Service Registry Pattern
```elixir
# Register multiple services
ExMCP.Native.register_service(:calculator, CalculatorService)
ExMCP.Native.register_service(:file_manager, FileService)

# Call any service
ExMCP.Native.call_service(:calculator, "add", %{a: 1, b: 2})
```

### Client Pool Pattern
```elixir
# For high-throughput HTTP scenarios
children = for i <- 1..10 do
  Supervisor.child_spec(
    {ExMCP.Client, [transport: :http, url: url]},
    id: :"client_#{i}"
  )
end

Supervisor.start_link(children, strategy: :one_for_one)
```

## Need More Help?

- **Full Guide**: [USER_GUIDE.md](USER_GUIDE.md)
- **Examples**: `examples/` directory
- **Migration**: [MIGRATION.md](MIGRATION.md)
- **Troubleshooting**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)