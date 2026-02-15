# ExMCP Quick Start Guide

Get up and running with ExMCP in just a few minutes! This guide shows you how to create MCP servers using the declarative DSL, connect clients, and use ExMCP's ultra-fast Native BEAM dispatcher.

**Next Steps:** After completing this quickstart, see the **[User Guide](../guides/USER_GUIDE.md)** for comprehensive examples, the **[DSL Guide](../DSL_GUIDE.md)** for advanced DSL patterns, and the **[Configuration Guide](../CONFIGURATION.md)** for production setup.

## Installation

Add ExMCP to your `mix.exs` dependencies:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.7.4"}
  ]
end
```

Run `mix deps.get` to install.

## Quick Examples

### 1. MCP Server with DSL (stdio transport)

Define tools, resources, and prompts declaratively with the DSL, then implement handlers:

```elixir
defmodule MyMCPServer do
  use ExMCP.Server, name: "my-server", version: "1.0.0"

  # Declare tools with the DSL
  deftool "echo" do
    meta do
      description("Echoes back the input message")

      input_schema(%{
        "type" => "object",
        "properties" => %{
          "message" => %{"type" => "string", "description" => "Message to echo"}
        },
        "required" => ["message"]
      })
    end
  end

  deftool "add" do
    meta do
      description("Adds two numbers together")

      input_schema(%{
        "type" => "object",
        "properties" => %{
          "a" => %{"type" => "number"},
          "b" => %{"type" => "number"}
        },
        "required" => ["a", "b"]
      })
    end
  end

  # Declare resources
  defresource "config://app/settings" do
    meta do
      name("App Settings")
      description("Current application configuration")
    end

    mime_type("application/json")
  end

  # Declare prompts
  defprompt "summarize" do
    meta do
      name("Summarizer")
      description("Summarize content in a given style")
    end

    arguments do
      arg(:text, required: true, description: "Text to summarize")
      arg(:style, description: "Summary style (concise, detailed, bullet)")
    end
  end

  # Implement tool handlers
  @impl true
  def handle_tool_call("echo", %{"message" => message}, state) do
    {:ok, %{content: [%{type: "text", text: "Echo: #{message}"}]}, state}
  end

  @impl true
  def handle_tool_call("add", %{"a" => a, "b" => b}, state) do
    {:ok, %{content: [%{type: "text", text: "#{a + b}"}]}, state}
  end

  # Implement resource handler
  @impl true
  def handle_resource_read("config://app/settings", _uri, state) do
    config = Jason.encode!(%{debug: false, log_level: "info"})
    {:ok, %{type: "text", text: config}, state}
  end

  # Implement prompt handler
  @impl true
  def handle_prompt_get("summarize", args, state) do
    style = Map.get(args, "style", "concise")

    {:ok, %{
      messages: [
        %{role: "user", content: %{
          type: "text",
          text: "Please provide a #{style} summary of: #{args["text"]}"
        }}
      ]
    }, state}
  end

  # Implement initialization
  @impl true
  def handle_initialize(params, state) do
    {:ok, %{
      "protocolVersion" => params["protocolVersion"],
      "serverInfo" => %{"name" => "my-server", "version" => "1.0.0"},
      "capabilities" => get_capabilities()
    }, state}
  end
end

# Start the server
{:ok, server} = ExMCP.Server.start_link(
  transport: :stdio,
  handler: MyMCPServer
)
```

The DSL automatically generates `handle_list_tools`, `handle_list_resources`, and `handle_list_prompts` from your declarations. You only need to implement the execution callbacks.

### 2. Native Service Dispatcher (ultra-fast)

Perfect for high-performance communication within Elixir clusters:

```elixir
defmodule CalculatorService do
  use ExMCP.Service, name: :calculator

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

### Native Service Dispatcher

```
Service A <--Direct GenServer.call()--> Service B
          (via pluggable Registry lookup)
```

**Use cases:**
- Trusted Elixir services within clusters
- High-performance internal communication
- Distributed Elixir applications
- Real-time systems

**Performance:** ~15μs local calls, ~50μs cross-node

## When to Use Which?

### Use Traditional MCP When:
- Communicating with external AI models
- Building language-agnostic tools
- Following strict MCP protocol compliance
- Network-based communication needed

### Use Native Service Dispatcher When:
- High-performance internal communication
- Trusted Elixir service clusters
- Real-time or latency-sensitive applications
- Large data processing (zero serialization)

## Advanced Features

### Cross-Node Communication

For distributed clusters, add Horde and configure the distributed registry:

```elixir
# mix.exs
{:horde, "~> 0.8"}

# config/config.exs
config :ex_mcp, :service_registry, ExMCP.ServiceRegistry.Horde
```

Services then work transparently across nodes:

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
# List all services
services = ExMCP.Native.list_services()
#=> [
#=>   {:calculator, #PID<0.123.0>, %{registered_at: ~U[...]}},
#=>   {:data_processor, #PID<0.124.0>, %{registered_at: ~U[...]}}
#=> ]

# Check specific service availability
if ExMCP.Native.service_available?(:calculator) do
  {:ok, result} = ExMCP.Native.call(:calculator, "tools/call", %{
    "name" => "add",
    "arguments" => %{"a" => 1, "b" => 2}
  })
end
```

## Next Steps

1. **Read the [DSL Guide](../DSL_GUIDE.md)** - Full DSL reference and advanced patterns
2. **Read the [User Guide](../guides/USER_GUIDE.md)** - Comprehensive documentation
3. **Check the [API Docs](https://hexdocs.pm/ex_mcp)** - Detailed API reference
4. **Explore [Examples](../../examples/)** - Real-world usage patterns
5. **Review [Transport Guide](../TRANSPORT_GUIDE.md)** - Transport selection and optimization

## Performance Tips

### Native Service Dispatcher Optimization

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

# If using Horde adapter, check cluster members
Horde.Cluster.members(ExMCP.ServiceRegistry.Horde.Registry)
```

**Performance issues:**
```elixir
# Measure call performance
:timer.tc(fn -> ExMCP.Native.call(:service, "method", %{}) end)
```
