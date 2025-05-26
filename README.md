# ExMCP

Model Context Protocol client/server library for Elixir.

> ⚠️ **Alpha Quality Software**: This library is in early development. APIs may change without notice until version 1.0.0 is released. Use in production at your own risk.

## Overview

ExMCP is an Elixir implementation of the [Model Context Protocol](https://modelcontextprotocol.io/), enabling AI models to communicate with external tools and resources through a standardized protocol. It provides both client and server implementations with multiple transport options.

## Features

- 🚀 **Full MCP Protocol Support** - Complete implementation of the MCP specification
- 🔌 **Multiple Transports** - stdio, SSE (Server-Sent Events), and BEAM native transports
- 🔄 **Auto-Reconnection** - Built-in reconnection with exponential backoff
- 🛠️ **Tool Integration** - Register and execute tools through the protocol
- 📚 **Resource Management** - List and read resources from servers
- 🎯 **Prompt Templates** - Manage and use prompt templates
- 🔍 **Server Discovery** - Automatic discovery of MCP servers
- ⚡ **Concurrent Requests** - Handle multiple requests simultaneously
- 🏗️ **OTP Integration** - Built on OTP principles with supervision trees

## Installation

Add `ex_mcp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.1.0"}
  ]
end
```

## Quick Start

### Creating a Client

```elixir
# Start a client with stdio transport
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: "node",
  args: ["path/to/mcp-server.js"]
)

# Initialize the connection
{:ok, _server_info} = ExMCP.Client.initialize(client)

# List available tools
{:ok, tools} = ExMCP.Client.list_tools(client)

# Execute a tool
{:ok, result} = ExMCP.Client.call_tool(client, "tool_name", %{param: "value"})
```

### Creating a Server

```elixir
defmodule MyMCPServer do
  use ExMCP.Server.Handler

  @impl true
  def handle_initialize(_params, state) do
    server_info = %{
      name: "my-server",
      version: "1.0.0",
      capabilities: %{
        tools: %{},
        resources: %{}
      }
    }
    {:ok, server_info, state}
  end

  @impl true
  def handle_list_tools(_params, state) do
    tools = [
      %{
        name: "hello",
        description: "Says hello",
        inputSchema: %{
          type: "object",
          properties: %{
            name: %{type: "string"}
          }
        }
      }
    ]
    {:ok, tools, state}
  end

  @impl true
  def handle_call_tool("hello", %{"name" => name}, state) do
    {:ok, %{content: [%{type: "text", text: "Hello, #{name}!"}]}, state}
  end
end

# Start the server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyMCPServer,
  transport: :stdio
)
```

## Transport Options

### stdio Transport

Standard input/output transport, ideal for process communication:

```elixir
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: "python",
  args: ["mcp_server.py"]
)
```

### SSE Transport

Server-Sent Events transport for HTTP streaming:

```elixir
{:ok, client} = ExMCP.Client.start_link(
  transport: :sse,
  url: "http://localhost:8080/sse"
)
```

### BEAM Transport

The BEAM transport enables MCP communication between Elixir/Erlang processes, either locally or across distributed nodes. This is ideal for building Elixir-native tool ecosystems.

#### Basic Usage

```elixir
# Start a server with a registered name
{:ok, server} = ExMCP.Server.start_link(
  handler: MyMCPServer,
  transport: :beam,
  name: :calculator_server
)

# Connect a client to the server
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: :calculator_server
)

# Use the server's tools
{:ok, result} = ExMCP.Client.call_tool(client, "add", %{"a" => 5, "b" => 3})
# => {:ok, %{"result" => 8}}
```

#### Distributed Usage

```elixir
# On node1@host1 - Start the server
{:ok, server} = ExMCP.Server.start_link(
  handler: WeatherService,
  transport: :beam,
  name: :weather_service
)

# On node2@host2 - Connect from another node
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: {:weather_service, :"node1@host1"}
)

# Works transparently across nodes
{:ok, weather} = ExMCP.Client.call_tool(client, "get_weather", %{"city" => "London"})
```

#### Advanced Features

```elixir
# Server with dynamic tool registration
defmodule DynamicToolServer do
  use ExMCP.Server.Handler
  
  def init(_args) do
    {:ok, %{tools: %{}}}
  end
  
  def add_tool(server, name, fun) do
    GenServer.call(server, {:add_tool, name, fun})
  end
  
  # ... handler implementation
end

# Client with auto-reconnection (built-in)
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: :my_server
)
# If server crashes and restarts, client automatically reconnects

# Server-initiated notifications work seamlessly
ExMCP.Server.notify_progress(server, "task-123", 50, 100)
# Client receives progress updates via handle_notification callback
```

## Advanced Features

### Sampling (LLM Integration)

ExMCP supports the sampling/createMessage feature for integrating with language models:

```elixir
# Define a server that can use an LLM
defmodule MyAIServer do
  use ExMCP.Server.Handler
  
  @impl true
  def handle_create_message(params, state) do
    messages = params["messages"]
    model_preferences = params["modelPreferences"]
    
    # Call your LLM provider here
    response = call_llm(messages, model_preferences)
    
    result = %{
      content: %{
        type: "text",
        text: response
      },
      model: "gpt-4",
      stopReason: "stop"
    }
    
    {:ok, result, state}
  end
end

# Client usage
{:ok, result} = ExMCP.Client.create_message(client, %{
  messages: [
    %{role: "user", content: %{type: "text", text: "Hello!"}}
  ],
  modelPreferences: %{
    hints: [%{name: "claude-3-sonnet"}],
    temperature: 0.7
  }
})
```

### Progress Notifications

Track progress of long-running operations:

```elixir
# Server implementation
def handle_call_tool("process_data", params, state) do
  progress_token = params["_progressToken"]
  
  # Send progress updates
  Task.start(fn ->
    for i <- 1..100 do
      ExMCP.Server.notify_progress(self(), progress_token, i, 100)
      Process.sleep(100)
    end
  end)
  
  {:ok, [%{type: "text", text: "Processing started"}], state}
end

# Client usage with progress token
{:ok, result} = ExMCP.Client.call_tool(
  client, 
  "process_data", 
  %{data: "..."},
  progress_token: "task-123"
)
```

### Change Notifications

Servers can notify clients about changes:

```elixir
# Notify when resources change
ExMCP.Server.notify_resources_changed(server)

# Notify when a specific resource is updated
ExMCP.Server.notify_resource_updated(server, "file:///path/to/resource")

# Notify when tools change
ExMCP.Server.notify_tools_changed(server)

# Notify when prompts change
ExMCP.Server.notify_prompts_changed(server)

# Clients automatically receive these notifications
# You can handle them by implementing a custom client
```

## Server Discovery

ExMCP can automatically discover MCP servers:

```elixir
# Discover npm packages with MCP servers
{:ok, servers} = ExMCP.Discovery.discover_npm_packages()

# Discover servers in a directory
{:ok, servers} = ExMCP.Discovery.discover_directory("/path/to/servers")
```

## Documentation

Full documentation is available on [HexDocs](https://hexdocs.pm/ex_mcp).

## Development

### Setup

```bash
# Install dependencies and set up git hooks
make setup

# Run all quality checks
make quality

# Run tests with coverage
make coverage

# Generate documentation
make docs
```

### Code Quality

This project uses several tools to maintain code quality:

- **Formatter** - Elixir's built-in code formatter
- **Credo** - Static code analysis for consistency and readability
- **Dialyzer** - Type checking and discrepancy analysis
- **Sobelow** - Security-focused static analysis
- **ExCoveralls** - Test coverage reporting
- **Git Hooks** - Pre-commit and pre-push hooks for quality checks

Run `make help` to see all available commands.

## Contributing

Contributions are welcome! Please see [TASKS.md](TASKS.md) for the current development status and roadmap.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.