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

Native Erlang/Elixir process communication:

```elixir
# Local process
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  target: {:local, :my_mcp_server}
)

# Remote node
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  target: {:remote, :my_mcp_server, :"node@host"}
)
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

## Contributing

Contributions are welcome! Please see [TASKS.md](TASKS.md) for the current development status and roadmap.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.