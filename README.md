# ExMCP

<div align="center">

[![Hex.pm](https://img.shields.io/hexpm/v/ex_mcp.svg)](https://hex.pm/packages/ex_mcp)
[![Documentation](https://img.shields.io/badge/docs-hexdocs-blue.svg)](https://hexdocs.pm/ex_mcp)
[![CI](https://github.com/azmaveth/ex_mcp/workflows/CI/badge.svg)](https://github.com/azmaveth/ex_mcp/actions)
[![Coverage](https://img.shields.io/codecov/c/github/azmaveth/ex_mcp.svg)](https://codecov.io/gh/azmaveth/ex_mcp)
[![License](https://img.shields.io/hexpm/l/ex_mcp.svg)](LICENSE)

**A complete Elixir implementation of the Model Context Protocol (MCP)**

[User Guide](USER_GUIDE.md) | [API Docs](https://hexdocs.pm/ex_mcp) | [Examples](examples/) | [Changelog](CHANGELOG.md)

</div>

---

> ⚠️ **Alpha Software**: This project is currently in alpha stage (v0.2.x). The API is unstable and may change significantly before v1.0 release.

## Overview

ExMCP is a comprehensive Elixir implementation of the [Model Context Protocol](https://modelcontextprotocol.io/), enabling AI models to securely interact with local and remote resources through a standardized protocol. It provides both client and server implementations with multiple transport options, making it easy to build MCP-compliant tools and services in Elixir.

## ✨ Features

### Core Protocol Support
- 🚀 **Full MCP Specification** - Implements protocol version 2025-03-26
- 🛠️ **Tools** - Register and execute functions with type-safe parameters
- 📚 **Resources** - List and read data from various sources
- 🎯 **Prompts** - Manage reusable prompt templates
- 🤖 **Sampling** - Direct LLM integration for response generation
- 🌳 **Roots** - URI-based resource boundaries (new in 2025-03-26)
- 🔔 **Subscriptions** - Monitor resources for changes (new in 2025-03-26)
- 📦 **Batch Requests** - Send multiple requests in a single call
- 🔁 **Bi-directional Communication** - Servers can make requests to clients
- 👤 **Human-in-the-Loop** - Approval flows for sensitive operations

### Transport Layers
- 📝 **stdio** - Process communication via standard I/O
- 🌐 **SSE** - HTTP Server-Sent Events for web integration
- ⚡ **BEAM** - Native Erlang/Elixir process communication
- 🔌 **WebSocket** - WebSocket client for real-time communication

### Advanced Features
- 🔄 **Auto-Reconnection** - Built-in reconnection with exponential backoff
- 📊 **Progress Notifications** - Track long-running operations
- 🔍 **Server Discovery** - Automatic discovery of MCP servers
- 🎭 **Change Notifications** - Real-time updates for resources, tools, and prompts
- 🏗️ **OTP Integration** - Built on solid OTP principles with supervision trees
- 🔌 **Extensible** - Easy to add custom transports and handlers
- ✅ **Approval Handlers** - Implement custom approval flows for HITL
- 🔐 **Security** - Comprehensive authentication, TLS/SSL, CORS, and origin validation

## 🎯 API Categories

ExMCP provides three categories of APIs:

### MCP Specification Features (Portable)
These implement the official MCP specification and work with any MCP implementation:
- Core client/server operations (list_tools, call_tool, etc.)
- Standard transports (stdio, SSE)
- Protocol encoding/decoding
- OAuth 2.1 authorization

### ExMCP Extensions (Elixir-specific)
Enhanced features unique to ExMCP:
- BEAM transport for native Erlang communication
- Automatic server discovery
- Multi-server management
- Batch operations
- Auto-reconnection

### Draft Features (Experimental)
Features from the draft MCP specification (currently none implemented).

See the [API Categories Guide](guides/api-categories.md) for detailed information on writing portable code.

## 📦 Installation

Add `ex_mcp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.2.0"}
  ]
end
```

Then run:

```bash
mix deps.get
```

## 🚀 Quick Start

### Creating an MCP Client

```elixir
# Connect to a stdio-based server
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["node", "my-mcp-server.js"]
)

# Connect with authentication (SSE/WebSocket)
{:ok, secure_client} = ExMCP.Client.start_link(
  transport: :sse,
  url: "https://api.example.com",
  security: %{
    auth: {:bearer, "your-token"},
    validate_origin: true,
    allowed_origins: ["https://app.example.com"]
  }
)

# List available tools
{:ok, tools} = ExMCP.Client.list_tools(client)

# Call a tool
{:ok, result} = ExMCP.Client.call_tool(client, "search", %{
  query: "Elixir programming",
  limit: 10
})

# Read a resource
{:ok, content} = ExMCP.Client.read_resource(client, "file:///data.json")
```

### Creating an MCP Server

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "my-server",
      version: "1.0.0",
      capabilities: %{
        tools: %{},
        resources: %{}
      }
    }, state}
  end

  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        name: "echo",
        description: "Echoes back the input",
        input_schema: %{
          type: "object",
          properties: %{
            message: %{type: "string", description: "Message to echo"}
          },
          required: ["message"]
        }
      }
    ]
    {:ok, tools, state}
  end

  @impl true
  def handle_call_tool("echo", %{"message" => msg}, state) do
    {:ok, [%{type: "text", text: "Echo: #{msg}"}], state}
  end

  # ... implement other required callbacks
end

# Start the server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyServer,
  transport: :stdio
)
```

## 🔌 Transport Options

### stdio Transport

Best for subprocess communication:

```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :stdio
)

# Client
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["python", "mcp-server.py"],
  args: ["--config", "prod.json"]
)
```

### SSE Transport

For HTTP-based streaming:

```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :sse,
  port: 8080,
  path: "/mcp"
)

# Client
{:ok, client} = ExMCP.Client.start_link(
  transport: :sse,
  url: "http://localhost:8080/mcp",
  headers: [{"Authorization", "Bearer token"}]
)
```

### BEAM Transport

For native Elixir/Erlang communication:

```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :beam,
  name: {:global, :my_mcp_server}
)

# Client (same node)
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: server
)

# Client (different node)
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: {:global, :my_mcp_server}
)
```

## 🎯 Key Features

### Tools with Annotations

```elixir
@impl true
def handle_list_tools(state) do
  tools = [
    %{
      name: "delete_file",
      description: "Permanently deletes a file",
      input_schema: %{...},
      # New annotations in MCP 2025-03-26
      readOnlyHint: false,
      destructiveHint: true,
      costHint: :low
    }
  ]
  {:ok, tools, state}
end
```

### Resource Subscriptions

Monitor resources for changes:

```elixir
# Client subscribes to a resource
{:ok, _} = ExMCP.Client.subscribe_resource(client, "file:///config.json")

# Server notifies when resource changes
ExMCP.Server.notify_resource_updated(server, "file:///config.json")
```

### Roots for Resource Organization

Define URI boundaries:

```elixir
@impl true
def handle_list_roots(state) do
  roots = [
    %{uri: "file:///home/user/projects", name: "Projects"},
    %{uri: "https://api.example.com/v1", name: "API"}
  ]
  {:ok, roots, state}
end
```

### Progress Tracking

For long-running operations:

```elixir
@impl true
def handle_call_tool("process", %{"_progressToken" => token}, state) do
  Task.start(fn ->
    for i <- 1..100 do
      ExMCP.Server.notify_progress(self(), token, i, 100)
      Process.sleep(100)
    end
  end)
  
  {:ok, [%{type: "text", text: "Processing started"}], state}
end
```

### Batch Requests (ExMCP Extension)

Send multiple requests in a single call for improved efficiency:

```elixir
# Note: This is an ExMCP extension, not part of the MCP specification
requests = [
  {:list_tools, []},
  {:list_resources, []},
  {:read_resource, ["file:///config.json"]}
]

{:ok, [tools, resources, config]} = ExMCP.Client.batch_request(client, requests)
```

### Bi-directional Communication

Enable servers to make requests to clients:

```elixir
# Define a client handler
defmodule MyClientHandler do
  @behaviour ExMCP.Client.Handler
  
  @impl true
  def handle_create_message(params, state) do
    # Server wants client to sample an LLM
    result = %{
      "role" => "assistant",
      "content" => %{"type" => "text", "text" => "Response from LLM"},
      "model" => "gpt-4"
    }
    {:ok, result, state}
  end
  
  @impl true
  def handle_list_roots(state) do
    {:ok, [%{uri: "file:///home", name: "Home"}], state}
  end
end

# Start client with handler
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: MyClientHandler
)

# Server can now make requests to the client
{:ok, response} = ExMCP.Server.create_message(server, %{
  "messages" => [%{"role" => "user", "content" => "Hello"}]
})
```

### Human-in-the-Loop Approval

Implement approval flows for sensitive operations:

```elixir
# Use the built-in console approval handler
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: {ExMCP.Client.DefaultHandler, [
    approval_handler: ExMCP.Approval.Console
  ]}
)

# Or implement a custom approval handler
defmodule MyApprovalHandler do
  @behaviour ExMCP.Approval
  
  @impl true
  def request_approval(:sampling, params, _opts) do
    # Show params to user and get approval
    case prompt_user("Approve LLM sampling?", params) do
      :yes -> {:approved, params}
      :no -> {:denied, "User rejected"}
      {:modify, new_params} -> {:modified, new_params}
    end
  end
end
```

## 📚 Documentation

- 📖 **[User Guide](USER_GUIDE.md)** - Comprehensive guide with examples
- 🔧 **[API Documentation](https://hexdocs.pm/ex_mcp)** - Detailed API reference
- 🔐 **[Security Guide](docs/SECURITY.md)** - Authentication, TLS, and security best practices
- 📂 **[Examples](examples/)** - Complete working examples
- 📋 **[TASKS.md](TASKS.md)** - Development roadmap and status

## 🛠️ Development

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

### Code Quality Tools

- **Formatter** - Elixir's built-in code formatter
- **Credo** - Static code analysis
- **Dialyzer** - Type checking
- **Sobelow** - Security analysis
- **ExCoveralls** - Test coverage
- **Git Hooks** - Pre-commit and pre-push checks

## 🤝 Contributing

We welcome contributions! Please see:

- [TASKS.md](TASKS.md) for current development priorities
- [CHANGELOG.md](CHANGELOG.md) for version history
- [GitHub Issues](https://github.com/azmaveth/ex_mcp/issues) for bug reports and feature requests

Before contributing:

1. Fork the repository
2. Create a feature branch
3. Run `make quality` to ensure code quality
4. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- The [Model Context Protocol](https://modelcontextprotocol.io/) specification creators
- The Elixir community for excellent tooling and libraries
- Contributors and early adopters providing feedback

---

<div align="center">
Made with ❤️ for the Elixir community
</div>