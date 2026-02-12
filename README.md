# ExMCP

<div align="center">

[![Hex.pm](https://img.shields.io/hexpm/v/ex_mcp.svg)](https://hex.pm/packages/ex_mcp)
[![Documentation](https://img.shields.io/badge/docs-hexdocs-blue.svg)](https://hexdocs.pm/ex_mcp)
[![CI](https://github.com/azmaveth/ex_mcp/workflows/CI/badge.svg)](https://github.com/azmaveth/ex_mcp/actions)
[![Coverage](https://img.shields.io/codecov/c/github/azmaveth/ex_mcp.svg)](https://codecov.io/gh/azmaveth/ex_mcp)
[![License](https://img.shields.io/hexpm/l/ex_mcp.svg)](LICENSE)

**A complete Elixir implementation of the Model Context Protocol (MCP)**

[Getting Started](docs/getting-started/) | [User Guide](docs/guides/USER_GUIDE.md) | [API Docs](https://hexdocs.pm/ex_mcp) | [Examples](examples/) | [Changelog](CHANGELOG.md)

</div>

---

## ✅ **Production Ready**: ExMCP v0.7.0 is production-ready with 100% MCP compliance and comprehensive testing. The API is stable and ready for production use.

## Overview

ExMCP is a comprehensive Elixir implementation of the [Model Context Protocol](https://modelcontextprotocol.io/), enabling AI models to securely interact with local and remote resources through a standardized protocol. It provides both client and server implementations with multiple transport options, **including native Phoenix integration via Plug compatibility**.

## ✨ Key Features

### Protocol & Standards
- 🚀 **Multiple MCP Versions** - Supports protocol versions 2024-11-05, 2025-03-26, 2025-06-18, and 2025-11-25
- ✅ **100% MCP Compliant** - Full implementation of official MCP specification
- 🛠️ **Complete Feature Set** - Tools, Resources, Prompts, Roots, Subscriptions, Batch requests
- 🔐 **OAuth 2.1 Support** - Complete Resource Server implementation

### Performance & Reliability
- ⚡ **Ultra-fast Native BEAM** - ~15μs local calls with zero serialization overhead
- 🔄 **Auto-Reconnection** - Built-in reconnection with exponential backoff
- 🏗️ **OTP Integration** - Built on solid OTP principles with supervision trees
- 📊 **Progress Notifications** - Track long-running operations

### Integration & Flexibility
- 🔌 **Phoenix Plug** - Native Phoenix integration with `ExMCP.HttpPlug` 
- 🌐 **Multiple Transports** - HTTP/SSE, stdio, and native BEAM support
- 🎯 **Session Management** - Automatic session tracking for SSE connections
- 🔄 **Bi-directional Communication** - Servers can make requests to clients

### Developer Experience
- 🧪 **Well Tested** - Comprehensive test suite with 2600+ tests
- 📚 **Extensive Documentation** - Complete guides and real-world examples
- 🔧 **Easy Configuration** - Sensible defaults with flexible customization
- 🛡️ **Security First** - Built-in authentication, TLS/SSL, CORS support

## 📦 Installation

Add `ex_mcp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.7.0"}
  ]
end
```

Then run:

```bash
mix deps.get
```

## 🚀 Quick Start

### Phoenix Integration (Recommended)

Add MCP server capabilities to your Phoenix app:

```elixir
# In your Phoenix router (lib/my_app_web/router.ex)
defmodule MyAppWeb.Router do
  use MyAppWeb, :router
  
  pipeline :mcp do
    plug :accepts, ["json"]
    # Add your authentication/authorization here
  end
  
  scope "/api/mcp" do
    pipe_through :mcp
    
    # Mount MCP server at /api/mcp
    forward "/", ExMCP.HttpPlug,
      handler: MyApp.MCPHandler,
      server_info: %{name: "my-phoenix-app", version: "1.0.0"},
      sse_enabled: true,
      cors_enabled: true
  end
end

# Create your MCP handler (lib/my_app/mcp_handler.ex)
defmodule MyApp.MCPHandler do
  use ExMCP.Server.Handler
  
  @impl true
  def init(_args), do: {:ok, %{}}
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "my-phoenix-app",
      version: "1.0.0",
      capabilities: %{tools: %{}, resources: %{}}
    }, state}
  end
  
  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        name: "get_user_count",
        description: "Get total number of users",
        input_schema: %{type: "object", properties: %{}}
      }
    ]
    {:ok, tools, state}
  end
  
  @impl true
  def handle_call_tool("get_user_count", _args, state) do
    count = MyApp.Accounts.count_users()
    {:ok, [%{type: "text", text: "Total users: #{count}"}], state}
  end
end
```

**Connect from any MCP client:**
```bash
mcp connect http://localhost:4000/api/mcp
```

### DSL Server (Quickest Way)

Define tools, resources, and prompts declaratively:

```elixir
defmodule MyServer do
  use ExMCP.Server

  deftool "greet" do
    description "Greets a person by name"
    args do
      field :name, :string, required: true, description: "Person to greet"
    end
  end

  defresource "info://about" do
    name "About"
    description "Server information"
    mime_type "text/plain"
  end

  @impl true
  def handle_tool_call("greet", %{"name" => name}, state) do
    {:ok, %{content: [text("Hello, #{name}!")]}, state}
  end

  @impl true
  def handle_resource_read("info://about", _uri, state) do
    {:ok, [text("MyServer v1.0")], state}
  end
end
```

See the [DSL Guide](docs/DSL_GUIDE.md) and [examples/](examples/) for more patterns.

### Standalone MCP Client

```elixir
# Connect to a stdio-based server
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["node", "my-mcp-server.js"]
)

# List available tools
{:ok, tools} = ExMCP.Client.list_tools(client)

# Call a tool
{:ok, result} = ExMCP.Client.call_tool(client, "search", %{
  query: "Elixir programming",
  limit: 10
})
```

### Ultra-Fast Native BEAM Services

For trusted Elixir clusters, use the native BEAM transport:

```elixir
# Create a service using the ExMCP.Service macro
defmodule MyToolService do
  use ExMCP.Service, name: :my_tools

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "ping",
        "description" => "Test tool",
        "inputSchema" => %{"type" => "object", "properties" => %{}}
      }
    ]
    {:ok, %{"tools" => tools}, state}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "ping"}, state) do
    {:ok, %{"content" => [%{"type" => "text", "text" => "Pong!"}]}, state}
  end
end

# Start your service (automatically registers with ExMCP.Native)
{:ok, _} = MyToolService.start_link()

# Direct service calls (~15μs latency)
{:ok, tools} = ExMCP.Native.call(:my_tools, "list_tools", %{})
```

## 📚 Documentation

ExMCP provides comprehensive documentation organized for different needs:

### 🚀 Getting Started
- **[Quick Start Guide](docs/getting-started/QUICKSTART.md)** - Get running in 5 minutes
- **[Quick Reference](docs/getting-started/QUICK_REFERENCE.md)** - One-page operation reference
- **[Migration Guide](docs/getting-started/MIGRATION.md)** - Version upgrade instructions

### 📖 Comprehensive Guides  
- **[User Guide](docs/guides/USER_GUIDE.md)** - Complete feature walkthrough with examples
- **[Phoenix Integration Guide](docs/guides/PHOENIX_GUIDE.md)** - Detailed Phoenix/Plug integration
- **[Configuration Guide](docs/CONFIGURATION.md)** - All configuration options and examples
- **[Transport Guide](docs/TRANSPORT_GUIDE.md)** - Transport selection and optimization
- **[Security Guide](docs/SECURITY.md)** - Authentication, TLS, and security best practices

### 🔧 Development & API
- **[Development Guide](docs/DEVELOPMENT.md)** - Setup, testing, and contributing
- **[API Documentation](https://hexdocs.pm/ex_mcp)** - Complete API reference
- **[Architecture Guide](docs/ARCHITECTURE.md)** - Internal architecture and design decisions
- **[Examples](examples/)** - Real-world implementation patterns

### 📋 Protocol & Specifications
- **[MCP Specifications](docs/mcp-specs/)** - Complete protocol documentation for all versions
- **[Protocol Support Matrix](docs/getting-started/QUICK_REFERENCE.md#protocol-support)** - Feature comparison across versions

## 🎯 Transport Performance

| Transport | Latency | Best For | Use Case |
|-----------|---------|----------|----------|
| **Native BEAM** | ~15μs | Internal services | Elixir cluster communication |
| **stdio** | ~1-5ms | External tools | Subprocess communication |
| **HTTP/SSE** | ~5-20ms | Network clients | Web applications, remote APIs |

## ✨ What's New in v0.7.0

- **MCP 2025-11-25 Support**: Latest protocol version with full spec compliance
- **Streamable HTTP**: Spec-compliant client and server (session IDs, SSE path, protocol version headers)
- **TypeScript SDK Interop**: Verified interoperability with the official TypeScript MCP SDK
- **Client State Machine**: Refactored client with GenStateMachine for better observability
- **Agent Simulation Tests**: Integration tests with MockLLM for agent workflows
- **Conformance Suites**: Automated conformance tests for all 4 protocol versions

See the [CHANGELOG](CHANGELOG.md) for complete details and breaking changes.

## 🤝 Contributing

We welcome contributions! Please see:

- [Development Guide](docs/DEVELOPMENT.md) for setup and testing instructions
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