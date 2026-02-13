# ExMCP

<div align="center">

[![Hex.pm](https://img.shields.io/hexpm/v/ex_mcp.svg)](https://hex.pm/packages/ex_mcp)
[![Documentation](https://img.shields.io/badge/docs-hexdocs-blue.svg)](https://hexdocs.pm/ex_mcp)
[![CI](https://github.com/azmaveth/ex_mcp/workflows/CI/badge.svg)](https://github.com/azmaveth/ex_mcp/actions)
[![Coverage](https://img.shields.io/codecov/c/github/azmaveth/ex_mcp.svg)](https://codecov.io/gh/azmaveth/ex_mcp)
[![License](https://img.shields.io/hexpm/l/ex_mcp.svg)](https://github.com/azmaveth/ex_mcp/blob/master/LICENSE)

**A complete Elixir implementation of the Model Context Protocol (MCP)**

[Getting Started](https://github.com/azmaveth/ex_mcp/tree/master/docs/getting-started) | [User Guide](docs/guides/USER_GUIDE.md) | [API Docs](https://hexdocs.pm/ex_mcp) | [Examples](https://github.com/azmaveth/ex_mcp/tree/master/examples) | [Changelog](CHANGELOG.md)

</div>

---

## Overview

ExMCP is a comprehensive Elixir implementation of the [Model Context Protocol](https://modelcontextprotocol.io/), enabling AI models to securely interact with local and remote resources through a standardized protocol. It provides both client and server implementations with multiple transport options, including native Phoenix integration via Plug compatibility.

## Key Features

- **Full MCP compliance** -- protocol versions 2024-11-05, 2025-03-26, 2025-06-18, and 2025-11-25
- **Multiple transports** -- HTTP/SSE, stdio, and native BEAM (~15us local calls)
- **Phoenix Plug** -- native Phoenix integration with `ExMCP.HttpPlug`
- **DSL and Handler APIs** -- declarative tool/resource/prompt definitions or callback-based handlers
- **OAuth 2.1** -- complete Resource Server implementation
- **OTP-native** -- supervision trees, auto-reconnection with exponential backoff, telemetry
- **2600+ tests** -- comprehensive suite including TypeScript SDK interop

## Installation

Add `ex_mcp` to your dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.7.2"}
  ]
end
```

## Quick Start

### Phoenix Integration

Add MCP server capabilities to your Phoenix app:

```elixir
# In your Phoenix router
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  scope "/api/mcp" do
    forward "/", ExMCP.HttpPlug,
      handler: MyApp.MCPHandler,
      server_info: %{name: "my-phoenix-app", version: "1.0.0"},
      sse_enabled: true,
      cors_enabled: true
  end
end

# Create your MCP handler
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
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "get_user_count",
        description: "Get total number of users",
        inputSchema: %{type: "object", properties: %{}}
      }
    ]
    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("get_user_count", _args, state) do
    count = MyApp.Accounts.count_users()
    {:ok, [%{type: "text", text: "Total users: #{count}"}], state}
  end
end
```

### DSL Server

Define tools, resources, and prompts declaratively:

```elixir
defmodule MyServer do
  use ExMCP.Server

  deftool "greet" do
    meta do
      name "Greet"
      description "Greets a person by name"
    end

    input_schema %{
      type: "object",
      properties: %{name: %{type: "string", description: "Person to greet"}},
      required: ["name"]
    }
  end

  defresource "info://about" do
    meta do
      name "About"
      description "Server information"
    end

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

See the [DSL Guide](docs/DSL_GUIDE.md) and [examples](https://github.com/azmaveth/ex_mcp/tree/master/examples) for more patterns.

### Standalone Client

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

### Native BEAM Transport

For trusted Elixir clusters, use the native BEAM transport:

```elixir
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

# Direct service calls (~15us latency)
{:ok, tools} = ExMCP.Native.call(:my_tools, "list_tools", %{})
```

## Transport Performance

| Transport | Latency | Best For |
|-----------|---------|----------|
| **Native BEAM** | ~15us | Elixir cluster communication |
| **stdio** | ~1-5ms | Subprocess communication |
| **HTTP/SSE** | ~5-20ms | Web applications, remote APIs |

## Documentation

### Getting Started
- **[Quick Start Guide](https://github.com/azmaveth/ex_mcp/blob/master/docs/getting-started/QUICKSTART.md)** -- Get running in 5 minutes
- **[Migration Guide](https://github.com/azmaveth/ex_mcp/blob/master/docs/getting-started/MIGRATION.md)** -- Version upgrade instructions

### Guides
- **[User Guide](docs/guides/USER_GUIDE.md)** -- Complete feature walkthrough
- **[Phoenix Integration](docs/guides/PHOENIX_GUIDE.md)** -- Detailed Phoenix/Plug integration
- **[DSL Guide](docs/DSL_GUIDE.md)** -- Declarative server definitions
- **[Transport Guide](docs/TRANSPORT_GUIDE.md)** -- Transport selection and optimization
- **[Configuration](docs/CONFIGURATION.md)** -- All configuration options
- **[Security](docs/SECURITY.md)** -- Authentication, TLS, and best practices
- **[Troubleshooting](docs/TROUBLESHOOTING.md)** -- Common issues and solutions

### Development & API
- **[Development Guide](docs/DEVELOPMENT.md)** -- Setup, testing, and contributing
- **[API Documentation](https://hexdocs.pm/ex_mcp)** -- Complete API reference
- **[Architecture](docs/ARCHITECTURE.md)** -- Internal design decisions
- **[Examples](https://github.com/azmaveth/ex_mcp/tree/master/examples)** -- Real-world patterns

## Contributing

Contributions welcome! See the [Development Guide](docs/DEVELOPMENT.md) for setup and testing instructions.

1. Fork the repository
2. Create a feature branch
3. Run `make quality` to ensure code quality
4. Submit a pull request

## License

MIT -- see [LICENSE](https://github.com/azmaveth/ex_mcp/blob/master/LICENSE).

## Acknowledgments

- The [Model Context Protocol](https://modelcontextprotocol.io/) specification creators
- The Elixir community for excellent tooling and libraries
- Contributors and early adopters providing feedback
