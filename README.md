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

> ⚠️ **Alpha Software**: This project is currently in alpha stage (v0.5.x). The API is unstable and may change significantly before v1.0 release.

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
- ❌ **Request Cancellation** - Cancel in-flight requests with `notifications/cancelled`
- 📋 **Structured Logging** - RFC 5424 compliant logging with `logging/setLevel` control

### Transport Layers
- 📝 **stdio** - Process communication via standard I/O (official MCP transport)
- 🌐 **HTTP/SSE** - HTTP with optional Server-Sent Events for streaming (official MCP transport)
- ⚡ **Native BEAM** - Direct process communication within Elixir clusters (ExMCP extension)
  - 🚀 **Zero Serialization** - Direct process-to-process communication with no JSON overhead
  - 📊 **Registry Discovery** - Built-in service discovery using Erlang Registry
  - 🔄 **OTP Integration** - Full supervision tree and fault tolerance support
  - 🌐 **Node Distribution** - Automatic cross-node communication support
  - ⚡ **Performance** - ~15μs local calls, ~50μs cross-node calls
- 🔌 **WebSocket** - WebSocket client for real-time communication (ExMCP extension)

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
- Standard transports (stdio, Streamable HTTP)
- Protocol encoding/decoding
- OAuth 2.1 authorization

### ExMCP Extensions (Elixir-specific)
Features unique to ExMCP:
- Native BEAM transport for direct process communication
- Automatic MCP server discovery
- Multi-server management
- Batch operations
- Auto-reconnection
- Resource unsubscribe (`resources/unsubscribe` - not in MCP spec)

### Draft Features (Experimental)

These features are from the draft MCP specification and may not be compatible with all MCP implementations:

- **Structured Tool Output** - Tools can define `outputSchema` and return `structuredContent` alongside regular content

⚠️ **Note**: These features are not part of the official MCP 2025-03-26 specification and should only be used when connecting to servers that explicitly support draft features.

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

# Connect with authentication (Streamable HTTP)
{:ok, secure_client} = ExMCP.Client.start_link(
  transport: :http,
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

### Streamable HTTP Transport

For HTTP-based communication with optional Server-Sent Events (SSE) streaming:

```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,  # Streamable HTTP transport
  port: 8080,
  path: "/mcp"
)

# Client
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8080",
  endpoint: "/mcp/v1",  # Optional, defaults to "/mcp/v1"
  headers: [{"Authorization", "Bearer token"}]
)
```

### Native BEAM Transport

For direct Elixir/Erlang process communication within trusted clusters:

```elixir
# Register a service (typically in your GenServer's init/1)
defmodule MyToolService do
  use GenServer

  def init(_) do
    ExMCP.Transport.Native.register_service(:my_tool_service)
    {:ok, %{}}
  end

  def handle_call({:mcp_request, %{"method" => "list_tools"}}, _from, state) do
    tools = [%{"name" => "ping", "description" => "Test tool"}]
    {:reply, {:ok, %{"tools" => tools}}, state}
  end
end

# Start your service
{:ok, _} = MyToolService.start_link([])

# Direct service calls
{:ok, tools} = ExMCP.Transport.Native.call(:my_tool_service, "list_tools", %{})

# Cross-node communication (automatic)
{:ok, result} = ExMCP.Transport.Native.call(
  {:my_service, :"node@host"}, 
  "call_tool", 
  %{"name" => "calculator", "arguments" => %{"operation" => "add", "a" => 1, "b" => 2}}
)

# Fire-and-forget notifications
:ok = ExMCP.Transport.Native.notify(:event_service, "resource_updated", %{
  "uri" => "file:///config.json",
  "type" => "modified"
})

# Service discovery
services = ExMCP.Transport.Native.list_services()
available? = ExMCP.Transport.Native.service_available?(:my_service)
```

## 🎯 Key Features

### Native BEAM Transport Advantages

ExMCP's Native BEAM transport leverages OTP's built-in features for maximum performance and reliability:

#### 🚀 Zero Serialization Overhead

Direct process-to-process communication with no JSON encoding/decoding for local calls:

```elixir
# Data passed directly as Elixir terms between processes
large_data = File.read!("large_dataset.json")  # 10MB file

# No serialization overhead - data passed by reference
{:ok, result} = ExMCP.Transport.Native.call(:data_service, "process_data", %{
  "data" => large_data
})

# Performance characteristics:
# - Local calls: ~15μs latency
# - Cross-node calls: ~50μs latency  
# - Memory overhead: Single Registry entry per service
```

#### 📊 Built-in Service Discovery

Uses Erlang's Registry for automatic service discovery across the cluster:

```elixir
# Register services
ExMCP.Transport.Native.register_service(:calculator)
ExMCP.Transport.Native.register_service(:file_manager)

# Discover all available services
services = ExMCP.Transport.Native.list_services()
#=> [{:calculator, #PID<0.123.0>, %{registered_at: ~U[...]}}, ...]

# Check service availability
if ExMCP.Transport.Native.service_available?(:calculator) do
  {:ok, result} = ExMCP.Transport.Native.call(:calculator, "add", %{"a" => 1, "b" => 2})
end
```

#### 🔄 OTP Fault Tolerance

Leverages OTP's proven fault tolerance and supervision patterns:

```elixir
# Services are normal GenServers with full OTP supervision
defmodule MyToolService do
  use GenServer

  def init(_) do
    # Register with native transport
    ExMCP.Transport.Native.register_service(:my_tools)
    {:ok, %{}}
  end

  def handle_call({:mcp_request, message}, _from, state) do
    # Handle MCP requests with normal GenServer patterns
    {:reply, {:ok, result}, state}
  end
end

# Add to supervision tree
children = [
  {MyToolService, []}
]

Supervisor.start_link(children, strategy: :one_for_one)
```

#### 🌐 Cross-Node Distribution

Automatic support for distributed Elixir clusters:

```elixir
# Call services on remote nodes transparently
{:ok, result} = ExMCP.Transport.Native.call(
  {:data_service, :"worker@cluster.local"},
  "process_dataset",
  %{"dataset_id" => "abc123"}
)

# Works seamlessly with Elixir clustering
Node.connect(:"worker@cluster.local")
# Services on remote nodes are now available
```

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

### Request Cancellation

Cancel in-flight requests to improve responsiveness:

```elixir
# Start a slow operation
task = Task.async(fn ->
  ExMCP.Client.call_tool(client, "slow_operation", %{})
end)

# Get pending requests
pending = ExMCP.Client.get_pending_requests(client)
#=> [12345]

# Cancel the request
:ok = ExMCP.Client.send_cancelled(client, 12345, "User clicked cancel")

# The task will return {:error, :cancelled}
result = Task.await(task)
#=> {:error, :cancelled}
```

### Progress Tracking and Metadata

Track long-running operations with progress tokens and pass custom metadata:

```elixir
# Call a tool with progress token
{:ok, result} = ExMCP.Client.call_tool(
  client,
  "long_operation",
  %{"data" => "..."},
  meta: %{"progressToken" => "op-123"}
)

# Pass custom metadata for tracing/debugging
{:ok, tools} = ExMCP.Client.list_tools(
  client,
  meta: %{
    "requestId" => "req-456",
    "userId" => "user-789",
    "trace" => true
  }
)

# In your handler, extract metadata
def handle_call_tool("long_operation", arguments, state) do
  {meta, args} = Map.pop(arguments, "_meta")
  
  if progress_token = meta && meta["progressToken"] do
    # Report progress
    ExMCP.Server.notify_progress(self(), progress_token, 50, 100)
  end
  
  # Process operation...
end
```

### Structured Logging

Comprehensive logging with automatic sanitization and dual output:

```elixir
# Configure global log level
ExMCP.Logging.set_global_level("debug")

# Client can control server log level
{:ok, _} = ExMCP.Client.set_log_level(client, "warning")

# Server logging with automatic security sanitization
defmodule MyServer do
  use ExMCP.Server.Handler

  @impl true
  def handle_call_tool("login", params, state) do
    server = self()
    
    # This logs to both MCP clients AND Elixir Logger
    # Sensitive data is automatically sanitized
    ExMCP.Logging.info(server, "User login attempt", %{
      username: params["username"],
      password: params["password"],  # Will be sanitized to "***"
      timestamp: DateTime.utc_now()
    })
    
    # ... handle login logic
    {:ok, result, state}
  end
end

# All RFC 5424 log levels supported
ExMCP.Logging.debug(server, "Debug info", %{details: "..."})
ExMCP.Logging.info(server, "Operation completed") 
ExMCP.Logging.warning(server, "Deprecated feature used")
ExMCP.Logging.error(server, "Operation failed", %{error: "connection_timeout"})
ExMCP.Logging.critical(server, "System component failure")
```

## ⚙️ Configuration

### Protocol Version

ExMCP supports multiple MCP protocol versions. Configure the preferred version in your `config/config.exs`:

```elixir
config :ex_mcp,
  # Options: "2024-11-05", "2025-03-26", "draft"
  protocol_version: "2025-03-26"  # Default: latest stable
```

Version capabilities:
- **2024-11-05**: Base MCP features
- **2025-03-26**: Adds resource subscriptions, logging control
- **draft**: Experimental features (batch requests, elicitation)

### Production Logger Configuration

ExMCP includes security audit logging with structured metadata. Configure logging appropriately for your environment:

#### Development Configuration

```elixir
# config/dev.exs
config :logger, :console,
  metadata: [:request_id, :tag, :audit, :client_id, :reason, :registration_type]
```

#### Production Configuration Options

**Option 1: JSON Structured Logging (Recommended)**

```elixir
# In mix.exs
{:logger_json, "~> 5.1"}

# In config/prod.exs
config :logger,
  backends: [LoggerJSON]

config :logger_json, :backend,
  metadata: :all,  # Captures ALL metadata automatically
  json_encoder: Jason,
  formatter: LoggerJSON.Formatters.GoogleCloudLogger
```

**Option 2: Separate Security Audit Logs**

```elixir
# config/prod.exs
config :logger,
  backends: [:console, {LoggerFileBackend, :security_audit}]

config :logger, :security_audit,
  path: "/var/log/ex_mcp/security_audit.log",
  level: :info,
  format: "$date $time [$level] $metadata $message\n",
  metadata: [:tag, :audit, :client_id, :reason, :registration_type],
  metadata_filter: [tag: :security_audit]  # Only security logs
```

**Option 3: External Log Aggregation**

```elixir
# For ELK Stack, Datadog, etc.
{:logstash_logger_backend, "~> 3.0"}

config :logger,
  backends: [{LogstashLoggerBackend, :logstash}]

config :logger, :logstash,
  host: "logstash.example.com",
  port: 5514,
  metadata: :all,
  type: "ex_mcp_security"
```

#### Logger Metadata Fields

ExMCP uses the following metadata fields for security and debugging:

- **`:tag`** - Log categorization (e.g., `:security_audit`, `:client_registration`)
- **`:audit`** - Detailed audit log entries with timestamps and actions
- **`:client_id`** - Client identification for request tracking
- **`:reason`** - Failure reasons and error details
- **`:registration_type`** - Client registration type (`:static` or `:dynamic`)
- **`:request_id`** - Correlation ID for distributed tracing

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

### Test Suites

ExMCP uses a comprehensive test tagging strategy for efficient test execution:

```bash
# Fast unit tests (default, ~5s)
mix test.suite unit

# MCP specification compliance tests
mix test.suite compliance

# Integration tests with real components
mix test.suite integration

# Transport-specific tests
mix test --only beam              # BEAM transport tests
mix test --only http              # Streamable HTTP transport tests
mix test --only stdio             # stdio transport tests

# Feature-specific tests
mix test --only security          # Security tests
mix test --only progress          # Progress notification tests
mix test --only resources         # Resource management tests

# Development workflows
mix test --include slow           # Include slow tests
mix test --exclude integration    # Skip integration tests

# CI/comprehensive testing
mix test.suite ci                 # CI-appropriate tests
mix test.suite all               # All tests including slow ones

# List all available tags
mix test.tags
```

See [test/TAGGING_STRATEGY.md](test/TAGGING_STRATEGY.md) for complete documentation.

### Code Quality Tools

- **Formatter** - Elixir's built-in code formatter
- **Credo** - Static code analysis
- **Dialyzer** - Type checking
- **Sobelow** - Security analysis
- **ExCoveralls** - Test coverage
- **Git Hooks** - Pre-commit and pre-push checks
- **Test Tagging** - Organized test execution with 8 predefined suites

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

### Projects That Influenced Our Design

ExMCP's design has been influenced by several excellent MCP implementations:

- **[Hermes MCP](https://github.com/deepcodeai/hermes_mcp)** - Inspired our telemetry integration and structured error handling approaches
- **[MCP SSE](https://github.com/example/mcp-sse)** - Influenced our SSE transport implementation and HTTP handling patterns
- **[Ash AI](https://github.com/ash-project/ash_ai)** - Inspired our tool DSL design for minimal boilerplate
- **[MCPhoenix](https://github.com/example/mcphoenix)** - Influenced our multi-server architecture and dynamic tool loading concepts
- **[mcp_ex](https://github.com/example/mcp_ex)** - Provided insights on test transport design and schema validation approaches

We're grateful to these projects for pushing the boundaries of MCP implementations and sharing their innovations with the community.

---

<div align="center">
Made with ❤️ for the Elixir community
</div>