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

> ⚠️ **Alpha Software**: This project is currently in alpha stage (v0.6.x). The API is unstable and may change significantly before v1.0 release.

## Overview

ExMCP is a comprehensive Elixir implementation of the [Model Context Protocol](https://modelcontextprotocol.io/), enabling AI models to securely interact with local and remote resources through a standardized protocol. It provides both client and server implementations with multiple transport options, **including native Phoenix integration via Plug compatibility**, making it easy to build MCP-compliant tools and services in Elixir.

## ✨ Features

### Core Protocol Support
- 🚀 **Multiple MCP Versions** - Supports protocol versions 2024-11-05, 2025-03-26, and 2025-06-18
- 🛠️ **Tools** - Register and execute functions with type-safe parameters
- 📚 **Resources** - List and read data from various sources
- 🎯 **Prompts** - Manage reusable prompt templates
- 🤖 **Sampling** - Direct LLM integration for response generation
- 🌳 **Roots** - URI-based resource boundaries
- 🔔 **Subscriptions** - Monitor resources for changes
- 📦 **Batch Requests** - Send multiple requests in a single call
- 🔁 **Bi-directional Communication** - Servers can make requests to clients
- 👤 **Human-in-the-Loop** - Approval flows for sensitive operations
- ❌ **Request Cancellation** - Cancel in-flight requests with `notifications/cancelled`
- 📋 **Structured Logging** - RFC 5424 compliant logging with `logging/setLevel` control

### Phoenix & Web Integration
- 🔌 **Phoenix Plug** - Native Phoenix integration with `ExMCP.HttpPlug` 
- 🌐 **HTTP/SSE** - HTTP with optional Server-Sent Events for streaming (official MCP transport)
- 📋 **CORS Support** - Built-in CORS headers for web clients
- 🎯 **Session Management** - Automatic session tracking for SSE connections
- 🛡️ **Pipeline Integration** - Works with Phoenix authentication/authorization pipelines

### Transport Layers  
- 📝 **stdio** - Process communication via standard I/O (official MCP transport)
- 🌐 **HTTP/SSE** - HTTP with optional Server-Sent Events for streaming (official MCP transport)

### Native BEAM Service Dispatcher (ExMCP Extension)
- ⚡ **ExMCP.Native** - Ultra-fast service dispatcher for trusted Elixir clusters
  - 🚀 **Zero Serialization** - Direct GenServer.call() with no JSON overhead
  - 🌐 **Horde.Registry** - Distributed service discovery via gossip protocol
  - 🔄 **OTP Integration** - Full supervision tree and fault tolerance support
  - 📡 **Cross-Node** - Automatic distributed communication (~50μs latency)
  - ⚡ **Performance** - ~15μs local calls vs 1-5ms HTTP equivalent
  - 🛠️ **Service Macro** - `use ExMCP.Service` for automatic registration
  - 🔧 **Optional Resilience** - `ExMCP.Resilience` for retry/fallback patterns

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
- **ExMCP.Native** - High-performance service dispatcher with Horde.Registry
- **ExMCP.Service** - `use` macro for automatic service registration
- **ExMCP.Resilience** - Optional resilience patterns (retry, fallback, circuit breaker)
- Automatic MCP server discovery
- Multi-server management
- Batch operations
- Auto-reconnection
- Resource unsubscribe (`resources/unsubscribe` - not in MCP spec)

## 📋 Protocol Version Support

ExMCP implements three versions of the Model Context Protocol, each with different feature sets:

| Feature | 2024-11-05 | 2025-03-26 | 2025-06-18 |
|---------|:----------:|:----------:|:----------:|
| **Core Features** | | | |
| Tools (`tools/list`, `tools/call`) | ✅ | ✅ | ✅ |
| Resources (`resources/list`, `resources/read`) | ✅ | ✅ | ✅ |
| Prompts (`prompts/list`, `prompts/get`) | ✅ | ✅ | ✅ |
| Completion (`completion/complete`) | ✅ | ✅ | ✅ |
| **Communication** | | | |
| Bi-directional requests | ✅ | ✅ | ✅ |
| Request cancellation | ✅ | ✅ | ✅ |
| Progress notifications | ✅ | ✅ | ✅ |
| **2025-03-26 Features** | | | |
| Resource subscriptions (`resources/subscribe`) | ❌ | ✅ | ✅ |
| Roots (`roots/list`) | ❌ | ✅ | ✅ |
| Structured logging (`logging/setLevel`) | ❌ | ✅ | ✅ |
| Tool annotations (`readOnlyHint`, `destructiveHint`) | ❌ | ✅ | ✅ |
| **2025-06-18 Features** | | | |
| Structured tool output (`outputSchema`) | ❌ | ❌ | ✅ |
| Elicitation support | ❌ | ❌ | ✅ |
| Resource links in tool results | ❌ | ❌ | ✅ |
| OAuth 2.1 Resource Server | ❌ | ❌ | ✅ |
| **ExMCP Extensions** | | | |
| Native BEAM dispatcher | ✅ | ✅ | ✅ |
| Batch requests | ✅ | ✅ | ❌ |
| Resource unsubscribe | ✅ | ✅ | ✅ |
| Auto-reconnection | ✅ | ✅ | ✅ |

### Version Selection

Configure your preferred version in `config/config.exs`:

```elixir
config :ex_mcp,
  protocol_version: "2025-03-26"  # Options: "2024-11-05", "2025-03-26", "2025-06-18"
```

**Recommendations:**
- **Production**: Use `"2025-03-26"` for the latest stable features
- **Compatibility**: Use `"2024-11-05"` for maximum compatibility
- **Latest**: Use `"2025-06-18"` for the newest stable features


## 📦 Installation

Add `ex_mcp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.6.0"}
  ]
end
```

Then run:

```bash
mix deps.get
```

## ✨ What's New in v0.6.0

v0.6.0 brings significant improvements and new features:

- **Enhanced Security**: Complete OAuth 2.1 Resource Server implementation
- **MCP 2025-06-18 Support**: Latest protocol version with structured tool output
- **Improved Testing**: Comprehensive compliance test suite
- **Better Performance**: Optimized native BEAM transport
- **Documentation**: Enhanced guides and examples

See the [CHANGELOG](CHANGELOG.md) for complete details and breaking changes.

## 🚀 Quick Start

### Phoenix Integration (Plug Compatible)

ExMCP provides seamless Phoenix integration via `ExMCP.HttpPlug`, making it easy to add MCP server capabilities to existing Phoenix applications:

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

**Features:**
- ✅ **Drop-in compatibility** with existing Phoenix apps
- ✅ **SSE streaming** for real-time MCP communication  
- ✅ **CORS support** for web clients
- ✅ **Automatic session management** with connection tracking
- ✅ **Standard Plug interface** - works with any Cowboy/Phoenix setup

**Connect from any MCP client:**
```bash
# Using the MCP SDK or compatible client
mcp connect http://localhost:4000/api/mcp
```

**Why integrate MCP with Phoenix?**
- 🤖 **AI-First Development** - Make your Phoenix app's data and functionality available to AI models
- 📊 **Real-time AI Tools** - Expose database queries, business logic, and APIs as AI-callable tools
- 🔄 **Bi-directional AI** - Allow AI to both read data and trigger actions in your application
- 🛡️ **Secure by Design** - Leverage Phoenix's existing authentication and authorization
- 📈 **Future-Proof** - Build for the AI-native application architecture

### Standalone Server (Non-Phoenix)

For standalone MCP servers without Phoenix:

```elixir
# Start standalone HTTP server
{:ok, _} = Plug.Cowboy.http(ExMCP.HttpPlug, [
  handler: MyApp.MCPHandler,
  server_info: %{name: "standalone-server", version: "1.0.0"}
], port: 4000)
```

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

### Native BEAM Service Dispatcher

For ultra-fast service communication within trusted Elixir clusters:

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

  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

# Start your service (automatically registers with ExMCP.Native)
{:ok, _} = MyToolService.start_link()

# Direct service calls
{:ok, tools} = ExMCP.Native.call(:my_tools, "list_tools", %{})

# Cross-node communication (automatic via Horde.Registry)
{:ok, result} = ExMCP.Native.call(
  {:my_service, :"node@host"}, 
  "tools/call", 
  %{"name" => "calculator", "arguments" => %{"a" => 1, "b" => 2}}
)

# Fire-and-forget notifications
:ok = ExMCP.Native.notify(:event_service, "resource_updated", %{
  "uri" => "file:///config.json",
  "type" => "modified"
})

# Service discovery (distributed via Horde)
services = ExMCP.Native.list_services()
available? = ExMCP.Native.service_available?(:my_service)
```

## 🎯 Key Features

### Native BEAM Service Dispatcher Advantages

ExMCP.Native leverages OTP and Horde for maximum performance and reliability:

#### 🚀 Zero Serialization Overhead

Direct GenServer.call() with no JSON encoding/decoding for local calls:

```elixir
# Data passed directly as Elixir terms between processes
large_data = File.read!("large_dataset.json")  # 10MB file

# No serialization overhead - data passed by reference
{:ok, result} = ExMCP.Native.call(:data_service, "process_data", %{
  "data" => large_data
})

# Performance characteristics:
# - Local calls: ~15μs latency
# - Cross-node calls: ~50μs latency  
# - Memory overhead: Single Horde.Registry entry per service
```

#### 📊 Distributed Service Discovery

Uses Horde.Registry for automatic service discovery across the cluster:

```elixir
# Services automatically register when using ExMCP.Service macro
defmodule CalculatorService do
  use ExMCP.Service, name: :calculator
  # Service automatically registered on startup
end

# Discover all available services (across all nodes)
services = ExMCP.Native.list_services()
#=> [{:calculator, #PID<0.123.0>, %{registered_at: ~U[...]}}, ...]

# Check service availability
if ExMCP.Native.service_available?(:calculator) do
  {:ok, result} = ExMCP.Native.call(:calculator, "add", %{"a" => 1, "b" => 2})
end
```

#### 🔧 Optional Resilience Patterns

Add resilience when needed without compromising core performance:

```elixir
# Retry with exponential backoff
{:ok, result} = ExMCP.Resilience.call_with_retry(
  :flaky_service,
  "process_data",
  %{"input" => "data"},
  max_attempts: 3,
  backoff: :exponential
)

# Fallback for unavailable services
result = ExMCP.Resilience.call_with_fallback(
  :unreliable_service,
  "get_data",
  %{},
  fallback: fn -> {:ok, %{"data" => "cached_value"}} end
)
```

#### 🔄 OTP Fault Tolerance

Leverages OTP's proven fault tolerance and supervision patterns:

```elixir
# Services use ExMCP.Service macro for automatic lifecycle management
defmodule MyToolService do
  use ExMCP.Service, name: :my_tools

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    # Handle MCP requests with normal patterns
    {:ok, %{"tools" => []}, state}
  end
end

# Add to supervision tree - registration/unregistration is automatic
children = [
  {MyToolService, []}
]

Supervisor.start_link(children, strategy: :one_for_one)
```

#### 🌐 Cross-Node Distribution

Automatic support for distributed Elixir clusters via Horde:

```elixir
# Call services on remote nodes transparently
{:ok, result} = ExMCP.Native.call(
  {:data_service, :"worker@cluster.local"},
  "process_dataset",
  %{"dataset_id" => "abc123"}
)

# Works seamlessly with Elixir clustering
Node.connect(:"worker@cluster.local")
# Services on remote nodes are automatically discovered via Horde gossip
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
  # Options: "2024-11-05", "2025-03-26", "2025-06-18"
  protocol_version: "2025-03-26"  # Default: latest stable
```

See the [Protocol Version Support](#-protocol-version-support) section above for detailed feature comparison across versions.

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

- 🏁 **[Getting Started](docs/getting-started/)** - Quick start guides, migration help, and reference
- 🚀 **[Phoenix Integration Guide](docs/guides/PHOENIX_GUIDE.md)** - Complete guide for Phoenix/Plug integration
- 📖 **[User Guide](docs/guides/USER_GUIDE.md)** - Comprehensive guide with examples  
- 🎨 **[DSL Guide](docs/DSL_GUIDE.md)** - Complete DSL reference with meta block design
- 🔧 **[API Documentation](https://hexdocs.pm/ex_mcp)** - Detailed API reference
- 🆕 **[v2 API Reference](docs/API_V2_REFERENCE.md)** - New v2 API with structured responses
- 🏗️ **[v2 Architecture](docs/V2_ARCHITECTURE.md)** - v2 design and architecture guide
- 🔄 **[Migration Guide](docs/getting-started/MIGRATION.md)** - Version migration guide
- 🔐 **[Security Guide](docs/SECURITY.md)** - Authentication, TLS, and security best practices
- 📂 **[Examples](examples/)** - Complete working examples

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

### Test Process Cleanup

Tests that start servers can sometimes leave processes running if they crash. ExMCP provides several tools to clean up these stray processes:

```bash
# Clean up before running tests (automatic with make test)
mix test.cleanup

# Manual cleanup with verbose output
mix test.cleanup --verbose

# Dry run to see what would be cleaned
mix test.cleanup --dry-run

# Alternative bash script
./scripts/cleanup_tests.sh

# Clean up as part of test run
make test  # Automatically runs cleanup first

# Skip automatic cleanup if needed
SKIP_TEST_CLEANUP=true mix test
```

The cleanup tools will:
- Stop any Cowboy listeners from tests
- Kill registered test processes
- Free up commonly used test ports (8080-8085, 9000-9002)
- Clean up stray beam.smp processes from test runs

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