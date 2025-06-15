# ExMCP User Guide

A comprehensive guide to using ExMCP, the Elixir implementation of the Model Context Protocol.

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Quick Start](#quick-start)
4. [Core Concepts](#core-concepts)
5. [Building MCP Servers](#building-mcp-servers)
6. [Building MCP Clients](#building-mcp-clients)
   - [Basic Client Usage](#basic-client-usage)
   - [Batch Requests](#batch-requests)
   - [Bi-directional Communication](#bi-directional-communication)
   - [Human-in-the-Loop Approval](#human-in-the-loop-approval)
7. [Transport Layers](#transport-layers)
8. [Advanced Features](#advanced-features)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

## Introduction

ExMCP is a complete Elixir implementation of the Model Context Protocol (MCP), enabling AI models to securely interact with local and remote resources through a standardized protocol. It supports all major MCP features including tools, resources, prompts, sampling, and the latest roots and subscription capabilities.

### Key Features

- **Full Protocol Support**: Implements MCP specification version 2025-03-26
- **Multiple Transports**: stdio, Streamable HTTP (with Server-Sent Events), WebSocket, and native BEAM
- **Both Client and Server**: Build MCP servers or connect to existing ones
- **OTP Integration**: Built on Elixir's OTP principles for reliability
- **Type Safety**: Comprehensive type specifications throughout
- **Extensible**: Easy to add custom tools, resources, and prompts

## Installation

Add `ex_mcp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.5.0"}
  ]
end
```

Then run:

```bash
mix deps.get
```

## Quick Start

### Creating a Simple MCP Server

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
        name: "hello",
        description: "Says hello",
        input_schema: %{
          type: "object",
          properties: %{
            name: %{type: "string", description: "Name to greet"}
          }
        }
      }
    ]
    {:ok, tools, state}
  end

  @impl true
  def handle_call_tool("hello", %{"name" => name}, state) do
    {:ok, [%{type: "text", text: "Hello, #{name}!"}], state}
  end

  # ... implement other required callbacks
end

# Start the server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyServer,
  transport: :stdio
)
```

### Creating a Simple MCP Client

```elixir
# Connect to a server
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["node", "some-mcp-server.js"]
)

# List available tools
{:ok, tools} = ExMCP.Client.list_tools(client)

# Call a tool
{:ok, result} = ExMCP.Client.call_tool(client, "hello", %{name: "World"})
```

## Core Concepts

### The Model Context Protocol

MCP enables AI models to interact with external systems through:

- **Tools**: Functions that can be called with parameters
- **Resources**: Data sources that can be read
- **Prompts**: Templates for generating model interactions
- **Sampling**: Direct LLM integration for response generation
- **Roots**: URI-based boundaries for organizing resources
- **Subscriptions**: Monitor resources for changes

### Client-Server Architecture

MCP follows a client-server model:

- **Clients** (typically AI assistants) connect to servers to access functionality
- **Servers** expose tools, resources, and prompts to clients
- Communication happens over various transport layers

### Request-Response Pattern

All MCP interactions follow JSON-RPC 2.0:

```json
// Request
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {"name": "hello", "arguments": {"name": "World"}},
  "id": 1
}

// Response
{
  "jsonrpc": "2.0",
  "result": {"content": [{"type": "text", "text": "Hello, World!"}]},
  "id": 1
}
```

## Building MCP Servers

### Server Handler Behaviour

All MCP servers implement the `ExMCP.Server.Handler` behaviour:

```elixir
defmodule MyHandler do
  use ExMCP.Server.Handler

  # Required callbacks
  @impl true
  def init(args), do: {:ok, initial_state}

  @impl true
  def handle_initialize(params, state), do: {:ok, server_info, state}

  @impl true
  def handle_list_tools(state), do: {:ok, tools, state}

  @impl true
  def handle_call_tool(name, arguments, state), do: {:ok, result, state}

  @impl true
  def handle_list_resources(state), do: {:ok, resources, state}

  @impl true
  def handle_read_resource(uri, state), do: {:ok, content, state}

  @impl true
  def handle_list_prompts(state), do: {:ok, prompts, state}

  @impl true
  def handle_get_prompt(name, arguments, state), do: {:ok, messages, state}

  # Optional callbacks for advanced features
  @impl true
  def handle_list_roots(state), do: {:ok, roots, state}

  @impl true
  def handle_subscribe_resource(uri, state), do: {:ok, %{}, state}

  @impl true
  def handle_unsubscribe_resource(uri, state), do: {:ok, %{}, state}

  @impl true
  def handle_create_message(params, state), do: {:ok, message, state}
end
```

### Implementing Tools

Tools allow clients to execute functions:

```elixir
@impl true
def handle_list_tools(state) do
  tools = [
    %{
      name: "file_search",
      description: "Search for files by pattern",
      input_schema: %{
        type: "object",
        properties: %{
          pattern: %{type: "string", description: "Glob pattern"},
          path: %{type: "string", description: "Search path"}
        },
        required: ["pattern"]
      },
      # Tool annotations (new in MCP 2025-03-26)
      readOnlyHint: true,
      destructiveHint: false,
      costHint: :low
    }
  ]
  {:ok, tools, state}
end

@impl true
def handle_call_tool("file_search", args, state) do
  pattern = args["pattern"]
  path = args["path"] || "."
  
  files = Path.wildcard(Path.join(path, pattern))
  result = [%{
    type: "text",
    text: "Found #{length(files)} files:\n#{Enum.join(files, "\n")}"
  }]
  
  {:ok, result, state}
end
```

### Implementing Resources

Resources provide data that clients can read:

```elixir
@impl true
def handle_list_resources(state) do
  resources = [
    %{
      uri: "file:///config.json",
      name: "Configuration",
      description: "Application configuration",
      mimeType: "application/json"
    }
  ]
  {:ok, resources, state}
end

@impl true
def handle_read_resource("file:///config.json", state) do
  content = %{
    uri: "file:///config.json",
    mimeType: "application/json",
    text: File.read!("config.json")
  }
  {:ok, content, state}
end
```

### Implementing Prompts

Prompts are templates for model interactions:

```elixir
@impl true
def handle_list_prompts(state) do
  prompts = [
    %{
      name: "code_review",
      description: "Review code for best practices",
      arguments: [
        %{
          name: "language",
          description: "Programming language",
          required: true
        }
      ]
    }
  ]
  {:ok, prompts, state}
end

@impl true
def handle_get_prompt("code_review", %{"language" => lang}, state) do
  messages = [
    %{
      role: "user",
      content: %{
        type: "text",
        text: "Please review the following #{lang} code for best practices..."
      }
    }
  ]
  {:ok, messages, state}
end
```

### Implementing Roots (New Feature)

Roots define URI boundaries for organizing resources:

```elixir
@impl true
def handle_list_roots(state) do
  roots = [
    %{uri: "file:///home/user/projects", name: "Projects"},
    %{uri: "https://api.example.com/v1", name: "API"}
  ]
  {:ok, roots, state}
end

# Notify clients when roots change
def add_root(server, root) do
  # Update state...
  ExMCP.Server.notify_roots_changed(server)
end
```

### Resource Subscriptions (New Feature)

Allow clients to monitor resource changes:

```elixir
@impl true
def handle_subscribe_resource(uri, state) do
  # Track subscription
  subscriptions = MapSet.put(state.subscriptions, uri)
  {:ok, %{}, %{state | subscriptions: subscriptions}}
end

# When resource changes
def update_resource(server, uri, new_content) do
  # Update resource...
  ExMCP.Server.notify_resource_updated(server, uri)
end
```

### Progress Notifications

For long-running operations:

```elixir
@impl true
def handle_call_tool("long_task", %{"_progressToken" => token}, state) do
  # Start async task
  Task.start(fn ->
    for i <- 1..100 do
      ExMCP.Server.notify_progress(self(), token, i, 100)
      Process.sleep(100)
    end
  end)
  
  {:ok, [%{type: "text", text: "Task started"}], state}
end
```

### Sampling/LLM Integration

For servers that provide LLM capabilities:

```elixir
@impl true
def handle_create_message(params, state) do
  %{
    "messages" => messages,
    "max_tokens" => max_tokens
  } = params
  
  # Call your LLM
  response = call_llm(messages, max_tokens)
  
  result = %{
    role: "assistant",
    content: %{type: "text", text: response}
  }
  
  {:ok, result, state}
end
```

### Server-to-Client Requests

Servers can make requests back to clients:

```elixir
# Ping the client
{:ok, _} = ExMCP.Server.ping(server)

# Request client's roots
{:ok, roots} = ExMCP.Server.list_roots(server)

# Ask client to sample an LLM
{:ok, response} = ExMCP.Server.create_message(server, %{
  "messages" => [
    %{"role" => "user", "content" => "What is the weather?"}
  ],
  "modelPreferences" => %{
    "hints" => ["gpt-4", "claude-3"],
    "temperature" => 0.7
  }
})

# The client must have a handler implemented to respond to these requests
```

## Building MCP Clients

### Connecting to Servers

```elixir
# Connect to a stdio server
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["python", "mcp_server.py"],
  args: ["--config", "prod.json"]
)

# Connect to a Streamable HTTP server
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8080",
  endpoint: "/mcp/v1"  # Optional: specify custom endpoint (defaults to "/mcp/v1")
)

# Connect to a BEAM server
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: {:global, :my_mcp_server}
)
```

### Using Tools

```elixir
# List available tools
{:ok, tools} = ExMCP.Client.list_tools(client)

Enum.each(tools, fn tool ->
  IO.puts("Tool: #{tool.name} - #{tool.description}")
end)

# Call a tool
{:ok, result} = ExMCP.Client.call_tool(
  client,
  "search",
  %{query: "elixir metaprogramming"},
  # Optional progress callback
  _progress_token = "search-123"
)

# Handle the result
Enum.each(result, fn
  %{type: "text", text: text} ->
    IO.puts(text)
  %{type: "image", data: data, mimeType: mime} ->
    # Handle image content
end)
```

### Working with Resources

```elixir
# List resources
{:ok, resources} = ExMCP.Client.list_resources(client)

# Read a resource
{:ok, content} = ExMCP.Client.read_resource(client, "file:///data.json")

case content do
  %{text: text} -> 
    IO.puts("Text content: #{text}")
  %{blob: blob} ->
    IO.puts("Binary content: #{byte_size(blob)} bytes")
end
```

### Using Prompts

```elixir
# List prompts
{:ok, prompts} = ExMCP.Client.list_prompts(client)

# Get a prompt
{:ok, messages} = ExMCP.Client.get_prompt(
  client,
  "analyze_code",
  %{language: "elixir", complexity: "high"}
)

# Use with your LLM
response = MyLLM.chat(messages)
```

### Working with Roots

```elixir
# List available roots
{:ok, roots} = ExMCP.Client.list_roots(client)

Enum.each(roots, fn root ->
  IO.puts("Root: #{root.name} at #{root.uri}")
end)
```

### Resource Subscriptions

```elixir
# Subscribe to resource changes
{:ok, _} = ExMCP.Client.subscribe_resource(client, "file:///config.json")

# Handle notifications in your client process
def handle_info({:mcp_notification, "resource/updated", %{"uri" => uri}}, state) do
  IO.puts("Resource updated: #{uri}")
  # Re-read the resource
  {:ok, content} = ExMCP.Client.read_resource(state.client, uri)
  # Process updated content...
  {:noreply, state}
end

# Unsubscribe when done
{:ok, _} = ExMCP.Client.unsubscribe_resource(client, "file:///config.json")
```

### Using Sampling

```elixir
# For servers that support LLM sampling
{:ok, response} = ExMCP.Client.create_message(
  client,
  %{
    messages: [
      %{role: "user", content: %{type: "text", text: "Explain quantum computing"}}
    ],
    max_tokens: 500,
    temperature: 0.7
  }
)

IO.puts("Assistant: #{response.content.text}")
```

### Batch Requests

Send multiple requests in a single call for better performance:

```elixir
# Define multiple requests
requests = [
  {:list_tools, []},
  {:list_resources, []},
  {:read_resource, ["file:///config.json"]},
  {:call_tool, ["get_status", %{}]}
]

# Send as batch
{:ok, [tools, resources, config, status]} = ExMCP.Client.batch_request(client, requests)

# Process results
IO.puts("Found #{length(tools)} tools")
IO.puts("Found #{length(resources)} resources")
```

### Bi-directional Communication

Enable servers to make requests back to clients:

```elixir
defmodule MyClientHandler do
  @behaviour ExMCP.Client.Handler
  
  @impl true
  def init(args) do
    {:ok, %{model: args[:model] || "gpt-4", roots: args[:roots]}}
  end
  
  @impl true
  def handle_ping(state) do
    {:ok, %{}, state}
  end
  
  @impl true
  def handle_list_roots(state) do
    {:ok, state.roots, state}
  end
  
  @impl true
  def handle_create_message(params, state) do
    # Server is asking client to sample an LLM
    messages = params["messages"]
    
    # Call your LLM integration
    response = MyLLM.chat(messages, model: state.model)
    
    result = %{
      "role" => "assistant",
      "content" => %{
        "type" => "text",
        "text" => response.content
      },
      "model" => state.model
    }
    
    {:ok, result, state}
  end
end

# Start client with handler
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: MyClientHandler,
  handler_state: %{
    model: "claude-3",
    roots: [%{uri: "file:///home", name: "Home"}]
  }
)
```

### Human-in-the-Loop Approval

Implement approval flows for sensitive operations:

```elixir
# Option 1: Use the built-in console approval handler
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: {ExMCP.Client.DefaultHandler, [
    approval_handler: ExMCP.Approval.Console,
    roots: [%{uri: "file:///data", name: "Data"}]
  ]}
)

# Option 2: Implement a custom approval handler
defmodule MyApprovalHandler do
  @behaviour ExMCP.Approval
  
  @impl true
  def request_approval(type, data, opts) do
    case type do
      :sampling ->
        # Show approval UI for LLM sampling
        if show_sampling_dialog(data) == :approved do
          {:approved, data}
        else
          {:denied, "User cancelled"}
        end
        
      :response ->
        # Review LLM response before sending
        reviewed_response = show_response_review(data)
        if reviewed_response do
          {:modified, reviewed_response}
        else
          {:denied, "Response rejected"}
        end
        
      :tool_call ->
        # Approve dangerous tool calls
        tool_name = data["name"]
        if dangerous_tool?(tool_name) do
          case show_tool_approval(tool_name, data["arguments"]) do
            :approve -> {:approved, data}
            :deny -> {:denied, "Tool call blocked"}
            {:modify, new_args} -> {:modified, %{data | "arguments" => new_args}}
          end
        else
          {:approved, data}
        end
    end
  end
  
  defp dangerous_tool?(name) do
    name in ["delete_file", "execute_command", "send_email"]
  end
end

# Use custom approval handler with default client handler
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: {ExMCP.Client.DefaultHandler, [
    approval_handler: MyApprovalHandler
  ]}
)
```

## Transport Layers

### stdio Transport

Best for local process communication:

```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :stdio
)

# Client
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["./my_mcp_server"]
)
```

### Streamable HTTP Transport

For HTTP-based communication with optional Server-Sent Events (SSE) streaming.

> **Note:** Use `transport: :http` for the HTTP transport with Server-Sent Events support. The `:sse` transport name is deprecated.

```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  port: 8080,
  path: "/mcp"
)

# Client
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8080",
  endpoint: "/mcp/v1",  # Optional: defaults to "/mcp/v1"
  headers: [{"Authorization", "Bearer token"}]
)

# Client with custom endpoint
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  endpoint: "/ai/mcp",  # Custom endpoint path
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

## Advanced Features

### Server Discovery

Automatically find available MCP servers:

```elixir
# Discover all servers
servers = ExMCP.Discovery.discover_servers()

# Discover specific types
servers = ExMCP.Discovery.discover_servers(
  methods: [:env, :npm, :well_known]
)

# Connect to discovered server
server_config = List.first(servers)
{:ok, client} = ExMCP.Client.start_link(server_config)
```

### Server Manager

Manage multiple server connections:

```elixir
# Start the server manager
{:ok, manager} = ExMCP.ServerManager.start_link()

# Add servers
{:ok, client1} = ExMCP.ServerManager.add_server(manager, %{
  name: "code-server",
  transport: :stdio,
  command: ["code-mcp-server"]
})

{:ok, client2} = ExMCP.ServerManager.add_server(manager, %{
  name: "data-server",
  transport: :http,
  url: "http://localhost:9000"
})

# List connected servers
servers = ExMCP.ServerManager.list_servers(manager)

# Get specific client
{:ok, client} = ExMCP.ServerManager.get_server(manager, "code-server")
```

### Error Handling

```elixir
# Client automatically reconnects on failure
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["flaky-server"],
  # Reconnection options
  reconnect: true,
  reconnect_interval: 1000,
  max_reconnect_attempts: 10
)

# Handle errors gracefully
case ExMCP.Client.call_tool(client, "risky_tool", %{}) do
  {:ok, result} ->
    process_result(result)
  {:error, %{"code" => -32603, "message" => msg}} ->
    Logger.error("Internal error: #{msg}")
  {:error, reason} ->
    Logger.error("Tool failed: #{inspect(reason)}")
end
```

### Custom Transports

Implement the `ExMCP.Transport` behaviour:

```elixir
defmodule MyTransport do
  @behaviour ExMCP.Transport

  @impl true
  def connect(opts) do
    # Establish connection
    {:ok, state}
  end

  @impl true
  def send_message(message, state) do
    # Send message
    {:ok, new_state}
  end

  @impl true
  def receive_message(state) do
    # Receive message (blocking)
    {:ok, message, new_state}
  end

  @impl true
  def close(state) do
    # Clean up
    :ok
  end
end

# Use custom transport
{:ok, client} = ExMCP.Client.start_link(
  transport: MyTransport,
  custom_option: "value"
)
```

## Best Practices

### 1. State Management

Keep server state immutable and use proper OTP patterns:

```elixir
defmodule MyHandler do
  use ExMCP.Server.Handler
  
  defstruct [:db_conn, :cache, subscriptions: MapSet.new()]

  @impl true
  def init(args) do
    # Initialize resources
    {:ok, db_conn} = Database.connect(args[:db_url])
    {:ok, %__MODULE__{db_conn: db_conn, cache: %{}}}
  end

  @impl true
  def terminate(_reason, state) do
    # Clean up resources
    Database.disconnect(state.db_conn)
  end
end
```

### 2. Error Handling

Always return proper error tuples:

```elixir
@impl true
def handle_call_tool("database_query", %{"sql" => sql}, state) do
  case Database.query(state.db_conn, sql) do
    {:ok, results} ->
      {:ok, format_results(results), state}
    
    {:error, :invalid_sql} ->
      {:error, "Invalid SQL syntax", state}
    
    {:error, reason} ->
      {:error, "Database error: #{inspect(reason)}", state}
  end
end
```

### 3. Tool Design

Make tools focused and composable:

```elixir
# Good: Focused tools
tools = [
  %{name: "list_files", description: "List files in directory"},
  %{name: "read_file", description: "Read file contents"},
  %{name: "search_files", description: "Search in files"}
]

# Bad: Monolithic tool
tools = [
  %{name: "file_manager", description: "List, read, search, modify files"}
]
```

### 4. Resource URIs

Use consistent URI schemes:

```elixir
# Good: Clear URI structure
resources = [
  %{uri: "file:///data/users.json"},
  %{uri: "db://postgres/users/table"},
  %{uri: "api://v1/users"}
]

# Bad: Inconsistent URIs
resources = [
  %{uri: "/data/users.json"},
  %{uri: "postgres:users"},
  %{uri: "users-api"}
]
```

### 5. Capability Declaration

Only declare capabilities you actually implement:

```elixir
@impl true
def handle_initialize(_params, state) do
  {:ok, %{
    name: "my-server",
    version: "1.0.0",
    capabilities: %{
      # Only include if you implement tools
      tools: %{},
      # Only include if you support subscriptions
      resources: %{subscribe: true},
      # Only include if you implement sampling
      sampling: %{}
    }
  }, state}
end
```

## Troubleshooting

### Common Issues

**Connection Failures**

```elixir
# Check server is running
{:error, :enoent} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["non-existent-server"]
)

# Solution: Verify command path
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["/usr/local/bin/mcp-server"]
)
```

**Protocol Errors**

```elixir
# Wrong protocol version
{:error, %{"code" => -32600}} = ExMCP.Client.list_tools(client)

# Solution: ExMCP uses protocol version 2025-03-26
# Ensure your server supports this version
```

**Transport Issues**

```elixir
# stdio: Process dies immediately
# Check server stderr
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["server", "--verbose"],
  env: [{"DEBUG", "true"}]
)

# HTTP streaming: Connection refused
# Ensure server is listening
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8080",
  endpoint: "/mcp/v1"  # Ensure endpoint matches server configuration
)

# BEAM: Node not connected
# Connect nodes first
Node.connect(:"server@host")
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: {:"server@host", :mcp_server}
)
```

### Debugging

Enable debug logging:

```elixir
# In config/config.exs
config :logger, level: :debug

# Or at runtime
Logger.configure(level: :debug)
```

Trace protocol messages:

```elixir
# Start a debugging proxy
defmodule DebugTransport do
  def send_message(message, state) do
    IO.puts(">>> #{message}")
    ActualTransport.send_message(message, state)
  end
  
  def receive_message(state) do
    case ActualTransport.receive_message(state) do
      {:ok, message, new_state} ->
        IO.puts("<<< #{message}")
        {:ok, message, new_state}
      other ->
        other
    end
  end
end
```

### Performance Tuning

```elixir
# Increase timeout for slow operations
{:ok, result} = ExMCP.Client.call_tool(
  client,
  "expensive_operation",
  %{size: "large"},
  60_000  # 60 second timeout
)

# Use connection pooling for multiple clients
defmodule MCPPool do
  use GenServer
  
  def start_link(server_config, pool_size: size) do
    # Create pool of clients
  end
  
  def checkout(pool) do
    # Get available client
  end
end
```

## Next Steps

- Explore the [examples](examples/) directory for complete working examples
- Read the [API documentation](https://hexdocs.pm/ex_mcp)
- Check out the [MCP specification](https://modelcontextprotocol.io)
- Join the community and contribute on [GitHub](https://github.com/azmaveth/ex_mcp)

## Support

For questions and issues:

- GitHub Issues: Report bugs and feature requests
- Discussions: Ask questions and share ideas
- Stack Overflow: Tag questions with `ex-mcp` and `elixir`

Remember to include:
- ExMCP version
- Elixir/OTP versions
- Transport being used
- Minimal reproduction code
- Error messages and stack traces