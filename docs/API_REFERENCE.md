# ExMCP v2 API Reference

## Overview

ExMCP v2 introduces a redesigned API that provides:
- **Structured responses** with dedicated `ExMCP.Response` and `ExMCP.Error` types
- **Simplified client API** with configuration objects and convenience functions
- **Enhanced DSL** for defining tools, resources, and prompts
- **HTTP Plug integration** for easy web service deployment
- **SSE support** with backpressure control and connection resumption

## Core Modules

### ExMCP

Top-level convenience module providing simplified access to v2 functionality.

```elixir
# Create a client configuration
config = ExMCP.client_config()
         |> ExMCP.put_transport(:stdio)
         |> ExMCP.put_command(["python", "server.py"])

# Connect to server
{:ok, client} = ExMCP.connect(config)

# List tools
{:ok, response} = ExMCP.list_tools(client)
tools = ExMCP.Response.tools(response)

# Call a tool
{:ok, response} = ExMCP.call_tool(client, "search", %{query: "elixir"})
text = ExMCP.Response.text_content(response)
```

### ExMCP.ClientConfig

Configuration builder for MCP clients.

#### Functions

**`new/0`** - Creates a new client configuration
```elixir
config = ExMCP.ClientConfig.new()
```

**`put_transport/2`** - Sets the transport type
```elixir
config = ExMCP.ClientConfig.put_transport(config, :stdio)
config = ExMCP.ClientConfig.put_transport(config, :http)
```

**`put_command/2`** - Sets the command for stdio transport
```elixir
config = ExMCP.ClientConfig.put_command(config, ["node", "server.js"])
```

**`put_url/2`** - Sets the URL for HTTP transport
```elixir
config = ExMCP.ClientConfig.put_url(config, "https://api.example.com")
```

**`put_transport_options/2`** - Sets transport-specific options
```elixir
config = ExMCP.ClientConfig.put_transport_options(config, 
  headers: [{"Authorization", "Bearer token"}],
  timeout: 10_000
)
```

### ExMCP.Client

Main client module for MCP v2 connections.

#### Functions

**`connect/1`** - Connects to an MCP server
```elixir
{:ok, client} = ExMCP.Client.connect(config)
```

**`disconnect/1`** - Disconnects from the server
```elixir
:ok = ExMCP.Client.disconnect(client)
```

**`list_tools/1`** - Lists available tools
```elixir
{:ok, response} = ExMCP.Client.list_tools(client)
# response is an ExMCP.Response struct
```

**`call_tool/3`** - Calls a tool
```elixir
{:ok, response} = ExMCP.Client.call_tool(client, "calculator", %{
  "operation" => "add",
  "a" => 5,
  "b" => 3
})
```

**`list_resources/1`** - Lists available resources
```elixir
{:ok, response} = ExMCP.Client.list_resources(client)
```

**`read_resource/2`** - Reads a resource
```elixir
{:ok, response} = ExMCP.Client.read_resource(client, "file:///data.json")
```

**`list_prompts/1`** - Lists available prompts
```elixir
{:ok, response} = ExMCP.Client.list_prompts(client)
```

**`get_prompt/3`** - Gets a prompt with arguments
```elixir
{:ok, response} = ExMCP.Client.get_prompt(client, "code_review", %{
  "language" => "elixir",
  "code" => "def add(a, b), do: a + b"
})
```

### ExMCP.Response

Structured response type for all MCP operations.

#### Types

```elixir
@type t :: %ExMCP.Response{
  type: response_type(),
  content: term(),
  source: String.t() | nil
}

@type response_type :: 
  :text | :json | :error | :tools | :resources | 
  :prompts | :server_info | :mixed
```

#### Functions

**`text/2`** - Creates a text response
```elixir
response = ExMCP.Response.text("Hello, world!", "greeting_tool")
```

**`json/2`** - Creates a JSON response
```elixir
response = ExMCP.Response.json(%{result: 42}, "calculator")
```

**`error/2`** - Creates an error response
```elixir
response = ExMCP.Response.error("Invalid input", "validation_tool")
```

**`text_content/1`** - Extracts text content
```elixir
text = ExMCP.Response.text_content(response)
# Returns nil if not a text response
```

**`json_content/1`** - Extracts JSON content
```elixir
data = ExMCP.Response.json_content(response)
# Returns nil if not a JSON response
```

**`tools/1`** - Extracts tools list
```elixir
tools = ExMCP.Response.tools(response)
# Returns list of tool definitions
```

**`resources/1`** - Extracts resources list
```elixir
resources = ExMCP.Response.resources(response)
# Returns list of resource definitions
```

### ExMCP.Error

Structured error type for MCP operations.

#### Types

```elixir
@type t :: %ExMCP.Error{
  code: integer(),
  message: String.t(),
  data: term() | nil
}
```

#### Error Code Constants

```elixir
# JSON-RPC standard errors
@parse_error -32700
@invalid_request -32600
@method_not_found -32601
@invalid_params -32602
@internal_error -32603

# MCP-specific errors
@tool_not_found -32001
@resource_not_found -32002
@prompt_not_found -32003
@tool_error -32004
@subscription_error -32005
```

#### Functions

**`parse_error/1`** - Creates a parse error
```elixir
error = ExMCP.Error.parse_error("Invalid JSON")
```

**`method_not_found/1`** - Creates a method not found error
```elixir
error = ExMCP.Error.method_not_found("tools/unknown")
```

**`tool_error/2`** - Creates a tool execution error
```elixir
error = ExMCP.Error.tool_error("Execution failed", "calculator")
```

**`to_json_rpc/1`** - Converts to JSON-RPC error format
```elixir
json_error = ExMCP.Error.to_json_rpc(error)
# => %{"code" => -32004, "message" => "Execution failed", "data" => %{...}}
```

**`category/1`** - Gets error category
```elixir
category = ExMCP.Error.category(error)
# => :protocol | :application | :transport
```

### ExMCP.Server

Enhanced server module for v2 with transport integration.

#### Functions

**`start/1`** - Starts a server with specified transport
```elixir
{:ok, server} = ExMCP.Server.start(
  handler: MyHandler,
  transport: :stdio
)

# HTTP transport with SSE
{:ok, server} = ExMCP.Server.start(
  handler: MyHandler,
  transport: :http,
  port: 8080,
  path: "/mcp"
)
```

### ExMCP.HttpPlug

Plug module for HTTP/SSE transport integration.

#### Usage with Phoenix

```elixir
# In your router
plug ExMCP.HttpPlug,
  handler: MyApp.MCPHandler,
  server_info: %{name: "my-app", version: "1.0.0"}
```

#### Standalone with Cowboy

```elixir
{:ok, _} = Plug.Cowboy.http(ExMCP.HttpPlug, [
  handler: MyHandler,
  server_info: %{name: "server", version: "1.0.0"}
], port: 8080)
```

#### SSE Support

The HTTP plug automatically supports Server-Sent Events for:
- Progress notifications
- Resource update notifications
- Streaming responses

Features:
- **Backpressure control** - Prevents memory leaks from slow clients
- **Last-Event-ID** - Supports connection resumption
- **Heartbeats** - Keeps connections alive
- **Error propagation** - Structured error handling in streams

## DSL Modules

### ExMCP.DSL.Tool

Declarative tool definition with validation.

```elixir
defmodule MyTools do
  use ExMCP.DSL.Tool

  # New v2 syntax
  tool "calculator" do
    description "Performs basic arithmetic"
    
    input_schema %{
      type: "object",
      properties: %{
        operation: %{type: "string", enum: ["add", "subtract", "multiply", "divide"]},
        a: %{type: "number"},
        b: %{type: "number"}
      },
      required: ["operation", "a", "b"]
    }
    
    handler fn args ->
      case args["operation"] do
        "add" -> ExMCP.Response.json(%{result: args["a"] + args["b"]}, "calculator")
        "subtract" -> ExMCP.Response.json(%{result: args["a"] - args["b"]}, "calculator")
        "multiply" -> ExMCP.Response.json(%{result: args["a"] * args["b"]}, "calculator")
        "divide" when args["b"] != 0 -> 
          ExMCP.Response.json(%{result: args["a"] / args["b"]}, "calculator")
        "divide" -> 
          ExMCP.Response.error("Division by zero", "calculator")
      end
    end
  end
end
```

### ExMCP.DSL.Resource

Declarative resource definition.

```elixir
defmodule MyResources do
  use ExMCP.DSL.Resource

  # New v2 syntax
  resource "config" do
    name "Application Configuration"
    description "Current application configuration"
    uri "file:///config.json"
    mime_type "application/json"
    
    reader fn _uri ->
      case File.read("config.json") do
        {:ok, content} -> 
          ExMCP.Response.json(Jason.decode!(content), "config")
        {:error, reason} -> 
          ExMCP.Response.error("Failed to read config: #{reason}", "config")
      end
    end
  end
end
```

### ExMCP.DSL.Prompt

Declarative prompt definition.

```elixir
defmodule MyPrompts do
  use ExMCP.DSL.Prompt

  # New v2 syntax
  prompt "code_review" do
    name "Code Review Assistant"
    description "Reviews code for quality and suggests improvements"
    
    arguments [
      %{name: "language", type: "string", required: true},
      %{name: "code", type: "string", required: true},
      %{name: "focus", type: "string", required: false}
    ]
    
    handler fn args ->
      review = """
      Reviewing #{args["language"]} code:
      #{args["code"]}
      
      Focus: #{args["focus"] || "general review"}
      """
      ExMCP.Response.text(review, "code_review")
    end
  end
end
```

## Migration from v1

### Client API Changes

```elixir
# v1
{:ok, tools} = ExMCP.Client.list_tools(client)
# tools is a raw map

# v2
{:ok, response} = ExMCP.Client.list_tools(client)
tools = ExMCP.Response.tools(response)
# response is a structured ExMCP.Response
```

### DSL Changes

```elixir
# v1
tool "search" do
  tool_description "Search the web"
  args %{query: :string}
  # ...
end

# v2
tool "search" do
  description "Search the web"  # Changed from tool_description
  input_schema %{               # Changed from args
    type: "object",
    properties: %{
      query: %{type: "string"}
    }
  }
  # ...
end
```

### Error Handling

```elixir
# v1
case ExMCP.Client.call_tool(client, "tool", %{}) do
  {:ok, result} -> # raw result
  {:error, error} -> # various error formats
end

# v2
case ExMCP.Client.call_tool(client, "tool", %{}) do
  {:ok, response} -> 
    if response.type == :error do
      # Handle error response
      error = response.content
    else
      # Handle success
      content = ExMCP.Response.text_content(response)
    end
  {:error, error} -> 
    # Transport or protocol error (ExMCP.Error struct)
end
```

## Performance Considerations

### SSE Backpressure

The v2 SSE implementation includes sophisticated backpressure control:

```elixir
# In SSEHandler
@max_mailbox_size 1000  # Configurable limit

# Producers are blocked when mailbox is full
# Automatically unblocked when space available
```

### Connection Configuration

```elixir
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.put_transport(:http)
         |> ExMCP.ClientConfig.put_transport_options(
           timeout: 30_000,        # 30 second timeout
           pool_size: 10,          # Connection pool size
           max_redirects: 3        # Follow redirects
         )
```

## Testing Support

### Test Helpers

```elixir
defmodule MyTest do
  use ExUnit.Case
  import ExMCP.TestHelpers

  test "tool execution" do
    {:ok, server} = start_test_server(
      tools: [
        %{name: "echo", handler: fn args -> 
          ExMCP.Response.text(args["message"], "echo")
        end}
      ]
    )
    
    {:ok, client} = connect_test_client(server)
    {:ok, response} = ExMCP.Client.call_tool(client, "echo", %{message: "hello"})
    
    assert ExMCP.Response.text_content(response) == "hello"
  end
end
```

## Best Practices

1. **Always use structured responses** - Return `ExMCP.Response` types from handlers
2. **Handle errors gracefully** - Use `ExMCP.Error` for consistent error reporting
3. **Configure timeouts appropriately** - Set realistic timeouts for your use case
4. **Monitor SSE connections** - Watch for slow clients and backpressure
5. **Use the DSL** - Leverage compile-time validation and cleaner syntax
6. **Test with the test helpers** - Use provided testing utilities for reliable tests

## Examples

See the `examples/v2/` directory for complete examples:
- `basic_client_v2.exs` - Simple v2 client usage
- `basic_server_v2.exs` - Simple v2 server with DSL
- `http_server_v2.exs` - HTTP/SSE server example
- `error_handling_v2.exs` - Comprehensive error handling
- `migration_example.exs` - Migrating from v1 to v2