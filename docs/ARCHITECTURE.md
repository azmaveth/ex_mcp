# ExMCP v2 Architecture Guide

## Overview

ExMCP v2 represents a complete redesign of the library architecture, focusing on developer experience, type safety, and production readiness. This guide explains the architectural decisions and design patterns used in v2.

## Core Design Principles

### 1. Structured Responses

v2 introduces dedicated response types (`ExMCP.Response` and `ExMCP.Error`) to provide:
- **Type safety** - Clear response types instead of raw maps
- **Consistency** - All operations return the same response structure
- **Discoverability** - Helper functions for content extraction

```elixir
# v1: Raw maps with unclear structure
{:ok, %{"tools" => tools}} = Client.list_tools(client)

# v2: Structured responses with helpers
{:ok, response} = Client.list_tools(client)
tools = ExMCP.Response.tools(response)
```

### 2. Configuration Builder Pattern

The `ExMCP.ClientConfig` module provides a fluent interface for client configuration:

```elixir
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.put_transport(:http)
         |> ExMCP.ClientConfig.put_url("https://api.example.com")
         |> ExMCP.ClientConfig.put_transport_options(timeout: 30_000)
```

Benefits:
- Compile-time validation of configuration
- Clear documentation of available options
- Immutable configuration objects
- Easy to extend with new options

### 3. Enhanced DSL

The v2 DSL modules provide:
- **Compile-time validation** of tool/resource/prompt definitions
- **Clear separation** between metadata and implementation
- **Consistent naming** aligned with MCP specification

```elixir
# Clear, declarative syntax
tool "search" do
  description "Search the web"
  input_schema %{...}  # JSON Schema validation
  handler fn args -> ... end
end
```

## Module Organization

### Core Modules

```
lib/ex_mcp_v2/
├── client.ex              # Main client implementation
├── client_config.ex       # Configuration builder
├── response.ex            # Structured response type
├── error.ex              # Structured error type
├── server_v2.ex          # Enhanced server with transport support
├── http_plug.ex          # HTTP/SSE transport integration
└── message_processor.ex   # Core protocol handling (renamed from Plug)
```

### DSL Modules

```
lib/ex_mcp_v2/dsl/
├── tool.ex      # Tool definition DSL
├── resource.ex  # Resource definition DSL
├── prompt.ex    # Prompt definition DSL
└── advanced.ex  # Advanced DSL features
```

### Support Modules

```
lib/ex_mcp_v2/
├── transport_manager.ex     # Transport lifecycle management
├── simple_client.ex        # Simplified client for testing
├── convenience_client.ex   # Top-level convenience functions
└── helpers.ex             # Shared utilities
```

## Key Architectural Patterns

### 1. Transport Abstraction

The transport layer is completely abstracted from the protocol layer:

```elixir
# Transport manager handles connection lifecycle
defmodule TransportManager do
  def connect(config) do
    case config.transport do
      :stdio -> StdioTransport.connect(config)
      :http -> HttpTransport.connect(config)
    end
  end
end
```

### 2. Message Processing Pipeline

```
Request → Transport → MessageProcessor → Handler → Response → Transport
```

The `MessageProcessor` (formerly Plug) handles:
- JSON-RPC protocol encoding/decoding
- Method routing
- Error handling
- Response formatting

### 3. HTTP/SSE Integration

The `HttpPlug` module provides:
- Standard HTTP POST for requests
- SSE endpoint for server-initiated messages
- Backpressure control for slow clients
- Connection resumption via Last-Event-ID

```elixir
# Automatic SSE support
plug ExMCP.HttpPlug,
  handler: MyHandler,
  server_info: %{name: "server", version: "1.0.0"}
```

### 4. Error Handling Strategy

Errors are categorized into three types:

1. **Protocol Errors** - JSON-RPC level errors
2. **Application Errors** - Tool/resource execution errors  
3. **Transport Errors** - Connection/network errors

```elixir
# Consistent error handling
case Client.call_tool(client, "tool", args) do
  {:ok, response} when response.type == :error ->
    # Application error (tool returned error)
    handle_app_error(response.content)
    
  {:ok, response} ->
    # Success
    process_response(response)
    
  {:error, error} ->
    # Protocol or transport error
    handle_protocol_error(error)
end
```

## Production Features

### 1. SSE Backpressure Control

The SSE handler implements sophisticated flow control:

```elixir
defmodule SSEHandler do
  @max_mailbox_size 1000
  
  def handle_call(:request_send, from, state) do
    {:message_queue_len, queue_len} = Process.info(self(), :message_queue_len)
    
    if queue_len > @max_mailbox_size do
      # Block producer until mailbox drains
      {:noreply, %{state | producers: MapSet.put(state.producers, from)}}
    else
      {:reply, :ok, state}
    end
  end
end
```

### 2. Connection Resilience

- Automatic reconnection with exponential backoff
- Connection state tracking
- Graceful degradation on errors

### 3. Deprecation Management

v2 includes comprehensive deprecation warnings with source location:

```elixir
defmacro tool_description(desc) do
  caller = __CALLER__
  Logger.warning(
    "tool_description/1 is deprecated. Use description/1 instead.",
    file: Path.relative_to_cwd(caller.file),
    line: caller.line
  )
end
```

## Testing Architecture

### 1. Test Helpers

```elixir
defmodule ExMCP.TestHelpers do
  def start_test_server(opts) do
    # Creates an in-memory test server
  end
  
  def connect_test_client(server) do
    # Connects directly without transport
  end
end
```

### 2. Property-Based Testing

v2 includes property-based tests for:
- Protocol encoding/decoding
- Response type conversions
- Error categorization

### 3. Integration Testing

Comprehensive integration tests cover:
- Concurrent client connections
- SSE streaming behavior
- Error propagation
- Performance characteristics

## Performance Considerations

### 1. Zero-Copy Message Passing

When using stdio transport within the same BEAM:
- Messages are passed by reference
- No serialization overhead
- ~15μs latency for local calls

### 2. Connection Pooling

HTTP transport supports connection pooling:
```elixir
config |> ExMCP.ClientConfig.put_transport_options(
  pool_size: 10,
  pool_timeout: 5000
)
```

### 3. Streaming Support

SSE enables efficient streaming of:
- Progress updates
- Resource notifications
- Large responses

## Migration Path

### 1. Compatibility Layer

v2 maintains backwards compatibility through:
- Deprecated function warnings
- Automatic response conversion
- Legacy DSL support

### 2. Incremental Migration

Applications can migrate incrementally:
1. Update client code to use v2 API
2. Migrate DSL definitions
3. Update error handling
4. Remove deprecated calls

### 3. Feature Detection

```elixir
# Check for v2 features
if function_exported?(ExMCP, :client_config, 0) do
  # Use v2 API
else
  # Fall back to v1
end
```

## Future Extensibility

### 1. Custom Response Types

v2 response system is extensible:

```elixir
defmodule MyApp.CustomResponse do
  def custom_type(data, source) do
    %ExMCP.Response{
      type: :custom,
      content: data,
      source: source
    }
  end
end
```

### 2. Transport Plugins

New transports can be added by implementing the behaviour:

```elixir
defmodule MyTransport do
  @behaviour ExMCP.Transport
  
  def connect(opts), do: ...
  def send_message(msg, state), do: ...
  def receive_message(state), do: ...
  def close(state), do: ...
end
```

### 3. Middleware Support

Future versions will support middleware:

```elixir
config |> ExMCP.ClientConfig.put_middleware([
  ExMCP.Middleware.Logger,
  ExMCP.Middleware.Retry,
  MyApp.CustomMiddleware
])
```

## Best Practices

1. **Always use structured responses** in handlers
2. **Configure timeouts** appropriately for your use case
3. **Monitor SSE connections** for backpressure
4. **Use the DSL** for cleaner, validated definitions
5. **Handle errors** at the appropriate level
6. **Test with property-based tests** for edge cases
7. **Profile performance** for production workloads

## Conclusion

ExMCP v2's architecture prioritizes:
- **Developer experience** through clear APIs and helpful errors
- **Type safety** with structured responses
- **Production readiness** with backpressure and monitoring
- **Extensibility** for future enhancements

The modular design allows teams to adopt v2 features incrementally while maintaining compatibility with existing code.