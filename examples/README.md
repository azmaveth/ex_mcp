# ExMCP Examples

This directory contains example implementations showing how to use ExMCP to build MCP (Model Context Protocol) servers and clients.

## 🚀 Getting Started (`getting_started/`)

**New to ExMCP? Start here!** This directory contains a complete demonstration of all transport types:

- **STDIO Server** - Simple tool using subprocess communication
- **HTTP Server** - Resources via standard HTTP
- **HTTP+SSE Server** - Prompts with real-time streaming
- **Native BEAM Server** - Full features with Erlang process communication
- **Demo Client** - Connects to all servers and demonstrates their features

```bash
cd examples/getting_started
./run_demo.sh  # Run the complete demo
```

## Server Examples (Using DSL)

All server examples use ExMCP's DSL (Domain Specific Language) for clean, declarative server definitions.

### 🎯 Basic DSL Server (`basic_dsl_server.exs`)

The simplest example showing core DSL features:
- Tool definition with `deftool`
- Resource definition with `defresource`
- Prompt definition with `defprompt`
- Basic handler implementations

```bash
elixir examples/basic_dsl_server.exs
```

### 🚀 Advanced DSL Server (`advanced_dsl_server.exs`)

Demonstrates advanced DSL capabilities:
- Complex argument schemas with nested objects
- Field validation (enums, ranges, patterns)
- Resource patterns and subscriptions
- Meta blocks for better organization
- Content helpers (text, json, image)
- State management

```bash
elixir examples/advanced_dsl_server.exs
```

### 🌤️ Weather Service (`weather_service.exs`)

A practical example showing a weather information service:
- Multiple related tools (current weather, forecast, comparison)
- Caching and state management
- Resource subscriptions for real-time updates
- Prompts for weather-based planning
- Realistic data simulation

```bash
elixir examples/weather_service.exs
```

### 📁 File Manager (`file_manager.exs`)

File management service demonstrating:
- File operations (list, read, write, search)
- Resource patterns for dynamic file access
- File watching and subscriptions
- Security considerations (sandboxed root)
- Metadata and file analysis
- Binary content handling

```bash
elixir examples/file_manager.exs
```

## Client Examples

### Basic Client (`basic_client.exs`)
Simple client showing how to connect and use MCP servers.

### Test HTTP Client (`test_http_client.exs`)
HTTP transport client for testing HTTP-based MCP servers.

### Simple Test Client (`simple_test_client.exs`)
Minimal client for quick testing and debugging.

## Advanced Examples

### OAuth Examples (`advanced/oauth/`)
- `full_flow.exs` - Complete OAuth 2.1 authorization flow
- `basic_pkce.exs` - PKCE implementation for public clients

### Sampling Example (`advanced/sampling/`)
- Note: Sampling examples are being updated for the latest MCP specification

## Utility Examples

### Error Handling (`utilities/error_handling.exs`)
Demonstrates proper error handling patterns in MCP.

### Client Configuration (`utilities/client_config.exs`)
Shows various client configuration options.

### Structured Responses (`utilities/structured_responses.exs`)
Working with ExMCP's structured response types.

## Running the Examples

1. Make sure you have Elixir installed (1.15+ recommended)
2. Clone the ExMCP repository
3. From the repository root, run any example:

```bash
# Run a server example
elixir examples/weather_service.exs

# In another terminal, connect a client
elixir examples/basic_client.exs
```

## DSL Quick Reference

### Tool Definition
```elixir
deftool "tool_name" do
  description "What this tool does"
  
  args do
    field :param1, :string, required: true
    field :param2, :integer, default: 10
  end
end
```

### Resource Definition
```elixir
defresource "protocol://path" do
  name "Resource Name"
  description "What this resource provides"
  mime_type "application/json"
  subscribable true
end
```

### Prompt Definition
```elixir
defprompt "prompt_name" do
  name "Prompt Display Name"
  description "What this prompt helps with"
  
  arguments do
    arg :param1, required: true
    arg :param2, description: "Optional parameter"
  end
end
```

### Content Helpers
```elixir
# In handler implementations
{:ok, %{content: [
  text("Plain text content"),
  json(%{data: "structured"}),
  image(base64_data, "image/png"),
  blob(binary_data, "application/pdf")
]}, state}
```

## Best Practices

1. **Use the DSL** - It provides better validation, documentation, and cleaner code
2. **Handle errors gracefully** - Return `{:error, reason, state}` for failures
3. **Manage state properly** - Return updated state from handlers
4. **Use content helpers** - They ensure proper MCP message formatting
5. **Validate inputs** - Use the schema validation in field definitions
6. **Document your API** - Use descriptions in your DSL definitions

## Learning Path

1. **Beginners**: Start with `basic_dsl_server.exs` to learn the DSL basics
2. **Intermediate**: Study `advanced_dsl_server.exs` for complex schemas and patterns
3. **Practical Examples**: Explore `weather_service.exs` or `file_manager.exs` for real-world patterns
4. **Client Development**: Check client examples for connecting to servers

## Transport Types

ExMCP supports multiple transport types:

- **STDIO** - Standard input/output (default for most examples)
- **HTTP** - HTTP with JSON-RPC
- **SSE** - Server-Sent Events for streaming
- **Native** - Direct Erlang process communication

## Need Help?

- Check the [main ExMCP documentation](../README.md)
- Look at the test files for more examples
- Explore the DSL modules in `lib/ex_mcp/dsl/`

## Contributing

When adding new examples:
1. Use the DSL for all server implementations
2. Include clear comments explaining the concepts
3. Make examples self-contained and runnable
4. Add appropriate error handling
5. Update this README with your example