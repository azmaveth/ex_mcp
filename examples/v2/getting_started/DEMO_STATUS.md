# ExMCP v2 Demo Status

## What We've Accomplished

### ✅ Created v2 DSL Examples

1. **Native Server** (`native_server.exs`)
   - Demonstrates native BEAM transport
   - ~15 lines of DSL code
   - Shows tools, resources, and prompts
   - Note: Native transport only works within same BEAM instance

2. **stdio Server** (`stdio_server.exs`)
   - JSON-RPC over stdin/stdout
   - ~50 lines vs v1's ~150 lines
   - Has some initialization timeout issues to debug

3. **HTTP Server** (`simple_http_server.exs`)
   - Built-in CORS and JSON-RPC
   - ~80 lines vs v1's ~175 lines
   - Zero-config HTTP transport

4. **SSE Server** (`sse_http_server.exs`)
   - Real-time streaming with Server-Sent Events
   - ~100 lines vs v1's ~275 lines
   - Built-in session management

### ✅ Fixed Issues

- Resolved import conflicts in DSL macros
- Fixed variable scoping in SSE server
- Updated module attribute access to runtime
- Created comprehensive README.md

### 📊 Code Reduction Achieved

- **60-80% less code** with v2 DSL
- Clean, declarative syntax
- Auto-capability detection
- Built-in JSON Schema generation
- Smart content helpers

## Current Status

### stdio Transport Issue

The stdio transport is experiencing initialization timeouts. This appears to be related to:
- The client expecting immediate responses
- Possible buffering issues with stdio
- The initialization handshake timing

### Testing Approach

For stdio transport:
1. Client spawns server as subprocess (not pre-running)
2. Communication via JSON-RPC over stdin/stdout
3. Server should respond to initialize within timeout

For HTTP/SSE transports:
1. Server runs independently
2. Client connects to running server
3. Communication over network

### Next Steps

To fully demonstrate stdio transport:
1. Debug the initialization timeout
2. Check stdio buffering/flushing
3. Verify JSON-RPC message format
4. Test with simpler server implementation

## How to Run Examples

```bash
# From project root, after mix compile

# HTTP Server (terminal 1)
elixir examples/v2/getting_started/simple_http_server.exs

# SSE Server (terminal 1)  
elixir examples/v2/getting_started/sse_http_server.exs

# Test clients (terminal 2)
elixir examples/v2/getting_started/test_http_client.exs
elixir examples/v2/getting_started/test_sse_client.exs

# stdio (needs debugging)
elixir examples/v2/getting_started/stdio_client.exs
```

## Key v2 DSL Benefits Demonstrated

1. **Declarative tool definitions** with automatic JSON Schema
2. **Resource definitions** with MIME types and subscriptions
3. **Prompt templates** with structured arguments
4. **Smart content helpers** (text, json, image, user, system)
5. **Auto-capability detection** from definitions
6. **Zero boilerplate** GenServer code