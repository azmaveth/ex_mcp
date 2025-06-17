# ExMCP Examples

This directory contains comprehensive examples demonstrating various features and use cases of ExMCP.

## 📚 Directory Structure

### 🚀 Getting Started
Start here if you're new to ExMCP:
- [`getting_started/`](getting_started/) - Basic examples for beginners
  - `hello_world.exs` - Your first MCP server and client
  - `comprehensive_example.exs` - Demonstrates all MCP features

### 🔌 Transports
Examples for each transport type:
- [`transports/stdio/`](transports/stdio/) - Standard I/O transport (MCP standard)
  - Bidirectional communication
  - Health checks and ping
- [`transports/http/`](transports/http/) - HTTP with SSE streaming
- [`transports/native/`](transports/native/) - ExMCP's high-performance Native Service Dispatcher
  - Calculator services
  - Clustering and distribution
  - Hot code reloading

### 🛠️ Features
Examples organized by MCP feature:
- [`features/tools/`](features/tools/) - Tool discovery and execution
- [`features/resources/`](features/resources/) - Resource management and subscriptions
- [`features/prompts/`](features/prompts/) - Prompt templates
- [`features/notifications/`](features/notifications/) - Change notifications and cancellation
- [`features/progress/`](features/progress/) - Progress tracking for long operations
- [`features/roots/`](features/roots/) - URI boundaries and roots
- [`features/batch_requests/`](features/batch_requests/) - Batch request handling

### 🔧 Advanced
Advanced features and patterns:
- [`advanced/oauth/`](advanced/oauth/) - OAuth 2.1 authorization flows
- [`advanced/security/`](advanced/security/) - Security features and secure servers
- [`advanced/sampling/`](advanced/sampling/) - LLM integration and sampling
- [`advanced/draft_features/`](advanced/draft_features/) - Experimental MCP features
- `human_in_the_loop.exs` - Approval flows and HITL patterns

### 🌐 Interoperability
Cross-language integration examples:
- [`interoperability/`](interoperability/) - Python MCP SDK integration
  - Elixir clients → Python servers
  - Python clients → Elixir servers
  - Hybrid architectures

## 🎯 Quick Navigation

### By Use Case

**"I want to build a simple MCP server"**
→ Start with [`getting_started/hello_world.exs`](getting_started/hello_world.exs)

**"I need maximum performance for Elixir services"**
→ See [`transports/native/`](transports/native/)

**"I'm integrating with Python/JavaScript/other languages"**
→ Check [`interoperability/`](interoperability/) and use stdio or HTTP transport

**"I need to implement authentication"**
→ Look at [`advanced/oauth/`](advanced/oauth/) and [`advanced/security/`](advanced/security/)

**"I want to track progress of long operations"**
→ See [`features/progress/`](features/progress/)

**"I need to handle file resources"**
→ Check [`features/resources/`](features/resources/)

### By Transport Type

| Transport | When to Use | Examples |
|-----------|-------------|----------|
| **stdio** | Standard MCP communication, subprocesses | [`transports/stdio/`](transports/stdio/) |
| **HTTP** | Network services, web integration | [`transports/http/`](transports/http/) |
| **Native** | Elixir-only, maximum performance | [`transports/native/`](transports/native/) |

## 🚦 Running Examples

Most examples are self-contained scripts that can be run directly:

```bash
# From the examples directory
elixir getting_started/hello_world.exs

# Or from the project root
elixir examples/getting_started/hello_world.exs
```

Some examples require additional setup:
- **Python interoperability**: Requires Python 3.8+ and `pip install mcp`
- **HTTP examples**: May require starting an HTTP server
- **Distributed examples**: Require multiple Elixir nodes

## 📖 Learning Path

1. Start with [`getting_started/hello_world.exs`](getting_started/hello_world.exs)
2. Explore [`getting_started/comprehensive_example.exs`](getting_started/comprehensive_example.exs)
3. Try different transports in [`transports/`](transports/)
4. Dive into specific features in [`features/`](features/)
5. Explore advanced patterns in [`advanced/`](advanced/)

## 🤝 Contributing

When adding new examples:
1. Place them in the appropriate category directory
2. Include clear comments explaining the concept
3. Make them self-contained and runnable
4. Update this README if adding new categories

## 📝 Notes

- Examples using `ExMCP.Native` are specific to ExMCP and provide ultra-high performance
- Examples using `:stdio` or `:http` transports follow the MCP specification
- The `interoperability/` examples require additional language runtimes