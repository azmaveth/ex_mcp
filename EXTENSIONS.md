# ExMCP Extensions Guide

This document distinguishes between MCP specification features and ExMCP-specific extensions.

## MCP Specification Features

These modules and features are part of the official Model Context Protocol specification:

### Protocol & Types
- `ExMCP.Protocol` - Full MCP protocol implementation
- `ExMCP.Types` - All MCP-defined types

### Core Client/Server
- `ExMCP.Client` (core methods only):
  - `initialize/2` - MCP handshake
  - `list_tools/2`, `call_tool/4` - Tool operations
  - `list_resources/2`, `read_resource/3` - Resource operations
  - `list_prompts/2`, `get_prompt/4` - Prompt operations
  - `create_message/4` - Sampling operations
  - `subscribe_resource/3`, `unsubscribe_resource/3` - Subscriptions
  - `list_resource_templates/2` - Resource templates
  - `ping/2`, `complete/4` - Utility operations
  - `send_cancelled/3`, `log_message/4` - Notifications

- `ExMCP.Server` - MCP server implementation
- `ExMCP.Server.Handler` - Server handler behaviour (implementation pattern)

### Transports
- `ExMCP.Transport.Stdio` - Standard I/O transport (primary MCP transport)
- `ExMCP.Transport.SSE` - Server-Sent Events transport (MCP-specified alternative)

## ExMCP Extensions

These features enhance the MCP experience but are not part of the official specification:

### Additional Transports
- `ExMCP.Transport.Beam` - Native Erlang/Elixir process communication
  - Local and distributed BEAM node support
  - Zero-serialization for same-VM communication
  - Process monitoring and supervision

### Discovery & Management
- `ExMCP.Discovery` - Automatic server discovery
  - Environment variable scanning
  - Configuration file parsing
  - NPM/Python package detection
  - Well-known paths scanning

- `ExMCP.ServerManager` - Multi-server management
  - Lifecycle management
  - Request routing
  - Connection pooling

### Enhanced Client Features
- **Auto-reconnection** - Exponential backoff reconnection
- **Concurrent requests** - Efficient request multiplexing
- **Connection monitoring** - Automatic failure detection

### OTP Integration
- **Supervision trees** - Fault-tolerant server processes
- **GenServer patterns** - Standard OTP behaviours
- **Hot code reloading** - Development convenience

## Usage Guidelines

### For MCP Compatibility
If you need your code to be portable across MCP implementations:
- Use only `@mcp_spec` marked features
- Stick to stdio or SSE transports
- Avoid Discovery and ServerManager
- Don't rely on auto-reconnection

### For Full ExMCP Power
If you're building an Elixir-specific solution:
- Use BEAM transport for better performance
- Leverage Discovery for automatic server finding
- Use ServerManager for complex multi-server setups
- Take advantage of OTP supervision

## Module Markers

Each ExMCP module includes markers in its documentation to clearly identify whether it's part of the MCP specification or an ExMCP extension:

### MCP Specification Modules

```elixir
@moduledoc """
@mcp_spec

Standard I/O transport implementation for MCP.
...
"""
```

These modules are marked with `@mcp_spec`:
- `ExMCP.Protocol` - JSON-RPC protocol implementation
- `ExMCP.Types` - MCP type definitions
- `ExMCP.Server` - MCP server implementation
- `ExMCP.Server.Handler` - Server handler behaviour
- `ExMCP.Transport` - Transport behaviour
- `ExMCP.Transport.Stdio` - Standard I/O transport
- `ExMCP.Transport.SSE` - Server-Sent Events transport

### ExMCP Extension Modules

```elixir
@moduledoc """
@exmcp_extension

BEAM-native transport for efficient Erlang/Elixir communication.
...
"""
```

These modules are marked with `@exmcp_extension`:
- `ExMCP.Transport.Beam` - BEAM transport
- `ExMCP.Discovery` - Server discovery
- `ExMCP.ServerManager` - Multi-server management
- `ExMCP.Specification` - Compliance tracking

### Mixed Modules

```elixir
@moduledoc """
@mcp_spec with @exmcp_extension features

MCP client for connecting to Model Context Protocol servers.
...
"""
```

These modules combine spec features with extensions:
- `ExMCP.Client` - Core MCP operations plus auto-reconnection
- `ExMCP` - Main module with overview

## Contributing

When adding new features:
1. Determine if it's part of the MCP spec or an extension
2. Add appropriate module marker
3. Update this document
4. Consider if spec features should avoid depending on extensions