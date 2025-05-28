# API Categories Guide

This guide explains the three categories of APIs in ExMCP and how they are marked in the documentation.

## API Categories

### 1. MCP Specification Features (Portable)

These APIs implement the official Model Context Protocol specification and are portable across all MCP implementations.

**Module marking:**
```elixir
@moduledoc """
@mcp_spec

Module description...
"""
```

**Examples:**
- `ExMCP.Client` - Core client operations (list_tools, call_tool, etc.)
- `ExMCP.Server` - Core server functionality
- `ExMCP.Protocol` - Message encoding/decoding
- `ExMCP.Types` - Standard MCP types
- `ExMCP.Authorization` - OAuth 2.1 support (part of MCP spec)

### 2. ExMCP Extensions (Elixir-specific)

These features are specific to ExMCP and not part of the official MCP specification.

**Module marking:**
```elixir
@moduledoc """
@exmcp_extension

Module description...

> #### Extension Module {: .info}
> This module is an ExMCP extension not part of the official MCP specification.
"""
```

**Function marking in mixed modules:**
```elixir
@doc """
Description...

> #### Extension Feature {: .warning}
> This is an ExMCP extension, not part of the MCP specification.
"""
```

**Examples:**
- `ExMCP.ServerManager` - Multi-server management
- `ExMCP.Discovery` - Server discovery
- `ExMCP.Transport.Beam` - BEAM process transport
- `ExMCP.Client.batch_request/3` - Batch operations
- `ExMCP.Client.get_pending_requests/1` - Request tracking

### 3. Draft Specification Features (Experimental)

These features implement the draft MCP specification and may change.

**Module marking:**
```elixir
@moduledoc """
@mcp_draft

Module description...

> #### Draft Feature {: .info}
> This implements draft MCP specification features that may change.
"""
```

**Function marking:**
```elixir
@doc """
Description...

> #### Draft Feature {: .info}
> This implements a draft MCP specification feature that may change.

@doc api: :draft
"""
```

**Current Draft Features:**
- **Structured Tool Output**:
  - Tools can define `outputSchema` in their schema
  - Tool results can include `structuredContent` alongside regular content
  - See `ExMCP.Types` for field definitions marked as "Draft feature"
- **Logging Level Control**:
  - `ExMCP.Client.set_log_level/3` - Set server log verbosity
  - `handle_set_log_level/2` callback in server handlers
  - Uses `logging/setLevel` protocol method

## Mixed Modules

Some modules contain both specification features and extensions:

```elixir
defmodule ExMCP.Client do
  @moduledoc """
  MCP client for connecting to Model Context Protocol servers.

  This module provides both MCP specification features and ExMCP extensions.

  ## MCP Specification Features
  
  Core protocol operations that are portable across all MCP implementations:
  - list_tools/2
  - call_tool/4
  ...

  ## ExMCP Extensions
  
  > #### Extension Features {: .warning}
  > These features are specific to ExMCP and not part of the official MCP specification.
  
  - batch_request/3
  - get_pending_requests/1
  ...
  """
end
```

## Writing Portable Code

To write code that works with any MCP implementation:

1. Only use functions marked as `@mcp_spec`
2. Use stdio or SSE transport (not BEAM)
3. Avoid extension features like batch operations
4. Don't rely on auto-reconnection

Example of portable code:
```elixir
# Portable - works with any MCP implementation
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"]
)
{:ok, tools} = ExMCP.Client.list_tools(client)
```

Example of non-portable code:
```elixir
# Non-portable - uses ExMCP extensions
{:ok, results} = ExMCP.Client.batch_request(client, [
  {:list_tools, []},
  {:list_resources, []}
])
```

## Checking API Category

To determine if an API is portable:

1. Check the module documentation for `@mcp_spec`, `@exmcp_extension`, or `@mcp_draft`
2. Look for warning boxes in function documentation
3. Check this guide for the module/function listing

When in doubt, assume an API is an extension unless explicitly marked as specification-compliant.