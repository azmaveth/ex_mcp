# ExMCP Client Consolidation Migration Guide

**Version:** 0.8.0  
**Date:** June 21, 2025

## Overview

This guide helps you migrate from the multiple client implementations (`ExMCP.Client`, `ExMCP.SimpleClient`, `ExMCP.ConvenienceClient`) to the new unified `ExMCP.Client` API.

## Quick Reference

### Which client are you using?

1. **ExMCP.Client** - The original client with complex state management
2. **ExMCP.SimpleClient** - Simplified client with TransportManager
3. **ExMCP.ConvenienceClient** - High-level wrapper with convenience methods

## Migration Paths

### From ExMCP.Client (Original)

Most of your code will work unchanged, but some adjustments may be needed:

#### Connection Options
```elixir
# Old
{:ok, client} = ExMCP.Client.start_link(
  transport: {:stdio, command: "mcp-server"},
  name: :my_client
)

# New (unchanged)
{:ok, client} = ExMCP.Client.start_link(
  transport: {:stdio, command: "mcp-server"},
  name: :my_client
)
```

#### Deprecated Features
- `batch_request/2` - Removed (no longer in MCP spec)
- Complex reconnection options simplified

### From ExMCP.SimpleClient

The SimpleClient API maps closely to the new unified client:

#### Basic Usage
```elixir
# Old
{:ok, client} = ExMCP.SimpleClient.start_link(
  transport: {:stdio, command: "mcp-server"}
)
{:ok, %{"tools" => tools}} = ExMCP.SimpleClient.list_tools(client)

# New
{:ok, client} = ExMCP.Client.start_link(
  transport: {:stdio, command: "mcp-server"}
)
{:ok, %{tools: tools}} = ExMCP.Client.list_tools(client)
```

#### Key Differences
1. **Return values** - Now returns structs instead of raw maps
2. **Health checks** - Now handled automatically
3. **Reconnection** - Managed by TransportManager

### From ExMCP.ConvenienceClient

The convenience methods are now part of the main client:

#### Connection
```elixir
# Old
{:ok, client} = ExMCP.ConvenienceClient.connect("stdio://mcp-server")
{:ok, client} = ExMCP.ConvenienceClient.connect("http://localhost:8080/mcp")

# New
{:ok, client} = ExMCP.Client.connect("stdio://mcp-server")
{:ok, client} = ExMCP.Client.connect("http://localhost:8080/mcp")
```

#### Convenience Methods
```elixir
# Old
tools = ExMCP.ConvenienceClient.tools(client)
result = ExMCP.ConvenienceClient.call(client, "tool_name", %{arg: "value"})
{:ok, tool} = ExMCP.ConvenienceClient.find_tool(client, "weather")

# New
{:ok, tools} = ExMCP.Client.tools(client)
{:ok, result} = ExMCP.Client.call(client, "tool_name", %{arg: "value"})
{:ok, tool} = ExMCP.Client.find_tool(client, "weather")
```

## Unified API Reference

### Core Functions

| Function | Description | Return Type |
|----------|-------------|-------------|
| `start_link/1` | Start a client process | `{:ok, pid}` or `{:error, reason}` |
| `connect/2` | Connect using URL or spec | `{:ok, pid}` or `{:error, reason}` |
| `list_tools/2` | List available tools | `{:ok, %{tools: [...]}}` or `{:error, reason}` |
| `call_tool/4` | Call a tool | `{:ok, result}` or `{:error, reason}` |
| `list_resources/2` | List resources | `{:ok, %{resources: [...]}}` or `{:error, reason}` |
| `read_resource/3` | Read a resource | `{:ok, content}` or `{:error, reason}` |
| `list_prompts/2` | List prompts | `{:ok, %{prompts: [...]}}` or `{:error, reason}` |
| `get_prompt/4` | Get a prompt | `{:ok, prompt}` or `{:error, reason}` |

### Convenience Functions

| Function | Description | Return Type |
|----------|-------------|-------------|
| `tools/2` | Alias for list_tools | Same as list_tools |
| `call/4` | Alias for call_tool | Same as call_tool |
| `find_tool/3` | Find tool by name/pattern | `{:ok, tool}` or `{:error, :not_found}` |
| `server_info/1` | Get server information | `{:ok, info}` or `{:error, reason}` |
| `ping/2` | Ping the server | `:ok` or `{:error, reason}` |

### Options

All functions accept an options keyword list as the last parameter:

- `:timeout` - Request timeout in milliseconds (default: 5000)
- `:async` - Return immediately with a task reference (default: false)

## Migration Script

Use this script to help automate the migration:

```bash
#!/bin/bash
# migrate_clients.sh

# Backup your code first!
cp -r lib lib.backup
cp -r test test.backup

# Update SimpleClient references
find lib test -name "*.ex" -type f -exec sed -i '' \
  -e 's/ExMCP\.SimpleClient/ExMCP.Client/g' \
  -e 's/%{"tools" => tools}/%{tools: tools}/g' \
  -e 's/%{"resources" => resources}/%{resources: resources}/g' \
  -e 's/%{"prompts" => prompts}/%{prompts: prompts}/g' \
  {} \;

# Update ConvenienceClient references
find lib test -name "*.ex" -type f -exec sed -i '' \
  -e 's/ExMCP\.ConvenienceClient/ExMCP.Client/g' \
  {} \;

echo "Migration complete! Please review changes and run tests."
```

## Common Issues

### 1. Return Value Changes

The new client returns structured data instead of raw maps:

```elixir
# Old SimpleClient
{:ok, %{"tools" => [%{"name" => "tool1"}]}}

# New Client
{:ok, %{tools: [%{name: "tool1"}]}}
```

### 2. Timeout Handling

Timeouts are now consistently handled:

```elixir
# Old (varied by client)
ExMCP.Client.call_tool(client, "tool", %{}, 10_000)
ExMCP.SimpleClient.call_tool(client, "tool", %{}, 10_000)

# New (consistent)
ExMCP.Client.call_tool(client, "tool", %{}, timeout: 10_000)
```

### 3. Error Handling

Errors now return consistent structures:

```elixir
# Old (varied)
{:error, "connection failed"}
{:error, %{error: "connection failed"}}

# New (consistent)
{:error, %ExMCP.Error{type: :transport, message: "connection failed"}}
```

## Testing Your Migration

1. **Run existing tests** - Most should pass with minor adjustments
2. **Check return values** - Update pattern matches for new structures
3. **Verify timeouts** - Ensure timeout options are passed correctly
4. **Test error cases** - Update error handling for new error structure

## Rollback Plan

If you need to rollback:

1. The old modules are deprecated but still available
2. Revert your code changes
3. Pin to ex_mcp version 0.7.x in mix.exs

## Getting Help

- Check the [CHANGELOG](CHANGELOG.md) for detailed changes
- See [examples/](examples/) for updated usage patterns
- Open an issue for migration problems

## Timeline

- **v0.8.0** - New unified client available, old clients deprecated
- **v0.9.0** - Deprecation warnings added to old clients
- **v1.0.0** - Old client modules removed

---

Remember: The new unified client combines the best of all three implementations while providing a consistent, maintainable API for the future.