# Migration Guide

This guide helps you upgrade your ExMCP applications between versions. Each section covers breaking changes and provides migration examples.

## Table of Contents

- [Upgrading to v0.6.0 from v0.5.x](#upgrading-to-v060-from-v05x)
- [Upgrading to v0.5.0 from v0.4.x](#upgrading-to-v050-from-v04x)
- [General Migration Tips](#general-migration-tips)

## Upgrading to v0.6.0 from v0.5.x

v0.6.0 includes significant enhancements and some breaking changes. This section provides step-by-step migration instructions.

### 1. Update Dependencies

Update your `mix.exs`:

```elixir
# Before (v0.5.x)
{:ex_mcp, "~> 0.5.0"}

# After (v0.6.x)
{:ex_mcp, "~> 0.6.0"}
```

Run `mix deps.update ex_mcp` to get the latest version.

### 2. Test Tagging System (New Feature)

v0.6.0 introduces a comprehensive test tagging system. If you have existing tests, consider adopting the new tagging strategy:

```elixir
# Add module tags to your tests
defmodule MyProjectTest do
  use ExUnit.Case, async: true
  
  # Add relevant tags
  @moduletag :unit
  @moduletag :my_feature
  
  # Your existing tests...
end
```

**New test tasks available:**
- `mix test.suite unit` - Run only unit tests
- `mix test.suite integration` - Run integration tests
- `mix test.tags` - List all available tags

### 3. OAuth 2.1 Enhancements

If you're using OAuth features, no breaking changes are required, but new capabilities are available:

```elixir
# New OAuth configuration options in config/config.exs
config :ex_mcp, :oauth2_server_config,
  # Enhanced security features now available
  introspection_endpoint: "https://auth.example.com/introspect",
  authorization_server: "https://auth.example.com",
  required_scopes: ["mcp:read"],
  token_cache_ttl: :timer.minutes(5)
```

### 4. MCP 2025-06-18 Protocol Support

v0.6.0 adds support for the latest MCP protocol version. Existing servers continue working, but you can opt into new features:

```elixir
# In your configuration
config :ex_mcp,
  protocol_version: "2025-06-18"  # Latest version
```

**New features available:**
- Structured tool output with `outputSchema`
- Enhanced resource metadata
- Improved security features

### 5. Native Service Dispatcher Updates

If you were using experimental `:beam` transport, migrate to the Native Service Dispatcher:

```elixir
# Before (deprecated :beam transport)
ExMCP.Client.start_link(transport: :beam, ...)

# After (Native Service Dispatcher)
defmodule MyService do
  use ExMCP.Service
  
  # Service automatically registered with Horde.Registry
end

# Call services with zero serialization overhead
ExMCP.Native.call_service(:my_service, "tool_name", %{args: "here"})
```

### 6. Python MCP SDK Interoperability

v0.6.0 adds complete Python MCP SDK integration examples. No breaking changes, but new capabilities:

- Elixir clients ↔ Python servers (stdio and HTTP)
- Python clients ↔ Elixir servers
- Hybrid architectures with performance-based routing

See `examples/python_integration/` for complete examples.

## Upgrading to v0.5.0 from v0.4.x

### 1. Transport Renaming

The biggest breaking change in v0.5.0 was transport renaming:

```elixir
# Before (v0.4.x)
ExMCP.Client.start_link(transport: :sse, ...)
ExMCP.Server.start_link(transport: :sse, ...)

# After (v0.5.x+)
ExMCP.Client.start_link(transport: :http, ...)
ExMCP.Server.start_link(transport: :http, ...)
```

**Rationale:** The `:sse` transport was renamed to `:http` to better reflect that it supports both regular HTTP and Server-Sent Events.

### 2. Authorization API Changes

OAuth 2.1 integration was significantly enhanced:

```elixir
# Before (v0.4.x)
# Limited OAuth support

# After (v0.5.x+)
# Full OAuth 2.1 Resource Server implementation
config :ex_mcp, :oauth2_server_config,
  introspection_endpoint: "https://auth.example.com/introspect",
  required_scopes: ["mcp:read"]
```

### 3. Logging Integration

Enhanced MCP logging protocol support:

```elixir
# New in v0.5.0+
ExMCP.Logging.set_level(:debug)
ExMCP.Logging.send_message(:info, "Operation completed", %{result: "success"})
```

## General Migration Tips

### 1. Check Breaking Changes

Always review the [CHANGELOG.md](CHANGELOG.md) for your target version to understand all breaking changes.

### 2. Update Tests Gradually

When upgrading:

1. Run your existing test suite against the new version
2. Fix any failing tests due to API changes
3. Consider adopting new testing patterns (like the v0.6.0 tagging system)

### 3. Configuration Updates

Review your `config/config.exs` for new configuration options:

```elixir
# Common configuration to review
config :ex_mcp,
  protocol_version: "2025-06-18",  # Latest protocol version
  oauth2_enabled: true,            # If using OAuth
  structured_output_enabled: true # New in v0.6.0
```

### 4. Example Code Updates

Check the `examples/` directory for updated patterns and new integration examples that match your use case.

### 5. Performance Considerations

Each version includes performance improvements. Consider:

- Using Native Service Dispatcher for ~15μs local calls
- HTTP transport for network communication
- stdio transport for external tool integration

## Getting Help

If you encounter issues during migration:

1. Check the [TROUBLESHOOTING.md](TROUBLESHOOTING.md) guide
2. Review relevant examples in the `examples/` directory
3. Open an issue on GitHub with your specific migration scenario

## Version Support

- **v0.6.x**: Current stable version with full MCP 2025-06-18 support
- **v0.5.x**: Previous stable version, still supported
- **v0.4.x**: Legacy version, upgrade recommended

Keep your ExMCP version up to date to benefit from the latest MCP protocol features and security improvements.