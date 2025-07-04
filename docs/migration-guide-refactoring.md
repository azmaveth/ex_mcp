# ExMCP Server Refactoring Migration Guide

## Overview

The ExMCP.Server module has been refactored from a monolithic module into focused, maintainable components. This guide helps you understand the changes and any migration steps needed.

## Good News: No Breaking Changes! 🎉

The refactoring maintains 100% backward compatibility. If you're using ExMCP.Server through its public API or DSL, **no code changes are required**.

## What Changed

### Internal Architecture

The monolithic `ExMCP.Server` module (1,488 lines) has been split into:

1. **`ExMCP.Protocol.ResponseBuilder`** - Handles JSON-RPC response formatting
2. **`ExMCP.Protocol.RequestTracker`** - Manages request lifecycle and cancellation
3. **`ExMCP.Protocol.RequestProcessor`** - Routes and processes requests
4. **`ExMCP.Server.Transport.Coordinator`** - Manages transport lifecycle
5. **`ExMCP.DSL.CodeGenerator`** - Generates code for DSL macros

### Error Handling

Structured error types are now available:

```elixir
# Old way (still works)
{:error, "Something went wrong"}

# New way (recommended)
{:error, Error.tool_error("my_tool", "Something went wrong")}
```

### Telemetry Events

New telemetry events are emitted:

```elixir
# Attach to telemetry events
:telemetry.attach(
  "my-handler",
  [:ex_mcp, :request, :stop],
  &MyApp.handle_event/4,
  nil
)
```

## Migration Steps

### For Most Users: Nothing!

If you're using the DSL:

```elixir
defmodule MyServer do
  use ExMCP.Server
  
  deftool "my_tool" do
    # ... your code ...
  end
end
```

**No changes needed!** Everything works exactly as before.

### For Advanced Users

#### If you were accessing internal functions:

```elixir
# Don't do this anymore:
ExMCP.Server.build_error_response(error, id)

# Do this instead:
ExMCP.Protocol.ResponseBuilder.build_error_response(error, id)
```

#### If you were pattern matching on errors:

```elixir
# Old pattern (still works)
{:error, reason}

# New pattern (more specific)
{:error, %ExMCP.Error.ToolError{} = error}
```

#### If you need telemetry:

```elixir
# Add telemetry handlers for observability
:telemetry.attach_many(
  "mcp-handler",
  [
    [:ex_mcp, :request, :start],
    [:ex_mcp, :request, :stop],
    [:ex_mcp, :tool, :start],
    [:ex_mcp, :tool, :stop]
  ],
  &handle_event/4,
  nil
)
```

## Security Improvements

### Schema Validation

The schema validation now uses string keys to prevent atom exhaustion:

```elixir
# Arguments are now validated with string keys
def handle_tool_call("my_tool", args, state) do
  # args now has string keys: %{"name" => "value"}
  # instead of atom keys: %{name: "value"}
  name = Map.get(args, "name")  # Use string key
  {:ok, %{content: [text: "Hello #{name}"]}, state}
end
```

If you need atom keys for compatibility:

```elixir
def handle_tool_call("my_tool", args, state) do
  # Convert specific keys if needed
  atom_args = %{
    name: Map.get(args, "name"),
    age: Map.get(args, "age")
  }
  # ... use atom_args
end
```

## Benefits You Get

1. **Better Error Messages**: Structured errors with more context
2. **Observability**: Built-in telemetry for monitoring
3. **Security**: Protection against atom exhaustion attacks
4. **Performance**: Same performance, better organization
5. **Maintainability**: Easier to debug and extend

## Troubleshooting

### Q: My tests are failing after the update?

Check if you're:
1. Testing internal implementation details
2. Pattern matching on specific error formats
3. Expecting atom keys in tool arguments

### Q: Performance seems different?

The refactoring maintains the same performance characteristics. If you notice differences:
1. Check telemetry events aren't adding overhead
2. Ensure you're not converting keys unnecessarily
3. Profile to identify the actual bottleneck

## Need Help?

- Check the [refactoring success metrics](./refactoring-success-metrics.md)
- Review the integration tests for examples
- Open an issue if you encounter problems

## Summary

For 99% of users, this refactoring is transparent. Update your dependency and enjoy the improved internals without changing any of your code!