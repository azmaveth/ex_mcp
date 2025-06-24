# Handler to DSL Migration Guide

This guide documents the complete process for migrating test files from the legacy `ExMCP.Server.Handler` behavior pattern to the modern `use ExMCP.Server` DSL pattern.

## Overview

The legacy tests use a handler-based architecture that was removed during cleanup. Tests must be migrated to use the current DSL-based server architecture while preserving all functionality and test coverage.

## Migration Checklist

### 1. Handler Module Conversion

**OLD PATTERN:**
```elixir
defmodule TestHandler do
  use ExMCP.Server.Handler
  
  @impl true
  def handle_initialize(_params, state), do: {...}
  
  @impl true 
  def handle_list_tools(_cursor, state), do: {...}
  
  @impl true
  def handle_call_tool(name, args, state), do: {...}
end
```

**NEW PATTERN:**
```elixir
defmodule TestServer do
  use ExMCP.Server
  
  deftool "tool_name" do
    meta do
      description("Tool description")
    end
    
    input_schema(%{
      type: "object",
      properties: %{...},
      required: [...]
    })
  end
  
  @impl true
  def handle_tool_call("tool_name", args, state), do: {...}
end
```

### 2. Tool Definitions Migration

Convert `handle_list_tools/2` to individual `deftool` blocks:

**Steps:**
1. Extract each tool definition from the tools list
2. Create a `deftool` block for each tool
3. Move `inputSchema` to `input_schema()` macro call
4. Add `meta` block with `description()`
5. Keep the `handle_call_tool/3` implementation

**Example:**
```elixir
# OLD: In handle_list_tools/2
%{
  name: "calculate",
  description: "Perform calculations", 
  inputSchema: %{type: "object", properties: %{...}}
}

# NEW: DSL definition
deftool "calculate" do
  meta do
    description("Perform calculations")
  end
  
  input_schema(%{
    type: "object",
    properties: %{...}
  })
end
```

### 3. Resource Definitions Migration

Convert `handle_list_resources/2` to `defresource` blocks:

```elixir
# OLD: In handle_list_resources/2
%{
  uri: "config://app",
  name: "App Config",
  description: "Application configuration",
  mime_type: "application/json"
}

# NEW: DSL definition  
defresource "config://app" do
  meta do
    name("App Config")
    description("Application configuration")
  end
  
  mime_type("application/json")
end
```

### 4. Test Setup Migration

**OLD SETUP:**
```elixir
setup do
  {:ok, server} = Server.start_link(
    handler: TestHandler,
    transport: :test
  )
  
  {:ok, client} = Client.start_link(
    transport: :test,
    server: server
  )
  
  %{server: server, client: client}
end
```

**NEW SETUP:**
```elixir
setup do
  {:ok, server} = TestServer.start_link(transport: :native)
  
  on_exit(fn ->
    if Process.alive?(server), do: GenServer.stop(server)
  end)
  
  %{server: server}
end
```

### 5. Test Assertion Updates

**Key Finding:** Content response format differs between patterns.

**DSL servers use atom keys:**
```elixir
assert content[:type] == "text"
assert content[:text] == "Result: 8"
```

**Handler servers used string keys:**
```elixir
assert content["type"] == "text"  
assert content["text"] == "Result: 8"
```

**Update all test assertions to use atom keys.**

### 6. State Management

If the handler had custom `init/1`, preserve it:

```elixir
# In DSL server
@impl true
def init(args) do
  initial_state = %{
    call_count: 0,
    last_progress_token: nil
  }
  {:ok, Map.merge(Map.new(args), initial_state)}
end
```

### 7. Progress Token Handling

Preserve progress token extraction in tool calls:

```elixir
@impl true
def handle_tool_call(name, arguments, state) do
  progress_token = get_in(arguments, ["_meta", "progressToken"])
  new_state = %{state | last_progress_token: progress_token}
  # ... tool implementation
end
```

## Common Patterns

### Multi-Tool Handler
Convert each tool in the case statement to a separate `deftool` block and `handle_tool_call/3` clause.

### Pagination Support
If the handler implements pagination in `handle_list_tools/2`, you may need to override `handle_tool_list/1` in the DSL server.

### Error Handling
Preserve error responses with `is_error: true` flag:

```elixir
{:ok, %{
  content: [%{type: "text", text: "Error: Division by zero"}],
  is_error: true
}, state}
```

### Async Operations
Preserve Task spawning and progress notification patterns.

## Validation Steps

1. **Compilation**: Ensure the migrated test compiles without errors
2. **Test Execution**: All individual tests should pass
3. **Coverage**: Verify test coverage is maintained  
4. **Functionality**: Ensure all original behavior is preserved

## Example Migration

See `test/ex_mcp/tools_test_migrated.exs` for a complete example migration from `test/ex_mcp/tools_test.exs`.

**Migration Results:**
- ✅ 6 tests passing (6 tests, 0 failures)
- ✅ All functionality preserved
- ✅ DSL patterns established
- ✅ Ready for template application

## Next Steps

1. Apply this template to migrate core tests (ResourcesTest, ComplianceTest)
2. Create batch migration scripts for remaining 34+ test files
3. Remove original handler-based test files once migration is complete