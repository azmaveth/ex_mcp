# Examples Migration Guide

## Issue: Non-existent `:beam` Transport

Many examples are using `transport: :beam` which doesn't exist as a valid transport option. The BEAM-related functionality has been reorganized into the Native Service Dispatcher.

## Migration Paths

### Option 1: Use Native Service Dispatcher (Recommended for internal services)

For high-performance communication between Elixir services, use the Native Service Dispatcher:

**Before (Non-working):**
```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  transport: :beam,
  handler: MyHandler
)

# Client
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: server
)
```

**After (Using Native Service Dispatcher):**
```elixir
# Define service using ExMCP.Service macro
defmodule MyService do
  use ExMCP.Service, name: :my_service
  
  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    {:ok, %{"tools" => []}, state}
  end
end

# Start service (auto-registers with Native)
{:ok, _} = MyService.start_link()

# Call service directly
{:ok, result} = ExMCP.Native.call(:my_service, "list_tools", %{})
```

### Option 2: Use Test Transport (For testing/examples)

For simple examples and testing, use the test transport:

```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  transport: :test,
  handler: MyHandler
)

# Client
{:ok, client} = ExMCP.Client.start_link(
  transport: :test,
  test_messages: [...]  # Pre-defined test messages
)
```

### Option 3: Use stdio Transport (For real MCP communication)

For standard MCP protocol communication:

```elixir
# Server
{:ok, server} = ExMCP.Server.start_link(
  transport: :stdio,
  handler: MyHandler
)

# Client
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["elixir", "server.exs"]
)
```

## Files Requiring Updates

The following example files need to be updated to remove `transport: :beam`:

1. `/examples/batch_requests.exs` - 2 occurrences
2. `/examples/beam_transport/` - Multiple files need complete rewrite
3. `/examples/advanced_features/notifications_server.ex` - 2 occurrences
4. `/examples/advanced_features/sampling_server.ex` - 2 occurrences
5. `/examples/audio_content_example.exs` - 2 occurrences
6. Many others...

## Recommendation

For examples demonstrating internal Elixir service communication, rewrite them to use the Native Service Dispatcher pattern. For examples demonstrating MCP protocol features, use stdio or HTTP transports.