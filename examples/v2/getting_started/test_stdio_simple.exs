#!/usr/bin/env elixir

# Simple stdio test - minimal example to debug initialization

Mix.install([
  {:ex_mcp, path: "../../../"}
])

IO.puts("🧪 Simple stdio Test\n")

# Start client with stdio transport
IO.puts("1. Starting client...")
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["elixir", Path.join([__DIR__, "stdio_server.exs"])]
)

IO.puts("2. Client started: #{inspect(client)}")

# Wait a bit
Process.sleep(2000)

# Check if client is alive
IO.puts("3. Client alive? #{Process.alive?(client)}")

# Try to get client state (for debugging)
state = :sys.get_state(client)
IO.puts("4. Client state keys: #{inspect(Map.keys(state))}")
IO.puts("   Connected? #{state.connected}")
IO.puts("   Transport: #{inspect(state.transport)}")

# Try listing tools without initialization
IO.puts("\n5. Trying to list tools...")
case ExMCP.Client.list_tools(client) do
  {:ok, result} -> 
    IO.puts("   ✅ Success: #{inspect(result)}")
  {:error, reason} -> 
    IO.puts("   ❌ Error: #{inspect(reason)}")
end

# Clean up
GenServer.stop(client)
IO.puts("\n✅ Test completed")