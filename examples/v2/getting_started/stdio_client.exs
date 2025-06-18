#!/usr/bin/env elixir

# stdio client example - connects to stdio_minimal.exs

Mix.install([
  {:ex_mcp, path: "../../../"}
])

IO.puts("🧪 stdio Client Demo\n")

# Connect to the minimal stdio server
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["elixir", Path.join([__DIR__, "stdio_minimal.exs"])]
)

IO.puts("✅ Client started")

# Wait for server to initialize
Process.sleep(3000)

# List tools (this will auto-initialize if needed)
IO.puts("\n📋 Listing tools...")
case ExMCP.Client.list_tools(client) do
  {:ok, %{"tools" => tools}} ->
    IO.puts("Found #{length(tools)} tools:")
    Enum.each(tools, fn tool ->
      IO.puts("  - #{tool["name"]}: #{tool["description"]}")
    end)
    
    # Call echo tool
    IO.puts("\n📤 Calling echo tool...")
    case ExMCP.Client.call_tool(client, "echo", %{"text" => "Hello from v2 DSL!"}) do
      {:ok, result} ->
        case result do
          %{"content" => [%{"text" => text} | _]} ->
            IO.puts("✅ #{text}")
          _ ->
            IO.puts("✅ Result: #{inspect(result)}")
        end
      {:error, reason} ->
        IO.puts("❌ Error: #{inspect(reason)}")
    end
    
  {:error, reason} ->
    IO.puts("❌ Error: #{inspect(reason)}")
end

# Clean up
GenServer.stop(client)
IO.puts("\n✅ Demo completed!")