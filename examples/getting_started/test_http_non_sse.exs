#!/usr/bin/env elixir

Mix.install([
  {:ex_mcp, path: "../.."}
])

IO.puts("Testing HTTP without SSE...")

{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8321",
  endpoint: "/",
  use_sse: false
)

start_time = System.monotonic_time(:millisecond)

try do
  result = ExMCP.Client.call_tool(client, "say_hello", %{"name" => "Test"})
  end_time = System.monotonic_time(:millisecond)
  duration = end_time - start_time
  IO.puts("✅ HTTP without SSE successful in #{duration}ms")
  IO.puts("   Result: #{inspect(result)}")
rescue
  e ->
    end_time = System.monotonic_time(:millisecond) 
    duration = end_time - start_time
    IO.puts("❌ HTTP without SSE failed after #{duration}ms: #{inspect(e)}")
end

GenServer.stop(client)