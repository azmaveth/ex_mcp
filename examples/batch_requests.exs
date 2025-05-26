#!/usr/bin/env elixir

# Example: Using batch requests for efficient multi-operation calls

Mix.install([
  {:ex_mcp, path: "."}
])

defmodule BatchExample do
  def run do
    # Start a simple echo server
    {:ok, server} = ExMCP.Server.start_link(
      transport: :beam,
      name: :batch_server,
      handler: ExMCP.Server.Handler.Echo
    )

    # Connect client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :batch_server
    )

    # Wait for initialization
    Process.sleep(100)

    IO.puts("Sending individual requests...")
    start_time = System.monotonic_time(:millisecond)

    # Individual requests (slower)
    {:ok, tools} = ExMCP.Client.list_tools(client)
    {:ok, resources} = ExMCP.Client.list_resources(client)
    {:ok, roots} = ExMCP.Client.list_roots(client)
    {:ok, echo_result} = ExMCP.Client.call_tool(client, "echo", %{"message" => "Hello"})

    individual_time = System.monotonic_time(:millisecond) - start_time
    IO.puts("Individual requests took: #{individual_time}ms")

    IO.puts("\nSending batch request...")
    start_time = System.monotonic_time(:millisecond)

    # Batch request (faster)
    requests = [
      {:list_tools, []},
      {:list_resources, []},
      {:list_roots, []},
      {:call_tool, ["echo", %{"message" => "Hello"}]}
    ]

    {:ok, [batch_tools, batch_resources, batch_roots, batch_echo]} = 
      ExMCP.Client.batch_request(client, requests)

    batch_time = System.monotonic_time(:millisecond) - start_time
    IO.puts("Batch request took: #{batch_time}ms")

    # Verify results are the same
    IO.puts("\nVerifying results match:")
    IO.puts("Tools match: #{tools == batch_tools}")
    IO.puts("Resources match: #{resources == batch_resources}")
    IO.puts("Roots match: #{roots == batch_roots}")
    IO.puts("Echo match: #{echo_result == batch_echo}")

    IO.puts("\nPerformance improvement: #{Float.round(individual_time / batch_time, 2)}x faster")

    # Complex batch with mixed operations
    IO.puts("\nComplex batch example:")
    complex_batch = [
      {:ping, []},
      {:list_tools, []},
      {:call_tool, ["echo", %{"message" => "First"}]},
      {:call_tool, ["echo", %{"message" => "Second"}]},
      {:call_tool, ["echo", %{"message" => "Third"}]}
    ]

    {:ok, results} = ExMCP.Client.batch_request(client, complex_batch)
    
    IO.puts("Batch contained #{length(results)} results")
    Enum.each(Enum.with_index(results), fn {result, idx} ->
      IO.puts("  Result #{idx + 1}: #{inspect(result, limit: 50)}")
    end)

    # Cleanup
    ExMCP.Client.stop(client)
    GenServer.stop(server)
  end
end

BatchExample.run()