#!/usr/bin/env elixir

# Example: Using batch requests for efficient multi-operation calls
# Fixed version using stdio transport

Mix.install([
  {:ex_mcp, path: "."}
])

defmodule BatchExample do
  def run do
    # Start a simple echo server using stdio transport
    {:ok, server} = ExMCP.Server.start_link(
      transport: :stdio,
      handler: ExMCP.Server.Handler.Echo
    )

    # Connect client via stdio
    {:ok, client} = ExMCP.Client.start_link(
      transport: :stdio,
      server: server
    )

    # Initialize the connection
    {:ok, _} = ExMCP.Client.initialize(client, %{
      protocolVersion: ExMCP.protocol_version(),
      clientInfo: %{name: "batch-example", version: "1.0.0"},
      capabilities: %{}
    })

    IO.puts("Sending individual requests...")
    start_time = System.monotonic_time(:millisecond)

    # Individual requests (slower due to round-trip overhead)
    {:ok, %{tools: tools}} = ExMCP.Client.list_tools(client)
    {:ok, %{resources: resources}} = ExMCP.Client.list_resources(client)
    {:ok, %{roots: roots}} = ExMCP.Client.list_roots(client)
    {:ok, echo_result} = ExMCP.Client.call_tool(client, "echo", %{message: "Hello"})

    individual_time = System.monotonic_time(:millisecond) - start_time
    IO.puts("Individual requests took: #{individual_time}ms")
    IO.puts("Found #{length(tools)} tools")

    IO.puts("\nSending batch request...")
    start_time = System.monotonic_time(:millisecond)

    # Batch request - sends all requests in one round-trip
    requests = [
      {:list_tools, []},
      {:list_resources, []}, 
      {:list_roots, []},
      {:call_tool, ["echo", %{message: "Hello"}]}
    ]

    {:ok, [batch_tools, batch_resources, batch_roots, batch_echo]} =
      ExMCP.Client.batch_request(client, requests)

    batch_time = System.monotonic_time(:millisecond) - start_time
    IO.puts("Batch request took: #{batch_time}ms")

    IO.puts("\nResults match: #{tools == batch_tools.tools}")
    IO.puts("\nComparison:")
    IO.puts("Individual: #{individual_time}ms")
    IO.puts("Batch: #{batch_time}ms")
    if batch_time > 0 do
      IO.puts("Speedup: #{Float.round(individual_time / batch_time, 2)}x")
    end

    # Cleanup
    GenServer.stop(client)
    GenServer.stop(server)

    :ok
  end
end

# Run the example
BatchExample.run()