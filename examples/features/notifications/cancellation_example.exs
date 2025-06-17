#!/usr/bin/env elixir

# Example demonstrating request cancellation in ExMCP
# Run with: elixir examples/cancellation_example.exs

Mix.install([
  {:ex_mcp, path: "."},
  {:jason, "~> 1.4"}
])

defmodule SlowToolHandler do
  @behaviour ExMCP.Server.Handler

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       name: "slow-tool-server",
       version: "1.0.0",
       capabilities: %{
         tools: %{}
       }
     }, state}
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "slow_operation",
        description: "A tool that takes 10 seconds to complete"
      },
      %{
        name: "fast_operation",
        description: "A tool that completes quickly"
      }
    ]

    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("slow_operation", _params, state) do
    IO.puts("Server: Starting slow operation (10 seconds)...")
    
    # Simulate a slow operation that can be interrupted
    try do
      Process.sleep(10_000)
      IO.puts("Server: Slow operation completed successfully")
      {:ok, [%{type: "text", text: "Slow operation completed after 10 seconds"}], state}
    catch
      :exit, _ ->
        IO.puts("Server: Slow operation was interrupted")
        {:error, :cancelled, state}
    end
  end

  def handle_call_tool("fast_operation", _params, state) do
    IO.puts("Server: Executing fast operation")
    {:ok, [%{type: "text", text: "Fast operation completed"}], state}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end

defmodule CancellationExample do
  def run do
    IO.puts("\n=== ExMCP Cancellation Example ===\n")

    # Start server
    {:ok, server} = ExMCP.Server.start_link(
      transport: :stdio,
      name: :cancellation_server,
      handler: SlowToolHandler
    )
    
    IO.puts("Server started")

    # Start client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :stdio,
      server: :cancellation_server
    )
    
    IO.puts("Client connected\n")

    # Wait for initialization
    Process.sleep(100)

    # Example 1: Cancel a slow operation
    example_1_cancel_slow_operation(client)
    
    # Example 2: Complete a fast operation (no cancellation)
    example_2_fast_operation(client)
    
    # Example 3: Cancel with a reason
    example_3_cancel_with_reason(client)
    
    # Example 4: Try to cancel a non-existent request (should be ignored)
    example_4_cancel_nonexistent(client)

    # Cleanup
    GenServer.stop(client)
    GenServer.stop(server)
    
    IO.puts("\n=== Example completed ===")
  end

  defp example_1_cancel_slow_operation(client) do
    IO.puts("--- Example 1: Cancelling a slow operation ---")
    
    # Start slow operation in a task
    task = Task.async(fn ->
      ExMCP.Client.call_tool(client, "slow_operation", %{})
    end)
    
    # Wait a bit then get pending requests
    Process.sleep(500)
    pending = ExMCP.Client.get_pending_requests(client)
    IO.puts("Pending requests: #{inspect(pending)}")
    
    if length(pending) > 0 do
      [request_id] = pending
      IO.puts("Cancelling request #{request_id}...")
      
      # Cancel the request
      :ok = ExMCP.Client.send_cancelled(client, request_id)
      
      # Wait for the task to complete
      result = Task.await(task, 2000)
      IO.puts("Task result: #{inspect(result)}")
    end
    
    IO.puts("")
  end

  defp example_2_fast_operation(client) do
    IO.puts("--- Example 2: Fast operation (no cancellation) ---")
    
    result = ExMCP.Client.call_tool(client, "fast_operation", %{})
    IO.puts("Result: #{inspect(result)}")
    IO.puts("")
  end

  defp example_3_cancel_with_reason(client) do
    IO.puts("--- Example 3: Cancel with a reason ---")
    
    # Start slow operation in a task
    task = Task.async(fn ->
      ExMCP.Client.call_tool(client, "slow_operation", %{})
    end)
    
    # Wait a bit then cancel with reason
    Process.sleep(500)
    pending = ExMCP.Client.get_pending_requests(client)
    
    if length(pending) > 0 do
      [request_id] = pending
      IO.puts("Cancelling request #{request_id} with reason...")
      
      # Cancel with a reason
      :ok = ExMCP.Client.send_cancelled(client, request_id, "User clicked cancel button")
      
      # Wait for the task to complete
      result = Task.await(task, 2000)
      IO.puts("Task result: #{inspect(result)}")
    end
    
    IO.puts("")
  end

  defp example_4_cancel_nonexistent(client) do
    IO.puts("--- Example 4: Cancel non-existent request (should be ignored) ---")
    
    # Try to cancel a request that doesn't exist
    :ok = ExMCP.Client.send_cancelled(client, "fake_request_id", "This should be ignored")
    IO.puts("Sent cancellation for non-existent request")
    
    # Verify client is still functional
    result = ExMCP.Client.call_tool(client, "fast_operation", %{})
    IO.puts("Client still works: #{inspect(result)}")
    IO.puts("")
  end
end

# Run the example
CancellationExample.run()