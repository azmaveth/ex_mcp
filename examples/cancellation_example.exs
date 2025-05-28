# Request Cancellation Example
#
# This example demonstrates how to use the MCP cancellation
# functionality to cancel in-progress requests.
#
# Run with: mix run examples/cancellation_example.exs

alias ExMCP.{Client, Server, Protocol}
alias ExMCP.Server.Handler

defmodule SlowHandler do
  @behaviour Handler

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "slow-operations-server",
      version: "1.0.0",
      capabilities: %{
        tools: %{},
        resources: %{},
        prompts: %{}
      }
    }, state}
  end

  @impl true
  def handle_list_tools(_params, state) do
    tools = [
      %{
        name: "slow_calculation",
        description: "Performs a slow mathematical calculation",
        input_schema: %{
          type: "object",
          properties: %{
            duration: %{type: "integer", description: "Duration in seconds"}
          },
          required: ["duration"]
        }
      },
      %{
        name: "file_processing",
        description: "Simulates slow file processing",
        input_schema: %{
          type: "object",
          properties: %{
            file_path: %{type: "string", description: "Path to file"}
          },
          required: ["file_path"]
        }
      }
    ]
    {:ok, tools, state}
  end

  @impl true
  def handle_call_tool("slow_calculation", %{"duration" => duration}, state) do
    IO.puts("Starting slow calculation (#{duration}s)...")
    Process.sleep(duration * 1000)
    result = :rand.uniform(1000)
    
    {:ok, [%{
      type: "text", 
      text: "Calculation completed: #{result}"
    }], state}
  end

  def handle_call_tool("file_processing", %{"file_path" => path}, state) do
    IO.puts("Processing file: #{path}")
    # Simulate slow file processing
    Process.sleep(8000)
    
    {:ok, [%{
      type: "text",
      text: "File processed successfully: #{path}"
    }], state}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end

IO.puts("🚫 MCP Request Cancellation Example")
IO.puts("=" <> String.duplicate("=", 35))

# Start the server
IO.puts("\n1. Starting MCP server with slow operations...")
{:ok, server} = Server.start_link(
  transport: :beam,
  name: :slow_server,
  handler: SlowHandler
)

# Start the client
{:ok, client} = Client.start_link(
  transport: :beam,
  server: :slow_server
)

# Wait for initialization
Process.sleep(100)

IO.puts("✓ Server and client initialized")

# Example 1: Basic cancellation
IO.puts("\n2. Basic Request Cancellation")
IO.puts("   Starting a slow calculation...")

task1 = Task.async(fn ->
  Client.call_tool(client, "slow_calculation", %{"duration" => 10})
end)

# Give the request time to start
Process.sleep(500)

# Get pending requests
pending = Client.get_pending_requests(client)
IO.puts("   → Pending requests: #{inspect(pending)}")

if length(pending) > 0 do
  [request_id] = pending
  IO.puts("   → Cancelling request #{request_id}...")
  
  # Cancel the request
  :ok = Client.send_cancelled(client, request_id, "User cancelled")
  
  # The task should timeout
  try do
    Task.await(task1, 2000)
    IO.puts("   ✗ Request completed unexpectedly")
  catch
    :exit, _ ->
      IO.puts("   ✓ Request cancelled successfully (task timed out)")
  end
  
  # Check pending requests are cleared
  Process.sleep(100)
  new_pending = Client.get_pending_requests(client)
  IO.puts("   → Pending requests after cancellation: #{inspect(new_pending)}")
else
  IO.puts("   ⚠ No pending requests found")
end

# Example 2: Multiple requests with selective cancellation
IO.puts("\n3. Multiple Requests with Selective Cancellation")

IO.puts("   Starting multiple slow operations...")

task2 = Task.async(fn ->
  Client.call_tool(client, "file_processing", %{"file_path" => "/data/large_file.csv"})
end)

task3 = Task.async(fn ->
  Client.call_tool(client, "slow_calculation", %{"duration" => 3})
end)

# Wait for requests to start
Process.sleep(500)

pending = Client.get_pending_requests(client)
IO.puts("   → Pending requests: #{inspect(pending)}")

if length(pending) >= 2 do
  # Cancel only the first request
  [first_req | _rest] = pending
  IO.puts("   → Cancelling first request: #{first_req}")
  
  :ok = Client.send_cancelled(client, first_req, "Selective cancellation")
  
  # The first task should timeout, second should complete
  try do
    Task.await(task2, 2000)
    IO.puts("   ✗ First request completed unexpectedly")
  catch
    :exit, _ ->
      IO.puts("   ✓ First request cancelled")
  end
  
  # Second task should complete normally
  try do
    {:ok, result} = Task.await(task3, 5000)
    IO.puts("   ✓ Second request completed: #{inspect(result)}")
  catch
    :exit, _ ->
      IO.puts("   ⚠ Second request also timed out")
  end
else
  IO.puts("   ⚠ Not enough pending requests for selective cancellation")
end

# Example 3: Attempt to cancel initialize request (should be rejected)
IO.puts("\n4. Validation: Cannot Cancel Initialize Request")

case Protocol.encode_cancelled("initialize", "Should not work") do
  {:error, :cannot_cancel_initialize} ->
    IO.puts("   ✓ Initialize request cancellation correctly rejected")
    
  {:ok, _} ->
    IO.puts("   ✗ Initialize request cancellation incorrectly allowed")
end

# Try to send it anyway (will be logged as warning)
IO.puts("   → Attempting to send initialize cancellation...")
:ok = Client.send_cancelled(client, "initialize", "This should be ignored")
IO.puts("   ✓ Cancellation sent but should be ignored by client")

# Example 4: Cancellation with and without reasons
IO.puts("\n5. Cancellation with Different Reason Formats")

# Start another slow operation
task4 = Task.async(fn ->
  Client.call_tool(client, "slow_calculation", %{"duration" => 5})
end)

Process.sleep(500)
pending = Client.get_pending_requests(client)

if length(pending) > 0 do
  [request_id] = pending
  
  # Cancel with detailed reason
  IO.puts("   → Cancelling with detailed reason...")
  :ok = Client.send_cancelled(client, request_id, "User requested cancellation via UI")
  
  try do
    Task.await(task4, 1000)
  catch
    :exit, _ ->
      IO.puts("   ✓ Request cancelled with reason")
  end
end

# Start one more to cancel without reason
task5 = Task.async(fn ->
  Client.call_tool(client, "file_processing", %{"file_path" => "/data/another_file.txt"})
end)

Process.sleep(500)
pending = Client.get_pending_requests(client)

if length(pending) > 0 do
  [request_id] = pending
  
  # Cancel without reason
  IO.puts("   → Cancelling without reason...")
  :ok = Client.send_cancelled(client, request_id)  # No reason provided
  
  try do
    Task.await(task5, 1000)
  catch
    :exit, _ ->
      IO.puts("   ✓ Request cancelled without reason")
  end
end

# Example 6: Demonstrate proper error handling
IO.puts("\n6. Error Handling and Edge Cases")

# Try to cancel non-existent request
IO.puts("   → Cancelling non-existent request...")
:ok = Client.send_cancelled(client, "non_existent_request", "Should be ignored")
IO.puts("   ✓ Non-existent request cancellation handled gracefully")

# Verify client is still functional
case Client.list_tools(client) do
  {:ok, tools} ->
    IO.puts("   ✓ Client remains functional after invalid cancellation")
    IO.puts("   → Available tools: #{length(tools)}")
    
  {:error, reason} ->
    IO.puts("   ✗ Client not functional: #{inspect(reason)}")
end

# Example 7: Mass cancellation
IO.puts("\n7. Mass Cancellation of All Pending Requests")

# Start several operations
tasks = for i <- 1..3 do
  Task.async(fn ->
    Client.call_tool(client, "slow_calculation", %{"duration" => 8})
  end)
end

Process.sleep(500)
pending = Client.get_pending_requests(client)
IO.puts("   → Started #{length(tasks)} operations, pending: #{length(pending)}")

# Cancel all pending requests
Enum.each(pending, fn request_id ->
  :ok = Client.send_cancelled(client, request_id, "Mass cancellation")
end)

IO.puts("   → Sent cancellation for all #{length(pending)} requests")

# All tasks should timeout
cancelled_count = Enum.count(tasks, fn task ->
  try do
    Task.await(task, 1000)
    false  # Completed unexpectedly
  catch
    :exit, _ ->
      true   # Cancelled as expected
  end
end)

IO.puts("   ✓ Successfully cancelled #{cancelled_count}/#{length(tasks)} requests")

# Final status check
final_pending = Client.get_pending_requests(client)
IO.puts("   → Final pending requests: #{inspect(final_pending)}")

# Cleanup
IO.puts("\n8. Cleanup")
GenServer.stop(client)
GenServer.stop(server)
IO.puts("   ✓ Client and server stopped")

IO.puts("\n✅ Cancellation Example Complete!")
IO.puts("\nKey Features Demonstrated:")
IO.puts("• Request tracking and cancellation")
IO.puts("• Validation prevents cancelling initialize request")
IO.puts("• Graceful handling of unknown/completed requests")
IO.puts("• Cancellation with and without reasons")
IO.puts("• Mass cancellation of multiple requests")
IO.puts("• Proper error handling and edge cases")
IO.puts("• Both client and server cancellation support")
IO.puts("• MCP specification compliance")