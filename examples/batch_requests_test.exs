#!/usr/bin/env elixir

# Example: Using batch requests with test transport for demonstration

Mix.install([
  {:ex_mcp, path: "."}
])

defmodule BatchExample do
  def run do
    # For demonstration, we'll use the Native Service Dispatcher
    # which provides the best performance for batch operations
    
    # Start a service
    {:ok, _} = EchoService.start_link()
    
    # Wait for service registration
    Process.sleep(100)

    # Use Native calls directly - this is much faster than MCP protocol
    IO.puts("Native Service Dispatcher Performance Demo")
    IO.puts("=" |> String.duplicate(50))
    
    IO.puts("\nSending individual Native calls...")
    start_time = System.monotonic_time(:microsecond)

    # Individual Native calls
    {:ok, tools} = ExMCP.Native.call(:echo_service, "list_tools", %{})
    {:ok, resources} = ExMCP.Native.call(:echo_service, "list_resources", %{})
    {:ok, echo_result} = ExMCP.Native.call(:echo_service, "tools/call", %{
      "name" => "echo",
      "arguments" => %{"message" => "Hello"}
    })

    individual_time = System.monotonic_time(:microsecond) - start_time
    IO.puts("Individual calls took: #{individual_time}μs")
    IO.puts("Found #{length(tools["tools"])} tools")

    IO.puts("\nFor batch operations with Native Service Dispatcher,")
    IO.puts("you can use Task.async_stream or similar patterns:")
    
    start_time = System.monotonic_time(:microsecond)
    
    # Parallel batch using Task.async
    tasks = [
      Task.async(fn -> ExMCP.Native.call(:echo_service, "list_tools", %{}) end),
      Task.async(fn -> ExMCP.Native.call(:echo_service, "list_resources", %{}) end),
      Task.async(fn -> 
        ExMCP.Native.call(:echo_service, "tools/call", %{
          "name" => "echo",
          "arguments" => %{"message" => "Hello"}
        })
      end)
    ]
    
    results = Task.await_many(tasks)
    batch_time = System.monotonic_time(:microsecond) - start_time
    
    IO.puts("Parallel batch took: #{batch_time}μs")
    
    IO.puts("\nComparison:")
    IO.puts("Sequential: #{individual_time}μs")
    IO.puts("Parallel: #{batch_time}μs")
    if batch_time > 0 do
      IO.puts("Speedup: #{Float.round(individual_time / batch_time, 2)}x")
    end

    IO.puts("\nNote: Native Service Dispatcher calls are ~15μs each locally,")
    IO.puts("compared to 1-5ms for traditional MCP over stdio/HTTP.")

    :ok
  end
end

defmodule EchoService do
  use ExMCP.Service, name: :echo_service

  @impl true
  def init(_args) do
    {:ok, %{call_count: 0}}
  end

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "echo",
        "description" => "Echoes back the message",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "message" => %{"type" => "string"}
          },
          "required" => ["message"]
        }
      },
      %{
        "name" => "reverse",
        "description" => "Reverses the message",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "message" => %{"type" => "string"}
          },
          "required" => ["message"]
        }
      }
    ]
    {:ok, %{"tools" => tools}, %{state | call_count: state.call_count + 1}}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "echo", "arguments" => %{"message" => msg}}, state) do
    {:ok, %{"content" => [%{"type" => "text", "text" => msg}]}, %{state | call_count: state.call_count + 1}}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "reverse", "arguments" => %{"message" => msg}}, state) do
    reversed = String.reverse(msg)
    {:ok, %{"content" => [%{"type" => "text", "text" => reversed}]}, %{state | call_count: state.call_count + 1}}
  end

  @impl true
  def handle_mcp_request("list_resources", _params, state) do
    {:ok, %{"resources" => []}, %{state | call_count: state.call_count + 1}}
  end

  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

# Run the example
BatchExample.run()