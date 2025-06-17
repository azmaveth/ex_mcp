#!/usr/bin/env elixir

# Example: Using Native Service Dispatcher for efficient multi-operation calls
# Demonstrates the performance benefits of ExMCP.Native over traditional MCP

Mix.install([
  {:ex_mcp, path: "."}
])

defmodule EchoService do
  @moduledoc """
  Echo service using ExMCP.Service macro for automatic registration.
  """
  use ExMCP.Service, name: :echo_service

  @impl true
  def init(_args) do
    {:ok, %{call_count: 0}}
  end

  @impl true
  def handle_mcp_request("ping", _params, state) do
    {:ok, %{}, %{state | call_count: state.call_count + 1}}
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
      }
    ]
    {:ok, %{"tools" => tools}, %{state | call_count: state.call_count + 1}}
  end

  @impl true
  def handle_mcp_request("list_resources", _params, state) do
    {:ok, %{"resources" => []}, %{state | call_count: state.call_count + 1}}
  end

  @impl true
  def handle_mcp_request("list_roots", _params, state) do
    {:ok, %{"roots" => []}, %{state | call_count: state.call_count + 1}}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "echo", "arguments" => %{"message" => msg}}, state) do
    content = [%{"type" => "text", "text" => msg}]
    {:ok, %{"content" => content}, %{state | call_count: state.call_count + 1}}
  end

  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

defmodule BatchExample do
  def run do
    # Start the echo service (automatically registers with ExMCP.Native)
    {:ok, _service} = EchoService.start_link()
    
    # Wait for service registration
    Process.sleep(100)

    IO.puts("Native Service Dispatcher Performance Demo")
    IO.puts("=" |> String.duplicate(50))
    
    IO.puts("\nSending individual Native calls...")
    start_time = System.monotonic_time(:microsecond)

    # Individual Native calls (much faster than MCP protocol)
    {:ok, tools_result} = ExMCP.Native.call(:echo_service, "list_tools", %{})
    {:ok, resources_result} = ExMCP.Native.call(:echo_service, "list_resources", %{})
    {:ok, roots_result} = ExMCP.Native.call(:echo_service, "list_roots", %{})
    {:ok, echo_result} = ExMCP.Native.call(:echo_service, "tools/call", %{
      "name" => "echo",
      "arguments" => %{"message" => "Hello"}
    })

    individual_time = System.monotonic_time(:microsecond) - start_time
    IO.puts("Individual calls took: #{individual_time}μs")
    IO.puts("Found #{length(tools_result["tools"])} tools")

    IO.puts("\nSending parallel batch with Task.async_stream...")
    start_time = System.monotonic_time(:microsecond)

    # Parallel Native calls using Task.async_stream
    operations = [
      {"list_tools", %{}},
      {"list_resources", %{}},
      {"list_roots", %{}},
      {"tools/call", %{"name" => "echo", "arguments" => %{"message" => "Hello"}}}
    ]

    batch_results = 
      operations
      |> Task.async_stream(fn {method, params} ->
        ExMCP.Native.call(:echo_service, method, params)
      end, max_concurrency: 4)
      |> Enum.map(fn {:ok, result} -> result end)

    batch_time = System.monotonic_time(:microsecond) - start_time
    IO.puts("Parallel batch took: #{batch_time}μs")

    # Extract results for comparison
    [{:ok, batch_tools}, {:ok, batch_resources}, {:ok, batch_roots}, {:ok, batch_echo}] = batch_results
    
    # Verify results are the same
    IO.puts("\nVerifying results match:")
    IO.puts("Tools match: #{tools_result == batch_tools}")
    IO.puts("Resources match: #{resources_result == batch_resources}")
    IO.puts("Roots match: #{roots_result == batch_roots}")
    IO.puts("Echo match: #{echo_result == batch_echo}")

    IO.puts("\nComparison:")
    IO.puts("Sequential: #{individual_time}μs")
    IO.puts("Parallel: #{batch_time}μs")
    if batch_time > 0 do
      IO.puts("Speedup: #{Float.round(individual_time / batch_time, 2)}x")
    end

    # Complex parallel operations demonstration
    IO.puts("\nComplex parallel operations:")
    complex_operations = [
      {"ping", %{}},
      {"list_tools", %{}},
      {"tools/call", %{"name" => "echo", "arguments" => %{"message" => "First"}}},
      {"tools/call", %{"name" => "echo", "arguments" => %{"message" => "Second"}}},
      {"tools/call", %{"name" => "echo", "arguments" => %{"message" => "Third"}}}
    ]

    start_time = System.monotonic_time(:microsecond)
    
    complex_results = 
      complex_operations
      |> Task.async_stream(fn {method, params} ->
        ExMCP.Native.call(:echo_service, method, params)
      end, max_concurrency: 8)
      |> Enum.map(fn {:ok, result} -> result end)
    
    complex_time = System.monotonic_time(:microsecond) - start_time
    
    IO.puts("Complex parallel batch (#{length(complex_operations)} operations) took: #{complex_time}μs")
    Enum.each(Enum.with_index(complex_results), fn {{:ok, result}, idx} ->
      content = case result do
        %{"content" => [%{"text" => text}]} -> text
        %{"tools" => tools} -> "#{length(tools)} tools"
        %{} -> "ping response"
        _ -> "other"
      end
      IO.puts("  Result #{idx + 1}: #{content}")
    end)
    
    IO.puts("\nNote: Native Service Dispatcher provides ~15μs local calls")
    IO.puts("vs 1-5ms for traditional MCP over stdio/HTTP transports.")

    # Cleanup (service will be automatically unregistered when process stops)
    :ok
  end
end

BatchExample.run()