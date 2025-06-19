# Performance Benchmarks for ExMCP v2
#
# Run with: mix run benchmarks/v2_performance.exs
#
# This benchmark suite measures the performance of various ExMCP v2 operations
# to ensure they meet production requirements.

Mix.install([
  {:benchee, "~> 1.0"},
  {:jason, "~> 1.4"}
])

defmodule ExMCP.Benchmarks.V2Performance do
  @moduledoc """
  Performance benchmarks for ExMCP v2 implementation.
  
  Measures:
  - Message encoding/decoding performance
  - Client request/response latency
  - DSL compilation time
  - Structured response creation
  - Error handling overhead
  - SSE message processing
  """
  
  def run do
    IO.puts("\n=== ExMCP v2 Performance Benchmarks ===\n")
    
    benchmark_message_processing()
    benchmark_structured_responses()
    benchmark_dsl_compilation()
    benchmark_error_handling()
    benchmark_client_operations()
    benchmark_sse_processing()
  end
  
  defp benchmark_message_processing do
    IO.puts("\n## Message Processing Benchmarks\n")
    
    # Sample messages
    small_request = %{
      "jsonrpc" => "2.0",
      "method" => "tools/call",
      "params" => %{"name" => "test", "arguments" => %{"value" => "hello"}},
      "id" => "1"
    }
    
    large_request = %{
      "jsonrpc" => "2.0",
      "method" => "tools/call",
      "params" => %{
        "name" => "complex_tool",
        "arguments" => %{
          "data" => List.duplicate("x", 1000) |> Enum.join(),
          "options" => Enum.into(1..100, %{}, fn i -> {"key_#{i}", "value_#{i}"} end)
        }
      },
      "id" => "2"
    }
    
    Benchee.run(%{
      "encode small message" => fn ->
        Jason.encode!(small_request)
      end,
      "encode large message" => fn ->
        Jason.encode!(large_request)
      end,
      "decode small message" => fn ->
        json = Jason.encode!(small_request)
        Jason.decode!(json)
      end,
      "decode large message" => fn ->
        json = Jason.encode!(large_request)
        Jason.decode!(json)
      end,
      "validate small request" => fn ->
        # Simulate validation
        Map.has_key?(small_request, "jsonrpc") and
        Map.has_key?(small_request, "method") and
        Map.has_key?(small_request, "id")
      end
    }, time: 5, warmup: 2)
  end
  
  defp benchmark_structured_responses do
    IO.puts("\n## Structured Response Benchmarks\n")
    
    # Pre-create some data
    text_data = "Hello, this is a response from the tool execution."
    json_data = %{result: 42, status: "success", items: Enum.to_list(1..10)}
    error_message = "Tool execution failed due to invalid parameters"
    
    Benchee.run(%{
      "create text response" => fn ->
        ExMCP.Response.text(text_data, "test_tool")
      end,
      "create json response" => fn ->
        ExMCP.Response.json(json_data, "test_tool")
      end,
      "create error response" => fn ->
        ExMCP.Response.error(error_message, "test_tool")
      end,
      "create error struct" => fn ->
        ExMCP.Error.tool_error(error_message, "test_tool")
      end,
      "error to json_rpc" => fn ->
        error = ExMCP.Error.tool_error(error_message, "test_tool")
        ExMCP.Error.to_json_rpc(error)
      end,
      "extract text content" => fn ->
        response = ExMCP.Response.text(text_data, "test_tool")
        ExMCP.Response.text_content(response)
      end
    }, time: 5, warmup: 2)
  end
  
  defp benchmark_dsl_compilation do
    IO.puts("\n## DSL Compilation Benchmarks\n")
    
    # Note: These benchmarks measure runtime overhead, not compile-time
    # since we can't easily benchmark macro expansion
    
    Benchee.run(%{
      "access tool metadata" => fn ->
        # Simulate accessing compiled tool metadata
        tools = %{
          "test_tool" => %{
            name: "test_tool",
            description: "A test tool",
            input_schema: %{
              "type" => "object",
              "properties" => %{
                "value" => %{"type" => "string"}
              }
            }
          }
        }
        Map.get(tools, "test_tool")
      end,
      "validate tool args" => fn ->
        schema = %{
          "type" => "object",
          "properties" => %{
            "value" => %{"type" => "string"},
            "count" => %{"type" => "integer"}
          },
          "required" => ["value"]
        }
        args = %{"value" => "test", "count" => 5}
        
        # Simple validation
        required = Map.get(schema, "required", [])
        Enum.all?(required, &Map.has_key?(args, &1))
      end
    }, time: 5, warmup: 2)
  end
  
  defp benchmark_error_handling do
    IO.puts("\n## Error Handling Benchmarks\n")
    
    Benchee.run(%{
      "create parse error" => fn ->
        ExMCP.Error.parse_error("Invalid JSON")
      end,
      "create method not found" => fn ->
        ExMCP.Error.method_not_found("unknown/method")
      end,
      "create tool error with data" => fn ->
        ExMCP.Error.tool_error("Execution failed", "calculator", 
          data: %{input: "2+2", error: "Parser error"})
      end,
      "error categorization" => fn ->
        error = ExMCP.Error.tool_error("Failed", "test")
        ExMCP.Error.category(error)
      end,
      "check error type" => fn ->
        error = ExMCP.Error.internal_error("Server error")
        ExMCP.Error.json_rpc_error?(error)
      end
    }, time: 5, warmup: 2)
  end
  
  defp benchmark_client_operations do
    IO.puts("\n## Client Operation Benchmarks\n")
    
    # Simulate client operations without actual network calls
    mock_response = %{
      "jsonrpc" => "2.0",
      "result" => %{
        "content" => [%{"type" => "text", "text" => "Result"}]
      },
      "id" => "123"
    }
    
    Benchee.run(%{
      "parse client response" => fn ->
        ExMCP.Response.from_raw_response(mock_response["result"])
      end,
      "build client request" => fn ->
        %{
          "jsonrpc" => "2.0",
          "method" => "tools/call",
          "params" => %{
            "name" => "test",
            "arguments" => %{"value" => "hello"}
          },
          "id" => System.unique_integer([:positive]) |> Integer.to_string()
        }
      end,
      "generate request id" => fn ->
        System.unique_integer([:positive]) |> Integer.to_string()
      end
    }, time: 5, warmup: 2)
  end
  
  defp benchmark_sse_processing do
    IO.puts("\n## SSE Processing Benchmarks\n")
    
    # SSE event data
    event_data = %{
      type: "tool_update",
      tool: "calculator",
      status: "completed",
      result: %{value: 42}
    }
    
    Benchee.run(%{
      "format SSE event" => fn ->
        json = Jason.encode!(event_data)
        "event: tool_update\ndata: #{json}\nid: #{System.unique_integer()}\n\n"
      end,
      "generate event ID" => fn ->
        "#{System.system_time(:microsecond)}-#{System.unique_integer([:positive])}"
      end,
      "check backpressure" => fn ->
        # Simulate mailbox check
        {:message_queue_len, _len} = Process.info(self(), :message_queue_len)
      end
    }, time: 5, warmup: 2)
  end
end

# Run the benchmarks
ExMCP.Benchmarks.V2Performance.run()

# Performance targets and analysis
IO.puts("\n=== Performance Analysis ===\n")
IO.puts("""
Target performance metrics:
- Message encoding/decoding: < 100μs for small messages, < 1ms for large
- Response creation: < 10μs
- Error handling: < 5μs
- SSE event formatting: < 50μs

Note: These benchmarks measure the core operations without network I/O.
Actual end-to-end latency will depend on transport and network conditions.
""")