defmodule StructuredOutputBenchmark do
  @moduledoc """
  Benchmarks for structured output validation performance.
  
  This benchmarks the performance improvement from compile-time schema caching
  versus the old runtime schema resolution approach.
  """

  use ExUnit.Case
  import ExUnit.CaptureLog

  # Mock the old runtime approach for comparison
  defmodule OldRuntimeApproach do
    @doc """
    Simulates the old approach where schema resolution happened at runtime
    """
    def validate_with_runtime_resolution(data, raw_schema) do
      # This mimics what used to happen on every tool call
      if Code.ensure_loaded?(ExJsonSchema) and raw_schema do
        try do
          # Convert atom keys to strings (expensive)
          string_schema = atomize_keys_to_strings(raw_schema)
          # Resolve schema (very expensive - this is what we optimized)
          resolved_schema = ExJsonSchema.Schema.resolve(string_schema)
          # Validate (normal cost)
          case ExJsonSchema.Validator.validate(resolved_schema, data) do
            :ok -> :ok
            {:error, errors} -> {:error, errors}
          end
        rescue
          e -> {:error, ["Schema validation error: #{inspect(e)}"]}
        end
      else
        :ok
      end
    end

    defp atomize_keys_to_strings(map) when is_map(map) do
      Map.new(map, fn
        {k, v} when is_atom(k) -> {Atom.to_string(k), atomize_keys_to_strings(v)}
        {k, v} -> {k, atomize_keys_to_strings(v)}
      end)
    end

    defp atomize_keys_to_strings(list) when is_list(list) do
      Enum.map(list, &atomize_keys_to_strings/1)
    end

    defp atomize_keys_to_strings(value), do: value
  end

  # Test server using the new optimized approach
  defmodule BenchmarkServer do
    use ExMCP.Server.Handler
    use ExMCP.Server.Tools

    tool "calc_benchmark" do
      description("Calculation tool for benchmarking")

      input_schema(%{
        type: "object",
        properties: %{
          expression: %{type: "string"}
        },
        required: ["expression"]
      })

      output_schema(%{
        type: "object",
        properties: %{
          result: %{type: "number"},
          expression: %{type: "string"},
          metadata: %{
            type: "object",
            properties: %{
              timestamp: %{type: "integer"},
              operation: %{type: "string"}
            }
          }
        },
        required: ["result", "expression"]
      })

      handle(fn %{expression: expr}, state ->
        result = case expr do
          "2+2" -> 4
          "10*5" -> 50
          "100/2" -> 50
          _ -> 42
        end

        {:ok,
         %{
           content: [%{type: "text", text: "Result: #{result}"}],
           structuredOutput: %{
             result: result,
             expression: expr,
             metadata: %{
               timestamp: System.system_time(:second),
               operation: "calculation"
             }
           }
         }, state}
      end)
    end
  end

  @complex_schema %{
    type: "object",
    properties: %{
      result: %{type: "number"},
      expression: %{type: "string"},
      metadata: %{
        type: "object",
        properties: %{
          timestamp: %{type: "integer"},
          operation: %{type: "string"}
        }
      }
    },
    required: ["result", "expression"]
  }

  @valid_data %{
    "result" => 42,
    "expression" => "2+2",
    "metadata" => %{
      "timestamp" => 1234567890,
      "operation" => "calculation"
    }
  }

  setup do
    # Ensure ExJsonSchema is available for benchmarking
    unless Code.ensure_loaded?(ExJsonSchema) do
      ExUnit.configure(exclude: [benchmark: true])
      :skip
    else
      :ok
    end
  end

  @tag benchmark: true
  test "benchmark: runtime vs compile-time schema resolution" do
    capture_log(fn ->
      IO.puts("\n🚀 Structured Output Performance Benchmark")
      IO.puts("=" * 50)

      # Benchmark the old runtime approach
      {runtime_time, _} = :timer.tc(fn ->
        for _ <- 1..1000 do
          OldRuntimeApproach.validate_with_runtime_resolution(@valid_data, @complex_schema)
        end
      end)

      # Benchmark the new compile-time approach
      # First get the pre-compiled schema
      compiled_schema = ExMCP.Server.Tools.compile_schema(@complex_schema)

      {compile_time_time, _} = :timer.tc(fn ->
        for _ <- 1..1000 do
          ExMCP.Server.Tools.validate_with_schema(@valid_data, compiled_schema)
        end
      end)

      runtime_ms = runtime_time / 1000
      compile_time_ms = compile_time_time / 1000

      improvement_factor = runtime_ms / compile_time_ms
      improvement_percent = ((runtime_ms - compile_time_ms) / runtime_ms) * 100

      IO.puts("📊 Results (1000 iterations):")
      IO.puts("  Runtime approach:     #{Float.round(runtime_ms, 2)} ms")
      IO.puts("  Compile-time approach: #{Float.round(compile_time_ms, 2)} ms")
      IO.puts("  🎯 Improvement:        #{Float.round(improvement_factor, 1)}x faster")
      IO.puts("  📈 Speed increase:     #{Float.round(improvement_percent, 1)}%")

      # Verify the improvement is significant
      assert improvement_factor > 2.0, "Expected at least 2x improvement, got #{improvement_factor}x"

      IO.puts("\n✅ Performance benchmark completed successfully!")
    end)
  end

  @tag benchmark: true  
  test "benchmark: end-to-end tool call performance" do
    capture_log(fn ->
      IO.puts("\n🔧 End-to-End Tool Call Benchmark")
      IO.puts("=" * 40)

      state = %{}
      params = %{name: "calc_benchmark", arguments: %{expression: "2+2"}}

      # Warm up
      for _ <- 1..10 do
        BenchmarkServer.handle_call_tool(params, state)
      end

      # Benchmark tool calls with validation
      {tool_call_time, results} = :timer.tc(fn ->
        for _ <- 1..1000 do
          BenchmarkServer.handle_call_tool(params, state)
        end
      end)

      # Verify all calls succeeded
      assert length(results) == 1000
      assert Enum.all?(results, fn
        {:ok, response, ^state} -> 
          Map.has_key?(response, :structuredOutput) and not Map.has_key?(response, :isError)
        _ -> false
      end)

      tool_call_ms = tool_call_time / 1000
      avg_per_call = tool_call_ms / 1000

      IO.puts("📊 End-to-End Results (1000 tool calls):")
      IO.puts("  Total time:        #{Float.round(tool_call_ms, 2)} ms")
      IO.puts("  Average per call:  #{Float.round(avg_per_call, 3)} ms")
      IO.puts("  Calls per second:  #{Float.round(1000 / avg_per_call, 0)}")

      # Verify reasonable performance (should be sub-millisecond per call)
      assert avg_per_call < 1.0, "Expected sub-millisecond per call, got #{avg_per_call} ms"

      IO.puts("\n✅ End-to-end benchmark completed successfully!")
    end)
  end

  @tag benchmark: true
  test "benchmark: schema compilation overhead" do
    capture_log(fn ->
      IO.puts("\n⚡ Schema Compilation Overhead Analysis")
      IO.puts("=" * 45)

      # Benchmark schema compilation (happens once at compile time)
      {compilation_time, compiled_schema} = :timer.tc(fn ->
        ExMCP.Server.Tools.compile_schema(@complex_schema)
      end)

      # Benchmark raw validation (happens on every call)
      {validation_time, _} = :timer.tc(fn ->
        for _ <- 1..100 do
          ExMCP.Server.Tools.validate_with_schema(@valid_data, compiled_schema)
        end
      end)

      compilation_ms = compilation_time / 1000
      validation_ms = validation_time / 1000
      avg_validation = validation_ms / 100

      IO.puts("📊 Compilation Analysis:")
      IO.puts("  Schema compilation:   #{Float.round(compilation_ms, 3)} ms (one-time)")
      IO.puts("  100 validations:      #{Float.round(validation_ms, 3)} ms")
      IO.puts("  Avg per validation:   #{Float.round(avg_validation, 4)} ms")
      
      breakeven_calls = Float.round(compilation_ms / avg_validation, 0)
      IO.puts("  💡 Break-even point:   #{breakeven_calls} tool calls")

      IO.puts("\n🎯 Key Insight: Compilation cost is amortized after #{breakeven_calls} calls")
      IO.puts("   In production, tools are called hundreds or thousands of times!")

      assert compiled_schema != nil, "Schema compilation should succeed"
      assert compilation_ms < 100, "Schema compilation should be fast (< 100ms)"

      IO.puts("\n✅ Compilation analysis completed successfully!")
    end)
  end
end