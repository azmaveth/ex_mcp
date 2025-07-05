defmodule ExMCP.Performance.SchemaCompilationPerformanceTest do
  @moduledoc """
  Performance test to verify the compile-time schema caching improvement.
  """
  use ExUnit.Case

  alias ExMCP.Server.Tools

  @moduletag :performance

  defp log_performance(message) do
    if System.get_env("VERBOSE_PERFORMANCE_TESTS") do
      IO.puts(message)
    end
  end

  @test_schema %{
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

  @test_data %{
    "result" => 42,
    "expression" => "2+2",
    "metadata" => %{
      "timestamp" => 1_234_567_890,
      "operation" => "calculation"
    }
  }

  describe "compile-time schema caching performance" do
    test "compile_schema/1 succeeds and produces valid schema" do
      compiled_schema = Tools.compile_schema(@test_schema)

      assert compiled_schema != nil
      assert is_struct(compiled_schema, ExJsonSchema.Schema.Root)

      # Verify the compiled schema can be used for validation
      result = Tools.validate_with_schema(@test_data, compiled_schema)
      assert result == :ok
    end

    test "validate_with_schema performance with pre-compiled schema" do
      # Pre-compile the schema (this happens at compile time in real usage)
      compiled_schema = Tools.compile_schema(@test_schema)

      # Measure validation performance (this happens at runtime)
      {time_microseconds, results} =
        :timer.tc(fn ->
          for _ <- 1..100 do
            Tools.validate_with_schema(@test_data, compiled_schema)
          end
        end)

      # Verify all validations succeeded
      assert Enum.all?(results, &(&1 == :ok))

      time_ms = time_microseconds / 1000
      avg_per_validation = time_ms / 100

      log_performance("\n📊 Validation Performance (100 iterations):")
      log_performance("  Total time: #{Float.round(time_ms, 2)} ms")
      log_performance("  Average per validation: #{Float.round(avg_per_validation, 4)} ms")
      log_performance("  Validations per second: #{Float.round(1000 / avg_per_validation, 0)}")

      # Assert reasonable performance (should be very fast with pre-compiled schemas)
      assert avg_per_validation < 0.1,
             "Expected sub-0.1ms per validation, got #{avg_per_validation} ms"
    end

    test "schema compilation is one-time cost" do
      # Measure compilation time (happens once at compile time)
      {compilation_time, compiled_schema} =
        :timer.tc(fn ->
          Tools.compile_schema(@test_schema)
        end)

      # Measure validation time (happens many times at runtime)
      {validation_time, _} =
        :timer.tc(fn ->
          Tools.validate_with_schema(@test_data, compiled_schema)
        end)

      compilation_ms = compilation_time / 1000
      validation_ms = validation_time / 1000

      # Calculate break-even point
      break_even_calls =
        if validation_ms > 0 do
          Float.round(compilation_ms / validation_ms, 0)
        else
          1
        end

      log_performance("\n⚡ Compilation vs Validation Cost:")
      log_performance("  Schema compilation: #{Float.round(compilation_ms, 3)} ms (one-time)")
      log_performance("  Single validation: #{Float.round(validation_ms, 4)} ms (per call)")
      log_performance("  Break-even point: #{break_even_calls} tool calls")

      log_performance(
        "\n💡 In production, tools are typically called hundreds or thousands of times!"
      )

      log_performance("   The compilation cost is amortized very quickly.")

      assert compiled_schema != nil
      # Compilation should be reasonably fast
      assert compilation_ms < 50, "Schema compilation should be fast (< 50ms)"
    end
  end

  describe "integration with tools DSL" do
    defmodule TestPerformanceServer do
      use ExMCP.Server.Handler
      use ExMCP.Server.Tools

      tool "perf_test" do
        description("Performance test tool")

        output_schema(%{
          type: "object",
          properties: %{
            result: %{type: "number"},
            timestamp: %{type: "integer"}
          },
          required: ["result"]
        })

        handle(fn _args, state ->
          {:ok,
           %{
             content: [%{type: "text", text: "Performance test"}],
             structuredOutput: %{
               result: 42,
               timestamp: System.system_time(:second)
             }
           }, state}
        end)
      end

      @impl true
      def handle_initialize(_params, state) do
        {:ok,
         %{
           "protocolVersion" => "2025-06-18",
           "serverInfo" => %{"name" => "Test Performance Server", "version" => "1.0.0"},
           "capabilities" => %{"tools" => %{}}
         }, state}
      end
    end

    test "end-to-end tool call performance with schema validation" do
      state = %{}
      params = %{name: "perf_test", arguments: %{}}

      # Warm up
      for _ <- 1..5 do
        TestPerformanceServer.handle_call_tool(params, state)
      end

      # Measure tool call performance
      {time_microseconds, results} =
        :timer.tc(fn ->
          for _ <- 1..100 do
            TestPerformanceServer.handle_call_tool(params, state)
          end
        end)

      # Verify all calls succeeded with valid structured output
      assert length(results) == 100

      assert Enum.all?(results, fn
               {:ok, response, ^state} ->
                 Map.has_key?(response, :structuredOutput) and
                   not Map.has_key?(response, :isError)

               _ ->
                 false
             end)

      time_ms = time_microseconds / 1000
      avg_per_call = time_ms / 100

      log_performance("\n🔧 End-to-End Tool Call Performance (100 calls):")
      log_performance("  Total time: #{Float.round(time_ms, 2)} ms")
      log_performance("  Average per call: #{Float.round(avg_per_call, 3)} ms")
      log_performance("  Calls per second: #{Float.round(1000 / avg_per_call, 0)}")

      # Assert reasonable performance
      assert avg_per_call < 1.0, "Expected sub-millisecond per call, got #{avg_per_call} ms"

      log_performance("\n✅ Performance test completed successfully!")
    end
  end
end
