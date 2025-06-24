#!/usr/bin/env elixir

# Simple benchmark script to measure schema compilation performance
Mix.install([
  {:ex_json_schema, "~> 0.7.4"},
  {:jason, "~> 1.4"}
])

defmodule SimpleBenchmark do
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
      "timestamp" => 1234567890,
      "operation" => "calculation"
    }
  }

  def benchmark_runtime_resolution(iterations \\ 1000) do
    {time, _} = :timer.tc(fn ->
      for _ <- 1..iterations do
        # This simulates the old approach: resolve schema on every call
        string_schema = atomize_keys_to_strings(@test_schema)
        resolved_schema = ExJsonSchema.Schema.resolve(string_schema)
        ExJsonSchema.Validator.validate(resolved_schema, @test_data)
      end
    end)
    
    time / 1000  # Convert to milliseconds
  end

  def benchmark_compile_time_approach(iterations \\ 1000) do
    # Pre-compile the schema once (compile-time simulation)
    string_schema = atomize_keys_to_strings(@test_schema)
    resolved_schema = ExJsonSchema.Schema.resolve(string_schema)
    
    {time, _} = :timer.tc(fn ->
      for _ <- 1..iterations do
        # This simulates the new approach: use pre-compiled schema
        ExJsonSchema.Validator.validate(resolved_schema, @test_data)
      end
    end)
    
    time / 1000  # Convert to milliseconds
  end

  def run_comparison do
    IO.puts("🚀 Schema Validation Performance Comparison")
    IO.puts("=" * 50)
    
    iterations = 1000
    
    IO.puts("Warming up...")
    benchmark_runtime_resolution(10)
    benchmark_compile_time_approach(10)
    
    IO.puts("Running benchmarks with #{iterations} iterations...")
    
    runtime_ms = benchmark_runtime_resolution(iterations)
    compile_time_ms = benchmark_compile_time_approach(iterations)
    
    improvement_factor = runtime_ms / compile_time_ms
    improvement_percent = ((runtime_ms - compile_time_ms) / runtime_ms) * 100
    
    IO.puts("")
    IO.puts("📊 Results:")
    IO.puts("  Runtime resolution:    #{Float.round(runtime_ms, 2)} ms")
    IO.puts("  Compile-time approach:  #{Float.round(compile_time_ms, 2)} ms")
    IO.puts("  🎯 Improvement:         #{Float.round(improvement_factor, 1)}x faster")
    IO.puts("  📈 Speed increase:      #{Float.round(improvement_percent, 1)}%")
    IO.puts("")
    
    if improvement_factor > 2.0 do
      IO.puts("✅ Significant performance improvement achieved!")
    else
      IO.puts("⚠️  Improvement less than expected (#{improvement_factor}x)")
    end
    
    {runtime_ms, compile_time_ms, improvement_factor}
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

# Run the benchmark
SimpleBenchmark.run_comparison()