defmodule ExMCP.Performance.Phase4DBenchmarksTest do
  @moduledoc """
  Phase 4D: Performance Benchmarks (Week 16)

  Comprehensive performance benchmarking suite for ExMCP implementing:
  - Transport-specific performance analysis
  - Protocol operation benchmarks  
  - Scalability and load testing
  - Memory and resource usage profiling
  - Performance regression detection
  - Baseline establishment for future releases
  """

  use ExUnit.Case, async: false

  alias ExMCP.Client
  alias ExMCP.Testing.PerformanceProfiler

  require Logger

  @moduletag :performance
  @moduletag :benchmarks
  # 10 minutes for comprehensive benchmarks
  @moduletag timeout: 600_000

  # Performance thresholds for different operation categories
  # Basic operations
  @fast_op_threshold_ms 10
  # Medium complexity operations
  @medium_op_threshold_ms 50
  # Resource-intensive operations
  @slow_op_threshold_ms 200
  # Minimum ops/sec for basic operations
  @throughput_threshold 100

  setup_all do
    # Create a comprehensive benchmark server
    defmodule BenchmarkServer do
      use ExMCP.Server

      # Fast operations for baseline measurement
      deftool "ping" do
        meta do
          description("Minimal latency test operation")
          input_schema(%{type: "object", properties: %{}})
        end
      end

      deftool "echo" do
        meta do
          description("Variable payload echo operation")

          input_schema(%{
            type: "object",
            properties: %{
              message: %{type: "string"},
              size: %{type: "integer", default: 100}
            },
            required: ["message"]
          })
        end
      end

      # CPU intensive operation
      deftool "fibonacci" do
        meta do
          description("CPU-intensive Fibonacci calculation")

          input_schema(%{
            type: "object",
            properties: %{
              n: %{type: "integer", default: 20}
            }
          })
        end
      end

      # Memory intensive operation
      deftool "allocate_memory" do
        meta do
          description("Memory allocation test")

          input_schema(%{
            type: "object",
            properties: %{
              size_kb: %{type: "integer", default: 1000}
            }
          })
        end
      end

      # Complex JSON processing
      deftool "json_processing" do
        meta do
          description("Complex JSON data processing")

          input_schema(%{
            type: "object",
            properties: %{
              depth: %{type: "integer", default: 5},
              width: %{type: "integer", default: 10}
            }
          })
        end
      end

      # Resource operations for different sizes
      defresource "benchmark://small" do
        meta do
          name("Small Resource")
          description("Small data resource (~1KB)")
        end
      end

      defresource "benchmark://medium" do
        meta do
          name("Medium Resource")
          description("Medium data resource (~100KB)")
        end
      end

      defresource "benchmark://large" do
        meta do
          name("Large Resource")
          description("Large data resource (~1MB)")
        end
      end

      # Complex prompt operations
      defprompt "simple_prompt" do
        meta do
          name("Simple Prompt")
          description("Basic prompt template")
        end
      end

      defprompt "complex_prompt" do
        meta do
          name("Complex Prompt")
          description("Complex prompt with multiple parameters")
        end
      end

      @impl true
      def handle_initialize(params, state) do
        {:ok,
         %{
           "protocolVersion" => params["protocolVersion"],
           "serverInfo" => %{
             "name" => "benchmark-server",
             "version" => "1.0.0"
           },
           "capabilities" => %{
             "tools" => %{},
             "resources" => %{},
             "prompts" => %{}
           }
         }, state}
      end

      @impl true
      def handle_tool_call("ping", _args, state) do
        {:ok, %{content: [%{"type" => "text", "text" => "pong"}]}, state}
      end

      @impl true
      def handle_tool_call("echo", %{"message" => msg} = args, state) do
        size = Map.get(args, "size", 100)

        padded_msg =
          if String.length(msg) < size do
            msg <> String.duplicate("x", size - String.length(msg))
          else
            msg
          end

        {:ok, %{content: [%{"type" => "text", "text" => padded_msg}]}, state}
      end

      @impl true
      def handle_tool_call("fibonacci", %{"n" => n}, state) do
        result = fibonacci(n)
        {:ok, %{content: [%{"type" => "text", "text" => "fib(#{n}) = #{result}"}]}, state}
      end

      @impl true
      def handle_tool_call("allocate_memory", %{"size_kb" => size_kb}, state) do
        # Allocate memory block
        size_bytes = size_kb * 1024
        _data = :binary.copy(<<0>>, size_bytes)
        {:ok, %{content: [%{"type" => "text", "text" => "Allocated #{size_kb}KB"}]}, state}
      end

      @impl true
      def handle_tool_call("json_processing", %{"depth" => depth, "width" => width}, state) do
        # Generate complex nested JSON structure
        data = generate_complex_json(depth, width)
        json_size = byte_size(Jason.encode!(data))

        {:ok, %{content: [%{"type" => "text", "text" => "Processed JSON: #{json_size} bytes"}]},
         state}
      end

      @impl true
      def handle_resource_read("benchmark://small", _full_uri, state) do
        # 1KB
        content = String.duplicate("x", 1024)

        {:ok,
         %{
           uri: "benchmark://small",
           mimeType: "text/plain",
           text: content
         }, state}
      end

      @impl true
      def handle_resource_read("benchmark://medium", _full_uri, state) do
        # ~100KB
        content = String.duplicate("x", 100_000)

        {:ok,
         %{
           uri: "benchmark://medium",
           mimeType: "text/plain",
           text: content
         }, state}
      end

      @impl true
      def handle_resource_read("benchmark://large", _full_uri, state) do
        # ~1MB
        content = String.duplicate("x", 1_000_000)

        {:ok,
         %{
           uri: "benchmark://large",
           mimeType: "text/plain",
           text: content
         }, state}
      end

      @impl true
      def handle_prompt_get("simple_prompt", _args, state) do
        {:ok,
         %{
           description: "Simple test prompt",
           messages: [
             %{
               role: "user",
               content: %{
                 type: "text",
                 text: "This is a simple prompt"
               }
             }
           ]
         }, state}
      end

      @impl true
      def handle_prompt_get("complex_prompt", args, state) do
        context = Map.get(args, "context", "general")
        complexity = Map.get(args, "complexity", "medium")

        messages = [
          %{
            role: "system",
            content: %{
              type: "text",
              text: "You are an expert assistant providing #{complexity} level analysis."
            }
          },
          %{
            role: "user",
            content: %{
              type: "text",
              text:
                "Generate a detailed response about #{context} with #{complexity} complexity level. Include examples, analysis, and recommendations."
            }
          }
        ]

        {:ok,
         %{
           description: "Complex prompt for #{context}",
           messages: messages
         }, state}
      end

      # Helper functions
      defp fibonacci(0), do: 0
      defp fibonacci(1), do: 1
      defp fibonacci(n) when n > 1, do: fibonacci(n - 1) + fibonacci(n - 2)

      defp generate_complex_json(0, _width), do: "leaf"

      defp generate_complex_json(depth, width) do
        Enum.reduce(1..width, %{}, fn i, acc ->
          Map.put(acc, "key_#{i}", generate_complex_json(depth - 1, width))
        end)
      end
    end

    %{server_module: BenchmarkServer}
  end

  describe "Phase 4D.1: Basic Operation Benchmarks" do
    test "establishes baseline performance for core MCP operations", %{
      server_module: server_module
    } do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Core MCP operation benchmarks
      operations = [
        {"ping", fn -> Client.call_tool(client, "ping", %{}) end},
        {"list_tools", fn -> Client.list_tools(client) end},
        {"list_resources", fn -> Client.list_resources(client) end},
        {"list_prompts", fn -> Client.list_prompts(client) end},
        {"simple_echo", fn -> Client.call_tool(client, "echo", %{"message" => "test"}) end}
      ]

      Logger.info("Running Phase 4D.1: Basic Operation Benchmarks...")
      results = PerformanceProfiler.profile_batch(operations, transport_type: :test)

      # Store baseline for regression detection
      PerformanceProfiler.store_baseline("phase_4d_basic", results)

      # Generate comprehensive report
      report =
        PerformanceProfiler.generate_report(results,
          format: :detailed,
          include_recommendations: true
        )

      Logger.info("Basic Operations Benchmark Report:\n#{report}")

      # Performance assertions
      assert results.summary.success_rate == 1.0, "All basic operations should succeed"

      assert results.summary.avg_time_ms < @fast_op_threshold_ms,
             "Average time should be under #{@fast_op_threshold_ms}ms"

      assert results.summary.throughput_ops_per_sec > @throughput_threshold,
             "Should achieve at least #{@throughput_threshold} ops/sec"

      # Individual operation thresholds
      Enum.each(results.operations, fn op ->
        case op.operation_name do
          "ping" ->
            assert op.execution_time_ms < 5, "Ping should be under 5ms"

          "list_tools" ->
            assert op.execution_time_ms < @fast_op_threshold_ms, "List tools should be fast"

          "list_resources" ->
            assert op.execution_time_ms < @fast_op_threshold_ms, "List resources should be fast"

          "list_prompts" ->
            assert op.execution_time_ms < @fast_op_threshold_ms, "List prompts should be fast"

          "simple_echo" ->
            assert op.execution_time_ms < @fast_op_threshold_ms, "Simple echo should be fast"
        end
      end)

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "Phase 4D.2: Payload Size Impact Analysis" do
    test "analyzes performance impact of different payload sizes", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Test different payload sizes
      payload_sizes = [
        {100, "tiny"},
        {1_000, "small"},
        {10_000, "medium"},
        {100_000, "large"},
        {1_000_000, "xlarge"}
      ]

      Logger.info("Running Phase 4D.2: Payload Size Impact Analysis...")

      payload_results =
        Enum.map(payload_sizes, fn {size, label} ->
          # Limit message size, use size parameter
          message = String.duplicate("x", min(size, 1000))

          {_result, metrics} =
            PerformanceProfiler.profile_operation(
              fn ->
                Client.call_tool(client, "echo", %{"message" => message, "size" => size})
              end,
              operation_name: "echo_#{label}",
              transport_type: :test,
              payload_size_bytes: size
            )

          metrics
        end)

      # Analyze scaling characteristics
      Logger.info("Payload Size Scaling Analysis:")

      Enum.each(payload_results, fn metrics ->
        Logger.info(
          "#{metrics.operation_name}: #{Float.round(metrics.execution_time_ms, 2)}ms (#{metrics.payload_size_bytes} bytes)"
        )
      end)

      # Performance assertions
      assert Enum.all?(payload_results, & &1.success), "All payload tests should succeed"

      # Check reasonable scaling
      times = Enum.map(payload_results, & &1.execution_time_ms)
      max_time = Enum.max(times)
      min_time = Enum.min(times)

      # Should not scale exponentially (max should be < 100x min)
      assert max_time < min_time * 100, "Performance should scale reasonably with payload size"

      # Large payloads should complete within reasonable time
      large_operations = Enum.filter(payload_results, &(&1.payload_size_bytes >= 100_000))

      Enum.each(large_operations, fn op ->
        assert op.execution_time_ms < @slow_op_threshold_ms,
               "Large payloads should complete within #{@slow_op_threshold_ms}ms"
      end)

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "Phase 4D.3: Resource Intensive Benchmarks" do
    test "measures performance of CPU and memory intensive operations", %{
      server_module: server_module
    } do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      Logger.info("Running Phase 4D.3: Resource Intensive Benchmarks...")

      # CPU intensive operations
      cpu_operations = [
        {"fibonacci_10", fn -> Client.call_tool(client, "fibonacci", %{"n" => 10}) end},
        {"fibonacci_20", fn -> Client.call_tool(client, "fibonacci", %{"n" => 20}) end},
        {"fibonacci_25", fn -> Client.call_tool(client, "fibonacci", %{"n" => 25}) end}
      ]

      cpu_results = PerformanceProfiler.profile_batch(cpu_operations, transport_type: :test)

      # Memory intensive operations
      memory_operations = [
        {"memory_1mb",
         fn -> Client.call_tool(client, "allocate_memory", %{"size_kb" => 1024}) end},
        {"memory_5mb",
         fn -> Client.call_tool(client, "allocate_memory", %{"size_kb" => 5120}) end},
        {"memory_10mb",
         fn -> Client.call_tool(client, "allocate_memory", %{"size_kb" => 10240}) end}
      ]

      memory_results = PerformanceProfiler.profile_batch(memory_operations, transport_type: :test)

      # JSON processing operations
      json_operations = [
        {"json_small",
         fn -> Client.call_tool(client, "json_processing", %{"depth" => 3, "width" => 5}) end},
        {"json_medium",
         fn -> Client.call_tool(client, "json_processing", %{"depth" => 5, "width" => 10}) end},
        {"json_large",
         fn -> Client.call_tool(client, "json_processing", %{"depth" => 7, "width" => 15}) end}
      ]

      json_results = PerformanceProfiler.profile_batch(json_operations, transport_type: :test)

      # Generate reports
      cpu_report = PerformanceProfiler.generate_report(cpu_results, format: :summary)
      memory_report = PerformanceProfiler.generate_report(memory_results, format: :summary)
      json_report = PerformanceProfiler.generate_report(json_results, format: :summary)

      Logger.info("CPU Intensive Operations:\n#{cpu_report}")
      Logger.info("Memory Intensive Operations:\n#{memory_report}")
      Logger.info("JSON Processing Operations:\n#{json_report}")

      # Performance assertions
      assert cpu_results.summary.success_rate == 1.0, "All CPU operations should succeed"
      assert memory_results.summary.success_rate == 1.0, "All memory operations should succeed"
      assert json_results.summary.success_rate == 1.0, "All JSON operations should succeed"

      # CPU operations should scale with complexity
      cpu_times = Enum.map(cpu_results.operations, & &1.execution_time_ms)
      assert Enum.at(cpu_times, 0) <= Enum.at(cpu_times, 1), "Fib(10) should be <= Fib(20)"
      assert Enum.at(cpu_times, 1) <= Enum.at(cpu_times, 2), "Fib(20) should be <= Fib(25)"

      # Memory operations should show measurable memory delta
      memory_deltas = Enum.map(memory_results.operations, & &1.memory_delta_mb)
      assert Enum.any?(memory_deltas, &(&1 > 0)), "Memory operations should increase memory usage"

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "Phase 4D.4: Resource Operations Benchmarks" do
    test "benchmarks resource read operations across different sizes", %{
      server_module: server_module
    } do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      Logger.info("Running Phase 4D.4: Resource Operations Benchmarks...")

      # Resource read operations
      resource_operations = [
        {"read_small", fn -> Client.read_resource(client, "benchmark://small") end},
        {"read_medium", fn -> Client.read_resource(client, "benchmark://medium") end},
        {"read_large", fn -> Client.read_resource(client, "benchmark://large") end}
      ]

      resource_results =
        PerformanceProfiler.profile_batch(resource_operations, transport_type: :test)

      # Prompt operations
      prompt_operations = [
        {"simple_prompt", fn -> Client.get_prompt(client, "simple_prompt", %{}) end},
        {"complex_prompt",
         fn ->
           Client.get_prompt(client, "complex_prompt", %{
             "context" => "testing",
             "complexity" => "high"
           })
         end}
      ]

      prompt_results = PerformanceProfiler.profile_batch(prompt_operations, transport_type: :test)

      # Generate reports
      resource_report = PerformanceProfiler.generate_report(resource_results, format: :detailed)
      prompt_report = PerformanceProfiler.generate_report(prompt_results, format: :summary)

      Logger.info("Resource Operations:\n#{resource_report}")
      Logger.info("Prompt Operations:\n#{prompt_report}")

      # Performance assertions
      assert resource_results.summary.success_rate == 1.0,
             "All resource operations should succeed"

      assert prompt_results.summary.success_rate == 1.0, "All prompt operations should succeed"

      # Resource operations should scale with size
      [small_time, medium_time, large_time] =
        Enum.map(resource_results.operations, & &1.execution_time_ms)

      # Allow reasonable scaling
      assert small_time <= medium_time * 2,
             "Small resource should not be much slower than expected"

      assert medium_time <= large_time * 2, "Medium resource should not be much slower than large"

      # Large resources should complete within reasonable time
      assert large_time < @slow_op_threshold_ms,
             "Large resource read should be under #{@slow_op_threshold_ms}ms"

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "Phase 4D.5: Concurrent Load Testing" do
    test "measures performance under concurrent client load", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      # Create multiple clients
      client_count = 10

      clients =
        Enum.map(1..client_count, fn i ->
          {:ok, client} =
            Client.start_link(
              transport: :test,
              server: server_pid
            )

          {i, client}
        end)

      Logger.info("Running Phase 4D.5: Concurrent Load Testing with #{client_count} clients...")

      # Define concurrent workload
      concurrent_workload = fn {_id, client} ->
        operations = [
          fn -> Client.call_tool(client, "ping", %{}) end,
          fn -> Client.list_tools(client) end,
          fn -> Client.call_tool(client, "echo", %{"message" => "concurrent_test"}) end,
          fn -> Client.read_resource(client, "benchmark://small") end
        ]

        # Each client performs multiple operations
        Enum.map(operations, fn op -> op.() end)
      end

      # Measure concurrent execution
      {_results, concurrent_metrics} =
        PerformanceProfiler.profile_operation(
          fn ->
            tasks =
              Enum.map(clients, fn client ->
                Task.async(fn -> concurrent_workload.(client) end)
              end)

            # Wait for all tasks to complete
            Enum.map(tasks, &Task.await(&1, 30_000))
          end,
          operation_name: "concurrent_load",
          transport_type: :test
        )

      Logger.info("Concurrent Load Test Results:")

      Logger.info(
        "- Total Execution Time: #{Float.round(concurrent_metrics.execution_time_ms, 2)}ms"
      )

      Logger.info("- Memory Delta: #{Float.round(concurrent_metrics.memory_delta_mb, 3)}MB")
      Logger.info("- GC Collections: #{concurrent_metrics.gc_collections}")
      Logger.info("- Success: #{concurrent_metrics.success}")

      # Performance assertions
      assert concurrent_metrics.success, "Concurrent operations should succeed"

      assert concurrent_metrics.execution_time_ms < 5000,
             "Concurrent operations should complete within 5 seconds"

      # Memory usage should be reasonable for concurrent operations
      assert concurrent_metrics.memory_delta_mb < 50,
             "Memory usage should be reasonable under load"

      # Cleanup clients
      Enum.each(clients, fn {_id, client} -> Client.stop(client) end)
      GenServer.stop(server_pid)
    end
  end

  describe "Phase 4D.6: Throughput Benchmarks" do
    test "measures maximum sustainable throughput", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      Logger.info("Running Phase 4D.6: Throughput Benchmarks...")

      # High-volume ping test
      operation_count = 1000

      operations =
        Enum.map(1..operation_count, fn i ->
          {"ping_#{i}", fn -> Client.call_tool(client, "ping", %{}) end}
        end)

      throughput_results = PerformanceProfiler.profile_batch(operations, transport_type: :test)

      # Generate throughput report
      report = PerformanceProfiler.generate_report(throughput_results, format: :summary)
      Logger.info("Throughput Benchmark Report:\n#{report}")

      # Key metrics logging
      Logger.info("Throughput Metrics:")
      Logger.info("- Operations: #{operation_count}")
      Logger.info("- Total Time: #{Float.round(throughput_results.summary.total_time_ms, 2)}ms")

      Logger.info(
        "- Throughput: #{Float.round(throughput_results.summary.throughput_ops_per_sec, 2)} ops/sec"
      )

      Logger.info(
        "- Average Latency: #{Float.round(throughput_results.summary.avg_time_ms, 2)}ms"
      )

      Logger.info(
        "- Min/Max Latency: #{Float.round(throughput_results.summary.min_time_ms, 2)}ms / #{Float.round(throughput_results.summary.max_time_ms, 2)}ms"
      )

      # Performance assertions
      assert throughput_results.summary.success_rate == 1.0,
             "All throughput operations should succeed"

      assert throughput_results.summary.throughput_ops_per_sec > @throughput_threshold,
             "Should achieve minimum throughput"

      assert throughput_results.summary.avg_time_ms < @fast_op_threshold_ms,
             "Average latency should be low"

      # Store throughput baseline
      PerformanceProfiler.store_baseline("phase_4d_throughput", throughput_results)

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "Phase 4D.7: Performance Regression Detection" do
    test "validates performance against established baselines", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      Logger.info("Running Phase 4D.7: Performance Regression Detection...")

      # Run current performance tests
      current_operations = [
        {"ping", fn -> Client.call_tool(client, "ping", %{}) end},
        {"echo", fn -> Client.call_tool(client, "echo", %{"message" => "regression_test"}) end}
      ]

      current_results =
        PerformanceProfiler.profile_batch(current_operations, transport_type: :test)

      # Check for regressions against basic baseline (if exists)
      case PerformanceProfiler.check_regression("phase_4d_basic", current_results) do
        %{error: error} ->
          Logger.info("No baseline found for regression detection: #{error}")

        regression_report ->
          Logger.info("Regression Analysis:")
          Logger.info("- Baseline Avg Time: #{regression_report.baseline_avg_time_ms}ms")
          Logger.info("- Current Avg Time: #{regression_report.current_avg_time_ms}ms")
          Logger.info("- Change: #{Float.round(regression_report.avg_time_change_percent, 1)}%")
          Logger.info("- Has Regression: #{regression_report.has_regression}")

          if regression_report.has_regression do
            Logger.warning("Performance Regressions Detected:")
            Enum.each(regression_report.regressions, &Logger.warning("- #{&1}"))
          end

          # Assert no significant regressions (allow 50% variance for test environment)
          assert regression_report.avg_time_change_percent < 50,
                 "Performance regression detected: #{regression_report.avg_time_change_percent}% slower"
      end

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end
end
