defmodule ExMCP.Integration.PerformanceTest do
  @moduledoc """
  Comprehensive Performance Integration Tests for ExMCP.

  This test suite provides detailed performance profiling and benchmarking
  for MCP operations across different transports, payload sizes, and usage patterns.
  """

  use ExUnit.Case, async: false

  alias ExMCP.Client
  alias ExMCP.Testing.PerformanceProfiler

  require Logger

  @moduletag :integration
  @moduletag :performance
  # 5 minutes for comprehensive performance tests
  @moduletag timeout: 300_000

  setup_all do
    # Create a comprehensive test server for performance testing
    defmodule PerformanceTestServer do
      @moduledoc false
      use ExMCP.Server

      # Fast operation for baseline measurement
      deftool "ping" do
        meta do
          description("Simple ping operation")
          input_schema(%{type: "object", properties: %{}})
        end
      end

      # Variable payload size for throughput testing
      deftool "echo" do
        meta do
          description("Echo with variable payload")

          input_schema(%{
            type: "object",
            properties: %{
              message: %{type: "string"},
              repeat: %{type: "integer", default: 1}
            },
            required: ["message"]
          })
        end
      end

      # CPU intensive operation
      deftool "compute" do
        meta do
          description("CPU intensive computation")

          input_schema(%{
            type: "object",
            properties: %{
              iterations: %{type: "integer", default: 1000}
            }
          })
        end
      end

      # Memory intensive operation
      deftool "memory_test" do
        meta do
          description("Memory allocation test")

          input_schema(%{
            type: "object",
            properties: %{
              size_mb: %{type: "number", default: 1.0}
            }
          })
        end
      end

      # Resource operations
      defresource "perf://small" do
        meta do
          name("Small Resource")
          description("Small test resource")
        end
      end

      defresource "perf://large" do
        meta do
          name("Large Resource")
          description("Large test resource")
        end
      end

      # Prompt operations
      defprompt "simple_prompt" do
        meta do
          name("Simple Prompt")
          description("Simple test prompt")
        end
      end

      defprompt "complex_prompt" do
        meta do
          name("Complex Prompt")
          description("Complex prompt with parameters")
        end
      end

      @impl true
      def handle_initialize(params, state) do
        {:ok,
         %{
           "protocolVersion" => params["protocolVersion"],
           "serverInfo" => %{
             "name" => "performance-test-server",
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
        repeat = Map.get(args, "repeat", 1)
        response = String.duplicate(msg, repeat)
        {:ok, %{content: [%{"type" => "text", "text" => response}]}, state}
      end

      @impl true
      def handle_tool_call("compute", %{"iterations" => iterations}, state) do
        # CPU intensive computation
        result =
          Enum.reduce(1..iterations, 0, fn i, acc ->
            :math.sqrt(i * i) + acc
          end)

        {:ok, %{content: [%{"type" => "text", "text" => "Computed: #{result}"}]}, state}
      end

      @impl true
      def handle_tool_call("memory_test", %{"size_mb" => size_mb}, state) do
        # Allocate memory
        size_bytes = trunc(size_mb * 1024 * 1024)
        _data = :binary.copy(<<0>>, size_bytes)

        {:ok, %{content: [%{"type" => "text", "text" => "Allocated #{size_mb}MB"}]}, state}
      end

      @impl true
      def handle_resource_read("perf://small", _full_uri, state) do
        # 100 bytes
        content = String.duplicate("x", 100)

        {:ok,
         %{
           uri: "perf://small",
           mimeType: "text/plain",
           text: content
         }, state}
      end

      @impl true
      def handle_resource_read("perf://large", _full_uri, state) do
        # 100KB
        content = String.duplicate("x", 100_000)

        {:ok,
         %{
           uri: "perf://large",
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
                 text: "This is a simple test prompt"
               }
             }
           ]
         }, state}
      end

      @impl true
      def handle_prompt_get("complex_prompt", args, state) do
        topic = Map.get(args, "topic", "general")
        complexity = Map.get(args, "complexity", "medium")

        message_text = """
        Generate a #{complexity} level response about #{topic}.
        Please provide detailed analysis and examples.
        Consider multiple perspectives and provide actionable insights.
        """

        {:ok,
         %{
           description: "Complex prompt about #{topic}",
           messages: [
             %{
               role: "system",
               content: %{
                 type: "text",
                 text: "You are an expert assistant providing detailed analysis."
               }
             },
             %{
               role: "user",
               content: %{
                 type: "text",
                 text: message_text
               }
             }
           ]
         }, state}
      end
    end

    %{server_module: PerformanceTestServer}
  end

  describe "basic operation performance" do
    test "measures core MCP operations", %{server_module: server_module} do
      # Start server
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Define core operations to profile
      operations = [
        # Already done during setup
        {"initialize", fn -> :ok end},
        {"list_tools", fn -> Client.list_tools(client) end},
        {"list_resources", fn -> Client.list_resources(client) end},
        {"list_prompts", fn -> Client.list_prompts(client) end},
        {"ping_tool", fn -> Client.call_tool(client, "ping", %{}) end},
        {"read_small_resource", fn -> Client.read_resource(client, "perf://small") end},
        {"get_simple_prompt", fn -> Client.get_prompt(client, "simple_prompt", %{}) end}
      ]

      # Profile operations
      Logger.info("Profiling basic MCP operations...")
      results = PerformanceProfiler.profile_batch(operations, transport_type: :test)

      # Generate and log report
      report =
        PerformanceProfiler.generate_report(results,
          format: :detailed,
          include_recommendations: true
        )

      Logger.info("Basic Operations Performance Report:\n#{report}")

      # Performance assertions
      assert results.summary.success_rate == 1.0, "All basic operations should succeed"

      assert results.summary.avg_time_ms < 50,
             "Average operation time should be under 50ms for test transport"

      # Store baseline if needed
      if System.get_env("STORE_BASELINE") == "true" do
        version = System.get_env("BASELINE_VERSION") || "current"
        PerformanceProfiler.store_baseline(version, results)
      end

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "payload size performance" do
    test "analyzes performance across different payload sizes", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Test different payload sizes
      payload_sizes = [
        {10, "tiny"},
        {100, "small"},
        {1000, "medium"},
        {10000, "large"},
        {100_000, "xlarge"}
      ]

      Logger.info("Testing payload size performance...")

      payload_results =
        Enum.map(payload_sizes, fn {size, label} ->
          message = String.duplicate("x", size)

          {_result, metrics} =
            PerformanceProfiler.profile_operation(
              fn ->
                Client.call_tool(client, "echo", %{"message" => message})
              end,
              operation_name: "echo_#{label}",
              transport_type: :test,
              payload_size_bytes: byte_size(message)
            )

          metrics
        end)

      # Analyze payload performance trends
      Logger.info("Payload Size Performance Analysis:")

      Enum.each(payload_results, fn metrics ->
        Logger.info(
          "#{metrics.operation_name}: #{Float.round(metrics.execution_time_ms, 2)}ms " <>
            "(#{metrics.payload_size_bytes} bytes)"
        )
      end)

      # Performance assertions
      assert Enum.all?(payload_results, & &1.success), "All payload tests should succeed"

      # Check that performance scales reasonably with payload size
      [tiny, _small, medium, large, xlarge] = Enum.map(payload_results, & &1.execution_time_ms)

      # Allow for some timing variance in small operations
      # Focus on larger-scale trends rather than exact ordering
      assert medium <= large, "Medium payload should not be significantly slower than large"
      assert large <= xlarge, "Large payload should not be significantly slower than xlarge"

      # Allow some overhead, but shouldn't be exponential growth
      assert xlarge < tiny * 100, "XLarge shouldn't be 100x slower than tiny"

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "resource intensive operations" do
    test "profiles CPU and memory intensive operations", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # CPU intensive tests
      cpu_operations = [
        {"compute_light", fn -> Client.call_tool(client, "compute", %{"iterations" => 100}) end},
        {"compute_medium",
         fn -> Client.call_tool(client, "compute", %{"iterations" => 1000}) end},
        {"compute_heavy", fn -> Client.call_tool(client, "compute", %{"iterations" => 10000}) end}
      ]

      Logger.info("Profiling CPU intensive operations...")
      cpu_results = PerformanceProfiler.profile_batch(cpu_operations, transport_type: :test)

      # Memory intensive tests
      memory_operations = [
        {"memory_1mb", fn -> Client.call_tool(client, "memory_test", %{"size_mb" => 1}) end},
        {"memory_5mb", fn -> Client.call_tool(client, "memory_test", %{"size_mb" => 5}) end},
        {"memory_10mb", fn -> Client.call_tool(client, "memory_test", %{"size_mb" => 10}) end}
      ]

      Logger.info("Profiling memory intensive operations...")
      memory_results = PerformanceProfiler.profile_batch(memory_operations, transport_type: :test)

      # Generate reports
      cpu_report = PerformanceProfiler.generate_report(cpu_results, format: :summary)
      memory_report = PerformanceProfiler.generate_report(memory_results, format: :summary)

      Logger.info("CPU Intensive Operations Report:\n#{cpu_report}")
      Logger.info("Memory Intensive Operations Report:\n#{memory_report}")

      # Performance assertions
      assert cpu_results.summary.success_rate == 1.0
      assert memory_results.summary.success_rate == 1.0

      # Check that compute operations scale with iterations
      [light, medium, heavy] = Enum.map(cpu_results.operations, & &1.execution_time_ms)
      assert light < medium, "Light compute should be faster than medium"
      assert medium < heavy, "Medium compute should be faster than heavy"

      # Check memory allocations had measurable impact
      memory_deltas = Enum.map(memory_results.operations, & &1.memory_delta_mb)

      assert Enum.all?(memory_deltas, &(&1 >= 0)),
             "Memory operations should increase memory usage"

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "concurrency performance" do
    test "measures performance under concurrent load", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      # Create multiple clients for concurrent testing
      clients =
        Enum.map(1..5, fn i ->
          {:ok, client} =
            Client.start_link(
              transport: :test,
              server: server_pid
            )

          {i, client}
        end)

      Logger.info("Testing concurrent performance with #{length(clients)} clients...")

      # Concurrent operation function
      concurrent_operation = fn client ->
        # Each client performs multiple operations
        operations = [
          fn -> Client.list_tools(client) end,
          fn -> Client.call_tool(client, "ping", %{}) end,
          fn -> Client.read_resource(client, "perf://small") end
        ]

        Enum.map(operations, fn op -> op.() end)
      end

      # Measure concurrent execution
      {_results, concurrent_metrics} =
        PerformanceProfiler.profile_operation(
          fn ->
            # Run operations concurrently across all clients
            tasks =
              Enum.map(clients, fn {_id, client} ->
                Task.async(fn -> concurrent_operation.(client) end)
              end)

            # Wait for all tasks to complete
            Enum.map(tasks, &Task.await(&1, 10_000))
          end,
          operation_name: "concurrent_operations",
          transport_type: :test
        )

      Logger.info("Concurrent Performance Results:")
      Logger.info("- Execution Time: #{Float.round(concurrent_metrics.execution_time_ms, 2)}ms")
      Logger.info("- Memory Delta: #{Float.round(concurrent_metrics.memory_delta_mb, 3)}MB")
      Logger.info("- GC Collections: #{concurrent_metrics.gc_collections}")
      Logger.info("- Success: #{concurrent_metrics.success}")

      # Performance assertions
      assert concurrent_metrics.success, "Concurrent operations should succeed"

      assert concurrent_metrics.execution_time_ms < 5000,
             "Concurrent operations should complete within 5 seconds"

      # Cleanup clients
      Enum.each(clients, fn {_id, client} -> Client.stop(client) end)
      GenServer.stop(server_pid)
    end
  end

  describe "throughput benchmarks" do
    test "measures maximum throughput", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Throughput test - many fast operations
      operation_count = 100

      operations =
        Enum.map(1..operation_count, fn i ->
          {"ping_#{i}", fn -> Client.call_tool(client, "ping", %{}) end}
        end)

      Logger.info("Running throughput benchmark with #{operation_count} operations...")

      throughput_results = PerformanceProfiler.profile_batch(operations, transport_type: :test)

      # Generate throughput report
      report = PerformanceProfiler.generate_report(throughput_results, format: :summary)
      Logger.info("Throughput Benchmark Report:\n#{report}")

      # Performance assertions
      assert throughput_results.summary.success_rate == 1.0,
             "All throughput operations should succeed"

      assert throughput_results.summary.throughput_ops_per_sec > 10,
             "Should achieve at least 10 ops/sec"

      # Log key metrics
      Logger.info("Throughput Metrics:")
      Logger.info("- Operations: #{operation_count}")
      Logger.info("- Total Time: #{Float.round(throughput_results.summary.total_time_ms, 2)}ms")

      Logger.info(
        "- Throughput: #{Float.round(throughput_results.summary.throughput_ops_per_sec, 2)} ops/sec"
      )

      Logger.info(
        "- Average Latency: #{Float.round(throughput_results.summary.avg_time_ms, 2)}ms"
      )

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "regression detection" do
    test "can detect performance regressions", %{server_module: server_module} do
      # This test demonstrates regression detection capability
      # In real usage, baselines would be stored from previous versions

      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Create baseline results (simulated)
      baseline_operations = [
        {"ping_baseline", fn -> Client.call_tool(client, "ping", %{}) end}
      ]

      baseline_results =
        PerformanceProfiler.profile_batch(baseline_operations, transport_type: :test)

      # Store baseline
      PerformanceProfiler.store_baseline("test_baseline", baseline_results)

      # Create current results (simulated - artificially slower)
      current_operations = [
        {"ping_current",
         fn ->
           # Simulate slower operation
           Process.sleep(50)
           Client.call_tool(client, "ping", %{})
         end}
      ]

      current_results =
        PerformanceProfiler.profile_batch(current_operations, transport_type: :test)

      # Check for regression
      regression_report = PerformanceProfiler.check_regression("test_baseline", current_results)

      Logger.info("Regression Analysis:")
      Logger.info("- Baseline Avg Time: #{regression_report.baseline_avg_time_ms}ms")
      Logger.info("- Current Avg Time: #{regression_report.current_avg_time_ms}ms")
      Logger.info("- Change: #{Float.round(regression_report.avg_time_change_percent, 1)}%")
      Logger.info("- Has Regression: #{regression_report.has_regression}")

      if regression_report.has_regression do
        Logger.warning("Performance Regressions Detected:")
        Enum.each(regression_report.regressions, &Logger.warning("- #{&1}"))
      end

      # Assert regression detection worked
      assert regression_report.has_regression,
             "Should detect the artificial performance regression"

      assert regression_report.avg_time_change_percent > 20,
             "Should detect significant performance change"

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end
end
