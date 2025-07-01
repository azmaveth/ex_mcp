defmodule ExMCP.Integration.PerformanceProfilingTest do
  @moduledoc """
  Performance Profiling Integration Test Suite.

  This test suite profiles the performance characteristics of MCP operations
  across different scenarios, loads, and configurations to ensure the system
  meets performance requirements and to identify potential bottlenecks.
  """

  use ExUnit.Case, async: false

  alias ExMCP.Client

  @moduletag :integration
  @moduletag :performance
  @moduletag timeout: 120_000

  describe "baseline performance profiling" do
    setup do
      defmodule PerfProfileServer do
        use ExMCP.Server

        deftool "fast_tool" do
          meta do
            description("Tool with minimal processing")
            input_schema(%{type: "object", properties: %{}})
          end
        end

        deftool "medium_tool" do
          meta do
            description("Tool with moderate processing")

            input_schema(%{
              type: "object",
              properties: %{
                iterations: %{type: "integer", default: 1000}
              }
            })
          end
        end

        deftool "slow_tool" do
          meta do
            description("Tool with intentional delay")

            input_schema(%{
              type: "object",
              properties: %{
                delay_ms: %{type: "integer", default: 100}
              }
            })
          end
        end

        deftool "payload_tool" do
          meta do
            description("Tool for testing payload sizes")

            input_schema(%{
              type: "object",
              properties: %{
                input_size: %{type: "integer"},
                output_size: %{type: "integer"}
              }
            })
          end
        end

        defresource "perf://small_resource" do
          meta do
            name("Small Resource")
            description("Small test resource")
          end
        end

        defresource "perf://large_resource" do
          meta do
            name("Large Resource")
            description("Large test resource")
          end
        end

        @impl true
        def handle_tool_call("fast_tool", _args, state) do
          {:ok, %{content: [%{"type" => "text", "text" => "fast_result"}]}, state}
        end

        @impl true
        def handle_tool_call("medium_tool", %{"iterations" => iterations}, state) do
          # Simulate some CPU work
          result = Enum.reduce(1..iterations, 0, fn i, acc -> acc + i end)
          {:ok, %{content: [%{"type" => "text", "text" => "computed: #{result}"}]}, state}
        end

        @impl true
        def handle_tool_call("slow_tool", %{"delay_ms" => delay}, state) do
          Process.sleep(delay)
          {:ok, %{content: [%{"type" => "text", "text" => "delayed_result"}]}, state}
        end

        @impl true
        def handle_tool_call("payload_tool", args, state) do
          input_size = Map.get(args, "input_size", 100)
          output_size = Map.get(args, "output_size", 100)

          # Generate output payload
          output = String.duplicate("x", output_size)
          {:ok, %{content: [%{"type" => "text", "text" => output}]}, state}
        end

        @impl true
        def handle_read_resource("perf://small_resource", state) do
          # ~60 bytes
          content = String.duplicate("small ", 10)

          {:ok,
           %{
             uri: "perf://small_resource",
             mimeType: "text/plain",
             text: content
           }, state}
        end

        @impl true
        def handle_read_resource("perf://large_resource", state) do
          # ~100KB
          content = String.duplicate("large resource data ", 5000)

          {:ok,
           %{
             uri: "perf://large_resource",
             mimeType: "text/plain",
             text: content
           }, state}
        end
      end

      %{server_module: PerfProfileServer}
    end

    test "connection establishment performance", %{server_module: server_module} do
      # Profile multiple connection establishments
      establishment_times =
        for _i <- 1..10 do
          {:ok, server_pid} = server_module.start_link(transport: :test)
          Process.sleep(5)

          start_time = System.monotonic_time(:microsecond)

          {:ok, client} =
            Client.start_link(
              transport: :test,
              server: server_pid
            )

          end_time = System.monotonic_time(:microsecond)

          establishment_time = end_time - start_time

          Client.stop(client)
          GenServer.stop(server_pid)

          establishment_time
        end

      avg_time = Enum.sum(establishment_times) / length(establishment_times)
      max_time = Enum.max(establishment_times)
      min_time = Enum.min(establishment_times)

      IO.puts("\nConnection Establishment Performance:")
      IO.puts("  Average: #{Float.round(avg_time / 1000, 2)}ms")
      IO.puts("  Min: #{Float.round(min_time / 1000, 2)}ms")
      IO.puts("  Max: #{Float.round(max_time / 1000, 2)}ms")

      # Assert reasonable performance
      # < 50ms average
      assert avg_time < 50_000
      # < 100ms max
      assert max_time < 100_000
    end

    test "tool execution performance profiling", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Profile fast tool
      fast_times =
        for _i <- 1..100 do
          start_time = System.monotonic_time(:microsecond)
          {:ok, _result} = Client.call_tool(client, "fast_tool", %{})
          end_time = System.monotonic_time(:microsecond)
          end_time - start_time
        end

      # Profile medium tool
      medium_times =
        for _i <- 1..10 do
          start_time = System.monotonic_time(:microsecond)
          {:ok, _result} = Client.call_tool(client, "medium_tool", %{"iterations" => 1000})
          end_time = System.monotonic_time(:microsecond)
          end_time - start_time
        end

      # Profile slow tool
      slow_times =
        for _i <- 1..5 do
          start_time = System.monotonic_time(:microsecond)
          {:ok, _result} = Client.call_tool(client, "slow_tool", %{"delay_ms" => 50})
          end_time = System.monotonic_time(:microsecond)
          end_time - start_time
        end

      fast_avg = Enum.sum(fast_times) / length(fast_times)
      medium_avg = Enum.sum(medium_times) / length(medium_times)
      slow_avg = Enum.sum(slow_times) / length(slow_times)

      IO.puts("\nTool Execution Performance:")
      IO.puts("  Fast tool average: #{Float.round(fast_avg / 1000, 2)}ms")
      IO.puts("  Medium tool average: #{Float.round(medium_avg / 1000, 2)}ms")
      IO.puts("  Slow tool average: #{Float.round(slow_avg / 1000, 2)}ms")

      # Assert reasonable performance
      # < 10ms for fast tool
      assert fast_avg < 10_000
      # < 100ms for medium tool
      assert medium_avg < 100_000
      # ~50-100ms for slow tool
      assert slow_avg > 50_000 and slow_avg < 100_000

      Client.stop(client)
      GenServer.stop(server_pid)
    end

    test "payload size performance impact", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      payload_sizes = [100, 1_000, 10_000, 100_000]

      performance_data =
        for size <- payload_sizes do
          times =
            for _i <- 1..5 do
              start_time = System.monotonic_time(:microsecond)

              {:ok, result} =
                Client.call_tool(client, "payload_tool", %{
                  "output_size" => size
                })

              end_time = System.monotonic_time(:microsecond)

              # Verify payload size
              actual_size = byte_size(hd(result["content"])["text"])
              assert actual_size == size

              end_time - start_time
            end

          avg_time = Enum.sum(times) / length(times)
          {size, avg_time}
        end

      IO.puts("\nPayload Size Performance Impact:")

      Enum.each(performance_data, fn {size, avg_time} ->
        IO.puts("  #{size} bytes: #{Float.round(avg_time / 1000, 2)}ms")
      end)

      # Performance should scale reasonably with payload size
      [{_, time_100}, {_, time_1k}, {_, time_10k}, {_, time_100k}] = performance_data

      # Larger payloads should not be dramatically slower (within reason)
      # 100KB shouldn't be 10x slower than 100B
      assert time_100k < time_100 * 10
      # 10KB shouldn't be 5x slower than 1KB
      assert time_10k < time_1k * 5

      Client.stop(client)
      GenServer.stop(server_pid)
    end

    test "resource access performance profiling", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Profile small resource access
      small_times =
        for _i <- 1..50 do
          start_time = System.monotonic_time(:microsecond)
          {:ok, _result} = Client.read_resource(client, "perf://small_resource")
          end_time = System.monotonic_time(:microsecond)
          end_time - start_time
        end

      # Profile large resource access
      large_times =
        for _i <- 1..10 do
          start_time = System.monotonic_time(:microsecond)
          {:ok, result} = Client.read_resource(client, "perf://large_resource")
          end_time = System.monotonic_time(:microsecond)

          # Verify we got the large content
          assert byte_size(result["text"]) > 50_000

          end_time - start_time
        end

      small_avg = Enum.sum(small_times) / length(small_times)
      large_avg = Enum.sum(large_times) / length(large_times)

      IO.puts("\nResource Access Performance:")
      IO.puts("  Small resource average: #{Float.round(small_avg / 1000, 2)}ms")
      IO.puts("  Large resource average: #{Float.round(large_avg / 1000, 2)}ms")

      # Assert reasonable performance
      # < 10ms for small resource
      assert small_avg < 10_000
      # < 100ms for large resource
      assert large_avg < 100_000

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "concurrent load performance" do
    test "concurrent client performance", %{server_module: server_module} do
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      # Create multiple clients
      clients =
        for _i <- 1..10 do
          {:ok, client} =
            Client.start_link(
              transport: :test,
              server: server_pid
            )

          client
        end

      # Run concurrent operations
      start_time = System.monotonic_time(:microsecond)

      tasks =
        for client <- clients do
          Task.async(fn ->
            # Each client performs multiple operations
            for _j <- 1..5 do
              {:ok, _} = Client.call_tool(client, "fast_tool", %{})
            end
          end)
        end

      # Wait for all tasks to complete
      Task.await_many(tasks, 10_000)

      end_time = System.monotonic_time(:microsecond)
      total_time = end_time - start_time

      # Calculate operations per second
      # 10 clients * 5 operations each
      total_operations = 10 * 5
      ops_per_second = total_operations / (total_time / 1_000_000)

      IO.puts("\nConcurrent Performance:")
      IO.puts("  Total operations: #{total_operations}")
      IO.puts("  Total time: #{Float.round(total_time / 1000, 2)}ms")
      IO.puts("  Operations per second: #{Float.round(ops_per_second, 2)}")

      # Should handle decent concurrent load
      # > 100 ops/sec
      assert ops_per_second > 100
      # < 5 seconds total
      assert total_time < 5_000_000

      # Cleanup
      Enum.each(clients, &Client.stop/1)
      GenServer.stop(server_pid)
    end

    test "memory usage during operations" do
      defmodule MemoryTestServer do
        use ExMCP.Server

        deftool "memory_intensive" do
          meta do
            description("Tool that uses memory")

            input_schema(%{
              type: "object",
              properties: %{
                size: %{type: "integer", default: 1000}
              }
            })
          end
        end

        @impl true
        def handle_tool_call("memory_intensive", %{"size" => size}, state) do
          # Allocate and return memory
          data = :crypto.strong_rand_bytes(size)
          encoded = Base.encode64(data)
          {:ok, %{content: [%{"type" => "text", "text" => encoded}]}, state}
        end
      end

      {:ok, server_pid} = MemoryTestServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Measure memory before operations
      {memory_before, _} = :erlang.process_info(self(), :memory)

      # Perform memory-intensive operations
      for size <- [1_000, 10_000, 100_000] do
        {:ok, result} = Client.call_tool(client, "memory_intensive", %{"size" => size})

        # Verify we got the expected data size
        encoded_size = byte_size(hd(result["content"])["text"])
        # Base64 encoding overhead
        expected_encoded_size = div(size * 4, 3) + 4
        assert encoded_size >= expected_encoded_size
      end

      # Force garbage collection
      :erlang.garbage_collect()
      Process.sleep(100)

      # Measure memory after operations
      {memory_after, _} = :erlang.process_info(self(), :memory)

      memory_increase = memory_after - memory_before

      IO.puts("\nMemory Usage:")
      IO.puts("  Memory before: #{memory_before} bytes")
      IO.puts("  Memory after: #{memory_after} bytes")
      IO.puts("  Memory increase: #{memory_increase} bytes")

      # Memory increase should be reasonable (not a huge leak)
      # < 10MB increase
      assert memory_increase < 10_000_000

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "reliability feature performance impact" do
    test "retry policy performance overhead" do
      defmodule RetryTestServer do
        use ExMCP.Server

        deftool "reliable_tool" do
          meta do
            description("Always succeeds")
            input_schema(%{type: "object", properties: %{}})
          end
        end

        @impl true
        def handle_tool_call("reliable_tool", _args, state) do
          {:ok, %{content: [%{"type" => "text", "text" => "success"}]}, state}
        end
      end

      {:ok, server_pid} = RetryTestServer.start_link(transport: :test)
      Process.sleep(10)

      # Client without retry policy
      {:ok, client_no_retry} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Client with retry policy
      {:ok, client_with_retry} =
        Client.start_link(
          transport: :test,
          server: server_pid,
          retry_policy: [
            max_attempts: 3,
            initial_delay: 10,
            max_delay: 100
          ]
        )

      # Measure performance without retry
      no_retry_times =
        for _i <- 1..50 do
          start_time = System.monotonic_time(:microsecond)
          {:ok, _} = Client.call_tool(client_no_retry, "reliable_tool", %{})
          end_time = System.monotonic_time(:microsecond)
          end_time - start_time
        end

      # Measure performance with retry policy
      with_retry_times =
        for _i <- 1..50 do
          start_time = System.monotonic_time(:microsecond)
          {:ok, _} = Client.call_tool(client_with_retry, "reliable_tool", %{})
          end_time = System.monotonic_time(:microsecond)
          end_time - start_time
        end

      no_retry_avg = Enum.sum(no_retry_times) / length(no_retry_times)
      with_retry_avg = Enum.sum(with_retry_times) / length(with_retry_times)

      overhead_percent = (with_retry_avg - no_retry_avg) / no_retry_avg * 100

      IO.puts("\nRetry Policy Performance Impact:")
      IO.puts("  Without retry: #{Float.round(no_retry_avg / 1000, 2)}ms")
      IO.puts("  With retry: #{Float.round(with_retry_avg / 1000, 2)}ms")
      IO.puts("  Overhead: #{Float.round(overhead_percent, 2)}%")

      # Retry policy should add minimal overhead for successful operations
      # < 20% overhead
      assert overhead_percent < 20

      Client.stop(client_no_retry)
      Client.stop(client_with_retry)
      GenServer.stop(server_pid)
    end
  end
end
