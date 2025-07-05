defmodule ExMCP.Benchmarks.ServerPerformanceTest do
  @moduledoc """
  Performance benchmarks for ExMCP.Server to establish baseline before refactoring.

  Run with: mix test test/ex_mcp/benchmarks/server_performance_test.exs --include benchmark
  """

  use ExUnit.Case, async: false

  describe "compilation performance" do
    @describetag :benchmark
    test "measure DSL compilation time" do
      # Measure time to compile a module with DSL
      {compile_time, _} =
        :timer.tc(fn ->
          defmodule BenchmarkDSLServer do
            use ExMCP.Server

            # Generate multiple tools to stress the macro system
            for i <- 1..50 do
              deftool "tool_#{i}" do
                meta do
                  description("Benchmark tool #{i}")
                end

                input_schema(%{
                  type: "object",
                  properties: %{
                    param1: %{type: "string"},
                    param2: %{type: "number"},
                    param3: %{type: "boolean"}
                  },
                  required: ["param1"]
                })
              end
            end

            # Generate multiple resources
            for i <- 1..30 do
              defresource "resource://bench/#{i}" do
                meta do
                  name("Benchmark Resource #{i}")
                  description("Test resource")
                end

                mime_type("application/json")
              end
            end

            # Generate multiple prompts
            for i <- 1..20 do
              defprompt "prompt_#{i}" do
                meta do
                  name("Benchmark Prompt #{i}")
                  description("Test prompt")
                end

                arguments do
                  arg(:param1, description: "First parameter")
                  arg(:param2, required: true)
                end
              end
            end

            # Implement handlers
            @impl true
            def handle_tool_call(tool_name, params, state) do
              {:ok, %{content: [text("Result for #{tool_name}")]}, state}
            end

            @impl true
            def handle_resource_read(uri, _full_uri, state) do
              {:ok, [json(%{uri: uri})], state}
            end

            @impl true
            def handle_prompt_get(prompt_name, _args, state) do
              {:ok, %{messages: [user("Test for #{prompt_name}")]}, state}
            end
          end
        end)

      IO.puts("\nDSL Compilation time: #{compile_time / 1000}ms")

      # Store baseline
      # Should compile in less than 10 seconds
      assert compile_time < 10_000_000

      # Cleanup
      :code.delete(BenchmarkDSLServer)
      :code.purge(BenchmarkDSLServer)
    end
  end

  describe "runtime performance" do
    @describetag :benchmark
    setup do
      # Create a test server with many endpoints
      defmodule RuntimeBenchServer do
        use ExMCP.Server

        deftool "compute" do
          meta do
            description("Compute intensive task")
          end

          input_schema(%{
            type: "object",
            properties: %{
              n: %{type: "integer"}
            }
          })
        end

        @impl true
        def handle_tool_call("compute", %{"n" => n}, state) do
          # Simulate some computation
          result = Enum.reduce(1..n, 0, &+/2)
          {:ok, %{content: [text("Result: #{result}")]}, state}
        end
      end

      {:ok, pid} = RuntimeBenchServer.start_link()

      on_exit(fn ->
        if Process.alive?(pid) do
          GenServer.stop(pid)
        end
      end)

      %{server: pid}
    end

    test "measure request processing time", %{server: server} do
      # Warm up
      for _ <- 1..100 do
        GenServer.call(server, :get_tools)
      end

      # Measure list_tools performance
      {list_time, _} =
        :timer.tc(fn ->
          for _ <- 1..1000 do
            GenServer.call(server, :get_tools)
          end
        end)

      # microseconds per call
      avg_list_time = list_time / 1000 / 1000
      IO.puts("\nAverage list_tools time: #{avg_list_time}μs")

      # Measure tool call performance
      {call_time, _} =
        :timer.tc(fn ->
          for _ <- 1..1000 do
            GenServer.call(server, {:execute_tool, "compute", %{"n" => 100}})
          end
        end)

      avg_call_time = call_time / 1000 / 1000
      IO.puts("Average tool call time: #{avg_call_time}μs")

      # Baseline assertions
      # Should be under 1ms
      assert avg_list_time < 1000
      # Should be under 5ms
      assert avg_call_time < 5000
    end

    test "measure memory usage patterns", %{server: server} do
      # Get initial memory
      :erlang.garbage_collect(server)
      {:memory, initial_memory} = Process.info(server, :memory)

      # Perform many operations
      for i <- 1..1000 do
        GenServer.call(server, {:execute_tool, "compute", %{"n" => i}})
      end

      # Check memory after operations
      {:memory, after_ops_memory} = Process.info(server, :memory)

      # Force GC and check again
      :erlang.garbage_collect(server)
      {:memory, after_gc_memory} = Process.info(server, :memory)

      memory_growth = after_ops_memory - initial_memory
      memory_retained = after_gc_memory - initial_memory

      IO.puts("\nMemory growth during operations: #{memory_growth} bytes")
      IO.puts("Memory retained after GC: #{memory_retained} bytes")

      # Memory should not grow unbounded
      # Less than 1MB retained
      assert memory_retained < 1_000_000
    end
  end

  describe "macro expansion performance" do
    @describetag :benchmark
    test "measure capability detection performance" do
      defmodule LargeCapabilityServer do
        use ExMCP.Server

        # Many tools to stress capability detection
        for i <- 1..100 do
          deftool "cap_tool_#{i}" do
            meta do
              description("Tool #{i}")
            end

            input_schema(%{type: "object"})
          end
        end

        # Handle tool calls with pattern matching fallback
        @impl true
        def handle_tool_call(tool_name, _params, state) when is_binary(tool_name) do
          case String.starts_with?(tool_name, "cap_tool_") do
            true -> {:ok, %{content: []}, state}
            false -> {:error, :tool_not_found, state}
          end
        end

        # Some subscribable resources
        for i <- 1..50 do
          defresource "cap://res/#{i}" do
            meta do
              name("Resource #{i}")
              description("Resource #{i}")
            end

            mime_type("text/plain")
            subscribable(true)
          end
        end

        # Handle resource reads with pattern matching fallback
        @impl true
        def handle_resource_read(uri, _full_uri, state) when is_binary(uri) do
          case String.starts_with?(uri, "cap://res/") do
            true -> {:ok, [], state}
            false -> {:error, :resource_not_found, state}
          end
        end
      end

      # Measure capability detection
      {cap_time, capabilities} =
        :timer.tc(fn ->
          LargeCapabilityServer.get_capabilities()
        end)

      IO.puts("\nCapability detection time: #{cap_time / 1000}ms")

      assert Map.has_key?(capabilities, "tools")
      assert capabilities["resources"]["subscribe"] == true
      # Should be under 100ms
      assert cap_time < 100_000

      # Cleanup
      :code.delete(LargeCapabilityServer)
      :code.purge(LargeCapabilityServer)
    end
  end
end
