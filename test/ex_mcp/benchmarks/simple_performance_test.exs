defmodule ExMCP.Benchmarks.SimplePerformanceTest do
  @moduledoc """
  Simple performance benchmarks for ExMCP.Server to establish baseline before refactoring.

  Run with: mix test test/ex_mcp/benchmarks/simple_performance_test.exs --include benchmark
  """

  use ExUnit.Case, async: false

  describe "baseline performance" do
    @describetag :benchmark
    test "measure simple DSL compilation time" do
      # Measure time to compile a basic module with DSL
      {compile_time, _} =
        :timer.tc(fn ->
          defmodule SimpleServerBench do
            use ExMCP.Server

            deftool "test_tool" do
              meta do
                description("Test tool")
              end

              input_schema(%{
                type: "object",
                properties: %{
                  input: %{type: "string"}
                }
              })
            end

            defresource "test://resource" do
              meta do
                name("Test Resource")
                description("Test resource")
              end

              mime_type("application/json")
            end

            defprompt "test_prompt" do
              meta do
                name("Test Prompt")
                description("Test prompt")
              end

              arguments do
                arg(:text, required: true)
              end
            end

            @impl true
            def handle_tool_call("test_tool", params, state) do
              {:ok, %{content: [text("Result")]}, state}
            end

            @impl true
            def handle_resource_read(_uri, _full_uri, state) do
              {:ok, [json(%{test: true})], state}
            end

            @impl true
            def handle_prompt_get(_prompt, _args, state) do
              {:ok, %{messages: [user("Test")]}, state}
            end
          end
        end)

      IO.puts("\nSimple DSL Compilation time: #{compile_time / 1000}ms")

      # Store baseline - should compile quickly
      # Should compile in less than 1 second
      assert compile_time < 1_000_000

      # Cleanup
      :code.delete(SimpleServerBench)
      :code.purge(SimpleServerBench)
    end

    test "measure runtime performance" do
      defmodule RuntimeTestServer do
        use ExMCP.Server

        deftool "compute" do
          meta do
            description("Compute task")
          end

          input_schema(%{
            type: "object",
            properties: %{n: %{type: "integer"}}
          })
        end

        @impl true
        def handle_tool_call("compute", %{"n" => n}, state) do
          result = Enum.reduce(1..n, 0, &+/2)
          {:ok, %{content: [text("Result: #{result}")]}, state}
        end
      end

      {:ok, server} = RuntimeTestServer.start_link()

      # Warm up
      for _ <- 1..100 do
        GenServer.call(server, {:mcp, :list_tools})
      end

      # Measure list_tools performance
      {list_time, _} =
        :timer.tc(fn ->
          for _ <- 1..1000 do
            GenServer.call(server, {:mcp, :list_tools})
          end
        end)

      # microseconds per call
      avg_list_time = list_time / 1000 / 1000
      IO.puts("\nAverage list_tools time: #{avg_list_time}μs")

      # Measure tool call performance
      {call_time, _} =
        :timer.tc(fn ->
          for _ <- 1..1000 do
            GenServer.call(server, {:mcp, :call_tool, "compute", %{"n" => 100}})
          end
        end)

      avg_call_time = call_time / 1000 / 1000
      IO.puts("Average tool call time: #{avg_call_time}μs")

      # Memory check
      :erlang.garbage_collect(server)
      {:memory, initial_memory} = Process.info(server, :memory)

      # Do many operations
      for i <- 1..1000 do
        GenServer.call(server, {:mcp, :call_tool, "compute", %{"n" => i}})
      end

      {:memory, after_ops_memory} = Process.info(server, :memory)
      :erlang.garbage_collect(server)
      {:memory, after_gc_memory} = Process.info(server, :memory)

      IO.puts("\nMemory growth: #{after_ops_memory - initial_memory} bytes")
      IO.puts("Memory after GC: #{after_gc_memory - initial_memory} bytes")

      GenServer.stop(server)

      # Baseline assertions
      # Should be under 1ms
      assert avg_list_time < 1000
      # Should be under 5ms
      assert avg_call_time < 5000
      # Less than 100KB growth
      assert after_gc_memory - initial_memory < 100_000
    end

    test "measure large module compilation" do
      # Generate a module with many tools/resources/prompts
      {compile_time, _} =
        :timer.tc(fn ->
          ast =
            quote do
              defmodule LargeServerBench do
                use ExMCP.Server

                # Generate 50 tools
                unquote_splicing(
                  for i <- 1..50 do
                    quote do
                      deftool unquote("tool_#{i}") do
                        meta do
                          description(unquote("Tool number #{i}"))
                        end

                        input_schema(%{
                          type: "object",
                          properties: %{
                            param: %{type: "string"}
                          }
                        })
                      end
                    end
                  end
                )

                # Generate 30 resources
                unquote_splicing(
                  for i <- 1..30 do
                    quote do
                      defresource unquote("resource://test/#{i}") do
                        meta do
                          name(unquote("Resource #{i}"))
                          description(unquote("Test resource #{i}"))
                        end

                        mime_type("application/json")
                      end
                    end
                  end
                )

                # Generate 20 prompts
                unquote_splicing(
                  for i <- 1..20 do
                    quote do
                      defprompt unquote("prompt_#{i}") do
                        meta do
                          name(unquote("Prompt #{i}"))
                          description(unquote("Test prompt #{i}"))
                        end

                        arguments do
                          arg(:text, description: "Text input")
                        end
                      end
                    end
                  end
                )

                # Single handler for all
                @impl true
                def handle_tool_call(_tool, _params, state) do
                  {:ok, %{content: [text("Done")]}, state}
                end

                @impl true
                def handle_resource_read(_uri, _full_uri, state) do
                  {:ok, [json(%{})], state}
                end

                @impl true
                def handle_prompt_get(_prompt, _args, state) do
                  {:ok, %{messages: []}, state}
                end
              end
            end

          {_result, _bindings} = Code.eval_quoted(ast)
          # Ensure module is compiled and available
          {:module, LargeServerBench} = Code.ensure_compiled(LargeServerBench)
        end)

      IO.puts(
        "\nLarge module (50 tools, 30 resources, 20 prompts) compilation time: #{compile_time / 1000}ms"
      )

      # Capability detection performance
      {cap_time, capabilities} =
        :timer.tc(fn ->
          LargeServerBench.get_capabilities()
        end)

      IO.puts("Capability detection time: #{cap_time}μs")

      # Should compile in less than 5 seconds
      assert compile_time < 5_000_000
      # Should detect capabilities in less than 10ms
      assert cap_time < 10_000
      assert Map.has_key?(capabilities, "tools")

      # Cleanup
      :code.delete(LargeServerBench)
      :code.purge(LargeServerBench)
    end
  end
end
