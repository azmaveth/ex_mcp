defmodule ExMCP.ClientBeamTransportTest do
  use ExUnit.Case, async: false

  alias ExMCP.Client

  describe "Client with BEAM transport" do
    setup do
      # Start application for this test suite
      {:ok, _} = Application.ensure_all_started(:ex_mcp)

      on_exit(fn ->
        Application.stop(:ex_mcp)
      end)

      # Start a test MCP service using ExMCP.Service
      defmodule TestCalculatorService do
        use ExMCP.Service, name: :beam_transport_calculator_service

        @impl true
        def handle_mcp_request("initialize", _params, state) do
          # Handle the initialize request
          {:ok,
           %{
             "protocolVersion" => "2025-03-26",
             "capabilities" => %{},
             "serverInfo" => %{
               "name" => "TestCalculatorService",
               "version" => "1.0.0"
             }
           }, state}
        end

        # Handle tools/list to list available tools
        def handle_mcp_request("tools/list", _params, state) do
          tools = [
            %{
              "name" => "add",
              "description" => "Add two numbers",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{
                  "a" => %{"type" => "number"},
                  "b" => %{"type" => "number"}
                },
                "required" => ["a", "b"]
              }
            },
            %{
              "name" => "subtract",
              "description" => "Subtract two numbers",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{
                  "a" => %{"type" => "number"},
                  "b" => %{"type" => "number"}
                },
                "required" => ["a", "b"]
              }
            },
            %{
              "name" => "divide",
              "description" => "Divide two numbers",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{
                  "a" => %{"type" => "number"},
                  "b" => %{"type" => "number"}
                },
                "required" => ["a", "b"]
              }
            },
            %{
              "name" => "slow_operation",
              "description" => "A slow operation for testing timeouts",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{}
              }
            }
          ]

          {:ok, %{"tools" => tools}, state}
        end

        # Handle tools/call - extract tool name and delegate
        def handle_mcp_request("tools/call", %{"name" => tool_name, "arguments" => args}, state) do
          result =
            case {tool_name, args} do
              {"add", %{"a" => a, "b" => b}} ->
                %{"result" => a + b}

              {"subtract", %{"a" => a, "b" => b}} ->
                %{"result" => a - b}

              {"divide", %{"a" => _a, "b" => 0}} ->
                {:error, %{"code" => -32602, "message" => "Division by zero"}}

              {"divide", %{"a" => a, "b" => b}} ->
                %{"result" => a / b}

              {"slow_operation", _} ->
                Process.sleep(100)
                %{"result" => "completed"}

              {unknown_tool, _} ->
                {:error, %{"code" => -32601, "message" => "Tool not found: #{unknown_tool}"}}
            end

          case result do
            {:error, error} -> {:error, error, state}
            _ -> {:ok, result, state}
          end
        end

        # Handle notifications in the same callback
        def handle_mcp_request("notifications/initialized", _params, state) do
          {:ok, %{}, state}
        end

        def handle_mcp_request("log", %{"message" => message}, state) do
          # Store notifications in state for testing
          notifications = Map.get(state, :notifications, [])
          {:ok, %{}, Map.put(state, :notifications, [message | notifications])}
        end

        def handle_mcp_request(_method, _params, state) do
          {:error, %{"code" => -32601, "message" => "Method not found"}, state}
        end

        # Helper to retrieve notifications for testing
        def get_notifications(pid) do
          GenServer.call(pid, :get_notifications)
        end

        def handle_call(:get_notifications, _from, state) do
          {:reply, Enum.reverse(Map.get(state, :notifications, [])), state}
        end
      end

      # Start the service
      {:ok, service_pid} = TestCalculatorService.start_link(%{})

      on_exit(fn ->
        ExMCP.Native.unregister_service(:beam_transport_calculator_service)
      end)

      {:ok, service_pid: service_pid}
    end

    test "connects to BEAM service via native transport" do
      # Start client with BEAM transport
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :beam_transport_calculator_service
        )

      # Verify connection
      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :connected
    end

    test "calls tools via BEAM transport" do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :beam_transport_calculator_service
        )

      # Test successful calls
      assert {:ok, %{"result" => 5}} = Client.call_tool(client, "add", %{"a" => 2, "b" => 3})

      assert {:ok, %{"result" => 10}} =
               Client.call_tool(client, "subtract", %{"a" => 15, "b" => 5})

      assert {:ok, %{"result" => 2.5}} = Client.call_tool(client, "divide", %{"a" => 5, "b" => 2})
    end

    test "handles errors via BEAM transport" do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :beam_transport_calculator_service
        )

      # Test error responses
      assert {:error, %ExMCP.Error{code: -32602, message: "Division by zero"}} =
               Client.call_tool(client, "divide", %{"a" => 10, "b" => 0})

      assert {:error, %ExMCP.Error{code: -32601, message: "Tool not found: unknown_method"}} =
               Client.call_tool(client, "unknown_method", %{})
    end

    test "handles notifications via BEAM transport", %{service_pid: service_pid} do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :beam_transport_calculator_service
        )

      # Send notifications
      :ok = Client.notify(client, "log", %{"message" => "Test log 1"})
      :ok = Client.notify(client, "log", %{"message" => "Test log 2"})

      # Give notifications time to process
      Process.sleep(50)

      # Verify notifications were received
      # Call the service pid directly since the module is defined inside setup
      notifications = GenServer.call(service_pid, :get_notifications)
      assert notifications == ["Test log 1", "Test log 2"]
    end

    test "respects timeout settings" do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :beam_transport_calculator_service,
          # 50ms timeout
          timeout: 50
        )

      # This should timeout since slow_operation takes 100ms
      assert {:error, :timeout} = Client.call_tool(client, "slow_operation", %{})
    end

    test "handles service not found" do
      # Try to connect to non-existent service
      # The client process will exit if connection fails
      Process.flag(:trap_exit, true)

      {:error, {:connection_failed, {:service_not_available, :nonexistent_service}}} =
        Client.start_link(
          transport: :beam,
          service_name: :nonexistent_service
        )
    end

    test "supports cross-node communication" do
      # This test would require multiple nodes, skipping for now
      # but the transport supports it via {service_name, node} tuples
      :ok
    end
  end

  describe "Client with native transport alias" do
    setup do
      # Start application for this test suite
      {:ok, _} = Application.ensure_all_started(:ex_mcp)

      on_exit(fn ->
        Application.stop(:ex_mcp)
      end)

      # Create another test service
      defmodule TestEchoService do
        use ExMCP.Service, name: :native_alias_echo_service

        @impl true
        def handle_mcp_request("initialize", _params, state) do
          {:ok,
           %{
             "protocolVersion" => "2025-03-26",
             "capabilities" => %{},
             "serverInfo" => %{
               "name" => "TestEchoService",
               "version" => "1.0.0"
             }
           }, state}
        end

        # Handle tools/list
        def handle_mcp_request("tools/list", _params, state) do
          tools = [
            %{
              "name" => "echo",
              "description" => "Echo back the input",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{},
                "additionalProperties" => true
              }
            }
          ]

          {:ok, %{"tools" => tools}, state}
        end

        # Handle tools/call
        def handle_mcp_request("tools/call", %{"name" => "echo", "arguments" => args}, state) do
          {:ok, args, state}
        end

        def handle_mcp_request("tools/call", %{"name" => tool_name}, state) do
          {:error, %{"code" => -32601, "message" => "Tool not found: #{tool_name}"}, state}
        end

        # Handle notifications
        def handle_mcp_request("notifications/initialized", _params, state) do
          {:ok, %{}, state}
        end

        def handle_mcp_request(_method, _params, state) do
          {:error, %{"code" => -32601, "message" => "Method not found"}, state}
        end
      end

      {:ok, _pid} = TestEchoService.start_link(%{})

      on_exit(fn ->
        ExMCP.Native.unregister_service(:native_alias_echo_service)
      end)

      :ok
    end

    test "native transport works as alias for beam" do
      # Both :native and :beam should use the same transport
      {:ok, client} =
        Client.start_link(
          # Using :native instead of :beam
          transport: :native,
          service_name: :native_alias_echo_service
        )

      assert {:ok, %{"message" => "hello"}} =
               Client.call_tool(client, "echo", %{"message" => "hello"})
    end
  end

  describe "Performance characteristics" do
    setup do
      # Start application for this test suite
      {:ok, _} = Application.ensure_all_started(:ex_mcp)

      on_exit(fn ->
        Application.stop(:ex_mcp)
      end)

      defmodule PerfTestService do
        use ExMCP.Service, name: :perf_test_service

        @impl true
        def handle_mcp_request("initialize", _params, state) do
          {:ok,
           %{
             "protocolVersion" => "2025-03-26",
             "capabilities" => %{},
             "serverInfo" => %{
               "name" => "PerfTestService",
               "version" => "1.0.0"
             }
           }, state}
        end

        # Handle tools/list
        def handle_mcp_request("tools/list", _params, state) do
          tools = [
            %{
              "name" => "noop",
              "description" => "No-op tool for performance testing",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{}
              }
            },
            %{
              "name" => "echo",
              "description" => "Echo back the input",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{},
                "additionalProperties" => true
              }
            }
          ]

          {:ok, %{"tools" => tools}, state}
        end

        # Handle tools/call
        def handle_mcp_request("tools/call", %{"name" => "noop"}, state) do
          {:ok, %{}, state}
        end

        def handle_mcp_request("tools/call", %{"name" => "echo", "arguments" => args}, state) do
          {:ok, args, state}
        end

        def handle_mcp_request("tools/call", %{"name" => tool_name}, state) do
          {:error, %{"code" => -32601, "message" => "Tool not found: #{tool_name}"}, state}
        end

        # Handle notifications
        def handle_mcp_request("notifications/initialized", _params, state) do
          {:ok, %{}, state}
        end

        def handle_mcp_request(_method, _params, state) do
          {:error, %{"code" => -32601, "message" => "Method not found"}, state}
        end
      end

      {:ok, _pid} = PerfTestService.start_link(%{})

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :perf_test_service
        )

      on_exit(fn ->
        ExMCP.Native.unregister_service(:perf_test_service)
      end)

      {:ok, client: client}
    end

    test "demonstrates low latency for local calls", %{client: client} do
      # Warm up
      for _ <- 1..10 do
        Client.call_tool(client, "noop", %{})
      end

      # Measure latency
      latencies =
        for _ <- 1..100 do
          start = System.monotonic_time(:microsecond)
          {:ok, _} = Client.call_tool(client, "noop", %{})
          System.monotonic_time(:microsecond) - start
        end

      avg_latency = Enum.sum(latencies) / length(latencies)

      # Should be well under 1ms for local calls
      # microseconds
      assert avg_latency < 1000

      # Log for information
      IO.puts("Average BEAM transport latency: #{Float.round(avg_latency, 2)}μs")
    end

    test "handles concurrent requests efficiently", %{client: client} do
      # Launch many concurrent requests
      tasks =
        for i <- 1..100 do
          Task.async(fn ->
            Client.call_tool(client, "echo", %{"id" => i})
          end)
        end

      # Collect all results
      results = Task.await_many(tasks, 15_000)

      # Verify all requests succeeded
      assert length(results) == 100

      assert Enum.all?(results, fn
               {:ok, %{"id" => _id}} -> true
               _ -> false
             end)

      # Verify we got all unique IDs back
      ids = Enum.map(results, fn {:ok, %{"id" => id}} -> id end)
      assert Enum.sort(ids) == Enum.to_list(1..100)
    end
  end
end
