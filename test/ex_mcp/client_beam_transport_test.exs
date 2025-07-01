defmodule ExMCP.ClientBeamTransportTest do
  use ExUnit.Case, async: false

  alias ExMCP.Client
  alias ExMCP.Server.BeamServer

  describe "Client with BEAM transport" do
    setup do
      # Start application for this test suite
      {:ok, _} = Application.ensure_all_started(:ex_mcp)

      on_exit(fn ->
        Application.stop(:ex_mcp)
      end)

      # Clean up any existing service first
      ExMCP.Native.unregister_service(:beam_transport_calculator_service)

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
                # Simulate slow operation
                Process.sleep(100)
                %{"result" => "completed"}

              _ ->
                {:error, %{"code" => -32601, "message" => "Tool not found: #{tool_name}"}}
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

      # Start the service with a map as initial state
      {:ok, service_pid} = TestCalculatorService.start_link(%{notifications: []})

      # Start a BEAM server that will handle the MCP protocol
      {:ok, server_pid} =
        BeamServer.start_link(
          handler: TestCalculatorService,
          handler_state: %{notifications: []},
          transport: [mode: :beam]
        )

      on_exit(fn ->
        ExMCP.Native.unregister_service(:beam_transport_calculator_service)
      end)

      {:ok, service_pid: service_pid, server_pid: server_pid}
    end

    test "connects to BEAM service via native transport", %{server_pid: server_pid} do
      # Start client with BEAM transport connecting to the server
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: server_pid
        )

      # Verify connection
      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :ready
    end

    test "calls tools via BEAM transport", %{server_pid: server_pid} do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: server_pid
        )

      # Test successful calls
      assert {:ok, %{"result" => 5}} =
               Client.call_tool(client, "add", %{"a" => 2, "b" => 3}, format: :map)

      assert {:ok, %{"result" => 10}} =
               Client.call_tool(client, "subtract", %{"a" => 15, "b" => 5}, format: :map)

      assert {:ok, %{"result" => 2.5}} =
               Client.call_tool(client, "divide", %{"a" => 5, "b" => 2}, format: :map)
    end

    test "handles errors via BEAM transport", %{server_pid: server_pid} do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: server_pid
        )

      # Test error responses
      assert {:error, %{"code" => -32602, "message" => "Division by zero"}} =
               Client.call_tool(client, "divide", %{"a" => 10, "b" => 0}, format: :map)

      assert {:error, %{"code" => -32601, "message" => "Tool not found: unknown_method"}} =
               Client.call_tool(client, "unknown_method", %{}, format: :map)
    end

    test "handles notifications via BEAM transport", %{
      service_pid: service_pid,
      server_pid: server_pid
    } do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: server_pid
        )

      # Send notifications
      :ok = Client.notify(client, "log", %{"message" => "Test log 1"})
      :ok = Client.notify(client, "log", %{"message" => "Test log 2"})

      # Give notifications time to process
      Process.sleep(50)

      # Verify notifications were received
      # Call the server pid which will forward to the service
      notifications = GenServer.call(server_pid, :get_notifications)
      assert notifications == ["Test log 1", "Test log 2"]
    end

    test "respects timeout settings", %{server_pid: server_pid} do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: server_pid,
          # 50ms timeout
          timeout: 50
        )

      # This should timeout since slow_operation takes 100ms
      assert {:error, :timeout} = Client.call_tool(client, "slow_operation", %{}, format: :map)
    end

    test "handles service not found" do
      # Try to connect to non-existent service
      # The client process will exit if connection fails
      Process.flag(:trap_exit, true)

      result =
        Client.start_link(
          transport: :beam,
          service_name: :nonexistent_service
        )

      # The error can be either not_supported or connection_failed
      assert match?({:error, _}, result)
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
      # Clean up any existing service first
      ExMCP.Native.unregister_service(:native_alias_echo_service)

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

        def handle_mcp_request("tools/list", _params, state) do
          tools = [
            %{
              "name" => "echo",
              "description" => "Echo back the input",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{
                  "message" => %{"type" => "string"}
                },
                "required" => ["message"]
              }
            }
          ]

          {:ok, %{"tools" => tools}, state}
        end

        def handle_mcp_request(
              "tools/call",
              %{"name" => "echo", "arguments" => %{"message" => msg}},
              state
            ) do
          {:ok, %{"echoed" => msg}, state}
        end

        def handle_mcp_request(_method, _params, state) do
          {:error, %{"code" => -32601, "message" => "Method not found"}, state}
        end
      end

      # Start the service
      {:ok, service_pid} = TestEchoService.start_link(%{})

      # Start a BEAM server with native mode
      {:ok, server_pid} =
        BeamServer.start_link(
          handler: TestEchoService,
          handler_state: %{},
          transport: [mode: :native]
        )

      on_exit(fn ->
        ExMCP.Native.unregister_service(:native_alias_echo_service)
      end)

      {:ok, service_pid: service_pid, server_pid: server_pid}
    end

    test "native transport is an alias for beam transport with raw terms", %{
      server_pid: server_pid
    } do
      # :native should behave exactly like :beam but with raw terms capability
      {:ok, client} =
        Client.start_link(
          transport: :native,
          server: server_pid
        )

      # Verify connection
      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :ready

      # Test tool call
      assert {:ok, %{"echoed" => "Hello, BEAM!"}} =
               Client.call_tool(client, "echo", %{"message" => "Hello, BEAM!"}, format: :map)
    end
  end

  describe "Client with batch operations over BEAM transport" do
    setup do
      # Start application for this test suite
      {:ok, _} = Application.ensure_all_started(:ex_mcp)

      on_exit(fn ->
        Application.stop(:ex_mcp)
      end)

      # Clean up any existing service first
      ExMCP.Native.unregister_service(:beam_batch_service)

      # Use the same calculator service
      defmodule TestBatchService do
        use ExMCP.Service, name: :beam_batch_service

        @impl true
        def handle_mcp_request("initialize", _params, state) do
          {:ok,
           %{
             "protocolVersion" => "2025-03-26",
             "capabilities" => %{},
             "serverInfo" => %{
               "name" => "TestBatchService",
               "version" => "1.0.0"
             }
           }, state}
        end

        def handle_mcp_request("tools/list", _params, state) do
          tools = [
            %{
              "name" => "multiply",
              "description" => "Multiply two numbers",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{
                  "a" => %{"type" => "number"},
                  "b" => %{"type" => "number"}
                },
                "required" => ["a", "b"]
              }
            }
          ]

          {:ok, %{"tools" => tools}, state}
        end

        def handle_mcp_request(
              "tools/call",
              %{"name" => "multiply", "arguments" => %{"a" => a, "b" => b}},
              state
            ) do
          {:ok, %{"result" => a * b}, state}
        end

        def handle_mcp_request(_method, _params, state) do
          {:error, %{"code" => -32601, "message" => "Method not found"}, state}
        end
      end

      # Start the service
      {:ok, service_pid} = TestBatchService.start_link(%{})

      # Start a BEAM server
      {:ok, server_pid} =
        BeamServer.start_link(
          handler: TestBatchService,
          handler_state: %{},
          transport: [mode: :beam]
        )

      on_exit(fn ->
        ExMCP.Native.unregister_service(:beam_batch_service)
      end)

      {:ok, service_pid: service_pid, server_pid: server_pid}
    end

    test "handles batch requests via BEAM transport", %{server_pid: server_pid} do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: server_pid
        )

      # Prepare batch requests
      requests = [
        {"tools/call", %{"name" => "multiply", "arguments" => %{"a" => 2, "b" => 3}}},
        {"tools/call", %{"name" => "multiply", "arguments" => %{"a" => 4, "b" => 5}}},
        {"tools/call", %{"name" => "multiply", "arguments" => %{"a" => 6, "b" => 7}}}
      ]

      # Send batch request
      {:ok, results} = Client.batch_request(client, requests)

      # Verify results
      assert length(results) == 3
      assert {:ok, %{"result" => 6}} = Enum.at(results, 0)
      assert {:ok, %{"result" => 20}} = Enum.at(results, 1)
      assert {:ok, %{"result" => 42}} = Enum.at(results, 2)
    end
  end

  describe "Client with concurrent operations over BEAM transport" do
    setup do
      # Start application for this test suite
      {:ok, _} = Application.ensure_all_started(:ex_mcp)

      on_exit(fn ->
        Application.stop(:ex_mcp)
      end)

      # Service for concurrent testing
      # Clean up any existing service first
      ExMCP.Native.unregister_service(:beam_concurrent_service)

      defmodule TestConcurrentService do
        use ExMCP.Service, name: :beam_concurrent_service

        @impl true
        def handle_mcp_request("initialize", _params, state) do
          {:ok,
           %{
             "protocolVersion" => "2025-03-26",
             "capabilities" => %{},
             "serverInfo" => %{
               "name" => "TestConcurrentService",
               "version" => "1.0.0"
             }
           }, state}
        end

        def handle_mcp_request("tools/list", _params, state) do
          tools = [
            %{
              "name" => "counter",
              "description" => "Increment and return counter",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{}
              }
            }
          ]

          {:ok, %{"tools" => tools}, state}
        end

        def handle_mcp_request("tools/call", %{"name" => "counter"}, state) do
          # Thread-safe counter using state
          counter = Map.get(state, :counter, 0) + 1
          new_state = Map.put(state, :counter, counter)
          {:ok, %{"count" => counter}, new_state}
        end

        def handle_mcp_request(_method, _params, state) do
          {:error, %{"code" => -32601, "message" => "Method not found"}, state}
        end
      end

      # Start the service
      {:ok, service_pid} = TestConcurrentService.start_link(%{})

      # Start a BEAM server
      {:ok, server_pid} =
        BeamServer.start_link(
          handler: TestConcurrentService,
          handler_state: %{},
          transport: [mode: :beam]
        )

      on_exit(fn ->
        ExMCP.Native.unregister_service(:beam_concurrent_service)
      end)

      {:ok, service_pid: service_pid, server_pid: server_pid}
    end

    test "handles concurrent requests via BEAM transport", %{server_pid: server_pid} do
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: server_pid
        )

      # Spawn 100 concurrent requests
      tasks =
        for _ <- 1..100 do
          Task.async(fn ->
            Client.call_tool(client, "counter", %{}, format: :map)
          end)
        end

      # Collect all results with explicit timeout
      results = Task.await_many(tasks, 15_000)

      # All should succeed
      assert Enum.all?(results, fn
               {:ok, %{"count" => _count}} -> true
               _ -> false
             end)

      # Extract counts
      counts =
        results
        |> Enum.map(fn {:ok, %{"count" => count}} -> count end)
        |> Enum.sort()

      # Should have sequential counts from 1 to 100 (though order may vary)
      assert length(Enum.uniq(counts)) == 100
    end
  end
end
