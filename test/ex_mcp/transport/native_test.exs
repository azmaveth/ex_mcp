defmodule ExMCP.Transport.NativeTest do
  use ExUnit.Case, async: true

  alias ExMCP.Transport.Native

  # Test service for the native transport
  defmodule TestService do
    use GenServer

    def start_link(name) do
      GenServer.start_link(__MODULE__, name, name: name)
    end

    def init(name) do
      # Register with the native transport
      Native.register_service(name)
      {:ok, %{name: name, call_count: 0}}
    end

    def handle_call({:mcp_request, %{"method" => "ping", "params" => params}}, _from, state) do
      response = %{"pong" => params, "service" => state.name, "calls" => state.call_count}
      {:reply, {:ok, response}, %{state | call_count: state.call_count + 1}}
    end

    def handle_call({:mcp_request, %{"method" => "list_tools"}}, _from, state) do
      tools = [
        %{
          "name" => "ping",
          "description" => "Ping the service",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "message" => %{"type" => "string"}
            }
          }
        }
      ]

      {:reply, {:ok, %{"tools" => tools}}, state}
    end

    def handle_call({:mcp_request, %{"method" => "error"}}, _from, state) do
      {:reply, {:error, %{"code" => -32001, "message" => "Test error"}}, state}
    end

    def handle_call({:mcp_request, %{"method" => "slow"}}, _from, state) do
      # Sleep for 100ms to test timeout
      Process.sleep(100)
      {:reply, {:ok, %{"slow" => "response"}}, state}
    end

    def handle_call(:get_state, _from, state) do
      {:reply, state, state}
    end

    def handle_cast({:mcp_notification, %{"method" => "notify_test", "params" => params}}, state) do
      # Just store the notification in state for testing
      new_state = Map.put(state, :last_notification, params)
      {:noreply, new_state}
    end

    def get_state(pid) do
      GenServer.call(pid, :get_state)
    end
  end

  setup do
    # Start the registry if not already started
    case Registry.start_link(keys: :unique, name: ExMCP.Registry) do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end

    # Start a test service
    {:ok, service_pid} = TestService.start_link(:test_service)

    on_exit(fn ->
      if Process.alive?(service_pid) do
        GenServer.stop(service_pid)
      end
    end)

    %{service_pid: service_pid}
  end

  describe "service registration" do
    test "registers and unregisters services" do
      # Register a new service
      assert :ok = Native.register_service(:temp_service)

      # Check it's in the service list
      services = Native.list_services()
      assert Enum.any?(services, fn {name, _pid, _meta} -> name == :temp_service end)

      # Check service availability
      assert Native.service_available?(:temp_service)

      # Unregister
      Native.unregister_service(:temp_service)

      # Should no longer be available
      refute Native.service_available?(:temp_service)
    end

    test "prevents duplicate registration" do
      assert :ok = Native.register_service(:dup_test)
      assert {:error, :already_registered} = Native.register_service(:dup_test)

      Native.unregister_service(:dup_test)
    end
  end

  describe "service calls" do
    test "calls service methods successfully", %{service_pid: _service_pid} do
      # Test successful call
      assert {:ok, result} = Native.call(:test_service, "ping", %{"test" => "data"})
      assert result["pong"]["test"] == "data"
      assert result["service"] == :test_service
      assert result["calls"] == 0

      # Call again to test state persistence
      assert {:ok, result2} = Native.call(:test_service, "ping", %{"second" => "call"})
      assert result2["calls"] == 1
    end

    test "handles service errors" do
      assert {:error, error} = Native.call(:test_service, "error", %{})
      assert error["code"] == -32001
      assert error["message"] == "Test error"
    end

    test "handles non-existent services" do
      assert {:error, error} = Native.call(:nonexistent, "ping", %{})
      assert error["code"] == -32601
      assert error["message"] =~ "Service not found"
    end

    test "handles service timeouts" do
      # Test with a slow service and short timeout
      assert {:error, error} = Native.call(:test_service, "slow", %{}, timeout: 10)
      assert error["code"] == -32603
      assert error["message"] == "Service timeout"
    end

    test "calls with progress token and metadata" do
      opts = [
        progress_token: "progress-123",
        meta: %{"trace_id" => "abc", "user_id" => "user1"}
      ]

      # The test service doesn't explicitly handle progress/meta, but the call should succeed
      assert {:ok, _result} = Native.call(:test_service, "ping", %{"test" => "meta"}, opts)
    end
  end

  describe "notifications" do
    test "sends notifications to services", %{service_pid: service_pid} do
      # Send notification
      assert :ok = Native.notify(:test_service, "notify_test", %{"event" => "test_event"})

      # Give the cast time to process
      Process.sleep(10)

      # Check that the service received the notification
      state = TestService.get_state(service_pid)
      assert state.last_notification["event"] == "test_event"
    end

    test "handles notifications to non-existent services" do
      assert {:error, _} = Native.notify(:nonexistent, "notify_test", %{})
    end
  end

  describe "transport behaviour" do
    test "implements connect/1" do
      assert {:ok, state} = Native.connect([])
      assert is_map(state)
    end

    test "implements connected?/1" do
      {:ok, state} = Native.connect([])
      assert Native.connected?(state) == true
    end

    test "implements close/1" do
      {:ok, state} = Native.connect([])
      assert Native.close(state) == :ok
    end

    test "implements send_message/2 with JSON compatibility" do
      {:ok, state} = Native.connect(service_name: :test_service)

      # Test JSON message format (for compatibility with Client API)
      json_message =
        Jason.encode!(%{
          "id" => "123",
          "method" => "list_tools",
          "params" => %{}
        })

      assert {:ok, response_json, _new_state} = Native.send_message(json_message, state)

      # Parse response
      assert {:ok, response} = Jason.decode(response_json)
      assert response["id"] == "123"
      assert is_list(response["result"]["tools"])
    end
  end

  describe "service discovery" do
    test "lists all registered services" do
      # Register additional test services
      Native.register_service(:service_1)
      Native.register_service(:service_2)

      services = Native.list_services()

      # Should include our test services
      service_names = Enum.map(services, fn {name, _pid, _meta} -> name end)
      assert :test_service in service_names
      assert :service_1 in service_names
      assert :service_2 in service_names

      # Cleanup
      Native.unregister_service(:service_1)
      Native.unregister_service(:service_2)
    end

    test "service_available?/1 works correctly" do
      assert Native.service_available?(:test_service)
      refute Native.service_available?(:nonexistent_service)
    end
  end
end
