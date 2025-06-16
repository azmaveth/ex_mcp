defmodule ExMCP.NativeTest do
  use ExUnit.Case, async: true

  alias ExMCP.Native

  describe "service registration" do
    test "registers and unregisters services" do
      # {:rand.uniform(10000)}
      service_name = :test_service_

      # Register service
      assert :ok = Native.register_service(service_name)

      # Check it's available
      assert Native.service_available?(service_name)

      # Should appear in service list
      services = Native.list_services()
      assert Enum.any?(services, fn {name, _pid, _meta} -> name == service_name end)

      # Unregister service
      assert :ok = Native.unregister_service(service_name)

      # Should no longer be available
      refute Native.service_available?(service_name)
    end

    test "handles duplicate registration gracefully" do
      # {:rand.uniform(10000)}
      service_name = :test_duplicate_

      # Register once
      assert :ok = Native.register_service(service_name)

      # Register again - should not fail
      assert :ok = Native.register_service(service_name)

      # Clean up
      Native.unregister_service(service_name)
    end
  end

  describe "service communication" do
    test "calls service successfully" do
      # Create a test service
      defmodule TestService do
        use GenServer

        def start_link(service_name) do
          GenServer.start_link(__MODULE__, service_name)
        end

        def init(service_name) do
          Native.register_service(service_name)
          {:ok, %{}}
        end

        def handle_call({:mcp_request, %{"method" => "ping", "params" => params}}, _from, state) do
          {:reply, {:ok, %{"message" => "pong", "echo" => params}}, state}
        end

        def handle_call({:mcp_request, %{"method" => "error"}}, _from, state) do
          {:reply, {:error, "test error"}, state}
        end
      end

      # {:rand.uniform(10000)}
      service_name = :test_ping_

      # Start the test service
      {:ok, _pid} = TestService.start_link(service_name)

      # Call the service
      assert {:ok, result} = Native.call(service_name, "ping", %{"test" => "data"})
      assert result["message"] == "pong"
      assert result["echo"]["test"] == "data"

      # Test error response
      assert {:error, "test error"} = Native.call(service_name, "error", %{})

      # Test service not found
      assert {:error, {:service_not_found, :nonexistent}} =
               Native.call(:nonexistent, "ping", %{})
    end

    test "sends notifications" do
      defmodule NotificationService do
        use GenServer

        def start_link(service_name) do
          GenServer.start_link(__MODULE__, service_name)
        end

        def init(service_name) do
          Native.register_service(service_name)
          {:ok, %{notifications: []}}
        end

        def handle_cast({:mcp_notification, message}, state) do
          notifications = [message | state.notifications]
          {:noreply, %{state | notifications: notifications}}
        end

        def get_notifications(pid) do
          GenServer.call(pid, :get_notifications)
        end

        def handle_call(:get_notifications, _from, state) do
          {:reply, Enum.reverse(state.notifications), state}
        end
      end

      # {:rand.uniform(10000)}
      service_name = :test_notify_

      # Start the test service
      {:ok, pid} = NotificationService.start_link(service_name)

      # Send notification
      assert :ok = Native.notify(service_name, "resource_updated", %{"uri" => "test://file"})

      # Give it a moment to process
      Process.sleep(10)

      # Check notification was received
      notifications = NotificationService.get_notifications(pid)
      assert length(notifications) == 1
      assert hd(notifications)["method"] == "resource_updated"
      assert hd(notifications)["params"]["uri"] == "test://file"
    end

    test "handles call timeout" do
      defmodule SlowService do
        use GenServer

        def start_link(service_name) do
          GenServer.start_link(__MODULE__, service_name)
        end

        def init(service_name) do
          Native.register_service(service_name)
          {:ok, %{}}
        end

        def handle_call({:mcp_request, %{"method" => "slow"}}, _from, state) do
          Process.sleep(1000)
          {:reply, {:ok, "done"}, state}
        end
      end

      # {:rand.uniform(10000)}
      service_name = :test_slow_

      # Start the slow service
      {:ok, _pid} = SlowService.start_link(service_name)

      # Call with short timeout
      assert {:error, :timeout} = Native.call(service_name, "slow", %{}, timeout: 50)
    end
  end

  describe "ExMCP.Service macro" do
    test "creates a service with automatic registration" do
      defmodule MacroTestService do
        use ExMCP.Service, name: :macro_test_service

        @impl true
        def handle_mcp_request("hello", params, state) do
          {:ok, %{"greeting" => "Hello, #{params["name"] || "World"}!"}, state}
        end

        def handle_mcp_request(_method, _params, state) do
          {:error, %{"code" => -32601, "message" => "Method not found"}, state}
        end
      end

      # Start the service
      {:ok, _pid} = MacroTestService.start_link(%{})

      # Verify it's registered
      assert Native.service_available?(:macro_test_service)

      # Test MCP call
      assert {:ok, result} = Native.call(:macro_test_service, "hello", %{"name" => "Alice"})
      assert result["greeting"] == "Hello, Alice!"

      # Test unknown method
      assert {:error, error} = Native.call(:macro_test_service, "unknown", %{})
      assert error["code"] == -32601
    end
  end
end
