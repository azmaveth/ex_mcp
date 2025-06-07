defmodule ExMCP.Transport.BeamHotReloadTest do
  @moduledoc """
  Comprehensive tests for BEAM transport hot code reloading support.

  Tests hot reloading capabilities for MCP servers:
  - Handler module reloading
  - State migration during reloads
  - Connection preservation
  - Graceful error handling
  """
  # Hot reloading tests can't be async
  use ExUnit.Case, async: false

  alias ExMCP.{Client, Server}
  alias ExMCP.Transport.Beam.{HotReload, ReloadManager}

  describe "handler module reloading" do
    test "detects when handler module code changes" do
      # Create a test handler module
      handler_module = create_test_handler("TestHandler1")

      # Start reload manager watching the handler
      {:ok, manager} =
        ReloadManager.start_link(%{
          handler_module: handler_module,
          # Use test process as dummy server
          server: self(),
          watch_paths: [code_path_for_module(handler_module)],
          config: %{subscriber: self()},
          watch_strategy: :module_timestamp,
          poll_interval: 100,
          throttle_ms: 0
        })

      # Simulate code change by updating module
      _updated_handler = update_test_handler(handler_module, "v2")

      # Give the poller time to detect the change (poll interval is 100ms)
      Process.sleep(200)

      # Should detect the change
      assert_receive {:code_change_detected, ^handler_module}, 5000

      ReloadManager.stop(manager)
    end

    test "reloads handler module when code changes" do
      # Create initial handler and server
      handler_module = create_test_handler("TestHandler2")
      {:ok, server} = start_test_server(handler_module)

      # Enable hot reloading
      {:ok, _manager} =
        enable_hot_reload_with_subscriber(server, %{
          auto_reload: true,
          state_migration: :automatic
        })

      # Verify initial behavior
      {:ok, response1} = call_test_tool(server, "version")
      assert response1["content"] == [%{"type" => "text", "text" => "Version: v1"}]

      # Update handler code
      updated_handler = update_test_handler(handler_module, "v2")

      # Wait for reload to complete
      assert_receive {:handler_reloaded, ^handler_module}, 5000

      # Verify updated behavior
      {:ok, response2} = call_test_tool(server, "version")
      assert response2["content"] == [%{"type" => "text", "text" => "Version: v2"}]

      # Cleanup
      Server.stop(server)
    end

    test "preserves server connections during reload" do
      handler_module = create_test_handler("TestHandler3")
      {:ok, server} = start_test_server(handler_module)

      # Connect multiple clients
      {:ok, client1} = connect_test_client(server)
      {:ok, client2} = connect_test_client(server)

      # Enable hot reloading
      {:ok, _manager} =
        enable_hot_reload_with_subscriber(server, %{
          preserve_connections: true
        })

      # Update handler
      updated_handler = update_test_handler(handler_module, "v2")

      # Wait for reload
      assert_receive {:handler_reloaded, ^handler_module}, 5000

      # Both clients should still be connected and functional
      {:ok, _response1} = Client.call_tool(client1, "version", %{})
      {:ok, _response2} = Client.call_tool(client2, "version", %{})

      # Cleanup
      Client.disconnect(client1)
      Client.disconnect(client2)
      Server.stop(server)
    end
  end

  describe "state migration" do
    test "migrates handler state during reload" do
      handler_module = create_test_handler("StatefulHandler")
      {:ok, server} = start_test_server(handler_module)

      # Set some state
      # Should increment counter to 1
      {:ok, _} = call_test_tool(server, "increment")
      {:ok, response1} = call_test_tool(server, "get_count")
      assert response1["content"] == [%{"type" => "text", "text" => "Count: 1"}]

      # Enable hot reloading with state migration
      {:ok, _manager} =
        enable_hot_reload_with_subscriber(server, %{
          state_migration: :callback,
          migration_callback: &migrate_counter_state/1
        })

      # Update handler (with different version but same state structure)
      updated_handler = update_test_handler(handler_module, "v2", "Counter v2")

      # Wait for reload
      assert_receive {:handler_reloaded, ^handler_module}, 5000

      # State should be preserved and migrated
      {:ok, response2} = call_test_tool(server, "get_count")
      assert response2["content"] == [%{"type" => "text", "text" => "Count: 1 (Counter v2)"}]

      Server.stop(server)
    end

    test "handles state migration failures gracefully" do
      handler_module = create_test_handler("FailingHandler")
      {:ok, server} = start_test_server(handler_module)

      # Enable hot reloading with failing migration
      {:ok, _manager} =
        enable_hot_reload_with_subscriber(server, %{
          state_migration: :callback,
          migration_callback: fn _state -> {:error, :migration_failed} end
        })

      # Update handler
      updated_handler = update_test_handler(handler_module, "v2")

      # Should receive migration failure notification
      assert_receive {:migration_failed, :migration_failed}, 5000

      # Server should still be functional with old state
      {:ok, response} = call_test_tool(server, "version")
      assert response["content"] == [%{"type" => "text", "text" => "Version: v1"}]

      Server.stop(server)
    end
  end

  describe "reload safety and error handling" do
    test "validates new handler module before reloading" do
      handler_module = create_test_handler("ValidatedHandler")
      {:ok, server} = start_test_server(handler_module)

      {:ok, _manager} =
        enable_hot_reload_with_subscriber(server, %{
          validation: :strict
        })

      # Create invalid handler (missing required callbacks)
      invalid_handler = create_invalid_handler("InvalidHandler")

      # Should detect validation failure
      assert_receive {:validation_failed, reasons}, 5000
      assert Enum.any?(reasons, &String.contains?(&1, "missing callback"))

      # Original handler should still be active
      {:ok, response} = call_test_tool(server, "version")
      assert response["content"] == [%{"type" => "text", "text" => "Version: v1"}]

      Server.stop(server)
    end

    test "supports rollback on reload failure" do
      handler_module = create_test_handler("RollbackHandler")
      {:ok, server} = start_test_server(handler_module)

      {:ok, _manager} =
        enable_hot_reload_with_subscriber(server, %{
          rollback_on_failure: true
        })

      # Update to a handler that will crash during init
      crashing_handler = create_crashing_handler("CrashingHandler")

      # Should detect failure and rollback
      assert_receive {:reload_failed, _reason}, 5000
      assert_receive {:rollback_completed, ^handler_module}, 5000

      # Original handler should be restored
      {:ok, response} = call_test_tool(server, "version")
      assert response["content"] == [%{"type" => "text", "text" => "Version: v1"}]

      Server.stop(server)
    end
  end

  describe "reload manager configuration" do
    test "supports different watch strategies" do
      handler_module = create_test_handler("WatchedHandler")

      # File system watching
      {:ok, fs_manager} =
        ReloadManager.start_link(%{
          handler_module: handler_module,
          server: self(),
          watch_strategy: :filesystem,
          watch_paths: [code_path_for_module(handler_module)],
          config: %{subscriber: self()},
          poll_interval: 1000,
          throttle_ms: 0
        })

      # Polling-based watching
      {:ok, poll_manager} =
        ReloadManager.start_link(%{
          handler_module: handler_module,
          server: self(),
          watch_strategy: :polling,
          poll_interval: 1000,
          config: %{subscriber: self()},
          throttle_ms: 0
        })

      # Module timestamp watching
      {:ok, timestamp_manager} =
        ReloadManager.start_link(%{
          handler_module: handler_module,
          server: self(),
          watch_strategy: :module_timestamp,
          config: %{subscriber: self()},
          poll_interval: 2000,
          throttle_ms: 0
        })

      # All should be running
      assert Process.alive?(fs_manager)
      assert Process.alive?(poll_manager)
      assert Process.alive?(timestamp_manager)

      # Cleanup
      ReloadManager.stop(fs_manager)
      ReloadManager.stop(poll_manager)
      ReloadManager.stop(timestamp_manager)
    end

    test "supports reload throttling" do
      handler_module = create_test_handler("ThrottledHandler")
      {:ok, server} = start_test_server(handler_module)

      {:ok, _manager} =
        enable_hot_reload_with_subscriber(server, %{
          # 2 second throttle
          throttle_ms: 2000
        })

      start_time = System.system_time(:millisecond)

      # Trigger multiple rapid updates
      for i <- 1..3 do
        update_test_handler(handler_module, "v#{i}")
        Process.sleep(100)
      end

      # Should only reload once due to throttling
      assert_receive {:handler_reloaded, ^handler_module}, 5000
      refute_receive {:handler_reloaded, ^handler_module}, 1000

      elapsed = System.system_time(:millisecond) - start_time
      # Should respect throttle time
      assert elapsed >= 2000

      Server.stop(server)
    end
  end

  describe "integration with BEAM transport" do
    test "reloads work with BEAM transport connections" do
      handler_module = create_test_handler("BeamHandler")

      # Start server with BEAM transport
      {:ok, server} =
        Server.start_link(
          handler: handler_module,
          transport: :beam,
          name: :beam_hot_reload_server
        )

      # Connect via BEAM transport
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :beam_hot_reload_server
        )

      # Enable hot reloading
      {:ok, _manager} =
        enable_hot_reload_with_subscriber(server, %{
          transport_aware: true
        })

      # Update handler
      updated_handler = update_test_handler(handler_module, "v2")

      # Wait for reload
      assert_receive {:handler_reloaded, ^handler_module}, 5000

      # BEAM transport should still work
      {:ok, response} = Client.call_tool(client, "version", %{})
      assert response["content"] == [%{"type" => "text", "text" => "Version: v2"}]

      # Cleanup
      Client.disconnect(client)
      Server.stop(server)
    end
  end

  # Helper functions for creating test handlers and modules

  defp enable_hot_reload_with_subscriber(server, config \\ %{}) do
    # Add the test process as a subscriber
    config_with_subscriber = Map.put(config, :subscriber, self())
    enable_hot_reload_with_subscriber(server, config_with_subscriber)
  end

  defp create_test_handler(name, version \\ "v1", description \\ "Test Handler") do
    module_name = String.to_atom("Elixir.#{name}")

    # Dynamically create a test handler module
    code = """
    defmodule #{name} do
      use ExMCP.Server.Handler

      @impl true
      def init(_args) do
        {:ok, %{counter: 0, version: "#{version}", description: "#{description}"}}
      end

      @impl true
      def handle_initialize(_params, state) do
        {:ok, %{
          name: "#{name}",
          version: "1.0.0",
          capabilities: %{tools: %{}}
        }, state}
      end

      @impl true
      def handle_list_tools(state) do
        tools = [
          %{name: "version", description: "Get version"},
          %{name: "increment", description: "Increment counter"},
          %{name: "get_count", description: "Get current count"}
        ]
        {:ok, tools, state}
      end

      @impl true
      def handle_call_tool("version", _params, state) do
        text = if state.description == "Test Handler" do
          "Version: \#{state.version}"
        else
          "Version: \#{state.version} (\#{state.description})"
        end
        {:ok, [%{type: "text", text: text}], state}
      end

      def handle_call_tool("increment", _params, state) do
        new_state = %{state | counter: state.counter + 1}
        {:ok, [%{type: "text", text: "Incremented"}], new_state}
      end

      def handle_call_tool("get_count", _params, state) do
        text = if state.description == "Test Handler" do
          "Count: \#{state.counter}"
        else
          "Count: \#{state.counter} (\#{state.description})"
        end
        {:ok, [%{type: "text", text: text}], state}
      end
    end
    """

    # Compile and load the module
    [{^module_name, _}] = Code.compile_string(code)
    module_name
  end

  defp update_test_handler(module_name, new_version, new_description \\ "Test Handler") do
    # Get the module name without Elixir prefix
    simple_name = module_name |> Atom.to_string() |> String.replace("Elixir.", "")

    # Force purge the old module to ensure timestamp changes
    :code.purge(module_name)
    :code.delete(module_name)

    # Small delay to ensure timestamp difference
    Process.sleep(10)

    # Create updated version
    create_test_handler(simple_name, new_version, new_description)
  end

  defp create_invalid_handler(name) do
    module_name = String.to_atom("Elixir.#{name}")

    code = """
    defmodule #{name} do
      # Missing use ExMCP.Server.Handler

      def some_function do
        :ok
      end
    end
    """

    [{^module_name, _}] = Code.compile_string(code)
    module_name
  end

  defp create_crashing_handler(name) do
    module_name = String.to_atom("Elixir.#{name}")

    code = """
    defmodule #{name} do
      use ExMCP.Server.Handler

      @impl true
      def init(_args) do
        raise "Intentional crash during init"
      end

      @impl true
      def handle_initialize(_params, state) do
        {:ok, %{name: "#{name}", version: "1.0.0"}, state}
      end

      @impl true
      def handle_list_tools(state) do
        {:ok, [], state}
      end
    end
    """

    [{^module_name, _}] = Code.compile_string(code)
    module_name
  end

  defp start_test_server(handler_module) do
    Server.start_link(
      handler: handler_module,
      transport: :beam,
      name: :"test_server_#{:rand.uniform(10000)}"
    )
  end

  defp connect_test_client(server_name) when is_atom(server_name) do
    Client.start_link(
      transport: :beam,
      server: server_name
    )
  end

  defp connect_test_client(server_pid) when is_pid(server_pid) do
    # Get the registered name or use PID directly
    Client.start_link(
      transport: :beam,
      server: server_pid
    )
  end

  defp call_test_tool(server, tool_name, params \\ %{}) do
    # For now, simulate tool calls directly via server
    # In a real implementation, this would go through a proper client
    case GenServer.call(server, {:call_tool, tool_name, params}) do
      {:ok, result} -> {:ok, %{"content" => result}}
      error -> error
    end
  end

  defp code_path_for_module(module_name) do
    # In a real implementation, this would return the actual file path
    # For tests, we use a mock path
    "/tmp/test_modules/#{module_name}.ex"
  end

  defp migrate_counter_state(old_state) do
    # Example state migration - preserve counter but update metadata
    {:ok, %{old_state | version: "v2", description: "Counter v2"}}
  end
end
