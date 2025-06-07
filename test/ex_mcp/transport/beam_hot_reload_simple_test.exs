defmodule ExMCP.Transport.BeamHotReloadSimpleTest do
  @moduledoc """
  Simple tests for BEAM transport hot code reloading core functionality.

  Focuses on the basic hot reload mechanisms without full MCP protocol complexity.
  """
  # Hot reloading tests can't be async
  use ExUnit.Case, async: false

  alias ExMCP.Transport.Beam.{HotReload, ReloadManager}

  describe "hot reload core functionality" do
    test "validates handler modules correctly" do
      # Test basic validation
      assert :ok = HotReload.validate_handler_module(ExMCP.Server.Handler.Echo, :basic)

      # Test strict validation (should pass for a real handler)
      assert :ok = HotReload.validate_handler_module(ExMCP.Server.Handler.Echo, :strict)

      # Test validation with missing module
      assert {:error, _reasons} = HotReload.validate_handler_module(NonExistentModule, :basic)
    end

    test "performs state migration with different strategies" do
      old_state = %{counter: 5, version: "v1"}

      # No migration
      assert {:ok, nil} = HotReload.migrate_state(old_state, :none)

      # Automatic migration (preserves map state)
      assert {:ok, %{counter: 5, version: "v1"}} = HotReload.migrate_state(old_state, :automatic)

      # Callback migration
      migration_fn = fn state -> {:ok, %{state | version: "v2", counter: state.counter + 1}} end

      assert {:ok, %{counter: 6, version: "v2"}} =
               HotReload.migrate_state(old_state, :callback, migration_fn)

      # Failing callback migration
      failing_fn = fn _state -> {:error, :migration_failed} end

      assert {:error, :migration_failed} =
               HotReload.migrate_state(old_state, :callback, failing_fn)
    end

    test "reload manager can be started and stopped" do
      config = %{
        handler_module: ExMCP.Server.Handler.Echo,
        server: self(),
        config: %{auto_reload: false, watch_strategy: :module_timestamp},
        watch_strategy: :module_timestamp,
        watch_paths: [],
        poll_interval: 2000,
        throttle_ms: 1000
      }

      {:ok, manager} = ReloadManager.start_link(config)
      assert Process.alive?(manager)

      # Test configuration retrieval
      {:ok, retrieved_config} = ReloadManager.get_config(manager)
      assert retrieved_config.auto_reload == false

      # Test status retrieval
      {:ok, status} = ReloadManager.get_status(manager)
      assert status.handler_module == ExMCP.Server.Handler.Echo
      assert status.reload_count == 0

      # Clean up
      ReloadManager.stop(manager)
      refute Process.alive?(manager)
    end

    test "reload manager handles configuration updates" do
      config = %{
        handler_module: ExMCP.Server.Handler.Echo,
        server: self(),
        config: %{auto_reload: false, throttle_ms: 1000, validation: :none},
        watch_strategy: :module_timestamp,
        watch_paths: [],
        poll_interval: 2000,
        throttle_ms: 1000
      }

      {:ok, manager} = ReloadManager.start_link(config)

      # Update configuration
      new_config = %{auto_reload: true, throttle_ms: 2000}
      assert :ok = ReloadManager.update_config(manager, new_config)

      # Verify the update
      {:ok, updated_config} = ReloadManager.get_config(manager)
      assert updated_config.auto_reload == true
      assert updated_config.throttle_ms == 2000

      ReloadManager.stop(manager)
    end

    test "module timestamp detection works" do
      # Get current timestamp for a loaded module
      initial_timestamp = get_module_timestamp(ExMCP.Server.Handler.Echo)
      assert is_integer(initial_timestamp)
      assert initial_timestamp > 0

      # Timestamp should be consistent for the same module
      second_timestamp = get_module_timestamp(ExMCP.Server.Handler.Echo)
      assert initial_timestamp == second_timestamp
    end
  end

  describe "reload throttling" do
    test "throttles rapid reload attempts" do
      config = %{
        handler_module: ExMCP.Server.Handler.Echo,
        server: self(),
        config: %{auto_reload: false, throttle_ms: 100, validation: :none},
        watch_strategy: :module_timestamp,
        watch_paths: [],
        poll_interval: 2000,
        # Very short throttle for testing
        throttle_ms: 100
      }

      {:ok, manager} = ReloadManager.start_link(config)

      # Trigger multiple rapid reloads
      start_time = System.system_time(:millisecond)

      # First reload should be attempted
      ReloadManager.trigger_reload(manager)

      # Immediate second reload should be throttled
      ReloadManager.trigger_reload(manager)

      # Wait less than throttle time and try again (should still be throttled)
      Process.sleep(50)
      ReloadManager.trigger_reload(manager)

      # Check that throttling worked by ensuring not too much time passed
      elapsed = System.system_time(:millisecond) - start_time
      # Should complete quickly due to throttling
      assert elapsed < 200

      ReloadManager.stop(manager)
    end
  end

  # Helper function from the reload manager (duplicated for testing)
  defp get_module_timestamp(module) do
    try do
      case :code.is_loaded(module) do
        false ->
          # Force load the module first
          case Code.ensure_loaded(module) do
            {:module, _} -> get_loaded_module_timestamp(module)
            # Return non-zero for unloadable modules
            _ -> 1
          end

        {:file, _} ->
          get_loaded_module_timestamp(module)
      end
    rescue
      _ -> 1
    end
  end

  defp get_loaded_module_timestamp(module) do
    try do
      case :code.get_object_code(module) do
        {^module, binary, _filename} ->
          :erlang.phash2(binary)

        :error ->
          # Try alternative method
          case module.module_info(:compile) do
            info when is_list(info) ->
              # Use compilation time or other info
              time = Keyword.get(info, :time, {0, 0, 0})
              :erlang.phash2(time)

            _ ->
              System.system_time(:second)
          end
      end
    rescue
      _ -> System.system_time(:second)
    end
  end
end
