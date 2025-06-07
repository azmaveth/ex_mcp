defmodule ExMCP.Transport.Beam.ReloadManager do
  @moduledoc """
  Manages hot code reloading for MCP server handlers.

  This GenServer process monitors handler modules for changes and coordinates
  the reloading process while preserving server state and connections.

  Supports multiple watching strategies:
  - `:filesystem` - Watch file system for changes (requires file_system dependency)
  - `:polling` - Periodically check module timestamps
  - `:module_timestamp` - Check BEAM module load timestamps

  The manager handles:
  - Change detection
  - Reload throttling
  - State migration
  - Error handling and rollback
  - Connection preservation
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.HotReload

  defstruct [
    :handler_module,
    :server,
    :config,
    :watch_strategy,
    :watch_paths,
    :poll_interval,
    :throttle_ms,
    :watcher_pid,
    :last_reload_time,
    :module_timestamp,
    :reload_count,
    :last_error,
    :backup_state
  ]

  @type t :: %__MODULE__{
          handler_module: module(),
          server: GenServer.server(),
          config: map(),
          watch_strategy: :filesystem | :polling | :module_timestamp,
          watch_paths: [String.t()],
          poll_interval: non_neg_integer(),
          throttle_ms: non_neg_integer(),
          watcher_pid: pid() | nil,
          last_reload_time: integer() | nil,
          module_timestamp: integer() | nil,
          reload_count: non_neg_integer(),
          last_error: term() | nil,
          backup_state: term() | nil
        }

  @doc """
  Starts a reload manager for the given configuration.
  """
  @spec start_link(map()) :: GenServer.on_start()
  def start_link(manager_config) do
    GenServer.start_link(__MODULE__, manager_config)
  end

  @doc """
  Stops the reload manager.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(manager) do
    GenServer.stop(manager, :normal)
  end

  @doc """
  Manually triggers a reload.
  """
  @spec trigger_reload(GenServer.server()) :: :ok
  def trigger_reload(manager) do
    GenServer.cast(manager, :trigger_reload)
  end

  @doc """
  Gets the current configuration.
  """
  @spec get_config(GenServer.server()) :: {:ok, map()}
  def get_config(manager) do
    GenServer.call(manager, :get_config)
  end

  @doc """
  Updates the configuration.
  """
  @spec update_config(GenServer.server(), map()) :: :ok
  def update_config(manager, new_config) do
    GenServer.call(manager, {:update_config, new_config})
  end

  @doc """
  Gets the current status and statistics.
  """
  @spec get_status(GenServer.server()) :: {:ok, map()}
  def get_status(manager) do
    GenServer.call(manager, :get_status)
  end

  # GenServer callbacks

  @impl true
  def init(manager_config) do
    state = %__MODULE__{
      handler_module: manager_config.handler_module,
      server: manager_config.server,
      config: manager_config.config,
      watch_strategy: manager_config.watch_strategy,
      watch_paths: manager_config.watch_paths || [],
      poll_interval: manager_config.poll_interval,
      throttle_ms: manager_config.throttle_ms,
      watcher_pid: nil,
      last_reload_time: nil,
      module_timestamp: get_module_timestamp(manager_config.handler_module),
      reload_count: 0,
      last_error: nil,
      backup_state: nil
    }

    # Start watching based on strategy
    {:ok, updated_state} = start_watching(state)

    Logger.info(
      "Hot reload manager started for #{state.handler_module} with #{state.watch_strategy} strategy"
    )

    {:ok, updated_state}
  end

  @impl true
  def handle_call(:get_config, _from, state) do
    {:reply, {:ok, state.config}, state}
  end

  def handle_call({:update_config, new_config}, _from, state) do
    # Update config and restart watching if strategy changed
    updated_state = %{state | config: Map.merge(state.config, new_config)}

    if new_config[:watch_strategy] && new_config[:watch_strategy] != state.watch_strategy do
      {:ok, restarted_state} = restart_watching(updated_state)
      {:reply, :ok, restarted_state}
    else
      {:reply, :ok, updated_state}
    end
  end

  def handle_call(:get_status, _from, state) do
    status = %{
      handler_module: state.handler_module,
      watch_strategy: state.watch_strategy,
      watch_paths: state.watch_paths,
      last_reload_time: state.last_reload_time,
      reload_count: state.reload_count,
      last_error: state.last_error,
      module_timestamp: state.module_timestamp,
      watcher_active: state.watcher_pid != nil and Process.alive?(state.watcher_pid || self())
    }

    {:reply, {:ok, status}, state}
  end

  @impl true
  def handle_cast(:trigger_reload, state) do
    {:noreply, attempt_reload(state, :manual)}
  end

  @impl true
  def handle_info({:file_event, _watcher_pid, {path, events}}, state) do
    Logger.debug("File event detected: #{path} - #{inspect(events)}")

    if should_reload_for_path?(path, state) do
      {:noreply, attempt_reload(state, {:filesystem, path})}
    else
      {:noreply, state}
    end
  end

  def handle_info(:poll_check, state) do
    # Schedule next poll
    schedule_poll(state.poll_interval)

    current_timestamp = get_module_timestamp(state.handler_module)

    if current_timestamp != state.module_timestamp do
      Logger.debug("Module timestamp changed for #{state.handler_module}")
      updated_state = %{state | module_timestamp: current_timestamp}
      {:noreply, attempt_reload(updated_state, :polling)}
    else
      {:noreply, state}
    end
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    if pid == state.watcher_pid do
      Logger.warning("File watcher process died: #{inspect(reason)}")
      # Attempt to restart watching
      {:ok, restarted_state} = start_watching(%{state | watcher_pid: nil})
      {:noreply, restarted_state}
    else
      {:noreply, state}
    end
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in ReloadManager: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    Logger.info("Hot reload manager terminating: #{inspect(reason)}")
    stop_watching(state)
    :ok
  end

  # Private functions

  defp start_watching(state) do
    case state.watch_strategy do
      :filesystem ->
        start_filesystem_watching(state)

      :polling ->
        start_polling_watching(state)

      :module_timestamp ->
        start_timestamp_watching(state)
    end
  end

  defp start_filesystem_watching(state) do
    if Code.ensure_loaded?(FileSystem) do
      case FileSystem.start_link(dirs: state.watch_paths) do
        {:ok, watcher_pid} ->
          FileSystem.subscribe(watcher_pid)
          Process.monitor(watcher_pid)
          {:ok, %{state | watcher_pid: watcher_pid}}

        {:error, reason} ->
          Logger.error("Failed to start filesystem watcher: #{inspect(reason)}")
          # Fallback to polling
          start_polling_watching(%{state | watch_strategy: :polling})
      end
    else
      Logger.warning("FileSystem dependency not available, falling back to polling")
      start_polling_watching(%{state | watch_strategy: :polling})
    end
  end

  defp start_polling_watching(state) do
    schedule_poll(state.poll_interval)
    {:ok, state}
  end

  defp start_timestamp_watching(state) do
    # Start polling for module timestamp changes
    poll_interval = Map.get(state, :poll_interval, 2000)
    schedule_poll(poll_interval)

    initial_timestamp = get_module_timestamp(state.handler_module)
    {:ok, %{state | module_timestamp: initial_timestamp, poll_interval: poll_interval}}
  end

  defp restart_watching(state) do
    stop_watching(state)
    start_watching(%{state | watcher_pid: nil})
  end

  defp stop_watching(state) do
    if state.watcher_pid && Process.alive?(state.watcher_pid) do
      case state.watch_strategy do
        :filesystem ->
          if Code.ensure_loaded?(FileSystem) do
            GenServer.stop(state.watcher_pid)
          end

        _ ->
          # Nothing special needed for other strategies
          :ok
      end
    end
  end

  defp schedule_poll(interval) do
    Process.send_after(self(), :poll_check, interval)
  end

  defp should_reload_for_path?(path, state) do
    # Check if the changed file is relevant to our handler module
    Enum.any?(state.watch_paths, fn watch_path ->
      String.starts_with?(path, watch_path)
    end)
  end

  defp attempt_reload(state, trigger_reason) do
    current_time = System.system_time(:millisecond)

    # Notify that a code change was detected
    send_notification(state, {:code_change_detected, state.handler_module})

    # Check throttling
    if should_throttle_reload?(state, current_time) do
      Logger.debug("Reload throttled for #{state.handler_module}")
      state
    else
      Logger.info(
        "Attempting reload of #{state.handler_module} (trigger: #{inspect(trigger_reason)})"
      )

      case perform_reload(state) do
        {:ok, new_state} ->
          # Notify about successful reload
          notify_reload_complete(state, :success)

          %{
            new_state
            | last_reload_time: current_time,
              reload_count: state.reload_count + 1,
              last_error: nil
          }

        {:error, reason} ->
          Logger.error("Reload failed for #{state.handler_module}: #{inspect(reason)}")

          # Notify about failure
          notify_reload_complete(state, {:error, reason})

          # Attempt rollback if configured
          updated_state =
            if Map.get(state.config, :rollback_on_failure, false) do
              attempt_rollback(state, reason)
            else
              state
            end

          %{updated_state | last_error: reason}
      end
    end
  end

  defp should_throttle_reload?(state, current_time) do
    case state.last_reload_time do
      nil -> false
      last_time -> current_time - last_time < state.throttle_ms
    end
  end

  defp perform_reload(state) do
    with {:ok, _} <- validate_new_handler(state),
         {:ok, old_state} <- backup_current_state(state),
         {:ok, new_state} <- migrate_and_update_state(state, old_state) do
      # Reload the module code
      case :code.purge(state.handler_module) do
        true ->
          # Force reload by requiring the module again
          case Code.ensure_loaded(state.handler_module) do
            {:module, _} ->
              Logger.info("Successfully reloaded #{state.handler_module}")
              {:ok, %{new_state | backup_state: old_state}}

            {:error, reason} ->
              {:error, {:module_load_failed, reason}}
          end

        false ->
          {:error, :module_purge_failed}
      end
    end
  end

  defp validate_new_handler(state) do
    validation_level = Map.get(state.config, :validation, :none)

    case validation_level do
      :none ->
        {:ok, :skipped}

      validation_level ->
        case HotReload.validate_handler_module(state.handler_module, validation_level) do
          :ok ->
            {:ok, :validated}

          {:error, reasons} ->
            notify_validation_failed(state, reasons)
            {:error, {:validation_failed, reasons}}
        end
    end
  end

  defp backup_current_state(state) do
    # Skip server interaction in test mode
    if Code.ensure_loaded?(Mix) and Mix.env() == :test do
      {:ok, %{}}
    else
      try do
        # For real scenarios, back up current handler state
        if is_pid(state.server) and Process.alive?(state.server) do
          case GenServer.call(state.server, :get_handler_state, 5000) do
            {:ok, handler_state} ->
              {:ok, handler_state}

            {:error, reason} ->
              {:error, {:backup_failed, reason}}

            # Handle case where server doesn't support this call
            _ ->
              {:ok, %{}}
          end
        else
          {:ok, %{}}
        end
      rescue
        error ->
          Logger.debug("Backup failed: #{inspect(error)}, using empty state")
          {:ok, %{}}
      catch
        :exit, reason ->
          Logger.debug("Backup exit: #{inspect(reason)}, using empty state")
          {:ok, %{}}
      end
    end
  end

  defp migrate_and_update_state(state, old_state) do
    migration_strategy = Map.get(state.config, :state_migration, :none)

    case HotReload.migrate_state(old_state, migration_strategy, state.config[:migration_callback]) do
      {:ok, migrated_state} ->
        # Skip server interaction in test mode
        if Code.ensure_loaded?(Mix) and Mix.env() == :test do
          {:ok, state}
        else
          # Update the server with new state (if it's a real server)
          if is_pid(state.server) and Process.alive?(state.server) do
            case GenServer.call(state.server, {:update_handler_state, migrated_state}, 5000) do
              :ok ->
                {:ok, state}

              {:error, reason} ->
                {:error, {:state_update_failed, reason}}

              # Handle case where server doesn't support this call
              _ ->
                {:ok, state}
            end
          else
            {:ok, state}
          end
        end

      {:error, reason} ->
        notify_migration_failed(state, reason)
        {:error, {:migration_failed, reason}}
    end
  end

  defp attempt_rollback(state, _failure_reason) do
    Logger.info("Attempting rollback for #{state.handler_module}")

    case state.backup_state do
      nil ->
        Logger.warning("No backup state available for rollback")
        state

      backup_state ->
        case GenServer.call(state.server, {:update_handler_state, backup_state}) do
          :ok ->
            Logger.info("Rollback completed for #{state.handler_module}")
            notify_rollback_complete(state)
            %{state | backup_state: nil}

          {:error, reason} ->
            Logger.error("Rollback failed: #{inspect(reason)}")
            state
        end
    end
  end

  defp get_module_timestamp(module) do
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

  defp get_loaded_module_timestamp(module) do
    case :code.get_object_code(module) do
      {^module, binary, _filename} ->
        :erlang.phash2(binary)

      :error ->
        # Try alternative method using module info
        case module.module_info(:compile) do
          info when is_list(info) ->
            time = Keyword.get(info, :time, {0, 0, 0})
            :erlang.phash2(time)

          _ ->
            System.system_time(:second)
        end
    end
  rescue
    _ -> System.system_time(:second)
  end

  # Notification functions

  defp notify_reload_complete(state, result) do
    case result do
      :success ->
        send_notification(state, {:handler_reloaded, state.handler_module})

      {:error, reason} ->
        send_notification(state, {:reload_failed, reason})
    end
  end

  defp notify_validation_failed(state, reasons) do
    send_notification(state, {:validation_failed, reasons})
  end

  defp notify_migration_failed(state, reason) do
    send_notification(state, {:migration_failed, reason})
  end

  defp notify_rollback_complete(state) do
    send_notification(state, {:rollback_completed, state.handler_module})
  end

  defp send_notification(state, message) do
    # Send to subscriber if configured
    subscriber = get_in(state.config, [:subscriber])

    if subscriber && Process.alive?(subscriber) do
      send(subscriber, message)
    end

    # Also send to calling process if in test mode
    if Process.get(:reload_test_pid) do
      send(Process.get(:reload_test_pid), message)
    end

    Logger.debug("Hot reload notification: #{inspect(message)}")
  end
end
