defmodule ExMCP.Transport.Beam.HotReload do
  @moduledoc """
  Hot code reloading support for BEAM transport MCP servers.

  Provides zero-downtime updates of MCP server handler modules while preserving:
  - Active client connections
  - Server state (with optional migration)
  - Transport layer continuity

  This leverages BEAM's hot code loading capabilities to enable live updates
  of MCP servers without service interruption.

  ## Features

  - **Handler Module Reloading**: Automatically detect and reload changed handler modules
  - **State Migration**: Support for migrating handler state during reloads
  - **Connection Preservation**: Keep client connections active during reloads
  - **Validation**: Validate new code before applying changes
  - **Rollback**: Automatic rollback on reload failures
  - **Multiple Watch Strategies**: File system, polling, or module timestamp watching

  ## Example Usage

      # Start server with hot reloading enabled
      {:ok, server} = ExMCP.Server.start_link([
        handler: MyHandler,
        transport: :beam
      ])

      # Enable hot reloading
      {:ok, manager} = HotReload.enable(server, %{
        auto_reload: true,
        state_migration: :automatic,
        preserve_connections: true,
        validation: :strict
      })

      # Server will now automatically reload when MyHandler module changes

      # Manually trigger reload if needed
      HotReload.reload_handler(server)

      # Disable hot reloading
      HotReload.disable(server)

  ## Configuration Options

  - `:auto_reload` - Enable automatic reloading on code changes (default: false)
  - `:state_migration` - State migration strategy (:none, :automatic, :callback)
  - `:migration_callback` - Function for custom state migration
  - `:preserve_connections` - Keep connections during reload (default: true)
  - `:validation` - Validation level (:none, :basic, :strict)
  - `:rollback_on_failure` - Enable automatic rollback (default: true)
  - `:throttle_ms` - Minimum time between reloads (default: 1000)
  - `:watch_strategy` - How to detect changes (:filesystem, :polling, :module_timestamp)
  - `:watch_paths` - Paths to watch for filesystem strategy
  - `:poll_interval` - Polling interval for polling strategy (default: 2000)
  """

  require Logger

  alias ExMCP.Server.Handler
  alias ExMCP.Transport.Beam.ReloadManager

  @type reload_config :: %{
          auto_reload: boolean(),
          state_migration: :none | :automatic | :callback,
          migration_callback: (any() -> {:ok, any()} | {:error, any()}) | nil,
          preserve_connections: boolean(),
          validation: :none | :basic | :strict,
          rollback_on_failure: boolean(),
          throttle_ms: non_neg_integer(),
          watch_strategy: :filesystem | :polling | :module_timestamp,
          watch_paths: [String.t()] | nil,
          poll_interval: non_neg_integer(),
          transport_aware: boolean()
        }

  @default_config %{
    auto_reload: false,
    state_migration: :automatic,
    migration_callback: nil,
    preserve_connections: true,
    validation: :basic,
    rollback_on_failure: true,
    throttle_ms: 1000,
    watch_strategy: :module_timestamp,
    watch_paths: nil,
    poll_interval: 2000,
    transport_aware: false
  }

  @doc """
  Enables hot code reloading for an MCP server.

  Returns a reload manager process that monitors the handler module
  and manages reloads according to the configuration.
  """
  @spec enable(GenServer.server()) :: {:ok, pid()} | {:error, term()}
  @spec enable(GenServer.server(), reload_config() | map()) ::
          {:ok, pid()} | {:error, term()}
  def enable(server, config \\ %{}) when is_pid(server) or is_atom(server) do
    full_config = Map.merge(@default_config, config)

    with {:ok, handler_module} <- get_handler_module(server),
         {:ok, manager_config} <- build_manager_config(handler_module, server, full_config) do
      case ReloadManager.start_link(manager_config) do
        {:ok, manager} ->
          # Register the manager with the server for cleanup
          register_manager(server, manager)
          {:ok, manager}

        error ->
          error
      end
    end
  end

  @doc """
  Disables hot code reloading for an MCP server.

  Stops the reload manager and cleans up watchers.
  """
  @spec disable(GenServer.server()) :: :ok
  def disable(server) do
    case get_manager(server) do
      {:ok, manager} ->
        ReloadManager.stop(manager)
        unregister_manager(server)
        :ok

      {:error, :not_found} ->
        :ok
    end
  end

  @doc """
  Manually triggers a handler reload.

  Bypasses automatic detection and immediately attempts to reload
  the handler module with the current configuration.
  """
  @spec reload_handler(GenServer.server()) ::
          :ok | {:error, term()}
  def reload_handler(server) do
    case get_manager(server) do
      {:ok, manager} ->
        ReloadManager.trigger_reload(manager)

      {:error, :not_found} ->
        {:error, :hot_reload_not_enabled}
    end
  end

  @doc """
  Gets the current reload configuration for a server.
  """
  @spec get_config(GenServer.server()) ::
          {:ok, reload_config()} | {:error, :not_found}
  def get_config(server) do
    case get_manager(server) do
      {:ok, manager} ->
        ReloadManager.get_config(manager)

      error ->
        error
    end
  end

  @doc """
  Updates the reload configuration for a server.
  """
  @spec update_config(GenServer.server(), reload_config()) ::
          :ok | {:error, term()}
  def update_config(server, new_config) do
    case get_manager(server) do
      {:ok, manager} ->
        ReloadManager.update_config(manager, new_config)

      error ->
        error
    end
  end

  @doc """
  Gets the status of hot reloading for a server.

  Returns information about the current state, last reload time,
  and any recent errors.
  """
  @spec get_status(GenServer.server()) ::
          {:ok, map()} | {:error, :not_found}
  def get_status(server) do
    case get_manager(server) do
      {:ok, manager} ->
        ReloadManager.get_status(manager)

      error ->
        error
    end
  end

  @doc """
  Validates that a handler module implements the required callbacks.

  Used internally during reloads to ensure new code is valid before
  applying changes.
  """
  @spec validate_handler_module(module(), :none | :basic | :strict) ::
          :ok | {:error, [String.t()]}
  def validate_handler_module(module, validation_level \\ :basic) do
    case validation_level do
      :none ->
        :ok

      :basic ->
        validate_basic_handler(module)

      :strict ->
        with :ok <- validate_basic_handler(module) do
          validate_strict_handler(module)
        end
    end
  end

  @doc """
  Performs a state migration during handler reload.

  Supports different migration strategies:
  - `:none` - No migration, use default state
  - `:automatic` - Attempt automatic migration preserving common fields
  - `:callback` - Use custom migration function
  """
  @spec migrate_state(any(), :none | :automatic | :callback, function() | nil) ::
          {:ok, any()} | {:error, term()}
  def migrate_state(old_state, strategy, callback \\ nil) do
    case strategy do
      :none ->
        {:ok, nil}

      :automatic ->
        migrate_state_automatic(old_state)

      :callback when is_function(callback, 1) ->
        try do
          callback.(old_state)
        rescue
          error -> {:error, {:migration_callback_failed, error}}
        catch
          :exit, reason -> {:error, {:migration_callback_exit, reason}}
          :throw, value -> {:error, {:migration_callback_throw, value}}
        end

      :callback ->
        {:error, :missing_migration_callback}
    end
  end

  # Private functions

  defp get_handler_module(server) do
    case GenServer.call(server, :get_handler_info) do
      {:ok, %{handler: handler_module}} ->
        {:ok, handler_module}

      {:ok, handler_module} when is_atom(handler_module) ->
        {:ok, handler_module}

      error ->
        {:error, {:invalid_handler_info, error}}
    end
  rescue
    error -> {:error, {:server_call_failed, error}}
  catch
    :exit, reason -> {:error, {:server_exit, reason}}
  end

  defp build_manager_config(handler_module, server, config) do
    manager_config = %{
      handler_module: handler_module,
      server: server,
      config: config,
      watch_strategy: config.watch_strategy,
      watch_paths: determine_watch_paths(handler_module, config),
      poll_interval: config.poll_interval,
      throttle_ms: config.throttle_ms
    }

    {:ok, manager_config}
  end

  defp determine_watch_paths(handler_module, config) do
    case config.watch_paths do
      nil ->
        # Try to determine from module
        case get_module_source_path(handler_module) do
          {:ok, path} -> [path]
          {:error, _} -> []
        end

      paths when is_list(paths) ->
        paths
    end
  end

  defp get_module_source_path(module) do
    case module.module_info(:compile) do
      info when is_list(info) ->
        case Keyword.get(info, :source) do
          path when is_list(path) ->
            {:ok, List.to_string(path)}

          nil ->
            {:error, :source_not_found}
        end

      _ ->
        {:error, :compile_info_not_available}
    end
  rescue
    _ -> {:error, :module_info_failed}
  end

  defp validate_basic_handler(module) do
    required_callbacks = [
      {:init, 1},
      {:handle_initialize, 2},
      # Takes cursor and state
      {:handle_list_tools, 2},
      # Required by Handler behaviour
      {:terminate, 2}
    ]

    errors =
      required_callbacks
      |> Enum.filter(fn {fun, arity} ->
        # Ensure the module is loaded first
        Code.ensure_loaded(module)
        not function_exported?(module, fun, arity)
      end)
      |> Enum.map(fn {fun, arity} ->
        "Missing required callback: #{fun}/#{arity}"
      end)

    case errors do
      [] -> :ok
      errors -> {:error, errors}
    end
  end

  defp validate_strict_handler(module) do
    # Additional validations for strict mode
    optional_callbacks = [
      {:handle_call_tool, 3},
      {:handle_get_resource, 2},
      {:handle_list_resources, 1},
      {:handle_get_prompt, 3},
      {:handle_list_prompts, 1}
    ]

    warnings =
      optional_callbacks
      |> Enum.filter(fn {fun, arity} ->
        not function_exported?(module, fun, arity)
      end)
      |> Enum.map(fn {fun, arity} ->
        "Optional callback not implemented: #{fun}/#{arity}"
      end)

    # For strict validation, we might want to check other things:
    # - Module uses ExMCP.Server.Handler
    # - No obvious code issues

    if uses_handler_behaviour?(module) do
      case warnings do
        [] ->
          :ok

        _ ->
          Logger.warning("Handler validation warnings: #{inspect(warnings)}")
          :ok
      end
    else
      {:error, ["Module does not use ExMCP.Server.Handler behaviour"]}
    end
  end

  defp uses_handler_behaviour?(module) do
    behaviours =
      module.module_info(:attributes)
      |> Keyword.get(:behaviour, [])

    Handler in behaviours
  rescue
    _ -> false
  end

  defp migrate_state_automatic(old_state) when is_map(old_state) do
    # For automatic migration, preserve the state as-is if it's a map
    # In a real implementation, you might want more sophisticated merging
    {:ok, old_state}
  end

  defp migrate_state_automatic(old_state) do
    # For non-map states, we can't automatically migrate
    Logger.warning("Cannot automatically migrate non-map state: #{inspect(old_state)}")
    {:ok, nil}
  end

  # Manager registry functions

  defp register_manager(server, manager) do
    # In a real implementation, this might use a registry or ETS table
    # For now, we'll use process dictionary
    Process.put({:hot_reload_manager, server}, manager)
  end

  defp unregister_manager(server) do
    Process.delete({:hot_reload_manager, server})
  end

  defp get_manager(server) do
    case Process.get({:hot_reload_manager, server}) do
      nil ->
        {:error, :not_found}

      manager when is_pid(manager) ->
        if Process.alive?(manager) do
          {:ok, manager}
        else
          unregister_manager(server)
          {:error, :not_found}
        end
    end
  end
end
