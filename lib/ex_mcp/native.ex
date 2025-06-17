defmodule ExMCP.Native do
  @moduledoc """
  Native BEAM service dispatcher for direct process communication within Elixir clusters.

  This module provides ultra-fast, direct process communication leveraging OTP's built-in features:
  - Direct GenServer.call between processes
  - Horde.Registry for distributed service discovery
  - Automatic distribution across BEAM nodes
  - Built-in fault tolerance and monitoring
  - Zero serialization overhead for local calls

  ## Use Cases

  - Trusted Elixir services within the same cluster
  - High-performance internal communication (~15μs local, ~50μs cross-node)
  - Services that need OTP supervision and monitoring

  > #### Future API Direction {: .info}
  >
  > In future versions, native BEAM communication will be available as a transport
  > option for `ExMCP.Client`, providing a unified API across all communication methods.
  > The current `ExMCP.Native` API will remain supported but may be superseded.

  ## Performance Characteristics

  - Local calls: ~15μs latency
  - Cross-node calls: ~50μs latency
  - Memory overhead: Single Horde.Registry entry per service
  - Throughput: Limited only by service processing speed

  ## Examples

      # Service registration (typically in init/1)
      ExMCP.Native.register_service(:my_tool_service)

      # Direct service calls
      {:ok, result} = ExMCP.Native.call(:my_service, "list_tools", %{})

      # Cross-node calls (automatic)
      {:ok, result} = ExMCP.Native.call({:my_service, :"node@host"}, "method", params)

      # Fire-and-forget notifications
      :ok = ExMCP.Native.notify(:event_service, "resource_updated", %{
        "uri" => "file:///config.json",
        "type" => "modified"
      })

  ## Service Discovery

      # List all available services across the cluster
      services = ExMCP.Native.list_services()

      # Check if a service is available
      available? = ExMCP.Native.service_available?(:my_service)
  """

  require Logger

  @registry_name ExMCP.ServiceRegistry
  @default_timeout 5_000

  @type service_id :: atom() | {atom(), node()}
  @type method :: String.t()
  @type params :: map()
  @type result :: term()

  # Public API

  @doc """
  Registers a service with the distributed registry.

  This should typically be called in a GenServer's init/1 callback.

  ## Examples

      def init(_) do
        ExMCP.Native.register_service(:my_tools)
        {:ok, %{}}
      end
  """
  @spec register_service(atom()) :: :ok
  def register_service(name) when is_atom(name) do
    case Horde.Registry.register(@registry_name, name, %{registered_at: DateTime.utc_now()}) do
      {:ok, _pid} ->
        Logger.info("Service registered: #{name}")
        :ok

      {:error, {:already_registered, _pid}} ->
        Logger.warning("Service already registered: #{name}")
        :ok
    end
  end

  @doc """
  Unregisters a service from the distributed registry.

  This should typically be called in a GenServer's terminate/2 callback.
  """
  @spec unregister_service(atom()) :: :ok
  def unregister_service(name) when is_atom(name) do
    Horde.Registry.unregister(@registry_name, name)
    Logger.info("Service unregistered: #{name}")
    :ok
  end

  @doc """
  Calls a service method with the given parameters.

  Supports both local and cross-node calls transparently through Horde.Registry.

  ## Options

  - `:timeout` - Call timeout in milliseconds (default: 5000)
  - `:progress_token` - Progress token for long-running operations
  - `:meta` - Additional metadata map

  ## Examples

      # Simple call
      {:ok, tools} = ExMCP.Native.call(:my_tools, "list_tools", %{})

      # Call with timeout and metadata
      {:ok, result} = ExMCP.Native.call(
        :my_tools,
        "process_data",
        %{"dataset" => "large_data"},
        timeout: 30_000,
        meta: %{"trace_id" => "abc", "user_id" => "user1"}
      )
  """
  @spec call(service_id(), method(), params(), keyword()) :: {:ok, result()} | {:error, term()}
  def call(service_id, method, params, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    progress_token = Keyword.get(opts, :progress_token)
    meta = Keyword.get(opts, :meta, %{})

    case lookup_service(service_id) do
      {:ok, pid} ->
        message = build_mcp_message(method, params, progress_token, meta)
        make_service_call(pid, message, timeout)

      {:error, :not_found} ->
        {:error, {:service_not_found, service_id}}
    end
  end

  @doc """
  Sends a notification to a service (fire-and-forget).

  Notifications do not expect a response and are sent asynchronously.

  ## Examples

      :ok = ExMCP.Native.notify(:event_service, "resource_updated", %{
        "uri" => "file:///config.json",
        "type" => "modified"
      })
  """
  @spec notify(service_id(), method(), params()) :: :ok | {:error, term()}
  def notify(service_id, method, params) do
    case lookup_service(service_id) do
      {:ok, pid} ->
        message = %{"method" => method, "params" => params}
        GenServer.cast(pid, {:mcp_notification, message})
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Lists all services registered in the distributed registry.

  Returns a list of tuples containing {service_name, pid, metadata}.
  """
  @spec list_services() :: [{atom(), pid(), map()}]
  def list_services do
    Horde.Registry.select(@registry_name, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}])
  end

  @doc """
  Checks if a service is available in the distributed registry.
  """
  @spec service_available?(atom()) :: boolean()
  def service_available?(service_name) when is_atom(service_name) do
    case Horde.Registry.lookup(@registry_name, service_name) do
      [{_pid, _metadata}] -> true
      [] -> false
    end
  end

  # Private functions

  defp lookup_service(service_id) when is_atom(service_id) do
    case Horde.Registry.lookup(@registry_name, service_id) do
      [{pid, _metadata}] -> {:ok, pid}
      [] -> {:error, :not_found}
    end
  end

  defp lookup_service({service_name, node}) when is_atom(service_name) and is_atom(node) do
    # Horde handles cross-node lookups transparently
    # No need for RPC calls - Horde gossips service registrations across the cluster
    case Horde.Registry.lookup(@registry_name, service_name) do
      [{pid, _metadata}] ->
        # Verify the PID is on the expected node
        if node(pid) == node do
          {:ok, pid}
        else
          {:error, :not_found}
        end

      [] ->
        {:error, :not_found}
    end
  end

  defp build_mcp_message(method, params, progress_token, meta) do
    message = %{"method" => method, "params" => params}
    message = add_progress_token(message, progress_token)
    add_meta(message, meta)
  end

  defp add_progress_token(message, nil), do: message

  defp add_progress_token(message, progress_token) do
    message = ensure_meta_exists(message)
    put_in(message["_meta"]["progressToken"], progress_token)
  end

  defp add_meta(message, meta) when map_size(meta) == 0, do: message

  defp add_meta(message, meta) do
    message = ensure_meta_exists(message)
    Map.put(message, "_meta", Map.merge(message["_meta"], meta))
  end

  defp ensure_meta_exists(message) do
    Map.put_new(message, "_meta", %{})
  end

  defp make_service_call(pid, message, timeout) do
    case safe_genserver_call(pid, {:mcp_request, message}, timeout) do
      {:ok, {:ok, result}} -> {:ok, result}
      {:ok, {:error, reason}} -> {:error, reason}
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
    end
  end

  defp safe_genserver_call(pid, message, timeout) do
    {:ok, GenServer.call(pid, message, timeout)}
  catch
    :exit, {:timeout, _} -> {:error, :timeout}
    :exit, {:noproc, _} -> {:error, :service_unavailable}
    :exit, {:normal, _} -> {:error, :service_terminated}
    :exit, reason -> {:error, {:service_exit, reason}}
  end
end
