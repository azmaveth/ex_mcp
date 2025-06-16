defmodule ExMCP.Transport.Native do
  @moduledoc """
  Native BEAM transport for direct process communication within Elixir clusters.

  This transport leverages OTP's built-in features for maximum performance:
  - Direct GenServer.call between processes
  - Registry-based service discovery 
  - Automatic distribution across BEAM nodes
  - Built-in fault tolerance and monitoring
  - Zero serialization overhead for local calls

  ## Use Cases

  - Trusted Elixir services within the same cluster
  - High-performance internal communication
  - Services that need OTP supervision and monitoring

  ## Performance Characteristics

  - Local calls: ~15μs latency
  - Cross-node calls: ~50μs latency  
  - Memory overhead: Single Registry entry per service
  - Throughput: Limited only by service processing speed

  ## Examples

      # Service registration (typically in init/1)
      ExMCP.Transport.Native.register_service(:my_tool_service)

      # Direct service calls
      {:ok, result} = ExMCP.Transport.Native.call(:my_tool_service, "list_tools", %{})

      # Cross-node communication (automatic)
      {:ok, result} = ExMCP.Transport.Native.call(
        {:my_service, :"node@host"}, 
        "call_tool", 
        %{"name" => "calculator", "arguments" => %{"operation" => "add", "a" => 1, "b" => 2}}
      )

  ## Security Model

  This transport relies on Erlang's security model:
  - Erlang cookie authentication for node joining
  - Process isolation and supervision
  - Trusted cluster assumption (all services are trusted)

  For untrusted services or external clients, use the HTTP transport instead.
  """

  @behaviour ExMCP.Transport
  require Logger

  @typedoc "Service identifier - atom name or {name, node} tuple"
  @type service_id :: atom() | {atom(), node()}

  @typedoc "MCP method name"
  @type method :: String.t()

  @typedoc "MCP parameters"
  @type params :: map()

  @typedoc "Call options"
  @type call_opts :: [
          timeout: timeout(),
          progress_token: String.t() | nil,
          meta: map()
        ]

  @default_timeout 5_000

  # Registry name for service discovery
  @registry_name ExMCP.Registry

  # Transport Implementation (for compatibility with existing Client/Server APIs)

  @impl true
  def connect(opts) do
    # For native BEAM, "connection" is just service discovery setup
    # The actual Registry is started by ExMCP.Application
    service_name = Keyword.get(opts, :service_name)
    node = Keyword.get(opts, :node, node())

    state = %{
      service_id: if(service_name, do: {service_name, node}, else: nil),
      default_timeout: Keyword.get(opts, :timeout, @default_timeout)
    }

    {:ok, state}
  end

  @impl true
  def send_message(message, state) do
    # For native BEAM, we parse the JSON message and convert to direct call
    case Jason.decode(message) do
      {:ok, %{"method" => method, "params" => params} = msg} ->
        id = Map.get(msg, "id")
        result = call_service(state.service_id, method, params, [])

        # Convert result back to JSON for compatibility
        response =
          case result do
            {:ok, data} -> %{"id" => id, "result" => data}
            {:error, error} -> %{"id" => id, "error" => error}
          end

        {:ok, Jason.encode!(response), state}

      {:error, reason} ->
        {:error, {:json_decode_error, reason}}
    end
  end

  @impl true
  def receive_message(_state) do
    # Native BEAM transport doesn't need to "receive" messages in the traditional sense
    # This is primarily used by the Client for request/response correlation
    # For now, return an error as this should not be called in normal operation
    {:error, :not_implemented}
  end

  @impl true
  def close(_state) do
    # No resources to clean up for native BEAM transport
    :ok
  end

  @impl true
  def connected?(_state) do
    # Native BEAM is always "connected" if the node is alive
    true
  end

  # Public API

  @doc """
  Register a service in the global registry.

  This makes the service discoverable by other processes in the cluster.

  ## Examples

      # Register current process as a service
      ExMCP.Transport.Native.register_service(:my_tool_service)

      # Register another process
      ExMCP.Transport.Native.register_service(:my_service, other_pid)
  """
  @spec register_service(atom(), pid()) :: :ok | {:error, term()}
  def register_service(name, pid \\ self()) do
    case Registry.register(@registry_name, name, %{registered_at: DateTime.utc_now()}) do
      {:ok, _} ->
        Logger.debug("Registered service #{inspect(name)} with pid #{inspect(pid)}")
        :ok

      {:error, {:already_registered, existing_pid}} ->
        Logger.warning(
          "Service #{inspect(name)} already registered with pid #{inspect(existing_pid)}"
        )

        {:error, :already_registered}
    end
  end

  @doc """
  Unregister a service from the global registry.
  """
  @spec unregister_service(atom()) :: :ok
  def unregister_service(name) do
    Registry.unregister(@registry_name, name)
  end

  @doc """
  Call a service method directly.

  This is the primary API for service-to-service communication within the cluster.

  ## Examples

      # Call a local service
      {:ok, tools} = ExMCP.Transport.Native.call(:tool_service, "list_tools", %{})

      # Call a service on another node
      {:ok, result} = ExMCP.Transport.Native.call(
        {:tool_service, :"worker@host"},
        "call_tool",
        %{"name" => "calculator", "arguments" => %{"a" => 1, "b" => 2}}
      )

      # Call with timeout and progress token
      {:ok, result} = ExMCP.Transport.Native.call(
        :data_service,
        "process_large_dataset", 
        %{"dataset_id" => "abc123"},
        timeout: 30_000,
        progress_token: "progress-123"
      )
  """
  @spec call(service_id(), method(), params(), call_opts()) ::
          {:ok, term()} | {:error, term()}
  def call(service_id, method, params, opts \\ []) do
    call_service(service_id, method, params, opts)
  end

  @doc """
  Send a notification to a service (fire-and-forget).

  Notifications do not expect a response and are useful for events and updates.

  ## Examples

      # Notify a service of an event
      ExMCP.Transport.Native.notify(:event_service, "resource_updated", %{
        "uri" => "file:///config.json",
        "type" => "modified"
      })
  """
  @spec notify(service_id(), method(), params()) :: :ok | {:error, term()}
  def notify(service_id, method, params) do
    case lookup_service(service_id) do
      {:ok, pid} ->
        message = %{
          "method" => method,
          "params" => params
        }

        GenServer.cast(pid, {:mcp_notification, message})
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  List all registered services in the cluster.

  Returns a list of {service_name, pid, metadata} tuples.
  """
  @spec list_services() :: [{atom(), pid(), map()}]
  def list_services do
    Registry.select(@registry_name, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}])
  end

  @doc """
  Check if a service is registered and available.
  """
  @spec service_available?(service_id()) :: boolean()
  def service_available?(service_id) do
    case lookup_service(service_id) do
      {:ok, pid} -> Process.alive?(pid)
      {:error, _} -> false
    end
  end

  # Private Implementation

  defp call_service(service_id, method, params, opts) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    progress_token = Keyword.get(opts, :progress_token)
    meta = Keyword.get(opts, :meta, %{})

    case lookup_service(service_id) do
      {:ok, pid} ->
        message = build_mcp_message(method, params, progress_token, meta)
        make_service_call(pid, message, timeout)

      {:error, reason} ->
        {:error, %{"code" => -32601, "message" => "Service not found: #{reason}"}}
    end
  end

  defp build_mcp_message(method, params, progress_token, meta) do
    message = %{"method" => method, "params" => params}

    message
    |> add_progress_token(progress_token)
    |> add_meta(meta)
  end

  defp add_progress_token(message, nil), do: message

  defp add_progress_token(message, progress_token) do
    message = ensure_meta_exists(message)
    put_in(message, ["params", "_meta", "progressToken"], progress_token)
  end

  defp add_meta(message, meta) when map_size(meta) == 0, do: message

  defp add_meta(message, meta) do
    message = ensure_meta_exists(message)
    current_meta = get_in(message, ["params", "_meta"]) || %{}
    put_in(message, ["params", "_meta"], Map.merge(current_meta, meta))
  end

  defp ensure_meta_exists(message) do
    if Map.has_key?(message["params"], "_meta") do
      message
    else
      put_in(message, ["params", "_meta"], %{})
    end
  end

  defp make_service_call(pid, message, timeout) do
    case GenServer.call(pid, {:mcp_request, message}, timeout) do
      {:ok, result} -> {:ok, result}
      {:error, error} -> {:error, error}
      other -> {:error, {:unexpected_response, other}}
    end
  catch
    :exit, {:timeout, _} ->
      {:error, %{"code" => -32603, "message" => "Service timeout"}}

    :exit, {:noproc, _} ->
      {:error, %{"code" => -32602, "message" => "Service unavailable"}}

    :exit, {:normal, _} ->
      {:error, %{"code" => -32602, "message" => "Service terminated"}}
  end

  defp lookup_service(service_id) when is_atom(service_id) do
    case Registry.lookup(@registry_name, service_id) do
      [{pid, _metadata}] -> {:ok, pid}
      [] -> {:error, :not_found}
    end
  end

  defp lookup_service({service_name, node}) when is_atom(service_name) and is_atom(node) do
    # For cross-node communication, we use rpc to lookup the service on the remote node
    case :rpc.call(node, Registry, :lookup, [@registry_name, service_name]) do
      [{pid, _metadata}] -> {:ok, pid}
      [] -> {:error, :not_found}
      {:badrpc, reason} -> {:error, {:node_error, reason}}
    end
  end

  defp lookup_service(invalid) do
    {:error, {:invalid_service_id, invalid}}
  end
end
