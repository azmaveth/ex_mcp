defmodule ExMCP.Transport.Beam do
  @moduledoc """
  BEAM message passing transport for MCP.

  This transport uses native Erlang message passing for communication
  between MCP clients and servers running on the same BEAM VM or
  distributed across Erlang nodes.

  Benefits:
  - Zero serialization overhead for local connections
  - Built-in supervision and fault tolerance
  - Native distributed support (connect to remote BEAM nodes)
  - Better performance for Elixir-based MCP tools
  - Maintains supervision tree integrity

  Supports both:
  - Local connections (same VM): Uses direct process references
  - Distributed connections (remote nodes): Uses {name, node} tuples
  """

  @behaviour ExMCP.Transport

  defstruct [
    # :local or :distributed
    :mode,
    # pid() for local, {name, node()} for distributed
    :target,
    # our client process
    :client_pid,
    # reference for monitoring
    :connection_ref,
    # node monitor reference for distributed mode
    :node_monitor
  ]

  @type t :: %__MODULE__{
          mode: :local | :distributed,
          target: pid() | {atom(), node()},
          client_pid: pid(),
          connection_ref: reference() | nil,
          node_monitor: reference() | nil
        }

  # Connection configurations
  @type local_config :: [target: pid()]
  @type distributed_config :: [target: {atom(), node()}]
  @type server_config :: [name: atom(), node: node() | nil]

  @impl true
  def connect(config) do
    client_pid = self()

    case Keyword.get(config, :target) do
      pid when is_pid(pid) ->
        connect_local(pid, client_pid)

      {name, node} when is_atom(name) and is_atom(node) ->
        connect_distributed({name, node}, client_pid)

      name when is_atom(name) ->
        # Local named process
        case Process.whereis(name) do
          nil ->
            {:error, {:process_not_found, name}}

          pid ->
            connect_local(pid, client_pid)
        end

      _ ->
        {:error, :invalid_target}
    end
  end

  @impl true
  def send_message(message, %__MODULE__{target: target} = state) do
    # Handle both JSON strings and maps
    parsed_message =
      case message do
        binary when is_binary(binary) ->
          case Jason.decode(binary) do
            {:ok, decoded} -> decoded
            {:error, _} -> %{"error" => "Invalid JSON"}
          end

        map when is_map(map) ->
          map
      end

    # Convert JSON-RPC style message to BEAM message
    beam_message = {:mcp_message, state.client_pid, parsed_message}

    case state.mode do
      :local ->
        send(target, beam_message)
        {:ok, state}

      :distributed ->
        # For distributed, we need to handle the case where the node might be down
        case send_distributed(target, beam_message) do
          :ok -> {:ok, state}
          :noconnection -> {:error, :node_disconnected}
          :nosuspend -> {:error, :node_suspended}
        end
    end
  end

  @impl true
  def receive_message(%__MODULE__{} = state) do
    receive do
      {:mcp_message, _from, message} ->
        # Convert back to JSON string for compatibility with existing client
        json_message = Jason.encode!(message)
        {:ok, json_message, state}

      {:mcp_response, _from, message} ->
        # Convert back to JSON string for compatibility with existing client
        json_message = Jason.encode!(message)
        {:ok, json_message, state}

      {:DOWN, ref, :process, _pid, reason} when ref == state.connection_ref ->
        {:error, {:process_down, reason}}

      {:EXIT, pid, reason} when pid == state.target ->
        {:error, {:process_down, reason}}

      {:nodedown, node, _info} when state.mode == :distributed ->
        case state.target do
          {_name, ^node} ->
            {:error, {:node_down, node}}

          _ ->
            # Not our target node, continue
            receive_message(state)
        end

      other ->
        # Unknown message, ignore and continue
        {:error, {:unexpected_message, other}}
    end
  end

  @impl true
  def close(%__MODULE__{connection_ref: ref, node_monitor: node_ref} = _state) do
    if ref, do: Process.demonitor(ref, [:flush])
    if node_ref, do: :net_kernel.monitor_nodes(false)
    :ok
  end

  # Server-side helpers

  @doc """
  Starts a BEAM transport server that can accept connections.

  This creates a named process that MCP clients can connect to
  using the BEAM transport.
  """
  @spec start_server(atom(), module(), keyword()) :: {:ok, pid()} | {:error, any()}
  def start_server(name, handler_module, opts \\ []) do
    GenServer.start_link(__MODULE__.Server, {handler_module, opts}, name: name)
  end

  @doc """
  Registers a server on the current node for discovery.

  This allows the server to be found by clients using
  ExMCP.Discovery functions.
  """
  @spec register_server(atom(), map()) :: :ok
  def register_server(name, server_info) do
    :persistent_term.put({__MODULE__, :servers, name}, server_info)
    :ok
  end

  @doc """
  Discovers BEAM transport servers on the current node.
  """
  @spec discover_local_servers() :: [map()]
  def discover_local_servers do
    :persistent_term.get()
    |> Enum.filter(fn
      {{module, :servers, _name}, _info} when module == __MODULE__ -> true
      _ -> false
    end)
    |> Enum.map(fn {{_module, :servers, name}, info} ->
      Map.merge(info, %{
        name: to_string(name),
        transport: "beam",
        target: name,
        node: Node.self()
      })
    end)
  end

  @doc """
  Discovers BEAM transport servers across the cluster.
  """
  @spec discover_cluster_servers() :: [map()]
  def discover_cluster_servers do
    nodes = [Node.self() | Node.list()]

    Enum.flat_map(nodes, fn node ->
      try do
        :rpc.call(node, __MODULE__, :discover_local_servers, [], 5000)
      rescue
        _ -> []
      catch
        :exit, _ -> []
      end
    end)
  end

  # Private functions

  defp connect_local(pid, client_pid) do
    # Monitor the target process
    ref = Process.monitor(pid)

    state = %__MODULE__{
      mode: :local,
      target: pid,
      client_pid: client_pid,
      connection_ref: ref,
      node_monitor: nil
    }

    # Send a connection request
    send(pid, {:mcp_connect, client_pid})

    # Wait for acknowledgment
    receive do
      {:mcp_connected, ^pid} ->
        {:ok, state}

      {:mcp_connection_refused, ^pid, reason} ->
        Process.demonitor(ref, [:flush])
        {:error, {:connection_refused, reason}}

      {:DOWN, ^ref, :process, ^pid, reason} ->
        {:error, {:process_down, reason}}
    after
      5000 ->
        Process.demonitor(ref, [:flush])
        {:error, :connection_timeout}
    end
  end

  defp connect_distributed({name, node}, client_pid) do
    # Monitor the node
    :net_kernel.monitor_nodes(true)

    # Check if node is connected
    connected_nodes = Node.list()

    connection_result =
      if node in connected_nodes do
        :ok
      else
        # Try to connect to the node
        case Node.connect(node) do
          true -> :ok
          false -> {:error, {:node_connection_failed, node}}
          :ignored -> {:error, {:node_connection_failed, node}}
        end
      end

    case connection_result do
      :ok ->
        state = %__MODULE__{
          mode: :distributed,
          target: {name, node},
          client_pid: client_pid,
          connection_ref: nil,
          node_monitor: true
        }

        # Send connection request to remote process
        case send_distributed({name, node}, {:mcp_connect, {client_pid, Node.self()}}) do
          :ok ->
            # Wait for acknowledgment
            receive do
              {:mcp_connected, {^name, ^node}} ->
                {:ok, state}

              {:mcp_connection_refused, {^name, ^node}, reason} ->
                :net_kernel.monitor_nodes(false)
                {:error, {:connection_refused, reason}}

              {:nodedown, ^node, _info} ->
                {:error, {:node_down, node}}
            after
              10000 ->
                :net_kernel.monitor_nodes(false)
                {:error, :connection_timeout}
            end

          error ->
            :net_kernel.monitor_nodes(false)
            {:error, error}
        end

      error ->
        :net_kernel.monitor_nodes(false)
        error
    end
  end

  defp send_distributed({name, node}, message) do
    try do
      send({name, node}, message)
      :ok
    catch
      :error, :noconnection -> :noconnection
      :error, :nosuspend -> :nosuspend
    end
  end
end

defmodule ExMCP.Transport.Beam.Server do
  @moduledoc """
  GenServer implementation for BEAM transport MCP servers.

  This server handles incoming BEAM transport connections and
  routes MCP messages to the appropriate handler.
  """

  use GenServer
  require Logger

  defstruct [
    :handler_module,
    :handler_state,
    :clients,
    :opts
  ]

  def start_link({handler_module, opts}) do
    GenServer.start_link(__MODULE__, {handler_module, opts})
  end

  @impl true
  def init({handler_module, opts}) do
    # Initialize the handler
    case handler_module.init(opts) do
      {:ok, handler_state} ->
        state = %__MODULE__{
          handler_module: handler_module,
          handler_state: handler_state,
          clients: %{},
          opts: opts
        }

        Logger.info("BEAM MCP server started with handler #{handler_module}")
        {:ok, state}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_info({:mcp_connect, client_ref}, state) do
    # Handle connection from client
    {client_pid, client_node} =
      case client_ref do
        pid when is_pid(pid) -> {pid, Node.self()}
        {pid, node} -> {pid, node}
      end

    # Monitor the client
    ref =
      if client_node == Node.self() do
        Process.monitor(client_pid)
      else
        # For distributed clients, we rely on nodedown messages
        nil
      end

    # Add client to our state
    client_info = %{
      pid: client_pid,
      node: client_node,
      ref: ref,
      connected_at: DateTime.utc_now()
    }

    new_clients = Map.put(state.clients, client_ref, client_info)
    new_state = %{state | clients: new_clients}

    # Send acknowledgment
    response_target =
      case client_ref do
        pid when is_pid(pid) -> pid
        {pid, node} -> {pid, node}
      end

    send(response_target, {:mcp_connected, self()})

    Logger.info("BEAM MCP client connected: #{inspect(client_ref)}")
    {:noreply, new_state}
  end

  def handle_info({:mcp_message, client_ref, message}, state) do
    # Route message to handler
    case Map.get(state.clients, client_ref) do
      nil ->
        # Unknown client, refuse connection
        send(client_ref, {:mcp_connection_refused, self(), :unknown_client})
        {:noreply, state}

      _client_info ->
        # Process the message
        case handle_mcp_message(message, state) do
          {:ok, response, new_handler_state} when not is_nil(response) ->
            # Send response back to client
            response_target =
              case client_ref do
                pid when is_pid(pid) -> pid
                {pid, _node} -> pid
              end

            send(response_target, {:mcp_response, self(), response})

            new_state = %{state | handler_state: new_handler_state}
            {:noreply, new_state}

          {:ok, nil, new_handler_state} ->
            # No response needed (notification)
            new_state = %{state | handler_state: new_handler_state}
            {:noreply, new_state}

          {:error, error} ->
            # Send error response
            error_response = %{
              "jsonrpc" => "2.0",
              "error" => %{
                "code" => -32603,
                "message" => "Internal error",
                "data" => inspect(error)
              },
              "id" => Map.get(message, "id")
            }

            response_target =
              case client_ref do
                pid when is_pid(pid) -> pid
                {pid, _node} -> pid
              end

            send(response_target, {:mcp_response, self(), error_response})
            {:noreply, state}
        end
    end
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    # Client disconnected
    client_ref =
      Enum.find_value(state.clients, fn {client_ref, %{ref: client_ref_ref}} ->
        if client_ref_ref == ref, do: client_ref, else: nil
      end)

    if client_ref do
      new_clients = Map.delete(state.clients, client_ref)
      new_state = %{state | clients: new_clients}
      Logger.info("BEAM MCP client disconnected: #{inspect(client_ref)}")
      {:noreply, new_state}
    else
      {:noreply, state}
    end
  end

  def handle_info({:nodedown, node, _info}, state) do
    # Remove all clients from the disconnected node
    new_clients =
      Enum.reject(state.clients, fn {_ref, %{node: client_node}} ->
        client_node == node
      end)
      |> Enum.into(%{})

    new_state = %{state | clients: new_clients}
    Logger.info("Node disconnected, removed clients from #{node}")
    {:noreply, new_state}
  end

  def handle_info(msg, state) do
    Logger.warning("BEAM MCP server received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private functions

  defp handle_mcp_message(message, state) do
    # Route to the appropriate handler method
    case message do
      %{"method" => "initialize", "params" => params, "id" => id} ->
        case state.handler_module.handle_initialize(params, state.handler_state) do
          {:ok, result, new_handler_state} ->
            response = %{
              "jsonrpc" => "2.0",
              "result" => result,
              "id" => id
            }

            {:ok, response, new_handler_state}

          {:error, error, new_handler_state} ->
            response = %{
              "jsonrpc" => "2.0",
              "error" => error,
              "id" => id
            }

            {:ok, response, new_handler_state}
        end

      %{"method" => "notifications/initialized"} ->
        # No response needed for notifications
        {:ok, nil, state.handler_state}

      %{"method" => "tools/list", "id" => id} ->
        case state.handler_module.handle_list_tools(state.handler_state) do
          {:ok, tools, new_handler_state} ->
            response = %{
              "jsonrpc" => "2.0",
              "result" => %{"tools" => tools},
              "id" => id
            }

            {:ok, response, new_handler_state}

          {:error, error, new_handler_state} ->
            response = %{
              "jsonrpc" => "2.0",
              "error" => error,
              "id" => id
            }

            {:ok, response, new_handler_state}
        end

      %{"method" => "tools/call", "params" => %{"name" => name, "arguments" => args}, "id" => id} ->
        case state.handler_module.handle_call_tool(name, args, state.handler_state) do
          {:ok, result, new_handler_state} ->
            response = %{
              "jsonrpc" => "2.0",
              "result" => %{"content" => result},
              "id" => id
            }

            {:ok, response, new_handler_state}

          {:error, error, new_handler_state} ->
            response = %{
              "jsonrpc" => "2.0",
              "error" => error,
              "id" => id
            }

            {:ok, response, new_handler_state}
        end

      %{"method" => method, "id" => id} ->
        # Unknown method
        response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32601,
            "message" => "Method not found: #{method}"
          },
          "id" => id
        }

        {:ok, response, state.handler_state}

      _ ->
        {:error, :invalid_message}
    end
  end
end
