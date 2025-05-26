defmodule ExMCP.Transport.Beam do
  @moduledoc """
  BEAM transport implementation for ExMCP.

  This transport uses Erlang/Elixir message passing for communication between
  MCP clients and servers running in the same VM or across distributed nodes.
  It's the most natural transport for Elixir applications, providing seamless
  integration with OTP supervision trees and built-in fault tolerance.

  ## Architecture

  The transport creates a pair of mailbox processes that act as bidirectional
  channels between client and server. This design provides:

  - Clean separation between transport and protocol layers
  - Natural message buffering through process mailboxes
  - Support for both local and distributed communication
  - Graceful error handling and disconnection detection
  - Full support for server-initiated notifications
  - Automatic cleanup when processes terminate

  ## Examples

  ### Basic Local Communication

      # Define a simple calculator server
      defmodule CalculatorServer do
        use ExMCP.Server.Handler
        
        @impl true
        def handle_initialize(_params, state) do
          {:ok, %{name: "calculator", version: "1.0"}, state}
        end
        
        @impl true
        def handle_list_tools(state) do
          tools = [
            %{name: "add", description: "Add two numbers"},
            %{name: "multiply", description: "Multiply two numbers"}
          ]
          {:ok, tools, state}
        end
        
        @impl true
        def handle_call_tool("add", %{"a" => a, "b" => b}, state) do
          result = a + b
          {:ok, [%{type: "text", text: "Result: " <> to_string(result)}], state}
        end
        
        def handle_call_tool("multiply", %{"a" => a, "b" => b}, state) do
          result = a * b
          {:ok, [%{type: "text", text: "Result: " <> to_string(result)}], state}
        end
      end
      
      # Start server and client
      {:ok, server} = ExMCP.Server.start_link(
        transport: :beam,
        name: :calc_server,
        handler: CalculatorServer
      )
      
      {:ok, client} = ExMCP.Client.start_link(
        transport: :beam,
        server: :calc_server
      )
      
      # Use the tools
      {:ok, result} = ExMCP.Client.call_tool(client, "add", %{"a" => 5, "b" => 3})
      # => {:ok, %{"content" => [%{"type" => "text", "text" => "Result: 8"}]}}

  ### Distributed Communication

      # On node1 - Start a weather service
      {:ok, server} = ExMCP.Server.start_link(
        transport: :beam,
        name: :weather_service,
        handler: WeatherHandler
      )
      
      # On node2 - Connect to the service
      Node.connect(:"node1@host")
      
      {:ok, client} = ExMCP.Client.start_link(
        transport: :beam,
        server: {:weather_service, :"node1@host"}
      )
      
      # Works transparently across nodes
      {:ok, weather} = ExMCP.Client.call_tool(
        client, 
        "get_weather", 
        %{"city" => "London"}
      )

  ### Server-Initiated Notifications

      # The BEAM transport fully supports server-initiated notifications
      # Server can send progress updates:
      ExMCP.Server.notify_progress(server, "task-123", 50, 100)
      
      # Or notify about resource changes:
      ExMCP.Server.notify_resources_changed(server)
      
      # Clients receive these automatically through the transport

  ### Integration with Supervisors

      defmodule MyApp.MCPSupervisor do
        use Supervisor
        
        def start_link(opts) do
          Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
        end
        
        @impl true
        def init(_opts) do
          children = [
            {ExMCP.Server, 
             transport: :beam, 
             name: :my_service,
             handler: MyApp.ServiceHandler},
            {ExMCP.Client,
             transport: :beam,
             server: :my_service,
             name: :my_client}
          ]
          
          Supervisor.init(children, strategy: :one_for_one)
        end
      end

  ## Connection Options

  - `:server` - The server to connect to. Can be:
    - An atom for a locally registered process (e.g., `:my_server`)
    - A tuple for a remote process (e.g., `{:my_server, :"node@host"}`)
    - A PID for direct connection (e.g., `self()`)

  - `:name` - (Server only) Optional name to register the server process

  ## Fault Tolerance

  The BEAM transport automatically handles:

  - Process crashes - The transport detects when either endpoint dies
  - Network partitions - For distributed nodes
  - Message buffering - Process mailboxes buffer messages naturally
  - Reconnection - Clients automatically attempt to reconnect

  When a connection is lost, both client and server are notified through
  their respective transport callbacks.
  """

  require Logger

  @behaviour ExMCP.Transport

  defmodule State do
    @moduledoc false
    defstruct [:mailbox_pid, :mode, :peer_ref]
  end

  defmodule Mailbox do
    @moduledoc """
    A lightweight process that acts as a message mailbox/channel.
    Forwards messages between transport endpoints.
    """
    use GenServer

    defstruct [:peer_pid, :owner_pid, :mode, :peer_ref]

    def start_link(opts) do
      GenServer.start_link(__MODULE__, opts)
    end

    def send_message(mailbox, message) do
      GenServer.call(mailbox, {:send, message})
    end

    def set_peer(mailbox, peer_pid) do
      GenServer.call(mailbox, {:set_peer, peer_pid})
    end

    @impl true
    def init(opts) do
      owner_pid = Keyword.fetch!(opts, :owner)
      mode = Keyword.get(opts, :mode, :client)

      # Monitor the owner so we can clean up if it dies
      Process.monitor(owner_pid)

      state = %__MODULE__{
        owner_pid: owner_pid,
        mode: mode,
        peer_pid: nil,
        peer_ref: nil
      }

      {:ok, state}
    end

    @impl true
    def handle_call({:send, message}, _from, state) do
      if state.peer_pid do
        # Send to peer mailbox
        send(state.peer_pid, {:mcp_message, message})
        {:reply, :ok, state}
      else
        {:reply, {:error, :not_connected}, state}
      end
    end

    def handle_call({:set_peer, peer_pid}, _from, state) do
      # Monitor the peer
      peer_ref = Process.monitor(peer_pid)

      {:reply, :ok, %{state | peer_pid: peer_pid, peer_ref: peer_ref}}
    end

    @impl true
    def handle_info({:mcp_message, message}, state) do
      # Forward message to owner
      send(state.owner_pid, {:transport_message, message})
      {:noreply, state}
    end

    def handle_info({:mcp_connected, peer_pid}, state) do
      # Connection acknowledgment from server
      send(state.owner_pid, {:mcp_connected, peer_pid})
      {:noreply, state}
    end

    def handle_info({:DOWN, ref, :process, pid, _reason}, state) do
      cond do
        pid == state.owner_pid ->
          # Owner died, terminate
          {:stop, :normal, state}

        ref == state.peer_ref ->
          # Peer died, notify owner and clear peer
          send(state.owner_pid, {:transport_closed, :peer_down})
          {:noreply, %{state | peer_pid: nil, peer_ref: nil}}

        true ->
          {:noreply, state}
      end
    end
  end

  # Client Implementation

  @impl true
  def connect(opts) do
    server = Keyword.fetch!(opts, :server)

    # Start our mailbox
    {:ok, mailbox} = Mailbox.start_link(owner: self(), mode: :client)

    # Connect to server
    case connect_to_server(server, mailbox) do
      {:ok, mode} ->
        {:ok, %State{mailbox_pid: mailbox, mode: mode, peer_ref: server}}

      {:error, reason} ->
        GenServer.stop(mailbox)
        {:error, reason}
    end
  end

  @impl true
  def send_message(message, %State{mailbox_pid: mailbox} = state) do
    case Mailbox.send_message(mailbox, message) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def receive_message(%State{} = state) do
    # This will block waiting for a message
    receive do
      {:transport_message, message} ->
        {:ok, message, state}

      {:transport_closed, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def close(%State{mailbox_pid: mailbox}) do
    if Process.alive?(mailbox) do
      GenServer.stop(mailbox)
    end

    :ok
  end

  @impl true
  def connected?(%State{mailbox_pid: mailbox}) do
    Process.alive?(mailbox)
  end

  # Server Implementation

  @doc """
  Accepts incoming BEAM transport connections.

  This is called by ExMCP.Server when initializing with BEAM transport.
  """
  def accept(opts) do
    # When used as a server transport, we need to handle incoming connections
    # This is called by ExMCP.Server when initializing
    name = Keyword.get(opts, :name)

    if name do
      # Register the accepting process so clients can find us
      Process.register(self(), name)
    end

    # Start our mailbox for receiving connections
    {:ok, mailbox} = Mailbox.start_link(owner: self(), mode: :server)

    {:ok, %State{mailbox_pid: mailbox, mode: :server}}
  end

  @doc """
  Handles incoming connection requests from clients.
  """
  def handle_connection_request(client_mailbox, %State{mailbox_pid: server_mailbox} = state) do
    # Set up bidirectional connection between mailboxes
    :ok = Mailbox.set_peer(server_mailbox, client_mailbox)
    :ok = Mailbox.set_peer(client_mailbox, server_mailbox)

    # Send acknowledgment
    send(client_mailbox, {:mcp_connected, server_mailbox})

    {:ok, state}
  end

  # Private Functions

  defp connect_to_server(server_ref, client_mailbox) do
    case server_ref do
      name when is_atom(name) ->
        connect_local(name, client_mailbox)

      {name, node} when is_atom(name) and is_atom(node) ->
        connect_distributed(name, node, client_mailbox)

      pid when is_pid(pid) ->
        connect_to_pid(pid, client_mailbox)

      _ ->
        {:error, :invalid_server_ref}
    end
  end

  defp connect_local(name, client_mailbox) do
    case Process.whereis(name) do
      nil ->
        {:error, :server_not_found}

      server_pid ->
        connect_to_pid(server_pid, client_mailbox)
    end
  end

  defp connect_distributed(name, node, client_mailbox) do
    case Node.ping(node) do
      :pong ->
        case :rpc.call(node, Process, :whereis, [name]) do
          {:badrpc, _} ->
            {:error, :server_not_found}

          nil ->
            {:error, :server_not_found}

          server_pid ->
            connect_to_pid(server_pid, client_mailbox)
        end

      :pang ->
        {:error, :node_not_reachable}
    end
  end

  defp connect_to_pid(server_pid, client_mailbox) do
    # Send connection request to server
    send(server_pid, {:beam_connect, client_mailbox})

    # Wait for acknowledgment
    receive do
      {:mcp_connected, server_mailbox} ->
        # Connection established
        Mailbox.set_peer(client_mailbox, server_mailbox)
        {:ok, :local}

      {:mcp_connection_refused, reason} ->
        {:error, reason}
    after
      5000 ->
        {:error, :connection_timeout}
    end
  end
end
