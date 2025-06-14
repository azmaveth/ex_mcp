defmodule ExMCP.Transport.Beam.Acceptor do
  @moduledoc """
  Ranch-based TCP acceptor for the enhanced BEAM transport.
  
  This module implements the ranch_protocol behaviour to handle incoming
  TCP connections for the BEAM transport. It creates Connection processes
  for each accepted socket and manages the connection lifecycle.
  
  ## Architecture
  
  - Uses ranch for efficient connection pooling and supervision
  - Creates Connection GenServer processes for each socket
  - Handles authentication and security validation
  - Provides backpressure control and connection limits
  
  ## Performance Benefits
  
  - Scalable connection handling with configurable pools
  - Efficient socket management with ranch optimizations
  - Process isolation prevents connection failures from affecting others
  - Built-in supervision and restart strategies
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.Connection

  @behaviour :ranch_protocol

  @default_max_connections 1000
  @default_port 9999

  defstruct [
    :ref,
    :socket,
    :transport,
    :handler_module,
    :handler_state,
    :auth_config,
    :connection_pid
  ]

  @type t :: %__MODULE__{}
  @type acceptor_opts :: [
    port: non_neg_integer(),
    max_connections: pos_integer(),
    handler: module(),
    auth_config: map() | nil,
    transport_opts: keyword()
  ]

  # Ranch Protocol API

  @doc """
  Starts the acceptor listener on the specified port.
  """
  @spec start_listener(atom(), acceptor_opts()) :: {:ok, pid()} | {:error, term()}
  def start_listener(ref, opts \\ []) do
    port = Keyword.get(opts, :port, @default_port)
    max_connections = Keyword.get(opts, :max_connections, @default_max_connections)
    handler = Keyword.fetch!(opts, :handler)
    auth_config = Keyword.get(opts, :auth_config)
    transport_opts = Keyword.get(opts, :transport_opts, [])

    # Ranch transport options
    ranch_opts = %{
      socket_opts: [port: port] ++ transport_opts,
      max_connections: max_connections
    }

    # Protocol options passed to start_link/3
    protocol_opts = [
      handler: handler,
      auth_config: auth_config
    ]

    case :ranch.start_listener(ref, :ranch_tcp, ranch_opts, __MODULE__, protocol_opts) do
      {:ok, listener_pid} ->
        Logger.info("BEAM transport acceptor started on port #{port} (max_connections: #{max_connections})")
        {:ok, listener_pid}
      
      {:error, reason} ->
        Logger.error("Failed to start BEAM transport acceptor: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Stops the acceptor listener.
  """
  @spec stop_listener(atom()) :: :ok | {:error, :not_found}
  def stop_listener(ref) do
    case :ranch.stop_listener(ref) do
      :ok ->
        Logger.info("BEAM transport acceptor #{ref} stopped")
        :ok
      
      {:error, :not_found} = error ->
        Logger.warning("Acceptor #{ref} not found")
        error
    end
  end

  @doc """
  Gets information about the acceptor listener.
  """
  @spec get_info(atom()) :: map() | {:error, :not_found}
  def get_info(ref) do
    try do
      info = :ranch.info(ref)
      %{
        ref: ref,
        status: Map.get(info, :status),
        ip: Map.get(info, :ip),
        port: Map.get(info, :port),
        max_connections: Map.get(info, :max_connections),
        active_connections: Map.get(info, :active_connections),
        all_connections: Map.get(info, :all_connections)
      }
    rescue
      _ -> {:error, :not_found}
    end
  end

  # Ranch protocol behaviour implementation

  @impl :ranch_protocol
  def start_link(ref, transport, opts) do
    GenServer.start_link(__MODULE__, {ref, transport, opts})
  end

  # GenServer callbacks

  @impl true
  def init({ref, transport, opts}) do
    # Get the socket from ranch
    {:ok, socket} = :ranch.handshake(ref)
    
    handler = Keyword.fetch!(opts, :handler)
    auth_config = Keyword.get(opts, :auth_config)

    state = %__MODULE__{
      ref: ref,
      socket: socket,
      transport: transport,
      handler_module: handler,
      auth_config: auth_config
    }

    # Start the connection process
    connection_opts = [
      socket: socket,
      peer_info: get_peer_info(socket),
      auth_config: auth_config,
      format: :etf,
      owner: self()
    ]

    case Connection.start_link(connection_opts) do
      {:ok, connection_pid} ->
        # Transfer socket ownership to connection process
        case transport.controlling_process(socket, connection_pid) do
          :ok ->
            # Initialize handler state
            case handler.init([]) do
              {:ok, handler_state} ->
                final_state = %{state | 
                  connection_pid: connection_pid,
                  handler_state: handler_state
                }
                
                Logger.debug("BEAM transport connection established from #{inspect(state.socket)}")
                {:ok, final_state}
              
              {:error, reason} ->
                Logger.error("Handler initialization failed: #{inspect(reason)}")
                {:stop, {:handler_init_failed, reason}}
            end
          
          {:error, reason} ->
            Logger.error("Failed to transfer socket control: #{inspect(reason)}")
            {:stop, {:socket_transfer_failed, reason}}
        end
      
      {:error, reason} ->
        Logger.error("Failed to start connection process: #{inspect(reason)}")
        {:stop, {:connection_start_failed, reason}}
    end
  end

  @impl true
  def handle_info({:connection_ready, connection_pid}, state) when connection_pid == state.connection_pid do
    Logger.debug("Connection ready, starting message handling")
    {:noreply, state}
  end

  def handle_info({:incoming_request, request, connection_pid}, state) when connection_pid == state.connection_pid do
    # Handle incoming MCP request through the handler
    case apply(state.handler_module, :handle_request, [request, state.handler_state]) do
      {:ok, response, new_handler_state} ->
        # Send response back through connection
        Connection.send_notification(connection_pid, response)
        {:noreply, %{state | handler_state: new_handler_state}}
      
      {:error, error_response, new_handler_state} ->
        # Send error response
        Connection.send_notification(connection_pid, error_response)
        {:noreply, %{state | handler_state: new_handler_state}}
      
      {:stop, reason, new_handler_state} ->
        {:stop, reason, %{state | handler_state: new_handler_state}}
    end
  end

  def handle_info({:incoming_notification, notification}, state) do
    # Handle incoming notification through the handler
    case apply(state.handler_module, :handle_notification, [notification, state.handler_state]) do
      {:ok, new_handler_state} ->
        {:noreply, %{state | handler_state: new_handler_state}}
      
      {:stop, reason, new_handler_state} ->
        {:stop, reason, %{state | handler_state: new_handler_state}}
    end
  end

  def handle_info({:connection_closed, connection_pid}, state) when connection_pid == state.connection_pid do
    Logger.debug("Connection closed")
    {:stop, :normal, state}
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) when pid == state.connection_pid do
    Logger.debug("Connection process died: #{inspect(reason)}")
    {:stop, :normal, state}
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in acceptor: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    Logger.debug("Acceptor terminating: #{inspect(reason)}")
    
    # Cleanup connection process if still alive
    if state.connection_pid && Process.alive?(state.connection_pid) do
      Connection.close(state.connection_pid)
    end
    
    # Cleanup handler state if applicable
    if state.handler_module && function_exported?(state.handler_module, :terminate, 2) do
      state.handler_module.terminate(reason, state.handler_state)
    end
    
    :ok
  end

  # Private helper functions

  defp get_peer_info(socket) do
    case :inet.peername(socket) do
      {:ok, {ip, port}} ->
        %{
          ip: ip,
          port: port,
          ip_string: :inet.ntoa(ip) |> to_string()
        }
      
      {:error, _reason} ->
        %{ip: :unknown, port: :unknown, ip_string: "unknown"}
    end
  end
end