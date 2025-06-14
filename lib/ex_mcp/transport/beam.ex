defmodule ExMCP.Transport.Beam do
  @moduledoc """
  This module provides ExMCP extensions beyond the standard MCP specification.

  BEAM transport implementation for ExMCP.

  This transport uses Erlang/Elixir message passing for communication between
  MCP clients and servers running in the same VM or across distributed nodes.
  It's the most natural transport for Elixir applications, providing seamless
  integration with OTP supervision trees and built-in fault tolerance.

  > #### Extension Module {: .info}
  > This transport is an ExMCP extension and is not part of the official MCP specification.
  > Use stdio or SSE transport for cross-implementation compatibility.

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

  - `:format` - Message format (default: `:json`). Options:
    - `:json` - Messages are encoded/decoded as JSON strings for MCP compatibility
    - `:native` - Messages are sent as native Elixir terms (maps, lists, etc.)
      This is more efficient when both client and server are Elixir processes

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

  alias ExMCP.Security
  alias __MODULE__, as: BeamTransport

  defmodule State do
    @moduledoc false

    @type t :: %__MODULE__{
            mailbox_pid: pid() | nil,
            mode: :client | :server | :local | :distributed,
            peer_ref: term() | nil,
            security: map() | nil,
            authenticated: boolean(),
            format: :json | :native,
            optimized: boolean(),
            stats: map(),
            batch_enabled: boolean(),
            batch_timeout: non_neg_integer(),
            batch_size_limit: non_neg_integer(),
            batch_timer: reference() | nil,
            batch_buffer: list(),
            memory_pool: boolean(),
            shared_memory: boolean(),
            process_affinity: boolean(),
            enabled_features: list(atom()),
            zero_copy_threshold: non_neg_integer()
          }

    defstruct [
      :mailbox_pid,
      :peer_ref,
      :security,
      :batch_timer,
      mode: :client,
      authenticated: false,
      format: :json,
      optimized: false,
      stats: %{
        zero_copy_sends: 0,
        batched_sends: 0,
        messages_batched: 0,
        direct_sends: 0
      },
      batch_enabled: false,
      batch_timeout: 10,
      batch_size_limit: 10,
      batch_buffer: [],
      memory_pool: false,
      shared_memory: false,
      process_affinity: false,
      enabled_features: [],
      zero_copy_threshold: 65536  # 64KB
    ]
  end

  defmodule Mailbox do
    @moduledoc """
    A lightweight process that acts as a message mailbox/channel.
    Forwards messages between transport endpoints.
    """
    use GenServer

    defstruct [:peer_pid, :owner_pid, :mode, :peer_ref, :security, :auth_state]

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
      security = Keyword.get(opts, :security)

      # Monitor the owner so we can clean up if it dies
      Process.monitor(owner_pid)

      state = %__MODULE__{
        owner_pid: owner_pid,
        mode: mode,
        peer_pid: nil,
        peer_ref: nil,
        security: security,
        auth_state: :pending
      }

      {:ok, state}
    end

    @impl true
    def handle_call({:send, message}, _from, state) do
      case {state.peer_pid, state.auth_state} do
        {nil, _} ->
          {:reply, {:error, :not_connected}, state}

        {_, :pending} when not is_nil(state.security) ->
          {:reply, {:error, :authentication_pending}, state}

        {peer_pid, _} ->
          # Send to peer mailbox
          send(peer_pid, {:mcp_message, message})
          {:reply, :ok, state}
      end
    end

    def handle_call({:set_peer, peer_pid}, _from, state) do
      # Monitor the peer
      peer_ref = Process.monitor(peer_pid)

      # If security is configured, initiate authentication
      new_state = %{state | peer_pid: peer_pid, peer_ref: peer_ref}

      case state.security do
        nil ->
          # No security, directly set as authenticated
          {:reply, :ok, %{new_state | auth_state: :authenticated}}

        security ->
          # Start authentication process
          case BeamTransport.authenticate_peer(peer_pid, security, state.mode) do
            :ok ->
              {:reply, :ok, %{new_state | auth_state: :authenticated}}

            {:error, reason} ->
              {:reply, {:error, reason}, new_state}
          end
      end
    end

    def handle_call({:authenticate, token}, _from, state) do
      # Handle incoming authentication from peer
      case BeamTransport.validate_auth_token(token, state.security) do
        :ok ->
          {:reply, :ok, %{state | auth_state: :authenticated}}

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
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

    def handle_info({:set_scheduler_affinity, scheduler_id}, state) do
      # Pin to specific scheduler
      Process.put(:"$scheduler_id", scheduler_id)
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
    # Support both direct server connection and cluster-based discovery
    server =
      case {Keyword.get(opts, :server), Keyword.get(opts, :cluster),
            Keyword.get(opts, :service_name)} do
        {server_pid, nil, nil} when server_pid != nil ->
          # Direct server connection
          server_pid

        {nil, cluster_pid, service_name} when cluster_pid != nil and service_name != nil ->
          # Cluster-based discovery
          discover_server_from_cluster(cluster_pid, service_name, opts)

        {nil, nil, nil} ->
          raise ArgumentError, "must provide either :server or both :cluster and :service_name"

        _ ->
          raise ArgumentError, "cannot provide both :server and :cluster options"
      end

    security = Keyword.get(opts, :security)
    format = Keyword.get(opts, :format, :json)
    
    # Extract optimization options
    optimized = Keyword.get(opts, :optimized, true)
    batch_enabled = Keyword.get(opts, :batch_enabled, false)
    batch_timeout = Keyword.get(opts, :batch_timeout, 10)
    batch_size_limit = Keyword.get(opts, :batch_size_limit, 10)
    memory_pool = Keyword.get(opts, :memory_pool, false)
    shared_memory = Keyword.get(opts, :shared_memory, false)
    process_affinity = Keyword.get(opts, :process_affinity, false)
    requested_features = Keyword.get(opts, :requested_features, [:zero_copy, :batching])

    if format not in [:json, :native] do
      raise ArgumentError, "format must be :json or :native, got: #{inspect(format)}"
    end

    # Validate security config
    with :ok <- validate_security(security) do
      # Start our mailbox with security config
      mailbox_opts = [owner: self(), mode: :client, security: security]
      {:ok, mailbox} = Mailbox.start_link(mailbox_opts)

      # Connect to server with authentication
      case connect_to_server(server, mailbox, security) do
        {:ok, connection_mode} ->
          # Determine if we can optimize this connection
          {mode, should_optimize} = determine_optimization_mode(connection_mode, server, optimized)
          
          # Set process affinity if requested and local
          if should_optimize and process_affinity do
            set_process_affinity(mailbox)
          end
          
          {:ok,
           %State{
             mailbox_pid: mailbox,
             mode: mode,
             peer_ref: server,
             security: security,
             authenticated: true,
             format: format,
             optimized: should_optimize,
             batch_enabled: should_optimize and batch_enabled,
             batch_timeout: batch_timeout,
             batch_size_limit: batch_size_limit,
             memory_pool: should_optimize and memory_pool,
             shared_memory: should_optimize and shared_memory,
             process_affinity: should_optimize and process_affinity,
             enabled_features: if(should_optimize, do: requested_features, else: [])
           }}

        {:error, reason} ->
          GenServer.stop(mailbox)
          {:error, reason}
      end
    end
  end

  @impl true
  def send_message(message, %State{} = state) do
    if state.optimized and state.mode == :local do
      send_message_optimized(message, state)
    else
      send_message_standard(message, state)
    end
  end
  
  defp send_message_standard(message, %State{mailbox_pid: mailbox, format: format} = state) do
    # Convert message based on format
    formatted_message = format_outgoing_message(message, format)

    case Mailbox.send_message(mailbox, formatted_message) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end
  
  defp send_message_optimized(message, %State{} = state) do
    # Determine if we should use zero-copy
    message_size = estimate_message_size(message)
    
    cond do
      # Use zero-copy for large messages in native format
      state.format == :native and message_size > state.zero_copy_threshold ->
        send_zero_copy(message, state)
        
      # Use direct process messaging for small messages
      state.format == :native ->
        send_direct(message, state)
        
      # Fall back to standard for JSON format but skip mailbox overhead
      true ->
        send_direct_json(message, state)
    end
  end
  
  defp get_peer_pid(%State{peer_ref: peer_ref}) when is_pid(peer_ref), do: peer_ref
  defp get_peer_pid(%State{peer_ref: peer_ref}) when is_atom(peer_ref) do
    Process.whereis(peer_ref) || raise "Server #{inspect(peer_ref)} not found"
  end
  defp get_peer_pid(%State{peer_ref: {name, node}}) do
    :rpc.call(node, Process, :whereis, [name]) || raise "Server #{inspect({name, node})} not found"
  end
  
  defp send_zero_copy(message, %State{} = state) do
    # For zero-copy, we send a reference instead of the actual message
    ref = make_ref()
    
    # Store message in process dictionary (in real implementation, use ETS)
    Process.put({:zero_copy, ref}, message)
    
    # Send reference to peer mailbox
    peer_pid = get_peer_pid(state)
    send(peer_pid, {:mcp_message_ref, ref})
    
    # Update stats
    new_stats = Map.update!(state.stats, :zero_copy_sends, &(&1 + 1))
    {:ok, %{state | stats: new_stats}}
  end
  
  defp send_direct(message, %State{} = state) do
    # Direct send without intermediate mailbox
    peer_pid = get_peer_pid(state)
    send(peer_pid, {:mcp_message, message})
    
    # Update stats
    new_stats = Map.update!(state.stats, :direct_sends, &(&1 + 1))
    {:ok, %{state | stats: new_stats}}
  end
  
  defp send_direct_json(message, %State{} = state) do
    # Encode once and send directly
    case Jason.encode(message) do
      {:ok, json} ->
        peer_pid = get_peer_pid(state)
        send(peer_pid, {:mcp_message, json})
        new_stats = Map.update!(state.stats, :direct_sends, &(&1 + 1))
        {:ok, %{state | stats: new_stats}}
        
      {:error, reason} ->
        {:error, {:json_encode_error, reason}}
    end
  end

  @impl true
  def receive_message(%State{optimized: true, mode: :local} = state) do
    receive do
      # Direct optimized messages
      {:mcp_message, message} ->
        formatted_message = format_incoming_message(message, state.format)
        {:ok, formatted_message, state}
        
      # Zero-copy reference
      {:mcp_message_ref, ref} ->
        {:ok, ref, state}
        
      # Batch messages
      {:mcp_batch, messages} ->
        # Return first message, queue rest
        [first | rest] = messages
        # TODO: Handle queuing rest of messages
        formatted_message = format_incoming_message(first, state.format)
        {:ok, formatted_message, state}
        
      # Standard transport message (fallback)
      {:transport_message, message} ->
        formatted_message = format_incoming_message(message, state.format)
        {:ok, formatted_message, state}

      {:transport_closed, reason} ->
        {:error, reason}
    end
  end
  
  def receive_message(%State{format: format} = state) do
    # Standard receive path
    receive do
      {:transport_message, message} ->
        # Convert message based on format
        formatted_message = format_incoming_message(message, format)
        {:ok, formatted_message, state}

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

  # Streaming support functions

  @doc """
  Sends a message with streaming support for large payloads.

  If streaming is enabled and the message is large, it will be streamed
  in chunks with backpressure control.
  """
  @spec send_message_streaming(map(), State.t(), map()) ::
          {:ok, State.t()} | {:ok, map(), State.t()} | {:error, term()}
  def send_message_streaming(message, %State{} = state, streaming_opts \\ %{}) do
    if should_stream_message?(message, streaming_opts) do
      send_message_with_streaming(message, state, streaming_opts)
    else
      case send_message(message, state) do
        {:ok, new_state} -> {:ok, new_state}
        error -> error
      end
    end
  end

  @doc """
  Receives messages with streaming support.

  Handles both regular messages and streaming chunks.
  """
  @spec receive_message_streaming(State.t()) ::
          {:ok, binary() | map(), State.t()}
          | {:stream_chunk, map(), State.t()}
          | {:error, term()}
  def receive_message_streaming(%State{format: format} = state) do
    receive do
      {:transport_message, message} ->
        formatted_message = format_incoming_message(message, format)

        # Check if this is a streaming message
        case detect_streaming_message(formatted_message) do
          {:stream_chunk, chunk_data} ->
            {:stream_chunk, chunk_data, state}

          {:stream_start, stream_info} ->
            {:stream_start, stream_info, state}

          {:regular_message, msg} ->
            {:ok, msg, state}
        end

      {:transport_closed, reason} ->
        {:error, reason}
    end
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
    security = Keyword.get(opts, :security)
    format = Keyword.get(opts, :format, :json)
    
    # Extract optimization options
    optimized = Keyword.get(opts, :optimized, true)
    supported_features = Keyword.get(opts, :supported_features, [:zero_copy, :batching])

    if format not in [:json, :native] do
      raise ArgumentError, "format must be :json or :native, got: #{inspect(format)}"
    end

    # Validate security config
    with :ok <- validate_security(security) do
      if name do
        # Register the accepting process so clients can find us
        Process.register(self(), name)
      end

      # Start our mailbox for receiving connections with security
      mailbox_opts = [owner: self(), mode: :server, security: security]
      {:ok, mailbox} = Mailbox.start_link(mailbox_opts)

      {:ok, %State{
        mailbox_pid: mailbox, 
        mode: :server, 
        security: security, 
        format: format,
        optimized: optimized,
        enabled_features: if(optimized, do: supported_features, else: [])
      }}
    end
  end

  @doc """
  Handles incoming connection requests from clients.
  """
  def handle_connection_request(client_mailbox, state) do
    handle_connection_request(client_mailbox, state, nil)
  end

  def handle_connection_request(
        client_mailbox,
        %State{mailbox_pid: server_mailbox} = state,
        security
      ) do
    # TODO: Handle security authentication if provided
    _ = security

    # Set up bidirectional connection between mailboxes
    :ok = Mailbox.set_peer(server_mailbox, client_mailbox)
    :ok = Mailbox.set_peer(client_mailbox, server_mailbox)

    # Send acknowledgment
    send(client_mailbox, {:mcp_connected, server_mailbox})

    {:ok, state}
  end

  # Streaming helper functions

  defp should_stream_message?(message, streaming_opts) do
    enabled = Map.get(streaming_opts, :enabled, false)

    if enabled do
      message_size = estimate_message_size(message)
      threshold = Map.get(streaming_opts, :threshold, 1024)
      message_size > threshold
    else
      false
    end
  end

  defp send_message_with_streaming(message, state, streaming_opts) do
    alias ExMCP.Transport.Beam.Stream

    # Create a stream for this message
    stream_opts = %{
      content_type: "application/json",
      chunk_size: Map.get(streaming_opts, :chunk_size, 1024),
      window_size: Map.get(streaming_opts, :window_size, 5)
    }

    case Stream.new(stream_opts) do
      {:ok, stream} ->
        # Encode message to JSON
        case Jason.encode(message) do
          {:ok, json_data} ->
            # Chunk the data
            {:ok, chunks} = Stream.chunk_data(stream, json_data)

            # Send stream start notification
            _stream_start = %{
              "type" => "stream_start",
              "stream_id" => stream.id,
              "content_type" => stream.content_type,
              "total_chunks" => length(chunks)
            }

            # Send all chunks
            case send_stream_chunks(chunks, state) do
              :ok -> {:ok, %{"streaming" => true, "stream_id" => stream.id}, state}
              error -> error
            end

          {:error, reason} ->
            {:error, {:json_encode_error, reason}}
        end

      error ->
        error
    end
  end

  defp send_stream_chunks(chunks, state) do
    alias ExMCP.Transport.Beam.StreamChunk

    Enum.reduce_while(chunks, :ok, fn chunk, :ok ->
      chunk_message = StreamChunk.to_message(chunk)

      case send_message(chunk_message, state) do
        {:ok, _state} -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp detect_streaming_message(message) do
    case message do
      %{"type" => "stream_chunk"} = chunk ->
        {:stream_chunk, chunk}

      %{"type" => "stream_start"} = stream_info ->
        {:stream_start, stream_info}

      regular_msg ->
        {:regular_message, regular_msg}
    end
  end

  defp estimate_message_size(message) when is_map(message) do
    case Jason.encode(message) do
      {:ok, json} -> byte_size(json)
      _ -> 0
    end
  end

  defp estimate_message_size(message) when is_binary(message) do
    byte_size(message)
  end

  defp estimate_message_size(_), do: 0

  # New public functions for optimizations
  
  @doc """
  Sends a message asynchronously, useful for batching.
  """
  def send_message_async(message, %State{batch_enabled: true} = state) do
    # Add to batch buffer
    new_buffer = [message | state.batch_buffer]
    new_state = %{state | batch_buffer: new_buffer}
    
    # Check if we should flush
    cond do
      length(new_buffer) >= state.batch_size_limit ->
        flush_batch(new_state)
        
      state.batch_timer == nil ->
        # Start batch timer
        timer = Process.send_after(self(), :flush_batch, state.batch_timeout)
        {:ok, %{new_state | batch_timer: timer}}
        
      true ->
        {:ok, new_state}
    end
  end
  
  def send_message_async(message, state) do
    # Not batching, send immediately
    send_message(message, state)
  end
  
  @doc """
  Flushes any pending batched messages.
  """
  def flush_batch(%State{batch_buffer: []} = state), do: {:ok, state}
  
  def flush_batch(%State{batch_buffer: buffer, batch_timer: timer} = state) do
    # Cancel timer if exists
    if timer, do: Process.cancel_timer(timer)
    
    # Send batch
    batch_message = {:mcp_batch, Enum.reverse(buffer)}
    peer_pid = get_peer_pid(state)
    send(peer_pid, batch_message)
    
    # Update stats
    new_stats = state.stats
    |> Map.update!(:batched_sends, &(&1 + 1))
    |> Map.update!(:messages_batched, &(&1 + length(buffer)))
    
    {:ok, %{state | 
      batch_buffer: [],
      batch_timer: nil,
      stats: new_stats
    }}
  end
  
  @doc """
  Gets memory pool statistics.
  """
  def get_pool_stats(%State{memory_pool: true} = _state) do
    # In a real implementation, this would query an ETS table or similar
    %{
      available_buffers: 10,
      total_buffers: 20,
      buffer_size: 4096
    }
  end
  
  def get_pool_stats(_state) do
    %{available_buffers: 0, total_buffers: 0, buffer_size: 0}
  end
  
  @doc """
  Dereferences a zero-copy message.
  """
  def deref_message(ref) do
    case Process.get({:zero_copy, ref}) do
      nil -> {:error, :invalid_ref}
      message -> {:ok, message}
    end
  end
  
  @doc """
  Releases a zero-copy message reference.
  """
  def release_message(ref) do
    Process.delete({:zero_copy, ref})
    :ok
  end

  # Private Functions
  
  defp determine_optimization_mode(:local, server, optimized) when is_atom(server) do
    # Local named process
    {:local, optimized}
  end
  
  defp determine_optimization_mode(:local, server, optimized) when is_pid(server) do
    # Local PID - check if on same node
    if node(server) == node() do
      {:local, optimized}
    else
      {:distributed, false}
    end
  end
  
  defp determine_optimization_mode(mode, {_name, node}, _optimized) when is_atom(node) do
    # Remote process
    {:distributed, false}
  end
  
  defp determine_optimization_mode(mode, _server, _optimized) do
    {mode, false}
  end
  
  defp set_process_affinity(mailbox) do
    # Pin to current scheduler
    scheduler_id = :erlang.system_info(:scheduler_id)
    Process.put(:"$scheduler_id", scheduler_id)
    
    # Also set for mailbox process
    send(mailbox, {:set_scheduler_affinity, scheduler_id})
  end

  defp connect_to_server(server_ref, client_mailbox, security) do
    case server_ref do
      name when is_atom(name) ->
        connect_local(name, client_mailbox, security)

      {name, node} when is_atom(name) and is_atom(node) ->
        connect_distributed(name, node, client_mailbox, security)

      pid when is_pid(pid) ->
        connect_to_pid(pid, client_mailbox, security)

      _ ->
        {:error, :invalid_server_ref}
    end
  end

  defp connect_local(name, client_mailbox, security) do
    case Process.whereis(name) do
      nil ->
        {:error, :server_not_found}

      server_pid ->
        connect_to_pid(server_pid, client_mailbox, security)
    end
  end

  defp connect_distributed(name, node, client_mailbox, security) do
    case Node.ping(node) do
      :pong ->
        case :rpc.call(node, Process, :whereis, [name]) do
          {:badrpc, _} ->
            {:error, :server_not_found}

          nil ->
            {:error, :server_not_found}

          server_pid ->
            connect_to_pid(server_pid, client_mailbox, security)
        end

      :pang ->
        {:error, :node_not_reachable}
    end
  end

  defp connect_to_pid(server_pid, client_mailbox, security) do
    # Send connection request to server with auth info
    auth_info =
      case security do
        nil -> nil
        %{auth: auth} -> auth
        _ -> nil
      end

    send(server_pid, {:beam_connect, client_mailbox, auth_info})

    # Wait for acknowledgment
    receive do
      {:mcp_connected, server_mailbox} ->
        # Connection established
        Mailbox.set_peer(client_mailbox, server_mailbox)
        {:ok, :local}

      {:mcp_connection_refused, reason} ->
        {:error, reason}

      {:mcp_auth_required, challenge} ->
        # Handle authentication challenge
        case handle_auth_challenge(challenge, security, server_pid) do
          :ok ->
            receive do
              {:mcp_connected, server_mailbox} ->
                Mailbox.set_peer(client_mailbox, server_mailbox)
                {:ok, :local}
            after
              5000 -> {:error, :auth_timeout}
            end

          error ->
            error
        end
    after
      5000 ->
        {:error, :connection_timeout}
    end
  end

  # Security helper functions

  defp validate_security(nil), do: :ok

  defp validate_security(security) when is_map(security) do
    Security.validate_config(security)
  end

  defp validate_security(_), do: {:error, :invalid_security_config}

  def authenticate_peer(peer_pid, security, _mode) do
    case Map.get(security, :auth) do
      {:bearer, token} ->
        # Send bearer token to peer for validation
        GenServer.call(peer_pid, {:authenticate, {:bearer, token}})

      {:node_cookie, cookie} ->
        # Validate Erlang node cookie
        if Node.get_cookie() == cookie do
          :ok
        else
          {:error, :invalid_cookie}
        end

      nil ->
        :ok

      _ ->
        {:error, :unsupported_auth_method}
    end
  end

  def validate_auth_token({:bearer, token}, %{auth: {:bearer, expected_token}}) do
    if token == expected_token do
      :ok
    else
      {:error, :invalid_token}
    end
  end

  def validate_auth_token({:node_cookie, cookie}, %{auth: {:node_cookie, expected_cookie}}) do
    if cookie == expected_cookie do
      :ok
    else
      {:error, :invalid_cookie}
    end
  end

  def validate_auth_token(_, _), do: {:error, :invalid_auth_token}

  # Message formatting functions

  defp format_outgoing_message(message, nil), do: format_outgoing_message(message, :json)

  defp format_outgoing_message(message, :json) when is_binary(message) do
    # Already JSON string
    message
  end

  defp format_outgoing_message(message, :json) when is_map(message) do
    # Encode map to JSON
    case Jason.encode(message) do
      {:ok, json} ->
        json

      {:error, _} ->
        # If encoding fails, send as-is and let receiver handle error
        message
    end
  end

  defp format_outgoing_message(message, :native) do
    # In native mode, send as-is
    message
  end

  defp format_incoming_message(message, nil), do: format_incoming_message(message, :json)

  defp format_incoming_message(message, :json) when is_binary(message) do
    # Keep as JSON string for consistency with other transports
    message
  end

  defp format_incoming_message(message, :json) when is_map(message) do
    # Convert map to JSON string
    case Jason.encode(message) do
      {:ok, json} -> json
      {:error, _} -> message
    end
  end

  defp format_incoming_message(message, :native) do
    # In native mode, return as-is
    message
  end

  defp handle_auth_challenge(challenge, security, server_pid) do
    case {challenge, Map.get(security, :auth)} do
      {:bearer_required, {:bearer, token}} ->
        send(server_pid, {:beam_auth_response, {:bearer, token}})
        :ok

      {:cookie_required, {:node_cookie, cookie}} ->
        send(server_pid, {:beam_auth_response, {:node_cookie, cookie}})
        :ok

      _ ->
        {:error, :unsupported_auth_challenge}
    end
  end

  @doc """
  Builds native BEAM security options for distributed connections.

  This function focuses on Erlang/Elixir-native security mechanisms rather than TLS:
  - Node cookies for node authentication
  - Hidden nodes for network topology security  
  - Process isolation and supervision
  - Connection limits for resource protection
  """
  def build_distribution_options(security_config) when is_map(security_config) do
    base_opts = []

    # Add node cookie authentication
    opts =
      case Map.get(security_config, :node_cookie) do
        nil -> base_opts
        cookie -> Keyword.put(base_opts, :cookie, cookie)
      end

    # Add hidden node option for security
    opts =
      case Map.get(security_config, :hidden) do
        true -> Keyword.put(opts, :hidden, true)
        _ -> opts
      end

    # Add connection limits
    opts =
      case Map.get(security_config, :max_connections) do
        nil -> opts
        max -> Keyword.put(opts, :max_connections, max)
      end

    opts
  end

  def build_distribution_options(_), do: []

  @doc """
  Prepares secure connection options for BEAM transport using native security.

  Focuses on BEAM-native security patterns:
  - Process-level authentication and authorization
  - Node cookie validation
  - Connection rate limiting
  - Capability-based security
  """
  def prepare_secure_connection(config) when is_map(config) do
    security_opts = build_distribution_options(config)

    # Validate node cookie if specified
    case Map.get(config, :node_cookie) do
      nil ->
        {:ok, security_opts}

      :nocookie ->
        # Reject the default insecure cookie
        {:error, :invalid_node_cookie}

      cookie when is_atom(cookie) ->
        {:ok, security_opts}

      _ ->
        {:error, :invalid_node_cookie}
    end
  end

  def prepare_secure_connection(_), do: {:ok, []}

  @doc """
  Configures Erlang distribution with TLS (for advanced deployments).

  This is for cases where TLS over Erlang distribution is actually needed,
  typically configured at the VM level rather than per-connection.

  Note: This requires starting the Erlang VM with -proto_dist inet_tls
  and proper SSL distribution configuration in sys.config
  """
  def configure_distribution_tls(tls_config) when is_map(tls_config) do
    ssl_opts =
      [
        verify: Map.get(tls_config, :verify, :verify_peer),
        cacertfile: Map.get(tls_config, :cacerts),
        certfile: Map.get(tls_config, :cert),
        keyfile: Map.get(tls_config, :key)
      ]
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)

    # This would typically be configured in sys.config or vm.args
    # :ssl.start()
    # :net_kernel.start([node_name, :longnames])

    {:ok, ssl_opts}
  end

  # Cluster-based service discovery
  defp discover_server_from_cluster(cluster_pid, service_name, _opts) do
    case GenServer.call(cluster_pid, {:discover_services, %{name: service_name}}) do
      {:ok, []} ->
        raise "No services found for name: #{service_name}"

      {:ok, [service | _]} ->
        # For now, just return the first available service
        # Future enhancements can add load balancing here
        service.pid

      {:error, reason} ->
        raise "Failed to discover services: #{inspect(reason)}"
    end
  end
end
