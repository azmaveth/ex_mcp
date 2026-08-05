defmodule ExMCP.HttpPlug.SSEHandler do
  @moduledoc """
  Server-Sent Events handler with backpressure control.

  This module implements a robust SSE handler that prevents memory leaks
  through demand-based flow control. It ensures that event producers
  cannot overwhelm the SSE connection with unbounded message queues.

  ## Features

  - Demand-based backpressure control
  - Event buffering with size limits
  - Last-Event-ID support for resumption
  - Structured error propagation
  - Connection health monitoring
  - Graceful shutdown

  ## Architecture

  The handler uses a GenServer that manages the SSE connection lifecycle.
  Event producers must request permission before sending events, preventing
  unbounded mailbox growth.
  """

  use GenServer
  require Logger

  alias ExMCP.HttpPlug.SessionRegistry
  alias ExMCP.HttpPlug.SSEConnection

  @max_mailbox_size 10
  @heartbeat_interval 30_000
  @event_id_buffer_size 1000

  defstruct [
    :conn,
    :conn_module,
    :conn_owner,
    :session_id,
    :opts,
    :event_counter,
    :event_buffer,
    :mailbox_monitor,
    :heartbeat_ref,
    :producers,
    :last_event_id
  ]

  @type t :: %__MODULE__{
          conn: Plug.Conn.t() | term(),
          conn_module: module(),
          conn_owner: pid() | nil,
          session_id: String.t(),
          opts: map(),
          event_counter: non_neg_integer(),
          event_buffer: :queue.queue(),
          mailbox_monitor: reference() | nil,
          heartbeat_ref: reference() | nil,
          producers: MapSet.t(pid()),
          last_event_id: String.t() | nil
        }

  @doc """
  Starts an SSE handler for the given connection.

  The calling process is treated as the connection owner: when it exits
  (client disconnect, timeout, crash), the handler shuts down and cleans up
  its session registration.
  """
  @spec start_link(Plug.Conn.t(), String.t(), map()) :: {:ok, pid()} | {:error, any()}
  def start_link(conn, session_id, opts) do
    GenServer.start_link(__MODULE__, {conn, session_id, opts, self()})
  end

  @doc """
  Requests permission to send an event. This implements backpressure.

  The caller will block until the handler is ready to accept more events.
  Returns `:ok` when it's safe to send, or `{:error, reason}` if the
  connection is closed or errored.
  """
  @spec request_send(pid()) :: :ok | {:error, any()}
  def request_send(handler) do
    GenServer.call(handler, :request_send, :infinity)
  end

  @doc """
  Sends an event after permission has been granted.

  This should only be called after `request_send/1` returns `:ok`.
  """
  @spec send_event(pid(), String.t(), any(), keyword()) :: :ok
  def send_event(handler, event_type, data, opts \\ []) do
    GenServer.cast(handler, {:send_event, event_type, data, opts})
  end

  @doc """
  Sends an error event and closes the connection gracefully.
  """
  @spec send_error(pid(), any()) :: :ok
  def send_error(handler, error) do
    GenServer.cast(handler, {:send_error, error})
  end

  @doc """
  Closes the SSE connection gracefully.
  """
  @spec close(pid()) :: :ok
  def close(handler) do
    GenServer.stop(handler, :normal)
  end

  # GenServer Callbacks

  @impl true
  def init({conn, session_id, opts}) do
    init({conn, session_id, opts, nil})
  end

  def init({conn, session_id, opts, conn_owner}) do
    Process.flag(:trap_exit, true)

    # The connection adapter is resolved once, here, so the write path has no
    # runtime branching on the shape of `conn` (audit L6).
    conn_module = SSEConnection.resolve(opts)

    # Extract Last-Event-ID if provided
    last_event_id = extract_last_event_id(conn_module, conn)

    # Send the configured handshake event. Streamable HTTP's legacy GET stream
    # keeps the historical `connected` event, while the deprecated 2024-11-05
    # HTTP+SSE transport must begin with a raw URI in an `endpoint` event.
    {initial_event_type, initial_event_data} =
      Map.get(opts, :initial_sse_event, {"connected", %{session_id: session_id}})

    event_id = generate_event_id(0)

    case send_sse_event(
           conn_module,
           conn,
           initial_event_type,
           initial_event_data,
           event_id
         ) do
      {:ok, conn} ->
        # Start heartbeat timer
        heartbeat_ref = Process.send_after(self(), :heartbeat, @heartbeat_interval)

        # Initialize state
        state = %__MODULE__{
          conn: conn,
          conn_module: conn_module,
          conn_owner: conn_owner,
          session_id: session_id,
          opts: opts,
          event_counter: 1,
          event_buffer: :queue.new(),
          heartbeat_ref: heartbeat_ref,
          producers: MapSet.new(),
          last_event_id: last_event_id
        }

        # If we have a Last-Event-ID, request replay
        if last_event_id do
          request_event_replay(state)
        end

        {:ok, state}

      {:error, reason} ->
        {:stop, {:connection_failed, reason}}
    end
  end

  @impl true
  def handle_call(:request_send, from, state) do
    # Check mailbox size
    {:message_queue_len, queue_len} = Process.info(self(), :message_queue_len)

    cond do
      # Connection is closed or errored
      state.conn == nil ->
        {:reply, {:error, :connection_closed}, state}

      # Mailbox is too full - apply backpressure
      queue_len > @max_mailbox_size ->
        # Don't reply yet - the caller will block
        # We'll reply when the mailbox drains
        state = %{state | producers: MapSet.put(state.producers, from)}
        {:noreply, state}

      # Safe to proceed
      true ->
        {:reply, :ok, state}
    end
  end

  @impl true
  def handle_cast({:send_event, event_type, data, opts}, state) do
    event_id = Keyword.get(opts, :event_id, generate_event_id(state.event_counter))

    case send_sse_event(state, event_type, data, event_id) do
      {:ok, conn} ->
        # Update state
        state = %{state | conn: conn, event_counter: state.event_counter + 1}

        # Store event in buffer for potential replay
        state = buffer_event(state, event_type, data, event_id)

        # Check if we can unblock any waiting producers
        state = maybe_unblock_producers(state)

        {:noreply, state}

      {:error, _reason} ->
        # Connection failed - stop the handler
        {:stop, :normal, %{state | conn: nil}}
    end
  end

  @impl true
  def handle_cast({:send_error, error}, state) do
    error_data = format_error(error)

    # Send error event
    case send_sse_event(state, "error", error_data, generate_event_id(state.event_counter)) do
      {:ok, conn} ->
        # Send close event
        send_sse_event(
          %{state | conn: conn},
          "close",
          %{reason: "error"},
          generate_event_id(state.event_counter + 1)
        )

      _ ->
        :ok
    end

    {:stop, :normal, %{state | conn: nil}}
  end

  @impl true
  def handle_info(:heartbeat, state) do
    # Send heartbeat
    case send_sse_event(state, "heartbeat", %{timestamp: System.system_time(:second)}, nil) do
      {:ok, conn} ->
        # Schedule next heartbeat
        heartbeat_ref = Process.send_after(self(), :heartbeat, @heartbeat_interval)
        {:noreply, %{state | conn: conn, heartbeat_ref: heartbeat_ref}}

      {:error, _reason} ->
        # Connection failed
        {:stop, :normal, %{state | conn: nil}}
    end
  end

  @impl true
  def handle_info(:check_mailbox, state) do
    # Periodic check to unblock producers if mailbox has drained
    state = maybe_unblock_producers(state)
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, pid, _reason}, %__MODULE__{conn_owner: pid} = state) do
    # The request process that owns the SSE socket exited (client disconnect,
    # timeout, crash). The connection is unusable, so stop and let terminate/2
    # clean up the session registration. conn is cleared to avoid writing a
    # close event to a dead socket.
    {:stop, :normal, %{state | conn: nil}}
  end

  @impl true
  def handle_info({:EXIT, _pid, _reason}, state) do
    # Ignore exits from other linked processes
    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.warning("SSE handler received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    # Cancel heartbeat timer
    if state.heartbeat_ref do
      Process.cancel_timer(state.heartbeat_ref)
    end

    # Send close event if connection is still open
    if state.conn do
      send_sse_event(
        state,
        "close",
        %{reason: "shutdown"},
        generate_event_id(state.event_counter)
      )
    end

    # Reply to any blocked producers
    Enum.each(state.producers, fn pid ->
      GenServer.reply(pid, {:error, :connection_closed})
    end)

    # Deregister on every exit path so client-first disconnects cannot leak
    # ETS entries. Scoped to this pid so a newer handler that re-registered
    # the same session id is left untouched.
    if state.session_id do
      SessionRegistry.unregister(state.session_id, self())
    end

    :ok
  end

  # Private Functions

  defp extract_last_event_id(conn_module, conn) do
    case conn_module.get_req_header(conn, "last-event-id") do
      [id | _] -> id
      _none -> nil
    end
  end

  defp generate_event_id(counter) do
    "#{System.system_time(:microsecond)}-#{counter}"
  end

  defp send_sse_event(%__MODULE__{} = state, event_type, data, event_id) do
    send_sse_event(state.conn_module, state.conn, event_type, data, event_id)
  end

  defp send_sse_event(conn_module, conn, event_type, data, event_id) do
    formatted_data = format_event_data(data)

    message =
      case event_id do
        nil ->
          "event: #{event_type}\ndata: #{formatted_data}\n\n"

        id ->
          "id: #{id}\nevent: #{event_type}\ndata: #{formatted_data}\n\n"
      end

    case conn_module.chunk(conn, message) do
      {:ok, conn} -> {:ok, conn}
      {:error, reason} -> {:error, reason}
    end
  end

  defp format_event_data({:raw, data}) when is_binary(data), do: data
  defp format_event_data(data), do: Jason.encode!(data)

  defp buffer_event(state, event_type, data, event_id) do
    # Add to buffer
    event = {event_id, event_type, data}
    buffer = :queue.in(event, state.event_buffer)

    # Trim buffer if too large
    buffer =
      if :queue.len(buffer) > @event_id_buffer_size do
        {_, buffer} = :queue.out(buffer)
        buffer
      else
        buffer
      end

    %{state | event_buffer: buffer}
  end

  defp maybe_unblock_producers(state) do
    {:message_queue_len, queue_len} = Process.info(self(), :message_queue_len)

    if queue_len < @max_mailbox_size and MapSet.size(state.producers) > 0 do
      # Unblock one producer
      case MapSet.to_list(state.producers) do
        [producer | _rest] ->
          GenServer.reply(producer, :ok)
          %{state | producers: MapSet.delete(state.producers, producer)}

        [] ->
          state
      end
    else
      # Schedule a check if we still have blocked producers
      if MapSet.size(state.producers) > 0 do
        Process.send_after(self(), :check_mailbox, 100)
      end

      state
    end
  end

  defp request_event_replay(state) do
    # This would integrate with your session manager to replay events
    # after the given last_event_id
    session_manager = Map.get(state.opts, :session_manager)

    if session_manager && function_exported?(session_manager, :replay_events_after, 3) do
      session_manager.replay_events_after(
        state.session_id,
        state.last_event_id,
        self()
      )
    end
  end

  defp format_error(error) do
    case error do
      %{__struct__: mod} = e
      when mod in [
             ExMCP.Error.ProtocolError,
             ExMCP.Error.TransportError,
             ExMCP.Error.ToolError,
             ExMCP.Error.ResourceError,
             ExMCP.Error.ValidationError
           ] ->
        ExMCP.Error.to_json_rpc(e)

      {type, reason} ->
        %{
          code: -32000,
          message: "#{type}: #{inspect(reason)}",
          data: nil
        }

      reason ->
        %{
          code: -32000,
          message: inspect(reason),
          data: nil
        }
    end
  end
end
