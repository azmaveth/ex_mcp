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

  @max_mailbox_size 10
  @heartbeat_interval 30_000
  @event_id_buffer_size 1000

  defstruct [
    :conn,
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
          conn: Plug.Conn.t(),
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
  """
  @spec start_link(Plug.Conn.t(), String.t(), map()) :: {:ok, pid()} | {:error, any()}
  def start_link(conn, session_id, opts) do
    GenServer.start_link(__MODULE__, {conn, session_id, opts})
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
    Process.flag(:trap_exit, true)

    # Extract Last-Event-ID if provided
    last_event_id = extract_last_event_id(conn)

    # Send initial connection event
    event_id = generate_event_id(0)

    case send_sse_event(conn, "connected", %{session_id: session_id}, event_id) do
      {:ok, conn} ->
        # Start heartbeat timer
        heartbeat_ref = Process.send_after(self(), :heartbeat, @heartbeat_interval)

        # Initialize state
        state = %__MODULE__{
          conn: conn,
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

    case send_sse_event(state.conn, event_type, data, event_id) do
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
    case send_sse_event(state.conn, "error", error_data, generate_event_id(state.event_counter)) do
      {:ok, conn} ->
        # Send close event
        send_sse_event(
          conn,
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
    case send_sse_event(state.conn, "heartbeat", %{timestamp: System.system_time(:second)}, nil) do
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
  def handle_info({:EXIT, _pid, _reason}, state) do
    # Handle linked process exits
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
        state.conn,
        "close",
        %{reason: "shutdown"},
        generate_event_id(state.event_counter)
      )
    end

    # Reply to any blocked producers
    Enum.each(state.producers, fn pid ->
      GenServer.reply(pid, {:error, :connection_closed})
    end)

    :ok
  end

  # Private Functions

  defp extract_last_event_id(%Plug.Conn{} = conn) do
    case Plug.Conn.get_req_header(conn, "last-event-id") do
      [id | _] -> id
      [] -> nil
    end
  end

  defp extract_last_event_id(conn) do
    # For testing or other conn implementations
    if is_struct(conn) and Map.has_key?(conn, :get_req_header) and
         is_function(Map.get(conn, :get_req_header), 2) do
      get_req_header = Map.get(conn, :get_req_header)

      case get_req_header.(conn, "last-event-id") do
        [id | _] -> id
        [] -> nil
        nil -> nil
      end
    else
      nil
    end
  end

  defp generate_event_id(counter) do
    "#{System.system_time(:microsecond)}-#{counter}"
  end

  defp do_chunk(%Plug.Conn{} = conn, message) do
    Plug.Conn.chunk(conn, message)
  end

  defp do_chunk(conn, message) do
    # For testing or other conn implementations
    if is_struct(conn) and Map.has_key?(conn, :chunk) and is_function(Map.get(conn, :chunk), 2) do
      chunk_fn = Map.get(conn, :chunk)
      chunk_fn.(conn, message)
    else
      {:error, :not_supported}
    end
  end

  defp send_sse_event(conn, event_type, data, event_id) do
    formatted_data = Jason.encode!(data)

    message =
      case event_id do
        nil ->
          "event: #{event_type}\ndata: #{formatted_data}\n\n"

        id ->
          "id: #{id}\nevent: #{event_type}\ndata: #{formatted_data}\n\n"
      end

    case do_chunk(conn, message) do
      {:ok, conn} -> {:ok, conn}
      {:error, reason} -> {:error, reason}
    end
  end

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
      %ExMCP.Error{} = e ->
        %{
          code: e.code,
          message: e.message,
          data: e.data
        }

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
