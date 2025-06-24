defmodule ExMCP.SessionManager do
  @moduledoc """
  Session management for streamable HTTP connections.

  This module provides session management capabilities for MCP servers
  using streamable HTTP transports like Server-Sent Events (SSE).
  It handles session lifecycle, event buffering, and session resumption
  through Last-Event-ID support.

  ## Features

  - Session lifecycle management (create, update, terminate)
  - Event buffering and replay for connection resumption
  - Last-Event-ID support for seamless reconnection
  - Session expiration and cleanup
  - Memory-efficient event storage with configurable limits
  - Session health monitoring and metrics

  ## Session Lifecycle

  1. **Session Creation**: New sessions are created when SSE connections are established
  2. **Event Storage**: Events are buffered with unique IDs for potential replay
  3. **Session Resumption**: Clients can reconnect using Last-Event-ID header
  4. **Session Termination**: Sessions are terminated on explicit DELETE or timeout

  ## Configuration

  - `:max_events_per_session` - Maximum events to buffer per session (default: 1000)
  - `:session_ttl_seconds` - Session TTL in seconds (default: 3600)
  - `:cleanup_interval_ms` - Cleanup interval in milliseconds (default: 60000)
  - `:storage_backend` - Storage backend (`:ets` or `:persistent_term`, default: `:ets`)

  ## Usage

      # Start the session manager
      {:ok, _pid} = ExMCP.SessionManager.start_link([])

      # Create a new session
      session_id = ExMCP.SessionManager.create_session(%{
        transport: :sse,
        client_info: %{user_agent: "my-client/1.0"}
      })

      # Store an event
      ExMCP.SessionManager.store_event(session_id, %{
        id: "event-123",
        type: "notification",
        data: %{message: "Hello"},
        timestamp: System.system_time(:microsecond)
      })

      # Replay events after a specific event ID
      events = ExMCP.SessionManager.replay_events_after(session_id, "event-122")

      # Terminate session
      ExMCP.SessionManager.terminate_session(session_id)
  """

  use GenServer
  require Logger

  # Default configuration
  @default_max_events 1000
  @default_session_ttl 3600
  @default_cleanup_interval 60_000
  @default_storage_backend :ets

  # ETS table names
  @sessions_table :session_manager_sessions
  @events_table :session_manager_events

  defstruct [
    :sessions_table,
    :events_table,
    :config,
    :cleanup_timer
  ]

  @type session_id :: String.t()
  @type event_id :: String.t()
  @type session_data :: %{
          id: session_id(),
          transport: atom(),
          client_info: map(),
          created_at: integer(),
          last_activity: integer(),
          event_count: non_neg_integer(),
          status: :active | :terminated
        }
  @type event_data :: %{
          id: event_id(),
          session_id: session_id(),
          type: String.t(),
          data: term(),
          timestamp: integer()
        }
  @type config :: %{
          max_events_per_session: pos_integer(),
          session_ttl_seconds: pos_integer(),
          cleanup_interval_ms: pos_integer(),
          storage_backend: :ets | :persistent_term
        }

  ## Public API

  @doc """
  Starts the session manager with optional configuration.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Creates a new session with the given metadata.

  Returns a unique session ID that can be used for subsequent operations.
  """
  @spec create_session(map()) :: session_id()
  def create_session(metadata \\ %{}) do
    GenServer.call(__MODULE__, {:create_session, metadata})
  end

  @doc """
  Stores an event for the given session.

  Events are stored with their ID, type, data, and timestamp for potential
  replay during session resumption.
  """
  @spec store_event(session_id(), event_data()) :: :ok | {:error, :session_not_found}
  def store_event(session_id, event_data) do
    GenServer.call(__MODULE__, {:store_event, session_id, event_data})
  end

  @doc """
  Replays events for a session after the given event ID.

  This is used when clients reconnect with a Last-Event-ID header
  to resume from where they left off.
  """
  @spec replay_events_after(session_id(), event_id() | nil) ::
          [event_data()] | {:error, :session_not_found}
  def replay_events_after(session_id, last_event_id \\ nil) do
    GenServer.call(__MODULE__, {:replay_events_after, session_id, last_event_id})
  end

  @doc """
  Replays events for a session after the given event ID and sends them to the handler.

  This is the callback function referenced in SSEHandler for session replay.
  """
  @spec replay_events_after(session_id(), event_id() | nil, pid()) ::
          :ok | {:error, :session_not_found}
  def replay_events_after(session_id, last_event_id, handler_pid) do
    case replay_events_after(session_id, last_event_id) do
      {:error, reason} ->
        {:error, reason}

      events when is_list(events) ->
        # Send events to the SSE handler
        Enum.each(events, fn event ->
          if Process.alive?(handler_pid) do
            # Use GenServer.cast to send the event to the SSE handler
            GenServer.cast(
              handler_pid,
              {:send_event, event.type, event.data, [event_id: event.id]}
            )
          end
        end)

        :ok
    end
  end

  @doc """
  Updates session metadata or activity timestamp.
  """
  @spec update_session(session_id(), map()) :: :ok | {:error, :session_not_found}
  def update_session(session_id, updates) do
    GenServer.call(__MODULE__, {:update_session, session_id, updates})
  end

  @doc """
  Terminates a session and cleans up its events.

  This should be called when SSE connections are explicitly closed
  or when DELETE requests are received.
  """
  @spec terminate_session(session_id()) :: :ok
  def terminate_session(session_id) do
    GenServer.call(__MODULE__, {:terminate_session, session_id})
  end

  @doc """
  Gets session information.
  """
  @spec get_session(session_id()) :: {:ok, session_data()} | {:error, :session_not_found}
  def get_session(session_id) do
    GenServer.call(__MODULE__, {:get_session, session_id})
  end

  @doc """
  Lists all active sessions.
  """
  @spec list_sessions() :: [session_data()]
  def list_sessions do
    GenServer.call(__MODULE__, :list_sessions)
  end

  @doc """
  Gets session statistics.
  """
  @spec get_stats() :: %{
          total_sessions: non_neg_integer(),
          active_sessions: non_neg_integer(),
          total_events: non_neg_integer(),
          memory_usage: non_neg_integer()
        }
  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  ## GenServer Callbacks

  @impl true
  def init(opts) do
    # Build configuration
    config = %{
      max_events_per_session: Keyword.get(opts, :max_events_per_session, @default_max_events),
      session_ttl_seconds: Keyword.get(opts, :session_ttl_seconds, @default_session_ttl),
      cleanup_interval_ms: Keyword.get(opts, :cleanup_interval_ms, @default_cleanup_interval),
      storage_backend: Keyword.get(opts, :storage_backend, @default_storage_backend)
    }

    # Create ETS tables for session and event storage
    sessions_table = :ets.new(@sessions_table, [:set, :public, :named_table])
    events_table = :ets.new(@events_table, [:ordered_set, :public, :named_table])

    # Start cleanup timer
    cleanup_timer =
      Process.send_after(self(), :cleanup_expired_sessions, config.cleanup_interval_ms)

    state = %__MODULE__{
      sessions_table: sessions_table,
      events_table: events_table,
      config: config,
      cleanup_timer: cleanup_timer
    }

    Logger.info("SessionManager started with config: #{inspect(config)}")
    {:ok, state}
  end

  @impl true
  def handle_call({:create_session, metadata}, _from, state) do
    session_id = generate_session_id()
    now = System.system_time(:microsecond)

    session_data = %{
      id: session_id,
      transport: Map.get(metadata, :transport, :http),
      client_info: Map.get(metadata, :client_info, %{}),
      created_at: now,
      last_activity: now,
      event_count: 0,
      status: :active
    }

    :ets.insert(state.sessions_table, {session_id, session_data})

    Logger.debug("Created session #{session_id} with transport #{session_data.transport}")
    {:reply, session_id, state}
  end

  @impl true
  def handle_call({:store_event, session_id, event_data}, _from, state) do
    case :ets.lookup(state.sessions_table, session_id) do
      [{^session_id, session}] when session.status == :active ->
        # Store the event
        event_key = {session_id, event_data.id}
        :ets.insert(state.events_table, {event_key, event_data})

        # Update session activity and event count
        updated_session = %{
          session
          | last_activity: System.system_time(:microsecond),
            event_count: session.event_count + 1
        }

        :ets.insert(state.sessions_table, {session_id, updated_session})

        # Trim old events if we exceed the limit
        trim_old_events(state, session_id, updated_session.event_count)

        {:reply, :ok, state}

      _ ->
        {:reply, {:error, :session_not_found}, state}
    end
  end

  @impl true
  def handle_call({:replay_events_after, session_id, last_event_id}, _from, state) do
    case :ets.lookup(state.sessions_table, session_id) do
      [{^session_id, _session}] ->
        events = get_events_after(state, session_id, last_event_id)
        {:reply, events, state}

      [] ->
        {:reply, {:error, :session_not_found}, state}
    end
  end

  @impl true
  def handle_call({:update_session, session_id, updates}, _from, state) do
    case :ets.lookup(state.sessions_table, session_id) do
      [{^session_id, session}] ->
        # Update session with new data
        updated_session =
          session
          |> Map.merge(updates)
          |> Map.put(:last_activity, System.system_time(:microsecond))

        :ets.insert(state.sessions_table, {session_id, updated_session})
        {:reply, :ok, state}

      [] ->
        {:reply, {:error, :session_not_found}, state}
    end
  end

  @impl true
  def handle_call({:terminate_session, session_id}, _from, state) do
    # Mark session as terminated
    case :ets.lookup(state.sessions_table, session_id) do
      [{^session_id, session}] ->
        terminated_session = %{session | status: :terminated}
        :ets.insert(state.sessions_table, {session_id, terminated_session})

        # Clean up events for this session
        cleanup_session_events(state, session_id)

        Logger.debug("Terminated session #{session_id}")

      [] ->
        :ok
    end

    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:get_session, session_id}, _from, state) do
    case :ets.lookup(state.sessions_table, session_id) do
      [{^session_id, session}] ->
        {:reply, {:ok, session}, state}

      [] ->
        {:reply, {:error, :session_not_found}, state}
    end
  end

  @impl true
  def handle_call(:list_sessions, _from, state) do
    sessions =
      :ets.tab2list(state.sessions_table)
      |> Enum.map(fn {_id, session} -> session end)
      |> Enum.filter(&(&1.status == :active))

    {:reply, sessions, state}
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    sessions = :ets.tab2list(state.sessions_table)
    active_sessions = Enum.count(sessions, fn {_id, session} -> session.status == :active end)
    total_events = :ets.info(state.events_table, :size)

    memory_usage =
      :ets.info(state.sessions_table, :memory) + :ets.info(state.events_table, :memory)

    stats = %{
      total_sessions: length(sessions),
      active_sessions: active_sessions,
      total_events: total_events,
      memory_usage: memory_usage
    }

    {:reply, stats, state}
  end

  @impl true
  def handle_info(:cleanup_expired_sessions, state) do
    cleanup_expired_sessions(state)

    # Schedule next cleanup
    cleanup_timer =
      Process.send_after(self(), :cleanup_expired_sessions, state.config.cleanup_interval_ms)

    {:noreply, %{state | cleanup_timer: cleanup_timer}}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.warning("SessionManager received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    if state.cleanup_timer do
      Process.cancel_timer(state.cleanup_timer)
    end

    # Clean up ETS tables
    :ets.delete(state.sessions_table)
    :ets.delete(state.events_table)

    :ok
  end

  ## Private Functions

  defp generate_session_id do
    :crypto.strong_rand_bytes(16) |> Base.url_encode64(padding: false)
  end

  defp get_events_after(state, session_id, nil) do
    # Return all events for the session if no last_event_id
    pattern = {{session_id, :"$1"}, :"$2"}

    :ets.match(state.events_table, pattern)
    |> Enum.map(fn [_event_id, event_data] -> event_data end)
    |> Enum.sort_by(& &1.timestamp)
  end

  defp get_events_after(state, session_id, last_event_id) do
    # Find all events after the given event ID
    pattern = {{session_id, :"$1"}, :"$2"}

    :ets.match(state.events_table, pattern)
    |> Enum.map(fn [_event_id, event_data] -> event_data end)
    |> Enum.filter(fn event ->
      # Compare event IDs - this assumes lexicographic ordering
      # or custom comparison logic based on your event ID format
      compare_event_ids(event.id, last_event_id) == :gt
    end)
    |> Enum.sort_by(& &1.timestamp)
  end

  defp compare_event_ids(event_id1, event_id2) do
    # Parse event IDs to compare them properly
    # Assumes format like "timestamp-counter" from SSEHandler
    case {parse_event_id(event_id1), parse_event_id(event_id2)} do
      {{ts1, counter1}, {ts2, counter2}} ->
        cond do
          ts1 > ts2 -> :gt
          ts1 < ts2 -> :lt
          counter1 > counter2 -> :gt
          counter1 < counter2 -> :lt
          true -> :eq
        end

      _ ->
        # Fallback to string comparison
        cond do
          event_id1 > event_id2 -> :gt
          event_id1 < event_id2 -> :lt
          true -> :eq
        end
    end
  end

  defp parse_event_id(event_id) do
    case String.split(event_id, "-", parts: 2) do
      [timestamp_str, counter_str] ->
        with {timestamp, ""} <- Integer.parse(timestamp_str),
             {counter, ""} <- Integer.parse(counter_str) do
          {timestamp, counter}
        else
          _ -> nil
        end

      _ ->
        nil
    end
  end

  defp trim_old_events(state, session_id, event_count) do
    if event_count > state.config.max_events_per_session do
      # Find all events for this session
      pattern = {{session_id, :"$1"}, :"$2"}

      events =
        :ets.match(state.events_table, pattern)
        |> Enum.map(fn [event_id, event_data] -> {event_id, event_data} end)
        |> Enum.sort_by(fn {_id, event_data} -> event_data.timestamp end)

      # Calculate how many events to remove
      events_to_remove = event_count - state.config.max_events_per_session

      # Remove the oldest events
      events
      |> Enum.take(events_to_remove)
      |> Enum.each(fn {event_id, _event_data} ->
        :ets.delete(state.events_table, {session_id, event_id})
      end)
    end
  end

  defp cleanup_session_events(state, session_id) do
    # Delete all events for the session
    pattern = {{session_id, :"$1"}, :_}

    :ets.match(state.events_table, pattern)
    |> Enum.each(fn [event_id] ->
      :ets.delete(state.events_table, {session_id, event_id})
    end)
  end

  defp cleanup_expired_sessions(state) do
    now = System.system_time(:microsecond)
    ttl_microseconds = state.config.session_ttl_seconds * 1_000_000

    expired_sessions =
      :ets.tab2list(state.sessions_table)
      |> Enum.filter(fn {_id, session} ->
        session.status == :active and now - session.last_activity > ttl_microseconds
      end)

    Enum.each(expired_sessions, fn {session_id, session} ->
      Logger.debug("Cleaning up expired session #{session_id}")

      # Mark as terminated and clean up events
      terminated_session = %{session | status: :terminated}

      :ets.insert(state.sessions_table, {session_id, terminated_session})
      cleanup_session_events(state, session_id)
    end)

    if length(expired_sessions) > 0 do
      Logger.info("Cleaned up #{length(expired_sessions)} expired sessions")
    end
  end
end
