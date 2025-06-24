defmodule ExMCP.SessionManagerBasicTest do
  @moduledoc """
  Basic unit tests for SessionManager that don't require the full application stack.
  """
  use ExUnit.Case, async: false

  alias ExMCP.SessionManager

  test "SessionManager can be started and stopped independently" do
    # Start the session manager independently
    {:ok, pid} =
      SessionManager.start_link(
        max_events_per_session: 10,
        session_ttl_seconds: 60,
        cleanup_interval_ms: 10000
      )

    assert Process.alive?(pid)

    # Test basic functionality
    session_id = GenServer.call(pid, {:create_session, %{transport: :test}})
    assert is_binary(session_id)

    {:ok, session} = GenServer.call(pid, {:get_session, session_id})
    assert session.id == session_id
    assert session.transport == :test
    assert session.status == :active

    # Stop the session manager
    GenServer.stop(pid)
    refute Process.alive?(pid)
  end

  test "SessionManager basic event storage and retrieval" do
    {:ok, pid} =
      SessionManager.start_link(
        max_events_per_session: 5,
        session_ttl_seconds: 60,
        cleanup_interval_ms: 10000
      )

    # Create session
    session_id = GenServer.call(pid, {:create_session, %{transport: :sse}})

    # Store event
    event = %{
      id: "test-event-1",
      session_id: session_id,
      type: "test",
      data: %{message: "hello"},
      timestamp: System.system_time(:microsecond)
    }

    :ok = GenServer.call(pid, {:store_event, session_id, event})

    # Retrieve events
    events = GenServer.call(pid, {:replay_events_after, session_id, nil})
    assert length(events) == 1
    assert hd(events).data.message == "hello"

    GenServer.stop(pid)
  end

  test "SessionManager session termination" do
    {:ok, pid} = SessionManager.start_link([])

    # Create session
    session_id = GenServer.call(pid, {:create_session, %{transport: :sse}})
    {:ok, session} = GenServer.call(pid, {:get_session, session_id})
    assert session.status == :active

    # Terminate session
    :ok = GenServer.call(pid, {:terminate_session, session_id})
    {:ok, session} = GenServer.call(pid, {:get_session, session_id})
    assert session.status == :terminated

    GenServer.stop(pid)
  end
end
