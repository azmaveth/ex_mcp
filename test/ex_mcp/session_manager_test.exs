defmodule ExMCP.SessionManagerTest do
  @moduledoc """
  Tests for ExMCP.SessionManager.

  Tests session lifecycle management, event storage and replay,
  session expiration, and integration with SSE transport.
  """
  use ExUnit.Case, async: true

  alias ExMCP.SessionManager

  setup do
    # Generate a unique name for this test's session manager
    name = :"test_session_manager_#{System.unique_integer([:positive])}"

    # Start a separate session manager for each test
    {:ok, pid} =
      SessionManager.start_link(
        name: name,
        max_events_per_session: 100,
        session_ttl_seconds: 1,
        cleanup_interval_ms: 100
      )

    on_exit(fn ->
      if Process.alive?(pid) do
        GenServer.stop(pid)
      end
    end)

    {:ok, session_manager: pid, session_manager_name: name}
  end

  describe "session lifecycle" do
    test "creates new sessions with unique IDs", %{session_manager_name: name} do
      session_id1 = GenServer.call(name, {:create_session, %{transport: :sse}})
      session_id2 = GenServer.call(name, {:create_session, %{transport: :http}})

      assert is_binary(session_id1)
      assert is_binary(session_id2)
      assert session_id1 != session_id2
    end

    test "stores session metadata", %{session_manager_name: name} do
      metadata = %{
        transport: :sse,
        client_info: %{user_agent: "test-client/1.0", ip: "127.0.0.1"}
      }

      session_id = GenServer.call(name, {:create_session, metadata})
      {:ok, session} = GenServer.call(name, {:get_session, session_id})

      assert session.id == session_id
      assert session.transport == :sse
      assert session.client_info == metadata.client_info
      assert session.status == :active
      assert is_integer(session.created_at)
      assert is_integer(session.last_activity)
      assert session.event_count == 0
    end

    test "updates session metadata and activity", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})
      {:ok, original_session} = GenServer.call(name, {:get_session, session_id})

      # Sleep briefly to ensure timestamp difference
      Process.sleep(1)

      updates = %{client_info: %{version: "2.0"}}
      :ok = GenServer.call(name, {:update_session, session_id, updates})

      {:ok, updated_session} = GenServer.call(name, {:get_session, session_id})
      assert updated_session.client_info == updates.client_info
      assert updated_session.last_activity > original_session.last_activity
    end

    test "terminates sessions", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})
      :ok = GenServer.call(name, {:terminate_session, session_id})

      {:ok, session} = GenServer.call(name, {:get_session, session_id})
      assert session.status == :terminated
    end

    test "handles non-existent sessions gracefully", %{session_manager_name: name} do
      assert {:error, :session_not_found} = GenServer.call(name, {:get_session, "non-existent"})

      assert {:error, :session_not_found} =
               GenServer.call(name, {:update_session, "non-existent", %{}})

      assert :ok = GenServer.call(name, {:terminate_session, "non-existent"})
    end
  end

  describe "event storage and replay" do
    test "stores events for sessions", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})

      event = %{
        id: "event-1",
        session_id: session_id,
        type: "notification",
        data: %{message: "Hello"},
        timestamp: System.system_time(:microsecond)
      }

      assert :ok = GenServer.call(name, {:store_event, session_id, event})

      # Verify session event count is updated
      {:ok, session} = GenServer.call(name, {:get_session, session_id})
      assert session.event_count == 1
    end

    test "stores multiple events and maintains order", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})

      events = [
        %{
          id: "event-1",
          session_id: session_id,
          type: "notification",
          data: %{message: "First"},
          timestamp: System.system_time(:microsecond)
        },
        %{
          id: "event-2",
          session_id: session_id,
          type: "notification",
          data: %{message: "Second"},
          timestamp: System.system_time(:microsecond) + 1000
        },
        %{
          id: "event-3",
          session_id: session_id,
          type: "notification",
          data: %{message: "Third"},
          timestamp: System.system_time(:microsecond) + 2000
        }
      ]

      Enum.each(events, &GenServer.call(name, {:store_event, session_id, &1}))

      # Verify all events can be replayed
      replayed_events = GenServer.call(name, {:replay_events_after, session_id, nil})
      assert length(replayed_events) == 3

      # Events should be in timestamp order
      messages = Enum.map(replayed_events, & &1.data.message)
      assert messages == ["First", "Second", "Third"]
    end

    test "replays events after specific event ID", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})

      # Create events with sortable IDs
      base_time = System.system_time(:microsecond)

      events = [
        %{
          id: "#{base_time}-1",
          session_id: session_id,
          type: "notification",
          data: %{message: "First"},
          timestamp: base_time
        },
        %{
          id: "#{base_time}-2",
          session_id: session_id,
          type: "notification",
          data: %{message: "Second"},
          timestamp: base_time + 1000
        },
        %{
          id: "#{base_time}-3",
          session_id: session_id,
          type: "notification",
          data: %{message: "Third"},
          timestamp: base_time + 2000
        }
      ]

      Enum.each(events, &GenServer.call(name, {:store_event, session_id, &1}))

      # Replay events after the first event
      replayed_events = GenServer.call(name, {:replay_events_after, session_id, "#{base_time}-1"})
      assert length(replayed_events) == 2

      messages = Enum.map(replayed_events, & &1.data.message)
      assert messages == ["Second", "Third"]
    end

    test "handles event replay for non-existent sessions", %{session_manager_name: name} do
      result = GenServer.call(name, {:replay_events_after, "non-existent", nil})
      assert result == {:error, :session_not_found}
    end

    test "prevents storing events for terminated sessions", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})
      :ok = GenServer.call(name, {:terminate_session, session_id})

      event = %{
        id: "event-1",
        session_id: session_id,
        type: "notification",
        data: %{message: "Hello"},
        timestamp: System.system_time(:microsecond)
      }

      assert {:error, :session_not_found} =
               GenServer.call(name, {:store_event, session_id, event})
    end
  end

  describe "SSE handler integration" do
    test "replays events to SSE handler process" do
      # These tests use the global SessionManager since the 3-arg replay_events_after
      # function internally calls the global API
      session_id = SessionManager.create_session(%{transport: :sse})

      # Create a mock SSE handler process
      test_pid = self()

      mock_handler =
        spawn_link(fn ->
          receive do
            {:"$gen_cast", {:send_event, type, data, opts}} ->
              send(test_pid, {:received_event, type, data, opts})
          after
            1000 -> :timeout
          end
        end)

      # Store an event
      event = %{
        id: "event-1",
        session_id: session_id,
        type: "notification",
        data: %{message: "Hello"},
        timestamp: System.system_time(:microsecond)
      }

      SessionManager.store_event(session_id, event)

      # Replay events to the handler (this function exists in the module API)
      :ok = SessionManager.replay_events_after(session_id, nil, mock_handler)

      # Verify the handler received the event
      assert_receive {:received_event, "notification", %{message: "Hello"},
                      [event_id: "event-1"]},
                     1000
    end

    test "handles dead handler processes gracefully" do
      session_id = SessionManager.create_session(%{transport: :sse})

      # Create and kill a handler process
      dead_handler = spawn(fn -> :ok end)
      Process.exit(dead_handler, :kill)
      # Ensure process is dead
      Process.sleep(10)

      # Store an event
      event = %{
        id: "event-1",
        session_id: session_id,
        type: "notification",
        data: %{message: "Hello"},
        timestamp: System.system_time(:microsecond)
      }

      SessionManager.store_event(session_id, event)

      # Should not crash when trying to send to dead process
      assert :ok = SessionManager.replay_events_after(session_id, nil, dead_handler)
    end
  end

  describe "session management" do
    test "lists active sessions", %{session_manager_name: name} do
      session_id1 = GenServer.call(name, {:create_session, %{transport: :sse}})
      session_id2 = GenServer.call(name, {:create_session, %{transport: :http}})
      session_id3 = GenServer.call(name, {:create_session, %{transport: :sse}})

      # Terminate one session
      GenServer.call(name, {:terminate_session, session_id2})

      sessions = GenServer.call(name, :list_sessions)
      active_ids = Enum.map(sessions, & &1.id)

      assert length(sessions) == 2
      assert session_id1 in active_ids
      assert session_id3 in active_ids
      assert session_id2 not in active_ids
    end

    test "provides session statistics", %{session_manager_name: name} do
      # Create some sessions and events
      session_id1 = GenServer.call(name, {:create_session, %{transport: :sse}})
      session_id2 = GenServer.call(name, {:create_session, %{transport: :http}})

      # Add events to first session
      for i <- 1..3 do
        event = %{
          id: "event-#{i}",
          session_id: session_id1,
          type: "notification",
          data: %{counter: i},
          timestamp: System.system_time(:microsecond)
        }

        GenServer.call(name, {:store_event, session_id1, event})
      end

      # Terminate second session
      GenServer.call(name, {:terminate_session, session_id2})

      stats = GenServer.call(name, :get_stats)
      assert stats.total_sessions == 2
      assert stats.active_sessions == 1
      assert stats.total_events == 3
      assert is_integer(stats.memory_usage)
      assert stats.memory_usage > 0
    end
  end

  describe "event limits and cleanup" do
    test "trims old events when limit is exceeded" do
      # Use a small event limit for testing
      name = :"test_event_limit_#{System.unique_integer([:positive])}"
      {:ok, manager} = SessionManager.start_link(name: name, max_events_per_session: 2)

      session_id = GenServer.call(manager, {:create_session, %{transport: :sse}})

      # Store more events than the limit
      events = [
        %{
          id: "event-1",
          session_id: session_id,
          type: "notification",
          data: %{message: "First"},
          timestamp: System.system_time(:microsecond)
        },
        %{
          id: "event-2",
          session_id: session_id,
          type: "notification",
          data: %{message: "Second"},
          timestamp: System.system_time(:microsecond) + 1000
        },
        %{
          id: "event-3",
          session_id: session_id,
          type: "notification",
          data: %{message: "Third"},
          timestamp: System.system_time(:microsecond) + 2000
        }
      ]

      Enum.each(events, &GenServer.call(manager, {:store_event, session_id, &1}))

      # Should only have the latest 2 events
      replayed_events = GenServer.call(manager, {:replay_events_after, session_id, nil})
      assert length(replayed_events) == 2

      messages = Enum.map(replayed_events, & &1.data.message)
      assert messages == ["Second", "Third"]

      GenServer.stop(manager)
    end

    test "cleans up expired sessions" do
      # Use very short TTL for testing
      name = :"test_session_manager_cleanup_#{System.unique_integer([:positive])}"

      {:ok, manager} =
        SessionManager.start_link(
          name: name,
          # Immediate expiration
          session_ttl_seconds: 0,
          cleanup_interval_ms: 50
        )

      session_id = GenServer.call(manager, {:create_session, %{transport: :sse}})

      # Add an event
      event = %{
        id: "event-1",
        session_id: session_id,
        type: "notification",
        data: %{message: "Hello"},
        timestamp: System.system_time(:microsecond)
      }

      GenServer.call(manager, {:store_event, session_id, event})

      # Wait for cleanup to run
      Process.sleep(100)

      # Session should be terminated
      {:ok, session} = GenServer.call(manager, {:get_session, session_id})
      assert session.status == :terminated

      GenServer.stop(manager)
    end
  end

  describe "edge cases" do
    test "handles empty event replay", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})
      events = GenServer.call(name, {:replay_events_after, session_id, nil})
      assert events == []
    end

    test "handles event replay with future event ID", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})

      # Store one event
      event = %{
        id: "#{System.system_time(:microsecond)}-1",
        session_id: session_id,
        type: "notification",
        data: %{message: "Hello"},
        timestamp: System.system_time(:microsecond)
      }

      GenServer.call(name, {:store_event, session_id, event})

      # Request events after a future ID
      future_id = "#{System.system_time(:microsecond) + 10000}-999"
      events = GenServer.call(name, {:replay_events_after, session_id, future_id})
      assert events == []
    end

    test "handles invalid event ID formats", %{session_manager_name: name} do
      session_id = GenServer.call(name, {:create_session, %{transport: :sse}})

      # Store events with non-standard IDs
      events = [
        %{
          id: "custom-event-1",
          session_id: session_id,
          type: "notification",
          data: %{message: "First"},
          timestamp: System.system_time(:microsecond)
        },
        %{
          id: "custom-event-2",
          session_id: session_id,
          type: "notification",
          data: %{message: "Second"},
          timestamp: System.system_time(:microsecond) + 1000
        }
      ]

      Enum.each(events, &GenServer.call(name, {:store_event, session_id, &1}))

      # Should fallback to string comparison
      replayed_events = GenServer.call(name, {:replay_events_after, session_id, "custom-event-1"})
      assert length(replayed_events) == 1
      assert hd(replayed_events).data.message == "Second"
    end
  end
end
