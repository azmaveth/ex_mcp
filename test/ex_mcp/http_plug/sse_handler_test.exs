defmodule ExMCP.HttpPlug.SSEHandlerTest do
  use ExUnit.Case, async: true
  import Mox

  alias ExMCP.HttpPlug.SessionRegistry
  alias ExMCP.HttpPlug.SSEHandler

  setup :verify_on_exit!

  defmodule MockConn do
    defstruct chunks: [], headers: %{}, chunk: nil, get_req_header: nil

    def new(headers \\ %{}) do
      conn = %__MODULE__{headers: headers}

      %{
        conn
        | chunk: fn conn, data ->
            {:ok, %{conn | chunks: conn.chunks ++ [data]}}
          end,
          get_req_header: fn conn, header ->
            Map.get(conn.headers, header, [])
          end
      }
    end
  end

  describe "backpressure control" do
    test "implements backpressure control mechanism" do
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # Verify that the handler module exports the backpressure functions
      assert function_exported?(SSEHandler, :request_send, 1)
      assert function_exported?(SSEHandler, :send_event, 4)

      # Test the basic flow: request permission, then send
      assert :ok = SSEHandler.request_send(handler)
      SSEHandler.send_event(handler, "test", %{data: "hello"})

      # Give time for event to process
      Process.sleep(50)

      # Verify event was sent
      state = :sys.get_state(handler)
      # Connected event + our test event
      assert length(state.conn.chunks) >= 2

      # Verify that the handler tracks producers set for backpressure
      assert Map.has_key?(state, :producers)
      assert is_struct(state.producers, MapSet)

      SSEHandler.close(handler)
    end

    test "unblocks producers when mailbox drains" do
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # First request should succeed immediately
      assert :ok = SSEHandler.request_send(handler)

      # Send an event
      SSEHandler.send_event(handler, "test", %{data: "test"})

      # Wait for event to be processed
      Process.sleep(50)

      # Next request should also succeed
      assert :ok = SSEHandler.request_send(handler)

      SSEHandler.close(handler)
    end

    test "returns error when connection is closed" do
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # Monitor the handler process
      ref = Process.monitor(handler)

      # Close the handler
      SSEHandler.close(handler)

      # Wait for the handler to stop
      assert_receive {:DOWN, ^ref, :process, ^handler, :normal}, 1000

      # Request should return error (process no longer exists)
      assert catch_exit(SSEHandler.request_send(handler))
    end
  end

  describe "SSE event formatting" do
    test "sends events with proper SSE format" do
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # Wait for initial connection event
      Process.sleep(50)

      # Request permission and send event
      assert :ok = SSEHandler.request_send(handler)
      SSEHandler.send_event(handler, "test_event", %{message: "hello"})

      # Wait for processing
      Process.sleep(50)

      # Get the handler state to check chunks
      state = :sys.get_state(handler)
      chunks = state.conn.chunks

      # Should have initial connection event and our test event
      assert length(chunks) >= 2

      # Check format of last event
      last_chunk = List.last(chunks)
      assert last_chunk =~ "event: test_event"
      assert last_chunk =~ "data: {\"message\":\"hello\"}"
      assert last_chunk =~ "id: "

      SSEHandler.close(handler)
    end

    test "sends error events and closes connection" do
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # Send error
      SSEHandler.send_error(handler, {:test_error, "Something went wrong"})

      # Wait for processing
      Process.sleep(100)

      # Handler should have stopped
      refute Process.alive?(handler)
    end
  end

  describe "Last-Event-ID support" do
    test "extracts Last-Event-ID from headers" do
      conn = MockConn.new(%{"last-event-id" => ["event-123"]})
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # Get state to check if last_event_id was extracted
      state = :sys.get_state(handler)
      assert state.last_event_id == "event-123"

      SSEHandler.close(handler)
    end

    test "handles missing Last-Event-ID header" do
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # Get state to check last_event_id is nil
      state = :sys.get_state(handler)
      assert state.last_event_id == nil

      SSEHandler.close(handler)
    end
  end

  describe "heartbeat mechanism" do
    test "sends periodic heartbeats" do
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # Wait for initial event
      Process.sleep(50)
      initial_chunks = length(:sys.get_state(handler).conn.chunks)

      # Trigger heartbeat manually
      send(handler, :heartbeat)
      Process.sleep(50)

      # Should have one more chunk
      final_chunks = length(:sys.get_state(handler).conn.chunks)
      assert final_chunks > initial_chunks

      # Check last chunk is a heartbeat
      last_chunk = List.last(:sys.get_state(handler).conn.chunks)
      assert last_chunk =~ "event: heartbeat"
      assert last_chunk =~ "timestamp"

      SSEHandler.close(handler)
    end
  end

  describe "event buffering" do
    test "buffers events for potential replay" do
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, "test_session", %{})

      # Send multiple events
      for i <- 1..5 do
        assert :ok = SSEHandler.request_send(handler)
        SSEHandler.send_event(handler, "event_#{i}", %{index: i})
        Process.sleep(10)
      end

      # Check buffer contains events
      state = :sys.get_state(handler)
      buffer_size = :queue.len(state.event_buffer)
      assert buffer_size >= 5

      SSEHandler.close(handler)
    end
  end

  describe "session registry cleanup" do
    setup do
      # The registry is normally started by the :ex_mcp application; start it
      # here only when running without the application (e.g. --no-start).
      if Process.whereis(SessionRegistry) == nil do
        start_supervised!(SessionRegistry)
      end

      :ok
    end

    test "conn owner exit stops the handler and removes the ETS entry" do
      session_id = "owner-exit-#{System.unique_integer([:positive])}"
      test_pid = self()

      owner =
        spawn(fn ->
          conn = MockConn.new()
          {:ok, handler} = SSEHandler.start_link(conn, session_id, %{})
          :ok = SessionRegistry.register(session_id, handler)
          send(test_pid, {:handler, handler})

          receive do
            :stop -> :ok
          end
        end)

      assert_receive {:handler, handler}
      assert {:ok, ^handler} = SessionRegistry.lookup(session_id)

      ref = Process.monitor(handler)
      Process.exit(owner, :kill)

      # The handler traps the owner's exit, cleans up in terminate/2, and
      # stops; the DOWN message is delivered only after terminate/2 ran.
      assert_receive {:DOWN, ^ref, :process, ^handler, _reason}, 1000
      assert :ets.lookup(SessionRegistry.table(), session_id) == []
    end

    test "graceful close removes the ETS entry" do
      session_id = "close-#{System.unique_integer([:positive])}"
      conn = MockConn.new()
      {:ok, handler} = SSEHandler.start_link(conn, session_id, %{})
      :ok = SessionRegistry.register(session_id, handler)

      ref = Process.monitor(handler)
      SSEHandler.close(handler)

      assert_receive {:DOWN, ^ref, :process, ^handler, :normal}, 1000
      assert :ets.lookup(SessionRegistry.table(), session_id) == []
    end

    test "terminate does not clobber a newer registration for the same session" do
      session_id = "reregister-#{System.unique_integer([:positive])}"
      conn = MockConn.new()

      {:ok, old_handler} = SSEHandler.start_link(conn, session_id, %{})
      :ok = SessionRegistry.register(session_id, old_handler)

      # A reconnect registers a newer handler pid under the same session id.
      new_handler =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      :ok = SessionRegistry.register(session_id, new_handler)

      ref = Process.monitor(old_handler)
      SSEHandler.close(old_handler)
      assert_receive {:DOWN, ^ref, :process, ^old_handler, :normal}, 1000

      # The newer registration must survive the old handler's cleanup.
      assert {:ok, ^new_handler} = SessionRegistry.lookup(session_id)

      SessionRegistry.unregister(session_id)
      Process.exit(new_handler, :kill)
    end
  end
end
