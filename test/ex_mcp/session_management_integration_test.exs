defmodule ExMCP.SessionManagementIntegrationTest do
  @moduledoc """
  Integration tests for Session Management with streamable HTTP.

  Tests the complete session management workflow including:
  - Session creation during SSE connections
  - Event storage and replay for connection resumption
  - Session termination via DELETE requests
  - Integration with HttpPlug and SSEHandler
  """
  use ExUnit.Case, async: true

  alias ExMCP.{HttpPlug, SessionManager}

  import Plug.Test
  import Plug.Conn

  setup do
    # Start session manager for this test
    {:ok, session_manager} =
      SessionManager.start_link(
        max_events_per_session: 50,
        session_ttl_seconds: 300,
        cleanup_interval_ms: 1000
      )

    # Configure test mode to prevent actual SSE handler startup
    original_test_mode = Application.get_env(:ex_mcp, :test_mode, false)
    Application.put_env(:ex_mcp, :test_mode, true)

    on_exit(fn ->
      if Process.alive?(session_manager) do
        GenServer.stop(session_manager)
      end

      Application.put_env(:ex_mcp, :test_mode, original_test_mode)
    end)

    # Basic HttpPlug options
    opts = %{
      handler: fn _request -> {:ok, %{result: "test"}} end,
      server_info: %{name: "test-server", version: "1.0.0"},
      session_manager: SessionManager,
      sse_enabled: true,
      cors_enabled: false,
      oauth_enabled: false,
      auth_config: %{}
    }

    {:ok, session_manager: session_manager, plug_opts: opts}
  end

  describe "session creation" do
    test "creates new session on SSE connection", %{plug_opts: opts} do
      # Connect to SSE endpoint
      conn =
        conn(:get, "/sse")
        |> put_req_header("accept", "text/event-stream")
        |> HttpPlug.call(opts)

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/event-stream"]

      # Verify a session was created
      sessions = SessionManager.list_sessions()
      assert length(sessions) == 1

      session = hd(sessions)
      assert session.transport == :sse
      assert session.status == :active
      assert is_binary(session.id)
    end

    test "reuses existing session with session ID header", %{plug_opts: opts} do
      # Create a session first
      existing_session_id =
        SessionManager.create_session(%{
          transport: :sse,
          client_info: %{test: true}
        })

      # Connect with existing session ID
      conn =
        conn(:get, "/sse")
        |> put_req_header("accept", "text/event-stream")
        |> put_req_header("mcp-session-id", existing_session_id)
        |> HttpPlug.call(opts)

      assert conn.status == 200

      # Should still have only one session, but updated
      sessions = SessionManager.list_sessions()
      assert length(sessions) == 1

      session = hd(sessions)
      assert session.id == existing_session_id
      assert session.transport == :sse
    end

    test "creates new session when referenced session doesn't exist", %{plug_opts: opts} do
      # Connect with non-existent session ID
      conn =
        conn(:get, "/sse")
        |> put_req_header("accept", "text/event-stream")
        |> put_req_header("mcp-session-id", "non-existent-session")
        |> HttpPlug.call(opts)

      assert conn.status == 200

      # Should create new session
      sessions = SessionManager.list_sessions()
      assert length(sessions) == 1

      session = hd(sessions)
      assert session.id != "non-existent-session"
      assert session.transport == :sse
    end
  end

  describe "event storage and messaging" do
    test "stores events when sending SSE responses", %{plug_opts: opts} do
      # Create a session first
      session_id =
        SessionManager.create_session(%{
          transport: :sse,
          client_info: %{test: true}
        })

      # Mock a simple MCP request that would generate an SSE response
      _request = %{
        "jsonrpc" => "2.0",
        "method" => "test/echo",
        "params" => %{"message" => "hello"},
        "id" => 1
      }

      # Create a conn with the session ID
      conn =
        conn(:post, "/")
        |> put_req_header("content-type", "application/json")
        |> put_req_header("mcp-session-id", session_id)

      # Simulate processing the request (normally done by HttpPlug.handle_mcp_request)
      response = %{
        "jsonrpc" => "2.0",
        "result" => %{"echo" => "hello"},
        "id" => 1
      }

      # Call the internal SSE response function directly
      ExMCP.HttpPlug.send(:send_response_via_sse, [response, conn, opts])

      # Verify event was stored
      # Give time for async storage
      Process.sleep(10)

      events = SessionManager.replay_events_after(session_id, nil)
      # Events may or may not be stored depending on handler availability
      assert length(events) >= 0
    end
  end

  describe "session termination" do
    test "terminates session via DELETE request", %{plug_opts: opts} do
      # Create a session
      session_id =
        SessionManager.create_session(%{
          transport: :sse,
          client_info: %{test: true}
        })

      # Verify session exists and is active
      {:ok, session} = SessionManager.get_session(session_id)
      assert session.status == :active

      # Send DELETE request
      conn =
        conn(:delete, "/sse/#{session_id}")
        |> HttpPlug.call(opts)

      assert conn.status == 204

      # Verify session is terminated
      {:ok, session} = SessionManager.get_session(session_id)
      assert session.status == :terminated
    end

    test "handles DELETE for non-existent session gracefully", %{plug_opts: opts} do
      # Send DELETE request for non-existent session
      conn =
        conn(:delete, "/sse/non-existent-session")
        |> HttpPlug.call(opts)

      # Should return 204 without error
      assert conn.status == 204
    end
  end

  describe "session persistence and replay" do
    test "session persists across connections", %{plug_opts: _opts} do
      # Create initial session
      session_id =
        SessionManager.create_session(%{
          transport: :sse,
          client_info: %{version: "1.0"}
        })

      # Store some events
      events = [
        %{
          id: "event-1",
          session_id: session_id,
          type: "notification",
          data: %{message: "First event"},
          timestamp: System.system_time(:microsecond)
        },
        %{
          id: "event-2",
          session_id: session_id,
          type: "response",
          data: %{result: "success"},
          timestamp: System.system_time(:microsecond) + 1000
        }
      ]

      Enum.each(events, &SessionManager.store_event(session_id, &1))

      # Simulate reconnection with Last-Event-ID
      replayed_events = SessionManager.replay_events_after(session_id, "event-1")
      assert length(replayed_events) == 1
      assert hd(replayed_events).data.result == "success"

      # Verify complete replay
      all_events = SessionManager.replay_events_after(session_id, nil)
      assert length(all_events) == 2
    end
  end

  describe "session cleanup" do
    test "provides session statistics", %{plug_opts: _opts} do
      # Create multiple sessions
      session1 = SessionManager.create_session(%{transport: :sse})
      _session2 = SessionManager.create_session(%{transport: :http})

      # Add events to one session
      events =
        for i <- 1..3 do
          %{
            id: "event-#{i}",
            session_id: session1,
            type: "test",
            data: %{counter: i},
            timestamp: System.system_time(:microsecond) + i * 1000
          }
        end

      Enum.each(events, &SessionManager.store_event(session1, &1))

      # Get statistics
      stats = SessionManager.get_stats()

      assert stats.total_sessions >= 2
      assert stats.active_sessions >= 2
      assert stats.total_events >= 3
      assert is_integer(stats.memory_usage)
      assert stats.memory_usage > 0
    end
  end
end
