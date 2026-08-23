defmodule ExMCP.ACP.Adapters.ZCode.SessionsTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.ZCode.Sessions

  describe "fetch_id/1" do
    test "returns ok for valid session id" do
      assert {:ok, "sess-1"} = Sessions.fetch_id(%{"sessionId" => "sess-1"})
    end

    test "returns error for missing session id" do
      assert {:error, _} = Sessions.fetch_id(%{})
    end

    test "returns error for empty session id" do
      assert {:error, _} = Sessions.fetch_id(%{"sessionId" => ""})
    end
  end

  describe "empty/2" do
    test "creates a session with defaults" do
      state = %{sessions: %{}, model: nil, mode_id: "build"}
      session = Sessions.empty("sess-1", state)

      assert session.id == "sess-1"
      assert session.mode_id == "build"
      assert session.active_prompt_acp_id == nil
    end

    test "uses default mode when state has none" do
      session = Sessions.empty("sess-1", %{sessions: %{}, model: nil})
      assert session.mode_id == "build"
    end
  end

  describe "put/3 and fetch/2" do
    test "stores and retrieves a session" do
      state = %{sessions: %{}, model: nil, mode_id: "build"}
      session = Sessions.empty("sess-1", state)

      state = Sessions.put(state, "sess-1", session)
      assert {:ok, ^session} = Sessions.fetch(state, "sess-1")
    end

    test "fetch returns error for unknown session" do
      state = %{sessions: %{}}
      assert {:error, _} = Sessions.fetch(state, "nope")
    end
  end

  describe "update/3" do
    test "updates an existing session" do
      state = %{sessions: %{}, model: nil, mode_id: "build"}
      session = Sessions.empty("sess-1", state)
      state = Sessions.put(state, "sess-1", session)

      state = Sessions.update(state, "sess-1", &Map.put(&1, :turn_id, "turn-1"))
      assert {:ok, session} = Sessions.fetch(state, "sess-1")
      assert session.turn_id == "turn-1"
    end

    test "creates session if it does not exist" do
      state = %{sessions: %{}, model: nil, mode_id: "build"}

      state = Sessions.update(state, "sess-1", &Map.put(&1, :turn_id, "turn-1"))
      assert {:ok, session} = Sessions.fetch(state, "sess-1")
      assert session.turn_id == "turn-1"
    end
  end

  describe "id_from_params/2" do
    test "extracts sessionId from params" do
      state = %{sessions: %{}}
      assert Sessions.id_from_params(%{"sessionId" => "sess-1"}, state) == "sess-1"
    end

    test "falls back to current_id" do
      state = %{sessions: %{"only" => %{}}}
      assert Sessions.id_from_params(%{}, state) == "only"
    end
  end

  describe "current_id/1" do
    test "returns the only session id" do
      state = %{sessions: %{"sess-1" => %{}}}
      assert Sessions.current_id(state) == "sess-1"
    end

    test "returns nil for multiple sessions" do
      state = %{sessions: %{"a" => %{}, "b" => %{}}}
      assert Sessions.current_id(state) == nil
    end

    test "returns nil for no sessions" do
      assert Sessions.current_id(%{sessions: %{}}) == nil
    end
  end

  describe "to_acp_session_info/1" do
    test "builds ACP session info" do
      session = %{
        "sessionId" => "sess-1",
        "workspace" => "/tmp/project",
        "title" => "My Session"
      }

      info = Sessions.to_acp_session_info(session)
      assert info["sessionId"] == "sess-1"
      assert info["cwd"] == "/tmp/project"
      assert info["title"] == "My Session"
    end

    test "extracts cwd from a ZCode workspace descriptor" do
      session = %{
        "sessionId" => "sess-1",
        "workspace" => %{
          "workspacePath" => "/tmp/project",
          "workspaceKey" => "/tmp/project"
        }
      }

      assert Sessions.to_acp_session_info(session)["cwd"] == "/tmp/project"
    end

    test "omits nil title" do
      session = %{"sessionId" => "sess-1", "workspace" => "/tmp"}

      info = Sessions.to_acp_session_info(session)
      refute Map.has_key?(info, "title")
    end
  end

  describe "from_snapshot/3" do
    test "stores the workspace path from a ZCode workspace descriptor" do
      state = %{sessions: %{}, model: nil, mode_id: "build"}

      snapshot = %{
        "session" => %{
          "sessionId" => "sess-1",
          "workspace" => %{
            "workspacePath" => "/tmp/project",
            "workspaceKey" => "/tmp/project"
          }
        }
      }

      assert Sessions.from_snapshot("sess-1", snapshot, state).workspace == "/tmp/project"
    end
  end

  describe "reset_prompt_accumulators/2" do
    test "clears accumulators and sets acp_id" do
      session = Sessions.empty("sess-1", %{sessions: %{}, model: nil, mode_id: "build"})

      session = Sessions.reset_prompt_accumulators(session, 42)

      assert session.active_prompt_acp_id == 42
      assert session.accumulated_text == []
      assert session.prompt_activity == false
    end
  end
end
