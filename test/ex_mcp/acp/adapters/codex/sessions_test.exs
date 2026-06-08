defmodule ExMCP.ACP.Adapters.Codex.SessionsTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex
  alias ExMCP.ACP.Adapters.Codex.Sessions

  setup do
    {:ok, state} = Codex.init(model: "gpt-5", mode_id: "read-only", reasoning_effort: "high")
    %{state: state}
  end

  test "builds sessions from Codex thread results", %{state: state} do
    result = %{
      "thread" => %{"id" => "thread-1", "cwd" => "/repo"},
      "model" => "gpt-5-codex",
      "reasoningEffort" => "medium"
    }

    session =
      Sessions.from_result("thread-1", result, state, fn session -> session.model <> ":id" end)

    assert session.id == "thread-1"
    assert session.cwd == "/repo"
    assert session.model == "gpt-5-codex"
    assert session.model_id == "gpt-5-codex:id"
    assert session.reasoning_effort == "medium"
  end

  test "stores, fetches, and updates sessions with state-first helpers", %{state: state} do
    session = Sessions.empty("thread-1", state)

    state =
      state
      |> Sessions.put("thread-1", session)
      |> Sessions.update("thread-1", &Map.put(&1, :turn_id, "turn-1"))

    assert Sessions.fetch(state, "thread-1") == {:ok, Map.put(session, :turn_id, "turn-1")}
    assert Sessions.current_id(state) == "thread-1"
    assert Sessions.id_from_params(%{}, state) == "thread-1"
  end

  test "validates session ids and derives thread ids", %{state: state} do
    assert Sessions.fetch_id(%{"sessionId" => "thread-1"}) == {:ok, "thread-1"}
    assert Sessions.fetch_id(%{}) == {:error, "sessionId is required"}
    assert Sessions.fetch(state, "missing") == {:error, "Unknown Codex session: missing"}

    assert Sessions.thread_id(%{"id" => "thread-1"}, %{}) == "thread-1"
    assert Sessions.thread_id(%{}, %{"sessionId" => "session-1"}) == "session-1"
  end
end
