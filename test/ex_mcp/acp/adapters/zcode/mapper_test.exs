defmodule ExMCP.ACP.Adapters.ZCode.MapperTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.ZCode
  alias ExMCP.ACP.Adapters.ZCode.{Mapper, Protocol}

  describe "runtime preferences" do
    test "uses a context-budget strategy accepted by current ZCode releases" do
      {:ok, state} = ZCode.init([])

      {[], [wire], ^state} =
        Mapper.reduce_message(
          %{
            "id" => "preferences-1",
            "method" => "session/requestRuntimePreferences",
            "params" => %{}
          },
          state
        )

      assert %{"result" => preferences} = wire |> IO.iodata_to_binary() |> Jason.decode!()
      assert preferences["modelContextBudgetStrategy"] == "preflight-v1"
    end
  end

  describe "Protocol.workspace_ref/1" do
    test "uses the local path as ZCode's fallback workspace key" do
      assert Protocol.workspace_ref("/tmp/project") == %{
               "workspacePath" => "/tmp/project",
               "workspaceKey" => "/tmp/project"
             }
    end
  end

  describe "Protocol.stop_reason/1" do
    test "success maps to end_turn" do
      assert Protocol.stop_reason("success") == "end_turn"
    end

    test "cancelled maps to cancelled" do
      assert Protocol.stop_reason("cancelled") == "cancelled"
    end

    test "error_max_turns maps to max_turn_requests" do
      assert Protocol.stop_reason("error_max_turns") == "max_turn_requests"
    end

    test "error_max_budget maps to max_turn_requests" do
      assert Protocol.stop_reason("error_max_budget") == "max_turn_requests"
    end

    test "error_max_tool_calls maps to max_turn_requests" do
      assert Protocol.stop_reason("error_max_tool_calls") == "max_turn_requests"
    end

    test "error_during_execution maps to refusal" do
      assert Protocol.stop_reason("error_during_execution") == "refusal"
    end

    test "nil maps to end_turn" do
      assert Protocol.stop_reason(nil) == "end_turn"
    end

    test "unknown maps to end_turn" do
      assert Protocol.stop_reason("unknown") == "end_turn"
    end
  end

  describe "Protocol.prompt_content/1" do
    test "nil returns empty string" do
      assert {:ok, ""} = Protocol.prompt_content(nil)
    end

    test "binary passes through" do
      assert {:ok, "hello"} = Protocol.prompt_content("hello")
    end

    test "list of text blocks concatenates" do
      blocks = [%{"type" => "text", "text" => "a"}, %{"type" => "text", "text" => "b"}]
      assert {:ok, "ab"} = Protocol.prompt_content(blocks)
    end

    test "non-text non-list returns error" do
      assert {:error, _} = Protocol.prompt_content(42)
    end
  end

  describe "Protocol.permission_result/2" do
    test "selected with allow_once option returns allow decision" do
      options = [
        %{"optionId" => "allow_once", "response" => %{"decision" => "allow"}}
      ]

      result =
        Protocol.permission_result(
          %{"outcome" => "selected", "optionId" => "allow_once"},
          options
        )

      assert result["decision"] == "allow"
    end

    test "cancelled returns deny" do
      result = Protocol.permission_result(%{"outcome" => "cancelled"}, [])
      assert result["decision"] == "deny"
    end

    test "unknown option returns deny" do
      result =
        Protocol.permission_result(
          %{"outcome" => "selected", "optionId" => "bogus"},
          [%{"optionId" => "allow", "response" => %{"decision" => "allow"}}]
        )

      assert result["decision"] == "deny"
    end
  end

  describe "Protocol.permission_options/1" do
    test "maps ZCode options to ACP options" do
      zcode_options = [
        %{
          "optionId" => "allow_once",
          "kind" => "allow",
          "name" => "Allow once",
          "response" => %{"decision" => "allow"}
        },
        %{
          "optionId" => "deny",
          "kind" => "deny",
          "name" => "Deny",
          "response" => %{"decision" => "deny"}
        }
      ]

      acp_options = Protocol.permission_options(zcode_options)
      assert length(acp_options) == 2
      assert hd(acp_options)["kind"] == "allow_once"
      assert Enum.at(acp_options, 1)["kind"] == "reject_once"
    end

    test "allow_always maps correctly" do
      zcode_options = [
        %{
          "optionId" => "allow_always",
          "kind" => "allow",
          "name" => "Always allow",
          "response" => %{"decision" => "allow"}
        }
      ]

      [acp] = Protocol.permission_options(zcode_options)
      assert acp["kind"] == "allow_always"
    end
  end

  describe "Mapper.session_result/3" do
    test "builds session result with modes and config options" do
      snapshot = %{
        "projection" => %{"mode" => "build"},
        "session" => %{"sessionId" => "sess-1", "workspace" => "/tmp"}
      }

      state = %{sessions: %{}, mode_id: "build", models: []}

      result = Mapper.session_result("sess-1", snapshot, state)

      assert result["sessionId"] == "sess-1"
      assert result["modes"]["currentModeId"] == "build"
      refute Map.has_key?(result, "models")
    end
  end
end
