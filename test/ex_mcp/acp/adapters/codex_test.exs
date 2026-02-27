defmodule ExMCP.ACP.Adapters.CodexTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex

  setup do
    {:ok, state} = Codex.init([])
    %{state: state}
  end

  describe "command/1" do
    test "returns :one_shot" do
      assert :one_shot = Codex.command([])
    end
  end

  describe "capabilities/0" do
    test "returns non-streaming capabilities" do
      caps = Codex.capabilities()
      assert caps["streaming"] == false
    end
  end

  describe "translate_outbound/2" do
    test "skips initialize", %{state: state} do
      msg = %{"method" => "initialize", "id" => 1, "params" => %{}}
      assert {:ok, :skip, _state} = Codex.translate_outbound(msg, state)
    end

    test "skips session/new", %{state: state} do
      msg = %{"method" => "session/new", "id" => 2, "params" => %{}}
      assert {:ok, :skip, _state} = Codex.translate_outbound(msg, state)
    end

    test "session/prompt returns one_shot command", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => 3,
        "params" => %{
          "sessionId" => "s1",
          "content" => [%{"type" => "text", "text" => "Fix the bug"}]
        }
      }

      assert {:one_shot, cmd_fn, new_state} = Codex.translate_outbound(msg, state)
      assert is_function(cmd_fn, 0)
      assert new_state.pending_prompt_id == 3
    end
  end

  describe "translate_inbound/2" do
    test "always skips (one-shot handles inbound internally)", %{state: state} do
      assert {:skip, ^state} = Codex.translate_inbound("anything", state)
    end
  end

  describe "init/1" do
    test "stores model from opts" do
      {:ok, state} = Codex.init(model: "gpt-5")
      assert state.model == "gpt-5"
    end

    test "defaults to nil model" do
      {:ok, state} = Codex.init([])
      assert state.model == nil
    end
  end
end
