defmodule ExMCP.ACP.Adapters.PiTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Pi

  setup do
    {:ok, state} = Pi.init([])
    %{state: state}
  end

  describe "command/1" do
    test "returns pi with rpc mode" do
      {cmd, args} = Pi.command([])
      assert cmd == "pi"
      assert "--mode" in args
      assert "rpc" in args
      assert "--no-themes" in args
    end

    test "includes model flag" do
      {_cmd, args} = Pi.command(model: "anthropic/claude-sonnet-4")
      assert "--model" in args
      assert "anthropic/claude-sonnet-4" in args
    end

    test "includes session path" do
      {_cmd, args} = Pi.command(session_path: "/tmp/session.jsonl")
      assert "--session" in args
      assert "/tmp/session.jsonl" in args
    end

    test "includes no-session flag" do
      {_cmd, args} = Pi.command(no_session: true)
      assert "--no-session" in args
    end
  end

  describe "capabilities/0" do
    test "returns streaming and features" do
      caps = Pi.capabilities()
      assert caps["streaming"] == true
      assert is_map(caps["features"])
      assert caps["features"]["steering"] == true
      assert caps["features"]["compaction"] == true
    end
  end

  describe "modes/0" do
    test "returns code mode" do
      modes = Pi.modes()
      assert length(modes) == 1
      assert hd(modes)["id"] == "code"
    end
  end

  describe "config_options/0" do
    test "returns model, thinking_level, and other options" do
      opts = Pi.config_options()
      ids = Enum.map(opts, & &1["id"])
      assert "model" in ids
      assert "thinking_level" in ids
      assert "auto_compaction" in ids
      assert "auto_retry" in ids
      assert "steering_mode" in ids
      assert "follow_up_mode" in ids
    end

    test "thinking_level has correct values" do
      opts = Pi.config_options()
      tl = Enum.find(opts, &(&1["id"] == "thinking_level"))
      assert tl["type"] == "enum"
      assert "off" in tl["values"]
      assert "high" in tl["values"]
    end
  end

  describe "translate_outbound/2 — session/set_config_option" do
    test "model routes to set_model RPC", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "model", "value" => "anthropic/claude-sonnet-4"}
      }

      assert {:ok, data, _state} = Pi.translate_outbound(msg, state)
      decoded = Jason.decode!(String.trim(IO.iodata_to_binary(data)))
      assert decoded["type"] == "set_model"
      assert decoded["provider"] == "anthropic"
      assert decoded["modelId"] == "claude-sonnet-4"
    end

    test "thinking_level routes to set_thinking_level", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "thinking_level", "value" => "high"}
      }

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      decoded = Jason.decode!(String.trim(IO.iodata_to_binary(data)))
      assert decoded["type"] == "set_thinking_level"
      assert decoded["level"] == "high"
      assert new_state.thinking_level == "high"
    end

    test "invalid thinking_level is skipped", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "thinking_level", "value" => "invalid"}
      }

      assert {:ok, :skip, _state} = Pi.translate_outbound(msg, state)
    end

    test "auto_compaction routes to set_auto_compaction", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "auto_compaction", "value" => false}
      }

      assert {:ok, data, _state} = Pi.translate_outbound(msg, state)
      decoded = Jason.decode!(String.trim(IO.iodata_to_binary(data)))
      assert decoded["type"] == "set_auto_compaction"
      assert decoded["enabled"] == false
    end

    test "unknown config option is skipped", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "nonexistent", "value" => "x"}
      }

      assert {:ok, :skip, _state} = Pi.translate_outbound(msg, state)
    end
  end

  describe "translate_outbound/2 — session/set_mode" do
    test "is a no-op for Pi (single mode)", %{state: state} do
      msg = %{"method" => "session/set_mode", "params" => %{"modeId" => "code"}}
      assert {:ok, :skip, _state} = Pi.translate_outbound(msg, state)
    end
  end

  describe "translate_outbound/2 — prompting" do
    test "session/prompt produces Pi RPC prompt", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => 1,
        "params" => %{"prompt" => "Hello"}
      }

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      decoded = Jason.decode!(String.trim(IO.iodata_to_binary(data)))
      assert decoded["type"] == "prompt"
      assert decoded["message"] == "Hello"
      assert new_state.pending_prompt_id == 1
    end

    test "session/cancel produces abort", %{state: state} do
      msg = %{"method" => "session/cancel"}
      assert {:ok, data, _state} = Pi.translate_outbound(msg, state)
      decoded = Jason.decode!(String.trim(IO.iodata_to_binary(data)))
      assert decoded["type"] == "abort"
    end
  end

  describe "translate_inbound/2 — text streaming" do
    test "text_delta produces agent_message_chunk", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "message_update",
          "assistantMessageEvent" => %{"type" => "text_delta", "delta" => "Hello"}
        })

      assert {:messages, [notification], new_state} = Pi.translate_inbound(line, state)
      assert notification["method"] == "session/update"
      update = notification["params"]["update"]
      assert update["type"] == "agent_message_chunk"
      assert update["content"]["text"] == "Hello"
      assert new_state.text_acc == ["Hello"]
    end

    test "thinking_delta produces thinking update", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "message_update",
          "assistantMessageEvent" => %{"type" => "thinking_delta", "delta" => "Let me think..."}
        })

      assert {:messages, [notification], _state} = Pi.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["type"] == "agent_message_chunk"
      assert update["content"]["type"] == "thinking"
    end
  end

  describe "translate_inbound/2 — agent_end" do
    test "produces prompt response with accumulated text", %{state: state} do
      state = %{state | pending_prompt_id: 5, text_acc: ["world", "Hello "]}

      line =
        Jason.encode!(%{
          "type" => "agent_end",
          "messages" => []
        })

      assert {:messages, [response], new_state} = Pi.translate_inbound(line, state)
      assert response["id"] == 5
      assert response["result"]["result"] == "Hello world"
      assert new_state.pending_prompt_id == nil
      assert new_state.text_acc == []
    end
  end

  describe "translate_inbound/2 — tool execution" do
    test "tool_execution_start emits notification", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "tool_execution_start",
          "toolCallId" => "tc-1",
          "toolName" => "bash",
          "args" => %{"command" => "ls"}
        })

      assert {:messages, [notification], _state} = Pi.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["type"] == "tool_execution"
      assert update["status"] == "started"
      assert update["toolName"] == "bash"
    end

    test "tool_execution_end emits tool result", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "tool_execution_end",
          "toolCallId" => "tc-1",
          "toolName" => "bash",
          "result" => %{"content" => [%{"type" => "text", "text" => "file1.txt"}]},
          "isError" => false
        })

      assert {:messages, [notification], _state} = Pi.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["type"] == "tool_result"
      assert update["content"] == "file1.txt"
      assert update["isError"] == false
    end
  end

  describe "translate_inbound/2 — skip/ignore" do
    test "empty lines are skipped", %{state: state} do
      assert {:skip, ^state} = Pi.translate_inbound("", state)
    end

    test "non-JSON lines are skipped", %{state: state} do
      assert {:skip, ^state} = Pi.translate_inbound("not json", state)
    end

    test "lifecycle events are skipped", %{state: state} do
      for type <- ["agent_start", "turn_start", "turn_end", "message_start", "message_end"] do
        line = Jason.encode!(%{"type" => type})
        assert {:skip, _state} = Pi.translate_inbound(line, state)
      end
    end
  end

  describe "list_sessions/1" do
    test "returns empty list when session dir doesn't exist", %{state: state} do
      state = %{state | session_dir: "/nonexistent/path"}
      assert {:ok, [], _state} = Pi.list_sessions(state)
    end

    test "scans session directory for jsonl files" do
      # Create a temp dir with fake session files
      tmp_dir =
        System.tmp_dir!() |> Path.join("pi_test_sessions_#{System.unique_integer([:positive])}")

      File.mkdir_p!(tmp_dir)

      File.write!(Path.join(tmp_dir, "session-abc.jsonl"), "{}")
      File.write!(Path.join(tmp_dir, "session-def.jsonl"), "{}")
      File.write!(Path.join(tmp_dir, "not-a-session.txt"), "ignore")

      {:ok, state} = Pi.init(session_dir: tmp_dir)
      assert {:ok, sessions, _state} = Pi.list_sessions(state)

      ids = Enum.map(sessions, & &1["sessionId"])
      assert "session-abc" in ids
      assert "session-def" in ids
      assert length(sessions) == 2

      # Each session has updatedAt
      assert Enum.all?(sessions, &Map.has_key?(&1, "updatedAt"))

      # Cleanup
      File.rm_rf!(tmp_dir)
    end
  end

  describe "tool result text extraction" do
    test "tool_execution_end with content blocks extracts text", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "tool_execution_end",
          "toolCallId" => "tc-1",
          "toolName" => "read",
          "result" => %{
            "content" => [
              %{"type" => "text", "text" => "file contents here"}
            ]
          },
          "isError" => false
        })

      assert {:messages, [notification], _state} = Pi.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["content"] == "file contents here"
    end

    test "tool_execution_end with diff in details", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "tool_execution_end",
          "toolCallId" => "tc-2",
          "toolName" => "edit",
          "result" => %{
            "content" => [],
            "details" => %{"diff" => "--- a/file.ex\n+++ b/file.ex\n@@ -1 +1 @@\n-old\n+new"}
          },
          "isError" => false
        })

      assert {:messages, [notification], _state} = Pi.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["content"] =~ "--- a/file.ex"
    end

    test "tool_execution_end with stdout/stderr/exitCode", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "tool_execution_end",
          "toolCallId" => "tc-3",
          "toolName" => "bash",
          "result" => %{
            "content" => [],
            "details" => %{
              "stdout" => "hello world",
              "stderr" => "warning: something",
              "exitCode" => 0
            }
          },
          "isError" => false
        })

      assert {:messages, [notification], _state} = Pi.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["content"] =~ "hello world"
      assert update["content"] =~ "stderr:"
      assert update["content"] =~ "exit code: 0"
    end
  end
end
