defmodule ExMCP.ACP.Adapters.ClaudeTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Claude

  setup do
    {:ok, state} = Claude.init([])
    %{state: state}
  end

  describe "command/1" do
    test "returns claude with stream-json args" do
      {cmd, args} = Claude.command([])

      assert cmd == "claude"
      assert "--output-format" in args
      assert "stream-json" in args
      assert "--input-format" in args
      assert "--verbose" in args
      assert "--dangerously-skip-permissions" in args
    end

    test "includes model when specified" do
      {_cmd, args} = Claude.command(model: "sonnet")
      assert "--model" in args
      assert "sonnet" in args
    end

    test "includes system prompt when specified" do
      {_cmd, args} = Claude.command(system_prompt: "You are helpful")
      assert "--system-prompt" in args
      assert "You are helpful" in args
    end

    test "includes session resume" do
      {_cmd, args} = Claude.command(session_id: "sess_abc")
      assert "--resume" in args
      assert "sess_abc" in args
    end

    test "uses custom cli_path" do
      {cmd, _args} = Claude.command(cli_path: "/custom/claude")
      assert cmd == "/custom/claude"
    end

    test "includes thinking budget" do
      {_cmd, args} = Claude.command(max_thinking_tokens: 20_000)
      assert "--max-thinking-tokens" in args
      assert "20000" in args
    end
  end

  describe "capabilities/0" do
    test "returns streaming capability" do
      caps = Claude.capabilities()
      assert caps["streaming"] == true
    end
  end

  describe "config_options/0" do
    test "returns model and thinking_budget options" do
      opts = Claude.config_options()
      ids = Enum.map(opts, & &1["id"])
      assert "model" in ids
      assert "thinking_budget" in ids
      assert Enum.all?(opts, &Map.has_key?(&1, "category"))
    end
  end

  describe "session/set_config_option" do
    test "stores model preference", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "model", "value" => "opus"}
      }

      assert {:ok, :skip, new_state} = Claude.translate_outbound(msg, state)
      assert new_state.model == "opus"
    end

    test "skips unknown config options", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "unknown", "value" => "x"}
      }

      assert {:ok, :skip, _state} = Claude.translate_outbound(msg, state)
    end
  end

  describe "translate_outbound/2" do
    test "skips initialize", %{state: state} do
      msg = %{"method" => "initialize", "id" => 1, "params" => %{}}
      assert {:ok, :skip, _state} = Claude.translate_outbound(msg, state)
    end

    test "skips session/new", %{state: state} do
      msg = %{"method" => "session/new", "id" => 2, "params" => %{}}
      assert {:ok, :skip, _state} = Claude.translate_outbound(msg, state)
    end

    test "converts session/prompt to stdin NDJSON", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => 3,
        "params" => %{
          "sessionId" => "sess_001",
          "prompt" => [%{"type" => "text", "text" => "Hello Claude"}]
        }
      }

      assert {:ok, data, new_state} = Claude.translate_outbound(msg, state)

      json_str = IO.iodata_to_binary(data)
      assert String.ends_with?(json_str, "\n")

      parsed = Jason.decode!(String.trim(json_str))
      assert parsed["type"] == "user"
      assert parsed["message"]["role"] == "user"
      assert parsed["message"]["content"] == "Hello Claude"
      assert parsed["session_id"] == "sess_001"
      assert new_state.pending_prompt_id == 3
    end

    test "resets accumulators on new prompt", %{state: state} do
      # Simulate accumulated state
      state = %{state | text_acc: ["old text"], thinking_acc: ["old think"]}

      msg = %{
        "method" => "session/prompt",
        "id" => 4,
        "params" => %{
          "sessionId" => "s1",
          "prompt" => [%{"type" => "text", "text" => "new prompt"}]
        }
      }

      {:ok, _data, new_state} = Claude.translate_outbound(msg, state)
      assert new_state.text_acc == []
      assert new_state.thinking_acc == []
    end
  end

  describe "translate_inbound/2 — text deltas" do
    test "text_delta produces session/update", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "stream_event",
          "event" => %{
            "type" => "content_block_delta",
            "delta" => %{"type" => "text_delta", "text" => "Hello "}
          }
        })

      assert {:messages, [msg], new_state} = Claude.translate_inbound(line, state)

      assert msg["method"] == "session/update"
      assert msg["params"]["update"]["sessionUpdate"] == "agent_message_chunk"
      assert msg["params"]["update"]["content"] == %{"type" => "text", "text" => "Hello "}
      assert new_state.text_acc == ["Hello "]
    end

    test "accumulates text across deltas", %{state: state} do
      lines = [
        %{
          "type" => "stream_event",
          "event" => %{
            "type" => "content_block_delta",
            "delta" => %{"type" => "text_delta", "text" => "Hello "}
          }
        },
        %{
          "type" => "stream_event",
          "event" => %{
            "type" => "content_block_delta",
            "delta" => %{"type" => "text_delta", "text" => "world"}
          }
        }
      ]

      state =
        Enum.reduce(lines, state, fn event, acc ->
          line = Jason.encode!(event)

          case Claude.translate_inbound(line, acc) do
            {:messages, _msgs, new_state} -> new_state
            {:skip, new_state} -> new_state
          end
        end)

      # text_acc is in reverse order (iodata prepend pattern)
      text = IO.iodata_to_binary(Enum.reverse(state.text_acc))
      assert text == "Hello world"
    end
  end

  describe "translate_inbound/2 — thinking deltas" do
    test "thinking_delta produces session/update with kind thinking", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "stream_event",
          "event" => %{
            "type" => "content_block_delta",
            "delta" => %{"type" => "thinking_delta", "thinking" => "Let me think..."}
          }
        })

      assert {:messages, [msg], new_state} = Claude.translate_inbound(line, state)

      assert msg["params"]["update"]["sessionUpdate"] == "thinking"
      assert msg["params"]["update"]["content"] == "Let me think..."
      assert new_state.current_block_type == :thinking
    end
  end

  describe "translate_inbound/2 — content block lifecycle" do
    test "content_block_start sets block type", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "stream_event",
          "event" => %{
            "type" => "content_block_start",
            "content_block" => %{"type" => "thinking"}
          }
        })

      assert {:skip, new_state} = Claude.translate_inbound(line, state)
      assert new_state.current_block_type == :thinking
    end

    test "content_block_stop finalizes thinking block", %{state: state} do
      # Accumulate some thinking
      state = %{state | thinking_acc: ["some thinking"], current_block_type: :thinking}

      line =
        Jason.encode!(%{
          "type" => "stream_event",
          "event" => %{"type" => "content_block_stop"}
        })

      assert {:skip, new_state} = Claude.translate_inbound(line, state)
      assert new_state.thinking_acc == []
      assert length(new_state.thinking_blocks) == 1
      assert hd(new_state.thinking_blocks).text == "some thinking"
    end
  end

  describe "translate_inbound/2 — assistant message" do
    test "extracts thinking blocks with dedup", %{state: state} do
      # First, accumulate thinking from streaming
      state = %{
        state
        | thinking_blocks: [%{type: :thinking, text: "Already seen", signature: nil}]
      }

      line =
        Jason.encode!(%{
          "type" => "assistant",
          "message" => %{
            "id" => "msg_123",
            "model" => "claude-sonnet-4-20250514",
            "content" => [
              %{"type" => "thinking", "thinking" => "Already seen", "signature" => "sig1"},
              %{"type" => "thinking", "thinking" => "New thinking", "signature" => "sig2"},
              %{"type" => "text", "text" => "Response text"}
            ]
          }
        })

      assert {:messages, notifications, new_state} = Claude.translate_inbound(line, state)

      # Text block emits an agent_message_chunk notification
      assert length(notifications) == 1

      assert %{"method" => "session/update", "params" => %{"update" => update}} =
               hd(notifications)

      assert update["sessionUpdate"] == "agent_message_chunk"

      # "Already seen" should be deduped, "New thinking" should be added
      assert length(new_state.thinking_blocks) == 2
      assert new_state.session_id == "msg_123"
      assert new_state.model == "claude-sonnet-4-20250514"
    end
  end

  describe "translate_inbound/2 — result event" do
    test "produces prompt response with usage", %{state: state} do
      state = %{state | text_acc: ["world", "Hello "], pending_prompt_id: 42, session_id: "s1"}

      line =
        Jason.encode!(%{
          "type" => "result",
          "session_id" => "s1",
          "usage" => %{
            "input_tokens" => 100,
            "output_tokens" => 50,
            "cache_read_input_tokens" => 10
          }
        })

      assert {:messages, messages, new_state} = Claude.translate_inbound(line, state)

      # Should have status update and prompt result
      assert length(messages) >= 1

      # Find the prompt result
      result_msg = Enum.find(messages, &(&1["id"] == 42))
      assert result_msg != nil
      assert result_msg["result"]["stopReason"] == "end_turn"
      assert result_msg["result"]["text"] == "Hello world"
      assert result_msg["result"]["usage"]["inputTokens"] == 100
      assert result_msg["result"]["usage"]["outputTokens"] == 50

      # Pending prompt ID cleared
      assert new_state.pending_prompt_id == nil
    end

    test "includes thinking in result when available", %{state: state} do
      state = %{
        state
        | text_acc: ["text"],
          thinking_blocks: [%{type: :thinking, text: "I thought about it", signature: "sig"}],
          pending_prompt_id: 10
      }

      line = Jason.encode!(%{"type" => "result", "usage" => %{}})

      assert {:messages, messages, _state} = Claude.translate_inbound(line, state)

      result_msg = Enum.find(messages, &(&1["id"] == 10))
      assert result_msg["result"]["thinking"] != nil
      assert length(result_msg["result"]["thinking"]) == 1
    end

    test "handles error result", %{state: state} do
      state = %{state | text_acc: [], pending_prompt_id: 11}

      line = Jason.encode!(%{"type" => "result", "is_error" => true, "usage" => %{}})

      assert {:messages, messages, _state} = Claude.translate_inbound(line, state)

      result_msg = Enum.find(messages, &(&1["id"] == 11))
      assert result_msg["result"]["stopReason"] == "error"
    end
  end

  describe "translate_inbound/2 — multi-turn tool use" do
    test "tool_use content block emits tool_call notification", %{state: state} do
      state = %{state | session_id: "s1"}

      line =
        Jason.encode!(%{
          "type" => "assistant",
          "message" => %{
            "content" => [
              %{
                "type" => "tool_use",
                "id" => "toolu_123",
                "name" => "Grep",
                "input" => %{"pattern" => "defmodule"}
              }
            ]
          }
        })

      assert {:messages, [notification], new_state} = Claude.translate_inbound(line, state)
      assert notification["method"] == "session/update"

      update = notification["params"]["update"]
      assert update["sessionUpdate"] == "tool_call_update"
      assert update["title"] == "Search: defmodule"
      assert update["toolCallId"] == "toolu_123"
      assert update["status"] == "running"
      assert update["pattern"] == "defmodule"
      assert new_state.in_tool_use == true
    end

    test "user event with tool_result emits tool_result notification", %{state: state} do
      state = %{state | session_id: "s1", in_tool_use: true}

      line =
        Jason.encode!(%{
          "type" => "user",
          "message" => %{
            "content" => [
              %{
                "type" => "tool_result",
                "tool_use_id" => "toolu_123",
                "content" => "Found 3 matches"
              }
            ]
          }
        })

      assert {:messages, [notification], _state} = Claude.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["sessionUpdate"] == "tool_call_update"
      assert update["toolCallId"] == "toolu_123"
      assert update["status"] == "completed"
    end

    test "Read tool includes filePath metadata", %{state: state} do
      state = %{state | session_id: "s1"}

      line =
        Jason.encode!(%{
          "type" => "assistant",
          "message" => %{
            "content" => [
              %{
                "type" => "tool_use",
                "id" => "toolu_read",
                "name" => "Read",
                "input" => %{"file_path" => "/src/main.ex"}
              }
            ]
          }
        })

      assert {:messages, [notification], _state} = Claude.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["title"] == "Read main.ex"
      assert update["filePath"] == "/src/main.ex"
    end

    test "Bash tool includes command metadata", %{state: state} do
      state = %{state | session_id: "s1"}

      line =
        Jason.encode!(%{
          "type" => "assistant",
          "message" => %{
            "content" => [
              %{
                "type" => "tool_use",
                "id" => "toolu_bash",
                "name" => "Bash",
                "input" => %{"command" => "mix test --only fast"}
              }
            ]
          }
        })

      assert {:messages, [notification], _state} = Claude.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["title"] == "Run: mix test --only fast"
      assert update["command"] == "mix test --only fast"
    end

    test "tool_result with error has failed status", %{state: state} do
      state = %{state | session_id: "s1", in_tool_use: true}

      line =
        Jason.encode!(%{
          "type" => "user",
          "message" => %{
            "content" => [
              %{
                "type" => "tool_result",
                "tool_use_id" => "toolu_err",
                "content" => "File not found",
                "is_error" => true
              }
            ]
          }
        })

      assert {:messages, [notification], _state} = Claude.translate_inbound(line, state)
      update = notification["params"]["update"]
      assert update["status"] == "failed"
      assert update["isError"] == true
    end

    test "assistant after tool use clears text_acc for final answer", %{state: state} do
      # Simulate state after tool_use cycle — text_acc might have stale text
      state = %{state | session_id: "s1", in_tool_use: true, text_acc: ["stale"]}

      line =
        Jason.encode!(%{
          "type" => "assistant",
          "message" => %{
            "content" => [
              %{"type" => "text", "text" => "The final answer"}
            ]
          }
        })

      assert {:messages, _notifications, new_state} = Claude.translate_inbound(line, state)
      assert new_state.text_acc == ["The final answer"]
      assert new_state.in_tool_use == false
    end

    test "full multi-turn tool use sequence produces correct result", %{state: state} do
      state = %{state | session_id: "s1", pending_prompt_id: 42}

      # Step 1: assistant with thinking
      line1 =
        Jason.encode!(%{
          "type" => "assistant",
          "message" => %{
            "content" => [
              %{"type" => "thinking", "thinking" => "I need to search...", "signature" => nil}
            ]
          }
        })

      {:skip, state} = Claude.translate_inbound(line1, state)

      # Step 2: assistant with tool_use
      line2 =
        Jason.encode!(%{
          "type" => "assistant",
          "message" => %{
            "content" => [
              %{
                "type" => "tool_use",
                "id" => "toolu_abc",
                "name" => "Grep",
                "input" => %{"pattern" => "trust_profile"}
              }
            ]
          }
        })

      {:messages, _tool_notifs, state} = Claude.translate_inbound(line2, state)
      assert state.in_tool_use == true

      # Step 3: user with tool_result
      line3 =
        Jason.encode!(%{
          "type" => "user",
          "message" => %{
            "content" => [
              %{
                "type" => "tool_result",
                "tool_use_id" => "toolu_abc",
                "content" => "trust_profile.ex:5: defmodule TrustProfile"
              }
            ]
          }
        })

      {:messages, _result_notifs, state} = Claude.translate_inbound(line3, state)

      # Step 4: assistant with final text answer
      line4 =
        Jason.encode!(%{
          "type" => "assistant",
          "message" => %{
            "content" => [
              %{"type" => "text", "text" => "I found the TrustProfile module."}
            ]
          }
        })

      {:messages, _text_notifs, state} = Claude.translate_inbound(line4, state)
      assert state.in_tool_use == false
      assert state.text_acc == ["I found the TrustProfile module."]

      # Step 5: result event
      line5 =
        Jason.encode!(%{
          "type" => "result",
          "session_id" => "s1",
          "result" => "I found the TrustProfile module.",
          "usage" => %{"input_tokens" => 200, "output_tokens" => 30}
        })

      {:messages, messages, final_state} = Claude.translate_inbound(line5, state)
      assert final_state.pending_prompt_id == nil

      response = Enum.find(messages, &(&1["id"] == 42))
      assert response["result"]["text"] == "I found the TrustProfile module."
      assert response["result"]["stopReason"] == "end_turn"
    end

    test "user event without tool_result content is skipped", %{state: state} do
      line =
        Jason.encode!(%{
          "type" => "user",
          "message" => %{"content" => "plain text"}
        })

      assert {:skip, ^state} = Claude.translate_inbound(line, state)
    end
  end

  describe "translate_inbound/2 — non-JSON" do
    test "skips non-JSON lines", %{state: state} do
      assert {:skip, ^state} = Claude.translate_inbound("not json at all", state)
    end

    test "system events emit status notification", %{state: state} do
      line = Jason.encode!(%{"type" => "system", "message" => "hello"})
      assert {:messages, [notification], _state} = Claude.translate_inbound(line, state)
      assert notification["params"]["update"]["sessionUpdate"] == "status"
      assert notification["params"]["update"]["message"] == "hello"
    end

    test "skips truly unknown event types", %{state: state} do
      line = Jason.encode!(%{"type" => "unknown_xyz", "data" => "test"})
      assert {:skip, ^state} = Claude.translate_inbound(line, state)
    end
  end
end
