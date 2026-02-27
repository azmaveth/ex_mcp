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
    test "returns streaming and modes" do
      caps = Claude.capabilities()
      assert caps["streaming"] == true
      assert is_list(caps["supportedModes"])
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
          "content" => [%{"type" => "text", "text" => "Hello Claude"}]
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
          "content" => [%{"type" => "text", "text" => "new prompt"}]
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
      assert msg["params"]["kind"] == "text"
      assert msg["params"]["content"] == "Hello "
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

      assert msg["params"]["kind"] == "thinking"
      assert msg["params"]["content"] == "Let me think..."
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

      assert {:skip, new_state} = Claude.translate_inbound(line, state)

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

  describe "translate_inbound/2 — non-JSON" do
    test "skips non-JSON lines", %{state: state} do
      assert {:skip, ^state} = Claude.translate_inbound("not json at all", state)
    end

    test "skips unknown event types", %{state: state} do
      line = Jason.encode!(%{"type" => "system", "message" => "hello"})
      assert {:skip, ^state} = Claude.translate_inbound(line, state)
    end
  end
end
