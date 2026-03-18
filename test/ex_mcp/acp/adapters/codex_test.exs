defmodule ExMCP.ACP.Adapters.CodexTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex

  setup do
    {:ok, state} = Codex.init([])
    %{state: state}
  end

  describe "command/1" do
    test "returns codex app-server command" do
      assert {"codex", ["app-server"]} = Codex.command([])
    end
  end

  describe "capabilities/0" do
    test "returns streaming capability" do
      caps = Codex.capabilities()
      assert caps["streaming"] == true
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

    test "starts in initializing phase" do
      {:ok, state} = Codex.init([])
      assert state.phase == :initializing
    end
  end

  describe "post_connect/1" do
    test "sends initialize request", %{state: state} do
      {:ok, data, new_state} = Codex.post_connect(state)
      json = IO.iodata_to_binary(data)
      assert {:ok, msg} = Jason.decode(json)
      assert msg["method"] == "initialize"
      assert msg["id"] == 1
      assert msg["params"]["clientInfo"]["name"] == "ex_mcp"
      assert new_state.next_id == 2
      assert Map.has_key?(new_state.pending_requests, 1)
    end

    test "uses custom client name from opts" do
      {:ok, state} = Codex.init(client_name: "my_app", client_version: "2.0.0")
      {:ok, data, _state} = Codex.post_connect(state)
      json = IO.iodata_to_binary(data)
      {:ok, msg} = Jason.decode(json)
      assert msg["params"]["clientInfo"]["name"] == "my_app"
      assert msg["params"]["clientInfo"]["version"] == "2.0.0"
    end
  end

  describe "translate_outbound/2" do
    test "skips initialize", %{state: state} do
      msg = %{"method" => "initialize", "id" => 1, "params" => %{}}
      assert {:ok, :skip, _state} = Codex.translate_outbound(msg, state)
    end

    test "session/new sends thread/start", %{state: state} do
      msg = %{
        "method" => "session/new",
        "id" => 2,
        "params" => %{"model" => "gpt-4o"}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      json = IO.iodata_to_binary(data)
      {:ok, codex_msg} = Jason.decode(json)
      assert codex_msg["method"] == "thread/start"
      assert codex_msg["params"]["model"] == "gpt-4o"
      assert new_state.pending_requests[new_state.next_id - 1].type == :thread_start
      assert new_state.pending_requests[new_state.next_id - 1].acp_id == 2
    end

    test "session/prompt sends turn/start", %{state: state} do
      state = %{state | thread_id: "thread-123"}

      msg = %{
        "method" => "session/prompt",
        "id" => 3,
        "params" => %{
          "sessionId" => "thread-123",
          "prompt" => [%{"type" => "text", "text" => "Fix the bug"}]
        }
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      json = IO.iodata_to_binary(data)
      {:ok, codex_msg} = Jason.decode(json)
      assert codex_msg["method"] == "turn/start"
      assert codex_msg["params"]["threadId"] == "thread-123"
      assert codex_msg["params"]["input"] == [%{"type" => "text", "text" => "Fix the bug"}]
      assert new_state.accumulated_text == []
      assert new_state.accumulated_thinking == []
    end

    test "session/prompt skips without thread_id", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => 3,
        "params" => %{
          "prompt" => [%{"type" => "text", "text" => "Fix the bug"}]
        }
      }

      assert {:ok, :skip, _state} = Codex.translate_outbound(msg, state)
    end

    test "session/cancel sends turn/interrupt", %{state: state} do
      state = %{state | thread_id: "t-1", turn_id: "turn-1"}

      msg = %{
        "method" => "session/cancel",
        "id" => 4,
        "params" => %{
          "sessionId" => "t-1",
          "turnId" => "turn-1"
        }
      }

      assert {:ok, data, _new_state} = Codex.translate_outbound(msg, state)
      json = IO.iodata_to_binary(data)
      {:ok, codex_msg} = Jason.decode(json)
      assert codex_msg["method"] == "turn/interrupt"
      assert codex_msg["params"]["threadId"] == "t-1"
      assert codex_msg["params"]["turnId"] == "turn-1"
    end

    test "unknown methods are skipped", %{state: state} do
      msg = %{"method" => "unknown/method", "id" => 5, "params" => %{}}
      assert {:ok, :skip, _state} = Codex.translate_outbound(msg, state)
    end
  end

  describe "translate_inbound/2 — responses" do
    test "initialize response triggers initialized write-back", %{state: state} do
      # Simulate post_connect tracking the init request
      state = %{state | pending_requests: %{0 => %{type: :initialize, acp_id: nil}}}

      line = Jason.encode!(%{"id" => 0, "result" => %{"capabilities" => %{}}})
      assert {:skip_and_write, data, new_state} = Codex.translate_inbound(line, state)
      json = IO.iodata_to_binary(data)
      {:ok, msg} = Jason.decode(json)
      assert msg["method"] == "initialized"
      assert new_state.phase == :ready
      assert new_state.pending_requests == %{}
    end

    test "thread/start response produces session/new result", %{state: state} do
      state = %{state | pending_requests: %{1 => %{type: :thread_start, acp_id: 2}}}

      line =
        Jason.encode!(%{
          "id" => 1,
          "result" => %{"thread" => %{"id" => "thread-abc"}}
        })

      assert {:messages, [msg], new_state} = Codex.translate_inbound(line, state)
      assert msg["id"] == 2
      assert msg["result"]["sessionId"] == "thread-abc"
      assert new_state.thread_id == "thread-abc"
    end

    test "turn/start response captures turn_id silently", %{state: state} do
      state = %{state | pending_requests: %{2 => %{type: :turn_start, acp_id: 3}}}

      line =
        Jason.encode!(%{
          "id" => 2,
          "result" => %{"turn" => %{"id" => "turn-xyz"}}
        })

      assert {:skip, new_state} = Codex.translate_inbound(line, state)
      assert new_state.turn_id == "turn-xyz"
    end

    test "error response for thread/start", %{state: state} do
      state = %{state | pending_requests: %{1 => %{type: :thread_start, acp_id: 2}}}

      line =
        Jason.encode!(%{
          "id" => 1,
          "error" => %{"code" => -32600, "message" => "Bad request"}
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)
      assert msg["id"] == 2
      assert msg["error"]["message"] == "Bad request"
    end
  end

  describe "translate_inbound/2 — notifications" do
    setup %{state: state} do
      state = %{state | thread_id: "t-1", turn_id: "turn-1", phase: :ready}
      %{state: state}
    end

    test "item/agentMessage/delta produces text update", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/agentMessage/delta",
          "params" => %{"delta" => "Hello ", "threadId" => "t-1", "turnId" => "turn-1"}
        })

      assert {:messages, [msg], new_state} = Codex.translate_inbound(line, state)
      assert msg["method"] == "session/update"
      assert msg["params"]["update"]["sessionUpdate"] == "agent_message_chunk"
      assert msg["params"]["update"]["content"] == %{"type" => "text", "text" => "Hello "}
      assert new_state.accumulated_text == ["Hello "]
    end

    test "item/reasoning/textDelta produces thinking update", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/reasoning/textDelta",
          "params" => %{"delta" => "Let me think...", "threadId" => "t-1"}
        })

      assert {:messages, [msg], new_state} = Codex.translate_inbound(line, state)
      assert msg["method"] == "session/update"
      assert msg["params"]["update"]["sessionUpdate"] == "thinking"
      assert msg["params"]["update"]["content"] == "Let me think..."
      assert new_state.accumulated_thinking == ["Let me think..."]
    end

    test "item/completed agent_message produces final text", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/completed",
          "params" => %{
            "item" => %{"type" => "agent_message", "text" => "Done!"},
            "threadId" => "t-1"
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)
      assert msg["params"]["update"]["sessionUpdate"] == "agent_message_chunk"
      assert msg["params"]["update"]["final"] == true
      assert msg["params"]["update"]["content"] == %{"type" => "text", "text" => "Done!"}
    end

    test "item/completed non-agent_message is skipped", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/completed",
          "params" => %{
            "item" => %{"type" => "command_execution"},
            "threadId" => "t-1"
          }
        })

      assert {:skip, _state} = Codex.translate_inbound(line, state)
    end

    test "turn/completed produces prompt response with accumulated text", %{state: state} do
      # Set current_prompt_acp_id (saved when turn/start response was processed)
      state = %{
        state
        | current_prompt_acp_id: 5,
          accumulated_text: ["world", "Hello "]
      }

      line =
        Jason.encode!(%{
          "method" => "turn/completed",
          "params" => %{
            "turn" => %{"id" => "turn-1", "status" => "completed"},
            "threadId" => "t-1"
          }
        })

      assert {:messages, messages, new_state} = Codex.translate_inbound(line, state)
      # Should have status notification + response
      response = Enum.find(messages, &Map.has_key?(&1, "id"))
      status = Enum.find(messages, &(&1["method"] == "session/update"))

      assert response["id"] == 5
      assert response["result"]["text"] == "Hello world"
      assert response["result"]["stopReason"] == "end_turn"
      assert response["result"]["sessionId"] == "t-1"
      assert status["params"]["update"]["sessionUpdate"] == "status"
      assert status["params"]["update"]["status"] == "completed"
      assert new_state.accumulated_text == []
      assert new_state.turn_id == nil
    end

    test "turn/completed with error status", %{state: state} do
      state = %{
        state
        | current_prompt_acp_id: 6,
          accumulated_text: []
      }

      line =
        Jason.encode!(%{
          "method" => "turn/completed",
          "params" => %{
            "turn" => %{"id" => "turn-1", "status" => "errored"},
            "threadId" => "t-1"
          }
        })

      assert {:messages, messages, _state} = Codex.translate_inbound(line, state)
      response = Enum.find(messages, &Map.has_key?(&1, "id"))
      assert response["result"]["stopReason"] == "error"
    end

    test "thread/tokenUsage/updated produces usage update", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "thread/tokenUsage/updated",
          "params" => %{
            "tokenUsage" => %{
              "total" => %{"inputTokens" => 100, "outputTokens" => 50, "cachedInputTokens" => 20}
            },
            "threadId" => "t-1"
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)
      assert msg["params"]["update"]["sessionUpdate"] == "usage"
      assert msg["params"]["update"]["content"]["inputTokens"] == 100
      assert msg["params"]["update"]["content"]["outputTokens"] == 50
    end

    test "error notification produces error update", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "error",
          "params" => %{
            "error" => %{"message" => "Rate limited"},
            "threadId" => "t-1"
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)
      assert msg["params"]["update"]["sessionUpdate"] == "error"
      assert msg["params"]["update"]["content"] == "Rate limited"
    end

    test "item/commandExecution/outputDelta produces tool_output", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/commandExecution/outputDelta",
          "params" => %{
            "delta" => "$ ls\nfoo.txt\n",
            "itemId" => "item-1",
            "threadId" => "t-1"
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)
      assert msg["params"]["update"]["sessionUpdate"] == "tool_output"
      assert msg["params"]["update"]["content"] == "$ ls\nfoo.txt\n"
      assert msg["params"]["update"]["itemId"] == "item-1"
    end

    test "unknown notifications are skipped", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "some/unknown/event",
          "params" => %{"threadId" => "t-1"}
        })

      assert {:skip, _state} = Codex.translate_inbound(line, state)
    end

    test "non-JSON lines are skipped", %{state: state} do
      assert {:skip, _state} = Codex.translate_inbound("not json", state)
    end
  end

  describe "streaming text accumulation" do
    test "accumulates multiple text deltas for turn completion", %{state: state} do
      state = %{
        state
        | thread_id: "t-1",
          turn_id: "turn-1",
          phase: :ready,
          current_prompt_acp_id: 10
      }

      # Simulate streaming deltas
      {:messages, _, state} =
        Codex.translate_inbound(
          Jason.encode!(%{
            "method" => "item/agentMessage/delta",
            "params" => %{"delta" => "Hello "}
          }),
          state
        )

      {:messages, _, state} =
        Codex.translate_inbound(
          Jason.encode!(%{
            "method" => "item/agentMessage/delta",
            "params" => %{"delta" => "world!"}
          }),
          state
        )

      # Turn completes
      {:messages, messages, _state} =
        Codex.translate_inbound(
          Jason.encode!(%{
            "method" => "turn/completed",
            "params" => %{
              "turn" => %{"id" => "turn-1", "status" => "completed"}
            }
          }),
          state
        )

      response = Enum.find(messages, &Map.has_key?(&1, "id"))
      assert response["result"]["text"] == "Hello world!"
    end
  end
end
