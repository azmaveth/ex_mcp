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
    test "returns stable ACP capabilities" do
      caps = Codex.capabilities()
      assert caps["loadSession"] == true
      assert caps["promptCapabilities"]["image"] == true
      assert caps["sessionCapabilities"]["resume"] == %{}
    end
  end

  describe "modes/0" do
    test "returns suggest, auto-edit, and full-auto modes" do
      modes = Codex.modes()
      ids = Enum.map(modes, & &1["id"])
      assert "suggest" in ids
      assert "auto-edit" in ids
      assert "full-auto" in ids
    end
  end

  describe "config_options/0" do
    test "does not advertise non-stable dynamic config options" do
      assert Codex.config_options() == []
    end
  end

  describe "session/set_config_option" do
    test "stores model for next turn", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "model", "value" => "gpt-5"}
      }

      assert {:ok, :skip, new_state} = Codex.translate_outbound(msg, state)
      assert new_state.model == "gpt-5"
    end
  end

  describe "session/set_mode" do
    # spec regression: Codex advertises three modes (`suggest`, `auto-edit`,
    # `full-auto`) via `modes/0`. Per ACP spec
    # (https://agentclientprotocol.com/protocol/session-modes), advertising
    # a mode is a promise that `session/set_mode` actually applies it.
    #
    # The previous implementation discarded the modeId in the set_mode
    # handler (`{:ok, :skip, state}` with no state mutation) and then never
    # read state.mode_id from `session/new` — so the advertised modes had
    # no effect. Any ACP client that trusts the advertisement would silently
    # get the wrong approval policy.
    #
    # The contract this test pins down: after `set_mode("suggest")`, the
    # next `session/new` MUST send `approvalPolicy: "suggest"` on the wire
    # (unless overridden by an explicit params["approvalPolicy"]).
    test "spec regression: set_mode stores modeId so it applies to next session/new",
         %{state: state} do
      set_mode_msg = %{
        "method" => "session/set_mode",
        "params" => %{"sessionId" => "ignored", "modeId" => "suggest"}
      }

      assert {:ok, :skip, state_after_set} = Codex.translate_outbound(set_mode_msg, state)

      # The mode MUST be stored in state — otherwise it cannot apply to
      # the next session/new.
      assert state_after_set.mode_id == "suggest",
             "set_mode must persist modeId in state. Codex advertises modes; storing nothing " <>
               "breaks the advertise→apply contract."

      # The next session/new MUST forward the stored mode as approvalPolicy
      # on the wire, unless params explicitly overrides it.
      session_new_msg = %{
        "method" => "session/new",
        "id" => 99,
        "params" => %{"cwd" => "/tmp/proj"}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(session_new_msg, state_after_set)
      {:ok, codex_msg} = Jason.decode(IO.iodata_to_binary(data))

      assert codex_msg["params"]["approvalPolicy"] == "suggest",
             ~s|After set_mode("suggest"), session/new must send approvalPolicy="suggest" | <>
               "to match the advertised mode. The adapter currently discards the modeId."
    end

    test "spec regression: explicit approvalPolicy in session/new params wins over stored mode",
         %{state: state} do
      # Caller-explicit > adapter-stored. This is the precedence ACP clients
      # will expect — if you pass approvalPolicy in session/new, it must
      # override the stored mode rather than the other way around.
      state_after_set = %{state | mode_id: "full-auto"}

      session_new_msg = %{
        "method" => "session/new",
        "id" => 100,
        "params" => %{"cwd" => "/tmp/proj", "approvalPolicy" => "suggest"}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(session_new_msg, state_after_set)
      {:ok, codex_msg} = Jason.decode(IO.iodata_to_binary(data))

      assert codex_msg["params"]["approvalPolicy"] == "suggest"
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
        "params" => %{"model" => "gpt-4o", "cwd" => "/tmp/proj"}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      json = IO.iodata_to_binary(data)
      {:ok, codex_msg} = Jason.decode(json)
      assert codex_msg["method"] == "thread/start"
      assert codex_msg["params"]["model"] == "gpt-4o"
      assert new_state.pending_requests[new_state.next_id - 1].type == :thread_start
      assert new_state.pending_requests[new_state.next_id - 1].acp_id == 2
    end

    # spec regression: the previous implementation used
    # `state.model || params["model"]` — meaning state's model (set via
    # adapter init or set_config_option) would override an explicit
    # caller-supplied model on `session/new`. That's the reverse of the
    # expected precedence: caller-explicit > adapter-default.
    #
    # This is the same bug class as Claude's `permission_mode`: the
    # caller supplies a value, the adapter returns `:ok`, but the wire
    # carries a different value than the caller asked for.
    test "spec regression: caller-supplied session/new params.model wins over state.model",
         %{state: state} do
      # State has a "default" model (from init opts or a prior set_config_option).
      state = %{state | model: "gpt-4o"}

      # Caller explicitly asks for gpt-5 in this session/new.
      msg = %{
        "method" => "session/new",
        "id" => 7,
        "params" => %{"model" => "gpt-5", "cwd" => "/tmp/proj"}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(msg, state)
      {:ok, codex_msg} = Jason.decode(IO.iodata_to_binary(data))

      assert codex_msg["params"]["model"] == "gpt-5",
             "Caller-explicit model in session/new params must win over state.model. " <>
               "The reverse precedence silently downgrades the caller's intent."
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
      assert msg["params"]["update"]["sessionUpdate"] == "agent_thought_chunk"

      assert msg["params"]["update"]["content"] == %{
               "type" => "text",
               "text" => "Let me think..."
             }

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

    # spec regression: the ACP spec
    # (https://agentclientprotocol.com/protocol/prompt-turn) defines
    # `sessionUpdate: "usage_update"` with `used`/`size`/`cost?` fields —
    # NOT `"usage"` with `inputTokens`/`outputTokens`. The previous
    # implementation emitted a non-spec discriminator that other ACP
    # clients (TypeScript reference, Zed, etc.) cannot recognize.
    #
    # If we want to surface input/output token billing data, the spec
    # offers `_meta` on every type, OR the prompt response result's
    # `usage` extension (which Pi already uses).
    test "spec regression: thread/tokenUsage/updated emits spec-compliant usage_update or nothing",
         %{state: state} do
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

      result = Codex.translate_inbound(line, state)

      # Either the adapter drops the non-spec emission entirely...
      case result do
        {:skip, _state} ->
          :ok

        {:messages, msgs, _state} ->
          # ...or every emitted session/update must use a spec-stable
          # discriminator. The non-spec "usage" is the bug.
          for msg <- msgs, msg["method"] == "session/update" do
            discriminator = msg["params"]["update"]["sessionUpdate"]

            refute discriminator == "usage",
                   "Non-spec sessionUpdate discriminator. Use \"usage_update\" with " <>
                     "used/size/cost per https://agentclientprotocol.com/protocol/prompt-turn"

            # If the adapter emits usage_update, it must have the spec fields.
            if discriminator == "usage_update" do
              update = msg["params"]["update"]
              assert Map.has_key?(update, "used"), "usage_update requires `used` per spec"
              assert Map.has_key?(update, "size"), "usage_update requires `size` per spec"
            end
          end
      end
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

    test "item/commandExecution/started produces stable tool_call", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/commandExecution/started",
          "params" => %{
            "command" => "mix test",
            "itemId" => "item-1",
            "threadId" => "t-1"
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)
      update = msg["params"]["update"]
      assert update["sessionUpdate"] == "tool_call"
      assert update["toolCallId"] == "item-1"
      assert update["kind"] == "execute"
      assert update["status"] == "in_progress"
      assert update["rawInput"] == %{"command" => "mix test"}
    end

    test "item/commandExecution/outputDelta produces stable tool_call_update", %{state: state} do
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
      update = msg["params"]["update"]
      assert update["sessionUpdate"] == "tool_call_update"
      assert update["toolCallId"] == "item-1"

      assert update["content"] == [
               %{
                 "type" => "content",
                 "content" => %{"type" => "text", "text" => "$ ls\nfoo.txt\n"}
               }
             ]
    end

    test "item/commandExecution/completed produces stable tool_call_update", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/commandExecution/completed",
          "params" => %{
            "output" => "ok\n",
            "exitCode" => 0,
            "itemId" => "item-1",
            "threadId" => "t-1"
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)
      update = msg["params"]["update"]
      assert update["sessionUpdate"] == "tool_call_update"
      assert update["toolCallId"] == "item-1"
      assert update["status"] == "completed"
      assert update["rawOutput"] == %{"exitCode" => 0, "output" => "ok\n"}
    end

    test "item/completed function_call_output produces stable tool_call_update", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/completed",
          "params" => %{
            "item" => %{
              "type" => "function_call_output",
              "callId" => "call-1",
              "output" => "done",
              "isError" => false
            },
            "threadId" => "t-1"
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)
      update = msg["params"]["update"]
      assert update["sessionUpdate"] == "tool_call_update"
      assert update["toolCallId"] == "call-1"
      assert update["status"] == "completed"

      assert update["content"] == [
               %{"type" => "content", "content" => %{"type" => "text", "text" => "done"}}
             ]
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
