defmodule ExMCP.ACP.Adapters.PiTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Pi

  setup do
    tmp_dir = Path.join(System.tmp_dir!(), "pi_test_#{System.unique_integer([:positive])}")
    session_dir = Path.join(tmp_dir, "sessions")
    session_map_path = Path.join(tmp_dir, "session-map.json")

    File.mkdir_p!(session_dir)

    {:ok, state} =
      Pi.init(
        cwd: tmp_dir,
        session_dir: session_dir,
        session_map_path: session_map_path
      )

    on_exit(fn -> File.rm_rf!(tmp_dir) end)

    %{
      state: state,
      tmp_dir: tmp_dir,
      session_dir: session_dir,
      session_map_path: session_map_path
    }
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

    test "does not pass api key through process argv" do
      {_cmd, args} = Pi.command(api_key: "secret")
      refute "--api-key" in args
      refute "secret" in args
    end

    test "includes no-session flag" do
      {_cmd, args} = Pi.command(no_session: true)
      assert "--no-session" in args
    end
  end

  describe "capabilities/0" do
    test "returns ACP-native capabilities and adapter metadata" do
      caps = Pi.capabilities()
      pi_meta = caps["_meta"]["ex_mcp.pi"]

      assert caps["loadSession"] == true
      assert caps["promptCapabilities"]["image"] == true
      assert caps["promptCapabilities"]["audio"] == false
      assert caps["mcpCapabilities"] == %{"http" => false, "sse" => false}
      refute Map.has_key?(pi_meta, "methods")
      assert pi_meta["features"]["slashCommands"] == true
      assert pi_meta["features"]["terminalAuth"] == true
      assert pi_meta["features"]["modelSelection"] == true
    end
  end

  describe "auth_methods/1" do
    test "advertises terminal login" do
      assert [%{"id" => "pi_terminal_login", "type" => "terminal"} = auth] =
               Pi.auth_methods(cli_path: "/bin/pi")

      assert auth["_meta"]["terminal-auth"]["command"] == "/bin/pi"
    end
  end

  describe "modes/0" do
    test "returns Pi thinking levels as ACP modes" do
      ids = Pi.modes() |> Enum.map(& &1["id"])
      assert ids == ["off", "minimal", "low", "medium", "high", "xhigh"]
    end
  end

  describe "config_options/0" do
    test "returns Pi runtime config options but not model or thinking selectors" do
      ids = Pi.config_options() |> Enum.map(& &1["id"])

      assert "auto_compaction" in ids
      assert "auto_retry" in ids
      assert "steering_mode" in ids
      assert "follow_up_mode" in ids
      refute "model" in ids
      refute "thinking_level" in ids
    end
  end

  describe "list_sessions/2" do
    test "returns empty list when session dir doesn't exist", %{state: state} do
      state = %{state | session_dir: "/nonexistent/path"}
      assert {:ok, [], _state} = Pi.list_sessions(%{}, state)
    end

    test "scans Pi jsonl session files and omits private file paths", %{
      state: state,
      session_dir: session_dir,
      tmp_dir: tmp_dir
    } do
      write_session(session_dir, "s1", tmp_dir, "First prompt", "Project session")
      write_session(session_dir, "s2", "/other/project", "Other prompt", nil)

      assert {:ok, sessions, _state} = Pi.list_sessions(%{}, state)

      assert Enum.map(sessions, & &1["sessionId"]) |> Enum.sort() == ["s1", "s2"]
      assert Enum.all?(sessions, &(not Map.has_key?(&1, "sessionFile")))
      assert Enum.any?(sessions, &(&1["title"] == "Project session"))
    end

    test "filters by cwd", %{state: state, session_dir: session_dir, tmp_dir: tmp_dir} do
      write_session(session_dir, "s1", tmp_dir, "First prompt", nil)
      write_session(session_dir, "s2", "/other/project", "Other prompt", nil)

      assert {:ok, [%{"sessionId" => "s1"}], _state} =
               Pi.list_sessions(%{"cwd" => tmp_dir}, state)
    end
  end

  describe "translate_outbound/2 — session lifecycle" do
    test "session/new sends Pi control requests and completes from correlated responses", %{
      state: state,
      tmp_dir: tmp_dir
    } do
      msg = %{"method" => "session/new", "id" => 11, "params" => %{"cwd" => tmp_dir}}

      assert {:ok, data, state} = Pi.translate_outbound(msg, state)
      requests = decode_many(data)

      assert Enum.map(requests, & &1["type"]) == [
               "new_session",
               "get_state",
               "get_available_models",
               "get_commands"
             ]

      ids_by_type = Map.new(requests, &{&1["type"], &1["id"]})

      state =
        respond(state, ids_by_type["new_session"], "new_session", %{})
        |> respond(ids_by_type["get_state"], "get_state", %{
          "sessionId" => "pi-session",
          "sessionFile" => Path.join(tmp_dir, "pi-session.jsonl"),
          "cwd" => tmp_dir,
          "thinkingLevel" => "high",
          "model" => %{"provider" => "anthropic", "id" => "claude-sonnet-4"}
        })
        |> respond(ids_by_type["get_available_models"], "get_available_models", %{
          "models" => [
            %{"provider" => "anthropic", "id" => "claude-sonnet-4", "name" => "Claude Sonnet 4"}
          ]
        })

      assert {:messages, [response, commands_update], state} =
               Pi.translate_inbound(
                 response_line(ids_by_type["get_commands"], "get_commands", %{
                   "commands" => [
                     %{
                       "name" => "model",
                       "description" => "Model picker",
                       "source" => "extension"
                     }
                   ]
                 }),
                 state
               )

      assert response["id"] == 11
      assert response["result"]["sessionId"] == "pi-session"
      assert response["result"]["modes"]["currentModeId"] == "high"
      assert response["result"]["models"]["currentModelId"] == "anthropic/claude-sonnet-4"
      assert commands_update["params"]["update"]["sessionUpdate"] == "available_commands_update"
      assert state.session_id == "pi-session"

      assert state.available_models == [
               %{
                 "modelId" => "anthropic/claude-sonnet-4",
                 "name" => "anthropic/Claude Sonnet 4",
                 "description" => nil
               }
             ]
    end

    test "session/new maps empty model list to auth-required", %{state: state, tmp_dir: tmp_dir} do
      msg = %{"method" => "session/new", "id" => 12, "params" => %{"cwd" => tmp_dir}}

      assert {:ok, data, state} = Pi.translate_outbound(msg, state)
      ids_by_type = data |> decode_many() |> Map.new(&{&1["type"], &1["id"]})

      state =
        respond(state, ids_by_type["new_session"], "new_session", %{})
        |> respond(ids_by_type["get_state"], "get_state", %{
          "sessionId" => "pi-session",
          "cwd" => tmp_dir
        })
        |> respond(ids_by_type["get_commands"], "get_commands", %{"commands" => []})

      assert {:messages, [error], _state} =
               Pi.translate_inbound(
                 response_line(ids_by_type["get_available_models"], "get_available_models", %{
                   "models" => []
                 }),
                 state
               )

      assert error["id"] == 12
      assert error["error"]["data"]["authMethods"] != []
    end

    test "session/load uses the local session map and replays messages", %{
      state: state,
      tmp_dir: tmp_dir,
      session_map_path: session_map_path
    } do
      session_file = Path.join(tmp_dir, "mapped.jsonl")

      File.write!(
        session_map_path,
        Jason.encode!(%{
          "version" => 1,
          "sessions" => %{
            "mapped-session" => %{
              "sessionId" => "mapped-session",
              "cwd" => tmp_dir,
              "sessionFile" => session_file
            }
          }
        })
      )

      msg = %{
        "method" => "session/load",
        "id" => 13,
        "params" => %{"sessionId" => "mapped-session", "cwd" => tmp_dir}
      }

      assert {:ok, data, state} = Pi.translate_outbound(msg, state)
      requests = decode_many(data)
      assert Enum.find(requests, &(&1["type"] == "switch_session"))["sessionPath"] == session_file
      ids_by_type = Map.new(requests, &{&1["type"], &1["id"]})

      state =
        respond(state, ids_by_type["switch_session"], "switch_session", %{})
        |> respond(ids_by_type["get_state"], "get_state", %{
          "sessionId" => "mapped-session",
          "cwd" => tmp_dir
        })
        |> respond(ids_by_type["get_available_models"], "get_available_models", %{
          "models" => [%{"provider" => "openai", "id" => "gpt-5.1"}]
        })
        |> respond(ids_by_type["get_commands"], "get_commands", %{"commands" => []})

      assert {:messages, messages, _state} =
               Pi.translate_inbound(
                 response_line(ids_by_type["get_messages"], "get_messages", %{
                   "messages" => [
                     %{"role" => "user", "content" => "Hello"},
                     %{"role" => "assistant", "content" => "Hi"}
                   ]
                 }),
                 state
               )

      assert Enum.any?(
               messages,
               &(get_in(&1, ["params", "update", "sessionUpdate"]) == "user_message_chunk")
             )

      assert Enum.any?(messages, &(&1["id"] == 13))
    end
  end

  describe "translate_outbound/2 — model, mode, and config" do
    test "session/set_model routes full ACP model IDs to Pi set_model", %{state: state} do
      msg = %{
        "method" => "session/set_model",
        "params" => %{"modelId" => "anthropic/claude-sonnet-4"}
      }

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      decoded = decode_one(data)
      assert decoded["type"] == "set_model"
      assert decoded["provider"] == "anthropic"
      assert decoded["modelId"] == "claude-sonnet-4"
      assert new_state.current_model_id == "anthropic/claude-sonnet-4"
    end

    test "session/set_model resolves bare model IDs from available models", %{state: state} do
      state = %{state | available_models: [%{"modelId" => "openai/gpt-5.1"}]}
      msg = %{"method" => "session/set_model", "params" => %{"modelId" => "gpt-5.1"}}

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      decoded = decode_one(data)
      assert decoded["provider"] == "openai"
      assert decoded["modelId"] == "gpt-5.1"
      assert new_state.current_model_id == "openai/gpt-5.1"
    end

    test "session/set_model rejects unknown bare IDs", %{state: state} do
      msg = %{"method" => "session/set_model", "params" => %{"modelId" => "missing-model"}}

      assert {:error, "Unknown modelId: missing-model", ^state} =
               Pi.translate_outbound(msg, state)
    end

    test "session/set_mode routes thinking level and emits current_mode_update", %{state: state} do
      msg = %{
        "method" => "session/set_mode",
        "params" => %{"sessionId" => "s1", "modeId" => "high"}
      }

      assert {:messages_and_write, [update], data, new_state} = Pi.translate_outbound(msg, state)
      assert update["params"]["update"]["currentModeId"] == "high"
      assert decode_one(data)["type"] == "set_thinking_level"
      assert new_state.thinking_level == "high"
    end

    test "session/set_mode rejects invalid thinking levels", %{state: state} do
      msg = %{"method" => "session/set_mode", "params" => %{"modeId" => "invalid"}}
      assert {:error, "Unknown modeId: \"invalid\"", ^state} = Pi.translate_outbound(msg, state)
    end

    test "auto_compaction routes to set_auto_compaction", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "auto_compaction", "value" => false}
      }

      assert {:ok, data, _state} = Pi.translate_outbound(msg, state)
      assert decode_one(data) == %{"type" => "set_auto_compaction", "enabled" => false}
    end

    test "unknown config option returns an error", %{state: state} do
      msg = %{
        "method" => "session/set_config_option",
        "params" => %{"configId" => "nonexistent", "value" => "x"}
      }

      assert {:error, "Unknown Pi config option: nonexistent", ^state} =
               Pi.translate_outbound(msg, state)
    end
  end

  describe "translate_outbound/2 — prompting and slash commands" do
    test "session/prompt produces Pi RPC prompt with content block normalization", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => 21,
        "params" => %{
          "sessionId" => "s1",
          "prompt" => [
            %{"type" => "text", "text" => "Hello"},
            %{"type" => "resource_link", "uri" => "file:///tmp/example.ex"},
            %{"type" => "audio", "mimeType" => "audio/wav", "data" => "abc"}
          ]
        }
      }

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      decoded = decode_one(data)
      assert decoded["type"] == "prompt"
      assert decoded["message"] =~ "Hello"
      assert decoded["message"] =~ "[Context] file:///tmp/example.ex"
      assert decoded["message"] =~ "[Audio]"
      assert new_state.pending_prompt.acp_id == 21
    end

    test "image prompts pass normalized images to Pi", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => 22,
        "params" => %{
          "prompt" => [
            %{"type" => "text", "text" => "What is this?"},
            %{
              "type" => "image",
              "mimeType" => "image/png",
              "data" => "data:image/png;base64,abc123"
            }
          ]
        }
      }

      assert {:ok, data, _state} = Pi.translate_outbound(msg, state)
      decoded = decode_one(data)

      assert decoded["images"] == [
               %{"type" => "image", "mimeType" => "image/png", "data" => "abc123"}
             ]
    end

    test "queued prompts stay pending and run after the active turn ends", %{state: state} do
      state = %{
        state
        | pending_prompt: %{acp_id: 1, msg_id: "msg-1", cancel_requested: false},
          session_id: "s1"
      }

      msg = %{
        "method" => "session/prompt",
        "id" => 23,
        "params" => %{"sessionId" => "s1", "prompt" => "second"}
      }

      assert {:messages, [_notice, _info], queued_state} = Pi.translate_outbound(msg, state)
      assert :queue.len(queued_state.prompt_queue) == 1

      assert {:messages_and_write, [first_response, _start_notice, _queue_info], data, next_state} =
               Pi.translate_inbound(
                 Jason.encode!(%{"type" => "agent_end", "messages" => []}),
                 queued_state
               )

      assert first_response["id"] == 1
      assert decode_one(data)["message"] == "second"
      assert next_state.pending_prompt.acp_id == 23
    end

    test "session/cancel cancels queued prompts and marks active prompt", %{state: state} do
      queue =
        :queue.in(%{acp_id: 31, message: "queued", images: [], params: %{}}, :queue.new())

      state = %{
        state
        | session_id: "s1",
          pending_prompt: %{acp_id: 30, msg_id: "msg-1", cancel_requested: false},
          prompt_queue: queue
      }

      assert {:messages_and_write, messages, data, new_state} =
               Pi.translate_outbound(%{"method" => "session/cancel"}, state)

      assert Enum.any?(messages, &(&1["id"] == 31))

      assert Enum.any?(
               messages,
               &(get_in(&1, ["params", "update", "content", "text"]) == "Cleared queued prompts.")
             )

      assert decode_one(data)["type"] == "abort"
      assert new_state.pending_prompt.cancel_requested == true
      assert :queue.is_empty(new_state.prompt_queue)
    end

    test "slash compact maps to Pi compact and replies after control response", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => 24,
        "params" => %{"sessionId" => "s1", "prompt" => "/compact keep tests"}
      }

      assert {:ok, data, state} = Pi.translate_outbound(msg, %{state | session_id: "s1"})
      request = decode_one(data)
      assert request["type"] == "compact"
      assert request["customInstructions"] == "keep tests"

      assert {:messages, [message, response], _state} =
               Pi.translate_inbound(
                 response_line(request["id"], "compact", %{
                   "summary" => "done",
                   "tokensBefore" => 12
                 }),
                 state
               )

      assert get_in(message, ["params", "update", "content", "text"]) =~ "Compaction completed."
      assert response["id"] == 24
      assert response["result"]["stopReason"] == "end_turn"
    end

    test "file slash commands expand to prompts", %{state: state, tmp_dir: tmp_dir} do
      prompts_dir = Path.join([tmp_dir, ".pi", "prompts"])
      File.mkdir_p!(prompts_dir)
      File.write!(Path.join(prompts_dir, "review.md"), "Review $1 and $@")

      msg = %{"method" => "session/new", "id" => 25, "params" => %{"cwd" => tmp_dir}}
      assert {:ok, _data, state} = Pi.translate_outbound(msg, state)

      state = %{
        state
        | file_commands:
            state.control_groups |> Map.values() |> hd() |> Map.fetch!(:file_commands)
      }

      prompt_msg = %{
        "method" => "session/prompt",
        "id" => 26,
        "params" => %{"prompt" => "/review src all files"}
      }

      assert {:ok, data, _state} = Pi.translate_outbound(prompt_msg, state)
      assert decode_one(data)["message"] == "Review src and src all files"
    end

    test "removed extension methods return explicit errors", %{state: state} do
      msg = %{"method" => "_ex_mcp.pi/steer", "params" => %{"message" => "look at diff"}}

      assert {:error,
              "Pi extension methods were removed; use ACP session methods or slash commands",
              ^state} = Pi.translate_outbound(msg, state)
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
      assert update["sessionUpdate"] == "agent_message_chunk"
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
      assert update["sessionUpdate"] == "agent_thought_chunk"
      assert update["content"] == %{"type" => "text", "text" => "Let me think..."}
    end
  end

  describe "translate_inbound/2 — agent_end" do
    test "produces prompt response with accumulated text and usage", %{state: state} do
      state = %{
        state
        | session_id: "s1",
          pending_prompt: %{acp_id: 5, msg_id: "msg-1", cancel_requested: false},
          text_acc: ["world", "Hello "]
      }

      line =
        Jason.encode!(%{
          "type" => "agent_end",
          "messages" => [%{"role" => "assistant", "usage" => %{"input" => 10, "output" => 2}}]
        })

      assert {:messages, [response], new_state} = Pi.translate_inbound(line, state)
      assert response["id"] == 5
      assert response["result"]["_meta"]["ex_mcp"]["text"] == "Hello world"
      assert response["result"]["usage"]["inputTokens"] == 10
      assert response["result"]["stopReason"] == "end_turn"
      assert new_state.pending_prompt == nil
      assert new_state.text_acc == []
    end
  end

  describe "translate_inbound/2 — tool execution" do
    test "tool_execution_start emits a new tool call with locations", %{
      state: state,
      tmp_dir: tmp_dir
    } do
      line =
        Jason.encode!(%{
          "type" => "tool_execution_start",
          "toolCallId" => "tc-1",
          "toolName" => "read",
          "args" => %{"path" => "lib/example.ex"}
        })

      assert {:messages, [notification], _state} =
               Pi.translate_inbound(line, %{state | cwd: tmp_dir})

      update = notification["params"]["update"]
      assert update["sessionUpdate"] == "tool_call"
      assert update["status"] == "in_progress"
      assert update["title"] == "read"
      assert hd(update["locations"])["path"] == Path.join(tmp_dir, "lib/example.ex")
    end

    test "toolcall_start and toolcall_delta emit call then update", %{state: state} do
      start =
        Jason.encode!(%{
          "type" => "message_update",
          "assistantMessageEvent" => %{
            "type" => "toolcall_start",
            "toolCall" => %{
              "id" => "tc-stream",
              "name" => "bash",
              "arguments" => %{"command" => "ls"}
            }
          }
        })

      assert {:messages, [call], state} = Pi.translate_inbound(start, state)
      assert call["params"]["update"]["sessionUpdate"] == "tool_call"

      delta =
        Jason.encode!(%{
          "type" => "message_update",
          "assistantMessageEvent" => %{
            "type" => "toolcall_delta",
            "toolCall" => %{
              "id" => "tc-stream",
              "name" => "bash",
              "arguments" => %{"command" => "ls -la"}
            }
          }
        })

      assert {:messages, [update], _state} = Pi.translate_inbound(delta, state)
      assert update["params"]["update"]["sessionUpdate"] == "tool_call_update"
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
      assert update["sessionUpdate"] == "tool_call_update"
      assert update["status"] == "completed"
      assert hd(update["content"])["content"]["text"] == "file1.txt"
    end

    test "edit tool completion emits structured diff when the file changed", %{
      state: state,
      tmp_dir: tmp_dir
    } do
      path = Path.join(tmp_dir, "example.txt")
      File.write!(path, "old\n")

      start =
        Jason.encode!(%{
          "type" => "tool_execution_start",
          "toolCallId" => "edit-1",
          "toolName" => "edit",
          "args" => %{"path" => path, "oldText" => "old"}
        })

      assert {:messages, [_notification], state} = Pi.translate_inbound(start, state)
      File.write!(path, "new\n")

      finish =
        Jason.encode!(%{
          "type" => "tool_execution_end",
          "toolCallId" => "edit-1",
          "result" => %{"content" => [%{"type" => "text", "text" => "updated"}]},
          "isError" => false
        })

      assert {:messages, [notification], _state} = Pi.translate_inbound(finish, state)
      update = notification["params"]["update"]
      assert hd(update["content"])["type"] == "diff"
      assert hd(update["content"])["oldText"] == "old\n"
      assert hd(update["content"])["newText"] == "new\n"
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

  defp decode_many(data) do
    data
    |> IO.iodata_to_binary()
    |> String.split("\n", trim: true)
    |> Enum.map(&Jason.decode!/1)
  end

  defp decode_one(data), do: data |> decode_many() |> List.first()

  defp response_line(id, command, data) do
    Jason.encode!(%{
      "type" => "response",
      "id" => id,
      "command" => command,
      "success" => true,
      "data" => data
    })
  end

  defp respond(state, id, command, data) do
    assert {:skip, state} = Pi.translate_inbound(response_line(id, command, data), state)
    state
  end

  defp write_session(session_dir, id, cwd, first_prompt, name) do
    file = Path.join(session_dir, "#{id}.jsonl")

    lines =
      [
        %{"type" => "session", "id" => id, "cwd" => cwd, "timestamp" => "2026-01-01T00:00:00Z"},
        %{
          "type" => "message",
          "timestamp" => "2026-01-01T00:00:01Z",
          "message" => %{"role" => "user", "content" => first_prompt}
        },
        if(name,
          do: %{"type" => "session_info", "name" => name, "timestamp" => "2026-01-01T00:00:02Z"}
        )
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.map_join("\n", &Jason.encode!/1)

    File.write!(file, lines <> "\n")
  end
end
