defmodule ExMCP.ACP.Adapters.CodexTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex

  setup do
    {:ok, state} = Codex.init([])
    %{state: state}
  end

  describe "static adapter metadata" do
    test "returns codex app-server command" do
      assert {"codex", ["app-server"]} = Codex.command([])
    end

    test "advertises only implemented ACP capabilities" do
      caps = Codex.capabilities()

      assert caps["loadSession"] == true
      assert caps["promptCapabilities"] == %{"image" => true, "embeddedContext" => true}
      assert caps["mcpCapabilities"]["http"] == true
      assert caps["sessionCapabilities"]["setModel"] == %{}
      assert caps["sessionCapabilities"]["list"] == %{}
      assert caps["sessionCapabilities"]["resume"] == %{}
      assert caps["sessionCapabilities"]["close"] == %{}
      assert caps["auth"]["logout"] == %{}
    end

    test "matches upstream Codex ACP mode ids" do
      ids = Codex.modes() |> Enum.map(& &1["id"])
      assert ids == ["read-only", "agent", "agent-full-access"]
    end

    test "advertises Codex auth methods" do
      ids = Codex.auth_methods([]) |> Enum.map(& &1["id"])
      assert "chat-gpt" in ids
      assert "api-key" in ids

      api_key = Enum.find(Codex.auth_methods([]), &(&1["id"] == "api-key"))
      assert api_key["_meta"]["api-key"]["provider"] == "openai"
    end
  end

  describe "post_connect/1" do
    test "sends initialize request", %{state: state} do
      {:ok, data, new_state} = Codex.post_connect(state)
      msg = decode(data)

      assert msg["method"] == "initialize"
      assert msg["id"] == 1
      assert msg["params"]["clientInfo"]["name"] == "ex_mcp"
      assert new_state.next_id == 2
      assert new_state.pending_requests[1].type == :initialize
    end
  end

  describe "session lifecycle outbound mapping" do
    test "session/new sends thread/start with mode and MCP config", %{state: state} do
      msg = %{
        "method" => "session/new",
        "id" => 2,
        "params" => %{
          "model" => "gpt-5",
          "cwd" => "/tmp/project",
          "mcpServers" => [
            %{
              "type" => "http",
              "name" => "remote tools",
              "url" => "http://localhost:4000/mcp",
              "headers" => [%{"name" => "Authorization", "value" => "Bearer token"}]
            },
            %{
              "type" => "stdio",
              "name" => "local tools",
              "command" => "tools",
              "args" => ["--stdio"],
              "env" => [%{"name" => "A", "value" => "B"}]
            }
          ]
        }
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "thread/start"
      assert codex_msg["params"]["model"] == "gpt-5"
      assert codex_msg["params"]["cwd"] == "/tmp/project"
      assert codex_msg["params"]["sandbox"] == "workspace-write"
      assert codex_msg["params"]["approvalPolicy"] == "on-request"

      assert get_in(codex_msg, ["params", "config", "mcp_servers", "remote_tools", "url"]) ==
               "http://localhost:4000/mcp"

      assert get_in(codex_msg, [
               "params",
               "config",
               "mcp_servers",
               "remote_tools",
               "http_headers"
             ]) ==
               %{"Authorization" => "Bearer token"}

      assert get_in(codex_msg, ["params", "config", "mcp_servers", "local_tools", "command"]) ==
               "tools"

      refute get_in(codex_msg, ["params", "config", "mcp_servers", "local_tools", "cwd"])

      assert new_state.pending_requests[new_state.next_id - 1].type == :thread_start
      assert new_state.pending_requests[new_state.next_id - 1].acp_id == 2
    end

    test "session/load sends thread/resume and caller model wins", %{state: state} do
      state = %{state | model: "gpt-4o"}

      msg = %{
        "method" => "session/load",
        "id" => 3,
        "params" => %{"sessionId" => "thread-1", "cwd" => "/tmp/project", "model" => "gpt-5"}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "thread/resume"
      assert codex_msg["params"]["threadId"] == "thread-1"
      assert codex_msg["params"]["model"] == "gpt-5"
      assert codex_msg["params"]["initialTurnsPage"]["itemsView"] == "full"
    end

    test "session/list sends thread/list", %{state: state} do
      msg = %{
        "method" => "session/list",
        "id" => 4,
        "params" => %{"cwd" => "/tmp/project", "cursor" => "abc"}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "thread/list"
      assert codex_msg["params"]["cwd"] == "/tmp/project"
      assert codex_msg["params"]["cursor"] == "abc"
      assert codex_msg["params"]["archived"] == false
    end

    test "session/close interrupts active turn and unsubscribes", %{state: state} do
      state = put_test_session(state, "thread-1", %{turn_id: "turn-1"})

      msg = %{"method" => "session/close", "id" => 5, "params" => %{"sessionId" => "thread-1"}}

      assert {:reply_and_write, %{}, data, new_state} = Codex.translate_outbound(msg, state)
      [interrupt, unsubscribe] = decode_lines(data)

      assert interrupt["method"] == "turn/interrupt"
      assert interrupt["params"] == %{"threadId" => "thread-1", "turnId" => "turn-1"}
      assert unsubscribe["method"] == "thread/unsubscribe"
      assert unsubscribe["params"] == %{"threadId" => "thread-1"}
      refute Map.has_key?(new_state.sessions, "thread-1")
    end
  end

  describe "prompt and config outbound mapping" do
    test "session/prompt requires a known session", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => 6,
        "params" => %{"sessionId" => "missing", "prompt" => [%{"type" => "text", "text" => "hi"}]}
      }

      assert {:error, "Unknown Codex session: missing", ^state} =
               Codex.translate_outbound(msg, state)
    end

    test "session/prompt maps text, images, resource links, and embedded text", %{state: state} do
      state = put_test_session(state, "thread-1")

      msg = %{
        "method" => "session/prompt",
        "id" => 7,
        "params" => %{
          "sessionId" => "thread-1",
          "prompt" => [
            %{"type" => "text", "text" => "Review "},
            %{"type" => "resource_link", "name" => "lib.ex", "uri" => "file:///tmp/lib.ex"},
            %{
              "type" => "resource",
              "resource" => %{"uri" => "file:///tmp/context.md", "text" => "extra context"}
            },
            %{"type" => "image", "mimeType" => "image/png", "data" => "abc"}
          ]
        }
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "turn/start"
      assert codex_msg["params"]["threadId"] == "thread-1"
      assert Enum.at(codex_msg["params"]["input"], 1)["text"] == "[@lib.ex](file:///tmp/lib.ex)"

      assert Enum.at(codex_msg["params"]["input"], 2)["text"] =~
               ~s(<context ref="file:///tmp/context.md">)

      assert Enum.at(codex_msg["params"]["input"], 3)["url"] == "data:image/png;base64,abc"

      session = new_state.sessions["thread-1"]
      assert session.active_prompt_acp_id == 7
      assert session.accumulated_text == []
    end

    test "session/prompt maps slash compact to native app-server request", %{state: state} do
      state = put_test_session(state, "thread-1")

      msg = %{
        "method" => "session/prompt",
        "id" => 8,
        "params" => %{
          "sessionId" => "thread-1",
          "prompt" => [%{"type" => "text", "text" => "/compact"}]
        }
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "thread/compact/start"
      assert codex_msg["params"] == %{"threadId" => "thread-1"}
      assert new_state.sessions["thread-1"].active_prompt_acp_id == 8
      assert new_state.pending_requests[new_state.next_id - 1].type == :prompt_command_start
    end

    test "session/prompt maps slash review targets to review/start", %{state: state} do
      state = put_test_session(state, "thread-1")

      msg = %{
        "method" => "session/prompt",
        "id" => 9,
        "params" => %{
          "sessionId" => "thread-1",
          "prompt" => [%{"type" => "text", "text" => "/review-branch main"}]
        }
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "review/start"
      assert codex_msg["params"]["threadId"] == "thread-1"
      assert codex_msg["params"]["delivery"] == "inline"
      assert codex_msg["params"]["target"] == %{"type" => "baseBranch", "branch" => "main"}
      assert new_state.sessions["thread-1"].active_prompt_acp_id == 9
    end

    test "session/cancel sends turn/interrupt for active turn", %{state: state} do
      state = put_test_session(state, "thread-1", %{turn_id: "turn-1"})

      msg = %{"method" => "session/cancel", "params" => %{"sessionId" => "thread-1"}}

      assert {:ok, data, _state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "turn/interrupt"
      assert codex_msg["params"] == %{"threadId" => "thread-1", "turnId" => "turn-1"}
    end

    test "session/set_mode updates current session state", %{
      state: state
    } do
      state = put_test_session(state, "thread-1")

      msg = %{
        "method" => "session/set_mode",
        "id" => 8,
        "params" => %{"sessionId" => "thread-1", "modeId" => "agent-full-access"}
      }

      assert {:messages_and_reply, [update], %{}, new_state} =
               Codex.translate_outbound(msg, state)

      assert update["params"]["update"]["sessionUpdate"] == "current_mode_update"
      assert update["params"]["update"]["currentModeId"] == "agent-full-access"
      assert new_state.sessions["thread-1"].mode_id == "agent-full-access"
    end

    test "session/set_model updates model and reasoning effort from catalog", %{state: state} do
      state =
        state
        |> put_catalog_models()
        |> put_test_session("thread-1", %{model: "gpt-5-codex", reasoning_effort: "medium"})

      msg = %{
        "method" => "session/set_model",
        "id" => 9,
        "params" => %{"sessionId" => "thread-1", "modelId" => "codex-mini/high"}
      }

      assert {:reply, result, new_state} = Codex.translate_outbound(msg, state)
      assert result["models"]["currentModelId"] == "codex-mini/high"
      assert Enum.any?(result["models"]["availableModels"], &(&1["modelId"] == "codex-mini/high"))
      assert new_state.sessions["thread-1"].model == "gpt-5-codex"
      assert new_state.sessions["thread-1"].model_id == "codex-mini/high"
    end

    test "session/set_config_option updates model and returns current options", %{state: state} do
      state = put_test_session(state, "thread-1")

      msg = %{
        "method" => "session/set_config_option",
        "id" => 9,
        "params" => %{"sessionId" => "thread-1", "configId" => "model", "value" => "gpt-5"}
      }

      assert {:reply, result, new_state} = Codex.translate_outbound(msg, state)
      assert new_state.sessions["thread-1"].model == "gpt-5"

      assert Enum.any?(
               result["configOptions"],
               &(&1["id"] == "model" && &1["currentValue"] == "gpt-5")
             )
    end
  end

  describe "auth outbound mapping" do
    test "chatgpt authenticate starts app-server login", %{state: state} do
      msg = %{"method" => "authenticate", "id" => 10, "params" => %{"methodId" => "chat-gpt"}}

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "account/login/start"
      assert codex_msg["params"] == %{"type" => "chatgpt"}
      assert new_state.pending_requests[new_state.next_id - 1].type == :authenticate
    end

    test "api key authenticate requires explicit adapter env" do
      {:ok, state} = Codex.init(env: [{"CODEX_API_KEY", "codex-key"}])

      msg = %{
        "method" => "authenticate",
        "id" => 11,
        "params" => %{"methodId" => "api-key"}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "account/login/start"
      assert codex_msg["params"] == %{"type" => "apiKey", "apiKey" => "codex-key"}
    end

    test "api key authenticate does not read ambient system env", %{state: state} do
      msg = %{
        "method" => "authenticate",
        "id" => 12,
        "params" => %{"methodId" => "api-key"}
      }

      assert {:error, message, ^state} = Codex.translate_outbound(msg, state)
      assert message =~ "CODEX_API_KEY or OPENAI_API_KEY must be supplied explicitly"
    end
  end

  describe "inbound responses" do
    test "initialize response triggers initialized write-back and model catalog request", %{
      state: state
    } do
      state = %{
        state
        | next_id: 2,
          pending_requests: %{1 => %{type: :initialize, acp_id: nil, meta: %{}}}
      }

      line = Jason.encode!(%{"id" => 1, "result" => %{"capabilities" => %{}}})
      assert {:skip_and_write, data, new_state} = Codex.translate_inbound(line, state)

      [initialized, model_list] = decode_lines(data)
      assert initialized["method"] == "initialized"
      assert model_list["method"] == "model/list"
      assert model_list["params"] == %{"includeHidden" => false}
      assert new_state.phase == :ready
      assert new_state.pending_requests == %{2 => %{type: :model_list, acp_id: nil, meta: %{}}}
    end

    test "model/list response stores normalized catalog", %{state: state} do
      state = %{state | pending_requests: %{2 => %{type: :model_list, acp_id: nil, meta: %{}}}}

      line =
        Jason.encode!(%{
          "id" => 2,
          "result" => %{
            "data" => [
              %{
                "id" => "codex-mini",
                "model" => "gpt-5-codex",
                "displayName" => "Codex Mini",
                "description" => "Fast coding model",
                "hidden" => false,
                "defaultReasoningEffort" => "medium",
                "supportedReasoningEfforts" => [
                  %{"reasoningEffort" => "medium", "description" => "Balanced"},
                  %{"reasoningEffort" => "xhigh", "description" => "Deep"}
                ]
              }
            ],
            "nextCursor" => nil
          }
        })

      assert {:skip, new_state} = Codex.translate_inbound(line, state)
      assert [model] = new_state.models
      assert model["id"] == "codex-mini"
      assert [%{"value" => "medium"}, %{"value" => "xhigh"}] = model["supportedReasoningEfforts"]
    end

    test "thread/start response produces ACP session result and stores session", %{state: state} do
      state = %{
        state
        | pending_requests: %{1 => %{type: :thread_start, acp_id: 20, meta: %{mode_id: "agent"}}}
      }

      line =
        Jason.encode!(%{
          "id" => 1,
          "result" => %{
            "model" => "gpt-5",
            "thread" => %{
              "id" => "thread-abc",
              "cwd" => "/tmp/project",
              "updatedAt" => 1_700_000_000
            }
          }
        })

      assert {:messages, [msg], new_state} = Codex.translate_inbound(line, state)

      assert msg["id"] == 20
      assert msg["result"]["sessionId"] == "thread-abc"
      refute Map.has_key?(msg["result"], "metadata")
      assert get_in(msg, ["result", "_meta", "ex_mcp", "codex", "thread", "id"]) == "thread-abc"
      assert msg["result"]["modes"]["currentModeId"] == "agent"
      assert msg["result"]["models"]["currentModelId"] == "gpt-5"
      assert Enum.any?(msg["result"]["configOptions"], &(&1["id"] == "model"))
      assert new_state.sessions["thread-abc"].model == "gpt-5"
    end

    test "thread/resume response replays embedded turns before load response", %{state: state} do
      state = %{
        state
        | pending_requests: %{1 => %{type: :thread_resume, acp_id: 21, meta: %{mode_id: "agent"}}}
      }

      line =
        Jason.encode!(%{
          "id" => 1,
          "result" => %{
            "thread" => %{
              "id" => "thread-abc",
              "turns" => [
                %{"items" => [%{"type" => "agent_message", "text" => "previous answer"}]}
              ]
            }
          }
        })

      assert {:messages, [replay, response], _state} = Codex.translate_inbound(line, state)

      assert replay["method"] == "session/update"
      assert replay["params"]["update"]["content"]["text"] == "previous answer"
      assert response["id"] == 21
      assert response["result"]["sessionId"] == "thread-abc"
    end

    test "session/list response maps Codex threads to ACP session info", %{state: state} do
      state = %{state | pending_requests: %{1 => %{type: :session_list, acp_id: 22, meta: %{}}}}

      line =
        Jason.encode!(%{
          "id" => 1,
          "result" => %{
            "nextCursor" => "next",
            "data" => [
              %{
                "id" => "thread-1",
                "cwd" => "/tmp/project",
                "name" => "Fix tests",
                "updatedAt" => 1_700_000_000
              }
            ]
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)

      assert msg["id"] == 22
      assert msg["result"]["nextCursor"] == "next"
      assert [session] = msg["result"]["sessions"]
      assert session["sessionId"] == "thread-1"
      assert session["cwd"] == "/tmp/project"
      assert session["title"] == "Fix tests"
      assert session["updatedAt"] == "2023-11-14T22:13:20Z"
    end

    test "authenticate response includes login metadata when app-server returns a URL", %{
      state: state
    } do
      state = %{state | pending_requests: %{1 => %{type: :authenticate, acp_id: 23, meta: %{}}}}

      line =
        Jason.encode!(%{
          "id" => 1,
          "result" => %{
            "type" => "chatgpt",
            "authUrl" => "https://example.com",
            "loginId" => "login-1"
          }
        })

      assert {:messages, [msg], _state} = Codex.translate_inbound(line, state)

      assert msg["id"] == 23

      assert get_in(msg, ["result", "_meta", "ex_mcp", "codex", "auth", "authUrl"]) ==
               "https://example.com"
    end
  end

  describe "inbound notifications" do
    setup %{state: state} do
      %{
        state: put_test_session(state, "thread-1", %{turn_id: "turn-1", active_prompt_acp_id: 30})
      }
    end

    test "routes text deltas to the session from params", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/agentMessage/delta",
          "params" => %{"delta" => "Hello ", "threadId" => "thread-1"}
        })

      assert {:messages, [msg], new_state} = Codex.translate_inbound(line, state)

      assert msg["params"]["sessionId"] == "thread-1"
      assert msg["params"]["update"]["sessionUpdate"] == "agent_message_chunk"
      assert new_state.sessions["thread-1"].accumulated_text == ["Hello "]
    end

    test "turn/completed responds to the active prompt for that session", %{state: state} do
      state =
        put_test_session(state, "thread-2", %{
          turn_id: "turn-2",
          active_prompt_acp_id: 31,
          accumulated_text: ["other"]
        })

      state =
        put_test_session(state, "thread-1", %{
          turn_id: "turn-1",
          active_prompt_acp_id: 30,
          accumulated_text: ["world", "Hello "]
        })

      line =
        Jason.encode!(%{
          "method" => "turn/completed",
          "params" => %{
            "threadId" => "thread-1",
            "turn" => %{"id" => "turn-1", "status" => "completed"}
          }
        })

      assert {:messages, messages, new_state} = Codex.translate_inbound(line, state)
      response = Enum.find(messages, &Map.has_key?(&1, "id"))

      assert response["id"] == 30
      assert response["result"]["stopReason"] == "end_turn"
      assert response["result"]["_meta"]["ex_mcp"]["text"] == "Hello world"
      assert new_state.sessions["thread-1"].active_prompt_acp_id == nil
      assert new_state.sessions["thread-2"].active_prompt_acp_id == 31
    end

    test "tool and error notifications keep stable ACP update shapes", %{state: state} do
      started =
        Jason.encode!(%{
          "method" => "item/commandExecution/started",
          "params" => %{"threadId" => "thread-1", "itemId" => "item-1", "command" => "mix test"}
        })

      completed =
        Jason.encode!(%{
          "method" => "item/commandExecution/completed",
          "params" => %{
            "threadId" => "thread-1",
            "itemId" => "item-1",
            "exitCode" => 0,
            "output" => "ok"
          }
        })

      assert {:messages, [start_msg], state} = Codex.translate_inbound(started, state)
      assert start_msg["params"]["update"]["sessionUpdate"] == "tool_call"
      assert start_msg["params"]["update"]["kind"] == "execute"

      assert {:messages, [done_msg], _state} = Codex.translate_inbound(completed, state)
      assert done_msg["params"]["update"]["sessionUpdate"] == "tool_call_update"

      assert done_msg["params"]["update"]["rawOutput"] == %{
               "exit_code" => 0,
               "formatted_output" => "ok"
             }
    end

    test "current Codex item events map commandExecution and fileChange variants", %{state: state} do
      started =
        Jason.encode!(%{
          "method" => "item/started",
          "params" => %{
            "threadId" => "thread-1",
            "item" => %{
              "type" => "commandExecution",
              "id" => "cmd-1",
              "command" => "mix test",
              "status" => "running"
            }
          }
        })

      completed =
        Jason.encode!(%{
          "method" => "item/completed",
          "params" => %{
            "threadId" => "thread-1",
            "item" => %{
              "type" => "fileChange",
              "id" => "edit-1",
              "status" => "completed",
              "changes" => [%{"path" => "lib/a.ex", "diff" => "@@ diff"}]
            }
          }
        })

      assert {:messages, [start_msg], state} = Codex.translate_inbound(started, state)
      assert start_msg["params"]["update"]["sessionUpdate"] == "tool_call"
      assert start_msg["params"]["update"]["toolCallId"] == "cmd-1"
      assert start_msg["params"]["update"]["status"] == "in_progress"

      assert {:messages, [done_msg], _state} = Codex.translate_inbound(completed, state)
      assert done_msg["params"]["update"]["sessionUpdate"] == "tool_call_update"
      assert done_msg["params"]["update"]["toolCallId"] == "edit-1"
      assert [diff] = done_msg["params"]["update"]["content"]
      assert diff["type"] == "diff"
    end

    test "token usage is accumulated for the prompt response and emitted as usage_update",
         %{
           state: state
         } do
      line =
        Jason.encode!(%{
          "method" => "thread/tokenUsage/updated",
          "params" => %{
            "threadId" => "thread-1",
            "tokenUsage" => %{
              "last" => %{"inputTokens" => 4, "outputTokens" => 1},
              "modelContextWindow" => 100,
              "total" => %{"inputTokens" => 10, "outputTokens" => 5, "cachedInputTokens" => 2}
            }
          }
        })

      assert {:messages, [usage_update], new_state} = Codex.translate_inbound(line, state)

      assert usage_update["params"]["update"]["sessionUpdate"] == "usage_update"
      assert usage_update["params"]["update"]["used"] == 5
      assert usage_update["params"]["update"]["size"] == 100

      assert new_state.sessions["thread-1"].accumulated_usage == %{
               "inputTokens" => 10,
               "outputTokens" => 5,
               "cachedInputTokens" => 2
             }
    end
  end

  describe "app-server permission requests" do
    test "approval request is converted to ACP request and client response returns Codex decision",
         %{
           state: state
         } do
      state = put_test_session(state, "thread-1")

      line =
        Jason.encode!(%{
          "id" => 99,
          "method" => "item/commandExecution/requestApproval",
          "params" => %{
            "threadId" => "thread-1",
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "command" => "mix test",
            "startedAtMs" => 1
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)

      assert request["method"] == "session/request_permission"
      assert request["params"]["sessionId"] == "thread-1"
      assert request["params"]["toolCall"]["toolName"] == "execute"
      assert request["params"]["toolCall"]["title"] == "mix test"

      response = %{
        "id" => request["id"],
        "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "allow_once"}}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(response, state)
      codex_response = decode(data)

      assert codex_response == %{"id" => 99, "result" => %{"decision" => "accept"}}
      assert new_state.pending_client_requests == %{}
    end
  end

  defp put_test_session(state, session_id, attrs \\ %{}) do
    session =
      %{
        id: session_id,
        cwd: "/tmp/project",
        model: nil,
        model_id: nil,
        mode_id: "agent",
        reasoning_effort: "medium",
        accumulated_text: [],
        accumulated_thinking: [],
        accumulated_usage: nil,
        turn_id: nil,
        active_prompt_acp_id: nil
      }
      |> Map.merge(attrs)

    %{state | sessions: Map.put(state.sessions, session_id, session)}
  end

  defp put_catalog_models(state) do
    %{state | models: catalog_models()}
  end

  defp catalog_models do
    [
      %{
        "id" => "codex-mini",
        "model" => "gpt-5-codex",
        "displayName" => "Codex Mini",
        "description" => "Fast coding model",
        "hidden" => false,
        "defaultReasoningEffort" => "medium",
        "supportedReasoningEfforts" => [
          %{"value" => "medium", "name" => "Medium", "description" => "Balanced"},
          %{"value" => "high", "name" => "High", "description" => "Deep"}
        ]
      }
    ]
  end

  defp decode(data) do
    data
    |> IO.iodata_to_binary()
    |> String.trim()
    |> Jason.decode!()
  end

  defp decode_lines(data) do
    data
    |> IO.iodata_to_binary()
    |> String.split("\n", trim: true)
    |> Enum.map(&Jason.decode!/1)
  end
end
