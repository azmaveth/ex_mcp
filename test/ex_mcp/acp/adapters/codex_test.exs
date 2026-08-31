defmodule ExMCP.ACP.Adapters.CodexTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex

  setup do
    {:ok, state} =
      Codex.init(
        workspace_roots: ["/tmp"],
        authorize_mcp_server: fn _server, _context -> true end,
        trust_authorized_workspaces: true
      )

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
      modes = Codex.modes()
      ids = Enum.map(modes, & &1["id"])
      assert ids == ["read-only", "agent", "agent-full-access"]

      assert Enum.map(modes, &get_in(&1, ["_meta", "kind"])) == [
               "standard",
               "auto_review",
               "full_access"
             ]

      assert Enum.map(modes, & &1["name"]) == [
               "Ask for approval",
               "Approve for me",
               "Full access"
             ]
    end

    test "advertises Codex auth methods" do
      ids = Codex.auth_methods([]) |> Enum.map(& &1["id"])
      assert "chat-gpt" in ids
      assert "api-key" in ids
      refute "chat-gpt-device-code" in ids

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
    test "secure defaults reject workspaces outside the configured roots" do
      {:ok, state} = Codex.init(cwd: File.cwd!())

      msg = %{
        "method" => "session/new",
        "id" => 1,
        "params" => %{"cwd" => "/tmp/outside", "mcpServers" => []}
      }

      assert {:error, "Workspace path is not authorized", ^state} =
               Codex.translate_outbound(msg, state)
    end

    test "secure defaults reject session-supplied MCP servers" do
      cwd = File.cwd!()
      {:ok, state} = Codex.init(cwd: cwd)

      msg = %{
        "method" => "session/new",
        "id" => 1,
        "params" => %{
          "cwd" => cwd,
          "mcpServers" => [
            %{
              "type" => "http",
              "name" => "remote",
              "url" => "https://example.test/mcp",
              "headers" => []
            }
          ]
        }
      }

      assert {:error, "MCP server is not authorized", ^state} =
               Codex.translate_outbound(msg, state)
    end

    test "a trusted server name cannot authorize caller-controlled connection details" do
      cwd = File.cwd!()
      {:ok, state} = Codex.init(cwd: cwd, trusted_mcp_servers: ["trusted-name"])

      spoofed_servers = [
        %{
          "type" => "http",
          "name" => "trusted-name",
          "url" => "https://attacker.invalid/mcp",
          "headers" => []
        },
        %{
          "name" => "trusted-name",
          "command" => "/tmp/attacker",
          "args" => [],
          "env" => []
        }
      ]

      for server <- spoofed_servers do
        msg = %{
          "method" => "session/new",
          "id" => 1,
          "params" => %{"cwd" => cwd, "mcpServers" => [server]}
        }

        assert {:error, "MCP server is not authorized", ^state} =
                 Codex.translate_outbound(msg, state)
      end
    end

    test "authorized workspaces are not marked trusted without an explicit opt-in" do
      cwd = File.cwd!()
      {:ok, state} = Codex.init(workspace_roots: [cwd])

      msg = %{
        "method" => "session/new",
        "id" => 1,
        "params" => %{"cwd" => cwd, "mcpServers" => []}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(msg, state)
      refute get_in(decode(data), ["params", "config", "projects"])
    end

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
              "command" => "/usr/bin/tools",
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
               "/usr/bin/tools"

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

      assert {:messages_and_write, [], data, new_state} = Codex.translate_outbound(msg, state)
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

    test "session/prompt turn/start includes approvalsReviewer", %{state: state} do
      state = put_test_session(state, "thread-1", %{mode_id: "read-only"})

      msg = %{
        "method" => "session/prompt",
        "id" => 71,
        "params" => %{
          "sessionId" => "thread-1",
          "prompt" => [%{"type" => "text", "text" => "hi"}]
        }
      }

      assert {:ok, data, _state} = Codex.translate_outbound(msg, state)
      codex_msg = decode(data)

      assert codex_msg["method"] == "turn/start"
      assert codex_msg["params"]["approvalsReviewer"] == "user"
      assert codex_msg["params"]["approvalPolicy"] == "on-request"
      assert codex_msg["params"]["sandboxPolicy"]["type"] == "workspaceWrite"

      state = put_test_session(state, "thread-2", %{mode_id: "agent"})

      msg = %{
        "method" => "session/prompt",
        "id" => 72,
        "params" => %{
          "sessionId" => "thread-2",
          "prompt" => [%{"type" => "text", "text" => "hi"}]
        }
      }

      assert {:ok, data, _state} = Codex.translate_outbound(msg, state)
      assert decode(data)["params"]["approvalsReviewer"] == "auto_review"
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

      assert Enum.map(msg["result"]["modes"]["availableModes"], &get_in(&1, ["_meta", "kind"])) ==
               ["standard", "auto_review", "full_access"]

      mode_option = Enum.find(msg["result"]["configOptions"], &(&1["id"] == "mode"))

      assert Enum.map(mode_option["options"], &get_in(&1, ["_meta", "kind"])) == [
               "standard",
               "auto_review",
               "full_access"
             ]

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
      assert new_state.sessions["thread-1"].prompt_activity
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

    test "warning notifications remain metadata and cannot become an agent response", %{
      state: state
    } do
      line =
        Jason.encode!(%{
          "method" => "warning",
          "params" => %{
            "threadId" => "thread-1",
            "message" => "Skill descriptions were shortened"
          }
        })

      assert {:messages, [msg], new_state} = Codex.translate_inbound(line, state)
      update = msg["params"]["update"]

      assert update["sessionUpdate"] == "session_info_update"

      assert update["_meta"]["ex_mcp"]["warning"]["message"] ==
               "Skill descriptions were shortened"

      assert new_state.sessions["thread-1"].accumulated_text == []
    end

    test "an exhausted rate limit with no model activity fails the active prompt", %{state: state} do
      rate_limits = %{
        "limitId" => "codex_spark",
        "primary" => %{"usedPercent" => 100, "resetsAt" => 1_800_000_000},
        "credits" => %{"hasCredits" => false, "unlimited" => false}
      }

      update =
        Jason.encode!(%{
          "method" => "account/rateLimits/updated",
          "params" => %{"rateLimits" => rate_limits}
        })

      assert {:messages, [info], state} = Codex.translate_inbound(update, state)
      assert info["params"]["update"]["sessionUpdate"] == "session_info_update"
      assert state.sessions["thread-1"].rate_limits == rate_limits

      completed =
        Jason.encode!(%{
          "method" => "turn/completed",
          "params" => %{
            "threadId" => "thread-1",
            "turn" => %{"id" => "turn-1", "status" => "completed"}
          }
        })

      assert {:messages, messages, new_state} = Codex.translate_inbound(completed, state)
      response = Enum.find(messages, &Map.has_key?(&1, "id"))

      assert response["id"] == 30
      assert response["error"]["code"] == -32_029
      assert response["error"]["data"]["kind"] == "rate_limit_exhausted"
      assert new_state.sessions["thread-1"].active_prompt_acp_id == nil
    end

    test "rate-limit metadata does not replace a response that made model progress", %{
      state: state
    } do
      state =
        put_test_session(state, "thread-1", %{
          turn_id: "turn-1",
          active_prompt_acp_id: 30,
          accumulated_text: ["done"],
          prompt_activity: true,
          rate_limits: %{
            "primary" => %{"usedPercent" => 100},
            "credits" => %{"hasCredits" => false, "unlimited" => false}
          }
        })

      completed =
        Jason.encode!(%{
          "method" => "turn/completed",
          "params" => %{
            "threadId" => "thread-1",
            "turn" => %{"id" => "turn-1", "status" => "completed"}
          }
        })

      assert {:messages, messages, _state} = Codex.translate_inbound(completed, state)
      response = Enum.find(messages, &Map.has_key?(&1, "id"))

      assert response["result"]["stopReason"] == "end_turn"
      refute Map.has_key?(response, "error")
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

    test "current fileChange patchUpdated reaches the snapshot mapper", %{state: state} do
      line =
        Jason.encode!(%{
          "method" => "item/fileChange/patchUpdated",
          "params" => %{
            "threadId" => "thread-1",
            "itemId" => "edit-1",
            "changes" => [
              %{"path" => "lib/a.ex", "newText" => "first"},
              %{"path" => "lib/b.ex", "diff" => "second"}
            ]
          }
        })

      assert {:messages, [message], ^state} = Codex.translate_inbound(line, state)

      update = get_in(message, ["params", "update"])
      assert update["sessionUpdate"] == "tool_call_update"
      assert update["toolCallId"] == "edit-1"

      assert Enum.map(update["content"], &{&1["path"], &1["newText"]}) ==
               [{"lib/a.ex", "first"}, {"lib/b.ex", "second"}]
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
      assert request["params"]["_meta"]["permission"]["title"] == "Run command?"

      assert Enum.map(request["params"]["options"], & &1["optionId"]) == [
               "allow_once",
               "allow_for_session",
               "decline",
               "cancel"
             ]

      response = %{
        "id" => request["id"],
        "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "allow_once"}}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(response, state)
      codex_response = decode(data)

      assert codex_response == %{"id" => 99, "result" => %{"decision" => "accept"}}
      assert new_state.pending_client_requests == %{}
    end

    test "incomplete availableDecisions cancels without prompting", %{state: state} do
      state = put_test_session(state, "thread-1")

      line =
        Jason.encode!(%{
          "id" => 102,
          "method" => "item/commandExecution/requestApproval",
          "params" => %{
            "threadId" => "thread-1",
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "command" => "python3 fill_contract.py",
            "startedAtMs" => 1,
            "availableDecisions" => ["accept"]
          }
        })

      assert {:skip_and_write, data, new_state} = Codex.translate_inbound(line, state)
      assert decode(data) == %{"id" => 102, "result" => %{"decision" => "cancel"}}
      assert new_state.pending_client_requests == %{}
    end

    test "structured available decisions round-trip through string ACP option ids", %{
      state: state
    } do
      state = put_test_session(state, "thread-1")

      structured_decision = %{
        "acceptWithExecpolicyAmendment" => %{
          "execpolicy_amendment" => ["touch", "/tmp/contract.hwp"]
        }
      }

      line =
        Jason.encode!(%{
          "id" => 100,
          "method" => "item/commandExecution/requestApproval",
          "params" => %{
            "threadId" => "thread-1",
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "command" => "python3 fill_contract.py",
            "startedAtMs" => 1,
            "proposedExecpolicyAmendment" => ["touch", "/tmp/contract.hwp"],
            "availableDecisions" => ["accept", structured_decision, "decline"]
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)

      assert [accept, structured, decline] = request["params"]["options"]

      assert accept == %{
               "optionId" => "allow_once",
               "name" => "Yes, proceed",
               "kind" => "allow_once"
             }

      assert decline == %{
               "optionId" => "decline",
               "name" => "No, continue without running it",
               "kind" => "reject_once"
             }

      assert structured["optionId"] == "accept_execpolicy_amendment"
      assert structured["kind"] == "allow_always"
      assert structured["name"] =~ "touch /tmp/contract.hwp"

      response = %{
        "id" => request["id"],
        "result" => %{
          "outcome" => %{"outcome" => "selected", "optionId" => structured["optionId"]}
        }
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(response, state)

      assert decode(data) == %{"id" => 100, "result" => %{"decision" => structured_decision}}
      assert new_state.pending_client_requests == %{}
    end

    test "file change approvals offer allow once, session, and cancel", %{state: state} do
      state = put_test_session(state, "thread-1")

      line =
        Jason.encode!(%{
          "id" => 103,
          "method" => "item/fileChange/requestApproval",
          "params" => %{
            "threadId" => "thread-1",
            "turnId" => "turn-1",
            "itemId" => "item-1"
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)

      assert Enum.map(request["params"]["options"], & &1["optionId"]) == [
               "allow_once",
               "allow_for_session",
               "cancel"
             ]

      refute Enum.any?(request["params"]["options"], &(&1["optionId"] == "decline"))
      assert request["params"]["_meta"]["permission"]["title"] == "Make edits?"

      response = %{
        "id" => request["id"],
        "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "cancel"}}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(response, state)

      assert decode(data) == %{"id" => 103, "result" => %{"decision" => "cancel"}}
      assert new_state.pending_client_requests == %{}
    end

    test "permission grants copy the requested network and filesystem profile", %{state: state} do
      state = put_test_session(state, "thread-1")

      permissions = %{
        "network" => %{"enabled" => true},
        "fileSystem" => %{"write" => ["/tmp/project"]}
      }

      line =
        Jason.encode!(%{
          "id" => 104,
          "method" => "item/permissions/requestApproval",
          "params" => %{
            "threadId" => "thread-1",
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "permissions" => permissions
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)

      assert Enum.map(request["params"]["options"], & &1["optionId"]) == [
               "allow_permissions_turn",
               "allow_permissions_turn_strict_auto_review",
               "allow_permissions_session",
               "reject_permissions"
             ]

      response = %{
        "id" => request["id"],
        "result" => %{
          "outcome" => %{
            "outcome" => "selected",
            "optionId" => "allow_permissions_turn_strict_auto_review"
          }
        }
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(response, state)

      assert decode(data) == %{
               "id" => 104,
               "result" => %{
                 "permissions" => permissions,
                 "scope" => "turn",
                 "strictAutoReview" => true
               }
             }

      assert new_state.pending_client_requests == %{}
    end

    test "permission cancel and reject include strictAutoReview false", %{state: state} do
      state = put_test_session(state, "thread-1")

      line =
        Jason.encode!(%{
          "id" => 105,
          "method" => "item/permissions/requestApproval",
          "params" => %{
            "threadId" => "thread-1",
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "permissions" => %{"network" => %{"enabled" => true}}
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)

      reject = %{
        "id" => request["id"],
        "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "reject_permissions"}}
      }

      assert {:ok, data, state} = Codex.translate_outbound(reject, state)

      assert decode(data) == %{
               "id" => 105,
               "result" => %{
                 "permissions" => %{},
                 "scope" => "turn",
                 "strictAutoReview" => false
               }
             }

      line =
        Jason.encode!(%{
          "id" => 106,
          "method" => "item/permissions/requestApproval",
          "params" => %{
            "threadId" => "thread-1",
            "turnId" => "turn-1",
            "itemId" => "item-2",
            "permissions" => %{"network" => %{"enabled" => true}}
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)

      cancel = %{
        "id" => request["id"],
        "result" => %{"outcome" => %{"outcome" => "cancelled"}}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(cancel, state)

      assert decode(data) == %{
               "id" => 106,
               "result" => %{
                 "permissions" => %{},
                 "scope" => "turn",
                 "strictAutoReview" => false
               }
             }
    end

    test "MCP persist options appear when _meta.persist is present", %{state: state} do
      line =
        Jason.encode!(%{
          "id" => 107,
          "method" => "mcpServer/elicitation/request",
          "params" => %{
            "threadId" => "thread-1",
            "mode" => "openai/form",
            "message" => "Allow tool call?",
            "serverName" => "tool-server",
            "_meta" => %{
              "codex_approval_kind" => "mcp_tool_call",
              "persist" => ["session", "always"]
            }
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)
      assert request["method"] == "session/request_permission"

      assert Enum.map(request["params"]["options"], & &1["optionId"]) == [
               "allow_once",
               "allow_session",
               "allow_always",
               "cancel"
             ]

      refute Enum.any?(request["params"]["options"], &(&1["optionId"] == "decline"))

      response = %{
        "id" => request["id"],
        "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "allow_session"}}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(response, state)

      assert decode(data) == %{
               "id" => 107,
               "result" => %{"action" => "accept", "_meta" => %{"persist" => "session"}}
             }
    end

    test "deny network policy amendments round-trip as reject-always ACP options", %{
      state: state
    } do
      state = put_test_session(state, "thread-1")

      structured_decision = %{
        "applyNetworkPolicyAmendment" => %{
          "network_policy_amendment" => %{"action" => "deny", "host" => "example.test"}
        }
      }

      line =
        Jason.encode!(%{
          "id" => 101,
          "method" => "item/commandExecution/requestApproval",
          "params" => %{
            "threadId" => "thread-1",
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "command" => "curl https://example.test",
            "startedAtMs" => 1,
            "networkApprovalContext" => %{"host" => "example.test", "protocol" => "https"},
            "proposedNetworkPolicyAmendments" => [
              %{"action" => "deny", "host" => "example.test"}
            ],
            "availableDecisions" => ["accept", structured_decision, "cancel"]
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)
      structured = Enum.find(request["params"]["options"], &(&1["kind"] == "reject_always"))
      assert structured["optionId"] == "apply_network_policy_amendment:0"
      assert structured["name"] == "No, and block this host in the future"

      response = %{
        "id" => request["id"],
        "result" => %{
          "outcome" => %{"outcome" => "selected", "optionId" => structured["optionId"]}
        }
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(response, state)

      assert decode(data) == %{"id" => 101, "result" => %{"decision" => structured_decision}}
      assert new_state.pending_client_requests == %{}
    end
  end

  describe "elicitation and close parity" do
    test "requestUserInput round-trips form answers and refuses secret fields", %{state: state} do
      state = %{
        state
        | client_capabilities: %{"elicitation" => %{"form" => %{}}}
      }

      line =
        Jason.encode!(%{
          "id" => 201,
          "method" => "item/tool/requestUserInput",
          "params" => %{
            "threadId" => "thread-1",
            "itemId" => "question-tool",
            "questions" => [
              %{
                "id" => "color",
                "header" => "Color",
                "question" => "Which color?",
                "options" => [%{"label" => "Blue", "description" => "Cool"}]
              }
            ]
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)
      assert request["method"] == "elicitation/create"
      assert request["params"]["mode"] == "form"
      assert request["params"]["message"] == "Which color?"

      response = %{
        "id" => request["id"],
        "result" => %{"action" => "accept", "content" => %{"color" => "Blue"}}
      }

      assert {:ok, data, _state} = Codex.translate_outbound(response, state)

      assert decode(data) == %{
               "id" => 201,
               "result" => %{"answers" => %{"color" => %{"answers" => ["Blue"]}}}
             }

      secret_line =
        Jason.encode!(%{
          "id" => 202,
          "method" => "item/tool/requestUserInput",
          "params" => %{
            "threadId" => "thread-1",
            "questions" => [
              %{"id" => "token", "question" => "API token", "isSecret" => true}
            ]
          }
        })

      assert {:skip_and_write, secret_data, _state} =
               Codex.translate_inbound(secret_line, state)

      assert decode(secret_data) == %{"id" => 202, "result" => %{"answers" => %{}}}
    end

    test "requestUserInput gives Other fields collision-safe ids", %{state: state} do
      state = %{state | client_capabilities: %{"elicitation" => %{"form" => %{}}}}

      line =
        Jason.encode!(%{
          "id" => 209,
          "method" => "item/tool/requestUserInput",
          "params" => %{
            "threadId" => "thread-1",
            "itemId" => "question-tool",
            "questions" => [
              %{
                "id" => "color",
                "question" => "Which color?",
                "isOther" => true,
                "options" => [%{"label" => "Blue"}]
              },
              %{"id" => "color__other", "question" => "Real second question"}
            ]
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)
      properties = request["params"]["requestedSchema"]["properties"]
      assert properties["color__other"]["description"] == "Real second question"
      assert properties["color__other1"]["_meta"]["codex"]["isOtherAnswer"] == true

      response = %{
        "id" => request["id"],
        "result" => %{
          "action" => "accept",
          "content" => %{"color__other1" => "Green", "color__other" => "second"}
        }
      }

      assert {:ok, data, _state} = Codex.translate_outbound(response, state)

      assert decode(data)["result"]["answers"] == %{
               "color" => %{"answers" => ["Green"]},
               "color__other" => %{"answers" => ["second"]}
             }
    end

    test "form MCP elicitation normalizes legacy enumNames recursively", %{state: state} do
      state = %{state | client_capabilities: %{"elicitation" => %{"form" => %{}}}}

      line =
        Jason.encode!(%{
          "id" => 210,
          "method" => "mcpServer/elicitation/request",
          "params" => %{
            "threadId" => "thread-1",
            "mode" => "form",
            "message" => "Pick a color",
            "requestedSchema" => %{
              "type" => "object",
              "properties" => %{
                "color" => %{
                  "type" => "string",
                  "enum" => ["red", "blue"],
                  "enumNames" => ["Red", "Blue"]
                }
              }
            }
          }
        })

      assert {:messages, [request], _state} = Codex.translate_inbound(line, state)

      assert request["params"]["requestedSchema"]["properties"]["color"] == %{
               "type" => "string",
               "oneOf" => [
                 %{"const" => "red", "title" => "Red"},
                 %{"const" => "blue", "title" => "Blue"}
               ]
             }
    end

    test "openai/form MCP elicitation stays on permission fallback", %{state: state} do
      state = %{state | client_capabilities: %{"elicitation" => %{"form" => %{}}}}

      line =
        Jason.encode!(%{
          "id" => 211,
          "method" => "mcpServer/elicitation/request",
          "params" => %{
            "threadId" => "thread-1",
            "mode" => "openai/form",
            "message" => "Unsupported arbitrary form",
            "requestedSchema" => %{}
          }
        })

      assert {:messages, [request], _state} = Codex.translate_inbound(line, state)
      assert request["method"] == "session/request_permission"
    end

    test "URL MCP elicitation completes the accepted client UI", %{state: state} do
      state = %{
        state
        | client_capabilities: %{"elicitation" => %{"url" => %{}}}
      }

      line =
        Jason.encode!(%{
          "id" => 203,
          "method" => "mcpServer/elicitation/request",
          "params" => %{
            "threadId" => "thread-1",
            "mode" => "url",
            "elicitationId" => "oauth-1",
            "url" => "https://example.com/authorize",
            "message" => "Authorize the MCP server"
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(line, state)
      assert request["method"] == "elicitation/create"

      assert {:ok, data, state} =
               Codex.translate_outbound(
                 %{
                   "id" => request["id"],
                   "result" => %{"action" => "accept", "content" => %{}}
                 },
                 state
               )

      assert decode(data) == %{
               "id" => 203,
               "result" => %{"action" => "accept", "content" => %{}}
             }

      completed =
        Jason.encode!(%{
          "method" => "serverRequest/resolved",
          "params" => %{"threadId" => "thread-1", "requestId" => 203}
        })

      assert {:messages, [notification], _state} = Codex.translate_inbound(completed, state)
      assert notification["method"] == "elicitation/complete"
      assert notification["params"]["elicitationId"] == "oauth-1"
    end

    test "concurrent URL MCP elicitations complete only their matching request", %{state: state} do
      state = %{state | client_capabilities: %{"elicitation" => %{"url" => %{}}}}

      {requests, state} =
        Enum.map_reduce([{212, "oauth-1"}, {213, "oauth-2"}], state, fn {id, elicitation_id},
                                                                        state ->
          line =
            Jason.encode!(%{
              "id" => id,
              "method" => "mcpServer/elicitation/request",
              "params" => %{
                "threadId" => "thread-1",
                "mode" => "url",
                "elicitationId" => elicitation_id,
                "url" => "https://example.com/#{elicitation_id}",
                "message" => "Authorize"
              }
            })

          assert {:messages, [request], state} = Codex.translate_inbound(line, state)

          assert {:ok, _data, state} =
                   Codex.translate_outbound(
                     %{"id" => request["id"], "result" => %{"action" => "accept"}},
                     state
                   )

          {request, state}
        end)

      assert map_size(state.url_elicitations) == 2

      completed =
        Jason.encode!(%{
          "method" => "serverRequest/resolved",
          "params" => %{"threadId" => "thread-1", "requestId" => 212}
        })

      assert {:messages, [notification], state} = Codex.translate_inbound(completed, state)
      assert notification["params"]["elicitationId"] == "oauth-1"
      assert map_size(state.url_elicitations) == 1
      assert Enum.all?(requests, &(&1["method"] == "elicitation/create"))
    end

    test "ChatGPT device login uses request-scoped URL elicitation", %{state: state} do
      state = %{
        state
        | client_capabilities: %{"elicitation" => %{"url" => %{}}}
      }

      assert Enum.any?(Codex.auth_methods([], state), &(&1["id"] == "chat-gpt-device-code"))
      assert Enum.any?(Codex.auth_methods([], %Codex{}), &(&1["id"] == "chat-gpt"))

      assert {:ok, login_data, state} =
               Codex.translate_outbound(
                 %{
                   "id" => 204,
                   "method" => "authenticate",
                   "params" => %{"methodId" => "chat-gpt-device-code"}
                 },
                 state
               )

      login_request = decode(login_data)

      app_response =
        Jason.encode!(%{
          "id" => login_request["id"],
          "result" => %{
            "type" => "chatgptDeviceCode",
            "loginId" => "login-1",
            "verificationUrl" => "https://chatgpt.com/device",
            "userCode" => "ABCD-EFGH"
          }
        })

      assert {:messages, [request], state} = Codex.translate_inbound(app_response, state)
      assert request["method"] == "elicitation/create"
      assert request["params"]["requestId"] == 204

      assert {:ok, :skip, state} =
               Codex.translate_outbound(
                 %{"id" => request["id"], "result" => %{"action" => "accept"}},
                 state
               )

      completed =
        Jason.encode!(%{
          "method" => "account/login/completed",
          "params" => %{"loginId" => "login-1", "success" => true}
        })

      assert {:messages, [notification, response], state} =
               Codex.translate_inbound(completed, state)

      assert notification["method"] == "elicitation/complete"
      assert response == %{"jsonrpc" => "2.0", "id" => 204, "result" => %{}}
      assert state.pending_auth == nil
    end

    test "closing an active prompt cancels it and fences late events", %{state: state} do
      state =
        put_test_session(state, "thread-1", %{
          turn_id: "turn-1",
          active_prompt_acp_id: 205
        })

      request = %{
        "id" => 206,
        "method" => "session/close",
        "params" => %{"sessionId" => "thread-1"}
      }

      assert {:messages_and_write, [response], _data, state} =
               Codex.translate_outbound(request, state)

      assert response["id"] == 205
      assert response["result"]["stopReason"] == "cancelled"

      late =
        Jason.encode!(%{
          "method" => "turn/completed",
          "params" => %{
            "threadId" => "thread-1",
            "turn" => %{"id" => "turn-1", "status" => "completed"}
          }
        })

      assert {:skip, ^state} = Codex.translate_inbound(late, state)
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
