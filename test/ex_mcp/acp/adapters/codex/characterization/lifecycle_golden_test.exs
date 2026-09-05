defmodule ExMCP.ACP.Adapters.Codex.LifecycleGoldenTest do
  @moduledoc """
  Characterization gate for the Codex ACP adapter's initialize and session
  lifecycle wire behavior (see `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Codex
  adapter restructuring" / "Characterization gate").

  Each test drives `ExMCP.ACP.Adapters.Codex` through `ExMCP.Test.CodexGolden`
  and compares the recorded transcript (app-server writes, ACP messages,
  replies, and errors, with generated ids normalized) against a committed
  fixture under `test/fixtures/acp/codex/lifecycle/`. The fixtures pin the
  adapter's current behavior so that the planned internal modularization can
  only change them deliberately.

  Area A1 (lifecycle) pins: the app-server `initialize` handshake and its
  `initialized` + `model/list` continuation, the ACP `initialize` clause that
  stores client capabilities, `session/new`, `session/load`, `session/resume`,
  `session/list`, `session/fork`, `session/close`, `session/delete`,
  `session/cancel`, `session/set_mode`, `session/set_model`,
  `session/set_config_option`, every `authenticate` method id, `logout`, and
  the error-reply handling for every pending app-server request type. MCP
  server conversion, prompt content, and catalog normalization are covered by
  their own golden files and appear here only on one happy path each.

  Ownership of the shared `session/set_*` clauses: this file owns
  `session/set_mode` completely (happy path and every error branch), while
  the `session/set_model` and `session/set_config_option` error branches
  (unknown model, unsupported effort, unknown config id, ...) belong to the
  A7 catalog golden file; only their happy paths appear here.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CODEX_GOLDEN=update`:

      CODEX_GOLDEN=update mix test test/ex_mcp/acp/adapters/codex/characterization/lifecycle_golden_test.exs

  That run rewrites the fixtures and fails on purpose, so review the diff and
  re-run without the variable to confirm.
  """

  use ExUnit.Case, async: true

  import ExUnit.CaptureLog, only: [with_log: 1]

  alias ExMCP.Test.CodexGolden

  @area "lifecycle"

  # -- existing exemplar scenarios (fixtures are byte-frozen) -------------------

  test "initialize_handshake" do
    transcript = CodexGolden.assert_golden(@area, "initialize_handshake", handshake_steps())

    assert [
             %{"id" => 1, "method" => "initialize"},
             %{"method" => "initialized"},
             %{"id" => 2, "method" => "model/list"}
           ] =
             CodexGolden.writes(transcript)

    assert CodexGolden.messages(transcript) == []
    assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
  end

  test "session_new_thread_start" do
    steps =
      handshake_steps() ++
        [
          {:note, "ACP client opens a session inside the authorized workspace root"},
          {:outbound,
           %{
             "method" => "session/new",
             "id" => 10,
             "params" => %{"cwd" => "/tmp/project", "mcpServers" => []}
           }},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_000
               }
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_thread_start", steps)

    assert [_initialize, _initialized, _model_list, %{"id" => 3, "method" => "thread/start"}] =
             CodexGolden.writes(transcript)

    assert [%{"id" => 10, "result" => %{"sessionId" => "thread-abc"}}] =
             CodexGolden.messages(transcript)

    assert %{tag: :messages} = CodexGolden.last_result(transcript)
  end

  test "session_new_rejects_unknown_workspace" do
    steps =
      handshake_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/new",
             "id" => 11,
             "params" => %{"cwd" => "/srv/elsewhere", "mcpServers" => []}
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_rejects_unknown_workspace", steps)

    assert length(CodexGolden.writes(transcript)) == 3

    assert %{tag: :error, error: "Workspace path is not authorized"} =
             CodexGolden.last_result(transcript)
  end

  # -- app-server initialize handshake variants ---------------------------------

  test "initialize_error_reply_still_advances" do
    steps = [
      init_step(),
      :post_connect,
      {:note, "the app-server rejects initialize; the adapter ignores the reply body"},
      {:inbound,
       %{"id" => 1, "error" => %{"code" => -32_600, "message" => "unsupported client"}}},
      {:inbound, %{"id" => 2, "result" => %{"data" => catalog_models(), "nextCursor" => nil}}}
    ]

    transcript = CodexGolden.assert_golden(@area, "initialize_error_reply_still_advances", steps)

    assert [_initialize, %{"method" => "initialized"}, %{"id" => 2, "method" => "model/list"}] =
             CodexGolden.writes(transcript)

    assert CodexGolden.messages(transcript) == []
  end

  test "model_list_error_leaves_catalog_empty" do
    steps = [
      init_step(),
      :post_connect,
      {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}},
      {:inbound,
       %{"id" => 2, "error" => %{"code" => -32_603, "message" => "catalog unavailable"}}},
      {:note, "with no catalog the session result only lists the thread's own model"},
      session_new_step(10),
      thread_start_reply(3)
    ]

    transcript = CodexGolden.assert_golden(@area, "model_list_error_leaves_catalog_empty", steps)

    assert [%{"id" => 10, "result" => %{"models" => models}}] = CodexGolden.messages(transcript)
    assert %{"currentModelId" => "gpt-5", "availableModels" => [%{"modelId" => "gpt-5"}]} = models
  end

  test "unknown_response_id_is_skipped" do
    steps = connected_steps() ++ [{:inbound, %{"id" => 99, "result" => %{}}}]

    transcript = CodexGolden.assert_golden(@area, "unknown_response_id_is_skipped", steps)

    assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
  end

  test "post_connect_uses_client_info_options" do
    steps = [init_step(client_name: "acme-ide", client_version: "2.3.4"), :post_connect]

    transcript = CodexGolden.assert_golden(@area, "post_connect_uses_client_info_options", steps)

    assert [
             %{
               "id" => 1,
               "method" => "initialize",
               "params" => %{"clientInfo" => %{"name" => "acme-ide", "version" => "2.3.4"}}
             }
           ] = CodexGolden.writes(transcript)
  end

  # -- ACP initialize (client capabilities) --------------------------------------

  test "acp_initialize_stores_client_capabilities" do
    steps =
      connected_steps() ++
        [
          acp_initialize_step(url_elicitation_capabilities()),
          {:note, "URL elicitation support unlocks the chat-gpt-device-code auth method"},
          authenticate_step(30, "chat-gpt-device-code")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "acp_initialize_stores_client_capabilities", steps)

    assert %{tag: :ok, skipped: true} = Enum.at(transcript, 4).result

    assert [
             %{
               "id" => 3,
               "method" => "account/login/start",
               "params" => %{"type" => "chatgptDeviceCode"}
             }
           ] =
             Enum.drop(CodexGolden.writes(transcript), 3)
  end

  test "acp_initialize_without_capabilities_rejects_device_code" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{"method" => "initialize", "id" => 0, "params" => %{"protocolVersion" => 1}}},
          authenticate_step(30, "chat-gpt-device-code")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "acp_initialize_without_capabilities_rejects_device_code",
        steps
      )

    assert %{
             tag: :error,
             error: "ChatGPT device-code authentication requires ACP URL elicitation support"
           } =
             CodexGolden.last_result(transcript)

    assert length(CodexGolden.writes(transcript)) == 3
  end

  # -- session/new variants ------------------------------------------------------

  test "session_new_with_explicit_model" do
    steps =
      connected_steps() ++
        [
          session_new_step(10, %{"model" => "gpt-5-codex"}),
          thread_start_reply(3, %{"model" => "gpt-5-codex"})
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_with_explicit_model", steps)

    assert %{"method" => "thread/start", "params" => %{"model" => "gpt-5-codex"}} =
             List.last(CodexGolden.writes(transcript))

    assert [%{"result" => %{"models" => %{"currentModelId" => "codex-mini/medium"}}}] =
             CodexGolden.messages(transcript)
  end

  test "session_new_uses_init_model_option" do
    steps = connected_steps(model: "gpt-5") ++ [session_new_step(10)]

    transcript = CodexGolden.assert_golden(@area, "session_new_uses_init_model_option", steps)

    assert %{"method" => "thread/start", "params" => %{"model" => "gpt-5"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "session_new_caller_model_wins_over_init_model" do
    steps =
      connected_steps(model: "gpt-5") ++
        [
          {:note,
           "params.model beats the init :model option on thread/start and on both thread/resume shapes"},
          session_new_step(10, %{"model" => "gpt-5-codex"}),
          thread_start_reply(3, %{"model" => "gpt-5-codex"}),
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 11,
             "params" => %{
               "sessionId" => "thread-abc",
               "cwd" => "/tmp/project",
               "model" => "gpt-5-codex"
             }
           }},
          thread_start_reply(4, %{"model" => "gpt-5-codex"}),
          {:outbound,
           %{
             "method" => "session/resume",
             "id" => 12,
             "params" => %{
               "sessionId" => "thread-abc",
               "cwd" => "/tmp/project",
               "model" => "gpt-5-codex"
             }
           }},
          thread_start_reply(5, %{"model" => "gpt-5-codex"}),
          {:note, "without params.model the init :model is the fallback"},
          session_new_step(13)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_caller_model_wins_over_init_model", steps)

    assert [
             %{"id" => 3, "method" => "thread/start", "params" => %{"model" => "gpt-5-codex"}},
             %{"id" => 4, "method" => "thread/resume", "params" => %{"model" => "gpt-5-codex"}},
             %{"id" => 5, "method" => "thread/resume", "params" => %{"model" => "gpt-5-codex"}},
             %{"id" => 6, "method" => "thread/start", "params" => %{"model" => "gpt-5"}}
           ] = Enum.drop(CodexGolden.writes(transcript), 3)

    assert [
             %{"id" => 10, "result" => %{"models" => %{"currentModelId" => "codex-mini/medium"}}},
             %{"id" => 11, "result" => %{"sessionId" => "thread-abc"}},
             %{"id" => 12, "result" => %{"sessionId" => "thread-abc"}}
           ] = CodexGolden.messages(transcript)
  end

  test "session_new_reply_model_and_effort_override_defaults" do
    steps =
      connected_steps(model: "gpt-5", reasoning_effort: "low") ++
        [
          {:note,
           "thread/start carries the init model; the reply's model and reasoningEffort then replace the init defaults in the stored session"},
          session_new_step(10),
          thread_start_reply(3, %{"model" => "gpt-5-codex", "reasoningEffort" => "high"}),
          prompt_step(20),
          {:note,
           "a reply without model or reasoningEffort keeps the init defaults for that session"},
          session_new_step(11),
          {:inbound,
           %{
             "id" => 5,
             "result" => %{
               "thread" => %{
                 "id" => "thread-def",
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_001
               }
             }
           }},
          {:outbound,
           %{
             "method" => "session/prompt",
             "id" => 21,
             "params" => %{
               "sessionId" => "thread-def",
               "prompt" => [%{"type" => "text", "text" => "hi again"}]
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_new_reply_model_and_effort_override_defaults",
        steps
      )

    assert [
             %{"id" => 3, "method" => "thread/start", "params" => %{"model" => "gpt-5"}},
             %{
               "id" => 4,
               "method" => "turn/start",
               "params" => %{
                 "threadId" => "thread-abc",
                 "model" => "gpt-5-codex",
                 "effort" => "high"
               }
             },
             %{"id" => 5, "method" => "thread/start", "params" => %{"model" => "gpt-5"}},
             %{
               "id" => 6,
               "method" => "turn/start",
               "params" => %{"threadId" => "thread-def", "model" => "gpt-5", "effort" => "low"}
             }
           ] = Enum.drop(CodexGolden.writes(transcript), 3)

    assert [
             %{
               "id" => 10,
               "result" => %{
                 "models" => %{"currentModelId" => "codex-mini/high"},
                 "configOptions" => first_options
               }
             },
             %{
               "id" => 11,
               "result" => %{
                 "models" => %{"currentModelId" => "gpt-5/low"},
                 "configOptions" => second_options
               }
             }
           ] = CodexGolden.messages(transcript)

    assert %{"currentValue" => "high"} =
             Enum.find(first_options, &(&1["id"] == "reasoning_effort"))

    assert %{"currentValue" => "low"} =
             Enum.find(second_options, &(&1["id"] == "reasoning_effort"))
  end

  test "session_new_mode_read_only" do
    steps =
      connected_steps() ++
        [session_new_step(10, %{"modeId" => "read-only"}), thread_start_reply(3)]

    transcript = CodexGolden.assert_golden(@area, "session_new_mode_read_only", steps)

    assert %{"params" => %{"sandbox" => "workspace-write", "approvalPolicy" => "on-request"}} =
             List.last(CodexGolden.writes(transcript))

    assert [%{"result" => %{"modes" => %{"currentModeId" => "read-only"}}}] =
             CodexGolden.messages(transcript)
  end

  test "session_new_mode_agent_explicit" do
    steps =
      connected_steps() ++
        [session_new_step(10, %{"modeId" => "agent"}), thread_start_reply(3)]

    transcript = CodexGolden.assert_golden(@area, "session_new_mode_agent_explicit", steps)

    assert %{"params" => %{"sandbox" => "workspace-write", "approvalPolicy" => "on-request"}} =
             List.last(CodexGolden.writes(transcript))

    assert [%{"result" => %{"modes" => %{"currentModeId" => "agent"}}}] =
             CodexGolden.messages(transcript)
  end

  test "session_new_mode_agent_full_access" do
    steps =
      connected_steps() ++
        [session_new_step(10, %{"modeId" => "agent-full-access"}), thread_start_reply(3)]

    transcript = CodexGolden.assert_golden(@area, "session_new_mode_agent_full_access", steps)

    assert %{"params" => %{"sandbox" => "danger-full-access", "approvalPolicy" => "never"}} =
             List.last(CodexGolden.writes(transcript))

    assert [%{"result" => %{"modes" => %{"currentModeId" => "agent-full-access"}}}] =
             CodexGolden.messages(transcript)
  end

  test "session_new_approval_policy_alias" do
    steps =
      connected_steps() ++
        [
          {:note, "approvalPolicy is accepted as an alias for modeId"},
          session_new_step(10, %{"approvalPolicy" => "agent-full-access"}),
          thread_start_reply(3)
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_approval_policy_alias", steps)

    assert %{"params" => %{"sandbox" => "danger-full-access", "approvalPolicy" => "never"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "session_new_mode_id_wins_over_approval_policy" do
    steps =
      connected_steps() ++
        [
          {:note, "modeId is consulted before its approvalPolicy alias when both are present"},
          session_new_step(10, %{"modeId" => "read-only", "approvalPolicy" => "agent-full-access"}),
          thread_start_reply(3)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_mode_id_wins_over_approval_policy", steps)

    assert %{"params" => %{"sandbox" => "workspace-write", "approvalPolicy" => "on-request"}} =
             List.last(CodexGolden.writes(transcript))

    assert [%{"id" => 10, "result" => %{"modes" => %{"currentModeId" => "read-only"}}}] =
             CodexGolden.messages(transcript)
  end

  test "session_new_unknown_mode_id_omits_sandbox" do
    steps =
      connected_steps() ++
        [
          {:note,
           "an unknown modeId is not validated: no sandbox keys, yet it is echoed as currentModeId"},
          session_new_step(10, %{"modeId" => "yolo"}),
          thread_start_reply(3)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_unknown_mode_id_omits_sandbox", steps)

    assert %{"params" => params} = List.last(CodexGolden.writes(transcript))
    refute Map.has_key?(params, "sandbox") or Map.has_key?(params, "approvalPolicy")

    assert [%{"result" => %{"modes" => %{"currentModeId" => "yolo"}}}] =
             CodexGolden.messages(transcript)
  end

  test "session_new_mode_from_thread_start_result" do
    steps =
      connected_steps() ++
        [
          {:note, "the app-server's activePermissionProfile wins over the requested mode"},
          session_new_step(10, %{"modeId" => "agent"}),
          thread_start_reply(3, %{"activePermissionProfile" => %{"id" => ":read-only"}})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_mode_from_thread_start_result", steps)

    assert [%{"result" => %{"modes" => %{"currentModeId" => "read-only"}}}] =
             CodexGolden.messages(transcript)
  end

  test "session_new_additional_directories" do
    steps =
      connected_steps() ++
        [
          {:note,
           "entries are trimmed and de-duplicated; they become writable roots and trusted projects"},
          session_new_step(10, %{
            "additionalDirectories" => ["/tmp/shared", "/tmp/shared", " /tmp/other "]
          }),
          thread_start_reply(3),
          {:note, "the stored directories widen the sandbox of every later turn"},
          prompt_step(20)
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_additional_directories", steps)

    assert [_, _, _, %{"method" => "thread/start", "params" => %{"config" => config}}, turn_start] =
             CodexGolden.writes(transcript)

    assert %{"sandbox_workspace_write" => %{"writable_roots" => ["/tmp/shared", "/tmp/other"]}} =
             config

    assert %{
             "params" => %{"sandboxPolicy" => %{"writableRoots" => ["/tmp/shared", "/tmp/other"]}}
           } =
             turn_start
  end

  test "session_new_additional_directories_validation_errors" do
    steps =
      connected_steps() ++
        [
          session_new_step(10, %{"additionalDirectories" => "/tmp/shared"}),
          session_new_step(11, %{"additionalDirectories" => ["relative/path"]}),
          session_new_step(12, %{"additionalDirectories" => [""]}),
          session_new_step(13, %{"additionalDirectories" => [42]}),
          session_new_step(14, %{"additionalDirectories" => ["/srv/elsewhere"]})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_new_additional_directories_validation_errors",
        steps
      )

    errors = transcript |> Enum.drop(4) |> Enum.map(& &1.result.error)

    assert errors == [
             "additionalDirectories must be a list of absolute paths",
             "additionalDirectories entries must be absolute paths",
             "additionalDirectories entries must not be empty",
             "additionalDirectories entries must be strings",
             "Workspace path is not authorized"
           ]
  end

  test "session_new_with_codex_config_map" do
    config = %{
      "model_reasoning_summary" => "detailed",
      "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/extra"], "network_access" => true}
    }

    steps =
      connected_steps(codex_config: config) ++
        [
          {:note,
           "adapter-level config is the base; session directories append to writable_roots"},
          session_new_step(10, %{"additionalDirectories" => ["/tmp/shared"]})
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_with_codex_config_map", steps)

    assert %{"params" => %{"config" => wire_config}} = List.last(CodexGolden.writes(transcript))
    assert wire_config["model_reasoning_summary"] == "detailed"

    assert %{"writable_roots" => ["/tmp/extra", "/tmp/shared"], "network_access" => true} =
             wire_config["sandbox_workspace_write"]
  end

  test "session_new_with_codex_config_json_string" do
    steps =
      connected_steps(codex_config: ~s({"model_reasoning_summary":"detailed"})) ++
        [session_new_step(10)]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_with_codex_config_json_string", steps)

    assert %{"params" => %{"config" => %{"model_reasoning_summary" => "detailed"}}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "session_new_with_invalid_codex_config_json_ignored" do
    steps =
      connected_steps(codex_config: "not json") ++
        [{:note, "an undecodable CODEX config string is silently dropped"}, session_new_step(10)]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_new_with_invalid_codex_config_json_ignored",
        steps
      )

    assert %{"params" => %{"config" => config}} = List.last(CodexGolden.writes(transcript))
    assert Map.keys(config) == ["projects"]
  end

  test "session_new_with_model_provider_init_option" do
    steps = connected_steps(model_provider: "azure") ++ [session_new_step(10)]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_with_model_provider_init_option", steps)

    assert %{"params" => %{"modelProvider" => "azure"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "session_new_after_gateway_authenticate" do
    steps =
      connected_steps() ++
        [
          {:note, "gateway auth never reaches the app-server; it only records provider config"},
          authenticate_step(30, "gateway", %{
            "_meta" => %{
              "gateway" => %{
                "baseUrl" => "https://gateway.example.com/v1",
                "providerName" => "Acme Gateway",
                "headers" => %{"Authorization" => "Bearer gw-token"}
              }
            }
          }),
          session_new_step(10),
          thread_start_reply(3)
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_after_gateway_authenticate", steps)

    assert %{tag: :reply, reply: %{}} = Enum.at(transcript, 5).result

    assert %{"params" => %{"modelProvider" => "custom-gateway", "config" => config}} =
             List.last(CodexGolden.writes(transcript))

    assert %{"model_providers" => %{"custom-gateway" => %{"name" => "Acme Gateway"}}} = config
  end

  test "session_new_requires_absolute_cwd" do
    steps =
      connected_steps() ++
        [
          session_new_step(10, %{"cwd" => "relative/dir"}),
          {:note, "without a cwd the adapter's :cwd init option is used"},
          {:outbound, %{"method" => "session/new", "id" => 11, "params" => %{"mcpServers" => []}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_requires_absolute_cwd", steps)

    assert %{tag: :error, error: "Workspace paths must be absolute"} =
             Enum.at(transcript, 4).result

    assert %{"params" => %{"cwd" => "/tmp/project"}} = List.last(CodexGolden.writes(transcript))
  end

  test "session_new_blank_cwd_errors" do
    steps =
      connected_steps() ++
        [
          {:note,
           "a blank cwd is truthy: it is validated instead of falling back to the init :cwd"},
          session_new_step(10, %{"cwd" => ""})
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_blank_cwd_errors", steps)

    assert %{tag: :error, error: "Workspace paths must be absolute"} =
             Enum.at(transcript, 5).result

    assert length(CodexGolden.writes(transcript)) == 3
  end

  test "session_new_mode_from_thread_start_settings" do
    steps =
      connected_steps() ++
        [
          {:note,
           "every reply shape the adapter derives a mode from; the requested mode differs"},
          session_new_step(10, %{"modeId" => "read-only"}),
          thread_reply(3, "thread-1", %{"activePermissionProfile" => %{"id" => ":workspace"}}),
          session_new_step(11, %{"modeId" => "read-only"}),
          thread_reply(4, "thread-2", %{
            "activePermissionProfile" => %{"id" => ":danger-no-sandbox"}
          }),
          session_new_step(12, %{"modeId" => "agent-full-access"}),
          thread_reply(5, "thread-3", %{
            "settings" => %{"activePermissionProfile" => %{"id" => ":read-only"}}
          }),
          session_new_step(13, %{"modeId" => "agent-full-access"}),
          thread_reply(6, "thread-4", %{
            "threadSettings" => %{
              "sandboxPolicy" => %{"type" => "workspaceWrite"},
              "approvalsReviewer" => "user"
            }
          }),
          session_new_step(14, %{"modeId" => "read-only"}),
          thread_reply(7, "thread-5", %{
            "threadSettings" => %{
              "sandboxPolicy" => %{"type" => "workspaceWrite"},
              "approvalsReviewer" => "auto_review"
            }
          }),
          session_new_step(15, %{"modeId" => "read-only"}),
          thread_reply(8, "thread-6", %{
            "settings" => %{"sandboxPolicy" => %{"type" => "dangerFullAccess"}}
          }),
          session_new_step(16, %{"modeId" => "agent-full-access"}),
          thread_reply(9, "thread-7", %{"sandbox" => "read-only"}),
          {:note, "an unknown profile id with no settings falls back to the requested mode"},
          session_new_step(17, %{"modeId" => "read-only"}),
          thread_reply(10, "thread-8", %{"activePermissionProfile" => %{"id" => ":custom"}})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_mode_from_thread_start_settings", steps)

    assert transcript
           |> CodexGolden.messages()
           |> Enum.map(& &1["result"]["modes"]["currentModeId"]) == [
             "agent",
             "agent-full-access",
             "read-only",
             "read-only",
             "agent",
             "agent-full-access",
             "read-only",
             "read-only"
           ]
  end

  test "session_new_thread_id_fallback_keys" do
    steps =
      connected_steps() ++
        [
          {:note, "thread.id, then thread.sessionId, then threadId, then sessionId"},
          session_new_step(10),
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-nested-id",
                 "sessionId" => "thread-nested-session-ignored",
                 "cwd" => "/tmp/project"
               },
               "threadId" => "thread-top-ignored",
               "sessionId" => "thread-flat-ignored"
             }
           }},
          session_new_step(11),
          {:inbound,
           %{
             "id" => 4,
             "result" => %{
               "model" => "gpt-5",
               "threadId" => "thread-top",
               "sessionId" => "thread-flat-ignored"
             }
           }},
          session_new_step(12),
          {:inbound,
           %{
             "id" => 5,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{"sessionId" => "thread-nested", "cwd" => "/tmp/project"},
               "threadId" => "thread-top-ignored"
             }
           }},
          session_new_step(13),
          {:inbound,
           %{"id" => 6, "result" => %{"model" => "gpt-5", "sessionId" => "thread-flat"}}},
          {:note, "with no id at all the result still answers, with an empty sessionId"},
          session_new_step(14),
          {:inbound, %{"id" => 7, "result" => %{"model" => "gpt-5"}}},
          {:note, "the fallback ids are real sessions; the empty one is not addressable"},
          {:outbound,
           %{"method" => "session/close", "id" => 15, "params" => %{"sessionId" => "thread-top"}}},
          {:outbound,
           %{"method" => "session/close", "id" => 16, "params" => %{"sessionId" => ""}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_thread_id_fallback_keys", steps)

    assert transcript
           |> CodexGolden.messages()
           |> Enum.map(& &1["result"]["sessionId"]) ==
             ["thread-nested-id", "thread-top", "thread-nested", "thread-flat", ""]

    assert %{writes: [%{"id" => 8, "method" => "thread/unsubscribe"}]} =
             Enum.at(transcript, 17).result

    assert %{tag: :error, error: "sessionId is required"} = CodexGolden.last_result(transcript)
  end

  test "session_new_result_cwd_wins_over_thread_cwd" do
    steps =
      connected_steps() ++
        [
          session_new_step(10),
          {:note,
           "a top-level result.cwd is the stored session cwd even when thread.cwd differs; it shows on the next turn/start when the prompt omits cwd"},
          thread_start_reply(3, %{"cwd" => "/tmp/project/sub"}),
          prompt_step(20)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_result_cwd_wins_over_thread_cwd", steps)

    assert %{"method" => "thread/start", "params" => %{"cwd" => "/tmp/project"}} =
             Enum.at(CodexGolden.writes(transcript), 3)

    assert %{"method" => "turn/start", "params" => %{"cwd" => "/tmp/project/sub"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "session_new_reply_history_is_not_replayed" do
    steps =
      connected_steps() ++
        [
          session_new_step(10),
          {:note,
           "history in a thread/start reply is ignored: only thread/resume replies replay turns"},
          thread_start_reply(3, %{
            "initialTurnsPage" => %{
              "data" => [
                %{
                  "id" => "turn-0",
                  "status" => "completed",
                  "items" => [
                    %{
                      "type" => "reasoning",
                      "id" => "item-1",
                      "text" => "Considering the request"
                    },
                    %{"type" => "agent_message", "id" => "item-2", "text" => "previous answer"}
                  ]
                }
              ],
              "nextCursor" => nil
            }
          })
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_reply_history_is_not_replayed", steps)

    assert [%{"id" => 10, "result" => %{"sessionId" => "thread-abc"}}] =
             CodexGolden.messages(transcript)
  end

  test "session_new_authorize_workspace_callback_arity_2" do
    callback = fn
      "/srv/data", %{kind: :cwd, adapter: ExMCP.ACP.Adapters.Codex} -> :ok
      "/srv/data/vendor", %{kind: {:additional_directory, "/srv/data"}} -> {:ok, :granted}
      "/srv/data", %{kind: :session_list} -> true
      "/boom", _context -> raise "callback crashed"
      _path, _context -> false
    end

    steps =
      connected_steps(authorize_workspace: callback) ++
        [
          {:note, "the callback replaces the workspace_roots check entirely"},
          session_new_step(10, %{
            "cwd" => "/srv/data",
            "additionalDirectories" => ["/srv/data/vendor"]
          }),
          session_new_step(11),
          {:note, "a raising callback is treated as a denial"},
          session_new_step(12, %{"cwd" => "/boom"}),
          session_new_step(13, %{
            "cwd" => "/srv/data",
            "additionalDirectories" => ["/srv/other"]
          }),
          {:outbound,
           %{"method" => "session/list", "id" => 14, "params" => %{"cwd" => "/srv/data"}}},
          {:outbound,
           %{"method" => "session/list", "id" => 15, "params" => %{"cwd" => "/tmp/project"}}}
        ]

    {transcript, log} =
      with_log(fn ->
        CodexGolden.assert_golden(
          @area,
          "session_new_authorize_workspace_callback_arity_2",
          steps
        )
      end)

    assert log =~ "Codex authorization callback failed"

    assert [
             _initialize,
             _initialized,
             _model_list,
             %{"id" => 3, "method" => "thread/start", "params" => %{"cwd" => "/srv/data"}},
             %{"id" => 4, "method" => "thread/list", "params" => %{"cwd" => "/srv/data"}}
           ] = CodexGolden.writes(transcript)

    assert transcript |> Enum.drop(5) |> Enum.map(& &1.result[:error]) == [
             nil,
             "Workspace path is not authorized",
             nil,
             "Workspace path is not authorized",
             "Workspace path is not authorized",
             nil,
             "Workspace path is not authorized"
           ]
  end

  test "session_new_authorize_workspace_callback_arity_1" do
    callback = fn
      "/srv/data" -> {:ok, "/srv/data"}
      "/srv/data/vendor" -> :ok
      "/boom" -> throw(:denied)
      _path -> false
    end

    steps =
      connected_steps(authorize_workspace: callback) ++
        [
          session_new_step(10, %{
            "cwd" => "/srv/data",
            "additionalDirectories" => ["/srv/data/vendor"]
          }),
          {:note, "a throwing callback is treated as a denial"},
          session_new_step(11, %{"cwd" => "/boom"}),
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/tmp/project"}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_authorize_workspace_callback_arity_1", steps)

    assert %{"method" => "thread/start", "params" => %{"cwd" => "/srv/data", "config" => config}} =
             List.last(CodexGolden.writes(transcript))

    assert %{"sandbox_workspace_write" => %{"writable_roots" => ["/srv/data/vendor"]}} = config

    assert transcript |> Enum.take(-2) |> Enum.map(& &1.result.error) ==
             ["Workspace path is not authorized", "Workspace path is not authorized"]
  end

  test "session_new_invalid_authorize_workspace_callback" do
    steps =
      connected_steps(authorize_workspace: :always) ++
        [
          {:note, "absoluteness is checked before the callback is inspected"},
          session_new_step(10, %{"cwd" => "relative/dir"}),
          session_new_step(11),
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/tmp/project"}
           }},
          {:outbound, %{"method" => "session/list", "id" => 13, "params" => %{}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_invalid_authorize_workspace_callback", steps)

    assert transcript |> Enum.drop(5) |> Enum.map(& &1.result.error) == [
             "Workspace paths must be absolute",
             "Invalid workspace authorization callback",
             "Invalid workspace authorization callback",
             "Invalid workspace authorization callback"
           ]

    assert length(CodexGolden.writes(transcript)) == 3
  end

  test "session_new_workspace_roots_defaults_to_cwd" do
    steps =
      [
        {:init,
         [
           authorize_mcp_server: fn _server, _context -> true end,
           trust_authorized_workspaces: true,
           cwd: "/tmp/project"
         ]}
        | handshake_steps()
      ] ++
        [
          {:note, "without workspace_roots the init :cwd is the only authorized root"},
          session_new_step(10, %{"cwd" => "/tmp/project/sub"}),
          thread_reply(3, "thread-sub"),
          session_new_step(11, %{"cwd" => "/tmp/other"}),
          {:outbound, %{"method" => "session/new", "id" => 12, "params" => %{"mcpServers" => []}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_workspace_roots_defaults_to_cwd", steps)

    assert [
             %{"method" => "thread/start", "params" => %{"cwd" => "/tmp/project/sub"}},
             %{"method" => "thread/start", "params" => %{"cwd" => "/tmp/project"}}
           ] = Enum.drop(CodexGolden.writes(transcript), 3)

    assert %{tag: :error, error: "Workspace path is not authorized"} =
             Enum.at(transcript, 7).result
  end

  test "session_new_workspace_roots_single_string" do
    steps =
      [
        {:init,
         [
           workspace_roots: "/tmp/project",
           authorize_mcp_server: fn _server, _context -> true end,
           trust_authorized_workspaces: true,
           cwd: "/tmp/project"
         ]}
        | handshake_steps()
      ] ++
        [
          session_new_step(10, %{"cwd" => "/tmp/project/sub"}),
          session_new_step(11, %{"cwd" => "/tmp/elsewhere"})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_workspace_roots_single_string", steps)

    assert %{"method" => "thread/start", "params" => %{"cwd" => "/tmp/project/sub"}} =
             List.last(CodexGolden.writes(transcript))

    assert %{tag: :error, error: "Workspace path is not authorized"} =
             CodexGolden.last_result(transcript)
  end

  test "session_new_without_any_cwd_errors" do
    steps =
      [{:init, CodexGolden.default_init_opts()} | handshake_steps()] ++
        [
          {:note,
           "no params.cwd and no init :cwd: the nil workspace fails the absolute-path check, not the roots check"},
          {:outbound,
           %{"method" => "session/new", "id" => 10, "params" => %{"mcpServers" => []}}},
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 11,
             "params" => %{"sessionId" => "thread-abc", "mcpServers" => []}
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_without_any_cwd_errors", steps)

    assert transcript |> Enum.drop(5) |> Enum.map(& &1.result) == [
             %{tag: :error, error: "Workspace paths must be absolute"},
             %{tag: :error, error: "Workspace paths must be absolute"}
           ]

    assert length(CodexGolden.writes(transcript)) == 3
  end

  test "session_requests_without_params_are_skipped" do
    steps =
      connected_steps() ++
        [
          {:note,
           "without a params key these match the ACP-response clause: no error and no write"},
          {:outbound, %{"method" => "session/new", "id" => 10}},
          {:outbound, %{"method" => "session/load", "id" => 11}},
          {:outbound, %{"method" => "session/resume", "id" => 12}},
          {:outbound, %{"method" => "session/list", "id" => 13}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_requests_without_params_are_skipped", steps)

    assert transcript |> Enum.drop(5) |> Enum.map(& &1.result) |> Enum.uniq() ==
             [%{tag: :ok, skipped: true}]

    assert length(CodexGolden.writes(transcript)) == 3
  end

  # -- session/load and session/resume -----------------------------------------

  test "session_load_thread_resume_replays_history" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{
               "sessionId" => "thread-abc",
               "cwd" => "/tmp/project",
               "model" => "gpt-5",
               "mcpServers" => []
             }
           }},
          {:note,
           "replayed history is emitted as session/update notifications before the load result"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_000
               },
               "initialTurnsPage" => %{
                 "data" => [
                   %{
                     "id" => "turn-0",
                     "status" => "completed",
                     "items" => [
                       %{
                         "type" => "reasoning",
                         "id" => "item-1",
                         "summary" => "Considering the request"
                       },
                       %{"type" => "agent_message", "id" => "item-2", "text" => "previous answer"}
                     ]
                   }
                 ],
                 "nextCursor" => nil
               }
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_load_thread_resume_replays_history", steps)

    assert %{
             "method" => "thread/resume",
             "params" => %{"threadId" => "thread-abc", "initialTurnsPage" => %{"limit" => 100}}
           } = List.last(CodexGolden.writes(transcript))

    assert [
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_thought_chunk"}}},
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_message_chunk"}}},
             %{"id" => 12, "result" => %{"sessionId" => "thread-abc"}}
           ] = CodexGolden.messages(transcript)
  end

  test "session_resume_exclude_turns" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/resume",
             "id" => 13,
             "params" => %{
               "sessionId" => "thread-abc",
               "cwd" => "/tmp/project",
               "modeId" => "read-only"
             }
           }},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_000
               }
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_resume_exclude_turns", steps)

    assert %{"method" => "thread/resume", "params" => %{"excludeTurns" => true} = params} =
             List.last(CodexGolden.writes(transcript))

    refute Map.has_key?(params, "initialTurnsPage")

    assert [%{"id" => 13, "result" => %{"modes" => %{"currentModeId" => "read-only"}}}] =
             CodexGolden.messages(transcript)
  end

  test "session_resume_replays_returned_history" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/resume",
             "id" => 13,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/tmp/project"}
           }},
          {:note,
           "resume shares the thread/resume reply handler with load: history returned despite excludeTurns is replayed before the result"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_000
               },
               "initialTurnsPage" => %{
                 "data" => [
                   %{
                     "id" => "turn-0",
                     "status" => "completed",
                     "items" => [
                       %{"type" => "agent_message", "id" => "item-1", "text" => "previous answer"}
                     ]
                   }
                 ],
                 "nextCursor" => nil
               }
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_resume_replays_returned_history", steps)

    assert %{"method" => "thread/resume", "params" => %{"excludeTurns" => true}} =
             List.last(CodexGolden.writes(transcript))

    assert [
             %{
               "method" => "session/update",
               "params" => %{
                 "update" => %{
                   "sessionUpdate" => "agent_message_chunk",
                   "content" => %{"text" => "previous answer"},
                   "_meta" => %{"ex_mcp" => %{"replay" => true}}
                 }
               }
             },
             %{"id" => 13, "result" => %{"sessionId" => "thread-abc"}}
           ] = CodexGolden.messages(transcript)
  end

  test "session_load_and_resume_require_session_id" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{"method" => "session/load", "id" => 12, "params" => %{"cwd" => "/tmp/project"}}},
          {:outbound,
           %{
             "method" => "session/resume",
             "id" => 13,
             "params" => %{"sessionId" => "", "cwd" => "/tmp/project"}
           }},
          {:note, "sessionId is checked before the workspace"},
          {:outbound,
           %{"method" => "session/load", "id" => 14, "params" => %{"cwd" => "/srv/elsewhere"}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_load_and_resume_require_session_id", steps)

    assert Enum.map(Enum.drop(transcript, 4), & &1.result[:error]) ==
             ["sessionId is required", "sessionId is required", nil, "sessionId is required"]
  end

  test "session_load_caller_cwd_wins_over_init_cwd" do
    steps =
      connected_steps() ++
        [
          {:note,
           "the caller's cwd is the one resumed, trusted and authorized; the init :cwd is only a fallback"},
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/tmp/other", "mcpServers" => []}
           }},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/other",
                 "updatedAt" => 1_700_000_000
               }
             }
           }},
          {:outbound,
           %{
             "method" => "session/resume",
             "id" => 13,
             "params" => %{"sessionId" => "thread-res", "cwd" => "/tmp/other"}
           }},
          {:inbound,
           %{
             "id" => 4,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-res",
                 "cwd" => "/tmp/other",
                 "updatedAt" => 1_700_000_000
               }
             }
           }},
          {:note,
           "a cwd outside the workspace roots is rejected although the init :cwd is inside them"},
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 14,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/srv/elsewhere"}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_load_caller_cwd_wins_over_init_cwd", steps)

    assert transcript
           |> CodexGolden.writes()
           |> Enum.drop(3)
           |> Enum.map(
             &{&1["method"], &1["params"]["cwd"],
              Map.keys(get_in(&1, ["params", "config", "projects"]))}
           ) == [
             {"thread/resume", "/tmp/other", ["/tmp/other"]},
             {"thread/resume", "/tmp/other", ["/tmp/other"]}
           ]

    assert %{tag: :error, error: "Workspace path is not authorized"} =
             Enum.at(transcript, 10).result

    assert length(CodexGolden.writes(transcript)) == 5
  end

  test "session_load_after_gateway_authenticate" do
    steps =
      connected_steps() ++
        [
          authenticate_step(30, "gateway", %{
            "_meta" => %{
              "gateway" => %{
                "baseUrl" => "https://gateway.example.com/v1",
                "providerName" => "Acme Gateway"
              }
            }
          }),
          {:note,
           "resumed threads carry the gateway provider instead of the \"openai\" resume default"},
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/tmp/project"}
           }},
          thread_reply(3, "thread-abc"),
          {:outbound,
           %{
             "method" => "session/resume",
             "id" => 13,
             "params" => %{"sessionId" => "thread-res", "cwd" => "/tmp/project"}
           }},
          thread_reply(4, "thread-res")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_load_after_gateway_authenticate", steps)

    assert transcript
           |> CodexGolden.writes()
           |> Enum.drop(3)
           |> Enum.map(
             &{&1["method"], &1["params"]["modelProvider"],
              get_in(&1, ["params", "config", "model_providers", "custom-gateway", "base_url"])}
           ) == [
             {"thread/resume", "custom-gateway", "https://gateway.example.com/v1"},
             {"thread/resume", "custom-gateway", "https://gateway.example.com/v1"}
           ]

    assert [%{"id" => 12, "result" => _}, %{"id" => 13, "result" => _}] =
             CodexGolden.messages(transcript)
  end

  test "session_load_and_resume_approval_policy_alias" do
    steps =
      connected_steps() ++
        [
          {:note, "approvalPolicy is accepted as a modeId alias on load and resume too"},
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{
               "sessionId" => "thread-abc",
               "cwd" => "/tmp/project",
               "approvalPolicy" => "read-only"
             }
           }},
          thread_reply(3, "thread-abc"),
          {:outbound,
           %{
             "method" => "session/resume",
             "id" => 13,
             "params" => %{
               "sessionId" => "thread-res",
               "cwd" => "/tmp/project",
               "approvalPolicy" => "agent-full-access"
             }
           }},
          thread_reply(4, "thread-res")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_load_and_resume_approval_policy_alias", steps)

    assert transcript
           |> CodexGolden.writes()
           |> Enum.drop(3)
           |> Enum.map(&{&1["params"]["sandbox"], &1["params"]["approvalPolicy"]}) ==
             [{"workspace-write", "on-request"}, {"danger-full-access", "never"}]

    assert transcript
           |> CodexGolden.messages()
           |> Enum.map(&get_in(&1, ["result", "modes", "currentModeId"])) ==
             ["read-only", "agent-full-access"]
  end

  test "session_load_replays_legacy_thread_turns" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/tmp/project"}
           }},
          {:note,
           "without initialTurnsPage the legacy thread.turns is replayed; non-text items go through the item/completed path"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/project",
                 "turns" => [
                   %{
                     "id" => "turn-0",
                     "items" => [
                       %{"type" => "reasoning", "id" => "item-1", "text" => "Thinking it over"},
                       %{
                         "type" => "commandExecution",
                         "id" => "item-2",
                         "command" => "mix test",
                         "status" => "completed",
                         "exitCode" => 0,
                         "aggregatedOutput" => "1 test, 0 failures\n"
                       },
                       %{"type" => "userMessage", "id" => "item-3", "text" => "and then?"},
                       %{"type" => "agent_message", "id" => "item-4", "message" => "All green."}
                     ]
                   }
                 ]
               }
             }
           }},
          {:note, "an empty initialTurnsPage.data wins over thread.turns: nothing is replayed"},
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 13,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/tmp/project"}
           }},
          {:inbound,
           %{
             "id" => 4,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/project",
                 "turns" => [
                   %{
                     "id" => "turn-0",
                     "items" => [%{"type" => "agent_message", "id" => "item-4", "text" => "old"}]
                   }
                 ]
               },
               "initialTurnsPage" => %{"data" => [], "nextCursor" => nil}
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_load_replays_legacy_thread_turns", steps)

    assert [
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_thought_chunk"}}},
             %{
               "params" => %{
                 "update" => %{
                   "sessionUpdate" => "tool_call_update",
                   "toolCallId" => "item-2",
                   "_meta" => %{"ex_mcp" => %{"replay" => true}}
                 }
               }
             },
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_message_chunk"}}},
             %{"id" => 12, "result" => %{"sessionId" => "thread-abc"}}
           ] = Enum.at(transcript, 6).result.messages

    assert %{messages: [%{"id" => 13, "result" => %{"sessionId" => "thread-abc"}}]} =
             CodexGolden.last_result(transcript)
  end

  test "session_load_replay_text_wins_over_message" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{"sessionId" => "thread-abc", "cwd" => "/tmp/project"}
           }},
          {:note,
           "replayed agent_message and reasoning items carrying both keys use text; message/summary are only fallbacks"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/project",
                 "turns" => [
                   %{
                     "id" => "turn-0",
                     "items" => [
                       %{
                         "type" => "agent_message",
                         "id" => "item-1",
                         "text" => "from text",
                         "message" => "from message"
                       },
                       %{
                         "type" => "reasoning",
                         "id" => "item-2",
                         "text" => "thought text",
                         "summary" => "thought summary"
                       }
                     ]
                   }
                 ]
               }
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_load_replay_text_wins_over_message", steps)

    assert [
             %{
               "params" => %{
                 "update" => %{
                   "sessionUpdate" => "agent_message_chunk",
                   "content" => %{"text" => "from text"}
                 }
               }
             },
             %{
               "params" => %{
                 "update" => %{
                   "sessionUpdate" => "agent_thought_chunk",
                   "content" => %{"text" => "thought text"}
                 }
               }
             },
             %{"id" => 12, "result" => %{"sessionId" => "thread-abc"}}
           ] = CodexGolden.messages(transcript)
  end

  test "session_additional_roots_meta_alias" do
    steps =
      connected_steps() ++
        [
          {:note,
           "_meta.additionalRoots is an alias read only when additionalDirectories is absent"},
          session_new_step(10, %{"_meta" => %{"additionalRoots" => ["/tmp/alias"]}}),
          thread_start_reply(3),
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 11,
             "params" => %{
               "sessionId" => "thread-load",
               "cwd" => "/tmp/project",
               "additionalDirectories" => ["/tmp/shared"],
               "_meta" => %{"additionalRoots" => ["/tmp/ignored"]}
             }
           }},
          thread_reply(4, "thread-load"),
          {:outbound,
           %{
             "method" => "session/resume",
             "id" => 12,
             "params" => %{
               "sessionId" => "thread-res",
               "cwd" => "/tmp/project",
               "_meta" => %{"additionalRoots" => ["/tmp/other"]}
             }
           }},
          thread_reply(5, "thread-res"),
          {:note, "the loaded session's directories widen its turns"},
          {:outbound,
           %{
             "method" => "session/prompt",
             "id" => 20,
             "params" => %{
               "sessionId" => "thread-load",
               "prompt" => [%{"type" => "text", "text" => "hi"}]
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_additional_roots_meta_alias", steps)

    assert transcript
           |> CodexGolden.writes()
           |> Enum.drop(3)
           |> Enum.map(
             &get_in(&1, ["params", "config", "sandbox_workspace_write", "writable_roots"])
           ) ==
             [["/tmp/alias"], ["/tmp/shared"], ["/tmp/other"], nil]

    assert %{
             "method" => "turn/start",
             "params" => %{"sandboxPolicy" => %{"writableRoots" => ["/tmp/shared"]}}
           } = List.last(CodexGolden.writes(transcript))
  end

  # -- session/list ------------------------------------------------------------

  test "session_list_thread_list" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/list",
             "id" => 14,
             "params" => %{"cwd" => "/tmp/project", "cursor" => "abc", "limit" => 10}
           }},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "data" => [
                 %{
                   "id" => "thread-1",
                   "cwd" => "/tmp/project",
                   "name" => "Fix tests",
                   "updatedAt" => 1_700_000_000
                 },
                 %{
                   "id" => "thread-2",
                   "cwd" => "/tmp/project",
                   "preview" => "Add docs",
                   "updatedAt" => "2024-01-02T03:04:05Z"
                 },
                 %{"id" => "thread-3"}
               ],
               "nextCursor" => "next"
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_list_thread_list", steps)

    assert %{
             "method" => "thread/list",
             "params" => %{
               "cwd" => "/tmp/project",
               "cursor" => "abc",
               "limit" => 10,
               "archived" => false
             }
           } = List.last(CodexGolden.writes(transcript))

    assert [%{"id" => 14, "result" => %{"nextCursor" => "next", "sessions" => [_, _, _]}}] =
             CodexGolden.messages(transcript)
  end

  test "session_list_defaults_to_adapter_cwd" do
    steps =
      connected_steps() ++
        [
          {:outbound, %{"method" => "session/list", "id" => 14, "params" => %{}}},
          {:inbound, %{"id" => 3, "result" => %{"data" => [], "nextCursor" => nil}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_list_defaults_to_adapter_cwd", steps)

    assert %{"params" => %{"cwd" => "/tmp/project", "archived" => false}} =
             List.last(CodexGolden.writes(transcript))

    assert [%{"id" => 14, "result" => %{"sessions" => []} = result}] =
             CodexGolden.messages(transcript)

    refute Map.has_key?(result, "nextCursor")
  end

  test "session_list_non_string_cwd_falls_back_to_adapter_cwd" do
    steps =
      connected_steps() ++
        [
          {:note,
           "a non-string params.cwd is ignored rather than rejected: the adapter cwd is listed"},
          {:outbound, %{"method" => "session/list", "id" => 14, "params" => %{"cwd" => 42}}},
          {:inbound, %{"id" => 3, "result" => %{"data" => [], "nextCursor" => nil}}}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_list_non_string_cwd_falls_back_to_adapter_cwd",
        steps
      )

    assert %{
             "method" => "thread/list",
             "params" => %{"cwd" => "/tmp/project", "archived" => false}
           } = List.last(CodexGolden.writes(transcript))

    assert [%{"id" => 14, "result" => %{"sessions" => []}}] = CodexGolden.messages(transcript)
  end

  test "session_list_unscoped_omits_cwd" do
    steps =
      connected_steps(allow_unscoped_session_list: true) ++
        [{:outbound, %{"method" => "session/list", "id" => 14, "params" => %{}}}]

    transcript = CodexGolden.assert_golden(@area, "session_list_unscoped_omits_cwd", steps)

    assert %{"method" => "thread/list", "params" => %{"archived" => false} = params} =
             List.last(CodexGolden.writes(transcript))

    refute Map.has_key?(params, "cwd")
  end

  test "session_list_rejects_unauthorized_cwd" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{"method" => "session/list", "id" => 14, "params" => %{"cwd" => "/srv/elsewhere"}}},
          {:outbound,
           %{"method" => "session/list", "id" => 15, "params" => %{"cwd" => "relative"}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_list_rejects_unauthorized_cwd", steps)

    assert Enum.map(Enum.drop(transcript, 4), & &1.result.error) ==
             ["Workspace path is not authorized", "Workspace paths must be absolute"]
  end

  test "session_list_title_prefers_name_over_preview" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{"method" => "session/list", "id" => 14, "params" => %{"cwd" => "/tmp/project"}}},
          {:note, "name wins over preview; an updatedAt of any other type is dropped"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "data" => [
                 %{
                   "id" => "thread-1",
                   "cwd" => "/tmp/project",
                   "name" => "Fix tests",
                   "preview" => "fix the failing tests",
                   "updatedAt" => 1_700_000_000.5
                 },
                 %{
                   "sessionId" => "thread-2",
                   "cwd" => "/tmp/project",
                   "name" => nil,
                   "preview" => "Add docs",
                   "updatedAt" => %{"seconds" => 1_700_000_000}
                 }
               ],
               "nextCursor" => nil
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_list_title_prefers_name_over_preview", steps)

    assert [%{"id" => 14, "result" => %{"sessions" => sessions}}] =
             CodexGolden.messages(transcript)

    assert sessions == [
             %{"sessionId" => "thread-1", "cwd" => "/tmp/project", "title" => "Fix tests"},
             %{"sessionId" => "thread-2", "cwd" => "/tmp/project", "title" => "Add docs"}
           ]
  end

  test "session_list_entry_id_wins_over_session_id" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{"method" => "session/list", "id" => 14, "params" => %{"cwd" => "/tmp/project"}}},
          {:note, "an entry carrying both id and sessionId is listed under its id"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "data" => [
                 %{
                   "id" => "thread-1",
                   "sessionId" => "legacy-1",
                   "cwd" => "/tmp/project",
                   "name" => "Fix tests",
                   "updatedAt" => 1_700_000_000
                 }
               ],
               "nextCursor" => nil
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_list_entry_id_wins_over_session_id", steps)

    assert [%{"id" => 14, "result" => %{"sessions" => [%{"sessionId" => "thread-1"}]}}] =
             CodexGolden.messages(transcript)
  end

  test "session_list_blank_next_cursor_passthrough" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{"method" => "session/list", "id" => 14, "params" => %{"cwd" => "/tmp/project"}}},
          {:note,
           "only a nil nextCursor is omitted from the result; a blank one is passed through"},
          {:inbound, %{"id" => 3, "result" => %{"data" => [], "nextCursor" => ""}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_list_blank_next_cursor_passthrough", steps)

    assert [%{"id" => 14, "result" => %{"sessions" => [], "nextCursor" => ""}}] =
             CodexGolden.messages(transcript)
  end

  test "session_list_limit_zero_kept_blank_cursor_dropped" do
    steps =
      connected_steps() ++
        [
          {:note, "wire params drop nil and blank values but keep a zero limit"},
          {:outbound,
           %{
             "method" => "session/list",
             "id" => 14,
             "params" => %{"cwd" => "/tmp/project", "cursor" => "", "limit" => 0}
           }},
          {:outbound,
           %{
             "method" => "session/list",
             "id" => 15,
             "params" => %{"cwd" => "/tmp/project", "cursor" => nil, "limit" => nil}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_list_limit_zero_kept_blank_cursor_dropped", steps)

    assert [
             %{
               "id" => 3,
               "method" => "thread/list",
               "params" => %{"cwd" => "/tmp/project", "limit" => 0, "archived" => false} = first
             },
             %{
               "id" => 4,
               "method" => "thread/list",
               "params" => %{"cwd" => "/tmp/project", "archived" => false} = second
             }
           ] = Enum.drop(CodexGolden.writes(transcript), 3)

    refute Map.has_key?(first, "cursor")
    refute Map.has_key?(second, "cursor") or Map.has_key?(second, "limit")
  end

  # -- session/fork ------------------------------------------------------------

  test "session_fork_falls_through_to_skip" do
    steps =
      session_steps() ++
        [
          {:note, "no session/fork clause exists; the catch-all skips it without a write"},
          {:outbound,
           %{"method" => "session/fork", "id" => 16, "params" => %{"sessionId" => "thread-abc"}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_fork_falls_through_to_skip", steps)

    assert %{tag: :ok, skipped: true} = CodexGolden.last_result(transcript)
    assert length(CodexGolden.writes(transcript)) == 4
  end

  # -- session/close, session/delete, session/cancel ---------------------------

  test "session_close_unknown_session_errors" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{"method" => "session/close", "id" => 17, "params" => %{"sessionId" => "missing"}}},
          {:outbound, %{"method" => "session/close", "id" => 18, "params" => %{}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_close_unknown_session_errors", steps)

    assert Enum.map(Enum.drop(transcript, 6), & &1.result.error) ==
             ["Unknown Codex session: missing", "sessionId is required"]
  end

  test "session_close_idle_session_unsubscribes" do
    steps =
      session_steps() ++
        [
          close_step(17),
          {:inbound, %{"id" => 4, "result" => %{}}},
          {:note, "the closed session is gone: a second close is an unknown session"},
          close_step(18)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_close_idle_session_unsubscribes", steps)

    assert %{
             tag: :messages_and_write,
             messages: [],
             writes: [%{"id" => 4, "method" => "thread/unsubscribe"}]
           } =
             Enum.at(transcript, 6).result

    assert %{tag: :error, error: "Unknown Codex session: thread-abc"} =
             CodexGolden.last_result(transcript)
  end

  test "session_close_active_turn_interrupts" do
    steps =
      active_turn_steps() ++
        [
          {:note,
           "unsubscribe takes the first id but interrupt is written first; the prompt settles as cancelled"},
          close_step(17)
        ]

    transcript = CodexGolden.assert_golden(@area, "session_close_active_turn_interrupts", steps)

    assert %{
             tag: :messages_and_write,
             messages: [%{"id" => 20, "result" => %{"stopReason" => "cancelled"}}],
             writes: [
               %{"id" => 6, "method" => "turn/interrupt", "params" => %{"turnId" => "turn-1"}},
               %{"id" => 5, "method" => "thread/unsubscribe"}
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "session_delete_live_session_with_active_turn" do
    steps = active_turn_steps() ++ [delete_step(17, "thread-abc")]

    transcript =
      CodexGolden.assert_golden(@area, "session_delete_live_session_with_active_turn", steps)

    assert %{
             tag: :messages_and_write,
             messages: [%{"id" => 20, "result" => %{"stopReason" => "cancelled"}}],
             writes: [
               %{"id" => 5, "method" => "turn/interrupt"},
               %{"id" => 6, "method" => "thread/unsubscribe"},
               %{
                 "id" => 7,
                 "method" => "thread/archive",
                 "params" => %{"threadId" => "thread-abc"}
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "session_delete_idle_session" do
    steps = session_steps() ++ [delete_step(17, "thread-abc")]

    transcript = CodexGolden.assert_golden(@area, "session_delete_idle_session", steps)

    assert %{
             tag: :messages_and_write,
             messages: [],
             writes: [
               %{"id" => 4, "method" => "thread/unsubscribe"},
               %{"id" => 5, "method" => "thread/archive"}
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "session_delete_unknown_session_archives" do
    steps =
      session_steps() ++
        [
          delete_step(17, "missing"),
          {:inbound, %{"id" => 4, "result" => %{}}},
          {:outbound, %{"method" => "session/delete", "id" => 18, "params" => %{}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_delete_unknown_session_archives", steps)

    assert %{
             tag: :messages_and_write,
             messages: [],
             writes: [
               %{"id" => 4, "method" => "thread/archive", "params" => %{"threadId" => "missing"}}
             ]
           } = Enum.at(transcript, 6).result

    assert %{tag: :error, error: "sessionId is required"} = CodexGolden.last_result(transcript)
  end

  test "session_delete_unknown_session_fences_later_notifications" do
    steps =
      session_steps() ++
        [
          delete_step(17, "missing"),
          {:inbound, %{"id" => 4, "result" => %{}}},
          {:note,
           "deleting an unknown id still fences it: later notifications for it are skipped and server requests auto-cancelled"},
          {:inbound,
           %{
             "method" => "item/agentMessage/delta",
             "params" => %{
               "threadId" => "missing",
               "turnId" => "turn-9",
               "itemId" => "item-9",
               "delta" => "stale"
             }
           }},
          {:inbound,
           %{
             "id" => 90,
             "method" => "item/commandExecution/requestApproval",
             "params" => %{
               "threadId" => "missing",
               "turnId" => "turn-9",
               "itemId" => "item-10",
               "command" => "mix test",
               "cwd" => "/tmp/project"
             }
           }},
          {:note, "the live session is unaffected"},
          prompt_step(20)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_delete_unknown_session_fences_later_notifications",
        steps
      )

    assert %{tag: :skip, skipped: true} = Enum.at(transcript, 9).result

    assert %{
             tag: :skip_and_write,
             writes: [%{"id" => 90, "result" => %{"decision" => "cancel"}}]
           } = Enum.at(transcript, 10).result

    assert %{"id" => 5, "method" => "turn/start", "params" => %{"threadId" => "thread-abc"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "session_cancel_interrupts_active_turn" do
    steps =
      active_turn_steps() ++
        [
          {:outbound,
           %{"method" => "session/cancel", "params" => %{"sessionId" => "thread-abc"}}},
          {:inbound, %{"id" => 5, "result" => %{}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_cancel_interrupts_active_turn", steps)

    assert %{
             tag: :ok,
             writes: [
               %{
                 "id" => 5,
                 "method" => "turn/interrupt",
                 "params" => %{"threadId" => "thread-abc", "turnId" => "turn-1"}
               }
             ]
           } = Enum.at(transcript, 8).result

    assert %{tag: :skip} = CodexGolden.last_result(transcript)
  end

  test "session_cancel_without_active_turn" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{"method" => "session/cancel", "params" => %{"sessionId" => "thread-abc"}}},
          {:note, "an explicit turnId bypasses the session's own turn tracking"},
          {:outbound,
           %{
             "method" => "session/cancel",
             "params" => %{"sessionId" => "thread-abc", "turnId" => "turn-x"}
           }},
          {:outbound, %{"method" => "session/cancel", "params" => %{"sessionId" => "missing"}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_cancel_without_active_turn", steps)

    assert %{tag: :error, error: "No active Codex turn for session"} =
             Enum.at(transcript, 6).result

    assert %{
             tag: :ok,
             writes: [%{"method" => "turn/interrupt", "params" => %{"turnId" => "turn-x"}}]
           } =
             Enum.at(transcript, 8).result

    assert %{tag: :error, error: "Unknown Codex session: missing"} =
             CodexGolden.last_result(transcript)
  end

  test "session_cancel_blank_turn_id_uses_active_turn" do
    steps =
      active_turn_steps() ++
        [
          {:note, "a blank or non-string turnId falls back to the session's own turn"},
          {:outbound,
           %{
             "method" => "session/cancel",
             "params" => %{"sessionId" => "thread-abc", "turnId" => ""}
           }},
          {:outbound,
           %{
             "method" => "session/cancel",
             "params" => %{"sessionId" => "thread-abc", "turnId" => 42}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_cancel_blank_turn_id_uses_active_turn", steps)

    assert [
             %{"id" => 5, "method" => "turn/interrupt", "params" => %{"turnId" => "turn-1"}},
             %{"id" => 6, "method" => "turn/interrupt", "params" => %{"turnId" => "turn-1"}}
           ] = Enum.drop(CodexGolden.writes(transcript), 5)
  end

  test "session_delete_closed_session_archives_again" do
    steps =
      session_steps() ++
        [
          close_step(17),
          {:note,
           "a closed session is unknown to delete: archive only, no unsubscribe, no error"},
          delete_step(18, "thread-abc"),
          delete_step(19, "thread-abc")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_delete_closed_session_archives_again", steps)

    assert [
             %{"id" => 4, "method" => "thread/unsubscribe"},
             %{
               "id" => 5,
               "method" => "thread/archive",
               "params" => %{"threadId" => "thread-abc"}
             },
             %{"id" => 6, "method" => "thread/archive", "params" => %{"threadId" => "thread-abc"}}
           ] = Enum.drop(CodexGolden.writes(transcript), 4)

    assert %{tag: :messages_and_write, messages: []} = CodexGolden.last_result(transcript)
  end

  test "session_new_reopens_closed_session_id" do
    steps =
      session_steps() ++
        [
          close_step(17),
          {:note,
           "while closed, notifications for the id are fenced and server requests are auto-cancelled"},
          {:inbound, agent_message_delta("turn-0", "item-0", "stale")},
          {:inbound, command_approval_request(90, "turn-0", "item-1")},
          {:note, "a thread/start reply for the same id lifts the fence"},
          session_new_step(18),
          thread_start_reply(5),
          prompt_step(20),
          {:inbound,
           %{"id" => 6, "result" => %{"turn" => %{"id" => "turn-1", "status" => "inProgress"}}}},
          {:inbound, agent_message_delta("turn-1", "item-2", "fresh")},
          {:inbound, command_approval_request(91, "turn-1", "item-3")}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_reopens_closed_session_id", steps)

    assert %{tag: :skip, skipped: true} = Enum.at(transcript, 8).result

    assert %{writes: [%{"id" => 90, "result" => %{"decision" => "cancel"}}]} =
             Enum.at(transcript, 9).result

    assert %{
             messages: [
               %{
                 "method" => "session/update",
                 "params" => %{
                   "sessionId" => "thread-abc",
                   "update" => %{
                     "sessionUpdate" => "agent_message_chunk",
                     "content" => %{"text" => "fresh"}
                   }
                 }
               }
             ]
           } = Enum.at(transcript, 15).result

    assert %{
             tag: :messages,
             messages: [
               %{
                 "id" => "codex-permission-<1>",
                 "method" => "session/request_permission",
                 "params" => %{"sessionId" => "thread-abc"}
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  # -- session/set_mode, set_model, set_config_option ---------------------------

  test "session_set_mode_emits_current_mode_update" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/set_mode",
             "id" => 15,
             "params" => %{"sessionId" => "thread-abc", "modeId" => "agent-full-access"}
           }},
          {:note, "the new mode shapes the next turn and is inherited by later sessions"},
          prompt_step(20),
          session_new_step(21)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_set_mode_emits_current_mode_update", steps)

    assert %{
             tag: :messages_and_reply,
             reply: %{},
             messages: [
               %{
                 "params" => %{
                   "update" => %{
                     "sessionUpdate" => "current_mode_update",
                     "currentModeId" => "agent-full-access"
                   }
                 }
               }
             ]
           } = Enum.at(transcript, 6).result

    assert [
             %{
               "method" => "turn/start",
               "params" => %{
                 "sandboxPolicy" => %{"type" => "dangerFullAccess"},
                 "approvalPolicy" => "never"
               }
             },
             %{"method" => "thread/start", "params" => %{"sandbox" => "danger-full-access"}}
           ] = Enum.drop(CodexGolden.writes(transcript), 4)
  end

  test "session_set_model_replies_with_catalog_selection" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/set_model",
             "id" => 15,
             "params" => %{"sessionId" => "thread-abc", "modelId" => "codex-mini/high"}
           }},
          prompt_step(20)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_set_model_replies_with_catalog_selection", steps)

    assert %{
             tag: :reply,
             reply: %{"models" => %{"currentModelId" => "codex-mini/high"}, "configOptions" => _}
           } =
             Enum.at(transcript, 6).result

    assert %{
             "method" => "turn/start",
             "params" => %{"model" => "gpt-5-codex", "effort" => "high"}
           } =
             List.last(CodexGolden.writes(transcript))
  end

  test "session_set_config_option_reasoning_effort" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/set_config_option",
             "id" => 15,
             "params" => %{
               "sessionId" => "thread-abc",
               "configId" => "reasoning_effort",
               "value" => "low"
             }
           }},
          prompt_step(20)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_set_config_option_reasoning_effort", steps)

    # Pinned as-is: the effort option and the next turn switch to "low", but
    # currentModelId keeps the model_id computed at thread/start ("gpt-5/high")
    # because the recomputation short-circuits on the session's existing model_id.
    assert %{
             tag: :reply,
             reply: %{"models" => %{"currentModelId" => "gpt-5/high"}, "configOptions" => options}
           } =
             Enum.at(transcript, 6).result

    assert %{"currentValue" => "low"} = Enum.find(options, &(&1["id"] == "reasoning_effort"))

    assert %{"method" => "turn/start", "params" => %{"model" => "gpt-5", "effort" => "low"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "session_set_mode_rejects_unknown_mode" do
    steps =
      session_steps(mode_id: "read-only") ++
        [
          {:outbound,
           %{
             "method" => "session/set_mode",
             "id" => 15,
             "params" => %{"sessionId" => "thread-abc", "modeId" => "yolo"}
           }},
          {:outbound,
           %{
             "method" => "session/set_mode",
             "id" => 16,
             "params" => %{"sessionId" => "missing", "modeId" => "agent"}
           }},
          {:outbound,
           %{"method" => "session/set_mode", "id" => 17, "params" => %{"modeId" => "agent"}}},
          {:note,
           "a missing modeId is normalized to the default mode, not the session's current one"},
          {:outbound,
           %{
             "method" => "session/set_mode",
             "id" => 18,
             "params" => %{"sessionId" => "thread-abc"}
           }},
          prompt_step(20)
        ]

    transcript = CodexGolden.assert_golden(@area, "session_set_mode_rejects_unknown_mode", steps)

    assert transcript |> Enum.slice(6, 3) |> Enum.map(& &1.result) == [
             %{tag: :error, error: "Unsupported Codex mode: \"yolo\""},
             %{tag: :error, error: "Unknown Codex session: missing"},
             %{tag: :error, error: "sessionId is required"}
           ]

    assert %{
             tag: :messages_and_reply,
             messages: [%{"params" => %{"update" => %{"currentModeId" => "agent"}}}]
           } = Enum.at(transcript, 10).result

    assert %{"method" => "turn/start", "params" => %{"approvalsReviewer" => "auto_review"}} =
             List.last(CodexGolden.writes(transcript))
  end

  # -- authenticate --------------------------------------------------------------

  test "authenticate_chat_gpt_login_start" do
    steps =
      connected_steps() ++
        [
          authenticate_step(30, "chat-gpt"),
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "type" => "chatgpt",
               "authUrl" => "https://auth.openai.com/login",
               "loginId" => "login-1"
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "authenticate_chat_gpt_login_start", steps)

    assert %{"id" => 3, "method" => "account/login/start", "params" => %{"type" => "chatgpt"}} =
             List.last(CodexGolden.writes(transcript))

    assert [
             %{
               "id" => 30,
               "result" => %{
                 "_meta" => %{"ex_mcp" => %{"codex" => %{"auth" => %{"authUrl" => _}}}}
               }
             }
           ] =
             CodexGolden.messages(transcript)
  end

  test "authenticate_reply_without_url_returns_empty_result" do
    steps =
      connected_steps() ++
        [
          authenticate_step(30, "chat-gpt"),
          {:inbound, %{"id" => 3, "result" => %{"type" => "chatgpt"}}}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_reply_without_url_returns_empty_result",
        steps
      )

    assert [%{"id" => 30, "result" => %{}}] = CodexGolden.messages(transcript)
  end

  test "authenticate_chatgpt_alias_and_fallback_keys" do
    steps =
      connected_steps() ++
        [
          {:note, "methodId 'chatgpt' is a legacy alias; 'provider' and 'id' are fallback keys"},
          authenticate_step(30, "chatgpt"),
          {:outbound,
           %{"method" => "authenticate", "id" => 31, "params" => %{"provider" => "chat-gpt"}}},
          {:outbound,
           %{"method" => "authenticate", "id" => 32, "params" => %{"id" => "chat-gpt"}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_chatgpt_alias_and_fallback_keys", steps)

    assert [
             %{"id" => 3, "method" => "account/login/start", "params" => %{"type" => "chatgpt"}},
             %{"id" => 4, "method" => "account/login/start", "params" => %{"type" => "chatgpt"}},
             %{"id" => 5, "method" => "account/login/start", "params" => %{"type" => "chatgpt"}}
           ] = Enum.drop(CodexGolden.writes(transcript), 3)
  end

  test "authenticate_method_id_wins_over_provider_alias" do
    steps =
      connected_steps() ++
        [
          {:note,
           "methodId is consulted before provider, and provider before id; the losing keys are never validated"},
          {:outbound,
           %{
             "method" => "authenticate",
             "id" => 30,
             "params" => %{
               "methodId" => "chat-gpt",
               "provider" => "gateway",
               "id" => "not-a-method"
             }
           }},
          {:outbound,
           %{
             "method" => "authenticate",
             "id" => 31,
             "params" => %{"provider" => "chat-gpt", "id" => "not-a-method"}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_method_id_wins_over_provider_alias", steps)

    assert [
             %{"id" => 3, "method" => "account/login/start", "params" => %{"type" => "chatgpt"}},
             %{"id" => 4, "method" => "account/login/start", "params" => %{"type" => "chatgpt"}}
           ] = Enum.drop(CodexGolden.writes(transcript), 3)

    assert CodexGolden.messages(transcript) == []
  end

  test "authenticate_device_code_completes_via_url_elicitation" do
    steps =
      device_code_steps() ++
        [
          {:outbound, &accept_auth_elicitation/1},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => true}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_device_code_completes_via_url_elicitation",
        steps
      )

    assert [
             %{
               "id" => "codex-auth-elicitation-<1>",
               "method" => "elicitation/create",
               "params" => %{
                 "requestId" => 31,
                 "mode" => "url",
                 "elicitationId" => "login-1",
                 "url" => "https://chatgpt.com/device"
               }
             },
             %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "login-1"}},
             %{"id" => 31, "result" => %{}}
           ] = CodexGolden.messages(transcript)

    assert %{tag: :ok, skipped: true} = Enum.at(transcript, 7).result
  end

  test "authenticate_device_code_declined_cancels_login" do
    steps =
      device_code_steps() ++
        [
          {:outbound,
           fn transcript ->
             %{"id" => auth_elicitation_id(transcript), "result" => %{"action" => "decline"}}
           end}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_device_code_declined_cancels_login", steps)

    assert %{
             tag: :messages_and_write,
             messages: [
               %{
                 "id" => 31,
                 "error" => %{
                   "code" => -32_603,
                   "message" => "Codex authentication was cancelled"
                 }
               }
             ],
             writes: [
               %{
                 "id" => 4,
                 "method" => "account/login/cancel",
                 "params" => %{"loginId" => "login-1"}
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "authenticate_device_code_login_failure" do
    steps =
      device_code_steps() ++
        [
          {:outbound, &accept_auth_elicitation/1},
          {:note, "a completion for another login id is ignored"},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-other", "success" => true}
           }},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => false, "error" => "access denied"}
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "authenticate_device_code_login_failure", steps)

    assert %{tag: :skip} = Enum.at(transcript, 9).result

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "elicitation/complete"},
               %{"id" => 31, "error" => %{"code" => -32_603, "message" => "access denied"}}
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "authenticate_device_code_reply_without_url_errors" do
    steps =
      connected_steps() ++
        [
          acp_initialize_step(url_elicitation_capabilities()),
          authenticate_step(31, "chat-gpt-device-code"),
          {:inbound,
           %{"id" => 3, "result" => %{"type" => "chatgptDeviceCode", "loginId" => "login-1"}}},
          {:note,
           "a blank or non-string verificationUrl counts as missing (authUrl is not consulted)"},
          authenticate_step(32, "chat-gpt-device-code"),
          {:inbound,
           %{
             "id" => 4,
             "result" => %{
               "type" => "chatgptDeviceCode",
               "loginId" => "login-2",
               "verificationUrl" => "",
               "authUrl" => "https://auth.openai.com/device"
             }
           }},
          authenticate_step(33, "chat-gpt-device-code"),
          {:inbound,
           %{
             "id" => 5,
             "result" => %{
               "type" => "chatgptDeviceCode",
               "loginId" => "login-3",
               "verificationUrl" => 42
             }
           }},
          {:note,
           "a null result bypasses the device-code clause and is answered with an empty result"},
          authenticate_step(34, "chat-gpt-device-code"),
          {:inbound, %{"id" => 6, "result" => nil}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_device_code_reply_without_url_errors", steps)

    assert [
             %{"id" => 31, "error" => %{"code" => -32_603, "message" => message}},
             %{"id" => 32, "error" => %{"code" => -32_603, "message" => message}},
             %{"id" => 33, "error" => %{"code" => -32_603, "message" => message}},
             %{"id" => 34, "result" => %{}}
           ] =
             CodexGolden.messages(transcript)

    assert message == "Codex device-code authentication did not return a verification URL"
  end

  test "authenticate_device_code_blank_user_code_uses_generic_message" do
    steps =
      connected_steps() ++
        [
          acp_initialize_step(url_elicitation_capabilities()),
          {:note,
           "a blank or non-string userCode falls back to the generic sign-in message instead of an empty code"},
          authenticate_step(31, "chat-gpt-device-code"),
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "type" => "chatgptDeviceCode",
               "loginId" => "login-1",
               "verificationUrl" => "https://chatgpt.com/device",
               "userCode" => ""
             }
           }},
          {:outbound, &accept_auth_elicitation/1},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => true}
           }},
          authenticate_step(32, "chat-gpt-device-code"),
          {:inbound,
           %{
             "id" => 4,
             "result" => %{
               "type" => "chatgptDeviceCode",
               "loginId" => "login-2",
               "verificationUrl" => "https://chatgpt.com/device",
               "userCode" => 1234
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_device_code_blank_user_code_uses_generic_message",
        steps
      )

    assert [
             %{
               "id" => "codex-auth-elicitation-<1>",
               "method" => "elicitation/create",
               "params" => %{"elicitationId" => "login-1", "message" => message}
             },
             %{"method" => "elicitation/complete"},
             %{"id" => 31, "result" => %{}},
             %{
               "id" => "codex-auth-elicitation-<2>",
               "method" => "elicitation/create",
               "params" => %{"elicitationId" => "login-2", "message" => message}
             }
           ] = CodexGolden.messages(transcript)

    assert message == "Sign in to ChatGPT to continue."
  end

  test "authenticate_device_code_verification_url_wins_over_auth_url" do
    steps =
      connected_steps() ++
        [
          acp_initialize_step(url_elicitation_capabilities()),
          authenticate_step(31, "chat-gpt-device-code"),
          {:note, "when the reply carries both urls the elicitation opens the verificationUrl"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "type" => "chatgptDeviceCode",
               "loginId" => "login-1",
               "verificationUrl" => "https://chatgpt.com/device",
               "authUrl" => "https://auth.openai.com/device-fallback",
               "userCode" => "ABCD-EFGH"
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_device_code_verification_url_wins_over_auth_url",
        steps
      )

    assert [
             %{
               "method" => "elicitation/create",
               "params" => %{
                 "url" => "https://chatgpt.com/device",
                 "message" => "Sign in to ChatGPT and enter this code: ABCD-EFGH"
               }
             }
           ] = CodexGolden.messages(transcript)
  end

  test "authenticate_device_code_cancelled_and_error_responses" do
    steps =
      connected_steps() ++
        [
          acp_initialize_step(url_elicitation_capabilities()),
          authenticate_step(31, "chat-gpt-device-code"),
          device_code_reply(3, "login-1"),
          {:outbound,
           fn transcript ->
             %{"id" => auth_elicitation_id(transcript, 1), "result" => %{"action" => "cancel"}}
           end},
          {:note, "the cancelled login is gone: its completion is ignored"},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => true}
           }},
          {:note, "a client error response to the elicitation cancels the login the same way"},
          authenticate_step(32, "chat-gpt-device-code"),
          device_code_reply(5, "login-2"),
          {:outbound,
           fn transcript ->
             %{
               "id" => auth_elicitation_id(transcript, 2),
               "error" => %{"code" => -32_000, "message" => "elicitation failed"}
             }
           end},
          {:note,
           "accept with a non-map content is a cancellation too; a non-string loginId is stringified for the elicitation but echoed as-is to the app-server"},
          authenticate_step(33, "chat-gpt-device-code"),
          device_code_reply(7, 42),
          {:outbound,
           fn transcript ->
             %{
               "id" => auth_elicitation_id(transcript, 3),
               "result" => %{"action" => "accept", "content" => "not-a-map"}
             }
           end}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_device_code_cancelled_and_error_responses",
        steps
      )

    assert [
             %{"id" => 3, "method" => "account/login/start"},
             %{
               "id" => 4,
               "method" => "account/login/cancel",
               "params" => %{"loginId" => "login-1"}
             },
             %{"id" => 5, "method" => "account/login/start"},
             %{
               "id" => 6,
               "method" => "account/login/cancel",
               "params" => %{"loginId" => "login-2"}
             },
             %{"id" => 7, "method" => "account/login/start"},
             %{"id" => 8, "method" => "account/login/cancel", "params" => %{"loginId" => 42}}
           ] = Enum.drop(CodexGolden.writes(transcript), 3)

    assert transcript
           |> CodexGolden.messages()
           |> Enum.filter(&Map.has_key?(&1, "error"))
           |> Enum.map(&{&1["id"], &1["error"]["message"]}) == [
             {31, "Codex authentication was cancelled"},
             {32, "Codex authentication was cancelled"},
             {33, "Codex authentication was cancelled"}
           ]

    assert %{tag: :skip, skipped: true} = Enum.at(transcript, 9).result

    assert %{"params" => %{"elicitationId" => "42"}} =
             transcript
             |> CodexGolden.messages()
             |> Enum.filter(&(&1["method"] == "elicitation/create"))
             |> List.last()
  end

  test "authenticate_device_code_without_login_id_generates_one" do
    steps =
      connected_steps() ++
        [
          acp_initialize_step(url_elicitation_capabilities()),
          authenticate_step(31, "chat-gpt-device-code"),
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "type" => "chatgptDeviceCode",
               "verificationUrl" => "https://chatgpt.com/device"
             }
           }},
          {:note,
           "without a loginId the adapter mints one and completion without loginId matches it"},
          {:outbound, &accept_auth_elicitation/1},
          {:inbound, %{"method" => "account/login/completed", "params" => %{"success" => true}}}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_device_code_without_login_id_generates_one",
        steps
      )

    assert [
             %{
               "method" => "elicitation/create",
               "params" => %{
                 "elicitationId" => "codex-login-<1>",
                 "message" => "Sign in to ChatGPT to continue."
               }
             },
             %{
               "method" => "elicitation/complete",
               "params" => %{"elicitationId" => "codex-login-<1>"}
             },
             %{"id" => 31, "result" => %{}}
           ] = CodexGolden.messages(transcript)
  end

  test "authenticate_device_code_via_provider_alias" do
    steps =
      connected_steps() ++
        [
          acp_initialize_step(url_elicitation_capabilities()),
          {:note,
           "the resolved method id is what the pending request remembers: device-code chosen via the provider alias still runs the URL elicitation flow"},
          {:outbound,
           %{
             "method" => "authenticate",
             "id" => 31,
             "params" => %{"provider" => "chat-gpt-device-code"}
           }},
          device_code_reply(3, "login-1"),
          {:outbound, &accept_auth_elicitation/1},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => true}
           }},
          {:note, "the id alias behaves the same"},
          {:outbound,
           %{
             "method" => "authenticate",
             "id" => 32,
             "params" => %{"id" => "chat-gpt-device-code"}
           }},
          device_code_reply(4, "login-2"),
          {:outbound,
           fn transcript ->
             %{"id" => auth_elicitation_id(transcript, 2), "result" => %{"action" => "accept"}}
           end},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-2", "success" => true}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_device_code_via_provider_alias", steps)

    assert [
             %{
               "id" => 3,
               "method" => "account/login/start",
               "params" => %{"type" => "chatgptDeviceCode"}
             },
             %{
               "id" => 4,
               "method" => "account/login/start",
               "params" => %{"type" => "chatgptDeviceCode"}
             }
           ] = Enum.drop(CodexGolden.writes(transcript), 3)

    assert transcript
           |> CodexGolden.messages()
           |> Enum.map(&{&1["method"], &1["id"], get_in(&1, ["params", "elicitationId"])}) == [
             {"elicitation/create", "codex-auth-elicitation-<1>", "login-1"},
             {"elicitation/complete", nil, "login-1"},
             {nil, 31, nil},
             {"elicitation/create", "codex-auth-elicitation-<2>", "login-2"},
             {"elicitation/complete", nil, "login-2"},
             {nil, 32, nil}
           ]
  end

  test "authenticate_device_code_non_string_login_id_completes" do
    steps =
      connected_steps() ++
        [
          acp_initialize_step(url_elicitation_capabilities()),
          authenticate_step(31, "chat-gpt-device-code"),
          device_code_reply(3, 42),
          {:outbound, &accept_auth_elicitation/1},
          {:note,
           "completion matches the raw loginId, while both elicitation notifications use its stringified form"},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => 42, "success" => true}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_device_code_non_string_login_id_completes",
        steps
      )

    assert transcript
           |> CodexGolden.messages()
           |> Enum.map(&{&1["method"], get_in(&1, ["params", "elicitationId"]), &1["result"]}) ==
             [
               {"elicitation/create", "42", nil},
               {"elicitation/complete", "42", nil},
               {nil, nil, %{}}
             ]
  end

  test "authenticate_api_key_from_meta" do
    steps =
      connected_steps() ++
        [
          authenticate_step(30, "api-key", %{
            "_meta" => %{"api-key" => %{"apiKey" => "sk-request"}}
          }),
          {:inbound, %{"id" => 3, "result" => %{"type" => "apiKey"}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "authenticate_api_key_from_meta", steps)

    assert %{
             "method" => "account/login/start",
             "params" => %{"type" => "apiKey", "apiKey" => "sk-request"}
           } =
             List.last(CodexGolden.writes(transcript))

    assert [%{"id" => 30, "result" => %{}}] = CodexGolden.messages(transcript)
  end

  test "authenticate_api_key_from_init_env_prefers_codex_key" do
    steps =
      connected_steps(env: [{"OPENAI_API_KEY", "openai-key"}, {"CODEX_API_KEY", "codex-key"}]) ++
        [authenticate_step(30, "api-key")]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_api_key_from_init_env_prefers_codex_key",
        steps
      )

    assert %{"params" => %{"type" => "apiKey", "apiKey" => "codex-key"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "authenticate_api_key_falls_back_to_openai_env" do
    steps =
      connected_steps(env: [{"OPENAI_API_KEY", "openai-key"}]) ++
        [authenticate_step(30, "api-key")]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_api_key_falls_back_to_openai_env", steps)

    assert %{"params" => %{"type" => "apiKey", "apiKey" => "openai-key"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "authenticate_api_key_without_env_errors" do
    steps = connected_steps() ++ [authenticate_step(30, "api-key")]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_api_key_without_env_errors", steps)

    assert %{
             tag: :error,
             error:
               "CODEX_API_KEY or OPENAI_API_KEY must be supplied explicitly in adapter_opts[:env] before authenticate"
           } =
             CodexGolden.last_result(transcript)
  end

  test "authenticate_codex_api_key_from_init_env" do
    steps =
      connected_steps(env: [{"OPENAI_API_KEY", "openai-key"}, {"CODEX_API_KEY", "codex-key"}]) ++
        [authenticate_step(30, "codex-api-key")]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_codex_api_key_from_init_env", steps)

    assert %{"params" => %{"type" => "apiKey", "apiKey" => "codex-key"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "authenticate_codex_api_key_ignores_openai_env" do
    steps =
      connected_steps(env: [{"OPENAI_API_KEY", "openai-key"}]) ++
        [authenticate_step(30, "codex-api-key")]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_codex_api_key_ignores_openai_env", steps)

    assert %{
             tag: :error,
             error:
               "CODEX_API_KEY must be supplied explicitly in adapter_opts[:env] before authenticate"
           } =
             CodexGolden.last_result(transcript)
  end

  test "authenticate_openai_api_key_from_init_env" do
    steps =
      connected_steps(env: [{"OPENAI_API_KEY", "openai-key"}, {"CODEX_API_KEY", "codex-key"}]) ++
        [authenticate_step(30, "openai-api-key")]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_openai_api_key_from_init_env", steps)

    assert %{"params" => %{"type" => "apiKey", "apiKey" => "openai-key"}} =
             List.last(CodexGolden.writes(transcript))
  end

  test "authenticate_openai_api_key_ignores_codex_env" do
    steps =
      connected_steps(env: [{"CODEX_API_KEY", "codex-key"}]) ++
        [authenticate_step(30, "openai-api-key")]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_openai_api_key_ignores_codex_env", steps)

    assert %{
             tag: :error,
             error:
               "OPENAI_API_KEY must be supplied explicitly in adapter_opts[:env] before authenticate"
           } =
             CodexGolden.last_result(transcript)
  end

  test "authenticate_gateway_without_base_url_errors" do
    steps =
      connected_steps() ++
        [
          authenticate_step(30, "gateway", %{
            "_meta" => %{"gateway" => %{"providerName" => "Acme"}}
          }),
          authenticate_step(31, "gateway", %{
            "_meta" => %{"gateway" => "https://gateway.example.com"}
          }),
          authenticate_step(32, "gateway")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_gateway_without_base_url_errors", steps)

    assert Enum.map(Enum.drop(transcript, 4), & &1.result.error) == [
             "gateway auth requires baseUrl",
             "gateway auth requires adapter_opts[:gateway]",
             "gateway auth requires adapter_opts[:gateway]"
           ]
  end

  test "authenticate_gateway_defaults_provider_name" do
    steps =
      connected_steps() ++
        [
          authenticate_step(30, "gateway", %{
            "_meta" => %{"gateway" => %{"baseUrl" => "https://gateway.example.com/v1"}}
          }),
          session_new_step(10)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_gateway_defaults_provider_name", steps)

    assert %{"params" => %{"config" => %{"model_providers" => %{"custom-gateway" => provider}}}} =
             List.last(CodexGolden.writes(transcript))

    assert %{
             "name" => "User-provided gateway",
             "http_headers" => %{"X-Client-Feature-ID" => "codex"}
           } = provider
  end

  test "authenticate_gateway_blank_provider_name_uses_default" do
    steps =
      connected_steps() ++
        [
          {:note, "providerName \"\" (or a non-string) is treated as absent"},
          authenticate_step(30, "gateway", %{
            "_meta" => %{
              "gateway" => %{"baseUrl" => "https://gateway.example.com/v1", "providerName" => ""}
            }
          }),
          session_new_step(10)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_gateway_blank_provider_name_uses_default",
        steps
      )

    assert %{"params" => %{"config" => %{"model_providers" => %{"custom-gateway" => provider}}}} =
             List.last(CodexGolden.writes(transcript))

    assert %{"name" => "User-provided gateway", "base_url" => "https://gateway.example.com/v1"} =
             provider
  end

  test "authenticate_gateway_header_override_wins_over_default" do
    steps =
      connected_steps() ++
        [
          {:note, "a user-supplied X-Client-Feature-ID replaces the \"codex\" default"},
          authenticate_step(30, "gateway", %{
            "_meta" => %{
              "gateway" => %{
                "baseUrl" => "https://gateway.example.com/v1",
                "headers" => %{
                  "X-Client-Feature-ID" => "acme-ide",
                  "Authorization" => "Bearer gw-token"
                }
              }
            }
          }),
          session_new_step(10),
          thread_reply(3, "thread-1"),
          {:note, "a later gateway authenticate replaces the stored provider config entirely"},
          authenticate_step(31, "gateway", %{
            "_meta" => %{"gateway" => %{"baseUrl" => "https://other.example.com/v1"}}
          }),
          session_new_step(11)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_gateway_header_override_wins_over_default",
        steps
      )

    assert [
             %{"id" => 3, "method" => "thread/start", "params" => %{"config" => first}},
             %{"id" => 4, "method" => "thread/start", "params" => %{"config" => second}}
           ] = Enum.drop(CodexGolden.writes(transcript), 3)

    assert %{
             "base_url" => "https://gateway.example.com/v1",
             "http_headers" => %{
               "X-Client-Feature-ID" => "acme-ide",
               "Authorization" => "Bearer gw-token"
             }
           } = first["model_providers"]["custom-gateway"]

    assert %{
             "base_url" => "https://other.example.com/v1",
             "http_headers" => %{"X-Client-Feature-ID" => "codex"}
           } = second["model_providers"]["custom-gateway"]
  end

  test "authenticate_unknown_method_errors" do
    steps = connected_steps() ++ [authenticate_step(30, "magic-link")]

    transcript = CodexGolden.assert_golden(@area, "authenticate_unknown_method_errors", steps)

    assert %{tag: :error, error: "Unsupported Codex auth method: magic-link"} =
             CodexGolden.last_result(transcript)
  end

  test "authenticate_missing_method_id_errors" do
    steps =
      connected_steps() ++
        [
          {:outbound, %{"method" => "authenticate", "id" => 30, "params" => %{}}},
          {:outbound, %{"method" => "authenticate", "id" => 31}}
        ]

    transcript = CodexGolden.assert_golden(@area, "authenticate_missing_method_id_errors", steps)

    assert Enum.map(Enum.drop(transcript, 4), & &1.result.error) ==
             ["authenticate requires methodId", "authenticate requires methodId"]
  end

  test "authenticate_error_reply_forwards_error" do
    steps =
      connected_steps() ++
        [
          authenticate_step(30, "chat-gpt"),
          {:inbound,
           %{"id" => 3, "error" => %{"code" => -32_001, "message" => "login already in progress"}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_error_reply_forwards_error", steps)

    assert [
             %{
               "id" => 30,
               "error" => %{"code" => -32_001, "message" => "login already in progress"}
             }
           ] =
             CodexGolden.messages(transcript)
  end

  test "authenticate_device_code_login_failure_without_error_message" do
    steps =
      device_code_steps() ++
        [
          {:outbound, &accept_auth_elicitation/1},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => false}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_device_code_login_failure_without_error_message",
        steps
      )

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "elicitation/complete"},
               %{
                 "id" => 31,
                 "error" => %{"code" => -32_603, "message" => "Codex authentication failed"}
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "authenticate_device_code_login_completed_without_success_key" do
    steps =
      device_code_steps() ++
        [
          {:outbound, &accept_auth_elicitation/1},
          {:note,
           "only success == true completes a login: a completion without the key, or with a non-boolean value, is a failure"},
          {:inbound,
           %{"method" => "account/login/completed", "params" => %{"loginId" => "login-1"}}},
          authenticate_step(32, "chat-gpt-device-code"),
          device_code_reply(4, "login-2"),
          {:outbound,
           fn transcript ->
             %{"id" => auth_elicitation_id(transcript, 2), "result" => %{"action" => "accept"}}
           end},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-2", "success" => "true"}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_device_code_login_completed_without_success_key",
        steps
      )

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "login-1"}},
               %{
                 "id" => 31,
                 "error" => %{"code" => -32_603, "message" => "Codex authentication failed"}
               }
             ]
           } = Enum.at(transcript, 9).result

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "login-2"}},
               %{
                 "id" => 32,
                 "error" => %{"code" => -32_603, "message" => "Codex authentication failed"}
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "authenticate_login_completed_without_pending_auth_is_skipped" do
    steps =
      connected_steps() ++
        [
          {:note, "no login in progress: the completion is ignored"},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => true}
           }},
          acp_initialize_step(url_elicitation_capabilities()),
          authenticate_step(31, "chat-gpt-device-code"),
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "type" => "chatgptDeviceCode",
               "loginId" => "login-1",
               "verificationUrl" => "https://chatgpt.com/device"
             }
           }},
          {:outbound, &accept_auth_elicitation/1},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => true}
           }},
          {:note,
           "completion popped the elicitation: a late accept and a repeat are both ignored"},
          {:outbound, &accept_auth_elicitation/1},
          {:inbound,
           %{
             "method" => "account/login/completed",
             "params" => %{"loginId" => "login-1", "success" => false}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "authenticate_login_completed_without_pending_auth_is_skipped",
        steps
      )

    assert %{tag: :skip, skipped: true} = Enum.at(transcript, 5).result

    assert [
             %{"method" => "elicitation/create"},
             %{"method" => "elicitation/complete"},
             %{"id" => 31, "result" => %{}}
           ] = CodexGolden.messages(transcript)

    assert transcript |> Enum.take(-2) |> Enum.map(& &1.result) ==
             [%{tag: :ok, skipped: true}, %{tag: :skip, skipped: true}]
  end

  test "authenticate_chat_gpt_reply_with_verification_url" do
    steps =
      connected_steps() ++
        [
          authenticate_step(30, "chat-gpt"),
          {:note, "verificationUrl is passed through like authUrl, user code included"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "type" => "chatgpt",
               "verificationUrl" => "https://chatgpt.com/device",
               "userCode" => "ABCD-EFGH"
             }
           }},
          {:note, "a null result is accepted and answered with an empty result"},
          authenticate_step(31, "chat-gpt"),
          {:inbound, %{"id" => 4, "result" => nil}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "authenticate_chat_gpt_reply_with_verification_url", steps)

    assert [
             %{
               "id" => 30,
               "result" => %{
                 "_meta" => %{
                   "ex_mcp" => %{
                     "codex" => %{"auth" => %{"verificationUrl" => _, "userCode" => "ABCD-EFGH"}}
                   }
                 }
               }
             },
             %{"id" => 31, "result" => %{}}
           ] = CodexGolden.messages(transcript)
  end

  # -- logout --------------------------------------------------------------------

  test "logout_replies_and_writes_account_logout" do
    steps =
      connected_steps() ++
        [
          {:outbound, %{"method" => "logout", "id" => 40, "params" => %{}}},
          {:inbound, %{"id" => 3, "result" => %{}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "logout_replies_and_writes_account_logout", steps)

    assert %{
             tag: :reply_and_write,
             reply: %{},
             writes: [%{"id" => 3, "method" => "account/logout", "params" => %{}}]
           } =
             Enum.at(transcript, 4).result

    assert %{tag: :skip} = CodexGolden.last_result(transcript)
  end

  # -- error replies per pending request type -----------------------------------

  test "error_reply_thread_start_forwards_error" do
    steps =
      connected_steps() ++
        [
          session_new_step(10),
          {:inbound,
           %{"id" => 3, "error" => %{"code" => -32_000, "message" => "thread limit reached"}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "error_reply_thread_start_forwards_error", steps)

    assert [%{"id" => 10, "error" => %{"code" => -32_000, "message" => "thread limit reached"}}] =
             CodexGolden.messages(transcript)
  end

  test "error_reply_thread_resume_forwards_error" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 12,
             "params" => %{"sessionId" => "thread-gone", "cwd" => "/tmp/project"}
           }},
          {:note, "an error without a code is normalized to -1"},
          {:inbound, %{"id" => 3, "error" => %{"message" => "thread not found"}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "error_reply_thread_resume_forwards_error", steps)

    assert [%{"id" => 12, "error" => %{"code" => -1, "message" => "thread not found"}}] =
             CodexGolden.messages(transcript)
  end

  test "error_reply_thread_list_forwards_error" do
    steps =
      connected_steps() ++
        [
          {:outbound,
           %{"method" => "session/list", "id" => 14, "params" => %{"cwd" => "/tmp/project"}}},
          {:inbound,
           %{"id" => 3, "error" => %{"code" => -32_603, "message" => "storage offline"}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "error_reply_thread_list_forwards_error", steps)

    assert [%{"id" => 14, "error" => %{"code" => -32_603, "message" => "storage offline"}}] =
             CodexGolden.messages(transcript)
  end

  test "error_replies_for_silent_request_types_are_skipped" do
    steps =
      active_turn_steps() ++
        [
          {:note, "delete emits interrupt (5), unsubscribe (6), archive (7); logout is 8"},
          delete_step(17, "thread-abc"),
          {:outbound, %{"method" => "logout", "id" => 40, "params" => %{}}},
          {:inbound, %{"id" => 5, "error" => %{"code" => -32_000, "message" => "no such turn"}}},
          {:inbound,
           %{"id" => 6, "error" => %{"code" => -32_000, "message" => "not subscribed"}}},
          {:inbound,
           %{"id" => 7, "error" => %{"code" => -32_000, "message" => "archive failed"}}},
          {:inbound, %{"id" => 8, "error" => %{"code" => -32_000, "message" => "logout failed"}}}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "error_replies_for_silent_request_types_are_skipped",
        steps
      )

    assert transcript |> Enum.take(-4) |> Enum.map(& &1.result) |> Enum.uniq() == [
             %{tag: :skip, skipped: true}
           ]
  end

  # -- step builders ---------------------------------------------------------------

  # Explicit init for new scenarios: workspace root plus a fixed cwd so nothing
  # falls back to File.cwd!/0. The three exemplar scenarios keep the harness
  # defaults (no cwd) to leave their fixtures untouched.
  defp init_step(extra \\ []) do
    {:init, CodexGolden.default_init_opts() ++ [cwd: "/tmp/project"] ++ extra}
  end

  defp handshake_steps do
    [
      :post_connect,
      {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}},
      {:inbound, %{"id" => 2, "result" => %{"data" => catalog_models(), "nextCursor" => nil}}}
    ]
  end

  # init + handshake; next app-server id is 3.
  defp connected_steps(extra \\ []), do: [init_step(extra) | handshake_steps()]

  # connected + session "thread-abc" opened with ACP id 10; next app-server id is 4.
  defp session_steps(extra \\ []) do
    connected_steps(extra) ++ [session_new_step(10), thread_start_reply(3)]
  end

  # session + prompt 20 running as turn "turn-1" (turn/start id 4); next app-server id is 5.
  defp active_turn_steps do
    session_steps() ++
      [
        prompt_step(20),
        {:inbound,
         %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1", "status" => "inProgress"}}}}
      ]
  end

  # connected + ACP initialize with URL elicitation + device-code login started
  # (id 3, ACP id 31) and answered with a verification URL; the adapter has
  # emitted elicitation/create with a generated id.
  defp device_code_steps do
    connected_steps() ++
      [
        acp_initialize_step(url_elicitation_capabilities()),
        authenticate_step(31, "chat-gpt-device-code"),
        {:inbound,
         %{
           "id" => 3,
           "result" => %{
             "type" => "chatgptDeviceCode",
             "loginId" => "login-1",
             "verificationUrl" => "https://chatgpt.com/device",
             "userCode" => "ABCD-EFGH"
           }
         }}
      ]
  end

  defp session_new_step(acp_id, params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/new",
       "id" => acp_id,
       "params" => Map.merge(%{"cwd" => "/tmp/project", "mcpServers" => []}, params)
     }}
  end

  defp thread_start_reply(id, extra \\ %{}) do
    {:inbound,
     %{
       "id" => id,
       "result" =>
         Map.merge(
           %{
             "model" => "gpt-5",
             "thread" => %{
               "id" => "thread-abc",
               "cwd" => "/tmp/project",
               "updatedAt" => 1_700_000_000
             }
           },
           extra
         )
     }}
  end

  # Like thread_start_reply/2 but for a named thread, so one scenario can
  # open several distinct sessions.
  defp thread_reply(id, thread_id, extra \\ %{}) do
    {:inbound,
     %{
       "id" => id,
       "result" =>
         Map.merge(
           %{
             "model" => "gpt-5",
             "thread" => %{
               "id" => thread_id,
               "cwd" => "/tmp/project",
               "updatedAt" => 1_700_000_000
             }
           },
           extra
         )
     }}
  end

  defp prompt_step(acp_id) do
    {:outbound,
     %{
       "method" => "session/prompt",
       "id" => acp_id,
       "params" => %{
         "sessionId" => "thread-abc",
         "prompt" => [%{"type" => "text", "text" => "hi"}]
       }
     }}
  end

  defp close_step(acp_id) do
    {:outbound,
     %{"method" => "session/close", "id" => acp_id, "params" => %{"sessionId" => "thread-abc"}}}
  end

  defp delete_step(acp_id, session_id) do
    {:outbound,
     %{"method" => "session/delete", "id" => acp_id, "params" => %{"sessionId" => session_id}}}
  end

  defp authenticate_step(acp_id, method_id, params \\ %{}) do
    {:outbound,
     %{
       "method" => "authenticate",
       "id" => acp_id,
       "params" => Map.merge(%{"methodId" => method_id}, params)
     }}
  end

  defp acp_initialize_step(client_capabilities) do
    {:outbound,
     %{
       "method" => "initialize",
       "id" => 0,
       "params" => %{"protocolVersion" => 1, "clientCapabilities" => client_capabilities}
     }}
  end

  defp url_elicitation_capabilities do
    %{
      "fs" => %{"readTextFile" => true, "writeTextFile" => true},
      "elicitation" => %{"url" => %{}}
    }
  end

  # Device-code login/start reply (app-server id `id`) that opens a URL
  # elicitation for `login_id`.
  defp device_code_reply(id, login_id) do
    {:inbound,
     %{
       "id" => id,
       "result" => %{
         "type" => "chatgptDeviceCode",
         "loginId" => login_id,
         "verificationUrl" => "https://chatgpt.com/device",
         "userCode" => "ABCD-EFGH"
       }
     }}
  end

  # App-server notification streaming agent text for session "thread-abc".
  defp agent_message_delta(turn_id, item_id, delta) do
    %{
      "method" => "item/agentMessage/delta",
      "params" => %{
        "threadId" => "thread-abc",
        "turnId" => turn_id,
        "itemId" => item_id,
        "delta" => delta
      }
    }
  end

  # App-server request (id `codex_id`) asking to run a command in "thread-abc".
  defp command_approval_request(codex_id, turn_id, item_id) do
    %{
      "id" => codex_id,
      "method" => "item/commandExecution/requestApproval",
      "params" => %{
        "threadId" => "thread-abc",
        "turnId" => turn_id,
        "itemId" => item_id,
        "command" => "mix test",
        "cwd" => "/tmp/project"
      }
    }
  end

  defp accept_auth_elicitation(transcript) do
    %{"id" => auth_elicitation_id(transcript), "result" => %{"action" => "accept"}}
  end

  # The `n`-th (1-based) auth elicitation id the adapter has minted so far.
  defp auth_elicitation_id(transcript, n \\ 1) do
    transcript
    |> CodexGolden.generated_ids()
    |> Enum.filter(&String.starts_with?(&1, "codex-auth-elicitation-"))
    |> Enum.at(n - 1)
  end

  # Uses the Codex app-server v2 `model/list` shape, where each entry of
  # `supportedReasoningEfforts` is `%{"reasoningEffort" => ..., "description" => ...}`.
  # The older helper in test/ex_mcp/acp/adapters/codex_test.exs uses
  # `%{"value" => ..., "name" => ..., "description" => ...}` instead; the adapter
  # accepts both, so neither shape is wrong.
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
          %{"reasoningEffort" => "medium", "description" => "Balanced"},
          %{"reasoningEffort" => "high", "description" => "Deep"}
        ]
      },
      %{
        "id" => "gpt-5",
        "model" => "gpt-5",
        "displayName" => "GPT-5",
        "description" => "General purpose model",
        "hidden" => false,
        "defaultReasoningEffort" => "high",
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "low", "description" => "Quick"},
          %{"reasoningEffort" => "high", "description" => "Deep"}
        ]
      }
    ]
  end
end
