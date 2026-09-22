defmodule ExMCP.ACP.Adapters.ClaudeSDK.LifecycleGoldenTest do
  @moduledoc """
  Characterization gate for the Claude SDK adapter's session lifecycle
  (`docs/POST_1_0_MAINTENANCE_PLAN.md`, "Claude adapter characterization
  gate": `post_connect`, session new/load/resume/list/close/delete, and
  `fork_session/2`).

  Each test drives `ExMCP.ACP.Adapters.ClaudeSDK` through
  `ExMCP.Test.ClaudeGolden` and compares the recorded transcript against a
  committed fixture under `test/fixtures/acp/claude/lifecycle/`. The
  fixtures pin:

    * the SDK `initialize` control request `post_connect/1` writes for the
      default options and for every option it forwards, and the ACP updates
      the matching control response emits (none before a session exists);
    * `session/new` minting `claude_sdk_<n>`, honouring an explicit
      `sessionId`, honouring the adapter's `session_id`/`resume` options,
      adopting `params.cwd`, and returning the dynamic modes and config
      options;
    * `session/load` replaying a persisted transcript as
      `user_message_chunk` / `agent_message_chunk` / tool updates before its
      reply, and failing on a missing, sidechain, or malformed session;
    * `session/resume` re-adopting an id without touching the store;
    * `session/list` and `list_sessions/2` reading the disk store: metadata
      extraction (`customTitle`, `firstPrompt`, `gitBranch`, `tag`,
      `createdAt`, `fileSize`), sorting by `lastModified`, pagination by
      `limit`/`offset`/`cursor`, and every skip rule (sidechain, blank
      summary, non-UUID name, empty file, missing cwd);
    * `session/close` and `session/delete` clearing session identity,
      cancelling the active and queued prompts, and the error replies for a
      missing session, a non-UUID id, and absent params;
    * `fork_session/2` copying the transcript under a new UUID with the
      session ids rewritten, and its failure modes;
    * the message-specific fork point at `_meta.jetbrains.air.fork`: an
      assistant turn addressed by its Anthropic message id, a user message
      addressed by its transcript uuid, the `:segment:<n>` suffix older
      JetBrains AIR builds append, every entry sharing one message id being
      kept, the fork point itself being *included*, an assistant uuid
      shadowed by that entry's message id, an unknown id failing with
      `{:invalid_params, _}` instead of silently copying everything, and a
      blank id or an unsupported `version` falling back to the whole-session
      copy.

  Prompt scheduling is characterized by the faults area, stream events by
  the session_updates area, and the mode/config catalogs by the catalog
  area; this file only pins the lifecycle envelopes that carry them.

  Mutation check (2026-09-21): in `claude_sdk.ex`, `cleanup_session/2` no
  longer clearing `session_id` (dropping that key from the struct update)
  fails `session_close_clears_session_identity`.

  Mutation check (2026-09-22): in `claude_sdk/session_store.ex`, making the
  fork point exclusive (`Enum.take(transcript, index)` in
  `take_through_fork_point/3`) fails the four fork-point scenarios that read
  the forked file back.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CLAUDE_GOLDEN=update mix test <this file>[:line]`; that run rewrites
  the fixture and fails on purpose, so review the diff and re-run without
  the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.ClaudeGolden
  alias ExMCP.Test.ClaudeGolden.Flows

  @area "lifecycle"

  # The session store reads mtime at whole-second granularity, so files written
  # in the same second tie on lastModified and the tie is broken by directory
  # listing order, which differs between filesystems. Scenarios that assert an
  # order pin these explicitly instead.
  @older_mtime 1_756_000_000
  @newer_mtime 1_757_000_000

  describe "post_connect" do
    test "post_connect_writes_default_initialize_control_request" do
      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "post_connect_writes_default_initialize_control_request",
          [
            :post_connect
          ]
        )

      assert [%{"type" => "control_request", "request" => %{"subtype" => "initialize"}}] =
               ClaudeGolden.writes(transcript)
    end

    test "post_connect_forwards_every_initialize_option" do
      steps = [
        {:init,
         append_system_prompt: "Be brief.",
         system_prompt: "You are a test agent.",
         plan_mode_instructions: "Plan first.",
         title: "Golden",
         skills: ["docx"],
         agents: [%{"name" => "reviewer"}],
         prompt_suggestions: ["Refactor this"],
         agent_progress_summaries: true,
         forward_subagent_text: true},
        :post_connect
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "post_connect_forwards_every_initialize_option", steps)

      assert [%{"request" => request}] = ClaudeGolden.writes(transcript)
      assert request["systemPrompt"] == ["You are a test agent."]
      assert request["title"] == "Golden"
    end

    test "post_connect_drops_blank_initialize_options" do
      steps = [
        {:note, "Empty lists and nil options are omitted from the control request"},
        {:init, skills: [], agents: [], title: nil, system_prompt: []},
        :post_connect
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "post_connect_drops_blank_initialize_options", steps)

      assert [%{"request" => request}] = ClaudeGolden.writes(transcript)
      assert Map.keys(request) == ["subtype"]
    end

    test "initialize_response_without_session_emits_no_updates" do
      steps = [:post_connect, {:respond_control, "initialize", %{"commands" => ["review"]}}]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "initialize_response_without_session_emits_no_updates",
          steps
        )

      assert ClaudeGolden.messages(transcript) == []
    end

    test "initialize_response_after_session_emits_catalog_updates" do
      steps =
        [Flows.initialize(), :post_connect, Flows.session_new()] ++
          [
            {:respond_control, "initialize",
             %{
               "commands" => ["review", %{"name" => "plan", "description" => "Plan it"}],
               "models" => [%{"value" => "sonnet", "displayName" => "Sonnet"}],
               "agents" => [%{"name" => "reviewer", "description" => "Reviews code"}]
             }}
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "initialize_response_after_session_emits_catalog_updates",
          steps
        )

      assert ClaudeGolden.update_types(transcript) == [
               "available_commands_update",
               "config_option_update",
               "current_mode_update"
             ]
    end

    test "unknown_control_response_is_ignored" do
      steps = [
        :post_connect,
        {:note, "A control response whose request_id was never pending changes nothing"},
        {:inbound,
         %{
           "type" => "control_response",
           "response" => %{
             "subtype" => "success",
             "request_id" => "ex_mcp_initialize_999999",
             "response" => %{"commands" => ["ignored"]}
           }
         }}
      ]

      transcript = ClaudeGolden.assert_golden(@area, "unknown_control_response_is_ignored", steps)

      assert %{tag: :skip} = ClaudeGolden.last_result(transcript)
    end
  end

  describe "session/new" do
    test "session_new_mints_generated_session_id" do
      transcript =
        ClaudeGolden.assert_golden(@area, "session_new_mints_generated_session_id", [
          Flows.session_new()
        ])

      assert %{tag: :reply, reply: %{"sessionId" => "claude_sdk_<1>"}} =
               ClaudeGolden.last_result(transcript)
    end

    test "session_new_honours_requested_session_id" do
      steps = [Flows.session_new("acp-new", %{"sessionId" => "host-session"})]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_new_honours_requested_session_id", steps)

      assert %{reply: %{"sessionId" => "host-session"}} = ClaudeGolden.last_result(transcript)
    end

    test "session_new_prefers_adapter_session_option" do
      steps = [
        {:init, session_id: "opt-session"},
        Flows.session_new("acp-new", %{"sessionId" => "host-session"})
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_new_prefers_adapter_session_option", steps)

      assert %{reply: %{"sessionId" => "opt-session"}} = ClaudeGolden.last_result(transcript)
    end

    test "session_new_prefers_adapter_resume_option" do
      steps = [{:init, resume: "resumed-session"}, Flows.session_new()]

      ClaudeGolden.assert_golden(@area, "session_new_prefers_adapter_resume_option", steps)
    end

    test "session_new_without_cwd_keeps_adapter_cwd" do
      steps = [
        {:outbound,
         %{"jsonrpc" => "2.0", "id" => "acp-new", "method" => "session/new", "params" => %{}}}
      ]

      ClaudeGolden.assert_golden(@area, "session_new_without_cwd_keeps_adapter_cwd", steps)
    end

    test "session_new_reply_carries_modes_and_config_options" do
      steps = [
        {:init, model: "opus", permission_mode: :plan, effort: "high"},
        Flows.session_new()
      ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "session_new_reply_carries_modes_and_config_options",
          steps
        )

      assert %{reply: %{"modes" => %{"currentModeId" => "plan"}, "configOptions" => options}} =
               ClaudeGolden.last_result(transcript)

      assert Enum.map(options, & &1["id"]) == ["mode", "model", "effort"]
    end

    test "session_new_twice_keeps_the_first_session_id" do
      steps = [Flows.session_new("acp-new"), Flows.session_new("acp-new-2")]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_new_twice_keeps_the_first_session_id", steps)

      assert [%{"sessionId" => first}, %{"sessionId" => second}] =
               transcript |> Enum.map(& &1.result[:reply]) |> Enum.reject(&is_nil/1)

      assert first == second
    end
  end

  describe "session/resume" do
    test "session_resume_adopts_the_requested_id" do
      steps = [Flows.session_resume("acp-resume", Flows.session_uuid(1))]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_resume_adopts_the_requested_id", steps)

      assert %{reply: %{"sessionId" => session_id}} = ClaudeGolden.last_result(transcript)
      assert session_id == Flows.session_uuid(1)
    end

    test "session_resume_without_session_id_mints_one" do
      steps = [
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-resume",
           "method" => "session/resume",
           "params" => %{"cwd" => Flows.cwd()}
         }}
      ]

      ClaudeGolden.assert_golden(@area, "session_resume_without_session_id_mints_one", steps)
    end
  end

  describe "session/load" do
    test "session_load_replays_persisted_transcript" do
      session = Flows.session_uuid(1)

      steps = [
        Flows.session_jsonl(session, [
          %{
            "type" => "user",
            "uuid" => "user-1",
            "cwd" => Flows.cwd(),
            "timestamp" => "2026-01-01T00:00:00Z",
            "message" => %{"role" => "user", "content" => "hello"}
          },
          %{
            "type" => "assistant",
            "uuid" => "assistant-1",
            "timestamp" => "2026-01-01T00:00:01Z",
            "message" => %{
              "role" => "assistant",
              "content" => [%{"type" => "text", "text" => "hi there"}]
            }
          }
        ]),
        Flows.session_load("acp-load", session)
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_load_replays_persisted_transcript", steps)

      assert ClaudeGolden.update_types(transcript) == [
               "user_message_chunk",
               "agent_message_chunk"
             ]
    end

    test "session_load_replays_tool_use_and_result" do
      session = Flows.session_uuid(1)

      steps = [
        Flows.session_jsonl(session, [
          %{
            "type" => "user",
            "uuid" => "user-1",
            "cwd" => Flows.cwd(),
            "message" => %{"role" => "user", "content" => "run it"}
          },
          %{
            "type" => "assistant",
            "uuid" => "assistant-1",
            "message" => %{
              "role" => "assistant",
              "content" => [
                %{
                  "type" => "tool_use",
                  "id" => "toolu_1",
                  "name" => "Bash",
                  "input" => %{"command" => "ls"}
                }
              ]
            }
          },
          %{
            "type" => "user",
            "uuid" => "user-2",
            "message" => %{
              "role" => "user",
              "content" => [
                %{"type" => "tool_result", "tool_use_id" => "toolu_1", "content" => "a\nb"}
              ]
            }
          }
        ]),
        Flows.session_load("acp-load", session)
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_load_replays_tool_use_and_result", steps)

      assert ClaudeGolden.update_types(transcript) == [
               "user_message_chunk",
               "tool_call",
               "tool_call_update",
               "tool_call_update"
             ]
    end

    test "session_load_replays_image_blocks" do
      session = Flows.session_uuid(1)

      steps = [
        Flows.session_jsonl(session, [
          %{
            "type" => "user",
            "uuid" => "user-1",
            "cwd" => Flows.cwd(),
            "message" => %{
              "role" => "user",
              "content" => [
                %{"type" => "text", "text" => "look"},
                %{
                  "type" => "image",
                  "source" => %{"type" => "base64", "media_type" => "image/jpeg", "data" => "AAA"}
                },
                %{"type" => "image", "source" => %{"data" => "BBB"}}
              ]
            }
          }
        ]),
        Flows.session_load("acp-load", session)
      ]

      ClaudeGolden.assert_golden(@area, "session_load_replays_image_blocks", steps)
    end

    test "session_load_missing_session_errors" do
      steps = [Flows.session_load("acp-load", Flows.session_uuid(2))]

      transcript = ClaudeGolden.assert_golden(@area, "session_load_missing_session_errors", steps)

      assert %{tag: :error} = ClaudeGolden.last_result(transcript)
    end

    test "session_load_non_uuid_session_errors" do
      steps = [Flows.session_load("acp-load", "not-a-uuid")]

      ClaudeGolden.assert_golden(@area, "session_load_non_uuid_session_errors", steps)
    end

    test "session_load_sidechain_session_errors" do
      session = Flows.session_uuid(1)

      steps = [
        Flows.session_jsonl(session, [
          %{
            "type" => "user",
            "isSidechain" => true,
            "cwd" => Flows.cwd(),
            "message" => %{"role" => "user", "content" => "sub-agent"}
          }
        ]),
        Flows.session_load("acp-load", session)
      ]

      ClaudeGolden.assert_golden(@area, "session_load_sidechain_session_errors", steps)
    end

    test "session_load_skips_non_json_lines" do
      session = Flows.session_uuid(1)

      steps = [
        {:write_file, Flows.session_path(session),
         ~s({"type":"user","cwd":"#{Flows.cwd()}","message":{"role":"user","content":"hi"}}\n) <>
           "not json\n"},
        Flows.session_load("acp-load", session)
      ]

      ClaudeGolden.assert_golden(@area, "session_load_skips_non_json_lines", steps)
    end
  end

  describe "session/list" do
    test "session_list_empty_store" do
      steps = [{:outbound, list_request()}]

      transcript = ClaudeGolden.assert_golden(@area, "session_list_empty_store", steps)

      assert %{reply: %{"sessions" => []}} = ClaudeGolden.last_result(transcript)
    end

    test "session_list_returns_metadata" do
      steps = [
        Flows.session_jsonl(
          Flows.session_uuid(1),
          Flows.summary_entries("summarize the repo",
            extra: [
              %{"type" => "tag", "tag" => "release"},
              %{"customTitle" => "Repo tour"}
            ]
          )
        ),
        {:outbound, list_request()}
      ]

      transcript = ClaudeGolden.assert_golden(@area, "session_list_returns_metadata", steps)

      assert %{reply: %{"sessions" => [session]}} = ClaudeGolden.last_result(transcript)
      assert session["title"] == "Repo tour"
    end

    test "session_list_sorts_by_last_modified" do
      steps = [
        Flows.session_jsonl(Flows.session_uuid(1), Flows.summary_entries("first"),
          mtime: @older_mtime
        ),
        Flows.session_jsonl(Flows.session_uuid(2), Flows.summary_entries("second"),
          mtime: @newer_mtime
        ),
        {:list_sessions, %{"cwd" => Flows.cwd()}}
      ]

      transcript = ClaudeGolden.assert_golden(@area, "session_list_sorts_by_last_modified", steps)

      assert %{reply: sessions} = ClaudeGolden.last_result(transcript)
      assert length(sessions) == 2

      # Newest first: the sort is what this scenario exists to pin.
      assert Enum.map(sessions, & &1["sessionId"]) ==
               [Flows.session_uuid(2), Flows.session_uuid(1)]
    end

    test "session_list_paginates_with_limit_and_offset" do
      steps = [
        Flows.session_jsonl(Flows.session_uuid(1), Flows.summary_entries("first"),
          mtime: @older_mtime
        ),
        Flows.session_jsonl(Flows.session_uuid(2), Flows.summary_entries("second"),
          mtime: @newer_mtime
        ),
        {:list_sessions, %{"cwd" => Flows.cwd(), "limit" => 1}},
        {:list_sessions, %{"cwd" => Flows.cwd(), "limit" => 1, "offset" => 1}},
        {:list_sessions, %{"cwd" => Flows.cwd(), "cursor" => "1"}},
        {:note, "A zero limit yields nothing at all"},
        {:list_sessions, %{"cwd" => Flows.cwd(), "limit" => 0}}
      ]

      ClaudeGolden.assert_golden(@area, "session_list_paginates_with_limit_and_offset", steps)
    end

    test "session_list_skips_unreadable_entries" do
      steps = [
        {:note, "Sidechain, blank-summary, non-UUID and empty files are all skipped"},
        Flows.session_jsonl(Flows.session_uuid(1), [
          %{
            "type" => "user",
            "isSidechain" => true,
            "cwd" => Flows.cwd(),
            "message" => %{"role" => "user", "content" => "sub"}
          }
        ]),
        Flows.session_jsonl(Flows.session_uuid(2), [%{"type" => "system", "cwd" => Flows.cwd()}]),
        {:write_file, "<sandbox>/claude/projects/<sandbox-key>-project/not-a-uuid.jsonl",
         Flows.summary_entries("ignored")},
        {:write_file, Flows.session_path(Flows.session_uuid(3)), ""},
        {:list_sessions, %{"cwd" => Flows.cwd()}}
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_list_skips_unreadable_entries", steps)

      assert %{reply: []} = ClaudeGolden.last_result(transcript)
    end

    test "session_list_without_cwd_scans_every_project" do
      steps = [
        Flows.session_jsonl(Flows.session_uuid(1), Flows.summary_entries("scoped")),
        {:list_sessions, %{}}
      ]

      ClaudeGolden.assert_golden(@area, "session_list_without_cwd_scans_every_project", steps)
    end

    test "session_list_drops_sessions_without_cwd" do
      steps = [
        {:note, "A store entry with no cwd cannot be mapped to an ACP SessionInfo"},
        {:write_file, Flows.session_path(Flows.session_uuid(1)),
         [%{"type" => "user", "message" => %{"role" => "user", "content" => "no cwd here"}}]},
        {:list_sessions, %{}}
      ]

      ClaudeGolden.assert_golden(@area, "session_list_drops_sessions_without_cwd", steps)
    end

    test "session_list_request_without_params" do
      steps = [
        Flows.session_jsonl(Flows.session_uuid(1), Flows.summary_entries("no params")),
        {:outbound, %{"jsonrpc" => "2.0", "id" => "acp-list", "method" => "session/list"}}
      ]

      ClaudeGolden.assert_golden(@area, "session_list_request_without_params", steps)
    end
  end

  describe "session/close" do
    test "session_close_clears_session_identity" do
      steps = [
        Flows.session_new(),
        Flows.session_close("acp-close"),
        {:note, "A second session/new now mints a fresh id"},
        Flows.session_new("acp-new-2")
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_close_clears_session_identity", steps)

      assert %{reply: %{"sessionId" => "claude_sdk_<2>"}} = ClaudeGolden.last_result(transcript)
    end

    test "session_close_cancels_the_active_prompt" do
      steps =
        [Flows.session_new(), Flows.prompt("acp-prompt", "hello")] ++
          [Flows.session_close("acp-close")]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_close_cancels_the_active_prompt", steps)

      assert %{
               tag: :messages_and_reply,
               messages: [%{"result" => %{"stopReason" => "cancelled"}}]
             } = ClaudeGolden.last_result(transcript)
    end

    test "session_close_for_another_session_keeps_the_prompt" do
      steps =
        [Flows.session_new(), Flows.prompt("acp-prompt", "hello")] ++
          [
            {:outbound,
             %{
               "jsonrpc" => "2.0",
               "id" => "acp-close",
               "method" => "session/close",
               "params" => %{"sessionId" => "other-session"}
             }}
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "session_close_for_another_session_keeps_the_prompt",
          steps
        )

      assert %{tag: :reply, reply: %{}} = ClaudeGolden.last_result(transcript)
    end

    test "session_close_without_params_replies_empty" do
      steps = [
        Flows.session_new(),
        {:outbound, %{"jsonrpc" => "2.0", "id" => "acp-close", "method" => "session/close"}}
      ]

      ClaudeGolden.assert_golden(@area, "session_close_without_params_replies_empty", steps)
    end
  end

  describe "session/delete" do
    test "session_delete_removes_the_transcript" do
      session = Flows.session_uuid(1)

      steps = [
        Flows.session_jsonl(session, Flows.summary_entries("delete me")),
        Flows.session_resume("acp-resume", session),
        delete_request("acp-delete", session),
        {:read_file, Flows.session_path(session)},
        {:list_sessions, %{"cwd" => Flows.cwd()}}
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "session_delete_removes_the_transcript", steps)

      assert %{reply: []} = ClaudeGolden.last_result(transcript)
    end

    test "session_delete_missing_session_errors" do
      steps = [delete_request("acp-delete", Flows.session_uuid(2))]

      ClaudeGolden.assert_golden(@area, "session_delete_missing_session_errors", steps)
    end

    test "session_delete_non_uuid_errors" do
      steps = [delete_request("acp-delete", "../escape")]

      ClaudeGolden.assert_golden(@area, "session_delete_non_uuid_errors", steps)
    end

    test "session_delete_without_session_id_errors" do
      steps = [
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-delete",
           "method" => "session/delete",
           "params" => %{"cwd" => Flows.cwd()}
         }}
      ]

      ClaudeGolden.assert_golden(@area, "session_delete_without_session_id_errors", steps)
    end

    test "session_delete_of_another_session_keeps_identity" do
      session = Flows.session_uuid(1)

      steps = [
        Flows.session_jsonl(session, Flows.summary_entries("delete me")),
        Flows.session_new(),
        delete_request("acp-delete", session),
        {:note, "The open session is untouched, so session/new still returns it"},
        Flows.session_new("acp-new-2")
      ]

      ClaudeGolden.assert_golden(@area, "session_delete_of_another_session_keeps_identity", steps)
    end
  end

  describe "fork_session/2" do
    test "fork_session_copies_transcript_under_a_new_uuid" do
      session = Flows.session_uuid(1)

      steps = [
        Flows.session_jsonl(session, [
          %{
            "type" => "user",
            "uuid" => "user-1",
            "sessionId" => session,
            "cwd" => Flows.cwd(),
            "timestamp" => "2026-01-01T00:00:00Z",
            "message" => %{"role" => "user", "content" => "fork me"}
          }
        ]),
        {:fork_session, %{"sessionId" => session, "cwd" => Flows.cwd()}},
        {:read_file, fn transcript -> Flows.session_path(forked_id(transcript)) end}
      ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "fork_session_copies_transcript_under_a_new_uuid",
          steps
        )

      assert %{exists: true, content: [%{"sessionId" => "<forked-1>"}]} =
               ClaudeGolden.last_result(transcript)
    end

    test "fork_session_uses_the_open_session_when_params_omit_it" do
      session = Flows.session_uuid(1)

      steps = [
        Flows.session_jsonl(session, Flows.summary_entries("fork me")),
        Flows.session_resume("acp-resume", session),
        {:fork_session, %{"cwd" => Flows.cwd()}}
      ]

      ClaudeGolden.assert_golden(
        @area,
        "fork_session_uses_the_open_session_when_params_omit_it",
        steps
      )
    end

    test "fork_session_missing_session_errors" do
      steps = [{:fork_session, %{"sessionId" => Flows.session_uuid(2), "cwd" => Flows.cwd()}}]

      transcript =
        ClaudeGolden.assert_golden(@area, "fork_session_missing_session_errors", steps)

      assert %{tag: :error} = ClaudeGolden.last_result(transcript)
    end

    test "fork_session_non_uuid_errors" do
      steps = [{:fork_session, %{"sessionId" => "nope", "cwd" => Flows.cwd()}}]

      ClaudeGolden.assert_golden(@area, "fork_session_non_uuid_errors", steps)
    end

    test "fork_session_without_any_session_errors" do
      steps = [{:fork_session, %{"cwd" => Flows.cwd()}}]

      ClaudeGolden.assert_golden(@area, "fork_session_without_any_session_errors", steps)
    end
  end

  describe "fork_session/2 fork points" do
    test "fork_session_at_an_assistant_message_id_keeps_that_message" do
      steps = forkable_session() ++ [fork_at("msg_first"), read_forked()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "fork_session_at_an_assistant_message_id_keeps_that_message",
          steps
        )

      assert %{content: content} = ClaudeGolden.last_result(transcript)
      assert Enum.map(content, & &1["uuid"]) == ["user-1", "assistant-1"]
    end

    test "fork_session_at_a_segment_suffixed_message_id_strips_the_suffix" do
      steps = forkable_session() ++ [fork_at("msg_first:segment:3"), read_forked()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "fork_session_at_a_segment_suffixed_message_id_strips_the_suffix",
          steps
        )

      assert %{content: content} = ClaudeGolden.last_result(transcript)
      assert Enum.map(content, & &1["uuid"]) == ["user-1", "assistant-1"]
    end

    test "fork_session_at_a_user_message_uuid_keeps_that_message" do
      steps = forkable_session() ++ [fork_at("user-2"), read_forked()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "fork_session_at_a_user_message_uuid_keeps_that_message",
          steps
        )

      assert %{content: content} = ClaudeGolden.last_result(transcript)
      assert Enum.map(content, & &1["uuid"]) == ["user-1", "assistant-1", "user-2"]
    end

    test "fork_session_keeps_every_entry_sharing_a_message_id" do
      session = Flows.session_uuid(1)

      entries = [
        transcript_user("user-1", "first"),
        transcript_assistant("assistant-1a", "msg_first", "answer "),
        transcript_assistant("assistant-1b", "msg_first", "one"),
        transcript_user("user-2", "second")
      ]

      steps = [Flows.session_jsonl(session, entries), fork_at("msg_first"), read_forked()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "fork_session_keeps_every_entry_sharing_a_message_id",
          steps
        )

      assert %{content: content} = ClaudeGolden.last_result(transcript)
      assert Enum.map(content, & &1["uuid"]) == ["user-1", "assistant-1a", "assistant-1b"]
    end

    test "fork_session_ignores_an_assistant_uuid_that_has_a_message_id" do
      steps = forkable_session() ++ [fork_at("assistant-1")]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "fork_session_ignores_an_assistant_uuid_that_has_a_message_id",
          steps
        )

      assert %{tag: :error, error: {:invalid_params, _message}} =
               ClaudeGolden.last_result(transcript)
    end

    test "fork_session_at_an_unknown_message_id_errors" do
      steps = forkable_session() ++ [fork_at("msg_nope")]

      transcript =
        ClaudeGolden.assert_golden(@area, "fork_session_at_an_unknown_message_id_errors", steps)

      assert %{tag: :error, error: {:invalid_params, _message}} =
               ClaudeGolden.last_result(transcript)
    end

    test "fork_session_with_a_blank_message_id_copies_everything" do
      steps = forkable_session() ++ [fork_at("   "), read_forked()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "fork_session_with_a_blank_message_id_copies_everything",
          steps
        )

      assert %{content: content} = ClaudeGolden.last_result(transcript)
      assert length(content) == 4
    end

    test "fork_session_with_an_unsupported_fork_version_copies_everything" do
      steps =
        forkable_session() ++ [fork_at("msg_first", %{"version" => 2}), read_forked()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "fork_session_with_an_unsupported_fork_version_copies_everything",
          steps
        )

      assert %{content: content} = ClaudeGolden.last_result(transcript)
      assert length(content) == 4
    end
  end

  defp list_request(params \\ %{}) do
    %{
      "jsonrpc" => "2.0",
      "id" => "acp-list",
      "method" => "session/list",
      "params" => Map.merge(%{"cwd" => Flows.cwd()}, params)
    }
  end

  defp delete_request(acp_id, session_id) do
    {:outbound,
     %{
       "jsonrpc" => "2.0",
       "id" => acp_id,
       "method" => "session/delete",
       "params" => %{"sessionId" => session_id, "cwd" => Flows.cwd()}
     }}
  end

  # A two-turn persisted transcript: user-1, assistant-1 ("msg_first"),
  # user-2, assistant-2 ("msg_second").
  defp forkable_session do
    session = Flows.session_uuid(1)

    entries = [
      transcript_user("user-1", "first"),
      transcript_assistant("assistant-1", "msg_first", "answer one"),
      transcript_user("user-2", "second"),
      transcript_assistant("assistant-2", "msg_second", "answer two")
    ]

    [Flows.session_jsonl(session, entries)]
  end

  defp transcript_user(uuid, text) do
    %{
      "type" => "user",
      "uuid" => uuid,
      "sessionId" => Flows.session_uuid(1),
      "cwd" => Flows.cwd(),
      "timestamp" => "2026-01-01T00:00:00Z",
      "message" => %{"role" => "user", "content" => text}
    }
  end

  defp transcript_assistant(uuid, message_id, text) do
    %{
      "type" => "assistant",
      "uuid" => uuid,
      "sessionId" => Flows.session_uuid(1),
      "cwd" => Flows.cwd(),
      "timestamp" => "2026-01-01T00:00:01Z",
      "message" => %{
        "id" => message_id,
        "role" => "assistant",
        "content" => [%{"type" => "text", "text" => text}]
      }
    }
  end

  # `session/fork` carrying the versioned fork point claude-agent-acp reads.
  defp fork_at(message_id, overrides \\ %{}) do
    fork = Map.merge(%{"version" => 1, "messageId" => message_id}, overrides)

    {:fork_session,
     %{
       "sessionId" => Flows.session_uuid(1),
       "cwd" => Flows.cwd(),
       "_meta" => %{"jetbrains" => %{"air" => %{"fork" => fork}}}
     }}
  end

  defp read_forked do
    {:read_file, fn transcript -> Flows.session_path(forked_id(transcript)) end}
  end

  defp forked_id(transcript) do
    transcript
    |> Enum.find_value(fn
      %{step: %{kind: :fork_session}, result: %{reply: %{"sessionId" => id}}} -> id
      _ -> nil
    end)
  end
end
