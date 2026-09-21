defmodule ExMCP.ACP.Adapters.Pi.RpcGoldenTest do
  @moduledoc """
  Characterization gate for the Pi ACP adapter's native RPC envelopes (area
  P1 of `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Pi adapter restructuring" /
  "Characterization gate": RPC messages for new, load, resume, fork, close,
  delete, and prompt flows).

  Each test drives `ExMCP.ACP.Adapters.Pi` through `ExMCP.Test.PiGolden` and
  compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/pi/rpc/`. The fixtures pin:

    * the four `new_session` / `get_state` / `get_available_models` /
      `get_commands` requests written by `session/new`, the `cwd` fallback
      to the init option, and the absolute-`cwd` validation errors;
    * the `switch_session` (+ `get_messages` for load only) + catalog
      requests written by `session/load` and `session/resume`, the session
      file resolution through the session map or a JSONL scan, the
      `Unknown sessionId` error, and the replay-before-response ordering;
    * `session/fork` being skipped, `session/close` and `session/delete`
      replying `{}` without any RPC write (and a closed session's pending
      prompt never being answered);
    * the `prompt` request with `msg-N` correlation ids, prompt content
      conversion (text, resource links, embedded resources, audio, images
      with data-URL stripping, `streamingBehavior`), and the prompt error
      responses including the auth-required mapping;
    * the id-less `abort` notification written by `session/cancel`;
    * `initialize` skip, `authenticate` reply, removed `pi/` and
      `_ex_mcp.pi/` extension methods erroring, unknown methods skipped;
    * unparseable or unclassifiable inbound lines being skipped, and an
      untracked `get_state` response silently re-pointing the session.

  Control-group completion and failure ordering are characterized by the
  control_groups area; stream events, prompt scheduling, configuration,
  slash commands and session-store safety by their own areas. The final
  `File.cwd!/0` fallback taken when neither the request nor the init
  options carry a `cwd` depends on the test runner's working directory and
  is deliberately not characterized.

  Mutation check (2026-09-20): dropping `get_commands` from the
  `session/new` batch in `do_start_session_new/3` fails
  `session_new_writes_four_control_requests_and_completes`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `PI_GOLDEN=update mix test <this file>[:line]`; that run rewrites the
  fixture and fails on purpose, so review the diff and re-run without the
  variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.PiGolden
  alias ExMCP.Test.PiGolden.Flows

  @area "rpc"

  describe "session/new" do
    test "session_new_writes_four_control_requests_and_completes" do
      transcript =
        PiGolden.assert_golden(
          @area,
          "session_new_writes_four_control_requests_and_completes",
          Flows.open_session(1)
        )

      assert ["new_session", "get_state", "get_available_models", "get_commands"] =
               transcript |> PiGolden.writes() |> Enum.map(& &1["type"])

      assert %{messages: [%{"id" => 1, "result" => %{"sessionId" => "pi-session"}}, _commands]} =
               PiGolden.last_result(transcript)
    end

    test "session_new_without_cwd_falls_back_to_init_cwd" do
      steps = [
        {:outbound, %{"method" => "session/new", "id" => 1, "params" => %{}}},
        {:respond, "new_session", %{}},
        {:respond, "get_state", Flows.state_data(%{"cwd" => nil, "sessionFile" => nil})},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}},
        {:list_sessions, %{}}
      ]

      transcript =
        PiGolden.assert_golden(@area, "session_new_without_cwd_falls_back_to_init_cwd", steps)

      assert %{tag: :ok, writes: [%{"type" => "new_session"} | _]} = Enum.at(transcript, 1).result
    end

    test "session_new_rejects_relative_and_missing_cwd" do
      steps = [
        Flows.session_new(1, %{"cwd" => "relative/dir"}),
        Flows.session_new(2, %{"cwd" => 42})
      ]

      transcript =
        PiGolden.assert_golden(@area, "session_new_rejects_relative_and_missing_cwd", steps)

      assert [
               %{error: "cwd must be an absolute path: relative/dir"},
               %{error: "cwd is required"}
             ] =
               transcript
               |> Enum.map(& &1.result)
               |> Enum.filter(&Map.has_key?(&1, :error))
               |> Enum.map(&Map.delete(&1, :tag))
    end
  end

  describe "session/load and session/resume" do
    test "session_load_writes_switch_replay_and_catalog_requests" do
      steps = [
        Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
        Flows.session_load(1, "mapped")
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_load_writes_switch_replay_and_catalog_requests",
          steps
        )

      assert [
               %{"type" => "switch_session", "sessionPath" => "<sandbox>/sessions/mapped.jsonl"},
               %{"type" => "get_messages"},
               %{"type" => "get_state"},
               %{"type" => "get_available_models"},
               %{"type" => "get_commands"}
             ] = PiGolden.writes(transcript)
    end

    test "session_load_replays_history_before_the_response" do
      steps = [
        Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
        Flows.session_load(1, "mapped"),
        {:respond, "switch_session", %{}},
        {:respond, "get_state", Flows.state_data(%{"sessionId" => "mapped"})},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}},
        {:respond, "get_messages",
         %{
           "messages" => [
             %{"role" => "user", "content" => "Hello"},
             %{"role" => "assistant", "content" => [%{"type" => "text", "text" => "Hi"}]},
             %{
               "role" => "toolResult",
               "toolName" => "bash",
               "toolCallId" => "tc-1",
               "content" => [%{"type" => "text", "text" => "ok"}]
             },
             %{"role" => "toolResult", "isError" => true, "content" => "boom"},
             %{"role" => "user", "content" => [%{"type" => "image"}]},
             %{"role" => "system", "content" => "ignored"}
           ]
         }}
      ]

      transcript =
        PiGolden.assert_golden(@area, "session_load_replays_history_before_the_response", steps)

      assert [
               "user_message_chunk",
               "agent_message_chunk",
               "tool_call",
               "tool_call_update",
               "tool_call",
               "tool_call_update",
               nil,
               "available_commands_update"
             ] =
               transcript
               |> PiGolden.last_result()
               |> Map.fetch!(:messages)
               |> Enum.map(&get_in(&1, ["params", "update", "sessionUpdate"]))

      assert ["tool-<1>"] = PiGolden.generated_ids(transcript) |> Enum.filter(&(&1 =~ "tool"))
    end

    test "session_load_unknown_session_errors" do
      steps = [
        Flows.session_load(1, "ghost"),
        {:outbound, %{"method" => "session/load", "id" => 2, "params" => %{"cwd" => "<sandbox>"}}}
      ]

      transcript = PiGolden.assert_golden(@area, "session_load_unknown_session_errors", steps)

      assert [%{error: "Unknown sessionId: ghost"}, %{error: "Unknown sessionId: "}] =
               Enum.map(Enum.drop(transcript, 1), & &1.result)
    end

    test "session_load_scans_session_dir_when_unmapped" do
      steps = [
        Flows.session_jsonl("scan-1"),
        Flows.session_load(1, "scan-1"),
        {:respond, "switch_session", %{}},
        {:respond, "get_messages", %{"messages" => []}},
        {:respond, "get_state", Flows.state_data(%{"sessionId" => "scan-1"})},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}}
      ]

      transcript =
        PiGolden.assert_golden(@area, "session_load_scans_session_dir_when_unmapped", steps)

      assert %{"sessionPath" => "<sandbox>/sessions/scan-1.jsonl"} =
               hd(PiGolden.writes(transcript))
    end

    test "session_load_and_resume_reject_relative_cwd" do
      steps = [
        Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
        Flows.session_load(1, "mapped", %{"cwd" => "rel"}),
        Flows.session_resume(2, "mapped", %{"cwd" => "rel"})
      ]

      transcript =
        PiGolden.assert_golden(@area, "session_load_and_resume_reject_relative_cwd", steps)

      assert [%{error: "cwd must be an absolute path: rel"}, %{error: _}] =
               Enum.map(Enum.drop(transcript, 2), & &1.result)
    end

    test "session_resume_writes_switch_and_catalog_without_replay" do
      steps = [
        Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
        Flows.session_resume(1, "mapped"),
        {:respond, "switch_session", %{}},
        {:respond, "get_state", Flows.state_data(%{"sessionId" => "mapped"})},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_resume_writes_switch_and_catalog_without_replay",
          steps
        )

      assert ["switch_session", "get_state", "get_available_models", "get_commands"] =
               transcript |> PiGolden.writes() |> Enum.map(& &1["type"])

      assert %{messages: [%{"id" => 1, "result" => %{"sessionId" => "mapped"}}, _]} =
               PiGolden.last_result(transcript)
    end

    test "session_resume_unknown_session_errors" do
      transcript =
        PiGolden.assert_golden(@area, "session_resume_unknown_session_errors", [
          Flows.session_resume(1, "ghost")
        ])

      assert %{error: "Unknown sessionId: ghost"} = PiGolden.last_result(transcript)
    end
  end

  describe "session/fork, session/close, session/delete" do
    test "session_fork_is_skipped" do
      transcript =
        PiGolden.assert_golden(@area, "session_fork_is_skipped", [
          {:outbound,
           %{
             "method" => "session/fork",
             "id" => 1,
             "params" => %{"sessionId" => "pi-session", "cwd" => "<sandbox>/project"}
           }}
        ])

      assert %{tag: :ok, skipped: true} = PiGolden.last_result(transcript)
    end

    test "session_close_replies_empty_without_rpc_writes" do
      steps =
        Flows.open_session(1) ++
          [
            {:outbound,
             %{"method" => "session/close", "id" => 2, "params" => %{"sessionId" => "other"}}},
            {:outbound,
             %{"method" => "session/close", "id" => 3, "params" => %{"sessionId" => "pi-session"}}},
            {:outbound, %{"method" => "session/close", "id" => 4, "params" => %{}}}
          ]

      transcript =
        PiGolden.assert_golden(@area, "session_close_replies_empty_without_rpc_writes", steps)

      assert [%{tag: :reply, reply: %{}}, %{tag: :reply, reply: %{}}, %{tag: :reply, reply: %{}}] =
               Enum.map(Enum.take(transcript, -3), & &1.result)
    end

    test "session_close_drops_pending_prompt_without_response" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "hello"),
            {:outbound,
             %{"method" => "session/close", "id" => 3, "params" => %{"sessionId" => "pi-session"}}},
            {:note, "The closed session's prompt is never answered"},
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_close_drops_pending_prompt_without_response",
          steps
        )

      assert %{tag: :skip} = PiGolden.last_result(transcript)
    end

    test "session_delete_replies_empty_and_removes_map_entry" do
      steps =
        Flows.open_session(1) ++
          [
            {:read_file, "<sandbox>/session-map.json"},
            {:outbound,
             %{
               "method" => "session/delete",
               "id" => 2,
               "params" => %{"sessionId" => "pi-session"}
             }},
            {:read_file, "<sandbox>/session-map.json"}
          ]

      transcript =
        PiGolden.assert_golden(@area, "session_delete_replies_empty_and_removes_map_entry", steps)

      assert %{content: %{"sessions" => %{}, "version" => 1}} = PiGolden.last_result(transcript)
    end

    test "session_delete_unknown_session_replies_and_materializes_map" do
      steps = [
        {:read_file, "<sandbox>/session-map.json"},
        {:outbound,
         %{"method" => "session/delete", "id" => 1, "params" => %{"sessionId" => "ghost"}}},
        {:read_file, "<sandbox>/session-map.json"},
        {:outbound, %{"method" => "session/delete", "id" => 2, "params" => %{}}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_delete_unknown_session_replies_and_materializes_map",
          steps
        )

      assert [%{exists: false}, %{tag: :reply}, %{exists: true}, %{tag: :reply}] =
               Enum.map(Enum.drop(transcript, 1), & &1.result)
    end
  end

  describe "session/prompt" do
    test "session_prompt_writes_prompt_requests_with_incrementing_msg_ids" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "one"),
            Flows.agent_settled(),
            Flows.prompt(3, "two"),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_prompt_writes_prompt_requests_with_incrementing_msg_ids",
          steps
        )

      assert [
               %{"id" => "msg-1", "message" => "one", "type" => "prompt"},
               %{"id" => "msg-2", "message" => "two", "type" => "prompt"}
             ] = transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "prompt"))
    end

    test "session_prompt_converts_content_blocks_into_one_message" do
      blocks = [
        %{"type" => "text", "text" => "Look at "},
        %{"type" => "resource_link", "uri" => "file:///tmp/example.ex", "name" => "example.ex"},
        %{
          "type" => "resource",
          "resource" => %{
            "uri" => "file:///tmp/notes.md",
            "mimeType" => "text/markdown",
            "text" => "# notes"
          }
        },
        %{
          "type" => "resource",
          "resource" => %{"uri" => "file:///tmp/blob.bin", "blob" => "AAAA"}
        },
        %{"type" => "resource", "resource" => %{"uri" => "file:///tmp/bare"}},
        %{"type" => "resource", "resource" => %{}},
        %{"type" => "audio", "mimeType" => "audio/wav", "data" => "abc"},
        %{"type" => "mystery", "text" => "dropped"},
        %{"type" => "text", "text" => " please"}
      ]

      steps = Flows.open_session(1) ++ [Flows.prompt(2, blocks)]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_prompt_converts_content_blocks_into_one_message",
          steps
        )

      assert %{writes: [%{"message" => message}]} = PiGolden.last_result(transcript)
      assert message =~ "[Context] file:///tmp/example.ex"
      assert message =~ "[Embedded Context] file:///tmp/notes.md (text/markdown)\n# notes"

      assert message =~
               "[Embedded Context] file:///tmp/blob.bin (application/octet-stream, 4 bytes)"

      assert message =~ "[Embedded Context] (unknown)"
      assert message =~ "[Audio] (audio/wav, 3 bytes) not supported by Pi"
      refute message =~ "dropped"
    end

    test "session_prompt_passes_images_and_streaming_behavior" do
      blocks = [
        %{"type" => "text", "text" => "What is this?"},
        %{"type" => "image", "mimeType" => "image/png", "data" => "data:image/png;base64,abc123"},
        %{"type" => "image", "data" => "data:image/jpeg;base64,def456"},
        %{"type" => "image", "data" => "rawbytes"},
        %{"type" => "image", "uri" => "file:///tmp/pic.png"}
      ]

      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, blocks, %{"streamingBehavior" => "steer"}),
            Flows.agent_settled(),
            Flows.prompt(3, %{"content" => [%{"type" => "text", "text" => "wrapped"}]}),
            Flows.agent_settled(),
            Flows.prompt(4, 42)
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_prompt_passes_images_and_streaming_behavior",
          steps
        )

      assert [
               %{"images" => [_, _, _, %{"uri" => _}], "streamingBehavior" => "steer"},
               %{"message" => "wrapped"},
               %{"message" => ""}
             ] = transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "prompt"))
    end

    test "session_prompt_error_response_answers_the_acp_request" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "hello"),
            {:respond_error, "prompt", "model failed"},
            Flows.prompt(3, "again"),
            {:respond_error, "prompt", nil},
            Flows.prompt(4, "once more"),
            {:respond_error, "prompt", "401 Unauthorized"},
            Flows.prompt(5, "and again"),
            {:respond, "prompt", %{}},
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_prompt_error_response_answers_the_acp_request",
          steps
        )

      assert [
               %{"error" => %{"code" => -32_603, "message" => "model failed"}, "id" => 2},
               %{"error" => %{"code" => -32_603, "message" => "Pi prompt failed"}, "id" => 3},
               %{"error" => %{"code" => -32_000, "data" => %{"authMethods" => [_]}}, "id" => 4},
               %{"id" => 5, "result" => %{"stopReason" => "end_turn"}}
             ] =
               transcript
               |> PiGolden.messages()
               |> Enum.filter(&Map.has_key?(&1, "id"))
               |> Enum.drop(1)
    end

    test "session_cancel_writes_an_id_less_abort_notification" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.cancel(),
            Flows.prompt(2, "hello"),
            Flows.cancel(%{}),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_cancel_writes_an_id_less_abort_notification",
          steps
        )

      assert [%{"type" => "abort"}, %{"type" => "abort"}] =
               transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "abort"))

      assert %{messages: [%{"id" => 2, "result" => %{"stopReason" => "cancelled"}}]} =
               PiGolden.last_result(transcript)
    end
  end

  describe "other ACP methods and inbound classification" do
    test "initialize_is_skipped_and_authenticate_replies_empty" do
      steps = [
        {:outbound,
         %{"method" => "initialize", "id" => 1, "params" => %{"protocolVersion" => 1}}},
        {:outbound,
         %{
           "method" => "authenticate",
           "id" => 2,
           "params" => %{"methodId" => "pi_terminal_login"}
         }}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "initialize_is_skipped_and_authenticate_replies_empty",
          steps
        )

      assert [%{tag: :ok, skipped: true}, %{tag: :reply, reply: %{}}] =
               Enum.map(Enum.drop(transcript, 1), & &1.result)
    end

    test "removed_extension_methods_error_and_unknown_methods_skip" do
      steps = [
        {:outbound,
         %{"method" => "_ex_mcp.pi/steer", "id" => 1, "params" => %{"message" => "x"}}},
        {:outbound, %{"method" => "pi/set_model", "params" => %{}}},
        {:outbound, %{"method" => "session/list", "id" => 2, "params" => %{}}},
        {:outbound, %{"method" => "session/set_permissions", "id" => 3, "params" => %{}}},
        {:outbound, %{"id" => 4, "result" => %{"outcome" => %{"outcome" => "selected"}}}},
        {:outbound, %{"id" => 5, "error" => %{"code" => -1, "message" => "nope"}}},
        {:outbound, %{"method" => "$/cancel_request", "params" => %{"requestId" => 6}}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "removed_extension_methods_error_and_unknown_methods_skip",
          steps
        )

      assert [
               %{
                 error:
                   "Pi extension methods were removed; use ACP session methods or slash commands"
               },
               %{error: _},
               %{tag: :ok, skipped: true},
               %{tag: :ok, skipped: true},
               %{tag: :ok, skipped: true},
               %{tag: :ok, skipped: true},
               %{tag: :ok, skipped: true}
             ] = Enum.map(Enum.drop(transcript, 1), & &1.result)
    end

    test "unclassifiable_inbound_lines_are_skipped" do
      steps = [
        {:inbound_raw, ""},
        {:inbound_raw, "   \n"},
        {:inbound_raw, "not json"},
        {:inbound_raw, ~s({"foo":1})},
        {:inbound_raw, "[1,2]"},
        {:inbound_raw, ~s({"type":"response","id":7,"success":true})},
        {:inbound_raw, ~s({"type":"response","id":"pi-99","success":true,"data":{}})},
        {:inbound_raw, ~s({"type":"response","id":"pi-99","success":false,"error":"late"})},
        {:inbound_raw, ~s({"type":42})}
      ]

      transcript =
        PiGolden.assert_golden(@area, "unclassifiable_inbound_lines_are_skipped", steps)

      assert Enum.all?(Enum.drop(transcript, 1), &match?(%{tag: :skip}, &1.result))
    end

    test "untracked_get_state_response_repoints_the_session" do
      steps =
        Flows.open_session(1) ++
          [
            {:inbound,
             %{
               "type" => "response",
               "id" => "stray",
               "command" => "get_state",
               "success" => true,
               "data" => %{
                 "sessionId" => "renamed",
                 "sessionFile" => "<sandbox>/sessions/renamed.jsonl",
                 "thinkingLevel" => "high"
               }
             }},
            {:inbound,
             %{
               "type" => "response",
               "id" => "stray",
               "command" => "get_state",
               "success" => true,
               "data" => "x"
             }},
            {:outbound,
             %{"method" => "session/prompt", "id" => 2, "params" => %{"prompt" => "hi"}}},
            Flows.agent_settled(),
            Flows.set_mode(3, "low", %{"sessionId" => nil})
          ]

      transcript =
        PiGolden.assert_golden(@area, "untracked_get_state_response_repoints_the_session", steps)

      assert %{"id" => 2, "result" => %{"_meta" => %{"ex_mcp" => %{"sessionId" => "renamed"}}}} =
               transcript |> PiGolden.messages() |> Enum.find(&(&1["id"] == 2))
    end
  end
end
