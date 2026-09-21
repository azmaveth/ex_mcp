defmodule ExMCP.ACP.Adapters.Pi.StreamEventsGoldenTest do
  @moduledoc """
  Characterization gate for the Pi ACP adapter's stream-event conversion
  (area P3 of `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Pi adapter restructuring"
  / "Characterization gate": assistant/thinking/tool/usage stream events).

  Each test drives `ExMCP.ACP.Adapters.Pi` through `ExMCP.Test.PiGolden` and
  compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/pi/stream_events/`. The fixtures pin:

    * `text_delta` becoming `agent_message_chunk` and accumulating into the
      prompt result's `_meta.ex_mcp.text`, `thinking_delta` becoming
      `agent_thought_chunk` without accumulating, and the `"default"`
      session id used before any session exists;
    * streamed tool calls (`toolcall_start` / `toolcall_delta` /
      `toolcall_end` / `tool_call`) in their `toolCall`, bare, and
      `partial.content[contentIndex]` shapes, `arguments` / `args` /
      `partialArgs` raw input extraction, the `pending` status, the first
      event emitting `tool_call` and later ones `tool_call_update`, and
      id-less events being skipped;
    * `tool_execution_start` / `_update` / `_end` becoming
      `tool_call` / `tool_call_update` with kind, status, locations
      resolved against the session cwd, text content from result content,
      details diff or bash output, and `failed` on `isError`;
    * the structured diff emitted for an `edit` tool whose file changed
      between start and end (with the unique-line number), and the plain
      text fallback for unchanged, missing, or failed edits;
    * `agent_end` usage conversion (last assistant message, cache and cost
      fields, zero defaults) and the usage reset when no prompt is pending;
    * auto-compaction / auto-retry status events, lifecycle events and
      unknown events being skipped;
    * extension UI requests (`select`, `confirm`, `input`, `editor`,
      `notify`, unknown) as `session/request_permission` requests or
      immediate `extension_ui_response` cancellations, and the ACP replies
      (selected, rejected, invalid choice, error, `$/cancel_request`);
    * batched port data being split on newlines with partial frames
      buffered until the rest arrives, and non-JSON lines ignored.

  Prompt settlement ordering with queued prompts is characterized by the
  prompt_flow area.

  Mutation check (2026-09-20): emitting `tool_call_update` instead of
  `tool_call` for a first-seen `tool_execution_start` fails
  `tool_execution_lifecycle_for_bash`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `PI_GOLDEN=update mix test <this file>[:line]`; that run rewrites the
  fixture and fails on purpose, so review the diff and re-run without the
  variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.PiGolden
  alias ExMCP.Test.PiGolden.Flows

  @area "stream_events"

  defp updates(transcript) do
    transcript
    |> PiGolden.messages()
    |> Enum.map(&get_in(&1, ["params", "update"]))
    |> Enum.reject(&is_nil/1)
  end

  defp active_prompt, do: Flows.open_session(1) ++ [Flows.prompt(2, "go")]

  # Extension-UI request ids continue the prompt counter (msg-1 is followed
  # by pi-extension-2), so replies look the id up in the raw transcript.
  defp permission_id(transcript, index \\ -1) do
    transcript
    |> PiGolden.messages()
    |> Enum.filter(&(&1["method"] == "session/request_permission"))
    |> Enum.at(index)
    |> Map.fetch!("id")
  end

  defp selected(option_id),
    do: %{"outcome" => %{"outcome" => "selected", "optionId" => option_id}}

  defp reply(result), do: {:outbound, fn t -> %{"id" => permission_id(t), "result" => result} end}

  describe "assistant text and thinking" do
    test "text_deltas_stream_and_accumulate_into_the_prompt_result" do
      steps =
        active_prompt() ++
          [
            Flows.text_delta("Hello"),
            Flows.text_delta(", "),
            Flows.thinking_delta("(not accumulated)"),
            Flows.text_delta("world"),
            Flows.agent_end(),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "text_deltas_stream_and_accumulate_into_the_prompt_result",
          steps
        )

      assert %{
               messages: [
                 %{
                   "id" => 2,
                   "result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Hello, world"}}}
                 }
               ]
             } = PiGolden.last_result(transcript)

      assert [
               "agent_message_chunk",
               "agent_message_chunk",
               "agent_thought_chunk",
               "agent_message_chunk"
             ] =
               transcript |> updates() |> Enum.map(& &1["sessionUpdate"]) |> Enum.drop(1)
    end

    test "deltas_before_any_session_use_the_default_session_id" do
      steps = [Flows.text_delta("early"), Flows.thinking_delta("hmm")]

      transcript =
        PiGolden.assert_golden(
          @area,
          "deltas_before_any_session_use_the_default_session_id",
          steps
        )

      assert [
               %{"params" => %{"sessionId" => "default"}},
               %{"params" => %{"sessionId" => "default"}}
             ] =
               PiGolden.messages(transcript)
    end

    test "text_accumulator_resets_between_prompts" do
      steps =
        active_prompt() ++
          [
            Flows.text_delta("first"),
            Flows.agent_settled(),
            Flows.prompt(3, "again"),
            Flows.text_delta("second"),
            Flows.agent_settled()
          ]

      transcript = PiGolden.assert_golden(@area, "text_accumulator_resets_between_prompts", steps)

      assert %{messages: [%{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "second"}}}}]} =
               PiGolden.last_result(transcript)
    end
  end

  describe "streamed tool calls" do
    test "toolcall_start_delta_end_with_tool_call_shape" do
      call = %{"id" => "tc-stream", "name" => "bash", "arguments" => %{"command" => "ls"}}

      steps =
        active_prompt() ++
          [
            Flows.message_update(%{"type" => "toolcall_start", "toolCall" => call}),
            Flows.message_update(%{
              "type" => "toolcall_delta",
              "toolCall" => %{call | "arguments" => %{"command" => "ls -la"}}
            }),
            Flows.message_update(%{"type" => "toolcall_end", "toolCall" => call}),
            Flows.tool_start("tc-stream", "bash", %{"command" => "ls -la"}),
            Flows.tool_end("tc-stream", Flows.text_result("a\nb"))
          ]

      transcript =
        PiGolden.assert_golden(@area, "toolcall_start_delta_end_with_tool_call_shape", steps)

      assert [
               %{"sessionUpdate" => "tool_call", "status" => "pending", "kind" => "other"},
               %{"sessionUpdate" => "tool_call_update", "status" => "pending"},
               %{"sessionUpdate" => "tool_call_update", "status" => "pending"},
               %{"sessionUpdate" => "tool_call_update", "status" => "in_progress"},
               %{"sessionUpdate" => "tool_call_update", "status" => "completed"}
             ] = transcript |> updates() |> Enum.drop(1)
    end

    test "toolcall_events_in_partial_content_and_bare_shapes" do
      steps =
        active_prompt() ++
          [
            Flows.message_update(%{
              "type" => "toolcall_start",
              "toolCall" => %{
                "id" => "tc-partial",
                "name" => "read",
                "args" => %{"path" => "lib/a.ex"}
              }
            }),
            Flows.message_update(%{
              "type" => "toolcall_delta",
              "id" => "tc-bare",
              "name" => "write"
            }),
            Flows.message_update(%{"type" => "tool_call", "id" => "tc-alias"}),
            Flows.message_update(%{
              "type" => "toolcall_start",
              "toolCall" => %{"name" => "no-id"}
            }),
            Flows.message_update(%{
              "type" => "toolcall_start",
              "toolCall" => %{"id" => "", "name" => "blank"}
            })
          ]

      transcript =
        PiGolden.assert_golden(@area, "toolcall_events_in_partial_content_and_bare_shapes", steps)

      assert [
               %{
                 "toolCallId" => "tc-partial",
                 "kind" => "read",
                 "locations" => [%{"path" => "<sandbox>/project/lib/a.ex"}]
               },
               %{"toolCallId" => "tc-bare", "kind" => "edit", "title" => "write"},
               %{"toolCallId" => "tc-alias", "kind" => "other", "title" => "tool"}
             ] = transcript |> updates() |> Enum.drop(1)

      assert [%{tag: :skip}, %{tag: :skip}] = Enum.map(Enum.take(transcript, -2), & &1.result)
    end

    test "toolcall_in_partial_content_is_addressed_by_content_index" do
      steps =
        active_prompt() ++
          [
            {:note,
             "a streamed tool call carried in partial.content is selected by contentIndex; before the fix Access raised on the list index"},
            Flows.message_update(%{
              "type" => "toolcall_start",
              "partial" => %{
                "content" => [
                  %{"id" => "tc-zero", "name" => "read", "args" => %{"path" => "lib/a.ex"}},
                  %{"id" => "tc-one", "name" => "write"}
                ]
              },
              "contentIndex" => 1
            }),
            {:note, "a missing contentIndex means the first entry"},
            Flows.message_update(%{
              "type" => "toolcall_start",
              "partial" => %{"content" => [%{"id" => "tc-default", "name" => "read"}]}
            }),
            {:note,
             "an out-of-range, negative, or non-integer index and a non-list content fall back to the event itself"},
            Flows.message_update(%{
              "type" => "toolcall_start",
              "partial" => %{"content" => [%{"id" => "tc-zero"}]},
              "contentIndex" => 7,
              "id" => "tc-out-of-range"
            }),
            Flows.message_update(%{
              "type" => "toolcall_start",
              "partial" => %{"content" => [%{"id" => "tc-zero"}]},
              "contentIndex" => -1,
              "id" => "tc-negative"
            }),
            Flows.message_update(%{
              "type" => "toolcall_start",
              "partial" => %{"content" => [%{"id" => "tc-zero"}]},
              "contentIndex" => "1",
              "id" => "tc-non-integer"
            }),
            Flows.message_update(%{
              "type" => "toolcall_start",
              "partial" => %{"content" => "not-a-list"},
              "id" => "tc-content-not-a-list"
            })
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "toolcall_in_partial_content_is_addressed_by_content_index",
          steps
        )

      assert [
               %{"toolCallId" => "tc-one", "title" => "write"},
               %{"toolCallId" => "tc-default", "title" => "read"},
               %{"toolCallId" => "tc-out-of-range"},
               %{"toolCallId" => "tc-negative"},
               %{"toolCallId" => "tc-non-integer"},
               %{"toolCallId" => "tc-content-not-a-list"}
             ] = transcript |> updates() |> Enum.drop(1)
    end

    test "toolcall_partial_args_are_decoded_when_valid_json" do
      steps =
        active_prompt() ++
          [
            Flows.message_update(%{
              "type" => "toolcall_delta",
              "toolCall" => %{
                "id" => "tc-json",
                "name" => "bash",
                "partialArgs" => ~s({"command":"ls"})
              }
            }),
            Flows.message_update(%{
              "type" => "toolcall_delta",
              "toolCall" => %{
                "id" => "tc-json",
                "name" => "bash",
                "partialArgs" => ~s({"command":"ls -)
              }
            }),
            Flows.message_update(%{
              "type" => "toolcall_delta",
              "toolCall" => %{"id" => "tc-json", "name" => "bash", "partialArgs" => "[1]"}
            }),
            Flows.message_update(%{
              "type" => "toolcall_delta",
              "toolCall" => %{"id" => "tc-json", "name" => "bash", "arguments" => "not-a-map"}
            })
          ]

      transcript =
        PiGolden.assert_golden(@area, "toolcall_partial_args_are_decoded_when_valid_json", steps)

      assert [
               %{"rawInput" => %{"command" => "ls"}},
               %{"rawInput" => %{"partialArgs" => ~s({"command":"ls -)}},
               %{"rawInput" => %{"partialArgs" => "[1]"}},
               %{"rawInput" => %{}}
             ] = transcript |> updates() |> Enum.drop(1)
    end
  end

  describe "tool execution" do
    test "tool_execution_lifecycle_for_bash" do
      steps =
        active_prompt() ++
          [
            Flows.tool_start("tc-1", "bash", %{"command" => "ls"}),
            Flows.tool_update("tc-1", Flows.text_result("file1")),
            Flows.tool_update("tc-1", nil),
            Flows.tool_end("tc-1", Flows.text_result("file1\nfile2")),
            {:note, "A second start for the same id after completion is a fresh tool_call"},
            Flows.tool_start("tc-1", "bash", %{"command" => "pwd"}),
            Flows.tool_start("tc-1", "bash", %{"command" => "pwd again"})
          ]

      transcript = PiGolden.assert_golden(@area, "tool_execution_lifecycle_for_bash", steps)

      assert [
               %{
                 "sessionUpdate" => "tool_call",
                 "status" => "in_progress",
                 "rawInput" => %{"command" => "ls"}
               },
               %{
                 "sessionUpdate" => "tool_call_update",
                 "status" => "in_progress",
                 "content" => [_]
               },
               %{"sessionUpdate" => "tool_call_update", "status" => "in_progress"},
               %{"sessionUpdate" => "tool_call_update", "status" => "completed"},
               %{"sessionUpdate" => "tool_call"},
               %{"sessionUpdate" => "tool_call_update", "status" => "in_progress"}
             ] = transcript |> updates() |> Enum.drop(1)
    end

    test "tool_execution_end_error_unknown_id_and_result_shapes" do
      steps =
        active_prompt() ++
          [
            Flows.tool_start("tc-err", "bash", %{"command" => "false"}),
            Flows.tool_end(
              "tc-err",
              %{
                "content" => [],
                "details" => %{"stdout" => "", "stderr" => "bad", "exitCode" => 1}
              },
              true
            ),
            Flows.tool_end("tc-unknown", "plain string result"),
            Flows.tool_end("tc-diff", %{"details" => %{"diff" => "--- a\n+++ b"}}),
            Flows.tool_end("tc-bash", %{
              "content" => [],
              "details" => %{"output" => "out", "stderr" => " ", "code" => 0}
            }),
            Flows.tool_end("tc-json", %{"custom" => [1, 2]}),
            Flows.tool_end("tc-nil", nil),
            Flows.tool_end("tc-empty", %{"content" => [%{"type" => "image"}], "details" => nil})
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "tool_execution_end_error_unknown_id_and_result_shapes",
          steps
        )

      assert [
               %{
                 "status" => "failed",
                 "content" => [%{"content" => %{"text" => "stderr:\nbad\n\nexit code: 1"}}]
               },
               %{
                 "status" => "completed",
                 "content" => [%{"content" => %{"text" => "plain string result"}}]
               },
               %{"content" => [%{"content" => %{"text" => "--- a\n+++ b"}}]},
               %{"content" => [%{"content" => %{"text" => "out\n\nexit code: 0"}}]},
               %{"content" => [%{"content" => %{"text" => ~s({"custom":[1,2]})}}]},
               %{"status" => "completed", "toolCallId" => "tc-nil"},
               %{"status" => "completed", "toolCallId" => "tc-empty"}
             ] = transcript |> updates() |> Enum.drop(2)
    end

    test "tool_kinds_and_locations_resolve_against_the_session_cwd" do
      steps =
        active_prompt() ++
          [
            Flows.tool_start("tc-read", "read", %{"path" => "lib/a.ex"}),
            Flows.tool_start("tc-write", "write", %{"path" => "/abs/b.ex", "content" => "x"}),
            Flows.tool_start("tc-edit", "edit", %{"path" => "missing.ex", "oldText" => "a"}),
            Flows.tool_start("tc-other", "grep", %{"pattern" => "x"}),
            Flows.tool_start("tc-nopath", "read", %{"path" => 7})
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "tool_kinds_and_locations_resolve_against_the_session_cwd",
          steps
        )

      assert [
               %{"kind" => "read", "locations" => [%{"path" => "<sandbox>/project/lib/a.ex"}]},
               %{"kind" => "edit", "locations" => [%{"path" => "/abs/b.ex"}]},
               %{"kind" => "edit", "locations" => [%{"path" => "<sandbox>/project/missing.ex"}]},
               %{"kind" => "other"},
               %{"kind" => "read"}
             ] = transcript |> updates() |> Enum.drop(1)

      refute Map.has_key?(Enum.at(updates(transcript), 4), "locations")
    end

    test "edit_tool_emits_a_structured_diff_when_the_file_changed" do
      steps =
        active_prompt() ++
          [
            {:write_file, "<sandbox>/project/lib/target.ex", "line one\nline two\nline three\n"},
            Flows.tool_start("edit-1", "edit", %{
              "path" => "lib/target.ex",
              "oldText" => "line two"
            }),
            {:write_file, "<sandbox>/project/lib/target.ex", "line one\nline 2\nline three\n"},
            Flows.tool_end("edit-1", Flows.text_result("updated")),
            {:note, "Ambiguous oldText yields no line number"},
            Flows.tool_start("edit-2", "edit", %{
              "path" => "<sandbox>/project/lib/target.ex",
              "oldText" => "line"
            }),
            {:write_file, "<sandbox>/project/lib/target.ex", "changed\n"},
            Flows.tool_end("edit-2", Flows.text_result(""))
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "edit_tool_emits_a_structured_diff_when_the_file_changed",
          steps
        )

      assert [
               %{"locations" => [%{"path" => "<sandbox>/project/lib/target.ex", "line" => 2}]},
               %{
                 "content" => [
                   %{
                     "type" => "diff",
                     "path" => "lib/target.ex",
                     "oldText" => "line one\nline two\nline three\n",
                     "newText" => "line one\nline 2\nline three\n"
                   },
                   %{"type" => "content"}
                 ]
               },
               %{"locations" => [%{"path" => "<sandbox>/project/lib/target.ex"}]},
               %{"content" => [%{"type" => "diff", "newText" => "changed\n"}]}
             ] = transcript |> updates() |> Enum.drop(1)
    end

    test "edit_tool_without_a_change_missing_file_or_error_emits_text_only" do
      steps =
        active_prompt() ++
          [
            {:write_file, "<sandbox>/project/same.ex", "same\n"},
            Flows.tool_start("edit-same", "edit", %{"path" => "same.ex", "oldText" => "same"}),
            Flows.tool_end("edit-same", Flows.text_result("no-op")),
            Flows.tool_start("edit-missing", "edit", %{"path" => "nope.ex", "oldText" => "x"}),
            {:write_file, "<sandbox>/project/nope.ex", "created\n"},
            Flows.tool_end("edit-missing", Flows.text_result("created")),
            {:write_file, "<sandbox>/project/fail.ex", "before\n"},
            Flows.tool_start("edit-fail", "edit", %{"path" => "fail.ex", "oldText" => "before"}),
            {:write_file, "<sandbox>/project/fail.ex", "after\n"},
            Flows.tool_end("edit-fail", Flows.text_result("failed"), true),
            Flows.tool_start("edit-nopath", "edit", %{"oldText" => "x"}),
            Flows.tool_end("edit-nopath", Flows.text_result("no path"))
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "edit_tool_without_a_change_missing_file_or_error_emits_text_only",
          steps
        )

      ends = transcript |> updates() |> Enum.filter(&(&1["sessionUpdate"] == "tool_call_update"))

      assert Enum.all?(ends, fn update ->
               Enum.all?(update["content"], &(&1["type"] == "content"))
             end)

      assert [
               %{"status" => "completed"},
               %{"status" => "completed"},
               %{"status" => "failed"},
               %{"status" => "completed"}
             ] = ends
    end
  end

  describe "usage and status events" do
    test "agent_end_usage_is_reported_on_settlement" do
      steps =
        active_prompt() ++
          [
            Flows.agent_end(%{
              "input" => 120,
              "output" => 30,
              "cacheRead" => 5,
              "cacheWrite" => 7,
              "cost" => %{"total" => 0.012}
            }),
            Flows.agent_settled(),
            Flows.prompt(3, "next"),
            {:inbound,
             %{
               "type" => "agent_end",
               "messages" => [
                 %{"role" => "assistant", "usage" => %{"input" => 1, "output" => 1}},
                 %{"role" => "user"},
                 %{"role" => "assistant", "usage" => %{"output" => 9}}
               ]
             }},
            Flows.agent_settled(),
            Flows.prompt(4, "third"),
            Flows.agent_end(nil),
            Flows.agent_settled(),
            Flows.prompt(5, "fourth"),
            {:inbound, %{"type" => "agent_end"}},
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(@area, "agent_end_usage_is_reported_on_settlement", steps)

      assert [
               %{
                 "inputTokens" => 120,
                 "outputTokens" => 30,
                 "cacheReadTokens" => 5,
                 "cacheWriteTokens" => 7,
                 "cost" => 0.012
               },
               %{"inputTokens" => 0, "outputTokens" => 9, "cost" => nil},
               %{"inputTokens" => 0, "outputTokens" => 0},
               %{"inputTokens" => 0}
             ] =
               transcript
               |> PiGolden.messages()
               |> Enum.filter(&(&1["id"] in [2, 3, 4, 5]))
               |> Enum.map(& &1["result"]["usage"])
    end

    test "usage_recorded_without_a_pending_prompt_is_discarded" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.agent_end(%{"input" => 99, "output" => 99}),
            Flows.agent_settled(),
            Flows.prompt(2, "go"),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "usage_recorded_without_a_pending_prompt_is_discarded",
          steps
        )

      assert %{messages: [%{"id" => 2, "result" => %{"usage" => %{}}}]} =
               PiGolden.last_result(transcript)
    end

    test "auto_compaction_and_retry_events_report_status" do
      steps =
        active_prompt() ++
          [
            {:inbound, %{"type" => "auto_compaction_start", "reason" => "threshold"}},
            {:inbound,
             %{"type" => "auto_compaction_end", "tokensBefore" => 100, "tokensAfter" => 10}},
            {:inbound, %{"type" => "auto_retry_start", "attempt" => 1}},
            {:inbound, %{"type" => "auto_retry_end", "attempt" => 1, "success" => true}}
          ]

      transcript =
        PiGolden.assert_golden(@area, "auto_compaction_and_retry_events_report_status", steps)

      assert [
               "Context nearing limit, compacting.",
               "Compaction finished, resuming.",
               "Retrying after transient failure.",
               "Retry finished, resuming."
             ] =
               transcript
               |> updates()
               |> Enum.filter(&(&1["sessionUpdate"] == "agent_message_chunk"))
               |> Enum.map(& &1["content"]["text"])
    end

    test "lifecycle_and_unknown_events_are_skipped" do
      steps =
        active_prompt() ++
          Enum.map(
            ["agent_start", "turn_start", "turn_end", "message_start", "message_end"],
            &{:inbound, %{"type" => &1}}
          ) ++
          [
            {:inbound, %{"type" => "message_update"}},
            {:inbound,
             %{
               "type" => "message_update",
               "assistantMessageEvent" => %{"type" => "thinking_start"}
             }},
            {:inbound,
             %{"type" => "message_update", "assistantMessageEvent" => %{"type" => "text_delta"}}},
            {:inbound, %{"type" => "mystery", "payload" => 1}},
            {:inbound, %{"type" => "tool_execution_start", "toolCallId" => "x"}},
            {:inbound, %{"type" => "extension_ui_request", "method" => "select"}}
          ]

      transcript =
        PiGolden.assert_golden(@area, "lifecycle_and_unknown_events_are_skipped", steps)

      assert Enum.all?(Enum.drop(transcript, 7), &match?(%{tag: :skip}, &1.result))
    end
  end

  describe "extension UI" do
    test "select_request_round_trips_through_permission_choices" do
      event = %{
        "type" => "extension_ui_request",
        "id" => "ui-1",
        "method" => "select",
        "title" => "Choose a target",
        "options" => ["staging", "production", 3],
        "internal" => "must not cross the ACP boundary"
      }

      steps =
        active_prompt() ++
          [
            {:inbound, event},
            reply(selected("choice-1")),
            {:note, "The answered request is gone: a second reply is ignored"},
            reply(selected("choice-0")),
            {:inbound, %{event | "id" => "ui-2", "options" => ["only"]}},
            reply(selected("choice-9")),
            {:inbound, %{event | "id" => "ui-3"}},
            reply(%{"outcome" => %{"outcome" => "rejected"}})
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "select_request_round_trips_through_permission_choices",
          steps
        )

      assert %{
               "method" => "session/request_permission",
               "id" => "pi-extension-2",
               "params" => %{
                 "options" => [
                   %{"optionId" => "choice-0"},
                   %{"optionId" => "choice-1"},
                   %{"optionId" => "choice-2", "name" => "3"}
                 ],
                 "toolCall" => %{"toolCallId" => "pi-ui-ui-1", "rawInput" => raw}
               }
             } =
               transcript
               |> PiGolden.messages()
               |> Enum.find(&(&1["method"] == "session/request_permission"))

      refute Map.has_key?(raw, "internal")

      assert [
               %{"id" => "ui-1", "type" => "extension_ui_response", "value" => "production"},
               %{"id" => "ui-2", "type" => "extension_ui_response", "cancelled" => true},
               %{"id" => "ui-3", "type" => "extension_ui_response", "cancelled" => true}
             ] =
               transcript
               |> PiGolden.writes()
               |> Enum.filter(&(&1["type"] == "extension_ui_response"))
    end

    test "confirm_request_maps_yes_no_and_cancellation" do
      event = %{
        "type" => "extension_ui_request",
        "id" => "c-1",
        "method" => "confirm",
        "message" => "Deploy?"
      }

      steps =
        active_prompt() ++
          [
            {:inbound, event},
            reply(selected("yes")),
            {:inbound, %{event | "id" => "c-2"}},
            reply(selected("no")),
            {:inbound, %{event | "id" => "c-3"}},
            {:outbound,
             fn t ->
               %{"id" => permission_id(t), "error" => %{"code" => -32_000, "message" => "denied"}}
             end},
            {:inbound, %{event | "id" => "c-4"}},
            {:outbound,
             fn t ->
               %{"method" => "$/cancel_request", "params" => %{"requestId" => permission_id(t)}}
             end},
            {:inbound, %{event | "id" => "c-5"}},
            reply(%{"outcome" => %{"outcome" => "cancelled"}})
          ]

      transcript =
        PiGolden.assert_golden(@area, "confirm_request_maps_yes_no_and_cancellation", steps)

      assert [
               %{"id" => "c-1", "confirmed" => true},
               %{"id" => "c-2", "confirmed" => false},
               %{"id" => "c-3", "cancelled" => true},
               %{"id" => "c-4", "cancelled" => true},
               %{"id" => "c-5", "cancelled" => true}
             ] =
               transcript
               |> PiGolden.writes()
               |> Enum.filter(&(&1["type"] == "extension_ui_response"))
    end

    test "unsupported_ui_methods_are_cancelled_immediately" do
      steps =
        active_prompt() ++
          [
            {:inbound,
             %{
               "type" => "extension_ui_request",
               "id" => "in-1",
               "method" => "input",
               "message" => "Secret?"
             }},
            {:inbound,
             %{
               "type" => "extension_ui_request",
               "id" => "ed-1",
               "method" => "editor",
               "prefill" => "x"
             }},
            {:inbound,
             %{
               "type" => "extension_ui_request",
               "id" => "n-1",
               "method" => "notify",
               "message" => "Done",
               "notifyType" => "warning"
             }},
            {:inbound, %{"type" => "extension_ui_request", "id" => "n-2", "method" => "notify"}},
            {:inbound,
             %{
               "type" => "extension_ui_request",
               "id" => "s-0",
               "method" => "select",
               "options" => []
             }},
            {:inbound, %{"type" => "extension_ui_request", "id" => "s-1", "method" => "select"}},
            {:inbound,
             %{"type" => "extension_ui_request", "id" => "x-1", "method" => "progress"}},
            {:note, "A blank id is still a string: it is cancelled with that blank id"},
            {:inbound,
             %{
               "type" => "extension_ui_request",
               "id" => "",
               "method" => "select",
               "options" => ["a"]
             }},
            {:inbound, %{"type" => "extension_ui_request", "id" => 12, "method" => "confirm"}}
          ]

      transcript =
        PiGolden.assert_golden(@area, "unsupported_ui_methods_are_cancelled_immediately", steps)

      assert ["in-1", "ed-1", "n-1", "n-2", "s-0", "s-1", "x-1", ""] =
               transcript
               |> PiGolden.writes()
               |> Enum.filter(&(&1["type"] == "extension_ui_response"))
               |> Enum.map(& &1["id"])

      assert %{
               "content" => %{"text" => "Done"},
               "_meta" => %{"piAcp" => %{"notify" => %{"level" => "warning"}}}
             } =
               transcript |> updates() |> Enum.find(&(&1["content"]["text"] == "Done"))

      assert %{tag: :skip} = PiGolden.last_result(transcript)
    end
  end

  describe "batched port data" do
    test "port_data_splits_lines_and_buffers_partial_frames" do
      first =
        Jason.encode!(%{
          "type" => "message_update",
          "assistantMessageEvent" => %{"type" => "text_delta", "delta" => "A"}
        })

      second =
        Jason.encode!(%{
          "type" => "message_update",
          "assistantMessageEvent" => %{"type" => "thinking_delta", "delta" => "B"}
        })

      third =
        Jason.encode!(%{
          "type" => "message_update",
          "assistantMessageEvent" => %{"type" => "text_delta", "delta" => "C"}
        })

      {head, tail} = String.split_at(third, 12)

      steps =
        active_prompt() ++
          [
            {:port_data, first <> "\n" <> second <> "\n" <> head},
            {:port_data, tail <> "\n"},
            {:port_data, ""},
            {:port_data, "\n\n"},
            {:port_data, "pi startup banner\nnot json either\n"},
            {:port_data, "{\"type\":\"agent_settled\"}\n"}
          ]

      transcript =
        PiGolden.assert_golden(@area, "port_data_splits_lines_and_buffers_partial_frames", steps)

      assert [
               %{tag: :messages, messages: [_, _]},
               %{tag: :messages, messages: [_]},
               %{tag: :skip},
               %{tag: :skip},
               %{tag: :skip},
               %{
                 tag: :messages,
                 messages: [%{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "AC"}}}}]
               }
             ] = Enum.map(Enum.take(transcript, -6), & &1.result)
    end
  end
end
