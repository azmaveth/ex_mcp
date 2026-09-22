defmodule ExMCP.ACP.Adapters.ClaudeSDK.SessionUpdatesGoldenTest do
  @moduledoc """
  Characterization gate for the Claude SDK adapter's session update
  ordering (`docs/POST_1_0_MAINTENANCE_PLAN.md`, "Claude adapter
  characterization gate": event ordering for `agent_message_chunk`,
  `agent_thought_chunk`, `tool_call` / `tool_call_update`, `plan`,
  `session_info_update`, `current_mode_update`, `config_option_update`,
  `available_commands_update`, and usage).

  Each test drives `ExMCP.ACP.Adapters.ClaudeSDK` through
  `ExMCP.Test.ClaudeGolden` and compares the recorded transcript against a
  committed fixture under `test/fixtures/acp/claude/session_updates/`. The
  fixtures pin:

    * partial `stream_event` conversion: `text_delta` and `thinking_delta`
      chunks, a `tool_use` block start announced as a pending tool call,
      and the deltas and block types that emit nothing;
    * complete `assistant` messages: text suppressed when the same turn
      already streamed it, text emitted when it did not, thinking blocks
      kept silent, `tool_use` emitting `tool_call` then `tool_call_update`
      (only the update when the block start already announced it), and
      `TodoWrite` emitting a `plan`;
    * `user` messages carrying `tool_result`: the Bash terminal shape with
      its `terminal_output` / `terminal_exit` metadata and exit code
      (including `bash_code_execution_result`), the generic content and
      `rawOutput` conversion for string, list and unknown payloads, and
      the `toolName` / `toolInput` metadata the adapter remembers from the
      matching `tool_use` (both are sent as explicit `null` when the tool
      is unknown, because `compact/1` only prunes the outer `_meta` map);
    * `result`: the `session/prompt` response - emitted *after* the updates
      of the same event - (stop reason, usage, the `_meta.quota`
      `token_count` and per-model breakdown, which is the increment since
      the previous reading of Claude's running `modelUsage` total, and
      `_meta.ex_mcp.claude_sdk` text/session/cost/errors), the
      `usage_update` that only appears with a known context window, the
      trailing `config_option_update` and `session_info_update`, the
      fallback `agent_message_chunk` when nothing streamed, the stop-reason
      table, auth and subtype error metadata, and a result arriving with no
      prompt pending;
    * `system` events: `init` (four updates, and three without slash
      commands), `status`, `session_state_changed`, `commands_changed`,
      `permission_denied`, an unrecognized subtype, and the background-task
      subtypes that both emit plans and defer a result until every spawned
      subagent drains;
    * the standalone `tool_progress`, `tool_use_summary` and
      `rate_limit_event` updates, and the message types that are skipped;
    * the `messageId` chunk updates carry: streamed chunks tagged with the
      id of the `message_start` that opened the message (and re-tagged by a
      second one), no id when no `message_start` arrived or it carried none,
      no id inherited across a turn boundary, and the two chunks that
      deliberately carry none - `tool_call` / `tool_call_update` / `plan`,
      which are not chunk updates at all, and the `result` fallback chunk,
      which ExMCP synthesizes from an event that has no message id.

  Prompt queueing and cancellation belong to the faults area; the contents
  of the config option catalog belong to the catalog area.

  Mutation check (2026-09-21): in `claude_sdk/mapper.ex`, emitting the
  terminal assistant text even after it was streamed (removing the
  `current_assistant_text_streamed?: true` clause of
  `handle_assistant_block/3`) fails
  `streamed_text_is_not_repeated_by_the_assistant_message`.

  Mutation check (2026-09-22): in `claude_sdk/mapper.ex`, removing the
  `message_start` clause of `handle_stream_event/2` fails
  `streamed_chunks_carry_the_message_start_id`,
  `a_second_message_start_restamps_the_chunks_that_follow`,
  `a_message_start_without_an_id_stamps_nothing` and
  `the_next_turn_does_not_inherit_the_previous_message_start_id`; stamping a
  message id on `result_text_chunk/3` fails
  `the_result_fallback_chunk_carries_no_message_id`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CLAUDE_GOLDEN=update mix test <this file>[:line]`; that run rewrites
  the fixture and fails on purpose, so review the diff and re-run without
  the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.ClaudeGolden
  alias ExMCP.Test.ClaudeGolden.Flows

  @area "session_updates"

  describe "stream events" do
    test "text_deltas_become_agent_message_chunks" do
      steps = turn() ++ [Flows.text_delta("Hel"), Flows.text_delta("lo")]

      transcript =
        ClaudeGolden.assert_golden(@area, "text_deltas_become_agent_message_chunks", steps)

      assert ["agent_message_chunk", "agent_message_chunk"] =
               ClaudeGolden.update_types(transcript)
    end

    test "thinking_deltas_become_agent_thought_chunks" do
      steps = turn() ++ [Flows.thinking_delta("Let me "), Flows.thinking_delta("think")]

      ClaudeGolden.assert_golden(@area, "thinking_deltas_become_agent_thought_chunks", steps)
    end

    test "tool_use_block_start_announces_a_pending_tool_call" do
      steps =
        turn() ++
          [
            Flows.block_start(%{
              "type" => "tool_use",
              "id" => "toolu_1",
              "name" => "Read",
              "input" => %{"file_path" => "#{Flows.cwd()}/lib/app.ex"}
            })
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "tool_use_block_start_announces_a_pending_tool_call",
          steps
        )

      assert ["tool_call"] = ClaudeGolden.update_types(transcript)
    end

    test "text_and_thinking_block_starts_emit_nothing" do
      steps =
        turn() ++
          [
            Flows.block_start(%{"type" => "text", "text" => ""}),
            Flows.block_start(%{"type" => "thinking", "thinking" => ""}),
            Flows.block_start(%{"no" => "type"}),
            Flows.block_stop()
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "text_and_thinking_block_starts_emit_nothing", steps)

      assert ClaudeGolden.messages(transcript) == []
    end

    test "input_json_and_unknown_deltas_emit_nothing" do
      steps =
        turn() ++
          [
            Flows.stream_event(%{
              "type" => "content_block_delta",
              "delta" => %{"type" => "input_json_delta", "partial_json" => "{\"a\":"}
            }),
            Flows.stream_event(%{
              "type" => "content_block_delta",
              "delta" => %{"type" => "citation_delta"}
            }),
            Flows.stream_event(%{"type" => "message_start", "message" => %{}})
          ]

      ClaudeGolden.assert_golden(@area, "input_json_and_unknown_deltas_emit_nothing", steps)
    end

    test "a_thinking_block_is_closed_by_its_stop" do
      steps =
        turn() ++
          [
            Flows.thinking_delta("deep"),
            Flows.block_stop(),
            {:note, "The closed thinking block is folded into state, not re-emitted"},
            Flows.text_delta("answer")
          ]

      ClaudeGolden.assert_golden(@area, "a_thinking_block_is_closed_by_its_stop", steps)
    end
  end

  describe "message ids" do
    test "streamed_chunks_carry_the_message_start_id" do
      steps =
        turn() ++
          [
            Flows.message_start("msg_stream_1"),
            Flows.text_delta("Hel"),
            Flows.text_delta("lo"),
            Flows.thinking_delta("hmm")
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "streamed_chunks_carry_the_message_start_id", steps)

      assert message_ids(transcript) == ["msg_stream_1", "msg_stream_1", "msg_stream_1"]
    end

    test "a_second_message_start_restamps_the_chunks_that_follow" do
      steps =
        turn() ++
          [
            Flows.message_start("msg_stream_1"),
            Flows.text_delta("first"),
            Flows.message_start("msg_stream_2"),
            Flows.text_delta("second")
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "a_second_message_start_restamps_the_chunks_that_follow",
          steps
        )

      assert message_ids(transcript) == ["msg_stream_1", "msg_stream_2"]
    end

    test "streamed_chunks_without_a_message_start_carry_no_message_id" do
      steps = turn() ++ [Flows.text_delta("Hello"), Flows.thinking_delta("hmm")]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "streamed_chunks_without_a_message_start_carry_no_message_id",
          steps
        )

      assert message_ids(transcript) == []
    end

    test "a_message_start_without_an_id_stamps_nothing" do
      steps =
        turn() ++
          [
            Flows.message_start("msg_stream_1"),
            Flows.text_delta("first"),
            Flows.stream_event(%{
              "type" => "message_start",
              "message" => %{"role" => "assistant"}
            }),
            Flows.text_delta("second")
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "a_message_start_without_an_id_stamps_nothing", steps)

      assert message_ids(transcript) == ["msg_stream_1"]
    end

    test "the_next_turn_does_not_inherit_the_previous_message_start_id" do
      steps =
        turn() ++
          [
            Flows.message_start("msg_stream_1"),
            Flows.text_delta("first"),
            Flows.result(),
            Flows.prompt("acp-prompt-2", "again"),
            Flows.text_delta("second")
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "the_next_turn_does_not_inherit_the_previous_message_start_id",
          steps
        )

      assert message_ids(transcript) == ["msg_stream_1"]
    end

    test "tool_calls_and_plans_never_carry_a_message_id" do
      steps =
        turn() ++
          [
            Flows.message_start("msg_stream_1"),
            Flows.assistant([
              Flows.tool_use("toolu_1", "TodoWrite", %{
                "todos" => [%{"content" => "ship", "status" => "pending"}]
              })
            ]),
            Flows.tool_result("toolu_1", "done")
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "tool_calls_and_plans_never_carry_a_message_id", steps)

      assert ["tool_call", "tool_call_update", "plan", "tool_call_update"] =
               ClaudeGolden.update_types(transcript)

      assert message_ids(transcript) == []
    end

    test "the_result_fallback_chunk_carries_no_message_id" do
      steps = turn() ++ [Flows.result(%{"result" => "nothing streamed"})]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "the_result_fallback_chunk_carries_no_message_id",
          steps
        )

      assert "agent_message_chunk" in ClaudeGolden.update_types(transcript)
      assert message_ids(transcript) == []
    end
  end

  describe "assistant messages" do
    test "streamed_text_is_not_repeated_by_the_assistant_message" do
      steps = turn() ++ [Flows.text_delta("Hello"), Flows.assistant_text("Hello")]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "streamed_text_is_not_repeated_by_the_assistant_message",
          steps
        )

      assert ["agent_message_chunk"] = ClaudeGolden.update_types(transcript)
    end

    test "unstreamed_assistant_text_is_emitted" do
      steps = turn() ++ [Flows.assistant_text("Hello")]

      transcript =
        ClaudeGolden.assert_golden(@area, "unstreamed_assistant_text_is_emitted", steps)

      assert ["agent_message_chunk"] = ClaudeGolden.update_types(transcript)
    end

    test "two_assistant_messages_after_one_stream_keep_the_second" do
      steps =
        turn() ++
          [
            Flows.text_delta("Hello"),
            Flows.assistant_text("Hello"),
            {:note, "A new assistant message did not stream, so its text is emitted"},
            Flows.assistant_text("Anything else?")
          ]

      ClaudeGolden.assert_golden(
        @area,
        "two_assistant_messages_after_one_stream_keep_the_second",
        steps
      )
    end

    test "every_text_block_of_an_unstreamed_message_is_emitted" do
      steps =
        turn() ++
          [
            Flows.assistant([
              %{"type" => "text", "text" => "one"},
              %{"type" => "text", "text" => "two"}
            ])
          ]

      ClaudeGolden.assert_golden(
        @area,
        "every_text_block_of_an_unstreamed_message_is_emitted",
        steps
      )
    end

    test "assistant_thinking_blocks_emit_nothing" do
      steps =
        turn() ++ [Flows.assistant([%{"type" => "thinking", "thinking" => "quietly"}])]

      transcript =
        ClaudeGolden.assert_golden(@area, "assistant_thinking_blocks_emit_nothing", steps)

      assert ClaudeGolden.messages(transcript) == []
    end

    test "assistant_tool_use_announces_then_starts_the_call" do
      steps =
        turn() ++ [Flows.assistant([Flows.tool_use("toolu_1", "Bash", %{"command" => "ls"})])]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "assistant_tool_use_announces_then_starts_the_call",
          steps
        )

      assert ["tool_call", "tool_call_update"] = ClaudeGolden.update_types(transcript)
    end

    test "an_already_announced_tool_use_only_updates" do
      steps =
        turn() ++
          [
            Flows.block_start(Flows.tool_use("toolu_1", "Bash", %{})),
            Flows.assistant([Flows.tool_use("toolu_1", "Bash", %{"command" => "ls"})])
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "an_already_announced_tool_use_only_updates", steps)

      assert ["tool_call", "tool_call_update"] = ClaudeGolden.update_types(transcript)
    end

    test "todo_write_emits_a_plan" do
      steps =
        turn() ++
          [
            Flows.assistant([
              Flows.tool_use("toolu_1", "TodoWrite", %{
                "todos" => [
                  %{"content" => "Read the code", "status" => "completed", "priority" => "high"},
                  %{"content" => "Write tests", "status" => "in_progress"},
                  %{"text" => "Ship", "status" => "pending"},
                  %{"title" => "Done", "status" => "done"},
                  %{"status" => "active"}
                ]
              })
            ])
          ]

      transcript = ClaudeGolden.assert_golden(@area, "todo_write_emits_a_plan", steps)

      assert ["tool_call", "tool_call_update", "plan"] = ClaudeGolden.update_types(transcript)
    end

    test "todo_write_without_todos_emits_no_plan" do
      steps = turn() ++ [Flows.assistant([Flows.tool_use("toolu_1", "TodoWrite", %{})])]

      transcript =
        ClaudeGolden.assert_golden(@area, "todo_write_without_todos_emits_no_plan", steps)

      assert ["tool_call", "tool_call_update"] = ClaudeGolden.update_types(transcript)
    end

    test "assistant_model_updates_the_config_catalog" do
      steps =
        turn() ++
          [
            Flows.assistant([%{"type" => "text", "text" => "hi"}], %{
              "message" => %{"model" => "claude-opus-4"}
            }),
            {:note, "The next config_option_update carries the model the assistant reported"},
            Flows.result()
          ]

      ClaudeGolden.assert_golden(@area, "assistant_model_updates_the_config_catalog", steps)
    end

    test "an_assistant_message_id_is_not_a_session_id" do
      steps =
        turn() ++
          [
            {:inbound,
             %{
               "type" => "assistant",
               "message" => %{
                 "role" => "assistant",
                 "id" => "msg_01ABC",
                 "content" => [%{"type" => "text", "text" => "hi"}]
               }
             }}
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "an_assistant_message_id_is_not_a_session_id", steps)

      assert [%{"params" => %{"sessionId" => "claude_sdk_<1>"}}] =
               ClaudeGolden.updates(transcript)
    end

    test "a_non_list_assistant_content_emits_nothing" do
      steps =
        turn() ++
          [{:inbound, %{"type" => "assistant", "message" => %{"content" => "plain string"}}}]

      ClaudeGolden.assert_golden(@area, "a_non_list_assistant_content_emits_nothing", steps)
    end
  end

  describe "tool results" do
    test "bash_results_carry_terminal_metadata" do
      steps =
        turn() ++
          [
            Flows.assistant([Flows.tool_use("toolu_1", "Bash", %{"command" => "ls -la"})]),
            Flows.tool_result("toolu_1", [%{"type" => "text", "text" => "total 0"}])
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "bash_results_carry_terminal_metadata", steps)

      assert %{"content" => [%{"type" => "terminal"}], "status" => "completed"} =
               last_update(transcript)
    end

    test "a_failed_bash_result_exits_one" do
      steps =
        turn() ++
          [
            Flows.assistant([Flows.tool_use("toolu_1", "Bash", %{"command" => "false"})]),
            Flows.tool_result("toolu_1", "boom", is_error: true)
          ]

      transcript = ClaudeGolden.assert_golden(@area, "a_failed_bash_result_exits_one", steps)

      assert %{"status" => "failed"} = last_update(transcript)
    end

    test "bash_code_execution_results_carry_their_return_code" do
      steps =
        turn() ++
          [
            Flows.assistant([Flows.tool_use("toolu_1", "Bash", %{"command" => "exit 2"})]),
            Flows.tool_result("toolu_1", %{
              "type" => "bash_code_execution_result",
              "stdout" => "out",
              "stderr" => "err",
              "return_code" => 2
            })
          ]

      ClaudeGolden.assert_golden(
        @area,
        "bash_code_execution_results_carry_their_return_code",
        steps
      )
    end

    test "generic_tool_results_convert_their_content" do
      steps =
        turn() ++
          [
            Flows.assistant([
              Flows.tool_use("toolu_1", "Read", %{"file_path" => "#{Flows.cwd()}/a.ex"})
            ]),
            Flows.tool_result("toolu_1", [
              %{"type" => "text", "text" => "line one"},
              %{"text" => "line two"},
              %{"type" => "image", "data" => "QUFB"}
            ])
          ]

      ClaudeGolden.assert_golden(@area, "generic_tool_results_convert_their_content", steps)
    end

    test "a_string_tool_result_becomes_one_content_block" do
      steps =
        turn() ++
          [
            Flows.assistant([Flows.tool_use("toolu_1", "Glob", %{"pattern" => "**/*.ex"})]),
            Flows.tool_result("toolu_1", "a.ex\nb.ex")
          ]

      ClaudeGolden.assert_golden(@area, "a_string_tool_result_becomes_one_content_block", steps)
    end

    test "an_unknown_tool_result_shape_has_no_content" do
      steps =
        turn() ++
          [
            Flows.assistant([Flows.tool_use("toolu_1", "Glob", %{})]),
            Flows.tool_result("toolu_1", %{"weird" => true})
          ]

      ClaudeGolden.assert_golden(@area, "an_unknown_tool_result_shape_has_no_content", steps)
    end

    test "a_result_for_an_unknown_tool_has_no_tool_metadata" do
      steps = turn() ++ [Flows.tool_result("toolu_unknown", "output")]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "a_result_for_an_unknown_tool_has_no_tool_metadata",
          steps
        )

      assert %{"_meta" => %{"ex_mcp" => %{"claude_sdk" => meta}}} = last_update(transcript)
      assert meta == %{"isError" => false, "toolName" => nil, "toolInput" => nil}
    end

    test "a_tool_result_is_only_reported_once" do
      steps =
        turn() ++
          [
            Flows.assistant([Flows.tool_use("toolu_1", "Read", %{"file_path" => "/a.ex"})]),
            Flows.tool_result("toolu_1", "first"),
            {:note, "The tool was forgotten, so a repeat carries no tool metadata"},
            Flows.tool_result("toolu_1", "second")
          ]

      ClaudeGolden.assert_golden(@area, "a_tool_result_is_only_reported_once", steps)
    end

    test "a_user_message_without_tool_results_emits_nothing" do
      steps =
        turn() ++
          [
            {:inbound,
             %{
               "type" => "user",
               "message" => %{
                 "role" => "user",
                 "content" => [%{"type" => "text", "text" => "more please"}]
               }
             }}
          ]

      ClaudeGolden.assert_golden(
        @area,
        "a_user_message_without_tool_results_emits_nothing",
        steps
      )
    end
  end

  describe "results" do
    test "a_result_settles_the_prompt" do
      steps = turn() ++ [Flows.text_delta("Hello"), Flows.result()]

      transcript = ClaudeGolden.assert_golden(@area, "a_result_settles_the_prompt", steps)

      assert %{tag: :messages, messages: messages} = ClaudeGolden.last_result(transcript)
      response = List.last(messages)

      assert response["result"]["stopReason"] == "end_turn"
      assert response["result"]["usage"]["inputTokens"] == 12
    end

    test "a_result_without_streamed_text_emits_a_fallback_chunk" do
      steps = turn() ++ [Flows.result(%{"result" => "All done."})]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "a_result_without_streamed_text_emits_a_fallback_chunk",
          steps
        )

      assert "agent_message_chunk" in ClaudeGolden.update_types(transcript)
    end

    test "a_result_with_a_context_window_emits_a_usage_update" do
      steps =
        turn() ++
          [
            Flows.result(%{
              "modelUsage" => %{
                "claude-sonnet-4" => %{"contextWindow" => 200_000, "inputTokens" => 12}
              }
            })
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "a_result_with_a_context_window_emits_a_usage_update",
          steps
        )

      assert "usage_update" in ClaudeGolden.update_types(transcript)
    end

    test "a_model_named_1m_uses_the_million_token_window" do
      steps =
        [{:init, model: "claude-opus-5[1m]"}] ++
          [Flows.session_new(), Flows.prompt("acp-prompt", "hi"), Flows.result()]

      transcript =
        ClaudeGolden.assert_golden(@area, "a_model_named_1m_uses_the_million_token_window", steps)

      assert "usage_update" in ClaudeGolden.update_types(transcript)
    end

    test "a_result_without_usage_emits_no_usage_update" do
      steps = turn() ++ [Flows.result(%{"usage" => %{}})]

      transcript =
        ClaudeGolden.assert_golden(@area, "a_result_without_usage_emits_no_usage_update", steps)

      refute "usage_update" in ClaudeGolden.update_types(transcript)
    end

    test "a_prompt_response_carries_per_model_quota" do
      steps =
        turn() ++
          [
            Flows.result(%{
              "modelUsage" => %{
                "claude-opus-5[1m]" => %{
                  "inputTokens" => 100,
                  "outputTokens" => 20,
                  "cacheReadInputTokens" => 5,
                  "cacheCreationInputTokens" => 2
                },
                "claude-haiku-4" => %{"inputTokens" => 7, "outputTokens" => 3}
              }
            })
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "a_prompt_response_carries_per_model_quota", steps)

      quota =
        transcript |> prompt_responses() |> List.last() |> get_in(["result", "_meta", "quota"])

      assert quota["token_count"] == %{
               "totalTokens" => 23,
               "inputTokens" => 12,
               "cachedInputTokens" => 3,
               "cachedWriteTokens" => 1,
               "outputTokens" => 7,
               "reasoningOutputTokens" => 0
             }

      assert Enum.map(quota["model_usage"], & &1["model"]) == [
               "claude-haiku-4",
               "claude-opus-5[1m]"
             ]
    end

    test "per_model_quota_is_the_increment_since_the_last_result" do
      steps =
        turn() ++
          [
            {:note, "modelUsage is a running total for the Claude process"},
            Flows.result(%{"modelUsage" => %{"sonnet" => %{"inputTokens" => 100}}}),
            Flows.prompt("acp-prompt-2", "again"),
            Flows.result(%{"modelUsage" => %{"sonnet" => %{"inputTokens" => 130}}}),
            {:note, "a reading that rewound restarts from the reading itself"},
            Flows.prompt("acp-prompt-3", "and again"),
            Flows.result(%{"modelUsage" => %{"sonnet" => %{"inputTokens" => 4}}})
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "per_model_quota_is_the_increment_since_the_last_result",
          steps
        )

      assert [100, 30, 4] = model_quota_inputs(transcript)
    end

    test "a_result_without_model_usage_reports_an_empty_breakdown" do
      steps = turn() ++ [Flows.result()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "a_result_without_model_usage_reports_an_empty_breakdown",
          steps
        )

      assert [] =
               transcript
               |> prompt_responses()
               |> List.last()
               |> quota()
               |> Map.fetch!("model_usage")
    end

    test "stop_reasons_are_mapped" do
      steps =
        turn() ++
          Enum.flat_map(
            ["end_turn", "stop", "tool_use", "max_tokens", "refusal", "cancelled", "banana"],
            fn reason ->
              [
                Flows.prompt("acp-prompt-#{reason}", reason),
                Flows.result(%{"stop_reason" => reason})
              ]
            end
          )

      transcript = ClaudeGolden.assert_golden(@area, "stop_reasons_are_mapped", steps)

      assert [
               "end_turn",
               "end_turn",
               "end_turn",
               "max_tokens",
               "refusal",
               "cancelled",
               "end_turn"
             ] = stop_reasons(transcript)
    end

    test "turn_limits_become_max_turn_requests" do
      steps =
        turn() ++
          [
            Flows.result(%{"subtype" => "error_max_turns"}),
            Flows.prompt("acp-prompt-2", "again"),
            Flows.result(%{"subtype" => "error_max_budget_usd"}),
            Flows.prompt("acp-prompt-3", "again"),
            Flows.result(%{"subtype" => "error_max_structured_output_retries"})
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "turn_limits_become_max_turn_requests", steps)

      assert ["max_turn_requests", "max_turn_requests", "max_turn_requests"] =
               stop_reasons(transcript)
    end

    test "an_errored_result_refuses" do
      steps =
        turn() ++ [Flows.result(%{"is_error" => true, "subtype" => "error_during_execution"})]

      ClaudeGolden.assert_golden(@area, "an_errored_result_refuses", steps)
    end

    test "an_auth_error_is_reported_in_meta" do
      steps =
        turn() ++
          [Flows.result(%{"error" => "authentication_failed", "is_error" => true})]

      transcript = ClaudeGolden.assert_golden(@area, "an_auth_error_is_reported_in_meta", steps)

      assert [%{"result" => %{"_meta" => %{"ex_mcp" => %{"claude_sdk" => meta}}}}] =
               prompt_responses(transcript)

      assert meta["authError"] == "authentication_failed"
    end

    test "cost_and_errors_travel_in_meta" do
      steps =
        turn() ++
          [
            Flows.result(%{
              "total_cost_usd" => 0.0123,
              "errors" => [%{"message" => "retried once"}],
              "modelUsage" => %{"sonnet" => %{"contextWindow" => 200_000}}
            })
          ]

      ClaudeGolden.assert_golden(@area, "cost_and_errors_travel_in_meta", steps)
    end

    test "a_result_without_a_pending_prompt_only_updates" do
      steps = [Flows.session_new(), Flows.result()]

      transcript =
        ClaudeGolden.assert_golden(@area, "a_result_without_a_pending_prompt_only_updates", steps)

      assert prompt_responses(transcript) == []
    end

    test "a_result_reports_fast_mode_state" do
      steps =
        turn() ++
          [
            Flows.result(%{"fast_mode_state" => "on"}),
            {:note, "cooldown still counts as enabled, off disables"},
            Flows.prompt("acp-prompt-2", "again"),
            Flows.result(%{"fast_mode_state" => "cooldown"}),
            Flows.prompt("acp-prompt-3", "again"),
            Flows.result(%{"fast_mode_state" => "off"})
          ]

      ClaudeGolden.assert_golden(@area, "a_result_reports_fast_mode_state", steps)
    end

    test "a_result_drains_the_queued_prompt" do
      steps =
        turn() ++
          [
            Flows.prompt("acp-prompt-2", "and then this"),
            {:note, "The queued prompt is written as soon as the first one settles"},
            Flows.result()
          ]

      transcript = ClaudeGolden.assert_golden(@area, "a_result_drains_the_queued_prompt", steps)

      assert %{tag: :messages_and_write, writes: [%{"type" => "user"}]} =
               ClaudeGolden.last_result(transcript)
    end
  end

  describe "system events" do
    test "system_init_emits_four_updates" do
      steps = [Flows.session_new(), Flows.system_init(%{"slash_commands" => ["review", "plan"]})]

      transcript = ClaudeGolden.assert_golden(@area, "system_init_emits_four_updates", steps)

      assert [
               "session_info_update",
               "current_mode_update",
               "config_option_update",
               "available_commands_update"
             ] = ClaudeGolden.update_types(transcript)
    end

    test "system_init_without_slash_commands_emits_three" do
      steps = [Flows.session_new(), Flows.system_init()]

      transcript =
        ClaudeGolden.assert_golden(@area, "system_init_without_slash_commands_emits_three", steps)

      assert length(ClaudeGolden.update_types(transcript)) == 3
    end

    test "system_init_adopts_the_reported_permission_mode" do
      steps = [Flows.session_new(), Flows.system_init(%{"permissionMode" => "acceptEdits"})]

      ClaudeGolden.assert_golden(@area, "system_init_adopts_the_reported_permission_mode", steps)
    end

    test "system_status_reports_compaction" do
      steps = [
        Flows.session_new(),
        Flows.system("status", %{
          "status" => "compacting",
          "compact_result" => "summarized 400 messages",
          "permissionMode" => "plan"
        })
      ]

      transcript = ClaudeGolden.assert_golden(@area, "system_status_reports_compaction", steps)

      assert ["session_info_update", "current_mode_update"] =
               ClaudeGolden.update_types(transcript)
    end

    test "session_state_changed_is_reported" do
      steps = [Flows.session_new(), Flows.system("session_state_changed", %{"state" => "busy"})]

      ClaudeGolden.assert_golden(@area, "session_state_changed_is_reported", steps)
    end

    test "commands_changed_replaces_the_catalog" do
      steps = [
        Flows.session_new(),
        Flows.system("commands_changed", %{
          "commands" => [
            "review",
            %{"id" => "plan"},
            %{"name" => "ship", "description" => "Ship"},
            7
          ]
        })
      ]

      ClaudeGolden.assert_golden(@area, "commands_changed_replaces_the_catalog", steps)
    end

    test "permission_denied_fails_the_tool_call" do
      steps = [
        Flows.session_new(),
        Flows.system("permission_denied", %{
          "tool_use_id" => "toolu_1",
          "tool_name" => "Bash",
          "message" => "Denied by policy",
          "decision_reason" => "hook",
          "decision_reason_type" => "permissionRule"
        })
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "permission_denied_fails_the_tool_call", steps)

      assert %{"status" => "failed"} = last_update(transcript)
    end

    test "an_unknown_system_subtype_is_forwarded_verbatim" do
      steps = [Flows.session_new(), Flows.system("weather_changed", %{"weather" => "rain"})]

      ClaudeGolden.assert_golden(@area, "an_unknown_system_subtype_is_forwarded_verbatim", steps)
    end

    test "task_events_emit_plans" do
      steps =
        turn() ++
          [
            Flows.system("task_started", %{
              "task_id" => "task-1",
              "subagent_type" => "explorer",
              "description" => "Explore the repo"
            }),
            Flows.system("task_progress", %{"task_id" => "task-1", "summary" => "Halfway"}),
            Flows.system("task_updated", %{
              "task_id" => "task-1",
              "patch" => %{"status" => "running", "description" => "Still going"}
            }),
            Flows.system("task_notification", %{
              "task_id" => "task-1",
              "status" => "completed",
              "summary" => "Explored"
            })
          ]

      transcript = ClaudeGolden.assert_golden(@area, "task_events_emit_plans", steps)

      assert ["plan", "plan", "plan", "plan"] = ClaudeGolden.update_types(transcript)
    end

    test "background_tasks_changed_emits_no_plan" do
      steps =
        turn() ++
          [
            Flows.system("task_started", %{
              "task_id" => "task-1",
              "subagent_type" => "explorer",
              "description" => "Explore"
            }),
            Flows.system("background_tasks_changed", %{"tasks" => []})
          ]

      ClaudeGolden.assert_golden(@area, "background_tasks_changed_emits_no_plan", steps)
    end

    test "a_result_waits_for_background_subagents" do
      steps =
        turn() ++
          [
            Flows.system("task_started", %{
              "task_id" => "task-1",
              "subagent_type" => "explorer",
              "description" => "Explore"
            }),
            {:note, "The result is deferred while the subagent is live"},
            Flows.result(),
            Flows.system("task_notification", %{"task_id" => "task-1", "status" => "completed"}),
            {:note, "The next result settles the deferred one"},
            Flows.result()
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "a_result_waits_for_background_subagents", steps)

      assert length(prompt_responses(transcript)) == 1
    end

    test "an_idle_session_state_settles_a_deferred_result" do
      steps =
        turn() ++
          [
            Flows.system("task_started", %{
              "task_id" => "task-1",
              "subagent_type" => "explorer",
              "description" => "Explore"
            }),
            Flows.result(),
            Flows.system("background_tasks_changed", %{"tasks" => []}),
            Flows.system("session_state_changed", %{"state" => "idle"})
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "an_idle_session_state_settles_a_deferred_result",
          steps
        )

      assert length(prompt_responses(transcript)) == 1
    end

    test "a_task_started_without_a_prompt_is_not_tracked" do
      steps = [
        Flows.session_new(),
        Flows.system("task_started", %{
          "task_id" => "task-1",
          "subagent_type" => "explorer",
          "description" => "Explore"
        }),
        Flows.prompt("acp-prompt", "hi"),
        {:note, "Nothing is deferred, so the result settles immediately"},
        Flows.result()
      ]

      ClaudeGolden.assert_golden(@area, "a_task_started_without_a_prompt_is_not_tracked", steps)
    end
  end

  describe "standalone updates" do
    test "tool_progress_updates_the_call" do
      steps =
        turn() ++
          [
            {:inbound,
             %{
               "type" => "tool_progress",
               "tool_use_id" => "toolu_1",
               "tool_name" => "Bash",
               "elapsed_time_seconds" => 12,
               "task_id" => "task-1"
             }}
          ]

      transcript = ClaudeGolden.assert_golden(@area, "tool_progress_updates_the_call", steps)

      assert %{"status" => "in_progress"} = last_update(transcript)
    end

    test "tool_use_summary_becomes_session_info" do
      steps =
        turn() ++
          [{:inbound, %{"type" => "tool_use_summary", "summary" => "Ran three commands"}}]

      ClaudeGolden.assert_golden(@area, "tool_use_summary_becomes_session_info", steps)
    end

    test "rate_limit_events_become_session_info" do
      steps =
        turn() ++
          [
            {:inbound,
             %{
               "type" => "rate_limit_event",
               "rate_limit_info" => %{"resetsAt" => "2026-01-01T00:00:00Z", "status" => "warning"}
             }}
          ]

      ClaudeGolden.assert_golden(@area, "rate_limit_events_become_session_info", steps)
    end

    test "unknown_and_blank_lines_are_skipped" do
      steps =
        turn() ++
          [
            {:inbound, %{"type" => "telemetry", "payload" => %{}}},
            {:inbound_raw, ""},
            {:inbound_raw, "   \n"},
            {:inbound_raw, "not json at all"},
            {:inbound, %{"no_type" => true}}
          ]

      transcript = ClaudeGolden.assert_golden(@area, "unknown_and_blank_lines_are_skipped", steps)

      assert ClaudeGolden.messages(transcript) == []
    end
  end

  # -- helpers ---------------------------------------------------------------

  defp turn do
    [Flows.session_new(), Flows.prompt("acp-prompt", "hi")]
  end

  defp message_ids(transcript) do
    transcript
    |> ClaudeGolden.updates()
    |> Enum.flat_map(fn update ->
      case get_in(update, ["params", "update", "messageId"]) do
        nil -> []
        message_id -> [message_id]
      end
    end)
  end

  defp last_update(transcript) do
    transcript |> ClaudeGolden.updates() |> List.last() |> get_in(["params", "update"])
  end

  defp prompt_responses(transcript) do
    transcript
    |> ClaudeGolden.messages()
    |> Enum.filter(&Map.has_key?(&1, "result"))
  end

  defp stop_reasons(transcript) do
    transcript |> prompt_responses() |> Enum.map(&get_in(&1, ["result", "stopReason"]))
  end

  defp quota(response), do: get_in(response, ["result", "_meta", "quota"])

  defp model_quota_inputs(transcript) do
    transcript
    |> prompt_responses()
    |> Enum.map(fn response ->
      response
      |> quota()
      |> Map.fetch!("model_usage")
      |> hd()
      |> get_in(["token_count", "inputTokens"])
    end)
  end
end
