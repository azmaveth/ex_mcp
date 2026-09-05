defmodule ExMCP.ACP.Adapters.Codex.SessionUpdatesGoldenTest do
  @moduledoc """
  Characterization gate for the Codex ACP adapter's session update stream
  (area A4 `session_updates`; see `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Codex
  adapter restructuring" / "Characterization gate").

  Each test drives `ExMCP.ACP.Adapters.Codex` through `ExMCP.Test.CodexGolden`
  across a whole prompt turn and compares the recorded transcript against a
  committed fixture under `test/fixtures/acp/codex/session_updates/`. The
  fixtures pin the ORDER and SHAPE of every ACP `session/update` the adapter
  emits between `turn/started` and `turn/completed` (agent message and
  reasoning chunks, tool call lifecycles for command execution, file changes,
  MCP, dynamic and web-search tools, plans, usage, rate limits, thread
  metadata, warnings and errors), the prompt settlement on `turn/completed`
  (success with `_meta.ex_mcp.text`/`usage`, cancelled, failed, errored, and
  capacity failures), and thread-history replay through `session/load`.

  Every scenario reaches its preconditions through the real flows (handshake,
  `session/new`, `thread/start` reply, `session/prompt`, `turn/start` reply,
  `turn/started`) rather than seeding adapter state.

  ## Resets on `turn/completed`

  After settling the prompt, `turn/completed` clears the session's
  `accumulated_text`, `streamed_items`, `accumulated_thinking`,
  `accumulated_usage`, `turn_id`, `active_prompt_acp_id` and `last_error`.
  Most of those resets are observable and pinned by the settlement scenarios
  below:

    * `accumulated_usage` - a `/status` prompt on the settled session reads it
      without going through the per-prompt reset
      (`status_command_after_turn_completed_reports_no_usage`).
    * `turn_id` - a late `session/cancel` finds no turn to interrupt
      (`session_cancel_after_turn_completed_has_no_active_turn`).
    * `streamed_items` - only a stray agent-message completion arriving after
      the settlement and before the next prompt can observe it
      (`stale_agent_message_completed_after_turn_completed_emits_full_text`).
    * `active_prompt_acp_id` and `last_error` - pinned by
      `stray_turn_completed_after_settled_prompt_emits_status_only` and
      `turn_completed_clears_stale_error_notification_for_next_prompt`.

  ## Known non-pinnable

  The following internals are known to be non-pinnable through public entry
  points, so silence from this file about them is expected, not a coverage
  gap. Each was checked against the adapter source (and, where a sequence
  could plausibly observe it, probed with a throwaway scenario):

    * `accumulated_text` reset on `turn/completed`: it is read only when
      `turn/completed` settles an active prompt, and every path that activates
      a prompt (`session/prompt`, `/compact`, `/review*`, `/logout`) resets it
      first. A refactor that stops clearing it on `turn/completed` is
      wire-equivalent.
    * `accumulated_thinking` reset on `turn/completed`: the accumulator is
      written by reasoning deltas and the two reset blocks but never read
      anywhere under `lib/` (`grep -rn accumulated_thinking lib/` shows only
      writes), so neither reset is observable.
    * `normalize_stop_reason("errored")`: `turn_failure/3` settles every turn
      whose status is `"failed"` or `"errored"` with an error response before
      `normalize_stop_reason/1` runs, so the `"errored"` clause is dead. A
      probe sending `turn/completed` with status `"errored"` (with and without
      `turn.error`, with and without a remembered error notification) always
      produced the error response pinned by
      `errored_turn_without_error_detail_fails_prompt`, never a `stopReason`.
    * mcpToolCall `item/completed` output fallback order
      (`item["result"] || item["error"]`): `Events.mcp_raw_output/1` returns
      `nil` only when both keys are present and `nil`, in which case either
      order yields the same `%{}` `rawOutput`; when either key is missing or
      set, `mcp_raw_output/1` wins and the fallback is never consulted
      (`mcp_tool_call_error_and_empty_outputs` pins the `nil`/`nil` case).

  To regenerate the fixtures after an intentional behavior change, run

      CODEX_GOLDEN=update mix test test/ex_mcp/acp/adapters/codex/characterization/session_updates_golden_test.exs

  That run rewrites the fixtures and fails on purpose; review the diff and
  re-run without the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.CodexGolden

  @area "session_updates"
  @session "thread-abc"
  @prompt_id 20

  # -- agent messages -------------------------------------------------------

  test "agent_message_streamed_then_completed_dedupes_final_chunk" do
    steps =
      turn_steps("Say hello") ++
        [
          agent_delta("msg-1", "Hello"),
          agent_delta("msg-1", " world"),
          {:note,
           "item/completed repeats the full text; only the unstreamed remainder (here nothing) is emitted, marked final"},
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello world"}),
          token_usage(%{"inputTokens" => 12, "outputTokens" => 3}, 272_000, %{
            "inputTokens" => 12,
            "outputTokens" => 3,
            "cachedInputTokens" => 0
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "agent_message_streamed_then_completed_dedupes_final_chunk",
        steps
      )

    assert [
             %{"params" => %{"update" => %{"content" => %{"text" => "Hello"}}}},
             %{"params" => %{"update" => %{"content" => %{"text" => " world"}}}},
             %{
               "params" => %{
                 "update" => %{
                   "content" => %{"text" => ""},
                   "_meta" => %{"ex_mcp" => %{"final" => true}}
                 }
               }
             },
             %{"params" => %{"update" => %{"sessionUpdate" => "usage_update", "used" => 15}}},
             %{"params" => %{"update" => %{"sessionUpdate" => "session_info_update"}}},
             %{"id" => @prompt_id, "result" => result}
           ] = turn_messages(transcript)

    assert result["stopReason"] == "end_turn"
    assert result["_meta"]["ex_mcp"]["text"] == "Hello world"

    assert result["usage"] == %{
             "inputTokens" => 12,
             "outputTokens" => 3,
             "cachedInputTokens" => 0
           }
  end

  test "agent_message_completed_extends_stream_with_remainder" do
    steps =
      turn_steps("Say hello") ++
        [
          agent_delta("msg-1", "Hel"),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "agent_message_completed_extends_stream_with_remainder",
        steps
      )

    assert [_hel, %{"params" => %{"update" => %{"content" => %{"text" => "lo"}}}}, _, _] =
             turn_messages(transcript)

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Hello"}}}} =
             List.last(turn_messages(transcript))
  end

  test "agent_message_completed_without_deltas_emits_full_text" do
    steps =
      turn_steps("Say hello") ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "PONG"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "agent_message_completed_without_deltas_emits_full_text",
        steps
      )

    assert [%{"params" => %{"update" => %{"content" => %{"text" => "PONG"}}}} | _] =
             turn_messages(transcript)

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "PONG"}}}} =
             List.last(turn_messages(transcript))
  end

  test "agent_message_completed_diverging_from_stream_emits_empty_chunk" do
    steps =
      turn_steps("Say hello") ++
        [
          agent_delta("msg-1", "Hello"),
          {:note,
           "completed text is not an extension of the stream: the adapter trusts the stream and emits an empty final chunk"},
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Goodbye"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "agent_message_completed_diverging_from_stream_emits_empty_chunk",
        steps
      )

    assert [_hello, %{"params" => %{"update" => %{"content" => %{"text" => ""}}}}, _, _] =
             turn_messages(transcript)

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Hello"}}}} =
             List.last(turn_messages(transcript))
  end

  test "multi_message_turn_emits_messages_in_order" do
    steps =
      turn_steps("Say several things") ++
        [
          agent_delta("msg-1", "Hello"),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          item_started(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "ls",
            "cwd" => "/tmp/project",
            "status" => "inProgress"
          }),
          item_completed(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "ls",
            "status" => "completed",
            "exitCode" => 0,
            "aggregatedOutput" => "lib\nmix.exs\n"
          }),
          item_completed(%{"type" => "agentMessage", "id" => "msg-2", "text" => "World"}),
          agent_delta("msg-3", "Aga"),
          item_completed(%{"type" => "agentMessage", "id" => "msg-3", "text" => "Again"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "multi_message_turn_emits_messages_in_order", steps)

    texts =
      transcript
      |> CodexGolden.messages()
      |> Enum.flat_map(fn
        %{"params" => %{"update" => %{"sessionUpdate" => "agent_message_chunk"} = update}} ->
          [update["content"]["text"]]

        _ ->
          []
      end)

    assert texts == ["Hello", "", "World", "Aga", "in"]

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "HelloWorldAgain"}}}} =
             List.last(turn_messages(transcript))
  end

  test "interleaved_agent_message_deltas_keep_per_item_remainders" do
    steps =
      turn_steps("Say two things at once") ++
        [
          agent_delta("msg-1", "Hello"),
          {:note,
           "a delta for a second item arrives before the first item completes: streams are tracked per item, so msg-1 completes with nothing left and msg-2 emits only its own unstreamed remainder"},
          agent_delta("msg-2", "Wor"),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          item_completed(%{"type" => "agentMessage", "id" => "msg-2", "text" => "World"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "interleaved_agent_message_deltas_keep_per_item_remainders",
        steps
      )

    assert [
             %{"params" => %{"update" => %{"content" => %{"text" => "Hello"}}}},
             %{"params" => %{"update" => %{"content" => %{"text" => "Wor"}}}},
             %{
               "params" => %{
                 "update" => %{
                   "content" => %{"text" => ""},
                   "_meta" => %{"ex_mcp" => %{"final" => true}}
                 }
               }
             },
             %{
               "params" => %{
                 "update" => %{
                   "content" => %{"text" => "ld"},
                   "_meta" => %{"ex_mcp" => %{"final" => true}}
                 }
               }
             },
             _status,
             %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "HelloWorld"}}}}
           ] = turn_messages(transcript)
  end

  test "legacy_agent_message_delta_without_item_id_uses_current_slot" do
    steps =
      turn_steps("Say hello") ++
        [
          {:note,
           "legacy agent_message/delta without itemId streams into the :current slot, which the next completed message consumes"},
          notify("agent_message/delta", %{"threadId" => @session, "delta" => "Hel"}),
          notify("item/agentMessage/delta", %{"threadId" => @session}),
          item_completed(%{"type" => "agent_message", "id" => "msg-1", "message" => "Hello"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "legacy_agent_message_delta_without_item_id_uses_current_slot",
        steps
      )

    assert [
             %{"params" => %{"update" => %{"content" => %{"text" => "Hel"}}}},
             %{"params" => %{"update" => %{"content" => %{"text" => ""}}}},
             %{"params" => %{"update" => %{"content" => %{"text" => "lo"}}}},
             _status,
             %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Hello"}}}}
           ] = turn_messages(transcript)
  end

  test "mixed_current_and_item_keyed_deltas_consumed_by_first_completion" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("item/agentMessage/delta", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "delta" => "X"
          }),
          agent_delta("msg-1", "Hello"),
          {:note,
           "the first completed message consumes BOTH its own item slot and the id-less :current slot, so a later message whose text equals the :current stream is emitted in full and accumulated again"},
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          item_completed(%{"type" => "agentMessage", "id" => "msg-2", "text" => "X"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "mixed_current_and_item_keyed_deltas_consumed_by_first_completion",
        steps
      )

    chunks =
      transcript
      |> updates()
      |> Enum.filter(&(&1["sessionUpdate"] == "agent_message_chunk"))
      |> Enum.map(& &1["content"]["text"])

    assert chunks == ["X", "Hello", "", "X"]

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "XHelloX"}}}} =
             List.last(turn_messages(transcript))
  end

  test "mixed_slots_completion_dedupes_against_item_slot_not_current" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("item/agentMessage/delta", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "delta" => "X"
          }),
          agent_delta("msg-1", "Hello"),
          {:note,
           "with both an id-less :current stream and an item-keyed stream, the completed item is compared against ITS OWN slot: the text extends \"Hello\", so the remainder is emitted rather than the empty chunk a comparison against the diverging :current stream (\"X\") would produce"},
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello world"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "mixed_slots_completion_dedupes_against_item_slot_not_current",
        steps
      )

    chunks =
      transcript
      |> updates()
      |> Enum.filter(&(&1["sessionUpdate"] == "agent_message_chunk"))
      |> Enum.map(& &1["content"]["text"])

    assert chunks == ["X", "Hello", " world"]

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "XHello world"}}}} =
             List.last(turn_messages(transcript))
  end

  # -- reasoning ------------------------------------------------------------

  test "reasoning_deltas_summary_parts_and_completion" do
    steps =
      turn_steps("Think about it") ++
        [
          notify("item/reasoning/textDelta", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "delta" => "Considering "
          }),
          notify("item/reasoning/summaryTextDelta", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "delta" => "the options"
          }),
          notify("item/reasoning/summaryPartAdded", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "text" => "**Weighing tradeoffs**"
          }),
          notify("item/reasoning/summaryPartAdded", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "summary" => "**Deciding**"
          }),
          {:note,
           "reasoning item/completed reads content before summary, so an empty content list yields an empty final chunk (reasoning-1) while a summary-only item re-emits the joined summary (reasoning-2); streamed reasoning is never deduplicated and never reaches the prompt result"},
          item_completed(%{
            "type" => "reasoning",
            "id" => "reasoning-1",
            "summary" => ["**Weighing tradeoffs**", "**Deciding**"],
            "content" => []
          }),
          item_completed(%{
            "type" => "reasoning",
            "id" => "reasoning-2",
            "summary" => ["**Weighing tradeoffs**", "**Deciding**"]
          }),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Done"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "reasoning_deltas_summary_parts_and_completion", steps)

    thoughts =
      transcript
      |> CodexGolden.messages()
      |> Enum.filter(
        &(get_in(&1, ["params", "update", "sessionUpdate"]) == "agent_thought_chunk")
      )

    assert length(thoughts) == 6

    assert %{"_meta" => %{"ex_mcp" => %{"final" => true}}, "content" => %{"text" => ""}} =
             get_in(Enum.at(thoughts, 4), ["params", "update"])

    assert %{"content" => %{"text" => "**Weighing tradeoffs**\n**Deciding**"}} =
             get_in(Enum.at(thoughts, 5), ["params", "update"])

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Done"}}}} =
             List.last(turn_messages(transcript))
  end

  test "reasoning_summary_part_key_and_text_fallbacks" do
    steps =
      turn_steps("Think about it") ++
        [
          notify("item/reasoning/summaryPartAdded", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "part" => "**Planning**"
          }),
          {:note,
           "summaryPartAdded without text/summary/part and textDelta without delta/text both emit an empty thought chunk; textDelta falls back to the `text` key"},
          notify("item/reasoning/summaryPartAdded", %{
            "threadId" => @session,
            "itemId" => "reasoning-1"
          }),
          notify("item/reasoning/textDelta", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "text" => "Still thinking"
          }),
          notify("item/reasoning/textDelta", %{"threadId" => @session, "itemId" => "reasoning-1"}),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Done"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "reasoning_summary_part_key_and_text_fallbacks", steps)

    thoughts =
      transcript
      |> updates()
      |> Enum.filter(&(&1["sessionUpdate"] == "agent_thought_chunk"))
      |> Enum.map(& &1["content"]["text"])

    assert thoughts == ["**Planning**", "", "Still thinking", ""]

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Done"}}}} =
             List.last(turn_messages(transcript))
  end

  test "reasoning_summary_part_added_text_beats_summary" do
    steps =
      turn_steps("Think about it") ++
        [
          {:note,
           "summaryPartAdded reads text, then summary, then part: with all three present the text wins; with summary and part the summary wins"},
          notify("item/reasoning/summaryPartAdded", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "text" => "**From text**",
            "summary" => "**From summary**",
            "part" => "**From part**"
          }),
          notify("item/reasoning/summaryPartAdded", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "summary" => "**From summary**",
            "part" => "**From part**"
          }),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Done"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "reasoning_summary_part_added_text_beats_summary", steps)

    thoughts =
      transcript
      |> updates()
      |> Enum.filter(&(&1["sessionUpdate"] == "agent_thought_chunk"))
      |> Enum.map(& &1["content"]["text"])

    assert thoughts == ["**From text**", "**From summary**"]
  end

  # -- command execution ----------------------------------------------------

  test "command_execution_lifecycle_with_terminal_metadata" do
    steps =
      turn_steps("Run the tests") ++
        [
          item_started(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "mix test",
            "cwd" => "/tmp/project",
            "status" => "inProgress"
          }),
          notify("item/commandExecution/outputDelta", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-1",
            "delta" => "Compiling 3 files\n"
          }),
          notify("item/commandExecution/outputDelta", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-1",
            "delta" => "3 tests, 0 failures\n"
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-1",
            "stdin" => "y\n"
          }),
          {:note,
           "both terminal updates can arrive for the same item: the per-item completed notification and the generic item/completed"},
          notify("item/commandExecution/completed", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-1",
            "exitCode" => 0,
            "output" => "Compiling 3 files\n3 tests, 0 failures\n"
          }),
          item_completed(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "mix test",
            "cwd" => "/tmp/project",
            "status" => "completed",
            "exitCode" => 0,
            "aggregatedOutput" => "Compiling 3 files\n3 tests, 0 failures\n"
          }),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "All green."}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "command_execution_lifecycle_with_terminal_metadata",
        steps
      )

    [started | _] = turn_messages(transcript)

    assert %{
             "sessionUpdate" => "tool_call",
             "toolCallId" => "cmd-1",
             "status" => "in_progress",
             "content" => [%{"type" => "terminal", "terminalId" => "cmd-1"}],
             "_meta" => %{"terminal_info" => %{"terminal_id" => "cmd-1", "cwd" => "/tmp/project"}}
           } = started["params"]["update"]

    assert Enum.count(turn_messages(transcript), fn message ->
             get_in(message, ["params", "update", "_meta", "terminal_exit", "exit_code"]) == 0
           end) == 2
  end

  test "command_execution_failed_exit_code_and_status_aliases" do
    steps =
      turn_steps("Run the tests") ++
        [
          item_started(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "mix test",
            "cwd" => "/tmp/project",
            "status" => "queued"
          }),
          notify("item/commandExecution/outputDelta", %{
            "threadId" => @session,
            "itemId" => "cmd-1",
            "delta" => "1 test, 1 failure\n"
          }),
          item_completed(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "mix test",
            "status" => "failed",
            "exitCode" => 1,
            "aggregatedOutput" => "1 test, 1 failure\n"
          }),
          {:note, "a declined command has no exit code; declined normalizes to failed"},
          item_started(%{
            "type" => "commandExecution",
            "id" => "cmd-2",
            "command" => "rm -rf /",
            "cwd" => "/tmp/project",
            "status" => "running"
          }),
          item_completed(%{
            "type" => "commandExecution",
            "id" => "cmd-2",
            "command" => "rm -rf /",
            "status" => "declined"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "command_execution_failed_exit_code_and_status_aliases",
        steps
      )

    updates = updates(transcript)

    assert %{"status" => "pending"} = Enum.at(updates, 0)
    assert %{"status" => "failed", "rawOutput" => %{"exit_code" => 1}} = Enum.at(updates, 2)
    assert %{"status" => "failed", "rawOutput" => %{"exit_code" => nil}} = Enum.at(updates, 4)
  end

  test "command_execution_call_id_precedence_and_terminal_interaction_fallbacks" do
    steps =
      turn_steps("Run an interactive command") ++
        [
          {:note,
           "item/started resolves the tool call id from params.itemId before item.callId and item.id; without it item.callId beats item.id"},
          notify("item/started", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "override-1",
            "item" => %{
              "type" => "commandExecution",
              "id" => "cmd-1",
              "callId" => "call-1",
              "command" => "python3 -i",
              "cwd" => "/tmp/project",
              "status" => "inProgress"
            }
          }),
          item_started(%{
            "type" => "commandExecution",
            "id" => "cmd-2",
            "callId" => "call-2",
            "command" => "python3 -i",
            "cwd" => "/tmp/project",
            "status" => "inProgress"
          }),
          {:note,
           "the per-item notifications prefer callId over itemId; terminalInteraction accepts stdin, text, input or delta alone and JSON-encodes the raw params when none is present; the two-key payloads below pin the pairwise order stdin > text > input > delta"},
          notify("item/commandExecution/outputDelta", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "callId" => "call-2",
            "delta" => ">>> "
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "callId" => "call-2",
            "text" => "print(1)"
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "input" => "print(2)"
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "delta" => "print(3)"
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "keys" => ["ctrl-d"]
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "stdin" => "print(4)",
            "text" => "print(4) via text"
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "text" => "print(5)",
            "input" => "print(5) via input"
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "input" => "print(6)",
            "delta" => "print(6) via delta"
          }),
          notify("item/commandExecution/completed", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "itemId" => "cmd-2",
            "callId" => "call-2",
            "exitCode" => 0,
            "output" => ">>> 1\n2\n3\n4\n5\n6\n"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "command_execution_call_id_precedence_and_terminal_interaction_fallbacks",
        steps
      )

    tool_updates = Enum.filter(updates(transcript), &Map.has_key?(&1, "toolCallId"))

    assert Enum.map(tool_updates, & &1["toolCallId"]) ==
             [
               "override-1",
               "call-2",
               "call-2",
               "call-2",
               "cmd-2",
               "cmd-2",
               "cmd-2",
               "cmd-2",
               "cmd-2",
               "cmd-2",
               "call-2"
             ]

    assert Enum.map(Enum.slice(tool_updates, 3, 7), fn update ->
             get_in(update, ["_meta", "terminal_output_delta", "data"])
           end) == [
             "\nprint(1)\n",
             "\nprint(2)\n",
             "\nprint(3)\n",
             "\n{\"itemId\":\"cmd-2\",\"keys\":[\"ctrl-d\"],\"threadId\":\"thread-abc\",\"turnId\":\"turn-1\"}\n",
             "\nprint(4)\n",
             "\nprint(5)\n",
             "\nprint(6)\n"
           ]
  end

  test "legacy_command_execution_notifications" do
    steps =
      turn_steps("Run the tests") ++
        [
          notify("item/commandExecution/started", %{
            "threadId" => @session,
            "itemId" => "cmd-1",
            "command" => "mix test"
          }),
          notify("item/commandExecution/completed", %{
            "threadId" => @session,
            "itemId" => "cmd-1",
            "exitCode" => 0,
            "output" => "ok"
          }),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "legacy_command_execution_notifications", steps)

    [started, completed | _] = turn_messages(transcript)
    assert %{"sessionUpdate" => "tool_call", "kind" => "execute"} = started["params"]["update"]
    refute Map.has_key?(started["params"]["update"], "content")

    assert %{"rawOutput" => %{"exit_code" => 0, "formatted_output" => "ok"}} =
             completed["params"]["update"]
  end

  test "legacy_command_execution_started_prefers_call_id" do
    steps =
      turn_steps("Run the tests") ++
        [
          {:note,
           "every legacy commandExecution notification, started included, resolves the tool call id from callId before itemId, so the whole lifecycle lands on call-1"},
          notify("item/commandExecution/started", %{
            "threadId" => @session,
            "itemId" => "cmd-1",
            "callId" => "call-1",
            "command" => "mix test"
          }),
          notify("item/commandExecution/outputDelta", %{
            "threadId" => @session,
            "itemId" => "cmd-1",
            "callId" => "call-1",
            "delta" => "3 tests, 0 failures\n"
          }),
          notify("item/commandExecution/completed", %{
            "threadId" => @session,
            "itemId" => "cmd-1",
            "callId" => "call-1",
            "exitCode" => 0,
            "output" => "3 tests, 0 failures\n"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "legacy_command_execution_started_prefers_call_id", steps)

    tool_updates = Enum.filter(updates(transcript), &Map.has_key?(&1, "toolCallId"))

    assert Enum.map(tool_updates, &{&1["sessionUpdate"], &1["toolCallId"]}) == [
             {"tool_call", "call-1"},
             {"tool_call_update", "call-1"},
             {"tool_call_update", "call-1"}
           ]
  end

  test "snake_case_item_id_alias_on_command_execution_notifications" do
    steps =
      turn_steps("Run the tests") ++
        [
          item_started(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "mix test",
            "cwd" => "/tmp/project",
            "status" => "inProgress"
          }),
          {:note,
           "outputDelta and terminalInteraction accept a snake_case item_id as the last-resort tool call id; item/commandExecution/completed does not read it, so its toolCallId is null"},
          notify("item/commandExecution/outputDelta", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "item_id" => "cmd-1",
            "delta" => "Compiling 3 files\n"
          }),
          notify("item/commandExecution/terminalInteraction", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "item_id" => "cmd-1",
            "stdin" => "y\n"
          }),
          notify("item/commandExecution/completed", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "item_id" => "cmd-1",
            "exitCode" => 0,
            "output" => "Compiling 3 files\n"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "snake_case_item_id_alias_on_command_execution_notifications",
        steps
      )

    tool_updates = Enum.filter(updates(transcript), &Map.has_key?(&1, "toolCallId"))

    assert Enum.map(tool_updates, & &1["toolCallId"]) == ["cmd-1", "cmd-1", "cmd-1", nil]

    assert %{
             "_meta" => %{
               "terminal_output_delta" => %{"terminal_id" => "cmd-1", "data" => "\ny\n\n"}
             }
           } =
             Enum.at(tool_updates, 2)
  end

  test "command_execution_completed_reads_only_aggregated_output" do
    steps =
      turn_steps("Run the tests") ++
        [
          item_started(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "mix test",
            "cwd" => "/tmp/project",
            "status" => "inProgress"
          }),
          {:note,
           "the generic item/completed reads aggregatedOutput only: a legacy output key on the item is ignored and formatted_output stays empty"},
          item_completed(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "mix test",
            "status" => "completed",
            "exitCode" => 0,
            "output" => "3 tests, 0 failures\n"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "command_execution_completed_reads_only_aggregated_output",
        steps
      )

    assert [
             %{"sessionUpdate" => "tool_call", "toolCallId" => "cmd-1"},
             %{
               "sessionUpdate" => "tool_call_update",
               "toolCallId" => "cmd-1",
               "status" => "completed",
               "rawOutput" => %{"exit_code" => 0, "formatted_output" => ""}
             }
             | _
           ] = updates(transcript)
  end

  # -- file changes ---------------------------------------------------------

  test "file_change_lifecycle_with_patch_updates" do
    steps =
      turn_steps("Fix the bug") ++
        [
          item_started(%{
            "type" => "fileChange",
            "id" => "edit-1",
            "status" => "inProgress",
            "changes" => [%{"path" => "lib/a.ex", "kind" => "update", "diff" => "@@ -1 +1 @@"}]
          }),
          notify("item/fileChange/outputDelta", %{
            "threadId" => @session,
            "itemId" => "edit-1",
            "delta" => "Applying patch\n"
          }),
          notify("item/fileChange/patchUpdated", %{
            "threadId" => @session,
            "itemId" => "edit-1",
            "changes" => [
              %{"path" => "lib/a.ex", "newText" => "first"},
              %{"path" => "lib/b.ex", "diff" => "second"}
            ]
          }),
          {:note,
           "changes with neither newText nor diff fall back to a text block of the raw change"},
          item_completed(%{
            "type" => "fileChange",
            "id" => "edit-1",
            "status" => "completed",
            "changes" => [
              %{"path" => "lib/a.ex", "newText" => "first"},
              %{"path" => "lib/b.ex", "diff" => "second"},
              %{"path" => "lib/c.ex", "kind" => "delete"}
            ]
          }),
          item_completed(%{"type" => "fileChange", "id" => "edit-2", "status" => "failed"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "file_change_lifecycle_with_patch_updates", steps)

    updates = updates(transcript)

    assert %{"sessionUpdate" => "tool_call", "title" => "Edit File", "status" => "in_progress"} =
             Enum.at(updates, 0)

    assert [%{"type" => "diff"}, %{"type" => "diff"}, %{"type" => "content"}] =
             Enum.at(updates, 3)["content"]

    assert %{"status" => "failed", "content" => [], "rawOutput" => %{"changes" => []}} =
             Enum.at(updates, 4)
  end

  test "file_change_output_delta_alias_precedence_and_mcp_progress_raw_fallback" do
    steps =
      turn_steps("Fix the bug") ++
        [
          item_started(%{
            "type" => "fileChange",
            "id" => "edit-1",
            "status" => "inProgress",
            "changes" => [%{"path" => "lib/a.ex", "kind" => "update", "diff" => "@@ -1 +1 @@"}]
          }),
          {:note,
           "item/fileChange/outputDelta accepts the legacy output key alone and, with two keys present, reads delta before text and text before output"},
          notify("item/fileChange/outputDelta", %{
            "threadId" => @session,
            "itemId" => "edit-1",
            "output" => "Applying patch\n"
          }),
          notify("item/fileChange/outputDelta", %{
            "threadId" => @session,
            "itemId" => "edit-1",
            "delta" => "delta wins\n",
            "text" => "text loses\n"
          }),
          notify("item/fileChange/outputDelta", %{
            "threadId" => @session,
            "itemId" => "edit-1",
            "text" => "text wins\n",
            "output" => "output loses\n"
          }),
          item_started(%{
            "type" => "mcpToolCall",
            "id" => "mcp-1",
            "server" => "repo",
            "tool" => "search",
            "arguments" => %{"query" => "defmodule"},
            "status" => "inProgress"
          }),
          {:note,
           "item/mcpToolCall/progress with neither message, delta nor a progress map JSON-encodes the whole params as the output delta"},
          notify("item/mcpToolCall/progress", %{
            "threadId" => @session,
            "itemId" => "mcp-1",
            "percent" => 50
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "file_change_output_delta_alias_precedence_and_mcp_progress_raw_fallback",
        steps
      )

    updates = updates(transcript)

    assert Enum.map(Enum.slice(updates, 1, 3), &hd(&1["content"])["content"]["text"]) == [
             "Applying patch\n",
             "delta wins\n",
             "text wins\n"
           ]

    assert %{
             "toolCallId" => "mcp-1",
             "_meta" => %{
               "mcp_output_delta" => %{
                 "data" => "{\"itemId\":\"mcp-1\",\"percent\":50,\"threadId\":\"thread-abc\"}"
               }
             }
           } = Enum.at(updates, 5)
  end

  test "legacy_patch_created_and_patch_item_completed" do
    steps =
      turn_steps("Fix the bug") ++
        [
          notify("item/patch/created", %{
            "threadId" => @session,
            "patch" => %{"id" => "patch-1", "path" => "lib/a.ex", "diff" => "+ok"}
          }),
          notify("item/fileChange/patchUpdated", %{
            "threadId" => @session,
            "patch" => %{"id" => "patch-1", "path" => "lib/a.ex", "text" => "+ok\n+more"}
          }),
          notify("item/fileChange/patchUpdated", %{
            "threadId" => @session,
            "itemId" => "patch-1",
            "path" => "lib/a.ex",
            "delta" => "+final"
          }),
          item_completed(%{
            "type" => "patch",
            "id" => "patch-1",
            "path" => "lib/a.ex",
            "diff" => "+final"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "legacy_patch_created_and_patch_item_completed", steps)

    updates = updates(transcript)

    assert %{"sessionUpdate" => "tool_call", "status" => "pending", "toolCallId" => "patch-1"} =
             Enum.at(updates, 0)

    assert [%{"type" => "diff", "newText" => "+ok\n+more"}] = Enum.at(updates, 1)["content"]
    assert [%{"type" => "diff", "newText" => "+final"}] = Enum.at(updates, 3)["content"]
  end

  test "patch_item_completed_text_fallback_and_call_id_precedence" do
    steps =
      turn_steps("Fix the bug") ++
        [
          {:note,
           "a legacy patch item names its tool call by callId before id and falls back from diff to text for the diff block; with neither the newText is empty"},
          item_completed(%{
            "type" => "patch",
            "id" => "patch-1",
            "callId" => "call-1",
            "path" => "lib/a.ex",
            "text" => "+text form"
          }),
          item_completed(%{"type" => "patch", "id" => "patch-2", "path" => "lib/b.ex"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "patch_item_completed_text_fallback_and_call_id_precedence",
        steps
      )

    assert [
             %{
               "toolCallId" => "call-1",
               "kind" => "edit",
               "status" => "completed",
               "content" => [%{"type" => "diff", "path" => "lib/a.ex", "newText" => "+text form"}]
             },
             %{
               "toolCallId" => "patch-2",
               "content" => [%{"type" => "diff", "path" => "lib/b.ex", "newText" => ""}]
             }
             | _
           ] = updates(transcript)
  end

  test "patch_created_item_id_fallback_and_precedence" do
    steps =
      turn_steps("Fix the bug") ++
        [
          {:note,
           "item/patch/created names the tool call by patch.id before params.itemId; without a patch map the params themselves are the patch, so itemId is the fallback id and path/diff are read flat"},
          notify("item/patch/created", %{
            "threadId" => @session,
            "itemId" => "override-1",
            "patch" => %{"id" => "patch-1", "path" => "lib/a.ex", "diff" => "+ok"}
          }),
          notify("item/patch/created", %{
            "threadId" => @session,
            "itemId" => "patch-2",
            "path" => "lib/b.ex",
            "diff" => "+flat"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "patch_created_item_id_fallback_and_precedence", steps)

    assert [
             %{
               "sessionUpdate" => "tool_call",
               "toolCallId" => "patch-1",
               "rawInput" => %{"path" => "lib/a.ex", "diff" => "+ok"}
             },
             %{
               "sessionUpdate" => "tool_call",
               "toolCallId" => "patch-2",
               "rawInput" => %{"path" => "lib/b.ex", "diff" => "+flat"}
             }
             | _
           ] = updates(transcript)
  end

  # -- MCP, dynamic, web search, function call and image tools -------------

  test "mcp_tool_call_progress_and_result" do
    steps =
      turn_steps("Search the repo") ++
        [
          item_started(%{
            "type" => "mcpToolCall",
            "id" => "mcp-1",
            "server" => "repo",
            "tool" => "search",
            "arguments" => %{"query" => "defmodule"},
            "status" => "inProgress"
          }),
          notify("item/mcpToolCall/progress", %{
            "threadId" => @session,
            "itemId" => "mcp-1",
            "message" => "Indexing 12 files  "
          }),
          notify("item/mcpToolCall/progress", %{
            "threadId" => @session,
            "itemId" => "mcp-1",
            "delta" => "Matched 3 files"
          }),
          notify("item/mcpToolCall/progress", %{
            "threadId" => @session,
            "itemId" => "mcp-1",
            "progress" => %{"progress" => 3, "total" => 3}
          }),
          item_completed(%{
            "type" => "mcpToolCall",
            "id" => "mcp-1",
            "server" => "repo",
            "tool" => "search",
            "arguments" => %{"query" => "defmodule"},
            "status" => "completed",
            "result" => %{"content" => [%{"type" => "text", "text" => "lib/a.ex"}]},
            "error" => nil
          }),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "mcp_tool_call_progress_and_result", steps)

    updates = updates(transcript)

    assert %{"title" => "mcp.repo.search", "_meta" => %{"is_mcp_tool_call" => true}} =
             Enum.at(updates, 0)

    assert %{"_meta" => %{"mcp_output_delta" => %{"data" => "Indexing 12 files"}}} =
             Enum.at(updates, 1)

    assert %{"status" => "completed", "rawOutput" => %{"result" => %{}, "error" => nil}} =
             Enum.at(updates, 4)
  end

  test "mcp_tool_call_error_and_empty_outputs" do
    steps =
      turn_steps("Search the repo") ++
        [
          item_started(%{
            "type" => "mcpToolCall",
            "id" => "mcp-1",
            "server" => "repo",
            "tool" => "search",
            "arguments" => %{"query" => "defmodule"},
            "status" => "inProgress"
          }),
          item_completed(%{
            "type" => "mcpToolCall",
            "id" => "mcp-1",
            "server" => "repo",
            "tool" => "search",
            "arguments" => %{"query" => "defmodule"},
            "status" => "failed",
            "result" => nil,
            "error" => %{"message" => "server disconnected"}
          }),
          {:note,
           "a completed MCP call whose result and error are both null falls back to an empty rawOutput"},
          item_started(%{"type" => "mcpToolCall", "id" => "mcp-2", "tool" => "ping"}),
          item_completed(%{
            "type" => "mcpToolCall",
            "id" => "mcp-2",
            "tool" => "ping",
            "result" => nil,
            "error" => nil
          }),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "mcp_tool_call_error_and_empty_outputs", steps)

    updates = updates(transcript)

    assert %{
             "status" => "failed",
             "rawOutput" => %{"error" => %{"message" => "server disconnected"}}
           } =
             Enum.at(updates, 1)

    assert %{"title" => "ping"} = Enum.at(updates, 2)
    assert %{"status" => "completed", "rawOutput" => raw_output} = Enum.at(updates, 3)
    assert raw_output == %{}
  end

  test "mcp_tool_call_error_without_status_is_failed" do
    steps =
      turn_steps("Search the repo") ++
        [
          item_started(%{
            "type" => "mcpToolCall",
            "id" => "mcp-1",
            "server" => "repo",
            "tool" => "search",
            "arguments" => %{"query" => "defmodule"},
            "status" => "inProgress"
          }),
          {:note,
           "no explicit status: the presence of an error map alone decides the terminal status (failed); a null error defaults to completed"},
          item_completed(%{
            "type" => "mcpToolCall",
            "id" => "mcp-1",
            "server" => "repo",
            "tool" => "search",
            "arguments" => %{"query" => "defmodule"},
            "result" => nil,
            "error" => %{"code" => -32_000, "message" => "tool crashed"}
          }),
          item_completed(%{
            "type" => "mcpToolCall",
            "id" => "mcp-2",
            "server" => "repo",
            "tool" => "ping",
            "result" => %{"content" => []},
            "error" => nil
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "mcp_tool_call_error_without_status_is_failed", steps)

    assert [
             %{"sessionUpdate" => "tool_call", "toolCallId" => "mcp-1"},
             %{
               "toolCallId" => "mcp-1",
               "status" => "failed",
               "rawOutput" => %{"result" => nil, "error" => %{"message" => "tool crashed"}}
             },
             %{"toolCallId" => "mcp-2", "status" => "completed"}
             | _
           ] = updates(transcript)
  end

  test "web_search_items_with_action_titles" do
    steps =
      turn_steps("Look it up") ++
        [
          item_started(%{
            "type" => "webSearch",
            "id" => "search-1",
            "query" => "elixir 1.18 release",
            "action" => %{"type" => "search", "query" => "elixir 1.18 release"}
          }),
          item_completed(%{
            "type" => "webSearch",
            "id" => "search-1",
            "query" => "elixir 1.18 release",
            "action" => %{
              "type" => "search",
              "queries" => ["elixir 1.18", "elixir release notes"]
            }
          }),
          item_completed(%{
            "type" => "webSearch",
            "id" => "search-2",
            "action" => %{"type" => "openPage", "url" => "https://elixir-lang.org"}
          }),
          item_completed(%{
            "type" => "webSearch",
            "id" => "search-3",
            "action" => %{
              "type" => "findInPage",
              "url" => "https://elixir-lang.org",
              "pattern" => "1.18"
            }
          }),
          item_completed(%{"type" => "webSearch", "id" => "search-4"}),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "web_search_items_with_action_titles", steps)

    titles =
      transcript
      |> CodexGolden.messages()
      |> Enum.map(&get_in(&1, ["params", "update", "title"]))
      |> Enum.reject(&is_nil/1)

    assert titles == [
             "Web search: elixir 1.18 release",
             "Web search: elixir 1.18, elixir release notes",
             "Open page: https://elixir-lang.org",
             "Find in page for '1.18' in https://elixir-lang.org",
             "Web search"
           ]

    assert %{"kind" => "search", "status" => "in_progress"} =
             get_in(hd(turn_messages(transcript)), ["params", "update"])
  end

  test "legacy_web_search_notifications" do
    steps =
      turn_steps("Look it up") ++
        [
          notify("item/webSearch/started", %{
            "threadId" => @session,
            "itemId" => "search-1",
            "query" => "elixir 1.18 release"
          }),
          notify("item/webSearch/completed", %{
            "threadId" => @session,
            "itemId" => "search-1",
            "results" => [%{"title" => "Elixir 1.18", "url" => "https://elixir-lang.org"}]
          }),
          notify("item/webSearch/started", %{"threadId" => @session, "itemId" => "search-2"}),
          notify("item/webSearch/completed", %{
            "threadId" => @session,
            "itemId" => "search-2",
            "results" => "plain text results"
          }),
          notify("item/webSearch/completed", %{"threadId" => @session, "itemId" => "search-3"}),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "legacy_web_search_notifications", steps)

    updates = updates(transcript)

    assert %{"title" => "Web Search", "kind" => "fetch"} = Enum.at(updates, 0)

    assert [
             %{
               "content" => %{
                 "text" => "[{\"title\":\"Elixir 1.18\",\"url\":\"https://elixir-lang.org\"}]"
               }
             }
           ] =
             Enum.at(updates, 1)["content"]

    assert %{"rawOutput" => nil, "content" => [%{"content" => %{"text" => ""}}]} =
             Enum.at(updates, 4)
  end

  test "dynamic_tool_call_success_and_failure" do
    steps =
      turn_steps("Use the plugin") ++
        [
          item_started(%{
            "type" => "dynamicToolCall",
            "id" => "dyn-1",
            "namespace" => "fs",
            "tool" => "read_file",
            "arguments" => %{"path" => "README.md"},
            "status" => "inProgress"
          }),
          item_completed(%{
            "type" => "dynamicToolCall",
            "id" => "dyn-1",
            "namespace" => "fs",
            "tool" => "read_file",
            "status" => "completed",
            "success" => true,
            "contentItems" => [
              %{"type" => "inputText", "text" => "# README"},
              %{"type" => "content", "content" => %{"type" => "text", "text" => "nested"}},
              %{"type" => "inputImage", "imageUrl" => "data:image/png;base64,AAAA"}
            ]
          }),
          item_started(%{
            "type" => "dynamicToolCall",
            "id" => "dyn-2",
            "tool" => "deploy",
            "arguments" => %{"env" => "prod"}
          }),
          item_completed(%{
            "type" => "dynamicToolCall",
            "id" => "dyn-2",
            "tool" => "deploy",
            "success" => false,
            "contentItems" => "permission denied"
          }),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "dynamic_tool_call_success_and_failure", steps)

    updates = updates(transcript)

    assert %{"title" => "fs:read_file", "kind" => "read"} = Enum.at(updates, 0)

    assert [
             %{"content" => %{"text" => "# README"}},
             %{"content" => %{"text" => "nested"}},
             %{
               "content" => %{
                 "text" => "{\"imageUrl\":\"data:image/png;base64,AAAA\",\"type\":\"inputImage\"}"
               }
             }
           ] = Enum.at(updates, 1)["content"]

    assert %{
             "status" => "failed",
             "content" => [%{"content" => %{"text" => "permission denied"}}]
           } =
             Enum.at(updates, 3)
  end

  test "legacy_function_call_items" do
    steps =
      turn_steps("Call a function") ++
        [
          notify("item/created", %{
            "threadId" => @session,
            "item" => %{
              "type" => "function_call",
              "id" => "fc-1",
              "callId" => "call-1",
              "name" => "shell",
              "arguments" => %{"command" => ["ls"]}
            }
          }),
          {:note,
           "item/created ignores every item type but function_call (camelCase functionCall included)"},
          notify("item/created", %{
            "threadId" => @session,
            "item" => %{"type" => "functionCall", "id" => "fc-2", "name" => "shell"}
          }),
          item_completed(%{
            "type" => "functionCall",
            "id" => "fc-1",
            "callId" => "call-1",
            "name" => "shell",
            "arguments" => %{"command" => ["ls"]}
          }),
          item_completed(%{
            "type" => "function_call_output",
            "id" => "fco-1",
            "callId" => "call-1",
            "output" => "lib\nmix.exs\n"
          }),
          item_completed(%{
            "type" => "function_call_output",
            "callId" => "call-2",
            "isError" => true,
            "text" => "boom"
          }),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "legacy_function_call_items", steps)

    updates = updates(transcript)

    assert %{"sessionUpdate" => "tool_call", "toolCallId" => "call-1", "kind" => "execute"} =
             Enum.at(updates, 0)

    assert %{"status" => "failed", "rawOutput" => "boom"} = Enum.at(updates, 3)
  end

  test "item_started_function_call_items" do
    steps =
      turn_steps("Call a function") ++
        [
          item_started(%{
            "type" => "function_call",
            "id" => "fc-1",
            "callId" => "call-1",
            "name" => "shell",
            "arguments" => %{"command" => ["ls"]}
          }),
          {:note,
           "unlike item/created, item/started also accepts camelCase functionCall; without a callId the item id names the tool call and the kind is derived from the name"},
          item_started(%{
            "type" => "functionCall",
            "id" => "fc-2",
            "name" => "read_file",
            "arguments" => %{"path" => "mix.exs"}
          }),
          item_completed(%{
            "type" => "function_call_output",
            "callId" => "call-1",
            "output" => "lib\nmix.exs\n"
          }),
          item_completed(%{
            "type" => "function_call_output",
            "callId" => "fc-2",
            "output" => "defmodule ExMCP.MixProject do"
          }),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "item_started_function_call_items", steps)

    assert [
             %{
               "sessionUpdate" => "tool_call",
               "toolCallId" => "call-1",
               "title" => "shell",
               "kind" => "execute",
               "status" => "pending",
               "rawInput" => %{"command" => ["ls"]}
             },
             %{
               "sessionUpdate" => "tool_call",
               "toolCallId" => "fc-2",
               "title" => "read_file",
               "kind" => "read",
               "status" => "pending"
             },
             %{"sessionUpdate" => "tool_call_update", "toolCallId" => "call-1"},
             %{"sessionUpdate" => "tool_call_update", "toolCallId" => "fc-2"}
             | _
           ] = updates(transcript)
  end

  test "image_view_and_generation_items" do
    steps =
      turn_steps("Draw something") ++
        [
          item_started(%{
            "type" => "imageView",
            "id" => "img-1",
            "path" => "/tmp/project/shot.png"
          }),
          item_completed(%{
            "type" => "imageView",
            "id" => "img-1",
            "path" => "/tmp/project/shot.png"
          }),
          item_started(%{
            "type" => "imageGeneration",
            "id" => "gen-1",
            "status" => "inProgress",
            "revisedPrompt" => "A cat"
          }),
          item_completed(%{
            "type" => "imageGeneration",
            "id" => "gen-1",
            "status" => "completed",
            "revisedPrompt" => "A cat wearing a hat",
            "result" => "iVBORw0KGgo=",
            "savedPath" => "/tmp/project/cat.png"
          }),
          item_completed(%{"type" => "imageGeneration", "id" => "gen-2", "status" => "failed"}),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "image_view_and_generation_items", steps)

    updates = updates(transcript)

    assert %{"title" => "View Image /tmp/project/shot.png", "kind" => "read"} =
             Enum.at(updates, 0)

    assert Enum.at(updates, 0) == Enum.at(updates, 1)

    assert [
             %{"content" => %{"text" => "Revised prompt: A cat wearing a hat"}},
             %{"content" => %{"type" => "image", "uri" => "/tmp/project/cat.png"}}
           ] = Enum.at(updates, 3)["content"]

    assert %{"status" => "failed", "content" => []} = Enum.at(updates, 4)
  end

  test "image_generation_without_saved_path_or_revised_prompt" do
    steps =
      turn_steps("Draw something") ++
        [
          {:note,
           "an empty revisedPrompt adds no text block and a missing savedPath omits the image uri; an empty result adds no image block; a missing status defaults to completed"},
          item_completed(%{
            "type" => "imageGeneration",
            "id" => "gen-1",
            "status" => "completed",
            "revisedPrompt" => "",
            "result" => "iVBORw0KGgo="
          }),
          item_completed(%{
            "type" => "imageGeneration",
            "id" => "gen-2",
            "revisedPrompt" => "A dog",
            "result" => ""
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "image_generation_without_saved_path_or_revised_prompt",
        steps
      )

    assert [
             %{
               "toolCallId" => "gen-1",
               "status" => "completed",
               "content" => [%{"type" => "content", "content" => image}]
             },
             %{
               "toolCallId" => "gen-2",
               "status" => "completed",
               "content" => [
                 %{"content" => %{"type" => "text", "text" => "Revised prompt: A dog"}}
               ]
             }
             | _
           ] = updates(transcript)

    assert image == %{"type" => "image", "data" => "iVBORw0KGgo=", "mimeType" => "image/png"}
  end

  # -- plans, usage, rate limits --------------------------------------------

  test "plan_updates_thread_turn_and_delta" do
    steps =
      turn_steps("Plan the work") ++
        [
          notify("thread/plan/updated", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "entries" => [
              %{"content" => "Read the code", "status" => "completed", "priority" => "high"},
              %{"content" => "Write the fix", "status" => "in_progress", "priority" => "high"}
            ]
          }),
          notify("turn/plan/updated", %{
            "threadId" => @session,
            "plan" => [%{"content" => "Write the fix", "status" => "completed"}]
          }),
          notify("turn/plan/updated", %{"threadId" => @session}),
          notify("item/plan/delta", %{
            "threadId" => @session,
            "itemId" => "plan-1",
            "delta" => "Next: run the tests"
          }),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "plan_updates_thread_turn_and_delta", steps)

    updates = updates(transcript)

    assert %{"sessionUpdate" => "plan", "entries" => [_, _]} = Enum.at(updates, 0)
    assert %{"sessionUpdate" => "plan", "entries" => []} = Enum.at(updates, 2)

    assert %{
             "sessionUpdate" => "agent_thought_chunk",
             "content" => %{"text" => "Next: run the tests"}
           } =
             Enum.at(updates, 3)
  end

  test "token_usage_accumulates_across_updates" do
    steps =
      turn_steps("Count tokens") ++
        [
          {:note,
           "without modelContextWindow no usage_update is emitted, but the total is still recorded"},
          token_usage(%{"inputTokens" => 100, "outputTokens" => 10}, nil, %{
            "inputTokens" => 100,
            "outputTokens" => 10
          }),
          token_usage(
            %{"totalTokens" => 250, "inputTokens" => 200, "outputTokens" => 40},
            272_000,
            %{
              "inputTokens" => 300,
              "outputTokens" => 50,
              "cachedInputTokens" => 64
            }
          ),
          token_usage(%{"inputTokens" => 0, "outputTokens" => 0}, 0, %{
            "inputTokens" => 300,
            "outputTokens" => 50,
            "cachedInputTokens" => 64
          }),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Counted."}),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "token_usage_accumulates_across_updates", steps)

    assert [
             %{
               "params" => %{
                 "update" => %{
                   "sessionUpdate" => "usage_update",
                   "used" => 250,
                   "size" => 272_000
                 }
               }
             },
             _final_chunk,
             _status,
             %{"result" => %{"usage" => usage}}
           ] = turn_messages(transcript)

    assert usage == %{"inputTokens" => 300, "outputTokens" => 50, "cachedInputTokens" => 64}
  end

  test "rate_limits_metadata_keeps_successful_response" do
    steps =
      turn_steps("Say hello") ++
        [
          {:note,
           "account/rateLimits/updated carries no threadId and is routed to the only session"},
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex",
              "primary" => %{
                "usedPercent" => 42,
                "windowDurationMins" => 300,
                "resetsAt" => 1_800_000_000
              },
              "secondary" => %{
                "usedPercent" => 7,
                "windowDurationMins" => 10_080,
                "resetsAt" => 1_800_500_000
              },
              "credits" => %{"hasCredits" => true, "unlimited" => false}
            }
          }),
          notify("account/rateLimits/updated", %{}),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "rate_limits_metadata_keeps_successful_response", steps)

    [first, second | _] = turn_messages(transcript)

    assert %{"_meta" => %{"ex_mcp" => %{"rateLimits" => %{"limitId" => "codex"}}}} =
             first["params"]["update"]

    assert %{"_meta" => %{"ex_mcp" => %{"rateLimits" => %{}}}} = second["params"]["update"]

    assert %{"result" => %{"stopReason" => "end_turn"}} =
             List.last(turn_messages(transcript))
  end

  test "plan_delta_text_alias" do
    steps =
      turn_steps("Plan the work") ++
        [
          {:note,
           "item/plan/delta reads delta before text: a text-only delta is accepted as an alias, and delta wins when both are present"},
          notify("item/plan/delta", %{
            "threadId" => @session,
            "itemId" => "plan-1",
            "text" => "Next: run the tests"
          }),
          notify("item/plan/delta", %{
            "threadId" => @session,
            "itemId" => "plan-1",
            "delta" => "from delta",
            "text" => "from text"
          }),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "plan_delta_text_alias", steps)

    thoughts =
      transcript
      |> updates()
      |> Enum.filter(&(&1["sessionUpdate"] == "agent_thought_chunk"))
      |> Enum.map(& &1["content"]["text"])

    assert thoughts == ["Next: run the tests", "from delta"]
  end

  test "token_usage_positive_last_with_zero_total_marks_activity" do
    steps =
      turn_steps("Say hello") ++
        [
          exhausted_rate_limits(),
          {:note,
           "a tokenUsage update whose last-turn usage is positive marks model activity even though the cumulative total is zero (an inconsistent payload), so the exhausted limit does not fail the empty turn; the zero total is still echoed as usage"},
          token_usage(%{"inputTokens" => 3, "outputTokens" => 1}, 272_000, %{
            "inputTokens" => 0,
            "outputTokens" => 0,
            "cachedInputTokens" => 0
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "token_usage_positive_last_with_zero_total_marks_activity",
        steps
      )

    assert [
             _rate_limits,
             %{"params" => %{"update" => %{"sessionUpdate" => "usage_update", "used" => 4}}},
             _status,
             %{
               "id" => @prompt_id,
               "result" => %{
                 "stopReason" => "end_turn",
                 "usage" => %{"inputTokens" => 0, "outputTokens" => 0, "cachedInputTokens" => 0},
                 "_meta" => %{"ex_mcp" => %{"text" => ""}}
               }
             }
           ] = turn_messages(transcript)
  end

  test "rate_limit_exhausted_with_prior_thread_usage_still_succeeds" do
    steps =
      turn_steps("Say hello") ++
        [
          exhausted_rate_limits(),
          {:note,
           "a resumed or multi-turn thread reports a positive cumulative total while this turn's last usage is zero and no item, text or reasoning arrived: the positive cumulative usage alone keeps the empty turn a success under exhausted limits"},
          token_usage(%{"inputTokens" => 0, "outputTokens" => 0}, 272_000, %{
            "inputTokens" => 500,
            "outputTokens" => 40,
            "cachedInputTokens" => 0
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_exhausted_with_prior_thread_usage_still_succeeds",
        steps
      )

    assert [
             _rate_limits,
             %{
               "params" => %{
                 "update" => %{"sessionUpdate" => "usage_update", "used" => 0, "size" => 272_000}
               }
             },
             _status,
             %{
               "id" => @prompt_id,
               "result" => %{
                 "stopReason" => "end_turn",
                 "usage" => %{"inputTokens" => 500, "outputTokens" => 40},
                 "_meta" => %{"ex_mcp" => %{"text" => ""}}
               }
             }
           ] = turn_messages(transcript)
  end

  test "rate_limits_reset_by_next_prompt_keep_empty_turn_successful" do
    steps =
      turn_steps("First") ++
        [
          exhausted_rate_limits(),
          item_started(%{"type" => "unknownItemType", "id" => "x-1"}),
          turn_completed("completed")
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [
          {:note,
           "app-server re-sends account/rateLimits/updated only when the limits change: the exhausted limits remembered for the first turn do not carry into the second prompt, so its empty, activity-free turn still settles as end_turn instead of a capacity failure"},
          turn_completed("completed", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limits_reset_by_next_prompt_keep_empty_turn_successful",
        steps
      )

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{"id" => 20, "result" => %{"stopReason" => "end_turn"}},
             %{
               "id" => 21,
               "result" => %{
                 "stopReason" => "end_turn",
                 "_meta" => %{"ex_mcp" => %{"text" => "", "turnId" => "turn-2"}}
               }
             }
           ] = settlements
  end

  test "rate_limits_and_warnings_with_two_sessions_are_dropped" do
    steps =
      two_session_steps() ++
        prompt_steps("Say hello", @prompt_id, 5) ++
        [
          {:note,
           "account/rateLimits/updated and a warning without threadId carry no session id: with two live sessions neither can be routed, so both are dropped and the exhausted limits never reach the prompt's capacity check; a warning naming its thread still flows"},
          exhausted_rate_limits(),
          notify("warning", %{"message" => "Skill descriptions were shortened"}),
          notify("warning", %{
            "threadId" => @session,
            "message" => "Guardian flagged a risky command"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limits_and_warnings_with_two_sessions_are_dropped",
        steps
      )

    assert [
             %{
               "params" => %{
                 "sessionId" => @session,
                 "update" => %{
                   "_meta" => %{
                     "ex_mcp" => %{
                       "warning" => %{"message" => "Guardian flagged a risky command"}
                     }
                   }
                 }
               }
             },
             _status,
             %{"id" => @prompt_id, "result" => %{"stopReason" => "end_turn"}}
           ] = Enum.reject(turn_messages(transcript), &match?(%{"id" => 12}, &1))

    routed =
      transcript
      |> Enum.filter(fn entry ->
        entry.step[:kind] == :inbound and
          entry.step.message["method"] in ["account/rateLimits/updated", "warning"]
      end)
      |> Enum.map(&{&1.step.message["params"]["threadId"], &1.result.tag})

    assert routed == [{nil, :skip}, {nil, :skip}, {@session, :messages}]
  end

  # -- thread metadata ------------------------------------------------------

  test "thread_status_name_and_goal_updates" do
    steps =
      turn_steps("Rename yourself") ++
        [
          notify("thread/status/changed", %{"threadId" => @session, "status" => "active"}),
          notify("thread/status/changed", %{
            "threadId" => @session,
            "threadStatus" => %{"type" => "idle"}
          }),
          notify("thread/name/updated", %{
            "threadId" => @session,
            "threadName" => "Fix flaky test"
          }),
          notify("thread/name/updated", %{"threadId" => @session}),
          notify("thread/goal/updated", %{
            "threadId" => @session,
            "goal" => %{
              "objective" => "  Make the suite green  ",
              "status" => "active",
              "tokenBudget" => 50_000
            }
          }),
          notify("thread/goal/updated", %{
            "threadId" => @session,
            "goal" => %{"summary" => "no objective key"}
          }),
          notify("thread/goal/cleared", %{"threadId" => @session}),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "thread_status_name_and_goal_updates", steps)

    updates = updates(transcript)

    assert %{"_meta" => %{"ex_mcp" => %{"status" => "active"}}} = Enum.at(updates, 0)
    assert %{"title" => "Fix flaky test"} = Enum.at(updates, 2)
    assert %{"title" => nil} = Enum.at(updates, 3)

    assert %{"_meta" => %{"codex" => %{"goal" => %{"objective" => "Make the suite green"}}}} =
             Enum.at(updates, 4)

    assert %{"_meta" => %{"codex" => %{"goal" => nil}}} = Enum.at(updates, 6)
  end

  test "thread_status_changed_with_state_key" do
    steps =
      session_steps() ++
        [
          {:note,
           "thread/status/changed reads status, then threadStatus, then the legacy state key: each pairwise step below carries the two adjacent keys, and a payload with none reports a nil status"},
          notify("thread/status/changed", %{"threadId" => @session, "state" => "idle"}),
          notify("thread/status/changed", %{
            "threadId" => @session,
            "status" => "active",
            "state" => "idle"
          }),
          notify("thread/status/changed", %{
            "threadId" => @session,
            "threadStatus" => "waiting",
            "state" => "idle"
          }),
          notify("thread/status/changed", %{
            "threadId" => @session,
            "status" => "active",
            "threadStatus" => "waiting"
          }),
          notify("thread/status/changed", %{"threadId" => @session})
        ]

    transcript = CodexGolden.assert_golden(@area, "thread_status_changed_with_state_key", steps)

    assert [
             %{"_meta" => %{"ex_mcp" => %{"adapter" => "codex", "status" => "idle"}}},
             %{"_meta" => %{"ex_mcp" => %{"status" => "active"}}},
             %{"_meta" => %{"ex_mcp" => %{"status" => "waiting"}}},
             %{"_meta" => %{"ex_mcp" => %{"status" => "active"}}},
             %{"_meta" => %{"ex_mcp" => %{"status" => nil}}}
           ] = updates(transcript)
  end

  test "thread_archived_unarchived_closed_metadata" do
    steps =
      session_steps() ++
        [
          notify("thread/archived", %{"threadId" => @session}),
          notify("thread/unarchived", %{"threadId" => @session}),
          notify("thread/closed", %{"threadId" => @session}),
          {:note, "thread/closed does not fence the session: later notifications still flow"},
          notify("thread/status/changed", %{"threadId" => @session, "status" => "closed"})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "thread_archived_unarchived_closed_metadata", steps)

    updates = updates(transcript)

    assert [
             %{"_meta" => %{"codex" => %{"archived" => true}}},
             %{"_meta" => %{"codex" => %{"archived" => false}}},
             %{"_meta" => %{"codex" => %{"closed" => true}}},
             %{"_meta" => %{"ex_mcp" => %{"status" => "closed"}}}
           ] = updates
  end

  test "available_commands_verification_and_moderation_passthrough" do
    steps =
      session_steps() ++
        [
          notify("thread/availableCommands/updated", %{
            "threadId" => @session,
            "commands" => [%{"name" => "review", "description" => "Review changes"}]
          }),
          notify("thread/availableCommands/updated", %{
            "threadId" => @session,
            "availableCommands" => [%{"name" => "compact"}]
          }),
          notify("thread/availableCommands/updated", %{"threadId" => @session}),
          notify("model/verification", %{
            "threadId" => @session,
            "model" => "gpt-5",
            "verified" => true
          }),
          notify("turn/moderationMetadata", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "flags" => ["none"]
          })
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "available_commands_verification_and_moderation_passthrough",
        steps
      )

    updates = updates(transcript)

    assert %{
             "sessionUpdate" => "available_commands_update",
             "availableCommands" => [%{"name" => "review"}]
           } =
             Enum.at(updates, 0)

    assert %{"availableCommands" => []} = Enum.at(updates, 2)

    assert %{
             "_meta" => %{
               "ex_mcp" => %{"event" => "model/verification", "params" => %{"verified" => true}}
             }
           } =
             Enum.at(updates, 3)
  end

  test "model_rerouted_and_context_compaction" do
    steps =
      turn_steps("Keep going") ++
        [
          notify("model/rerouted", %{
            "threadId" => @session,
            "fromModel" => "gpt-5",
            "toModel" => "gpt-5-codex",
            "reason" => "capacity"
          }),
          notify("model/rerouted", %{
            "threadId" => @session,
            "from" => "gpt-5",
            "to" => "gpt-5-mini"
          }),
          notify("thread/compacted", %{"threadId" => @session}),
          item_completed(%{"type" => "contextCompaction", "id" => "compact-1"}),
          {:note,
           "compaction chunks are emitted but never folded into the prompt's _meta.ex_mcp.text"},
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Continuing."}),
          turn_completed("completed")
        ]

    transcript = CodexGolden.assert_golden(@area, "model_rerouted_and_context_compaction", steps)

    updates = updates(transcript)

    assert %{"content" => %{"text" => "Model rerouted from gpt-5 to gpt-5-codex (capacity).\n\n"}} =
             Enum.at(updates, 0)

    assert %{"content" => %{"text" => "Model rerouted from gpt-5 to gpt-5-mini (unknown).\n\n"}} =
             Enum.at(updates, 1)

    assert %{
             "sessionUpdate" => "agent_message_chunk",
             "content" => %{"text" => "Context compacted\n"}
           } =
             Enum.at(updates, 2)

    assert %{"result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Continuing."}}}} =
             List.last(turn_messages(transcript))
  end

  # -- warnings and errors --------------------------------------------------

  test "warnings_and_error_notification_metadata" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("warning", %{
            "threadId" => @session,
            "message" => "Skill descriptions were shortened"
          }),
          notify("guardianWarning", %{
            "threadId" => @session,
            "warning" => "Guardian flagged a risky command"
          }),
          notify("warning", %{"threadId" => @session, "severity" => "low"}),
          notify("error", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "error" => %{"message" => "stream reconnecting", "code" => "stream_reconnect"},
            "willRetry" => true
          }),
          notify("error", %{"threadId" => @session, "error" => %{}}),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          {:note,
           "a completed turn discards the remembered error notification and settles as success"},
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "warnings_and_error_notification_metadata", steps)

    updates = updates(transcript)

    assert %{
             "_meta" => %{
               "ex_mcp" => %{"warning" => %{"message" => "Skill descriptions were shortened"}}
             }
           } =
             Enum.at(updates, 0)

    assert %{
             "_meta" => %{
               "ex_mcp" => %{
                 "warning" => %{"message" => "{\"severity\":\"low\",\"threadId\":\"thread-abc\"}"}
               }
             }
           } =
             Enum.at(updates, 2)

    assert %{
             "_meta" => %{
               "ex_mcp" => %{"error" => %{"message" => "Unknown error", "code" => nil}}
             }
           } =
             Enum.at(updates, 4)

    assert %{"result" => %{"stopReason" => "end_turn"}} =
             List.last(turn_messages(transcript))
  end

  test "alias_precedence_when_both_keys_present" do
    steps =
      turn_steps("Search the repo") ++
        [
          {:note,
           "when a payload carries both aliases the first-listed key wins: message over warning on warnings, message over delta on mcpToolCall progress, output over text on function_call_output"},
          notify("warning", %{
            "threadId" => @session,
            "message" => "from message",
            "warning" => "from warning"
          }),
          notify("item/mcpToolCall/progress", %{
            "threadId" => @session,
            "itemId" => "mcp-1",
            "message" => "from message",
            "delta" => "from delta"
          }),
          item_completed(%{
            "type" => "function_call_output",
            "callId" => "call-1",
            "output" => "from output",
            "text" => "from text"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "alias_precedence_when_both_keys_present", steps)

    assert [
             %{"_meta" => %{"ex_mcp" => %{"warning" => %{"message" => "from message"}}}},
             %{"_meta" => %{"mcp_output_delta" => %{"data" => "from message"}}},
             %{
               "toolCallId" => "call-1",
               "content" => [%{"content" => %{"text" => "from output"}}],
               "rawOutput" => "from output"
             }
             | _
           ] = updates(transcript)
  end

  # -- turn/completed settlement --------------------------------------------

  test "turn_completed_without_active_prompt_emits_status_only" do
    steps =
      session_steps() ++
        [
          notify("turn/started", %{
            "threadId" => @session,
            "turn" => %{"id" => "turn-ext", "status" => "inProgress"}
          }),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "External turn"}),
          turn_completed("completed", %{"id" => "turn-ext"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "turn_completed_without_active_prompt_emits_status_only",
        steps
      )

    assert [
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_message_chunk"}}},
             %{"params" => %{"update" => %{"sessionUpdate" => "session_info_update"}}}
           ] = turn_messages(transcript)
  end

  test "turn_completed_interrupted_after_cancel_returns_partial_text" do
    steps =
      session_steps() ++
        [
          {:outbound, prompt("Write a poem", @prompt_id)},
          {:note,
           "the turn/start result carries no turn id; turn/started supplies it for session/cancel"},
          {:inbound, %{"id" => 4, "result" => %{}}},
          notify("turn/started", %{"threadId" => @session, "turnId" => "turn-1"}),
          agent_delta("msg-1", "Roses are"),
          {:outbound, %{"method" => "session/cancel", "params" => %{"sessionId" => @session}}},
          {:inbound, %{"id" => 5, "result" => %{}}},
          turn_completed("interrupted")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "turn_completed_interrupted_after_cancel_returns_partial_text",
        steps
      )

    assert [
             _,
             _,
             _,
             _,
             _,
             %{"id" => 5, "method" => "turn/interrupt", "params" => %{"turnId" => "turn-1"}}
           ] =
             CodexGolden.writes(transcript)

    assert %{
             "result" => %{
               "stopReason" => "cancelled",
               "_meta" => %{"ex_mcp" => %{"text" => "Roses are"}}
             }
           } =
             List.last(turn_messages(transcript))
  end

  test "turn_completed_status_passthrough_and_unknown" do
    steps =
      turn_steps("First") ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Cut short"}),
          turn_completed("max_tokens"),
          {:note,
           "a second prompt on the same session: accumulators were reset by the previous turn/completed"}
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-2", "text" => "Fresh"}),
          turn_completed("somethingNew", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "turn_completed_status_passthrough_and_unknown", steps)

    responses = Enum.filter(turn_messages(transcript), &Map.has_key?(&1, "result"))

    assert [
             %{
               "id" => 20,
               "result" => %{
                 "stopReason" => "max_tokens",
                 "_meta" => %{"ex_mcp" => %{"text" => "Cut short"}}
               }
             },
             %{
               "id" => 21,
               "result" => %{
                 "stopReason" => "end_turn",
                 "_meta" => %{"ex_mcp" => %{"text" => "Fresh", "turnId" => "turn-2"}}
               }
             }
           ] = responses
  end

  test "turn_completed_cancelled_max_turn_requests_and_refusal_passthrough" do
    steps =
      turn_steps("First") ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Stopped"}),
          {:note,
           "cancelled, max_turn_requests and refusal each settle as a successful response whose stopReason is the status itself; the fixture pins the wire value, not which normalize_stop_reason clause produced it"},
          turn_completed("cancelled")
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [turn_completed("max_turn_requests", %{"id" => "turn-2"})] ++
        prompt_steps("Third", @prompt_id + 2, 6, "turn-3") ++
        [turn_completed("refusal", %{"id" => "turn-3"})]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "turn_completed_cancelled_max_turn_requests_and_refusal_passthrough",
        steps
      )

    responses = Enum.filter(turn_messages(transcript), &Map.has_key?(&1, "result"))

    assert Enum.map(responses, &{&1["id"], &1["result"]["stopReason"]}) == [
             {20, "cancelled"},
             {21, "max_turn_requests"},
             {22, "refusal"}
           ]

    assert Enum.map(responses, &get_in(&1, ["result", "_meta", "ex_mcp", "text"])) ==
             ["Stopped", "", ""]
  end

  test "failed_turn_usage_limit_exceeded" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("error", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "error" => %{
              "message" => "You've hit your usage limit. Try again at Sep 6th, 2026 9:27 PM.",
              "codexErrorInfo" => "usageLimitExceeded",
              "additionalDetails" => nil
            },
            "willRetry" => false
          }),
          turn_completed("failed", %{
            "error" => %{
              "message" => "You've hit your usage limit. Try again at Sep 6th, 2026 9:27 PM.",
              "codexErrorInfo" => "usageLimitExceeded"
            }
          })
        ]

    transcript = CodexGolden.assert_golden(@area, "failed_turn_usage_limit_exceeded", steps)

    assert [
             %{"params" => %{"update" => %{"_meta" => %{"ex_mcp" => %{"error" => _}}}}},
             %{
               "params" => %{"update" => %{"_meta" => %{"ex_mcp" => %{"status" => "completed"}}}}
             },
             %{"id" => @prompt_id, "error" => error}
           ] = turn_messages(transcript)

    assert %{
             "code" => -32_029,
             "data" => %{"kind" => "rate_limit_exhausted", "turnStatus" => "failed"}
           } =
             error
  end

  test "failed_turn_error_field_beats_remembered_notification" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("error", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "error" => %{
              "message" => "You've hit your usage limit. Try again at Sep 6th, 2026 9:27 PM.",
              "codexErrorInfo" => "usageLimitExceeded",
              "additionalDetails" => nil
            },
            "willRetry" => false
          }),
          {:note,
           "turn.error wins over the remembered error notification when the two differ: the response carries the turn's own message, error info and code"},
          turn_completed("failed", %{
            "error" => %{
              "message" => "stream disconnected before completion",
              "codexErrorInfo" => %{"responseStreamDisconnected" => %{"httpStatusCode" => 502}}
            }
          })
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "failed_turn_error_field_beats_remembered_notification",
        steps
      )

    assert %{
             "id" => @prompt_id,
             "error" => %{
               "code" => -32_030,
               "message" => "stream disconnected before completion",
               "data" => %{
                 "kind" => "turn_failed",
                 "turnStatus" => "failed",
                 "codexErrorInfo" => %{"responseStreamDisconnected" => %{"httpStatusCode" => 502}}
               }
             }
           } = List.last(turn_messages(transcript))
  end

  test "failed_turn_401_uses_last_error_notification" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("error", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "error" => %{
              "message" =>
                "unexpected status 401 Unauthorized: Missing bearer or basic authentication in header",
              "codexErrorInfo" => %{"responseStreamDisconnected" => %{"httpStatusCode" => 401}}
            },
            "willRetry" => false
          }),
          turn_completed("failed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "failed_turn_401_uses_last_error_notification", steps)

    assert %{
             "error" => %{
               "code" => -32_031,
               "message" => message,
               "data" => %{"kind" => "unauthenticated"}
             }
           } =
             List.last(turn_messages(transcript))

    assert message =~ "401 Unauthorized"
  end

  test "failed_turn_system_error_with_additional_details" do
    steps =
      turn_steps("Say hello") ++
        [
          agent_delta("msg-1", "Partial"),
          turn_completed("errored", %{
            "error" => %{
              "message" => "internal server error",
              "codexErrorInfo" => "internalServerError",
              "additionalDetails" => "request-id: req_123"
            }
          })
        ]

    transcript =
      CodexGolden.assert_golden(@area, "failed_turn_system_error_with_additional_details", steps)

    assert %{
             "error" => %{
               "code" => -32_030,
               "message" => "internal server error",
               "data" => %{
                 "kind" => "turn_failed",
                 "turnStatus" => "errored",
                 "codexErrorInfo" => "internalServerError",
                 "additionalDetails" => "request-id: req_123"
               }
             }
           } = List.last(turn_messages(transcript))
  end

  test "failed_turn_without_error_detail" do
    steps = turn_steps("Say hello") ++ [turn_completed("failed")]

    transcript = CodexGolden.assert_golden(@area, "failed_turn_without_error_detail", steps)

    assert %{"error" => %{"code" => -32_030, "message" => "Codex turn failed", "data" => data}} =
             List.last(turn_messages(transcript))

    assert data == %{"kind" => "turn_failed", "provider" => "codex", "turnStatus" => "failed"}
  end

  test "errored_turn_without_error_detail_fails_prompt" do
    steps =
      turn_steps("First") ++
        [
          {:note,
           "status errored is a failure like failed: with no turn.error and no remembered notification the prompt gets the default -32030 / \"Codex turn failed\", never a refusal stopReason"},
          turn_completed("errored")
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [
          notify("error", %{
            "threadId" => @session,
            "turnId" => "turn-2",
            "error" => %{
              "message" => "context window exceeded",
              "codexErrorInfo" => "contextWindowExceeded"
            },
            "willRetry" => false
          }),
          {:note, "an errored turn without turn.error reuses the remembered error notification"},
          turn_completed("errored", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "errored_turn_without_error_detail_fails_prompt", steps)

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{
               "id" => 20,
               "error" => %{"code" => -32_030, "message" => "Codex turn failed", "data" => data}
             },
             %{
               "id" => 21,
               "error" => %{
                 "code" => -32_030,
                 "message" => "context window exceeded",
                 "data" => %{
                   "kind" => "turn_failed",
                   "turnStatus" => "errored",
                   "codexErrorInfo" => "contextWindowExceeded"
                 }
               }
             }
           ] = settlements

    assert data == %{"kind" => "turn_failed", "provider" => "codex", "turnStatus" => "errored"}
  end

  test "failed_turn_403_is_unauthenticated" do
    steps =
      turn_steps("Say hello") ++
        [
          {:note, "a 403 response-stream disconnect is classified like a 401"},
          turn_completed("failed", %{
            "error" => %{
              "message" =>
                "unexpected status 403 Forbidden: account is not entitled to this model",
              "codexErrorInfo" => %{"responseStreamDisconnected" => %{"httpStatusCode" => 403}}
            }
          })
        ]

    transcript = CodexGolden.assert_golden(@area, "failed_turn_403_is_unauthenticated", steps)

    assert %{
             "id" => @prompt_id,
             "error" => %{
               "code" => -32_031,
               "message" =>
                 "unexpected status 403 Forbidden: account is not entitled to this model",
               "data" => %{
                 "kind" => "unauthenticated",
                 "turnStatus" => "failed",
                 "codexErrorInfo" => %{"responseStreamDisconnected" => %{"httpStatusCode" => 403}}
               }
             }
           } = List.last(turn_messages(transcript))
  end

  test "failed_turn_map_form_usage_limit_exceeded" do
    steps =
      turn_steps("Say hello") ++
        [
          {:note,
           "codexErrorInfo may be a map keyed by usageLimitExceeded instead of the bare string; both classify as rate_limit_exhausted and the map is echoed in data"},
          turn_completed("failed", %{
            "error" => %{
              "message" => "You've hit your usage limit. Try again at Sep 6th, 2026 9:27 PM.",
              "codexErrorInfo" => %{
                "usageLimitExceeded" => %{"planType" => "plus", "resetsAt" => 1_800_000_000}
              }
            }
          })
        ]

    transcript =
      CodexGolden.assert_golden(@area, "failed_turn_map_form_usage_limit_exceeded", steps)

    assert %{
             "id" => @prompt_id,
             "error" => %{
               "code" => -32_029,
               "data" => %{
                 "kind" => "rate_limit_exhausted",
                 "turnStatus" => "failed",
                 "codexErrorInfo" => %{"usageLimitExceeded" => %{"planType" => "plus"}}
               }
             }
           } = List.last(turn_messages(transcript))
  end

  test "legacy_flat_turn_completed_status" do
    steps =
      turn_steps("First") ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Partial"}),
          {:note,
           "legacy turn/completed carries threadId/turnId/status flat with no turn map: the flat status drives the stopReason and the failure path alike"},
          notify("turn/completed", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "status" => "interrupted"
          })
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [
          notify("turn/completed", %{
            "threadId" => @session,
            "turnId" => "turn-2",
            "status" => "failed"
          })
        ]

    transcript = CodexGolden.assert_golden(@area, "legacy_flat_turn_completed_status", steps)

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{
               "id" => 20,
               "result" => %{
                 "stopReason" => "cancelled",
                 "_meta" => %{"ex_mcp" => %{"text" => "Partial", "turnId" => "turn-1"}}
               }
             },
             %{
               "id" => 21,
               "error" => %{
                 "code" => -32_030,
                 "message" => "Codex turn failed",
                 "data" => %{"kind" => "turn_failed", "turnStatus" => "failed"}
               }
             }
           ] = settlements
  end

  test "events_before_turn_start_reply_are_kept" do
    steps =
      session_steps() ++
        [
          {:outbound, prompt("Say hello", @prompt_id)},
          {:note,
           "app-server may stream turn/started and deltas before answering turn/start: the reply only records the turn id and must not reset what was already accumulated"},
          notify("turn/started", %{
            "threadId" => @session,
            "turn" => %{"id" => "turn-1", "status" => "inProgress", "items" => []}
          }),
          agent_delta("msg-1", "Early"),
          {:inbound,
           %{
             "id" => 4,
             "result" => %{"turn" => %{"id" => "turn-1", "status" => "inProgress", "items" => []}}
           }},
          agent_delta("msg-1", " and late"),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Early and late"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "events_before_turn_start_reply_are_kept", steps)

    assert [
             %{"params" => %{"update" => %{"content" => %{"text" => "Early"}}}},
             %{"params" => %{"update" => %{"content" => %{"text" => " and late"}}}},
             %{"params" => %{"update" => %{"content" => %{"text" => ""}}}},
             _status,
             %{
               "id" => @prompt_id,
               "result" => %{
                 "stopReason" => "end_turn",
                 "_meta" => %{"ex_mcp" => %{"text" => "Early and late", "turnId" => "turn-1"}}
               }
             }
           ] = turn_messages(transcript)
  end

  test "turn_start_reply_flat_turn_id_feeds_meta_and_cancel" do
    steps =
      session_steps() ++
        [
          {:outbound, prompt("Write a poem", @prompt_id)},
          {:note,
           "the turn/start result carries a flat turnId and no turn map, and no turn/started follows: that id is what session/cancel interrupts and what the response reports"},
          {:inbound,
           %{"id" => 4, "result" => %{"threadId" => @session, "turnId" => "turn-flat"}}},
          notify("item/agentMessage/delta", %{
            "threadId" => @session,
            "turnId" => "turn-flat",
            "itemId" => "msg-1",
            "delta" => "Roses are"
          }),
          {:outbound, %{"method" => "session/cancel", "params" => %{"sessionId" => @session}}},
          {:inbound, %{"id" => 5, "result" => %{}}},
          turn_completed("interrupted", %{"id" => "turn-flat"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "turn_start_reply_flat_turn_id_feeds_meta_and_cancel",
        steps
      )

    assert %{"id" => 5, "method" => "turn/interrupt", "params" => %{"turnId" => "turn-flat"}} =
             List.last(CodexGolden.writes(transcript))

    assert %{
             "id" => @prompt_id,
             "result" => %{
               "stopReason" => "cancelled",
               "_meta" => %{"ex_mcp" => %{"text" => "Roses are", "turnId" => "turn-flat"}}
             }
           } = List.last(turn_messages(transcript))
  end

  test "turn_completed_clears_stale_error_notification_for_next_prompt" do
    steps =
      turn_steps("First") ++
        [
          notify("error", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "error" => %{
              "message" =>
                "unexpected status 401 Unauthorized: Missing bearer or basic authentication in header",
              "codexErrorInfo" => %{"responseStreamDisconnected" => %{"httpStatusCode" => 401}}
            },
            "willRetry" => true
          }),
          {:note,
           "the turn recovers and completes: a remembered error never fails a completed turn without turn.error"},
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Recovered"}),
          turn_completed("completed")
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [
          {:note,
           "the previous turn/completed forgot the 401: a bare failed turn reports the generic turn failure, not the stale notification"},
          turn_completed("failed", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "turn_completed_clears_stale_error_notification_for_next_prompt",
        steps
      )

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{
               "id" => 20,
               "result" => %{
                 "stopReason" => "end_turn",
                 "_meta" => %{"ex_mcp" => %{"text" => "Recovered"}}
               }
             },
             %{
               "id" => 21,
               "error" => %{"code" => -32_030, "message" => "Codex turn failed", "data" => data}
             }
           ] = settlements

    assert data == %{"kind" => "turn_failed", "provider" => "codex", "turnStatus" => "failed"}
  end

  test "completed_status_with_error_map_still_fails" do
    steps =
      turn_steps("Say hello") ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          turn_completed("completed", %{"error" => %{"message" => "post-processing failed"}})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "completed_status_with_error_map_still_fails", steps)

    assert %{
             "error" => %{
               "code" => -32_030,
               "message" => "post-processing failed",
               "data" => %{"turnStatus" => "completed"}
             }
           } =
             List.last(turn_messages(transcript))
  end

  test "capacity_failure_rate_limit_exhausted_without_activity" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex_spark",
              "primary" => %{"usedPercent" => 100, "resetsAt" => 1_800_000_000},
              "credits" => %{"hasCredits" => false, "unlimited" => false}
            }
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "capacity_failure_rate_limit_exhausted_without_activity",
        steps
      )

    assert %{
             "id" => @prompt_id,
             "error" => %{"code" => -32_029, "message" => message, "data" => data}
           } =
             List.last(turn_messages(transcript))

    assert message == "Codex rate limit exhausted before the model produced a response"

    assert %{"kind" => "rate_limit_exhausted", "rateLimits" => %{"limitId" => "codex_spark"}} =
             data
  end

  test "rate_limit_exhausted_with_tool_activity_still_succeeds" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "rateLimitReachedType" => "primary",
              "primary" => %{"usedPercent" => 100},
              "credits" => %{"hasCredits" => false, "unlimited" => false}
            }
          }),
          {:note,
           "any item activity counts as model progress, so the empty turn settles as end_turn"},
          item_started(%{"type" => "unknownItemType", "id" => "x-1"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_exhausted_with_tool_activity_still_succeeds",
        steps
      )

    assert %{"result" => %{"stopReason" => "end_turn", "_meta" => %{"ex_mcp" => %{"text" => ""}}}} =
             List.last(turn_messages(transcript))
  end

  test "rate_limit_exhausted_with_item_completed_activity_still_succeeds" do
    steps =
      turn_steps("Run the tests") ++
        [
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex_spark",
              "primary" => %{"usedPercent" => 100, "resetsAt" => 1_800_000_000},
              "credits" => %{"hasCredits" => false, "unlimited" => false}
            }
          }),
          {:note,
           "item/completed alone (no item/started, no text, no usage) marks model activity, so the exhausted limit does not turn the empty result into a capacity failure"},
          item_completed(%{
            "type" => "commandExecution",
            "id" => "cmd-1",
            "command" => "mix test",
            "status" => "completed",
            "exitCode" => 0,
            "aggregatedOutput" => "3 tests, 0 failures\n"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_exhausted_with_item_completed_activity_still_succeeds",
        steps
      )

    assert %{"result" => %{"stopReason" => "end_turn", "_meta" => %{"ex_mcp" => %{"text" => ""}}}} =
             List.last(turn_messages(transcript))
  end

  test "rate_limit_exhausted_with_reasoning_activity_still_succeeds" do
    steps =
      turn_steps("Think about it") ++
        [
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex_spark",
              "primary" => %{"usedPercent" => 100, "resetsAt" => 1_800_000_000},
              "credits" => %{"hasCredits" => false, "unlimited" => false}
            }
          }),
          {:note,
           "a reasoning delta is model activity even though reasoning never reaches _meta.ex_mcp.text"},
          notify("item/reasoning/textDelta", %{
            "threadId" => @session,
            "itemId" => "reasoning-1",
            "delta" => "Considering the request"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_exhausted_with_reasoning_activity_still_succeeds",
        steps
      )

    assert [
             _rate_limits,
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_thought_chunk"}}},
             _status,
             %{
               "result" => %{
                 "stopReason" => "end_turn",
                 "_meta" => %{"ex_mcp" => %{"text" => ""}}
               }
             }
           ] = turn_messages(transcript)
  end

  test "rate_limit_exhausted_with_empty_agent_delta_still_succeeds" do
    steps =
      turn_steps("Say hello") ++
        [
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex_spark",
              "primary" => %{"usedPercent" => 100, "resetsAt" => 1_800_000_000},
              "credits" => %{"hasCredits" => false, "unlimited" => false}
            }
          }),
          {:note,
           "an agent message delta with an empty delta is still emitted and still counts as activity, so the empty text settles as end_turn"},
          agent_delta("msg-1", ""),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_exhausted_with_empty_agent_delta_still_succeeds",
        steps
      )

    assert [
             _rate_limits,
             %{"params" => %{"update" => %{"content" => %{"text" => ""}}}},
             _status,
             %{
               "result" => %{
                 "stopReason" => "end_turn",
                 "_meta" => %{"ex_mcp" => %{"text" => ""}}
               }
             }
           ] = turn_messages(transcript)
  end

  test "capacity_failure_via_rate_limit_reached_type" do
    steps =
      turn_steps("Say hello") ++
        [
          {:note,
           "rateLimitReachedType alone triggers the capacity failure: no window is at 100% and credits are available, neither of which is consulted once the reached type is set"},
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex",
              "rateLimitReachedType" => "secondary",
              "primary" => %{"usedPercent" => 35, "resetsAt" => 1_800_000_000},
              "secondary" => %{"usedPercent" => 88, "resetsAt" => 1_800_500_000},
              "credits" => %{"hasCredits" => true, "unlimited" => false}
            }
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "capacity_failure_via_rate_limit_reached_type", steps)

    assert %{
             "id" => @prompt_id,
             "error" => %{
               "code" => -32_029,
               "message" => "Codex rate limit exhausted before the model produced a response",
               "data" => %{
                 "kind" => "rate_limit_exhausted",
                 "rateLimits" => %{"rateLimitReachedType" => "secondary"}
               }
             }
           } = List.last(turn_messages(transcript))
  end

  test "rate_limit_window_exhausted_with_unlimited_credits_still_succeeds" do
    steps =
      turn_steps("First") ++
        [
          {:note,
           "an exhausted primary window is not a capacity failure while credits are unlimited (hasCredits false is irrelevant)"},
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex",
              "primary" => %{"usedPercent" => 100, "resetsAt" => 1_800_000_000},
              "credits" => %{"hasCredits" => false, "unlimited" => true}
            }
          }),
          turn_completed("completed")
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [
          {:note,
           "a fresh rateLimits update for the second turn: an exhausted secondary window alone, with no credits, is a capacity failure (the per-prompt reset itself is pinned by rate_limits_reset_by_next_prompt_keep_empty_turn_successful)"},
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex",
              "primary" => %{"usedPercent" => 60, "resetsAt" => 1_800_000_000},
              "secondary" => %{"usedPercent" => 100, "resetsAt" => 1_800_500_000},
              "credits" => %{"hasCredits" => false, "unlimited" => false}
            }
          }),
          turn_completed("completed", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_window_exhausted_with_unlimited_credits_still_succeeds",
        steps
      )

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{"id" => 20, "result" => %{"stopReason" => "end_turn"}},
             %{
               "id" => 21,
               "error" => %{
                 "code" => -32_029,
                 "data" => %{"rateLimits" => %{"secondary" => %{"usedPercent" => 100}}}
               }
             }
           ] = settlements
  end

  test "failed_turn_under_exhausted_rate_limits_reports_turn_error_not_capacity" do
    steps =
      turn_steps("Say hello") ++
        [
          exhausted_rate_limits(),
          notify("error", %{
            "threadId" => @session,
            "turnId" => "turn-1",
            "error" => %{
              "message" => "You've hit your usage limit. Try again at Sep 6th, 2026 9:27 PM.",
              "codexErrorInfo" => "usageLimitExceeded",
              "additionalDetails" => nil
            },
            "willRetry" => false
          }),
          {:note,
           "the realistic usage-limit flow: limits exhausted, no activity, and the turn itself fails. The turn's own error settles the prompt (its message, codexErrorInfo and turnStatus), never the capacity message with data.rateLimits"},
          turn_completed("failed", %{
            "error" => %{
              "message" => "You've hit your usage limit. Try again at Sep 6th, 2026 9:27 PM.",
              "codexErrorInfo" => "usageLimitExceeded"
            }
          })
        ] ++
        prompt_steps("Again", @prompt_id + 1, 5, "turn-2") ++
        [
          exhausted_rate_limits(),
          {:note,
           "a bare failed turn under the same exhausted limits is still the turn's failure: -32030 \"Codex turn failed\", not the -32029 capacity failure"},
          turn_completed("failed", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "failed_turn_under_exhausted_rate_limits_reports_turn_error_not_capacity",
        steps
      )

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{
               "id" => 20,
               "error" => %{
                 "code" => -32_029,
                 "message" => "You've hit your usage limit. Try again at Sep 6th, 2026 9:27 PM.",
                 "data" => first_data
               }
             },
             %{
               "id" => 21,
               "error" => %{
                 "code" => -32_030,
                 "message" => "Codex turn failed",
                 "data" => second_data
               }
             }
           ] = settlements

    assert first_data == %{
             "kind" => "rate_limit_exhausted",
             "provider" => "codex",
             "turnStatus" => "failed",
             "codexErrorInfo" => "usageLimitExceeded"
           }

    assert second_data == %{
             "kind" => "turn_failed",
             "provider" => "codex",
             "turnStatus" => "failed"
           }
  end

  test "rate_limit_window_exhausted_with_purchased_credits_still_succeeds" do
    steps =
      turn_steps("First") ++
        [
          {:note,
           "an exhausted primary window with purchased credits (hasCredits true, unlimited false) and no activity is not a capacity failure"},
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limitId" => "codex",
              "primary" => %{"usedPercent" => 100, "resetsAt" => 1_800_000_000},
              "credits" => %{"hasCredits" => true, "unlimited" => false}
            }
          }),
          turn_completed("completed")
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [
          {:note,
           "the snake_case has_credits alias exempts the window too (used_percent alias exhaustion itself is pinned by rate_limit_snake_case_aliases_trigger_capacity_failure)"},
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limit_id" => "codex",
              "primary" => %{"used_percent" => 100, "resets_at" => 1_800_000_000},
              "credits" => %{"has_credits" => true, "unlimited" => false}
            }
          }),
          turn_completed("completed", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_window_exhausted_with_purchased_credits_still_succeeds",
        steps
      )

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{"id" => 20, "result" => %{"stopReason" => "end_turn"}},
             %{"id" => 21, "result" => %{"stopReason" => "end_turn"}}
           ] = settlements
  end

  test "rate_limit_snake_case_aliases_trigger_capacity_failure" do
    steps =
      turn_steps("First") ++
        [
          {:note,
           "a window reported with the snake_case used_percent key at 100 and has_credits false is exhausted"},
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limit_id" => "codex",
              "primary" => %{"used_percent" => 100, "resets_at" => 1_800_000_000},
              "credits" => %{"has_credits" => false, "unlimited" => false}
            }
          }),
          turn_completed("completed")
        ] ++
        prompt_steps("Second", @prompt_id + 1, 5, "turn-2") ++
        [
          {:note,
           "the snake_case rate_limit_reached_type alias short-circuits like rateLimitReachedType: neither the low window nor the available credits are consulted"},
          notify("account/rateLimits/updated", %{
            "rateLimits" => %{
              "limit_id" => "codex",
              "rate_limit_reached_type" => "primary",
              "primary" => %{"usedPercent" => 12, "resetsAt" => 1_800_000_000},
              "credits" => %{"hasCredits" => true, "unlimited" => false}
            }
          }),
          turn_completed("completed", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_snake_case_aliases_trigger_capacity_failure",
        steps
      )

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{
               "id" => 20,
               "error" => %{
                 "code" => -32_029,
                 "data" => %{"rateLimits" => %{"primary" => %{"used_percent" => 100}}}
               }
             },
             %{
               "id" => 21,
               "error" => %{
                 "code" => -32_029,
                 "data" => %{"rateLimits" => %{"rate_limit_reached_type" => "primary"}}
               }
             }
           ] = settlements
  end

  test "rate_limit_exhausted_with_legacy_item_created_activity_still_succeeds" do
    steps =
      turn_steps("Call a function") ++
        [
          exhausted_rate_limits(),
          {:note,
           "a legacy item/created function_call alone marks model activity, so the exhausted limit does not turn the empty result into a capacity failure"},
          notify("item/created", %{
            "threadId" => @session,
            "item" => %{
              "type" => "function_call",
              "id" => "fc-1",
              "callId" => "call-1",
              "name" => "shell",
              "arguments" => %{"command" => ["ls"]}
            }
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "rate_limit_exhausted_with_legacy_item_created_activity_still_succeeds",
        steps
      )

    assert [%{"sessionUpdate" => "tool_call", "toolCallId" => "call-1"}] =
             Enum.filter(updates(transcript), &(&1["sessionUpdate"] == "tool_call"))

    assert %{"result" => %{"stopReason" => "end_turn", "_meta" => %{"ex_mcp" => %{"text" => ""}}}} =
             List.last(turn_messages(transcript))
  end

  test "turn_start_error_then_stray_turn_completed_replies_twice" do
    steps =
      session_steps() ++
        [
          {:outbound, prompt("Say hello", @prompt_id)},
          {:inbound,
           %{"id" => 4, "error" => %{"code" => -32_600, "message" => "thread is busy"}}},
          {:note,
           "the prompt was already failed, but its acp id stays active on the session: a later turn/completed answers it a second time"},
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "turn_start_error_then_stray_turn_completed_replies_twice",
        steps
      )

    assert [
             %{
               "id" => @prompt_id,
               "error" => %{"code" => -32_600, "message" => "thread is busy"}
             },
             _status,
             %{"id" => @prompt_id, "result" => %{"stopReason" => "end_turn"}}
           ] = turn_messages(transcript)
  end

  test "stray_turn_completed_after_settled_prompt_emits_status_only" do
    steps =
      turn_steps("Say hello") ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          turn_completed("completed"),
          {:note,
           "turn/completed released the prompt id: a later external turn on the same thread only reports its status and never answers acp id 20 a second time"},
          notify("turn/started", %{
            "threadId" => @session,
            "turn" => %{"id" => "turn-ext", "status" => "inProgress", "items" => []}
          }),
          notify("item/completed", %{
            "threadId" => @session,
            "turnId" => "turn-ext",
            "item" => %{"type" => "agentMessage", "id" => "msg-2", "text" => "External turn"}
          }),
          turn_completed("completed", %{"id" => "turn-ext"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "stray_turn_completed_after_settled_prompt_emits_status_only",
        steps
      )

    assert [
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_message_chunk"}}},
             %{"params" => %{"update" => %{"sessionUpdate" => "session_info_update"}}},
             %{"id" => @prompt_id, "result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Hello"}}}},
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_message_chunk"}}},
             %{"params" => %{"update" => %{"sessionUpdate" => "session_info_update"}}}
           ] = turn_messages(transcript)
  end

  test "session_cancel_after_turn_completed_has_no_active_turn" do
    steps =
      turn_steps("Say hello") ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          turn_completed("completed"),
          {:note,
           "turn/completed forgets the turn id: a late session/cancel (a client race against the settlement) has no turn to interrupt, fails, and writes no turn/interrupt"},
          {:outbound, %{"method" => "session/cancel", "params" => %{"sessionId" => @session}}}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_cancel_after_turn_completed_has_no_active_turn",
        steps
      )

    assert %{result: %{tag: :error, error: "No active Codex turn for session"}} =
             List.last(transcript)

    assert %{"id" => 4, "method" => "turn/start"} = List.last(CodexGolden.writes(transcript))
  end

  test "session_cancel_turn_id_param_overrides_tracked_turn" do
    steps =
      turn_steps("Write a poem") ++
        [
          agent_delta("msg-1", "Roses are"),
          {:note,
           "ExMCP extension: session/cancel may name the turn to interrupt, and that turnId beats the tracked turn; the settlement still reports the tracked one"},
          {:outbound,
           %{
             "method" => "session/cancel",
             "params" => %{"sessionId" => @session, "turnId" => "turn-other"}
           }},
          {:inbound, %{"id" => 5, "result" => %{}}},
          turn_completed("interrupted")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_cancel_turn_id_param_overrides_tracked_turn",
        steps
      )

    assert %{
             "id" => 5,
             "method" => "turn/interrupt",
             "params" => %{"threadId" => @session, "turnId" => "turn-other"}
           } = List.last(CodexGolden.writes(transcript))

    assert %{
             "id" => @prompt_id,
             "result" => %{
               "stopReason" => "cancelled",
               "_meta" => %{"ex_mcp" => %{"text" => "Roses are", "turnId" => "turn-1"}}
             }
           } = List.last(turn_messages(transcript))
  end

  test "turn_started_turn_map_id_beats_flat_turn_id" do
    steps =
      session_steps() ++
        [
          {:outbound, prompt("Write a poem", @prompt_id)},
          {:inbound, %{"id" => 4, "result" => %{}}},
          {:note,
           "turn/started carrying both a turn map and a flat turnId: the map's id is the tracked turn, so session/cancel interrupts it and the settlement reports it"},
          notify("turn/started", %{
            "threadId" => @session,
            "turnId" => "turn-flat",
            "turn" => %{"id" => "turn-map", "status" => "inProgress", "items" => []}
          }),
          {:outbound, %{"method" => "session/cancel", "params" => %{"sessionId" => @session}}},
          {:inbound, %{"id" => 5, "result" => %{}}},
          turn_completed("interrupted", %{"id" => "turn-map"})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "turn_started_turn_map_id_beats_flat_turn_id", steps)

    assert %{"id" => 5, "method" => "turn/interrupt", "params" => %{"turnId" => "turn-map"}} =
             List.last(CodexGolden.writes(transcript))

    assert %{
             "id" => @prompt_id,
             "result" => %{
               "stopReason" => "cancelled",
               "_meta" => %{"ex_mcp" => %{"turnId" => "turn-map"}}
             }
           } = List.last(turn_messages(transcript))
  end

  test "turn_id_tracking_degenerate_payloads" do
    steps =
      session_steps() ++
        [
          {:outbound, prompt("First", @prompt_id)},
          {:note,
           "a turn/start reply carrying both turn.id and a flat turnId tracks turn.id; a session/cancel whose turnId is the empty string falls back to that tracked turn"},
          {:inbound,
           %{
             "id" => 4,
             "result" => %{
               "turn" => %{"id" => "turn-map", "status" => "inProgress", "items" => []},
               "turnId" => "turn-flat"
             }
           }},
          {:outbound,
           %{"method" => "session/cancel", "params" => %{"sessionId" => @session, "turnId" => ""}}},
          {:inbound, %{"id" => 5, "result" => %{}}},
          turn_completed("interrupted", %{"id" => "turn-map"}),
          {:outbound, prompt("Second", @prompt_id + 1)},
          {:inbound,
           %{
             "id" => 6,
             "result" => %{"turn" => %{"id" => "turn-2", "status" => "inProgress", "items" => []}}
           }},
          {:note,
           "turn/started with neither turn.id nor turnId clears the turn tracked from the turn/start reply, so a session/cancel has nothing to interrupt and the settlement reports no turnId"},
          notify("turn/started", %{
            "threadId" => @session,
            "turn" => %{"status" => "inProgress", "items" => []}
          }),
          {:outbound, %{"method" => "session/cancel", "params" => %{"sessionId" => @session}}},
          turn_completed("completed", %{"id" => "turn-2"})
        ]

    transcript = CodexGolden.assert_golden(@area, "turn_id_tracking_degenerate_payloads", steps)

    assert [%{"id" => 5, "params" => %{"threadId" => @session, "turnId" => "turn-map"}}] =
             Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "turn/interrupt"))

    cancels =
      transcript
      |> Enum.filter(
        &(&1.step[:kind] == :outbound and &1.step.message["method"] == "session/cancel")
      )
      |> Enum.map(& &1.result.tag)

    assert cancels == [:ok, :error]

    settlements =
      Enum.filter(turn_messages(transcript), &(&1["id"] in [@prompt_id, @prompt_id + 1]))

    assert [
             %{"id" => 20, "result" => %{"_meta" => %{"ex_mcp" => %{"turnId" => "turn-map"}}}},
             %{"id" => 21, "result" => %{"_meta" => %{"ex_mcp" => %{"turnId" => nil}}}}
           ] = settlements
  end

  test "notification_routed_by_turn_thread_id_with_two_sessions" do
    steps =
      two_session_steps() ++
        prompt_steps("Say hello", @prompt_id, 5) ++
        [
          {:note,
           "with two live sessions a notification without threadId or sessionId is routed by turn.threadId: the delta reaches thread-abc's stream and its prompt text"},
          notify("item/agentMessage/delta", %{
            "turn" => %{"threadId" => @session, "id" => "turn-1"},
            "itemId" => "msg-1",
            "delta" => "Hello"
          }),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "notification_routed_by_turn_thread_id_with_two_sessions",
        steps
      )

    assert [
             %{
               "params" => %{
                 "sessionId" => @session,
                 "update" => %{"content" => %{"text" => "Hello"}}
               }
             },
             _status,
             %{"id" => @prompt_id, "result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Hello"}}}}
           ] = Enum.reject(turn_messages(transcript), &match?(%{"id" => 12}, &1))
  end

  test "turn_start_reply_foreign_thread_id_does_not_move_the_prompt" do
    steps =
      session_steps() ++
        [
          {:outbound, prompt("Say hello", @prompt_id)},
          {:note,
           "the turn/start reply names a thread other than the one the request was tracked for: the tracked session wins, so the turn id and the active prompt stay on thread-abc and turn/completed settles the prompt there"},
          {:inbound,
           %{
             "id" => 4,
             "result" => %{
               "threadId" => "thread-xyz",
               "turn" => %{"id" => "turn-1", "status" => "inProgress", "items" => []}
             }
           }},
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "turn_start_reply_foreign_thread_id_does_not_move_the_prompt",
        steps
      )

    assert [
             %{"params" => %{"update" => %{"sessionUpdate" => "agent_message_chunk"}}},
             _status,
             %{
               "id" => @prompt_id,
               "result" => %{
                 "stopReason" => "end_turn",
                 "_meta" => %{
                   "ex_mcp" => %{"text" => "Hello", "sessionId" => @session, "turnId" => "turn-1"}
                 }
               }
             }
           ] = turn_messages(transcript)
  end

  test "stale_agent_message_completed_after_turn_completed_emits_full_text" do
    steps =
      turn_steps("Say hello") ++
        [
          agent_delta("msg-1", "Hello"),
          turn_completed("completed"),
          {:note,
           "turn/completed forgets the streamed items: a stray completion of msg-1 arriving after the settlement finds no stream to dedupe against and is emitted in full, with no prompt left to answer"},
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "stale_agent_message_completed_after_turn_completed_emits_full_text",
        steps
      )

    assert [
             %{"params" => %{"update" => %{"content" => %{"text" => "Hello"}}}},
             _status,
             %{"id" => @prompt_id, "result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Hello"}}}},
             %{
               "params" => %{
                 "update" => %{
                   "content" => %{"text" => "Hello"},
                   "_meta" => %{"ex_mcp" => %{"final" => true}}
                 }
               }
             }
           ] = turn_messages(transcript)
  end

  test "prompt_after_turn_start_error_resets_streamed_items" do
    steps =
      session_steps() ++
        [
          {:outbound, prompt("First", @prompt_id)},
          notify("turn/started", %{
            "threadId" => @session,
            "turn" => %{"id" => "turn-1", "status" => "inProgress", "items" => []}
          }),
          agent_delta("msg-1", "Hello"),
          {:inbound,
           %{"id" => 4, "error" => %{"code" => -32_600, "message" => "thread is busy"}}},
          {:note,
           "the failed prompt leaves msg-1 half-streamed and nothing settles it; the next session/prompt starts from empty stream slots, so a stale completion of msg-1 is emitted in full and counted in the new turn's text"},
          {:outbound, prompt("Second", @prompt_id + 1)},
          {:inbound,
           %{
             "id" => 5,
             "result" => %{"turn" => %{"id" => "turn-2", "status" => "inProgress", "items" => []}}
           }},
          notify("turn/started", %{
            "threadId" => @session,
            "turn" => %{"id" => "turn-2", "status" => "inProgress", "items" => []}
          }),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          turn_completed("completed", %{"id" => "turn-2"})
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "prompt_after_turn_start_error_resets_streamed_items",
        steps
      )

    assert [
             %{"params" => %{"update" => %{"content" => %{"text" => "Hello"}}}},
             %{"id" => @prompt_id, "error" => %{"message" => "thread is busy"}},
             %{
               "params" => %{
                 "update" => %{
                   "content" => %{"text" => "Hello"},
                   "_meta" => %{"ex_mcp" => %{"final" => true}}
                 }
               }
             },
             _status,
             %{
               "id" => 21,
               "result" => %{"_meta" => %{"ex_mcp" => %{"text" => "Hello", "turnId" => "turn-2"}}}
             }
           ] = turn_messages(transcript)
  end

  test "status_command_after_turn_completed_reports_no_usage" do
    steps =
      turn_steps("Count tokens") ++
        [
          token_usage(%{"inputTokens" => 5, "outputTokens" => 5}, 272_000, %{
            "inputTokens" => 5,
            "outputTokens" => 5,
            "cachedInputTokens" => 0
          }),
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Counted."}),
          turn_completed("completed"),
          {:note,
           "turn/completed forgets the accumulated usage after echoing it in the response: a /status prompt on the settled session replies without usage and reports the token usage as not available"},
          {:outbound, prompt("/status", @prompt_id + 1)}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "status_command_after_turn_completed_reports_no_usage",
        steps
      )

    assert %{
             "id" => @prompt_id,
             "result" => %{
               "usage" => %{"inputTokens" => 5, "outputTokens" => 5, "cachedInputTokens" => 0}
             }
           } = Enum.find(turn_messages(transcript), &(&1["id"] == @prompt_id))

    assert %{result: %{tag: :messages_and_reply, messages: [status_chunk], reply: reply}} =
             List.last(transcript)

    refute Map.has_key?(reply, "usage")

    assert String.ends_with?(
             get_in(status_chunk, ["params", "update", "content", "text"]),
             "**Token usage:** data not available yet"
           )
  end

  test "thread_settings_model_feeds_next_turn_start_not_status" do
    steps =
      session_steps() ++
        [
          {:note,
           "thread/settings/updated replaces the session model: the next turn/start carries it, but /status keeps reporting the catalog model id resolved when the session was created"},
          notify("thread/settings/updated", %{
            "threadId" => @session,
            "threadSettings" => %{"model" => "gpt-5-codex"}
          }),
          {:outbound, prompt("/status", @prompt_id)}
        ] ++
        prompt_steps("Say hello", @prompt_id + 1) ++
        [
          item_completed(%{"type" => "agentMessage", "id" => "msg-1", "text" => "Hello"}),
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_model_feeds_next_turn_start_not_status",
        steps
      )

    assert %{"id" => 4, "method" => "turn/start", "params" => %{"model" => "gpt-5-codex"}} =
             List.last(CodexGolden.writes(transcript))

    status_chunk =
      transcript
      |> updates()
      |> Enum.find(&(&1["sessionUpdate"] == "agent_message_chunk"))

    assert String.starts_with?(status_chunk["content"]["text"], "**Model:** gpt-5/high  \n")
  end

  # -- replay ---------------------------------------------------------------

  test "session_load_replays_thread_history" do
    steps =
      handshake_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 11,
             "params" => %{"sessionId" => @session, "cwd" => "/tmp/project", "mcpServers" => []}
           }},
          {:note,
           "replayed items precede the session/load response and are marked _meta.ex_mcp.replay"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => @session,
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_000
               },
               "initialTurnsPage" => %{
                 "nextCursor" => nil,
                 "data" => [
                   %{
                     "id" => "turn-0",
                     "status" => "completed",
                     "items" => [
                       %{
                         "type" => "userMessage",
                         "id" => "user-0",
                         "content" => [%{"type" => "text", "text" => "Fix the flaky test"}]
                       },
                       %{
                         "type" => "reasoning",
                         "id" => "reasoning-0",
                         "summary" => ["Planning the fix"]
                       },
                       %{
                         "type" => "commandExecution",
                         "id" => "cmd-0",
                         "command" => "mix test",
                         "status" => "completed",
                         "exitCode" => 0,
                         "aggregatedOutput" => "1 test, 0 failures\n"
                       },
                       %{
                         "type" => "fileChange",
                         "id" => "edit-0",
                         "status" => "completed",
                         "changes" => [%{"path" => "test/a_test.exs", "diff" => "@@ -1 +1 @@"}]
                       },
                       %{
                         "type" => "mcpToolCall",
                         "id" => "mcp-0",
                         "server" => "repo",
                         "tool" => "search",
                         "arguments" => %{"query" => "flaky"},
                         "status" => "completed",
                         "result" => %{"content" => []},
                         "error" => nil
                       },
                       %{"type" => "agent_message", "id" => "msg-0", "text" => "previous answer"}
                     ]
                   }
                 ]
               }
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_load_replays_thread_history", steps)

    assert [
             _,
             _,
             %{"id" => 3, "method" => "thread/resume", "params" => %{"threadId" => @session}}
           ] =
             Enum.drop(CodexGolden.writes(transcript), 1)

    messages = CodexGolden.messages(transcript)
    assert length(messages) == 6

    assert Enum.all?(Enum.take(messages, 5), fn message ->
             get_in(message, ["params", "update", "_meta", "ex_mcp", "replay"]) == true
           end)

    assert %{"id" => 11, "result" => %{"sessionId" => @session}} = List.last(messages)
  end

  test "session_load_replays_reasoning_text_before_summary" do
    steps =
      handshake_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/load",
             "id" => 11,
             "params" => %{"sessionId" => @session, "cwd" => "/tmp/project", "mcpServers" => []}
           }},
          {:note,
           "replayed reasoning reads text before summary (a summary list is passed through as-is, unlike the live item/completed path which joins content/summary); an item with neither replays an empty thought"},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => @session,
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_000
               },
               "initialTurnsPage" => %{
                 "nextCursor" => nil,
                 "data" => [
                   %{
                     "id" => "turn-0",
                     "status" => "completed",
                     "items" => [
                       %{
                         "type" => "reasoning",
                         "id" => "reasoning-0",
                         "text" => "Reasoned in text form",
                         "summary" => ["Summary form"]
                       },
                       %{
                         "type" => "reasoning",
                         "id" => "reasoning-1",
                         "summary" => "Plain summary"
                       },
                       %{"type" => "reasoning", "id" => "reasoning-2"}
                     ]
                   }
                 ]
               }
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_load_replays_reasoning_text_before_summary",
        steps
      )

    thoughts =
      transcript
      |> CodexGolden.messages()
      |> Enum.filter(
        &(get_in(&1, ["params", "update", "sessionUpdate"]) == "agent_thought_chunk")
      )
      |> Enum.map(&get_in(&1, ["params", "update", "content", "text"]))

    assert thoughts == ["Reasoned in text form", "Plain summary", ""]

    assert Enum.all?(Enum.drop(CodexGolden.messages(transcript), -1), fn message ->
             get_in(message, ["params", "update", "_meta", "ex_mcp", "replay"]) == true
           end)
  end

  # -- skipped inputs -------------------------------------------------------

  test "unknown_items_and_notifications_are_skipped" do
    steps =
      turn_steps("Say hello") ++
        [
          item_started(%{"type" => "enteredReviewMode", "id" => "review-1"}),
          item_completed(%{"type" => "exitedReviewMode", "id" => "review-1"}),
          notify("item/completed", %{"threadId" => @session}),
          notify("mcpServer/startupStatus/updated", %{"name" => "repo", "status" => "ready"}),
          notify("thread/somethingNew", %{"threadId" => @session}),
          {:inbound_raw, "not json at all"},
          {:inbound, %{"id" => 999, "result" => %{}}},
          turn_completed("completed")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "unknown_items_and_notifications_are_skipped", steps)

    assert [
             %{"params" => %{"update" => %{"sessionUpdate" => "session_info_update"}}},
             %{"id" => @prompt_id, "result" => %{"stopReason" => "end_turn"}}
           ] = turn_messages(transcript)
  end

  # -- transcript views -----------------------------------------------------

  # ACP messages emitted after the session/new response: the thread/start
  # reply answers session/new (id 10) first, everything later belongs to the
  # session's turns.
  defp turn_messages(transcript) do
    transcript
    |> CodexGolden.messages()
    |> Enum.reject(&match?(%{"id" => 10}, &1))
  end

  # The `update` payload of every session/update notification, in order.
  defp updates(transcript) do
    transcript
    |> turn_messages()
    |> Enum.filter(&(&1["method"] == "session/update"))
    |> Enum.map(&get_in(&1, ["params", "update"]))
  end

  # -- step builders --------------------------------------------------------

  defp init_opts do
    [
      workspace_roots: ["/tmp"],
      cwd: "/tmp/project",
      authorize_mcp_server: fn _server, _context -> true end,
      trust_authorized_workspaces: true
    ]
  end

  # initialize (1) -> initialized + model/list (2)
  defp handshake_steps do
    [
      {:init, init_opts()},
      :post_connect,
      {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}},
      {:inbound, %{"id" => 2, "result" => %{"data" => catalog_models(), "nextCursor" => nil}}}
    ]
  end

  # handshake -> session/new -> thread/start (3) reply creating @session
  defp session_steps do
    handshake_steps() ++
      [
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
               "id" => @session,
               "cwd" => "/tmp/project",
               "updatedAt" => 1_700_000_000
             }
           }
         }}
      ]
  end

  # session/prompt -> turn/start (native_id) reply -> turn/started
  defp prompt_steps(text, acp_id \\ @prompt_id, native_id \\ 4, turn_id \\ "turn-1") do
    [
      {:outbound, prompt(text, acp_id)},
      {:inbound,
       %{
         "id" => native_id,
         "result" => %{"turn" => %{"id" => turn_id, "status" => "inProgress", "items" => []}}
       }},
      notify("turn/started", %{
        "threadId" => @session,
        "turn" => %{"id" => turn_id, "status" => "inProgress", "items" => []}
      })
    ]
  end

  defp turn_steps(text), do: session_steps() ++ prompt_steps(text)

  # session_steps -> a second session/new (acp id 12) -> thread/start (4) reply
  # creating "thread-def", so Sessions.current_id/1 can no longer pick a
  # session for notifications that carry no thread id.
  defp two_session_steps do
    session_steps() ++
      [
        {:outbound,
         %{
           "method" => "session/new",
           "id" => 12,
           "params" => %{"cwd" => "/tmp/other", "mcpServers" => []}
         }},
        {:inbound,
         %{
           "id" => 4,
           "result" => %{
             "model" => "gpt-5",
             "thread" => %{
               "id" => "thread-def",
               "cwd" => "/tmp/other",
               "updatedAt" => 1_700_000_100
             }
           }
         }}
      ]
  end

  # Primary window at 100% with no credits: exhausted unless the turn shows
  # model activity or positive usage.
  defp exhausted_rate_limits do
    notify("account/rateLimits/updated", %{
      "rateLimits" => %{
        "limitId" => "codex_spark",
        "primary" => %{"usedPercent" => 100, "resetsAt" => 1_800_000_000},
        "credits" => %{"hasCredits" => false, "unlimited" => false}
      }
    })
  end

  defp prompt(text, acp_id) do
    %{
      "method" => "session/prompt",
      "id" => acp_id,
      "params" => %{"sessionId" => @session, "prompt" => [%{"type" => "text", "text" => text}]}
    }
  end

  defp notify(method, params), do: {:inbound, %{"method" => method, "params" => params}}

  defp agent_delta(item_id, delta) do
    notify("item/agentMessage/delta", %{
      "threadId" => @session,
      "turnId" => "turn-1",
      "itemId" => item_id,
      "delta" => delta
    })
  end

  defp item_started(item) do
    notify("item/started", %{"threadId" => @session, "turnId" => "turn-1", "item" => item})
  end

  defp item_completed(item) do
    notify("item/completed", %{"threadId" => @session, "turnId" => "turn-1", "item" => item})
  end

  defp token_usage(last, model_context_window, total) do
    token_usage =
      %{"last" => last, "total" => total}
      |> then(fn usage ->
        if model_context_window,
          do: Map.put(usage, "modelContextWindow", model_context_window),
          else: usage
      end)

    notify("thread/tokenUsage/updated", %{
      "threadId" => @session,
      "turnId" => "turn-1",
      "tokenUsage" => token_usage
    })
  end

  defp turn_completed(status, turn_extra \\ %{}) do
    turn = Map.merge(%{"id" => "turn-1", "status" => status}, turn_extra)
    notify("turn/completed", %{"threadId" => @session, "turn" => turn})
  end

  # Codex app-server v2 `model/list` shape (same catalog as the lifecycle area).
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
