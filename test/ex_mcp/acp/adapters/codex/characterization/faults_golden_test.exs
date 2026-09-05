defmodule ExMCP.ACP.Adapters.Codex.FaultsGoldenTest do
  @moduledoc """
  Characterization gate for the Codex ACP adapter's cancellation and fault
  handling wire behavior (area A6 of `docs/POST_1_0_MAINTENANCE_PLAN.md`,
  "Codex adapter restructuring" / "Characterization gate").

  Each test drives `ExMCP.ACP.Adapters.Codex` through `ExMCP.Test.CodexGolden`
  and compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/codex/faults/`. The fixtures pin:

    * `session/cancel` with an active turn (the `turn/interrupt` write, the
      swallowed interrupt reply, and prompt settlement on the following
      `turn/completed`), with an explicit `turnId` (a non-string or blank one
      falls back to the session's turn), and without an active turn, an
      unknown session, or a `sessionId`;
    * `session/close` and `session/delete` for idle, active, unknown, and
      already-closed sessions: the interrupt / unsubscribe / archive writes,
      the immediate `stopReason: "cancelled"` prompt response, the native
      cancel result written for every pending client request of that session
      (and only that session), `elicitation/complete` for accepted url
      elicitations, and the fencing of later notifications and app-server
      requests for the closed id (including the id-less and `sessionId`-keyed
      events that escape the fence, and a session re-opened by `session/load`
      accepting events again);
    * `session/close` issued while the `turn/start` reply is still
      outstanding: the pending entry is not pruned, so a late success reply
      silently re-creates the closed session (a later `session/cancel` and a
      second `session/close` succeed and the prompt is answered a second time)
      and a late error reply answers the already-cancelled prompt again;
    * app-server responses for unknown ids and late duplicates, and the error
      reply for every pending request type (`thread/start`, `thread/resume`,
      `thread/list`, `turn/start`, `thread/compact/start`,
      `account/login/start`, `model/list`, `turn/interrupt`,
      `thread/unsubscribe`, `thread/archive`, `account/logout`);
    * ACP replies for unknown, already-answered, or server-resolved client
      requests, ACP error replies to every pending client request kind (each
      writes that kind's native cancel result), `serverRequest/resolved` for
      an id with no pending entry, duplicate app-server requests sharing a
      native id, and ACP messages the adapter does not handle;
    * app-server requests for an open session the adapter refuses natively
      with `-32601 "Unsupported app-server request: <method>"` (the named
      `item/tool/call`, `account/chatgptAuthTokens/refresh` and
      `attestation/generate` methods as well as an unknown one), and
      `item/tool/requestUserInput` answered `{answers: {}}` at once when the
      client lacks form elicitation or a question is `isSecret`;
    * `serverRequest/resolved` for an accepted url elicitation: only that
      flow's `elicitation/complete` is emitted, a second accepted flow stays
      open and completes on `session/close`;
    * `thread/closed` / `thread/archived` notices reported as session
      metadata while the session is open and fenced after `session/close`;
    * malformed, partial, and otherwise unclassifiable inbound lines;
    * `session/prompt` for an unknown or closed session, and notifications
      and app-server requests for a never-opened session.

  Failed-turn settlement (`turn_failure` / `capacity_failure`, error codes
  -32029 / -32030 / -32031) is characterized by the session_updates area, not
  here.

  Client-side request timeouts, subprocess exit, and port closure are owned by
  `ExMCP.ACP.Client` and `ExMCP.ACP.AdapterBridge`, not by the adapter (it
  never expires a pending request and only fences by state), so they are
  deliberately not characterized here.

  Adapter-generated ACP request ids are normalized to placeholders such as
  `"codex-permission-<1>"`; step functions answer them with the real id via
  `ExMCP.Test.CodexGolden.generated_ids/1`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CODEX_GOLDEN=update mix test <this file>[:line]`; that run rewrites the
  fixture and fails on purpose, so review the diff and re-run without the
  variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.CodexGolden

  @area "faults"
  @thread "thread-1"
  @other_thread "thread-2"

  @form_and_url_caps %{"elicitation" => %{"form" => %{}, "url" => %{}}}

  # -- session/cancel ---------------------------------------------------------

  describe "session/cancel" do
    test "cancel_active_turn_interrupts_and_settles_on_turn_completed" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, delta("Partial ")},
            {:outbound, cancel(%{"sessionId" => @thread})},
            {:note, "The turn/interrupt reply is swallowed; settlement is inbound-owned"},
            {:inbound, %{"id" => 5, "result" => %{}}},
            {:inbound, turn_completed("interrupted")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "cancel_active_turn_interrupts_and_settles_on_turn_completed",
          steps
        )

      assert %{"method" => "turn/interrupt", "params" => %{"threadId" => @thread}} =
               Enum.at(CodexGolden.writes(transcript), 5)

      assert %{
               messages: [
                 %{"params" => %{"update" => %{"sessionUpdate" => "session_info_update"}}},
                 %{"id" => 20, "result" => %{"stopReason" => "cancelled"}}
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "cancel_with_explicit_turn_id_overrides_session_turn" do
      steps =
        active_turn_steps() ++
          [
            {:outbound, cancel(%{"sessionId" => @thread, "turnId" => "turn-override"})},
            {:inbound, %{"id" => 5, "result" => %{}}},
            {:inbound, turn_completed("cancelled")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "cancel_with_explicit_turn_id_overrides_session_turn",
          steps
        )

      assert %{"params" => %{"turnId" => "turn-override"}} =
               Enum.at(CodexGolden.writes(transcript), 5)

      assert %{messages: [_info, %{"id" => 20, "result" => %{"stopReason" => "cancelled"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "cancel_without_active_turn_errors" do
      steps =
        session_steps() ++
          [
            {:note, "Idle session: no turn to interrupt"},
            {:outbound, cancel(%{"sessionId" => @thread})},
            {:note, "Prompt sent but the turn/start reply has not arrived: still no turn id"},
            {:outbound, prompt(20, @thread)},
            {:outbound, cancel(%{"sessionId" => @thread})},
            {:inbound, turn_start_result(4, "turn-1")},
            {:outbound, cancel(%{"sessionId" => @thread})}
          ]

      transcript = CodexGolden.assert_golden(@area, "cancel_without_active_turn_errors", steps)

      assert [
               %{error: "No active Codex turn for session"},
               %{error: "No active Codex turn for session"}
             ] =
               transcript
               |> Enum.map(& &1.result)
               |> Enum.filter(&Map.has_key?(&1, :error))

      assert %{writes: [%{"id" => 5, "method" => "turn/interrupt"}]} =
               CodexGolden.last_result(transcript)
    end

    test "cancel_with_non_string_turn_id_falls_back_to_session_turn" do
      steps =
        session_steps() ++
          [
            {:note, "A non-string or blank turnId is ignored: an idle session still has no turn"},
            {:outbound, cancel(%{"sessionId" => @thread, "turnId" => 7})},
            {:outbound, prompt(20, @thread)},
            {:inbound, turn_start_result(4, "turn-1")},
            {:note, "With an active turn the same values fall back to the session's turn id"},
            {:outbound, cancel(%{"sessionId" => @thread, "turnId" => 7})},
            {:outbound, cancel(%{"sessionId" => @thread, "turnId" => ""})},
            {:outbound, cancel(%{"sessionId" => @thread, "turnId" => nil})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "cancel_with_non_string_turn_id_falls_back_to_session_turn",
          steps
        )

      assert %{error: "No active Codex turn for session"} =
               result_of(
                 transcript,
                 &match?(%{kind: :outbound, message: %{"method" => "session/cancel"}}, &1)
               )

      assert [
               %{"id" => 5, "params" => %{"turnId" => "turn-1"}},
               %{"id" => 6, "params" => %{"turnId" => "turn-1"}},
               %{"id" => 7, "params" => %{"turnId" => "turn-1"}}
             ] =
               transcript
               |> CodexGolden.writes()
               |> Enum.filter(&(&1["method"] == "turn/interrupt"))
    end

    test "cancel_unknown_or_closed_session_errors" do
      steps =
        session_steps() ++
          [
            {:outbound, cancel(%{"sessionId" => "missing"})},
            {:outbound, close(@thread)},
            {:note, "A closed session is unknown to session/cancel"},
            {:outbound, cancel(%{"sessionId" => @thread})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "cancel_unknown_or_closed_session_errors", steps)

      assert [
               %{error: "Unknown Codex session: missing"},
               %{error: "Unknown Codex session: thread-1"}
             ] = results_of(transcript, &match?(%{message: %{"method" => "session/cancel"}}, &1))
    end

    test "turn_interrupt_error_reply_is_swallowed" do
      steps =
        active_turn_steps() ++
          [
            {:outbound, cancel(%{"sessionId" => @thread})},
            {:inbound, %{"id" => 5, "error" => %{"code" => -32_600, "message" => "no turn"}}},
            {:note, "The app-server still finishes the turn; the prompt settles normally"},
            {:inbound, turn_completed("completed")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "turn_interrupt_error_reply_is_swallowed", steps)

      assert %{tag: :skip, skipped: true} =
               result_of(transcript, &match?(%{message: %{"id" => 5, "error" => _}}, &1))

      assert %{messages: [_info, %{"id" => 20, "result" => %{"stopReason" => "end_turn"}}]} =
               CodexGolden.last_result(transcript)
    end
  end

  # -- session/close ----------------------------------------------------------

  describe "session/close" do
    test "close_active_prompt_cancels_and_fences_late_events" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, delta("Hel")},
            {:outbound, close(@thread)},
            {:note, "Replies to the interrupt and unsubscribe requests are swallowed"},
            {:inbound, %{"id" => 5, "result" => %{}}},
            {:inbound, %{"id" => 6, "result" => %{}}},
            {:note, "Notifications keyed by threadId, sessionId, or turn.threadId are fenced"},
            {:inbound, delta("lo")},
            {:inbound,
             %{
               "method" => "item/started",
               "params" => %{
                 "threadId" => @thread,
                 "turnId" => "turn-1",
                 "item" => %{
                   "type" => "commandExecution",
                   "id" => "item-1",
                   "command" => "mix test",
                   "cwd" => "/tmp/project",
                   "status" => "inProgress"
                 }
               }
             }},
            {:inbound,
             %{
               "method" => "error",
               "params" => %{
                 "sessionId" => @thread,
                 "error" => %{"message" => "late failure", "code" => 500}
               }
             }},
            {:inbound,
             %{
               "method" => "turn/completed",
               "params" => %{
                 "turn" => %{"id" => "turn-1", "threadId" => @thread, "status" => "completed"}
               }
             }},
            {:note, "An app-server request for the closed thread is auto-answered"},
            {:inbound, command_request(100)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "close_active_prompt_cancels_and_fences_late_events",
          steps
        )

      assert %{
               messages: [%{"id" => 20, "result" => %{"stopReason" => "cancelled"}}],
               writes: [
                 %{"id" => 6, "method" => "turn/interrupt"},
                 %{"id" => 5, "method" => "thread/unsubscribe"}
               ]
             } = result_of(transcript, &match?(%{message: %{"method" => "session/close"}}, &1))

      assert transcript
             |> results_between_notes(
               "Notifications keyed by threadId, sessionId, or turn.threadId are fenced",
               "An app-server request for the closed thread is auto-answered"
             )
             |> Enum.all?(&(&1 == %{tag: :skip, skipped: true}))

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "cancel"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "close_before_turn_start_reply_late_ok_reply_resurrects_session" do
      steps =
        session_steps() ++
          [
            {:outbound, prompt(20, @thread)},
            {:note, "Closing before the turn/start reply: no turn id, so no interrupt"},
            {:outbound, close(@thread)},
            {:inbound, %{"id" => 5, "result" => %{}}},
            {:note, "The pending turn/start entry survives the close and re-creates the session"},
            {:inbound, turn_start_result(4, "turn-1")},
            {:note, "The resurrected session is live for outbound lookups..."},
            {:outbound, cancel(%{"sessionId" => @thread})},
            {:inbound, %{"id" => 6, "result" => %{}}},
            {:note, "...while its notifications and app-server requests stay fenced"},
            {:inbound, turn_completed("cancelled")},
            {:inbound, command_request(100)},
            {:note, "A second close answers the already-cancelled prompt a second time"},
            {:outbound, close(@thread)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "close_before_turn_start_reply_late_ok_reply_resurrects_session",
          steps
        )

      assert [
               %{
                 messages: [%{"id" => 20, "result" => %{"stopReason" => "cancelled"}}],
                 writes: [%{"id" => 5, "method" => "thread/unsubscribe"}]
               },
               %{
                 messages: [%{"id" => 20, "result" => %{"stopReason" => "cancelled"}}],
                 writes: [
                   %{
                     "id" => 8,
                     "method" => "turn/interrupt",
                     "params" => %{"turnId" => "turn-1"}
                   },
                   %{"id" => 7, "method" => "thread/unsubscribe"}
                 ]
               }
             ] = results_of(transcript, &match?(%{message: %{"method" => "session/close"}}, &1))

      assert %{writes: [%{"id" => 6, "method" => "turn/interrupt"}]} =
               result_of(transcript, &match?(%{message: %{"method" => "session/cancel"}}, &1))

      assert [%{tag: :skip, skipped: true}, %{tag: :skip_and_write, writes: [%{"id" => 100}]}] =
               results_of(transcript, &match?(%{message: %{"method" => "turn/completed"}}, &1)) ++
                 results_of(transcript, &match?(%{message: %{"id" => 100}}, &1))
    end

    test "close_before_turn_start_reply_late_error_reply_answers_prompt_again" do
      steps =
        session_steps() ++
          [
            {:outbound, prompt(20, @thread)},
            {:outbound, close(@thread)},
            {:inbound, %{"id" => 5, "result" => %{}}},
            {:note, "The late turn/start error answers ACP id 20 a second time"},
            {:inbound, %{"id" => 4, "error" => %{"code" => -1, "message" => "thread is closed"}}},
            {:note, "The error path never touches sessions: the thread stays closed"},
            {:outbound, cancel(%{"sessionId" => @thread})},
            {:outbound, close(@thread)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "close_before_turn_start_reply_late_error_reply_answers_prompt_again",
          steps
        )

      assert [
               %{"id" => 20, "result" => %{"stopReason" => "cancelled"}},
               %{"id" => 20, "error" => _}
             ] =
               transcript |> CodexGolden.messages() |> Enum.filter(&(&1["id"] == 20))

      assert %{error: "Unknown Codex session: thread-1"} =
               result_of(transcript, &match?(%{message: %{"method" => "session/cancel"}}, &1))

      assert [%{tag: :messages_and_write}, %{error: "Unknown Codex session: thread-1"}] =
               results_of(transcript, &match?(%{message: %{"method" => "session/close"}}, &1))
    end

    test "close_idle_session_unsubscribes_only" do
      steps =
        session_steps() ++
          [
            {:outbound, close(@thread)},
            {:inbound, %{"id" => 4, "result" => %{}}},
            {:note, "Closing again is an unknown-session error"},
            {:outbound, close(@thread)}
          ]

      transcript = CodexGolden.assert_golden(@area, "close_idle_session_unsubscribes_only", steps)

      assert [
               %{messages: [], writes: [%{"id" => 4, "method" => "thread/unsubscribe"}]},
               %{error: "Unknown Codex session: thread-1"}
             ] = results_of(transcript, &match?(%{message: %{"method" => "session/close"}}, &1))
    end

    # The native cancel responses are written in the pending-map's key order,
    # which is stable only because the closed session holds at most one
    # request per generated-id prefix ("codex-elicitation-" < "codex-permission-"
    # < "codex-user-input-"). Do not add a second same-prefix pending request
    # for @thread to this scenario: its write order would depend on the
    # unique_integer suffixes and the fixture would flake.
    test "close_with_pending_client_requests_cancels_them_natively" do
      steps =
        active_turn_steps(@form_and_url_caps) ++
          [
            {:note, "A second session whose pending request must survive the close"},
            {:outbound, session_new(11)},
            {:inbound, thread_start_result(5, @other_thread)},
            {:inbound, command_request(100)},
            {:inbound, user_input_request(101)},
            {:inbound, form_elicitation_request(102)},
            {:inbound, url_elicitation_request(103, "oauth-1")},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:inbound, command_request(200, %{"threadId" => @other_thread})},
            {:outbound, close(@thread)},
            {:note, "The other session's request still answers; the cancelled one is gone"},
            {:outbound, select("allow_once", -1)},
            {:outbound, select("allow_once", 0)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "close_with_pending_client_requests_cancels_them_natively",
          steps
        )

      assert %{
               messages: [
                 %{"id" => 20, "result" => %{"stopReason" => "cancelled"}},
                 %{
                   "method" => "elicitation/complete",
                   "params" => %{"elicitationId" => "oauth-1"}
                 }
               ],
               writes: [
                 %{"id" => 7, "method" => "turn/interrupt"},
                 %{"id" => 6, "method" => "thread/unsubscribe"},
                 %{"id" => 102, "result" => %{"action" => "cancel"}},
                 %{"id" => 100, "result" => %{"decision" => "cancel"}},
                 %{"id" => 101, "result" => %{"answers" => %{}}}
               ]
             } = result_of(transcript, &match?(%{message: %{"method" => "session/close"}}, &1))

      assert [
               %{writes: [%{"id" => 200, "result" => %{"decision" => "accept"}}]},
               %{tag: :ok, skipped: true}
             ] = results_of(transcript, &match?(%{message: %{"result" => %{"outcome" => _}}}, &1))
    end

    test "close_with_pending_legacy_exec_approval_aborts" do
      steps =
        active_turn_steps() ++
          [{:inbound, legacy_request(100, "execCommandApproval")}, {:outbound, close(@thread)}]

      transcript =
        CodexGolden.assert_golden(@area, "close_with_pending_legacy_exec_approval_aborts", steps)

      assert %{
               writes: [
                 _interrupt,
                 _unsubscribe,
                 %{"id" => 100, "result" => %{"decision" => "abort"}}
               ]
             } =
               CodexGolden.last_result(transcript)
    end

    test "close_with_pending_mcp_permission_fallback_cancels" do
      steps =
        active_turn_steps() ++
          [
            {:note, "No elicitation capability: the MCP request became a permission request"},
            {:inbound, form_elicitation_request(100)},
            {:outbound, close(@thread)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "close_with_pending_mcp_permission_fallback_cancels",
          steps
        )

      assert [%{"method" => "session/request_permission"}] = acp_requests(transcript)

      assert %{
               writes: [
                 _interrupt,
                 _unsubscribe,
                 %{"id" => 100, "result" => %{"action" => "cancel"}}
               ]
             } =
               CodexGolden.last_result(transcript)
    end

    test "closed_session_server_requests_get_late_results" do
      steps =
        session_steps() ++
          [
            {:outbound, close(@thread)},
            {:inbound, command_request(100)},
            {:inbound, user_input_request(101)},
            {:inbound, form_elicitation_request(102)},
            {:inbound, permissions_request(103)},
            {:inbound, legacy_request(104, "execCommandApproval")},
            {:inbound, legacy_request(105, "applyPatchApproval")},
            {:inbound, file_change_request(106)},
            {:note, "The closed-session fence precedes the unsupported-method rejection"},
            {:inbound,
             %{
               "id" => 107,
               "method" => "item/tool/call",
               "params" => %{"threadId" => @thread, "tool" => "lookup"}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "closed_session_server_requests_get_late_results", steps)

      assert acp_requests(transcript) == []

      assert [
               %{"id" => 100, "result" => %{"decision" => "cancel"}},
               %{"id" => 101, "result" => %{"answers" => %{}}},
               %{"id" => 102, "result" => %{"action" => "cancel"}},
               %{"id" => 103, "result" => %{"permissions" => %{}, "scope" => "turn"}},
               %{"id" => 104, "result" => %{"decision" => "abort"}},
               %{"id" => 105, "result" => %{"decision" => "abort"}},
               %{"id" => 106, "result" => %{"decision" => "cancel"}},
               %{"id" => 107, "result" => %{"decision" => "cancel"}}
             ] =
               transcript
               |> results_of(&match?(%{kind: :inbound, message: %{"id" => _, "method" => _}}, &1))
               |> Enum.flat_map(&Map.fetch!(&1, :writes))
    end

    test "closed_session_server_request_with_session_id_key_is_not_fenced" do
      steps =
        session_steps() ++
          [
            {:outbound, close(@thread)},
            {:note, "The fence matches only a literal threadId; sessionId slips through"},
            {:inbound, command_request(100, %{"sessionId" => @thread}, ["threadId"])},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "closed_session_server_request_with_session_id_key_is_not_fenced",
          steps
        )

      assert [%{"method" => "session/request_permission", "params" => %{"sessionId" => @thread}}] =
               acp_requests(transcript)

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "idless_event_after_close_is_not_fenced" do
      steps =
        session_steps() ++
          [
            {:outbound, close(@thread)},
            {:note, "No session left: the delta is attributed to \"default\""},
            {:inbound,
             %{"method" => "item/agentMessage/delta", "params" => %{"delta" => "stray"}}},
            {:outbound, session_new(11)},
            {:inbound, thread_start_result(5, @other_thread)},
            {:note, "One live session: the id-less delta is attributed to it"},
            {:inbound,
             %{"method" => "item/agentMessage/delta", "params" => %{"delta" => "stray"}}}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "idless_event_after_close_is_not_fenced", steps)

      assert [
               %{messages: [%{"params" => %{"sessionId" => "default"}}]},
               %{messages: [%{"params" => %{"sessionId" => @other_thread}}]}
             ] =
               results_of(
                 transcript,
                 &match?(%{message: %{"method" => "item/agentMessage/delta"}}, &1)
               )
    end

    test "reopened_session_after_close_accepts_events" do
      steps =
        session_steps() ++
          [
            {:outbound, close(@thread)},
            {:inbound, delta("fenced")},
            {:outbound,
             %{
               "method" => "session/load",
               "id" => 12,
               "params" => %{"sessionId" => @thread, "cwd" => "/tmp/project", "mcpServers" => []}
             }},
            {:inbound,
             %{
               "id" => 5,
               "result" => %{
                 "model" => "gpt-5",
                 "thread" => %{"id" => @thread, "cwd" => "/tmp/project", "turns" => []}
               }
             }},
            {:inbound, delta("welcome back")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "reopened_session_after_close_accepts_events", steps)

      assert [
               %{tag: :skip},
               %{
                 messages: [
                   %{"params" => %{"update" => %{"content" => %{"text" => "welcome back"}}}}
                 ]
               }
             ] =
               results_of(
                 transcript,
                 &match?(%{message: %{"method" => "item/agentMessage/delta"}}, &1)
               )

      assert %{messages: [%{"id" => 12, "result" => %{"sessionId" => @thread}}]} =
               result_of(transcript, &match?(%{kind: :inbound, message: %{"id" => 5}}, &1))
    end

    test "thread_closed_and_archived_are_reported_while_open_and_fenced_after_close" do
      steps =
        session_steps() ++
          [
            {:note, "App-server lifecycle notices for an open session become metadata"},
            {:inbound, thread_notice("thread/closed")},
            {:inbound, thread_notice("thread/archived")},
            {:note, "Neither notice closes the session on the adapter side"},
            {:outbound, cancel(%{"sessionId" => @thread})},
            {:outbound, close(@thread)},
            {:note, "After session/close the same notices are fenced"},
            {:inbound, thread_notice("thread/closed")},
            {:inbound, thread_notice("thread/archived")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "thread_closed_and_archived_are_reported_while_open_and_fenced_after_close",
          steps
        )

      assert [
               %{
                 messages: [
                   %{
                     "params" => %{
                       "sessionId" => @thread,
                       "update" => %{
                         "sessionUpdate" => "session_info_update",
                         "_meta" => %{"codex" => %{"closed" => true}}
                       }
                     }
                   }
                 ]
               },
               %{tag: :skip, skipped: true}
             ] = results_of(transcript, &match?(%{message: %{"method" => "thread/closed"}}, &1))

      assert [
               %{messages: [%{"params" => %{"update" => %{"_meta" => %{"codex" => archived}}}}]},
               %{tag: :skip, skipped: true}
             ] =
               results_of(transcript, &match?(%{message: %{"method" => "thread/archived"}}, &1))

      assert archived == %{"archived" => true}

      assert %{error: "No active Codex turn for session"} =
               result_of(transcript, &match?(%{message: %{"method" => "session/cancel"}}, &1))
    end
  end

  # -- session/delete and session lookups -------------------------------------

  describe "session/delete and session lookups" do
    test "delete_unknown_session_archives_only_and_fences" do
      steps =
        handshake_steps() ++
          [
            {:outbound, delete("ghost")},
            {:inbound, delta("late", "ghost")},
            {:inbound, command_request(100, %{"threadId" => "ghost"})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "delete_unknown_session_archives_only_and_fences", steps)

      assert %{
               messages: [],
               writes: [
                 %{"id" => 3, "method" => "thread/archive", "params" => %{"threadId" => "ghost"}}
               ]
             } = result_of(transcript, &match?(%{message: %{"method" => "session/delete"}}, &1))

      assert %{tag: :skip} =
               result_of(
                 transcript,
                 &match?(%{message: %{"method" => "item/agentMessage/delta"}}, &1)
               )

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "cancel"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "delete_active_session_with_pending_permissions_request" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, permissions_request(100)},
            {:outbound, delete(@thread)},
            {:inbound, turn_completed("completed")},
            {:inbound, %{"id" => 7, "result" => %{}}},
            {:note, "Deleting again only re-archives"},
            {:outbound, delete(@thread)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "delete_active_session_with_pending_permissions_request",
          steps
        )

      [first_delete, second_delete] =
        results_of(transcript, &match?(%{message: %{"method" => "session/delete"}}, &1))

      assert %{
               messages: [%{"id" => 20, "result" => %{"stopReason" => "cancelled"}}],
               writes: [
                 %{"id" => 5, "method" => "turn/interrupt"},
                 %{"id" => 6, "method" => "thread/unsubscribe"},
                 %{
                   "id" => 100,
                   "result" => %{
                     "permissions" => %{},
                     "scope" => "turn",
                     "strictAutoReview" => false
                   }
                 },
                 %{"id" => 7, "method" => "thread/archive"}
               ]
             } = first_delete

      assert %{messages: [], writes: [%{"id" => 8, "method" => "thread/archive"}]} = second_delete
    end

    test "session_id_required_errors" do
      steps =
        handshake_steps() ++
          [
            {:outbound, %{"method" => "session/close", "id" => 30, "params" => %{}}},
            {:outbound, %{"method" => "session/delete", "id" => 31, "params" => %{}}},
            {:outbound, %{"method" => "session/cancel", "params" => %{}}},
            {:outbound,
             %{
               "method" => "session/prompt",
               "id" => 32,
               "params" => %{"prompt" => [%{"type" => "text", "text" => "hi"}]}
             }},
            {:note, "An empty sessionId counts as missing"},
            {:outbound, %{"method" => "session/cancel", "params" => %{"sessionId" => ""}}}
          ]

      transcript = CodexGolden.assert_golden(@area, "session_id_required_errors", steps)

      assert [_, _, _, _, _] =
               session_results =
               results_of(
                 transcript,
                 &match?(%{kind: :outbound, message: %{"method" => "session/" <> _}}, &1)
               )

      assert Enum.all?(session_results, &(&1 == %{tag: :error, error: "sessionId is required"}))
    end

    test "prompt_unknown_or_closed_session_errors" do
      steps =
        session_steps() ++
          [
            {:outbound, prompt(20, "missing")},
            {:outbound, close(@thread)},
            {:outbound, prompt(21, @thread)}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "prompt_unknown_or_closed_session_errors", steps)

      assert [
               %{error: "Unknown Codex session: missing"},
               %{error: "Unknown Codex session: thread-1"}
             ] = results_of(transcript, &match?(%{message: %{"method" => "session/prompt"}}, &1))
    end

    test "event_for_unknown_session_materializes_phantom_session" do
      steps =
        handshake_steps() ++
          [
            {:note, "turn/completed for a never-opened thread emits a status update"},
            {:inbound, turn_completed("completed", "phantom")},
            {:note, "...and the phantom session now exists for outbound lookups"},
            {:outbound, cancel(%{"sessionId" => "phantom"})},
            {:outbound, close("phantom")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "event_for_unknown_session_materializes_phantom_session",
          steps
        )

      assert %{
               messages: [
                 %{
                   "params" => %{
                     "sessionId" => "phantom",
                     "update" => %{"sessionUpdate" => "session_info_update"}
                   }
                 }
               ]
             } = result_of(transcript, &match?(%{message: %{"method" => "turn/completed"}}, &1))

      assert %{error: "No active Codex turn for session"} =
               result_of(transcript, &match?(%{message: %{"method" => "session/cancel"}}, &1))

      assert %{
               writes: [
                 %{
                   "id" => 3,
                   "method" => "thread/unsubscribe",
                   "params" => %{"threadId" => "phantom"}
                 }
               ]
             } = result_of(transcript, &match?(%{message: %{"method" => "session/close"}}, &1))
    end

    test "request_for_unknown_session_emits_phantom_permission_request" do
      steps =
        handshake_steps() ++
          [
            {:note, "An approval request for a never-opened thread is not fenced"},
            {:inbound, command_request(100, %{"threadId" => "phantom"})},
            {:outbound, select("allow_once")},
            {:note, "Unlike a notification, the request path does not materialize the session"},
            {:outbound, cancel(%{"sessionId" => "phantom"})},
            {:outbound, close("phantom")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "request_for_unknown_session_emits_phantom_permission_request",
          steps
        )

      assert [
               %{
                 "method" => "session/request_permission",
                 "params" => %{"sessionId" => "phantom"}
               }
             ] = acp_requests(transcript)

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]} =
               result_of(transcript, &match?(%{message: %{"result" => %{"outcome" => _}}}, &1))

      assert [
               %{error: "Unknown Codex session: phantom"},
               %{error: "Unknown Codex session: phantom"}
             ] =
               results_of(
                 transcript,
                 &match?(
                   %{message: %{"method" => m}} when m in ["session/cancel", "session/close"],
                   &1
                 )
               )
    end
  end

  # -- app-server responses ---------------------------------------------------

  describe "app-server responses" do
    test "unknown_response_ids_are_skipped" do
      steps =
        handshake_steps() ++
          [
            {:inbound, %{"id" => 99, "result" => %{"ok" => true}}},
            {:inbound, %{"id" => 99, "error" => %{"code" => -1, "message" => "nope"}}},
            {:inbound, %{"id" => "acp-like", "result" => %{}}},
            {:note, "The initialize request id was consumed by its first reply"},
            {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}}
          ]

      transcript = CodexGolden.assert_golden(@area, "unknown_response_ids_are_skipped", steps)

      assert [%{tag: :skip}, %{tag: :skip}, %{tag: :skip}] =
               results_of(
                 transcript,
                 &match?(
                   %{kind: :inbound, message: %{"id" => id}} when id in [99, "acp-like"],
                   &1
                 )
               )

      assert [%{tag: :skip_and_write}, %{tag: :skip, skipped: true}] =
               results_of(transcript, &match?(%{kind: :inbound, message: %{"id" => 1}}, &1))
    end

    test "late_duplicate_replies_are_skipped" do
      steps =
        active_turn_steps() ++
          [
            {:note, "A second thread/start reply for id 3 must not open thread-9"},
            {:inbound, thread_start_result(3, "thread-9")},
            {:inbound, turn_start_result(4, "turn-9")},
            {:inbound, turn_completed("completed")}
          ]

      transcript = CodexGolden.assert_golden(@area, "late_duplicate_replies_are_skipped", steps)

      assert [%{tag: :messages}, %{tag: :skip}] =
               results_of(transcript, &match?(%{kind: :inbound, message: %{"id" => 3}}, &1))

      # The first turn/start reply is itself silent (it only records the turn id).
      assert [%{tag: :skip}, %{tag: :skip}] =
               results_of(transcript, &match?(%{kind: :inbound, message: %{"id" => 4}}, &1))

      assert %{
               messages: [
                 _info,
                 %{"id" => 20, "result" => %{"_meta" => %{"ex_mcp" => %{"turnId" => "turn-1"}}}}
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "thread_start_error_reply_fails_session_new" do
      steps =
        handshake_steps() ++
          [
            {:outbound, session_new(10)},
            {:note, "code and message pass through; data is dropped"},
            {:inbound,
             %{
               "id" => 3,
               "error" => %{"code" => -32_000, "message" => "boom", "data" => %{"detail" => "x"}}
             }},
            {:outbound, session_new(11)},
            {:inbound, %{"id" => 4, "error" => %{"message" => "no code"}}},
            {:outbound, session_new(12)},
            {:inbound, %{"id" => 5, "error" => "plain string"}},
            {:outbound, session_new(13)},
            {:inbound, %{"id" => 6, "error" => 42}}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "thread_start_error_reply_fails_session_new", steps)

      assert [
               %{"id" => 10, "error" => %{"code" => -32_000, "message" => "boom"} = first},
               %{"id" => 11, "error" => %{"code" => -1, "message" => "no code"}},
               %{"id" => 12, "error" => %{"code" => -1, "message" => "plain string"}},
               %{"id" => 13, "error" => %{"code" => -1, "message" => "42"}}
             ] = CodexGolden.messages(transcript)

      refute Map.has_key?(first, "data")
    end

    test "thread_resume_error_reply_fails_session_load" do
      steps =
        handshake_steps() ++
          [
            {:outbound,
             %{
               "method" => "session/load",
               "id" => 12,
               "params" => %{
                 "sessionId" => "thread-old",
                 "cwd" => "/tmp/project",
                 "mcpServers" => []
               }
             }},
            {:inbound,
             %{"id" => 3, "error" => %{"code" => -32_602, "message" => "thread not found"}}}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "thread_resume_error_reply_fails_session_load", steps)

      assert [%{"id" => 3, "method" => "thread/resume"}] =
               Enum.drop(CodexGolden.writes(transcript), 3)

      assert %{
               messages: [
                 %{"id" => 12, "error" => %{"code" => -32_602, "message" => "thread not found"}}
               ]
             } =
               CodexGolden.last_result(transcript)
    end

    test "thread_list_error_reply_fails_session_list" do
      steps =
        handshake_steps() ++
          [
            {:outbound,
             %{"method" => "session/list", "id" => 14, "params" => %{"cwd" => "/tmp/project"}}},
            {:inbound,
             %{"id" => 3, "error" => %{"code" => -32_603, "message" => "storage unavailable"}}}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "thread_list_error_reply_fails_session_list", steps)

      assert [%{"id" => 3, "method" => "thread/list"}] =
               Enum.drop(CodexGolden.writes(transcript), 3)

      assert %{messages: [%{"id" => 14, "error" => %{"message" => "storage unavailable"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "turn_start_error_reply_fails_prompt" do
      steps =
        session_steps() ++
          [
            {:outbound, prompt(20, @thread)},
            {:inbound,
             %{"id" => 4, "error" => %{"code" => -32_000, "message" => "model unavailable"}}},
            {:note, "The prompt stays marked active: a later turn/completed answers id 20 again"},
            {:inbound, turn_completed("completed")}
          ]

      transcript = CodexGolden.assert_golden(@area, "turn_start_error_reply_fails_prompt", steps)

      assert %{messages: [%{"id" => 20, "error" => %{"message" => "model unavailable"}}]} =
               result_of(transcript, &match?(%{kind: :inbound, message: %{"id" => 4}}, &1))

      assert %{messages: [_info, %{"id" => 20, "result" => %{"stopReason" => "end_turn"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "prompt_command_start_error_reply_fails_prompt" do
      steps =
        session_steps() ++
          [
            {:outbound, prompt(20, @thread, "/compact")},
            {:inbound,
             %{"id" => 4, "error" => %{"code" => -32_000, "message" => "nothing to compact"}}}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "prompt_command_start_error_reply_fails_prompt", steps)

      assert [%{"id" => 4, "method" => "thread/compact/start"}] =
               Enum.drop(CodexGolden.writes(transcript), 4)

      assert %{messages: [%{"id" => 20, "error" => %{"message" => "nothing to compact"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "authenticate_error_reply_fails_authenticate" do
      steps =
        handshake_steps() ++
          [
            {:outbound,
             %{"method" => "authenticate", "id" => 15, "params" => %{"methodId" => "chat-gpt"}}},
            {:inbound, %{"id" => 3, "error" => %{"code" => -32_000, "message" => "login failed"}}}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "authenticate_error_reply_fails_authenticate", steps)

      assert [%{"id" => 3, "method" => "account/login/start"}] =
               Enum.drop(CodexGolden.writes(transcript), 3)

      assert %{
               messages: [
                 %{"id" => 15, "error" => %{"code" => -32_000, "message" => "login failed"}}
               ]
             } =
               CodexGolden.last_result(transcript)
    end

    test "model_list_error_reply_is_skipped" do
      steps = [
        init_step(),
        :post_connect,
        {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}},
        {:inbound,
         %{"id" => 2, "error" => %{"code" => -32_000, "message" => "catalog unavailable"}}},
        {:note, "Sessions still open against an empty catalog"},
        {:outbound, session_new(10)},
        {:inbound, thread_start_result(3, @thread)}
      ]

      transcript = CodexGolden.assert_golden(@area, "model_list_error_reply_is_skipped", steps)

      assert %{tag: :skip} = result_of(transcript, &match?(%{message: %{"id" => 2}}, &1))

      assert %{messages: [%{"id" => 10, "result" => %{"sessionId" => @thread}}]} =
               CodexGolden.last_result(transcript)
    end

    test "unsubscribe_archive_and_logout_error_replies_are_swallowed" do
      steps =
        session_steps() ++
          [
            {:outbound, close(@thread)},
            {:inbound,
             %{"id" => 4, "error" => %{"code" => -32_000, "message" => "not subscribed"}}},
            {:outbound, delete(@thread)},
            {:inbound,
             %{"id" => 5, "error" => %{"code" => -32_000, "message" => "already archived"}}},
            {:outbound, %{"method" => "logout", "id" => 16, "params" => %{}}},
            {:inbound,
             %{"id" => 6, "error" => %{"code" => -32_000, "message" => "not logged in"}}}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "unsubscribe_archive_and_logout_error_replies_are_swallowed",
          steps
        )

      assert [
               %{"method" => "thread/unsubscribe"},
               %{"method" => "thread/archive"},
               %{"method" => "account/logout"}
             ] =
               Enum.drop(CodexGolden.writes(transcript), 4)

      assert [%{tag: :skip}, %{tag: :skip}, %{tag: :skip}] =
               results_of(transcript, &match?(%{kind: :inbound, message: %{"error" => _}}, &1))
    end
  end

  # -- ACP replies and unhandled messages -------------------------------------

  describe "ACP replies and unhandled messages" do
    test "client_reply_for_unknown_id_is_skipped" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, command_request(100)},
            {:outbound,
             %{
               "jsonrpc" => "2.0",
               "id" => "acp-unknown",
               "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "allow_once"}}
             }},
            {:outbound,
             %{
               "jsonrpc" => "2.0",
               "id" => 999,
               "error" => %{"code" => -32_601, "message" => "unknown"}
             }},
            {:note, "The pending request is untouched and still answers"},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "client_reply_for_unknown_id_is_skipped", steps)

      assert [%{tag: :ok, skipped: true}, %{tag: :ok, skipped: true}] =
               results_of(
                 transcript,
                 &match?(
                   %{kind: :outbound, message: %{"id" => id}} when id in ["acp-unknown", 999],
                   &1
                 )
               )

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "client_reply_after_answered_is_skipped" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, command_request(100)},
            {:outbound, select("allow_once")},
            {:outbound, select("decline")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "client_reply_after_answered_is_skipped", steps)

      assert [
               %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]},
               %{tag: :ok, skipped: true}
             ] = results_of(transcript, &match?(%{message: %{"result" => %{"outcome" => _}}}, &1))
    end

    test "client_error_reply_cancels_pending_request_natively" do
      steps =
        active_turn_steps(@form_and_url_caps) ++
          [
            {:inbound, command_request(100)},
            {:outbound, error_reply(-32_000, "client refused")},
            {:inbound, file_change_request(101)},
            {:outbound, error_reply(-32_000, "client refused")},
            {:inbound, permissions_request(102)},
            {:outbound, error_reply(-32_000, "client refused")},
            {:inbound, form_elicitation_request(103)},
            {:outbound, error_reply(-32_000, "client refused")},
            {:inbound, url_elicitation_request(104, "oauth-1")},
            {:outbound, error_reply(-32_000, "client refused")},
            {:inbound, user_input_request(105)},
            {:outbound, error_reply(-32_000, "client refused")},
            {:inbound, legacy_request(106, "execCommandApproval")},
            {:outbound, error_reply(-32_000, "client refused")},
            {:note, "The url elicitation was never accepted, so resolving it completes nothing"},
            {:inbound, resolved(104)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "client_error_reply_cancels_pending_request_natively",
          steps
        )

      assert [
               %{"id" => 100, "result" => %{"decision" => "cancel"}},
               %{"id" => 101, "result" => %{"decision" => "cancel"}},
               %{"id" => 102, "result" => %{"permissions" => %{}, "scope" => "turn"}},
               %{"id" => 103, "result" => %{"action" => "cancel"}},
               %{"id" => 104, "result" => %{"action" => "cancel"}},
               %{"id" => 105, "result" => %{"answers" => %{}}},
               %{"id" => 106, "result" => %{"decision" => "abort"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))

      assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
    end

    test "unhandled_acp_messages_are_skipped" do
      steps =
        session_steps() ++
          [
            {:note, "An id-bearing request with no handler is treated as a reply to nothing"},
            {:outbound,
             %{"method" => "session/fork", "id" => 40, "params" => %{"sessionId" => @thread}}},
            {:outbound,
             %{"method" => "session/unknown_notification", "params" => %{"sessionId" => @thread}}},
            {:outbound, %{"method" => "session/cancel"}}
          ]

      transcript = CodexGolden.assert_golden(@area, "unhandled_acp_messages_are_skipped", steps)

      assert [%{tag: :ok, skipped: true}, %{tag: :ok, skipped: true}, %{tag: :ok, skipped: true}] =
               results_of(
                 transcript,
                 &match?(
                   %{kind: :outbound, message: %{"method" => m}}
                   when m in ["session/fork", "session/unknown_notification", "session/cancel"],
                   &1
                 )
               )
    end

    test "duplicate_native_request_id_creates_two_pending_requests" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, command_request(100)},
            {:inbound, command_request(100)},
            {:outbound, select("allow_once", 0)},
            {:outbound, select("decline", 1)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "duplicate_native_request_id_creates_two_pending_requests",
          steps
        )

      assert [%{"id" => "codex-permission-<1>"}, %{"id" => "codex-permission-<2>"}] =
               acp_requests(transcript)

      assert [
               %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]},
               %{writes: [%{"id" => 100, "result" => %{"decision" => "decline"}}]}
             ] = results_of(transcript, &match?(%{message: %{"result" => %{"outcome" => _}}}, &1))
    end

    test "server_resolved_drops_all_duplicate_pending_requests" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, command_request(100)},
            {:inbound, command_request(100)},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }},
            {:outbound, select("allow_once", 0)},
            {:outbound, select("allow_once", 1)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "server_resolved_drops_all_duplicate_pending_requests",
          steps
        )

      assert %{tag: :skip} =
               result_of(
                 transcript,
                 &match?(%{message: %{"method" => "serverRequest/resolved"}}, &1)
               )

      assert [%{tag: :ok, skipped: true}, %{tag: :ok, skipped: true}] =
               results_of(transcript, &match?(%{message: %{"result" => %{"outcome" => _}}}, &1))
    end

    test "server_resolved_for_unknown_request_id_is_skipped" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, resolved(999)},
            {:inbound, command_request(100)},
            {:note, "Resolving a different id leaves the pending request answerable"},
            {:inbound, resolved(999)},
            {:outbound, select("allow_once")},
            {:note, "Resolving an answered id, before and after close, is a no-op"},
            {:inbound, resolved(100)},
            {:outbound, close(@thread)},
            {:inbound, resolved(100)},
            {:inbound, %{"method" => "serverRequest/resolved", "params" => %{"requestId" => 100}}}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "server_resolved_for_unknown_request_id_is_skipped",
          steps
        )

      assert transcript
             |> results_of(&match?(%{message: %{"method" => "serverRequest/resolved"}}, &1))
             |> Enum.all?(&(&1 == %{tag: :skip, skipped: true}))

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]} =
               result_of(transcript, &match?(%{message: %{"result" => %{"outcome" => _}}}, &1))
    end
  end

  # -- app-server requests on open sessions -----------------------------------

  describe "app-server requests on open sessions" do
    test "unsupported_server_request_on_open_session_is_rejected_natively" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "Requests the adapter cannot serve are refused natively, without an ACP request"},
            {:inbound,
             server_request(100, "item/tool/call", %{
               "threadId" => @thread,
               "turnId" => "turn-1",
               "callId" => "call-1",
               "tool" => "lookup",
               "arguments" => %{"query" => "mix test"}
             })},
            {:inbound,
             server_request(101, "account/chatgptAuthTokens/refresh", %{
               "reason" => "expired",
               "previousAccountId" => "acct-1"
             })},
            {:inbound,
             server_request(102, "attestation/generate", %{
               "threadId" => @thread,
               "challenge" => "c2VydmVyLW5vbmNl"
             })},
            {:note, "A method the adapter has never heard of gets the same rejection"},
            {:inbound,
             server_request(103, "item/experimental/requestReview", %{
               "threadId" => @thread,
               "turnId" => "turn-1",
               "itemId" => "item-1"
             })},
            {:note, "Nothing was consumed: a later approval still answers, the next id is 5"},
            {:inbound, command_request(104)},
            {:outbound, select("allow_once")},
            {:outbound, cancel(%{"sessionId" => @thread})},
            {:inbound, turn_completed("cancelled")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "unsupported_server_request_on_open_session_is_rejected_natively",
          steps
        )

      assert [
               %{
                 tag: :skip_and_write,
                 writes: [
                   %{
                     "id" => 100,
                     "error" => %{
                       "code" => -32_601,
                       "message" => "Unsupported app-server request: item/tool/call"
                     }
                   }
                 ]
               },
               %{writes: [%{"id" => 101, "error" => %{"code" => -32_601, "message" => refresh}}]},
               %{writes: [%{"id" => 102, "error" => %{"code" => -32_601}}]},
               %{writes: [%{"id" => 103, "error" => %{"code" => -32_601, "message" => unknown}}]}
             ] =
               results_of(transcript, &match?(%{message: %{"id" => id}} when id in 100..103, &1))

      assert refresh == "Unsupported app-server request: account/chatgptAuthTokens/refresh"
      assert unknown == "Unsupported app-server request: item/experimental/requestReview"

      assert [%{"method" => "session/request_permission", "id" => "codex-permission-<1>"}] =
               acp_requests(transcript)

      assert %{writes: [%{"id" => 5, "method" => "turn/interrupt"}]} =
               result_of(transcript, &match?(%{message: %{"method" => "session/cancel"}}, &1))
    end

    test "user_input_request_without_form_capability_is_answered_empty" do
      steps =
        active_turn_steps() ++
          [
            {:note, "No elicitation.form capability: the question is answered empty at once"},
            {:inbound, user_input_request(100)},
            {:inbound, turn_completed("completed")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "user_input_request_without_form_capability_is_answered_empty",
          steps
        )

      assert acp_requests(transcript) == []

      assert %{tag: :skip_and_write, writes: [%{"id" => 100, "result" => %{"answers" => %{}}}]} =
               result_of(transcript, &match?(%{message: %{"id" => 100}}, &1))

      assert %{messages: [_info, %{"id" => 20, "result" => %{"stopReason" => "end_turn"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "secret_user_input_request_is_answered_empty" do
      steps =
        active_turn_steps(@form_and_url_caps) ++
          [
            {:note, "One isSecret question short-circuits the whole request"},
            {:inbound, secret_user_input_request(100)},
            {:note, "A non-secret request from the same turn still becomes a form elicitation"},
            {:inbound, user_input_request(101)},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"color" => "Blue"}})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "secret_user_input_request_is_answered_empty", steps)

      assert %{tag: :skip_and_write, writes: [%{"id" => 100, "result" => %{"answers" => %{}}}]} =
               result_of(transcript, &match?(%{message: %{"id" => 100}}, &1))

      assert [%{"method" => "elicitation/create", "id" => "codex-user-input-<1>"}] =
               acp_requests(transcript)

      assert %{writes: [%{"id" => 101, "result" => %{"answers" => %{"color" => _}}}]} =
               CodexGolden.last_result(transcript)
    end

    test "server_resolved_completes_accepted_url_elicitation" do
      steps =
        active_turn_steps(@form_and_url_caps) ++
          [
            {:inbound, url_elicitation_request(100, "oauth-1")},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:inbound, url_elicitation_request(101, "oauth-2")},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:note, "The app-server resolves the first flow: only oauth-1 completes"},
            {:inbound, resolved(100)},
            {:note, "Resolving it again, or replying to it again, is a no-op"},
            {:inbound, resolved(100)},
            {:outbound, elicitation_reply(%{"action" => "accept"}, 0)},
            {:note, "The second flow is still open and completes when the session closes"},
            {:outbound, close(@thread)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "server_resolved_completes_accepted_url_elicitation",
          steps
        )

      assert [
               %{
                 tag: :messages,
                 messages: [
                   %{
                     "method" => "elicitation/complete",
                     "params" => %{"elicitationId" => "oauth-1"}
                   }
                 ]
               },
               %{tag: :skip, skipped: true}
             ] =
               results_of(
                 transcript,
                 &match?(%{message: %{"method" => "serverRequest/resolved"}}, &1)
               )

      assert %{tag: :ok, skipped: true} =
               transcript
               |> results_of(
                 &match?(%{kind: :outbound, message: %{"result" => %{"action" => _}}}, &1)
               )
               |> List.last()

      assert %{
               messages: [
                 %{"id" => 20, "result" => %{"stopReason" => "cancelled"}},
                 %{
                   "method" => "elicitation/complete",
                   "params" => %{"elicitationId" => "oauth-2"}
                 }
               ]
             } = result_of(transcript, &match?(%{message: %{"method" => "session/close"}}, &1))
    end
  end

  # -- malformed inbound lines ------------------------------------------------

  describe "malformed inbound lines" do
    test "malformed_and_partial_inbound_lines_are_skipped" do
      steps =
        active_turn_steps() ++
          [
            {:inbound_raw, "not-json"},
            {:inbound_raw,
             ~s({"method":"item/agentMessage/delta","params":{"threadId":"thread-1","delta":"Hel)},
            {:inbound_raw, ""},
            {:inbound_raw, ~s({"foo":1})},
            {:inbound_raw, "[]"},
            {:inbound_raw, ~s({"method":5,"params":{}})},
            {:inbound_raw, ~s({"id":7})},
            {:inbound_raw, ~s({"id":null,"result":{}})},
            {:note, "A well-formed line with its NDJSON terminator still decodes"},
            {:inbound_raw,
             ~s({"method":"item/agentMessage/delta","params":{"threadId":"thread-1","delta":"ok"}}\n)},
            {:inbound, turn_completed("completed")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "malformed_and_partial_inbound_lines_are_skipped", steps)

      assert transcript
             |> results_of(&(&1.kind == :inbound_raw))
             |> Enum.drop(-1)
             |> Enum.all?(&(&1 == %{tag: :skip, skipped: true}))

      assert %{messages: [%{"params" => %{"update" => %{"content" => %{"text" => "ok"}}}}]} =
               transcript |> results_of(&(&1.kind == :inbound_raw)) |> List.last()

      assert %{
               messages: [
                 _info,
                 %{"id" => 20, "result" => %{"_meta" => %{"ex_mcp" => %{"text" => "ok"}}}}
               ]
             } =
               CodexGolden.last_result(transcript)
    end
  end

  # -- transcript helpers -----------------------------------------------------

  # ACP requests (id-bearing method calls) the adapter sent to the client;
  # notifications such as session/update are excluded.
  defp acp_requests(transcript) do
    transcript
    |> CodexGolden.messages()
    |> Enum.filter(&(Map.has_key?(&1, "method") and Map.has_key?(&1, "id")))
  end

  # Results of every entry whose recorded step satisfies `pred`, in order.
  # Looking entries up by their step keeps assertions independent of the
  # absolute position of a step in the scenario.
  defp results_of(transcript, pred) do
    transcript |> Enum.filter(&pred.(&1.step)) |> Enum.map(& &1.result)
  end

  # The result of the first entry whose recorded step satisfies `pred`.
  defp result_of(transcript, pred), do: transcript |> results_of(pred) |> hd()

  # Results of the entries recorded strictly between the notes `from` and `to`
  # (both must exist), with any nested notes excluded.
  defp results_between_notes(transcript, from, to) do
    transcript
    |> Enum.drop_while(&(&1.step != %{kind: :note, text: from}))
    |> Enum.drop(1)
    |> Enum.take_while(&(&1.step != %{kind: :note, text: to}))
    |> results_of(&(&1.kind != :note))
  end

  # -- step helpers -----------------------------------------------------------

  defp init_step do
    {:init,
     [
       workspace_roots: ["/tmp"],
       cwd: "/tmp/project",
       authorize_mcp_server: fn _server, _context -> true end,
       trust_authorized_workspaces: true
     ]}
  end

  # post_connect, app-server initialize + model/list replies, then the ACP
  # initialize carrying the client capabilities the scenario needs.
  defp handshake_steps(client_capabilities \\ %{}) do
    [
      init_step(),
      :post_connect,
      {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}},
      {:inbound, %{"id" => 2, "result" => %{"data" => catalog_models(), "nextCursor" => nil}}},
      {:outbound,
       %{
         "method" => "initialize",
         "id" => 0,
         "params" => %{"protocolVersion" => 1, "clientCapabilities" => client_capabilities}
       }}
    ]
  end

  # An idle session: session/new (ACP id 10) -> thread/start (native id 3) reply.
  defp session_steps(client_capabilities \\ %{}) do
    handshake_steps(client_capabilities) ++
      [{:outbound, session_new(10)}, {:inbound, thread_start_result(3, @thread)}]
  end

  # A session with an in-flight turn: session/prompt (ACP id 20) -> turn/start
  # (native id 4) reply -> turn/started. The next native id is 5.
  defp active_turn_steps(client_capabilities \\ %{}) do
    session_steps(client_capabilities) ++
      [
        {:outbound, prompt(20, @thread)},
        {:inbound, turn_start_result(4, "turn-1")},
        {:inbound,
         %{
           "method" => "turn/started",
           "params" => %{"threadId" => @thread, "turn" => %{"id" => "turn-1"}}
         }}
      ]
  end

  # -- ACP message builders ---------------------------------------------------

  defp session_new(id) do
    %{
      "method" => "session/new",
      "id" => id,
      "params" => %{"cwd" => "/tmp/project", "mcpServers" => []}
    }
  end

  defp prompt(id, thread_id, text \\ "Run the test suite") do
    %{
      "method" => "session/prompt",
      "id" => id,
      "params" => %{"sessionId" => thread_id, "prompt" => [%{"type" => "text", "text" => text}]}
    }
  end

  defp cancel(params), do: %{"method" => "session/cancel", "params" => params}

  defp close(thread_id) do
    %{"method" => "session/close", "id" => 30, "params" => %{"sessionId" => thread_id}}
  end

  defp delete(thread_id) do
    %{"method" => "session/delete", "id" => 31, "params" => %{"sessionId" => thread_id}}
  end

  # -- app-server message builders --------------------------------------------

  defp thread_start_result(id, thread_id) do
    %{
      "id" => id,
      "result" => %{
        "model" => "gpt-5",
        "thread" => %{"id" => thread_id, "cwd" => "/tmp/project", "updatedAt" => 1_700_000_000}
      }
    }
  end

  defp turn_start_result(id, turn_id) do
    %{"id" => id, "result" => %{"turn" => %{"id" => turn_id, "status" => "inProgress"}}}
  end

  defp delta(text, thread_id \\ @thread) do
    %{
      "method" => "item/agentMessage/delta",
      "params" => %{"threadId" => thread_id, "itemId" => "item-1", "delta" => text}
    }
  end

  defp turn_completed(status, thread_id \\ @thread) do
    %{
      "method" => "turn/completed",
      "params" => %{"threadId" => thread_id, "turn" => %{"id" => "turn-1", "status" => status}}
    }
  end

  defp resolved(request_id, thread_id \\ @thread) do
    %{
      "method" => "serverRequest/resolved",
      "params" => %{"threadId" => thread_id, "requestId" => request_id}
    }
  end

  # thread/closed, thread/archived, ... lifecycle notices for @thread.
  defp thread_notice(method), do: %{"method" => method, "params" => %{"threadId" => @thread}}

  # An arbitrary app-server request the adapter has no handler for.
  defp server_request(id, method, params) do
    %{"id" => id, "method" => method, "params" => params}
  end

  defp command_request(id, extra \\ %{}, drop \\ []) do
    params =
      %{
        "threadId" => @thread,
        "turnId" => "turn-1",
        "itemId" => "item-1",
        "command" => "mix test",
        "cwd" => "/tmp/project",
        "reason" => "The agent wants to run the test suite",
        "startedAtMs" => 1_700_000_000_000
      }
      |> Map.merge(extra)
      |> Map.drop(drop)

    %{"id" => id, "method" => "item/commandExecution/requestApproval", "params" => params}
  end

  defp file_change_request(id) do
    %{
      "id" => id,
      "method" => "item/fileChange/requestApproval",
      "params" => %{
        "threadId" => @thread,
        "turnId" => "turn-1",
        "itemId" => "item-2",
        "reason" => "Update the README",
        "grantRoot" => "/tmp/project"
      }
    }
  end

  defp permissions_request(id) do
    %{
      "id" => id,
      "method" => "item/permissions/requestApproval",
      "params" => %{
        "threadId" => @thread,
        "turnId" => "turn-1",
        "itemId" => "item-3",
        "reason" => "Needs network access to fetch dependencies",
        "permissions" => %{
          "network" => %{"enabled" => true},
          "fileSystem" => %{"write" => ["/tmp/project"]}
        }
      }
    }
  end

  defp legacy_request(id, method) do
    %{
      "id" => id,
      "method" => method,
      "params" => %{
        "threadId" => @thread,
        "command" => ["mix", "test"],
        "cwd" => "/tmp/project",
        "reason" => "Run tests"
      }
    }
  end

  defp form_elicitation_request(id) do
    %{
      "id" => id,
      "method" => "mcpServer/elicitation/request",
      "params" => %{
        "threadId" => @thread,
        "mode" => "form",
        "serverName" => "tool-server",
        "message" => "Which environment?",
        "requestedSchema" => %{
          "type" => "object",
          "properties" => %{"env" => %{"type" => "string", "enum" => ["dev", "prod"]}}
        }
      }
    }
  end

  defp url_elicitation_request(id, elicitation_id) do
    %{
      "id" => id,
      "method" => "mcpServer/elicitation/request",
      "params" => %{
        "threadId" => @thread,
        "mode" => "url",
        "serverName" => "tool-server",
        "elicitationId" => elicitation_id,
        "url" => "https://example.com/authorize/#{elicitation_id}",
        "message" => "Authorize the MCP server"
      }
    }
  end

  defp user_input_request(id) do
    %{
      "id" => id,
      "method" => "item/tool/requestUserInput",
      "params" => %{
        "threadId" => @thread,
        "turnId" => "turn-1",
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
    }
  end

  # A user-input request whose second question carries isSecret: true.
  defp secret_user_input_request(id) do
    %{
      "id" => id,
      "method" => "item/tool/requestUserInput",
      "params" => %{
        "threadId" => @thread,
        "turnId" => "turn-1",
        "itemId" => "credentials-tool",
        "questions" => [
          %{
            "id" => "username",
            "header" => "Username",
            "question" => "Which account?",
            "options" => [%{"label" => "deploy-bot", "description" => "Service account"}]
          },
          %{
            "id" => "token",
            "header" => "API token",
            "question" => "Paste the API token",
            "isSecret" => true
          }
        ]
      }
    }
  end

  # -- ACP reply builders (step functions resolving the generated id) ---------

  # Answers the adapter-generated request id at `index` (default: the most
  # recent one) with `reply`.
  defp reply_at(reply, index) do
    fn transcript ->
      ids = CodexGolden.generated_ids(transcript)
      Map.merge(%{"jsonrpc" => "2.0", "id" => Enum.at(ids, index)}, reply)
    end
  end

  defp select(option_id, index \\ -1) do
    reply_at(
      %{"result" => %{"outcome" => %{"outcome" => "selected", "optionId" => option_id}}},
      index
    )
  end

  defp elicitation_reply(result, index \\ -1), do: reply_at(%{"result" => result}, index)

  defp error_reply(code, message, index \\ -1),
    do: reply_at(%{"error" => %{"code" => code, "message" => message}}, index)

  # Codex app-server v2 `model/list` shape (see the lifecycle golden test).
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
