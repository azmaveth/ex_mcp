defmodule ExMCP.ACP.Adapters.ClaudeSDK.PermissionsGoldenTest do
  @moduledoc """
  Characterization gate for the Claude SDK adapter's permission bridge
  (`docs/POST_1_0_MAINTENANCE_PLAN.md`, "Claude adapter characterization
  gate": `can_use_tool`, permission modes, the `set_permission_mode`
  control, the `allowDangerouslySkipPermissions` session opt-out, and
  AskUserQuestion answer folding).

  Each test drives `ExMCP.ACP.Adapters.ClaudeSDK` through
  `ExMCP.Test.ClaudeGolden` and compares the recorded transcript against a
  committed fixture under `test/fixtures/acp/claude/permissions/`. The
  fixtures pin:

    * the ACP `session/request_permission` request a `can_use_tool` control
      request produces - its tool call (from `ToolInfo`), its option set,
      its `_meta.permission` title and `decision_reason` description - and
      the `tool_call` update that precedes it only when the tool is not
      already known;
    * every `PermissionResult` written back: `allow_once`, `allow_always`
      with and without `permission_suggestions`, `reject_once`, a cancelled
      outcome, an unknown option id, a flat (unwrapped) outcome, and a
      client error reply;
    * the `ExitPlanMode` option set for each elevated mode (auto, bypass,
      accept-edits), the `setMode` permission each selection writes, the
      "keep planning" rejection with its `interrupt`, and the fail-closed
      answer for an option that was never offered;
    * the tool-call id minted for a request that carries none, and the
      `toolUseID` spelling;
    * `session/set_mode` and the `mode` / `permission_mode` config aliases:
      the `set_permission_mode` control they write, the modes result they
      reply with, and the errors for a mode outside the current catalog;
    * the `_meta.claudeCode.options.allowDangerouslySkipPermissions: false`
      opt-out on `session/new`, `session/load` and `session/resume` -
      removing `bypassPermissions` from the catalog, refusing it in
      `set_mode`, clamping an active bypass mode back to `default`, and
      never being overridden by a `true` from the host;
    * `AskUserQuestion` becoming an `elicitation/create` form request and
      the answer folding that comes back: single- and multi-select picks,
      a typed custom answer replacing or annotating a pick, the CLI's
      quoted multi-select join, `decline`, `cancel`, a client error, an
      existing `annotations` map, and the two fail-closed refusals
      (no form elicitation capability, no valid questions).

  Control-request shapes the adapter does not implement, late replies, and
  cancellation belong to the faults area; the catalog contents themselves
  belong to the catalog area.

  Mutation check (2026-09-21): in `claude_sdk/protocol.ex`, making
  `permission_result/2` answer an `allow_always` with no
  `permission_suggestions` as an ordinary allow (returning the `allow_once`
  body instead of `cancelled_permission/1`) fails
  `allow_always_without_suggestions_fails_closed`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CLAUDE_GOLDEN=update mix test <this file>[:line]`; that run rewrites
  the fixture and fails on purpose, so review the diff and re-run without
  the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.ClaudeGolden
  alias ExMCP.Test.ClaudeGolden.Flows

  @area "permissions"
  @form_caps %{"elicitation" => %{"form" => %{}}}
  @auto_models [%{"value" => "sonnet", "displayName" => "Sonnet", "supportsAutoMode" => true}]
  @no_auto_models [%{"value" => "opus", "displayName" => "Opus", "supportsAutoMode" => false}]

  describe "can_use_tool options and results" do
    test "allow_once_allows_with_the_original_input" do
      steps = session() ++ [Flows.can_use_tool(), Flows.select("allow_once")]

      transcript =
        ClaudeGolden.assert_golden(@area, "allow_once_allows_with_the_original_input", steps)

      assert [%{"method" => "session/request_permission", "params" => params}] =
               acp_requests(transcript)

      assert Enum.map(params["options"], & &1["optionId"]) == ["allow_once", "reject_once"]

      assert %{writes: [%{"response" => %{"response" => %{"behavior" => "allow"}}}]} =
               ClaudeGolden.last_result(transcript)
    end

    test "reject_once_denies_with_the_default_message" do
      steps = session() ++ [Flows.can_use_tool(), Flows.select("reject_once")]

      ClaudeGolden.assert_golden(@area, "reject_once_denies_with_the_default_message", steps)
    end

    test "decision_reason_becomes_the_denial_message_and_description" do
      steps =
        session() ++
          [
            Flows.can_use_tool("req-1", %{"decision_reason" => "Writes outside the workspace"}),
            Flows.select("reject_once")
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "decision_reason_becomes_the_denial_message_and_description",
          steps
        )

      assert [%{"_meta" => %{"permission" => %{"description" => description}}}] =
               acp_requests(transcript)

      assert description == "Reason: Writes outside the workspace"
    end

    test "cancelled_outcome_denies_and_interrupts" do
      steps = session() ++ [Flows.can_use_tool(), Flows.cancelled()]

      ClaudeGolden.assert_golden(@area, "cancelled_outcome_denies_and_interrupts", steps)
    end

    test "unknown_option_id_denies_and_interrupts" do
      steps = session() ++ [Flows.can_use_tool(), Flows.select("allow_for_session")]

      ClaudeGolden.assert_golden(@area, "unknown_option_id_denies_and_interrupts", steps)
    end

    test "flat_outcome_is_accepted" do
      steps =
        session() ++
          [
            Flows.can_use_tool(),
            {:note, "A client that does not nest the outcome is still understood"},
            Flows.reply_last(%{"outcome" => "selected", "optionId" => "allow_once"})
          ]

      ClaudeGolden.assert_golden(@area, "flat_outcome_is_accepted", steps)
    end

    test "client_error_reply_becomes_a_control_error" do
      steps = session() ++ [Flows.can_use_tool(), Flows.error_reply()]

      transcript =
        ClaudeGolden.assert_golden(@area, "client_error_reply_becomes_a_control_error", steps)

      assert %{writes: [%{"response" => %{"subtype" => "error"}}]} =
               ClaudeGolden.last_result(transcript)
    end

    test "permission_suggestions_add_a_persistent_option" do
      steps =
        session() ++
          [
            Flows.can_use_tool("req-1", %{
              "permission_suggestions" => [
                %{"type" => "addRules", "rules" => [%{"toolName" => "Bash"}]}
              ]
            }),
            Flows.select("allow_always")
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "permission_suggestions_add_a_persistent_option", steps)

      assert [%{"params" => params}] = acp_requests(transcript)

      assert Enum.map(params["options"], & &1["optionId"]) == [
               "allow_once",
               "allow_always",
               "reject_once"
             ]
    end

    test "empty_permission_suggestions_offer_no_persistent_option" do
      steps =
        session() ++
          [
            Flows.can_use_tool("req-1", %{"permission_suggestions" => []}),
            Flows.select("allow_once")
          ]

      ClaudeGolden.assert_golden(
        @area,
        "empty_permission_suggestions_offer_no_persistent_option",
        steps
      )
    end

    test "allow_always_without_suggestions_fails_closed" do
      steps =
        session() ++
          [
            {:note, "allow_always is not offered here, so selecting it must not allow"},
            Flows.can_use_tool(),
            Flows.select("allow_always")
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "allow_always_without_suggestions_fails_closed", steps)

      assert %{
               writes: [
                 %{"response" => %{"response" => %{"behavior" => "deny", "interrupt" => true}}}
               ]
             } = ClaudeGolden.last_result(transcript)
    end

    test "rich_tool_info_is_carried_into_the_permission_request" do
      steps =
        session() ++
          [
            Flows.can_use_tool("req-1", %{
              "tool_name" => "Edit",
              "input" => %{
                "file_path" => "#{Flows.cwd()}/lib/app.ex",
                "old_string" => "a",
                "new_string" => "b"
              }
            }),
            Flows.select("allow_once")
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "rich_tool_info_is_carried_into_the_permission_request",
          steps
        )

      assert [%{"params" => %{"toolCall" => tool_call}}] = acp_requests(transcript)
      assert tool_call["kind"] == "edit"
      assert tool_call["title"] == "Edit lib/app.ex"
    end

    test "missing_tool_use_id_is_minted" do
      steps =
        session() ++
          [Flows.can_use_tool("req-1", %{"tool_use_id" => nil}), Flows.select("allow_once")]

      transcript = ClaudeGolden.assert_golden(@area, "missing_tool_use_id_is_minted", steps)

      assert [%{"params" => %{"toolCall" => %{"toolCallId" => "tool_<1>"}}}] =
               acp_requests(transcript)
    end

    test "camel_case_tool_use_id_is_accepted" do
      steps =
        session() ++
          [
            Flows.can_use_tool("req-1", %{"tool_use_id" => nil, "toolUseID" => "toolu_camel"}),
            Flows.select("allow_once")
          ]

      ClaudeGolden.assert_golden(@area, "camel_case_tool_use_id_is_accepted", steps)
    end

    test "known_tool_call_is_not_re_announced" do
      steps =
        session() ++
          [
            Flows.prompt("acp-prompt", "run it"),
            Flows.assistant([Flows.tool_use("toolu_1", "Bash", %{"command" => "ls -la"})]),
            {:note, "The tool_call update was already emitted by the assistant message"},
            Flows.can_use_tool(),
            Flows.select("allow_once")
          ]

      transcript = ClaudeGolden.assert_golden(@area, "known_tool_call_is_not_re_announced", steps)

      assert ["tool_call", "tool_call_update"] = ClaudeGolden.update_types(transcript)
    end

    test "unknown_tool_call_is_announced_as_pending" do
      steps = session() ++ [Flows.can_use_tool(), Flows.select("allow_once")]

      transcript =
        ClaudeGolden.assert_golden(@area, "unknown_tool_call_is_announced_as_pending", steps)

      assert ["tool_call"] = ClaudeGolden.update_types(transcript)
    end

    test "a_tool_named_by_a_request_is_remembered_for_its_result" do
      steps =
        session() ++
          [
            Flows.prompt("acp-prompt", "run it"),
            Flows.can_use_tool(),
            Flows.select("allow_once"),
            {:note, "The result update carries the tool name the request registered"},
            Flows.tool_result("toolu_1", "total 0")
          ]

      ClaudeGolden.assert_golden(
        @area,
        "a_tool_named_by_a_request_is_remembered_for_its_result",
        steps
      )
    end
  end

  describe "ExitPlanMode" do
    test "exit_plan_offers_auto_by_default" do
      steps = session() ++ [exit_plan_request(), Flows.select("exit-plan-auto")]

      transcript = ClaudeGolden.assert_golden(@area, "exit_plan_offers_auto_by_default", steps)

      assert [%{"params" => params}] = acp_requests(transcript)

      assert Enum.map(params["options"], & &1["optionId"]) == [
               "exit-plan-auto",
               "exit-plan-default",
               "reject"
             ]
    end

    test "exit_plan_offers_auto_when_the_model_supports_it" do
      steps =
        auto_session() ++ [exit_plan_request(), Flows.select("exit-plan-auto")]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "exit_plan_offers_auto_when_the_model_supports_it",
          steps
        )

      assert [%{"params" => params}] = acp_requests(transcript)
      assert hd(params["options"])["optionId"] == "exit-plan-auto"
    end

    test "exit_plan_prefers_auto_over_bypass" do
      steps =
        session(init: [allow_dangerously_skip_permissions: true]) ++
          [exit_plan_request(), Flows.select("exit-plan-auto")]

      transcript = ClaudeGolden.assert_golden(@area, "exit_plan_prefers_auto_over_bypass", steps)

      assert [%{"params" => params}] = acp_requests(transcript)
      assert hd(params["options"])["optionId"] == "exit-plan-auto"
    end

    test "exit_plan_auto_falls_back_for_a_model_without_auto" do
      steps = no_auto_session() ++ [exit_plan_request(), Flows.select("exit-plan-auto")]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "exit_plan_auto_falls_back_for_a_model_without_auto",
          steps
        )

      assert %{tag: :messages_and_write, writes: [%{"response" => %{"response" => response}}]} =
               ClaudeGolden.last_result(transcript)

      assert [%{"type" => "setMode", "mode" => "acceptEdits"}] = response["updatedPermissions"]
      assert ["tool_call", "agent_message_chunk"] = ClaudeGolden.update_types(transcript)
    end

    test "exit_plan_default_stays_temporary" do
      steps = session() ++ [exit_plan_request(), Flows.select("exit-plan-default")]

      transcript = ClaudeGolden.assert_golden(@area, "exit_plan_default_stays_temporary", steps)

      assert %{writes: [%{"response" => %{"response" => response}}]} =
               ClaudeGolden.last_result(transcript)

      assert response["decisionClassification"] == "user_temporary"
      assert [%{"mode" => "default"}] = response["updatedPermissions"]
    end

    test "exit_plan_rejection_keeps_planning" do
      steps = session() ++ [exit_plan_request(), Flows.select("reject")]

      transcript = ClaudeGolden.assert_golden(@area, "exit_plan_rejection_keeps_planning", steps)

      assert %{writes: [%{"response" => %{"response" => response}}]} =
               ClaudeGolden.last_result(transcript)

      assert response["message"] == "User chose to keep planning"
      assert response["interrupt"] == true
    end

    test "exit_plan_option_that_was_not_offered_fails_closed" do
      steps =
        session() ++
          [
            {:note, "bypass is not in this catalog, so selecting it must not change the mode"},
            exit_plan_request(),
            Flows.select("exit-plan-bypass")
          ]

      ClaudeGolden.assert_golden(
        @area,
        "exit_plan_option_that_was_not_offered_fails_closed",
        steps
      )
    end

    test "exit_plan_title_is_ready_to_code" do
      steps = session() ++ [exit_plan_request(), Flows.select("exit-plan-default")]

      transcript = ClaudeGolden.assert_golden(@area, "exit_plan_title_is_ready_to_code", steps)

      assert [%{"_meta" => %{"permission" => %{"title" => "Ready to code?"}}}] =
               acp_requests(transcript)
    end
  end

  describe "permission modes" do
    test "set_mode_writes_the_permission_mode_control" do
      steps = session() ++ [Flows.set_mode("acp-mode", "acceptEdits")]

      transcript =
        ClaudeGolden.assert_golden(@area, "set_mode_writes_the_permission_mode_control", steps)

      assert %{
               tag: :reply_and_write,
               writes: [
                 %{"request" => %{"subtype" => "set_permission_mode", "mode" => "acceptEdits"}}
               ]
             } = ClaudeGolden.last_result(transcript)
    end

    test "set_mode_plan_and_default_round_trip" do
      steps =
        session() ++
          [Flows.set_mode("acp-mode-1", "plan"), Flows.set_mode("acp-mode-2", "default")]

      ClaudeGolden.assert_golden(@area, "set_mode_plan_and_default_round_trip", steps)
    end

    test "set_mode_auto_falls_back_without_model_support" do
      steps =
        no_auto_session() ++
          [
            Flows.set_mode("acp-mode-1", "auto"),
            {:note, "the notice is published once, however often Auto is re-selected"},
            Flows.set_mode("acp-mode-2", "auto")
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "set_mode_auto_falls_back_without_model_support", steps)

      assert %{
               tag: :messages_and_reply_and_write,
               reply: %{"modes" => %{"currentModeId" => "acceptEdits"}},
               writes: [%{"request" => %{"mode" => "acceptEdits"}}]
             } = ClaudeGolden.last_result(transcript)

      assert ["agent_message_chunk", "current_mode_update", "current_mode_update"] =
               ClaudeGolden.update_types(transcript)
    end

    test "set_mode_auto_is_accepted_for_a_model_that_supports_it" do
      steps = auto_session() ++ [Flows.set_mode("acp-mode", "auto")]

      ClaudeGolden.assert_golden(
        @area,
        "set_mode_auto_is_accepted_for_a_model_that_supports_it",
        steps
      )
    end

    test "set_mode_bypass_requires_the_dangerous_opt_in" do
      steps = session() ++ [Flows.set_mode("acp-mode", "bypassPermissions")]

      ClaudeGolden.assert_golden(@area, "set_mode_bypass_requires_the_dangerous_opt_in", steps)
    end

    test "set_mode_bypass_is_accepted_with_the_dangerous_opt_in" do
      steps =
        session(init: [allow_dangerously_skip_permissions: true]) ++
          [Flows.set_mode("acp-mode", "bypassPermissions")]

      ClaudeGolden.assert_golden(
        @area,
        "set_mode_bypass_is_accepted_with_the_dangerous_opt_in",
        steps
      )
    end

    test "set_mode_rejects_an_unknown_mode" do
      steps = session() ++ [Flows.set_mode("acp-mode", "yolo")]

      ClaudeGolden.assert_golden(@area, "set_mode_rejects_an_unknown_mode", steps)
    end

    test "mode_config_option_is_an_alias_for_set_mode" do
      steps = session() ++ [Flows.set_config("acp-config", "mode", "plan")]

      transcript =
        ClaudeGolden.assert_golden(@area, "mode_config_option_is_an_alias_for_set_mode", steps)

      assert %{reply: %{"modes" => %{"currentModeId" => "plan"}}} =
               ClaudeGolden.last_result(transcript)
    end

    test "permission_mode_config_option_is_an_alias_for_set_mode" do
      steps = session() ++ [Flows.set_config("acp-config", "permission_mode", "acceptEdits")]

      ClaudeGolden.assert_golden(
        @area,
        "permission_mode_config_option_is_an_alias_for_set_mode",
        steps
      )
    end

    test "an_initial_bypass_mode_is_advertised" do
      steps = session(init: [permission_mode: :bypass]) ++ [:modes]

      transcript =
        ClaudeGolden.assert_golden(@area, "an_initial_bypass_mode_is_advertised", steps)

      assert %{reply: %{"modes" => %{"currentModeId" => "bypassPermissions"}}} =
               transcript |> Enum.at(1) |> Map.get(:result)
    end
  end

  describe "allowDangerouslySkipPermissions opt-out" do
    test "opt_out_removes_bypass_from_the_catalog" do
      steps = [
        {:init, allow_dangerously_skip_permissions: true},
        Flows.session_new("acp-new", opt_out(false))
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "opt_out_removes_bypass_from_the_catalog", steps)

      assert %{reply: %{"modes" => %{"availableModes" => modes}}} =
               ClaudeGolden.last_result(transcript)

      refute "bypassPermissions" in Enum.map(modes, & &1["id"])
    end

    test "opt_out_clamps_an_active_bypass_mode" do
      steps = [
        {:init, permission_mode: :bypass},
        Flows.session_new("acp-new", opt_out(false))
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "opt_out_clamps_an_active_bypass_mode", steps)

      assert %{
               tag: :reply_and_write,
               writes: [
                 %{"request" => %{"subtype" => "set_permission_mode", "mode" => "default"}}
               ]
             } = ClaudeGolden.last_result(transcript)
    end

    test "opt_out_refuses_a_later_set_mode_bypass" do
      steps =
        [
          {:init, allow_dangerously_skip_permissions: true},
          Flows.session_new("acp-new", opt_out(false))
        ] ++ [Flows.set_mode("acp-mode", "bypassPermissions")]

      ClaudeGolden.assert_golden(@area, "opt_out_refuses_a_later_set_mode_bypass", steps)
    end

    test "opt_in_true_does_not_override_the_adapter_option" do
      steps = [Flows.session_new("acp-new", opt_out(true))]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "opt_in_true_does_not_override_the_adapter_option",
          steps
        )

      assert %{reply: %{"modes" => %{"availableModes" => modes}}} =
               ClaudeGolden.last_result(transcript)

      assert Enum.map(modes, & &1["id"]) == ["default", "acceptEdits", "plan", "auto"]
    end

    test "opt_out_applies_to_session_resume" do
      steps = [
        {:init, permission_mode: :bypass},
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-resume",
           "method" => "session/resume",
           "params" =>
             Map.merge(
               %{"sessionId" => Flows.session_uuid(1), "cwd" => Flows.cwd()},
               opt_out(false)
             )
         }}
      ]

      ClaudeGolden.assert_golden(@area, "opt_out_applies_to_session_resume", steps)
    end

    test "opt_out_applies_to_session_load" do
      session = Flows.session_uuid(1)

      steps = [
        {:init, permission_mode: :bypass},
        Flows.session_jsonl(session, Flows.summary_entries("loaded")),
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-load",
           "method" => "session/load",
           "params" => Map.merge(%{"sessionId" => session, "cwd" => Flows.cwd()}, opt_out(false))
         }}
      ]

      transcript = ClaudeGolden.assert_golden(@area, "opt_out_applies_to_session_load", steps)

      assert %{tag: :messages_and_reply_and_write} = ClaudeGolden.last_result(transcript)
    end

    test "a_session_without_the_opt_out_keeps_bypass" do
      steps = [
        {:init, allow_dangerously_skip_permissions: true},
        Flows.session_new()
      ]

      ClaudeGolden.assert_golden(@area, "a_session_without_the_opt_out_keeps_bypass", steps)
    end
  end

  describe "AskUserQuestion" do
    test "single_select_question_round_trips" do
      steps =
        form_session() ++
          [
            ask_user_question(),
            Flows.reply_last(%{"action" => "accept", "content" => %{"question_0" => "Blue"}})
          ]

      transcript = ClaudeGolden.assert_golden(@area, "single_select_question_round_trips", steps)

      assert [%{"method" => "elicitation/create", "params" => params}] = acp_requests(transcript)
      assert params["mode"] == "form"
      assert params["message"] == "Which color?"

      assert %{writes: [%{"response" => %{"response" => %{"updatedInput" => input}}}]} =
               ClaudeGolden.last_result(transcript)

      assert input["answers"] == %{"Which color?" => "Blue"}
    end

    test "single_select_custom_answer_replaces_an_empty_pick" do
      steps =
        form_session() ++
          [
            ask_user_question(),
            Flows.reply_last(%{
              "action" => "accept",
              "content" => %{"question_0_custom" => "  Teal  "}
            })
          ]

      ClaudeGolden.assert_golden(
        @area,
        "single_select_custom_answer_replaces_an_empty_pick",
        steps
      )
    end

    test "single_select_custom_answer_becomes_a_note" do
      steps =
        form_session() ++
          [
            ask_user_question(),
            Flows.reply_last(%{
              "action" => "accept",
              "content" => %{"question_0" => "Blue", "question_0_custom" => "but lighter"}
            })
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "single_select_custom_answer_becomes_a_note", steps)

      assert %{writes: [%{"response" => %{"response" => %{"updatedInput" => input}}}]} =
               ClaudeGolden.last_result(transcript)

      assert input["annotations"] == %{"Which color?" => %{"notes" => "but lighter"}}
    end

    test "multi_select_joins_its_picks" do
      steps =
        form_session() ++
          [
            ask_user_question(multi_select: true),
            Flows.reply_last(%{
              "action" => "accept",
              "content" => %{"question_0" => ["Blue", "Green"]}
            })
          ]

      ClaudeGolden.assert_golden(@area, "multi_select_joins_its_picks", steps)
    end

    test "multi_select_appends_a_custom_answer" do
      steps =
        form_session() ++
          [
            ask_user_question(multi_select: true),
            Flows.reply_last(%{
              "action" => "accept",
              "content" => %{"question_0" => ["Blue"], "question_0_custom" => "Teal"}
            })
          ]

      ClaudeGolden.assert_golden(@area, "multi_select_appends_a_custom_answer", steps)
    end

    test "multi_select_quotes_an_answer_containing_the_separator" do
      steps =
        form_session() ++
          [
            ask_user_question(multi_select: true),
            Flows.reply_last(%{
              "action" => "accept",
              "content" => %{
                "question_0" => ["Blue"],
                "question_0_custom" => "Redis, not Memcached"
              }
            })
          ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "multi_select_quotes_an_answer_containing_the_separator",
          steps
        )

      assert %{writes: [%{"response" => %{"response" => %{"updatedInput" => input}}}]} =
               ClaudeGolden.last_result(transcript)

      assert input["answers"] == %{"Which color?" => ~s(Blue, "Redis, not Memcached")}
    end

    test "an_empty_answer_is_omitted" do
      steps =
        form_session() ++
          [
            ask_user_question(),
            Flows.reply_last(%{"action" => "accept", "content" => %{"question_0" => ""}})
          ]

      ClaudeGolden.assert_golden(@area, "an_empty_answer_is_omitted", steps)
    end

    test "decline_allows_with_no_answers" do
      steps = form_session() ++ [ask_user_question(), Flows.reply_last(%{"action" => "decline"})]

      transcript = ClaudeGolden.assert_golden(@area, "decline_allows_with_no_answers", steps)

      assert %{writes: [%{"response" => %{"response" => response}}]} =
               ClaudeGolden.last_result(transcript)

      assert response["behavior"] == "allow"
      assert response["updatedInput"]["answers"] == %{}
    end

    test "cancel_denies_and_interrupts" do
      steps = form_session() ++ [ask_user_question(), Flows.reply_last(%{"action" => "cancel"})]

      ClaudeGolden.assert_golden(@area, "cancel_denies_and_interrupts", steps)
    end

    test "a_client_error_cancels_the_question" do
      steps = form_session() ++ [ask_user_question(), Flows.error_reply()]

      transcript = ClaudeGolden.assert_golden(@area, "a_client_error_cancels_the_question", steps)

      assert %{
               writes: [
                 %{"response" => %{"response" => %{"behavior" => "deny", "interrupt" => true}}}
               ]
             } = ClaudeGolden.last_result(transcript)
    end

    test "existing_annotations_are_merged" do
      steps =
        form_session() ++
          [
            ask_user_question(
              input_extra: %{"annotations" => %{"Other" => %{"notes" => "keep"}}}
            ),
            Flows.reply_last(%{
              "action" => "accept",
              "content" => %{"question_0" => "Blue", "question_0_custom" => "but lighter"}
            })
          ]

      ClaudeGolden.assert_golden(@area, "existing_annotations_are_merged", steps)
    end

    test "two_questions_describe_each_field" do
      steps =
        form_session() ++
          [
            ask_user_question(
              questions: [
                question("Which color?", "Color", ["Blue", "Green"]),
                question("Which size?", "Size", ["Small", "Large"])
              ]
            ),
            Flows.reply_last(%{
              "action" => "accept",
              "content" => %{"question_0" => "Blue", "question_1" => "Large"}
            })
          ]

      transcript = ClaudeGolden.assert_golden(@area, "two_questions_describe_each_field", steps)

      assert [%{"params" => params}] = acp_requests(transcript)
      assert params["message"] == "Please answer the following questions."
    end

    test "option_previews_travel_in_meta" do
      steps =
        form_session() ++
          [
            ask_user_question(
              questions: [
                %{
                  "question" => "Which plan?",
                  "header" => "Plan",
                  "multiSelect" => false,
                  "options" => [
                    %{"label" => "Small", "description" => "One box", "preview" => "1 node"},
                    %{"label" => "Large"}
                  ]
                }
              ]
            ),
            Flows.reply_last(%{"action" => "accept", "content" => %{"question_0" => "Small"}})
          ]

      ClaudeGolden.assert_golden(@area, "option_previews_travel_in_meta", steps)
    end

    test "without_form_elicitation_the_question_is_denied" do
      steps = session() ++ [ask_user_question()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "without_form_elicitation_the_question_is_denied",
          steps
        )

      assert %{
               tag: :skip_and_write,
               writes: [%{"response" => %{"response" => %{"message" => message}}}]
             } = ClaudeGolden.last_result(transcript)

      assert message == "AskUserQuestion requires ACP form elicitation support"
    end

    test "questions_without_valid_options_are_denied" do
      steps =
        form_session() ++
          [
            ask_user_question(
              questions: [
                %{"question" => "No options?", "options" => []},
                %{"question" => "Blank label?", "options" => [%{"label" => ""}]},
                %{"options" => [%{"label" => "Orphan"}]}
              ]
            )
          ]

      ClaudeGolden.assert_golden(@area, "questions_without_valid_options_are_denied", steps)
    end

    test "invalid_questions_are_filtered_before_the_form" do
      steps =
        form_session() ++
          [
            ask_user_question(
              questions: [
                %{"question" => "Bad", "options" => []},
                question("Which color?", "Color", ["Blue", "Green"])
              ]
            ),
            Flows.reply_last(%{"action" => "accept", "content" => %{"question_0" => "Green"}})
          ]

      ClaudeGolden.assert_golden(@area, "invalid_questions_are_filtered_before_the_form", steps)
    end
  end

  # -- helpers ---------------------------------------------------------------

  defp session(opts \\ []) do
    [{:init, Keyword.get(opts, :init, [])}, Flows.session_new()]
  end

  defp form_session do
    Flows.open_session(capabilities: @form_caps)
  end

  defp auto_session do
    [{:init, model: "sonnet"}] ++
      [
        Flows.initialize(),
        :post_connect,
        {:respond_control, "initialize", %{"models" => @auto_models}},
        Flows.session_new()
      ]
  end

  # A session on a model Claude described as not supporting Auto mode, which
  # is what arms the Auto fallback (an undescribed model is assumed capable).
  defp no_auto_session do
    [{:init, model: "opus"}] ++
      [
        Flows.initialize(),
        :post_connect,
        {:respond_control, "initialize", %{"models" => @no_auto_models}},
        Flows.session_new()
      ]
  end

  defp opt_out(value) do
    %{"_meta" => %{"claudeCode" => %{"options" => %{"allowDangerouslySkipPermissions" => value}}}}
  end

  defp exit_plan_request do
    Flows.can_use_tool("req-plan", %{
      "tool_name" => "ExitPlanMode",
      "tool_use_id" => "toolu_plan",
      "input" => %{"plan" => "1. Read\n2. Edit"}
    })
  end

  defp ask_user_question(opts \\ []) do
    questions =
      Keyword.get(opts, :questions, [
        question(
          "Which color?",
          "Color",
          ["Blue", "Green"],
          Keyword.get(opts, :multi_select, false)
        )
      ])

    input =
      Map.merge(%{"questions" => questions}, Keyword.get(opts, :input_extra, %{}))

    Flows.can_use_tool("req-ask", %{
      "tool_name" => "AskUserQuestion",
      "tool_use_id" => "toolu_ask",
      "input" => input
    })
  end

  defp question(text, header, labels, multi_select \\ false) do
    %{
      "question" => text,
      "header" => header,
      "multiSelect" => multi_select,
      "options" => Enum.map(labels, &%{"label" => &1, "description" => "#{&1} option"})
    }
  end

  # Requests the adapter sent to the client; `session/update` notifications
  # carry a method too, so an id is what distinguishes a request.
  defp acp_requests(transcript) do
    transcript
    |> ClaudeGolden.messages()
    |> Enum.filter(&(Map.has_key?(&1, "method") and Map.has_key?(&1, "id")))
  end
end
