defmodule ExMCP.ACP.Adapters.Codex.PermissionsGoldenTest do
  @moduledoc """
  Characterization gate for the Codex ACP adapter's permission and
  elicitation wire behavior (area A3 of `docs/POST_1_0_MAINTENANCE_PLAN.md`,
  "Codex adapter restructuring" / "Characterization gate").

  Each test drives `ExMCP.ACP.Adapters.Codex` through `ExMCP.Test.CodexGolden`
  and compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/codex/permissions/`. The fixtures pin:

    * the ACP `session/request_permission` request (tool call, option set,
      `_meta`) produced for every app-server approval request the adapter
      handles - `item/commandExecution/requestApproval` (default decisions,
      execpolicy amendments, network approval context and policy amendments,
      additional permissions, explicit `availableDecisions`),
      `item/fileChange/requestApproval`, `item/permissions/requestApproval`,
      the legacy `execCommandApproval` / `applyPatchApproval` aliases, and
      `mcpServer/elicitation/request` on the permission fallback (persist
      options from `_meta.persist`, unknown scopes filtered);
    * command option-set normalization and validation: the stable
      allow-before-reject sort of explicit `availableDecisions`, an explicit
      `availableDecisions: null` falling back to the defaults, the network
      context taking precedence over `additionalPermissions`, an explicit
      `acceptForSession` on an `additionalPermissions` request, network
      amendment option indices following the explicit decision order (and
      the reply being mapped by that same order), non-map network
      amendments being ignored, and every fail-closed shape (duplicate ids,
      no allow or no reject decision, amendments without their context, an
      empty execpolicy amendment, a network amendment whose action is
      neither `allow` nor `deny` or whose host/action pair was not proposed)
      answering `cancel` without an ACP request;
    * `_meta.permission` carrying no `description` when the reason is blank
      or not a string (the blank case is pinned by the whole request: a
      mutation that makes `trimmed_permission_text/1` return `""` instead of
      `nil` is wire-equivalent because `maybe_put/3` delegates to
      `ExMCP.Internal.Maps.put_non_empty/3`, which drops `""` as well as
      `nil`; only a variant that emits the untrimmed whitespace is
      observable, and that one is caught), the legacy tool call id
      precedence `itemId` > `callId` > `approvalId`, approval / user-input
      requests without a `threadId` being routed to the only open session,
      and `threadId` winning over `sessionId` when both name an open session;
    * the exact native decision written back for every ACP reply shape:
      each selected option, a cancelled outcome, an unknown option id, a
      malformed result, an ACP error response, and replies for unknown,
      already-answered, or server-resolved request ids; a non-object
      `permissions` profile granting an empty profile;
    * `item/tool/requestUserInput` form elicitation round trips, including
      secret-field and missing-capability refusals (a boolean `form`
      capability counts as missing), `__other` id collisions, blank
      `__other` text falling back to the selected option, an empty-string
      answer omitting its question, the request message winning over a
      single question's text, `required` omitted when every question is
      optional, `isOther` without options or with an explicit empty options
      list, and a request without any questions;
    * `mcpServer/elicitation/request` in form mode (legacy `enumNames`
      normalization, the `openai/form` fallback, `toolCallId` / `sessionId`
      precedence, `threadId` winning over `sessionId`, the default tool
      call title on the permission fallback) and url mode (the default
      prompt, completion of the accepted client UI via
      `serverRequest/resolved`, concurrent url elicitations, requests without
      an `elicitationId` never being completed, and a resolved url
      elicitation being forgotten so that neither a duplicate resolution nor
      a later `session/close` completes it a second time);
    * `serverRequest/resolved` dropping pending user-input and form
      elicitation entries so late client replies write nothing;
    * the native error for unsupported app-server requests;
    * the ChatGPT device-code login flow and its request-scoped url
      elicitation, including completion without client consent, the no-code
      prompt for an empty `userCode`, and the default failure message when
      `account/login/completed` carries no error;
    * the `approvalsReviewer` / approval policy wire params `turn/start`
      carries per session mode.

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

  @area "permissions"
  @thread "thread-1"

  @form_caps %{"elicitation" => %{"form" => %{}}}
  @url_caps %{"elicitation" => %{"url" => %{}}}
  @form_and_url_caps %{"elicitation" => %{"form" => %{}, "url" => %{}}}
  # A boolean capability value is not a capability object and must not count as support.
  @boolean_form_caps %{"elicitation" => %{"form" => true}}

  # -- item/commandExecution/requestApproval ---------------------------------

  describe "command execution approvals" do
    test "command_approval_allow_once_accepts" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "Default decision set (no availableDecisions); reason becomes the description"},
            {:inbound, command_request(100)},
            {:outbound, select("allow_once")}
          ]

      transcript = CodexGolden.assert_golden(@area, "command_approval_allow_once_accepts", steps)

      assert [%{"method" => "session/request_permission", "params" => params}] =
               acp_requests(transcript)

      assert Enum.map(params["options"], & &1["optionId"]) ==
               ["allow_once", "allow_for_session", "decline", "cancel"]

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_allow_for_session_accepts_for_session" do
      steps =
        active_turn_steps() ++
          [{:inbound, command_request(100)}, {:outbound, select("allow_for_session")}]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_allow_for_session_accepts_for_session",
          steps
        )

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "acceptForSession"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_decline_declines" do
      steps =
        active_turn_steps() ++ [{:inbound, command_request(100)}, {:outbound, select("decline")}]

      transcript = CodexGolden.assert_golden(@area, "command_approval_decline_declines", steps)

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "decline"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_cancel_option_cancels" do
      steps =
        active_turn_steps() ++ [{:inbound, command_request(100)}, {:outbound, select("cancel")}]

      transcript =
        CodexGolden.assert_golden(@area, "command_approval_cancel_option_cancels", steps)

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "cancel"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_cancelled_outcome_cancels" do
      steps = active_turn_steps() ++ [{:inbound, command_request(100)}, {:outbound, cancelled()}]

      transcript =
        CodexGolden.assert_golden(@area, "command_approval_cancelled_outcome_cancels", steps)

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "cancel"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_unknown_option_id_cancels" do
      steps =
        active_turn_steps() ++
          [
            {:note, "allow_always is not in the offered set for command approvals"},
            {:inbound, command_request(100)},
            {:outbound, select("allow_always")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "command_approval_unknown_option_id_cancels", steps)

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "cancel"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_malformed_and_error_replies_cancel" do
      steps =
        active_turn_steps() ++
          [
            {:note, "A result without an outcome map is treated as cancel"},
            {:inbound, command_request(100)},
            {:outbound, reply_last(%{"result" => %{"outcome" => "selected"}})},
            {:note, "An ACP error response from the client is also treated as cancel"},
            {:inbound, command_request(101, %{"itemId" => "item-2"})},
            {:outbound,
             reply_last(%{"error" => %{"code" => -32_603, "message" => "client exploded"}})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_malformed_and_error_replies_cancel",
          steps
        )

      assert [
               %{"id" => 100, "result" => %{"decision" => "cancel"}},
               %{"id" => 101, "result" => %{"decision" => "cancel"}}
             ] = transcript |> CodexGolden.writes() |> Enum.take(-2)
    end

    test "command_approval_reply_for_unknown_id_is_skipped" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, command_request(100)},
            {:note, "Replies whose id does not match a pending client request are dropped"},
            {:outbound,
             %{
               "jsonrpc" => "2.0",
               "id" => "codex-permission-424242",
               "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "allow_once"}}
             }},
            {:outbound,
             %{
               "jsonrpc" => "2.0",
               "id" => 7,
               "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "allow_once"}}
             }},
            {:note, "The real request is still pending and can be answered afterwards"},
            {:outbound, select("allow_once", 0)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_reply_for_unknown_id_is_skipped",
          steps
        )

      assert [%{tag: :ok, skipped: true}, %{tag: :ok, skipped: true}] =
               transcript |> Enum.take(-4) |> Enum.take(2) |> Enum.map(& &1.result)

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_duplicate_reply_is_skipped" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, command_request(100)},
            {:outbound, select("allow_once")},
            {:note, "A second reply to the same request id is dropped without a write"},
            {:outbound, select("decline")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "command_approval_duplicate_reply_is_skipped", steps)

      assert %{tag: :ok, skipped: true} = CodexGolden.last_result(transcript)

      assert [%{"id" => 100, "result" => %{"decision" => "accept"}}] =
               Enum.take(CodexGolden.writes(transcript), -1)
    end

    test "command_approval_reply_after_server_resolved_is_skipped" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, command_request(100)},
            {:note, "The app-server resolves the request itself (e.g. the turn was interrupted)"},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }},
            {:note, "The late client reply finds no pending entry and is dropped"},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_reply_after_server_resolved_is_skipped",
          steps
        )

      assert [%{tag: :skip, skipped: true}, %{tag: :ok, skipped: true}] =
               transcript
               |> Enum.take(-3)
               |> Enum.reject(&(&1.step.kind == :note))
               |> Enum.map(& &1.result)

      refute Enum.any?(CodexGolden.writes(transcript), &(&1["id"] == 100))
    end

    test "command_approval_execpolicy_amendment_option" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "A proposed execpolicy amendment adds an allow_always option to the defaults"},
            {:inbound,
             command_request(100, %{
               "command" => "touch /tmp/contract.hwp",
               "proposedExecpolicyAmendment" => ["touch", "/tmp/contract.hwp"]
             })},
            {:outbound, select("accept_execpolicy_amendment")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "command_approval_execpolicy_amendment_option", steps)

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)

      assert Enum.map(options, & &1["optionId"]) ==
               [
                 "allow_once",
                 "allow_for_session",
                 "accept_execpolicy_amendment",
                 "decline",
                 "cancel"
               ]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{
                     "decision" => %{
                       "acceptWithExecpolicyAmendment" => %{
                         "execpolicy_amendment" => ["touch", "/tmp/contract.hwp"]
                       }
                     }
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "command_approval_multiline_execpolicy_prefix_is_omitted" do
      steps =
        active_turn_steps() ++
          [
            {:note, "An amendment whose prefix contains a newline is not offered as an option"},
            {:inbound,
             command_request(100, %{
               "command" => "printf 'a\\nb'",
               "proposedExecpolicyAmendment" => ["printf", "a\nb"]
             })},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_multiline_execpolicy_prefix_is_omitted",
          steps
        )

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)
      refute Enum.any?(options, &(&1["optionId"] == "accept_execpolicy_amendment"))
    end

    test "command_approval_explicit_available_decisions" do
      amendment = %{
        "acceptWithExecpolicyAmendment" => %{
          "execpolicy_amendment" => ["python3", "fill_contract.py"]
        }
      }

      steps =
        active_turn_steps() ++
          [
            {:note,
             "availableDecisions restricts and orders the option set; structured decisions round-trip"},
            {:inbound,
             command_request(100, %{
               "command" => "python3 fill_contract.py",
               "proposedExecpolicyAmendment" => ["python3", "fill_contract.py"],
               "availableDecisions" => ["accept", amendment, "decline"]
             })},
            {:outbound, select("accept_execpolicy_amendment")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "command_approval_explicit_available_decisions", steps)

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)

      assert Enum.map(options, & &1["optionId"]) ==
               ["allow_once", "accept_execpolicy_amendment", "decline"]

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => ^amendment}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_explicit_decisions_are_reordered" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "Options are sorted allow_once, allow_always, then rejects; the sort is stable, so cancel stays ahead of decline"},
            {:inbound,
             command_request(100, %{
               "availableDecisions" => ["cancel", "decline", "acceptForSession", "accept"]
             })},
            {:outbound, select("decline")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_explicit_decisions_are_reordered",
          steps
        )

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)

      assert Enum.map(options, & &1["optionId"]) ==
               ["allow_once", "allow_for_session", "cancel", "decline"]

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "decline"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_network_amendments_ignore_non_map_entries" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "Only map entries of proposedNetworkPolicyAmendments become options; the index counts offered options only"},
            {:inbound,
             command_request(100, %{
               "command" => "curl https://example.test/api",
               "networkApprovalContext" => %{"host" => "example.test", "protocol" => "https"},
               "proposedNetworkPolicyAmendments" => [
                 "allow example.test",
                 nil,
                 %{"action" => "allow", "host" => "example.test"}
               ]
             })},
            {:outbound, select("apply_network_policy_amendment:0")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_network_amendments_ignore_non_map_entries",
          steps
        )

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)

      assert Enum.map(options, & &1["optionId"]) ==
               [
                 "allow_once",
                 "allow_for_session",
                 "apply_network_policy_amendment:0",
                 "decline",
                 "cancel"
               ]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{
                     "decision" => %{
                       "applyNetworkPolicyAmendment" => %{
                         "network_policy_amendment" => %{
                           "action" => "allow",
                           "host" => "example.test"
                         }
                       }
                     }
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "command_approval_invalid_available_decisions_cancel_immediately" do
      steps =
        active_turn_steps() ++
          [
            {:note, "No reject decision offered: fail closed without asking the client"},
            {:inbound, command_request(100, %{"availableDecisions" => ["accept"]})},
            {:note, "Empty decision list"},
            {:inbound, command_request(101, %{"itemId" => "item-2", "availableDecisions" => []})},
            {:note, "Non-list decision value"},
            {:inbound,
             command_request(102, %{"itemId" => "item-3", "availableDecisions" => "accept"})},
            {:note, "Unknown decision string"},
            {:inbound,
             command_request(103, %{
               "itemId" => "item-4",
               "availableDecisions" => ["accept", "reject"]
             })},
            {:note, "Execpolicy amendment that does not match proposedExecpolicyAmendment"},
            {:inbound,
             command_request(104, %{
               "itemId" => "item-5",
               "proposedExecpolicyAmendment" => ["mix", "test"],
               "availableDecisions" => [
                 "accept",
                 %{"acceptWithExecpolicyAmendment" => %{"execpolicy_amendment" => ["rm", "-rf"]}},
                 "decline"
               ]
             })},
            {:note, "Network amendment for a host other than the approval context host"},
            {:inbound,
             command_request(105, %{
               "itemId" => "item-6",
               "command" => "curl https://example.test",
               "networkApprovalContext" => %{"host" => "example.test", "protocol" => "https"},
               "proposedNetworkPolicyAmendments" => [
                 %{"action" => "allow", "host" => "example.test"}
               ],
               "availableDecisions" => [
                 "accept",
                 %{
                   "applyNetworkPolicyAmendment" => %{
                     "network_policy_amendment" => %{"action" => "allow", "host" => "evil.test"}
                   }
                 },
                 "cancel"
               ]
             })},
            {:note, "Duplicate option ids"},
            {:inbound,
             command_request(106, %{
               "itemId" => "item-7",
               "availableDecisions" => ["accept", "accept", "decline"]
             })},
            {:note, "No allow decision offered"},
            {:inbound,
             command_request(107, %{
               "itemId" => "item-8",
               "availableDecisions" => ["decline", "cancel"]
             })},
            {:note, "Network amendment without a networkApprovalContext"},
            {:inbound,
             command_request(108, %{
               "itemId" => "item-9",
               "command" => "curl https://example.test",
               "proposedNetworkPolicyAmendments" => [
                 %{"action" => "allow", "host" => "example.test"}
               ],
               "availableDecisions" => [
                 "accept",
                 %{
                   "applyNetworkPolicyAmendment" => %{
                     "network_policy_amendment" => %{
                       "action" => "allow",
                       "host" => "example.test"
                     }
                   }
                 },
                 "decline"
               ]
             })}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_invalid_available_decisions_cancel_immediately",
          steps
        )

      assert acp_requests(transcript) == []

      assert Enum.map(100..108, &%{"id" => &1, "result" => %{"decision" => "cancel"}}) ==
               Enum.take(CodexGolden.writes(transcript), -9)
    end

    test "command_approval_network_context_default_options" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "Network approval context titles the prompt and adds one option per proposed amendment"},
            {:inbound,
             command_request(100, %{
               "command" => "curl https://example.test/api",
               "networkApprovalContext" => %{"host" => "example.test", "protocol" => "https"},
               "proposedNetworkPolicyAmendments" => [
                 %{"action" => "allow", "host" => "example.test"},
                 %{"action" => "deny", "host" => "example.test"}
               ]
             })},
            {:outbound, select("apply_network_policy_amendment:0")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_network_context_default_options",
          steps
        )

      assert [%{"params" => %{"options" => options, "_meta" => meta}}] =
               acp_requests(transcript)

      assert meta["permission"]["title"] == "Allow network access?"

      assert Enum.map(options, &{&1["optionId"], &1["kind"]}) == [
               {"allow_once", "allow_once"},
               {"allow_for_session", "allow_always"},
               {"apply_network_policy_amendment:0", "allow_always"},
               {"apply_network_policy_amendment:1", "reject_always"},
               {"decline", "reject_once"},
               {"cancel", "reject_once"}
             ]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{
                     "decision" => %{
                       "applyNetworkPolicyAmendment" => %{
                         "network_policy_amendment" => %{
                           "action" => "allow",
                           "host" => "example.test"
                         }
                       }
                     }
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "command_approval_deny_network_amendment_rejects_always" do
      deny = %{
        "applyNetworkPolicyAmendment" => %{
          "network_policy_amendment" => %{"action" => "deny", "host" => "example.test"}
        }
      }

      steps =
        active_turn_steps() ++
          [
            {:inbound,
             command_request(100, %{
               "command" => "curl https://example.test",
               "networkApprovalContext" => %{"host" => "example.test", "protocol" => "https"},
               "proposedNetworkPolicyAmendments" => [
                 %{"action" => "deny", "host" => "example.test"}
               ],
               "availableDecisions" => ["accept", deny, "cancel"]
             })},
            {:outbound, select("apply_network_policy_amendment:0")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_deny_network_amendment_rejects_always",
          steps
        )

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)

      assert %{"optionId" => "apply_network_policy_amendment:0", "kind" => "reject_always"} =
               Enum.find(options, &(&1["kind"] == "reject_always"))

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => ^deny}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_additional_permissions_options" do
      steps =
        active_turn_steps() ++
          [
            {:note, "additionalPermissions requests default to accept/cancel only"},
            {:inbound,
             command_request(100, %{
               "command" => "npm install",
               "additionalPermissions" => %{"network" => %{"enabled" => true}}
             })},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "command_approval_additional_permissions_options", steps)

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)
      assert Enum.map(options, & &1["optionId"]) == ["allow_once", "cancel"]

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "accept"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_network_context_wins_over_additional_permissions" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "When both networkApprovalContext and additionalPermissions are present the network default set (with its amendments) is offered, not accept/cancel"},
            {:inbound,
             command_request(100, %{
               "command" => "npm install",
               "networkApprovalContext" => %{
                 "host" => "registry.npmjs.org",
                 "protocol" => "https"
               },
               "proposedNetworkPolicyAmendments" => [
                 %{"action" => "allow", "host" => "registry.npmjs.org"}
               ],
               "additionalPermissions" => %{"network" => %{"enabled" => true}}
             })},
            {:outbound, select("allow_for_session")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_network_context_wins_over_additional_permissions",
          steps
        )

      assert [%{"params" => %{"options" => options, "_meta" => meta}}] =
               acp_requests(transcript)

      assert meta["permission"]["title"] == "Allow network access?"

      assert Enum.map(options, &{&1["optionId"], &1["name"]}) == [
               {"allow_once", "Yes, just this once"},
               {"allow_for_session", "Yes, and allow this host for this conversation"},
               {"apply_network_policy_amendment:0", "Yes, and allow this host in the future"},
               {"decline", "No, continue without running it"},
               {"cancel", "No, and tell Codex what to do differently"}
             ]

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "acceptForSession"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_null_available_decisions_use_defaults" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "An explicit availableDecisions: null is treated like an absent key (defaults, including the execpolicy amendment), not like an invalid list"},
            {:inbound,
             command_request(100, %{
               "availableDecisions" => nil,
               "proposedExecpolicyAmendment" => ["mix", "test"]
             })},
            {:outbound, select("allow_for_session")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_null_available_decisions_use_defaults",
          steps
        )

      assert [%{"method" => "session/request_permission", "params" => %{"options" => options}}] =
               acp_requests(transcript)

      assert Enum.map(options, & &1["optionId"]) ==
               [
                 "allow_once",
                 "allow_for_session",
                 "accept_execpolicy_amendment",
                 "decline",
                 "cancel"
               ]

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "acceptForSession"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_empty_execpolicy_amendment_decision_cancels" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "An explicit acceptWithExecpolicyAmendment with an empty execpolicy_amendment is not a recognized decision: fail closed without asking the client"},
            {:inbound,
             command_request(100, %{
               "proposedExecpolicyAmendment" => ["mix", "test"],
               "availableDecisions" => [
                 "accept",
                 %{"acceptWithExecpolicyAmendment" => %{"execpolicy_amendment" => []}},
                 "decline"
               ]
             })}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_empty_execpolicy_amendment_decision_cancels",
          steps
        )

      assert acp_requests(transcript) == []

      assert %{
               tag: :skip_and_write,
               writes: [%{"id" => 100, "result" => %{"decision" => "cancel"}}]
             } =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_network_amendment_invalid_action_cancels" do
      block_amendment = %{"action" => "block", "host" => "example.test"}

      block_decision = %{
        "applyNetworkPolicyAmendment" => %{"network_policy_amendment" => block_amendment}
      }

      network_params = %{
        "command" => "curl https://example.test",
        "networkApprovalContext" => %{"host" => "example.test", "protocol" => "https"},
        "proposedNetworkPolicyAmendments" => [block_amendment]
      }

      steps =
        active_turn_steps() ++
          [
            {:note,
             "An explicit network amendment whose action is neither allow nor deny fails closed, even though it matches the context host and a proposed amendment"},
            {:inbound,
             command_request(
               100,
               Map.put(network_params, "availableDecisions", ["accept", block_decision, "cancel"])
             )},
            {:note,
             "Without availableDecisions the same proposed amendment is not validated: it is offered as a reject_always option and written back verbatim"},
            {:inbound, command_request(101, Map.put(network_params, "itemId", "item-2"))},
            {:outbound, select("apply_network_policy_amendment:0")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_network_amendment_invalid_action_cancels",
          steps
        )

      assert %{
               tag: :skip_and_write,
               writes: [%{"id" => 100, "result" => %{"decision" => "cancel"}}]
             } =
               inbound_result(transcript, 100)

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)

      assert %{
               "optionId" => "apply_network_policy_amendment:0",
               "kind" => "reject_always",
               "name" => "No, and block this host in the future"
             } = Enum.find(options, &(&1["kind"] == "reject_always"))

      assert %{writes: [%{"id" => 101, "result" => %{"decision" => ^block_decision}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_blank_reason_omits_description" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "A whitespace-only reason yields _meta.permission with a title but no description key"},
            {:inbound, command_request(100, %{"reason" => "   \n  "})},
            {:outbound, select("allow_once")},
            {:note, "A non-string reason (JSON null) is dropped the same way"},
            {:inbound, file_change_request(101, %{"itemId" => "item-2", "reason" => nil})},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "command_approval_blank_reason_omits_description", steps)

      assert [
               %{"params" => %{"_meta" => %{"permission" => command_meta}}},
               %{"params" => %{"_meta" => %{"permission" => file_change_meta}}}
             ] = acp_requests(transcript)

      assert command_meta == %{"version" => 1, "title" => "Run command?"}
      assert file_change_meta == %{"version" => 1, "title" => "Make edits?"}

      assert [
               %{"id" => 100, "result" => %{"decision" => "accept"}},
               %{"id" => 101, "result" => %{"decision" => "accept"}}
             ] = Enum.take(CodexGolden.writes(transcript), -2)
    end

    test "command_approval_additional_permissions_explicit_session_option" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "The additionalPermissions defaults never offer acceptForSession, but an explicit availableDecisions can; it is named for the permissions, not the command"},
            {:inbound,
             command_request(100, %{
               "command" => "npm install",
               "additionalPermissions" => %{"network" => %{"enabled" => true}},
               "availableDecisions" => ["accept", "acceptForSession", "cancel"]
             })},
            {:outbound, select("allow_for_session")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_additional_permissions_explicit_session_option",
          steps
        )

      assert [%{"params" => %{"options" => options, "_meta" => meta}}] =
               acp_requests(transcript)

      assert meta["permission"]["title"] == "Run command?"

      assert Enum.map(options, &{&1["optionId"], &1["name"]}) == [
               {"allow_once", "Yes, proceed"},
               {"allow_for_session", "Yes, and allow these permissions for this session"},
               {"cancel", "No, and tell Codex what to do differently"}
             ]

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "acceptForSession"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "command_approval_network_amendment_action_mismatch_cancels" do
      deny_decision = %{
        "applyNetworkPolicyAmendment" => %{
          "network_policy_amendment" => %{"action" => "deny", "host" => "example.test"}
        }
      }

      steps =
        active_turn_steps() ++
          [
            {:note,
             "An explicit deny amendment for the context host is rejected when only an allow amendment was proposed for it: host and action must both match a proposed amendment"},
            {:inbound,
             command_request(100, %{
               "command" => "curl https://example.test",
               "networkApprovalContext" => %{"host" => "example.test", "protocol" => "https"},
               "proposedNetworkPolicyAmendments" => [
                 %{"action" => "allow", "host" => "example.test"}
               ],
               "availableDecisions" => ["accept", deny_decision, "cancel"]
             })}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_network_amendment_action_mismatch_cancels",
          steps
        )

      assert acp_requests(transcript) == []

      assert %{
               tag: :skip_and_write,
               writes: [%{"id" => 100, "result" => %{"decision" => "cancel"}}]
             } = inbound_result(transcript, 100)
    end

    test "command_approval_explicit_network_amendment_order_differs_from_proposed" do
      allow_amendment = %{"action" => "allow", "host" => "example.test"}
      deny_amendment = %{"action" => "deny", "host" => "example.test"}

      deny_decision = %{
        "applyNetworkPolicyAmendment" => %{"network_policy_amendment" => deny_amendment}
      }

      allow_decision = %{
        "applyNetworkPolicyAmendment" => %{"network_policy_amendment" => allow_amendment}
      }

      steps =
        active_turn_steps() ++
          [
            {:note,
             "Amendment option indices follow the explicit availableDecisions order (deny first), not the proposedNetworkPolicyAmendments order (allow first); the reply is mapped by the same explicit list"},
            {:inbound,
             command_request(100, %{
               "command" => "curl https://example.test",
               "networkApprovalContext" => %{"host" => "example.test", "protocol" => "https"},
               "proposedNetworkPolicyAmendments" => [allow_amendment, deny_amendment],
               "availableDecisions" => ["accept", deny_decision, allow_decision, "cancel"]
             })},
            {:outbound, select("apply_network_policy_amendment:0")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "command_approval_explicit_network_amendment_order_differs_from_proposed",
          steps
        )

      assert [%{"params" => %{"options" => options}}] = acp_requests(transcript)

      assert Enum.map(options, &{&1["optionId"], &1["kind"]}) == [
               {"allow_once", "allow_once"},
               {"apply_network_policy_amendment:1", "allow_always"},
               {"apply_network_policy_amendment:0", "reject_always"},
               {"cancel", "reject_once"}
             ]

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => ^deny_decision}}]} =
               CodexGolden.last_result(transcript)
    end
  end

  # -- item/fileChange/requestApproval ---------------------------------------

  describe "file change approvals" do
    test "file_change_approval_option_decisions" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, file_change_request(100)},
            {:outbound, select("allow_once")},
            {:inbound, file_change_request(101, %{"itemId" => "item-2"})},
            {:outbound, select("allow_for_session")},
            {:inbound, file_change_request(102, %{"itemId" => "item-3"})},
            {:outbound, select("cancel")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "file_change_approval_option_decisions", steps)

      assert [%{"params" => %{"options" => options, "_meta" => meta}} | _] =
               acp_requests(transcript)

      assert Enum.map(options, & &1["optionId"]) == ["allow_once", "allow_for_session", "cancel"]
      assert meta["permission"]["title"] == "Make edits?"

      assert [
               %{"id" => 100, "result" => %{"decision" => "accept"}},
               %{"id" => 101, "result" => %{"decision" => "acceptForSession"}},
               %{"id" => 102, "result" => %{"decision" => "cancel"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "file_change_approval_cancelled_unknown_and_error_replies_cancel" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, file_change_request(100)},
            {:outbound, cancelled()},
            {:note, "reject_once is not an offered file-change option"},
            {:inbound, file_change_request(101, %{"itemId" => "item-2"})},
            {:outbound, select("reject_once")},
            {:inbound, file_change_request(102, %{"itemId" => "item-3"})},
            {:outbound, reply_last(%{"error" => %{"code" => -32_603, "message" => "boom"}})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "file_change_approval_cancelled_unknown_and_error_replies_cancel",
          steps
        )

      assert Enum.map(100..102, &%{"id" => &1, "result" => %{"decision" => "cancel"}}) ==
               transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end
  end

  # -- item/permissions/requestApproval --------------------------------------

  describe "permission profile approvals" do
    test "permissions_approval_grant_turn_copies_requested_profile" do
      steps =
        active_turn_steps() ++
          [
            {:note, "Only the network and fileSystem keys of the requested profile are granted"},
            {:inbound, permissions_request(100)},
            {:outbound, select("allow_permissions_turn")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "permissions_approval_grant_turn_copies_requested_profile",
          steps
        )

      assert [%{"params" => %{"options" => options, "_meta" => meta}}] =
               acp_requests(transcript)

      assert meta["permission"]["title"] == "Grant permissions?"

      assert Enum.map(options, & &1["optionId"]) == [
               "allow_permissions_turn",
               "allow_permissions_turn_strict_auto_review",
               "allow_permissions_session",
               "reject_permissions"
             ]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{
                     "permissions" => %{
                       "network" => %{"enabled" => true},
                       "fileSystem" => %{"write" => ["/tmp/project"]}
                     },
                     "scope" => "turn",
                     "strictAutoReview" => false
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "permissions_approval_grant_turn_strict_auto_review" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, permissions_request(100)},
            {:outbound, select("allow_permissions_turn_strict_auto_review")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "permissions_approval_grant_turn_strict_auto_review",
          steps
        )

      assert %{
               writes: [
                 %{"id" => 100, "result" => %{"scope" => "turn", "strictAutoReview" => true}}
               ]
             } =
               CodexGolden.last_result(transcript)
    end

    test "permissions_approval_grant_session" do
      steps =
        active_turn_steps() ++
          [{:inbound, permissions_request(100)}, {:outbound, select("allow_permissions_session")}]

      transcript = CodexGolden.assert_golden(@area, "permissions_approval_grant_session", steps)

      assert %{
               writes: [
                 %{"id" => 100, "result" => %{"scope" => "session", "strictAutoReview" => false}}
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "permissions_approval_grant_without_requested_profile" do
      steps =
        active_turn_steps() ++
          [
            {:note, "A request without a permissions map grants an empty profile"},
            {:inbound,
             %{
               "id" => 100,
               "method" => "item/permissions/requestApproval",
               "params" => %{"threadId" => @thread, "turnId" => "turn-1", "itemId" => "item-1"}
             }},
            {:outbound, select("allow_permissions_session")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "permissions_approval_grant_without_requested_profile",
          steps
        )

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{
                     "permissions" => %{},
                     "scope" => "session",
                     "strictAutoReview" => false
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "permissions_approval_reject_cancelled_and_error_replies" do
      steps =
        active_turn_steps() ++
          [
            {:inbound, permissions_request(100)},
            {:outbound, select("reject_permissions")},
            {:inbound, permissions_request(101, %{"itemId" => "item-2"})},
            {:outbound, cancelled()},
            {:inbound, permissions_request(102, %{"itemId" => "item-3"})},
            {:outbound, reply_last(%{"error" => %{"code" => -32_603, "message" => "boom"}})},
            {:note, "An option id from another approval kind is rejected as well"},
            {:inbound, permissions_request(103, %{"itemId" => "item-4"})},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "permissions_approval_reject_cancelled_and_error_replies",
          steps
        )

      rejected = %{"permissions" => %{}, "scope" => "turn", "strictAutoReview" => false}

      assert Enum.map(100..103, &%{"id" => &1, "result" => rejected}) ==
               transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "permissions_approval_grant_with_non_map_profile" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "A permissions value that is not an object is still prompted for, but a grant copies nothing: the profile written back is empty"},
            {:inbound, permissions_request(100, %{"permissions" => ["network", "fileSystem"]})},
            {:outbound, select("allow_permissions_turn")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "permissions_approval_grant_with_non_map_profile", steps)

      assert [%{"method" => "session/request_permission", "params" => %{"toolCall" => tool_call}}] =
               acp_requests(transcript)

      assert tool_call["rawInput"]["permissions"] == ["network", "fileSystem"]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{
                     "permissions" => %{},
                     "scope" => "turn",
                     "strictAutoReview" => false
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end
  end

  # -- legacy execCommandApproval / applyPatchApproval -----------------------

  describe "legacy approval methods" do
    test "legacy_exec_command_approval_decisions" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "Legacy approvals use callId as the tool call id and a fixed three-option set"},
            {:inbound, legacy_request(100, "execCommandApproval", %{"callId" => "call-1"})},
            {:outbound, select("allow_once")},
            {:inbound, legacy_request(101, "execCommandApproval", %{"callId" => "call-2"})},
            {:outbound, select("allow_always")},
            {:inbound, legacy_request(102, "execCommandApproval", %{"callId" => "call-3"})},
            {:outbound, select("reject_once")},
            {:inbound, legacy_request(103, "execCommandApproval", %{"callId" => "call-4"})},
            {:outbound, cancelled()}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "legacy_exec_command_approval_decisions", steps)

      assert [
               %{
                 "params" => %{"toolCall" => %{"toolCallId" => "call-1", "toolName" => "execute"}}
               }
               | _
             ] =
               acp_requests(transcript)

      assert [
               %{"id" => 100, "result" => %{"decision" => "approved"}},
               %{"id" => 101, "result" => %{"decision" => "approved_for_session"}},
               %{"id" => 102, "result" => %{"decision" => "denied"}},
               %{"id" => 103, "result" => %{"decision" => "abort"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "legacy_exec_command_approval_string_command_title" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "A string command becomes the tool call title; argv lists fall back to Run Command"},
            {:inbound,
             legacy_request(100, "execCommandApproval", %{
               "callId" => "call-1",
               "command" => "cat /etc/hosts"
             })},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "legacy_exec_command_approval_string_command_title",
          steps
        )

      assert [%{"params" => %{"toolCall" => %{"title" => "cat /etc/hosts"}, "_meta" => meta}}] =
               acp_requests(transcript)

      assert meta["permission"] == %{
               "version" => 1,
               "title" => "Run command?",
               "description" => "Run tests"
             }

      assert %{writes: [%{"id" => 100, "result" => %{"decision" => "approved"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "legacy_apply_patch_approval_heuristic_decisions" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "Unknown option ids are mapped by substring: always/session, allow/accept/approved, cancel, else denied"},
            {:inbound,
             legacy_request(100, "applyPatchApproval", %{"approvalId" => "approval-1"})},
            {:outbound, select("approved-for-session")},
            {:inbound,
             legacy_request(101, "applyPatchApproval", %{"approvalId" => "approval-2"})},
            {:outbound, select("custom-cancel")},
            {:inbound,
             legacy_request(102, "applyPatchApproval", %{"approvalId" => "approval-3"})},
            {:outbound, select("nope")},
            {:inbound,
             legacy_request(103, "applyPatchApproval", %{"approvalId" => "approval-4"})},
            {:outbound, reply_last(%{"error" => %{"code" => -32_603, "message" => "boom"}})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "legacy_apply_patch_approval_heuristic_decisions", steps)

      assert [
               %{"params" => %{"toolCall" => %{"toolCallId" => "approval-1", "kind" => "edit"}}}
               | _
             ] =
               acp_requests(transcript)

      assert [
               %{"id" => 100, "result" => %{"decision" => "approved_for_session"}},
               %{"id" => 101, "result" => %{"decision" => "abort"}},
               %{"id" => 102, "result" => %{"decision" => "denied"}},
               %{"id" => 103, "result" => %{"decision" => "abort"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "legacy_exec_command_approval_item_id_wins_over_call_id" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "The tool call id precedence is itemId, then callId, then approvalId; each request carries two of them"},
            {:inbound,
             legacy_request(100, "execCommandApproval", %{
               "callId" => "call-1",
               "itemId" => "item-1"
             })},
            {:outbound, select("allow_once")},
            {:inbound,
             legacy_request(101, "applyPatchApproval", %{
               "callId" => "call-2",
               "approvalId" => "approval-2"
             })},
            {:outbound, select("allow_once")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "legacy_exec_command_approval_item_id_wins_over_call_id",
          steps
        )

      assert [
               %{"params" => %{"toolCall" => %{"toolCallId" => "item-1"}}},
               %{"params" => %{"toolCall" => %{"toolCallId" => "call-2"}}}
             ] = acp_requests(transcript)

      assert [
               %{"id" => 100, "result" => %{"decision" => "approved"}},
               %{"id" => 101, "result" => %{"decision" => "approved"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end
  end

  # -- mcpServer/elicitation/request on the permission fallback --------------

  describe "MCP elicitation permission fallback" do
    test "mcp_tool_approval_persist_options_round_trip" do
      meta = %{"codex_approval_kind" => "mcp_tool_call", "persist" => ["session", "always"]}

      steps =
        active_turn_steps() ++
          [
            {:note,
             "Client has no elicitation capability, so openai/form falls back to a permission request"},
            {:inbound, mcp_elicitation_request(100, %{"mode" => "openai/form", "_meta" => meta})},
            {:outbound, select("allow_session")},
            {:inbound, mcp_elicitation_request(101, %{"mode" => "openai/form", "_meta" => meta})},
            {:outbound, select("allow_always")},
            {:inbound, mcp_elicitation_request(102, %{"mode" => "openai/form", "_meta" => meta})},
            {:outbound, select("allow_once")},
            {:note, "decline is not offered for tool approvals and maps to cancel"},
            {:inbound, mcp_elicitation_request(103, %{"mode" => "openai/form", "_meta" => meta})},
            {:outbound, select("decline")},
            {:inbound, mcp_elicitation_request(104, %{"mode" => "openai/form", "_meta" => meta})},
            {:outbound, cancelled()}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "mcp_tool_approval_persist_options_round_trip", steps)

      assert [%{"method" => "session/request_permission", "params" => params} | _] =
               acp_requests(transcript)

      assert params["toolCall"]["toolName"] == "mcp:tool-server"

      assert Enum.map(params["options"], & &1["optionId"]) ==
               ["allow_once", "allow_session", "allow_always", "cancel"]

      assert [
               %{
                 "id" => 100,
                 "result" => %{"action" => "accept", "_meta" => %{"persist" => "session"}}
               },
               %{
                 "id" => 101,
                 "result" => %{"action" => "accept", "_meta" => %{"persist" => "always"}}
               },
               %{"id" => 102, "result" => %{"action" => "accept"}},
               %{"id" => 103, "result" => %{"action" => "cancel"}},
               %{"id" => 104, "result" => %{"action" => "cancel"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "mcp_elicitation_fallback_without_persist" do
      steps =
        active_turn_steps() ++
          [
            {:note, "Generic (non tool-approval) request without _meta: accept/decline/cancel"},
            {:inbound,
             mcp_elicitation_request(100, %{"mode" => "form", "message" => "Approve?"})},
            {:outbound, select("accept")},
            {:inbound,
             mcp_elicitation_request(101, %{"mode" => "form", "message" => "Approve?"})},
            {:outbound, select("decline")},
            {:inbound,
             mcp_elicitation_request(102, %{"mode" => "form", "message" => "Approve?"})},
            {:outbound, select("cancel")},
            {:note, "allow_once belongs to the tool-approval shape and maps to cancel here"},
            {:inbound,
             mcp_elicitation_request(103, %{"mode" => "form", "message" => "Approve?"})},
            {:outbound, select("allow_once")},
            {:note, "A persist scope that was not offered maps to cancel"},
            {:inbound,
             mcp_elicitation_request(104, %{"mode" => "form", "message" => "Approve?"})},
            {:outbound, select("allow_session")},
            {:inbound,
             mcp_elicitation_request(105, %{"mode" => "form", "message" => "Approve?"})},
            {:outbound, reply_last(%{"error" => %{"code" => -32_603, "message" => "boom"}})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "mcp_elicitation_fallback_without_persist", steps)

      assert [%{"params" => %{"options" => options}} | _] = acp_requests(transcript)
      assert Enum.map(options, & &1["optionId"]) == ["accept", "decline", "cancel"]

      assert [
               %{"id" => 100, "result" => %{"action" => "accept"}},
               %{"id" => 101, "result" => %{"action" => "decline"}},
               %{"id" => 102, "result" => %{"action" => "cancel"}},
               %{"id" => 103, "result" => %{"action" => "cancel"}},
               %{"id" => 104, "result" => %{"action" => "cancel"}},
               %{"id" => 105, "result" => %{"action" => "cancel"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "mcp_elicitation_persist_string_scope" do
      steps =
        active_turn_steps() ++
          [
            {:note, "_meta.persist as a bare string offers only that scope"},
            {:inbound,
             mcp_elicitation_request(100, %{
               "mode" => "openai/form",
               "_meta" => %{"persist" => "session"}
             })},
            {:outbound, select("allow_always")},
            {:inbound,
             mcp_elicitation_request(101, %{
               "mode" => "openai/form",
               "_meta" => %{"persist" => "always"}
             })},
            {:outbound, select("allow_always")}
          ]

      transcript = CodexGolden.assert_golden(@area, "mcp_elicitation_persist_string_scope", steps)

      assert [%{"params" => %{"options" => first}}, %{"params" => %{"options" => second}}] =
               acp_requests(transcript)

      assert Enum.map(first, & &1["optionId"]) == ["accept", "allow_session", "decline", "cancel"]
      assert Enum.map(second, & &1["optionId"]) == ["accept", "allow_always", "decline", "cancel"]

      assert [
               %{"id" => 100, "result" => %{"action" => "cancel"}},
               %{
                 "id" => 101,
                 "result" => %{"action" => "accept", "_meta" => %{"persist" => "always"}}
               }
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "mcp_elicitation_persist_list_filters_unknown_scopes" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "Unknown entries in a _meta.persist list are dropped; only session is offered"},
            {:inbound,
             mcp_elicitation_request(100, %{
               "mode" => "form",
               "message" => "Approve?",
               "_meta" => %{"persist" => ["session", "bogus"]}
             })},
            {:outbound, select("allow_session")},
            {:inbound,
             mcp_elicitation_request(101, %{
               "mode" => "form",
               "message" => "Approve?",
               "_meta" => %{"persist" => ["session", "bogus"]}
             })},
            {:outbound, select("allow_always")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_elicitation_persist_list_filters_unknown_scopes",
          steps
        )

      assert [%{"params" => %{"options" => options}} | _] = acp_requests(transcript)

      assert Enum.map(options, & &1["optionId"]) == [
               "accept",
               "allow_session",
               "decline",
               "cancel"
             ]

      assert [
               %{
                 "id" => 100,
                 "result" => %{"action" => "accept", "_meta" => %{"persist" => "session"}}
               },
               %{"id" => 101, "result" => %{"action" => "cancel"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "mcp_openai_form_stays_on_permission_fallback" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "The client supports form elicitation, but openai/form is not a supported mode"},
            {:inbound,
             mcp_elicitation_request(100, %{
               "mode" => "openai/form",
               "message" => "Unsupported arbitrary form",
               "requestedSchema" => %{}
             })},
            {:outbound, select("accept")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "mcp_openai_form_stays_on_permission_fallback", steps)

      assert [%{"method" => "session/request_permission"}] = acp_requests(transcript)

      assert %{writes: [%{"id" => 100, "result" => %{"action" => "accept"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "mcp_url_elicitation_without_url_capability_falls_back" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:inbound,
             mcp_elicitation_request(100, %{
               "mode" => "url",
               "elicitationId" => "oauth-1",
               "url" => "https://example.com/authorize",
               "message" => "Authorize the MCP server"
             })},
            {:outbound, select("accept")},
            {:note, "No url elicitation was tracked, so the server resolution completes nothing"},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_url_elicitation_without_url_capability_falls_back",
          steps
        )

      assert [%{"method" => "session/request_permission"}] = acp_requests(transcript)
      assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
    end

    test "mcp_elicitation_fallback_without_message_default_title" do
      steps =
        active_turn_steps() ++
          [
            {:note,
             "No client elicitation capability and no message: the permission request's tool call falls back to the MCP Elicitation title"},
            {:inbound,
             without_param(
               mcp_elicitation_request(100, %{
                 "mode" => "form",
                 "requestedSchema" => %{"properties" => %{"name" => %{"type" => "string"}}}
               }),
               "message"
             )},
            {:outbound, select("accept")}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_elicitation_fallback_without_message_default_title",
          steps
        )

      assert [%{"method" => "session/request_permission", "params" => %{"toolCall" => tool_call}}] =
               acp_requests(transcript)

      assert tool_call["title"] == "MCP Elicitation"
      assert tool_call["toolName"] == "mcp:tool-server"

      assert %{writes: [%{"id" => 100, "result" => %{"action" => "accept"}}]} =
               CodexGolden.last_result(transcript)
    end
  end

  # -- mcpServer/elicitation/request in form mode ----------------------------

  describe "MCP form elicitation" do
    test "mcp_form_elicitation_normalizes_enum_names" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "enum + enumNames become oneOf recursively; type is forced to object; _meta passes through"},
            {:inbound,
             mcp_elicitation_request(100, %{
               "mode" => "form",
               "message" => "Pick a color",
               "itemId" => "mcp-call-1",
               "_meta" => %{"progressToken" => "tok-1"},
               "requestedSchema" => %{
                 "type" => "object",
                 "properties" => %{
                   "color" => %{
                     "type" => "string",
                     "enum" => ["red", "blue"],
                     "enumNames" => ["Red", "Blue"]
                   },
                   "nested" => %{
                     "type" => "object",
                     "properties" => %{
                       "shade" => %{
                         "type" => "string",
                         "enum" => ["light", "dark"],
                         "enumNames" => ["Light"]
                       }
                     }
                   },
                   "already" => %{
                     "type" => "string",
                     "enum" => ["a"],
                     "enumNames" => ["A"],
                     "oneOf" => [%{"const" => "a", "title" => "Kept"}]
                   }
                 },
                 "required" => ["color"]
               }
             })},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"color" => "red"}})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "mcp_form_elicitation_normalizes_enum_names", steps)

      assert [%{"method" => "elicitation/create", "params" => params}] =
               acp_requests(transcript)

      assert params["mode"] == "form"
      assert params["toolCallId"] == "mcp-call-1"

      assert params["requestedSchema"]["properties"]["color"] == %{
               "type" => "string",
               "oneOf" => [
                 %{"const" => "red", "title" => "Red"},
                 %{"const" => "blue", "title" => "Blue"}
               ]
             }

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{"action" => "accept", "content" => %{"color" => "red"}}
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "mcp_form_elicitation_decline_and_cancel_pass_through" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note, "A request without message or schema uses the defaults"},
            {:inbound, mcp_elicitation_request(100, %{"mode" => "form", "message" => nil})},
            {:outbound, elicitation_reply(%{"action" => "decline"})},
            {:inbound,
             mcp_elicitation_request(101, %{
               "mode" => "form",
               "message" => "Name?",
               "requestedSchema" => %{"properties" => %{"name" => %{"type" => "string"}}}
             })},
            {:outbound,
             elicitation_reply(%{"action" => "cancel", "_meta" => %{"reason" => "user closed"}})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_form_elicitation_decline_and_cancel_pass_through",
          steps
        )

      assert [%{"params" => %{"message" => "Input requested", "requestedSchema" => schema}} | _] =
               acp_requests(transcript)

      assert schema == %{"type" => "object", "properties" => %{}}

      assert [
               %{"id" => 100, "result" => %{"action" => "decline"}},
               %{
                 "id" => 101,
                 "result" => %{"action" => "cancel", "_meta" => %{"reason" => "user closed"}}
               }
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "mcp_form_elicitation_tool_call_id_and_session_id_fallback" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note, "toolCallId wins over itemId"},
            {:inbound,
             mcp_elicitation_request(100, %{
               "mode" => "form",
               "message" => "Name?",
               "toolCallId" => "call-9",
               "itemId" => "item-9"
             })},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"name" => "Ada"}})},
            {:note, "Without a threadId the sessionId param routes the request"},
            {:inbound,
             %{
               "id" => 101,
               "method" => "mcpServer/elicitation/request",
               "params" => %{
                 "sessionId" => @thread,
                 "serverName" => "tool-server",
                 "mode" => "form",
                 "message" => "Name?"
               }
             }},
            {:outbound, elicitation_reply(%{"action" => "accept", "content" => %{}})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_form_elicitation_tool_call_id_and_session_id_fallback",
          steps
        )

      assert [%{"params" => first}, %{"params" => second}] = acp_requests(transcript)
      assert first["toolCallId"] == "call-9"
      assert {second["sessionId"], Map.has_key?(second, "toolCallId")} == {@thread, false}
    end

    test "mcp_elicitation_thread_id_wins_over_session_id" do
      steps =
        active_turn_steps(@form_and_url_caps) ++
          [
            {:note, "A second session is open so both ids in the request name a real session"},
            {:outbound,
             %{
               "method" => "session/new",
               "id" => 11,
               "params" => %{"cwd" => "/tmp/project", "mcpServers" => []}
             }},
            {:inbound, thread_start_result(5, "thread-2")},
            {:note,
             "When a request carries both threadId and sessionId, the form and url elicitations are routed by threadId"},
            {:inbound,
             mcp_elicitation_request(100, %{
               "sessionId" => "thread-2",
               "mode" => "form",
               "message" => "Name?"
             })},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"name" => "Ada"}})},
            {:inbound,
             mcp_elicitation_request(101, %{
               "sessionId" => "thread-2",
               "mode" => "url",
               "elicitationId" => "oauth-1",
               "url" => "https://example.com/authorize/oauth-1",
               "message" => "Authorize the MCP server"
             })},
            {:outbound, elicitation_reply(%{"action" => "accept"})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "mcp_elicitation_thread_id_wins_over_session_id", steps)

      assert [
               %{"method" => "elicitation/create", "params" => %{"mode" => "form"} = form},
               %{"method" => "elicitation/create", "params" => %{"mode" => "url"} = url}
             ] = acp_requests(transcript)

      assert {form["sessionId"], url["sessionId"]} == {@thread, @thread}

      assert [
               %{
                 "id" => 100,
                 "result" => %{"action" => "accept", "content" => %{"name" => "Ada"}}
               },
               %{"id" => 101, "result" => %{"action" => "accept"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "mcp_form_elicitation_malformed_replies_cancel" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note, "accept with non-map content is turned into cancel"},
            {:inbound, mcp_elicitation_request(100, %{"mode" => "form", "message" => "Name?"})},
            {:outbound, elicitation_reply(%{"action" => "accept", "content" => "not a map"})},
            {:note, "accept without content passes through"},
            {:inbound, mcp_elicitation_request(101, %{"mode" => "form", "message" => "Name?"})},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:note, "A result without an action, and an error response, are both cancel"},
            {:inbound, mcp_elicitation_request(102, %{"mode" => "form", "message" => "Name?"})},
            {:outbound, reply_last(%{"result" => %{"content" => %{}}})},
            {:inbound, mcp_elicitation_request(103, %{"mode" => "form", "message" => "Name?"})},
            {:outbound, reply_last(%{"error" => %{"code" => -32_603, "message" => "boom"}})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "mcp_form_elicitation_malformed_replies_cancel", steps)

      assert [
               %{"id" => 100, "result" => %{"action" => "cancel"}},
               %{"id" => 101, "result" => %{"action" => "accept"}},
               %{"id" => 102, "result" => %{"action" => "cancel"}},
               %{"id" => 103, "result" => %{"action" => "cancel"}}
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end
  end

  # -- mcpServer/elicitation/request in url mode -----------------------------

  describe "MCP url elicitation" do
    test "mcp_url_elicitation_completes_on_server_resolved" do
      steps =
        active_turn_steps(@url_caps) ++
          [
            {:inbound, url_elicitation_request(100, "oauth-1")},
            {:outbound, elicitation_reply(%{"action" => "accept", "content" => %{}})},
            {:note, "The app-server resolves the request once the OAuth flow finishes"},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }},
            {:note, "Resolving an unknown request id is a no-op"},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 999}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_url_elicitation_completes_on_server_resolved",
          steps
        )

      assert [
               %{
                 "method" => "elicitation/create",
                 "params" => %{"mode" => "url", "elicitationId" => "oauth-1"}
               },
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "oauth-1"}}
             ] = acp_requests(transcript)

      assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
    end

    test "mcp_url_elicitations_concurrent_complete_only_matching" do
      steps =
        active_turn_steps(@url_caps) ++
          [
            {:inbound, url_elicitation_request(100, "oauth-1")},
            {:inbound, url_elicitation_request(101, "oauth-2")},
            {:note, "Both client UIs are accepted; each reply targets its own generated id"},
            {:outbound, elicitation_reply(%{"action" => "accept"}, 0)},
            {:outbound, elicitation_reply(%{"action" => "accept"}, 1)},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 101}
             }},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_url_elicitations_concurrent_complete_only_matching",
          steps
        )

      assert [
               %{"method" => "elicitation/create", "id" => "codex-elicitation-<1>"},
               %{"method" => "elicitation/create", "id" => "codex-elicitation-<2>"},
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "oauth-2"}},
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "oauth-1"}}
             ] = acp_requests(transcript)

      assert [
               %{"id" => 100, "result" => %{"action" => "accept"}},
               %{"id" => 101, "result" => %{"action" => "accept"}}
             ] =
               transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "mcp_url_elicitation_declined_is_not_completed" do
      steps =
        active_turn_steps(@url_caps) ++
          [
            {:inbound, url_elicitation_request(100, "oauth-1")},
            {:outbound, elicitation_reply(%{"action" => "decline"})},
            {:note, "Only accepted url elicitations are completed on resolution"},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "mcp_url_elicitation_declined_is_not_completed", steps)

      assert [%{"method" => "elicitation/create"}] = acp_requests(transcript)

      assert [%{"id" => 100, "result" => %{"action" => "decline"}}] =
               transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))

      assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
    end

    test "mcp_url_elicitation_without_elicitation_id_is_not_completed" do
      steps =
        active_turn_steps(@url_caps) ++
          [
            {:note, "A url request without an elicitationId is forwarded but never tracked"},
            {:inbound,
             mcp_elicitation_request(100, %{
               "mode" => "url",
               "url" => "https://example.com/authorize",
               "message" => "Authorize the MCP server"
             })},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_url_elicitation_without_elicitation_id_is_not_completed",
          steps
        )

      assert [%{"method" => "elicitation/create", "params" => params}] = acp_requests(transcript)
      assert params["mode"] == "url" and is_nil(params["elicitationId"])

      assert [%{"id" => 100, "result" => %{"action" => "accept"}}] =
               transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))

      assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
    end

    test "mcp_url_elicitation_without_message_default_prompt" do
      steps =
        active_turn_steps(@url_caps) ++
          [
            {:note, "A url request without a message gets the adapter's default prompt"},
            {:inbound,
             without_param(
               mcp_elicitation_request(100, %{
                 "mode" => "url",
                 "elicitationId" => "oauth-1",
                 "url" => "https://example.com/authorize/oauth-1"
               }),
               "message"
             )},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_url_elicitation_without_message_default_prompt",
          steps
        )

      assert [
               %{"method" => "elicitation/create", "params" => params},
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "oauth-1"}}
             ] = acp_requests(transcript)

      assert params["message"] == "Open the requested URL to continue"
      assert params["url"] == "https://example.com/authorize/oauth-1"
    end

    test "mcp_url_elicitation_resolved_is_not_recompleted_on_close" do
      steps =
        active_turn_steps(@url_caps) ++
          [
            {:inbound, url_elicitation_request(100, "oauth-1")},
            {:outbound, elicitation_reply(%{"action" => "accept", "content" => %{}})},
            {:inbound, server_resolved(100)},
            {:note,
             "Resolution forgets the url elicitation: a duplicate serverRequest/resolved for the same request id completes nothing"},
            {:inbound, server_resolved(100)},
            {:note,
             "Closing the session completes only url elicitations still open, so the resolved one is not completed a second time"},
            {:outbound,
             %{"method" => "session/close", "id" => 30, "params" => %{"sessionId" => @thread}}}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "mcp_url_elicitation_resolved_is_not_recompleted_on_close",
          steps
        )

      assert [
               %{"method" => "elicitation/create", "params" => %{"elicitationId" => "oauth-1"}},
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "oauth-1"}}
             ] = acp_requests(transcript)

      assert %{tag: :skip, skipped: true} =
               inbound_step_result(transcript, server_resolved(100), 1)

      assert %{tag: :messages_and_write, messages: close_messages} =
               outbound_result(transcript, 30)

      assert [%{"id" => 20, "result" => %{"stopReason" => "cancelled"}}] = close_messages
    end
  end

  # -- serverRequest/resolved for non-permission client requests -------------

  describe "server-resolved client requests" do
    test "server_resolved_user_input_and_form_elicitation_drop_late_replies" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:inbound,
             user_input_request(100, %{
               "questions" => [%{"id" => "name", "question" => "Your name?"}]
             })},
            {:inbound, mcp_elicitation_request(101, %{"mode" => "form", "message" => "Name?"})},
            {:note, "The app-server resolves both requests before the client answers"},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 100}
             }},
            {:inbound,
             %{
               "method" => "serverRequest/resolved",
               "params" => %{"threadId" => @thread, "requestId" => 101}
             }},
            {:note, "Late client replies find no pending entry and write nothing"},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"name" => "Ada"}}, 0)},
            {:outbound, elicitation_reply(%{"action" => "accept", "content" => %{}}, 1)}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "server_resolved_user_input_and_form_elicitation_drop_late_replies",
          steps
        )

      assert [
               %{"method" => "elicitation/create", "id" => "codex-user-input-<1>"},
               %{"method" => "elicitation/create", "id" => "codex-elicitation-<1>"}
             ] = acp_requests(transcript)

      assert [%{tag: :ok, skipped: true}, %{tag: :ok, skipped: true}] =
               transcript |> Enum.take(-2) |> Enum.map(& &1.result)

      refute Enum.any?(CodexGolden.writes(transcript), &(&1["id"] in [100, 101]))
    end
  end

  # -- item/tool/requestUserInput --------------------------------------------

  describe "tool user input" do
    test "user_input_single_question_round_trip" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note, "A single question without a message uses the question text as the prompt"},
            {:inbound,
             user_input_request(100, %{
               "questions" => [
                 %{
                   "id" => "color",
                   "header" => "Color",
                   "question" => "Which color?",
                   "options" => [
                     %{"label" => "Blue", "description" => "Cool"},
                     %{"label" => "Red"}
                   ]
                 }
               ]
             })},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"color" => "Blue"}})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_single_question_round_trip", steps)

      assert [%{"method" => "elicitation/create", "params" => params}] =
               acp_requests(transcript)

      assert params["message"] == "Which color?"
      assert params["requestedSchema"]["required"] == ["color"]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{"answers" => %{"color" => %{"answers" => ["Blue"]}}}
                 }
               ]
             } =
               CodexGolden.last_result(transcript)
    end

    test "user_input_multiple_questions_and_list_answers" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "Questions without a usable id are dropped; an isOther question is optional and gets an __other field"},
            {:inbound,
             user_input_request(100, %{
               "message" => "A few details, please",
               "autoResolutionMs" => 30_000,
               "isBlocking" => true,
               "questions" => [
                 %{"id" => "name", "header" => "Name", "question" => "Your name?"},
                 %{
                   "id" => "langs",
                   "header" => "Languages",
                   "question" => "Which languages?",
                   "isOther" => true,
                   "options" => [%{"label" => "Elixir"}, %{"label" => "Erlang"}]
                 },
                 %{"id" => "", "question" => "Ignored (empty id)"},
                 %{"question" => "Ignored (no id)"},
                 %{"id" => "notes", "question" => "Anything else?"}
               ]
             })},
            {:note,
             "Lists keep only their string elements; a whitespace-only string answer is kept; __other text wins over the selected option"},
            {:outbound,
             elicitation_reply(%{
               "action" => "accept",
               "content" => %{
                 "name" => ["Ada", 42, "Lovelace"],
                 "langs" => "Elixir",
                 "langs__other" => "Gleam",
                 "notes" => "   "
               }
             })}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_multiple_questions_and_list_answers", steps)

      assert [%{"params" => params}] = acp_requests(transcript)
      assert params["message"] == "A few details, please"
      assert params["requestedSchema"]["required"] == ["name", "notes"]

      assert Map.keys(params["requestedSchema"]["properties"]) == [
               "langs",
               "langs__other",
               "name",
               "notes"
             ]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{
                     "answers" => %{
                       "name" => %{"answers" => ["Ada", "Lovelace"]},
                       "langs" => %{"answers" => ["Gleam"]},
                       "notes" => %{"answers" => ["   "]}
                     }
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "user_input_empty_string_answer_is_omitted" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:inbound,
             user_input_request(100, %{
               "questions" => [
                 %{"id" => "name", "header" => "Name", "question" => "Your name?"},
                 %{"id" => "notes", "header" => "Notes", "question" => "Anything else?"}
               ]
             })},
            {:note,
             "A text field the user left untouched comes back as an empty string; that question is omitted from the native answers instead of being written as [\"\"]"},
            {:outbound,
             elicitation_reply(%{
               "action" => "accept",
               "content" => %{"name" => "", "notes" => "Ship it"}
             })}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_empty_string_answer_is_omitted", steps)

      assert [%{"params" => %{"requestedSchema" => %{"required" => ["name", "notes"]}}}] =
               acp_requests(transcript)

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{"answers" => %{"notes" => %{"answers" => ["Ship it"]}}}
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "user_input_single_question_message_wins_over_question_text" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "With a single question the request message still wins over the question text as the prompt; the question text stays in the property description"},
            {:inbound,
             user_input_request(100, %{
               "message" => "Codex needs one detail before continuing",
               "questions" => [
                 %{"id" => "color", "header" => "Color", "question" => "Which color?"}
               ]
             })},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"color" => "Blue"}})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "user_input_single_question_message_wins_over_question_text",
          steps
        )

      assert [%{"method" => "elicitation/create", "params" => params}] =
               acp_requests(transcript)

      assert params["message"] == "Codex needs one detail before continuing"
      assert params["requestedSchema"]["properties"]["color"]["description"] == "Which color?"

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{"answers" => %{"color" => %{"answers" => ["Blue"]}}}
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "user_input_boolean_form_capability_refused" do
      steps =
        active_turn_steps(@boolean_form_caps) ++
          [
            {:note,
             "elicitation.form = true is not a capability object: user input is refused natively"},
            {:inbound,
             user_input_request(100, %{
               "questions" => [%{"id" => "name", "question" => "Your name?"}]
             })},
            {:note, "and an MCP form elicitation takes the permission fallback"},
            {:inbound, mcp_elicitation_request(101, %{"mode" => "form", "message" => "Name?"})},
            {:outbound, select("accept")}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_boolean_form_capability_refused", steps)

      assert %{tag: :skip_and_write, writes: [%{"id" => 100, "result" => %{"answers" => %{}}}]} =
               inbound_result(transcript, 100)

      assert [%{"method" => "session/request_permission"}] = acp_requests(transcript)

      assert %{writes: [%{"id" => 101, "result" => %{"action" => "accept"}}]} =
               CodexGolden.last_result(transcript)
    end

    test "user_input_other_field_id_collision" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "A real question already named color__other pushes the synthesized field to color__other1"},
            {:inbound,
             user_input_request(100, %{
               "questions" => [
                 %{
                   "id" => "color",
                   "question" => "Which color?",
                   "isOther" => true,
                   "options" => [%{"label" => "Blue"}]
                 },
                 %{"id" => "color__other", "question" => "Real second question"}
               ]
             })},
            {:outbound,
             elicitation_reply(%{
               "action" => "accept",
               "content" => %{"color__other1" => "Green", "color__other" => "second"}
             })}
          ]

      transcript = CodexGolden.assert_golden(@area, "user_input_other_field_id_collision", steps)

      assert [%{"params" => %{"requestedSchema" => %{"properties" => properties}}}] =
               acp_requests(transcript)

      assert properties["color__other"]["description"] == "Real second question"
      assert properties["color__other1"]["_meta"]["codex"]["isOtherAnswer"] == true

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{
                     "answers" => %{
                       "color" => %{"answers" => ["Green"]},
                       "color__other" => %{"answers" => ["second"]}
                     }
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "user_input_blank_other_falls_back_to_option" do
      questions = [
        %{
          "id" => "langs",
          "header" => "Languages",
          "question" => "Which languages?",
          "isOther" => true,
          "options" => [%{"label" => "Elixir"}, %{"label" => "Erlang"}]
        }
      ]

      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note, "Whitespace-only __other text is ignored in favour of the selected option"},
            {:inbound, user_input_request(100, %{"questions" => questions})},
            {:outbound,
             elicitation_reply(%{
               "action" => "accept",
               "content" => %{"langs" => "Elixir", "langs__other" => "   "}
             })},
            {:note, "So is an empty __other string"},
            {:inbound,
             user_input_request(101, %{"itemId" => "tool-2", "questions" => questions})},
            {:outbound,
             elicitation_reply(%{
               "action" => "accept",
               "content" => %{"langs" => "Elixir", "langs__other" => ""}
             })}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_blank_other_falls_back_to_option", steps)

      assert [
               %{
                 "id" => 100,
                 "result" => %{"answers" => %{"langs" => %{"answers" => ["Elixir"]}}}
               },
               %{
                 "id" => 101,
                 "result" => %{"answers" => %{"langs" => %{"answers" => ["Elixir"]}}}
               }
             ] = transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end

    test "user_input_all_other_questions_omit_required" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "Every question is isOther with options, so nothing is required and the key is omitted"},
            {:inbound,
             user_input_request(100, %{
               "questions" => [
                 %{
                   "id" => "langs",
                   "question" => "Which languages?",
                   "isOther" => true,
                   "options" => [%{"label" => "Elixir"}]
                 },
                 %{
                   "id" => "editor",
                   "question" => "Which editor?",
                   "isOther" => true,
                   "options" => [%{"label" => "Vim"}, %{"label" => "Emacs"}]
                 }
               ]
             })},
            {:note, "A non-string, non-list answer value is dropped"},
            {:outbound,
             elicitation_reply(%{
               "action" => "accept",
               "content" => %{"langs__other" => "Gleam", "editor" => 42}
             })}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_all_other_questions_omit_required", steps)

      assert [%{"params" => %{"requestedSchema" => schema}}] = acp_requests(transcript)
      refute Map.has_key?(schema, "required")

      assert Map.keys(schema["properties"]) ==
               ["editor", "editor__other", "langs", "langs__other"]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{"answers" => %{"langs" => %{"answers" => ["Gleam"]}}}
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "user_input_other_without_options_stays_required" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "isOther without options gets no __other field and remains a required free-text question"},
            {:inbound,
             user_input_request(100, %{
               "questions" => [
                 %{"id" => "notes", "question" => "Anything else?", "isOther" => true}
               ]
             })},
            {:note, "A stray notes__other value is not an other-field and is ignored"},
            {:outbound,
             elicitation_reply(%{
               "action" => "accept",
               "content" => %{"notes" => "Ship it", "notes__other" => "ignored"}
             })}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_other_without_options_stays_required", steps)

      assert [%{"params" => %{"requestedSchema" => schema}}] = acp_requests(transcript)
      assert schema["required"] == ["notes"]
      assert Map.keys(schema["properties"]) == ["notes"]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{"answers" => %{"notes" => %{"answers" => ["Ship it"]}}}
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "user_input_other_with_empty_options_stays_required" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "An explicit empty options list is treated like no options: no __other field, and the question stays required"},
            {:inbound,
             user_input_request(100, %{
               "questions" => [
                 %{
                   "id" => "notes",
                   "question" => "Anything else?",
                   "isOther" => true,
                   "options" => []
                 }
               ]
             })},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"notes" => "Ship it"}})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "user_input_other_with_empty_options_stays_required",
          steps
        )

      assert [%{"params" => %{"requestedSchema" => schema}}] = acp_requests(transcript)
      assert schema["required"] == ["notes"]
      assert Map.keys(schema["properties"]) == ["notes"]

      assert %{
               writes: [
                 %{
                   "id" => 100,
                   "result" => %{"answers" => %{"notes" => %{"answers" => ["Ship it"]}}}
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "user_input_without_questions_round_trip" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "A request with no questions and no message still becomes a form elicitation with an empty object schema and the default prompt"},
            {:inbound, user_input_request(100, %{})},
            {:outbound, elicitation_reply(%{"action" => "accept", "content" => %{}})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_without_questions_round_trip", steps)

      assert [%{"method" => "elicitation/create", "params" => params}] = acp_requests(transcript)
      assert params["message"] == "Input requested"
      assert params["requestedSchema"] == %{"type" => "object", "properties" => %{}}

      assert %{writes: [%{"id" => 100, "result" => %{"answers" => %{}}}]} =
               CodexGolden.last_result(transcript)
    end

    test "user_input_secret_question_refused" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "Secret questions are never forwarded; the app-server gets empty answers at once"},
            {:inbound,
             user_input_request(100, %{
               "questions" => [
                 %{"id" => "name", "question" => "Your name?"},
                 %{
                   "id" => "token",
                   "header" => "Token",
                   "question" => "API token",
                   "isSecret" => true
                 }
               ]
             })}
          ]

      transcript = CodexGolden.assert_golden(@area, "user_input_secret_question_refused", steps)

      assert acp_requests(transcript) == []

      assert %{tag: :skip_and_write, writes: [%{"id" => 100, "result" => %{"answers" => %{}}}]} =
               CodexGolden.last_result(transcript)
    end

    test "user_input_without_form_capability_refused" do
      steps =
        active_turn_steps(@url_caps) ++
          [
            {:inbound,
             user_input_request(100, %{
               "questions" => [%{"id" => "name", "question" => "Your name?"}]
             })}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_without_form_capability_refused", steps)

      assert acp_requests(transcript) == []

      assert %{tag: :skip_and_write, writes: [%{"id" => 100, "result" => %{"answers" => %{}}}]} =
               CodexGolden.last_result(transcript)
    end

    test "user_input_non_accept_replies_answer_empty" do
      questions = [%{"id" => "name", "question" => "Your name?"}]

      steps =
        active_turn_steps(@form_caps) ++
          [
            {:inbound, user_input_request(100, %{"questions" => questions})},
            {:outbound, elicitation_reply(%{"action" => "decline"})},
            {:inbound,
             user_input_request(101, %{"itemId" => "tool-2", "questions" => questions})},
            {:outbound, elicitation_reply(%{"action" => "cancel"})},
            {:note, "accept without map content also yields no answers"},
            {:inbound,
             user_input_request(102, %{"itemId" => "tool-3", "questions" => questions})},
            {:outbound, elicitation_reply(%{"action" => "accept", "content" => "Ada"})},
            {:inbound,
             user_input_request(103, %{"itemId" => "tool-4", "questions" => questions})},
            {:outbound, reply_last(%{"error" => %{"code" => -32_603, "message" => "boom"}})}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "user_input_non_accept_replies_answer_empty", steps)

      assert Enum.map(100..103, &%{"id" => &1, "result" => %{"answers" => %{}}}) ==
               transcript |> CodexGolden.writes() |> Enum.filter(&Map.has_key?(&1, "result"))
    end
  end

  # -- unsupported app-server requests ----------------------------------------

  # -- current-session routing ------------------------------------------------

  describe "approval request session routing" do
    test "permission_request_without_thread_id_routes_to_current_session" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note,
             "Approval and user-input requests carrying neither threadId nor sessionId are routed to the only open session"},
            {:inbound, without_thread_id(command_request(100))},
            {:outbound, select("allow_once")},
            {:inbound, without_thread_id(file_change_request(101, %{"itemId" => "item-2"}))},
            {:outbound, select("allow_for_session")},
            {:inbound,
             without_thread_id(
               user_input_request(102, %{
                 "questions" => [%{"id" => "name", "question" => "Your name?"}]
               })
             )},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"name" => "Ada"}})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "permission_request_without_thread_id_routes_to_current_session",
          steps
        )

      assert [
               %{"method" => "session/request_permission", "params" => %{"sessionId" => @thread}},
               %{"method" => "session/request_permission", "params" => %{"sessionId" => @thread}},
               %{"method" => "elicitation/create", "params" => %{"sessionId" => @thread}}
             ] = acp_requests(transcript)

      assert [
               %{"id" => 100, "result" => %{"decision" => "accept"}},
               %{"id" => 101, "result" => %{"decision" => "acceptForSession"}},
               %{"id" => 102, "result" => %{"answers" => %{"name" => %{"answers" => ["Ada"]}}}}
             ] = Enum.take(CodexGolden.writes(transcript), -3)
    end

    test "permission_request_thread_id_wins_over_session_id" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:note, "A second session is open so both ids in the request name a real session"},
            {:outbound,
             %{
               "method" => "session/new",
               "id" => 11,
               "params" => %{"cwd" => "/tmp/project", "mcpServers" => []}
             }},
            {:inbound, thread_start_result(5, "thread-2")},
            {:note,
             "Approval and user-input requests carrying both threadId and sessionId are routed by threadId"},
            {:inbound, command_request(100, %{"sessionId" => "thread-2"})},
            {:outbound, select("allow_once")},
            {:inbound,
             file_change_request(101, %{"itemId" => "item-2", "sessionId" => "thread-2"})},
            {:outbound, select("allow_for_session")},
            {:inbound,
             user_input_request(102, %{
               "sessionId" => "thread-2",
               "questions" => [%{"id" => "name", "question" => "Your name?"}]
             })},
            {:outbound,
             elicitation_reply(%{"action" => "accept", "content" => %{"name" => "Ada"}})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "permission_request_thread_id_wins_over_session_id",
          steps
        )

      assert [
               %{"method" => "session/request_permission", "params" => %{"sessionId" => @thread}},
               %{"method" => "session/request_permission", "params" => %{"sessionId" => @thread}},
               %{"method" => "elicitation/create", "params" => %{"sessionId" => @thread}}
             ] = acp_requests(transcript)

      assert [
               %{"id" => 100, "result" => %{"decision" => "accept"}},
               %{"id" => 101, "result" => %{"decision" => "acceptForSession"}},
               %{"id" => 102, "result" => %{"answers" => %{"name" => %{"answers" => ["Ada"]}}}}
             ] = Enum.take(CodexGolden.writes(transcript), -3)
    end
  end

  describe "unsupported server requests" do
    test "unsupported_server_request_replies_native_error" do
      steps =
        active_turn_steps(@form_caps) ++
          [
            {:inbound,
             %{
               "id" => 100,
               "method" => "item/tool/call",
               "params" => %{
                 "threadId" => @thread,
                 "turnId" => "turn-1",
                 "itemId" => "item-1",
                 "tool" => "grep"
               }
             }},
            {:inbound,
             %{
               "id" => "req-2",
               "method" => "account/chatgptAuthTokens/refresh",
               "params" => %{"reason" => "expired"}
             }},
            {:inbound,
             %{
               "id" => 102,
               "method" => "thread/somethingNew",
               "params" => %{"threadId" => @thread}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(@area, "unsupported_server_request_replies_native_error", steps)

      assert acp_requests(transcript) == []

      assert [
               %{
                 "id" => 100,
                 "error" => %{
                   "code" => -32_601,
                   "message" => "Unsupported app-server request: item/tool/call"
                 }
               },
               %{"id" => "req-2", "error" => %{"code" => -32_601}},
               %{
                 "id" => 102,
                 "error" => %{
                   "code" => -32_601,
                   "message" => "Unsupported app-server request: thread/somethingNew"
                 }
               }
             ] = Enum.take(CodexGolden.writes(transcript), -3)
    end
  end

  # -- ChatGPT device-code login ---------------------------------------------

  describe "ChatGPT device-code login" do
    test "chatgpt_device_code_login_completes_via_url_elicitation" do
      steps =
        handshake_steps(@url_caps) ++
          [
            {:outbound, authenticate(30)},
            {:inbound, device_code_result(3, %{"loginId" => "login-1"})},
            {:note,
             "Consent to the url elicitation defers: nothing is written until the app-server reports completion"},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:inbound,
             %{
               "method" => "account/login/completed",
               "params" => %{"loginId" => "login-1", "success" => true}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "chatgpt_device_code_login_completes_via_url_elicitation",
          steps
        )

      assert [
               %{
                 "id" => 3,
                 "method" => "account/login/start",
                 "params" => %{"type" => "chatgptDeviceCode"}
               }
             ] =
               Enum.take(CodexGolden.writes(transcript), -1)

      assert [
               %{
                 "method" => "elicitation/create",
                 "id" => "codex-auth-elicitation-<1>",
                 "params" => %{"requestId" => 30, "mode" => "url"}
               },
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "login-1"}},
               %{"id" => 30, "result" => %{}}
             ] = CodexGolden.messages(transcript)
    end

    test "chatgpt_device_code_login_declined_cancels_login" do
      steps =
        handshake_steps(@url_caps) ++
          [
            {:outbound, authenticate(30)},
            {:inbound, device_code_result(3, %{"loginId" => "login-1"})},
            {:outbound, elicitation_reply(%{"action" => "decline"})},
            {:note, "A late completion for the cancelled login is ignored"},
            {:inbound,
             %{
               "method" => "account/login/completed",
               "params" => %{"loginId" => "login-1", "success" => true}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "chatgpt_device_code_login_declined_cancels_login",
          steps
        )

      assert %{
               tag: :messages_and_write,
               messages: [
                 %{
                   "id" => 30,
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
             } = transcript |> Enum.at(-3) |> Map.fetch!(:result)

      assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
    end

    test "chatgpt_device_code_login_failure_without_login_id" do
      steps =
        handshake_steps(@url_caps) ++
          [
            {:outbound, authenticate(30)},
            {:note,
             "Without a loginId the adapter mints one, which doubles as the elicitationId"},
            {:inbound, device_code_result(3, %{"userCode" => nil})},
            {:note,
             "The minted login id is also a generated id, so the reply targets the request id explicitly"},
            {:outbound, elicitation_reply(%{"action" => "accept"}, 0)},
            {:note, "A completion for another login is ignored; one without a loginId matches"},
            {:inbound,
             %{
               "method" => "account/login/completed",
               "params" => %{"loginId" => "other-login", "success" => true}
             }},
            {:inbound,
             %{
               "method" => "account/login/completed",
               "params" => %{"success" => false, "error" => "Device code expired"}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "chatgpt_device_code_login_failure_without_login_id",
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
               %{"id" => 30, "error" => %{"code" => -32_603, "message" => "Device code expired"}}
             ] = CodexGolden.messages(transcript)
    end

    test "chatgpt_device_code_login_failure_without_error_message" do
      steps =
        handshake_steps(@url_caps) ++
          [
            {:outbound, authenticate(30)},
            {:inbound, device_code_result(3, %{"loginId" => "login-1"})},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:note,
             "A failed completion without an error string fails authenticate with the adapter's default message"},
            {:inbound,
             %{
               "method" => "account/login/completed",
               "params" => %{"loginId" => "login-1", "success" => false}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "chatgpt_device_code_login_failure_without_error_message",
          steps
        )

      assert %{
               tag: :messages,
               messages: [
                 %{
                   "method" => "elicitation/complete",
                   "params" => %{"elicitationId" => "login-1"}
                 },
                 %{
                   "id" => 30,
                   "error" => %{"code" => -32_603, "message" => "Codex authentication failed"}
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end

    test "chatgpt_device_code_login_empty_user_code_message" do
      steps =
        handshake_steps(@url_caps) ++
          [
            {:outbound, authenticate(30)},
            {:note,
             "An empty userCode string is treated like a missing one: the prompt does not ask the user to enter a code"},
            {:inbound, device_code_result(3, %{"loginId" => "login-1", "userCode" => ""})},
            {:outbound, elicitation_reply(%{"action" => "accept"})},
            {:inbound,
             %{
               "method" => "account/login/completed",
               "params" => %{"loginId" => "login-1", "success" => true}
             }}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "chatgpt_device_code_login_empty_user_code_message",
          steps
        )

      assert [
               %{
                 "method" => "elicitation/create",
                 "params" => %{
                   "mode" => "url",
                   "url" => "https://chatgpt.com/device",
                   "message" => "Sign in to ChatGPT to continue."
                 }
               },
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "login-1"}},
               %{"id" => 30, "result" => %{}}
             ] = CodexGolden.messages(transcript)
    end

    test "chatgpt_device_code_login_completes_without_consent" do
      steps =
        handshake_steps(@url_caps) ++
          [
            {:outbound, authenticate(30)},
            {:inbound, device_code_result(3, %{"loginId" => "login-1"})},
            {:note,
             "The client never answers the url elicitation; the app-server completion alone resolves authenticate"},
            {:inbound,
             %{
               "method" => "account/login/completed",
               "params" => %{"loginId" => "login-1", "success" => true}
             }},
            {:note, "The late consent finds no pending entry and is dropped"},
            {:outbound, elicitation_reply(%{"action" => "accept"})}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "chatgpt_device_code_login_completes_without_consent",
          steps
        )

      assert [
               %{"method" => "elicitation/create", "id" => "codex-auth-elicitation-<1>"},
               %{"method" => "elicitation/complete", "params" => %{"elicitationId" => "login-1"}},
               %{"id" => 30, "result" => %{}}
             ] = CodexGolden.messages(transcript)

      assert %{tag: :ok, skipped: true} = CodexGolden.last_result(transcript)
    end

    test "chatgpt_device_code_login_requires_url_capability" do
      steps =
        handshake_steps(@form_caps) ++
          [
            {:note,
             "Only form elicitation is advertised: the authenticate request fails before any write"},
            {:outbound, authenticate(30)},
            {:note,
             "The client re-initializes with url support; a result without a verification URL is an error"},
            {:outbound,
             %{
               "method" => "initialize",
               "id" => 0,
               "params" => %{"protocolVersion" => 1, "clientCapabilities" => @url_caps}
             }},
            {:outbound, authenticate(31)},
            {:inbound,
             %{"id" => 3, "result" => %{"type" => "chatgptDeviceCode", "loginId" => "login-2"}}}
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "chatgpt_device_code_login_requires_url_capability",
          steps
        )

      assert %{
               tag: :error,
               error: "ChatGPT device-code authentication requires ACP URL elicitation support"
             } =
               transcript |> Enum.at(-5) |> Map.fetch!(:result)

      assert %{
               tag: :messages,
               messages: [
                 %{
                   "id" => 31,
                   "error" => %{
                     "message" =>
                       "Codex device-code authentication did not return a verification URL"
                   }
                 }
               ]
             } = CodexGolden.last_result(transcript)
    end
  end

  # -- approvalsReviewer per mode ---------------------------------------------

  describe "turn approval policy" do
    test "turn_start_approvals_reviewer_per_mode" do
      steps =
        handshake_steps() ++
          Enum.flat_map(
            Enum.with_index([
              {"read-only", "thread-ro"},
              {"agent", "thread-agent"},
              {"agent-full-access", "thread-full"}
            ]),
            fn {{mode_id, thread_id}, index} ->
              [
                {:note, "Session in #{mode_id} mode"},
                {:outbound,
                 %{
                   "method" => "session/new",
                   "id" => 10 + index,
                   "params" => %{"cwd" => "/tmp/project", "mcpServers" => [], "modeId" => mode_id}
                 }},
                {:inbound, thread_start_result(3 + 2 * index, thread_id)},
                {:outbound, prompt(20 + index, thread_id)}
              ]
            end
          )

      transcript =
        CodexGolden.assert_golden(@area, "turn_start_approvals_reviewer_per_mode", steps)

      assert [
               %{
                 "method" => "turn/start",
                 "params" => %{
                   "approvalsReviewer" => "user",
                   "approvalPolicy" => "on-request",
                   "sandboxPolicy" => %{"type" => "workspaceWrite"}
                 }
               },
               %{
                 "method" => "turn/start",
                 "params" => %{
                   "approvalsReviewer" => "auto_review",
                   "approvalPolicy" => "on-request"
                 }
               },
               %{
                 "method" => "turn/start",
                 "params" => %{
                   "approvalsReviewer" => "user",
                   "approvalPolicy" => "never",
                   "sandboxPolicy" => %{"type" => "dangerFullAccess"}
                 }
               }
             ] =
               transcript |> CodexGolden.writes() |> Enum.filter(&(&1["method"] == "turn/start"))
    end
  end

  # ACP requests and notifications the adapter emitted (the session/new
  # response that opens every session is a message too, so filter it out).
  defp acp_requests(transcript) do
    transcript |> CodexGolden.messages() |> Enum.filter(&Map.has_key?(&1, "method"))
  end

  # The result recorded for the inbound app-server message carrying `id`
  # (step-anchored, so assertions do not depend on the transcript length).
  defp inbound_result(transcript, id) do
    %{result: result} =
      Enum.find(transcript, &match?(%{step: %{kind: :inbound, message: %{"id" => ^id}}}, &1))

    result
  end

  # The result recorded for the `nth` (zero-based) inbound step whose message
  # equals `message`; anchors assertions on repeated notifications, which
  # carry no id.
  defp inbound_step_result(transcript, message, nth) do
    %{result: result} =
      transcript
      |> Enum.filter(&match?(%{step: %{kind: :inbound, message: ^message}}, &1))
      |> Enum.at(nth)

    result
  end

  # The result recorded for the outbound ACP request carrying `id`.
  defp outbound_result(transcript, id) do
    %{result: result} =
      Enum.find(transcript, &match?(%{step: %{kind: :outbound, message: %{"id" => ^id}}}, &1))

    result
  end

  defp server_resolved(request_id) do
    %{
      "method" => "serverRequest/resolved",
      "params" => %{"threadId" => @thread, "requestId" => request_id}
    }
  end

  # Strips the thread id from an app-server request so the adapter must fall
  # back to the current (only open) session.
  defp without_thread_id(request), do: without_param(request, "threadId")

  # Removes one params key from an app-server request built by the helpers
  # below (as opposed to sending it as JSON null), so the adapter's default
  # for an absent field is what gets pinned.
  defp without_param(request, key) do
    update_in(request["params"], &Map.delete(&1, key))
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

  # A session with an in-flight turn: session/new -> thread/start reply ->
  # session/prompt -> turn/start reply -> turn/started.
  defp active_turn_steps(client_capabilities \\ %{}) do
    handshake_steps(client_capabilities) ++
      [
        {:outbound,
         %{
           "method" => "session/new",
           "id" => 10,
           "params" => %{"cwd" => "/tmp/project", "mcpServers" => []}
         }},
        {:inbound, thread_start_result(3, @thread)},
        {:outbound, prompt(20, @thread)},
        {:inbound,
         %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1", "status" => "inProgress"}}}},
        {:inbound,
         %{
           "method" => "turn/started",
           "params" => %{"threadId" => @thread, "turn" => %{"id" => "turn-1"}}
         }}
      ]
  end

  defp thread_start_result(id, thread_id) do
    %{
      "id" => id,
      "result" => %{
        "model" => "gpt-5",
        "thread" => %{"id" => thread_id, "cwd" => "/tmp/project", "updatedAt" => 1_700_000_000}
      }
    }
  end

  defp prompt(id, thread_id) do
    %{
      "method" => "session/prompt",
      "id" => id,
      "params" => %{
        "sessionId" => thread_id,
        "prompt" => [%{"type" => "text", "text" => "Run the test suite"}]
      }
    }
  end

  defp authenticate(id) do
    %{"method" => "authenticate", "id" => id, "params" => %{"methodId" => "chat-gpt-device-code"}}
  end

  defp device_code_result(id, extra) do
    %{
      "id" => id,
      "result" =>
        Map.merge(
          %{
            "type" => "chatgptDeviceCode",
            "verificationUrl" => "https://chatgpt.com/device",
            "userCode" => "ABCD-EFGH"
          },
          extra
        )
    }
  end

  # -- app-server request builders -------------------------------------------

  defp command_request(id, extra \\ %{}) do
    %{
      "id" => id,
      "method" => "item/commandExecution/requestApproval",
      "params" =>
        Map.merge(
          %{
            "threadId" => @thread,
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "command" => "mix test",
            "cwd" => "/tmp/project",
            "reason" => "  The agent wants to run the test suite  ",
            "startedAtMs" => 1_700_000_000_000
          },
          extra
        )
    }
  end

  defp file_change_request(id, extra \\ %{}) do
    %{
      "id" => id,
      "method" => "item/fileChange/requestApproval",
      "params" =>
        Map.merge(
          %{
            "threadId" => @thread,
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "reason" => "Update the README",
            "grantRoot" => "/tmp/project"
          },
          extra
        )
    }
  end

  defp permissions_request(id, extra \\ %{}) do
    %{
      "id" => id,
      "method" => "item/permissions/requestApproval",
      "params" =>
        Map.merge(
          %{
            "threadId" => @thread,
            "turnId" => "turn-1",
            "itemId" => "item-1",
            "reason" => "Needs network access to fetch dependencies",
            "permissions" => %{
              "network" => %{"enabled" => true},
              "fileSystem" => %{"write" => ["/tmp/project"]},
              "environment" => %{"variables" => ["HOME"]}
            }
          },
          extra
        )
    }
  end

  defp legacy_request(id, method, extra) do
    %{
      "id" => id,
      "method" => method,
      "params" =>
        Map.merge(
          %{
            "threadId" => @thread,
            "command" => ["mix", "test"],
            "cwd" => "/tmp/project",
            "reason" => "Run tests"
          },
          extra
        )
    }
  end

  defp mcp_elicitation_request(id, extra) do
    %{
      "id" => id,
      "method" => "mcpServer/elicitation/request",
      "params" =>
        Map.merge(
          %{
            "threadId" => @thread,
            "serverName" => "tool-server",
            "message" => "Allow tool call?"
          },
          extra
        )
    }
  end

  defp url_elicitation_request(id, elicitation_id) do
    mcp_elicitation_request(id, %{
      "mode" => "url",
      "elicitationId" => elicitation_id,
      "url" => "https://example.com/authorize/#{elicitation_id}",
      "message" => "Authorize the MCP server"
    })
  end

  defp user_input_request(id, extra) do
    %{
      "id" => id,
      "method" => "item/tool/requestUserInput",
      "params" =>
        Map.merge(
          %{"threadId" => @thread, "turnId" => "turn-1", "itemId" => "question-tool"},
          extra
        )
    }
  end

  # -- ACP reply builders (step functions resolving the generated id) ---------

  # Answers the most recently generated adapter request id with `result`.
  defp reply_last(reply, index \\ -1) do
    fn transcript ->
      ids = CodexGolden.generated_ids(transcript)
      Map.merge(%{"jsonrpc" => "2.0", "id" => Enum.at(ids, index)}, reply)
    end
  end

  defp select(option_id, index \\ -1) do
    reply_last(
      %{"result" => %{"outcome" => %{"outcome" => "selected", "optionId" => option_id}}},
      index
    )
  end

  defp cancelled do
    reply_last(%{"result" => %{"outcome" => %{"outcome" => "cancelled"}}})
  end

  defp elicitation_reply(result, index \\ -1), do: reply_last(%{"result" => result}, index)

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
