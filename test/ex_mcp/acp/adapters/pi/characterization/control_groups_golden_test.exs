defmodule ExMCP.ACP.Adapters.Pi.ControlGroupsGoldenTest do
  @moduledoc """
  Characterization gate for the Pi ACP adapter's control-group completion
  and failure ordering (area P2 of `docs/POST_1_0_MAINTENANCE_PLAN.md`,
  "Pi adapter restructuring" / "Characterization gate").

  Each test drives `ExMCP.ACP.Adapters.Pi` through `ExMCP.Test.PiGolden` and
  compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/pi/control_groups/`. The fixtures pin:

    * a `session/new` group completing only once all four correlated
      responses arrived, in whatever order Pi answers, and the emitted
      order `response`, `available_commands_update`;
    * the first failed member answering the ACP request at once with
      `-32603 <error>` (or the `-32000` auth-required error when the text
      looks like an authentication failure, or when the model list is
      empty) and every later response for that group being dropped as
      untracked;
    * the fallbacks applied to a sparse `get_state` (minted `pi-<n>` session
      id, first catalog entry as current model, default thinking level,
      no session-map entry without a session file), a current model that
      is not in the catalog, catalog entries without provider or id, a
      catalog payload without a `models` list, and thinking-level
      normalization;
    * two overlapping `session/new` groups completing independently, a
      duplicate response after completion being skipped, and non-string
      error payloads;
    * the same rules for `session/load` groups (including the requested
      session id winning over the one reported by `get_state`) and for
      single-request slash-command groups, including the two-phase
      `/autocompact` toggle whose second request is written from the
      first response.

  Subprocess exit failing pending groups is characterized by the
  prompt_flow area; the request envelopes themselves by the rpc area. A
  catalog whose `models` is not a list or whose entries are not maps makes
  the adapter raise inside `translate_inbound/2` (there is no transcript to
  pin), so those payloads are deliberately not characterized.

  Mutation check (2026-09-20): making `finish_control_error/3` keep the
  group instead of deleting it (so later responses stay tracked) fails
  `failed_new_session_errors_at_once_and_drops_later_responses`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `PI_GOLDEN=update mix test <this file>[:line]`; that run rewrites the
  fixture and fails on purpose, so review the diff and re-run without the
  variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.PiGolden
  alias ExMCP.Test.PiGolden.Flows

  @area "control_groups"

  defp late_responses do
    [
      {:note, "Responses for the failed group are untracked and skipped"},
      {:respond, "get_state", Flows.state_data()},
      {:respond, "get_available_models", %{"models" => Flows.models()}},
      {:respond, "get_commands", %{"commands" => []}}
    ]
  end

  defp only_error_result(transcript) do
    transcript
    |> Enum.map(& &1.result)
    |> Enum.filter(&Map.has_key?(&1, :messages))
  end

  describe "session/new groups" do
    test "session_new_completes_after_all_responses_in_any_order" do
      steps = [
        Flows.session_new(1),
        {:respond, "get_commands", %{"commands" => []}},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_state", Flows.state_data()},
        {:respond, "new_session", %{}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_new_completes_after_all_responses_in_any_order",
          steps
        )

      assert [%{tag: :skip}, %{tag: :skip}, %{tag: :skip}, %{tag: :messages, messages: [_, _]}] =
               Enum.map(Enum.drop(transcript, 2), & &1.result)
    end

    test "failed_new_session_errors_at_once_and_drops_later_responses" do
      steps = [Flows.session_new(1), {:respond_error, "new_session", "boom"}] ++ late_responses()

      transcript =
        PiGolden.assert_golden(
          @area,
          "failed_new_session_errors_at_once_and_drops_later_responses",
          steps
        )

      assert [%{messages: [%{"error" => %{"code" => -32_603, "message" => "boom"}, "id" => 1}]}] =
               only_error_result(transcript)

      assert Enum.all?(Enum.take(transcript, -3), &match?(%{tag: :skip}, &1.result))
    end

    test "failed_get_state_after_partial_success_errors" do
      steps = [
        Flows.session_new(1),
        {:respond, "new_session", %{}},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond_error, "get_state", "state unavailable"},
        {:respond, "get_commands", %{"commands" => []}}
      ]

      transcript =
        PiGolden.assert_golden(@area, "failed_get_state_after_partial_success_errors", steps)

      assert [%{messages: [%{"error" => %{"message" => "state unavailable"}}]}] =
               only_error_result(transcript)
    end

    test "failed_last_member_errors_instead_of_completing" do
      steps = [
        Flows.session_new(1),
        {:respond, "new_session", %{}},
        {:respond, "get_state", Flows.state_data()},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond_error, "get_commands", "commands unavailable"}
      ]

      transcript =
        PiGolden.assert_golden(@area, "failed_last_member_errors_instead_of_completing", steps)

      assert %{messages: [%{"error" => %{"message" => "commands unavailable"}}]} =
               PiGolden.last_result(transcript)
    end

    test "auth_like_error_text_maps_to_auth_required" do
      texts = [
        "Missing API key for anthropic",
        "no key configured",
        "Not Configured",
        "HTTP 403 forbidden",
        "permission denied",
        "Authentication failed"
      ]

      steps =
        Enum.flat_map(Enum.with_index(texts, 1), fn {text, acp_id} ->
          [Flows.session_new(acp_id), {:respond_error, "get_available_models", text}]
        end)

      transcript =
        PiGolden.assert_golden(@area, "auth_like_error_text_maps_to_auth_required", steps)

      errors = transcript |> PiGolden.messages() |> Enum.map(& &1["error"])
      assert length(errors) == 6

      assert Enum.all?(
               errors,
               &match?(
                 %{
                   "code" => -32_000,
                   "message" => "Configure an API key or log in with an OAuth provider.",
                   "data" => %{
                     "authMethods" => [%{"id" => "pi_terminal_login", "type" => "terminal"}]
                   }
                 },
                 &1
               )
             )
    end

    test "empty_model_list_is_auth_required_even_when_every_response_succeeded" do
      steps = [
        Flows.session_new(1),
        {:respond, "new_session", %{}},
        {:respond, "get_state", Flows.state_data()},
        {:respond, "get_commands", %{"commands" => []}},
        {:respond, "get_available_models", %{"models" => []}},
        {:note, "The failed session left no map entry"},
        {:read_file, "<sandbox>/session-map.json"}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "empty_model_list_is_auth_required_even_when_every_response_succeeded",
          steps
        )

      assert [%{messages: [%{"error" => %{"code" => -32_000}, "id" => 1}]}] =
               only_error_result(transcript)

      assert %{exists: false} = PiGolden.last_result(transcript)
    end

    test "sparse_get_state_uses_fallbacks" do
      steps = [
        Flows.session_new(1),
        {:respond, "new_session", %{}},
        {:respond, "get_state", %{}},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}},
        {:read_file, "<sandbox>/session-map.json"},
        Flows.prompt(2, "hi", %{"sessionId" => nil}),
        Flows.agent_settled()
      ]

      transcript = PiGolden.assert_golden(@area, "sparse_get_state_uses_fallbacks", steps)

      assert %{
               "id" => 1,
               "result" => %{
                 "sessionId" => "pi-<5>",
                 "models" => %{"currentModelId" => "anthropic/claude-sonnet-4"},
                 "modes" => %{"currentModeId" => "medium"},
                 "_meta" => %{"ex_mcp" => %{"pi" => %{}}}
               }
             } = transcript |> PiGolden.messages() |> Enum.find(&(&1["id"] == 1))

      assert %{exists: false} = Enum.at(transcript, 6).result
    end

    test "current_model_outside_catalog_and_malformed_catalog_entries" do
      models = [
        %{"provider" => "anthropic", "id" => "claude-sonnet-4"},
        %{"provider" => "", "id" => "no-provider"},
        %{"provider" => "openai"},
        %{"provider" => 7, "id" => 42, "name" => "numeric"}
      ]

      steps = [
        Flows.session_new(1),
        {:respond, "new_session", %{}},
        {:respond, "get_state",
         Flows.state_data(%{
           "model" => %{"provider" => "local", "id" => "llama"},
           "thinkingLevel" => "ultra"
         })},
        {:respond, "get_available_models", %{"models" => models}},
        {:respond, "get_commands", %{"commands" => []}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "current_model_outside_catalog_and_malformed_catalog_entries",
          steps
        )

      assert %{
               "result" => %{
                 "models" => %{
                   "currentModelId" => "local/llama",
                   "availableModels" => [
                     %{
                       "modelId" => "anthropic/claude-sonnet-4",
                       "name" => "anthropic/claude-sonnet-4"
                     },
                     %{"modelId" => "7/42", "name" => "7/numeric"}
                   ]
                 },
                 "modes" => %{"currentModeId" => "medium"}
               }
             } = transcript |> PiGolden.messages() |> Enum.find(&(&1["id"] == 1))
    end

    test "catalog_without_models_key_completes_without_model_option" do
      steps = [
        Flows.session_new(1),
        {:respond, "new_session", %{}},
        {:respond, "get_state", Flows.state_data(%{"model" => nil})},
        {:respond, "get_available_models", %{}},
        {:respond, "get_commands", %{"commands" => []}},
        Flows.session_new(2),
        {:respond, "new_session", %{}},
        {:respond, "get_state", Flows.state_data(%{"model" => nil, "thinkingLevel" => "xhigh"})},
        {:respond, "get_available_models", %{"other" => 1}},
        {:respond, "get_commands", %{"commands" => []}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "catalog_without_models_key_completes_without_model_option",
          steps
        )

      responses = transcript |> PiGolden.messages() |> Enum.filter(&Map.has_key?(&1, "id"))

      assert [
               %{
                 "result" => %{
                   "models" => nil,
                   "configOptions" => [%{"id" => "thought_level"} | _]
                 }
               },
               %{"result" => %{"models" => nil, "modes" => %{"currentModeId" => "xhigh"}}}
             ] = responses
    end

    test "overlapping_session_new_groups_complete_independently" do
      steps = [
        Flows.session_new(1),
        Flows.session_new(2, %{"cwd" => "<sandbox>/project"}),
        {:note, "Answer the second group first"},
        {:respond, "new_session", %{}},
        {:respond, "get_state", Flows.state_data(%{"sessionId" => "second"})},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}},
        {:inbound, fn t -> success(t, "new_session", 0, %{}) end},
        {:inbound,
         fn t -> success(t, "get_state", 0, Flows.state_data(%{"sessionId" => "first"})) end},
        {:inbound,
         fn t -> success(t, "get_available_models", 0, %{"models" => Flows.models()}) end},
        {:inbound, fn t -> success(t, "get_commands", 0, %{"commands" => []}) end},
        {:note, "A duplicate response after completion is untracked"},
        {:inbound, fn t -> success(t, "get_commands", 0, %{"commands" => []}) end}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "overlapping_session_new_groups_complete_independently",
          steps
        )

      assert [
               %{"id" => 2, "result" => %{"sessionId" => "second"}},
               %{"id" => 1, "result" => %{"sessionId" => "first"}}
             ] = transcript |> PiGolden.messages() |> Enum.filter(&Map.has_key?(&1, "id"))

      assert %{tag: :skip} = PiGolden.last_result(transcript)
    end

    test "non_string_error_payloads_are_stringified" do
      steps = [
        Flows.session_new(1),
        {:respond_error, "new_session", 42},
        Flows.session_new(2),
        {:respond_error, "new_session", nil},
        Flows.session_new(3),
        {:inbound,
         fn t ->
           %{
             "type" => "response",
             "id" => PiGolden.request_id(t, "get_state"),
             "success" => false
           }
         end}
      ]

      transcript =
        PiGolden.assert_golden(@area, "non_string_error_payloads_are_stringified", steps)

      assert [
               %{"error" => %{"message" => "42"}},
               %{"error" => %{"message" => ""}},
               %{"error" => %{"message" => ""}}
             ] = PiGolden.messages(transcript)
    end
  end

  describe "session/load groups" do
    test "failed_switch_session_errors_and_drops_later_responses" do
      steps =
        [
          Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
          Flows.session_load(1, "mapped"),
          {:respond_error, "switch_session", "no such session"},
          {:respond, "get_messages", %{"messages" => []}}
        ] ++ late_responses()

      transcript =
        PiGolden.assert_golden(
          @area,
          "failed_switch_session_errors_and_drops_later_responses",
          steps
        )

      assert [%{messages: [%{"error" => %{"message" => "no such session"}, "id" => 1}]}] =
               only_error_result(transcript)
    end

    test "load_with_empty_models_is_auth_required" do
      steps = [
        Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
        Flows.session_load(1, "mapped"),
        {:respond, "switch_session", %{}},
        {:respond, "get_messages", %{"messages" => [%{"role" => "user", "content" => "hi"}]}},
        {:respond, "get_state", Flows.state_data(%{"sessionId" => "mapped"})},
        {:respond, "get_commands", %{"commands" => []}},
        {:respond, "get_available_models", %{"models" => []}}
      ]

      transcript = PiGolden.assert_golden(@area, "load_with_empty_models_is_auth_required", steps)

      assert %{messages: [%{"error" => %{"code" => -32_000}, "id" => 1}]} =
               PiGolden.last_result(transcript)
    end

    test "load_response_uses_requested_session_id_over_get_state" do
      steps = [
        Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
        Flows.session_resume(1, "mapped"),
        {:respond, "switch_session", %{}},
        {:respond, "get_state",
         Flows.state_data(%{
           "sessionId" => "pi-reported",
           "sessionFile" => "<sandbox>/sessions/reported.jsonl",
           "cwd" => "<sandbox>/elsewhere"
         })},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}},
        {:read_file, "<sandbox>/session-map.json"}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "load_response_uses_requested_session_id_over_get_state",
          steps
        )

      assert %{
               content: %{
                 "sessions" => %{
                   "mapped" => %{
                     "cwd" => "<sandbox>/elsewhere",
                     "sessionFile" => "<sandbox>/sessions/reported.jsonl"
                   }
                 }
               }
             } = PiGolden.last_result(transcript)
    end
  end

  describe "slash-command groups" do
    test "single_request_group_success_and_failure" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "/session"),
            {:respond, "get_session_stats", %{"sessionId" => "pi-session", "totalMessages" => 3}},
            Flows.prompt(3, "/session"),
            {:respond_error, "get_session_stats", "stats unavailable"},
            Flows.prompt(4, "/compact"),
            {:respond_error, "compact", "401 unauthorized"},
            {:note, "A late duplicate response is untracked"},
            {:respond, "compact", %{"summary" => "late"}}
          ]

      transcript =
        PiGolden.assert_golden(@area, "single_request_group_success_and_failure", steps)

      assert [
               %{"id" => 2, "result" => %{"stopReason" => "end_turn"}},
               %{"id" => 3, "error" => %{"code" => -32_603, "message" => "stats unavailable"}},
               %{"id" => 4, "error" => %{"code" => -32_000}}
             ] =
               transcript
               |> PiGolden.messages()
               |> Enum.filter(&(Map.has_key?(&1, "id") and &1["id"] != 1))

      assert %{tag: :skip} = PiGolden.last_result(transcript)
    end

    test "autocompact_toggle_is_a_two_phase_group" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "/autocompact"),
            {:respond, "get_state", %{"autoCompactionEnabled" => true}},
            {:respond, "set_auto_compaction", %{}},
            Flows.prompt(3, "/autocompact"),
            {:respond, "get_state", %{"autoCompactionEnabled" => "false"}},
            {:respond_error, "set_auto_compaction", "cannot toggle"},
            Flows.prompt(4, "/autocompact"),
            {:respond_error, "get_state", "state failed"}
          ]

      transcript = PiGolden.assert_golden(@area, "autocompact_toggle_is_a_two_phase_group", steps)

      assert [
               %{"type" => "get_state"},
               %{"type" => "set_auto_compaction", "enabled" => false},
               %{"type" => "get_state"},
               %{"type" => "set_auto_compaction", "enabled" => true},
               %{"type" => "get_state"}
             ] = transcript |> PiGolden.writes() |> Enum.drop(4)

      assert [
               %{"id" => 2, "result" => %{"stopReason" => "end_turn"}},
               %{"id" => 3, "error" => %{"message" => "cannot toggle"}},
               %{"id" => 4, "error" => %{"message" => "state failed"}}
             ] =
               transcript
               |> PiGolden.messages()
               |> Enum.filter(&(Map.has_key?(&1, "id") and &1["id"] != 1))
    end
  end

  defp success(transcript, type, index, data) do
    id = transcript |> PiGolden.request_ids(type) |> Enum.at(index)
    %{"type" => "response", "id" => id, "command" => type, "success" => true, "data" => data}
  end
end
