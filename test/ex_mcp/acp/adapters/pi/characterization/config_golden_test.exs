defmodule ExMCP.ACP.Adapters.Pi.ConfigGoldenTest do
  @moduledoc """
  Characterization gate for the Pi ACP adapter's configuration updates
  (area P5 of `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Pi adapter restructuring"
  / "Characterization gate": model, thinking-level, and boolean
  configuration updates).

  Each test drives `ExMCP.ACP.Adapters.Pi` through `ExMCP.Test.PiGolden` and
  compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/pi/config/`. The fixtures pin:

    * `session/set_mode` for every thinking level: the `current_mode_update`
      and `config_option_update` notifications, the `set_thinking_level`
      write, the `Unknown modeId` error, and the session id fallback;
    * `session/set_model` with a full `provider/model` id (including a
      model id that itself contains a slash), a bare id resolved against
      the advertised catalog (exact or suffix match), unknown, missing and
      non-string ids, and the `config_option_update` that omits the model
      selector when no catalog is known;
    * `session/set_config_option` for `model`, `thought_level` (valid and
      invalid), `auto_compaction` / `auto_retry` with boolean and `"true"` /
      `"false"` string values (any other value is an unknown option),
      `steering_mode` / `follow_up_mode`, and unknown option ids;
    * the config option catalog carried by `session/new` responses and
      `config_option_update` notifications (model catalog and thinking level
      following the session state, then the four runtime selectors), and
      how a mode change and a model change compose;
    * managed mode replying synchronously: `set_config_option` answers with
      `configOptions` whose model selector omits the `options` catalog,
      `set_mode` and boolean options reply `{}`, and every update fails
      with `:no_active_pi_session` before a session exists.

  The `/model` and `/thinking` slash notices are characterized by the
  slash_commands area, and `steering_mode` / `follow_up_mode` also by the
  prompt_flow area.

  A `get_available_models` payload is agent-controlled, so the malformed
  shapes are pinned here too: entries that are not maps, or lack a provider
  or id, are dropped from the selector, and a `models` value that is not a
  list yields no model selector at all. Before the fix each of those raised.

  Mutation check (2026-09-20): keeping the model selector's `options` in
  `confirmation_config_options_for_state/1` fails
  `managed_set_config_replies_with_confirmation_without_model_catalog`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `PI_GOLDEN=update mix test <this file>[:line]`; that run rewrites the
  fixture and fails on purpose, so review the diff and re-run without the
  variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.PiGolden
  alias ExMCP.Test.PiGolden.Flows

  @area "config"
  @levels ~w(off minimal low medium high xhigh)

  defp updates(transcript) do
    transcript
    |> PiGolden.messages()
    |> Enum.map(&get_in(&1, ["params", "update"]))
    |> Enum.reject(&is_nil/1)
  end

  defp option(update, id), do: Enum.find(update["configOptions"] || [], &(&1["id"] == id))

  # `session/new` carries the catalog on its reply rather than in a
  # `session/update` notification.
  defp reply_options(transcript) do
    transcript
    |> PiGolden.messages()
    |> Enum.find_value(%{}, fn message ->
      options = get_in(message, ["result", "configOptions"])
      if is_list(options), do: %{"configOptions" => options}
    end)
  end

  describe "session/set_mode" do
    test "set_mode_accepts_every_thinking_level" do
      steps =
        Flows.open_session(1) ++
          Enum.map(Enum.with_index(@levels, 2), fn {level, acp_id} ->
            Flows.set_mode(acp_id, level)
          end)

      transcript = PiGolden.assert_golden(@area, "set_mode_accepts_every_thinking_level", steps)

      assert @levels ==
               transcript
               |> PiGolden.writes()
               |> Enum.filter(&(&1["type"] == "set_thinking_level"))
               |> Enum.map(& &1["level"])

      assert @levels ==
               transcript
               |> updates()
               |> Enum.filter(&(&1["sessionUpdate"] == "current_mode_update"))
               |> Enum.map(& &1["currentModeId"])

      assert @levels ==
               transcript
               |> updates()
               |> Enum.filter(&(&1["sessionUpdate"] == "config_option_update"))
               |> Enum.map(&option(&1, "thought_level")["currentValue"])
    end

    test "set_mode_rejects_unknown_levels" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_mode(2, "ultra"),
            Flows.set_mode(3, "HIGH"),
            Flows.set_mode(4, nil),
            {:outbound, %{"method" => "session/set_mode", "id" => 5, "params" => %{}}}
          ]

      transcript = PiGolden.assert_golden(@area, "set_mode_rejects_unknown_levels", steps)

      assert [
               %{error: "Unknown modeId: \"ultra\""},
               %{error: "Unknown modeId: \"HIGH\""},
               %{error: "Unknown modeId: nil"},
               %{error: "Unknown modeId: nil"}
             ] = transcript |> Enum.take(-4) |> Enum.map(&Map.take(&1.result, [:error]))
    end

    test "set_mode_before_a_session_uses_the_request_session_id" do
      steps = [
        Flows.set_mode(1, "high", %{"sessionId" => "requested"}),
        Flows.set_mode(2, "low", %{"sessionId" => nil})
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "set_mode_before_a_session_uses_the_request_session_id",
          steps
        )

      assert ["requested", "requested", "default", "default"] =
               transcript |> PiGolden.messages() |> Enum.map(& &1["params"]["sessionId"])

      refute transcript |> updates() |> Enum.any?(&option(&1, "model"))
    end
  end

  describe "session/set_model" do
    test "set_model_with_full_ids_writes_set_model_and_updates_the_catalog" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_model(2, "openai/gpt-5.1"),
            Flows.set_model(3, "custom/org/model-x"),
            Flows.set_model(4, "unknown-provider/anything")
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "set_model_with_full_ids_writes_set_model_and_updates_the_catalog",
          steps
        )

      assert [
               %{"type" => "set_model", "provider" => "openai", "modelId" => "gpt-5.1"},
               %{"type" => "set_model", "provider" => "custom", "modelId" => "org/model-x"},
               %{"type" => "set_model", "provider" => "unknown-provider", "modelId" => "anything"}
             ] = transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "set_model"))

      assert ["openai/gpt-5.1", "custom/org/model-x", "unknown-provider/anything"] =
               transcript
               |> updates()
               |> Enum.map(&option(&1, "model")["currentValue"])
               |> Enum.reject(&is_nil/1)
    end

    test "set_model_resolves_bare_ids_against_the_catalog" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_model(2, "gpt-5.1"),
            Flows.set_model(3, "claude-sonnet-4"),
            Flows.set_model(4, "missing-model"),
            Flows.set_model(5, ""),
            Flows.set_model(6, nil),
            Flows.set_model(7, 42)
          ]

      transcript =
        PiGolden.assert_golden(@area, "set_model_resolves_bare_ids_against_the_catalog", steps)

      assert [
               %{"provider" => "openai", "modelId" => "gpt-5.1"},
               %{"provider" => "anthropic", "modelId" => "claude-sonnet-4"}
             ] = transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "set_model"))

      assert [
               %{error: "Unknown modelId: missing-model"},
               %{error: "session/set_model requires modelId"},
               %{error: "session/set_model requires modelId"},
               %{error: "session/set_model requires modelId"}
             ] = transcript |> Enum.take(-4) |> Enum.map(&Map.take(&1.result, [:error]))
    end

    test "malformed_catalog_entries_are_dropped_not_raised" do
      steps =
        Flows.open_session(1,
          models: [
            "not-a-map",
            42,
            nil,
            %{"provider" => "openai", "id" => "gpt-5.1"},
            %{"provider" => "anthropic"},
            %{"id" => "orphan"},
            %{"provider" => "custom", "id" => "usable", "name" => "Usable"}
          ]
        )

      transcript =
        PiGolden.assert_golden(@area, "malformed_catalog_entries_are_dropped_not_raised", steps)

      assert %{
               "options" => [
                 %{"value" => "openai/gpt-5.1", "name" => "openai/gpt-5.1"},
                 %{"value" => "custom/usable", "name" => "custom/Usable"}
               ]
             } = transcript |> reply_options() |> option("model")
    end

    test "a_catalog_whose_models_is_not_a_list_yields_no_selector" do
      steps =
        Flows.open_session(1, models: %{"openai" => ["gpt-5.1"]}) ++
          [{:note, "a string payload is equally unusable and equally must not raise"}] ++
          Flows.open_session(2, models: "gpt-5.1")

      transcript =
        PiGolden.assert_golden(
          @area,
          "a_catalog_whose_models_is_not_a_list_yields_no_selector",
          steps
        )

      assert option(reply_options(transcript), "model") == nil
    end

    test "set_model_without_a_catalog_omits_the_model_selector" do
      steps = [
        Flows.set_model(1, "gpt-5.1"),
        Flows.set_model(2, "openai/gpt-5.1"),
        Flows.set_mode(3, "high")
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "set_model_without_a_catalog_omits_the_model_selector",
          steps
        )

      assert %{error: "Unknown modelId: gpt-5.1"} = Enum.at(transcript, 1).result

      assert ["thought_level", "auto_compaction", "auto_retry", "steering_mode", "follow_up_mode"] =
               transcript
               |> updates()
               |> List.first()
               |> Map.fetch!("configOptions")
               |> Enum.map(& &1["id"])
    end
  end

  describe "session/set_config_option" do
    test "model_option_routes_to_set_model" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_config(2, "model", "openai/gpt-5.1"),
            Flows.set_config(3, "model", "claude-sonnet-4"),
            Flows.set_config(4, "model", "nope"),
            Flows.set_config(5, "model", nil)
          ]

      transcript = PiGolden.assert_golden(@area, "model_option_routes_to_set_model", steps)

      assert [
               %{
                 tag: :messages_and_write,
                 writes: [%{"type" => "set_model", "provider" => "openai"}]
               },
               %{
                 tag: :messages_and_write,
                 writes: [%{"type" => "set_model", "provider" => "anthropic"}]
               },
               %{tag: :error, error: "Unknown modelId: nope"},
               %{tag: :error, error: "session/set_model requires modelId"}
             ] =
               transcript
               |> Enum.take(-4)
               |> Enum.map(&Map.take(&1.result, [:tag, :writes, :error]))
    end

    test "thought_level_option_routes_to_set_thinking_level" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_config(2, "thought_level", "xhigh"),
            Flows.set_config(3, "thought_level", "off"),
            Flows.set_config(4, "thought_level", "extreme"),
            Flows.set_config(5, "thought_level", 3)
          ]

      transcript =
        PiGolden.assert_golden(@area, "thought_level_option_routes_to_set_thinking_level", steps)

      assert [
               %{
                 tag: :messages_and_write,
                 writes: [%{"type" => "set_thinking_level", "level" => "xhigh"}]
               },
               %{
                 tag: :messages_and_write,
                 writes: [%{"type" => "set_thinking_level", "level" => "off"}]
               },
               %{tag: :error, error: "Unknown thinking level: extreme"},
               %{tag: :error, error: "Unknown thinking level: 3"}
             ] =
               transcript
               |> Enum.take(-4)
               |> Enum.map(&Map.take(&1.result, [:tag, :writes, :error]))

      assert ["xhigh", "off"] =
               transcript
               |> updates()
               |> Enum.filter(&(&1["sessionUpdate"] == "current_mode_update"))
               |> Enum.map(& &1["currentModeId"])
    end

    test "boolean_options_accept_booleans_and_true_false_strings" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_config(2, "auto_compaction", false),
            Flows.set_config(3, "auto_compaction", "true"),
            Flows.set_config(4, "auto_compaction", "false"),
            Flows.set_config(5, "auto_retry", true),
            Flows.set_config(6, "auto_retry", "false"),
            Flows.set_config(7, "auto_compaction", "yes"),
            Flows.set_config(8, "auto_retry", 1),
            Flows.set_config(9, "auto_retry", nil)
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "boolean_options_accept_booleans_and_true_false_strings",
          steps
        )

      assert [
               %{tag: :ok, writes: [%{"type" => "set_auto_compaction", "enabled" => false}]},
               %{tag: :ok, writes: [%{"type" => "set_auto_compaction", "enabled" => true}]},
               %{tag: :ok, writes: [%{"type" => "set_auto_compaction", "enabled" => false}]},
               %{tag: :ok, writes: [%{"type" => "set_auto_retry", "enabled" => true}]},
               %{tag: :ok, writes: [%{"type" => "set_auto_retry", "enabled" => false}]},
               %{tag: :error, error: "Unknown Pi config option: auto_compaction"},
               %{tag: :error, error: "Unknown Pi config option: auto_retry"},
               %{tag: :error, error: "Unknown Pi config option: auto_retry"}
             ] =
               transcript
               |> Enum.take(-8)
               |> Enum.map(&Map.take(&1.result, [:tag, :writes, :error]))

      assert Enum.all?(Enum.take(transcript, -8), &(not Map.has_key?(&1.result, :messages)))
    end

    test "unknown_option_ids_error" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_config(2, "nonexistent", "x"),
            Flows.set_config(3, nil, "x"),
            {:outbound, %{"method" => "session/set_config_option", "id" => 4, "params" => %{}}}
          ]

      transcript = PiGolden.assert_golden(@area, "unknown_option_ids_error", steps)

      assert [
               %{error: "Unknown Pi config option: nonexistent"},
               %{error: "Unknown Pi config option: "},
               %{error: "Unknown Pi config option: "}
             ] = transcript |> Enum.take(-3) |> Enum.map(&Map.take(&1.result, [:error]))
    end
  end

  describe "config option catalog" do
    test "session_config_options_follow_the_session_state" do
      models = [
        %{"provider" => "anthropic", "id" => "claude-sonnet-4", "name" => "Claude Sonnet 4"},
        %{"provider" => "openai", "id" => "gpt-5.1"}
      ]

      steps =
        Flows.open_session(1,
          models: models,
          state: %{
            "thinkingLevel" => "xhigh",
            "model" => %{"provider" => "openai", "id" => "gpt-5.1"}
          }
        ) ++
          [
            Flows.set_mode(2, "low"),
            Flows.set_model(3, "claude-sonnet-4")
          ]

      transcript =
        PiGolden.assert_golden(@area, "session_config_options_follow_the_session_state", steps)

      %{"result" => %{"configOptions" => options}} =
        transcript |> PiGolden.messages() |> Enum.find(&(&1["id"] == 1))

      assert [
               "model",
               "thought_level",
               "auto_compaction",
               "auto_retry",
               "steering_mode",
               "follow_up_mode"
             ] =
               Enum.map(options, & &1["id"])

      assert %{
               "currentValue" => "openai/gpt-5.1",
               "options" => [
                 %{"name" => "anthropic/Claude Sonnet 4"},
                 %{"name" => "openai/gpt-5.1"}
               ]
             } =
               Enum.find(options, &(&1["id"] == "model"))

      assert %{"currentValue" => "xhigh"} = Enum.find(options, &(&1["id"] == "thought_level"))

      assert [{"openai/gpt-5.1", "low"}, {"anthropic/claude-sonnet-4", "low"}] =
               transcript
               |> updates()
               |> Enum.filter(&(&1["sessionUpdate"] == "config_option_update"))
               |> Enum.map(
                 &{option(&1, "model")["currentValue"],
                  option(&1, "thought_level")["currentValue"]}
               )
    end

    test "config_updates_after_load_use_the_loaded_session" do
      steps = [
        Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
        Flows.session_resume(1, "mapped"),
        {:respond, "switch_session", %{}},
        {:respond, "get_state",
         Flows.state_data(%{"sessionId" => "mapped", "thinkingLevel" => "off"})},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}},
        Flows.set_config(2, "thought_level", "high", %{"sessionId" => "ignored-by-config-options"}),
        Flows.set_config(3, "auto_retry", false, %{"sessionId" => "ignored"})
      ]

      transcript =
        PiGolden.assert_golden(@area, "config_updates_after_load_use_the_loaded_session", steps)

      assert ["mapped", "mapped"] =
               transcript
               |> Enum.at(7)
               |> Map.fetch!(:result)
               |> Map.fetch!(:messages)
               |> Enum.map(& &1["params"]["sessionId"])
    end
  end

  describe "managed mode" do
    test "managed_set_config_replies_with_confirmation_without_model_catalog" do
      steps =
        [{:init, managed: true}] ++
          Flows.open_session(1) ++
          [
            Flows.set_config(2, "model", "openai/gpt-5.1"),
            Flows.set_config(3, "thought_level", "high"),
            Flows.set_config(4, "auto_compaction", false),
            Flows.set_mode(5, "low"),
            Flows.set_model(6, "claude-sonnet-4")
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "managed_set_config_replies_with_confirmation_without_model_catalog",
          steps
        )

      [model_reply, thought_reply, bool_reply, mode_reply, set_model_reply] =
        transcript |> Enum.take(-5) |> Enum.map(& &1.result)

      assert %{
               tag: :messages_and_reply,
               reply: %{"configOptions" => confirmation},
               port_writes: [%{"type" => "set_model"}]
             } =
               model_reply

      confirmed_model = Enum.find(confirmation, &(&1["id"] == "model"))
      assert confirmed_model["currentValue"] == "openai/gpt-5.1"
      refute Map.has_key?(confirmed_model, "options")
      assert Map.has_key?(Enum.find(confirmation, &(&1["id"] == "thought_level")), "options")

      assert %{
               tag: :messages_and_reply,
               reply: %{"configOptions" => _},
               port_writes: [%{"type" => "set_thinking_level"}]
             } =
               thought_reply

      assert %{tag: :reply, reply: %{}, port_writes: [%{"type" => "set_auto_compaction"}]} =
               bool_reply

      assert %{
               tag: :messages_and_reply,
               reply: %{},
               port_writes: [%{"type" => "set_thinking_level"}]
             } = mode_reply

      assert %{
               tag: :messages_and_reply,
               reply: %{"configOptions" => _},
               port_writes: [%{"type" => "set_model"}]
             } =
               set_model_reply
    end

    test "managed_updates_without_a_process_fail" do
      steps = [
        {:init, managed: true},
        Flows.set_mode(1, "high"),
        Flows.set_model(2, "openai/gpt-5.1"),
        Flows.set_config(3, "auto_retry", true),
        Flows.set_config(4, "thought_level", "low"),
        Flows.set_config(5, "steering_mode", "all"),
        Flows.set_config(6, "model", "nope")
      ]

      transcript = PiGolden.assert_golden(@area, "managed_updates_without_a_process_fail", steps)

      assert [
               %{error: ":no_active_pi_session"},
               %{error: ":no_active_pi_session"},
               %{error: ":no_active_pi_session"},
               %{error: ":no_active_pi_session"},
               %{error: ":no_active_pi_session"},
               %{error: "Unknown modelId: nope"}
             ] = transcript |> Enum.drop(1) |> Enum.map(&Map.take(&1.result, [:error]))
    end

    test "managed_updates_after_close_fail_until_a_new_session" do
      steps =
        [{:init, managed: true}] ++
          Flows.open_session(1) ++
          [
            {:outbound,
             %{"method" => "session/close", "id" => 2, "params" => %{"sessionId" => "pi-session"}}},
            Flows.set_mode(3, "high"),
            Flows.set_config(4, "auto_compaction", true)
          ] ++
          Flows.open_session(5) ++
          [Flows.set_config(6, "auto_compaction", true)]

      transcript =
        PiGolden.assert_golden(
          @area,
          "managed_updates_after_close_fail_until_a_new_session",
          steps
        )

      assert [
               %{tag: :reply, port: :closed},
               %{tag: :error, error: ":no_active_pi_session"},
               %{tag: :error, error: ":no_active_pi_session"}
             ] =
               transcript
               |> Enum.slice(6..8)
               |> Enum.map(&Map.take(&1.result, [:tag, :error, :port]))

      assert %{tag: :reply, reply: %{}, port_writes: [%{"type" => "set_auto_compaction"}]} =
               PiGolden.last_result(transcript)
    end
  end

  describe "session id targeting" do
    test "set_mode_and_set_model_target_the_request_session_id" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_mode(2, "high", %{"sessionId" => "other-session"}),
            Flows.set_model(3, "openai/gpt-5.1", %{"sessionId" => "other-session"}),
            Flows.set_mode(4, "low", %{"sessionId" => nil}),
            {:note, "Config options always use the adapter's own session id"},
            Flows.set_config(5, "thought_level", "off", %{"sessionId" => "other-session"})
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "set_mode_and_set_model_target_the_request_session_id",
          steps
        )

      assert [
               "other-session",
               "other-session",
               "other-session",
               "pi-session",
               "pi-session",
               "pi-session",
               "pi-session"
             ] =
               transcript
               |> PiGolden.messages()
               |> Enum.map(& &1["params"]["sessionId"])
               |> Enum.reject(&is_nil/1)
               |> Enum.drop(1)
    end
  end
end
