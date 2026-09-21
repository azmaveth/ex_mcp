defmodule ExMCP.ACP.Adapters.ClaudeSDK.CatalogGoldenTest do
  @moduledoc """
  Characterization gate for the Claude SDK adapter's catalogs
  (`docs/POST_1_0_MAINTENANCE_PLAN.md`, "Claude adapter characterization
  gate": model, mode, and config-option catalog normalization).

  Each test drives `ExMCP.ACP.Adapters.ClaudeSDK` through
  `ExMCP.Test.ClaudeGolden` and compares the recorded transcript against a
  committed fixture under `test/fixtures/acp/claude/catalog/`. The fixtures
  pin:

    * the static `modes/0` and `config_options/0` callbacks, and the
      dynamic `modes` / `configOptions` a `session/new` reply carries;
    * model options built from the SDK `initialize` response: the
      `displayName` / `display_name` / `name` / `id` / `value` name and
      value precedence, descriptions, and the built-in Default/Sonnet/Opus
      list used when Claude reported no models;
    * effort options from `supportedEffortLevels` (and its snake-case
      spelling), the humanized labels, and the built-in level list;
    * the fast-mode option: present only for a model that supports it,
      rendered as a boolean for a client with the
      `session.configOptions.boolean` capability and as an on/off select
      otherwise, and tracking `fast_mode_state` from the initialize
      response, a `system` init event and a `result`;
    * the agent option: only when the initialize response named agents,
      with the upstream built-in personas filtered out, accepting both
      `agents` and `supportedAgents`, string entries, and dropping entries
      that are neither;
    * mode gating: `auto` only for a model that advertises it,
      `bypassPermissions` only behind the dangerous opt-in or an inherited
      bypass mode, and `currentModeId` falling back to `default` when the
      configured mode is outside the catalog;
    * `session/set_config_option` for `model` (including the extra
      `set_permission_mode` clamp when the effective mode fell back to
      `default`), `effort` (with `"default"` meaning no level), `fast`
      (boolean, `"on"`, `"off"`, and the error for anything else), `agent`
      (with `"default"` meaning no SDK agent), and an unknown option id;
    * the command catalog normalization applied to the initialize response
      (bare strings, `name` maps, `id` maps, and anything else).

  The `available_commands_update` / `current_mode_update` ordering itself
  belongs to the session_updates area, and mode *permissions* to the
  permissions area.

  Mutation check (2026-09-21): in `claude_sdk/mapper.ex`, letting
  `normalize_agents/1` keep the upstream built-in personas (dropping the
  `Enum.reject/2` name list) fails
  `agent_option_filters_the_built_in_personas`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CLAUDE_GOLDEN=update mix test <this file>[:line]`; that run rewrites
  the fixture and fails on purpose, so review the diff and re-run without
  the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.ClaudeGolden
  alias ExMCP.Test.ClaudeGolden.Flows

  @area "catalog"
  @boolean_caps %{"session" => %{"configOptions" => %{"boolean" => true}}}

  describe "static catalogs" do
    test "static_modes_and_config_options" do
      transcript =
        ClaudeGolden.assert_golden(@area, "static_modes_and_config_options", [
          :modes,
          :config_options
        ])

      assert %{reply: options} = ClaudeGolden.last_result(transcript)
      assert Enum.map(options, & &1["id"]) == ["mode", "model", "effort"]
    end

    test "session_new_carries_the_default_catalogs" do
      ClaudeGolden.assert_golden(@area, "session_new_carries_the_default_catalogs", [
        Flows.session_new()
      ])
    end
  end

  describe "model options" do
    test "model_options_prefer_display_name" do
      transcript =
        catalog("model_options_prefer_display_name", %{
          "models" => [
            %{"value" => "sonnet", "displayName" => "Sonnet 4", "description" => "Balanced"},
            %{"id" => "opus", "display_name" => "Opus 4"},
            %{"name" => "haiku"},
            %{"id" => "only-id"},
            %{"value" => "only-value"}
          ]
        })

      assert ["Sonnet 4", "Opus 4", "haiku", "only-id", "only-value"] =
               model_option(transcript)["options"] |> Enum.map(& &1["name"])
    end

    test "model_options_fall_back_to_the_built_in_list" do
      transcript = catalog("model_options_fall_back_to_the_built_in_list", %{"models" => []})

      assert ["default", "sonnet", "opus"] =
               model_option(transcript)["options"] |> Enum.map(& &1["value"])
    end

    test "the_current_model_is_the_adapter_option" do
      steps =
        [{:init, model: "opus"}] ++
          [
            Flows.initialize(),
            :post_connect,
            {:respond_control, "initialize",
             %{"models" => [%{"value" => "sonnet"}, %{"value" => "opus"}]}},
            Flows.session_new()
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "the_current_model_is_the_adapter_option", steps)

      assert model_option(transcript)["currentValue"] == "opus"
    end
  end

  describe "effort options" do
    test "effort_options_come_from_the_model" do
      transcript =
        catalog(
          "effort_options_come_from_the_model",
          %{
            "models" => [
              %{
                "value" => "sonnet",
                "supportedEffortLevels" => ["low", "medium", "very-high", "ultra_max"]
              }
            ]
          },
          model: "sonnet"
        )

      assert ["Default", "Low", "Medium", "Very High", "Ultra Max"] =
               option(transcript, "effort")["options"] |> Enum.map(& &1["name"])
    end

    test "effort_options_accept_the_snake_case_spelling" do
      catalog(
        "effort_options_accept_the_snake_case_spelling",
        %{"models" => [%{"value" => "sonnet", "supported_effort_levels" => ["low", "high"]}]},
        model: "sonnet"
      )
    end

    test "effort_options_fall_back_to_the_built_in_levels" do
      transcript =
        catalog("effort_options_fall_back_to_the_built_in_levels", %{"models" => []})

      assert ["default", "low", "medium", "high"] =
               option(transcript, "effort")["options"] |> Enum.map(& &1["value"])
    end

    test "a_non_string_effort_level_is_stringified" do
      catalog(
        "a_non_string_effort_level_is_stringified",
        %{"models" => [%{"value" => "sonnet", "supportedEffortLevels" => [3, "high"]}]},
        model: "sonnet"
      )
    end
  end

  describe "fast mode option" do
    test "fast_mode_is_absent_for_a_model_without_support" do
      transcript =
        catalog("fast_mode_is_absent_for_a_model_without_support", %{
          "models" => [%{"value" => "sonnet"}]
        })

      assert option(transcript, "fast") == nil
    end

    test "fast_mode_is_a_select_without_the_boolean_capability" do
      transcript =
        catalog(
          "fast_mode_is_a_select_without_the_boolean_capability",
          %{"models" => [%{"value" => "sonnet", "supportsFastMode" => true}]},
          model: "sonnet"
        )

      assert %{"type" => "select", "currentValue" => "off"} = option(transcript, "fast")
    end

    test "fast_mode_is_a_boolean_with_the_capability" do
      transcript =
        catalog(
          "fast_mode_is_a_boolean_with_the_capability",
          %{"models" => [%{"value" => "sonnet", "supports_fast_mode" => true}]},
          model: "sonnet",
          capabilities: @boolean_caps
        )

      assert %{"type" => "boolean", "currentValue" => false} = option(transcript, "fast")
    end

    test "fast_mode_state_is_read_from_the_initialize_response" do
      transcript =
        catalog(
          "fast_mode_state_is_read_from_the_initialize_response",
          %{
            "models" => [%{"value" => "sonnet", "supportsFastMode" => true}],
            "fast_mode_state" => "on"
          },
          model: "sonnet",
          capabilities: @boolean_caps
        )

      assert %{"currentValue" => true} = option(transcript, "fast")
    end

    test "fast_mode_state_is_read_from_a_system_init_event" do
      steps =
        catalog_steps(
          %{
            "models" => [%{"value" => "sonnet", "supportsFastMode" => true}]
          },
          model: "sonnet",
          capabilities: @boolean_caps
        ) ++ [Flows.system_init(%{"fast_mode_state" => "cooldown"})]

      ClaudeGolden.assert_golden(
        @area,
        "fast_mode_state_is_read_from_a_system_init_event",
        steps
      )
    end
  end

  describe "agent option" do
    test "agent_option_is_absent_without_agents" do
      transcript = catalog("agent_option_is_absent_without_agents", %{})

      assert option(transcript, "agent") == nil
    end

    test "agent_option_filters_the_built_in_personas" do
      transcript =
        catalog("agent_option_filters_the_built_in_personas", %{
          "agents" => [
            %{"name" => "claude"},
            %{"name" => "general-purpose"},
            %{"name" => "Explore"},
            %{"name" => "Plan"},
            %{"name" => "statusline-setup"},
            %{"name" => "default"},
            %{"name" => "reviewer", "description" => "Reviews code"}
          ]
        })

      assert ["default", "reviewer"] =
               option(transcript, "agent")["options"] |> Enum.map(& &1["value"])
    end

    test "agent_option_accepts_supported_agents_and_strings" do
      transcript =
        catalog("agent_option_accepts_supported_agents_and_strings", %{
          "supportedAgents" => ["reviewer", %{"name" => "writer"}, 42, %{"no" => "name"}]
        })

      assert ["default", "reviewer", "writer"] =
               option(transcript, "agent")["options"] |> Enum.map(& &1["value"])
    end

    test "the_current_agent_is_the_adapter_option" do
      transcript =
        catalog(
          "the_current_agent_is_the_adapter_option",
          %{"agents" => [%{"name" => "reviewer"}]},
          agent: "reviewer"
        )

      assert option(transcript, "agent")["currentValue"] == "reviewer"
    end
  end

  describe "mode gating" do
    test "auto_mode_needs_model_support" do
      transcript =
        catalog(
          "auto_mode_needs_model_support",
          %{"models" => [%{"value" => "sonnet", "supportsAutoMode" => true}]},
          model: "sonnet"
        )

      assert ["default", "acceptEdits", "plan", "auto"] = mode_ids(transcript)
    end

    test "auto_mode_accepts_the_snake_case_spelling" do
      catalog(
        "auto_mode_accepts_the_snake_case_spelling",
        %{"models" => [%{"value" => "sonnet", "supports_auto_mode" => true}]},
        model: "sonnet"
      )
    end

    test "auto_and_bypass_can_both_be_offered" do
      transcript =
        catalog(
          "auto_and_bypass_can_both_be_offered",
          %{"models" => [%{"value" => "sonnet", "supportsAutoMode" => true}]},
          model: "sonnet",
          init: [allow_dangerously_skip_permissions: true]
        )

      assert ["default", "acceptEdits", "plan", "auto", "bypassPermissions"] =
               mode_ids(transcript)
    end

    test "a_mode_outside_the_catalog_falls_back_to_default" do
      steps = [{:init, permission_mode: :auto}, Flows.session_new()]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "a_mode_outside_the_catalog_falls_back_to_default",
          steps
        )

      assert %{reply: %{"modes" => %{"currentModeId" => "default"}}} =
               ClaudeGolden.last_result(transcript)
    end
  end

  describe "set_config_option" do
    test "setting_the_model_writes_a_set_model_control" do
      steps =
        catalog_steps(%{"models" => [%{"value" => "sonnet"}, %{"value" => "opus"}]}) ++
          [Flows.set_config("acp-config", "model", "opus")]

      transcript =
        ClaudeGolden.assert_golden(@area, "setting_the_model_writes_a_set_model_control", steps)

      assert %{tag: :reply_and_write, writes: [%{"request" => request}]} =
               ClaudeGolden.last_result(transcript)

      assert request == %{"subtype" => "set_model", "model" => "opus"}
    end

    test "setting_the_model_clamps_a_mode_outside_the_catalog" do
      steps = [
        {:note, "auto is not in this catalog, so the effective mode already fell back"},
        {:init, permission_mode: :auto},
        Flows.session_new(),
        Flows.set_config("acp-config", "model", "opus")
      ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "setting_the_model_clamps_a_mode_outside_the_catalog",
          steps
        )

      assert %{writes: [%{"request" => %{"subtype" => "set_model"}}, %{"request" => clamp}]} =
               ClaudeGolden.last_result(transcript)

      assert clamp == %{"subtype" => "set_permission_mode", "mode" => "default"}
    end

    test "setting_the_effort_writes_a_flag_setting" do
      steps = [
        Flows.session_new(),
        Flows.set_config("acp-config-1", "effort", "high"),
        {:note, "The default level clears the SDK flag"},
        Flows.set_config("acp-config-2", "effort", "default")
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "setting_the_effort_writes_a_flag_setting", steps)

      assert %{writes: [%{"request" => %{"settings" => %{"effortLevel" => nil}}}]} =
               ClaudeGolden.last_result(transcript)
    end

    test "setting_fast_mode_accepts_booleans_and_on_off" do
      steps = [
        Flows.session_new(),
        Flows.set_config("acp-config-1", "fast", true),
        Flows.set_config("acp-config-2", "fast", "on"),
        Flows.set_config("acp-config-3", "fast", "off"),
        Flows.set_config("acp-config-4", "fast", false)
      ]

      ClaudeGolden.assert_golden(@area, "setting_fast_mode_accepts_booleans_and_on_off", steps)
    end

    test "setting_fast_mode_to_anything_else_errors" do
      steps = [Flows.session_new(), Flows.set_config("acp-config", "fast", "maybe")]

      transcript =
        ClaudeGolden.assert_golden(@area, "setting_fast_mode_to_anything_else_errors", steps)

      assert %{tag: :error, error: ~s(Invalid fast mode value: "maybe")} =
               ClaudeGolden.last_result(transcript)
    end

    test "setting_the_agent_writes_a_flag_setting" do
      steps =
        catalog_steps(%{"agents" => [%{"name" => "reviewer"}]}) ++
          [
            Flows.set_config("acp-config-1", "agent", "reviewer"),
            {:note, "The default persona clears the SDK agent"},
            Flows.set_config("acp-config-2", "agent", "default")
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "setting_the_agent_writes_a_flag_setting", steps)

      assert %{writes: [%{"request" => %{"settings" => %{"agent" => nil}}}]} =
               ClaudeGolden.last_result(transcript)
    end

    test "an_unknown_config_option_replies_with_the_catalog" do
      steps = [Flows.session_new(), Flows.set_config("acp-config", "temperature", 0.2)]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "an_unknown_config_option_replies_with_the_catalog",
          steps
        )

      assert %{tag: :reply, reply: %{"configOptions" => _}} = ClaudeGolden.last_result(transcript)
    end
  end

  describe "command catalog" do
    test "initialize_commands_are_normalized" do
      steps = [
        {:note, "The catalog only reaches the client once a session is open"},
        Flows.initialize(),
        :post_connect,
        Flows.session_new(),
        {:respond_control, "initialize",
         %{
           "commands" => [
             "review",
             %{"name" => "plan", "description" => "Plan it"},
             %{"id" => "ship"},
             42
           ]
         }}
      ]

      transcript = ClaudeGolden.assert_golden(@area, "initialize_commands_are_normalized", steps)

      assert [%{"params" => %{"update" => %{"availableCommands" => commands}}} | _] =
               ClaudeGolden.updates(transcript)

      assert Enum.map(commands, & &1["name"]) == ["review", "plan", "ship", "42"]
    end
  end

  # -- helpers ---------------------------------------------------------------

  defp catalog(name, init_response, opts \\ []) do
    ClaudeGolden.assert_golden(@area, name, catalog_steps(init_response, opts))
  end

  # A session opened after Claude answered the SDK `initialize` control
  # request with `init_response`, so the reply carries the dynamic catalogs.
  defp catalog_steps(init_response, opts \\ []) do
    init =
      Keyword.get(opts, :init, [])
      |> then(fn init ->
        case Keyword.get(opts, :model) do
          nil -> init
          model -> Keyword.put(init, :model, model)
        end
      end)
      |> then(fn init ->
        case Keyword.get(opts, :agent) do
          nil -> init
          agent -> Keyword.put(init, :agent, agent)
        end
      end)

    [
      {:init, init},
      Flows.initialize("acp-init", Keyword.get(opts, :capabilities, %{})),
      :post_connect,
      {:respond_control, "initialize", init_response},
      Flows.session_new()
    ]
  end

  defp config_options(transcript) do
    %{reply: %{"configOptions" => options}} = ClaudeGolden.last_result(transcript)
    options
  end

  defp option(transcript, id) do
    transcript |> config_options() |> Enum.find(&(&1["id"] == id))
  end

  defp model_option(transcript), do: option(transcript, "model")

  defp mode_ids(transcript) do
    %{reply: %{"modes" => %{"availableModes" => modes}}} = ClaudeGolden.last_result(transcript)
    Enum.map(modes, & &1["id"])
  end
end
