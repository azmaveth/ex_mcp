defmodule ExMCP.ACP.Adapters.Codex.CatalogGoldenTest do
  @moduledoc """
  Characterization gate for the Codex ACP adapter's model and mode catalog
  behavior (area A7 of `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Codex adapter
  restructuring" / "Characterization gate").

  It pins how `ExMCP.ACP.Adapters.Codex` normalizes `model/list` replies (v2
  and legacy shapes, hidden models, malformed entries, empty and error
  replies), how the catalog surfaces in `session/new` results (`models`,
  `configOptions`, `modes` with `_meta.kind` and `currentModeId`), how
  `session/set_model`, `session/set_config_option` and `session/set_mode`
  resolve ids, efforts and modes and seed later `session/new` calls, how
  `thread/settings/updated`, `model/rerouted` and `thread/started`
  notifications and `thread/start` results feed the session's model, effort
  and mode, and the model/effort/mode defaults taken from init options.

  A recurring theme is which selections are session-scoped and which are
  adapter-wide (seeding later `session/new` calls): `session/set_model`,
  `session/set_config_option` and `session/set_mode` are adapter-wide, while a
  `session/new` `modeId`/`approvalPolicy` or `model` param, a `thread/start`
  result and a `thread/settings/updated` notification only affect their own
  session. A selection that carries no effort (a catalog model without
  supported efforts) leaves the adapter-wide effort in force.

  Each test drives the adapter through `ExMCP.Test.CodexGolden` and compares
  the recorded transcript with a fixture under
  `test/fixtures/acp/codex/catalog/`. To regenerate after an intentional
  behavior change run

      CODEX_GOLDEN=update mix test test/ex_mcp/acp/adapters/codex/characterization/catalog_golden_test.exs

  which rewrites the fixtures and fails on purpose; review the diff and re-run
  without the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.CodexGolden

  @area "catalog"
  @session_id "thread-abc"

  # -- model/list normalization ---------------------------------------------

  test "model_list_v2_shape_expands_supported_efforts" do
    steps = open_session_steps()

    transcript =
      CodexGolden.assert_golden(@area, "model_list_v2_shape_expands_supported_efforts", steps)

    result = session_result(transcript)

    assert model_ids(result) == [
             "codex-mini/medium",
             "codex-mini/high",
             "gpt-5/low",
             "gpt-5/medium",
             "gpt-5/high"
           ]

    assert result["models"]["currentModelId"] == "gpt-5/medium"
    assert Enum.map(result["configOptions"], & &1["id"]) == ["mode", "model", "reasoning_effort"]
  end

  test "model_list_legacy_shape_normalizes_snake_case_fields" do
    catalog = [
      %{
        "id" => "o3",
        "model" => "o3-2025",
        "display_name" => "O3",
        "description" => "Legacy catalog entry",
        "default_reasoning_effort" => "high",
        "supported_reasoning_efforts" => [
          %{"value" => "medium", "name" => "Medium", "description" => "Balanced"},
          %{"effort" => "high"},
          "extra_high",
          %{"description" => "no value key"},
          nil
        ]
      }
    ]

    steps =
      open_session_steps(
        note:
          "Legacy model/list shape: snake_case keys, efforts keyed by value/effort or bare strings; entries without a value are dropped and names are humanized. The model option is keyed by catalog id but its currentValue is the wire model, so the wire model is also prepended as an extra entry",
        catalog: catalog,
        thread_result: %{"model" => "o3-2025"}
      )

    transcript =
      CodexGolden.assert_golden(
        @area,
        "model_list_legacy_shape_normalizes_snake_case_fields",
        steps
      )

    result = session_result(transcript)
    assert model_ids(result) == ["o3/medium", "o3/high", "o3/extra_high"]

    assert Enum.map(option(result, "reasoning_effort")["options"], & &1["name"]) ==
             ["Medium", "High", "Extra High"]

    assert option(result, "model")["options"] == [
             %{"value" => "o3-2025", "name" => "o3-2025"},
             %{"value" => "o3", "name" => "O3", "description" => "Legacy catalog entry"}
           ]
  end

  test "model_list_mixed_shape_camel_case_wins" do
    mixed_entry = %{
      "id" => "codex-mini",
      "model" => "gpt-5-codex",
      "displayName" => "Codex Mini",
      "display_name" => "Legacy Codex Mini",
      "description" => "Fast coding model",
      "defaultReasoningEffort" => "high",
      "default_reasoning_effort" => "low",
      "supportedReasoningEfforts" => [
        %{"reasoningEffort" => "medium", "effort" => "low", "value" => "minimal"},
        %{"effort" => "high", "value" => "minimal"},
        %{"value" => "low"}
      ],
      "supported_reasoning_efforts" => [%{"value" => "minimal"}]
    }

    steps =
      open_session_steps(
        note:
          "One entry carries both v2 camelCase and legacy snake_case keys: displayName, defaultReasoningEffort and supportedReasoningEfforts win, and inside an effort reasoningEffort wins over effort, which wins over value. The configured effort minimal is not offered, so the camelCase default high selects the model id",
        init: [reasoning_effort: "minimal"],
        catalog: [mixed_entry, List.last(v2_catalog())],
        thread_result: %{"model" => "gpt-5-codex"}
      )

    transcript =
      CodexGolden.assert_golden(@area, "model_list_mixed_shape_camel_case_wins", steps)

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "codex-mini/high"

    assert Enum.take(result["models"]["availableModels"], 3) == [
             %{
               "modelId" => "codex-mini/medium",
               "name" => "Codex Mini (medium)",
               "description" => "Fast coding model"
             },
             %{
               "modelId" => "codex-mini/high",
               "name" => "Codex Mini (high)",
               "description" => "Fast coding model"
             },
             %{
               "modelId" => "codex-mini/low",
               "name" => "Codex Mini (low)",
               "description" => "Fast coding model"
             }
           ]

    assert %{"name" => "Codex Mini"} =
             Enum.find(option(result, "model")["options"], &(&1["value"] == "codex-mini"))
  end

  test "model_list_hidden_model_is_omitted" do
    steps = open_session_steps(catalog: v2_catalog() ++ [hidden_model()])

    transcript = CodexGolden.assert_golden(@area, "model_list_hidden_model_is_omitted", steps)
    result = session_result(transcript)

    refute Enum.any?(model_ids(result), &String.starts_with?(&1, "gpt-5-internal"))
    assert Enum.map(option(result, "model")["options"], & &1["value"]) == ["codex-mini", "gpt-5"]
  end

  test "model_list_hidden_model_listed_when_current" do
    steps =
      open_session_steps(
        note: "thread/start selects the hidden model, so it is listed alongside visible ones",
        catalog: v2_catalog() ++ [hidden_model()],
        thread_result: %{"model" => "gpt-5-internal-preview"}
      )

    transcript =
      CodexGolden.assert_golden(@area, "model_list_hidden_model_listed_when_current", steps)

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "gpt-5-internal/medium"
    assert "gpt-5-internal/medium" in model_ids(result)

    assert Enum.map(option(result, "model")["options"], & &1["value"]) ==
             ["gpt-5-internal-preview", "codex-mini", "gpt-5", "gpt-5-internal"]
  end

  test "model_list_hidden_model_listed_when_current_by_catalog_id" do
    steps =
      open_session_steps(
        note:
          "thread/start reports the hidden entry's catalog id rather than its wire model; the id match alone keeps it listed and no raw entry is prepended",
        catalog: v2_catalog() ++ [hidden_model()],
        thread_result: %{"model" => "gpt-5-internal"}
      )

    transcript =
      CodexGolden.assert_golden(
        @area,
        "model_list_hidden_model_listed_when_current_by_catalog_id",
        steps
      )

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "gpt-5-internal/medium"

    assert List.last(result["models"]["availableModels"])["name"] == "GPT-5 Internal (medium)"

    assert Enum.map(option(result, "model")["options"], & &1["value"]) ==
             ["codex-mini", "gpt-5", "gpt-5-internal"]
  end

  test "model_list_entries_without_ids_are_dropped" do
    catalog = [
      %{"model" => "gpt-5-no-id", "displayName" => "No id"},
      %{"id" => "", "model" => "gpt-5-blank-id"},
      "gpt-5-string-entry",
      %{"id" => "o3"}
    ]

    steps =
      open_session_steps(
        note:
          "Only entries with a non-empty binary id survive; a bare id gets model/displayName defaults and no efforts",
        catalog: catalog,
        thread_result: %{"model" => "o3"}
      )

    transcript =
      CodexGolden.assert_golden(@area, "model_list_entries_without_ids_are_dropped", steps)

    result = session_result(transcript)

    assert result["models"] == %{
             "currentModelId" => "o3",
             "availableModels" => [%{"modelId" => "o3", "name" => "o3"}]
           }

    assert Enum.map(option(result, "reasoning_effort")["options"], & &1["value"]) ==
             ["minimal", "low", "medium", "high"]
  end

  test "model_list_empty_catalog_uses_thread_model" do
    steps = open_session_steps(catalog: [])

    transcript =
      CodexGolden.assert_golden(@area, "model_list_empty_catalog_uses_thread_model", steps)

    result = session_result(transcript)

    assert result["models"] == %{
             "currentModelId" => "gpt-5",
             "availableModels" => [%{"modelId" => "gpt-5", "name" => "gpt-5"}]
           }

    assert option(result, "model")["options"] == [%{"value" => "gpt-5", "name" => "gpt-5"}]
  end

  test "thread_start_blank_model_omits_model_option" do
    steps =
      open_session_steps(
        note:
          "An empty-string model in the thread/start result with an empty catalog omits the model config option, while models still reports the blank id as current and as an ensured entry; turn/start carries no model",
        catalog: [],
        thread_result: %{"model" => ""}
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "thread_start_blank_model_omits_model_option", steps)

    result = session_result(transcript)
    assert Enum.map(result["configOptions"], & &1["id"]) == ["mode", "reasoning_effort"]

    assert result["models"] == %{
             "currentModelId" => "",
             "availableModels" => [%{"modelId" => "", "name" => ""}]
           }

    refute Map.has_key?(outbound_write(transcript, 20)["params"], "model")
  end

  test "model_list_non_list_data_is_ignored" do
    steps = open_session_steps(catalog_reply: %{"data" => %{"models" => v2_catalog()}})

    transcript = CodexGolden.assert_golden(@area, "model_list_non_list_data_is_ignored", steps)
    result = session_result(transcript)

    assert model_ids(result) == ["gpt-5"]
  end

  test "model_list_error_reply_leaves_catalog_empty" do
    steps =
      [{:init, init_opts()}, :post_connect, initialize_reply()] ++
        [
          {:inbound,
           %{
             "id" => 2,
             "error" => %{"code" => -32_601, "message" => "model/list is not supported"}
           }},
          {:note, "thread/start result carries no model either, so no model is known at all"},
          session_new(),
          thread_start_reply(Map.delete(thread_result(), "model"))
        ]

    transcript =
      CodexGolden.assert_golden(@area, "model_list_error_reply_leaves_catalog_empty", steps)

    result = session_result(transcript)
    assert result["models"] == %{"availableModels" => []}
    assert Enum.map(result["configOptions"], & &1["id"]) == ["mode", "reasoning_effort"]
  end

  test "model_list_next_cursor_is_not_followed" do
    [codex_mini | _] = v2_catalog()

    steps =
      [{:init, init_opts()}, :post_connect, initialize_reply()] ++
        [
          {:inbound,
           %{"id" => 2, "result" => %{"data" => [codex_mini], "nextCursor" => "page-2"}}},
          {:note,
           "The adapter never requests the next page: the catalog is the first page only, so the thread model is ensured as a raw entry"},
          session_new(),
          thread_start_reply(thread_result())
        ]

    transcript = CodexGolden.assert_golden(@area, "model_list_next_cursor_is_not_followed", steps)

    assert Enum.count(CodexGolden.writes(transcript), &(&1["method"] == "model/list")) == 1

    assert model_ids(session_result(transcript)) == [
             "gpt-5",
             "codex-mini/medium",
             "codex-mini/high"
           ]
  end

  test "model_list_effort_descriptions_join_with_model_description" do
    catalog = [
      %{
        "id" => "o3",
        "displayName" => "O3",
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "low"},
          %{"reasoningEffort" => "high", "description" => "Deep"}
        ]
      },
      %{
        "id" => "o4",
        "displayName" => "O4",
        "description" => "Reasoning model",
        "supportedReasoningEfforts" => [%{"reasoningEffort" => "medium"}]
      }
    ]

    steps =
      open_session_steps(
        note:
          "availableModels descriptions join model and effort descriptions with a space, so a model without either yields an empty string rather than omitting the key, while select options drop it",
        catalog: catalog,
        thread_result: %{"model" => "o3"}
      )

    transcript =
      CodexGolden.assert_golden(
        @area,
        "model_list_effort_descriptions_join_with_model_description",
        steps
      )

    result = session_result(transcript)

    assert result["models"]["availableModels"] == [
             %{"modelId" => "o3/low", "name" => "O3 (low)", "description" => ""},
             %{"modelId" => "o3/high", "name" => "O3 (high)", "description" => "Deep"},
             %{
               "modelId" => "o4/medium",
               "name" => "O4 (medium)",
               "description" => "Reasoning model"
             }
           ]

    assert result["models"]["currentModelId"] == "o3/low"

    assert option(result, "reasoning_effort")["options"] == [
             %{"value" => "low", "name" => "Low"},
             %{"value" => "high", "name" => "High", "description" => "Deep"}
           ]
  end

  test "model_list_model_without_efforts_uses_display_name" do
    catalog = [
      %{
        "id" => "o3",
        "model" => "o3",
        "displayName" => "O3 Pro",
        "description" => "Pro",
        "hidden" => false
      },
      List.last(v2_catalog())
    ]

    steps =
      open_session_steps(
        note:
          "A non-reasoning entry (no supportedReasoningEfforts) is listed once by its bare id and named by its displayName, in availableModels as well as the model select option; the effort option falls back to the static list",
        catalog: catalog,
        thread_result: %{"model" => "o3"}
      )

    transcript =
      CodexGolden.assert_golden(
        @area,
        "model_list_model_without_efforts_uses_display_name",
        steps
      )

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "o3"

    assert hd(result["models"]["availableModels"]) ==
             %{"modelId" => "o3", "name" => "O3 Pro", "description" => "Pro"}

    assert hd(option(result, "model")["options"]) ==
             %{"value" => "o3", "name" => "O3 Pro", "description" => "Pro"}

    assert Enum.map(option(result, "reasoning_effort")["options"], & &1["value"]) ==
             ["minimal", "low", "medium", "high"]
  end

  test "model_list_effort_name_is_preserved" do
    catalog = [
      %{
        "id" => "o3",
        "model" => "o3",
        "displayName" => "O3",
        "defaultReasoningEffort" => "high",
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "high", "name" => "Deep Think", "description" => "Slow"},
          %{"reasoningEffort" => "xhigh", "name" => "Extra High"},
          "minimal"
        ]
      }
    ]

    steps =
      open_session_steps(
        note:
          "An effort's explicit name is kept even when it differs from the humanized value (Deep Think and Extra High rather than High and Xhigh); a bare string is humanized. availableModels names still use the raw value",
        catalog: catalog,
        thread_result: %{"model" => "o3"}
      )

    transcript = CodexGolden.assert_golden(@area, "model_list_effort_name_is_preserved", steps)
    result = session_result(transcript)

    assert option(result, "reasoning_effort")["options"] == [
             %{"value" => "high", "name" => "Deep Think", "description" => "Slow"},
             %{"value" => "xhigh", "name" => "Extra High"},
             %{"value" => "minimal", "name" => "Minimal"}
           ]

    assert Enum.map(result["models"]["availableModels"], & &1["name"]) ==
             ["O3 (high)", "O3 (xhigh)", "O3 (minimal)"]

    assert result["models"]["currentModelId"] == "o3/high"
  end

  test "model_list_null_camel_efforts_do_not_fall_back_to_snake_case" do
    catalog = [
      %{
        "id" => "o3",
        "model" => "o3",
        "displayName" => "O3",
        "supportedReasoningEfforts" => nil,
        "supported_reasoning_efforts" => [%{"value" => "high"}]
      },
      List.last(v2_catalog())
    ]

    steps =
      open_session_steps(
        note:
          "An explicit null supportedReasoningEfforts is taken as-is: the legacy supported_reasoning_efforts list is consulted only when the camelCase key is absent, so o3 ends up with no efforts",
        catalog: catalog,
        thread_result: %{"model" => "o3"}
      )

    transcript =
      CodexGolden.assert_golden(
        @area,
        "model_list_null_camel_efforts_do_not_fall_back_to_snake_case",
        steps
      )

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "o3"
    assert hd(result["models"]["availableModels"]) == %{"modelId" => "o3", "name" => "O3"}

    assert Enum.map(option(result, "reasoning_effort")["options"], & &1["value"]) ==
             ["minimal", "low", "medium", "high"]
  end

  # -- session/new surfacing -------------------------------------------------

  test "session_new_model_outside_catalog_is_ensured" do
    steps = open_session_steps(thread_result: %{"model" => "o3-custom"})

    transcript =
      CodexGolden.assert_golden(@area, "session_new_model_outside_catalog_is_ensured", steps)

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "o3-custom"

    assert hd(result["models"]["availableModels"]) ==
             %{"modelId" => "o3-custom", "name" => "o3-custom"}

    assert hd(option(result, "model")["options"]) ==
             %{"value" => "o3-custom", "name" => "o3-custom"}
  end

  test "session_new_without_any_model_lists_catalog_with_null_current" do
    steps =
      [{:init, init_opts()}, :post_connect, initialize_reply(), catalog_reply()] ++
        [
          session_new(),
          {:note,
           "No init model and no model in the thread/start result: the catalog is listed with no currentModelId, the model option is present with a null currentValue and no ensured entry, and turn/start carries no model"},
          thread_start_reply(Map.delete(thread_result(), "model")),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_new_without_any_model_lists_catalog_with_null_current",
        steps
      )

    result = session_result(transcript)
    refute Map.has_key?(result["models"], "currentModelId")
    assert length(result["models"]["availableModels"]) == 5

    assert %{
             "currentValue" => nil,
             "options" => [%{"value" => "codex-mini"}, %{"value" => "gpt-5"}]
           } =
             option(result, "model")

    refute Map.has_key?(last_write(transcript)["params"], "model")
  end

  test "session_new_effort_unsupported_by_model_uses_model_default" do
    steps =
      open_session_steps(
        note:
          "Configured effort minimal is not offered by gpt-5 (low/medium/high, default medium): the model id falls back to the model default while the option keeps minimal",
        init: [reasoning_effort: "minimal"]
      )

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_new_effort_unsupported_by_model_uses_model_default",
        steps
      )

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "gpt-5/medium"
    assert option(result, "reasoning_effort")["currentValue"] == "minimal"
  end

  test "session_new_fast_tier_model_adds_fast_mode_option" do
    steps =
      open_session_steps(
        catalog: [fast_model() | v2_catalog()],
        thread_result: %{"model" => "gpt-5-fast", "serviceTier" => "fast"}
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_fast_tier_model_adds_fast_mode_option", steps)

    result = session_result(transcript)
    assert option(result, "fast-mode")["currentValue"] == "on"

    assert %{
             "method" => "turn/start",
             "params" => %{"serviceTier" => "fast", "effort" => "medium"}
           } = last_write(transcript)
  end

  test "session_new_unknown_mode_id_omits_sandbox_params" do
    steps =
      open_session_steps(
        note: "session/new does not validate modeId; an unknown id is echoed back unchanged",
        session_params: %{"modeId" => "yolo"}
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_unknown_mode_id_omits_sandbox_params", steps)

    [_init, _initialized, _model_list, thread_start, turn_start] = CodexGolden.writes(transcript)
    refute Map.has_key?(thread_start["params"], "sandbox")
    refute Map.has_key?(turn_start["params"], "sandboxPolicy")
    assert session_result(transcript)["modes"]["currentModeId"] == "yolo"
  end

  test "session_new_approval_policy_alias_selects_mode" do
    steps = open_session_steps(session_params: %{"approvalPolicy" => "agent-full-access"})

    transcript =
      CodexGolden.assert_golden(@area, "session_new_approval_policy_alias_selects_mode", steps)

    assert %{"params" => %{"sandbox" => "danger-full-access", "approvalPolicy" => "never"}} =
             Enum.find(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

    assert session_result(transcript)["modes"]["currentModeId"] == "agent-full-access"
  end

  test "session_new_mode_id_wins_over_approval_policy_and_model_overrides_init" do
    steps =
      open_session_steps(
        init: [model: "gpt-5"],
        session_params: %{
          "modeId" => "read-only",
          "approvalPolicy" => "agent-full-access",
          "model" => "gpt-5-codex"
        },
        thread_result: %{"model" => "gpt-5-codex"}
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "session_new_mode_id_wins_over_approval_policy_and_model_overrides_init",
        steps
      )

    assert %{
             "params" => %{
               "model" => "gpt-5-codex",
               "sandbox" => "workspace-write",
               "approvalPolicy" => "on-request"
             }
           } = Enum.find(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

    assert session_result(transcript)["modes"]["currentModeId"] == "read-only"
    assert %{"params" => %{"approvalsReviewer" => "user"}} = last_write(transcript)
  end

  test "session_new_mode_id_is_session_scoped" do
    steps =
      open_session_steps(session_params: %{"modeId" => "read-only"}) ++
        [
          prompt(),
          {:note,
           "session/new's modeId and its approvalPolicy alias are session-scoped, unlike set_mode and set_config_option mode: the second session opened with the alias does not inherit read-only, and the third session opened without either opens in the adapter default agent (workspace-write sandbox, auto_review reviewer)"},
          session_new(%{"approvalPolicy" => "agent-full-access"}, 11),
          thread_start_reply(thread_result("thread-def"), 5),
          prompt(session_id: "thread-def", id: 21),
          session_new(%{}, 12),
          thread_start_reply(thread_result("thread-ghi"), 7),
          prompt(session_id: "thread-ghi", id: 22)
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_mode_id_is_session_scoped", steps)

    assert Enum.map(
             [@session_id, "thread-def", "thread-ghi"],
             &session_result(transcript, &1)["modes"]["currentModeId"]
           ) == ["read-only", "agent-full-access", "agent"]

    assert %{"params" => %{"sandbox" => "workspace-write", "approvalPolicy" => "on-request"}} =
             outbound_write(transcript, 12)

    assert Enum.map([20, 21, 22], &outbound_write(transcript, &1)["params"]["approvalPolicy"]) ==
             ["on-request", "never", "on-request"]

    assert Enum.map([20, 22], &outbound_write(transcript, &1)["params"]["approvalsReviewer"]) ==
             ["user", "auto_review"]
  end

  test "session_new_model_param_is_not_adopted" do
    steps =
      [{:init, init_opts()}, :post_connect, initialize_reply(), catalog_reply()] ++
        [
          session_new(%{"model" => "o3-custom"}),
          {:note,
           "The requested model only goes on this thread/start wire and is not adopted adapter-wide: with a thread/start result naming no model the session has no current model, turn/start carries none, and a later session/new without model sends none either"},
          thread_start_reply(Map.delete(thread_result(), "model")),
          prompt(),
          session_new(%{}, 11),
          thread_start_reply(Map.delete(thread_result("thread-def"), "model"), 5),
          prompt(session_id: "thread-def", id: 21)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "session_new_model_param_is_not_adopted", steps)

    assert Enum.map([10, 11], &outbound_write(transcript, &1)["params"]["model"]) ==
             ["o3-custom", nil]

    refute Map.has_key?(session_result(transcript)["models"], "currentModelId")

    assert Enum.map([20, 21], &Map.has_key?(outbound_write(transcript, &1)["params"], "model")) ==
             [false, false]
  end

  test "fast_mode_on_model_without_fast_tier_is_accepted_but_inert" do
    steps =
      open_session_steps(thread_result: %{"serviceTier" => "fast"}) ++
        [
          {:note,
           "gpt-5 lists no fast speed tier: the fast serviceTier reported by thread/start and an explicit fast-mode on are both accepted, but no fast-mode option is shown and turn/start carries no serviceTier"},
          set_config_option("fast-mode", "on"),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "fast_mode_on_model_without_fast_tier_is_accepted_but_inert",
        steps
      )

    assert Enum.map(session_result(transcript)["configOptions"], & &1["id"]) ==
             ["mode", "model", "reasoning_effort"]

    assert %{tag: :reply} = result_at(transcript, -2)
    assert is_nil(option(reply_at(transcript, -2), "fast-mode"))
    refute Map.has_key?(last_write(transcript)["params"], "serviceTier")
  end

  # -- thread/start results and init defaults -------------------------------

  test "thread_start_active_permission_profile_overrides_requested_mode" do
    steps =
      open_session_steps(
        session_params: %{"modeId" => "agent-full-access"},
        thread_result: %{"activePermissionProfile" => %{"id" => ":read-only"}}
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_start_active_permission_profile_overrides_requested_mode",
        steps
      )

    assert session_result(transcript)["modes"]["currentModeId"] == "read-only"
    assert option(session_result(transcript), "mode")["currentValue"] == "read-only"

    assert %{"params" => %{"approvalsReviewer" => "user", "approvalPolicy" => "on-request"}} =
             last_write(transcript)
  end

  test "thread_start_settings_permission_profile_sets_full_access" do
    steps =
      open_session_steps(
        thread_result: %{
          "settings" => %{"activePermissionProfile" => %{"id" => ":danger-no-sandbox"}}
        }
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_start_settings_permission_profile_sets_full_access",
        steps
      )

    assert session_result(transcript)["modes"]["currentModeId"] == "agent-full-access"

    assert %{
             "params" => %{
               "sandboxPolicy" => %{"type" => "dangerFullAccess"},
               "approvalPolicy" => "never"
             }
           } = last_write(transcript)
  end

  test "thread_start_thread_settings_sandbox_policy_sets_read_only" do
    steps =
      open_session_steps(
        thread_result: %{
          "threadSettings" => %{
            "sandboxPolicy" => %{"type" => "workspaceWrite", "writableRoots" => []},
            "approvalsReviewer" => "user"
          }
        }
      )

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_start_thread_settings_sandbox_policy_sets_read_only",
        steps
      )

    assert session_result(transcript)["modes"]["currentModeId"] == "read-only"
  end

  test "thread_start_workspace_permission_profile_selects_agent" do
    steps =
      open_session_steps(
        session_params: %{"modeId" => "agent-full-access"},
        thread_result: %{"activePermissionProfile" => %{"id" => ":workspace"}}
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_start_workspace_permission_profile_selects_agent",
        steps
      )

    assert session_result(transcript)["modes"]["currentModeId"] == "agent"

    assert %{
             "params" => %{
               "approvalsReviewer" => "auto_review",
               "sandboxPolicy" => %{"type" => "workspaceWrite"}
             }
           } = last_write(transcript)
  end

  test "thread_start_read_only_sandbox_policy_sets_read_only" do
    steps =
      open_session_steps(
        session_params: %{"modeId" => "agent-full-access"},
        thread_result: %{"threadSettings" => %{"sandboxPolicy" => %{"type" => "readOnly"}}}
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_start_read_only_sandbox_policy_sets_read_only",
        steps
      )

    assert session_result(transcript)["modes"]["currentModeId"] == "read-only"

    assert %{"params" => %{"approvalsReviewer" => "user", "approvalPolicy" => "on-request"}} =
             last_write(transcript)
  end

  test "thread_start_permission_profile_precedence" do
    steps =
      open_session_steps(
        thread_result: %{
          "activePermissionProfile" => %{"id" => ":read-only"},
          "settings" => %{"activePermissionProfile" => %{"id" => ":danger-no-sandbox"}}
        }
      ) ++
        [
          {:note,
           "The top-level profile wins over settings.activePermissionProfile; an unknown top-level profile id falls through to the sandboxPolicy in threadSettings"},
          session_new(%{}, 11),
          thread_start_reply(
            Map.merge(thread_result("thread-def"), %{
              "activePermissionProfile" => %{"id" => ":custom"},
              "threadSettings" => %{"sandboxPolicy" => %{"type" => "dangerFullAccess"}}
            }),
            4
          )
        ]

    transcript =
      CodexGolden.assert_golden(@area, "thread_start_permission_profile_precedence", steps)

    assert session_result(transcript, @session_id)["modes"]["currentModeId"] == "read-only"

    assert session_result(transcript, "thread-def")["modes"]["currentModeId"] ==
             "agent-full-access"
  end

  test "thread_start_thread_settings_win_over_settings" do
    steps =
      open_session_steps(
        note:
          "No permission profile anywhere: the sandboxPolicy under threadSettings is consulted before the one under settings, so readOnly wins over dangerFullAccess and the requested full-access mode",
        session_params: %{"modeId" => "agent-full-access"},
        thread_result: %{
          "threadSettings" => %{"sandboxPolicy" => %{"type" => "readOnly"}},
          "settings" => %{"sandboxPolicy" => %{"type" => "dangerFullAccess"}}
        }
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "thread_start_thread_settings_win_over_settings", steps)

    assert session_result(transcript)["modes"]["currentModeId"] == "read-only"

    assert %{"params" => %{"approvalsReviewer" => "user", "approvalPolicy" => "on-request"}} =
             last_write(transcript)
  end

  test "thread_start_result_reasoning_effort_seeds_session" do
    steps = open_session_steps(thread_result: %{"reasoningEffort" => "low"}) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_start_result_reasoning_effort_seeds_session",
        steps
      )

    assert session_result(transcript)["models"]["currentModelId"] == "gpt-5/low"
    assert %{"params" => %{"effort" => "low", "model" => "gpt-5"}} = last_write(transcript)
  end

  test "init_reasoning_effort_nil_falls_back_to_medium" do
    steps = open_session_steps(init: [reasoning_effort: nil]) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "init_reasoning_effort_nil_falls_back_to_medium", steps)

    assert option(session_result(transcript), "reasoning_effort")["currentValue"] == "medium"
    assert %{"params" => %{"effort" => "medium"}} = last_write(transcript)
  end

  test "init_model_effort_and_mode_seed_session" do
    steps =
      open_session_steps(
        init: [model: "gpt-5-codex", reasoning_effort: "high", mode_id: "read-only"],
        thread_result: %{"model" => "gpt-5-codex"}
      ) ++ [prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "init_model_effort_and_mode_seed_session", steps)

    assert %{"params" => %{"model" => "gpt-5-codex", "sandbox" => "workspace-write"}} =
             Enum.find(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "codex-mini/high"
    assert result["modes"]["currentModeId"] == "read-only"

    assert %{"params" => %{"effort" => "high", "approvalsReviewer" => "user"}} =
             last_write(transcript)
  end

  test "thread_start_without_model_falls_back_to_init_model" do
    steps =
      [{:init, init_opts(model: "gpt-5-codex")}, :post_connect, initialize_reply()] ++
        [
          catalog_reply(),
          session_new(),
          {:note, "The thread/start result names no model, so the init model stays in force"},
          thread_start_reply(Map.delete(thread_result(), "model")),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_start_without_model_falls_back_to_init_model",
        steps
      )

    assert %{"params" => %{"model" => "gpt-5-codex"}} =
             Enum.find(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

    result = session_result(transcript)
    assert result["models"]["currentModelId"] == "codex-mini/medium"
    assert option(result, "model")["currentValue"] == "gpt-5-codex"
    assert %{"params" => %{"model" => "gpt-5-codex"}} = last_write(transcript)
  end

  # -- session/set_model -----------------------------------------------------

  test "set_model_catalog_id_with_effort_suffix" do
    steps = open_session_steps() ++ [set_model("codex-mini/high"), prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "set_model_catalog_id_with_effort_suffix", steps)

    reply = reply_at(transcript, -2)
    assert reply["models"]["currentModelId"] == "codex-mini/high"
    assert option(reply, "model")["currentValue"] == "gpt-5-codex"
    assert %{"params" => %{"model" => "gpt-5-codex", "effort" => "high"}} = last_write(transcript)
  end

  test "set_model_whitespace_padded_id_is_trimmed" do
    steps =
      open_session_steps() ++
        [
          {:note, "Surrounding whitespace is trimmed before the id/effort lookup"},
          set_model("  codex-mini/high \n"),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_model_whitespace_padded_id_is_trimmed", steps)

    reply = reply_at(transcript, -2)
    assert reply["models"]["currentModelId"] == "codex-mini/high"
    assert option(reply, "model")["currentValue"] == "gpt-5-codex"
    assert %{"params" => %{"model" => "gpt-5-codex", "effort" => "high"}} = last_write(transcript)
  end

  test "set_model_unsupported_effort_suffix_falls_back_to_default" do
    steps =
      open_session_steps() ++
        [
          {:note, "xhigh is not offered by codex-mini, so its default effort medium is used"},
          set_model("codex-mini/xhigh")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_model_unsupported_effort_suffix_falls_back_to_default",
        steps
      )

    assert last_reply(transcript)["models"]["currentModelId"] == "codex-mini/medium"
    assert option(last_reply(transcript), "reasoning_effort")["currentValue"] == "medium"
  end

  test "set_model_plain_catalog_id_keeps_session_effort" do
    steps = open_session_steps(init: [reasoning_effort: "high"]) ++ [set_model("codex-mini")]

    transcript =
      CodexGolden.assert_golden(@area, "set_model_plain_catalog_id_keeps_session_effort", steps)

    assert last_reply(transcript)["models"]["currentModelId"] == "codex-mini/high"
  end

  test "set_model_plain_catalog_id_prefers_thread_start_effort" do
    steps =
      open_session_steps(thread_result: %{"reasoningEffort" => "low"}) ++
        [
          {:note,
           "thread/start seeded the session's effort with low while the adapter-wide effort is still the default medium: selecting a plain catalog id resolves the effort from the session, not the adapter"},
          set_model("gpt-5"),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_model_plain_catalog_id_prefers_thread_start_effort",
        steps
      )

    reply = outbound_reply(transcript, 30)
    assert reply["models"]["currentModelId"] == "gpt-5/low"
    assert option(reply, "reasoning_effort")["currentValue"] == "low"
    assert %{"params" => %{"model" => "gpt-5", "effort" => "low"}} = last_write(transcript)
  end

  test "set_model_unknown_id_prefers_session_effort_over_adapter_effort" do
    steps =
      open_session_steps(thread_result: %{"reasoningEffort" => "low"}) ++
        [
          {:note,
           "thread/start seeded the session's effort with low while the adapter-wide effort is still the default medium: a raw (non-catalog) selection carries the session's effort and then makes it adapter-wide"},
          set_model("o3-custom"),
          prompt(),
          {:note,
           "The next session inherits low; thread/settings/updated raises its effort to high, and a raw selection there follows the session again rather than the adapter-wide low"},
          session_new(%{}, 11),
          thread_start_reply(Map.delete(thread_result("thread-def"), "model"), 5),
          settings_updated(%{"effort" => "high"}, session_id: "thread-def"),
          set_model("o3-custom", session_id: "thread-def", id: 31),
          prompt(session_id: "thread-def", id: 21)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_model_unknown_id_prefers_session_effort_over_adapter_effort",
        steps
      )

    first = outbound_reply(transcript, 30)
    assert first["models"]["currentModelId"] == "o3-custom"
    assert option(first, "reasoning_effort")["currentValue"] == "low"
    assert option(outbound_reply(transcript, 31), "reasoning_effort")["currentValue"] == "high"

    assert [
             %{
               "method" => "turn/start",
               "params" => %{"model" => "o3-custom", "effort" => "low"}
             },
             %{"method" => "thread/start", "params" => %{"model" => "o3-custom"}},
             %{
               "method" => "turn/start",
               "params" => %{"model" => "o3-custom", "effort" => "high"}
             }
           ] = Enum.take(CodexGolden.writes(transcript), -3)
  end

  test "set_model_model_without_efforts_keeps_adapter_effort" do
    steps =
      open_session_steps(
        init: [reasoning_effort: "high"],
        catalog: v2_catalog() ++ [%{"id" => "o3"}]
      ) ++
        [
          {:note,
           "o3 lists no supported efforts, so the selection carries no effort: the reply's effort option and this turn/start fall back to the adapter-wide effort high, which also stays in force for the next session/new"},
          set_model("o3"),
          prompt(),
          session_new(%{}, 11),
          thread_start_reply(Map.delete(thread_result("thread-def"), "model"), 5),
          prompt(session_id: "thread-def", id: 21)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_model_model_without_efforts_keeps_adapter_effort",
        steps
      )

    reply = outbound_reply(transcript, 30)
    assert reply["models"]["currentModelId"] == "o3"
    assert option(reply, "reasoning_effort")["currentValue"] == "high"

    assert [
             %{"method" => "turn/start", "params" => %{"model" => "o3", "effort" => "high"}},
             %{"method" => "thread/start", "params" => %{"model" => "o3"}},
             %{"method" => "turn/start", "params" => %{"model" => "o3", "effort" => "high"}}
           ] = Enum.take(CodexGolden.writes(transcript), -3)
  end

  test "set_model_wire_name_resolves_catalog_entry" do
    steps = open_session_steps() ++ [set_model("gpt-5-codex"), prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "set_model_wire_name_resolves_catalog_entry", steps)

    assert reply_at(transcript, -2)["models"]["currentModelId"] == "codex-mini/medium"
    assert %{"params" => %{"model" => "gpt-5-codex"}} = last_write(transcript)
  end

  test "set_model_catalog_id_wins_over_colliding_wire_name" do
    catalog = [
      %{
        "id" => "gpt-5-legacy",
        "model" => "gpt-5",
        "displayName" => "GPT-5 (legacy alias)",
        "description" => "Pinned to the original gpt-5 snapshot",
        "hidden" => false,
        "defaultReasoningEffort" => "medium",
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "medium", "description" => "Balanced"}
        ]
      },
      %{
        "id" => "gpt-5",
        "model" => "gpt-5-2025",
        "displayName" => "GPT-5",
        "description" => "Latest gpt-5 snapshot",
        "hidden" => false,
        "defaultReasoningEffort" => "medium",
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "medium", "description" => "Balanced"},
          %{"reasoningEffort" => "high", "description" => "Deep"}
        ]
      }
    ]

    steps =
      open_session_steps(
        note:
          "The legacy entry's wire model equals the other entry's catalog id: a plain set_model of that string resolves by catalog id first, so it selects gpt-5 (wire gpt-5-2025) rather than gpt-5-legacy (wire gpt-5)",
        catalog: catalog,
        thread_result: %{"model" => "gpt-5-2025"}
      ) ++ [set_model("gpt-5"), prompt()]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_model_catalog_id_wins_over_colliding_wire_name",
        steps
      )

    reply = outbound_reply(transcript, 30)
    assert reply["models"]["currentModelId"] == "gpt-5/medium"
    assert option(reply, "model")["currentValue"] == "gpt-5-2025"
    assert %{"params" => %{"model" => "gpt-5-2025"}} = outbound_write(transcript, 20)
  end

  test "set_model_unknown_id_replies_raw_model" do
    steps = open_session_steps() ++ [set_model("o3-custom"), prompt()]

    transcript = CodexGolden.assert_golden(@area, "set_model_unknown_id_replies_raw_model", steps)

    reply = reply_at(transcript, -2)
    assert reply["models"]["currentModelId"] == "o3-custom"

    assert hd(reply["models"]["availableModels"]) ==
             %{"modelId" => "o3-custom", "name" => "o3-custom"}

    assert %{"params" => %{"model" => "o3-custom", "effort" => "medium"}} = last_write(transcript)
  end

  test "set_model_effort_suffix_without_catalog_is_raw" do
    steps =
      open_session_steps(catalog: []) ++
        [
          {:note,
           "Without a catalog the id/effort form is not split: the whole string becomes the wire model"},
          set_model("gpt-5/high"),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_model_effort_suffix_without_catalog_is_raw", steps)

    assert reply_at(transcript, -2)["models"]["currentModelId"] == "gpt-5/high"

    assert %{"params" => %{"model" => "gpt-5/high", "effort" => "medium"}} =
             last_write(transcript)
  end

  test "set_model_rejects_blank_id_and_unknown_session" do
    steps =
      open_session_steps() ++
        [
          set_model("   "),
          {:outbound,
           %{
             "method" => "session/set_model",
             "id" => 31,
             "params" => %{"sessionId" => @session_id}
           }},
          {:outbound,
           %{
             "method" => "session/set_model",
             "id" => 32,
             "params" => %{"sessionId" => "thread-missing", "modelId" => "gpt-5"}
           }},
          {:outbound,
           %{"method" => "session/set_model", "id" => 33, "params" => %{"modelId" => "gpt-5"}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_model_rejects_blank_id_and_unknown_session", steps)

    assert Enum.map(Enum.take(transcript, -4), & &1.result.error) == [
             "modelId is required",
             "modelId is required",
             "Unknown Codex session: thread-missing",
             "sessionId is required"
           ]
  end

  test "set_model_seeds_subsequent_session_new" do
    steps =
      open_session_steps() ++
        [
          set_model("codex-mini/high"),
          {:note,
           "The selection is adapter-wide: the next thread/start carries the wire model, and the new session inherits model and effort even though its thread/start result names no model"},
          session_new(%{}, 11),
          thread_start_reply(Map.delete(thread_result("thread-def"), "model"), 4),
          prompt(session_id: "thread-def", id: 21)
        ]

    transcript = CodexGolden.assert_golden(@area, "set_model_seeds_subsequent_session_new", steps)

    assert %{"id" => 4, "params" => %{"model" => "gpt-5-codex"}} =
             Enum.find(
               CodexGolden.writes(transcript),
               &(&1["method"] == "thread/start" and &1["id"] == 4)
             )

    assert session_result(transcript, "thread-def")["models"]["currentModelId"] ==
             "codex-mini/high"

    assert %{
             "params" => %{
               "threadId" => "thread-def",
               "model" => "gpt-5-codex",
               "effort" => "high"
             }
           } =
             last_write(transcript)
  end

  # -- session/set_config_option --------------------------------------------

  test "set_config_option_model_resolves_catalog_entry" do
    steps = open_session_steps() ++ [set_config_option("model", "codex-mini"), prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_model_resolves_catalog_entry", steps)

    reply = reply_at(transcript, -2)
    assert option(reply, "model")["currentValue"] == "gpt-5-codex"
    assert reply["models"]["currentModelId"] == "codex-mini/medium"
    assert %{"params" => %{"model" => "gpt-5-codex"}} = last_write(transcript)
  end

  test "set_config_option_model_without_efforts_keeps_adapter_effort" do
    steps =
      open_session_steps(
        init: [reasoning_effort: "high"],
        catalog: v2_catalog() ++ [%{"id" => "o3"}]
      ) ++
        [
          {:note,
           "The set_config_option twin of the set_model case: o3 lists no supported efforts, so the selection carries no effort and the adapter-wide effort high keeps feeding the reply, this turn/start, and the next session/new (whose thread/start result names no model, so it also inherits o3)"},
          set_config_option("model", "o3"),
          prompt(),
          session_new(%{}, 11),
          thread_start_reply(Map.delete(thread_result("thread-def"), "model"), 5),
          prompt(session_id: "thread-def", id: 21)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_config_option_model_without_efforts_keeps_adapter_effort",
        steps
      )

    reply = outbound_reply(transcript, 40)
    assert reply["models"]["currentModelId"] == "o3"
    assert option(reply, "reasoning_effort")["currentValue"] == "high"
    assert session_result(transcript, "thread-def")["models"]["currentModelId"] == "o3"

    assert Enum.map([20, 11, 21], &outbound_write(transcript, &1)["params"])
           |> Enum.map(&Map.take(&1, ["model", "effort"])) == [
             %{"model" => "o3", "effort" => "high"},
             %{"model" => "o3"},
             %{"model" => "o3", "effort" => "high"}
           ]
  end

  test "set_config_option_reasoning_effort_catalog_model" do
    steps =
      open_session_steps() ++
        [
          {:note,
           "The effort option and the next turn/start follow the new value, but currentModelId keeps the id computed at thread/start"},
          set_config_option("reasoning_effort", "high"),
          set_config_option("reasoning_effort", "xhigh", id: 41),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_reasoning_effort_catalog_model", steps)

    assert option(reply_at(transcript, -3), "reasoning_effort")["currentValue"] == "high"
    assert reply_at(transcript, -3)["models"]["currentModelId"] == "gpt-5/medium"
    assert result_at(transcript, -2).error == "Unsupported reasoning_effort: xhigh"
    assert %{"params" => %{"effort" => "high"}} = last_write(transcript)
  end

  test "set_config_option_reasoning_effort_without_catalog" do
    steps =
      open_session_steps(catalog: []) ++
        [
          {:note, "With no catalog model the static minimal/low/medium/high list applies"},
          set_config_option("reasoning_effort", "low"),
          set_config_option("reasoning_effort", "xhigh", id: 41)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_config_option_reasoning_effort_without_catalog",
        steps
      )

    assert option(reply_at(transcript, -2), "reasoning_effort")["currentValue"] == "low"
    assert result_at(transcript, -1).error == "Unsupported reasoning_effort: xhigh"
  end

  test "set_config_option_reasoning_effort_model_without_efforts_accepts_any" do
    steps =
      open_session_steps(catalog: [%{"id" => "o3"}], thread_result: %{"model" => "o3"}) ++
        [set_config_option("reasoning_effort", "ultra")]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_config_option_reasoning_effort_model_without_efforts_accepts_any",
        steps
      )

    assert option(last_reply(transcript), "reasoning_effort")["currentValue"] == "ultra"
    assert last_reply(transcript)["models"]["currentModelId"] == "o3"
  end

  test "set_config_option_mode_valid_and_invalid" do
    steps =
      open_session_steps() ++
        [
          {:note, "Unlike session/set_mode, no current_mode_update notification is emitted"},
          set_config_option("mode", "read-only"),
          set_config_option("mode", "full-auto", id: 41),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_mode_valid_and_invalid", steps)

    assert %{tag: :reply} = result_at(transcript, -3)
    assert option(reply_at(transcript, -3), "mode")["currentValue"] == "read-only"
    assert result_at(transcript, -2).error == "Unsupported Codex mode: \"full-auto\""
    assert %{"params" => %{"approvalsReviewer" => "user"}} = last_write(transcript)
  end

  test "set_config_option_fast_mode_on_off_invalid" do
    steps =
      open_session_steps(
        catalog: [fast_model() | v2_catalog()],
        thread_result: %{"model" => "gpt-5-fast"}
      ) ++
        [
          set_config_option("fast-mode", "on"),
          prompt(),
          set_config_option("fast-mode", false, id: 41),
          set_config_option("fast-mode", "turbo", id: 42)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_fast_mode_on_off_invalid", steps)

    assert option(reply_at(transcript, -4), "fast-mode")["currentValue"] == "on"
    assert %{"params" => %{"serviceTier" => "fast"}} = last_write(transcript)
    assert option(reply_at(transcript, -2), "fast-mode")["currentValue"] == "off"
    assert result_at(transcript, -1).error == "Unsupported fast-mode value: \"turbo\""
  end

  test "set_config_option_rejects_unknown_and_malformed" do
    steps =
      open_session_steps() ++
        [
          set_config_option("theme", "dark"),
          {:outbound,
           %{
             "method" => "session/set_config_option",
             "id" => 41,
             "params" => %{"sessionId" => @session_id, "value" => "gpt-5"}
           }},
          {:note, "A missing value is reported as an unsupported option, not as a missing value"},
          {:outbound,
           %{
             "method" => "session/set_config_option",
             "id" => 42,
             "params" => %{"sessionId" => @session_id, "configId" => "model"}
           }},
          set_config_option("model", 123, id: 43),
          set_config_option("model", "", id: 44),
          {:outbound,
           %{
             "method" => "session/set_config_option",
             "id" => 45,
             "params" => %{
               "sessionId" => "thread-missing",
               "configId" => "mode",
               "value" => "agent"
             }
           }},
          {:outbound,
           %{
             "method" => "session/set_config_option",
             "id" => 46,
             "params" => %{"configId" => "mode", "value" => "agent"}
           }}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_rejects_unknown_and_malformed", steps)

    assert Enum.map(Enum.take(transcript, -8), &Map.get(&1.result, :error)) == [
             "Unsupported Codex config option: theme",
             "configId and value are required",
             nil,
             "Unsupported Codex config option: model",
             "Unsupported Codex config option: model",
             "modelId is required",
             "Unknown Codex session: thread-missing",
             "sessionId is required"
           ]
  end

  test "set_config_option_fast_mode_boolean_true" do
    steps =
      open_session_steps(
        catalog: [fast_model() | v2_catalog()],
        thread_result: %{"model" => "gpt-5-fast"}
      ) ++ [set_config_option("fast-mode", true), prompt()]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_fast_mode_boolean_true", steps)

    assert option(reply_at(transcript, -2), "fast-mode")["currentValue"] == "on"
    assert %{"params" => %{"serviceTier" => "fast"}} = last_write(transcript)
  end

  test "set_config_option_seeds_subsequent_session_new" do
    steps =
      open_session_steps() ++
        [
          set_config_option("model", "codex-mini"),
          set_config_option("reasoning_effort", "high", id: 41),
          set_config_option("mode", "read-only", id: 42),
          {:note,
           "model, reasoning_effort and mode are adapter-wide: the next thread/start and the new session's first turn/start follow them"},
          session_new(%{}, 11),
          thread_start_reply(Map.delete(thread_result("thread-def"), "model"), 4),
          prompt(session_id: "thread-def", id: 21)
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_seeds_subsequent_session_new", steps)

    assert %{"params" => %{"model" => "gpt-5-codex", "sandbox" => "workspace-write"}} =
             Enum.find(
               CodexGolden.writes(transcript),
               &(&1["method"] == "thread/start" and &1["id"] == 4)
             )

    result = session_result(transcript, "thread-def")
    assert result["modes"]["currentModeId"] == "read-only"
    assert result["models"]["currentModelId"] == "codex-mini/high"

    assert %{
             "params" => %{
               "model" => "gpt-5-codex",
               "effort" => "high",
               "approvalsReviewer" => "user"
             }
           } =
             last_write(transcript)
  end

  # -- session/set_mode ------------------------------------------------------

  test "set_mode_emits_current_mode_update_and_reshapes_turn_start" do
    steps =
      open_session_steps() ++
        [
          set_mode("read-only"),
          prompt(),
          {:note,
           "The mode is adapter-wide too: the next thread/start follows the latest set_mode"},
          set_mode("agent-full-access", 51),
          session_new(%{}, 11)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_mode_emits_current_mode_update_and_reshapes_turn_start",
        steps
      )

    assert %{
             tag: :messages_and_reply,
             reply: %{},
             messages: [
               %{
                 "method" => "session/update",
                 "params" => %{
                   "sessionId" => @session_id,
                   "update" => %{
                     "sessionUpdate" => "current_mode_update",
                     "currentModeId" => "read-only"
                   }
                 }
               }
             ]
           } = result_at(transcript, -5)

    assert [
             %{"method" => "turn/start", "params" => %{"approvalsReviewer" => "user"}},
             %{"method" => "thread/start", "params" => %{"sandbox" => "danger-full-access"}}
           ] = Enum.take(CodexGolden.writes(transcript), -2)
  end

  test "set_mode_nil_defaults_to_agent_and_rejects_invalid" do
    steps =
      open_session_steps(session_params: %{"modeId" => "read-only"}) ++
        [
          {:note, "A missing modeId normalizes to the default agent mode"},
          set_mode_params(%{"sessionId" => @session_id}, 50),
          prompt(),
          set_mode("full-auto", 51),
          set_mode_params(%{"sessionId" => "thread-missing", "modeId" => "agent"}, 52),
          set_mode_params(%{"modeId" => "agent"}, 53)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "set_mode_nil_defaults_to_agent_and_rejects_invalid",
        steps
      )

    assert %{
             messages: [%{"params" => %{"update" => %{"currentModeId" => "agent"}}}],
             reply: %{}
           } = result_at(transcript, -5)

    assert %{"params" => %{"approvalsReviewer" => "auto_review"}} = last_write(transcript)

    assert Enum.map(Enum.take(transcript, -3), & &1.result.error) == [
             "Unsupported Codex mode: \"full-auto\"",
             "Unknown Codex session: thread-missing",
             "sessionId is required"
           ]
  end

  # -- notifications ---------------------------------------------------------

  test "thread_settings_updated_model_and_effort" do
    steps =
      open_session_steps() ++
        [
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{
               "threadId" => @session_id,
               "threadSettings" => %{"model" => "gpt-5-codex", "effort" => "high"}
             }
           }},
          {:note,
           "Readback: the model option follows the new wire model, but currentModelId keeps the id computed at thread/start"},
          readback(),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "thread_settings_updated_model_and_effort", steps)

    reply = reply_at(transcript, -2)
    assert option(reply, "model")["currentValue"] == "gpt-5-codex"
    assert reply["models"]["currentModelId"] == "gpt-5/medium"
    assert %{"params" => %{"model" => "gpt-5-codex", "effort" => "high"}} = last_write(transcript)
  end

  test "thread_settings_updated_settings_key_reasoning_effort" do
    steps =
      open_session_steps() ++
        [
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{"threadId" => @session_id, "settings" => %{"reasoningEffort" => "low"}}
           }},
          readback(),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_settings_key_reasoning_effort",
        steps
      )

    assert option(reply_at(transcript, -2), "reasoning_effort")["currentValue"] == "low"
    assert %{"params" => %{"model" => "gpt-5", "effort" => "low"}} = last_write(transcript)
  end

  test "thread_settings_updated_alias_precedence" do
    steps =
      open_session_steps() ++
        [
          {:note,
           "Both containers and both effort keys are present: threadSettings wins over settings (so the model stays gpt-5) and effort wins over reasoningEffort"},
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{
               "threadId" => @session_id,
               "threadSettings" => %{"effort" => "high", "reasoningEffort" => "low"},
               "settings" => %{
                 "model" => "ignored-model",
                 "effort" => "low",
                 "reasoningEffort" => "low"
               }
             }
           }},
          readback(),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "thread_settings_updated_alias_precedence", steps)

    reply = outbound_reply(transcript, 40)
    assert option(reply, "reasoning_effort")["currentValue"] == "high"
    assert option(reply, "model")["currentValue"] == "gpt-5"
    assert %{"params" => %{"model" => "gpt-5", "effort" => "high"}} = last_write(transcript)
  end

  test "thread_settings_updated_mode_from_permission_profile" do
    steps =
      open_session_steps() ++
        [
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{
               "threadId" => @session_id,
               "activePermissionProfile" => %{"id" => ":danger-no-sandbox"},
               "threadSettings" => %{}
             }
           }},
          readback(),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_mode_from_permission_profile",
        steps
      )

    assert option(reply_at(transcript, -2), "mode")["currentValue"] == "agent-full-access"

    assert %{"params" => %{"sandboxPolicy" => %{"type" => "dangerFullAccess"}}} =
             last_write(transcript)
  end

  test "thread_settings_updated_profile_wins_over_sandbox_policy" do
    steps =
      open_session_steps() ++
        [
          {:note,
           "The params-level permission profile is consulted before threadSettings.sandboxPolicy, so read-only wins over dangerFullAccess"},
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{
               "threadId" => @session_id,
               "activePermissionProfile" => %{"id" => ":read-only"},
               "threadSettings" => %{"sandboxPolicy" => %{"type" => "dangerFullAccess"}}
             }
           }},
          readback(),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_profile_wins_over_sandbox_policy",
        steps
      )

    assert option(outbound_reply(transcript, 40), "mode")["currentValue"] == "read-only"

    assert %{"params" => %{"approvalsReviewer" => "user", "approvalPolicy" => "on-request"}} =
             last_write(transcript)
  end

  test "thread_settings_updated_mode_from_sandbox_policy" do
    steps =
      open_session_steps(session_params: %{"modeId" => "agent-full-access"}) ++
        [
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{
               "threadId" => @session_id,
               "threadSettings" => %{
                 "sandboxPolicy" => %{"type" => "workspaceWrite", "writableRoots" => []},
                 "approvalsReviewer" => "auto_review"
               }
             }
           }},
          readback(),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "thread_settings_updated_mode_from_sandbox_policy", steps)

    assert option(reply_at(transcript, -2), "mode")["currentValue"] == "agent"
    assert %{"params" => %{"approvalsReviewer" => "auto_review"}} = last_write(transcript)
  end

  test "thread_settings_updated_without_mode_keeps_session_mode" do
    steps =
      open_session_steps(session_params: %{"modeId" => "read-only"}) ++
        [
          {:note,
           "The session was opened read-only while the adapter-wide mode is still agent: an update carrying only a model keeps the session's own read-only mode rather than falling back to the adapter-wide one"},
          settings_updated(%{"model" => "gpt-5-codex"}),
          readback(),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_without_mode_keeps_session_mode",
        steps
      )

    reply = outbound_reply(transcript, 40)
    assert option(reply, "mode")["currentValue"] == "read-only"
    assert option(reply, "model")["currentValue"] == "gpt-5-codex"

    assert %{"params" => %{"approvalsReviewer" => "user", "model" => "gpt-5-codex"}} =
             outbound_write(transcript, 20)
  end

  test "thread_settings_updated_legacy_sandbox_string" do
    steps =
      open_session_steps() ++
        [
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{"threadId" => @session_id, "settings" => %{"sandbox" => "read-only"}}
           }},
          readback()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "thread_settings_updated_legacy_sandbox_string", steps)

    assert option(last_reply(transcript), "mode")["currentValue"] == "read-only"
  end

  test "thread_settings_updated_legacy_sandbox_strings_select_each_mode" do
    steps =
      open_session_steps() ++
        [
          settings_updated(%{"sandbox" => "workspace-write", "approvalsReviewer" => "user"}),
          readback(),
          settings_updated(%{"sandbox" => "danger-full-access"}),
          readback(id: 41),
          {:note, "workspace-write without a reviewer selects agent"},
          settings_updated(%{"sandbox" => "workspace-write"}),
          readback(id: 42),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_legacy_sandbox_strings_select_each_mode",
        steps
      )

    assert Enum.map([-7, -5, -2], &option(reply_at(transcript, &1), "mode")["currentValue"]) ==
             ["read-only", "agent-full-access", "agent"]

    assert %{"params" => %{"approvalsReviewer" => "auto_review"}} = last_write(transcript)
  end

  test "thread_settings_updated_sandbox_policy_reviewer_variants" do
    steps =
      open_session_steps(session_params: %{"modeId" => "read-only"}) ++
        [
          {:note, "workspaceWrite without an approvalsReviewer selects agent"},
          settings_updated(%{"sandboxPolicy" => %{"type" => "workspaceWrite"}}),
          readback(),
          settings_updated(%{"sandboxPolicy" => %{"type" => "readOnly"}}),
          readback(id: 41),
          {:note, "An unrecognized reviewer also selects agent"},
          settings_updated(%{
            "sandboxPolicy" => %{"type" => "workspaceWrite", "writableRoots" => []},
            "approvalsReviewer" => "custom"
          }),
          readback(id: 42),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_sandbox_policy_reviewer_variants",
        steps
      )

    assert Enum.map([-7, -5, -2], &option(reply_at(transcript, &1), "mode")["currentValue"]) ==
             ["agent", "read-only", "agent"]

    assert %{"params" => %{"approvalsReviewer" => "auto_review"}} = last_write(transcript)
  end

  test "thread_settings_updated_unknown_thread_creates_session" do
    steps =
      open_session_steps() ++
        [
          {:note,
           "A settings update for a thread the adapter never opened materializes a session for it"},
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{
               "threadId" => "thread-ghost",
               "threadSettings" => %{"model" => "gpt-5-codex"}
             }
           }},
          readback(session_id: "thread-ghost")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_unknown_thread_creates_session",
        steps
      )

    assert %{tag: :reply} = CodexGolden.last_result(transcript)
    assert last_reply(transcript)["models"]["currentModelId"] == "codex-mini/medium"
  end

  test "thread_settings_updated_ghost_thread_effort_change_keeps_model_id" do
    steps =
      open_session_steps() ++
        [
          settings_updated(%{"model" => "gpt-5-codex"}, session_id: "thread-ghost"),
          {:note,
           "The session materialized by the update stores the model id computed from it (codex-mini/medium); a later effort change on that session keeps this id while the effort option and turn/start follow the new value, as for sessions opened by thread/start"},
          set_config_option("reasoning_effort", "high", session_id: "thread-ghost"),
          prompt(session_id: "thread-ghost")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_ghost_thread_effort_change_keeps_model_id",
        steps
      )

    reply = outbound_reply(transcript, 40)
    assert reply["models"]["currentModelId"] == "codex-mini/medium"
    assert option(reply, "reasoning_effort")["currentValue"] == "high"

    assert %{
             "params" => %{
               "threadId" => "thread-ghost",
               "model" => "gpt-5-codex",
               "effort" => "high"
             }
           } =
             outbound_write(transcript, 20)
  end

  test "thread_settings_updated_without_thread_id_is_ignored" do
    steps =
      open_session_steps() ++
        [
          {:inbound,
           %{
             "method" => "thread/settings/updated",
             "params" => %{
               "sessionId" => @session_id,
               "threadSettings" => %{"model" => "gpt-5-codex"}
             }
           }},
          readback()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_settings_updated_without_thread_id_is_ignored",
        steps
      )

    assert %{tag: :skip} = result_at(transcript, -2)
    assert option(last_reply(transcript), "model")["currentValue"] == "gpt-5"
  end

  test "model_rerouted_emits_thought_chunk" do
    steps =
      open_session_steps() ++
        [
          {:inbound,
           %{
             "method" => "model/rerouted",
             "params" => %{
               "threadId" => @session_id,
               "fromModel" => "gpt-5",
               "toModel" => "gpt-5-codex",
               "reason" => "capacity"
             }
           }},
          {:inbound,
           %{
             "method" => "model/rerouted",
             "params" => %{"threadId" => @session_id, "from" => "gpt-5", "to" => "gpt-5-codex"}
           }},
          {:note, "Rerouting is reported only; the session's model is unchanged"},
          readback()
        ]

    transcript = CodexGolden.assert_golden(@area, "model_rerouted_emits_thought_chunk", steps)

    assert [
             %{
               "params" => %{
                 "update" => %{
                   "sessionUpdate" => "agent_thought_chunk",
                   "content" => %{"text" => first}
                 }
               }
             },
             %{"params" => %{"update" => %{"content" => %{"text" => second}}}}
           ] = Enum.drop(CodexGolden.messages(transcript), 1)

    assert first == "Model rerouted from gpt-5 to gpt-5-codex (capacity).\n\n"
    assert second == "Model rerouted from gpt-5 to gpt-5-codex (unknown).\n\n"
    assert option(last_reply(transcript), "model")["currentValue"] == "gpt-5"
  end

  test "model_rerouted_session_id_alias_and_single_session_fallback" do
    steps =
      open_session_steps() ++
        [
          {:note,
           "The session is resolved like for other notifications: sessionId is accepted as an alias for threadId, and a notification with no id at all is attributed to the single open session"},
          {:inbound,
           %{
             "method" => "model/rerouted",
             "params" => %{
               "sessionId" => @session_id,
               "fromModel" => "gpt-5",
               "toModel" => "gpt-5-codex",
               "reason" => "capacity"
             }
           }},
          {:inbound,
           %{
             "method" => "model/rerouted",
             "params" => %{
               "fromModel" => "gpt-5",
               "toModel" => "gpt-5-codex",
               "reason" => "capacity"
             }
           }}
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "model_rerouted_session_id_alias_and_single_session_fallback",
        steps
      )

    assert [
             %{
               "params" => %{
                 "sessionId" => @session_id,
                 "update" => %{"content" => %{"text" => text}}
               }
             },
             %{
               "params" => %{
                 "sessionId" => @session_id,
                 "update" => %{"content" => %{"text" => text}}
               }
             }
           ] = Enum.drop(CodexGolden.messages(transcript), 1)

    assert text == "Model rerouted from gpt-5 to gpt-5-codex (capacity).\n\n"
  end

  test "thread_started_notification_registers_session" do
    steps =
      open_session_steps() ++
        [
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{
               "thread" => %{
                 "id" => "thread-2",
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_100
               },
               "model" => "gpt-5-codex"
             }
           }},
          readback(session_id: "thread-2")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "thread_started_notification_registers_session", steps)

    assert %{tag: :reply} = CodexGolden.last_result(transcript)
    assert last_reply(transcript)["models"]["currentModelId"] == "codex-mini/medium"
  end

  test "thread_started_thread_id_fallback_keys" do
    steps =
      open_session_steps() ++
        [
          {:note,
           "Without thread.id the session id is taken from thread.sessionId, params.threadId or params.sessionId; each alone registers a session that a config write can address"},
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{
               "thread" => %{"sessionId" => "thread-2", "cwd" => "/tmp/project"},
               "model" => "gpt-5-codex"
             }
           }},
          readback(session_id: "thread-2"),
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{"threadId" => "thread-3", "model" => "gpt-5-codex"}
           }},
          readback(session_id: "thread-3", id: 41),
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{"sessionId" => "thread-4", "model" => "gpt-5-codex"}
           }},
          readback(session_id: "thread-4", id: 42),
          {:note,
           "Payloads carrying several keys pin the order: thread.sessionId beats params.threadId, and params.threadId beats params.sessionId; the losing key registers nothing"},
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{
               "thread" => %{"sessionId" => "thread-5", "cwd" => "/tmp/project"},
               "threadId" => "thread-6",
               "model" => "gpt-5-codex"
             }
           }},
          readback(session_id: "thread-5", id: 43),
          readback(session_id: "thread-6", id: 44),
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{
               "threadId" => "thread-7",
               "sessionId" => "thread-8",
               "model" => "gpt-5-codex"
             }
           }},
          readback(session_id: "thread-7", id: 45),
          readback(session_id: "thread-8", id: 46)
        ]

    transcript = CodexGolden.assert_golden(@area, "thread_started_thread_id_fallback_keys", steps)

    assert Enum.map([40, 41, 42, 43, 45], &outbound_result(transcript, &1).tag) ==
             [:reply, :reply, :reply, :reply, :reply]

    assert Enum.map(
             [40, 41, 42, 43, 45],
             &outbound_reply(transcript, &1)["models"]["currentModelId"]
           ) ==
             List.duplicate("codex-mini/medium", 5)

    assert Enum.map([44, 46], &outbound_result(transcript, &1).error) ==
             ["Unknown Codex session: thread-6", "Unknown Codex session: thread-8"]
  end

  test "thread_started_thread_id_wins_over_thread_session_id" do
    steps =
      open_session_steps() ++
        [
          {:note,
           "A thread object carrying both id and sessionId registers the session under id; sessionId registers nothing"},
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{
               "thread" => %{
                 "id" => "thread-2",
                 "sessionId" => "thread-3",
                 "cwd" => "/tmp/project"
               },
               "model" => "gpt-5-codex"
             }
           }},
          readback(session_id: "thread-2"),
          readback(session_id: "thread-3", id: 41)
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_started_thread_id_wins_over_thread_session_id",
        steps
      )

    assert outbound_reply(transcript, 40)["models"]["currentModelId"] == "codex-mini/medium"
    assert outbound_result(transcript, 41).error == "Unknown Codex session: thread-3"
  end

  test "thread_started_effort_and_service_tier_seed_session" do
    steps =
      open_session_steps(catalog: [fast_model() | v2_catalog()]) ++
        [
          {:note,
           "reasoningEffort and serviceTier at the params level of thread/started seed the new session like a thread/start result would: its first turn/start carries effort high and serviceTier fast. The prompt runs before the readback because the readback turns fast mode off"},
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{
               "thread" => %{
                 "id" => "thread-2",
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_100
               },
               "model" => "gpt-5-fast",
               "reasoningEffort" => "high",
               "serviceTier" => "fast"
             }
           }},
          prompt(session_id: "thread-2"),
          readback(session_id: "thread-2")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_started_effort_and_service_tier_seed_session",
        steps
      )

    assert %{
             "params" => %{
               "threadId" => "thread-2",
               "model" => "gpt-5-fast",
               "effort" => "high",
               "serviceTier" => "fast"
             }
           } = outbound_write(transcript, 20)

    reply = outbound_reply(transcript, 40)
    assert reply["models"]["currentModelId"] == "gpt-5-fast/high"
    assert option(reply, "reasoning_effort")["currentValue"] == "high"
  end

  test "thread_started_for_existing_session_resets_model_and_mode" do
    steps =
      open_session_steps(thread_result: %{"activePermissionProfile" => %{"id" => ":read-only"}}) ++
        [
          {:note,
           "thread/started for the already-open thread rebuilds its session: the model follows the notification and the read-only mode selected at thread/start falls back to the adapter default"},
          {:inbound,
           %{
             "method" => "thread/started",
             "params" => %{
               "thread" => %{
                 "id" => @session_id,
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_100
               },
               "model" => "gpt-5-codex"
             }
           }},
          readback(),
          prompt()
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "thread_started_for_existing_session_resets_model_and_mode",
        steps
      )

    assert option(session_result(transcript), "mode")["currentValue"] == "read-only"

    reply = reply_at(transcript, -2)
    assert option(reply, "model")["currentValue"] == "gpt-5-codex"
    assert option(reply, "mode")["currentValue"] == "agent"

    assert %{"params" => %{"model" => "gpt-5-codex", "approvalsReviewer" => "auto_review"}} =
             last_write(transcript)
  end

  test "thread_started_without_thread_id_is_ignored" do
    steps =
      open_session_steps() ++
        [
          {:inbound, %{"method" => "thread/started", "params" => %{"model" => "gpt-5-codex"}}},
          readback()
        ]

    transcript =
      CodexGolden.assert_golden(@area, "thread_started_without_thread_id_is_ignored", steps)

    assert %{tag: :skip} = result_at(transcript, -2)
    assert option(last_reply(transcript), "model")["currentValue"] == "gpt-5"
  end

  # -- step helpers ----------------------------------------------------------

  defp init_opts(overrides \\ []) do
    Keyword.merge([workspace_roots: ["/tmp"], cwd: "/tmp/project"], overrides)
  end

  # init -> initialize -> model/list -> session/new -> thread/start reply.
  # App-server request ids: 1 initialize, 2 model/list, 3 thread/start, then 4
  # for the next turn/start or thread/start. ACP request ids: 10 session/new,
  # 20 session/prompt, 30 session/set_model, 40 session/set_config_option,
  # 50 session/set_mode; scenarios with several calls of one kind count up.
  # A `:note` is recorded right after the init step: a scenario must not start
  # with a `{:note, ...}` step, because the harness would then prepend its
  # default `{:init, ...}` before the scenario's own one.
  defp open_session_steps(opts \\ []) do
    catalog_reply =
      Keyword.get_lazy(opts, :catalog_reply, fn ->
        %{"data" => Keyword.get(opts, :catalog, v2_catalog()), "nextCursor" => nil}
      end)

    note =
      case Keyword.fetch(opts, :note) do
        {:ok, text} -> [{:note, text}]
        :error -> []
      end

    [{:init, init_opts(Keyword.get(opts, :init, []))}] ++
      note ++
      [
        :post_connect,
        initialize_reply(),
        {:inbound, %{"id" => 2, "result" => catalog_reply}},
        session_new(Keyword.get(opts, :session_params, %{})),
        thread_start_reply(Map.merge(thread_result(), Keyword.get(opts, :thread_result, %{})))
      ]
  end

  defp initialize_reply, do: {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}}

  defp catalog_reply(catalog \\ v2_catalog()) do
    {:inbound, %{"id" => 2, "result" => %{"data" => catalog, "nextCursor" => nil}}}
  end

  defp session_new(params \\ %{}, id \\ 10) do
    {:outbound,
     %{
       "method" => "session/new",
       "id" => id,
       "params" => Map.merge(%{"cwd" => "/tmp/project", "mcpServers" => []}, params)
     }}
  end

  defp thread_result(session_id \\ @session_id) do
    %{
      "model" => "gpt-5",
      "thread" => %{"id" => session_id, "cwd" => "/tmp/project", "updatedAt" => 1_700_000_000}
    }
  end

  defp thread_start_reply(result, id \\ 3), do: {:inbound, %{"id" => id, "result" => result}}

  defp prompt(opts \\ []) do
    {:outbound,
     %{
       "method" => "session/prompt",
       "id" => Keyword.get(opts, :id, 20),
       "params" => %{
         "sessionId" => Keyword.get(opts, :session_id, @session_id),
         "prompt" => [%{"type" => "text", "text" => "hi"}]
       }
     }}
  end

  defp set_model(model_id, opts \\ []) do
    {:outbound,
     %{
       "method" => "session/set_model",
       "id" => Keyword.get(opts, :id, 30),
       "params" => %{
         "sessionId" => Keyword.get(opts, :session_id, @session_id),
         "modelId" => model_id
       }
     }}
  end

  defp set_config_option(config_id, value, opts \\ []) do
    {:outbound,
     %{
       "method" => "session/set_config_option",
       "id" => Keyword.get(opts, :id, 40),
       "params" => %{
         "sessionId" => Keyword.get(opts, :session_id, @session_id),
         "configId" => config_id,
         "value" => value
       }
     }}
  end

  # session/set_mode; `set_mode_params/2` takes the whole params map so a
  # scenario can omit modeId or sessionId.
  defp set_mode(mode_id, id \\ 50) do
    {:outbound,
     %{
       "method" => "session/set_mode",
       "id" => id,
       "params" => %{"sessionId" => @session_id, "modeId" => mode_id}
     }}
  end

  defp set_mode_params(params, id) do
    {:outbound, %{"method" => "session/set_mode", "id" => id, "params" => params}}
  end

  # A config write whose reply reads the session's current models/configOptions
  # back without changing model, effort, or mode. It is side-effect-free only
  # while fast mode is off: it turns fast mode off, so a scenario that pins the
  # fast-mode state must read it through turn/start's serviceTier before it.
  defp readback(opts \\ []), do: set_config_option("fast-mode", "off", opts)

  defp settings_updated(settings, opts \\ []) do
    {:inbound,
     %{
       "method" => "thread/settings/updated",
       "params" => %{
         "threadId" => Keyword.get(opts, :session_id, @session_id),
         "threadSettings" => settings
       }
     }}
  end

  # -- catalog fixtures ------------------------------------------------------

  # Codex app-server v2 `model/list` shape: efforts are
  # `%{"reasoningEffort" => ..., "description" => ...}`.
  defp v2_catalog do
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
        "defaultReasoningEffort" => "medium",
        "inputModalities" => ["text", "image"],
        "additionalSpeedTiers" => [],
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "low", "description" => "Quick"},
          %{"reasoningEffort" => "medium", "description" => "Balanced"},
          %{"reasoningEffort" => "high", "description" => "Deep"}
        ]
      }
    ]
  end

  defp hidden_model do
    %{
      "id" => "gpt-5-internal",
      "model" => "gpt-5-internal-preview",
      "displayName" => "GPT-5 Internal",
      "description" => "Preview model",
      "hidden" => true,
      "defaultReasoningEffort" => "medium",
      "supportedReasoningEfforts" => [
        %{"reasoningEffort" => "medium", "description" => "Balanced"}
      ]
    }
  end

  defp fast_model do
    %{
      "id" => "gpt-5-fast",
      "model" => "gpt-5-fast",
      "displayName" => "GPT-5 Fast",
      "description" => "Priority processing",
      "hidden" => false,
      "defaultReasoningEffort" => "medium",
      "additionalSpeedTiers" => ["fast"],
      "supportedReasoningEfforts" => [
        %{"reasoningEffort" => "medium", "description" => "Balanced"},
        %{"reasoningEffort" => "high", "description" => "Deep"}
      ]
    }
  end

  # -- transcript accessors --------------------------------------------------

  # The ACP session/new result for `session_id` (the first one when nil).
  defp session_result(transcript, session_id \\ nil) do
    transcript
    |> CodexGolden.messages()
    |> Enum.find_value(fn
      %{"result" => %{"sessionId" => id} = result} when is_nil(session_id) or id == session_id ->
        result

      _ ->
        nil
    end)
  end

  defp result_at(transcript, index), do: Enum.at(transcript, index).result
  defp reply_at(transcript, index), do: result_at(transcript, index).reply

  # The result recorded for the ACP request with the given id (step-anchored;
  # ACP ids never collide with app-server ids because they start at 10).
  defp outbound_result(transcript, id) do
    Enum.find(transcript, fn
      %{step: %{kind: :outbound, message: %{"id" => ^id}}} -> true
      _ -> false
    end).result
  end

  defp outbound_reply(transcript, id), do: outbound_result(transcript, id).reply

  # The single app-server write produced by the ACP request with the given id.
  defp outbound_write(transcript, id) do
    [write] = outbound_result(transcript, id).writes
    write
  end

  defp last_reply(transcript), do: CodexGolden.last_result(transcript).reply
  defp last_write(transcript), do: transcript |> CodexGolden.writes() |> List.last()
  defp model_ids(result), do: Enum.map(result["models"]["availableModels"], & &1["modelId"])
  defp option(result, id), do: Enum.find(result["configOptions"], &(&1["id"] == id))
end
