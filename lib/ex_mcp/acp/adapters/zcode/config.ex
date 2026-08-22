defmodule ExMCP.ACP.Adapters.ZCode.Config do
  @moduledoc """
  Pure config and mode helpers for the ZCode ACP adapter.

  ZCode has five operational modes (plan, build, edit, auto, yolo) that
  correspond to ACP `SessionMode` IDs. Config options expose model selection
  and thought-level (reasoning effort) control.
  """

  @default_mode "build"

  @modes %{
    "plan" => %{
      "id" => "plan",
      "name" => "Plan",
      "description" => "Planning mode — no tool execution, analysis only."
    },
    "build" => %{
      "id" => "build",
      "name" => "Build",
      "description" => "Standard mode — full tool access with permission prompts."
    },
    "edit" => %{
      "id" => "edit",
      "name" => "Edit",
      "description" => "Auto-accept file edit operations."
    },
    "auto" => %{
      "id" => "auto",
      "name" => "Auto",
      "description" => "Use a model classifier to auto-approve permission prompts."
    },
    "yolo" => %{
      "id" => "yolo",
      "name" => "Yolo",
      "description" => "No permission prompts — all operations allowed without asking."
    }
  }

  @default_thought_level "medium"

  @thought_levels [
    %{"value" => "off", "label" => "Off"},
    %{"value" => "minimal", "label" => "Minimal"},
    %{"value" => "low", "label" => "Low"},
    %{"value" => "medium", "label" => "Medium"},
    %{"value" => "high", "label" => "High"}
  ]

  @spec default_mode() :: String.t()
  def default_mode, do: @default_mode

  @spec default_thought_level() :: String.t()
  def default_thought_level, do: @default_thought_level

  @mode_order ["plan", "build", "edit", "auto", "yolo"]

  @doc "Returns the static mode list for ZCode in canonical order."
  @spec modes() :: [map()]
  def modes, do: Enum.map(@mode_order, &Map.fetch!(@modes, &1))

  @doc "Normalizes a requested mode ID, returning an error for unknown modes."
  @spec normalize_requested_mode(any()) :: {:ok, String.t()} | {:error, String.t()}
  def normalize_requested_mode(mode_id) do
    normalized = normalize_mode_id(mode_id)

    if Map.has_key?(@modes, normalized) do
      {:ok, normalized}
    else
      {:error, "Unsupported ZCode mode: #{inspect(mode_id)}"}
    end
  end

  @doc "Normalizes a mode ID to a string, defaulting to the built-in default."
  @spec normalize_mode_id(any()) :: String.t()
  def normalize_mode_id(nil), do: @default_mode
  def normalize_mode_id(mode_id) when is_binary(mode_id), do: mode_id
  def normalize_mode_id(mode_id), do: to_string(mode_id)

  @doc "Builds the dynamic ACP config options from adapter state."
  @spec config_options(map()) :: [map()]
  def config_options(state) do
    [mode_option(state), model_option(state), thought_level_option(state)]
    |> Enum.reject(&is_nil/1)
  end

  defp mode_option(state) do
    current = Map.get(state, :mode_id) || @default_mode

    %{
      "id" => "mode",
      "name" => "Mode",
      "description" => "Session operational mode",
      "category" => "mode",
      "type" => "select",
      "currentValue" => current,
      "options" =>
        Enum.map(modes(), fn mode ->
          %{"value" => mode["id"], "name" => mode["name"], "description" => mode["description"]}
        end)
    }
  end

  defp model_option(%{models: []}), do: nil
  defp model_option(%{models: nil}), do: nil

  defp model_option(state) do
    models = Map.get(state, :models) || []

    if models == [] do
      nil
    else
      current = current_model_id(state)

      %{
        "id" => "model",
        "name" => "Model",
        "description" => "AI model to use",
        "category" => "model",
        "type" => "select",
        "currentValue" => current,
        "options" => Enum.map(models, &model_select_option/1)
      }
    end
  end

  defp thought_level_option(state) do
    levels = reasoning_levels(state)

    options =
      if levels == [] do
        @thought_levels
      else
        levels
      end

    %{
      "id" => "thought_level",
      "name" => "Thought Level",
      "description" => "Reasoning effort for this session",
      "category" => "thought_level",
      "type" => "select",
      "currentValue" => Map.get(state, :thought_level) || @default_thought_level,
      "options" =>
        Enum.map(options, fn level ->
          %{
            "value" => level["value"] || level[:value],
            "name" => level["label"] || level[:label] || humanize(level["value"] || level[:value])
          }
        end)
    }
  end

  defp model_select_option(model) do
    ref = model["ref"] || model[:ref] || %{}
    provider_id = ref["providerId"] || ref[:providerId] || ""
    model_id = ref["modelId"] || ref[:modelId] || ""
    model_key = if provider_id != "", do: "#{provider_id}/#{model_id}", else: model_id

    %{
      "value" => model_key,
      "name" => model["label"] || model[:label] || model_key,
      "description" => model["description"] || model[:description]
    }
  end

  defp current_model_id(state) do
    case Map.get(state, :current_model) || Map.get(state, :model) do
      %{"providerId" => p, "modelId" => m} -> "#{p}/#{m}"
      %{providerId: p, modelId: m} -> "#{p}/#{m}"
      _ -> "default"
    end
  end

  defp reasoning_levels(state) do
    state
    |> current_model_entry()
    |> case do
      %{"reasoning" => %{"levels" => levels}} when is_list(levels) -> levels
      %{reasoning: %{levels: levels}} when is_list(levels) -> levels
      _ -> []
    end
  end

  defp current_model_entry(state) do
    current = current_model_id(state)
    models = Map.get(state, :models) || []

    Enum.find(models, fn model ->
      model_key(model) == current
    end)
  end

  defp model_key(model) do
    ref = model["ref"] || model[:ref] || %{}
    p = ref["providerId"] || ref[:providerId] || ""
    m = ref["modelId"] || ref[:modelId] || ""
    if p != "", do: "#{p}/#{m}", else: m
  end

  defp humanize(value) when is_binary(value) do
    value
    |> String.split(~r/[_-]+/, trim: true)
    |> Enum.map_join(" ", fn
      <<first::binary-size(1), rest::binary>> -> String.upcase(first) <> rest
      "" -> ""
    end)
  end

  defp humanize(value), do: to_string(value)
end
