defmodule ExMCP.ACP.Adapters.Pi.Config do
  @moduledoc false

  # Pure model-catalog, thinking-level and config-option helpers for the Pi ACP
  # adapter. Nothing here owns a Port or performs I/O: the update planners
  # return the ACP notifications plus the native RPC payload for the root
  # adapter to deliver.

  alias ExMCP.ACP.AdapterEvents
  alias ExMCP.ACP.Adapters.Pi.RPC

  @thinking_levels ~w(off minimal low medium high xhigh)
  @default_thinking_level "medium"
  @model_config_id "model"
  @thought_level_config_id "thought_level"

  @typedoc """
  The subset of the adapter struct the config helpers read and update.
  """
  @type state :: %{
          required(:available_models) => list(),
          required(:current_model_id) => String.t() | nil,
          required(:thinking_level) => String.t() | nil,
          required(:session_id) => String.t() | nil,
          optional(any()) => any()
        }

  @spec thinking_levels() :: [String.t()]
  def thinking_levels, do: @thinking_levels

  @spec default_thinking_level() :: String.t()
  def default_thinking_level, do: @default_thinking_level

  @spec model_config_id() :: String.t()
  def model_config_id, do: @model_config_id

  @spec thought_level_config_id() :: String.t()
  def thought_level_config_id, do: @thought_level_config_id

  @spec modes() :: [map()]
  def modes do
    Enum.map(@thinking_levels, fn level ->
      %{"id" => level, "name" => "Thinking: #{level}", "description" => nil}
    end)
  end

  @spec runtime_config_options() :: [map()]
  def runtime_config_options do
    [
      %{
        "id" => "auto_compaction",
        "name" => "Auto Compaction",
        "category" => "other",
        "description" => "Automatically compact context when nearly full",
        "type" => "select",
        "currentValue" => "true",
        "options" => boolean_options()
      },
      %{
        "id" => "auto_retry",
        "name" => "Auto Retry",
        "category" => "other",
        "description" => "Automatically retry on transient errors",
        "type" => "select",
        "currentValue" => "true",
        "options" => boolean_options()
      },
      %{
        "id" => "steering_mode",
        "name" => "Steering Mode",
        "category" => "other",
        "description" => "How steering messages are delivered",
        "type" => "select",
        "currentValue" => "all",
        "options" => mode_options()
      },
      %{
        "id" => "follow_up_mode",
        "name" => "Follow-up Mode",
        "category" => "other",
        "description" => "How follow-up messages are delivered",
        "type" => "select",
        "currentValue" => "all",
        "options" => mode_options()
      }
    ]
  end

  @spec model_state(term(), map()) :: map() | nil
  def model_state(data, state_data) do
    available =
      data
      |> catalog_models()
      |> Enum.flat_map(fn model ->
        provider = model["provider"] |> to_string_or_nil()
        id = model["id"] |> to_string_or_nil()

        if provider && id do
          name = model["name"] || id

          [
            %{
              "modelId" => "#{provider}/#{id}",
              "name" => "#{provider}/#{name}",
              "description" => nil
            }
          ]
        else
          []
        end
      end)

    current =
      case state_data["model"] do
        %{"provider" => provider, "id" => id} when is_binary(provider) and is_binary(id) ->
          "#{provider}/#{id}"

        _ ->
          get_in(available, [Access.at(0), "modelId"])
      end

    if available == [] and is_nil(current) do
      nil
    else
      %{"availableModels" => available, "currentModelId" => current || "default"}
    end
  end

  # A `get_available_models` payload is agent-controlled: `models` may be absent,
  # not a list, or hold entries that are not maps. Anything unusable is dropped
  # rather than raised on, so one malformed catalog entry cannot take down the
  # session that requested the catalog.
  @spec catalog_models(term()) :: [map()]
  def catalog_models(data) when is_map(data) do
    case Map.get(data, "models") do
      models when is_list(models) -> Enum.filter(models, &is_map/1)
      _other -> []
    end
  end

  def catalog_models(_data), do: []

  @spec thinking_state(map()) :: map()
  def thinking_state(state_data) do
    current = normalize_thinking_level(state_data["thinkingLevel"])
    %{"availableModes" => modes(), "currentModeId" => current}
  end

  @spec normalize_thinking_level(term()) :: String.t()
  def normalize_thinking_level(level) when level in @thinking_levels, do: level
  def normalize_thinking_level(_level), do: @default_thinking_level

  @spec session_config_options(map() | nil, map()) :: [map()]
  def session_config_options(models, modes) do
    model_options =
      case model_config_option(models) do
        nil -> []
        option -> [option]
      end

    model_options ++ [thinking_config_option(modes)] ++ runtime_config_options()
  end

  @spec config_options_for_state(state()) :: [map()]
  def config_options_for_state(state) do
    models =
      case state.available_models do
        [] ->
          nil

        available when is_list(available) ->
          %{
            "availableModels" => available,
            "currentModelId" =>
              state.current_model_id || get_in(available, [Access.at(0), "modelId"]) ||
                "default"
          }
      end

    modes = %{
      "availableModes" => modes(),
      "currentModeId" => normalize_thinking_level(state.thinking_level)
    }

    session_config_options(models, modes)
  end

  # Model catalogs are advertised by session/new and config-option updates.
  # A synchronous set_config_option result only needs to confirm currentValue;
  # repeating Pi's full multi-provider catalog can exceed bounded ACP clients.
  @spec confirmation_config_options_for_state(state()) :: [map()]
  def confirmation_config_options_for_state(state) do
    state
    |> config_options_for_state()
    |> Enum.map(fn
      %{"id" => @model_config_id} = option -> Map.delete(option, "options")
      option -> option
    end)
  end

  @spec config_options_update(String.t() | nil, state()) :: map()
  def config_options_update(session_id, state) do
    AdapterEvents.config_option_update(session_id, config_options_for_state(state))
  end

  @spec model_config_option(map() | nil) :: map() | nil
  def model_config_option(%{"availableModels" => models} = catalog) when is_list(models) do
    if models == [] do
      nil
    else
      %{
        "id" => @model_config_id,
        "name" => "Model",
        "category" => "model",
        "description" => "Select the model for this session",
        "type" => "select",
        "currentValue" => catalog["currentModelId"],
        "options" =>
          Enum.map(models, fn model ->
            %{
              "value" => model["modelId"],
              "name" => model["name"],
              "description" => model["description"]
            }
          end)
      }
    end
  end

  def model_config_option(_models), do: nil

  @spec thinking_config_option(map()) :: map()
  def thinking_config_option(modes) do
    available = modes["availableModes"] || modes()

    %{
      "id" => @thought_level_config_id,
      "name" => "Thinking",
      "category" => "thought_level",
      "description" => "Set the reasoning effort for this session",
      "type" => "select",
      "currentValue" => normalize_thinking_level(modes["currentModeId"]),
      "options" =>
        Enum.map(available, fn mode ->
          %{
            "value" => mode["id"],
            "name" => mode["name"],
            "description" => mode["description"]
          }
        end)
    }
  end

  @spec resolve_model(term(), term()) ::
          {:ok, String.t(), String.t(), String.t()} | {:error, String.t()}
  def resolve_model(model_id, _available_models)
      when not is_binary(model_id) or model_id == "" do
    {:error, "session/set_model requires modelId"}
  end

  def resolve_model(model_id, available_models) when is_binary(model_id) do
    if String.contains?(model_id, "/") do
      [provider | rest] = String.split(model_id, "/")
      model = Enum.join(rest, "/")
      {:ok, provider, model, model_id}
    else
      case find_available_model(model_id, available_models) do
        {:ok, current_model_id} -> resolve_model(current_model_id, available_models)
        :error -> {:error, "Unknown modelId: #{model_id}"}
      end
    end
  end

  @spec find_available_model(String.t(), term()) :: {:ok, String.t()} | :error
  def find_available_model(model_id, available_models) when is_list(available_models) do
    Enum.find_value(available_models, :error, fn model ->
      advertised_id = model["modelId"]

      cond do
        advertised_id == model_id ->
          {:ok, advertised_id}

        is_binary(advertised_id) and List.last(String.split(advertised_id, "/")) == model_id ->
          {:ok, advertised_id}

        true ->
          false
      end
    end)
  end

  def find_available_model(_model_id, _available_models), do: :error

  @doc """
  Plans a model switch: the ACP notifications to emit, the native RPC payload
  to write, and the updated state. Delivery is the caller's decision.
  """
  @spec set_model_plan(term(), String.t() | nil, state()) ::
          {:ok, [map()], String.t(), state()} | {:error, String.t()}
  def set_model_plan(model_id, session_id_override, state) do
    case resolve_model(model_id, state.available_models) do
      {:ok, provider, resolved_id, current_model_id} ->
        data =
          RPC.line(
            RPC.notification(RPC.method(:set_model), %{
              "provider" => provider,
              "modelId" => resolved_id
            })
          )

        state = %{state | current_model_id: current_model_id}
        session_id = session_id_override || state.session_id

        {:ok, [config_options_update(session_id, state)], data, state}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Plans a thinking-level switch. Returns `:error` for an unknown level so each
  caller can keep its own ACP-visible error wording.
  """
  @spec set_thinking_plan(term(), String.t() | nil, state()) ::
          {:ok, [map()], String.t(), state()} | :error
  def set_thinking_plan(level, session_id_override, state) when level in @thinking_levels do
    state = %{state | thinking_level: level}
    session_id = session_id_override || state.session_id

    messages = [
      AdapterEvents.current_mode_update(session_id, level),
      config_options_update(session_id, state)
    ]

    data = RPC.encode_notification(RPC.method(:set_thinking_level), %{"level" => level})

    {:ok, messages, data, state}
  end

  def set_thinking_plan(_level, _session_id_override, _state), do: :error

  @doc """
  Translates a `session/set_config_option` request into a delivery plan.

  `{:ack, data}` writes `data` and replies `%{}`; `{:ok, messages, data, state}`
  writes `data`, emits `messages`, and replies with the confirmation config
  options.
  """
  @spec update_plan(term(), term(), state()) ::
          {:ack, String.t()} | {:ok, [map()], String.t(), state()} | {:error, String.t()}
  def update_plan("auto_compaction", value, _state) when is_boolean(value) do
    {:ack, RPC.encode_notification(RPC.method(:set_auto_compaction), %{"enabled" => value})}
  end

  def update_plan("auto_compaction", value, state) when value in ["true", "false"],
    do: update_plan("auto_compaction", value == "true", state)

  def update_plan("auto_retry", value, _state) when is_boolean(value) do
    {:ack, RPC.encode_notification(RPC.method(:set_auto_retry), %{"enabled" => value})}
  end

  def update_plan("auto_retry", value, state) when value in ["true", "false"],
    do: update_plan("auto_retry", value == "true", state)

  def update_plan(@model_config_id, value, state), do: set_model_plan(value, nil, state)

  def update_plan(@thought_level_config_id, value, state) do
    case set_thinking_plan(value, nil, state) do
      {:ok, messages, data, state} -> {:ok, messages, data, state}
      :error -> {:error, "Unknown thinking level: #{value}"}
    end
  end

  def update_plan("steering_mode", value, _state) when value in ["all", "one-at-a-time"] do
    {:ack, RPC.encode_notification(RPC.method(:set_steering_mode), %{"mode" => value})}
  end

  def update_plan("follow_up_mode", value, _state) when value in ["all", "one-at-a-time"] do
    {:ack, RPC.encode_notification(RPC.method(:set_follow_up_mode), %{"mode" => value})}
  end

  def update_plan(config_id, _value, _state),
    do: {:error, "Unknown Pi config option: #{config_id}"}

  @spec boolean_options() :: [map()]
  def boolean_options do
    [%{"value" => "true", "name" => "On"}, %{"value" => "false", "name" => "Off"}]
  end

  @spec mode_options() :: [map()]
  def mode_options do
    [
      %{"value" => "all", "name" => "All"},
      %{"value" => "one-at-a-time", "name" => "One at a time"}
    ]
  end

  defp to_string_or_nil(nil), do: nil
  defp to_string_or_nil(""), do: nil
  defp to_string_or_nil(value), do: to_string(value)
end
