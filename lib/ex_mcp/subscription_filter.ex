defmodule ExMCP.SubscriptionFilter do
  @moduledoc false

  @filter_keys [
    "toolsListChanged",
    "promptsListChanged",
    "resourcesListChanged",
    "resourceSubscriptions",
    "taskIds"
  ]

  @spec keys() :: [String.t()]
  def keys, do: @filter_keys

  @spec normalize(map()) :: {:ok, map()} | {:error, atom()}
  def normalize(filter) when is_map(filter) do
    Enum.reduce_while(filter, {:ok, %{}, MapSet.new()}, fn {key, value}, {:ok, acc, seen} ->
      case normalize_key(key) do
        {:ok, key} when key in @filter_keys ->
          if MapSet.member?(seen, key) do
            {:halt, {:error, :invalid_subscription_filter}}
          else
            case normalize_value(key, value) do
              {:ok, nil} -> {:cont, {:ok, acc, MapSet.put(seen, key)}}
              {:ok, value} -> {:cont, {:ok, Map.put(acc, key, value), MapSet.put(seen, key)}}
              {:error, reason} -> {:halt, {:error, reason}}
            end
          end

        _unknown ->
          {:halt, {:error, :unknown_subscription_filter}}
      end
    end)
    |> case do
      {:ok, normalized, _seen} -> {:ok, normalized}
      error -> error
    end
  end

  def normalize(_filter), do: {:error, :subscription_filter_required}

  @spec subset?(map(), map()) :: boolean()
  def subset?(acknowledged, requested)
      when is_map(acknowledged) and is_map(requested) do
    Enum.all?(acknowledged, fn
      {"resourceSubscriptions", uris} when is_list(uris) ->
        Enum.all?(uris, &(&1 in Map.get(requested, "resourceSubscriptions", [])))

      {"taskIds", task_ids} when is_list(task_ids) ->
        Enum.all?(task_ids, &(&1 in Map.get(requested, "taskIds", [])))

      {key, true} when key in @filter_keys ->
        Map.get(requested, key) == true

      _invalid ->
        false
    end)
  end

  def subset?(_acknowledged, _requested), do: false

  defp normalize_key(key) when is_binary(key), do: {:ok, key}
  defp normalize_key(key) when is_atom(key), do: {:ok, Atom.to_string(key)}
  defp normalize_key(_key), do: :error

  defp normalize_value(key, true) when key not in ["resourceSubscriptions", "taskIds"],
    do: {:ok, true}

  defp normalize_value(key, false) when key not in ["resourceSubscriptions", "taskIds"],
    do: {:ok, nil}

  defp normalize_value("resourceSubscriptions", uris) when is_list(uris) do
    if Enum.all?(uris, &(is_binary(&1) and byte_size(&1) > 0)),
      do: {:ok, Enum.uniq(uris)},
      else: {:error, :invalid_resource_subscription}
  end

  defp normalize_value("taskIds", task_ids) when is_list(task_ids) do
    if Enum.all?(task_ids, &(is_binary(&1) and byte_size(&1) > 0)),
      do: {:ok, Enum.uniq(task_ids)},
      else: {:error, :invalid_task_subscription}
  end

  defp normalize_value(_key, _value), do: {:error, :invalid_subscription_filter}
end
