defmodule ExMCP.ACP.Maps do
  @moduledoc """
  Small pure map helpers used by ACP protocol, client, agent, and adapters.

  The functions intentionally work with string and atom keys because ACP data
  crosses JSON boundaries while local Elixir call sites often use atom keys.
  """

  @spec get(map() | nil, String.t() | atom()) :: any()
  def get(map, key) when is_map(map) and is_binary(key) do
    case Map.fetch(map, key) do
      {:ok, value} -> value
      :error -> Map.get(map, existing_atom(key))
    end
  end

  def get(map, key) when is_map(map) and is_atom(key) do
    case Map.fetch(map, key) do
      {:ok, value} -> value
      :error -> Map.get(map, Atom.to_string(key))
    end
  end

  def get(_map, _key), do: nil

  @spec truthy?(any()) :: boolean()
  def truthy?(nil), do: false
  def truthy?(false), do: false
  def truthy?(_value), do: true

  @doc "Puts `value` into `map` when the value is not `nil`."
  @spec put_present(map(), any(), any()) :: map()
  defdelegate put_present(map, key, value), to: ExMCP.Internal.Maps

  @doc "Puts `value` into `map` when the value is present and not empty."
  @spec put_non_empty(map(), any(), any()) :: map()
  defdelegate put_non_empty(map, key, value), to: ExMCP.Internal.Maps

  @doc "Puts `value` into `map` unless it equals `skip_value`."
  @spec put_unless(map(), any(), any(), any()) :: map()
  defdelegate put_unless(map, key, value, skip_value), to: ExMCP.Internal.Maps

  @spec stringify_keys(map()) :: map()
  def stringify_keys(map) when is_map(map) do
    Map.new(map, fn
      {key, value} when is_atom(key) -> {camelize_atom(key), value}
      {key, value} -> {key, value}
    end)
  end

  @spec camelize_atom(atom()) :: String.t()
  def camelize_atom(:stopReason), do: "stopReason"

  def camelize_atom(atom) when is_atom(atom) do
    atom
    |> Atom.to_string()
    |> Macro.camelize()
    |> decapitalize()
  end

  @spec decapitalize(String.t()) :: String.t()
  def decapitalize(<<first::utf8, rest::binary>>) do
    String.downcase(<<first::utf8>>) <> rest
  end

  def decapitalize(value), do: value

  defp existing_atom(key) do
    String.to_existing_atom(key)
  rescue
    ArgumentError -> nil
  end
end
