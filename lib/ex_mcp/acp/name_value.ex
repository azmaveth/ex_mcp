defmodule ExMCP.ACP.NameValue do
  @moduledoc """
  Pure normalizers for ACP name/value list shapes.
  """

  @spec list(map() | list(), (String.t(), String.t() -> map())) :: [map()]
  def list(values, builder \\ &entry/2)

  def list(values, builder) when is_map(values) do
    Enum.map(values, fn {name, value} -> builder.(to_string(name), to_string(value)) end)
  end

  def list(values, builder) when is_list(values) do
    Enum.map(values, fn
      %{"name" => name, "value" => value} = item ->
        Map.merge(item, %{"name" => to_string(name), "value" => to_string(value)})

      %{name: name, value: value} ->
        builder.(to_string(name), to_string(value))

      {name, value} ->
        builder.(to_string(name), to_string(value))
    end)
  end

  def list(_values, _builder), do: []

  @spec map(map() | list()) :: map()
  def map(values) when is_map(values) do
    Map.new(values, fn {name, value} -> {to_string(name), to_string(value)} end)
  end

  def map(values) when is_list(values) do
    Map.new(values, fn
      %{"name" => name, "value" => value} -> {to_string(name), to_string(value)}
      %{name: name, value: value} -> {to_string(name), to_string(value)}
      {name, value} -> {to_string(name), to_string(value)}
    end)
  end

  def map(_values), do: %{}

  @spec entry(String.t(), String.t()) :: map()
  def entry(name, value), do: %{"name" => name, "value" => value}
end
