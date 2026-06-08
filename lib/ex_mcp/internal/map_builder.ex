defmodule ExMCP.Internal.MapBuilder do
  @moduledoc """
  Small pipe-friendly helpers for building protocol maps.

  These helpers keep optional-field decisions explicit at call sites while
  avoiding repeated private `maybe_put` functions across MCP modules.
  """

  @spec put_if_present(map(), any(), any()) :: map()
  def put_if_present(map, _key, nil), do: map
  def put_if_present(map, key, value), do: Map.put(map, key, value)

  @spec put_if_truthy(map(), any(), any()) :: map()
  def put_if_truthy(map, _key, value) when value in [nil, false], do: map
  def put_if_truthy(map, key, value), do: Map.put(map, key, value)

  @spec put_unless(map(), any(), any(), any()) :: map()
  def put_unless(map, _key, value, value), do: map
  def put_unless(map, key, value, _skip_value), do: Map.put(map, key, value)
end
