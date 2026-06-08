defmodule ExMCP.Internal.MapBuilder do
  @moduledoc """
  Small pipe-friendly helpers for building protocol maps.

  These helpers keep optional-field decisions explicit at call sites while
  avoiding repeated private `maybe_put` functions across MCP modules.
  """

  @spec put_if_present(map(), any(), any()) :: map()
  defdelegate put_if_present(map, key, value), to: ExMCP.Internal.Maps, as: :put_present

  @spec put_if_truthy(map(), any(), any()) :: map()
  defdelegate put_if_truthy(map, key, value), to: ExMCP.Internal.Maps, as: :put_truthy

  @spec put_unless(map(), any(), any(), any()) :: map()
  defdelegate put_unless(map, key, value, skip_value), to: ExMCP.Internal.Maps
end
