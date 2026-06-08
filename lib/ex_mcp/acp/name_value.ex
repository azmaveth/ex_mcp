defmodule ExMCP.ACP.NameValue do
  @moduledoc """
  Pure normalizers for ACP name/value list shapes.
  """

  @spec list(map() | list(), (String.t(), String.t() -> map())) :: [map()]
  def list(values, builder \\ &entry/2) do
    ExMCP.Internal.NameValue.list(values, builder)
  end

  @doc "Normalizes a name/value list or map into a map."
  @spec map(map() | list()) :: map()
  defdelegate map(values), to: ExMCP.Internal.NameValue

  @doc "Builds a standard ACP name/value entry."
  @spec entry(String.t(), String.t()) :: map()
  defdelegate entry(name, value), to: ExMCP.Internal.NameValue
end
