defmodule ExMCP.Internal.LogSummary do
  @moduledoc false

  @spec describe(term()) :: String.t()
  def describe(%_{} = struct), do: "struct(#{inspect(struct.__struct__)})"
  def describe(value) when is_map(value), do: "map(size=#{map_size(value)})"
  def describe(value) when is_binary(value), do: "binary(bytes=#{byte_size(value)})"
  def describe(value) when is_list(value), do: describe_list(value)
  def describe(value) when is_tuple(value), do: "tuple(size=#{tuple_size(value)})"
  def describe(value) when is_atom(value), do: "atom"
  def describe(value) when is_integer(value), do: "integer"
  def describe(value) when is_float(value), do: "float"
  def describe(value) when is_pid(value), do: "pid"
  def describe(value) when is_reference(value), do: "reference"
  def describe(value) when is_function(value), do: "function"
  def describe(_value), do: "term"

  @spec fingerprint(term()) :: String.t()
  def fingerprint(value) do
    value
    |> :erlang.term_to_binary()
    |> then(&:crypto.hash(:sha256, &1))
    |> binary_part(0, 8)
    |> Base.encode16(case: :lower)
  end

  defp describe_list(value) do
    if List.ascii_printable?(value) do
      "charlist(bytes=#{IO.iodata_length(value)})"
    else
      "list"
    end
  rescue
    _exception -> "list"
  end
end
