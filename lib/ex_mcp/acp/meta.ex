defmodule ExMCP.ACP.Meta do
  @moduledoc """
  Pure helpers for ACP `_meta` extension placement.
  """

  @spec put_ex_mcp(map(), map()) :: map()
  def put_ex_mcp(result, extensions) when is_map(result) and is_map(extensions) do
    meta =
      case Map.get(result, "_meta", %{}) do
        map when is_map(map) -> map
        _ -> %{}
      end

    ex_mcp =
      case Map.get(meta, "ex_mcp", %{}) do
        map when is_map(map) -> Map.merge(map, extensions)
        _ -> extensions
      end

    Map.put(result, "_meta", Map.put(meta, "ex_mcp", ex_mcp))
  end

  @spec move_extensions(map(), [String.t()]) :: map()
  def move_extensions(result, allowed_keys) when is_map(result) and is_list(allowed_keys) do
    extensions = Map.drop(result, allowed_keys)

    if map_size(extensions) == 0 do
      result
    else
      result
      |> Map.take(allowed_keys)
      |> put_ex_mcp(extensions)
    end
  end
end
