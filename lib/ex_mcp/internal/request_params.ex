defmodule ExMCP.Internal.RequestParams do
  @moduledoc false

  @spec empty() :: map()
  def empty, do: %{}

  @spec cursor(term()) :: map()
  def cursor(nil), do: %{}
  def cursor(cursor), do: %{"cursor" => cursor}

  @spec cursor_from_opts(keyword()) :: map()
  def cursor_from_opts(opts) do
    opts
    |> Keyword.get(:cursor)
    |> cursor()
  end

  @spec take_cursor(keyword()) :: {map(), keyword()}
  def take_cursor(opts) do
    {cursor, opts} = Keyword.pop(opts, :cursor)
    {cursor(cursor), opts}
  end

  @spec uri(String.t()) :: map()
  def uri(uri), do: %{"uri" => uri}

  @spec named(String.t(), map()) :: map()
  def named(name, arguments \\ %{}) do
    %{"name" => name, "arguments" => arguments}
  end

  @spec completion(term(), term()) :: map()
  def completion(ref, argument) do
    %{"ref" => ref, "argument" => argument}
  end

  @spec with_meta(map(), map() | nil | term()) :: map()
  def with_meta(params, nil), do: params
  def with_meta(params, meta) when is_map(meta), do: Map.put(params, "_meta", meta)
  def with_meta(params, _), do: params

  @spec with_non_empty_meta(map(), map() | nil | term()) :: map()
  def with_non_empty_meta(params, meta) when is_map(meta) and map_size(meta) > 0 do
    Map.put(params, "_meta", meta)
  end

  def with_non_empty_meta(params, _), do: params

  @spec with_opts_meta(map(), keyword()) :: map()
  def with_opts_meta(params, opts) do
    with_meta(params, Keyword.get(opts, :meta))
  end

  @spec with_progress_or_meta(map(), ExMCP.Types.progress_token() | nil | map()) :: map()
  def with_progress_or_meta(params, nil), do: params

  def with_progress_or_meta(params, token) when is_binary(token) or is_integer(token) do
    with_non_empty_meta(params, %{"progressToken" => token})
  end

  def with_progress_or_meta(params, meta) when is_map(meta) do
    with_non_empty_meta(params, meta)
  end
end
