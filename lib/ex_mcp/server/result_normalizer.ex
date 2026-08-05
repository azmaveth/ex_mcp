defmodule ExMCP.Server.ResultNormalizer do
  @moduledoc """
  Shared result and error normalization for every server dispatch path.

  `ExMCP.Server.Dispatch` (handler-process transports and stdio),
  `ExMCP.MessageProcessor.MethodHandlers` (HTTP) and
  `ExMCP.Protocol.RequestProcessor` (DSL servers) all convert handler return
  values into JSON-RPC results. Keeping that conversion here guarantees the
  transports agree on tool-result shape, key stringification and, most
  importantly, on what is safe to send back to a client.

  ## Client-facing error messages

  `error_message/2` never runs `inspect/1` over an arbitrary term into a
  response. Handler-authored detail (a binary reason, or a map/struct with a
  `message` field) is preserved because MCP servers are expected to explain
  themselves; anything else is logged with `Logger.error/1` and replaced by a
  generic message so internal structs, pids, or file paths cannot leak.
  """

  require Logger

  alias ExMCP.Internal.VersionInfo
  alias ExMCP.Protocol.CacheableResult
  alias ExMCP.Transport.HTTP.ToolHeaders

  @server_info_key "io.modelcontextprotocol/serverInfo"

  @doc """
  Recursively converts atom keys to strings.

  Known MCP protocol fields such as `:input_schema`, `:mime_type` and
  `:is_error` are mapped to their lower-camel-case wire names so raw Handler
  implementations may use idiomatic Elixir keys.
  """
  @spec stringify_keys(term()) :: term()
  def stringify_keys(list) when is_list(list), do: Enum.map(list, &stringify_keys/1)

  def stringify_keys(map) when is_map(map) and not is_struct(map) do
    Map.new(map, fn
      {key, value} when is_atom(key) -> {stringify_key(key), stringify_keys(value)}
      {key, value} -> {key, stringify_keys(value)}
    end)
  end

  def stringify_keys(value), do: value

  @doc """
  Applies the result envelope required by the request's protocol era.

  Legacy results are returned unchanged. Modern results receive a
  `resultType` discriminator and result metadata identifying the server.
  Handler-supplied `input_required` (or extension) result types are preserved.
  """
  @spec protocol_result(map(), map(), keyword()) :: map()
  def protocol_result(result, request_context, opts \\ []) when is_map(result) do
    if Map.get(request_context, :era) == :modern do
      result = result |> stringify_keys() |> normalize_tools_list(request_context)
      server_info = Keyword.get(opts, :server_info) || default_server_info()
      result_type = normalize_result_type(Map.get(result, "resultType"))

      result =
        result
        |> Map.put("resultType", result_type)
        |> normalize_cache_hints(request_context, result_type)

      meta =
        case Map.get(result, "_meta") do
          existing when is_map(existing) -> existing
          _other -> %{}
        end

      result
      |> Map.put("_meta", Map.put(meta, @server_info_key, stringify_keys(server_info)))
    else
      result
    end
  end

  defp normalize_tools_list(%{"tools" => tools} = result, %{method: "tools/list"})
       when is_list(tools) do
    tools =
      tools
      |> ToolHeaders.filter_valid_tools()
      |> Enum.sort_by(&tool_sort_key/1)

    Map.put(result, "tools", tools)
  end

  defp normalize_tools_list(result, _request_context), do: result

  # Tool names are required and unique on the wire. The full tool term is a
  # defensive tie-breaker for malformed/duplicate definitions so even those
  # inputs cannot reintroduce handler iteration order into a modern response.
  defp tool_sort_key(%{"name" => name} = tool) when is_binary(name), do: {0, name, tool}
  defp tool_sort_key(tool), do: {1, "", tool}

  defp normalize_cache_hints(result, %{method: method}, "complete") do
    if CacheableResult.cacheable_method?(method) do
      result
      |> normalize_ttl_ms(method)
      |> normalize_cache_scope(method)
    else
      result
    end
  end

  defp normalize_cache_hints(result, %{method: method}, _result_type) do
    if CacheableResult.cacheable_method?(method) do
      Map.drop(result, ["ttlMs", "cacheScope"])
    else
      result
    end
  end

  defp normalize_cache_hints(result, _request_context, _result_type), do: result

  defp normalize_ttl_ms(%{"ttlMs" => ttl_ms} = result, _method)
       when is_integer(ttl_ms) and ttl_ms >= 0,
       do: result

  defp normalize_ttl_ms(%{"ttlMs" => _invalid} = result, method) do
    Logger.warning("Replacing invalid ttlMs on #{method} with the safe default 0")
    Map.put(result, "ttlMs", 0)
  end

  defp normalize_ttl_ms(result, _method), do: Map.put(result, "ttlMs", 0)

  defp normalize_cache_scope(%{"cacheScope" => scope} = result, _method)
       when scope in ["public", "private"],
       do: result

  defp normalize_cache_scope(%{"cacheScope" => scope} = result, _method)
       when scope in [:public, :private],
       do: Map.put(result, "cacheScope", Atom.to_string(scope))

  defp normalize_cache_scope(%{"cacheScope" => _invalid} = result, method) do
    Logger.warning("Replacing invalid cacheScope on #{method} with the safe default private")
    Map.put(result, "cacheScope", "private")
  end

  defp normalize_cache_scope(result, _method), do: Map.put(result, "cacheScope", "private")

  defp normalize_result_type(type) when is_binary(type) and type != "", do: type

  defp normalize_result_type(type) when is_atom(type) and not is_nil(type),
    do: Atom.to_string(type)

  defp normalize_result_type(_type), do: "complete"

  defp default_server_info do
    %{"name" => "ExMCP", "version" => VersionInfo.version()}
  end

  defp stringify_key(:input_schema), do: "inputSchema"
  defp stringify_key(:output_schema), do: "outputSchema"
  defp stringify_key(:mime_type), do: "mimeType"
  defp stringify_key(:uri_template), do: "uriTemplate"
  defp stringify_key(:list_pattern), do: "listPattern"
  defp stringify_key(:is_error), do: "isError"
  defp stringify_key(:is_error?), do: "isError"
  defp stringify_key(:ttl_ms), do: "ttlMs"
  defp stringify_key(:cache_scope), do: "cacheScope"
  defp stringify_key(key), do: Atom.to_string(key)

  @doc """
  Normalizes a `handle_call_tool/3` result into an MCP `tools/call` result.

  Accepts a bare content list, a binary (wrapped as a text content item), or a
  map that already carries `content`.

  ## Options

    * `:wrap_bare_map` - when `true`, a map that carries no `content` key is
      wrapped as `%{"content" => map}` instead of being used as the result
      verbatim. The handler-process transports (`ExMCP.Server.Dispatch`) have
      always done this; the HTTP path has not, and both behaviours are relied
      on by existing servers.
  """
  @spec tool_result(term(), keyword()) :: map()
  def tool_result(result, opts \\ [])

  def tool_result(result, _opts) when is_list(result) do
    %{"content" => stringify_keys(result)}
  end

  def tool_result(result, _opts) when is_binary(result) do
    %{"content" => [%{"type" => "text", "text" => result}]}
  end

  def tool_result(%{content: content} = result, _opts) do
    result
    |> Map.delete(:content)
    |> Map.put("content", stringify_keys(List.wrap(content)))
    |> stringify_keys()
  end

  def tool_result(%{"content" => content} = result, _opts) do
    result
    |> Map.put("content", stringify_keys(List.wrap(content)))
    |> stringify_keys()
  end

  def tool_result(result, opts) when is_map(result) do
    if Keyword.get(opts, :wrap_bare_map, false) do
      %{"content" => stringify_keys(result)}
    else
      stringify_keys(result)
    end
  end

  @doc """
  Builds an MCP tool result that reports a failure through `isError`.
  """
  @spec tool_error_result(term()) :: map()
  def tool_error_result(reason) do
    text = error_message("Tool execution failed", reason)

    %{
      "content" => [%{"type" => "text", "text" => text}],
      "isError" => true
    }
  end

  @doc """
  Builds a paginated list result such as `%{"tools" => [...], "nextCursor" => ...}`.
  """
  @spec paginated(String.t(), list(), String.t() | nil) :: map()
  def paginated(key, entries, next_cursor \\ nil)
  def paginated(key, entries, nil), do: %{key => entries}
  def paginated(key, entries, next_cursor), do: %{key => entries, "nextCursor" => next_cursor}

  @doc """
  Builds a client-safe error message from a handler error reason.

  Detail that the handler clearly authored (a binary, an atom, or a
  `:message` / `"message"` field) is kept. Everything else is logged and
  omitted from the response.
  """
  @spec error_message(String.t(), term()) :: String.t()
  def error_message(prefix, reason) do
    case client_safe_detail(reason) do
      nil ->
        Logger.error("#{prefix}: #{inspect(reason)}")
        prefix

      detail ->
        "#{prefix}: #{detail}"
    end
  end

  @doc """
  Returns the JSON-RPC error code a handler error reason should map to.

  Cursor complaints map to invalid params (`-32602`); everything else uses
  `default`. Codes embedded in the reason are deliberately *not* honoured:
  handlers have historically returned `%{"code" => ...}` maps whose codes do
  not match the transport-level meaning of the failure.
  """
  @spec error_code(term(), integer()) :: integer()
  def error_code(reason, default \\ -32000)
  def error_code("Invalid cursor" <> _, _default), do: -32602
  def error_code(_reason, default), do: default

  defp client_safe_detail(reason) when is_binary(reason), do: reason
  defp client_safe_detail(reason) when is_boolean(reason) or is_nil(reason), do: nil
  defp client_safe_detail(reason) when is_atom(reason), do: Atom.to_string(reason)
  defp client_safe_detail(%{"message" => message}) when is_binary(message), do: message
  defp client_safe_detail(%{message: message}) when is_binary(message), do: message
  defp client_safe_detail(_reason), do: nil
end
