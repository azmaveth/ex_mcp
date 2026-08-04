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

  @doc """
  Recursively converts atom keys to strings.

  `:is_error` is mapped to `"isError"` so handlers may use idiomatic Elixir
  keys for the MCP tool-result flag.
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

  defp stringify_key(:is_error), do: "isError"
  defp stringify_key(:is_error?), do: "isError"
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
