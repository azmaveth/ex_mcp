defmodule ExMCP.Server.DSL.Result do
  @moduledoc """
  Response helpers for the modern server DSL.

  When a module uses `ExMCP.Server.DSL`, this module is aliased as `ToolResult`.
  Outside DSL modules, call the functions via `ExMCP.Server.DSL.Result`.

  ## Building results

      ToolResult.text("hello")
      ToolResult.error("something went wrong")
      ToolResult.structured("done", %{count: 1})

  ## Normalization

  Handlers may also return plain values; the DSL normalizes them:

  * binaries and `%{text: ...}` → text content
  * `%{content: ...}` maps → used as tool results
  * `{:ok, result}` / `{:ok, result, state}` / `{:error, reason}` tuples
  """

  @doc """
  Builds a text tool result.
  """
  @spec text(String.t()) :: map()
  def text(text) when is_binary(text), do: %{content: [%{type: "text", text: text}]}

  @doc """
  Builds an error tool result.
  """
  @spec error(any()) :: map()
  def error(reason) when is_binary(reason),
    do: %{content: [%{type: "text", text: reason}], isError: true}

  def error(reason), do: reason |> inspect() |> error()

  @doc """
  Builds a tool result with both text and structured content.
  """
  @spec structured(String.t(), map()) :: map()
  def structured(text, data) when is_binary(text) and is_map(data) do
    %{content: [%{type: "text", text: text}], structuredContent: data}
  end

  @doc false
  def normalize_tool({:ok, result}, state), do: {:ok, normalize_tool_result(result), state}

  def normalize_tool({:ok, result, new_state}, _state),
    do: {:ok, normalize_tool_result(result), new_state}

  def normalize_tool({:error, reason}, state), do: {:ok, error(reason), state}
  def normalize_tool({:error, reason, new_state}, _state), do: {:ok, error(reason), new_state}

  @doc false
  def normalize_tool_result(result) when is_binary(result), do: text(result)
  def normalize_tool_result(text: value) when is_binary(value), do: text(value)
  def normalize_tool_result(%{text: value}) when is_binary(value), do: text(value)
  def normalize_tool_result(%{content: _} = result), do: normalize_structured_key(result)

  def normalize_tool_result(%{structuredContent: _} = result),
    do: Map.put_new(result, :content, [])

  def normalize_tool_result(%{structuredOutput: _} = result), do: normalize_structured_key(result)
  def normalize_tool_result(result), do: result |> inspect() |> text()

  @doc false
  def normalize_resource({:ok, result}, uri, mime_type, state) do
    {:ok, normalize_resource_result(result, uri, mime_type), state}
  end

  def normalize_resource({:ok, result, new_state}, uri, mime_type, _state) do
    {:ok, normalize_resource_result(result, uri, mime_type), new_state}
  end

  def normalize_resource({:error, reason}, _uri, _mime_type, state), do: {:error, reason, state}

  def normalize_resource({:error, reason, new_state}, _uri, _mime_type, _state),
    do: {:error, reason, new_state}

  @doc false
  def normalize_prompt({:ok, result}, state), do: {:ok, normalize_prompt_result(result), state}

  def normalize_prompt({:ok, result, new_state}, _state),
    do: {:ok, normalize_prompt_result(result), new_state}

  def normalize_prompt({:error, reason}, state), do: {:error, reason, state}
  def normalize_prompt({:error, reason, new_state}, _state), do: {:error, reason, new_state}

  defp normalize_structured_key(%{structuredOutput: structured_output} = result) do
    result
    |> Map.put(:structuredContent, structured_output)
    |> Map.delete(:structuredOutput)
    |> Map.put_new(:content, [])
  end

  defp normalize_structured_key(result), do: result

  defp normalize_resource_result(result, uri, mime_type) when is_binary(result) do
    %{uri: uri, text: result}
    |> put_optional(:mimeType, mime_type)
  end

  defp normalize_resource_result(%{text: _} = result, uri, mime_type) do
    result
    |> Map.put_new(:uri, uri)
    |> put_optional(:mimeType, mime_type)
  end

  defp normalize_resource_result(%{blob: _} = result, uri, mime_type) do
    result
    |> Map.put_new(:uri, uri)
    |> put_optional(:mimeType, mime_type)
  end

  defp normalize_resource_result(result, _uri, _mime_type), do: result

  defp normalize_prompt_result(%{messages: _} = result), do: result
  defp normalize_prompt_result(text: text) when is_binary(text), do: normalize_prompt_result(text)
  defp normalize_prompt_result(messages) when is_list(messages), do: %{messages: messages}

  defp normalize_prompt_result(text) when is_binary(text) do
    %{messages: [%{role: "user", content: %{type: "text", text: text}}]}
  end

  defp put_optional(map, _key, nil), do: map
  defp put_optional(map, key, value), do: Map.put(map, key, value)
end
