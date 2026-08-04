defmodule ExMCP.Client.Operations.Tools do
  @moduledoc """
  Tool operations for ExMCP client.

  This module handles all tool-related operations including listing available tools,
  calling specific tools, and finding tools by name or pattern.
  """

  alias ExMCP.Client.Types
  alias ExMCP.Internal.RequestParams
  alias ExMCP.Response

  @doc """
  Lists all available tools from the MCP server.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :struct)

  ## Examples

      {:ok, tools} = ExMCP.Client.Operations.Tools.list_tools(client)
      {:ok, tools} = ExMCP.Client.Operations.Tools.list_tools(client, timeout: 10_000)
  """
  @spec list_tools(Types.client(), Types.request_opts()) :: Types.mcp_response()
  def list_tools(client, opts \\ []) do
    ExMCP.Client.make_request(
      client,
      "tools/list",
      RequestParams.cursor_from_opts(opts),
      opts,
      5_000
    )
  end

  @doc """
  Alias for `list_tools/2`.
  """
  @spec tools(Types.client(), Types.request_opts()) :: Types.mcp_response()
  def tools(client, opts \\ []) do
    list_tools(client, opts)
  end

  @doc """
  Calls a tool on the MCP server.

  ## Options

  - `:timeout` - Request timeout (default: 30000)
  - `:format` - Response format (default: :struct)
  - `:progress_token` - Token sent as `_meta.progressToken`, which the server
    handler receives in its arguments map and can use to emit
    `notifications/progress` for long-running work
  - `:meta` - Additional `_meta` entries; merged with `:progress_token`

  ## Examples

      ExMCP.Client.Operations.Tools.call_tool(client, "my_tool", %{arg1: "value"})
      ExMCP.Client.Operations.Tools.call_tool(client, "my_tool", %{arg1: "value"}, timeout: 60_000)

      ExMCP.Client.Operations.Tools.call_tool(client, "slow_tool", %{},
        progress_token: "job-42"
      )
  """
  @spec call_tool(
          Types.client(),
          Types.tool_name(),
          Types.tool_arguments(),
          Types.request_opts_or_timeout()
        ) :: Types.mcp_response()
  def call_tool(client, tool_name, arguments, timeout_or_opts \\ 30_000)

  def call_tool(client, tool_name, arguments, timeout) when is_integer(timeout) do
    call_tool(client, tool_name, arguments, timeout: timeout)
  end

  def call_tool(client, tool_name, arguments, opts) when is_list(opts) do
    params =
      tool_name
      |> RequestParams.named(arguments)
      |> RequestParams.with_opts_meta(opts)

    # Add tool_name to opts for proper Response struct construction
    enhanced_opts = Keyword.put(opts, :tool_name, tool_name)
    ExMCP.Client.make_request(client, "tools/call", params, enhanced_opts, 30_000)
  end

  @doc """
  Finds a tool by name or pattern.

  If `name_or_pattern` is nil, it returns the first tool from the list.

  Handles both response formats: the default `:struct` format
  (`%ExMCP.Response{}`) and the raw `:map` format.

  ## Options

  - `:fuzzy` - If true, performs a fuzzy search (default: false)
  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :struct)

  ## Examples

      {:ok, tool} = ExMCP.Client.Operations.Tools.find_tool(client, "my_tool")
      {:ok, tool} = ExMCP.Client.Operations.Tools.find_tool(client, "tool", fuzzy: true)
  """
  @spec find_tool(Types.client(), String.t() | nil, Types.request_opts()) ::
          {:ok, map()} | {:error, :not_found} | {:error, any()}
  def find_tool(client, name_or_pattern \\ nil, opts \\ []) do
    case list_tools(client, opts) do
      {:ok, %Response{tools: tools}} ->
        do_find_matching_tool(List.wrap(tools), name_or_pattern, opts)

      {:ok, %{"tools" => tools}} when is_list(tools) ->
        do_find_matching_tool(tools, name_or_pattern, opts)

      {:ok, %{tools: tools}} when is_list(tools) ->
        do_find_matching_tool(tools, name_or_pattern, opts)

      {:ok, _other} ->
        {:error, :not_found}

      error ->
        error
    end
  end

  # Private helpers

  defp do_find_matching_tool(tools, nil, _opts) do
    case List.first(tools) do
      nil -> {:error, :not_found}
      tool -> {:ok, tool}
    end
  end

  defp do_find_matching_tool(tools, name, opts) do
    fuzzy? = Keyword.get(opts, :fuzzy, false)

    result =
      if fuzzy? do
        Enum.find(tools, fn tool ->
          String.contains?(
            String.downcase(tool_name(tool) || ""),
            String.downcase(name)
          )
        end)
      else
        Enum.find(tools, &(tool_name(&1) == name))
      end

    case result do
      nil -> {:error, :not_found}
      tool -> {:ok, tool}
    end
  end

  # Tool entries carry string keys in :map format and may carry atom keys
  # after struct normalization.
  defp tool_name(%{"name" => name}), do: name
  defp tool_name(%{name: name}), do: name
  defp tool_name(_tool), do: nil
end
