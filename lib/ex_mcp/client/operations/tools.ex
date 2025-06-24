defmodule ExMCP.Client.Operations.Tools do
  @moduledoc """
  Tool operations for ExMCP client.

  This module handles all tool-related operations including listing available tools,
  calling specific tools, and finding tools by name or pattern.
  """

  alias ExMCP.Client.Types

  @doc """
  Lists all available tools from the MCP server.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :map)

  ## Examples

      {:ok, tools} = ExMCP.Client.Operations.Tools.list_tools(client)
      {:ok, tools} = ExMCP.Client.Operations.Tools.list_tools(client, timeout: 10_000)
  """
  @spec list_tools(Types.client(), Types.request_opts()) :: Types.mcp_response()
  def list_tools(client, opts \\ []) do
    ExMCP.Client.make_request(client, "tools/list", %{}, opts, 5_000)
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
  - `:format` - Response format (default: :map)

  ## Examples

      ExMCP.Client.Operations.Tools.call_tool(client, "my_tool", %{arg1: "value"})
      ExMCP.Client.Operations.Tools.call_tool(client, "my_tool", %{arg1: "value"}, timeout: 60_000)
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
    params = %{
      "name" => tool_name,
      "arguments" => arguments
    }

    ExMCP.Client.make_request(client, "tools/call", params, opts, 30_000)
  end

  @doc """
  Finds a tool by name or pattern.

  If `name_or_pattern` is nil, it returns the first tool from the list.

  ## Options

  - `:fuzzy` - If true, performs a fuzzy search (default: false)
  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :map)

  ## Examples

      {:ok, tool} = ExMCP.Client.Operations.Tools.find_tool(client, "my_tool")
      {:ok, tool} = ExMCP.Client.Operations.Tools.find_tool(client, "tool", fuzzy: true)
  """
  @spec find_tool(Types.client(), String.t() | nil, Types.request_opts()) ::
          {:ok, map()} | {:error, :tool_not_found | any()}
  def find_tool(client, name_or_pattern \\ nil, opts \\ []) do
    case list_tools(client, opts) do
      {:ok, %{"tools" => tools}} ->
        do_find_matching_tool(tools, name_or_pattern, opts)

      error ->
        error
    end
  end

  # Private helpers

  defp do_find_matching_tool(tools, nil, _opts), do: {:ok, List.first(tools)}

  defp do_find_matching_tool(tools, name, opts) do
    fuzzy? = Keyword.get(opts, :fuzzy, false)

    result =
      if fuzzy? do
        Enum.find(tools, fn tool ->
          String.contains?(
            String.downcase(tool["name"] || ""),
            String.downcase(name)
          )
        end)
      else
        Enum.find(tools, &(&1["name"] == name))
      end

    case result do
      nil -> {:error, :tool_not_found}
      tool -> {:ok, tool}
    end
  end
end
