defmodule ExMCP.Client.Operations.Prompts do
  @moduledoc """
  Prompt operations for ExMCP client.

  This module handles all prompt-related operations including listing available
  prompts and retrieving specific prompts with their arguments.
  """

  alias ExMCP.Client.Types

  @doc """
  Lists all available prompts from the MCP server.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :map)

  ## Examples

      {:ok, prompts} = ExMCP.Client.Operations.Prompts.list_prompts(client)
  """
  @spec list_prompts(Types.client(), Types.request_opts()) :: Types.mcp_response()
  def list_prompts(client, opts \\ []) do
    ExMCP.Client.make_request(client, "prompts/list", %{}, opts, 5_000)
  end

  @doc """
  Gets a specific prompt from the MCP server, optionally with arguments.

  ## Parameters

  - `client` - The client process.
  - `prompt_name` - The name of the prompt to retrieve.
  - `arguments` - A map of arguments to pass to the prompt.
  - `opts` - A keyword list of options.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :map)

  ## Examples

      {:ok, prompt} = ExMCP.Client.Operations.Prompts.get_prompt(client, "my_prompt")
      {:ok, prompt} = ExMCP.Client.Operations.Prompts.get_prompt(client, "my_prompt", %{"name" => "World"})
  """
  @spec get_prompt(Types.client(), String.t(), map(), Types.request_opts()) ::
          Types.mcp_response()
  def get_prompt(client, prompt_name, arguments \\ %{}, opts \\ []) do
    params = %{
      "name" => prompt_name,
      "arguments" => arguments
    }

    ExMCP.Client.make_request(client, "prompts/get", params, opts, 5_000)
  end
end
