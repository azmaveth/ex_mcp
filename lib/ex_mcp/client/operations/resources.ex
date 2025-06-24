defmodule ExMCP.Client.Operations.Resources do
  @moduledoc """
  Resource operations for ExMCP client.

  This module handles all resource-related operations including listing available
  resources, reading resource content, and managing resource subscriptions.
  """

  alias ExMCP.Client.Types

  @doc """
  Lists all available resources from the MCP server.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :map)

  ## Examples

      {:ok, resources} = ExMCP.Client.Operations.Resources.list_resources(client)
  """
  @spec list_resources(Types.client(), Types.request_opts()) :: Types.mcp_response()
  def list_resources(client, opts \\ []) do
    ExMCP.Client.make_request(client, "resources/list", %{}, opts, 5_000)
  end

  @doc """
  Reads a specific resource by its URI.

  ## Parameters

  - `client` - The MCP client process.
  - `uri` - The URI of the resource to read.

  ## Options

  - `:timeout` - Request timeout (default: 10000)
  - `:format` - Response format (default: :map)

  ## Examples

      {:ok, resource_content} = ExMCP.Client.Operations.Resources.read_resource(client, "mcp://example/resource")
  """
  @spec read_resource(Types.client(), Types.uri(), Types.request_opts()) :: Types.mcp_response()
  def read_resource(client, uri, opts \\ []) do
    params = %{"uri" => uri}
    ExMCP.Client.make_request(client, "resources/read", params, opts, 10_000)
  end

  @doc """
  Subscribes to changes for a specific resource.

  The client will receive notifications when the resource is updated.

  ## Parameters

  - `client` - The MCP client process.
  - `uri` - The URI of the resource to subscribe to.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :map)

  ## Examples

      {:ok, subscription} = ExMCP.Client.Operations.Resources.subscribe_resource(client, "mcp://example/resource")
  """
  @spec subscribe_resource(Types.client(), Types.uri(), Types.request_opts()) ::
          Types.mcp_response()
  def subscribe_resource(client, uri, opts \\ []) do
    params = %{"uri" => uri}
    ExMCP.Client.make_request(client, "resources/subscribe", params, opts, 5_000)
  end

  @doc """
  Unsubscribes from changes for a specific resource.

  ## Parameters

  - `client` - The MCP client process.
  - `uri` - The URI of the resource to unsubscribe from.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Response format (default: :map)

  ## Examples

      {:ok, result} = ExMCP.Client.Operations.Resources.unsubscribe_resource(client, "mcp://example/resource")
  """
  @spec unsubscribe_resource(Types.client(), Types.uri(), Types.request_opts()) ::
          Types.mcp_response()
  def unsubscribe_resource(client, uri, opts \\ []) do
    params = %{"uri" => uri}
    ExMCP.Client.make_request(client, "resources/unsubscribe", params, opts, 5_000)
  end
end
