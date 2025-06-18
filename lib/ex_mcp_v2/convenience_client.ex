defmodule ExMCP.ConvenienceClient do
  @moduledoc """
  High-level, convenience-focused MCP client for ExMCP v2.

  This module provides a developer-friendly interface that wraps the robust
  ExMCP.SimpleClient with enhanced convenience features:

  - Simplified API calls with sensible defaults
  - Automatic response normalization 
  - Enhanced error messages with actionable guidance
  - Batch operations and concurrent processing
  - Helper functions for common patterns
  - Smart connection management with URL parsing

  ## Quick Start

      # Simple connection
      {:ok, client} = ExMCP.ConvenienceClient.connect("http://localhost:8080")
      
      # List and call tools
      tools = ExMCP.ConvenienceClient.tools(client)
      result = ExMCP.ConvenienceClient.call(client, "calculator", %{operation: "add", a: 1, b: 2})
      
      # Work with resources
      resources = ExMCP.ConvenienceClient.resources(client)
      content = ExMCP.ConvenienceClient.read(client, "file://data.txt")

  ## Advanced Usage

      # Connection with fallback
      {:ok, client} = ExMCP.ConvenienceClient.connect([
        "http://primary:8080",
        "http://backup:8080", 
        {:stdio, command: "local-server"}
      ])
      
      # Batch operations
      results = ExMCP.ConvenienceClient.batch(client, [
        {:call_tool, "greet", %{name: "Alice"}},
        {:call_tool, "greet", %{name: "Bob"}},
        {:list_resources, %{}}
      ])
  """

  alias ExMCP.SimpleClient
  alias ExMCP.Client.{Response, Error}

  @type client :: pid()
  @type connection_spec :: String.t() | {atom(), keyword()} | [connection_spec()]
  @type batch_operation ::
          {:call_tool, String.t(), map()}
          | {:list_tools, map()}
          | {:list_resources, map()}
          | {:list_prompts, map()}
          | {:read_resource, String.t()}
          | {:get_prompt, String.t(), map()}

  # Connection Management

  @doc """
  Connects to an MCP server with automatic transport detection and fallback.

  ## Examples

      # HTTP connection
      {:ok, client} = ExMCP.ConvenienceClient.connect("http://localhost:8080")
      
      # Stdio connection  
      {:ok, client} = ExMCP.ConvenienceClient.connect({:stdio, command: "my-server"})
      
      # Multiple transports with fallback
      {:ok, client} = ExMCP.ConvenienceClient.connect([
        "http://primary:8080",
        "http://backup:8080",
        {:stdio, command: "fallback-server"}
      ])
      
      # With options
      {:ok, client} = ExMCP.ConvenienceClient.connect("http://localhost:8080", 
        timeout: 10_000,
        retry_attempts: 3
      )
  """
  @spec connect(connection_spec(), keyword()) :: {:ok, client()} | {:error, any()}
  def connect(connection_spec, opts \\ [])

  def connect(url, opts) when is_binary(url) do
    transport_opts = parse_url_to_transport(url, opts)
    start_client(transport_opts, opts)
  end

  def connect({transport_type, transport_opts}, opts) do
    full_opts = Keyword.merge([transport: transport_type], transport_opts ++ opts)
    start_client(full_opts, opts)
  end

  def connect(connection_list, opts) when is_list(connection_list) do
    transports = Enum.map(connection_list, &parse_connection_spec/1)

    full_opts =
      [
        transports: transports,
        fallback_strategy: Keyword.get(opts, :fallback_strategy, :sequential)
      ] ++ opts

    start_client(full_opts, opts)
  end

  @doc """
  Disconnects from the MCP server.
  """
  @spec disconnect(client()) :: :ok
  def disconnect(client) do
    GenServer.stop(client, :normal)
  end

  @doc """
  Gets connection status and health information.
  """
  @spec status(client()) :: {:ok, map()} | {:error, any()}
  def status(client) do
    case SimpleClient.get_status(client) do
      {:ok, status} -> {:ok, normalize_status(status)}
      error -> error
    end
  end

  # Tool Operations

  @doc """
  Lists available tools with normalized response.

  Returns a list of tool maps with standardized fields.
  """
  @spec tools(client(), keyword()) :: [map()] | {:error, any()}
  def tools(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)

    case SimpleClient.list_tools(client, timeout) do
      {:ok, %{"tools" => tools}} ->
        Enum.map(tools, &Response.normalize_tool/1)

      {:ok, response} ->
        Error.format(:unexpected_response, "Expected tools list", response)

      {:error, reason} ->
        Error.format(:tool_list_failed, reason)
    end
  end

  @doc """
  Calls a tool with automatic response normalization.

  ## Examples

      # Simple call
      result = ExMCP.ConvenienceClient.call(client, "calculator", %{op: "add", a: 1, b: 2})
      
      # With options
      result = ExMCP.ConvenienceClient.call(client, "slow_operation", %{data: "..."}, 
        timeout: 30_000,
        normalize: false  # Return raw response
      )
  """
  @spec call(client(), String.t(), map(), keyword()) :: any() | {:error, any()}
  def call(client, tool_name, args \\ %{}, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    normalize? = Keyword.get(opts, :normalize, true)

    case SimpleClient.call_tool(client, tool_name, args, timeout) do
      {:ok, response} when normalize? ->
        Response.normalize_tool_result(response)

      {:ok, response} ->
        response

      {:error, reason} ->
        Error.format(:tool_call_failed, reason, %{tool: tool_name, args: args})
    end
  end

  @doc """
  Finds a tool by name with enhanced search capabilities.

  ## Examples

      # Exact match
      tool = ExMCP.ConvenienceClient.find_tool(client, "calculator")
      
      # Fuzzy search
      tool = ExMCP.ConvenienceClient.find_tool(client, "calc", fuzzy: true)
      
      # Filter by capability
      tools = ExMCP.ConvenienceClient.find_tool(client, nil, has_schema: true)
  """
  @spec find_tool(client(), String.t() | nil, keyword()) :: map() | [map()] | nil
  def find_tool(client, name_or_pattern \\ nil, opts \\ []) do
    case tools(client) do
      tools when is_list(tools) ->
        apply_tool_filters(tools, name_or_pattern, opts)

      error ->
        error
    end
  end

  # Resource Operations

  @doc """
  Lists available resources with normalized response.
  """
  @spec resources(client(), keyword()) :: [map()] | {:error, any()}
  def resources(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)

    case SimpleClient.list_resources(client, timeout) do
      {:ok, %{"resources" => resources}} ->
        Enum.map(resources, &Response.normalize_resource/1)

      {:ok, response} ->
        Error.format(:unexpected_response, "Expected resources list", response)

      {:error, reason} ->
        Error.format(:resource_list_failed, reason)
    end
  end

  @doc """
  Reads a resource with automatic content type detection.

  ## Examples

      # Read text content
      content = ExMCP.ConvenienceClient.read(client, "file://data.txt")
      
      # Read with options
      content = ExMCP.ConvenienceClient.read(client, "file://large.json", 
        timeout: 10_000,
        parse_json: true
      )
  """
  @spec read(client(), String.t(), keyword()) :: any() | {:error, any()}
  def read(client, uri, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 10_000)

    case SimpleClient.read_resource(client, uri, timeout) do
      {:ok, response} ->
        Response.normalize_resource_content(response, opts)

      {:error, reason} ->
        Error.format(:resource_read_failed, reason, %{uri: uri})
    end
  end

  # Prompt Operations

  @doc """
  Lists available prompts with normalized response.
  """
  @spec prompts(client(), keyword()) :: [map()] | {:error, any()}
  def prompts(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)

    case SimpleClient.list_prompts(client, timeout) do
      {:ok, %{"prompts" => prompts}} ->
        Enum.map(prompts, &Response.normalize_prompt/1)

      {:ok, response} ->
        Error.format(:unexpected_response, "Expected prompts list", response)

      {:error, reason} ->
        Error.format(:prompt_list_failed, reason)
    end
  end

  @doc """
  Gets a prompt with automatic message normalization.
  """
  @spec prompt(client(), String.t(), map(), keyword()) :: map() | {:error, any()}
  def prompt(client, prompt_name, args \\ %{}, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)

    case SimpleClient.get_prompt(client, prompt_name, args, timeout) do
      {:ok, response} ->
        Response.normalize_prompt_result(response)

      {:error, reason} ->
        Error.format(:prompt_get_failed, reason, %{prompt: prompt_name, args: args})
    end
  end

  # Batch Operations

  @doc """
  Executes multiple operations in batch for efficiency.

  ## Examples

      results = ExMCP.ConvenienceClient.batch(client, [
        {:call_tool, "greet", %{name: "Alice"}},
        {:call_tool, "greet", %{name: "Bob"}},
        {:list_resources, %{}},
        {:read_resource, "file://config.json"}
      ])
  """
  @spec batch(client(), [batch_operation()], keyword()) :: [any()] | {:error, any()}
  def batch(client, operations, opts \\ []) do
    max_concurrency = Keyword.get(opts, :max_concurrency, 5)
    timeout = Keyword.get(opts, :timeout, 30_000)

    operations
    |> Enum.chunk_every(max_concurrency)
    |> Enum.flat_map(fn chunk ->
      chunk
      |> Enum.map(&Task.async(fn -> execute_operation(client, &1, timeout) end))
      |> Task.await_many(timeout)
    end)
  end

  # Utility Functions

  @doc """
  Tests connectivity to an MCP server without establishing a persistent connection.
  """
  @spec ping(connection_spec(), keyword()) :: :ok | {:error, any()}
  def ping(connection_spec, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)

    case connect(connection_spec, Keyword.put(opts, :test_connection, true)) do
      {:ok, client} ->
        try do
          SimpleClient.health_check(client, timeout)
        after
          disconnect(client)
        end

      error ->
        error
    end
  end

  @doc """
  Gets server information and capabilities.
  """
  @spec server_info(client()) :: {:ok, map()} | {:error, any()}
  def server_info(client) do
    case status(client) do
      {:ok, %{server_info: info}} -> {:ok, Response.normalize_server_info(info)}
      {:ok, _} -> {:error, :no_server_info}
      error -> error
    end
  end

  @doc """
  Executes a function with automatic error formatting.

  Useful for wrapping operations that might fail with enhanced error messages.
  """
  @spec with_error_formatting((-> any()), atom(), map()) :: any()
  def with_error_formatting(fun, error_type, context \\ %{}) do
    try do
      fun.()
    catch
      :exit, reason -> Error.format(error_type, reason, context)
      :error, reason -> Error.format(error_type, reason, context)
    end
  end

  # Private Implementation

  defp start_client(transport_opts, _client_opts) do
    SimpleClient.start_link(transport_opts)
  end

  defp parse_url_to_transport(url, _opts) do
    uri = URI.parse(url)

    case uri.scheme do
      "http" -> [transport: :http, url: url]
      "https" -> [transport: :http, url: url]
      "stdio" -> [transport: :stdio, command: uri.path]
      "file" -> [transport: :stdio, command: uri.path]
      # Default fallback
      _ -> [transport: :http, url: url]
    end
  end

  defp parse_connection_spec(url) when is_binary(url) do
    transport_opts = parse_url_to_transport(url, [])

    transport_mod =
      case Keyword.get(transport_opts, :transport) do
        :http -> ExMCP.Transport.HTTP
        :stdio -> ExMCP.Transport.Stdio
        :sse -> ExMCP.Transport.SSE
        :native -> ExMCP.Transport.Native
      end

    opts = Keyword.delete(transport_opts, :transport)
    {transport_mod, opts}
  end

  defp parse_connection_spec({transport_type, transport_opts}) do
    transport_mod =
      case transport_type do
        :http -> ExMCP.Transport.HTTP
        :stdio -> ExMCP.Transport.Stdio
        :sse -> ExMCP.Transport.SSE
        :native -> ExMCP.Transport.Native
        mod when is_atom(mod) -> mod
      end

    {transport_mod, transport_opts}
  end

  defp normalize_status(status) do
    %{
      connected: status.connection_status == :connected,
      status: status.connection_status,
      server_info: status.server_info,
      reconnect_attempts: status.reconnect_attempts,
      last_activity: status.last_activity,
      pending_requests: status.pending_requests
    }
  end

  defp apply_tool_filters(tools, nil, opts) do
    tools
    |> maybe_filter_by_schema(opts)
    |> maybe_limit_results(opts)
  end

  defp apply_tool_filters(tools, name, opts) do
    fuzzy? = Keyword.get(opts, :fuzzy, false)

    filtered =
      if fuzzy? do
        fuzzy_search_tools(tools, name)
      else
        Enum.find(tools, fn tool -> tool.name == name end)
      end

    case {filtered, Keyword.get(opts, :return_list, false)} do
      {nil, _} -> nil
      {tool, false} when not is_list(tool) -> tool
      {tool, true} when not is_list(tool) -> [tool]
      {tools, _} when is_list(tools) -> maybe_limit_results(tools, opts)
    end
  end

  defp fuzzy_search_tools(tools, pattern) do
    pattern_lower = String.downcase(pattern)

    tools
    |> Enum.filter(fn tool ->
      String.contains?(String.downcase(tool.name), pattern_lower) or
        String.contains?(String.downcase(tool.description || ""), pattern_lower)
    end)
    |> Enum.sort_by(fn tool ->
      # Sort by relevance - exact name match first, then description matches
      cond do
        tool.name == pattern -> 0
        String.starts_with?(String.downcase(tool.name), pattern_lower) -> 1
        String.contains?(String.downcase(tool.name), pattern_lower) -> 2
        true -> 3
      end
    end)
  end

  defp maybe_filter_by_schema(tools, opts) do
    if Keyword.get(opts, :has_schema, false) do
      Enum.filter(tools, fn tool -> not is_nil(tool.input_schema) end)
    else
      tools
    end
  end

  defp maybe_limit_results(tools, opts) do
    case Keyword.get(opts, :limit) do
      nil -> tools
      limit -> Enum.take(tools, limit)
    end
  end

  defp execute_operation(client, {:call_tool, name, args}, timeout) do
    call(client, name, args, timeout: timeout)
  end

  defp execute_operation(client, {:list_tools, _args}, timeout) do
    tools(client, timeout: timeout)
  end

  defp execute_operation(client, {:list_resources, _args}, timeout) do
    resources(client, timeout: timeout)
  end

  defp execute_operation(client, {:list_prompts, _args}, timeout) do
    prompts(client, timeout: timeout)
  end

  defp execute_operation(client, {:read_resource, uri}, timeout) do
    read(client, uri, timeout: timeout)
  end

  defp execute_operation(client, {:get_prompt, name, args}, timeout) do
    prompt(client, name, args, timeout: timeout)
  end
end
