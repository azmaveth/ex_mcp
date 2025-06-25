defmodule ExMCP.MessageProcessor.Handlers do
  @moduledoc """
  Unified message handlers for all MCP methods.

  This module contains the business logic for handling each MCP method,
  extracted from the duplicate handler implementations in MessageProcessor.
  Each handler follows a consistent pattern and can be called from any
  dispatch mode (direct, genserver, or handler).
  """

  alias ExMCP.MessageProcessor.Conn

  require Logger

  @type conn :: Conn.t()
  @type handler :: module() | pid()
  @type mode :: :direct | :genserver | :handler

  @doc """
  Handles ping requests.
  """
  @spec handle_ping(conn, any()) :: conn
  def handle_ping(conn, _id) do
    # TODO: Implement proper protocol response
    conn
  end

  @doc """
  Handles initialize requests.
  """
  @spec handle_initialize(conn, handler, mode, map(), any(), map()) :: conn
  def handle_initialize(conn, _handler, _mode, _params, _id, _server_info) do
    # TODO: Implement proper initialize handler with protocol integration
    conn
  end

  @doc """
  Handles tools/list requests.
  """
  @spec handle_tools_list(conn, handler, mode, map(), any()) :: conn
  def handle_tools_list(conn, handler, mode, params, id) do
    handle_standard_request(conn, handler, mode, :handle_list_tools, params, id)
  end

  @doc """
  Handles tools/call requests.
  """
  @spec handle_tools_call(conn, handler, mode, map(), any()) :: conn
  def handle_tools_call(conn, handler, mode, params, id) do
    # Convert string keys to atoms for tool calls
    atomized_params = atomize_params(params)
    handle_standard_request(conn, handler, mode, :handle_call_tool, atomized_params, id)
  end

  @doc """
  Handles resources/list requests.
  """
  @spec handle_resources_list(conn, handler, mode, map(), any()) :: conn
  def handle_resources_list(conn, handler, mode, params, id) do
    handle_standard_request(conn, handler, mode, :handle_list_resources, params, id)
  end

  @doc """
  Handles resources/read requests.
  """
  @spec handle_resources_read(conn, handler, mode, map(), any()) :: conn
  def handle_resources_read(conn, handler, mode, params, id) do
    handle_standard_request(conn, handler, mode, :handle_read_resource, params, id)
  end

  @doc """
  Handles resources/subscribe requests.
  """
  @spec handle_resources_subscribe(conn, handler, mode, map(), any()) :: conn
  def handle_resources_subscribe(conn, handler, mode, params, id) do
    handle_standard_request(conn, handler, mode, :handle_subscribe_resource, params, id)
  end

  @doc """
  Handles resources/unsubscribe requests.
  """
  @spec handle_resources_unsubscribe(conn, handler, mode, map(), any()) :: conn
  def handle_resources_unsubscribe(conn, handler, mode, params, id) do
    handle_standard_request(conn, handler, mode, :handle_unsubscribe_resource, params, id)
  end

  @doc """
  Handles prompts/list requests.
  """
  @spec handle_prompts_list(conn, handler, mode, map(), any()) :: conn
  def handle_prompts_list(conn, handler, mode, params, id) do
    handle_standard_request(conn, handler, mode, :handle_list_prompts, params, id)
  end

  @doc """
  Handles prompts/get requests.
  """
  @spec handle_prompts_get(conn, handler, mode, map(), any()) :: conn
  def handle_prompts_get(conn, handler, mode, params, id) do
    handle_standard_request(conn, handler, mode, :handle_get_prompt, params, id)
  end

  @doc """
  Handles completion/complete requests.
  """
  @spec handle_completion_complete(conn, handler, mode, map(), any()) :: conn
  def handle_completion_complete(conn, handler, mode, params, id) do
    handle_standard_request(conn, handler, mode, :handle_complete, params, id)
  end

  @doc """
  Handles custom/unknown method requests.
  """
  @spec handle_custom_method(conn, handler, mode, String.t(), map(), any()) :: conn
  def handle_custom_method(conn, _handler, _mode, _method, _params, _id) do
    # TODO: Implement custom method handling with proper protocol integration
    conn
  end

  # Private helper functions

  defp handle_standard_request(conn, _handler, _mode, _callback, _params, _id) do
    # TODO: Implement standard request handling with proper protocol integration
    conn
  end

  defp atomize_params(params) when is_map(params) do
    Map.new(params, fn
      {key, value} when is_binary(key) ->
        {String.to_existing_atom(key), atomize_params(value)}

      {key, value} ->
        {key, atomize_params(value)}
    end)
  rescue
    ArgumentError ->
      # If atom doesn't exist, keep string keys
      params
  end

  defp atomize_params(value), do: value
end
