defmodule ExMCP.MessageProcessor.Handlers do
  @moduledoc """
  Unified message handlers for all MCP methods.

  This module contains the business logic for handling each MCP method,
  extracted from the duplicate handler implementations in MessageProcessor.
  Each handler follows a consistent pattern and can be called from any
  dispatch mode (direct, genserver, or handler).
  """

  alias ExMCP.MessageProcessor.Conn
  alias ExMCP.Protocol
  alias ExMCP.Server.Handler
  alias ExMCP.Types

  require Logger

  @type conn :: Conn.t()
  @type handler :: module() | pid()
  @type mode :: :direct | :genserver | :handler

  @doc """
  Handles ping requests.
  """
  @spec handle_ping(conn, any()) :: conn
  def handle_ping(conn, id) do
    response = Protocol.response(id, %{})
    Conn.put_response(conn, response)
  end

  @doc """
  Handles initialize requests.
  """
  @spec handle_initialize(conn, handler, mode, map(), any(), map()) :: conn
  def handle_initialize(conn, handler, mode, params, id, server_info) do
    try do
      result = call_handler(handler, mode, :handle_initialize, [params], conn.state)

      case result do
        {:ok, result, new_state} ->
          # Merge server info with result
          merged_result = Map.merge(result, server_info)
          response = Protocol.response(id, merged_result)

          conn
          |> Conn.put_response(response)
          |> Map.put(:state, new_state)

        {:error, reason} ->
          error_response = Protocol.error_response(id, reason)
          Conn.put_response(conn, error_response)
      end
    rescue
      e ->
        Logger.error("Error in initialize handler: #{inspect(e)}")
        error_response = Protocol.error_response(id, "Internal server error")
        Conn.put_response(conn, error_response)
    end
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
  def handle_custom_method(conn, handler, mode, method, params, id) do
    # Check if handler implements the custom method
    handler_module = get_handler_module(handler, mode)

    if function_exported?(handler_module, :handle_custom_request, 3) do
      handle_standard_request(
        conn,
        handler,
        mode,
        :handle_custom_request,
        %{method: method, params: params},
        id
      )
    else
      error = Protocol.error(:method_not_found, "Unknown method: #{method}")
      response = Protocol.error_response(id, error)
      Conn.put_response(conn, response)
    end
  end

  # Private helper functions

  defp handle_standard_request(conn, handler, mode, callback, params, id) do
    try do
      result = call_handler(handler, mode, callback, [params], conn.state)

      case result do
        {:ok, result, new_state} ->
          response = Protocol.response(id, result)

          conn
          |> Conn.put_response(response)
          |> Map.put(:state, new_state)

        {:error, reason} ->
          error_response = Protocol.error_response(id, reason)
          Conn.put_response(conn, error_response)
      end
    rescue
      e ->
        Logger.error("Error in #{callback} handler: #{inspect(e)}")
        error_response = Protocol.error_response(id, "Internal server error")
        Conn.put_response(conn, error_response)
    end
  end

  defp call_handler(handler, :direct, callback, args, state) do
    # Direct call to handler module
    apply(handler, callback, args ++ [state])
  end

  defp call_handler(handler, :genserver, callback, args, _state) do
    # GenServer call
    GenServer.call(handler, {callback, args})
  end

  defp call_handler(handler, :handler, callback, args, state) do
    # Handler behaviour call
    case apply(Handler, callback, [handler | args] ++ [state]) do
      {:ok, _result, _new_state} = success -> success
      {:error, _reason} = error -> error
      # Handle legacy response format
      other -> {:ok, other, state}
    end
  end

  defp get_handler_module(handler, :direct) when is_atom(handler), do: handler
  defp get_handler_module(handler, :handler) when is_atom(handler), do: handler
  defp get_handler_module(_handler, _mode), do: nil

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
