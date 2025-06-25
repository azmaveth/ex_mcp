defmodule ExMCP.MessageProcessor.Dispatcher do
  @moduledoc """
  Unified message dispatcher that eliminates duplication between different handler modes.

  This module provides a single dispatch mechanism that can handle:
  - Direct handler calls (DSL servers)
  - GenServer-based handlers
  - Handler module calls

  All business logic is delegated to the Handlers module, while this module
  handles routing and response adaptation based on the calling mode.
  """

  alias ExMCP.MessageProcessor.Handlers

  @type mode :: :direct | :genserver | :handler
  @type conn :: ExMCP.MessageProcessor.Conn.t()

  @doc """
  Dispatches a request to the appropriate handler based on the method and mode.

  ## Parameters

  - `conn` - The connection struct containing the request
  - `handler` - The handler module or server PID
  - `mode` - The dispatch mode (:direct, :genserver, or :handler)
  - `server_info` - Optional server information

  ## Returns

  Returns an updated connection struct with the response set.
  """
  @spec dispatch(conn, module() | pid(), mode, map()) :: conn
  def dispatch(conn, handler, mode, server_info \\ %{}) do
    request = conn.request
    method = Map.get(request, "method")
    params = Map.get(request, "params", %{})
    id = get_request_id(request)

    # Route to appropriate handler based on method
    dispatch_method(method, conn, handler, mode, params, id, server_info)
  end

  # Define method routing as separate function clauses
  defp dispatch_method("ping", conn, _handler, _mode, _params, id, _server_info) do
    Handlers.handle_ping(conn, id)
  end

  defp dispatch_method("initialize", conn, handler, mode, params, id, server_info) do
    Handlers.handle_initialize(conn, handler, mode, params, id, server_info)
  end

  defp dispatch_method("tools/list", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_tools_list(conn, handler, mode, params, id)
  end

  defp dispatch_method("tools/call", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_tools_call(conn, handler, mode, params, id)
  end

  defp dispatch_method("resources/list", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_resources_list(conn, handler, mode, params, id)
  end

  defp dispatch_method("resources/read", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_resources_read(conn, handler, mode, params, id)
  end

  defp dispatch_method("resources/subscribe", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_resources_subscribe(conn, handler, mode, params, id)
  end

  defp dispatch_method("resources/unsubscribe", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_resources_unsubscribe(conn, handler, mode, params, id)
  end

  defp dispatch_method("prompts/list", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_prompts_list(conn, handler, mode, params, id)
  end

  defp dispatch_method("prompts/get", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_prompts_get(conn, handler, mode, params, id)
  end

  defp dispatch_method("completion/complete", conn, handler, mode, params, id, _server_info) do
    Handlers.handle_completion_complete(conn, handler, mode, params, id)
  end

  defp dispatch_method(method, conn, handler, mode, params, id, _server_info) do
    # Unknown method - delegate to custom handler
    Handlers.handle_custom_method(conn, handler, mode, method, params, id)
  end

  @doc """
  Extracts the request ID from a request map.
  """
  @spec get_request_id(map()) :: any()
  def get_request_id(request) when is_map(request) do
    Map.get(request, "id")
  end

  def get_request_id(_), do: nil
end
