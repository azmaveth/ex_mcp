defmodule ExMCP.MessageProcessor.MethodHandlers do
  @moduledoc false

  alias ExMCP.Internal.JSONRPC

  @default_protocol_version "2025-11-25"

  def handle_initialize(conn, server_pid, params, id, _server_info) do
    case GenServer.call(server_pid, {:initialize, params}, 5000) do
      {:ok, result} ->
        result
        |> normalize_initialize_result()
        |> deep_stringify_keys()
        |> then(&put_success(conn, &1, id))

      {:ok, result, _state} ->
        result
        |> normalize_initialize_result()
        |> deep_stringify_keys()
        |> then(&put_success(conn, &1, id))

      {:error, reason} ->
        put_error(conn, "Initialize failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Initialize failed", error, id)
  end

  def handle_tools_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_tools, cursor}, 5000) do
      {:ok, tools, next_cursor, _state} ->
        paginated_result("tools", tools, next_cursor)
        |> put_success_result(conn, id)

      {:ok, result, _state} when is_map(result) ->
        result
        |> deep_stringify_keys()
        |> put_success_result(conn, id)

      {:ok, result} when is_map(result) ->
        result
        |> deep_stringify_keys()
        |> put_success_result(conn, id)

      {:error, reason} ->
        put_error(conn, "Tools list failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Tools list failed", error, id)
  end

  def handle_tools_call(conn, server_pid, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :tool, :called],
      %{},
      %{tool_name: tool_name, mode: :handler}
    )

    case GenServer.call(server_pid, {:call_tool, tool_name, arguments}, 10_000) do
      {:ok, result} ->
        put_success(conn, wrap_tool_result(result), id)

      {:ok, result, _state} ->
        put_success(conn, wrap_tool_result(result), id)

      {:error, reason} ->
        put_success(conn, tool_error_result(reason), id)

      {:error, reason, _state} ->
        put_success(conn, tool_error_result(reason), id)
    end
  rescue
    error -> put_error(conn, "Tool call failed", error, id)
  end

  def handle_resources_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_resources, cursor}, 5000) do
      {:ok, resources, next_cursor, _state} ->
        paginated_result("resources", resources, next_cursor)
        |> put_success_result(conn, id)

      {:ok, result, _state} when is_map(result) ->
        result
        |> deep_stringify_keys()
        |> put_success_result(conn, id)

      {:ok, result} when is_map(result) ->
        result
        |> deep_stringify_keys()
        |> put_success_result(conn, id)

      {:error, reason} ->
        put_error(conn, "Resources list failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Resources list failed", error, id)
  end

  def handle_resources_read(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    :telemetry.execute(
      [:ex_mcp, :server, :resource, :read],
      %{},
      %{uri: uri, mode: :handler}
    )

    case GenServer.call(server_pid, {:read_resource, uri}, 5000) do
      {:ok, contents, _state} ->
        put_success(conn, %{"contents" => deep_stringify_keys(List.wrap(contents))}, id)

      {:ok, contents} ->
        put_success(conn, %{"contents" => deep_stringify_keys(List.wrap(contents))}, id)

      {:error, reason} ->
        put_error(conn, "Resource read failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Resource read failed", error, id)
  end

  def handle_resources_subscribe(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:subscribe_resource, uri}, 5000) do
      :ok -> register_subscription(conn, uri, id)
      {:ok, _state} -> register_subscription(conn, uri, id)
      {:error, reason} -> put_error(conn, "Subscribe failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Subscribe failed", error, id)
  end

  def handle_resources_unsubscribe(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:unsubscribe_resource, uri}, 5000) do
      :ok -> unregister_subscription(conn, uri, id)
      {:ok, _state} -> unregister_subscription(conn, uri, id)
      {:error, reason} -> put_error(conn, "Unsubscribe failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Unsubscribe failed", error, id)
  end

  defp register_subscription(%{session_id: session_id} = conn, uri, id)
       when is_binary(session_id) do
    case ExMCP.SubscriptionRegistry.subscribe(session_id, uri) do
      :ok -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Subscribe failed", reason, id)
    end
  end

  defp register_subscription(conn, _uri, id), do: put_success(conn, %{}, id)

  defp unregister_subscription(%{session_id: session_id} = conn, uri, id)
       when is_binary(session_id) do
    case ExMCP.SubscriptionRegistry.unsubscribe(session_id, uri) do
      :ok -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Unsubscribe failed", reason, id)
    end
  end

  defp unregister_subscription(conn, _uri, id), do: put_success(conn, %{}, id)

  def handle_prompts_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_prompts, cursor}, 5000) do
      {:ok, prompts, next_cursor, _state} ->
        paginated_result("prompts", prompts, next_cursor)
        |> put_success_result(conn, id)

      {:ok, result, _state} when is_map(result) ->
        result
        |> deep_stringify_keys()
        |> put_success_result(conn, id)

      {:ok, result} when is_map(result) ->
        result
        |> deep_stringify_keys()
        |> put_success_result(conn, id)

      {:error, reason} ->
        put_error(conn, "Prompts list failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Prompts list failed", error, id)
  end

  def handle_prompts_get(conn, server_pid, params, id) do
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :prompt, :rendered],
      %{},
      %{name: name, mode: :handler}
    )

    case GenServer.call(server_pid, {:get_prompt, name, arguments}, 5000) do
      {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
      {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Prompt get failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Prompt get failed", error, id)
  end

  def handle_completion_complete(conn, server_pid, params, id) do
    case GenServer.call(server_pid, {:complete, params["ref"], params["argument"]}, 5000) do
      {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
      {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Completion failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Completion failed", error, id)
  end

  def handle_custom_method(conn, server_pid, method, params, id) do
    case GenServer.call(server_pid, {:request, method, params}, 5000) do
      {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
      {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
      {:error, _reason} -> put_method_not_found(conn, id)
      _ -> put_method_not_found(conn, id)
    end
  catch
    :exit, _ -> put_method_not_found(conn, id)
  end

  defp normalize_initialize_result(result) do
    result =
      result
      |> Map.put_new("protocolVersion", @default_protocol_version)
      |> Map.put_new(:protocolVersion, @default_protocol_version)

    if Map.has_key?(result, "serverInfo") or Map.has_key?(result, :serverInfo) do
      result
    else
      name = Map.get(result, "name") || Map.get(result, :name)
      version = Map.get(result, "version") || Map.get(result, :version)

      if name && version do
        Map.put(result, "serverInfo", %{"name" => name, "version" => version})
      else
        result
      end
    end
  end

  defp paginated_result(key, entries, nil), do: %{key => deep_stringify_keys(entries)}

  defp paginated_result(key, entries, next_cursor) do
    key
    |> paginated_result(entries, nil)
    |> Map.put("nextCursor", next_cursor)
  end

  defp put_success_result(result, conn, id), do: put_success(conn, result, id)
  defp put_success(conn, result, id), do: %{conn | response: JSONRPC.response(id, result)}

  defp put_error(conn, message, reason, id) do
    %{conn | response: JSONRPC.error(id, -32603, message, %{"reason" => inspect(reason)})}
  end

  defp put_method_not_found(conn, id) do
    %{conn | response: JSONRPC.error(id, -32601, "Method not found")}
  end

  defp wrap_tool_result(result) when is_list(result) do
    %{"content" => deep_stringify_keys(result)}
  end

  defp wrap_tool_result(%{content: content} = result) do
    result
    |> Map.delete(:content)
    |> Map.put("content", deep_stringify_keys(List.wrap(content)))
    |> deep_stringify_keys()
  end

  defp wrap_tool_result(%{"content" => _} = result), do: deep_stringify_keys(result)
  defp wrap_tool_result(result) when is_map(result), do: deep_stringify_keys(result)

  defp tool_error_result(reason) do
    %{
      "content" => [%{"type" => "text", "text" => to_string(reason)}],
      "isError" => true
    }
  end

  defp deep_stringify_keys(list) when is_list(list) do
    Enum.map(list, &deep_stringify_keys/1)
  end

  defp deep_stringify_keys(map) when is_map(map) and not is_struct(map) do
    Map.new(map, fn
      {key, value} when is_atom(key) -> {Atom.to_string(key), deep_stringify_keys(value)}
      {key, value} -> {key, deep_stringify_keys(value)}
    end)
  end

  defp deep_stringify_keys(value), do: value
end
