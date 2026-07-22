defmodule ExMCP.MessageProcessor.MethodHandlers do
  @moduledoc false

  alias ExMCP.Internal.JSONRPC

  require Logger

  @default_protocol_version "2025-11-25"

  # Default timeout for GenServer calls into handler processes. Override per
  # request via the `:handler_call_timeout` option of
  # `ExMCP.MessageProcessor.process/2`.
  @default_handler_call_timeout 10_000

  def handle_initialize(conn, server_pid, params, id, _server_info) do
    safe_call(conn, server_pid, {:initialize, params}, id, "Initialize failed", fn
      {:ok, result} -> put_initialize_result(conn, result, id)
      {:ok, result, _state} -> put_initialize_result(conn, result, id)
      {:error, reason} -> put_error(conn, "Initialize failed", reason, id)
    end)
  end

  def handle_tools_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    safe_call(conn, server_pid, {:list_tools, cursor}, id, "Tools list failed", fn
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
    end)
  end

  def handle_tools_call(conn, server_pid, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :tool, :called],
      %{},
      %{tool_name: tool_name, mode: :handler}
    )

    safe_call(conn, server_pid, {:call_tool, tool_name, arguments}, id, "Tool call failed", fn
      {:ok, result} -> put_success(conn, wrap_tool_result(result), id)
      {:ok, result, _state} -> put_success(conn, wrap_tool_result(result), id)
      {:error, reason} -> put_success(conn, tool_error_result(reason), id)
      {:error, reason, _state} -> put_success(conn, tool_error_result(reason), id)
    end)
  end

  def handle_resources_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    safe_call(conn, server_pid, {:list_resources, cursor}, id, "Resources list failed", fn
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
    end)
  end

  def handle_resources_read(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    :telemetry.execute(
      [:ex_mcp, :server, :resource, :read],
      %{},
      %{uri: uri, mode: :handler}
    )

    safe_call(conn, server_pid, {:read_resource, uri}, id, "Resource read failed", fn
      {:ok, contents, _state} ->
        put_success(conn, %{"contents" => deep_stringify_keys(List.wrap(contents))}, id)

      {:ok, contents} ->
        put_success(conn, %{"contents" => deep_stringify_keys(List.wrap(contents))}, id)

      {:error, reason} ->
        put_error(conn, "Resource read failed", reason, id)
    end)
  end

  def handle_resources_subscribe(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    safe_call(conn, server_pid, {:subscribe_resource, uri}, id, "Subscribe failed", fn
      :ok -> put_success(conn, %{}, id)
      {:ok, _state} -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Subscribe failed", reason, id)
    end)
  end

  def handle_resources_unsubscribe(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    safe_call(conn, server_pid, {:unsubscribe_resource, uri}, id, "Unsubscribe failed", fn
      :ok -> put_success(conn, %{}, id)
      {:ok, _state} -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Unsubscribe failed", reason, id)
    end)
  end

  def handle_prompts_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    safe_call(conn, server_pid, {:list_prompts, cursor}, id, "Prompts list failed", fn
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
    end)
  end

  def handle_prompts_get(conn, server_pid, params, id) do
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :prompt, :rendered],
      %{},
      %{name: name, mode: :handler}
    )

    safe_call(conn, server_pid, {:get_prompt, name, arguments}, id, "Prompt get failed", fn
      {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
      {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Prompt get failed", reason, id)
    end)
  end

  def handle_completion_complete(conn, server_pid, params, id) do
    safe_call(
      conn,
      server_pid,
      {:complete, params["ref"], params["argument"]},
      id,
      "Completion failed",
      fn
        {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
        {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
        {:error, reason} -> put_error(conn, "Completion failed", reason, id)
      end
    )
  end

  def handle_custom_method(conn, server_pid, method, params, id) do
    case GenServer.call(server_pid, {:request, method, params}, call_timeout(conn)) do
      {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
      {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
      {:error, _reason} -> put_method_not_found(conn, id)
      _ -> put_method_not_found(conn, id)
    end
  catch
    :exit, {:timeout, _} = reason ->
      Logger.error("Custom method call timed out: #{inspect(reason)}")
      put_failure(conn, "Custom method failed", "handler_timeout", id)

    :exit, _reason ->
      put_method_not_found(conn, id)
  end

  # Calls the handler process, converting raised exceptions *and* exits
  # (handler crash, :noproc, call timeout) into JSON-RPC internal errors so
  # that a misbehaving handler can never crash the request process.
  defp safe_call(conn, server_pid, request, id, label, on_reply) do
    reply = GenServer.call(server_pid, request, call_timeout(conn))
    on_reply.(reply)
  rescue
    error ->
      Logger.error("#{label}: #{Exception.format(:error, error, __STACKTRACE__)}")
      put_failure(conn, label, "handler_crash", id)
  catch
    :exit, reason ->
      Logger.error("#{label}: handler exited: #{inspect(reason)}")
      put_failure(conn, label, exit_failure_type(reason), id)
  end

  defp call_timeout(conn) do
    Map.get(conn.assigns, :handler_call_timeout, @default_handler_call_timeout)
  end

  defp exit_failure_type({:timeout, _}), do: "handler_timeout"
  defp exit_failure_type(_reason), do: "handler_crash"

  defp put_initialize_result(conn, result, id) do
    result
    |> normalize_initialize_result()
    |> deep_stringify_keys()
    |> then(&put_success(conn, &1, id))
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

  # Error details are logged, never embedded in the JSON-RPC response: the
  # `data.type` field carries a stable, machine-readable classification
  # instead of `inspect(reason)` output (audit M12).
  defp put_error(conn, message, reason, id) do
    Logger.error("#{message}: #{inspect(reason)}")
    put_failure(conn, message, "handler_error", id)
  end

  defp put_failure(conn, message, type, id) do
    %{conn | response: JSONRPC.error(id, -32603, message, %{"type" => type})}
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
