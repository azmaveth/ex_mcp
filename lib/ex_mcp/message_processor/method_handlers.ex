defmodule ExMCP.MessageProcessor.MethodHandlers do
  @moduledoc false

  # HTTP-side MCP method table.
  #
  # Unlike ExMCP.Server.Dispatch, this path talks to a handler *process* over
  # GenServer.call/3, so it owns crash/timeout handling. Method coverage
  # mirrors ExMCP.Server.Dispatch and all result/error shaping is delegated to
  # ExMCP.Server.ResultNormalizer, so both paths answer identically (audit M9).

  alias ExMCP.Internal.{JSONRPC, VersionRegistry}
  alias ExMCP.Server.{Dispatch, ResultNormalizer}

  require Logger

  # Default timeout for GenServer calls into handler processes. Override per
  # request via the `:handler_call_timeout` option of
  # `ExMCP.MessageProcessor.process/2`.
  @default_handler_call_timeout 10_000

  def handle_initialize(conn, server_pid, params, id, _server_info) do
    safe_call(conn, server_pid, {:initialize, params}, id, "Initialize failed", fn
      {:ok, result} -> put_initialize_result(conn, result, id)
      {:error, reason} -> put_error(conn, "Initialize failed", reason, id)
    end)
  end

  def handle_tools_list(conn, server_pid, params, id) do
    list_call(conn, server_pid, {:list_tools, cursor(params)}, id, "Tools list failed", "tools")
  end

  def handle_tools_call(conn, server_pid, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Dispatch.tool_arguments(params)

    :telemetry.execute(
      [:ex_mcp, :server, :tool, :called],
      %{},
      %{tool_name: tool_name, mode: :handler}
    )

    safe_call(conn, server_pid, {:call_tool, tool_name, arguments}, id, "Tool call failed", fn
      {:ok, result} -> put_success(conn, ResultNormalizer.tool_result(result), id)
      {:error, reason} -> put_success(conn, ResultNormalizer.tool_error_result(reason), id)
    end)
  end

  def handle_resources_list(conn, server_pid, params, id) do
    list_call(
      conn,
      server_pid,
      {:list_resources, cursor(params)},
      id,
      "Resources list failed",
      "resources"
    )
  end

  def handle_resource_templates_list(conn, server_pid, params, id) do
    list_call(
      conn,
      server_pid,
      {:list_resource_templates, cursor(params)},
      id,
      "Resource templates list failed",
      "resourceTemplates"
    )
  end

  def handle_resources_read(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    :telemetry.execute(
      [:ex_mcp, :server, :resource, :read],
      %{},
      %{uri: uri, mode: :handler}
    )

    safe_call(conn, server_pid, {:read_resource, uri}, id, "Resource read failed", fn
      {:ok, contents} ->
        contents = ResultNormalizer.stringify_keys(List.wrap(contents))
        put_success(conn, %{"contents" => contents}, id)

      {:error, reason} ->
        put_error(conn, "Resource read failed", reason, id)
    end)
  end

  def handle_resources_subscribe(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    safe_call(conn, server_pid, {:subscribe_resource, uri}, id, "Subscribe failed", fn
      {:ok, _result} -> register_subscription(conn, uri, id)
      {:error, reason} -> put_error(conn, "Subscribe failed", reason, id)
    end)
  end

  def handle_resources_unsubscribe(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    safe_call(conn, server_pid, {:unsubscribe_resource, uri}, id, "Unsubscribe failed", fn
      {:ok, _result} -> unregister_subscription(conn, uri, id)
      {:error, reason} -> put_error(conn, "Unsubscribe failed", reason, id)
    end)
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
    :ok = ExMCP.SubscriptionRegistry.unsubscribe(session_id, uri)
    put_success(conn, %{}, id)
  end

  defp unregister_subscription(conn, _uri, id), do: put_success(conn, %{}, id)

  def handle_prompts_list(conn, server_pid, params, id) do
    list_call(
      conn,
      server_pid,
      {:list_prompts, cursor(params)},
      id,
      "Prompts list failed",
      "prompts"
    )
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
      {:ok, result} -> put_success(conn, ResultNormalizer.stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Prompt get failed", reason, id)
    end)
  end

  def handle_completion_complete(conn, server_pid, params, id) do
    request = {:complete, Map.get(params, "ref"), Map.get(params, "argument")}

    safe_call(conn, server_pid, request, id, "Completion failed", fn
      {:ok, result} -> put_success(conn, ResultNormalizer.stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Completion failed", reason, id)
    end)
  end

  # Routed through the handler rather than short-circuited with a canned
  # success, so servers can actually observe the level change (audit M10).
  def handle_set_log_level(conn, server_pid, params, id) do
    level = Map.get(params, "level")

    safe_call(conn, server_pid, {:set_log_level, level}, id, "Set log level failed", fn
      {:ok, _result} -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Set log level failed", reason, id)
    end)
  end

  def handle_roots_list(conn, server_pid, _params, id) do
    safe_call(conn, server_pid, {:list_roots}, id, "Roots list failed", fn
      {:ok, roots} ->
        put_success(conn, %{"roots" => ResultNormalizer.stringify_keys(List.wrap(roots))}, id)

      {:error, reason} ->
        put_error(conn, "Roots list failed", reason, id)
    end)
  end

  # Task methods (2025-11-25). Previously unreachable on this path: the
  # handler bridge answered "Unknown method" for them (audit M15).
  def handle_task_get(conn, server_pid, params, id) do
    request = {:task_get, Map.get(params, "taskId")}

    safe_call(conn, server_pid, request, id, "Task get failed", fn
      {:ok, result} -> put_success(conn, ResultNormalizer.stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Task get failed", reason, id)
    end)
  end

  def handle_task_result(conn, server_pid, params, id) do
    request = {:task_result, Map.get(params, "taskId")}

    safe_call(conn, server_pid, request, id, "Task result failed", fn
      {:ok, result} -> put_success(conn, ResultNormalizer.stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Task result failed", reason, id)
    end)
  end

  def handle_task_cancel(conn, server_pid, params, id) do
    request = {:task_cancel, Map.get(params, "taskId")}

    safe_call(conn, server_pid, request, id, "Task cancel failed", fn
      {:ok, result} -> put_success(conn, ResultNormalizer.stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Task cancel failed", reason, id)
    end)
  end

  def handle_task_list(conn, server_pid, params, id) do
    list_call(conn, server_pid, {:task_list, cursor(params)}, id, "Task list failed", "tasks")
  end

  @doc """
  Handles a method outside the MCP method table via the handler's
  `{:request, method, params}` escape hatch.

  Only an explicit "unknown method" answer maps to `-32601`; handler errors,
  crashes and timeouts map to `-32603` so genuine failures are not disguised
  as a missing method (audit M11).
  """
  def handle_custom_method(conn, server_pid, method, params, id) do
    reply = GenServer.call(server_pid, {:request, method, params}, call_timeout(conn))

    case normalize_reply(reply) do
      {:ok, result} ->
        put_success(conn, ResultNormalizer.stringify_keys(result), id)

      {:error, reason} ->
        if unknown_method?(reason) do
          put_method_not_found(conn, id)
        else
          put_error(conn, "Method failed", reason, id)
        end
    end
  rescue
    error ->
      Logger.error("Custom method #{method}: #{Exception.format(:error, error, __STACKTRACE__)}")
      put_failure(conn, "Method failed", "handler_crash", id)
  catch
    :exit, reason ->
      Logger.error("Custom method #{method}: handler exited: #{inspect(reason)}")
      put_failure(conn, "Method failed", exit_failure_type(reason), id)
  end

  # The handler bridge reports an unimplemented method as "Unknown method: x".
  # Anything else is a real failure.
  defp unknown_method?(reason) when is_binary(reason), do: reason =~ "Unknown method"
  defp unknown_method?(:unknown_method), do: true
  defp unknown_method?({:unknown_method, _}), do: true
  defp unknown_method?({:unexpected_reply, _}), do: false
  defp unknown_method?(_reason), do: false

  # Calls the handler process, converting raised exceptions *and* exits
  # (handler crash, :noproc, call timeout) into JSON-RPC internal errors so
  # that a misbehaving handler can never crash the request process.
  defp safe_call(conn, server_pid, request, id, label, on_reply) do
    reply = GenServer.call(server_pid, request, call_timeout(conn))

    reply
    |> normalize_reply()
    |> on_reply.()
  rescue
    error ->
      Logger.error("#{label}: #{Exception.format(:error, error, __STACKTRACE__)}")
      put_failure(conn, label, "handler_crash", id)
  catch
    :exit, reason ->
      Logger.error("#{label}: handler exited: #{inspect(reason)}")
      put_failure(conn, label, exit_failure_type(reason), id)
  end

  # Paginated list methods: handlers may answer with entries plus a cursor, or
  # with a ready-made result map.
  defp list_call(conn, server_pid, request, id, label, key) do
    reply = GenServer.call(server_pid, request, call_timeout(conn))

    case normalize_list_reply(reply) do
      {:list, entries, next_cursor} ->
        key
        |> ResultNormalizer.paginated(ResultNormalizer.stringify_keys(entries), next_cursor)
        |> put_success_result(conn, id)

      {:result, result} ->
        result
        |> ResultNormalizer.stringify_keys()
        |> put_success_result(conn, id)

      {:error, reason} ->
        put_error(conn, label, reason, id)
    end
  rescue
    error ->
      Logger.error("#{label}: #{Exception.format(:error, error, __STACKTRACE__)}")
      put_failure(conn, label, "handler_crash", id)
  catch
    :exit, reason ->
      Logger.error("#{label}: handler exited: #{inspect(reason)}")
      put_failure(conn, label, exit_failure_type(reason), id)
  end

  # Collapses every reply shape a handler process may produce into one of
  # `{:ok, result}` / `{:error, reason}` (audit M13). `ExMCP.Server.Handler`'s
  # GenServer bridge now emits the canonical shapes; the legacy variants stay
  # accepted here for hand-written handlers.
  defp normalize_reply(:ok), do: {:ok, %{}}
  defp normalize_reply({:ok, result}), do: {:ok, result}
  defp normalize_reply({:ok, result, _state}), do: {:ok, result}
  defp normalize_reply({:error, reason}), do: {:error, reason}
  defp normalize_reply({:error, reason, _state}), do: {:error, reason}
  defp normalize_reply(other), do: {:error, {:unexpected_reply, other}}

  defp normalize_list_reply({:ok, entries, next_cursor, _state}) when is_list(entries),
    do: {:list, entries, next_cursor}

  defp normalize_list_reply({:ok, entries, next_cursor})
       when is_list(entries) and (is_nil(next_cursor) or is_binary(next_cursor)),
       do: {:list, entries, next_cursor}

  defp normalize_list_reply({:ok, entries}) when is_list(entries), do: {:list, entries, nil}
  defp normalize_list_reply({:ok, result, _state}) when is_map(result), do: {:result, result}
  defp normalize_list_reply({:ok, result}) when is_map(result), do: {:result, result}
  defp normalize_list_reply(other), do: normalize_reply(other)

  defp cursor(params), do: Map.get(params, "cursor")

  defp call_timeout(conn) do
    Map.get(conn.assigns, :handler_call_timeout, @default_handler_call_timeout)
  end

  defp exit_failure_type({:timeout, _}), do: "handler_timeout"
  defp exit_failure_type(_reason), do: "handler_crash"

  defp put_initialize_result(conn, result, id) do
    result
    |> normalize_initialize_result()
    |> ResultNormalizer.stringify_keys()
    |> then(&put_success(conn, &1, id))
  end

  defp normalize_initialize_result(result) do
    default_version = VersionRegistry.latest_version()

    result =
      result
      |> Map.put_new("protocolVersion", default_version)
      |> Map.put_new(:protocolVersion, default_version)

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
end
