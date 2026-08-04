defmodule ExMCP.Server.Dispatch do
  @moduledoc """
  Single MCP method table for handler modules invoked with their own state.

  `ExMCP.Server.HandlerServer` (test/BEAM transports) and
  `ExMCP.Server.StdioServer` both drive an `ExMCP.Server.Handler` module by
  calling its callbacks with the handler state and turning the return value
  into a JSON-RPC response. They used to carry two independently maintained
  copies of that table, which drifted: stdio never implemented
  `completion/complete`, `resources/subscribe`, `roots/list`,
  `logging/setLevel` or the task methods, and the two copies normalized tool
  results differently.

  This module owns the table. Transports keep their own framing (stdout lines
  vs. transport `send_message/2`), batching, and cancellation bookkeeping, and
  delegate callback invocation plus result shaping here. Shaping itself lives
  in `ExMCP.Server.ResultNormalizer`, which the HTTP path
  (`ExMCP.MessageProcessor.MethodHandlers`) shares, so every transport agrees
  on tool-result shape and on which error detail is safe to return.

  ## Return value

  * `{:response, response_map, new_handler_state}` for requests
  * `{:notification, new_handler_state}` for notifications (no response)

  Callbacks that `ExMCP.Server.Handler` marks optional are probed with
  `function_exported?/3`; a handler that does not implement one answers
  `-32601 Method not found`.
  """

  alias ExMCP.Internal.JSONRPC
  alias ExMCP.Protocol.ErrorCodes
  alias ExMCP.Server.ResultNormalizer

  @type handler_state :: term()
  @type result ::
          {:response, map(), handler_state()} | {:notification, handler_state()}

  # Methods this dispatcher answers. Transports use `known_method?/1` to decide
  # whether to fall back to their own custom-method escape hatch.
  @methods [
    "initialize",
    "ping",
    "tools/list",
    "tools/call",
    "resources/list",
    "resources/templates/list",
    "resources/read",
    "resources/subscribe",
    "resources/unsubscribe",
    "prompts/list",
    "prompts/get",
    "completion/complete",
    "logging/setLevel",
    "roots/list",
    "tasks/get",
    "tasks/list",
    "tasks/result",
    "tasks/cancel",
    "notifications/elicitation/complete"
  ]

  @doc """
  Returns the list of MCP methods handled by this dispatcher.
  """
  @spec methods() :: [String.t()]
  def methods, do: @methods

  @doc """
  Whether `method` is part of the shared MCP method table.
  """
  @spec known_method?(String.t()) :: boolean()
  def known_method?(method), do: method in @methods

  @doc """
  Dispatches a decoded JSON-RPC request or notification to `handler_module`.
  """
  @spec dispatch(map(), module(), handler_state()) :: result()
  def dispatch(request, handler_module, handler_state)

  def dispatch(%{"method" => method} = request, handler_module, state) do
    ctx = %{
      method: method,
      id: Map.get(request, "id"),
      params: Map.get(request, "params") || %{},
      request?: Map.has_key?(request, "id"),
      module: handler_module
    }

    do_dispatch(method, ctx, state)
  end

  def dispatch(_invalid_request, _handler_module, state) do
    {:response, JSONRPC.error(nil, ErrorCodes.invalid_request(), "Invalid Request"), state}
  end

  @doc """
  Builds the argument map handed to `c:ExMCP.Server.Handler.handle_call_tool/3`.

  Request-level `_meta` (which carries `progressToken`) lives beside
  `arguments` in the JSON-RPC params, but `ExMCP.Server.Handler` documents it
  as reachable from the arguments map, so it is merged in here. Arguments that
  already carry their own `_meta` win.
  """
  @spec tool_arguments(map()) :: map()
  def tool_arguments(params) when is_map(params) do
    arguments = Map.get(params, "arguments") || %{}

    case Map.get(params, "_meta") do
      nil -> arguments
      meta -> Map.put_new(arguments, "_meta", meta)
    end
  end

  defp do_dispatch("initialize", ctx, state) do
    call(ctx, :handle_initialize, [ctx.params, state], "Initialize error", state)
  end

  defp do_dispatch("ping", ctx, state) do
    {:response, JSONRPC.response(ctx.id, %{}), state}
  end

  defp do_dispatch("tools/list", ctx, state) do
    args = [cursor(ctx), state]
    paginated(ctx, :handle_list_tools, args, "tools", "List tools error", state)
  end

  defp do_dispatch("tools/call", ctx, state) do
    name = Map.get(ctx.params, "name")
    arguments = tool_arguments(ctx.params)

    call(ctx, :handle_call_tool, [name, arguments, state], "Tool call error", state,
      on_ok: &ResultNormalizer.tool_result(&1, wrap_bare_map: true)
    )
  end

  defp do_dispatch("resources/list", ctx, state) do
    args = [cursor(ctx), state]
    paginated(ctx, :handle_list_resources, args, "resources", "List resources error", state)
  end

  defp do_dispatch("resources/templates/list", ctx, state) do
    args = [cursor(ctx), state]

    paginated(
      ctx,
      :handle_list_resource_templates,
      args,
      "resourceTemplates",
      "List resource templates error",
      state
    )
  end

  defp do_dispatch("resources/read", ctx, state) do
    uri = Map.get(ctx.params, "uri")

    call(ctx, :handle_read_resource, [uri, state], "Read resource error", state,
      on_ok: &%{"contents" => List.wrap(&1)}
    )
  end

  defp do_dispatch("resources/subscribe", ctx, state) do
    uri = Map.get(ctx.params, "uri")
    call(ctx, :handle_subscribe_resource, [uri, state], "Subscribe resource error", state)
  end

  defp do_dispatch("resources/unsubscribe", ctx, state) do
    uri = Map.get(ctx.params, "uri")
    call(ctx, :handle_unsubscribe_resource, [uri, state], "Unsubscribe resource error", state)
  end

  defp do_dispatch("prompts/list", ctx, state) do
    args = [cursor(ctx), state]
    paginated(ctx, :handle_list_prompts, args, "prompts", "List prompts error", state)
  end

  defp do_dispatch("prompts/get", ctx, state) do
    name = Map.get(ctx.params, "name")
    arguments = Map.get(ctx.params, "arguments") || %{}
    call(ctx, :handle_get_prompt, [name, arguments, state], "Get prompt error", state)
  end

  defp do_dispatch("completion/complete", ctx, state) do
    ref = Map.get(ctx.params, "ref")
    argument = Map.get(ctx.params, "argument")
    call(ctx, :handle_complete, [ref, argument, state], "Completion error", state)
  end

  defp do_dispatch("logging/setLevel", ctx, state) do
    set_log_level(ctx, state)
  end

  defp do_dispatch("roots/list", ctx, state) do
    call(ctx, :handle_list_roots, [state], "List roots error", state, on_ok: &%{"roots" => &1})
  end

  defp do_dispatch("tasks/get", ctx, state) do
    args = [Map.get(ctx.params, "taskId"), state]
    call(ctx, :handle_task_get, args, "Task get error", state)
  end

  defp do_dispatch("tasks/result", ctx, state) do
    args = [Map.get(ctx.params, "taskId"), state]
    call(ctx, :handle_task_result, args, "Task result error", state)
  end

  defp do_dispatch("tasks/cancel", ctx, state) do
    args = [Map.get(ctx.params, "taskId"), state]
    call(ctx, :handle_task_cancel, args, "Task cancel error", state)
  end

  defp do_dispatch("tasks/list", ctx, state) do
    args = [cursor(ctx), state]
    paginated(ctx, :handle_task_list, args, "tasks", "Task list error", state)
  end

  defp do_dispatch("notifications/elicitation/complete", ctx, state) do
    elicitation_id = Map.get(ctx.params, "elicitationId", "")

    if exported?(ctx.module, :handle_elicitation_complete, 2) do
      case ctx.module.handle_elicitation_complete(elicitation_id, state) do
        {:ok, new_state} -> {:notification, new_state}
        {:error, _reason, new_state} -> {:notification, new_state}
        _other -> {:notification, state}
      end
    else
      {:notification, state}
    end
  end

  defp do_dispatch(method, ctx, state) do
    if ctx.request? do
      {:response, method_not_found(ctx.id, method), state}
    else
      # Unknown notification: nothing to answer.
      {:notification, state}
    end
  end

  # logging/setLevel answers {:ok, state} (no result term), so it cannot go
  # through call/6. A handler without the callback still succeeds: logging is
  # optional and clients should not have to special-case it.
  defp set_log_level(ctx, state) do
    level = Map.get(ctx.params, "level")

    if exported?(ctx.module, :handle_set_log_level, 2) do
      case ctx.module.handle_set_log_level(level, state) do
        {:ok, new_state} ->
          {:response, JSONRPC.response(ctx.id, %{}), new_state}

        {:ok, _result, new_state} ->
          {:response, JSONRPC.response(ctx.id, %{}), new_state}

        {:error, reason, new_state} ->
          {:response, error_response(ctx.id, "Set log level error", reason), new_state}

        {:error, reason} ->
          {:response, error_response(ctx.id, "Set log level error", reason), state}
      end
    else
      {:response, JSONRPC.response(ctx.id, %{}), state}
    end
  end

  # Invokes a callback returning {:ok, result, state} | {:error, reason, state}.
  defp call(ctx, fun, args, label, state, opts \\ []) do
    if exported?(ctx.module, fun, length(args)) do
      on_ok = Keyword.get(opts, :on_ok, & &1)

      case apply(ctx.module, fun, args) do
        {:ok, result, new_state} ->
          {:response, JSONRPC.response(ctx.id, on_ok.(result)), new_state}

        {:ok, result} ->
          {:response, JSONRPC.response(ctx.id, on_ok.(result)), state}

        {:error, reason, new_state} ->
          {:response, error_response(ctx.id, label, reason), new_state}

        {:error, reason} ->
          {:response, error_response(ctx.id, label, reason), state}
      end
    else
      {:response, method_not_found(ctx.id, ctx.method), state}
    end
  end

  # Invokes a paginated list callback returning {:ok, entries, cursor, state}.
  defp paginated(ctx, fun, args, key, label, state) do
    if exported?(ctx.module, fun, length(args)) do
      case apply(ctx.module, fun, args) do
        {:ok, entries, next_cursor, new_state} ->
          result = ResultNormalizer.paginated(key, entries, next_cursor)
          {:response, JSONRPC.response(ctx.id, result), new_state}

        {:ok, entries, new_state} when is_list(entries) ->
          result = ResultNormalizer.paginated(key, entries, nil)
          {:response, JSONRPC.response(ctx.id, result), new_state}

        {:error, reason, new_state} ->
          {:response, error_response(ctx.id, label, reason), new_state}

        {:error, reason} ->
          {:response, error_response(ctx.id, label, reason), state}
      end
    else
      {:response, method_not_found(ctx.id, ctx.method), state}
    end
  end

  defp cursor(ctx), do: Map.get(ctx.params, "cursor")

  defp method_not_found(id, method) do
    JSONRPC.error(id, ErrorCodes.method_not_found(), "Method not found: #{method}")
  end

  # Error details are logged by ResultNormalizer; only handler-authored text
  # reaches the client (audit M12).
  defp error_response(id, label, reason) do
    JSONRPC.error(
      id,
      ResultNormalizer.error_code(reason, -32000),
      ResultNormalizer.error_message(label, reason)
    )
  end

  defp exported?(module, fun, arity) do
    function_exported?(module, fun, arity) or
      (Code.ensure_loaded?(module) and function_exported?(module, fun, arity))
  end
end
