defmodule ExMCP.Protocol.RequestProcessor do
  @moduledoc """
  Processes JSON-RPC requests for MCP servers.

  This module handles:
  - Request routing based on method name
  - Standard MCP method implementations
  - Delegation to custom handlers
  - Response building for each method type

  ## Supported Methods

  - `initialize` - Protocol handshake and capability negotiation
  - `tools/list` - List available tools
  - `tools/call` - Execute a tool
  - `resources/list` - List available resources
  - `resources/read` - Read a resource
  - `prompts/list` - List available prompts
  - `prompts/get` - Get a prompt
  - `notifications/initialized` - Client initialization complete
  """

  alias ExMCP.Error
  alias ExMCP.Internal.VersionRegistry
  alias ExMCP.Protocol.ResponseBuilder

  @type request :: map()
  @type state :: map()
  @type process_result ::
          {:response, map(), state()}
          | {:notification, state()}
          | {:async, state()}

  @doc """
  Processes a JSON-RPC request and returns the appropriate response.

  Returns:
  - `{:response, response_map, new_state}` - Synchronous response
  - `{:notification, new_state}` - For notifications (no response needed)
  - `{:async, new_state}` - For async operations (response sent separately)

  ## Examples

      iex> request = %{"method" => "tools/list", "id" => 123}
      iex> RequestProcessor.process(request, state)
      {:response, %{"jsonrpc" => "2.0", "id" => 123, "result" => %{"tools" => []}}, state}
  """
  @spec process(request(), state()) :: process_result()
  def process(%{"method" => method} = request, state) do
    request_id = Map.get(request, "id", "notification_#{System.unique_integer([:positive])}")
    server = Map.get(state, :__module__, :unknown)

    metadata = %{
      request_id: to_string(request_id),
      method: method,
      server: server
    }

    ExMCP.Telemetry.span([:ex_mcp, :request], metadata, fn ->
      dispatch_method(method, request, state)
    end)
  end

  def process(_request, state) do
    # Invalid request format
    error = Error.protocol_error(-32600, "Invalid Request")
    error_response = ResponseBuilder.build_error_response(error, nil)
    {:response, error_response, state}
  end

  defp dispatch_method("initialize", req, state), do: process_initialize(req, state)
  defp dispatch_method("tools/list", req, state), do: process_tools_list(req, state)
  defp dispatch_method("tools/call", req, state), do: process_tools_call(req, state)
  defp dispatch_method("resources/list", req, state), do: process_resources_list(req, state)
  defp dispatch_method("resources/read", req, state), do: process_resources_read(req, state)
  defp dispatch_method("prompts/list", req, state), do: process_prompts_list(req, state)
  defp dispatch_method("prompts/get", req, state), do: process_prompts_get(req, state)

  defp dispatch_method("notifications/initialized", req, state),
    do: process_initialized_notification(req, state)

  defp dispatch_method("notifications/elicitation/complete", req, state),
    do: process_elicitation_complete(req, state)

  defp dispatch_method("notifications/tasks/status", req, state),
    do: process_task_status_notification(req, state)

  defp dispatch_method("tasks/get", req, state), do: process_task_get(req, state)
  defp dispatch_method("tasks/list", req, state), do: process_task_list(req, state)
  defp dispatch_method("tasks/result", req, state), do: process_task_result(req, state)
  defp dispatch_method("tasks/cancel", req, state), do: process_task_cancel(req, state)
  defp dispatch_method(_, req, state), do: process_unknown_method(req, state)

  # Initialize request processing
  defp process_initialize(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    module = state.__module__

    # Check if the module has custom handle_initialize implementation
    if function_exported?(module, :handle_initialize, 2) do
      case module.handle_initialize(params, state) do
        {:ok, result, new_state} ->
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, reason)
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      # Default DSL server initialization
      process_default_initialize(id, params, state)
    end
  end

  defp process_default_initialize(id, params, state) do
    client_version = Map.get(params, "protocolVersion", "2025-06-18")
    supported_versions = VersionRegistry.supported_versions()

    if client_version in supported_versions do
      result = %{
        "protocolVersion" => client_version,
        "serverInfo" => get_server_info(state),
        "capabilities" => get_capabilities(state)
      }

      response = ResponseBuilder.build_success_response(result, id)
      new_state = Map.put(state, :protocol_version, client_version)
      {:response, response, new_state}
    else
      error =
        Error.protocol_error(
          -32600,
          "Unsupported protocol version: #{client_version}",
          %{"supported_versions" => supported_versions}
        )

      error_response = ResponseBuilder.build_error_response(error, id)

      {:response, error_response, state}
    end
  end

  # Tools list request
  defp process_tools_list(%{"id" => id}, state) do
    module = state.__module__

    if function_exported?(module, :handle_list_tools, 2) do
      case module.handle_list_tools(nil, state) do
        {:ok, tools, next_cursor, new_state} ->
          result = %{"tools" => deep_stringify_keys(tools)}
          result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, to_string(reason))
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      response = ResponseBuilder.build_success_response(%{"tools" => []}, id)
      {:response, response, state}
    end
  end

  # Tools call request - delegates to handle_call for async processing
  defp process_tools_call(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})
    module = state.__module__

    if function_exported?(module, :handle_call_tool, 3) do
      execute_tool_call(module, tool_name, arguments, id, state)
    else
      # No custom implementation, return default error
      error = %Error.ToolError{
        tool_name: tool_name || "unknown",
        reason: "Tool not implemented",
        arguments: nil
      }

      error_response = ResponseBuilder.build_error_response(error, id)
      {:response, error_response, state}
    end
  end

  # Execute tool call with telemetry
  defp execute_tool_call(module, tool_name, arguments, id, state) do
    metadata = %{
      tool_name: tool_name || "unknown",
      request_id: to_string(id)
    }

    result =
      ExMCP.Telemetry.span([:ex_mcp, :tool], metadata, fn ->
        module.handle_call_tool(tool_name, arguments, state)
      end)

    handle_tool_result(result, tool_name, id)
  end

  # Handle tool execution result
  defp handle_tool_result({:ok, result, new_state}, _tool_name, id) do
    tool_result = normalize_tool_result(result)
    response = ResponseBuilder.build_success_response(tool_result, id)
    {:response, response, new_state}
  end

  defp handle_tool_result({:error, reason, new_state}, tool_name, id) do
    error = normalize_error(reason, tool_name)
    error_response = ResponseBuilder.build_error_response(error, id)
    {:response, error_response, new_state}
  end

  defp normalize_tool_result(result) when is_list(result) do
    %{"content" => deep_stringify_keys(result)}
  end

  defp normalize_tool_result(%{content: content} = result) do
    result
    |> Map.delete(:content)
    |> Map.put("content", deep_stringify_keys(List.wrap(content)))
    |> maybe_put_is_error()
    |> deep_stringify_keys()
  end

  defp normalize_tool_result(%{"content" => _content} = result), do: deep_stringify_keys(result)

  defp normalize_tool_result(result) when is_binary(result),
    do: %{"content" => [%{"type" => "text", "text" => result}]}

  defp normalize_tool_result(result) when is_map(result), do: deep_stringify_keys(result)

  defp maybe_put_is_error(%{is_error?: true} = result), do: Map.put(result, "isError", true)
  defp maybe_put_is_error(result), do: result

  # Normalize various error formats to Error structs
  defp normalize_error(%Error.ProtocolError{} = err, _tool_name), do: err
  defp normalize_error(%Error.TransportError{} = err, _tool_name), do: err
  defp normalize_error(%Error.ToolError{} = err, _tool_name), do: err
  defp normalize_error(%Error.ResourceError{} = err, _tool_name), do: err
  defp normalize_error(%Error.ValidationError{} = err, _tool_name), do: err

  defp normalize_error(reason, tool_name) do
    # Create ToolError struct for plain reasons
    %Error.ToolError{
      tool_name: tool_name || "unknown",
      reason: reason,
      arguments: nil
    }
  end

  # Resources list request
  defp process_resources_list(%{"id" => id}, state) do
    module = state.__module__

    if function_exported?(module, :handle_list_resources, 2) do
      case module.handle_list_resources(nil, state) do
        {:ok, resources, next_cursor, new_state} ->
          result = %{"resources" => deep_stringify_keys(resources)}
          result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, to_string(reason))
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      response = ResponseBuilder.build_success_response(%{"resources" => []}, id)
      {:response, response, state}
    end
  end

  # Resources read request
  defp process_resources_read(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    uri = Map.get(params, "uri")
    module = state.__module__

    if function_exported?(module, :handle_read_resource, 2) do
      metadata = %{
        uri: uri || "unknown",
        request_id: to_string(id)
      }

      result =
        ExMCP.Telemetry.span([:ex_mcp, :resource, :read], metadata, fn ->
          module.handle_read_resource(uri, state)
        end)

      case result do
        {:ok, content, new_state} ->
          # Calculate content size if possible
          bytes =
            case content do
              %{"text" => text} when is_binary(text) -> byte_size(text)
              _ -> nil
            end

          if bytes do
            :telemetry.execute(
              [:ex_mcp, :resource, :read, :bytes],
              %{bytes: bytes},
              metadata
            )
          end

          result = %{"contents" => deep_stringify_keys(List.wrap(content))}
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.resource_error(uri || "unknown", :read, reason)
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      # No custom implementation, return default error
      error = Error.resource_error(uri || "unknown", :read, "Resource reading not implemented")
      error_response = ResponseBuilder.build_error_response(error, id)
      {:response, error_response, state}
    end
  end

  # Prompts list request
  defp process_prompts_list(%{"id" => id}, state) do
    module = state.__module__

    if function_exported?(module, :handle_list_prompts, 2) do
      case module.handle_list_prompts(nil, state) do
        {:ok, prompts, next_cursor, new_state} ->
          result = %{"prompts" => deep_stringify_keys(prompts)}
          result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, to_string(reason))
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      response = ResponseBuilder.build_success_response(%{"prompts" => []}, id)
      {:response, response, state}
    end
  end

  # Prompts get request
  defp process_prompts_get(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    prompt_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})
    module = state.__module__

    # Check if custom implementation exists
    if function_exported?(module, :handle_get_prompt, 3) do
      case module.handle_get_prompt(prompt_name, arguments, state) do
        {:ok, result, new_state} ->
          response = ResponseBuilder.build_success_response(deep_stringify_keys(result), id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, inspect(reason))
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      # No custom implementation, return default error
      error = Error.protocol_error(-32000, "Prompt retrieval not implemented")
      error_response = ResponseBuilder.build_error_response(error, id)
      {:response, error_response, state}
    end
  end

  # Initialized notification
  defp process_initialized_notification(_request, state) do
    # This is a notification from client, not a request - just acknowledge it
    {:notification, state}
  end

  # Elicitation complete notification (2025-11-25)
  defp process_elicitation_complete(%{"params" => params}, state) do
    module = state.__module__
    elicitation_id = Map.get(params, "elicitationId", "")

    if function_exported?(module, :handle_elicitation_complete, 2) do
      case module.handle_elicitation_complete(elicitation_id, state) do
        {:ok, new_state} -> {:notification, new_state}
        {:error, _reason, new_state} -> {:notification, new_state}
      end
    else
      {:notification, state}
    end
  end

  defp process_elicitation_complete(_request, state), do: {:notification, state}

  # Task status notification (2025-11-25)
  defp process_task_status_notification(_request, state), do: {:notification, state}

  # Tasks/get (2025-11-25)
  defp process_task_get(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    task_id = Map.get(params, "taskId")
    module = state.__module__

    if function_exported?(module, :handle_task_get, 2) do
      case module.handle_task_get(task_id, state) do
        {:ok, result, new_state} ->
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, to_string(reason))
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      error = Error.protocol_error(-32601, "Method not found: tasks/get")
      error_response = ResponseBuilder.build_error_response(error, id)
      {:response, error_response, state}
    end
  end

  # Tasks/list (2025-11-25)
  defp process_task_list(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    cursor = Map.get(params, "cursor")
    module = state.__module__

    if function_exported?(module, :handle_task_list, 2) do
      case module.handle_task_list(cursor, state) do
        {:ok, tasks, next_cursor, new_state} ->
          result = %{"tasks" => tasks}
          result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, to_string(reason))
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      error = Error.protocol_error(-32601, "Method not found: tasks/list")
      error_response = ResponseBuilder.build_error_response(error, id)
      {:response, error_response, state}
    end
  end

  # Tasks/result (2025-11-25)
  defp process_task_result(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    task_id = Map.get(params, "taskId")
    module = state.__module__

    if function_exported?(module, :handle_task_result, 2) do
      case module.handle_task_result(task_id, state) do
        {:ok, result, new_state} ->
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, to_string(reason))
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      error = Error.protocol_error(-32601, "Method not found: tasks/result")
      error_response = ResponseBuilder.build_error_response(error, id)
      {:response, error_response, state}
    end
  end

  # Tasks/cancel (2025-11-25)
  defp process_task_cancel(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    task_id = Map.get(params, "taskId")
    module = state.__module__

    if function_exported?(module, :handle_task_cancel, 2) do
      case module.handle_task_cancel(task_id, state) do
        {:ok, result, new_state} ->
          response = ResponseBuilder.build_success_response(result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.protocol_error(-32000, to_string(reason))
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      error = Error.protocol_error(-32601, "Method not found: tasks/cancel")
      error_response = ResponseBuilder.build_error_response(error, id)
      {:response, error_response, state}
    end
  end

  # Unknown method
  defp process_unknown_method(%{"method" => method, "id" => id}, state) do
    error = Error.protocol_error(-32601, "Method not found: #{method}")
    error_response = ResponseBuilder.build_error_response(error, id)
    {:response, error_response, state}
  end

  defp get_server_info(state) do
    if function_exported?(state.__module__, :get_server_info_from_opts, 0) do
      state.__module__.get_server_info_from_opts()
    else
      %{"name" => "ExMCP Server", "version" => "0.1.0"}
    end
  end

  defp get_capabilities(state) do
    if function_exported?(state.__module__, :get_capabilities, 0) do
      state.__module__.get_capabilities()
    else
      %{}
    end
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
