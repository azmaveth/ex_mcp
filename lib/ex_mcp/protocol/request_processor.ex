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
  require Logger

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
      case method do
        "initialize" -> process_initialize(request, state)
        "tools/list" -> process_tools_list(request, state)
        "tools/call" -> process_tools_call(request, state)
        "resources/list" -> process_resources_list(request, state)
        "resources/read" -> process_resources_read(request, state)
        "prompts/list" -> process_prompts_list(request, state)
        "prompts/get" -> process_prompts_get(request, state)
        "notifications/initialized" -> process_initialized_notification(request, state)
        _ -> process_unknown_method(request, state)
      end
    end)
  end

  def process(_request, state) do
    # Invalid request format
    error = Error.protocol_error(-32600, "Invalid Request")
    error_response = ResponseBuilder.build_error_response(error, nil)
    {:response, error_response, state}
  end

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
    tools = get_tools(state) |> Map.values()
    result = %{"tools" => tools}
    response = ResponseBuilder.build_success_response(result, id)
    {:response, response, state}
  end

  # Tools call request - delegates to handle_call for async processing
  defp process_tools_call(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})
    module = state.__module__

    # Check if the module has custom handle_tool_call implementation
    if function_exported?(module, :handle_tool_call, 3) do
      metadata = %{
        tool_name: tool_name || "unknown",
        request_id: to_string(id)
      }

      result =
        ExMCP.Telemetry.span([:ex_mcp, :tool], metadata, fn ->
          module.handle_tool_call(tool_name, arguments, state)
        end)

      case result do
        {:ok, result, new_state} ->
          # Build tool response with proper structure
          tool_result =
            Map.merge(
              %{"content" => result.content},
              if(Map.get(result, :is_error?, false), do: %{"isError" => true}, else: %{})
            )

          response = ResponseBuilder.build_success_response(tool_result, id)
          {:response, response, new_state}

        {:error, reason, new_state} ->
          error = Error.tool_error(tool_name || "unknown", reason)
          error_response = ResponseBuilder.build_error_response(error, id)
          {:response, error_response, new_state}
      end
    else
      # No custom implementation, return default error
      error = Error.tool_error(tool_name || "unknown", "Tool not implemented")
      error_response = ResponseBuilder.build_error_response(error, id)
      {:response, error_response, state}
    end
  end

  # Resources list request
  defp process_resources_list(%{"id" => id}, state) do
    resources = get_resources(state) |> Map.values()
    result = %{"resources" => resources}
    response = ResponseBuilder.build_success_response(result, id)
    {:response, response, state}
  end

  # Resources read request
  defp process_resources_read(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    uri = Map.get(params, "uri")
    module = state.__module__

    # Check if the module has custom handle_resource_read implementation
    if function_exported?(module, :handle_resource_read, 3) do
      metadata = %{
        uri: uri || "unknown",
        request_id: to_string(id)
      }

      result =
        ExMCP.Telemetry.span([:ex_mcp, :resource, :read], metadata, fn ->
          module.handle_resource_read(uri, uri, state)
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

          result = %{"contents" => [content]}
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
    prompts = get_prompts(state) |> Map.values()
    result = %{"prompts" => prompts}
    response = ResponseBuilder.build_success_response(result, id)
    {:response, response, state}
  end

  # Prompts get request
  defp process_prompts_get(%{"id" => id} = request, state) do
    params = Map.get(request, "params", %{})
    prompt_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})
    module = state.__module__

    # Check if custom implementation exists
    if function_exported?(module, :handle_prompt_get, 3) do
      case module.handle_prompt_get(prompt_name, arguments, state) do
        {:ok, result, new_state} ->
          response = ResponseBuilder.build_success_response(result, id)
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

  # Unknown method
  defp process_unknown_method(%{"method" => method, "id" => id}, state) do
    error = Error.protocol_error(-32601, "Method not found: #{method}")
    error_response = ResponseBuilder.build_error_response(error, id)
    {:response, error_response, state}
  end

  # Helper functions to get data from state
  # These assume the state has these functions available from the DSL

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

  defp get_tools(state) do
    if function_exported?(state.__module__, :get_tools, 0) do
      state.__module__.get_tools()
    else
      %{}
    end
  end

  defp get_resources(state) do
    if function_exported?(state.__module__, :get_resources, 0) do
      state.__module__.get_resources()
    else
      %{}
    end
  end

  defp get_prompts(state) do
    if function_exported?(state.__module__, :get_prompts, 0) do
      state.__module__.get_prompts()
    else
      %{}
    end
  end
end
