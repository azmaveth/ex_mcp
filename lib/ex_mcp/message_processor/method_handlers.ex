defmodule ExMCP.MessageProcessor.MethodHandlers do
  @moduledoc false
  # Unified method handlers for all three server modes.
  # Each handler fetches data via the appropriate mode, then builds
  # a standard JSON-RPC response.

  require Logger

  @default_protocol_version "2025-11-25"

  # --- initialize ---

  def handle_initialize(conn, handler, :direct, _params, id, server_info) do
    put_success(
      conn,
      %{
        "protocolVersion" => @default_protocol_version,
        "capabilities" => deep_stringify_keys(handler.get_capabilities()),
        "serverInfo" => deep_stringify_keys(server_info)
      },
      id
    )
  end

  def handle_initialize(conn, server_pid, :genserver, _params, id, _server_info) do
    info = GenServer.call(server_pid, :get_server_info, 5000)
    capabilities = GenServer.call(server_pid, :get_capabilities, 5000)

    put_success(
      conn,
      %{
        "protocolVersion" => @default_protocol_version,
        "capabilities" => deep_stringify_keys(capabilities),
        "serverInfo" => deep_stringify_keys(info)
      },
      id
    )
  rescue
    error -> put_error(conn, "Initialize failed", error, id)
  end

  def handle_initialize(conn, server_pid, :handler, params, id, _server_info) do
    case GenServer.call(server_pid, {:handle_initialize, params}, 5000) do
      {:ok, result, _state} ->
        normalized = normalize_initialize_result(result)
        put_success(conn, deep_stringify_keys(normalized), id)

      {:error, reason} ->
        put_error(conn, "Initialize failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Initialize failed", error, id)
  end

  defp normalize_initialize_result(result) do
    result
    |> Map.put_new("protocolVersion", @default_protocol_version)
    |> Map.put_new(:protocolVersion, @default_protocol_version)
  end

  # --- tools/list ---

  def handle_tools_list(conn, handler, :direct, _params, id) do
    tools = handler.get_tools() |> Map.values() |> deep_stringify_keys()
    put_success(conn, %{"tools" => tools}, id)
  end

  def handle_tools_list(conn, server_pid, :genserver, _params, id) do
    tools = GenServer.call(server_pid, :get_tools, 5000) |> Map.values() |> deep_stringify_keys()
    put_success(conn, %{"tools" => tools}, id)
  rescue
    error -> put_error(conn, "Tools list failed", error, id)
  end

  def handle_tools_list(conn, server_pid, :handler, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_tools, cursor}, 5000) do
      {:ok, tools, next_cursor, _state} ->
        result = %{"tools" => deep_stringify_keys(tools)}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
        put_success(conn, result, id)

      {:error, reason} ->
        put_error(conn, "Tools list failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Tools list failed", error, id)
  end

  # --- tools/call ---

  def handle_tools_call(conn, handler, :direct, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :tool, :called],
      %{},
      %{tool_name: tool_name, mode: :direct}
    )

    case handler.handle_tool_call(tool_name, arguments, %{}) do
      {:ok, result} ->
        put_success(conn, wrap_tool_result(result), id)

      {:error, reason} ->
        put_success(conn, tool_error_result(reason), id)
    end
  end

  def handle_tools_call(conn, server_pid, :genserver, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :tool, :called],
      %{},
      %{tool_name: tool_name, mode: :genserver}
    )

    case GenServer.call(server_pid, {:execute_tool, tool_name, arguments}, 10000) do
      {:ok, result} ->
        put_success(conn, wrap_tool_result(result), id)

      {:error, reason} ->
        put_success(conn, tool_error_result(reason), id)
    end
  rescue
    error -> put_error(conn, "Tool call failed", error, id)
  end

  def handle_tools_call(conn, server_pid, :handler, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :tool, :called],
      %{},
      %{tool_name: tool_name, mode: :handler}
    )

    case GenServer.call(server_pid, {:call_tool, tool_name, arguments}, 10000) do
      {:ok, result} ->
        put_success(conn, wrap_tool_result(result), id)

      {:error, reason} ->
        put_success(conn, tool_error_result(reason), id)
    end
  rescue
    error -> put_error(conn, "Tool call failed", error, id)
  end

  # --- resources/list ---

  def handle_resources_list(conn, handler, :direct, _params, id) do
    resources = handler.get_resources() |> Map.values() |> deep_stringify_keys()
    put_success(conn, %{"resources" => resources}, id)
  end

  def handle_resources_list(conn, server_pid, :genserver, _params, id) do
    resources =
      GenServer.call(server_pid, :get_resources, 5000) |> Map.values() |> deep_stringify_keys()

    put_success(conn, %{"resources" => resources}, id)
  rescue
    error -> put_error(conn, "Resources list failed", error, id)
  end

  def handle_resources_list(conn, server_pid, :handler, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_resources, cursor}, 5000) do
      {:ok, resources, next_cursor, _state} ->
        result = %{"resources" => deep_stringify_keys(resources)}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
        put_success(conn, result, id)

      {:error, reason} ->
        put_error(conn, "Resources list failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Resources list failed", error, id)
  end

  # --- resources/read ---

  def handle_resources_read(conn, handler, :direct, params, id) do
    uri = Map.get(params, "uri")

    :telemetry.execute(
      [:ex_mcp, :server, :resource, :read],
      %{},
      %{uri: uri, mode: :direct}
    )

    case handler.handle_resource_read(uri, uri, %{}) do
      {:ok, contents, _state} ->
        put_success(conn, %{"contents" => deep_stringify_keys(List.wrap(contents))}, id)

      {:error, reason} ->
        put_error(conn, "Resource read failed", reason, id)
    end
  end

  def handle_resources_read(conn, server_pid, :genserver, params, id) do
    uri = Map.get(params, "uri")

    :telemetry.execute(
      [:ex_mcp, :server, :resource, :read],
      %{},
      %{uri: uri, mode: :genserver}
    )

    case GenServer.call(server_pid, {:read_resource, uri}, 5000) do
      {:ok, contents} ->
        put_success(conn, %{"contents" => deep_stringify_keys(List.wrap(contents))}, id)

      {:error, reason} ->
        put_error(conn, "Resource read failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Resource read failed", error, id)
  end

  def handle_resources_read(conn, server_pid, :handler, params, id) do
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

  # --- resources/subscribe ---

  def handle_resources_subscribe(conn, _handler, :direct, params, id) do
    _uri = Map.get(params, "uri")
    put_success(conn, %{}, id)
  end

  def handle_resources_subscribe(conn, server_pid, :genserver, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:subscribe_resource, uri}, 5000) do
      :ok -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Subscribe failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Subscribe failed", error, id)
  end

  def handle_resources_subscribe(conn, server_pid, :handler, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:subscribe_resource, uri}, 5000) do
      :ok -> put_success(conn, %{}, id)
      {:ok, _state} -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Subscribe failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Subscribe failed", error, id)
  end

  # --- resources/unsubscribe ---

  def handle_resources_unsubscribe(conn, _handler, :direct, params, id) do
    _uri = Map.get(params, "uri")
    put_success(conn, %{}, id)
  end

  def handle_resources_unsubscribe(conn, server_pid, :genserver, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:unsubscribe_resource, uri}, 5000) do
      :ok -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Unsubscribe failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Unsubscribe failed", error, id)
  end

  def handle_resources_unsubscribe(conn, server_pid, :handler, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:unsubscribe_resource, uri}, 5000) do
      :ok -> put_success(conn, %{}, id)
      {:ok, _state} -> put_success(conn, %{}, id)
      {:error, reason} -> put_error(conn, "Unsubscribe failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Unsubscribe failed", error, id)
  end

  # --- prompts/list ---

  def handle_prompts_list(conn, handler, :direct, _params, id) do
    prompts = handler.get_prompts() |> Map.values() |> deep_stringify_keys()
    put_success(conn, %{"prompts" => prompts}, id)
  end

  def handle_prompts_list(conn, server_pid, :genserver, _params, id) do
    prompts =
      GenServer.call(server_pid, :get_prompts, 5000) |> Map.values() |> deep_stringify_keys()

    put_success(conn, %{"prompts" => prompts}, id)
  rescue
    error -> put_error(conn, "Prompts list failed", error, id)
  end

  def handle_prompts_list(conn, server_pid, :handler, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_prompts, cursor}, 5000) do
      {:ok, prompts, next_cursor, _state} ->
        result = %{"prompts" => deep_stringify_keys(prompts)}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
        put_success(conn, result, id)

      {:error, reason} ->
        put_error(conn, "Prompts list failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Prompts list failed", error, id)
  end

  # --- prompts/get ---

  def handle_prompts_get(conn, handler, :direct, params, id) do
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :prompt, :rendered],
      %{},
      %{name: name, mode: :direct}
    )

    case handler.handle_get_prompt(name, arguments, %{}) do
      {:ok, result, _state} ->
        put_success(conn, deep_stringify_keys(result), id)

      {:error, reason} ->
        put_error(conn, "Prompt get failed", reason, id)
    end
  end

  def handle_prompts_get(conn, server_pid, :genserver, params, id) do
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :prompt, :rendered],
      %{},
      %{name: name, mode: :genserver}
    )

    case GenServer.call(server_pid, {:get_prompt, name, arguments}, 5000) do
      {:ok, result} ->
        put_success(conn, deep_stringify_keys(result), id)

      {:error, reason} ->
        put_error(conn, "Prompt get failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Prompt get failed", error, id)
  end

  def handle_prompts_get(conn, server_pid, :handler, params, id) do
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    :telemetry.execute(
      [:ex_mcp, :server, :prompt, :rendered],
      %{},
      %{name: name, mode: :handler}
    )

    case GenServer.call(server_pid, {:get_prompt, name, arguments}, 5000) do
      {:ok, result, _state} ->
        put_success(conn, deep_stringify_keys(result), id)

      {:ok, result} ->
        put_success(conn, deep_stringify_keys(result), id)

      {:error, reason} ->
        put_error(conn, "Prompt get failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Prompt get failed", error, id)
  end

  # --- completion/complete ---

  def handle_completion_complete(conn, _handler, :direct, _params, id) do
    put_success(conn, %{"completion" => %{"values" => [], "hasMore" => false, "total" => 0}}, id)
  end

  def handle_completion_complete(conn, server_pid, :genserver, params, id) do
    case GenServer.call(server_pid, {:complete, params}, 5000) do
      {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Completion failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Completion failed", error, id)
  end

  def handle_completion_complete(conn, server_pid, :handler, params, id) do
    case GenServer.call(server_pid, {:complete, params}, 5000) do
      {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
      {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
      {:error, reason} -> put_error(conn, "Completion failed", reason, id)
    end
  rescue
    error -> put_error(conn, "Completion failed", error, id)
  end

  # --- custom method ---

  def handle_custom_method(conn, handler, :direct, method, params, id) do
    if function_exported?(handler, :handle_custom_request, 3) do
      case handler.handle_custom_request(method, params, %{}) do
        {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
        {:error, reason} -> put_error(conn, "Custom method failed", reason, id)
      end
    else
      put_method_not_found(conn, id)
    end
  end

  def handle_custom_method(conn, server_pid, :genserver, method, params, id) do
    case GenServer.call(server_pid, {:custom_method, method, params}, 5000) do
      {:ok, result} ->
        put_success(conn, deep_stringify_keys(result), id)

      {:error, reason} when reason in [:method_not_found, :unknown_request] ->
        put_method_not_found(conn, id)

      {:error, reason} ->
        put_error(conn, "Custom method failed", reason, id)

      _ ->
        put_method_not_found(conn, id)
    end
  catch
    :exit, _ -> put_method_not_found(conn, id)
  end

  def handle_custom_method(conn, server_pid, :handler, method, params, id) do
    case GenServer.call(server_pid, {:custom_request, method, params}, 5000) do
      {:ok, result, _state} -> put_success(conn, deep_stringify_keys(result), id)
      {:ok, result} -> put_success(conn, deep_stringify_keys(result), id)
      {:error, :method_not_found} -> put_method_not_found(conn, id)
      {:error, reason} -> put_error(conn, "Custom method failed", reason, id)
    end
  rescue
    _ -> put_method_not_found(conn, id)
  end

  # --- Response helpers ---

  defp put_success(conn, result, id) do
    response = %{"jsonrpc" => "2.0", "result" => result, "id" => id}
    %{conn | response: response}
  end

  defp put_error(conn, message, reason, id) do
    response = %{
      "jsonrpc" => "2.0",
      "error" => %{
        "code" => -32603,
        "message" => message,
        "data" => %{"reason" => inspect(reason)}
      },
      "id" => id
    }

    %{conn | response: response}
  end

  defp put_method_not_found(conn, id) do
    response = %{
      "jsonrpc" => "2.0",
      "error" => %{
        "code" => -32601,
        "message" => "Method not found"
      },
      "id" => id
    }

    %{conn | response: response}
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

  defp wrap_tool_result(result) when is_map(result) do
    deep_stringify_keys(result)
  end

  defp tool_error_result(reason) do
    %{
      "content" => [%{"type" => "text", "text" => to_string(reason)}],
      "isError" => true
    }
  end

  # Recursively convert atom keys to strings
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
