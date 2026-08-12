defmodule ExMCP.ACP.RequestValidation do
  @moduledoc false

  @uint32_max 4_294_967_295
  @uint64_max 18_446_744_073_709_551_615
  @terminal_id_methods ~w(
    terminal/output terminal/release terminal/wait_for_exit terminal/kill
  )
  @permission_kinds ~w(allow_once allow_always reject_once reject_always)
  @tool_kinds ~w(read edit delete move search execute think fetch switch_mode other)
  @tool_statuses ~w(pending in_progress completed failed)
  @plan_priorities ~w(high medium low)
  @plan_statuses ~w(pending in_progress completed)

  @spec validate_protocol_version(term(), [non_neg_integer()]) ::
          :ok | {:error, :invalid_protocol_version | {:unsupported_protocol_version, term()}}
  def validate_protocol_version(version, supported_versions) do
    with :ok <- validate_protocol_version(version) do
      if version in supported_versions,
        do: :ok,
        else: {:error, {:unsupported_protocol_version, version}}
    end
  end

  @spec validate_protocol_version(term()) :: :ok | {:error, :invalid_protocol_version}
  def validate_protocol_version(version)
      when is_integer(version) and version >= 0 and version <= 65_535,
      do: :ok

  def validate_protocol_version(_version), do: {:error, :invalid_protocol_version}

  @spec validate_agent_request(String.t(), term()) ::
          :ok | {:error, :method_not_found | :invalid_params}
  def validate_agent_request("session/request_permission", params),
    do: validate_permission_request(params)

  def validate_agent_request("fs/read_text_file", params), do: validate_file_read(params)
  def validate_agent_request("fs/write_text_file", params), do: validate_file_write(params)
  def validate_agent_request("terminal/create", params), do: validate_terminal_create(params)

  def validate_agent_request(method, params) when method in @terminal_id_methods,
    do: validate_terminal_id_request(params)

  def validate_agent_request("terminal/" <> _method, _params), do: {:error, :method_not_found}
  def validate_agent_request(_method, _params), do: :ok

  @spec validate_client_request(String.t(), term()) :: :ok | {:error, :invalid_params}
  def validate_client_request("authenticate", %{"methodId" => method_id}),
    do: require_non_empty_string(method_id)

  def validate_client_request("logout", params) when is_map(params), do: :ok

  def validate_client_request("session/new", params),
    do: validate_session_lifecycle(params, false, true)

  def validate_client_request("session/load", params),
    do: validate_session_lifecycle(params, true, true)

  def validate_client_request("session/resume", params),
    do: validate_session_lifecycle(params, true, false)

  def validate_client_request("session/fork", params),
    do: validate_session_lifecycle(params, true, false)

  def validate_client_request("session/list", params), do: validate_session_list(params)

  def validate_client_request(method, params)
      when method in ["session/close", "session/delete"],
      do: validate_session_id_params(params)

  def validate_client_request("session/prompt", params), do: validate_prompt(params)

  def validate_client_request("session/set_mode", %{
        "sessionId" => session_id,
        "modeId" => mode_id
      }) do
    require_strings([session_id, mode_id])
  end

  def validate_client_request("session/set_model", %{
        "sessionId" => session_id,
        "modelId" => model_id
      }) do
    require_strings([session_id, model_id])
  end

  def validate_client_request("session/set_config_option", params),
    do: validate_set_config_option(params)

  def validate_client_request(method, _params)
      when method in [
             "authenticate",
             "logout",
             "session/new",
             "session/load",
             "session/resume",
             "session/fork",
             "session/list",
             "session/close",
             "session/delete",
             "session/prompt",
             "session/set_mode",
             "session/set_model",
             "session/set_config_option"
           ],
      do: {:error, :invalid_params}

  def validate_client_request(_method, _params), do: :ok

  @spec validate_session_update(term()) :: :ok | {:error, :invalid_params}
  def validate_session_update(%{"sessionId" => session_id, "update" => update} = params)
      when is_map(update) do
    if valid_session_id?(session_id) and valid_session_update?(update) and
         optional_meta?(params) do
      :ok
    else
      {:error, :invalid_params}
    end
  end

  def validate_session_update(_params), do: {:error, :invalid_params}

  @spec absolute_path?(term()) :: boolean()
  def absolute_path?(path) when is_binary(path) and path != "",
    do: Path.type(path) == :absolute

  def absolute_path?(_path), do: false

  defp validate_permission_request(%{
         "sessionId" => session_id,
         "toolCall" => %{"toolCallId" => tool_call_id},
         "options" => options
       })
       when is_list(options) do
    if valid_session_id?(session_id) and non_empty_string?(tool_call_id) and
         Enum.all?(options, &valid_permission_option?/1) do
      :ok
    else
      {:error, :invalid_params}
    end
  end

  defp validate_permission_request(_params), do: {:error, :invalid_params}

  defp valid_permission_option?(%{
         "optionId" => option_id,
         "name" => name,
         "kind" => kind
       }) do
    non_empty_string?(option_id) and is_binary(name) and kind in @permission_kinds
  end

  defp valid_permission_option?(_option), do: false

  defp validate_session_lifecycle(params, require_session_id?, require_mcp_servers?)
       when is_map(params) do
    session_valid? = not require_session_id? or valid_session_id?(params["sessionId"])

    mcp_valid? =
      case Map.fetch(params, "mcpServers") do
        {:ok, servers} when is_list(servers) -> Enum.all?(servers, &valid_mcp_server?/1)
        :error -> not require_mcp_servers?
        _invalid -> false
      end

    directories = Map.get(params, "additionalDirectories", [])

    if session_valid? and absolute_path?(params["cwd"]) and mcp_valid? and
         is_list(directories) and Enum.all?(directories, &absolute_path?/1) do
      :ok
    else
      {:error, :invalid_params}
    end
  end

  defp validate_session_lifecycle(_params, _require_session_id?, _require_mcp_servers?),
    do: {:error, :invalid_params}

  defp valid_mcp_server?(%{"type" => type, "name" => name, "url" => url, "headers" => headers})
       when type in ["http", "sse"] and is_list(headers) do
    non_empty_string?(name) and non_empty_string?(url) and
      Enum.all?(headers, &valid_name_value?/1)
  end

  defp valid_mcp_server?(%{"type" => "acp", "name" => name, "serverId" => server_id}),
    do: non_empty_string?(name) and non_empty_string?(server_id)

  defp valid_mcp_server?(%{"name" => name, "command" => command, "args" => args, "env" => env})
       when is_list(args) and is_list(env) do
    non_empty_string?(name) and absolute_path?(command) and Enum.all?(args, &is_binary/1) and
      Enum.all?(env, &valid_name_value?/1)
  end

  defp valid_mcp_server?(_server), do: false

  defp valid_name_value?(%{"name" => name, "value" => value}),
    do: non_empty_string?(name) and is_binary(value)

  defp valid_name_value?(_value), do: false

  defp validate_session_list(params) when is_map(params) do
    cwd_valid? =
      case Map.fetch(params, "cwd") do
        :error -> true
        {:ok, nil} -> true
        {:ok, cwd} -> absolute_path?(cwd)
      end

    cursor_valid? =
      case Map.fetch(params, "cursor") do
        :error -> true
        {:ok, nil} -> true
        {:ok, cursor} -> is_binary(cursor)
      end

    if cwd_valid? and cursor_valid?, do: :ok, else: {:error, :invalid_params}
  end

  defp validate_session_list(_params), do: {:error, :invalid_params}

  defp validate_session_id_params(%{"sessionId" => session_id}) do
    if valid_session_id?(session_id), do: :ok, else: {:error, :invalid_params}
  end

  defp validate_session_id_params(_params), do: {:error, :invalid_params}

  defp validate_prompt(%{"sessionId" => session_id, "prompt" => prompt}) when is_list(prompt) do
    if valid_session_id?(session_id) and Enum.all?(prompt, &valid_content_block?/1),
      do: :ok,
      else: {:error, :invalid_params}
  end

  defp validate_prompt(_params), do: {:error, :invalid_params}

  defp valid_content_block?(%{"type" => "text", "text" => text} = block),
    do:
      is_binary(text) and optional_nullable_map?(block, "annotations") and
        optional_meta?(block)

  defp valid_content_block?(%{"type" => type, "data" => data, "mimeType" => mime_type} = block)
       when type in ["image", "audio"],
       do:
         is_binary(data) and is_binary(mime_type) and
           (type == "audio" or optional_nullable_string?(block, "uri")) and
           optional_nullable_map?(block, "annotations") and optional_meta?(block)

  defp valid_content_block?(%{"type" => "resource_link", "name" => name, "uri" => uri} = block),
    do:
      is_binary(name) and is_binary(uri) and optional_nullable_string?(block, "description") and
        optional_nullable_string?(block, "mimeType") and
        optional_nullable_string?(block, "title") and optional_nullable_int?(block, "size") and
        optional_nullable_map?(block, "annotations") and optional_meta?(block)

  defp valid_content_block?(%{"type" => "resource", "resource" => resource} = block)
       when is_map(resource),
       do:
         valid_embedded_resource?(resource) and optional_nullable_map?(block, "annotations") and
           optional_meta?(block)

  defp valid_content_block?(_block), do: false

  defp valid_embedded_resource?(resource),
    do:
      is_binary(resource["uri"]) and
        (is_binary(resource["text"]) or is_binary(resource["blob"])) and
        optional_nullable_string?(resource, "mimeType") and optional_meta?(resource)

  defp valid_session_update?(%{"sessionUpdate" => type} = update)
       when type in ["user_message_chunk", "agent_message_chunk", "agent_thought_chunk"] do
    valid_content_block?(update["content"]) and optional_nullable_string?(update, "messageId") and
      optional_meta?(update)
  end

  defp valid_session_update?(%{"sessionUpdate" => "tool_call"} = update) do
    non_empty_string?(update["toolCallId"]) and is_binary(update["title"]) and
      optional_enum?(update, "kind", @tool_kinds) and
      optional_enum?(update, "status", @tool_statuses) and
      optional_list?(update, "content", &valid_tool_call_content?/1) and
      optional_list?(update, "locations", &valid_tool_call_location?/1) and
      optional_nullable_string?(update, "name") and optional_meta?(update)
  end

  defp valid_session_update?(%{"sessionUpdate" => "tool_call_update"} = update) do
    non_empty_string?(update["toolCallId"]) and
      optional_nullable_enum?(update, "kind", @tool_kinds) and
      optional_nullable_enum?(update, "status", @tool_statuses) and
      optional_nullable_string?(update, "title") and
      optional_nullable_string?(update, "name") and
      optional_nullable_list?(update, "content", &valid_tool_call_content?/1) and
      optional_nullable_list?(update, "locations", &valid_tool_call_location?/1) and
      optional_meta?(update)
  end

  defp valid_session_update?(%{"sessionUpdate" => "plan", "entries" => entries} = update)
       when is_list(entries),
       do: Enum.all?(entries, &valid_plan_entry?/1) and optional_meta?(update)

  defp valid_session_update?(%{"sessionUpdate" => "plan_update", "plan" => plan} = update),
    do: valid_plan_update?(plan) and optional_meta?(update)

  defp valid_session_update?(%{"sessionUpdate" => "plan_removed", "planId" => plan_id} = update),
    do: non_empty_string?(plan_id) and optional_meta?(update)

  defp valid_session_update?(
         %{
           "sessionUpdate" => "available_commands_update",
           "availableCommands" => commands
         } = update
       )
       when is_list(commands),
       do: Enum.all?(commands, &valid_available_command?/1) and optional_meta?(update)

  defp valid_session_update?(
         %{
           "sessionUpdate" => "config_option_update",
           "configOptions" => options
         } = update
       )
       when is_list(options),
       do: Enum.all?(options, &valid_config_option?/1) and optional_meta?(update)

  defp valid_session_update?(
         %{
           "sessionUpdate" => "current_mode_update",
           "currentModeId" => mode_id
         } = update
       ),
       do: non_empty_string?(mode_id) and optional_meta?(update)

  defp valid_session_update?(%{"sessionUpdate" => "session_info_update"} = update),
    do:
      optional_nullable_string?(update, "title") and
        optional_nullable_string?(update, "updatedAt") and optional_meta?(update)

  defp valid_session_update?(
         %{
           "sessionUpdate" => "usage_update",
           "used" => used,
           "size" => size
         } = update
       ),
       do:
         uint?(used, @uint64_max) and uint?(size, @uint64_max) and
           optional_cost?(update) and optional_meta?(update)

  defp valid_session_update?(_update), do: false

  defp valid_tool_call_content?(%{"type" => "content", "content" => content} = value),
    do: valid_content_block?(content) and optional_meta?(value)

  defp valid_tool_call_content?(
         %{"type" => "diff", "path" => path, "newText" => new_text} = value
       ),
       do:
         is_binary(path) and is_binary(new_text) and optional_nullable_string?(value, "oldText") and
           optional_meta?(value)

  defp valid_tool_call_content?(%{"type" => "terminal", "terminalId" => terminal_id} = value),
    do: non_empty_string?(terminal_id) and optional_meta?(value)

  defp valid_tool_call_content?(_content), do: false

  defp valid_tool_call_location?(%{"path" => path} = location),
    do:
      is_binary(path) and optional_nullable_uint?(location, "line", @uint32_max) and
        optional_meta?(location)

  defp valid_tool_call_location?(_location), do: false

  defp valid_plan_entry?(
         %{"content" => content, "priority" => priority, "status" => status} = entry
       ),
       do:
         is_binary(content) and priority in @plan_priorities and status in @plan_statuses and
           optional_meta?(entry)

  defp valid_plan_entry?(_entry), do: false

  defp valid_plan_update?(%{"type" => "items", "planId" => plan_id, "entries" => entries} = plan)
       when is_list(entries),
       do:
         non_empty_string?(plan_id) and Enum.all?(entries, &valid_plan_entry?/1) and
           optional_meta?(plan)

  defp valid_plan_update?(%{"type" => "file", "planId" => plan_id, "uri" => uri} = plan),
    do: non_empty_string?(plan_id) and is_binary(uri) and optional_meta?(plan)

  defp valid_plan_update?(
         %{
           "type" => "markdown",
           "planId" => plan_id,
           "content" => content
         } = plan
       ),
       do: non_empty_string?(plan_id) and is_binary(content) and optional_meta?(plan)

  defp valid_plan_update?(_plan), do: false

  defp valid_available_command?(%{"name" => name, "description" => description} = command) do
    non_empty_string?(name) and is_binary(description) and optional_command_input?(command) and
      optional_meta?(command)
  end

  defp valid_available_command?(_command), do: false

  defp optional_command_input?(command) do
    case Map.fetch(command, "input") do
      :error -> true
      {:ok, nil} -> true
      {:ok, %{"hint" => hint} = input} -> is_binary(hint) and optional_meta?(input)
      _invalid -> false
    end
  end

  defp valid_config_option?(%{"id" => id, "name" => name, "type" => "select"} = option) do
    non_empty_string?(id) and is_binary(name) and non_empty_string?(option["currentValue"]) and
      valid_select_options?(option["options"]) and valid_config_option_common?(option)
  end

  defp valid_config_option?(%{"id" => id, "name" => name, "type" => "boolean"} = option) do
    non_empty_string?(id) and is_binary(name) and is_boolean(option["currentValue"]) and
      valid_config_option_common?(option)
  end

  defp valid_config_option?(_option), do: false

  defp valid_config_option_common?(option),
    do:
      optional_nullable_string?(option, "description") and
        optional_nullable_string?(option, "category") and optional_meta?(option)

  defp valid_select_options?(options) when is_list(options) do
    options == [] or Enum.all?(options, &valid_select_option?/1) or
      Enum.all?(options, &valid_select_group?/1)
  end

  defp valid_select_options?(_options), do: false

  defp valid_select_option?(%{"value" => value, "name" => name} = option),
    do:
      non_empty_string?(value) and is_binary(name) and
        optional_nullable_string?(option, "description") and optional_meta?(option)

  defp valid_select_option?(_option), do: false

  defp valid_select_group?(%{"group" => group, "name" => name, "options" => options} = value)
       when is_list(options),
       do:
         non_empty_string?(group) and is_binary(name) and
           Enum.all?(options, &valid_select_option?/1) and optional_meta?(value)

  defp valid_select_group?(_group), do: false

  defp optional_cost?(update) do
    case Map.fetch(update, "cost") do
      :error ->
        true

      {:ok, nil} ->
        true

      {:ok, %{"amount" => amount, "currency" => currency} = cost} ->
        is_number(amount) and is_binary(currency) and optional_meta?(cost)

      _invalid ->
        false
    end
  end

  defp optional_meta?(value) do
    case Map.fetch(value, "_meta") do
      :error -> true
      {:ok, nil} -> true
      {:ok, meta} -> is_map(meta)
    end
  end

  defp optional_enum?(value, key, allowed) do
    case Map.fetch(value, key) do
      :error -> true
      {:ok, candidate} -> candidate in allowed
    end
  end

  defp optional_nullable_enum?(value, key, allowed) do
    case Map.fetch(value, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, candidate} -> candidate in allowed
    end
  end

  defp optional_nullable_string?(value, key) do
    case Map.fetch(value, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, candidate} -> is_binary(candidate)
    end
  end

  defp optional_nullable_map?(value, key) do
    case Map.fetch(value, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, candidate} -> is_map(candidate)
    end
  end

  defp optional_nullable_int?(value, key) do
    case Map.fetch(value, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, candidate} -> is_integer(candidate)
    end
  end

  defp optional_list?(value, key, validator) do
    case Map.fetch(value, key) do
      :error -> true
      {:ok, items} when is_list(items) -> Enum.all?(items, validator)
      _invalid -> false
    end
  end

  defp optional_nullable_list?(value, key, validator) do
    case Map.fetch(value, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, items} when is_list(items) -> Enum.all?(items, validator)
      _invalid -> false
    end
  end

  defp optional_nullable_uint?(value, key, maximum) do
    case Map.fetch(value, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, candidate} -> uint?(candidate, maximum)
    end
  end

  defp uint?(value, maximum), do: is_integer(value) and value >= 0 and value <= maximum

  defp validate_set_config_option(
         %{
           "sessionId" => session_id,
           "configId" => config_id,
           "value" => value
         } = params
       ) do
    type_valid? = is_binary(value) or (is_boolean(value) and params["type"] == "boolean")

    if valid_session_id?(session_id) and non_empty_string?(config_id) and type_valid?,
      do: :ok,
      else: {:error, :invalid_params}
  end

  defp validate_set_config_option(_params), do: {:error, :invalid_params}

  defp require_strings(values) do
    if Enum.all?(values, &non_empty_string?/1), do: :ok, else: {:error, :invalid_params}
  end

  defp require_non_empty_string(value) do
    if non_empty_string?(value), do: :ok, else: {:error, :invalid_params}
  end

  defp non_empty_string?(value), do: is_binary(value) and value != ""

  defp validate_file_read(params) when is_map(params) do
    if valid_session_id?(params["sessionId"]) and absolute_path?(params["path"]) and
         optional_positive_uint?(params, "line", @uint32_max) and
         optional_uint?(params, "limit", @uint32_max) do
      :ok
    else
      {:error, :invalid_params}
    end
  end

  defp validate_file_read(_params), do: {:error, :invalid_params}

  defp validate_file_write(params) when is_map(params) do
    if valid_session_id?(params["sessionId"]) and absolute_path?(params["path"]) and
         is_binary(params["content"]) do
      :ok
    else
      {:error, :invalid_params}
    end
  end

  defp validate_file_write(_params), do: {:error, :invalid_params}

  defp validate_terminal_create(params) when is_map(params) do
    if valid_session_id?(params["sessionId"]) and is_binary(params["command"]) and
         params["command"] != "" and optional_string_list?(params, "args") and
         optional_env?(params) and optional_absolute_path?(params, "cwd") and
         optional_uint?(params, "outputByteLimit", @uint64_max) do
      :ok
    else
      {:error, :invalid_params}
    end
  end

  defp validate_terminal_create(_params), do: {:error, :invalid_params}

  defp validate_terminal_id_request(params) when is_map(params) do
    if valid_session_id?(params["sessionId"]) and is_binary(params["terminalId"]) and
         params["terminalId"] != "" do
      :ok
    else
      {:error, :invalid_params}
    end
  end

  defp validate_terminal_id_request(_params), do: {:error, :invalid_params}

  defp valid_session_id?(session_id), do: is_binary(session_id) and session_id != ""

  defp optional_uint?(params, key, maximum) do
    case Map.fetch(params, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, value} -> is_integer(value) and value >= 0 and value <= maximum
    end
  end

  defp optional_positive_uint?(params, key, maximum) do
    case Map.fetch(params, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, value} -> is_integer(value) and value >= 1 and value <= maximum
    end
  end

  defp optional_string_list?(params, key) do
    case Map.fetch(params, key) do
      :error -> true
      {:ok, values} when is_list(values) -> Enum.all?(values, &is_binary/1)
      _other -> false
    end
  end

  defp optional_env?(params) do
    case Map.fetch(params, "env") do
      :error ->
        true

      {:ok, values} when is_list(values) ->
        Enum.all?(values, fn
          %{"name" => name, "value" => value} -> is_binary(name) and is_binary(value)
          _other -> false
        end)

      _other ->
        false
    end
  end

  defp optional_absolute_path?(params, key) do
    case Map.fetch(params, key) do
      :error -> true
      {:ok, nil} -> true
      {:ok, path} -> absolute_path?(path)
    end
  end
end
