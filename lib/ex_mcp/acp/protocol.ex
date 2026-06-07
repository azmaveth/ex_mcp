defmodule ExMCP.ACP.Protocol do
  @moduledoc """
  ACP-specific message encoding.

  Delegates JSON-RPC 2.0 framing to `ExMCP.Internal.Protocol` and adds
  ACP method-specific encoding on top.

  ACP uses integer protocol versions (default: 1) rather than MCP's date-based strings.
  """

  alias ExMCP.ACP.{Envelope, LifecycleParams, Maps, Meta}
  alias ExMCP.Internal.Protocol

  @default_protocol_version 1
  @stop_reasons ~w(end_turn max_tokens max_turn_requests refusal cancelled)

  @doc "Generates a unique request ID."
  defdelegate generate_id, to: Protocol

  @doc "Encodes a JSON-RPC success response."
  defdelegate encode_response(result, id), to: Protocol

  @doc "Encodes a JSON-RPC error response."
  defdelegate encode_error(code, message, data \\ nil, id), to: Protocol

  @doc "Parses a raw ACP JSON-RPC message with structural validation."
  @spec parse_message(String.t() | map()) ::
          {:request, String.t(), map(), integer() | String.t()}
          | {:notification, String.t(), map()}
          | {:result, any(), integer() | String.t()}
          | {:error, map(), integer() | String.t() | nil}
          | {:error, :invalid_message}
  def parse_message(data) when is_binary(data) do
    case Jason.decode(data) do
      {:ok, decoded} -> parse_message(decoded)
      {:error, _} -> {:error, :invalid_message}
    end
  end

  def parse_message(%{"jsonrpc" => "2.0"} = message) do
    cond do
      mixed_request_response?(message) ->
        {:error, :invalid_message}

      Map.has_key?(message, "method") ->
        parse_method_message(message)

      Map.has_key?(message, "result") ->
        parse_result_message(message)

      Map.has_key?(message, "error") ->
        parse_error_message(message)

      true ->
        {:error, :invalid_message}
    end
  end

  def parse_message(_), do: {:error, :invalid_message}

  defp mixed_request_response?(message) do
    (Map.has_key?(message, "method") and
       (Map.has_key?(message, "result") or Map.has_key?(message, "error"))) or
      (Map.has_key?(message, "result") and Map.has_key?(message, "error"))
  end

  defp parse_method_message(%{"method" => method, "id" => id} = message)
       when is_binary(method) and (is_integer(id) or is_binary(id)) do
    case Map.get(message, "params", %{}) do
      params when is_map(params) -> {:request, method, params, id}
      _ -> {:error, :invalid_message}
    end
  end

  defp parse_method_message(%{"method" => method, "id" => _id}) when is_binary(method) do
    {:error, :invalid_message}
  end

  defp parse_method_message(%{"method" => method} = message) when is_binary(method) do
    case Map.get(message, "params", %{}) do
      params when is_map(params) -> {:notification, method, params}
      _ -> {:error, :invalid_message}
    end
  end

  defp parse_method_message(_), do: {:error, :invalid_message}

  defp parse_result_message(%{"result" => result, "id" => id})
       when is_integer(id) or is_binary(id) do
    {:result, result, id}
  end

  defp parse_result_message(_), do: {:error, :invalid_message}

  defp parse_error_message(%{
         "error" => %{"code" => code, "message" => message} = error,
         "id" => id
       })
       when is_integer(code) and is_binary(message) and
              (is_integer(id) or is_binary(id) or is_nil(id)) do
    {:error, error, id}
  end

  defp parse_error_message(_), do: {:error, :invalid_message}

  # ACP Request Encoding

  @doc """
  Encodes an `initialize` request.

  ## Parameters

  - `client_info` — `%{"name" => ..., "version" => ...}`
  - `capabilities` — client capabilities map (optional)
  - `protocol_version` — integer (default: #{@default_protocol_version})
  """
  @spec encode_initialize(map(), map() | nil, pos_integer()) :: map()
  def encode_initialize(
        client_info,
        capabilities \\ nil,
        protocol_version \\ @default_protocol_version
      ) do
    params = %{"clientInfo" => client_info, "protocolVersion" => protocol_version}

    params =
      if capabilities, do: Map.put(params, "clientCapabilities", capabilities), else: params

    Envelope.request("initialize", params, generate_id())
  end

  @doc """
  Encodes a `session/new` request.

  `cwd` is required by the ACP spec
  (https://agentclientprotocol.com/protocol/session-setup) and must be an
  absolute path string. Passing `nil` raises `FunctionClauseError`.
  """
  @spec encode_session_new(String.t(), keyword() | map() | [map()] | nil) :: map()
  def encode_session_new(cwd, opts \\ nil) when is_binary(cwd) do
    params = session_lifecycle_params(%{"cwd" => cwd}, opts)

    Envelope.request("session/new", params, generate_id())
  end

  @doc """
  Encodes a `session/load` request to load an existing session and replay history.

  `cwd` is required by the ACP spec
  (https://agentclientprotocol.com/protocol/session-setup) and must be an
  absolute path string. Passing `nil` raises `FunctionClauseError`.
  """
  @spec encode_session_load(String.t(), String.t(), keyword() | map() | [map()] | nil) :: map()
  def encode_session_load(session_id, cwd, opts \\ nil) when is_binary(cwd) do
    params = session_lifecycle_params(%{"sessionId" => session_id, "cwd" => cwd}, opts)

    Envelope.request("session/load", params, generate_id())
  end

  @doc "Encodes a `session/list` request. Stabilized in ACP spec March 9, 2026."
  @spec encode_session_list(keyword()) :: map()
  def encode_session_list(opts \\ []) do
    params = %{}
    params = maybe_put(params, "cursor", option_value(opts, :cursor, "cursor"))
    params = maybe_put(params, "cwd", option_value(opts, :cwd, "cwd"))

    Envelope.request("session/list", params, generate_id())
  end

  @doc """
  Encodes an `authenticate` request.

  ACP v1 authentication uses a `"methodId"` selected from the agent's
  `authMethods` initialize response. A map may still be passed for adapter
  compatibility.

  ## Parameters

  - `method_id_or_params` — auth method ID string or full params map
  """
  @spec encode_authenticate(String.t() | map()) :: map()
  def encode_authenticate(method_id_or_params \\ %{})

  def encode_authenticate(method_id) when is_binary(method_id) do
    encode_authenticate(%{"methodId" => method_id})
  end

  def encode_authenticate(params) when is_map(params) do
    Envelope.request("authenticate", params, generate_id())
  end

  @doc "Encodes a `logout` request. Stabilized in ACP spec May 21, 2026."
  @spec encode_logout() :: map()
  def encode_logout do
    Envelope.request("logout", %{}, generate_id())
  end

  @doc """
  Encodes a `session/resume` request. Stabilized in ACP spec April 22, 2026.

  `cwd` is required (same shape as `session/load` per
  https://agentclientprotocol.com/protocol/session-list). Passing `nil`
  raises `FunctionClauseError`.
  """
  @spec encode_session_resume(String.t(), String.t(), keyword() | map() | [map()] | nil) ::
          map()
  def encode_session_resume(session_id, cwd, opts \\ nil) when is_binary(cwd) do
    params = session_lifecycle_params(%{"sessionId" => session_id, "cwd" => cwd}, opts)

    Envelope.request("session/resume", params, generate_id())
  end

  @doc """
  Encodes a `session/delete` request. Gated by `sessionCapabilities.delete`
  per https://agentclientprotocol.com/protocol/session-list.
  """
  @spec encode_session_delete(String.t()) :: map()
  def encode_session_delete(session_id) when is_binary(session_id) do
    Envelope.request("session/delete", %{"sessionId" => session_id}, generate_id())
  end

  @doc "Encodes a `session/prompt` request."
  @spec encode_session_prompt(String.t(), [map()]) :: map()
  def encode_session_prompt(session_id, content_blocks) do
    Envelope.request(
      "session/prompt",
      %{"sessionId" => session_id, "prompt" => content_blocks},
      generate_id()
    )
  end

  @doc "Encodes a `session/cancel` notification (no id field)."
  @spec encode_session_cancel(String.t()) :: map()
  def encode_session_cancel(session_id) do
    Envelope.notification("session/cancel", %{"sessionId" => session_id})
  end

  @doc "Encodes a `session/close` request. Stabilized in ACP spec April 23, 2026."
  @spec encode_session_close(String.t()) :: map()
  def encode_session_close(session_id) do
    Envelope.request("session/close", %{"sessionId" => session_id}, generate_id())
  end

  @doc "Encodes a `session/set_mode` request."
  @spec encode_session_set_mode(String.t(), String.t()) :: map()
  def encode_session_set_mode(session_id, mode_id) do
    Envelope.request(
      "session/set_mode",
      %{"sessionId" => session_id, "modeId" => mode_id},
      generate_id()
    )
  end

  @doc "Encodes a `session/set_config_option` request."
  @spec encode_session_set_config_option(String.t(), String.t(), any()) :: map()
  def encode_session_set_config_option(session_id, config_id, value) do
    Envelope.request(
      "session/set_config_option",
      %{"sessionId" => session_id, "configId" => config_id, "value" => value},
      generate_id()
    )
  end

  # Agent response and notification encoding

  @doc "Encodes an agent `initialize` response."
  @spec encode_initialize_response(
          integer() | String.t(),
          map(),
          map() | nil,
          [map()] | nil,
          pos_integer()
        ) :: map()
  def encode_initialize_response(
        id,
        agent_info,
        capabilities \\ nil,
        auth_methods \\ nil,
        protocol_version \\ @default_protocol_version
      ) do
    result =
      %{"agentInfo" => agent_info, "protocolVersion" => protocol_version}
      |> maybe_put("agentCapabilities", capabilities)
      |> maybe_put("authMethods", auth_methods)

    encode_response(result, id)
  end

  @doc "Encodes a `session/new`, `session/load`, or similar session ID response."
  @spec encode_session_response(integer() | String.t(), String.t() | map() | nil) :: map()
  def encode_session_response(id, session_id) when is_binary(session_id) do
    encode_response(%{"sessionId" => session_id}, id)
  end

  def encode_session_response(id, result) when is_map(result) or is_nil(result) do
    encode_response(result || %{}, id)
  end

  @doc "Encodes a `session/list` response."
  @spec encode_session_list_response(integer() | String.t(), [map()], String.t() | nil) :: map()
  def encode_session_list_response(id, sessions, next_cursor \\ nil) when is_list(sessions) do
    %{"sessions" => sessions}
    |> maybe_put("nextCursor", next_cursor)
    |> encode_response(id)
  end

  @doc "Encodes a `session/prompt` response."
  @spec encode_prompt_response(integer() | String.t(), String.t() | map()) :: map()
  def encode_prompt_response(id, stop_reason) when is_binary(stop_reason) do
    encode_response(%{"stopReason" => validate_stop_reason!(stop_reason)}, id)
  end

  def encode_prompt_response(id, result) when is_map(result) do
    encode_response(normalize_prompt_result(result), id)
  end

  @doc "Encodes a stable ACP `session/update` notification."
  @spec encode_session_update(String.t(), map()) :: map()
  def encode_session_update(session_id, update) when is_binary(session_id) and is_map(update) do
    Envelope.notification("session/update", %{
      "sessionId" => session_id,
      "update" => update
    })
  end

  @doc "Encodes an `agent_message_chunk` update notification."
  @spec encode_agent_message_chunk(String.t(), String.t() | map()) :: map()
  def encode_agent_message_chunk(session_id, text) when is_binary(text) do
    encode_agent_message_chunk(session_id, %{"type" => "text", "text" => text})
  end

  def encode_agent_message_chunk(session_id, content) when is_map(content) do
    encode_session_update(session_id, %{
      "sessionUpdate" => "agent_message_chunk",
      "content" => content
    })
  end

  @doc "Encodes an `agent_thought_chunk` update notification."
  @spec encode_agent_thought_chunk(String.t(), String.t() | map()) :: map()
  def encode_agent_thought_chunk(session_id, text) when is_binary(text) do
    encode_agent_thought_chunk(session_id, %{"type" => "text", "text" => text})
  end

  def encode_agent_thought_chunk(session_id, content) when is_map(content) do
    encode_session_update(session_id, %{
      "sessionUpdate" => "agent_thought_chunk",
      "content" => content
    })
  end

  @doc "Encodes a `tool_call` update notification."
  @spec encode_tool_call(String.t(), map()) :: map()
  def encode_tool_call(session_id, tool_call) when is_map(tool_call) do
    encode_session_update(session_id, Map.put_new(tool_call, "sessionUpdate", "tool_call"))
  end

  @doc "Encodes a `tool_call_update` notification."
  @spec encode_tool_call_update(String.t(), map()) :: map()
  def encode_tool_call_update(session_id, tool_call_update) when is_map(tool_call_update) do
    encode_session_update(
      session_id,
      Map.put_new(tool_call_update, "sessionUpdate", "tool_call_update")
    )
  end

  @doc "Encodes an ACP `plan` update notification."
  @spec encode_plan(String.t(), [map()]) :: map()
  def encode_plan(session_id, entries) when is_list(entries) do
    encode_session_update(session_id, %{"sessionUpdate" => "plan", "entries" => entries})
  end

  @doc "Encodes an `available_commands_update` notification."
  @spec encode_available_commands_update(String.t(), [map()]) :: map()
  def encode_available_commands_update(session_id, commands) when is_list(commands) do
    encode_session_update(session_id, %{
      "sessionUpdate" => "available_commands_update",
      "availableCommands" => commands
    })
  end

  @doc "Encodes a `current_mode_update` notification."
  @spec encode_current_mode_update(String.t(), String.t()) :: map()
  def encode_current_mode_update(session_id, current_mode_id) do
    encode_session_update(session_id, %{
      "sessionUpdate" => "current_mode_update",
      "currentModeId" => current_mode_id
    })
  end

  @doc "Encodes a `config_option_update` notification."
  @spec encode_config_option_update(String.t(), [map()]) :: map()
  def encode_config_option_update(session_id, config_options) when is_list(config_options) do
    encode_session_update(session_id, %{
      "sessionUpdate" => "config_option_update",
      "configOptions" => config_options
    })
  end

  @doc "Encodes a `session_info_update` notification."
  @spec encode_session_info_update(String.t(), map()) :: map()
  def encode_session_info_update(session_id, info) when is_map(info) do
    encode_session_update(session_id, Map.put_new(info, "sessionUpdate", "session_info_update"))
  end

  @doc "Encodes a `usage_update` notification."
  @spec encode_usage_update(String.t(), non_neg_integer(), non_neg_integer(), map() | nil) ::
          map()
  def encode_usage_update(session_id, used, size, cost \\ nil)
      when is_integer(used) and used >= 0 and is_integer(size) and size >= 0 do
    update =
      %{"sessionUpdate" => "usage_update", "used" => used, "size" => size}
      |> maybe_put("cost", cost)

    encode_session_update(session_id, update)
  end

  # Agent requests to the client

  # Spec-defined PermissionOption.kind enum
  # (https://agentclientprotocol.com/protocol/tool-calls).
  @permission_option_kinds ~w(allow_once allow_always reject_once reject_always)

  @doc """
  Encodes a `session/request_permission` request from agent to client.

  Each option's `kind` must be one of the spec enums
  `allow_once`, `allow_always`, `reject_once`, `reject_always`
  per https://agentclientprotocol.com/protocol/tool-calls. Non-spec
  values raise `ArgumentError` — a client receiving an unrecognized
  kind cannot render the correct UI affordance.
  """
  @spec encode_permission_request(String.t(), map(), [map()]) :: map()
  def encode_permission_request(session_id, tool_call, options) when is_list(options) do
    tool_call = validate_permission_tool_call!(tool_call)
    options = Enum.map(options, &validate_permission_option!/1)

    Envelope.request(
      "session/request_permission",
      %{
        "sessionId" => session_id,
        "toolCall" => tool_call,
        "options" => options
      },
      generate_id()
    )
  end

  defp validate_permission_tool_call!(tool_call) when is_map(tool_call) do
    tool_call = Maps.stringify_keys(tool_call)

    case tool_call do
      %{"toolCallId" => id} when is_binary(id) and id != "" ->
        tool_call

      _ ->
        raise ArgumentError,
              "Permission toolCall is missing required `toolCallId`: #{inspect(tool_call)}"
    end
  end

  defp validate_permission_tool_call!(tool_call) do
    raise ArgumentError, "Permission toolCall must be a map: #{inspect(tool_call)}"
  end

  defp validate_permission_option!(option) when is_map(option) do
    option = Maps.stringify_keys(option)

    case option do
      %{"kind" => kind, "name" => name, "optionId" => option_id}
      when kind in @permission_option_kinds and is_binary(name) and name != "" and
             is_binary(option_id) and option_id != "" ->
        option

      %{"kind" => kind} when kind not in @permission_option_kinds ->
        raise_invalid_permission_kind!(kind)

      _ ->
        raise ArgumentError,
              "PermissionOption must include required `kind`, `name`, and `optionId` fields: " <>
                inspect(option)
    end
  end

  defp validate_permission_option!(option) do
    raise ArgumentError, "PermissionOption must be a map: #{inspect(option)}"
  end

  defp raise_invalid_permission_kind!(kind) do
    raise ArgumentError,
          "PermissionOption.kind #{inspect(kind)} is not in the spec enum " <>
            "(#{Enum.join(@permission_option_kinds, ", ")}). " <>
            "See https://agentclientprotocol.com/protocol/tool-calls."
  end

  @doc "Encodes an `fs/read_text_file` request from agent to client."
  @spec encode_file_read_request(String.t(), String.t(), keyword() | map()) :: map()
  def encode_file_read_request(session_id, path, opts \\ []) do
    params =
      %{"sessionId" => session_id, "path" => path}
      |> maybe_put("line", option_value(opts, :line, "line"))
      |> maybe_put("limit", option_value(opts, :limit, "limit"))

    Envelope.request("fs/read_text_file", params, generate_id())
  end

  @doc "Encodes an `fs/write_text_file` request from agent to client."
  @spec encode_file_write_request(String.t(), String.t(), String.t()) :: map()
  def encode_file_write_request(session_id, path, content) do
    Envelope.request(
      "fs/write_text_file",
      %{"sessionId" => session_id, "path" => path, "content" => content},
      generate_id()
    )
  end

  @doc "Encodes a stable `terminal/*` request from agent to client."
  @spec encode_terminal_request(String.t(), String.t(), map()) :: map()
  def encode_terminal_request(method, session_id, params)
      when is_binary(method) and is_binary(session_id) and is_map(params) do
    Envelope.request(method, Map.put_new(params, "sessionId", session_id), generate_id())
  end

  # Responses to agent requests

  @doc "Encodes a response to a `session/request_permission` request from the agent."
  @spec encode_permission_response(integer() | String.t(), map()) :: map()
  def encode_permission_response(id, %{"outcome" => %{"outcome" => _}} = response) do
    encode_response(response, id)
  end

  def encode_permission_response(id, outcome) do
    encode_response(%{"outcome" => outcome}, id)
  end

  @doc "Encodes a response to a `fs/read_text_file` request from the agent."
  @spec encode_file_read_response(integer() | String.t(), String.t()) :: map()
  def encode_file_read_response(id, content) do
    encode_response(%{"content" => content}, id)
  end

  @doc "Encodes a response to a `fs/write_text_file` request from the agent."
  @spec encode_file_write_response(integer() | String.t()) :: map()
  def encode_file_write_response(id) do
    encode_response(%{}, id)
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp session_lifecycle_params(params, opts) do
    params
    # Always include mcpServers (some agents like Gemini require it even if empty)
    |> LifecycleParams.normalize(opts)
  end

  defp normalize_prompt_result(%{"stopReason" => reason} = result) do
    result
    |> Map.put("stopReason", validate_stop_reason!(reason))
    |> move_prompt_response_extensions()
  end

  defp normalize_prompt_result(%{stopReason: _} = result) do
    result
    |> Maps.stringify_keys()
    |> normalize_prompt_result()
  end

  defp normalize_prompt_result(%{stop_reason: reason} = result) do
    result
    |> Map.delete(:stop_reason)
    |> Maps.stringify_keys()
    |> Map.put("stopReason", validate_stop_reason!(reason))
  end

  defp normalize_prompt_result(result), do: result

  defp validate_stop_reason!(reason) when reason in @stop_reasons, do: reason

  defp validate_stop_reason!(reason) do
    raise ArgumentError,
          "StopReason #{inspect(reason)} is not in the spec enum " <>
            "(#{Enum.join(@stop_reasons, ", ")}). " <>
            "See https://agentclientprotocol.com/protocol/prompt-turn."
  end

  defp move_prompt_response_extensions(result) do
    Meta.move_extensions(result, ["_meta", "stopReason", "usage"])
  end

  defp option_value(opts, atom_key, _string_key) when is_list(opts),
    do: Keyword.get(opts, atom_key)

  defp option_value(opts, atom_key, string_key) when is_map(opts) do
    Map.get(opts, string_key) || Map.get(opts, atom_key)
  end
end
