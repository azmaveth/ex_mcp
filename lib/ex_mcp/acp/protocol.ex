defmodule ExMCP.ACP.Protocol do
  @moduledoc """
  ACP-specific message encoding.

  Delegates JSON-RPC 2.0 framing to `ExMCP.Internal.Protocol` and adds
  ACP method-specific encoding on top.

  ACP uses integer protocol versions (default: 1) rather than MCP's date-based strings.
  """

  alias ExMCP.Internal.Protocol

  @default_protocol_version 1

  @doc "Generates a unique request ID."
  defdelegate generate_id, to: Protocol

  @doc "Encodes a JSON-RPC success response."
  defdelegate encode_response(result, id), to: Protocol

  @doc "Encodes a JSON-RPC error response."
  defdelegate encode_error(code, message, data \\ nil, id), to: Protocol

  @doc "Parses a raw JSON-RPC message without validation."
  defdelegate parse_message(data), to: Protocol, as: :parse_message_unvalidated

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

    %{
      "jsonrpc" => "2.0",
      "method" => "initialize",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc "Encodes a `session/new` request."
  @spec encode_session_new(String.t() | nil, [map()] | nil) :: map()
  def encode_session_new(cwd \\ nil, mcp_servers \\ nil) do
    params = %{}
    params = maybe_put(params, "cwd", cwd)
    # Always include mcpServers (some agents like Gemini require it even if empty)
    params = Map.put(params, "mcpServers", mcp_servers || [])

    %{
      "jsonrpc" => "2.0",
      "method" => "session/new",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc "Encodes a `session/load` request to resume an existing session."
  @spec encode_session_load(String.t(), String.t() | nil, [map()] | nil) :: map()
  def encode_session_load(session_id, cwd \\ nil, mcp_servers \\ nil) do
    params = %{"sessionId" => session_id}
    params = maybe_put(params, "cwd", cwd)
    params = maybe_put(params, "mcpServers", mcp_servers)

    %{
      "jsonrpc" => "2.0",
      "method" => "session/load",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc "Encodes a `session/list` request. Stabilized in ACP spec March 9, 2026."
  @spec encode_session_list(keyword()) :: map()
  def encode_session_list(opts \\ []) do
    params = %{}
    params = maybe_put(params, "cursor", Keyword.get(opts, :cursor))

    %{
      "jsonrpc" => "2.0",
      "method" => "session/list",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc """
  Encodes an `authenticate` request.

  Authentication is currently in RFD draft stage in the ACP spec.
  Error code -32000 indicates authentication is required.

  ## Parameters

  - `params` — authentication parameters (provider-specific)
  """
  @spec encode_authenticate(map()) :: map()
  def encode_authenticate(params \\ %{}) do
    %{
      "jsonrpc" => "2.0",
      "method" => "authenticate",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc "Encodes a `session/prompt` request."
  @spec encode_session_prompt(String.t(), [map()]) :: map()
  def encode_session_prompt(session_id, content_blocks) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/prompt",
      "params" => %{"sessionId" => session_id, "prompt" => content_blocks},
      "id" => generate_id()
    }
  end

  @doc "Encodes a `session/cancel` notification (no id field)."
  @spec encode_session_cancel(String.t()) :: map()
  def encode_session_cancel(session_id) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/cancel",
      "params" => %{"sessionId" => session_id}
    }
  end

  @doc "Encodes a `session/setMode` request."
  @spec encode_session_set_mode(String.t(), String.t()) :: map()
  def encode_session_set_mode(session_id, mode_id) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/set_mode",
      "params" => %{"sessionId" => session_id, "modeId" => mode_id},
      "id" => generate_id()
    }
  end

  @doc "Encodes a `session/setConfigOption` request."
  @spec encode_session_set_config_option(String.t(), String.t(), any()) :: map()
  def encode_session_set_config_option(session_id, config_id, value) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/set_config_option",
      "params" => %{"sessionId" => session_id, "configId" => config_id, "value" => value},
      "id" => generate_id()
    }
  end

  # Responses to agent requests

  @doc "Encodes a response to a `session/request_permission` request from the agent."
  @spec encode_permission_response(integer() | String.t(), map()) :: map()
  def encode_permission_response(id, outcome) do
    encode_response(outcome, id)
  end

  @doc "Encodes a response to a `fs/read_text_file` request from the agent."
  @spec encode_file_read_response(integer() | String.t(), String.t()) :: map()
  def encode_file_read_response(id, content) do
    encode_response(%{"content" => content}, id)
  end

  @doc "Encodes a response to a `fs/write_text_file` request from the agent."
  @spec encode_file_write_response(integer() | String.t()) :: map()
  def encode_file_write_response(id) do
    encode_response(nil, id)
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
