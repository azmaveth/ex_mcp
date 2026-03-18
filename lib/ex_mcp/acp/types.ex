defmodule ExMCP.ACP.Types do
  @moduledoc """
  Type specifications and builder functions for the Agent Client Protocol (ACP).

  ACP uses JSON-RPC 2.0 as its wire format (same as MCP). All types are plain maps
  matching the ex_mcp convention — no structs for protocol types.

  ## Content Blocks

  ACP supports text and image content blocks in prompts and responses:

      text_block("Hello, world!")
      image_block("image/png", "base64data...")

  ## Session Management

  Sessions track agent conversations. Create with `new_session_params/2`,
  send prompts with `prompt_params/2`.
  """

  # Content blocks

  @type content_block ::
          text_block()
          | image_block()
          | audio_block()
          | resource_link_block()
          | resource_block()

  @type text_block :: %{
          required(:type) => :text,
          required(:text) => String.t()
        }

  @type image_block :: %{
          required(:type) => :image,
          required(:mimeType) => String.t(),
          required(:data) => String.t()
        }

  @type audio_block :: %{
          required(:type) => :audio,
          required(:mimeType) => String.t(),
          required(:data) => String.t()
        }

  @type resource_link_block :: %{
          required(:type) => :resource_link,
          required(:uri) => String.t(),
          optional(:name) => String.t(),
          optional(:mimeType) => String.t()
        }

  @type resource_block :: %{
          required(:type) => :resource,
          required(:uri) => String.t(),
          optional(:name) => String.t(),
          optional(:mimeType) => String.t(),
          optional(:text) => String.t(),
          optional(:blob) => String.t()
        }

  # Capabilities

  @type client_capabilities :: %{
          optional(:fs) => %{
            optional(:readTextFile) => boolean(),
            optional(:writeTextFile) => boolean()
          },
          optional(:terminal) => boolean()
        }

  @type agent_capabilities :: %{
          optional(:loadSession) => boolean(),
          optional(:promptCapabilities) => %{
            optional(:image) => boolean(),
            optional(:audio) => boolean(),
            optional(:embeddedContext) => boolean()
          },
          optional(:mcpCapabilities) => %{
            optional(:http) => boolean(),
            optional(:sse) => boolean()
          },
          optional(:sessionCapabilities) => %{
            optional(:list) => session_list_capabilities() | nil
          }
        }

  @type session_list_capabilities :: %{
          optional(:supportsNameFilter) => boolean()
        }

  @type mode :: %{
          required(:id) => String.t(),
          required(:name) => String.t(),
          optional(:description) => String.t()
        }

  @type config_option :: %{
          required(:id) => String.t(),
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:type) => String.t(),
          optional(:default) => any()
        }

  # Initialize

  @type client_info :: %{
          required(:name) => String.t(),
          required(:version) => String.t()
        }

  @type agent_info :: %{
          required(:name) => String.t(),
          required(:version) => String.t()
        }

  @type initialize_request :: %{
          required(:clientInfo) => client_info(),
          optional(:clientCapabilities) => client_capabilities(),
          optional(:protocolVersion) => pos_integer()
        }

  @type initialize_response :: %{
          required(:agentInfo) => agent_info(),
          optional(:agentCapabilities) => agent_capabilities(),
          optional(:protocolVersion) => pos_integer()
        }

  # Sessions

  @type mcp_server :: %{
          required(:uri) => String.t(),
          optional(:name) => String.t()
        }

  @type new_session_request :: %{
          optional(:cwd) => String.t(),
          optional(:mcpServers) => [mcp_server()]
        }

  @type new_session_response :: %{
          required(:sessionId) => String.t()
        }

  @type list_sessions_request :: %{
          optional(:cursor) => String.t()
        }

  @type list_sessions_response :: %{
          required(:sessions) => [session_info()],
          optional(:nextCursor) => String.t()
        }

  @type session_info :: %{
          required(:sessionId) => String.t(),
          optional(:name) => String.t(),
          optional(:createdAt) => String.t()
        }

  @type load_session_request :: %{
          required(:sessionId) => String.t(),
          optional(:cwd) => String.t(),
          optional(:mcpServers) => [mcp_server()]
        }

  @type prompt_request :: %{
          required(:sessionId) => String.t(),
          required(:prompt) => [content_block()]
        }

  @type prompt_response :: %{
          required(:stopReason) => String.t()
        }

  # Session updates — nested under "update" with "sessionUpdate" discriminator
  #
  # Official ACP spec types (https://agentclientprotocol.com/protocol/schema):
  #   user_message_chunk, agent_message_chunk, tool_call_update, plan_update,
  #   available_commands_update, config_option_update, current_mode_update,
  #   session_info_update
  #
  # Extension types (not in spec, used by adapters):
  #   thinking, status, usage, error, tool_output, tool_call, tool_execution,
  #   tool_result, rpc_response, rpc_error, extension_ui_request

  @type session_update_params :: %{
          required(:sessionId) => String.t(),
          required(:update) => session_update()
        }

  @type session_update ::
          user_message_chunk_update()
          | agent_message_chunk_update()
          | tool_call_update()
          | plan_update()
          | available_commands_update()
          | config_option_update()
          | current_mode_update()
          | session_info_update()
          | thinking_update()
          | status_update()

  # ── Spec-defined session update types ──────────────────────────

  @type user_message_chunk_update :: %{
          required(:sessionUpdate) => :user_message_chunk,
          required(:content) => content_block()
        }

  @type agent_message_chunk_update :: %{
          required(:sessionUpdate) => :agent_message_chunk,
          required(:content) => content_block()
        }

  @type tool_call_update :: %{
          required(:sessionUpdate) => :tool_call_update,
          required(:toolCallId) => String.t(),
          required(:title) => String.t(),
          required(:status) => String.t(),
          optional(:content) => [content_block()]
        }

  @type plan_update :: %{
          required(:sessionUpdate) => :plan_update,
          required(:entries) => [plan_entry()]
        }

  @type plan_entry :: %{
          required(:content) => String.t(),
          required(:priority) => :high | :medium | :low,
          required(:status) => :pending | :in_progress | :completed
        }

  @type available_commands_update :: %{
          required(:sessionUpdate) => :available_commands_update,
          required(:commands) => [map()]
        }

  @type config_option_update :: %{
          required(:sessionUpdate) => :config_option_update,
          required(:configId) => String.t(),
          required(:value) => any()
        }

  @type current_mode_update :: %{
          required(:sessionUpdate) => :current_mode_update,
          required(:modeId) => String.t()
        }

  @type session_info_update :: %{
          required(:sessionUpdate) => :session_info_update,
          optional(:sessionName) => String.t(),
          optional(:metadata) => map()
        }

  # ── Extension session update types ─────────────────────────────

  @type thinking_update :: %{
          required(:sessionUpdate) => :thinking,
          required(:content) => String.t()
        }

  @type status_update :: %{
          required(:sessionUpdate) => :status,
          required(:status) => String.t(),
          optional(:message) => String.t()
        }

  # Permission handling

  @type permission_option :: %{
          required(:optionId) => String.t(),
          required(:name) => String.t(),
          required(:kind) => String.t(),
          optional(:description) => String.t()
        }

  @type permission_outcome :: %{
          required(:outcome) => String.t(),
          optional(:optionId) => String.t()
        }

  @type permission_request :: %{
          required(:sessionId) => String.t(),
          required(:toolCall) => tool_call_info(),
          required(:options) => [permission_option()]
        }

  @type tool_call_info :: %{
          required(:toolName) => String.t(),
          optional(:toolCallId) => String.t(),
          optional(:arguments) => map()
        }

  # File operations

  @type file_read_request :: %{
          required(:sessionId) => String.t(),
          required(:path) => String.t(),
          optional(:range) => file_range()
        }

  @type file_range :: %{
          optional(:start) => non_neg_integer(),
          optional(:end) => non_neg_integer()
        }

  @type file_write_request :: %{
          required(:sessionId) => String.t(),
          required(:path) => String.t(),
          required(:content) => String.t()
        }

  # Builder functions

  @doc "Creates a text content block."
  @spec text_block(String.t()) :: map()
  def text_block(text) when is_binary(text) do
    %{"type" => "text", "text" => text}
  end

  @doc "Creates an image content block."
  @spec image_block(String.t(), String.t()) :: map()
  def image_block(mime_type, data) when is_binary(mime_type) and is_binary(data) do
    %{"type" => "image", "mimeType" => mime_type, "data" => data}
  end

  @doc "Creates an audio content block."
  @spec audio_block(String.t(), String.t()) :: map()
  def audio_block(mime_type, data) when is_binary(mime_type) and is_binary(data) do
    %{"type" => "audio", "mimeType" => mime_type, "data" => data}
  end

  @doc "Creates a resource link content block."
  @spec resource_link_block(String.t(), keyword()) :: map()
  def resource_link_block(uri, opts \\ []) when is_binary(uri) do
    %{"type" => "resource_link", "uri" => uri}
    |> maybe_put_kw("name", opts)
    |> maybe_put_kw("mimeType", opts)
  end

  @doc "Creates a resource content block."
  @spec resource_block(String.t(), keyword()) :: map()
  def resource_block(uri, opts \\ []) when is_binary(uri) do
    %{"type" => "resource", "uri" => uri}
    |> maybe_put_kw("name", opts)
    |> maybe_put_kw("mimeType", opts)
    |> maybe_put_kw("text", opts)
    |> maybe_put_kw("blob", opts)
  end

  @doc "Creates client info for the initialize handshake."
  @spec client_info(String.t(), String.t()) :: map()
  def client_info(name, version) when is_binary(name) and is_binary(version) do
    %{"name" => name, "version" => version}
  end

  @doc """
  Creates params for a new session request.

  ## Options

  - `:mcp_servers` - list of MCP server maps with `:uri` and optional `:name`
  """
  @spec new_session_params(String.t() | nil, keyword()) :: map()
  def new_session_params(cwd \\ nil, opts \\ []) do
    params = %{}
    params = if cwd, do: Map.put(params, "cwd", cwd), else: params

    case Keyword.get(opts, :mcp_servers) do
      nil -> params
      servers -> Map.put(params, "mcpServers", servers)
    end
  end

  @doc """
  Creates params for a prompt request.

  Content can be a string (auto-wrapped as text block) or a list of content block maps.
  """
  @spec prompt_params(String.t(), String.t() | [map()]) :: map()
  def prompt_params(session_id, content) when is_binary(session_id) do
    blocks =
      case content do
        text when is_binary(text) -> [text_block(text)]
        blocks when is_list(blocks) -> blocks
      end

    %{"sessionId" => session_id, "prompt" => blocks}
  end

  # Private helpers

  defp maybe_put_kw(map, key, opts) do
    atom_key = String.to_existing_atom(key)

    case Keyword.get(opts, atom_key) do
      nil -> map
      value -> Map.put(map, key, value)
    end
  rescue
    ArgumentError -> map
  end
end
