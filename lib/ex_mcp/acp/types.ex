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

  @type content_block :: text_block() | image_block()

  @type text_block :: %{
          required(:type) => :text,
          required(:text) => String.t()
        }

  @type image_block :: %{
          required(:type) => :image,
          required(:mimeType) => String.t(),
          required(:data) => String.t()
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
          optional(:mcp) => %{
            optional(:http) => boolean(),
            optional(:sse) => boolean()
          }
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
  # Spec-defined types: agent_message_chunk, tool_call, tool_call_update, plan
  # Extension types: thinking, status, usage, error, tool_output

  @type session_update_params :: %{
          required(:sessionId) => String.t(),
          required(:update) => session_update()
        }

  @type session_update ::
          agent_message_chunk_update()
          | tool_call_update()
          | tool_call_status_update()
          | plan_update()
          | thinking_update()
          | status_update()

  @type agent_message_chunk_update :: %{
          required(:sessionUpdate) => :agent_message_chunk,
          required(:content) => content_block()
        }

  @type tool_call_update :: %{
          required(:sessionUpdate) => :tool_call,
          required(:toolCallId) => String.t(),
          required(:title) => String.t(),
          required(:kind) => String.t(),
          required(:status) => String.t()
        }

  @type tool_call_status_update :: %{
          required(:sessionUpdate) => :tool_call_update,
          required(:toolCallId) => String.t(),
          required(:status) => String.t(),
          optional(:content) => [map()]
        }

  @type plan_update :: %{
          required(:sessionUpdate) => :plan,
          required(:entries) => [plan_entry()]
        }

  @type plan_entry :: %{
          required(:content) => String.t(),
          required(:priority) => String.t(),
          required(:status) => String.t()
        }

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
end
