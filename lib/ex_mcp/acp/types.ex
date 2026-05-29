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
          required(:name) => String.t(),
          optional(:mimeType) => String.t(),
          optional(:title) => String.t(),
          optional(:description) => String.t(),
          optional(:size) => non_neg_integer()
        }

  @type resource_block :: %{
          required(:type) => :resource,
          required(:resource) => embedded_resource()
        }

  @type embedded_resource ::
          %{
            required(:uri) => String.t(),
            required(:text) => String.t(),
            optional(:mimeType) => String.t()
          }
          | %{
              required(:uri) => String.t(),
              required(:blob) => String.t(),
              optional(:mimeType) => String.t()
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
          optional(:auth) => %{
            optional(:logout) => map() | nil
          },
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
            optional(:list) => session_list_capabilities() | nil,
            optional(:resume) => session_resume_capabilities() | nil,
            optional(:close) => session_close_capabilities() | nil
          }
        }

  @type session_list_capabilities :: map()
  @type session_resume_capabilities :: map()
  @type session_close_capabilities :: map()

  @type mode :: %{
          required(:id) => String.t(),
          required(:name) => String.t(),
          optional(:description) => String.t()
        }

  @type config_option :: %{
          required(:id) => String.t(),
          required(:name) => String.t(),
          required(:type) => String.t(),
          required(:currentValue) => String.t(),
          required(:options) => list(),
          optional(:description) => String.t(),
          optional(:category) => String.t()
        }

  # ACP Error Codes
  # Standard JSON-RPC: -32700 (parse), -32600 (invalid request), -32601 (method not found),
  #                    -32602 (invalid params), -32603 (internal error)
  # ACP-specific:
  @auth_required_code -32_000
  @resource_not_found_code -32_002

  @doc "Error code indicating authentication is required."
  @spec auth_required_code() :: integer()
  def auth_required_code, do: @auth_required_code

  @doc "Error code indicating a resource was not found."
  @spec resource_not_found_code() :: integer()
  def resource_not_found_code, do: @resource_not_found_code

  # Initialize

  @type client_info :: %{
          required(:name) => String.t(),
          required(:version) => String.t(),
          optional(:title) => String.t()
        }

  @type agent_info :: %{
          required(:name) => String.t(),
          required(:version) => String.t(),
          optional(:title) => String.t()
        }

  @type initialize_request :: %{
          required(:clientInfo) => client_info(),
          optional(:clientCapabilities) => client_capabilities(),
          optional(:protocolVersion) => pos_integer()
        }

  @type initialize_response :: %{
          required(:agentInfo) => agent_info(),
          optional(:agentCapabilities) => agent_capabilities(),
          optional(:authMethods) => [auth_method()],
          optional(:protocolVersion) => pos_integer()
        }

  @type auth_method :: %{
          required(:id) => String.t(),
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:type) => String.t()
        }

  # Sessions

  @type mcp_server :: stdio_mcp_server() | http_mcp_server() | sse_mcp_server()

  @type stdio_mcp_server :: %{
          required(:type) => :stdio,
          required(:name) => String.t(),
          required(:command) => String.t(),
          required(:args) => [String.t()],
          required(:env) => [env_variable()]
        }

  @type http_mcp_server :: %{
          required(:type) => :http,
          required(:name) => String.t(),
          required(:url) => String.t(),
          required(:headers) => [http_header()]
        }

  @type sse_mcp_server :: %{
          required(:type) => :sse,
          required(:name) => String.t(),
          required(:url) => String.t(),
          required(:headers) => [http_header()]
        }

  @type env_variable :: %{
          required(:name) => String.t(),
          required(:value) => String.t()
        }

  @type http_header :: %{
          required(:name) => String.t(),
          required(:value) => String.t()
        }

  @type new_session_request :: %{
          required(:cwd) => String.t(),
          required(:mcpServers) => [mcp_server()]
        }

  @type new_session_response :: %{
          required(:sessionId) => String.t()
        }

  @type list_sessions_request :: %{
          optional(:cursor) => String.t(),
          optional(:cwd) => String.t()
        }

  @type list_sessions_response :: %{
          required(:sessions) => [session_info()],
          optional(:nextCursor) => String.t()
        }

  @type session_info :: %{
          required(:sessionId) => String.t(),
          required(:cwd) => String.t(),
          optional(:title) => String.t(),
          optional(:updatedAt) => String.t()
        }

  @type load_session_request :: %{
          required(:sessionId) => String.t(),
          required(:cwd) => String.t(),
          required(:mcpServers) => [mcp_server()]
        }

  @type resume_session_request :: %{
          required(:sessionId) => String.t(),
          required(:cwd) => String.t(),
          optional(:mcpServers) => [mcp_server()]
        }

  @type close_session_request :: %{
          required(:sessionId) => String.t()
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
  #   user_message_chunk, agent_message_chunk, tool_call, tool_call_update, plan,
  #   available_commands_update, config_option_update, current_mode_update,
  #   session_info_update, usage_update, agent_thought_chunk
  #
  # Extension types (not in spec, used by adapters):
  #   status, usage, error, rpc_response, rpc_error, extension_ui_request

  @type session_update_params :: %{
          required(:sessionId) => String.t(),
          required(:update) => session_update()
        }

  @type session_update ::
          user_message_chunk_update()
          | agent_message_chunk_update()
          | agent_thought_chunk_update()
          | tool_call()
          | tool_call_update()
          | plan()
          | available_commands_update()
          | config_option_update()
          | current_mode_update()
          | session_info_update()
          | usage_update()
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

  @type agent_thought_chunk_update :: %{
          required(:sessionUpdate) => :agent_thought_chunk,
          required(:content) => content_block()
        }

  @type tool_call :: %{
          required(:sessionUpdate) => :tool_call,
          required(:toolCallId) => String.t(),
          required(:title) => String.t(),
          optional(:status) => String.t(),
          optional(:content) => [map()]
        }

  @type tool_call_update :: %{
          required(:sessionUpdate) => :tool_call_update,
          required(:toolCallId) => String.t(),
          optional(:title) => String.t(),
          optional(:status) => String.t(),
          optional(:content) => [map()]
        }

  @type plan :: %{
          required(:sessionUpdate) => :plan,
          required(:entries) => [plan_entry()]
        }

  @type plan_entry :: %{
          required(:content) => String.t(),
          required(:priority) => :high | :medium | :low,
          required(:status) => :pending | :in_progress | :completed
        }

  @type available_commands_update :: %{
          required(:sessionUpdate) => :available_commands_update,
          required(:availableCommands) => [map()]
        }

  @type config_option_update :: %{
          required(:sessionUpdate) => :config_option_update,
          required(:configOptions) => [config_option()]
        }

  @type current_mode_update :: %{
          required(:sessionUpdate) => :current_mode_update,
          required(:currentModeId) => String.t()
        }

  @type session_info_update :: %{
          required(:sessionUpdate) => :session_info_update,
          optional(:title) => String.t(),
          optional(:updatedAt) => String.t()
        }

  @type usage_update :: %{
          required(:sessionUpdate) => :usage_update,
          required(:used) => non_neg_integer(),
          required(:size) => non_neg_integer(),
          optional(:cost) => map()
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
          optional(:line) => non_neg_integer(),
          optional(:limit) => non_neg_integer()
        }

  @type file_write_request :: %{
          required(:sessionId) => String.t(),
          required(:path) => String.t(),
          required(:content) => String.t()
        }

  # Builder functions

  @doc "Creates a text content block."
  @spec text_block(String.t(), keyword()) :: map()
  def text_block(text, opts \\ []) when is_binary(text) do
    %{"type" => "text", "text" => text}
    |> maybe_put_kw("annotations", opts)
    |> maybe_put_kw("_meta", opts)
  end

  @doc "Creates an image content block."
  @spec image_block(String.t(), String.t(), keyword()) :: map()
  def image_block(mime_type, data, opts \\ []) when is_binary(mime_type) and is_binary(data) do
    %{"type" => "image", "mimeType" => mime_type, "data" => data}
    |> maybe_put_kw("uri", opts)
    |> maybe_put_kw("annotations", opts)
    |> maybe_put_kw("_meta", opts)
  end

  @doc "Creates an audio content block."
  @spec audio_block(String.t(), String.t(), keyword()) :: map()
  def audio_block(mime_type, data, opts \\ []) when is_binary(mime_type) and is_binary(data) do
    %{"type" => "audio", "mimeType" => mime_type, "data" => data}
    |> maybe_put_kw("annotations", opts)
    |> maybe_put_kw("_meta", opts)
  end

  @doc "Creates a resource link content block."
  @spec resource_link_block(String.t(), keyword()) :: map()
  def resource_link_block(uri, opts \\ []) when is_binary(uri) do
    name = Keyword.get(opts, :name, Path.basename(uri))

    %{"type" => "resource_link", "uri" => uri, "name" => name}
    |> maybe_put_kw("mimeType", opts)
    |> maybe_put_kw("title", opts)
    |> maybe_put_kw("description", opts)
    |> maybe_put_kw("size", opts)
    |> maybe_put_kw("annotations", opts)
    |> maybe_put_kw("_meta", opts)
  end

  @doc "Creates a resource content block."
  @spec resource_block(String.t(), keyword()) :: map()
  def resource_block(uri, opts \\ []) when is_binary(uri) do
    resource =
      %{"uri" => uri}
      |> maybe_put_kw("mimeType", opts)
      |> maybe_put_kw("_meta", opts)

    resource =
      case Keyword.fetch(opts, :blob) do
        {:ok, blob} -> Map.put(resource, "blob", blob)
        :error -> Map.put(resource, "text", Keyword.get(opts, :text, ""))
      end

    %{"type" => "resource", "resource" => resource}
    |> maybe_put_kw("annotations", opts)
    |> maybe_put_kw("_meta", opts)
  end

  @doc "Creates client info for the initialize handshake."
  @spec client_info(String.t(), String.t(), keyword()) :: map()
  def client_info(name, version, opts \\ []) when is_binary(name) and is_binary(version) do
    %{"name" => name, "version" => version}
    |> maybe_put_kw("title", opts)
    |> maybe_put_kw("_meta", opts)
  end

  @doc "Creates an authentication method advertised by an agent."
  @spec auth_method(String.t(), String.t(), keyword()) :: map()
  def auth_method(id, name, opts \\ []) when is_binary(id) and is_binary(name) do
    %{"id" => id, "name" => name}
    |> maybe_put_kw("description", opts)
    |> maybe_put_kw("type", opts)
  end

  @doc """
  Creates ACP agent capabilities.

  Supported options: `:load_session`, `:http_mcp`, `:sse_mcp`, `:image`,
  `:audio`, `:embedded_context`, `:session_list`, `:session_resume`,
  `:session_close`, and `:logout`.
  """
  @spec agent_capabilities(keyword()) :: map()
  def agent_capabilities(opts \\ []) do
    %{}
    |> maybe_put_bool("loadSession", opts, :load_session)
    |> put_if_not_empty("promptCapabilities", prompt_capabilities(opts))
    |> put_if_not_empty("mcpCapabilities", mcp_capabilities(opts))
    |> put_if_not_empty("sessionCapabilities", session_capabilities(opts))
    |> put_if_not_empty("auth", auth_capabilities(opts))
  end

  @doc "Creates session capability metadata."
  @spec session_capabilities(keyword()) :: map()
  def session_capabilities(opts \\ []) do
    %{}
    |> maybe_put_capability("list", Keyword.get(opts, :list, Keyword.get(opts, :session_list)))
    |> maybe_put_capability(
      "resume",
      Keyword.get(opts, :resume, Keyword.get(opts, :session_resume))
    )
    |> maybe_put_capability("close", Keyword.get(opts, :close, Keyword.get(opts, :session_close)))
  end

  @doc "Creates a plan entry."
  @spec plan_entry(String.t(), String.t(), String.t()) :: map()
  def plan_entry(content, priority \\ "medium", status \\ "pending") do
    %{"content" => content, "priority" => priority, "status" => status}
  end

  @doc "Creates a stable ACP `plan` session update notification."
  @spec plan(String.t(), [map()]) :: map()
  def plan(session_id, entries) when is_list(entries) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id,
        "update" => %{
          "sessionUpdate" => "plan",
          "entries" => entries
        }
      }
    }
  end

  @doc "Creates a stable ACP `plan` session update notification."
  @spec plan_update(String.t(), [map()]) :: map()
  def plan_update(session_id, entries) when is_list(entries) do
    plan(session_id, entries)
  end

  @doc "Creates an available_commands_update session update notification."
  @spec available_commands_update(String.t(), [map()]) :: map()
  def available_commands_update(session_id, commands) when is_list(commands) do
    session_update(session_id, %{
      "sessionUpdate" => "available_commands_update",
      "availableCommands" => commands
    })
  end

  @doc "Creates a config_option_update session update notification."
  @spec config_option_update(String.t(), [map()]) :: map()
  def config_option_update(session_id, config_options) when is_list(config_options) do
    session_update(session_id, %{
      "sessionUpdate" => "config_option_update",
      "configOptions" => config_options
    })
  end

  @doc "Creates a current_mode_update session update notification."
  @spec current_mode_update(String.t(), String.t()) :: map()
  def current_mode_update(session_id, current_mode_id) do
    session_update(session_id, %{
      "sessionUpdate" => "current_mode_update",
      "currentModeId" => current_mode_id
    })
  end

  @doc "Creates a session_info_update session update notification."
  @spec session_info_update(String.t(), keyword()) :: map()
  def session_info_update(session_id, opts \\ []) do
    update =
      %{"sessionUpdate" => "session_info_update"}
      |> maybe_put_kw("title", opts)
      |> maybe_put_kw("updatedAt", opts)

    session_update(session_id, update)
  end

  @doc "Creates a usage_update session update notification."
  @spec usage_update(String.t(), non_neg_integer(), non_neg_integer(), keyword()) :: map()
  def usage_update(session_id, used, size, opts \\ []) do
    update =
      %{"sessionUpdate" => "usage_update", "used" => used, "size" => size}
      |> maybe_put_kw("cost", opts)

    session_update(session_id, update)
  end

  @doc "Creates a config option value for select-style session config."
  @spec config_option_value(String.t(), String.t(), keyword()) :: map()
  def config_option_value(value, name, opts \\ []) do
    %{"value" => value, "name" => name}
    |> maybe_put_kw("description", opts)
  end

  @doc "Creates a select-style session config option."
  @spec select_config_option(String.t(), String.t(), String.t(), [map()], keyword()) :: map()
  def select_config_option(id, name, current_value, options, opts \\ []) do
    %{
      "id" => id,
      "name" => name,
      "type" => "select",
      "currentValue" => current_value,
      "options" => options
    }
    |> maybe_put_kw("description", opts)
    |> maybe_put_kw("category", opts)
  end

  @doc "Creates a session info entry returned by session/list."
  @spec session_info(String.t(), String.t(), keyword()) :: map()
  def session_info(session_id, cwd, opts \\ []) do
    %{"sessionId" => session_id, "cwd" => cwd}
    |> maybe_put_kw("title", opts)
    |> maybe_put_kw("updatedAt", opts)
  end

  @doc "Creates an environment variable entry for a stdio MCP server."
  @spec env_variable(String.t(), String.t()) :: map()
  def env_variable(name, value), do: %{"name" => name, "value" => value}

  @doc "Creates an HTTP header entry for an HTTP/SSE MCP server."
  @spec http_header(String.t(), String.t()) :: map()
  def http_header(name, value), do: %{"name" => name, "value" => value}

  @doc "Creates a stdio MCP server config for ACP session setup."
  @spec stdio_mcp_server(String.t(), String.t(), keyword()) :: map()
  def stdio_mcp_server(name, command, opts \\ []) do
    %{
      "type" => "stdio",
      "name" => name,
      "command" => command,
      "args" => Keyword.get(opts, :args, []),
      "env" => normalize_env(Keyword.get(opts, :env, []))
    }
  end

  @doc "Creates an HTTP MCP server config for ACP session setup."
  @spec http_mcp_server(String.t(), String.t(), keyword()) :: map()
  def http_mcp_server(name, url, opts \\ []) do
    %{
      "type" => "http",
      "name" => name,
      "url" => url,
      "headers" => normalize_headers(Keyword.get(opts, :headers, []))
    }
  end

  @doc "Creates an SSE MCP server config for ACP session setup."
  @spec sse_mcp_server(String.t(), String.t(), keyword()) :: map()
  def sse_mcp_server(name, url, opts \\ []) do
    %{
      "type" => "sse",
      "name" => name,
      "url" => url,
      "headers" => normalize_headers(Keyword.get(opts, :headers, []))
    }
  end

  @doc """
  Creates params for a new session request.

  ## Options

  - `:mcp_servers` - list of MCP server maps, preferably from
    `stdio_mcp_server/3`, `http_mcp_server/3`, or `sse_mcp_server/3`
  """
  @spec new_session_params(String.t() | nil, keyword()) :: map()
  def new_session_params(cwd \\ nil, opts \\ []) do
    params = %{}
    params = if cwd, do: Map.put(params, "cwd", cwd), else: params

    case Keyword.get(opts, :mcp_servers) do
      nil -> Map.put(params, "mcpServers", [])
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

  defp prompt_capabilities(opts) do
    %{}
    |> maybe_put_bool("image", opts, :image)
    |> maybe_put_bool("audio", opts, :audio)
    |> maybe_put_bool("embeddedContext", opts, :embedded_context)
  end

  defp mcp_capabilities(opts) do
    %{}
    |> maybe_put_bool("http", opts, :http_mcp)
    |> maybe_put_bool("sse", opts, :sse_mcp)
  end

  defp auth_capabilities(opts) do
    %{}
    |> maybe_put_capability("logout", Keyword.get(opts, :logout))
  end

  defp session_update(session_id, update) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id,
        "update" => update
      }
    }
  end

  defp maybe_put_bool(map, key, opts, opt_key) do
    case Keyword.get(opts, opt_key) do
      nil -> map
      value -> Map.put(map, key, value)
    end
  end

  defp maybe_put_capability(map, _key, nil), do: map
  defp maybe_put_capability(map, _key, false), do: map
  defp maybe_put_capability(map, key, true), do: Map.put(map, key, %{})
  defp maybe_put_capability(map, key, value) when is_map(value), do: Map.put(map, key, value)

  defp put_if_not_empty(map, _key, value) when value == %{}, do: map
  defp put_if_not_empty(map, key, value), do: Map.put(map, key, value)

  defp normalize_env(env) when is_map(env) do
    Enum.map(env, fn {name, value} -> env_variable(to_string(name), to_string(value)) end)
  end

  defp normalize_env(env) when is_list(env) do
    Enum.map(env, fn
      %{"name" => _, "value" => _} = item -> item
      %{name: name, value: value} -> env_variable(to_string(name), to_string(value))
      {name, value} -> env_variable(to_string(name), to_string(value))
    end)
  end

  defp normalize_headers(headers) when is_map(headers) do
    Enum.map(headers, fn {name, value} -> http_header(to_string(name), to_string(value)) end)
  end

  defp normalize_headers(headers) when is_list(headers) do
    Enum.map(headers, fn
      %{"name" => _, "value" => _} = item -> item
      %{name: name, value: value} -> http_header(to_string(name), to_string(value))
      {name, value} -> http_header(to_string(name), to_string(value))
    end)
  end

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
