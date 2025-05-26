defmodule ExMCP.Types do
  @moduledoc """
  Type definitions for the Model Context Protocol.

  This module defines the core types used throughout ExMCP,
  matching the MCP specification.
  """

  @type client_info :: %{
          name: String.t(),
          version: String.t()
        }

  @type server_info :: %{
          name: String.t(),
          version: String.t()
        }

  @type capabilities :: %{
          optional(:tools) => map(),
          optional(:resources) => map(),
          optional(:prompts) => map(),
          optional(:logging) => map(),
          optional(:sampling) => map()
        }

  @type tool :: %{
          required(:name) => String.t(),
          required(:description) => String.t(),
          optional(:input_schema) => json_schema()
        }

  @type resource :: %{
          required(:uri) => String.t(),
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:mime_type) => String.t()
        }

  @type resource_content :: %{
          required(:uri) => String.t(),
          optional(:mime_type) => String.t(),
          optional(:text) => String.t(),
          # base64 encoded
          optional(:blob) => String.t()
        }

  @type prompt :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:arguments) => [prompt_argument()]
        }

  @type prompt_argument :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:required) => boolean()
        }

  @type prompt_message :: %{
          role: String.t(),
          content: prompt_content()
        }

  @type prompt_content :: String.t() | [content_part()]

  @type text_content :: %{type: String.t(), text: String.t()}
  @type image_content :: %{type: String.t(), data: String.t(), mime_type: String.t()}
  @type resource_ref :: %{type: String.t(), resource: resource_content()}

  @type content_part :: text_content() | image_content() | resource_ref()

  @type tool_result :: [content_part()] | {:error, String.t()}

  @type json_schema :: map()

  @type completion_argument :: %{
          name: String.t(),
          value: String.t()
        }

  @type log_level :: :debug | :info | :warning | :error

  @type transport :: :stdio | :sse | :websocket | module()

  @type initialize_result :: %{
          protocolVersion: String.t(),
          serverInfo: server_info(),
          capabilities: capabilities()
        }

  @type error_code :: -32700..-32600 | -32099..-32000

  @type sampling_message :: %{
          required(:role) => String.t(),
          required(:content) => prompt_content()
        }

  @type model_preferences :: %{
          optional(:hints) => [String.t()],
          optional(:costPriority) => float(),
          optional(:speedPriority) => float(),
          optional(:intelligencePriority) => float()
        }

  @type sampling_params :: %{
          optional(:temperature) => float(),
          optional(:topP) => float(),
          optional(:topK) => integer(),
          optional(:maxTokens) => integer(),
          optional(:stopSequences) => [String.t()],
          optional(:seed) => integer()
        }

  @type create_message_params :: %{
          required(:messages) => [sampling_message()],
          optional(:modelPreferences) => model_preferences(),
          optional(:systemPrompt) => String.t(),
          optional(:includeContext) => String.t(),
          optional(:metadata) => map()
        }

  @type create_message_result :: %{
          required(:role) => String.t(),
          required(:content) => prompt_content(),
          optional(:model) => String.t(),
          optional(:stopReason) => String.t()
        }

  @type progress_token :: String.t() | integer()

  @type progress_notification :: %{
          required(:progressToken) => progress_token(),
          required(:progress) => number(),
          optional(:total) => number()
        }
end
