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
          optional(:logging) => map()
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
end
