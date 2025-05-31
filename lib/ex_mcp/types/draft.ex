defmodule ExMCP.Types.Draft do
  @moduledoc """
  Type definitions for MCP draft protocol version.

  This module contains type definitions for the draft version of the
  Model Context Protocol specification, including experimental features.

  ## Draft Features

  - Elicitation support for enhanced user interaction
  - Tool output schemas for structured responses
  - Component content type for UI elements
  - Batch request/response processing
  - Enhanced message annotations
  """

  # Re-export all types from stable version
  @type json_value :: any()
  @type json_schema :: map()
  @type request_id :: ExMCP.Types.request_id()
  @type error_code :: ExMCP.Types.error_code()
  @type cursor :: ExMCP.Types.cursor()
  @type log_level :: ExMCP.Types.log_level()

  # Version-specific protocol version
  @protocol_version "draft"
  def protocol_version, do: @protocol_version

  # Enhanced client capabilities with elicitation
  @type client_capabilities :: %{
          optional(:experimental) => %{String.t() => any()},
          optional(:sampling) => %{
            optional(:elicitation) => boolean()
          },
          optional(:roots) => %{
            optional(:listChanged) => boolean()
          }
        }

  # Enhanced server capabilities
  @type server_capabilities :: %{
          optional(:experimental) => %{
            optional(:batch) => boolean(),
            optional(:structuredContent) => boolean()
          },
          optional(:prompts) => %{
            optional(:listChanged) => boolean()
          },
          optional(:resources) => %{
            optional(:subscribe) => boolean(),
            optional(:listChanged) => boolean()
          },
          optional(:tools) => %{
            optional(:listChanged) => boolean()
          },
          optional(:logging) => %{
            optional(:setLevel) => boolean()
          }
        }

  # Tool with output schema support
  @type tool :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          required(:inputSchema) => map(),
          optional(:outputSchema) => map()
        }

  # Enhanced content types including component
  @type text_content :: %{
          required(:type) => :text,
          required(:text) => String.t(),
          optional(:annotations) => content_annotations()
        }

  @type image_content :: %{
          required(:type) => :image,
          required(:data) => String.t(),
          required(:mimeType) => String.t(),
          optional(:annotations) => content_annotations()
        }

  @type audio_content :: %{
          required(:type) => :audio,
          required(:data) => String.t(),
          required(:mimeType) => String.t(),
          optional(:annotations) => content_annotations()
        }

  @type resource_content :: %{
          required(:type) => :resource,
          required(:resource) => resource(),
          optional(:annotations) => content_annotations()
        }

  @type content :: text_content() | image_content() | audio_content() | resource_content()
  @type content_type :: :text | :image | :audio | :resource

  @type content_annotations :: %{
          optional(:audience) => [:user | :assistant],
          optional(:priority) => float()
        }

  # Resource definition
  @type resource :: %{
          required(:uri) => String.t(),
          optional(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:mimeType) => String.t(),
          optional(:annotations) => resource_annotations()
        }

  @type resource_annotations :: %{
          optional(:audience) => [:user | :assistant],
          optional(:priority) => float()
        }

  # Message with annotations
  @type message :: %{
          required(:role) => :user | :assistant,
          required(:content) => content() | [content()],
          optional(:annotations) => message_annotations()
        }

  @type message_annotations :: %{
          optional(:audience) => [:user | :assistant],
          optional(:priority) => float()
        }

  # Enhanced sampling with elicitation
  @type create_message_params :: %{
          required(:messages) => [message()],
          optional(:modelPreferences) => model_preferences(),
          optional(:systemPrompt) => String.t(),
          optional(:maxTokens) => pos_integer(),
          optional(:elicitation) => elicitation_options()
        }

  @type elicitation_options :: %{
          optional(:enabled) => boolean(),
          optional(:tools) => [String.t()],
          optional(:prompts) => [String.t()],
          optional(:resources) => [String.t()]
        }

  @type model_preferences :: %{
          optional(:hints) => [model_hint()],
          optional(:costPriority) => float(),
          optional(:speedPriority) => float(),
          optional(:intelligencePriority) => float()
        }

  @type model_hint :: %{
          optional(:name) => String.t()
        }

  # Batch request support
  @type batch_request :: %{
          required(:requests) => [json_rpc_request()]
        }

  @type batch_response :: %{
          required(:responses) => [json_rpc_response() | json_rpc_error()]
        }

  @type json_rpc_request :: %{
          required(:jsonrpc) => String.t(),
          required(:method) => String.t(),
          optional(:params) => map(),
          optional(:id) => request_id()
        }

  @type json_rpc_response :: %{
          required(:jsonrpc) => String.t(),
          required(:result) => any(),
          required(:id) => request_id()
        }

  @type json_rpc_error :: %{
          required(:jsonrpc) => String.t(),
          required(:error) => %{
            required(:code) => error_code(),
            required(:message) => String.t(),
            optional(:data) => any()
          },
          required(:id) => request_id() | nil
        }
end

