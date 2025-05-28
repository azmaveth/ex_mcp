defmodule ExMCP.Types do
  @moduledoc """
  Type definitions for the Model Context Protocol.

  This module defines the core types used throughout ExMCP,
  matching the MCP specification version 2025-03-26 and draft features.

  ## MCP Specification Types
  All core types are from the official MCP specification.

  ## Draft Specification Types
  Types marked with draft comments implement features from the draft MCP specification.
  """

  # Core types
  @type progress_token :: String.t() | integer()
  @type cursor :: String.t()
  @type request_id :: String.t() | integer()
  # "user" | "assistant"
  @type role :: String.t()
  # RFC-5424 levels
  @type log_level :: String.t()
  # ref/resource or ref/prompt
  @type complete_ref :: map()
  @type complete_argument :: map()
  @type complete_result :: map()

  @type implementation :: %{
          String.t() => any()
        }

  @type client_info :: implementation()
  @type server_info :: implementation()

  @type client_capabilities :: %{
          String.t() => any()
        }

  @type server_capabilities :: %{
          String.t() => any()
        }

  @type capabilities :: client_capabilities() | server_capabilities()

  # Annotations
  @type annotations :: %{
          String.t() => any()
        }

  # JSON Schema type (draft)
  @type json_schema :: %{
          required(:type) => String.t(),
          optional(:properties) => %{String.t() => map()},
          optional(:required) => [String.t()]
        }

  # Tool types
  @type tool_annotations :: %{
          String.t() => any()
        }

  @type tool :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          required(:inputSchema) => json_schema(),
          # Draft feature: outputSchema for structured tool output
          optional(:outputSchema) => json_schema(),
          optional(:annotations) => tool_annotations()
        }

  # Resource types
  @type resource :: %{
          String.t() => any()
        }

  @type resource_template :: %{
          String.t() => any()
        }

  @type text_resource_contents :: %{
          String.t() => any()
        }

  @type blob_resource_contents :: %{
          String.t() => any()
        }

  @type resource_contents :: text_resource_contents() | blob_resource_contents()
  # Alias for backwards compatibility
  @type resource_content :: map()

  # Root type
  @type root :: %{
          String.t() => any()
        }

  # Prompt types
  @type prompt :: %{
          String.t() => any()
        }

  @type prompt_argument :: %{
          String.t() => any()
        }

  @type prompt_message :: %{
          String.t() => any()
        }

  # Content types
  @type text_content :: %{
          String.t() => any()
        }

  @type image_content :: %{
          String.t() => any()
        }

  @type audio_content :: %{
          String.t() => any()
        }

  @type embedded_resource :: %{
          String.t() => any()
        }

  @type content :: text_content() | image_content() | audio_content() | embedded_resource()

  # Tool result
  @type tool_result :: %{
          required(:content) => [content()],
          optional(:isError) => boolean(),
          # Draft features: structured tool output
          optional(:structuredContent) => %{String.t() => any()}
        }

  # Completion types
  @type resource_reference :: %{
          String.t() => any()
        }

  @type prompt_reference :: %{
          String.t() => any()
        }

  @type completion_reference :: resource_reference() | prompt_reference()

  @type completion_argument :: %{
          String.t() => any()
        }

  @type completion_result :: %{
          String.t() => any()
        }

  # Logging types (RFC-5424)
  @type logging_level ::
          :debug | :info | :notice | :warning | :error | :critical | :alert | :emergency

  # Sampling types
  @type sampling_message :: %{
          String.t() => any()
        }

  @type model_hint :: %{
          String.t() => any()
        }

  @type model_preferences :: %{
          String.t() => any()
        }

  @type create_message_params :: %{
          String.t() => any()
        }

  @type create_message_result :: %{
          String.t() => any()
        }

  # Progress notification
  @type progress_notification :: %{
          String.t() => any()
        }

  # Initialize types
  @type initialize_request :: %{
          String.t() => any()
        }

  @type initialize_result :: %{
          String.t() => any()
        }

  # Error codes (JSON-RPC)
  @type error_code :: integer()

  # Transport types
  @type transport :: :stdio | :sse | :beam | module()

  # Pagination types
  @type paginated_request :: %{
          String.t() => any()
        }

  @type paginated_result :: %{
          String.t() => any()
        }

  # List results with pagination
  @type list_resources_result :: %{
          String.t() => any()
        }

  @type list_resource_templates_result :: %{
          String.t() => any()
        }

  @type list_tools_result :: %{
          String.t() => any()
        }

  @type list_prompts_result :: %{
          String.t() => any()
        }

  @type list_roots_result :: %{
          String.t() => any()
        }

  # Subscription results
  @type subscribe_result :: %{}
  @type unsubscribe_result :: %{}

  # Notification types
  @type cancelled_notification :: %{
          String.t() => any()
        }

  @type log_notification :: %{
          String.t() => any()
        }

  @type resource_updated_notification :: %{
          String.t() => any()
        }

  @type list_changed_notification :: %{}
end
