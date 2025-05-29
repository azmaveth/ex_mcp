defmodule ExMCP.Types do
  @moduledoc """
  Type definitions for the Model Context Protocol.

  This module defines the core types used throughout ExMCP,
  matching the MCP specification version 2025-03-26.

  ## MCP Specification Types
  All core types are from the official MCP specification 2025-03-26.

  ## Draft Specification Types
  Types marked with "Draft feature" comments implement features from the
  draft MCP specification and should not be used in production.

  ### Draft features included:
  - Elicitation (elicit_request, elicit_result, elicitation field in ClientCapabilities)
  - Tool output schemas (outputSchema field in Tool)
  - Structured tool output (structuredContent field in CallToolResult)
  - Logging level control (logging/setLevel method)
  - Batch processing (JSONRPCBatchRequest, JSONRPCBatchResponse)
  """

  # Protocol version constants
  @latest_protocol_version "2025-03-26"
  @jsonrpc_version "2.0"

  # Error code constants
  @parse_error -32700
  @invalid_request -32600
  @method_not_found -32601
  @invalid_params -32602
  @internal_error -32603

  # Core JSON-RPC types
  @type request_id :: String.t() | integer()
  @type progress_token :: String.t() | integer()
  @type cursor :: String.t()

  # Role types
  @type role :: :user | :assistant
  @type role_string :: String.t()

  # Log level types (RFC-5424)
  @type log_level ::
          :debug | :info | :notice | :warning | :error | :critical | :alert | :emergency

  # String version for protocol messages
  @type log_level_string :: String.t()

  # Implementation info
  @type implementation :: %{
          required(:name) => String.t(),
          required(:version) => String.t()
        }

  @type client_info :: implementation()
  @type server_info :: implementation()

  # Capabilities
  @type client_capabilities :: %{
          optional(:experimental) => %{String.t() => map()},
          optional(:roots) => %{
            optional(:listChanged) => boolean()
          },
          optional(:sampling) => %{},
          # Draft feature: elicitation capability
          optional(:elicitation) => %{}
        }

  @type server_capabilities :: %{
          optional(:experimental) => %{String.t() => map()},
          optional(:logging) => %{},
          optional(:completions) => %{},
          optional(:prompts) => %{
            optional(:listChanged) => boolean()
          },
          optional(:resources) => %{
            optional(:subscribe) => boolean(),
            optional(:listChanged) => boolean()
          },
          optional(:tools) => %{
            optional(:listChanged) => boolean()
          }
        }

  # Annotations
  @type annotations :: %{
          optional(:audience) => [role()],
          optional(:priority) => float()
        }

  # JSON Schema type
  @type json_schema :: %{
          required(:type) => String.t(),
          optional(:properties) => %{String.t() => map()},
          optional(:required) => [String.t()],
          optional(:additionalProperties) => boolean(),
          optional(:description) => String.t()
        }

  # Tool types
  @type tool_annotations :: %{
          optional(:title) => String.t(),
          optional(:readOnlyHint) => boolean(),
          optional(:destructiveHint) => boolean(),
          optional(:idempotentHint) => boolean(),
          optional(:openWorldHint) => boolean()
        }

  @type tool :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          required(:inputSchema) => json_schema(),
          # Draft feature: outputSchema
          optional(:outputSchema) => json_schema(),
          optional(:annotations) => tool_annotations()
        }

  # Content types
  @type text_content :: %{
          required(:type) => :text,
          required(:text) => String.t(),
          optional(:annotations) => annotations()
        }

  @type image_content :: %{
          required(:type) => :image,
          required(:data) => String.t(),
          required(:mimeType) => String.t(),
          optional(:annotations) => annotations()
        }

  @type audio_content :: %{
          required(:type) => :audio,
          required(:data) => String.t(),
          required(:mimeType) => String.t(),
          optional(:annotations) => annotations()
        }

  @type embedded_resource :: %{
          required(:type) => :resource,
          required(:resource) => resource_contents(),
          optional(:annotations) => annotations()
        }

  @type content :: text_content() | image_content() | audio_content() | embedded_resource()

  # Content type atoms
  @type content_type :: :text | :image | :audio | :resource

  # Tool result
  @type tool_result :: %{
          required(:content) => [content()],
          optional(:isError) => boolean(),
          # Draft feature: structuredContent
          optional(:structuredContent) => %{String.t() => any()}
        }

  # Resource types
  @type resource :: %{
          required(:uri) => String.t(),
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:mimeType) => String.t(),
          optional(:annotations) => annotations(),
          optional(:size) => integer()
        }

  @type resource_template :: %{
          required(:uriTemplate) => String.t(),
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:mimeType) => String.t(),
          optional(:annotations) => annotations()
        }

  @type text_resource_contents :: %{
          required(:uri) => String.t(),
          required(:text) => String.t(),
          optional(:mimeType) => String.t()
        }

  @type blob_resource_contents :: %{
          required(:uri) => String.t(),
          required(:blob) => String.t(),
          optional(:mimeType) => String.t()
        }

  @type resource_contents :: text_resource_contents() | blob_resource_contents()

  # Root type
  @type root :: %{
          required(:uri) => String.t(),
          optional(:name) => String.t()
        }

  # Prompt types
  @type prompt_argument :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:required) => boolean()
        }

  @type prompt :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:arguments) => [prompt_argument()]
        }

  @type prompt_message :: %{
          required(:role) => role(),
          required(:content) => content()
        }

  # Completion types
  @type resource_reference :: %{
          required(:type) => :"ref/resource",
          required(:uri) => String.t()
        }

  @type prompt_reference :: %{
          required(:type) => :"ref/prompt",
          required(:name) => String.t()
        }

  @type completion_reference :: resource_reference() | prompt_reference()
  @type complete_ref :: completion_reference()

  @type completion_argument :: %{
          required(:name) => String.t(),
          required(:value) => String.t()
        }
  @type complete_argument :: completion_argument()

  @type completion_result :: %{
          required(:completion) => %{
            required(:values) => [String.t()],
            optional(:total) => integer(),
            optional(:hasMore) => boolean()
          }
        }
  @type complete_result :: completion_result()

  # Sampling types
  @type sampling_message :: %{
          required(:role) => role(),
          required(:content) => text_content() | image_content() | audio_content()
        }

  @type model_hint :: %{
          optional(:name) => String.t()
        }

  @type model_preferences :: %{
          optional(:hints) => [model_hint()],
          optional(:costPriority) => float(),
          optional(:speedPriority) => float(),
          optional(:intelligencePriority) => float()
        }

  @type include_context :: :none | :thisServer | :allServers

  @type create_message_params :: %{
          required(:messages) => [sampling_message()],
          required(:maxTokens) => integer(),
          optional(:modelPreferences) => model_preferences(),
          optional(:systemPrompt) => String.t(),
          optional(:includeContext) => include_context(),
          optional(:temperature) => float(),
          optional(:stopSequences) => [String.t()],
          optional(:metadata) => map()
        }

  @type create_message_result :: %{
          required(:role) => role(),
          required(:content) => text_content() | image_content() | audio_content(),
          required(:model) => String.t(),
          optional(:stopReason) => String.t()
        }

  # Progress notification
  @type progress_notification :: %{
          required(:progressToken) => progress_token(),
          required(:progress) => number(),
          optional(:total) => number(),
          optional(:message) => String.t()
        }

  # Initialize types
  @type initialize_request :: %{
          required(:protocolVersion) => String.t(),
          required(:capabilities) => client_capabilities(),
          required(:clientInfo) => client_info()
        }

  @type initialize_result :: %{
          required(:protocolVersion) => String.t(),
          required(:capabilities) => server_capabilities(),
          required(:serverInfo) => server_info(),
          optional(:instructions) => String.t()
        }

  # Paginated results
  @type paginated_result :: %{
          optional(:nextCursor) => cursor()
        }

  @type list_resources_result :: %{
          required(:resources) => [resource()],
          optional(:nextCursor) => cursor()
        }

  @type list_resource_templates_result :: %{
          required(:resourceTemplates) => [resource_template()],
          optional(:nextCursor) => cursor()
        }

  @type list_tools_result :: %{
          required(:tools) => [tool()],
          optional(:nextCursor) => cursor()
        }

  @type list_prompts_result :: %{
          required(:prompts) => [prompt()],
          optional(:nextCursor) => cursor()
        }

  @type list_roots_result :: %{
          required(:roots) => [root()]
        }

  # Read resource result
  @type read_resource_result :: %{
          required(:contents) => [resource_contents()]
        }

  # Get prompt result
  @type get_prompt_result :: %{
          optional(:description) => String.t(),
          required(:messages) => [prompt_message()]
        }

  # Call tool result
  @type call_tool_result :: tool_result()

  # Subscription results
  @type subscribe_result :: %{}
  @type unsubscribe_result :: %{}

  # Empty result
  @type empty_result :: %{}

  # Notification types
  @type cancelled_notification :: %{
          required(:requestId) => request_id(),
          optional(:reason) => String.t()
        }

  @type log_notification :: %{
          required(:level) => log_level_string(),
          optional(:logger) => String.t(),
          required(:data) => any()
        }

  @type resource_updated_notification :: %{
          required(:uri) => String.t()
        }

  @type list_changed_notification :: %{}

  # Transport types
  @type transport :: :stdio | :http | :beam | :sse | :websocket | module()

  # Request parameter types
  @type paginated_request :: %{
          optional(:cursor) => cursor()
        }

  @type ping_request :: %{}

  @type list_resources_request :: paginated_request()
  @type list_resource_templates_request :: paginated_request()
  @type list_tools_request :: paginated_request()
  @type list_prompts_request :: paginated_request()
  @type list_roots_request :: %{}

  @type read_resource_request :: %{
          required(:uri) => String.t()
        }

  @type get_prompt_request :: %{
          required(:name) => String.t(),
          optional(:arguments) => %{String.t() => String.t()}
        }

  @type call_tool_request :: %{
          required(:name) => String.t(),
          optional(:arguments) => %{String.t() => any()}
        }

  @type subscribe_request :: %{
          required(:uri) => String.t()
        }

  @type unsubscribe_request :: %{
          required(:uri) => String.t()
        }

  # Draft feature: logging/setLevel
  @type set_level_request :: %{
          required(:level) => log_level_string()
        }

  @type complete_request :: %{
          required(:ref) => completion_reference(),
          required(:argument) => completion_argument()
        }

  # Union types for client/server messages
  @type client_request ::
          ping_request()
          | initialize_request()
          | list_resources_request()
          | read_resource_request()
          | list_prompts_request()
          | get_prompt_request()
          | list_tools_request()
          | call_tool_request()
          | complete_request()
          | set_level_request()
          | subscribe_request()
          | unsubscribe_request()

  @type server_request ::
          ping_request()
          | create_message_params()
          | list_roots_request()
          | elicit_request()

  # Draft feature: Elicitation types
  @type primitive_schema ::
          %{
            required(:type) => String.t(),
            optional(:title) => String.t(),
            optional(:description) => String.t(),
            optional(:enum) => [String.t()],
            optional(:enumNames) => [String.t()],
            optional(:default) => any(),
            optional(:minLength) => integer(),
            optional(:maxLength) => integer(),
            optional(:minimum) => number(),
            optional(:maximum) => number(),
            optional(:format) => String.t()
          }

  @type elicit_request :: %{
          required(:message) => String.t(),
          required(:requestedSchema) => %{
            required(:type) => String.t(),
            required(:properties) => %{String.t() => primitive_schema()},
            optional(:required) => [String.t()]
          }
        }

  @type elicit_result :: %{
          required(:action) => :accept | :decline | :cancel,
          optional(:content) => %{String.t() => any()}
        }

  # Error types
  @type error_code :: integer()
  @type error_data :: any()

  @type jsonrpc_error :: %{
          required(:code) => error_code(),
          required(:message) => String.t(),
          optional(:data) => error_data()
        }

  # JSON-RPC message types
  @type jsonrpc_request :: %{
          required(:jsonrpc) => String.t(),
          required(:id) => request_id(),
          required(:method) => String.t(),
          optional(:params) => map()
        }

  @type jsonrpc_notification :: %{
          required(:jsonrpc) => String.t(),
          required(:method) => String.t(),
          optional(:params) => map()
        }

  @type jsonrpc_response :: %{
          required(:jsonrpc) => String.t(),
          required(:id) => request_id(),
          required(:result) => any()
        }

  @type jsonrpc_error_response :: %{
          required(:jsonrpc) => String.t(),
          required(:id) => request_id(),
          required(:error) => jsonrpc_error()
        }

  # Draft feature: Batch processing types
  @type jsonrpc_batch_request :: [jsonrpc_request() | jsonrpc_notification()]
  @type jsonrpc_batch_response :: [jsonrpc_response() | jsonrpc_error_response()]

  @type jsonrpc_message ::
          jsonrpc_request()
          | jsonrpc_notification()
          | jsonrpc_response()
          | jsonrpc_error_response()
          | jsonrpc_batch_request()
          | jsonrpc_batch_response()

  # Accessor functions for constants
  def latest_protocol_version, do: @latest_protocol_version
  def jsonrpc_version, do: @jsonrpc_version

  def parse_error, do: @parse_error
  def invalid_request, do: @invalid_request
  def method_not_found, do: @method_not_found
  def invalid_params, do: @invalid_params
  def internal_error, do: @internal_error

  # Union types for results
  @type client_result ::
          empty_result() | create_message_result() | list_roots_result() | elicit_result()

  @type server_result ::
          empty_result()
          | initialize_result()
          | complete_result()
          | get_prompt_result()
          | list_prompts_result()
          | list_resource_templates_result()
          | list_resources_result()
          | read_resource_result()
          | call_tool_result()
          | list_tools_result()

  # Union types for notifications
  @type client_notification ::
          cancelled_notification()
          | progress_notification()
          | list_changed_notification()

  @type server_notification ::
          cancelled_notification()
          | progress_notification()
          | log_notification()
          | resource_updated_notification()
          | list_changed_notification()

  # Helper functions for type conversions
  @doc """
  Converts a string role to an atom.
  """
  @spec string_to_role(String.t()) :: role()
  def string_to_role("user"), do: :user
  def string_to_role("assistant"), do: :assistant

  @doc """
  Converts a role atom to a string.
  """
  @spec role_to_string(role()) :: String.t()
  def role_to_string(:user), do: "user"
  def role_to_string(:assistant), do: "assistant"

  @doc """
  Converts a string log level to an atom.
  """
  @spec string_to_log_level(String.t()) :: log_level()
  def string_to_log_level("debug"), do: :debug
  def string_to_log_level("info"), do: :info
  def string_to_log_level("notice"), do: :notice
  def string_to_log_level("warning"), do: :warning
  def string_to_log_level("error"), do: :error
  def string_to_log_level("critical"), do: :critical
  def string_to_log_level("alert"), do: :alert
  def string_to_log_level("emergency"), do: :emergency

  @doc """
  Converts a log level atom to a string.
  """
  @spec log_level_to_string(log_level()) :: String.t()
  def log_level_to_string(:debug), do: "debug"
  def log_level_to_string(:info), do: "info"
  def log_level_to_string(:notice), do: "notice"
  def log_level_to_string(:warning), do: "warning"
  def log_level_to_string(:error), do: "error"
  def log_level_to_string(:critical), do: "critical"
  def log_level_to_string(:alert), do: "alert"
  def log_level_to_string(:emergency), do: "emergency"

  @doc """
  Converts a string content type to an atom.
  """
  @spec string_to_content_type(String.t()) :: content_type()
  def string_to_content_type("text"), do: :text
  def string_to_content_type("image"), do: :image
  def string_to_content_type("audio"), do: :audio
  def string_to_content_type("resource"), do: :resource

  @doc """
  Converts a content type atom to a string.
  """
  @spec content_type_to_string(content_type()) :: String.t()
  def content_type_to_string(:text), do: "text"
  def content_type_to_string(:image), do: "image"
  def content_type_to_string(:audio), do: "audio"
  def content_type_to_string(:resource), do: "resource"

  @doc """
  Converts a string include context to an atom.
  """
  @spec string_to_include_context(String.t()) :: include_context()
  def string_to_include_context("none"), do: :none
  def string_to_include_context("thisServer"), do: :thisServer
  def string_to_include_context("allServers"), do: :allServers

  @doc """
  Converts an include context atom to a string.
  """
  @spec include_context_to_string(include_context()) :: String.t()
  def include_context_to_string(:none), do: "none"
  def include_context_to_string(:thisServer), do: "thisServer"
  def include_context_to_string(:allServers), do: "allServers"
end
