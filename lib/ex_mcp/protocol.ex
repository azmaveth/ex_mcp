defmodule ExMCP.Protocol do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.Protocol`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.Protocol instead. Note that internal modules are not part of the public API."

  # Delegate all function calls to the new location
  defdelegate encode_initialize(client_info, capabilities \\ nil, version \\ nil),
    to: ExMCP.Internal.Protocol

  defdelegate encode_initialized(), to: ExMCP.Internal.Protocol
  defdelegate encode_list_tools(cursor \\ nil, meta \\ nil), to: ExMCP.Internal.Protocol

  defdelegate encode_call_tool(name, arguments, meta_or_progress_token \\ nil),
    to: ExMCP.Internal.Protocol

  defdelegate encode_list_resources(cursor \\ nil, meta \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_read_resource(uri), to: ExMCP.Internal.Protocol
  defdelegate encode_list_prompts(cursor \\ nil, meta \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_get_prompt(name, arguments \\ %{}, meta \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_complete(ref, argument, meta \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_create_message(params, meta \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_list_roots(), to: ExMCP.Internal.Protocol
  defdelegate encode_subscribe_resource(uri), to: ExMCP.Internal.Protocol
  defdelegate encode_unsubscribe_resource(uri), to: ExMCP.Internal.Protocol
  defdelegate encode_response(result, id), to: ExMCP.Internal.Protocol
  defdelegate encode_error(code, message, data \\ nil, id), to: ExMCP.Internal.Protocol
  defdelegate encode_notification(method, params), to: ExMCP.Internal.Protocol
  defdelegate encode_resources_changed(), to: ExMCP.Internal.Protocol
  defdelegate encode_tools_changed(), to: ExMCP.Internal.Protocol
  defdelegate encode_prompts_changed(), to: ExMCP.Internal.Protocol
  defdelegate encode_resource_updated(uri), to: ExMCP.Internal.Protocol

  defdelegate encode_progress(progress_token, progress, total \\ nil, message \\ nil),
    to: ExMCP.Internal.Protocol

  defdelegate encode_roots_changed(), to: ExMCP.Internal.Protocol
  defdelegate encode_cancelled(request_id, reason \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_cancelled!(request_id, reason \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_ping(), to: ExMCP.Internal.Protocol
  defdelegate encode_pong(id), to: ExMCP.Internal.Protocol
  defdelegate encode_list_resource_templates(cursor \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_log_message(level, message, data \\ nil), to: ExMCP.Internal.Protocol
  defdelegate encode_set_log_level(level), to: ExMCP.Internal.Protocol
  defdelegate encode_elicitation_create(message, requested_schema), to: ExMCP.Internal.Protocol
  defdelegate encode_batch(messages), to: ExMCP.Internal.Protocol
  defdelegate parse_batch_response(responses), to: ExMCP.Internal.Protocol
  defdelegate parse_message(data), to: ExMCP.Internal.Protocol
  defdelegate method_available?(method, version), to: ExMCP.Internal.Protocol
  defdelegate validate_message_version(message, version), to: ExMCP.Internal.Protocol
  defdelegate encode_to_string(message), to: ExMCP.Internal.Protocol
  defdelegate generate_id(), to: ExMCP.Internal.Protocol
  defdelegate parse_error(), to: ExMCP.Internal.Protocol
  defdelegate invalid_request(), to: ExMCP.Internal.Protocol
  defdelegate method_not_found(), to: ExMCP.Internal.Protocol
  defdelegate invalid_params(), to: ExMCP.Internal.Protocol
  defdelegate internal_error(), to: ExMCP.Internal.Protocol
end
