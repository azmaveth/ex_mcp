defmodule ExMCP.Server.RefactorHelpers do
  @moduledoc """
  Helper module to support the gradual refactoring of ExMCP.Server.

  This module provides compatibility shims and delegation functions to ensure
  backward compatibility during the refactoring process.
  """

  alias ExMCP.Protocol.ResponseBuilder

  @doc """
  Delegates to ResponseBuilder.build_success_response/2 for compatibility.
  """
  defdelegate build_success_response(result, id), to: ResponseBuilder

  @doc """
  Delegates to ResponseBuilder.build_error_response/4 for compatibility.
  """
  defdelegate build_error_response(code, message, data \\ nil, id), to: ResponseBuilder

  @doc """
  Delegates to ResponseBuilder.build_notification/2 for compatibility.
  """
  defdelegate build_notification(method, params), to: ResponseBuilder

  @doc """
  Delegates to ResponseBuilder.build_request/3 for compatibility.
  """
  defdelegate build_request(method, params, id), to: ResponseBuilder

  @doc """
  Delegates to ResponseBuilder.build_mcp_error/4 for compatibility.
  """
  defdelegate build_mcp_error(error_atom, id, custom_message \\ nil, data \\ nil),
    to: ResponseBuilder

  @doc """
  Delegates to ResponseBuilder.build_tool_error/3 for compatibility.
  """
  defdelegate build_tool_error(error_text, is_error \\ true, id), to: ResponseBuilder

  @doc """
  Delegates to ResponseBuilder.build_batch_error/1 for compatibility.
  """
  defdelegate build_batch_error(protocol_version \\ "2025-06-18"), to: ResponseBuilder
end
