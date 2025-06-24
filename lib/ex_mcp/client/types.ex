defmodule ExMCP.Client.Types do
  @moduledoc """
  Shared type definitions for ExMCP.Client modules.

  This module provides common type definitions used across the client
  architecture to ensure consistency and type safety.
  """

  @typedoc """
  A client process reference - can be a PID, registered name, or GenServer reference.
  """
  @type client :: GenServer.server()

  @typedoc """
  Options for MCP requests, typically including timeout and format options.
  """
  @type request_opts :: keyword()

  @typedoc """
  Standard MCP response format - either success with data or error with reason.
  """
  @type mcp_response :: {:ok, any()} | {:error, any()}

  @typedoc """
  MCP method name used in requests.
  """
  @type mcp_method :: String.t()

  @typedoc """
  Parameters map for MCP requests.
  """
  @type mcp_params :: map()

  @typedoc """
  Default timeout value in milliseconds.
  """
  @type default_timeout :: pos_integer()

  @typedoc """
  MCP resource URI - typically a file:// or http:// URI.
  """
  @type uri :: String.t()

  @typedoc """
  MCP tool name.
  """
  @type tool_name :: String.t()

  @typedoc """
  Arguments map for tool calls.
  """
  @type tool_arguments :: map()

  @typedoc """
  Request options or timeout value.
  """
  @type request_opts_or_timeout :: keyword() | timeout()
end
