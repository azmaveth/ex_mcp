defmodule ExMCP.Protocol.ErrorCodes do
  @moduledoc """
  JSON-RPC 2.0 and MCP-specific error codes.

  This module provides constants and helper functions for working with
  error codes in the MCP protocol. All error codes follow the JSON-RPC 2.0
  specification with MCP-specific extensions.

  ## Standard JSON-RPC 2.0 Error Codes

  - `-32700` - Parse error: Invalid JSON was received
  - `-32600` - Invalid Request: The JSON sent is not a valid Request object
  - `-32601` - Method not found: The method does not exist or is not available
  - `-32602` - Invalid params: Invalid method parameter(s)
  - `-32603` - Internal error: Internal JSON-RPC error

  ## MCP-Specific Error Codes

  - `-32001` - Request cancelled: The request was cancelled by the client
  - `-32002` - Consent required: User consent is required for the operation
  - `-32003` - Consent denied: User denied consent for the operation
  - `-32000` - Generic server error: Catch-all for server-side errors

  ## Usage

      iex> ExMCP.Protocol.ErrorCodes.invalid_params()
      -32602

      iex> ExMCP.Protocol.ErrorCodes.error_message(:invalid_params)
      "Invalid params"

      iex> ExMCP.Protocol.ErrorCodes.is_protocol_error?(-32602)
      true
  """

  # Standard JSON-RPC 2.0 error codes
  @parse_error -32700
  @invalid_request -32600
  @method_not_found -32601
  @invalid_params -32602
  @internal_error -32603

  # MCP-specific error codes
  @request_cancelled -32001
  @consent_required -32002
  @consent_denied -32003
  @server_error -32000

  # Server-defined error codes range
  @server_error_start -32099
  @server_error_end -32000

  @doc "Parse error: Invalid JSON was received by the server"
  def parse_error, do: @parse_error

  @doc "Invalid Request: The JSON sent is not a valid Request object"
  def invalid_request, do: @invalid_request

  @doc "Method not found: The method does not exist or is not available"
  def method_not_found, do: @method_not_found

  @doc "Invalid params: Invalid method parameter(s)"
  def invalid_params, do: @invalid_params

  @doc "Internal error: Internal JSON-RPC error"
  def internal_error, do: @internal_error

  @doc "Request cancelled: The request was cancelled by the client"
  def request_cancelled, do: @request_cancelled

  @doc "Consent required: User consent is required for the operation"
  def consent_required, do: @consent_required

  @doc "Consent denied: User denied consent for the operation"
  def consent_denied, do: @consent_denied

  @doc "Generic server error: Catch-all for server-side errors"
  def server_error, do: @server_error

  @doc """
  Returns a human-readable error message for the given error code or atom.

  ## Examples

      iex> ExMCP.Protocol.ErrorCodes.error_message(-32602)
      "Invalid params"

      iex> ExMCP.Protocol.ErrorCodes.error_message(:invalid_params)
      "Invalid params"
  """
  @spec error_message(integer() | atom()) :: String.t()
  def error_message(code) when is_integer(code) do
    case code do
      @parse_error -> "Parse error"
      @invalid_request -> "Invalid Request"
      @method_not_found -> "Method not found"
      @invalid_params -> "Invalid params"
      @internal_error -> "Internal error"
      @request_cancelled -> "Request cancelled"
      @consent_required -> "Consent required"
      @consent_denied -> "Consent denied"
      @server_error -> "Server error"
      code when code >= @server_error_start and code <= @server_error_end -> "Server error"
      _ -> "Unknown error"
    end
  end

  def error_message(atom) when is_atom(atom) do
    case atom do
      :parse_error -> error_message(@parse_error)
      :invalid_request -> error_message(@invalid_request)
      :method_not_found -> error_message(@method_not_found)
      :invalid_params -> error_message(@invalid_params)
      :internal_error -> error_message(@internal_error)
      :request_cancelled -> error_message(@request_cancelled)
      :consent_required -> error_message(@consent_required)
      :consent_denied -> error_message(@consent_denied)
      :server_error -> error_message(@server_error)
      _ -> "Unknown error"
    end
  end

  @doc """
  Checks if the given error code is a standard JSON-RPC protocol error.

  ## Examples

      iex> ExMCP.Protocol.ErrorCodes.is_protocol_error?(-32602)
      true

      iex> ExMCP.Protocol.ErrorCodes.is_protocol_error?(-32001)
      false
  """
  @spec is_protocol_error?(integer()) :: boolean()
  def is_protocol_error?(code) when is_integer(code) do
    code in [@parse_error, @invalid_request, @method_not_found, @invalid_params, @internal_error]
  end

  @doc """
  Checks if the given error code is an MCP-specific error.

  ## Examples

      iex> ExMCP.Protocol.ErrorCodes.is_mcp_error?(-32001)
      true

      iex> ExMCP.Protocol.ErrorCodes.is_mcp_error?(-32602)
      false
  """
  @spec is_mcp_error?(integer()) :: boolean()
  def is_mcp_error?(code) when is_integer(code) do
    code in [@request_cancelled, @consent_required, @consent_denied] or
      (code >= @server_error_start and code <= @server_error_end)
  end

  @doc """
  Creates an error response map with the given code and message.

  ## Examples

      iex> ExMCP.Protocol.ErrorCodes.error_response(:invalid_params, "Missing required field: name")
      %{code: -32602, message: "Invalid params: Missing required field: name"}
  """
  @spec error_response(atom() | integer(), String.t() | nil) :: map()
  def error_response(code_or_atom, custom_message \\ nil)

  def error_response(atom, custom_message) when is_atom(atom) do
    code = atom_to_code(atom)
    base_message = error_message(code)

    message =
      if custom_message do
        "#{base_message}: #{custom_message}"
      else
        base_message
      end

    %{code: code, message: message}
  end

  def error_response(code, custom_message) when is_integer(code) do
    base_message = error_message(code)

    message =
      if custom_message do
        "#{base_message}: #{custom_message}"
      else
        base_message
      end

    %{code: code, message: message}
  end

  # Private helper to convert atom to error code
  defp atom_to_code(atom) do
    case atom do
      :parse_error -> @parse_error
      :invalid_request -> @invalid_request
      :method_not_found -> @method_not_found
      :invalid_params -> @invalid_params
      :internal_error -> @internal_error
      :request_cancelled -> @request_cancelled
      :consent_required -> @consent_required
      :consent_denied -> @consent_denied
      :server_error -> @server_error
      _ -> @server_error
    end
  end
end
