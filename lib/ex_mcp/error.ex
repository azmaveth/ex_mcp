defmodule ExMCP.Error do
  @moduledoc """
  Error types and utilities for ExMCP.

  This module provides structured error handling with proper error types
  that can be pattern matched and provide useful debugging information.
  """

  defmodule ProtocolError do
    @moduledoc """
    Errors related to MCP protocol violations.
    """
    defexception [:code, :message, :data]

    @impl true
    def message(%{code: code, message: message}) do
      "MCP Protocol Error (#{code}): #{message}"
    end
  end

  defmodule TransportError do
    @moduledoc """
    Errors related to transport layer issues.
    """
    defexception [:transport, :reason, :details]

    @impl true
    def message(%{transport: transport, reason: reason}) do
      "Transport Error (#{transport}): #{inspect(reason)}"
    end
  end

  defmodule ToolError do
    @moduledoc """
    Errors that occur during tool execution.
    """
    defexception [:tool_name, :reason, :arguments]

    @impl true
    def message(%{tool_name: tool_name, reason: reason}) do
      "Tool Error (#{tool_name}): #{inspect(reason)}"
    end
  end

  defmodule ResourceError do
    @moduledoc """
    Errors that occur during resource operations.
    """
    defexception [:uri, :operation, :reason]

    @impl true
    def message(%{uri: uri, operation: operation, reason: reason}) do
      "Resource Error (#{operation} #{uri}): #{inspect(reason)}"
    end
  end

  defmodule ValidationError do
    @moduledoc """
    Errors related to input validation.
    """
    defexception [:field, :value, :reason]

    @impl true
    def message(%{field: field, reason: reason}) do
      "Validation Error (#{field}): #{reason}"
    end
  end

  @doc """
  Creates a protocol error with the given JSON-RPC error code.

  ## Standard JSON-RPC Error Codes

  * `-32700` - Parse error
  * `-32600` - Invalid Request
  * `-32601` - Method not found
  * `-32602` - Invalid params
  * `-32603` - Internal error
  * `-32000` to `-32099` - Server error
  """
  def protocol_error(code, message, data \\ nil) do
    %ProtocolError{
      code: code,
      message: message,
      data: data
    }
  end

  @doc """
  Creates a transport error.
  """
  def transport_error(transport, reason, details \\ nil) do
    %TransportError{
      transport: transport,
      reason: reason,
      details: details
    }
  end

  @doc """
  Creates a tool execution error.
  """
  def tool_error(tool_name, reason, arguments \\ nil) do
    %ToolError{
      tool_name: tool_name,
      reason: reason,
      arguments: arguments
    }
  end

  @doc """
  Creates a resource operation error.
  """
  def resource_error(uri, operation, reason) do
    %ResourceError{
      uri: uri,
      operation: operation,
      reason: reason
    }
  end

  @doc """
  Creates a validation error.
  """
  def validation_error(field, value, reason) do
    %ValidationError{
      field: field,
      value: value,
      reason: reason
    }
  end

  @doc """
  Converts an error to a JSON-RPC error response format.
  """
  def to_json_rpc(%ProtocolError{code: code, message: message, data: data}) do
    error = %{
      "code" => code,
      "message" => message
    }

    if data do
      Map.put(error, "data", data)
    else
      error
    end
  end

  def to_json_rpc(%TransportError{} = error) do
    %{
      "code" => -32000,
      "message" => "Transport error",
      "data" => %{
        "transport" => error.transport,
        "reason" => inspect(error.reason),
        "details" => error.details
      }
    }
  end

  def to_json_rpc(%ToolError{} = error) do
    %{
      "code" => -32000,
      "message" => "Tool execution error",
      "data" => %{
        "tool" => error.tool_name,
        "reason" => inspect(error.reason)
      }
    }
  end

  def to_json_rpc(%ResourceError{} = error) do
    %{
      "code" => -32000,
      "message" => "Resource operation error",
      "data" => %{
        "uri" => error.uri,
        "operation" => error.operation,
        "reason" => inspect(error.reason)
      }
    }
  end

  def to_json_rpc(%ValidationError{} = error) do
    %{
      "code" => -32602,
      "message" => "Invalid params",
      "data" => %{
        "field" => error.field,
        "reason" => error.reason
      }
    }
  end

  def to_json_rpc(error) do
    %{
      "code" => -32603,
      "message" => "Internal error",
      "data" => inspect(error)
    }
  end

  @doc """
  Wraps a function call and converts exceptions to proper error tuples.

  ## Examples

      ExMCP.Error.wrap(fn ->
        do_something_dangerous()
      end)
      # => {:ok, result} or {:error, %ExMCP.Error.SomeError{}}
  """
  def wrap(fun) when is_function(fun, 0) do
    {:ok, fun.()}
  rescue
    e in [ProtocolError, TransportError, ToolError, ResourceError, ValidationError] ->
      {:error, e}

    e ->
      {:error, %RuntimeError{message: Exception.message(e)}}
  end

  @doc """
  Wraps a function call with a custom error transformer.

  ## Examples

      ExMCP.Error.wrap_with(fn ->
        read_file(path)
      end, fn
        {:error, :enoent} -> ExMCP.Error.resource_error(path, :read, :not_found)
        error -> error
      end)
  """
  def wrap_with(fun, error_transformer)
      when is_function(fun, 0) and is_function(error_transformer, 1) do
    case wrap(fun) do
      {:ok, result} -> {:ok, result}
      {:error, error} -> {:error, error_transformer.(error)}
    end
  end

  @doc """
  Creates a connection error (transport-related).
  """
  def connection_error(reason) do
    transport_error(:connection, reason)
  end

  @doc """
  Creates an internal error.
  """
  def internal_error(reason) do
    protocol_error(-32603, reason)
  end

  @doc """
  Creates an error from JSON-RPC error data.
  """
  def from_json_rpc_error(error_data, _opts \\ []) when is_map(error_data) do
    code = Map.get(error_data, "code", -32000)
    message = Map.get(error_data, "message", "Unknown error")
    data = Map.get(error_data, "data")

    protocol_error(code, message, data)
  end
end
