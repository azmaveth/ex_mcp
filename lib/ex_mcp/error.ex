defmodule ExMCP.Error do
  @moduledoc """
  Error types and utilities for ExMCP.

  This module provides structured error handling with proper error types
  that can be pattern matched and provide useful debugging information.
  """

  defstruct [:code, :message, :data, :request_id, __exception__: true]

  @type t :: %__MODULE__{
          code: integer(),
          message: String.t(),
          data: any(),
          request_id: String.t() | nil,
          __exception__: true
        }

  # Implement the Exception behaviour for Error struct
  def exception(value) when is_map(value), do: struct(__MODULE__, value)
  def exception(msg) when is_binary(msg), do: %__MODULE__{message: msg, __exception__: true}
  def message(%__MODULE__{message: message}), do: message || ""

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
  Creates a protocol error exception with the given JSON-RPC error code.

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
  Creates a transport error exception.
  """
  # Transport error struct versions for tests (binary details)
  def transport_error(details) when is_binary(details) do
    %__MODULE__{
      code: -32003,
      message: "Transport error: #{details}",
      data: nil,
      request_id: nil,
      __exception__: true
    }
  end

  def transport_error(details, opts) when is_binary(details) and is_list(opts) do
    %__MODULE__{
      code: -32003,
      message: "Transport error: #{details}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  # Transport error exception versions (transport/reason pattern)
  def transport_error(transport, reason) do
    %TransportError{
      transport: transport,
      reason: reason,
      details: nil
    }
  end

  def transport_error(transport, reason, details) do
    %TransportError{
      transport: transport,
      reason: reason,
      details: details
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

  # Wrapper functions for backward compatibility with tests
  # These handle the different call patterns between the exception-based API
  # and the struct-based API expected by tests

  # Pattern 1: tool_error(details) - struct API
  def tool_error(details) when is_binary(details) do
    ExMCP.ErrorHelpers.tool_error(details, nil, [])
  end

  # Pattern 3: tool_error(details, tool_name) - struct API (needs to come first)
  def tool_error(details, tool_name)
      when is_binary(details) and (is_binary(tool_name) or is_nil(tool_name)) do
    ExMCP.ErrorHelpers.tool_error(details, tool_name, [])
  end

  # Pattern 2: tool_error(tool_name, reason) - exception API
  def tool_error(tool_name, reason) when is_atom(tool_name) do
    %ToolError{
      tool_name: tool_name,
      reason: reason,
      arguments: nil
    }
  end

  # Handle case where reason is an exception struct - pass it through directly
  def tool_error(_tool_name, %ProtocolError{} = error), do: error
  def tool_error(_tool_name, %TransportError{} = error), do: error
  def tool_error(_tool_name, %ToolError{} = error), do: error
  def tool_error(_tool_name, %ResourceError{} = error), do: error
  def tool_error(_tool_name, %ValidationError{} = error), do: error

  # Pattern 5: tool_error(details, tool_name, opts) - struct API with options (needs to come first)
  def tool_error(details, tool_name, opts)
      when is_binary(details) and is_binary(tool_name) and is_list(opts) do
    ExMCP.ErrorHelpers.tool_error(details, tool_name, opts)
  end

  # Pattern 4: tool_error(tool_name, reason, arguments) - exception API with optional arguments
  def tool_error(tool_name, reason, arguments) when is_atom(tool_name) or is_binary(tool_name) do
    %ToolError{
      tool_name: tool_name,
      reason: reason,
      arguments: arguments
    }
  end

  # resource_error patterns
  def resource_error(details, uri) when is_binary(details) and is_binary(uri) do
    ExMCP.ErrorHelpers.resource_error(details, uri, [])
  end

  def resource_error(uri, operation, reason) when is_atom(operation) do
    %ResourceError{
      uri: uri,
      operation: operation,
      reason: reason
    }
  end

  def resource_error(details, uri, opts)
      when is_binary(details) and is_binary(uri) and is_list(opts) do
    ExMCP.ErrorHelpers.resource_error(details, uri, opts)
  end

  def authentication_error(details) when is_binary(details) do
    %__MODULE__{
      code: -32004,
      message: "Authentication error: #{details}",
      data: nil,
      request_id: nil,
      __exception__: true
    }
  end

  def authorization_error(details) when is_binary(details) do
    %__MODULE__{
      code: -32005,
      message: "Authorization error: #{details}",
      data: nil,
      request_id: nil,
      __exception__: true
    }
  end

  def connection_error_struct(details) when is_binary(details) do
    %__MODULE__{
      code: :connection_error,
      message: "Connection error: #{details}",
      data: nil,
      request_id: nil,
      __exception__: true
    }
  end

  def connection_error_struct(details, opts) when is_binary(details) and is_list(opts) do
    %__MODULE__{
      code: :connection_error,
      message: "Connection error: #{details}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  # Delegate functions for backward compatibility with tests
  defdelegate from_json_rpc_error(json_error, opts \\ []), to: ExMCP.ErrorHelpers
  defdelegate parse_error(details \\ "", opts \\ []), to: ExMCP.ErrorHelpers
  defdelegate invalid_request(details, opts \\ []), to: ExMCP.ErrorHelpers
  defdelegate method_not_found(method, opts \\ []), to: ExMCP.ErrorHelpers
  defdelegate invalid_params(details, opts \\ []), to: ExMCP.ErrorHelpers
  defdelegate internal_error(details, opts \\ []), to: ExMCP.ErrorHelpers
  defdelegate prompt_error(details, prompt_name, opts \\ []), to: ExMCP.ErrorHelpers

  # This delegate creates the Error struct for tests
  defdelegate connection_error(details), to: ExMCP.ErrorHelpers
  defdelegate connection_error(details, opts), to: ExMCP.ErrorHelpers

  # Classification functions for Error structs
  def json_rpc_error?(%__MODULE__{code: code}) when is_integer(code), do: true
  def json_rpc_error?(_), do: false

  def mcp_error?(%__MODULE__{code: code}) when code in [-32000, -32001, -32002], do: true
  def mcp_error?(_), do: false

  def category(%__MODULE__{code: -32700}), do: "Parse Error"
  def category(%__MODULE__{code: -32600}), do: "Invalid Request"
  def category(%__MODULE__{code: -32601}), do: "Method Not Found"
  def category(%__MODULE__{code: -32602}), do: "Invalid Params"
  def category(%__MODULE__{code: -32603}), do: "Internal Error"
  def category(%__MODULE__{code: -32000}), do: "Tool Error"
  def category(%__MODULE__{code: -32001}), do: "Resource Error"
  def category(%__MODULE__{code: -32002}), do: "Prompt Error"
  def category(%__MODULE__{code: -32003}), do: "Transport Error"
  def category(%__MODULE__{code: -32004}), do: "Authentication Error"
  def category(%__MODULE__{code: -32005}), do: "Authorization Error"
  def category(%__MODULE__{code: :connection_error}), do: "Connection Error"
  def category(_), do: "Unknown Error"

  # Convert errors to JSON-RPC format
  def to_json_rpc(%__MODULE__{code: -32000, message: _message, data: data}) do
    # Tool error - use standard message for consistency
    result = %{
      "code" => -32000,
      "message" => "Tool execution error"
    }

    if data do
      Map.put(result, "data", data)
    else
      result
    end
  end

  def to_json_rpc(%__MODULE__{code: :connection_error, message: message, data: data}) do
    result = %{
      "code" => -32003,
      "message" => message
    }

    if data do
      Map.put(result, "data", data)
    else
      result
    end
  end

  def to_json_rpc(%__MODULE__{code: code, message: message, data: data}) when is_integer(code) do
    result = %{
      "code" => code,
      "message" => message
    }

    if data do
      Map.put(result, "data", data)
    else
      result
    end
  end

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
        "operation" => to_string(error.operation),
        "reason" => inspect(error.reason)
      }
    }
  end

  def to_json_rpc(%ValidationError{} = error) do
    %{
      "code" => -32000,
      "message" => "Tool execution error",
      "data" => %{
        "tool" => error.field,
        "reason" => inspect(error.reason)
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
end
