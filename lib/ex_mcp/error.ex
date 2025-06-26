defmodule ExMCP.Error do
  @moduledoc """
  Structured error types for MCP operations.

  This module provides standardized error handling for MCP operations,
  following JSON-RPC 2.0 error codes and MCP-specific error patterns.

  ## Error Categories

  ### JSON-RPC 2.0 Standard Errors
  - `-32700` - Parse error
  - `-32600` - Invalid request
  - `-32601` - Method not found
  - `-32602` - Invalid params
  - `-32603` - Internal error

  ### MCP-Specific Errors
  - `-32001` - Tool not found
  - `-32002` - Resource not found
  - `-32003` - Prompt not found
  - `-32004` - Tool execution error
  - `-32005` - Subscription error

  ## Usage

      # Create standard errors
      error = ExMCP.Error.parse_error("Invalid JSON")
      error = ExMCP.Error.method_not_found("unknown/method")

      # Create tool-specific errors
      error = ExMCP.Error.tool_error("Execution failed", "calculator",
        data: %{input: "1/0", reason: "division by zero"}
      )

      # Convert to JSON-RPC format
      json_error = ExMCP.Error.to_json_rpc(error)

      # Categorize errors
      category = ExMCP.Error.category(error)
      # => :protocol | :application | :transport

  ## Error Handling Best Practices

  1. Use specific error functions when available
  2. Include helpful data in error responses
  3. Maintain error code consistency
  4. Provide actionable error messages
  """

  alias ExMCP.Protocol.ErrorCodes

  defexception [:code, :message, :data, :request_id]

  @type t :: %__MODULE__{
          code: integer() | atom(),
          message: String.t(),
          data: any(),
          request_id: String.t() | nil
        }

  # JSON-RPC 2.0 Error Codes (using ErrorCodes module)
  @parse_error ErrorCodes.parse_error()
  @invalid_request ErrorCodes.invalid_request()
  @method_not_found ErrorCodes.method_not_found()
  @invalid_params ErrorCodes.invalid_params()
  @internal_error ErrorCodes.internal_error()

  # MCP-specific Error Codes
  @tool_error -32000
  @resource_error -32001
  @prompt_error -32002
  @transport_error -32003
  @authentication_error -32004
  @authorization_error -32005

  @doc """
  Creates an error from a JSON-RPC error response.

  ## Examples

      iex> json_error = %{"code" => -32601, "message" => "Method not found"}
      iex> ExMCP.Error.from_json_rpc_error(json_error)
      %ExMCP.Error{
        code: -32601,
        message: "Method not found",
        data: nil,
        request_id: nil
      }
  """
  @spec from_json_rpc_error(map(), keyword()) :: t()
  def from_json_rpc_error(error_data, opts \\ []) when is_map(error_data) do
    %__MODULE__{
      code: Map.get(error_data, "code"),
      message: Map.get(error_data, "message", "Unknown error"),
      data: Map.get(error_data, "data"),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates a connection error.

  ## Examples

      iex> ExMCP.Error.connection_error("Connection refused")
      %ExMCP.Error{
        code: :connection_error,
        message: "Connection error: Connection refused",
        data: nil,
        request_id: nil
      }
  """
  @spec connection_error(String.t(), keyword()) :: t()
  def connection_error(reason, opts \\ []) do
    %__MODULE__{
      code: :connection_error,
      message: "Connection error: #{reason}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates a parse error (JSON-RPC -32700).

  ## Examples

      iex> ExMCP.Error.parse_error("Invalid JSON")
      %ExMCP.Error{
        code: -32700,
        message: "Parse error: Invalid JSON",
        data: nil,
        request_id: nil
      }
  """
  @spec parse_error(String.t(), keyword()) :: t()
  def parse_error(reason \\ "Parse error", opts \\ []) do
    %__MODULE__{
      code: @parse_error,
      message: "Parse error: #{reason}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates an invalid request error (JSON-RPC -32600).
  """
  @spec invalid_request(String.t(), keyword()) :: t()
  def invalid_request(reason \\ "Invalid request", opts \\ []) do
    %__MODULE__{
      code: @invalid_request,
      message: "Invalid request: #{reason}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates a method not found error (JSON-RPC -32601).
  """
  @spec method_not_found(String.t(), keyword()) :: t()
  def method_not_found(method_name \\ "unknown", opts \\ []) do
    %__MODULE__{
      code: @method_not_found,
      message: "Method not found: #{method_name}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates an invalid params error (JSON-RPC -32602).
  """
  @spec invalid_params(String.t(), keyword()) :: t()
  def invalid_params(reason \\ "Invalid parameters", opts \\ []) do
    %__MODULE__{
      code: @invalid_params,
      message: "Invalid params: #{reason}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates an internal error (JSON-RPC -32603).
  """
  @spec internal_error(String.t(), keyword()) :: t()
  def internal_error(reason \\ "Internal error", opts \\ []) do
    %__MODULE__{
      code: @internal_error,
      message: "Internal error: #{reason}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates a tool execution error (MCP -32000).
  """
  @spec tool_error(String.t(), String.t() | nil, keyword()) :: t()
  def tool_error(reason, tool_name \\ nil, opts \\ []) do
    message =
      case tool_name do
        nil -> "Tool error: #{reason}"
        name -> "Tool error in '#{name}': #{reason}"
      end

    %__MODULE__{
      code: @tool_error,
      message: message,
      data: Keyword.get(opts, :data, %{tool_name: tool_name}),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates a resource error (MCP -32001).
  """
  @spec resource_error(String.t(), String.t() | nil, keyword()) :: t()
  def resource_error(reason, resource_uri \\ nil, opts \\ []) do
    message =
      case resource_uri do
        nil -> "Resource error: #{reason}"
        uri -> "Resource error for '#{uri}': #{reason}"
      end

    %__MODULE__{
      code: @resource_error,
      message: message,
      data: Keyword.get(opts, :data, %{resource_uri: resource_uri}),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates a prompt error (MCP -32002).
  """
  @spec prompt_error(String.t(), String.t() | nil, keyword()) :: t()
  def prompt_error(reason, prompt_name \\ nil, opts \\ []) do
    message =
      case prompt_name do
        nil -> "Prompt error: #{reason}"
        name -> "Prompt error in '#{name}': #{reason}"
      end

    %__MODULE__{
      code: @prompt_error,
      message: message,
      data: Keyword.get(opts, :data, %{prompt_name: prompt_name}),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates a transport error (MCP -32003).
  """
  @spec transport_error(String.t(), keyword()) :: t()
  def transport_error(reason, opts \\ []) do
    %__MODULE__{
      code: @transport_error,
      message: "Transport error: #{reason}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates an authentication error (MCP -32004).
  """
  @spec authentication_error(String.t(), keyword()) :: t()
  def authentication_error(reason \\ "Authentication failed", opts \\ []) do
    %__MODULE__{
      code: @authentication_error,
      message: "Authentication error: #{reason}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Creates an authorization error (MCP -32005).
  """
  @spec authorization_error(String.t(), keyword()) :: t()
  def authorization_error(reason \\ "Access denied", opts \\ []) do
    %__MODULE__{
      code: @authorization_error,
      message: "Authorization error: #{reason}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id)
    }
  end

  @doc """
  Converts the error to JSON-RPC error format.

  ## Examples

      iex> error = ExMCP.Error.method_not_found("test_method")
      iex> ExMCP.Error.to_json_rpc(error)
      %{
        "code" => -32601,
        "message" => "Method not found: test_method",
        "data" => nil
      }
  """
  @spec to_json_rpc(t()) :: map()
  def to_json_rpc(%__MODULE__{} = error) do
    base = %{
      "code" => error.code,
      "message" => error.message
    }

    case error.data do
      nil -> base
      data -> Map.put(base, "data", data)
    end
  end

  @doc """
  Checks if an error is a JSON-RPC standard error.
  """
  @spec json_rpc_error?(t()) :: boolean()
  def json_rpc_error?(%__MODULE__{code: code}) when is_integer(code) do
    code >= -32768 && code <= -32000
  end

  def json_rpc_error?(_), do: false

  @doc """
  Checks if an error is an MCP-specific error.
  """
  @spec mcp_error?(t()) :: boolean()
  def mcp_error?(%__MODULE__{code: code}) when is_integer(code) do
    code >= -32005 && code <= -32000
  end

  def mcp_error?(_), do: false

  @doc """
  Gets a human-readable error category.

  ## Examples

      iex> error = ExMCP.Error.tool_error("Execution failed", "calculate")
      iex> ExMCP.Error.category(error)
      "Tool Error"
  """
  @spec category(t()) :: String.t()
  def category(%__MODULE__{code: @parse_error}), do: "Parse Error"
  def category(%__MODULE__{code: @invalid_request}), do: "Invalid Request"
  def category(%__MODULE__{code: @method_not_found}), do: "Method Not Found"
  def category(%__MODULE__{code: @invalid_params}), do: "Invalid Parameters"
  def category(%__MODULE__{code: @internal_error}), do: "Internal Error"
  def category(%__MODULE__{code: @tool_error}), do: "Tool Error"
  def category(%__MODULE__{code: @resource_error}), do: "Resource Error"
  def category(%__MODULE__{code: @prompt_error}), do: "Prompt Error"
  def category(%__MODULE__{code: @transport_error}), do: "Transport Error"
  def category(%__MODULE__{code: @authentication_error}), do: "Authentication Error"
  def category(%__MODULE__{code: @authorization_error}), do: "Authorization Error"
  def category(%__MODULE__{code: :connection_error}), do: "Connection Error"
  def category(%__MODULE__{}), do: "Unknown Error"

  # Exception behavior implementation
  @impl Exception
  def message(%__MODULE__{message: msg}), do: msg
end
