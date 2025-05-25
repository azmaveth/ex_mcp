defmodule ExMCP.Protocol do
  @moduledoc """
  MCP protocol message encoding and decoding.
  
  Implements the Model Context Protocol JSON-RPC message format.
  This module handles the low-level protocol details for both
  client and server implementations.
  """

  @protocol_version "2024-11-05"

  @type json_rpc_id :: String.t() | integer()
  @type method :: String.t()
  @type params :: map()
  @type result :: any()
  @type error :: %{code: integer(), message: String.t(), data: any()}

  # Client Request Encoding

  @doc """
  Encodes an initialize request from client to server.
  """
  @spec encode_initialize(map()) :: map()
  def encode_initialize(client_info) do
    %{
      "jsonrpc" => "2.0",
      "method" => "initialize",
      "params" => %{
        "protocolVersion" => @protocol_version,
        "capabilities" => %{
          "roots" => %{},
          "sampling" => %{}
        },
        "clientInfo" => client_info
      },
      "id" => generate_id()
    }
  end

  @doc """
  Encodes an initialized notification.
  """
  @spec encode_initialized() :: map()
  def encode_initialized() do
    %{
      "jsonrpc" => "2.0",
      "method" => "notifications/initialized",
      "params" => %{}
    }
  end

  @doc """
  Encodes a request to list available tools.
  """
  @spec encode_list_tools() :: map()
  def encode_list_tools() do
    %{
      "jsonrpc" => "2.0",
      "method" => "tools/list",
      "params" => %{},
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a tool call request.
  """
  @spec encode_call_tool(String.t(), map()) :: map()
  def encode_call_tool(name, arguments) do
    %{
      "jsonrpc" => "2.0",
      "method" => "tools/call",
      "params" => %{
        "name" => name,
        "arguments" => arguments
      },
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a request to list available resources.
  """
  @spec encode_list_resources() :: map()
  def encode_list_resources() do
    %{
      "jsonrpc" => "2.0",
      "method" => "resources/list",
      "params" => %{},
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a resource read request.
  """
  @spec encode_read_resource(String.t()) :: map()
  def encode_read_resource(uri) do
    %{
      "jsonrpc" => "2.0",
      "method" => "resources/read",
      "params" => %{
        "uri" => uri
      },
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a request to list available prompts.
  """
  @spec encode_list_prompts() :: map()
  def encode_list_prompts() do
    %{
      "jsonrpc" => "2.0",
      "method" => "prompts/list",
      "params" => %{},
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a prompt get request.
  """
  @spec encode_get_prompt(String.t(), map()) :: map()
  def encode_get_prompt(name, arguments \\ %{}) do
    %{
      "jsonrpc" => "2.0",
      "method" => "prompts/get",
      "params" => %{
        "name" => name,
        "arguments" => arguments
      },
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a completion request.
  """
  @spec encode_complete(String.t(), map()) :: map()
  def encode_complete(ref, params) do
    %{
      "jsonrpc" => "2.0",
      "method" => "completion/complete",
      "params" => Map.merge(%{"ref" => ref}, params),
      "id" => generate_id()
    }
  end

  # Server Response Encoding

  @doc """
  Encodes a successful response.
  """
  @spec encode_response(result(), json_rpc_id()) :: map()
  def encode_response(result, id) do
    %{
      "jsonrpc" => "2.0",
      "result" => result,
      "id" => id
    }
  end

  @doc """
  Encodes an error response.
  """
  @spec encode_error(integer(), String.t(), any(), json_rpc_id()) :: map()
  def encode_error(code, message, data \\ nil, id) do
    error = %{
      "code" => code,
      "message" => message
    }
    
    error = if data, do: Map.put(error, "data", data), else: error
    
    %{
      "jsonrpc" => "2.0",
      "error" => error,
      "id" => id
    }
  end

  @doc """
  Encodes a notification (no id field).
  """
  @spec encode_notification(method(), params()) :: map()
  def encode_notification(method, params) do
    %{
      "jsonrpc" => "2.0",
      "method" => method,
      "params" => params
    }
  end

  # Message Parsing

  @doc """
  Parses a JSON-RPC message.
  
  Returns one of:
  - `{:request, method, params, id}` - An incoming request
  - `{:notification, method, params}` - An incoming notification
  - `{:result, result, id}` - A response to our request
  - `{:error, error, id}` - An error response
  - `{:error, :invalid_message}` - Invalid message format
  """
  @spec parse_message(String.t() | map()) :: 
    {:request, method(), params(), json_rpc_id()} |
    {:notification, method(), params()} |
    {:result, result(), json_rpc_id()} |
    {:error, error(), json_rpc_id()} |
    {:error, :invalid_message}
  def parse_message(data) when is_binary(data) do
    case Jason.decode(data) do
      {:ok, decoded} -> parse_message(decoded)
      {:error, _} -> {:error, :invalid_message}
    end
  end

  def parse_message(%{"jsonrpc" => "2.0", "method" => method, "params" => params, "id" => id}) do
    {:request, method, params, id}
  end

  def parse_message(%{"jsonrpc" => "2.0", "method" => method, "params" => params}) do
    {:notification, method, params}
  end

  def parse_message(%{"jsonrpc" => "2.0", "result" => result, "id" => id}) do
    {:result, result, id}
  end

  def parse_message(%{"jsonrpc" => "2.0", "error" => error, "id" => id}) do
    {:error, error, id}
  end

  def parse_message(_), do: {:error, :invalid_message}

  # Utilities

  @doc """
  Encodes a message to JSON string.
  """
  @spec encode_to_string(map()) :: {:ok, String.t()} | {:error, any()}
  def encode_to_string(message) do
    Jason.encode(message)
  end

  @doc """
  Generates a unique ID for requests.
  """
  @spec generate_id() :: integer()
  def generate_id() do
    System.unique_integer([:positive, :monotonic])
  end

  # Error Codes (from JSON-RPC spec)
  @parse_error -32700
  @invalid_request -32600
  @method_not_found -32601
  @invalid_params -32602
  @internal_error -32603

  @doc """
  Standard JSON-RPC error codes.
  """
  def parse_error, do: @parse_error
  def invalid_request, do: @invalid_request
  def method_not_found, do: @method_not_found
  def invalid_params, do: @invalid_params
  def internal_error, do: @internal_error
end