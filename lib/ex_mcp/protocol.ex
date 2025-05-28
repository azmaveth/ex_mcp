defmodule ExMCP.Protocol do
  @moduledoc """
  @mcp_spec

  MCP protocol message encoding and decoding.

  Implements the Model Context Protocol JSON-RPC message format.
  This module handles the low-level protocol details for both
  client and server implementations.

  All methods in this module are part of the official MCP specification.

  """

  @protocol_version "2025-03-26"

  @type json_rpc_id :: String.t() | integer()
  @type method :: String.t()
  @type params :: map()
  @type result :: any()
  @type error :: %{code: integer(), message: String.t(), data: any()}

  # Client Request Encoding

  @doc """
  Encodes an initialize request from client to server.
  """
  @spec encode_initialize(map(), map() | nil) :: map()
  def encode_initialize(client_info, capabilities \\ nil) do
    # Use provided capabilities or defaults
    caps = capabilities || %{"roots" => %{}, "sampling" => %{}}

    %{
      "jsonrpc" => "2.0",
      "method" => "initialize",
      "params" => %{
        "protocolVersion" => @protocol_version,
        "capabilities" => caps,
        "clientInfo" => client_info
      },
      "id" => generate_id()
    }
  end

  @doc """
  Encodes an initialized notification.
  """
  @spec encode_initialized() :: map()
  def encode_initialized do
    %{
      "jsonrpc" => "2.0",
      "method" => "notifications/initialized",
      "params" => %{}
    }
  end

  @doc """
  Encodes a request to list available tools.
  """
  @spec encode_list_tools(String.t() | nil) :: map()
  def encode_list_tools(cursor \\ nil) do
    params = if cursor, do: %{"cursor" => cursor}, else: %{}

    %{
      "jsonrpc" => "2.0",
      "method" => "tools/list",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a tool call request.
  """
  @spec encode_call_tool(String.t(), map(), ExMCP.Types.progress_token() | nil) :: map()
  def encode_call_tool(name, arguments, progress_token \\ nil) do
    params = %{
      "name" => name,
      "arguments" => arguments
    }

    params =
      if progress_token do
        Map.put(params, "_meta", %{"progressToken" => progress_token})
      else
        params
      end

    %{
      "jsonrpc" => "2.0",
      "method" => "tools/call",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a request to list available resources.
  """
  @spec encode_list_resources(String.t() | nil) :: map()
  def encode_list_resources(cursor \\ nil) do
    params = if cursor, do: %{"cursor" => cursor}, else: %{}

    %{
      "jsonrpc" => "2.0",
      "method" => "resources/list",
      "params" => params,
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
  @spec encode_list_prompts(String.t() | nil) :: map()
  def encode_list_prompts(cursor \\ nil) do
    params = if cursor, do: %{"cursor" => cursor}, else: %{}

    %{
      "jsonrpc" => "2.0",
      "method" => "prompts/list",
      "params" => params,
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
  @spec encode_complete(ExMCP.Types.complete_ref(), ExMCP.Types.complete_argument()) :: map()
  def encode_complete(ref, argument) do
    %{
      "jsonrpc" => "2.0",
      "method" => "completion/complete",
      "params" => %{
        "ref" => ref,
        "argument" => argument
      },
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a sampling create message request.
  """
  @spec encode_create_message(ExMCP.Types.create_message_params()) :: map()
  def encode_create_message(params) do
    %{
      "jsonrpc" => "2.0",
      "method" => "sampling/createMessage",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a request to list roots.
  """
  @spec encode_list_roots() :: map()
  def encode_list_roots do
    %{
      "jsonrpc" => "2.0",
      "method" => "roots/list",
      "params" => %{},
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a resource subscription request.
  """
  @spec encode_subscribe_resource(String.t()) :: map()
  def encode_subscribe_resource(uri) do
    %{
      "jsonrpc" => "2.0",
      "method" => "resources/subscribe",
      "params" => %{"uri" => uri},
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a resource unsubscribe request.
  """
  @spec encode_unsubscribe_resource(String.t()) :: map()
  def encode_unsubscribe_resource(uri) do
    %{
      "jsonrpc" => "2.0",
      "method" => "resources/unsubscribe",
      "params" => %{"uri" => uri},
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
  @spec encode_error(integer(), String.t(), any(), json_rpc_id() | nil) :: map()
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

  @doc """
  Encodes a resources list changed notification.
  """
  @spec encode_resources_changed() :: map()
  def encode_resources_changed do
    encode_notification("notifications/resources/list_changed", %{})
  end

  @doc """
  Encodes a tools list changed notification.
  """
  @spec encode_tools_changed() :: map()
  def encode_tools_changed do
    encode_notification("notifications/tools/list_changed", %{})
  end

  @doc """
  Encodes a prompts list changed notification.
  """
  @spec encode_prompts_changed() :: map()
  def encode_prompts_changed do
    encode_notification("notifications/prompts/list_changed", %{})
  end

  @doc """
  Encodes a resource updated notification.
  """
  @spec encode_resource_updated(String.t()) :: map()
  def encode_resource_updated(uri) do
    encode_notification("notifications/resources/updated", %{"uri" => uri})
  end

  @doc """
  Encodes a progress notification.
  """
  @spec encode_progress(ExMCP.Types.progress_token(), number(), number() | nil) :: map()
  def encode_progress(progress_token, progress, total \\ nil) do
    params = %{
      "progressToken" => progress_token,
      "progress" => progress
    }

    params = if total, do: Map.put(params, "total", total), else: params
    encode_notification("notifications/progress", params)
  end

  @doc """
  Encodes a roots list changed notification.
  """
  @spec encode_roots_changed() :: map()
  def encode_roots_changed do
    encode_notification("notifications/roots/list_changed", %{})
  end

  @doc """
  Encodes a cancelled notification.

  Per MCP specification, the "initialize" request cannot be cancelled.
  This function will validate the request ID and return an error for
  invalid cancellation attempts.

  ## Examples

      # Valid cancellation
      {:ok, notification} = Protocol.encode_cancelled("req_123", "User cancelled")

      # Invalid - initialize cannot be cancelled  
      {:error, :cannot_cancel_initialize} = Protocol.encode_cancelled("initialize", "reason")

  """
  @spec encode_cancelled(ExMCP.Types.request_id(), String.t() | nil) ::
          {:ok, map()} | {:error, :cannot_cancel_initialize}
  def encode_cancelled(request_id, reason \\ nil) do
    # Per MCP spec: The "initialize" request CANNOT be cancelled
    if request_id == "initialize" do
      {:error, :cannot_cancel_initialize}
    else
      params = %{"requestId" => request_id}
      params = if reason, do: Map.put(params, "reason", reason), else: params
      notification = encode_notification("notifications/cancelled", params)
      {:ok, notification}
    end
  end

  @doc """
  Encodes a cancelled notification (legacy version).

  This version maintains backward compatibility but does not validate
  against cancelling the initialize request.
  """
  @spec encode_cancelled!(ExMCP.Types.request_id(), String.t() | nil) :: map()
  def encode_cancelled!(request_id, reason \\ nil) do
    params = %{"requestId" => request_id}
    params = if reason, do: Map.put(params, "reason", reason), else: params
    encode_notification("notifications/cancelled", params)
  end

  @doc """
  Encodes a ping request.
  """
  @spec encode_ping() :: map()
  def encode_ping do
    %{
      "jsonrpc" => "2.0",
      "method" => "ping",
      "params" => %{},
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a pong response.
  """
  @spec encode_pong(json_rpc_id()) :: map()
  def encode_pong(id) do
    encode_response(%{}, id)
  end

  @doc """
  Encodes a request to list resource templates.
  """
  @spec encode_list_resource_templates(String.t() | nil) :: map()
  def encode_list_resource_templates(cursor \\ nil) do
    params = if cursor, do: %{"cursor" => cursor}, else: %{}

    %{
      "jsonrpc" => "2.0",
      "method" => "resources/templates/list",
      "params" => params,
      "id" => generate_id()
    }
  end

  @doc """
  Encodes a logging message.
  """
  @spec encode_log_message(ExMCP.Types.log_level(), String.t(), any()) :: map()
  def encode_log_message(level, message, data \\ nil) do
    params = %{
      "level" => level,
      "message" => message
    }

    params = if data, do: Map.put(params, "data", data), else: params
    encode_notification("notifications/log", params)
  end

  @doc """
  Encodes a logging/setLevel request.
  """
  @spec encode_set_log_level(String.t()) :: map()
  def encode_set_log_level(level) do
    %{
      "jsonrpc" => "2.0",
      "method" => "logging/setLevel",
      "params" => %{"level" => level},
      "id" => generate_id()
    }
  end

  # Batch Request Support

  @doc """
  Encodes a batch of requests and/or notifications.

  ## Example

      batch = [
        ExMCP.Protocol.encode_list_tools(),
        ExMCP.Protocol.encode_list_resources(),
        ExMCP.Protocol.encode_notification("initialized", %{})
      ]
      
      encoded = ExMCP.Protocol.encode_batch(batch)
  """
  @spec encode_batch(list(map())) :: list(map())
  def encode_batch(messages) when is_list(messages) do
    messages
  end

  @doc """
  Processes a batch of responses.

  Returns a list of parsed messages.
  """
  @spec parse_batch_response(list(map())) :: list(tuple())
  def parse_batch_response(responses) when is_list(responses) do
    Enum.map(responses, &parse_message/1)
  end

  # Message Parsing

  @doc """
  Parses a JSON-RPC message.

  Returns one of:
  - `{:request, method, params, id}` - An incoming request
  - `{:notification, method, params}` - An incoming notification
  - `{:result, result, id}` - A response to our request
  - `{:error, error, id}` - An error response
  - `{:batch, messages}` - A batch of messages
  - `{:error, :invalid_message}` - Invalid message format
  """
  @spec parse_message(String.t() | map() | list()) ::
          {:request, method(), params(), json_rpc_id()}
          | {:notification, method(), params()}
          | {:result, result(), json_rpc_id()}
          | {:error, error(), json_rpc_id()}
          | {:batch, list()}
          | {:error, :invalid_message}
  def parse_message(data) when is_binary(data) do
    case Jason.decode(data) do
      {:ok, decoded} -> parse_message(decoded)
      {:error, _} -> {:error, :invalid_message}
    end
  end

  def parse_message(messages) when is_list(messages) do
    {:batch, messages}
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
  @spec encode_to_string(map() | list(map())) :: {:ok, String.t()} | {:error, any()}
  def encode_to_string(message) do
    Jason.encode(message)
  end

  @doc """
  Generates a unique ID for requests.
  """
  @spec generate_id() :: integer()
  def generate_id do
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
