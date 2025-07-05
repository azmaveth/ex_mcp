defmodule ExMCP.ErrorHelpers do
  @moduledoc """
  Helper functions for creating ExMCP.Error structs.
  This module provides backward compatibility for tests.
  """

  alias ExMCP.Error

  @doc """
  Creates an error struct from a JSON-RPC error response.
  """
  def from_json_rpc_error(json_error, opts \\ []) do
    %Error{
      code: Map.get(json_error, "code"),
      message: Map.get(json_error, "message", "Unknown error"),
      data: Map.get(json_error, "data"),
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  @doc """
  Creates a parse error.
  """
  def parse_error(details \\ "", opts \\ []) do
    message =
      if details == "" do
        "Parse error"
      else
        "Parse error: #{details}"
      end

    %Error{
      code: -32700,
      message: message,
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  @doc """
  Creates an invalid request error.
  """
  def invalid_request(details, opts \\ []) do
    %Error{
      code: -32600,
      message: "Invalid request: #{details}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  @doc """
  Creates a method not found error.
  """
  def method_not_found(method, opts \\ []) do
    %Error{
      code: -32601,
      message: "Method not found: #{method}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  @doc """
  Creates an invalid params error.
  """
  def invalid_params(details, opts \\ []) do
    %Error{
      code: -32602,
      message: "Invalid params: #{details}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  @doc """
  Creates an internal error.
  """
  def internal_error(details, opts \\ []) do
    %Error.ProtocolError{
      code: -32603,
      message: "Internal error: #{details}",
      data: Keyword.get(opts, :data)
    }
  end

  @doc """
  Creates a tool error for the Error struct format.
  """
  def tool_error(details, tool_name \\ nil, opts \\ []) do
    message =
      if tool_name do
        "Tool error in '#{tool_name}': #{details}"
      else
        "Tool error: #{details}"
      end

    data =
      case Keyword.get(opts, :data) do
        nil when tool_name != nil -> %{tool_name: tool_name}
        nil -> nil
        custom_data -> custom_data
      end

    %Error{
      code: -32000,
      message: message,
      data: data,
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  @doc """
  Creates a resource error for the Error struct format.
  """
  def resource_error(details, uri, opts \\ []) do
    data =
      case Keyword.get(opts, :data) do
        nil -> %{resource_uri: uri}
        custom_data -> custom_data
      end

    %Error{
      code: -32001,
      message: "Resource error for '#{uri}': #{details}",
      data: data,
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  @doc """
  Creates a prompt error for the Error struct format.
  """
  def prompt_error(details, prompt_name, opts \\ []) do
    data =
      case Keyword.get(opts, :data) do
        nil -> %{prompt_name: prompt_name}
        custom_data -> custom_data
      end

    %Error{
      code: -32002,
      message: "Prompt error in '#{prompt_name}': #{details}",
      data: data,
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end

  @doc """
  Creates a connection error that returns a main Error struct.
  """
  def connection_error(details) do
    %Error{
      code: :connection_error,
      message: "Connection error: #{details}",
      data: nil,
      request_id: nil,
      __exception__: true
    }
  end

  def connection_error(details, opts) when is_list(opts) do
    %Error{
      code: :connection_error,
      message: "Connection error: #{details}",
      data: Keyword.get(opts, :data),
      request_id: Keyword.get(opts, :request_id),
      __exception__: true
    }
  end
end
