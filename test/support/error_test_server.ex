defmodule ExMCP.TestHelpers.ErrorTestServer do
  @moduledoc """
  Test server that always returns errors for testing error handling paths.

  Note: This module generates compiler warnings about unreachable clauses.
  This is expected because the DSL generates both success and error handling
  patterns, but this test server only ever returns errors. The warnings are
  benign and can be ignored.
  """
  use ExMCP.Server

  deftool "protocol_error" do
    meta do
      description("Raises a protocol error")
      input_schema(%{"type" => "object", "properties" => %{}})
    end
  end

  deftool "transport_error" do
    meta do
      description("Raises a transport error")
      input_schema(%{"type" => "object", "properties" => %{}})
    end
  end

  deftool "tool_error" do
    meta do
      description("Raises a tool error")
      input_schema(%{"type" => "object", "properties" => %{}})
    end
  end

  deftool "resource_error" do
    meta do
      description("Raises a resource error")
      input_schema(%{"type" => "object", "properties" => %{}})
    end
  end

  deftool "validation_error" do
    meta do
      description("Raises a validation error")
      input_schema(%{"type" => "object", "properties" => %{}})
    end
  end

  deftool "generic_error" do
    meta do
      description("Raises a generic error")
      input_schema(%{"type" => "object", "properties" => %{}})
    end
  end

  defresource "error://test" do
    meta do
      name("Test resource that errors")
      description("A resource that always raises an error for testing")
    end
  end

  @impl true
  def handle_tool_call(nil, _args, state) do
    # Handle nil tool name case
    {:error, "Missing tool name", state}
  end

  @impl true
  def handle_tool_call("protocol_error", _args, state) do
    # Return a protocol error with specific structure expected by tests
    error = %ExMCP.Error.ProtocolError{
      code: -32602,
      message: "MCP Protocol Error (-32602): Invalid parameters",
      data: %{"field" => "name"}
    }

    {:error, error, state}
  end

  @impl true
  def handle_tool_call("transport_error", _args, state) do
    # Return a transport error with specific structure expected by tests
    error = %ExMCP.Error.TransportError{
      transport: "stdio",
      reason: :connection_lost,
      details: %{"attempts" => 3}
    }

    {:error, error, state}
  end

  @impl true
  def handle_tool_call("tool_error", _args, state) do
    # Return a tool error with specific structure expected by tests
    error = %ExMCP.Error.ToolError{
      tool_name: "test_tool",
      reason: "Tool execution failed",
      arguments: nil
    }

    {:error, error, state}
  end

  @impl true
  def handle_tool_call("resource_error", _args, state) do
    # Return a resource error with specific structure expected by tests
    error = %ExMCP.Error.ResourceError{
      uri: "file:///test.txt",
      operation: :read,
      reason: "Permission denied"
    }

    {:error, error, state}
  end

  @impl true
  def handle_tool_call("validation_error", _args, state) do
    # Return a validation error with specific structure expected by tests
    error = %ExMCP.Error.ValidationError{
      field: "validation_error",
      value: nil,
      reason: "Must be positive"
    }

    {:error, error, state}
  end

  @impl true
  def handle_tool_call("generic_error", _args, state) do
    # Return a generic error
    {:error, "Something went wrong", state}
  end

  @impl true
  def handle_resource_read("error://test", _uri, state) do
    {:error, "Resource not found", state}
  end

  # Add handlers that would never be called but satisfy pattern matching
  @impl true
  def handle_prompt_get(_prompt_name, _arguments, state) do
    {:error, :prompt_not_implemented, state}
  end

  @impl true
  def handle_resource_subscribe(_uri, state) do
    {:ok, state}
  end

  @impl true
  def handle_resource_unsubscribe(_uri, state) do
    {:ok, state}
  end
end
