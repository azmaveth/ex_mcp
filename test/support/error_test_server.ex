defmodule ExMCP.TestHelpers.ErrorTestServer do
  @moduledoc false
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
  def handle_tool_call("protocol_error", _args, state) do
    # Return an error to simulate protocol error handling
    {:error, "Invalid parameters", state}
  end

  @impl true
  def handle_tool_call("transport_error", _args, state) do
    # Return an error to simulate transport error handling
    {:error, "Connection lost", state}
  end

  @impl true
  def handle_tool_call("tool_error", _args, state) do
    # Return an error to simulate tool error handling
    {:error, "Tool execution failed", state}
  end

  @impl true
  def handle_tool_call("resource_error", _args, state) do
    # Return an error to simulate resource error handling
    {:error, "Permission denied", state}
  end

  @impl true
  def handle_tool_call("validation_error", _args, state) do
    # Return an error to simulate validation error handling
    {:error, "Must be positive", state}
  end

  @impl true
  def handle_tool_call("generic_error", _args, state) do
    # Return an error to simulate generic error handling
    {:error, "Something went wrong", state}
  end

  @impl true
  def handle_resource_read("error://test", _uri, state) do
    {:error, "Resource not found", state}
  end
end
