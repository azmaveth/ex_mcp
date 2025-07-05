defmodule ExMCP.TestHelpers.RefactoredTestServer do
  @moduledoc """
  Test server for integration testing.

  Note: This module may generate compiler warnings about unreachable clauses
  due to the DSL generating comprehensive pattern matches. These warnings are
  benign and can be ignored.
  """
  use ExMCP.Server,
    name: "Refactored Test Server",
    version: "1.0.0"

  deftool "test_tool" do
    meta do
      description("Test tool")

      input_schema(%{
        "type" => "object",
        "properties" => %{"index" => %{"type" => "number"}},
        "required" => []
      })
    end
  end

  defresource "test://resource" do
    meta do
      name("Test resource")
      description("A test resource for integration testing")
    end
  end

  defprompt "test_prompt" do
    meta do
      name("Test prompt")
      description("A test prompt for integration testing")
    end
  end

  @impl true
  def handle_tool_call("test_tool", _args, state) do
    # Add a delay to allow tests to observe pending state
    Process.sleep(100)
    result = %{content: [%{"type" => "text", "text" => "Tool executed"}]}
    {:ok, result, state}
  end

  @impl true
  def handle_resource_read("test://resource", _uri, state) do
    content = %{"text" => "Resource content"}
    {:ok, content, state}
  end

  @impl true
  def handle_initialize(params, state) do
    client_version = Map.get(params, "protocolVersion", "2025-06-18")

    result = %{
      "protocolVersion" => client_version,
      "serverInfo" => %{"name" => "Refactored Test Server", "version" => "1.0.0"},
      "capabilities" => get_capabilities()
    }

    new_state = Map.put(state, :protocol_version, client_version)
    {:ok, result, new_state}
  end

  @impl true
  def handle_prompt_get("test_prompt", _args, state) do
    result = %{
      messages: [
        %{role: "user", content: "Test prompt"}
      ]
    }

    {:ok, result, state}
  end
end
