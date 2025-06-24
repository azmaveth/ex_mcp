defmodule ExMCP.Integration.ServerDSLTest do
  use ExUnit.Case, async: true

  # Test server using the full DSL
  defmodule TestServer do
    use ExMCP.Server

    deftool "calculate_sum" do
      meta do
        description("Adds two numbers together")
      end

      input_schema(%{
        type: "object",
        properties: %{
          a: %{type: "number", description: "First number"},
          b: %{type: "number", description: "Second number"}
        },
        required: ["a", "b"]
      })
    end

    deftool "process_data" do
      meta do
        description("Processes data with options")
      end

      input_schema(%{
        type: "object",
        properties: %{
          data: %{type: "string"},
          options: %{
            type: "object",
            properties: %{
              format: %{type: "string", default: "json"},
              validate: %{type: "boolean", default: true}
            }
          }
        },
        required: ["data"]
      })
    end

    defresource "config://app/settings" do
      meta do
        name("Application Settings")
        description("Current application configuration")
      end

      mime_type("application/json")

      annotations(%{
        audience: ["admin"],
        priority: 0.8
      })
    end

    defresource "file://logs/*.log" do
      meta do
        name("Log Files")
        description("Application log files")
      end

      mime_type("text/plain")
      list_pattern(true)
      subscribable(true)
    end

    defprompt "code_review" do
      meta do
        name("Code Review Assistant")
        description("Reviews code with specific focus areas")
      end

      arguments do
        arg(:code, required: true, description: "Code to review")
        arg(:language, description: "Programming language")
        arg(:focus, description: "Review focus")
      end
    end

    defprompt "simple_greeting" do
      meta do
        name("Simple Greeting")
        description("A simple greeting prompt")
      end
    end

    # Handler implementations
    @impl true
    def handle_tool_call("calculate_sum", %{"a" => a, "b" => b}, state) do
      result = %{content: [text("Result: #{a + b}")]}
      {:ok, result, state}
    end

    @impl true
    def handle_tool_call("process_data", %{"data" => data, "options" => options}, state) do
      format = Map.get(options, "format", "json")
      result = %{content: [text("Processed #{data} as #{format}")]}
      {:ok, result, state}
    end

    @impl true
    def handle_resource_read("config://app/settings", _uri, state) do
      content = [json(%{debug: true, port: 8080})]
      {:ok, content, state}
    end

    @impl true
    def handle_resource_read("file://logs/" <> _filename = uri, _full_uri, state) do
      content = [text("Log content for #{uri}")]
      {:ok, content, state}
    end

    @impl true
    def handle_prompt_get("code_review", args, state) do
      code = Map.get(args, "code", "")
      language = Map.get(args, "language", "unknown")
      focus = Map.get(args, "focus", "general")

      messages = [
        user("Please review this #{language} code focusing on #{focus}:\n\n#{code}"),
        assistant("I'll review your code with attention to #{focus} aspects.")
      ]

      {:ok, %{messages: messages}, state}
    end

    @impl true
    def handle_prompt_get("simple_greeting", _args, state) do
      messages = [
        user("Hello there!"),
        assistant("Hello! How can I help you today?")
      ]

      {:ok, %{messages: messages}, state}
    end
  end

  describe "DSL compilation" do
    test "compiles tools correctly" do
      tools = TestServer.get_tools()

      assert Map.has_key?(tools, "calculate_sum")
      assert Map.has_key?(tools, "process_data")

      sum_tool = tools["calculate_sum"]
      assert sum_tool.name == "calculate_sum"
      assert sum_tool.description == "Adds two numbers together"
      assert sum_tool.input_schema["type"] == "object"
      assert sum_tool.input_schema["required"] == ["a", "b"]

      process_tool = tools["process_data"]
      options_schema = process_tool.input_schema["properties"]["options"]
      assert options_schema["type"] == "object"
      assert options_schema["properties"]["format"]["default"] == "json"
    end

    test "compiles resources correctly" do
      resources = TestServer.get_resources()

      assert Map.has_key?(resources, "config://app/settings")
      assert Map.has_key?(resources, "file://logs/*.log")

      config_resource = resources["config://app/settings"]
      assert config_resource.name == "Application Settings"
      assert config_resource.mime_type == "application/json"
      assert config_resource.annotations.audience == ["admin"]
      refute config_resource.subscribable

      log_resource = resources["file://logs/*.log"]
      assert log_resource.list_pattern
      assert log_resource.subscribable
    end

    test "compiles prompts correctly" do
      prompts = TestServer.get_prompts()

      assert Map.has_key?(prompts, "code_review")
      assert Map.has_key?(prompts, "simple_greeting")

      review_prompt = prompts["code_review"]
      assert review_prompt.name == "code_review"
      assert review_prompt.display_name == "Code Review Assistant"
      assert length(review_prompt.arguments) == 3

      greeting_prompt = prompts["simple_greeting"]
      assert greeting_prompt.arguments == []
    end

    test "auto-detects capabilities" do
      capabilities = TestServer.get_capabilities()

      assert Map.has_key?(capabilities, "tools")
      assert capabilities["tools"]["listChanged"] == true

      assert Map.has_key?(capabilities, "resources")
      assert capabilities["resources"]["subscribe"] == true
      assert capabilities["resources"]["listChanged"] == true

      assert Map.has_key?(capabilities, "prompts")
      assert capabilities["prompts"]["listChanged"] == true
    end
  end

  describe "handler execution" do
    setup do
      {:ok, pid} = TestServer.start_link()
      %{server: pid}
    end

    test "executes tool handlers", %{server: _server} do
      # Test calculate_sum
      {:ok, result, _state} =
        TestServer.handle_tool_call("calculate_sum", %{"a" => 5, "b" => 3}, %{})

      assert result.content == [%{"type" => "text", "text" => "Result: 8"}]
    end

    test "executes resource handlers", %{server: _server} do
      # Test config resource
      {:ok, content, _state} =
        TestServer.handle_resource_read("config://app/settings", "config://app/settings", %{})

      assert length(content) == 1
      [json_content] = content
      assert json_content["type"] == "text"
      assert String.contains?(json_content["text"], "debug")
    end

    test "executes prompt handlers", %{server: _server} do
      args = %{"code" => "def hello, do: :world", "language" => "elixir", "focus" => "syntax"}
      {:ok, result, _state} = TestServer.handle_prompt_get("code_review", args, %{})

      assert length(result.messages) == 2
      [user_msg, assistant_msg] = result.messages
      assert user_msg["role"] == "user"
      assert assistant_msg["role"] == "assistant"
      assert String.contains?(user_msg["content"], "elixir")
    end
  end
end
