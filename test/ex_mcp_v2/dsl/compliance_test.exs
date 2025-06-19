defmodule ExMCP.DSL.ComplianceTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

  # Test server using design-compliant DSL syntax
  defmodule DesignCompliantServer do
    use ExMCP.ServerV2

    deftool "say_hello" do
      description("Says hello to a given name")

      input_schema(%{
        type: "object",
        properties: %{name: %{type: "string"}},
        required: ["name"]
      })
    end

    deftool "calculate_sum" do
      description("Adds two numbers together")

      args do
        field(:a, :number, required: true, description: "First number")
        field(:b, :number, required: true, description: "Second number")
      end
    end

    defresource "config://app/settings" do
      resource_name("Application Settings")
      resource_description("Current application configuration")
      mime_type("application/json")

      annotations(%{
        audience: ["admin"],
        priority: 0.8
      })
    end

    defresource "file://logs/*.log" do
      resource_name("Log Files")
      resource_description("Application log files")
      mime_type("text/plain")
      list_pattern(true)
      subscribable(true)
    end

    defprompt "code_review" do
      prompt_name("Code Review Assistant")
      prompt_description("Reviews code with specific focus areas")

      arguments do
        arg(:code, required: true, description: "Code to review")
        arg(:language, description: "Programming language")
        arg(:focus, description: "Review focus")
      end
    end

    defprompt "simple_greeting" do
      prompt_name("Simple Greeting")
      prompt_description("A simple greeting prompt")
    end

    # Handler implementations
    @impl true
    def handle_tool_call("say_hello", %{"name" => name}, state) do
      result = %{content: [%{"type" => "text", "text" => "Hello, #{name}!"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_tool_call("calculate_sum", %{"a" => a, "b" => b}, state) do
      result = %{content: [%{"type" => "text", "text" => "Result: #{a + b}"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_resource_read("config://app/settings", _uri, state) do
      content = [%{"type" => "text", "text" => "{\"debug\": true, \"port\": 8080}"}]
      {:ok, content, state}
    end

    @impl true
    def handle_resource_read("file://logs/" <> _filename = uri, _full_uri, state) do
      content = [%{"type" => "text", "text" => "Log content for #{uri}"}]
      {:ok, content, state}
    end

    @impl true
    def handle_prompt_get("code_review", args, state) do
      code = Map.get(args, "code", "")
      language = Map.get(args, "language", "unknown")
      focus = Map.get(args, "focus", "general")

      messages = [
        %{
          "role" => "user",
          "content" => "Please review this #{language} code focusing on #{focus}:\n\n#{code}"
        },
        %{
          "role" => "assistant",
          "content" => "I'll review your code with attention to #{focus} aspects."
        }
      ]

      {:ok, %{messages: messages}, state}
    end

    @impl true
    def handle_prompt_get("simple_greeting", _args, state) do
      messages = [
        %{"role" => "user", "content" => "Hello there!"},
        %{"role" => "assistant", "content" => "Hello! How can I help you today?"}
      ]

      {:ok, %{messages: messages}, state}
    end
  end

  # Test server using deprecated syntax (with warnings)
  defmodule DeprecatedSyntaxServer do
    use ExMCP.ServerV2

    deftool "legacy_tool" do
      # Should generate warning
      tool_description("A legacy tool")

      args do
        field(:data, :string, required: true)
      end
    end

    defresource "legacy://resource" do
      # Should generate warning
      resource_name("Legacy Resource")
      # Should generate warning
      resource_description("A legacy resource")
      mime_type("text/plain")
    end

    defprompt "legacy_prompt" do
      # Should generate warning
      prompt_name("Legacy Prompt")
      # Should generate warning
      prompt_description("A legacy prompt")
    end

    # Handler implementations
    @impl true
    def handle_tool_call("legacy_tool", %{"data" => data}, state) do
      result = %{content: [%{"type" => "text", "text" => "Processed: #{data}"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_resource_read("legacy://resource", _uri, state) do
      content = [%{"type" => "text", "text" => "Legacy content"}]
      {:ok, content, state}
    end

    @impl true
    def handle_prompt_get("legacy_prompt", _args, state) do
      messages = [%{"role" => "assistant", "content" => "Legacy response"}]
      {:ok, %{messages: messages}, state}
    end
  end

  describe "Design-compliant DSL syntax" do
    test "compiles tools correctly with compliant syntax" do
      tools = DesignCompliantServer.get_tools()

      assert Map.has_key?(tools, "say_hello")
      assert Map.has_key?(tools, "calculate_sum")

      # Test input_schema tool
      say_hello_tool = tools["say_hello"]
      assert say_hello_tool.name == "say_hello"
      assert say_hello_tool.description == "Says hello to a given name"
      assert say_hello_tool.input_schema["type"] == "object"
      assert say_hello_tool.input_schema["properties"]["name"]["type"] == "string"
      assert say_hello_tool.input_schema["required"] == ["name"]

      # Test args-based tool
      sum_tool = tools["calculate_sum"]
      assert sum_tool.name == "calculate_sum"
      assert sum_tool.description == "Adds two numbers together"
      assert sum_tool.input_schema["type"] == "object"
      assert sum_tool.input_schema["required"] == ["a", "b"]
    end

    test "compiles resources correctly with compliant syntax" do
      resources = DesignCompliantServer.get_resources()

      assert Map.has_key?(resources, "config://app/settings")
      assert Map.has_key?(resources, "file://logs/*.log")

      # Test config resource
      config_resource = resources["config://app/settings"]
      assert config_resource.name == "Application Settings"
      assert config_resource.description == "Current application configuration"
      assert config_resource.mime_type == "application/json"
      assert config_resource.annotations.audience == ["admin"]
      refute config_resource.subscribable

      # Test log resource
      log_resource = resources["file://logs/*.log"]
      assert log_resource.name == "Log Files"
      assert log_resource.description == "Application log files"
      assert log_resource.list_pattern
      assert log_resource.subscribable
    end

    test "compiles prompts correctly with compliant syntax" do
      prompts = DesignCompliantServer.get_prompts()

      assert Map.has_key?(prompts, "code_review")
      assert Map.has_key?(prompts, "simple_greeting")

      # Test review prompt
      review_prompt = prompts["code_review"]
      assert review_prompt.name == "code_review"
      assert review_prompt.display_name == "Code Review Assistant"
      assert review_prompt.description == "Reviews code with specific focus areas"
      assert length(review_prompt.arguments) == 3

      # Test simple prompt
      greeting_prompt = prompts["simple_greeting"]
      assert greeting_prompt.display_name == "Simple Greeting"
      assert greeting_prompt.description == "A simple greeting prompt"
      assert greeting_prompt.arguments == []
    end

    test "auto-detects capabilities correctly" do
      capabilities = DesignCompliantServer.get_capabilities()

      assert Map.has_key?(capabilities, "tools")
      assert capabilities["tools"]["listChanged"] == true

      assert Map.has_key?(capabilities, "resources")
      assert capabilities["resources"]["subscribe"] == true
      assert capabilities["resources"]["listChanged"] == true

      assert Map.has_key?(capabilities, "prompts")
      assert capabilities["prompts"]["listChanged"] == true
    end
  end

  describe "Deprecated syntax with warnings" do
    test "tool_description generates deprecation warning" do
      log_output =
        capture_log(fn ->
          # Force compilation by accessing tools
          _tools = DeprecatedSyntaxServer.get_tools()
        end)

      assert log_output =~ "tool_description/1 is deprecated. Use description/1 instead."
    end

    test "resource_name and resource_description generate deprecation warnings" do
      log_output =
        capture_log(fn ->
          # Force compilation by accessing resources
          _resources = DeprecatedSyntaxServer.get_resources()
        end)

      assert log_output =~ "resource_name/1 is deprecated. Use name/1 instead."
      assert log_output =~ "resource_description/1 is deprecated. Use description/1 instead."
    end

    test "prompt_name and prompt_description generate deprecation warnings" do
      log_output =
        capture_log(fn ->
          # Force compilation by accessing prompts
          _prompts = DeprecatedSyntaxServer.get_prompts()
        end)

      assert log_output =~ "prompt_name/1 is deprecated. Use name/1 instead."
      assert log_output =~ "prompt_description/1 is deprecated. Use description/1 instead."
    end

    test "deprecated syntax still works functionally" do
      tools = DeprecatedSyntaxServer.get_tools()
      resources = DeprecatedSyntaxServer.get_resources()
      prompts = DeprecatedSyntaxServer.get_prompts()

      # Tools work with deprecated syntax
      assert Map.has_key?(tools, "legacy_tool")
      legacy_tool = tools["legacy_tool"]
      assert legacy_tool.description == "A legacy tool"

      # Resources work with deprecated syntax
      assert Map.has_key?(resources, "legacy://resource")
      legacy_resource = resources["legacy://resource"]
      assert legacy_resource.name == "Legacy Resource"
      assert legacy_resource.description == "A legacy resource"

      # Prompts work with deprecated syntax
      assert Map.has_key?(prompts, "legacy_prompt")
      legacy_prompt = prompts["legacy_prompt"]
      assert legacy_prompt.display_name == "Legacy Prompt"
      assert legacy_prompt.description == "A legacy prompt"
    end
  end

  describe "Handler execution with compliant DSL" do
    setup do
      {:ok, pid} = DesignCompliantServer.start_link()
      %{server: pid}
    end

    test "executes tools defined with compliant syntax", %{server: _server} do
      # Test input_schema tool
      {:ok, result, _state} =
        DesignCompliantServer.handle_tool_call("say_hello", %{"name" => "World"}, %{})

      assert result.content == [%{"type" => "text", "text" => "Hello, World!"}]

      # Test args-based tool
      {:ok, result, _state} =
        DesignCompliantServer.handle_tool_call("calculate_sum", %{"a" => 5, "b" => 3}, %{})

      assert result.content == [%{"type" => "text", "text" => "Result: 8"}]
    end

    test "executes resources defined with compliant syntax", %{server: _server} do
      {:ok, content, _state} =
        DesignCompliantServer.handle_resource_read(
          "config://app/settings",
          "config://app/settings",
          %{}
        )

      assert length(content) == 1
      [json_content] = content
      assert json_content["type"] == "text"
      assert String.contains?(json_content["text"], "debug")
    end

    test "executes prompts defined with compliant syntax", %{server: _server} do
      args = %{"code" => "def hello, do: :world", "language" => "elixir", "focus" => "syntax"}
      {:ok, result, _state} = DesignCompliantServer.handle_prompt_get("code_review", args, %{})

      assert length(result.messages) == 2
      [user_msg, assistant_msg] = result.messages
      assert user_msg["role"] == "user"
      assert assistant_msg["role"] == "assistant"
      assert String.contains?(user_msg["content"], "elixir")
    end
  end
end
