defmodule ExMCP.TestServer do
  @moduledoc """
  Test MCP server for integration testing.

  Provides a simple server with basic tools and resources for testing
  the convenience functions with real network connections.
  """

  use ExMCP.Server

  # Define test tools
  deftool "echo" do
    meta do
      description("Echoes the input message")

      input_schema(%{
        type: "object",
        properties: %{message: %{type: "string"}},
        required: ["message"]
      })
    end
  end

  deftool "add" do
    meta do
      description("Adds two numbers")

      input_schema(%{
        type: "object",
        properties: %{
          a: %{type: "number"},
          b: %{type: "number"}
        },
        required: ["a", "b"]
      })
    end
  end

  deftool "greet" do
    meta do
      description("Greets a person by name")

      input_schema(%{
        type: "object",
        properties: %{name: %{type: "string"}},
        required: ["name"]
      })
    end
  end

  # Define test resources
  defresource "test://config" do
    meta do
      name("Test Configuration")
      description("Test configuration data")
      mime_type("application/json")
    end
  end

  defresource "test://data.txt" do
    meta do
      name("Test Data")
      description("Simple text data")
      mime_type("text/plain")
    end
  end

  defresource "test://logs/*" do
    meta do
      name("Test Logs")
      description("Test log files")
      mime_type("text/plain")
      list_pattern(true)
    end
  end

  # Define test prompts
  defprompt "greeting" do
    meta do
      name("Greeting Template")
      description("A template for greetings")

      arguments do
        arg(:style, description: "Greeting style (formal/casual)")
        arg(:name, required: true, description: "Person to greet")
      end
    end
  end

  defprompt "code_review" do
    meta do
      name("Code Review")
      description("Reviews code snippets")

      arguments do
        arg(:code, required: true, description: "Code to review")
        arg(:language, description: "Programming language")
      end
    end
  end

  # Tool implementations
  @impl true
  def handle_tool_call("echo", %{"message" => message}, state) do
    result = %{content: [text("Echo: #{message}")]}
    {:ok, result, state}
  end

  @impl true
  def handle_tool_call("add", %{"a" => a, "b" => b}, state) do
    sum = a + b
    result = %{content: [text("#{a} + #{b} = #{sum}")]}
    {:ok, result, state}
  end

  @impl true
  def handle_tool_call("greet", %{"name" => name}, state) do
    result = %{content: [text("Hello, #{name}!")]}
    {:ok, result, state}
  end

  # Handle unknown tools
  def handle_tool_call(_name, _args, state) do
    {:error, "Unknown tool", state}
  end

  # Resource implementations
  @impl true
  def handle_resource_read("test://config", _uri, state) do
    config =
      json(%{
        environment: "test",
        debug: true,
        port: 8080,
        features: ["tools", "resources", "prompts"]
      })

    {:ok, [config], state}
  end

  @impl true
  def handle_resource_read("test://data.txt", _uri, state) do
    content = [text("This is test data from the test server.\nLine 2 of test data.")]
    {:ok, content, state}
  end

  @impl true
  def handle_resource_read("test://logs/" <> filename, _uri, state) do
    content = [text("Log file: #{filename}\n[INFO] Test log entry 1\n[WARN] Test log entry 2")]
    {:ok, content, state}
  end

  # Handle unknown resources
  def handle_resource_read(_uri, _original_uri, state) do
    {:error, "Resource not found", state}
  end

  # Prompt implementations
  @impl true
  def handle_prompt_get("greeting", args, state) do
    name = Map.get(args, "name", "friend")
    style = Map.get(args, "style", "casual")

    greeting =
      case style do
        "formal" -> "Good day, #{name}. I hope this message finds you well."
        _ -> "Hey #{name}! How's it going?"
      end

    messages = [
      user("Please greet #{name} in a #{style} style"),
      assistant(greeting)
    ]

    {:ok, %{messages: messages}, state}
  end

  @impl true
  def handle_prompt_get("code_review", args, state) do
    code = Map.get(args, "code", "")
    language = Map.get(args, "language", "unknown")

    messages = [
      user("Please review this #{language} code:\n\n#{code}"),
      assistant("I'll review your #{language} code for style, correctness, and best practices.")
    ]

    {:ok, %{messages: messages}, state}
  end

  # List implementations
  @impl true
  def handle_resource_list(state) do
    resources = [
      %{uri: "test://config", name: "Test Configuration"},
      %{uri: "test://data.txt", name: "Test Data"},
      %{uri: "test://logs/app.log", name: "Application Log"},
      %{uri: "test://logs/error.log", name: "Error Log"}
    ]

    {:ok, resources, state}
  end

  @impl true
  def handle_prompt_list(state) do
    prompts = [
      %{name: "greeting", description: "A template for greetings"},
      %{name: "code_review", description: "Reviews code snippets"}
    ]

    {:ok, prompts, state}
  end
end
