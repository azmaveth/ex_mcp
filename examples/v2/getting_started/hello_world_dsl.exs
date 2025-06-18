#!/usr/bin/env elixir

# ExMCP v2 Hello World Example with DSL
# 
# This example demonstrates the new developer-friendly DSL for defining
# MCP servers with tools, resources, and prompts.

Mix.install([
  {:ex_mcp, path: "../../.."}
])

defmodule HelloWorldServer do
  @moduledoc """
  A simple MCP server demonstrating the new DSL syntax.
  
  Compare this to the legacy hello_world.exs - this is much more concise
  and expressive while providing the same functionality.
  """
  
  use ExMCP.ServerV2

  # Tool definitions using the new DSL
  deftool "say_hello" do
    tool_description "Says hello to someone"
    
    args do
      field :name, :string, required: true, description: "Name of the person to greet"
      field :formal, :boolean, default: false, description: "Use formal greeting"
    end
  end

  deftool "calculate" do
    tool_description "Performs basic calculations"
    
    args do
      field :operation, :string, required: true, description: "Operation to perform"
      field :numbers, {:array, :number}, required: true, description: "Numbers to operate on"
      field :precision, :integer, min: 0, max: 10, default: 2, description: "Decimal precision"
    end
  end

  # Resource definitions
  defresource "config://server/info" do
    resource_name "Server Information"
    resource_description "Basic information about this server"
    mime_type "application/json"
  end

  defresource "memory://greetings/*" do
    resource_name "Greeting History"
    resource_description "History of greetings sent"
    mime_type "text/plain"
    list_pattern true
    subscribable true
  end

  # Prompt definitions
  defprompt "code_review" do
    prompt_name "Code Review Assistant"
    prompt_description "Helps review code with specific focus areas"
    
    arguments do
      arg :code, required: true, description: "Code to review"
      arg :language, description: "Programming language"
      arg :focus, description: "Areas to focus on (security, performance, etc.)"
    end
  end

  defprompt "creative_writing" do
    prompt_name "Creative Writing Helper"
    prompt_description "Assists with creative writing tasks"
    
    arguments do
      arg :genre, description: "Writing genre"
      arg :tone, description: "Desired tone"
    end
  end

  # Tool handlers
  @impl true
  def handle_tool_call("say_hello", %{"name" => name, "formal" => formal}, state) do
    greeting = if formal do
      "Good day, #{name}. I hope you are well."
    else
      "Hello, #{name}! How are you doing?"
    end
    
    # Store greeting in memory for demonstration
    :ets.insert(:greetings, {System.system_time(), name, greeting})
    
    result = %{content: [text(greeting)]}
    {:ok, result, state}
  end

  def handle_tool_call("say_hello", %{"name" => name}, state) do
    # Handle case where formal parameter is not provided
    handle_tool_call("say_hello", %{"name" => name, "formal" => false}, state)
  end

  @impl true
  def handle_tool_call("calculate", %{"operation" => op, "numbers" => numbers, "precision" => precision}, state) do
    result = case op do
      "add" -> Enum.sum(numbers)
      "multiply" -> Enum.reduce(numbers, 1, &*/2)
      "average" -> Enum.sum(numbers) / length(numbers)
      "max" -> Enum.max(numbers)
      "min" -> Enum.min(numbers)
      _ -> {:error, "Unknown operation: #{op}"}
    end
    
    case result do
      {:error, message} ->
        error_result = %{content: [text(message)], is_error: true}
        {:ok, error_result, state}
      
      value when is_number(value) ->
        formatted = Float.round(value * 1.0, precision)
        success_result = %{content: [text("Result: #{formatted}")]}
        {:ok, success_result, state}
    end
  end

  def handle_tool_call("calculate", %{"operation" => op, "numbers" => numbers}, state) do
    # Handle case where precision is not provided
    handle_tool_call("calculate", %{"operation" => op, "numbers" => numbers, "precision" => 2}, state)
  end

  # Resource handlers
  @impl true
  def handle_resource_read("config://server/info", _uri, state) do
    info = %{
      name: "HelloWorldServer",
      version: "2.0.0",
      capabilities: get_capabilities(),
      uptime: System.system_time(:second) - elem(state[:start_time] || {0, 0, 0}, 0)
    }
    
    content = [json(info)]
    {:ok, content, state}
  end

  @impl true
  def handle_resource_read("memory://greetings/" <> id, _uri, state) do
    case :ets.lookup(:greetings, String.to_integer(id)) do
      [{timestamp, name, greeting}] ->
        content = [text("#{DateTime.from_unix!(timestamp, :native)} - #{name}: #{greeting}")]
        {:ok, content, state}
      [] ->
        {:error, "Greeting not found", state}
    end
  end

  @impl true
  def handle_resource_list(state) do
    # List all greetings
    greetings = :ets.tab2list(:greetings)
    
    resources = Enum.map(greetings, fn {timestamp, name, _greeting} ->
      %{
        uri: "memory://greetings/#{timestamp}",
        name: "Greeting to #{name}",
        description: "Greeting sent at #{DateTime.from_unix!(timestamp, :native)}",
        mimeType: "text/plain"
      }
    end)
    
    # Add server info resource
    all_resources = [
      %{
        uri: "config://server/info",
        name: "Server Information", 
        description: "Basic information about this server",
        mimeType: "application/json"
      } | resources
    ]
    
    {:ok, all_resources, state}
  end

  # Prompt handlers
  @impl true
  def handle_prompt_get("code_review", args, state) do
    code = Map.get(args, "code", "")
    language = Map.get(args, "language", "unknown")
    focus = Map.get(args, "focus", "general quality")
    
    messages = [
      system("You are an expert code reviewer with deep knowledge of software engineering best practices."),
      user("Please review this #{language} code, focusing on #{focus}:\n\n```#{language}\n#{code}\n```"),
      assistant("I'll carefully review your #{language} code with attention to #{focus}. Let me analyze it step by step...")
    ]
    
    {:ok, %{messages: messages}, state}
  end

  @impl true
  def handle_prompt_get("creative_writing", args, state) do
    genre = Map.get(args, "genre", "general fiction")
    tone = Map.get(args, "tone", "engaging")
    
    messages = [
      system("You are a creative writing assistant specializing in #{genre} with a #{tone} tone."),
      user("I need help with my #{genre} writing. Please assist me in developing ideas and improving my style."),
      assistant("I'd be delighted to help you with your #{genre} writing! I'll focus on creating a #{tone} tone throughout our collaboration...")
    ]
    
    {:ok, %{messages: messages}, state}
  end

  # Server initialization
  @impl true
  def init(_args) do
    # Create ETS table for storing greetings
    :ets.new(:greetings, [:named_table, :public, :ordered_set])
    
    state = %{
      start_time: :erlang.timestamp()
    }
    
    {:ok, state}
  end
end

# Demo script
defmodule Demo do
  def run do
    IO.puts("🚀 Starting ExMCP v2 Hello World Server with DSL...")
    IO.puts("📊 Server capabilities: #{inspect(HelloWorldServer.get_capabilities())}")
    IO.puts("🔧 Defined tools: #{Map.keys(HelloWorldServer.get_tools()) |> Enum.join(", ")}")
    IO.puts("📁 Defined resources: #{Map.keys(HelloWorldServer.get_resources()) |> Enum.join(", ")}")
    IO.puts("💭 Defined prompts: #{Map.keys(HelloWorldServer.get_prompts()) |> Enum.join(", ")}")
    IO.puts("")
    
    # Start the server
    {:ok, pid} = HelloWorldServer.start_link()
    
    IO.puts("✅ Server started! (PID: #{inspect(pid)})")
    IO.puts("")
    IO.puts("🎯 Demo: Calling tools...")
    
    # Demo tool calls
    {:ok, result1, _} = HelloWorldServer.handle_tool_call(
      "say_hello", 
      %{"name" => "Alice", "formal" => false}, 
      %{}
    )
    IO.puts("👋 Informal greeting: #{hd(result1.content)["text"]}")
    
    {:ok, result2, _} = HelloWorldServer.handle_tool_call(
      "say_hello", 
      %{"name" => "Dr. Smith", "formal" => true}, 
      %{}
    )
    IO.puts("🎩 Formal greeting: #{hd(result2.content)["text"]}")
    
    {:ok, result3, _} = HelloWorldServer.handle_tool_call(
      "calculate", 
      %{"operation" => "add", "numbers" => [10, 20, 30], "precision" => 1}, 
      %{}
    )
    IO.puts("🧮 Calculation: #{hd(result3.content)["text"]}")
    
    IO.puts("")
    IO.puts("📄 Demo: Reading resources...")
    
    # Demo resource reads
    {:ok, content, _} = HelloWorldServer.handle_resource_read("config://server/info", "", %{start_time: :erlang.timestamp()})
    server_info = Jason.decode!(hd(content)["text"])
    IO.puts("ℹ️  Server info: #{server_info["name"]} v#{server_info["version"]}")
    
    IO.puts("")
    IO.puts("💭 Demo: Getting prompts...")
    
    # Demo prompt calls
    {:ok, prompt_result, _} = HelloWorldServer.handle_prompt_get(
      "code_review", 
      %{"code" => "def hello, do: :world", "language" => "elixir", "focus" => "style"}, 
      %{}
    )
    IO.puts("🔍 Code review prompt has #{length(prompt_result.messages)} messages")
    
    IO.puts("")
    IO.puts("🎉 Demo completed! The DSL makes MCP servers much easier to write.")
    IO.puts("📝 Compare this to the legacy examples - much more concise and expressive!")
    
    # Keep server running briefly
    Process.sleep(1000)
    
    IO.puts("🛑 Shutting down server...")
    GenServer.stop(pid)
  end
end

# Run the demo
Demo.run()