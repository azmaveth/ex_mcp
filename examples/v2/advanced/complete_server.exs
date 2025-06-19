#!/usr/bin/env elixir

# Complete DSL Server Example
# Demonstrates the v2 DSL with meta blocks and defhandler macro

Mix.install([
  {:ex_mcp, path: Path.expand("../../..", __DIR__)}
])

defmodule DSLExampleServer do
  use ExMCP.ServerV2
  
  # Server metadata
  @name "dsl-example-server"
  @version "2.0.0"
  
  # Define tools using the DSL
  deftool "echo" do
    meta do
      name "Echo Tool"
      description "Echoes back the input message"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        message: %{type: "string", description: "Message to echo"}
      },
      required: ["message"]
    }
  end
  
  deftool "add_numbers" do
    meta do
      name "Number Addition"
      description "Adds two numbers together"
    end
    
    # Alternative: Elixir-native schema syntax
    args do
      field :a, :number, required: true, description: "First number"
      field :b, :number, required: true, description: "Second number"
    end
  end
  
  deftool "get_time" do
    meta do
      name "Time Tool"
      description "Gets the current time in various formats"
    end
    
    args do
      field :format, :string, 
        enum: ["iso8601", "unix", "human"],
        default: "iso8601",
        description: "Time format"
    end
  end
  
  # Define resources using the DSL
  defresource "server://config" do
    meta do
      name "Server Configuration"
      description "Current server configuration and settings"
    end
    
    mime_type "application/json"
    
  end
  
  defresource "server://stats" do
    meta do
      name "Server Statistics"
      description "Runtime statistics and metrics"
    end
    mime_type "application/json"
    annotations %{
      refresh_interval: 5000,
      cacheable: false
    }
    
  end
  
  # Define prompts using the DSL
  defprompt "code_review" do
    meta do
      name "Code Review Assistant"
      description "Helps review code with specific focus areas"
    end
    
    arguments [
      %{name: "code", type: "string", required: true, description: "The code to review"},
      %{name: "language", type: "string", required: true, description: "Programming language"},
      %{name: "focus", type: "string", description: "Specific areas to focus on"}
    ]
    
  end
  
  defprompt "explain_concept" do
    meta do
      name "Concept Explainer"
      description "Explains technical concepts at different levels"
    end
    
    arguments [
      %{name: "concept", type: "string", required: true},
      %{name: "level", type: "string", enum: ["beginner", "intermediate", "expert"], default: "intermediate"}
    ]
    
  end
  
  # Tool handlers using defhandler
  defhandler :tool, "echo", %{"message" => message}, state do
    response = text("Echo: #{message}")
    {:ok, [response], state}
  end
  
  defhandler :tool, "add_numbers", %{"a" => a, "b" => b}, state do
    result = a + b
    response = text("Result: #{result}")
    {:ok, [response], state}
  end
  
  defhandler :tool, "get_time", args, state do
    format = args["format"] || "iso8601"
    
    time_str = case format do
      "unix" -> "#{System.system_time(:second)}"
      "human" -> Calendar.strftime(DateTime.utc_now(), "%B %d, %Y at %I:%M %p UTC")
      _ -> DateTime.utc_now() |> DateTime.to_iso8601()
    end
    
    response = text("Current time: #{time_str}")
    {:ok, [response], state}
  end
  
  # Resource handlers using defhandler
  defhandler :resource, "server://config", _uri, state do
    config = %{
      name: @name,
      version: @version,
      environment: Mix.env(),
      started_at: Map.get(state, :started_at, "unknown")
    }
    
    content = json(config)
    {:ok, [content], state}
  end
  
  defhandler :resource, "server://stats", _uri, state do
    stats = %{
      uptime_seconds: System.system_time(:second) - Map.get(state, :started_at, System.system_time(:second)),
      memory_mb: :erlang.memory(:total) / 1_048_576,
      process_count: length(Process.list())
    }
    
    content = json(stats)
    {:ok, [content], state}
  end
  
  # Prompt handlers using defhandler
  defhandler :prompt, "code_review", args, state do
    code = args["code"]
    language = args["language"]
    focus = args["focus"] || "general review"
    
    messages = [
      system("You are an expert code reviewer focusing on #{focus}."),
      user("Please review this #{language} code:\n\n```#{language}\n#{code}\n```")
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  defhandler :prompt, "explain_concept", args, state do
    concept = args["concept"]
    level = args["level"] || "intermediate"
    
    system_msg = case level do
      "beginner" -> "Explain this concept as if to someone new to programming."
      "expert" -> "Provide a deep technical explanation with advanced details."
      _ -> "Explain this concept clearly for someone with programming experience."
    end
    
    messages = [
      system(system_msg),
      user("Please explain: #{concept}")
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  # Server lifecycle callbacks (optional)
  @impl true
  def init(_args) do
    IO.puts("[DSL Server] Initializing...")
    {:ok, %{started_at: System.system_time(:second)}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    IO.puts("[DSL Server] Client connected!")
    {:ok, %{
      name: @name,
      version: @version,
      capabilities: %{
        tools: %{},
        resources: %{},
        prompts: %{}
      }
    }, state}
  end
end

# Start the server
IO.puts("""
==========================================
ExMCP v2 DSL Server Example
==========================================

This server demonstrates:
- Tool definitions with meta blocks and input_schema/args syntax
- Resource definitions with meta blocks
- Prompt definitions with meta blocks and arguments
- Handler implementations using defhandler macro
- Proper v2 response types (text/json/image/embedded)

Starting server on stdio...
""")

{:ok, _server} = ExMCP.Server.start_link(
  handler: DSLExampleServer,
  transport: :stdio
)

# List what's available
IO.puts("\nAvailable tools:")
IO.puts("  - echo: Echoes messages")
IO.puts("  - add_numbers: Adds two numbers")
IO.puts("  - get_time: Gets current time")

IO.puts("\nAvailable resources:")
IO.puts("  - server://config: Server configuration")
IO.puts("  - server://stats: Runtime statistics")

IO.puts("\nAvailable prompts:")
IO.puts("  - code_review: Code review assistant")
IO.puts("  - explain_concept: Concept explainer")

IO.puts("\nServer running. Connect with a client to interact.")

# Keep running
Process.sleep(:infinity)