#!/usr/bin/env elixir

# HTTP Server with SSE and Hello World Prompt
# 
# This server demonstrates HTTP transport with Server-Sent Events
# It provides a prompt for generating greetings

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)},
  {:plug_cowboy, "~> 2.6"},
  {:jason, "~> 1.4"},
  {:ex_json_schema, "~> 0.10"},
  {:html_entities, "~> 0.5"}
], verbose: false)

# Configure logging for cleaner demo output
Logger.configure(level: :info)

defmodule HttpSseHelloServer do
  use ExMCP.Server
  
  @impl true
  def init(_args) do
    {:ok, %{
      prompt_uses: 0,
      subscriptions: []
    }}
  end
  
  # Define a hello world prompt
  defprompt "hello_generator" do
    meta do
      name "Hello Message Generator"
      description "Generates personalized hello messages"
    end
    
    arguments do
      arg :recipient, required: true, description: "Who to greet"
      arg :style, description: "Style of greeting (formal/casual/excited)"
      arg :include_time, description: "Include current time in greeting"
    end
  end
  
  # Define a streaming prompt that could benefit from SSE
  defprompt "story_starter" do
    meta do
      name "Story Starter"
      description "Generates story beginnings that could be streamed"
    end
    
    arguments do
      arg :character, required: true, description: "Main character name"
      arg :setting, description: "Where the story takes place"
    end
  end
  
  # Also add a subscribable resource to show SSE capabilities
  defresource "events://greetings" do
    meta do
      name "Greeting Events Stream"
      description "Real-time stream of greeting events"
    end
    mime_type "text/event-stream"
    subscribable true
  end
  
  @impl true
  def handle_prompt_get("hello_generator", args, state) do
    recipient = args["recipient"]
    style = Map.get(args, "style", "casual")
    include_time = Map.get(args, "include_time", "false") == "true"
    
    new_state = %{state | prompt_uses: state.prompt_uses + 1}
    
    # Notify subscribers about new greeting
    notify_subscribers(recipient, new_state)
    
    time_part = if include_time do
      " at #{DateTime.utc_now() |> DateTime.to_time() |> Time.to_string()}"
    else
      ""
    end
    
    messages = case style do
      "formal" ->
        [
          system("You are a formal and polite assistant."),
          user("Please greet #{recipient}#{time_part}"),
          assistant("Good day, #{recipient}. It is my pleasure to extend you a warm welcome#{time_part}. I trust you are well.")
        ]
        
      "excited" ->
        [
          system("You are an enthusiastic and excited assistant!"),
          user("Please greet #{recipient}#{time_part}"),
          assistant("OH WOW! HI #{String.upcase(recipient)}!!! 🎉✨ I'm SO EXCITED to see you#{time_part}! This is AMAZING!")
        ]
        
      _ ->
        [
          system("You are a friendly assistant."),
          user("Please greet #{recipient}#{time_part}"),
          assistant("Hey #{recipient}! Great to see you#{time_part}. Hope you're having a good day!")
        ]
    end
    
    {:ok, %{messages: messages}, new_state}
  end
  
  @impl true
  def handle_prompt_get("story_starter", args, state) do
    character = args["character"]
    setting = Map.get(args, "setting", "a mysterious place")
    
    messages = [
      system("You are a creative storyteller. Generate engaging story beginnings."),
      user("Start a story with #{character} in #{setting}"),
      assistant("""
      The morning mist clung to #{setting} as #{character} stepped cautiously forward. 
      Something was different today - the air itself seemed to whisper secrets.
      
      [With SSE, this story could be streamed word by word for a typewriter effect!]
      """)
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  @impl true
  def handle_resource_subscribe("events://greetings", state) do
    # Add subscriber (in a real implementation, we'd track the actual connection)
    new_state = %{state | subscriptions: [:greeting_events | state.subscriptions]}
    
    IO.puts("New SSE subscription to greeting events!")
    
    # Send initial event
    spawn(fn ->
      Process.sleep(100)
      # In real implementation, this would send SSE event
      IO.puts("SSE Event: subscription confirmed")
    end)
    
    {:ok, new_state}
  end
  
  @impl true
  def handle_resource_read("events://greetings", _uri, state) do
    content = [
      text("""
      This is a subscribable resource that streams events via SSE.
      Total greetings generated: #{state.prompt_uses}
      Active subscriptions: #{length(state.subscriptions)}
      
      With SSE transport, clients can receive real-time updates!
      """)
    ]
    
    {:ok, content, state}
  end
  
  defp notify_subscribers(recipient, state) do
    if length(state.subscriptions) > 0 do
      spawn(fn ->
        # In real implementation, this would send SSE events to all subscribers
        IO.puts("SSE Event: New greeting for #{recipient} (prompt use ##{state.prompt_uses})")
      end)
    end
  end
end

# Start the HTTP+SSE server
if System.get_env("MCP_ENV") != "test" do
  port = String.to_integer(System.get_env("HTTP_SSE_PORT", "8081"))
  
  IO.puts("Starting HTTP+SSE Hello Server on port #{port}...")
  IO.puts("Server ready to accept HTTP connections with SSE support")
  IO.puts("Features:")
  IO.puts("  - Prompts: hello_generator, story_starter")
  IO.puts("  - Subscribable resource: events://greetings")
  IO.puts("  - Real-time event streaming via SSE")
  
  {:ok, _server} = HttpSseHelloServer.start_link(
    transport: :http,
    port: port,
    use_sse: true,  # Enable SSE
    name: :http_sse_hello_server
  )
  
  # Keep the server running
  Process.sleep(:infinity)
end