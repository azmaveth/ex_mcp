#!/usr/bin/env elixir

# Native BEAM Server with Full Feature Set
# 
# This server demonstrates native BEAM transport (direct Erlang process communication)
# It provides a tool, resource, and prompt to showcase all capabilities

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)},
  {:jason, "~> 1.4"},
  {:ex_json_schema, "~> 0.10"},
  {:html_entities, "~> 0.5"}
], verbose: false)

# Configure logging for cleaner demo output
Logger.configure(level: :info)

defmodule BeamHelloServer do
  use ExMCP.Server
  
  @impl true
  def init(_args) do
    {:ok, %{
      messages: [],
      stats: %{
        tools_called: 0,
        resources_read: 0,
        prompts_used: 0
      }
    }}
  end
  
  # Tool: Distributed Hello
  deftool "distributed_hello" do
    meta do
      description "Sends a hello across BEAM nodes"
    end
    
    args do
      field :target_node, :string, 
        required: true,
        description: "Name of the target node (e.g., 'client@hostname')"
        
      field :message, :string,
        default: "Hello from the BEAM!",
        description: "Custom message to send"
    end
  end
  
  # Resource: BEAM System Info
  defresource "beam://system/info" do
    meta do
      name "BEAM System Information"
      description "Current BEAM VM system information"
    end
    mime_type "application/json"
  end
  
  # Resource: Message History
  defresource "beam://messages" do
    meta do
      name "Message History"
      description "History of distributed messages"
    end
    mime_type "application/json"
    subscribable true
  end
  
  # Prompt: BEAM Expert
  defprompt "beam_expert" do
    meta do
      name "BEAM/OTP Expert"
      description "Expert advice on BEAM, OTP, and distributed Elixir"
    end
    
    arguments do
      arg :topic, required: true, description: "What BEAM topic to discuss"
      arg :level, description: "Expertise level (beginner/intermediate/advanced)"
    end
  end
  
  @impl true
  def handle_tool_call("distributed_hello", args, state) do
    target_node = String.to_atom(args["target_node"])
    message = args["message"]
    
    # Attempt to send message to target node
    result = try do
      case Node.ping(target_node) do
        :pong ->
          # Node is reachable
          :rpc.call(target_node, IO, :puts, ["📨 Received via BEAM: #{message}"])
          
          new_message = %{
            timestamp: DateTime.utc_now(),
            target: target_node,
            message: message,
            status: :delivered
          }
          
          new_state = %{
            state | 
            messages: [new_message | state.messages],
            stats: %{state.stats | tools_called: state.stats.tools_called + 1}
          }
          
          content = [
            text("✅ Message delivered to #{target_node}!"),
            json(%{
              delivered: true,
              target: target_node,
              message: message,
              node_info: %{
                current_node: Node.self(),
                connected_nodes: Node.list()
              }
            })
          ]
          
          {:ok, %{content: content}, new_state}
          
        :pang ->
          # Node is not reachable
          new_message = %{
            timestamp: DateTime.utc_now(),
            target: target_node,
            message: message,
            status: :unreachable
          }
          
          new_state = %{
            state |
            messages: [new_message | state.messages],
            stats: %{state.stats | tools_called: state.stats.tools_called + 1}
          }
          
          {:error, "Node #{target_node} is not reachable. Make sure it's running and connected.", new_state}
      end
    catch
      error, reason ->
        {:error, "Failed to send message: #{inspect({error, reason})}", state}
    end
  end
  
  @impl true
  def handle_resource_read("beam://system/info", _uri, state) do
    info = %{
      node: Node.self(),
      cookie: :erlang.get_cookie(),
      connected_nodes: Node.list(),
      system_info: %{
        otp_release: :erlang.system_info(:otp_release) |> to_string(),
        erts_version: :erlang.system_info(:version) |> to_string(),
        process_count: :erlang.system_info(:process_count),
        port_count: :erlang.system_info(:port_count),
        atom_count: :erlang.system_info(:atom_count)
      },
      scheduler_info: %{
        schedulers: :erlang.system_info(:schedulers),
        schedulers_online: :erlang.system_info(:schedulers_online)
      }
    }
    
    new_state = %{state | stats: %{state.stats | resources_read: state.stats.resources_read + 1}}
    
    {:ok, [json(info)], new_state}
  end
  
  @impl true
  def handle_resource_read("beam://messages", _uri, state) do
    content = %{
      total_messages: length(state.messages),
      messages: Enum.take(state.messages, 10),  # Last 10 messages
      stats: state.stats
    }
    
    new_state = %{state | stats: %{state.stats | resources_read: state.stats.resources_read + 1}}
    
    {:ok, [json(content)], new_state}
  end
  
  @impl true
  def handle_prompt_get("beam_expert", args, state) do
    topic = args["topic"]
    level = Map.get(args, "level", "intermediate")
    
    new_state = %{state | stats: %{state.stats | prompts_used: state.stats.prompts_used + 1}}
    
    messages = [
      system("""
      You are an expert in BEAM, OTP, and distributed Elixir systems.
      Provide advice appropriate for #{level} level developers.
      Focus on practical examples and best practices.
      """),
      user("I need help with: #{topic}"),
      assistant("""
      I'd be happy to help with #{topic}! 
      
      Native BEAM transport in MCP offers unique advantages:
      - Direct process communication (no serialization overhead)
      - Built-in distribution and clustering
      - Access to OTP behaviors and supervision trees
      - Hot code reloading capabilities
      
      What specific aspect of #{topic} would you like to explore?
      """)
    ]
    
    {:ok, %{messages: messages}, new_state}
  end
  
  @impl true
  def handle_resource_subscribe("beam://messages", state) do
    IO.puts("New subscription to message history - will receive updates on new messages")
    {:ok, state}
  end
end

# Start the BEAM server
if System.get_env("MCP_ENV") != "test" do
  # Set up distributed Erlang if not already done
  node_name = System.get_env("BEAM_NODE", "mcp_server@localhost")
  cookie = System.get_env("BEAM_COOKIE", "mcp_secret_cookie")
  
  # Start distributed Erlang
  case Node.start(String.to_atom(node_name)) do
    {:ok, _} -> IO.puts("Started distributed node: #{node_name}")
    {:error, {:already_started, _}} -> IO.puts("Node already started: #{node_name}")
    error -> IO.puts("Failed to start node: #{inspect(error)}")
  end
  
  # Set the cookie
  Node.set_cookie(String.to_atom(cookie))
  
  IO.puts("\n🚀 Starting Native BEAM Hello Server")
  IO.puts("Node: #{Node.self()}")
  IO.puts("Cookie: #{cookie}")
  IO.puts("\nFeatures:")
  IO.puts("  - Tool: distributed_hello - Send messages between BEAM nodes")
  IO.puts("  - Resources: beam://system/info, beam://messages")
  IO.puts("  - Prompt: beam_expert - Get BEAM/OTP advice")
  IO.puts("\nThis server uses native BEAM transport for zero-overhead communication!")
  
  {:ok, _server} = BeamHelloServer.start_link(
    transport: :native,
    name: :beam_hello_server
  )
  
  # Register globally for easier discovery
  :global.register_name(:beam_mcp_server, self())
  
  # Keep the server running
  Process.sleep(:infinity)
end