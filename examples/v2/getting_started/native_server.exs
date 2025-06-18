#!/usr/bin/env elixir

# Native MCP Server using ExMCP v2 DSL with BEAM transport
# Ultra-fast Elixir-to-Elixir communication

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule NativeServerV2 do
  @moduledoc """
  A native MCP server using the v2 DSL with BEAM transport.
  
  🎯 Key features:
  - ✨ 15 lines of DSL for a complete server
  - ⚡ Ultra-fast BEAM communication
  - 📋 Auto-capability detection
  - 🔧 Built-in service discovery
  - 📝 Smart content helpers
  """
  
  use ExMCP.ServerV2

  # Clean DSL - look how simple this is!
  deftool "say_hello" do
    tool_description "Say hello via native BEAM transport"
    
    args do
      field :name, :string, required: true, description: "Name to greet"
      field :wave, :boolean, default: false, description: "Add a wave emoji"
    end
  end

  deftool "beam_info" do
    tool_description "Get information about the BEAM runtime"
    
    args do
      field :category, :string, required: true, 
        enum: ["system", "memory", "statistics"], 
        description: "What info to retrieve"
    end
  end

  # Native-specific resources
  defresource "beam://node/info" do
    resource_name "BEAM Node Information"
    resource_description "Current BEAM node information"
    mime_type "application/json"
  end

  defresource "beam://processes" do
    resource_name "Process List"
    resource_description "List of running processes"
    mime_type "application/json"
  end

  # Interactive prompt
  defprompt "beam_assistant" do
    prompt_name "BEAM Assistant"
    prompt_description "Expert help with BEAM/Erlang/Elixir"
    
    arguments do
      arg :topic, required: true, description: "Topic to discuss"
      arg :level, description: "Expertise level (beginner, intermediate, expert)"
    end
  end

  # Handler implementations
  @impl true
  def handle_tool_call("say_hello", %{"name" => name, "wave" => wave}, state) do
    greeting = "Hello, #{name}! Welcome to ExMCP v2 via Native BEAM! ⚡"
    greeting = if wave, do: greeting <> " 👋", else: greeting
    
    {:ok, %{content: [text(greeting)]}, state}
  end

  def handle_tool_call("say_hello", %{"name" => name}, state) do
    handle_tool_call("say_hello", %{"name" => name, "wave" => false}, state)
  end

  @impl true
  def handle_tool_call("beam_info", %{"category" => category}, state) do
    info = case category do
      "system" ->
        %{
          node: Node.self(),
          cookie: :erlang.get_cookie(),
          otp_release: :erlang.system_info(:otp_release),
          erts_version: :erlang.system_info(:version),
          schedulers: :erlang.system_info(:schedulers)
        }
      
      "memory" ->
        memory = :erlang.memory()
        %{
          total_mb: memory[:total] / (1024 * 1024) |> Float.round(2),
          processes_mb: memory[:processes] / (1024 * 1024) |> Float.round(2),
          atom_mb: memory[:atom] / (1024 * 1024) |> Float.round(2),
          binary_mb: memory[:binary] / (1024 * 1024) |> Float.round(2),
          ets_mb: memory[:ets] / (1024 * 1024) |> Float.round(2)
        }
      
      "statistics" ->
        %{
          process_count: :erlang.system_info(:process_count),
          port_count: :erlang.system_info(:port_count),
          atom_count: :erlang.system_info(:atom_count),
          module_count: length(:code.all_loaded()),
          uptime_seconds: :erlang.statistics(:wall_clock) |> elem(0) |> div(1000)
        }
    end
    
    {:ok, %{content: [json(info)]}, state}
  end

  @impl true
  def handle_resource_read("beam://node/info", _uri, state) do
    node_info = %{
      node_name: Node.self(),
      alive: Node.alive?(),
      cookie: :erlang.get_cookie(),
      connected_nodes: Node.list(),
      otp_release: :erlang.system_info(:otp_release) |> List.to_string(),
      erts_version: :erlang.system_info(:version) |> List.to_string(),
      capabilities: get_capabilities(),
      improvements: [
        "15 lines of DSL vs manual GenServer implementation",
        "Auto-service discovery via ExMCP.Registry",
        "Built-in BEAM transport",
        "Native Erlang term serialization",
        "Zero network overhead"
      ]
    }
    
    content = [json(node_info)]
    {:ok, content, state}
  end

  @impl true
  def handle_resource_read("beam://processes", _uri, state) do
    processes = Process.list()
    |> Enum.take(10)  # Just show first 10
    |> Enum.map(fn pid ->
      info = Process.info(pid, [:registered_name, :current_function, :memory])
      %{
        pid: inspect(pid),
        name: info[:registered_name] || "anonymous",
        function: inspect(info[:current_function]),
        memory_kb: (info[:memory] || 0) / 1024 |> Float.round(2)
      }
    end)
    
    content = [json(%{processes: processes, total: length(Process.list())})]
    {:ok, content, state}
  end

  @impl true
  def handle_prompt_get("beam_assistant", args, state) do
    topic = Map.get(args, "topic", "BEAM")
    level = Map.get(args, "level", "intermediate")
    
    messages = [
      system("You are an expert in BEAM, Erlang, Elixir, and OTP. Adjust explanations to #{level} level."),
      user("I need help understanding #{topic} in the context of BEAM/Elixir."),
      assistant("I'd be happy to help you understand #{topic}! Since you're at the #{level} level, I'll tailor my explanation accordingly. What specific aspect of #{topic} would you like to explore?")
    ]
    
    {:ok, %{messages: messages}, state}
  end
end

# Start the native server
defmodule NativeRunner do
  def start do
    IO.puts("""
    ⚡ Starting ExMCP v2 Native BEAM Server...
    
    🎯 Native BEAM transport features:
    - Direct process communication (no JSON serialization)
    - Auto-discovery via Registry
    - Zero network overhead
    - Native Erlang term passing
    
    🔧 Compare to v1:
    - v1: Manual GenServer + Registry setup
    - v2: ~15 lines of clean DSL
    - Auto-capability detection
    - Built-in service discovery
    """)

    # Start the server
    {:ok, server} = NativeServerV2.start_link()
    
    # For native transport within same BEAM instance only
    # (Different OS processes can't share process registrations)
    Process.register(server, :native_mcp_server)
    ExMCP.Registry.register("native_server_v2", server)
    
    IO.puts("""
    
    ✅ Native server started and registered!
    
    📡 Service registered as:
    - Process name: :native_mcp_server
    - Registry name: "native_server_v2"
    
    ✨ Auto-detected capabilities: #{inspect(Map.keys(NativeServerV2.get_capabilities()))}
    🔧 Available tools: #{inspect(Map.keys(NativeServerV2.get_tools()))}
    📁 Available resources: #{inspect(Map.keys(NativeServerV2.get_resources()))}
    💭 Available prompts: #{inspect(Map.keys(NativeServerV2.get_prompts()))}
    
    🧪 Test with ExMCP client:
    
    {:ok, client} = ExMCP.Client.start_link(
      transport: :native,
      service_name: "native_server_v2"
    )
    
    # Or connect directly via process name:
    {:ok, client} = ExMCP.Client.start_link(
      transport: :native,
      server_pid: :native_mcp_server
    )
    
    Keep this script running to maintain the server.
    Press Ctrl+C to stop.
    """)
    
    # Keep running
    Process.sleep(:infinity)
  end
end

# Start the server
NativeRunner.start()