#!/usr/bin/env elixir

# Simple test client to connect to native server and list tools

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule TestNativeClient do
  def run do
    IO.puts("🧪 Testing Native Server Connection...")
    
    # Native transport only works within the same BEAM instance
    # Try to find the server by its registered name
    case Process.whereis(:native_mcp_server) do
      pid when is_pid(pid) ->
        IO.puts("✅ Found native server by process name: #{inspect(pid)}")
        test_by_pid(pid)
        
      nil ->
        IO.puts("❌ No native server found by name :native_mcp_server")
        IO.puts("   Trying Registry lookup...")
        
        # Since native servers run in different OS processes, we can't connect
        IO.puts("\n📝 Note: Native transport requires the client and server")
        IO.puts("   to run in the same BEAM instance (same Elixir process).")
        IO.puts("   You cannot connect to a native server running in a")
        IO.puts("   different terminal/process.")
        IO.puts("\n💡 For cross-process communication, use:")
        IO.puts("   - stdio transport")
        IO.puts("   - HTTP transport") 
        IO.puts("   - SSE transport")
        IO.puts("\n🧪 Starting a local demo server instead...")
        start_demo_server()
    end
  end
  
  defp test_by_pid(server_pid) do
    IO.puts("\n🔗 Connecting to native server by PID...")
    
    case ExMCP.Client.start_link(transport: :native, server_pid: server_pid) do
      {:ok, client} ->
        IO.puts("✅ Client started successfully")
        test_client(client)
        GenServer.stop(client)
        
      {:error, reason} ->
        IO.puts("❌ Failed to start client: #{inspect(reason)}")
    end
  end
  
  defp test_by_service_name(service_name) do
    IO.puts("\n🔗 Connecting to native server by service name...")
    
    case ExMCP.Client.start_link(transport: :native, service_name: service_name) do
      {:ok, client} ->
        IO.puts("✅ Client started successfully")
        test_client(client)
        GenServer.stop(client)
        
      {:error, reason} ->
        IO.puts("❌ Failed to start client: #{inspect(reason)}")
    end
  end
  
  defp test_client(client) do
    # Initialize the connection
    IO.puts("\n📡 Initializing connection...")
    case ExMCP.Client.initialize(client) do
      {:ok, response} ->
        IO.puts("✅ Initialized successfully!")
        IO.puts("   Server info: #{inspect(response)}")
        
      {:error, reason} ->
        IO.puts("❌ Initialize failed: #{inspect(reason)}")
    end
    
    # List tools
    IO.puts("\n🔧 Listing tools...")
    case ExMCP.Client.list_tools(client) do
      {:ok, tools} ->
        IO.puts("✅ Found #{length(tools)} tools:")
        Enum.each(tools, fn tool ->
          IO.puts("   - #{tool["name"]}: #{tool["description"]}")
        end)
        
      {:error, reason} ->
        IO.puts("❌ Failed to list tools: #{inspect(reason)}")
    end
    
    # List resources
    IO.puts("\n📁 Listing resources...")
    case ExMCP.Client.list_resources(client) do
      {:ok, resources} ->
        IO.puts("✅ Found #{length(resources)} resources:")
        Enum.each(resources, fn resource ->
          IO.puts("   - #{resource["uri"]}: #{resource["name"]}")
        end)
        
      {:error, reason} ->
        IO.puts("❌ Failed to list resources: #{inspect(reason)}")
    end
    
    # List prompts
    IO.puts("\n💭 Listing prompts...")
    case ExMCP.Client.list_prompts(client) do
      {:ok, prompts} ->
        IO.puts("✅ Found #{length(prompts)} prompts:")
        Enum.each(prompts, fn prompt ->
          IO.puts("   - #{prompt["name"]}: #{prompt["description"]}")
        end)
        
      {:error, reason} ->
        IO.puts("❌ Failed to list prompts: #{inspect(reason)}")
    end
  end
  
  defp start_demo_server do
    # Define a simple demo server
    defmodule DemoNativeServer do
      use ExMCP.ServerV2
      
      deftool "hello" do
        tool_description "Say hello"
        args do
          field :name, :string, required: true
        end
      end
      
      deftool "info" do
        tool_description "Get server info"
        args do
        end
      end
      
      defresource "demo://status" do
        resource_name "Demo Status"
        resource_description "Current demo status"
        mime_type "application/json"
      end
      
      defprompt "assistant" do
        prompt_name "Demo Assistant"
        prompt_description "A helpful assistant"
        arguments do
          arg :query, required: true
        end
      end
      
      @impl true
      def handle_tool_call("hello", %{"name" => name}, state) do
        {:ok, %{content: [text("Hello, #{name}! This is a demo native server.")]}, state}
      end
      
      @impl true
      def handle_tool_call("info", _args, state) do
        info = %{
          server: "DemoNativeServer",
          transport: "native",
          capabilities: Map.keys(get_capabilities())
        }
        {:ok, %{content: [json(info)]}, state}
      end
      
      @impl true
      def handle_resource_read("demo://status", _uri, state) do
        {:ok, [json(%{status: "running", time: DateTime.utc_now()})], state}
      end
      
      @impl true
      def handle_prompt_get("assistant", %{"query" => query}, state) do
        messages = [
          system("You are a helpful demo assistant."),
          user(query)
        ]
        {:ok, %{messages: messages}, state}
      end
    end
    
    # Start the demo server
    {:ok, server} = DemoNativeServer.start_link()
    IO.puts("✅ Started local demo server: #{inspect(server)}")
    
    # Test it
    test_by_pid(server)
    
    # Clean up
    GenServer.stop(server)
  end
end

# Run the test
TestNativeClient.run()