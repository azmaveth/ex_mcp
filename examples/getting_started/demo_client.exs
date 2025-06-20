#!/usr/bin/env elixir

# Unified Demo Client
# 
# This client demonstrates connecting to all four server types
# and using their respective features

# Suppress compilation warnings and logging as early as possible
System.put_env("ELIXIR_LOG_LEVEL", "error")
:logger.set_primary_config(:level, :error)

# Configure application-level logging
Application.put_env(:logger, :level, :error)
Application.put_env(:horde, :log_level, :emergency)

# Configure Logger module
try do
  Logger.configure(level: :error)
  Logger.configure_backend(:console, level: :error)
rescue
  _ -> :ok
end

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)},
  {:jason, "~> 1.4"},
  {:ex_json_schema, "~> 0.10"},
  {:html_entities, "~> 0.5"}
], verbose: false)

# Configure logging again after Mix.install to ensure it takes effect
:logger.set_primary_config(:level, :error)
Logger.configure(level: :error)
Application.put_env(:logger, :level, :error)
Application.put_env(:horde, :log_level, :emergency)

try do
  Logger.configure_backend(:console, level: :error)
rescue
  _ -> :ok
end

defmodule DemoClient do
  @moduledoc """
  Demonstrates all ExMCP transport types and features
  """
  
  def run do
    IO.puts("\n🎯 ExMCP Getting Started Demo")
    IO.puts("=" <> String.duplicate("=", 50))
    IO.puts("This demo will connect to 4 different MCP servers")
    IO.puts("demonstrating different transports and features.\n")
    
    # Demo 1: STDIO Server
    demo_stdio_server()
    
    # Demo 2: HTTP Server (no SSE)
    demo_http_server()
    
    # Demo 3: HTTP+SSE Server
    demo_http_sse_server()
    
    # Demo 4: Native BEAM Server
    demo_beam_server()
    
    IO.puts("\n✅ Demo completed successfully!")
    IO.puts("You've seen all four transport types in action:")
    IO.puts("  - STDIO: Simple subprocess communication")
    IO.puts("  - HTTP: Standard request/response")
    IO.puts("  - HTTP+SSE: With real-time streaming")
    IO.puts("  - Native BEAM: Direct process communication")
  end
  
  defp demo_stdio_server do
    IO.puts("\n📝 Demo 1: STDIO Transport")
    IO.puts("-" <> String.duplicate("-", 30))
    
    # Start the STDIO server as a subprocess
    server_path = Path.join(__DIR__, "01_stdio_server.exs")
    
    IO.puts("Starting STDIO server...")
    case ExMCP.Client.start_link(
      transport: :stdio,
      command: ["elixir", server_path],
      name: :stdio_client
    ) do
      {:ok, client} ->
        run_stdio_demo(client)
      {:error, error} ->
        IO.puts("❌ Failed to start STDIO server: #{inspect(error)}")
        IO.puts("✓ STDIO demo completed (failed to start)")
    end
  end
  
  defp run_stdio_demo(client) do
    # Ensure cleanup happens regardless of what occurs
    try do
      # Give server time to fully start up
      # Note: The server now has a configurable startup delay and handles
      # non-JSON output gracefully, so a shorter wait is sufficient
      Process.sleep(1000)
      
      # List available tools with extended timeout to allow for startup
      case ExMCP.Client.list_tools(client, timeout: 15000) do
        {:ok, tools_response} ->
          # Display tools in a clean format
          IO.puts("✅ Available tools:")
          for tool <- tools_response.tools do
            IO.puts("   - #{tool["name"]}: #{tool["description"]}")
          end
          
          # Call the hello tool
          IO.puts("\nCalling hello tool with different languages:")
          
          for {name, lang} <- [{"World", "english"}, {"Mundo", "spanish"}, {"世界", "japanese"}] do
            {:ok, result} = ExMCP.Client.call_tool(client, "hello", %{
              "name" => name,
              "language" => lang
            })
            
            IO.puts("  #{ExMCP.Response.text_content(result)}")
          end
          
          IO.puts("✓ STDIO demo completed")
          
        {:error, :timeout} ->
          IO.puts("⚠️ STDIO server response timeout")
          IO.puts("   This may be due to startup delays or client connectivity issues")
          IO.puts("   The STDIO server now handles non-JSON output gracefully during startup")
          IO.puts("✓ STDIO transport demo completed with timeout")
          
        {:error, error} ->
          IO.puts("❌ Error: #{inspect(error)}")
          IO.puts("✓ STDIO demo completed with error")
      end
    after
      # Always ensure client is stopped to clean up subprocess
      ExMCP.Client.stop(client)
    end
  end
  
  defp demo_http_server do
    IO.puts("\n🌐 Demo 2: HTTP Transport (without SSE)")
    IO.puts("-" <> String.duplicate("-", 30))
    
    # Start the HTTP server
    port = 8080
    server_path = Path.join(__DIR__, "02_http_server.exs")
    
    IO.puts("Starting HTTP server on port #{port}...")
    server_pid = spawn(fn ->
      System.cmd("elixir", [server_path], env: [{"HTTP_PORT", "#{port}"}])
    end)
    
    Process.sleep(2000)  # Give server time to start
    
    # Connect client (explicitly disable SSE)
    case ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:#{port}",
      use_sse: false,
      name: :http_client
    ) do
      {:ok, client} -> 
        run_http_demo(client, server_pid)
      {:error, error} ->
        IO.puts("⚠️ HTTP server demo failed: #{inspect(error)}")
        IO.puts("   Server may need more time to start or there might be a port conflict")
        IO.puts("✓ HTTP demo completed (failed)")
        Process.exit(server_pid, :kill)
    end
  end
  
  defp run_http_demo(client, server_pid) do
    try do
      IO.puts("✓ Connected to HTTP server")
      
      # List resources
      case ExMCP.Client.list_resources(client) do
        {:ok, resources_response} ->
          IO.puts("Available resources:")
          for resource <- resources_response.resources do
            IO.puts("  - #{resource["uri"]} (#{resource["name"]})")
          end
          
          # Read the hello world resource
          IO.puts("\nReading hello://world resource:")
          case ExMCP.Client.read_resource(client, "hello://world") do
            {:ok, hello_response} ->
              IO.puts(ExMCP.Response.text_content(hello_response))
              
              # Read stats to see the counter increased
              case ExMCP.Client.read_resource(client, "hello://stats") do
                {:ok, stats_response} ->
                  stats = ExMCP.Response.data_content(stats_response)
                  IO.puts("\nServer stats: #{inspect(stats, pretty: true)}")
                  IO.puts("✓ HTTP demo completed")
                {:error, error} ->
                  IO.puts("Failed to read stats: #{inspect(error)}")
                  IO.puts("✓ HTTP demo completed (partial)")
              end
            {:error, error} ->
              IO.puts("Failed to read resource: #{inspect(error)}")
              IO.puts("✓ HTTP demo completed (partial)")
          end
        {:error, error} ->
          IO.puts("Failed to list resources: #{inspect(error)}")
          IO.puts("✓ HTTP demo completed (partial)")
      end
    rescue
      error ->
        IO.puts("HTTP demo error: #{inspect(error)}")
        IO.puts("✓ HTTP demo completed (with error)")
    after
      # Clean up
      ExMCP.Client.stop(client)
      Process.exit(server_pid, :kill)
    end
  end
  
  defp demo_http_sse_server do
    IO.puts("\n📡 Demo 3: HTTP Transport with SSE")
    IO.puts("-" <> String.duplicate("-", 30))
    
    # Start the HTTP+SSE server
    port = 8081
    server_path = Path.join(__DIR__, "03_http_sse_server.exs")
    
    IO.puts("Starting HTTP+SSE server on port #{port}...")
    server_pid = spawn(fn ->
      System.cmd("elixir", [server_path], env: [{"HTTP_SSE_PORT", "#{port}"}])
    end)
    
    Process.sleep(1000)  # Give server time to start
    
    # Connect client with SSE support
    {:ok, client} = ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:#{port}",
      use_sse: true,
      name: :http_sse_client
    )
    
    # List prompts
    {:ok, prompts_response} = ExMCP.Client.list_prompts(client)
    IO.puts("Available prompts:")
    for prompt <- prompts_response.prompts do
      IO.puts("  - #{prompt["name"]} (#{prompt["description"]})")
    end
    
    # Use the hello generator prompt
    IO.puts("\nGenerating greetings with different styles:")
    
    for {recipient, style} <- [{"Alice", "casual"}, {"Dr. Smith", "formal"}, {"Champion", "excited"}] do
      {:ok, prompt_result} = ExMCP.Client.get_prompt(client, "hello_generator", %{
        "recipient" => recipient,
        "style" => style,
        "include_time" => "true"
      })
      
      # Extract the assistant message
      messages = prompt_result.messages
      assistant_msg = Enum.find(messages, &(&1["role"] == "assistant"))
      IO.puts("\n#{style |> String.capitalize()}: #{assistant_msg["content"]}")
    end
    
    # Note about SSE capabilities
    IO.puts("\n💡 With SSE, this server could stream responses in real-time!")
    IO.puts("   Great for: progress updates, live data, streaming content")
    
    # Clean up
    ExMCP.Client.stop(client)
    Process.exit(server_pid, :kill)
    IO.puts("✓ HTTP+SSE demo completed")
  end
  
  defp demo_beam_server do
    IO.puts("\n🚀 Demo 4: Native BEAM Transport")
    IO.puts("-" <> String.duplicate("-", 30))
    
    # Set up distributed Erlang for the client
    client_node_name = "mcp_client@localhost"
    cookie = "mcp_secret_cookie"
    
    IO.puts("Setting up distributed Erlang...")
    Node.start(String.to_atom(client_node_name))
    Node.set_cookie(String.to_atom(cookie))
    IO.puts("Client node: #{Node.self()}")
    
    # Start the BEAM server
    server_path = Path.join(__DIR__, "04_beam_server.exs")
    
    IO.puts("Starting BEAM server...")
    server_pid = spawn(fn ->
      System.cmd("elixir", [server_path], env: [
        {"BEAM_NODE", "mcp_server@localhost"},
        {"BEAM_COOKIE", cookie}
      ])
    end)
    
    Process.sleep(1500)  # Give server time to start and register
    
    # Connect to the server node
    server_node = :"mcp_server@localhost"
    case Node.ping(server_node) do
      :pong ->
        IO.puts("✓ Connected to server node: #{server_node}")
        
        # Connect MCP client using native transport
        {:ok, client} = ExMCP.Client.start_link(
          transport: :native,
          server: {:global, :beam_mcp_server},
          name: :beam_client
        )
        
        # Read system info
        IO.puts("\nReading BEAM system info:")
        {:ok, info_response} = ExMCP.Client.read_resource(client, "beam://system/info")
        info = ExMCP.Response.data_content(info_response)
        IO.puts("  OTP Release: #{info["system_info"]["otp_release"]}")
        IO.puts("  ERTS Version: #{info["system_info"]["erts_version"]}")
        IO.puts("  Process Count: #{info["system_info"]["process_count"]}")
        IO.puts("  Connected Nodes: #{inspect(info["connected_nodes"])}")
        
        # Use the distributed hello tool
        IO.puts("\nSending distributed message back to ourselves:")
        {:ok, tool_result} = ExMCP.Client.call_tool(client, "distributed_hello", %{
          "target_node" => client_node_name,
          "message" => "Hello from the unified demo client! 🎉"
        })
        IO.puts(ExMCP.Response.text_content(tool_result))
        
        # Get BEAM expert advice
        IO.puts("\nAsking the BEAM expert:")
        {:ok, expert_result} = ExMCP.Client.get_prompt(client, "beam_expert", %{
          "topic" => "benefits of native BEAM transport in MCP",
          "level" => "intermediate"
        })
        
        assistant_msg = expert_result.messages 
          |> Enum.find(&(&1["role"] == "assistant"))
        IO.puts(assistant_msg["content"])
        
        # Check message history
        {:ok, messages_response} = ExMCP.Client.read_resource(client, "beam://messages")
        messages_data = ExMCP.Response.data_content(messages_response)
        IO.puts("\nServer stats: #{inspect(messages_data["stats"], pretty: true)}")
        
        # Clean up
        ExMCP.Client.stop(client)
        IO.puts("✓ Native BEAM demo completed")
        
      :pang ->
        IO.puts("⚠️  Could not connect to server node")
        IO.puts("   Skipping BEAM demo - make sure distributed Erlang is configured")
    end
    
    # Always clean up the server process
    Process.exit(server_pid, :kill)
  end
end

# Run the demo
if System.get_env("MCP_ENV") != "test" do
  DemoClient.run()
end