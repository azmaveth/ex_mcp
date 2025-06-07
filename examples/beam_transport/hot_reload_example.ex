defmodule ExMCP.Examples.BeamTransport.HotReloadExample do
  @moduledoc """
  Example demonstrating hot code reloading with BEAM transport.
  
  This example shows how to enable hot reloading for MCP servers, allowing
  you to update handler code without stopping the server or losing connections.
  
  ## Usage
  
      # Start the example
      ExMCP.Examples.BeamTransport.HotReloadExample.run()
      
      # In another terminal, you can connect a client and see hot reloading in action
  """
  
  alias ExMCP.{Server, Client}
  alias ExMCP.Transport.Beam.HotReload
  
  defmodule DynamicHandler do
    @moduledoc """
    A handler that can be hot-reloaded with different behavior.
    """
    use ExMCP.Server.Handler
    
    @impl true
    def init(_args) do
      {:ok, %{version: "1.0", counter: 0, features: ["basic"]}}
    end
    
    @impl true
    def handle_initialize(_params, state) do
      {:ok, %{
        name: "hot-reload-example",
        version: state.version,
        capabilities: %{
          tools: %{}
        }
      }, state}
    end
    
    @impl true
    def handle_list_tools(_cursor, state) do
      tools = [
        %{
          name: "get_info",
          description: "Get server information and version",
          inputSchema: %{type: "object", properties: %{}}
        },
        %{
          name: "increment",
          description: "Increment the counter",
          inputSchema: %{type: "object", properties: %{}}
        }
      ] ++ build_feature_tools(state.features)
      
      {:ok, tools, nil, state}
    end
    
    @impl true
    def handle_call_tool("get_info", _args, state) do
      info = %{
        version: state.version,
        counter: state.counter,
        features: state.features,
        uptime: "Server running with hot reload enabled"
      }
      
      result = [%{
        type: "text", 
        text: "Server Info: #{inspect(info, pretty: true)}"
      }]
      
      {:ok, result, state}
    end
    
    def handle_call_tool("increment", _args, state) do
      new_state = %{state | counter: state.counter + 1}
      
      result = [%{
        type: "text",
        text: "Counter incremented to #{new_state.counter}"
      }]
      
      {:ok, result, new_state}
    end
    
    def handle_call_tool("advanced_feature", _args, state) do
      if "advanced" in state.features do
        result = [%{
          type: "text",
          text: "🚀 Advanced feature activated! Version: #{state.version}"
        }]
        {:ok, result, state}
      else
        {:error, "Advanced feature not available in version #{state.version}", state}
      end
    end
    
    def handle_call_tool(name, _args, state) do
      {:error, "Unknown tool: #{name}", state}
    end
    
    # Helper to build dynamic tools based on features
    defp build_feature_tools(features) do
      if "advanced" in features do
        [%{
          name: "advanced_feature",
          description: "Use advanced features (available in v2.0+)",
          inputSchema: %{type: "object", properties: %{}}
        }]
      else
        []
      end
    end
  end
  
  def run do
    IO.puts("🔥 Starting Hot Reload Example")
    IO.puts("================================")
    
    # Start the server
    {:ok, server} = Server.start_link([
      handler: DynamicHandler,
      transport: :beam,
      name: :hot_reload_example_server
    ])
    
    IO.puts("✅ Server started as :hot_reload_example_server")
    
    # Enable hot reloading
    {:ok, _manager} = HotReload.enable(server, %{
      auto_reload: false,  # Manual reload for demo
      state_migration: :automatic,
      preserve_connections: true,
      validation: :strict,
      rollback_on_failure: true
    })
    
    IO.puts("🔥 Hot reload enabled!")
    
    # Start a client to demonstrate functionality
    {:ok, client} = Client.start_link([
      transport: :beam,
      server: :hot_reload_example_server
    ])
    
    IO.puts("📡 Client connected")
    
    # Demonstrate initial functionality
    IO.puts("\n📋 Testing initial functionality:")
    test_tools(client)
    
    IO.puts("\n🔄 Hot reload capabilities enabled!")
    IO.puts("You can now modify the handler and test hot reloading.")
    IO.puts("\nTry these commands:")
    IO.puts("  • HotReload.reload_handler(server) - manually trigger reload")
    IO.puts("  • test_tools(client) - test current functionality")
    IO.puts("  • simulate_upgrade() - simulate upgrading to v2.0")
    
    # Return the server and client PIDs for interactive use
    %{
      server: server,
      client: client,
      test_tools: fn -> test_tools(client) end,
      simulate_upgrade: fn -> simulate_upgrade(server, client) end,
      reload: fn -> HotReload.reload_handler(server) end
    }
  end
  
  def test_tools(client) do
    IO.puts("  → Calling get_info...")
    case Client.call_tool(client, "get_info", %{}) do
      {:ok, result} ->
        IO.puts("    ✅ #{result["content"] |> hd() |> Map.get("text")}")
      {:error, reason} ->
        IO.puts("    ❌ Error: #{inspect(reason)}")
    end
    
    IO.puts("  → Calling increment...")
    case Client.call_tool(client, "increment", %{}) do
      {:ok, result} ->
        IO.puts("    ✅ #{result["content"] |> hd() |> Map.get("text")}")
      {:error, reason} ->
        IO.puts("    ❌ Error: #{inspect(reason)}")
    end
    
    IO.puts("  → Trying advanced_feature...")
    case Client.call_tool(client, "advanced_feature", %{}) do
      {:ok, result} ->
        IO.puts("    ✅ #{result["content"] |> hd() |> Map.get("text")}")
      {:error, reason} ->
        IO.puts("    ❌ #{inspect(reason)}")
    end
  end
  
  def simulate_upgrade(server, client) do
    IO.puts("\n🚀 Simulating upgrade to version 2.0...")
    
    # This would normally be done by modifying the actual module file
    # For demonstration, we'll show what the upgrade process looks like
    
    IO.puts("  1. New version would add 'advanced' to features")
    IO.puts("  2. Version would be updated to '2.0'")
    IO.puts("  3. Hot reload would preserve counter state")
    
    # Trigger manual reload (in real scenario, this would detect file changes)
    IO.puts("  → Triggering hot reload...")
    case HotReload.reload_handler(server) do
      :ok ->
        IO.puts("    ✅ Hot reload completed!")
        IO.puts("  → Testing upgraded functionality:")
        test_tools(client)
      {:error, reason} ->
        IO.puts("    ❌ Hot reload failed: #{inspect(reason)}")
    end
  end
  
  def stop_example(pids) when is_map(pids) do
    IO.puts("\n🛑 Stopping example...")
    
    if Map.has_key?(pids, :client) do
      Client.disconnect(pids.client)
      IO.puts("📡 Client disconnected")
    end
    
    if Map.has_key?(pids, :server) do
      HotReload.disable(pids.server)
      Server.stop(pids.server)
      IO.puts("🔥 Hot reload disabled")
      IO.puts("🛑 Server stopped")
    end
    
    IO.puts("✅ Example stopped cleanly")
  end
end