defmodule Examples.Native.HotReloadExample do
  @moduledoc """
  Example demonstrating dynamic service updates with Native Service Dispatcher.
  
  This example shows how to:
  - Update service behavior at runtime
  - Preserve service state during updates
  - Handle graceful service restarts
  - Demonstrate version migration patterns
  
  ## Usage
  
      # Start the example
      Examples.Native.HotReloadExample.run()
      
      # Test the interactive update capabilities
  """
  
  require Logger
  
  defmodule DynamicService do
    @moduledoc """
    A service that can be updated with different behavior at runtime.
    """
    use ExMCP.Service, name: :dynamic_service
    
    @impl true
    def init(_args) do
      {:ok, %{version: "1.0", counter: 0, features: ["basic"]}}
    end
    
    @impl true
    def handle_mcp_request("list_tools", _params, state) do
      tools = [
        %{
          "name" => "get_info",
          "description" => "Get service information and version",
          "inputSchema" => %{"type" => "object", "properties" => %{}}
        },
        %{
          "name" => "increment",
          "description" => "Increment the counter",
          "inputSchema" => %{"type" => "object", "properties" => %{}}
        },
        %{
          "name" => "update_version",
          "description" => "Update service to a new version",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "version" => %{"type" => "string"},
              "features" => %{"type" => "array", "items" => %{"type" => "string"}}
            },
            "required" => ["version"]
          }
        }
      ] ++ build_feature_tools(state.features)
      
      {:ok, %{"tools" => tools}, state}
    end
    
    def handle_mcp_request("tools/call", %{"name" => "get_info", "arguments" => _args}, state) do
      info = %{
        version: state.version,
        counter: state.counter,
        features: state.features,
        uptime: "Service running with dynamic updates enabled",
        pid: inspect(self()),
        node: node()
      }
      
      content = [%{
        "type" => "text", 
        "text" => "Service Info:\n#{Jason.encode!(info, pretty: true)}"
      }]
      
      {:ok, %{"content" => content}, state}
    end
    
    def handle_mcp_request("tools/call", %{"name" => "increment", "arguments" => _args}, state) do
      new_state = %{state | counter: state.counter + 1}
      
      content = [%{
        "type" => "text",
        "text" => "Counter incremented to #{new_state.counter}"
      }]
      
      {:ok, %{"content" => content}, new_state}
    end
    
    def handle_mcp_request("tools/call", %{"name" => "update_version", "arguments" => args}, state) do
      new_version = Map.get(args, "version", state.version)
      new_features = Map.get(args, "features", state.features)
      
      # Preserve counter across version updates
      new_state = %{state | version: new_version, features: new_features}
      
      content = [%{
        "type" => "text",
        "text" => "Service updated to version #{new_version} with features: #{inspect(new_features)}"
      }]
      
      {:ok, %{"content" => content}, new_state}
    end
    
    def handle_mcp_request("tools/call", %{"name" => "advanced_feature", "arguments" => _args}, state) do
      if "advanced" in state.features do
        content = [%{
          "type" => "text",
          "text" => "🚀 Advanced feature activated! Version: #{state.version}"
        }]
        {:ok, %{"content" => content}, state}
      else
        {:error, %{"code" => -32001, "message" => "Advanced feature not available in version #{state.version}"}, state}
      end
    end
    
    def handle_mcp_request("tools/call", %{"name" => name}, state) do
      {:error, %{"code" => -32601, "message" => "Unknown tool: #{name}"}, state}
    end
    
    def handle_mcp_request(method, _params, state) do
      {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
    end
    
    # Helper to build dynamic tools based on features
    defp build_feature_tools(features) do
      if "advanced" in features do
        [%{
          "name" => "advanced_feature",
          "description" => "Use advanced features (available in v2.0+)",
          "inputSchema" => %{"type" => "object", "properties" => %{}}
        }]
      else
        []
      end
    end
  end
  
  def run do
    IO.puts("🔥 Starting Dynamic Service Update Example with Native Service Dispatcher")
    IO.puts("=" |> String.duplicate(75))
    
    # Start the dynamic service
    {:ok, _service_pid} = DynamicService.start_link([])
    
    # Wait for service registration
    Process.sleep(100)
    
    IO.puts("✅ Dynamic service started and registered")
    
    # Demonstrate initial functionality
    IO.puts("\n📋 Testing initial functionality:")
    test_service_calls()
    
    IO.puts("\n🔄 Dynamic update capabilities available!")
    IO.puts("Service state is preserved across runtime updates.")
    IO.puts("\nTry these interactive functions:")
    IO.puts("  • test_service_calls() - test current functionality")
    IO.puts("  • simulate_version_upgrade() - upgrade to v2.0 with advanced features")
    IO.puts("  • simulate_restart() - restart service preserving state")
    
    # Return interactive functions
    %{
      test_calls: fn -> test_service_calls() end,
      upgrade: fn -> simulate_version_upgrade() end,
      restart: fn -> simulate_restart() end,
      get_stats: fn -> get_service_stats() end
    }
  end
  
  def test_service_calls do
    IO.puts("  → Calling get_info...")
    start_time = System.monotonic_time(:microsecond)
    case ExMCP.Native.call(:dynamic_service, "tools/call", %{
      "name" => "get_info",
      "arguments" => %{}
    }) do
      {:ok, %{"content" => [%{"text" => text}]}} ->
        elapsed = System.monotonic_time(:microsecond) - start_time
        IO.puts("    ✅ #{text}")
        IO.puts("    ⚡ Completed in #{elapsed}μs")
      {:error, reason} ->
        IO.puts("    ❌ Error: #{inspect(reason)}")
    end
    
    IO.puts("  → Calling increment...")
    start_time = System.monotonic_time(:microsecond)
    case ExMCP.Native.call(:dynamic_service, "tools/call", %{
      "name" => "increment",
      "arguments" => %{}
    }) do
      {:ok, %{"content" => [%{"text" => text}]}} ->
        elapsed = System.monotonic_time(:microsecond) - start_time
        IO.puts("    ✅ #{text}")
        IO.puts("    ⚡ Completed in #{elapsed}μs")
      {:error, reason} ->
        IO.puts("    ❌ Error: #{inspect(reason)}")
    end
    
    IO.puts("  → Trying advanced_feature...")
    start_time = System.monotonic_time(:microsecond)
    case ExMCP.Native.call(:dynamic_service, "tools/call", %{
      "name" => "advanced_feature",
      "arguments" => %{}
    }) do
      {:ok, %{"content" => [%{"text" => text}]}} ->
        elapsed = System.monotonic_time(:microsecond) - start_time
        IO.puts("    ✅ #{text}")
        IO.puts("    ⚡ Completed in #{elapsed}μs")
      {:error, %{"message" => message}} ->
        IO.puts("    ❌ #{message} (expected for v1.0)")
    end
  end
  
  def simulate_version_upgrade do
    IO.puts("\n🚀 Simulating upgrade to version 2.0...")
    
    # Update the service to v2.0 with advanced features
    IO.puts("  → Upgrading service with new features...")
    start_time = System.monotonic_time(:microsecond)
    case ExMCP.Native.call(:dynamic_service, "tools/call", %{
      "name" => "update_version",
      "arguments" => %{
        "version" => "2.0",
        "features" => ["basic", "advanced", "analytics"]
      }
    }) do
      {:ok, %{"content" => [%{"text" => text}]}} ->
        elapsed = System.monotonic_time(:microsecond) - start_time
        IO.puts("    ✅ #{text}")
        IO.puts("    ⚡ Update completed in #{elapsed}μs")
        
        IO.puts("  → Testing upgraded functionality:")
        test_service_calls()
      {:error, reason} ->
        IO.puts("    ❌ Upgrade failed: #{inspect(reason)}")
    end
  end
  
  def simulate_restart do
    IO.puts("\n🔄 Simulating service restart (preserves state in real implementation)...")
    
    # Get current state
    {:ok, %{"content" => [%{"text" => current_info}]}} = 
      ExMCP.Native.call(:dynamic_service, "tools/call", %{
        "name" => "get_info",
        "arguments" => %{}
      })
    
    IO.puts("  Current state: #{String.slice(current_info, 0, 100)}...")
    
    # In a real implementation, you would:
    # 1. Save current state
    # 2. Stop service
    # 3. Start new service with saved state
    # 4. Re-register with same name
    
    IO.puts("  ✅ In production: state would be preserved across restart")
    IO.puts("  ✅ Service would maintain same :dynamic_service name")
    IO.puts("  ✅ Clients would experience minimal downtime")
  end
  
  def get_service_stats do
    IO.puts("\n📊 Service Statistics")
    IO.puts("=" |> String.duplicate(30))
    
    # Check if service is available
    available = ExMCP.Native.service_available?(:dynamic_service)
    IO.puts("Service available: #{if available, do: "✓", else: "✗"}")
    
    if available do
      # Get service info
      case ExMCP.Native.call(:dynamic_service, "tools/call", %{
        "name" => "get_info",
        "arguments" => %{}
      }) do
        {:ok, %{"content" => [%{"text" => info_text}]}} ->
          IO.puts("Service info:")
          IO.puts(info_text)
        {:error, reason} ->
          IO.puts("Failed to get info: #{inspect(reason)}")
      end
      
      # List available tools
      case ExMCP.Native.call(:dynamic_service, "list_tools", %{}) do
        {:ok, %{"tools" => tools}} ->
          IO.puts("\nAvailable tools: #{length(tools)}")
          Enum.each(tools, fn tool ->
            IO.puts("  • #{tool["name"]}: #{tool["description"]}")
          end)
        {:error, reason} ->
          IO.puts("Failed to list tools: #{inspect(reason)}")
      end
    end
  end
  
  def stop_example(_interactive_funcs \\ %{}) do
    IO.puts("\n🛑 Stopping dynamic service example...")
    
    # In a real system, you might want to gracefully stop the service
    # For this example, the service will continue running
    IO.puts("✅ Example completed!")
    IO.puts("💡 Service continues running - use ExMCP.Native calls to interact")
  end
end

# Demo runner
defmodule Examples.Native.HotReloadDemo do
  def run do
    Logger.info("Starting Dynamic Service Update Demo with Native Service Dispatcher")
    Logger.info("=" |> String.duplicate(65))
    
    # Start and run the example
    interactive_funcs = Examples.Native.HotReloadExample.run()
    
    Process.sleep(500)
    
    # Demonstrate the upgrade flow
    Logger.info("\n=== Demonstrating Version Upgrade ===")
    interactive_funcs.upgrade.()
    
    Process.sleep(500)
    
    # Show final stats
    Logger.info("\n=== Final Service Statistics ===")
    interactive_funcs.get_stats.()
    
    Logger.info("\n=== Demo Benefits ===")
    IO.puts("✓ Runtime service updates without downtime")
    IO.puts("✓ State preservation across updates") 
    IO.puts("✓ Sub-millisecond update operations")
    IO.puts("✓ Zero serialization overhead for updates")
    IO.puts("✓ Graceful version migration patterns")
    
    :ok
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/hot_reload_example.ex") do
  Examples.Native.HotReloadDemo.run()
  
  IO.puts("\nService is running. Press Ctrl+C to stop.")
  Process.sleep(:infinity)
end