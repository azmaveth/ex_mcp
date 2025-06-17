defmodule Examples.Advanced.NotificationsService do
  @moduledoc """
  Example MCP service demonstrating Native Service Dispatcher features:
  - Service notifications using ExMCP.Native.notify
  - Progress tracking for long operations
  - Resource change notifications
  - Dynamic tool updates
  """
  
  use ExMCP.Service, name: :notifications_service
  require Logger
  
  defmodule State do
    defstruct [
      resources: %{},
      tools: [],
      prompts: [],
      processing_tasks: %{},
      subscribers: MapSet.new()
    ]
  end
  
  @impl true
  def init(_args) do
    Logger.info("Notifications service starting...")
    
    initial_state = %State{
      resources: %{
        "file:///data/config.json" => %{
          "name" => "Configuration",
          "description" => "System configuration",
          "content" => Jason.encode!(%{theme: "dark", lang: "en"})
        },
        "file:///data/users.csv" => %{
          "name" => "User Data", 
          "description" => "User database export",
          "content" => "id,name,email\n1,Alice,alice@example.com\n2,Bob,bob@example.com"
        }
      },
      tools: [
        %{
          "name" => "process_file",
          "description" => "Process a file with progress tracking"
        },
        %{
          "name" => "update_config",
          "description" => "Update configuration and notify"
        }
      ],
      prompts: [
        %{
          "name" => "analyze_data",
          "description" => "Analyze data patterns"
        }
      ]
    }
    
    {:ok, initial_state}
  end
  
  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "process_file",
        "description" => "Process a file with progress notifications",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "path" => %{"type" => "string"},
            "steps" => %{"type" => "integer", "default" => 10}
          },
          "required" => ["path"]
        }
      },
      %{
        "name" => "update_config",
        "description" => "Update configuration (triggers resource notification)",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "key" => %{"type" => "string"},
            "value" => %{"type" => "string"}
          },
          "required" => ["key", "value"]
        }
      },
      %{
        "name" => "add_tool",
        "description" => "Dynamically add a new tool (triggers tool list notification)",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "name" => %{"type" => "string"},
            "description" => %{"type" => "string"}
          },
          "required" => ["name", "description"]
        }
      },
      %{
        "name" => "subscribe_notifications",
        "description" => "Subscribe to service notifications",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "subscriber_id" => %{"type" => "string"}
          },
          "required" => ["subscriber_id"]
        }
      }
    ]
    
    {:ok, %{"tools" => tools}, state}
  end
  
  @impl true
  def handle_mcp_request("tools/call", %{"name" => "process_file", "arguments" => %{"path" => path} = args}, state) do
    steps = Map.get(args, "steps", 10)
    progress_token = get_in(args, ["meta", "progressToken"]) || "process-#{System.unique_integer()}"
    
    # Start async task for processing with progress updates
    task_pid = spawn_link(fn ->
      simulate_file_processing(path, steps, progress_token)
    end)
    
    new_state = %{state | processing_tasks: Map.put(state.processing_tasks, progress_token, task_pid)}
    
    content = [%{
      "type" => "text",
      "text" => "Started processing #{path} with #{steps} steps. Progress token: #{progress_token}"
    }]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "update_config", "arguments" => %{"key" => key, "value" => value}}, state) do
    # Update the configuration resource
    config_uri = "file:///data/config.json"
    current_config = state.resources[config_uri]["content"] |> Jason.decode!()
    new_config = Map.put(current_config, key, value)
    new_content = Jason.encode!(new_config)
    
    updated_resource = Map.put(state.resources[config_uri], "content", new_content)
    new_resources = Map.put(state.resources, config_uri, updated_resource)
    new_state = %{state | resources: new_resources}
    
    # Notify subscribers of resource change
    notify_resource_changed(config_uri, state.subscribers)
    
    content = [%{
      "type" => "text",
      "text" => "Updated configuration: #{key} = #{value}. Subscribers notified."
    }]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "add_tool", "arguments" => %{"name" => name, "description" => desc}}, state) do
    new_tool = %{
      "name" => name,
      "description" => desc,
      "inputSchema" => %{
        "type" => "object",
        "properties" => %{}
      }
    }
    
    new_tools = [new_tool | state.tools]
    new_state = %{state | tools: new_tools}
    
    # Notify subscribers of tool list change
    notify_tools_changed(state.subscribers)
    
    content = [%{
      "type" => "text",
      "text" => "Added new tool '#{name}'. Tool list updated. Subscribers notified."
    }]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "subscribe_notifications", "arguments" => %{"subscriber_id" => sub_id}}, state) do
    new_subscribers = MapSet.put(state.subscribers, sub_id)
    new_state = %{state | subscribers: new_subscribers}
    
    content = [%{
      "type" => "text",
      "text" => "Subscribed #{sub_id} to notifications. Total subscribers: #{MapSet.size(new_subscribers)}"
    }]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("list_resources", _params, state) do
    resources = state.resources
    |> Enum.map(fn {uri, resource} ->
      %{
        "uri" => uri,
        "name" => resource["name"],
        "description" => resource["description"],
        "mimeType" => if String.ends_with?(uri, ".json"), do: "application/json", else: "text/csv"
      }
    end)
    
    {:ok, %{"resources" => resources}, state}
  end
  
  def handle_mcp_request("resources/read", %{"uri" => uri}, state) do
    case Map.get(state.resources, uri) do
      nil ->
        {:error, %{"code" => -32602, "message" => "Resource not found: #{uri}"}, state}
      
      resource ->
        content = %{
          "uri" => uri,
          "mimeType" => if String.ends_with?(uri, ".json"), do: "application/json", else: "text/csv",
          "text" => resource["content"]
        }
        {:ok, %{"contents" => [content]}, state}
    end
  end
  
  def handle_mcp_request("list_prompts", _params, state) do
    {:ok, %{"prompts" => state.prompts}, state}
  end
  
  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
  
  # Notification helpers
  
  defp simulate_file_processing(path, steps, progress_token) do
    Logger.info("Starting to process #{path} with #{steps} steps")
    
    Enum.each(1..steps, fn step ->
      # Simulate processing time
      Process.sleep(100)
      
      # Send progress notification to any subscribed services
      progress = step / steps
      ExMCP.Native.notify(:notification_hub, "progress_update", %{
        "token" => progress_token,
        "progress" => progress,
        "step" => step,
        "total" => steps,
        "path" => path
      })
      
      Logger.debug("Processing #{path}: #{step}/#{steps} (#{Float.round(progress * 100, 1)}%)")
    end)
    
    # Final completion notification
    ExMCP.Native.notify(:notification_hub, "processing_complete", %{
      "token" => progress_token,
      "path" => path,
      "steps" => steps
    })
    
    Logger.info("Completed processing #{path}")
  end
  
  defp notify_resource_changed(uri, subscribers) do
    Enum.each(subscribers, fn subscriber_id ->
      ExMCP.Native.notify(:notification_hub, "resource_changed", %{
        "subscriber" => subscriber_id,
        "uri" => uri,
        "type" => "modified",
        "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
      })
    end)
    
    Logger.info("Notified #{MapSet.size(subscribers)} subscribers of resource change: #{uri}")
  end
  
  defp notify_tools_changed(subscribers) do
    Enum.each(subscribers, fn subscriber_id ->
      ExMCP.Native.notify(:notification_hub, "tools_changed", %{
        "subscriber" => subscriber_id,
        "type" => "list_updated",
        "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
      })
    end)
    
    Logger.info("Notified #{MapSet.size(subscribers)} subscribers of tool list change")
  end
end

# Notification Hub Service - collects and logs all notifications
defmodule Examples.Advanced.NotificationHub do
  use ExMCP.Service, name: :notification_hub
  require Logger
  
  @impl true
  def init(_args) do
    {:ok, %{notification_count: 0}}
  end
  
  @impl true
  def handle_mcp_notification("progress_update", params, state) do
    Logger.info("PROGRESS: #{params["path"]} - #{params["step"]}/#{params["total"]} (#{Float.round(params["progress"] * 100, 1)}%)")
    {:noreply, %{state | notification_count: state.notification_count + 1}}
  end
  
  def handle_mcp_notification("processing_complete", params, state) do
    Logger.info("COMPLETED: #{params["path"]} processing finished after #{params["steps"]} steps")
    {:noreply, %{state | notification_count: state.notification_count + 1}}
  end
  
  def handle_mcp_notification("resource_changed", params, state) do
    Logger.info("RESOURCE CHANGED: #{params["uri"]} for subscriber #{params["subscriber"]}")
    {:noreply, %{state | notification_count: state.notification_count + 1}}
  end
  
  def handle_mcp_notification("tools_changed", params, state) do
    Logger.info("TOOLS CHANGED: #{params["type"]} for subscriber #{params["subscriber"]}")
    {:noreply, %{state | notification_count: state.notification_count + 1}}
  end
  
  @impl true
  def handle_mcp_request("get_stats", _params, state) do
    {:ok, %{"notification_count" => state.notification_count}, state}
  end
  
  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

# Demo module
defmodule Examples.Advanced.NotificationsDemo do
  require Logger
  
  def run do
    Logger.info("Starting Native Service Dispatcher Notifications Demo...")
    Logger.info("=" |> String.duplicate(60))
    
    # Start services
    {:ok, _} = Examples.Advanced.NotificationHub.start_link()
    {:ok, _} = Examples.Advanced.NotificationsService.start_link()
    
    # Wait for registration
    Process.sleep(100)
    
    Logger.info("\n=== Example 1: Subscribe to Notifications ===")
    {:ok, %{"content" => [%{"text" => msg}]}} = 
      ExMCP.Native.call(:notifications_service, "tools/call", %{
        "name" => "subscribe_notifications",
        "arguments" => %{"subscriber_id" => "demo_client"}
      })
    IO.puts(msg)
    
    Logger.info("\n=== Example 2: File Processing with Progress ===")
    {:ok, %{"content" => [%{"text" => msg}]}} = 
      ExMCP.Native.call(:notifications_service, "tools/call", %{
        "name" => "process_file",
        "arguments" => %{"path" => "/data/large_file.csv", "steps" => 5}
      })
    IO.puts(msg)
    
    # Wait for processing to complete
    Process.sleep(1000)
    
    Logger.info("\n=== Example 3: Resource Update Notification ===")
    {:ok, %{"content" => [%{"text" => msg}]}} = 
      ExMCP.Native.call(:notifications_service, "tools/call", %{
        "name" => "update_config",
        "arguments" => %{"key" => "theme", "value" => "light"}
      })
    IO.puts(msg)
    
    Logger.info("\n=== Example 4: Dynamic Tool Addition ===")
    {:ok, %{"content" => [%{"text" => msg}]}} = 
      ExMCP.Native.call(:notifications_service, "tools/call", %{
        "name" => "add_tool",
        "arguments" => %{"name" => "new_analyzer", "description" => "Dynamically added analyzer tool"}
      })
    IO.puts(msg)
    
    # Check notification hub stats
    {:ok, stats} = ExMCP.Native.call(:notification_hub, "get_stats", %{})
    Logger.info("\n=== Notification Hub Stats ===")
    IO.puts("Total notifications processed: #{stats["notification_count"]}")
    
    Logger.info("\n=== Demo Benefits ===")
    IO.puts("✓ Zero serialization overhead for notifications")
    IO.puts("✓ Fire-and-forget notifications with ExMCP.Native.notify")
    IO.puts("✓ Distributed notification patterns across services")
    IO.puts("✓ Sub-millisecond notification delivery")
    
    :ok
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/notifications_service.ex") do
  Examples.Advanced.NotificationsDemo.run()
  
  IO.puts("\nServices are running. Press Ctrl+C to stop.")
  Process.sleep(:infinity)
end