defmodule Examples.AdvancedFeatures.NotificationsServer do
  @moduledoc """
  Example MCP server demonstrating all notification types:
  - Progress notifications for long operations
  - Resource change notifications
  - Tool change notifications
  - Prompt change notifications
  
  NOTE: For Native Service Dispatcher examples, see notifications_service.ex
  which demonstrates the same features with much better performance.
  """
  
  use ExMCP.Server.Handler
  require Logger
  
  defmodule State do
    defstruct [
      resources: %{},
      tools: [],
      prompts: [],
      processing_tasks: %{}
    ]
  end
  
  @impl true
  def init(_args) do
    Logger.info("Notifications demo server starting...")
    
    initial_state = %State{
      resources: %{
        "file:///data/config.json" => %{
          name: "Configuration",
          description: "System configuration",
          content: Jason.encode!(%{theme: "dark", lang: "en"})
        },
        "file:///data/users.csv" => %{
          name: "User Data", 
          description: "User database export",
          content: "id,name,email\n1,Alice,alice@example.com\n2,Bob,bob@example.com"
        }
      },
      tools: [
        %{
          name: "process_file",
          description: "Process a file with progress tracking"
        },
        %{
          name: "update_config",
          description: "Update configuration and notify"
        }
      ],
      prompts: [
        %{
          name: "analyze_data",
          description: "Analyze data patterns"
        }
      ]
    }
    
    {:ok, initial_state}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    server_info = %{
      name: "notifications-demo",
      version: "1.0.0",
      capabilities: %{
        tools: %{},
        resources: %{},
        prompts: %{}
      }
    }
    
    {:ok, server_info, state}
  end
  
  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        name: "process_file",
        description: "Process a file with progress notifications",
        inputSchema: %{
          type: "object",
          properties: %{
            path: %{type: "string"},
            steps: %{type: "integer", default: 10}
          },
          required: ["path"]
        }
      },
      %{
        name: "update_config",
        description: "Update configuration (triggers resource notification)",
        inputSchema: %{
          type: "object",
          properties: %{
            key: %{type: "string"},
            value: %{type: "string"}
          },
          required: ["key", "value"]
        }
      },
      %{
        name: "add_tool",
        description: "Dynamically add a new tool (triggers tool notification)",
        inputSchema: %{
          type: "object",
          properties: %{
            name: %{type: "string"},
            description: %{type: "string"}
          },
          required: ["name", "description"]
        }
      },
      %{
        name: "add_prompt",
        description: "Add a new prompt template (triggers prompt notification)",
        inputSchema: %{
          type: "object",
          properties: %{
            name: %{type: "string"},
            template: %{type: "string"}
          },
          required: ["name", "template"]
        }
      },
      %{
        name: "bulk_update",
        description: "Update multiple resources (triggers multiple notifications)",
        inputSchema: %{
          type: "object",
          properties: %{
            count: %{type: "integer", default: 3}
          }
        }
      }
    ] ++ state.tools
    
    {:ok, tools, state}
  end
  
  @impl true
  def handle_call_tool("process_file", params, state) do
    path = params["path"]
    steps = params["steps"] || 10
    progress_token = get_in(params, ["_meta", "progressToken"])
    
    if progress_token do
      # Start async task with progress updates
      server_pid = self()
      
      task = Task.async(fn ->
        Logger.info("Starting file processing with progress tracking")
        
        for step <- 1..steps do
          # Simulate work
          Process.sleep(500)
          
          # Send progress notification
          ExMCP.Server.notify_progress(server_pid, progress_token, step, steps)
          
          Logger.debug("Progress: #{step}/#{steps}")
        end
        
        "Processed #{steps} chunks of #{path}"
      end)
      
      # Store task reference
      new_state = put_in(state.processing_tasks[progress_token], task)
      
      content = [%{
        type: "text",
        text: "Processing #{path} in #{steps} steps. Track progress with token: #{progress_token}"
      }]
      
      {:ok, content, new_state}
    else
      # No progress token, process synchronously
      Process.sleep(1000)
      content = [%{type: "text", text: "Processed #{path} (no progress tracking)"}]
      {:ok, content, state}
    end
  end
  
  def handle_call_tool("update_config", params, state) do
    key = params["key"]
    value = params["value"]
    
    # Update the config resource
    config_uri = "file:///data/config.json"
    config = state.resources[config_uri]
    
    # Parse and update config
    current_config = Jason.decode!(config.content)
    updated_config = Map.put(current_config, key, value)
    
    # Update state
    updated_resource = %{config | content: Jason.encode!(updated_config)}
    new_state = put_in(state.resources[config_uri], updated_resource)
    
    # Send resource updated notification
    server_pid = self()
    Task.start(fn ->
      Process.sleep(100)  # Small delay to simulate async update
      ExMCP.Server.notify_resource_updated(server_pid, config_uri)
      Logger.info("Sent resource update notification for #{config_uri}")
    end)
    
    content = [%{
      type: "text",
      text: "Updated config: #{key} = #{value}"
    }]
    
    {:ok, content, new_state}
  end
  
  def handle_call_tool("add_tool", params, state) do
    name = params["name"]
    description = params["description"]
    
    new_tool = %{
      name: "dynamic_#{name}",
      description: description
    }
    
    new_state = %{state | tools: [new_tool | state.tools]}
    
    # Send tools changed notification
    server_pid = self()
    Task.start(fn ->
      Process.sleep(100)
      ExMCP.Server.notify_tools_changed(server_pid)
      Logger.info("Sent tools changed notification")
    end)
    
    content = [%{
      type: "text",
      text: "Added new tool: dynamic_#{name}"
    }]
    
    {:ok, content, new_state}
  end
  
  def handle_call_tool("add_prompt", params, state) do
    name = params["name"]
    template = params["template"]
    
    new_prompt = %{
      name: "custom_#{name}",
      description: "Custom prompt: #{name}",
      template: template
    }
    
    new_state = %{state | prompts: [new_prompt | state.prompts]}
    
    # Send prompts changed notification
    server_pid = self()
    Task.start(fn ->
      Process.sleep(100)
      ExMCP.Server.notify_prompts_changed(server_pid)
      Logger.info("Sent prompts changed notification")
    end)
    
    content = [%{
      type: "text",
      text: "Added new prompt: custom_#{name}"
    }]
    
    {:ok, content, new_state}
  end
  
  def handle_call_tool("bulk_update", params, state) do
    count = params["count"] || 3
    
    # Simulate bulk updates with multiple notifications
    server_pid = self()
    
    Task.start(fn ->
      # First, notify resources list changed
      ExMCP.Server.notify_resources_changed(server_pid)
      Logger.info("Sent resources list changed notification")
      
      Process.sleep(500)
      
      # Then send individual resource updates
      for i <- 1..count do
        Process.sleep(300)
        uri = "file:///data/resource_#{i}.txt"
        ExMCP.Server.notify_resource_updated(server_pid, uri)
        Logger.info("Sent update notification for #{uri}")
      end
    end)
    
    content = [%{
      type: "text",
      text: "Started bulk update of #{count} resources. Watch for notifications!"
    }]
    
    {:ok, content, state}
  end
  
  def handle_call_tool(name, _params, state) do
    # Handle dynamically added tools
    if String.starts_with?(name, "dynamic_") do
      content = [%{
        type: "text",
        text: "Executed dynamic tool: #{name}"
      }]
      {:ok, content, state}
    else
      {:error, "Unknown tool: #{name}", state}
    end
  end
  
  @impl true
  def handle_list_resources(state) do
    resources = state.resources
    |> Enum.map(fn {uri, resource} ->
      %{
        uri: uri,
        name: resource.name,
        description: resource.description,
        mimeType: "text/plain"
      }
    end)
    
    {:ok, resources, state}
  end
  
  @impl true
  def handle_read_resource(uri, state) do
    case Map.get(state.resources, uri) do
      nil ->
        {:error, "Resource not found: #{uri}", state}
        
      resource ->
        content = %{
          uri: uri,
          mimeType: "text/plain",
          text: resource.content
        }
        {:ok, content, state}
    end
  end
  
  @impl true
  def handle_list_prompts(state) do
    {:ok, state.prompts, state}
  end
  
  @impl true
  def handle_get_prompt(name, _args, state) do
    prompt = Enum.find(state.prompts, fn p -> 
      p.name == name || p.name == "custom_#{name}"
    end)
    
    if prompt do
      messages = [
        %{
          role: "user",
          content: %{
            type: "text",
            text: prompt[:template] || "Analyze the following data..."
          }
        }
      ]
      {:ok, %{messages: messages}, state}
    else
      {:error, "Prompt not found: #{name}", state}
    end
  end
end

defmodule Examples.AdvancedFeatures.NotificationsClient do
  @moduledoc """
  Example client demonstrating notification handling.
  """
  
  require Logger
  
  def demo do
    Logger.info("Starting notifications demo with stdio transport...")
    
    # Start server with stdio transport
    {:ok, server} = ExMCP.Server.start_link(
      transport: :stdio,
      handler: Examples.AdvancedFeatures.NotificationsServer
    )
    
    # For demonstration purposes, we'll use the server directly
    # In a real scenario, you'd connect a client via stdio with a command
    client = server
    
    Process.sleep(100)
    
    # Example 1: Progress notifications
    Logger.info("\n=== Example 1: Progress Notifications ===")
    {:ok, _result} = ExMCP.Client.call_tool(
      client,
      "process_file",
      %{"path" => "/data/large.csv", "steps" => 5},
      progress_token: "file-process-123"
    )
    
    # Wait to see progress logs
    Process.sleep(3000)
    
    # Example 2: Resource update notification
    Logger.info("\n=== Example 2: Resource Update Notification ===")
    {:ok, _result} = ExMCP.Client.call_tool(
      client,
      "update_config",
      %{"key" => "theme", "value" => "light"}
    )
    
    Process.sleep(500)
    
    # Example 3: Tool change notification
    Logger.info("\n=== Example 3: Tool Change Notification ===")
    {:ok, _result} = ExMCP.Client.call_tool(
      client,
      "add_tool",
      %{"name" => "calculator", "description" => "Performs calculations"}
    )
    
    Process.sleep(500)
    
    # Verify new tool exists
    {:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client)
    dynamic_tools = Enum.filter(tools, fn t -> 
      String.starts_with?(t["name"], "dynamic_")
    end)
    Logger.info("Dynamic tools: #{inspect(Enum.map(dynamic_tools, & &1["name"]))}")
    
    # Example 4: Prompt change notification
    Logger.info("\n=== Example 4: Prompt Change Notification ===")
    {:ok, _result} = ExMCP.Client.call_tool(
      client,
      "add_prompt",
      %{"name" => "summarize", "template" => "Summarize the following text:"}
    )
    
    Process.sleep(500)
    
    # Example 5: Multiple notifications
    Logger.info("\n=== Example 5: Bulk Update (Multiple Notifications) ===")
    {:ok, _result} = ExMCP.Client.call_tool(
      client,
      "bulk_update",
      %{"count" => 3}
    )
    
    # Wait for all notifications
    Process.sleep(2000)
    
    # Cleanup
    GenServer.stop(client)
    GenServer.stop(server)
    
    Logger.info("\nNotifications demo completed!")
  end
end