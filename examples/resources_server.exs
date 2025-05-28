#!/usr/bin/env elixir

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule ResourcesServer do
  @moduledoc """
  Example MCP server that demonstrates resource management.
  
  This example shows:
  - Serving various types of resources (text, JSON, binary)
  - Resource pagination
  - Resource templates for dynamic URIs
  - Resource subscriptions and update notifications
  - Different URI schemes (file://, config://, data://)
  
  ## Usage
  
      # Start the server
      ./examples/resources_server.exs
      
      # In another terminal, connect with a client
      ./examples/client.exs stdio
      
      # List available resources
      iex> {:ok, %{resources: resources}} = Client.list_resources(client)
      iex> Enum.map(resources, & &1.uri)
      ["config://app/settings.json", "file:///readme.md", ...]
      
      # Read a resource
      iex> {:ok, content} = Client.read_resource(client, "config://app/settings.json")
      iex> hd(content.contents).text
      "{\"theme\": \"dark\", \"language\": \"en\"}"
      
      # Subscribe to resource updates
      iex> Client.subscribe_resource(client, "data://metrics/cpu")
  """
  
  use ExMCP.Server.Handler
  require Logger

  @impl true
  def init(_args) do
    Logger.info("Resources server starting...")
    
    # Simulate some initial data
    initial_metrics = %{
      cpu: %{value: 45.2, unit: "%"},
      memory: %{value: 2048, unit: "MB"},
      disk: %{value: 67.8, unit: "%"}
    }
    
    {:ok, %{
      resources: build_resources(),
      subscriptions: %{},
      metrics: initial_metrics,
      start_time: DateTime.utc_now()
    }}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       protocolVersion: "2025-03-26",
       serverInfo: %{
         name: "resources-example-server",
         version: "1.0.0"
       },
       capabilities: %{
         resources: %{
           subscribe: true,
           listChanged: true
         }
       }
     }, state}
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    {:ok, [], nil, state}
  end

  @impl true
  def handle_call_tool(_name, _arguments, state) do
    {:error, "No tools available", state}
  end

  @impl true
  def handle_list_resources(cursor, state) do
    resources = state.resources
    page_size = 5
    
    {items, next_cursor} = 
      case cursor do
        nil ->
          {Enum.take(resources, page_size), 
           if(length(resources) > page_size, do: "page2", else: nil)}
           
        "page2" ->
          remaining = Enum.drop(resources, page_size)
          {Enum.take(remaining, page_size),
           if(length(remaining) > page_size, do: "page3", else: nil)}
           
        "page3" ->
          {Enum.drop(resources, page_size * 2), nil}
           
        _ ->
          {[], nil}
      end
    
    {:ok, items, next_cursor, state}
  end

  @impl true
  def handle_read_resource(uri, state) do
    case generate_resource_content(uri, state) do
      {:ok, content} ->
        {:ok, content, state}
        
      {:error, _} = error ->
        error
    end
  end

  @impl true
  def handle_subscribe_resource(uri, state) do
    # Check if resource exists
    if resource_exists?(uri, state) do
      # Add to subscriptions map
      client_pid = self()  # In real implementation, track actual client
      subscriptions = Map.update(state.subscriptions, uri, [client_pid], &[client_pid | &1])
      
      Logger.info("Client subscribed to #{uri}")
      {:ok, %{}, %{state | subscriptions: subscriptions}}
    else
      {:error, "Resource not found: #{uri}", state}
    end
  end

  @impl true
  def handle_unsubscribe_resource(uri, state) do
    client_pid = self()
    subscriptions = Map.update(state.subscriptions, uri, [], &List.delete(&1, client_pid))
    
    Logger.info("Client unsubscribed from #{uri}")
    {:ok, %{}, %{state | subscriptions: subscriptions}}
  end

  @impl true
  def handle_list_resource_templates(state) do
    templates = [
      %{
        uriTemplate: "file:///{path}",
        name: "Local Files",
        description: "Access local files by path",
        mimeType: "text/plain"
      },
      %{
        uriTemplate: "config://{app}/{setting}",
        name: "Configuration",
        description: "Application configuration values",
        mimeType: "application/json"
      },
      %{
        uriTemplate: "data://metrics/{metric}",
        name: "Live Metrics",
        description: "Real-time system metrics",
        mimeType: "application/json"
      },
      %{
        uriTemplate: "db://{table}/{id}",
        name: "Database Records",
        description: "Access database records by table and ID",
        mimeType: "application/json"
      }
    ]
    
    {:ok, templates, state}
  end

  # Start a metrics updater (for demonstration)
  def start_metrics_updater(server_pid) do
    spawn_link(fn ->
      Process.sleep(5000)  # Wait 5 seconds before starting
      update_metrics_loop(server_pid)
    end)
  end

  defp update_metrics_loop(server_pid) do
    # Update CPU metric
    new_cpu = 30 + :rand.uniform() * 40  # Random between 30-70%
    GenServer.cast(server_pid, {:update_metric, :cpu, new_cpu})
    
    Process.sleep(3000)  # Update every 3 seconds
    update_metrics_loop(server_pid)
  end

  @impl true
  def handle_cast({:update_metric, metric, value}, state) do
    # Update the metric
    new_metrics = put_in(state.metrics[metric].value, value)
    new_state = %{state | metrics: new_metrics}
    
    # Notify subscribers
    uri = "data://metrics/#{metric}"
    if Map.has_key?(state.subscriptions, uri) do
      ExMCP.Server.notify_resource_updated(self(), uri)
      Logger.info("Notified subscribers of #{uri} update: #{value}")
    end
    
    {:noreply, new_state}
  end

  # Private functions

  defp build_resources do
    [
      # Configuration resources
      %{
        uri: "config://app/settings.json",
        name: "Application Settings",
        description: "Main application configuration",
        mimeType: "application/json"
      },
      %{
        uri: "config://app/features.json",
        name: "Feature Flags",
        description: "Feature toggle configuration",
        mimeType: "application/json"
      },
      
      # File resources
      %{
        uri: "file:///readme.md",
        name: "README",
        description: "Project documentation",
        mimeType: "text/markdown"
      },
      %{
        uri: "file:///logo.svg",
        name: "Logo",
        description: "Application logo",
        mimeType: "image/svg+xml"
      },
      
      # Data resources
      %{
        uri: "data://metrics/cpu",
        name: "CPU Usage",
        description: "Current CPU utilization",
        mimeType: "application/json"
      },
      %{
        uri: "data://metrics/memory",
        name: "Memory Usage",
        description: "Current memory utilization",
        mimeType: "application/json"
      },
      %{
        uri: "data://metrics/disk",
        name: "Disk Usage",
        description: "Current disk utilization",
        mimeType: "application/json"
      },
      
      # Database resources
      %{
        uri: "db://users/1",
        name: "User Record",
        description: "Sample user data",
        mimeType: "application/json"
      },
      %{
        uri: "db://products/catalog",
        name: "Product Catalog",
        description: "Product listing",
        mimeType: "application/json"
      }
    ]
  end

  defp resource_exists?(uri, state) do
    Enum.any?(state.resources, & &1.uri == uri) or
    String.starts_with?(uri, "data://metrics/")  # Dynamic metrics
  end

  defp generate_resource_content(uri, state) do
    case uri do
      "config://app/settings.json" ->
        content = %{
          uri: uri,
          mimeType: "application/json",
          text: Jason.encode!(%{
            theme: "dark",
            language: "en",
            autoSave: true,
            debugMode: false
          })
        }
        {:ok, content, state}
        
      "config://app/features.json" ->
        content = %{
          uri: uri,
          mimeType: "application/json",
          text: Jason.encode!(%{
            newUI: true,
            betaFeatures: false,
            analytics: true
          })
        }
        {:ok, content, state}
        
      "file:///readme.md" ->
        content = %{
          uri: uri,
          mimeType: "text/markdown",
          text: """
          # Resources Example Server
          
          This server demonstrates MCP resource capabilities:
          
          - Multiple resource types
          - Dynamic content generation
          - Resource subscriptions
          - Live updates
          
          ## Available Resources
          
          - Configuration files
          - System metrics
          - Database records
          """
        }
        {:ok, content, state}
        
      "file:///logo.svg" ->
        # For demo, return a simple SVG
        svg_data = """
        <svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">
          <circle cx="50" cy="50" r="40" fill="blue"/>
          <text x="50" y="55" text-anchor="middle" fill="white">MCP</text>
        </svg>
        """
        content = %{
          uri: uri,
          mimeType: "image/svg+xml",
          blob: Base.encode64(svg_data)  # Binary resources use base64
        }
        {:ok, content, state}
        
      "data://metrics/" <> metric_name ->
        case Map.get(state.metrics, String.to_existing_atom(metric_name)) do
          nil ->
            {:error, "Unknown metric: #{metric_name}", state}
            
          metric_data ->
            content = %{
              uri: uri,
              mimeType: "application/json",
              text: Jason.encode!(%{
                metric: metric_name,
                value: metric_data.value,
                unit: metric_data.unit,
                timestamp: DateTime.utc_now()
              })
            }
            {:ok, content, state}
        end
        
      "db://users/1" ->
        content = %{
          uri: uri,
          mimeType: "application/json",
          text: Jason.encode!(%{
            id: 1,
            name: "Alice Smith",
            email: "alice@example.com",
            role: "admin"
          })
        }
        {:ok, content, state}
        
      "db://products/catalog" ->
        products = [
          %{id: 1, name: "Widget", price: 9.99, stock: 100},
          %{id: 2, name: "Gadget", price: 19.99, stock: 50},
          %{id: 3, name: "Doohickey", price: 5.99, stock: 200}
        ]
        content = %{
          uri: uri,
          mimeType: "application/json",
          text: Jason.encode!(%{products: products})
        }
        {:ok, content, state}
        
      _ ->
        {:error, "Resource not found: #{uri}", state}
    end
  end
end

# Start the server
{:ok, server} = ExMCP.Server.start_link(
  transport: :stdio,
  handler: ResourcesServer
)

# Start the metrics updater
ResourcesServer.start_metrics_updater(server)

Logger.info("Resources server started. Waiting for connections...")
Logger.info("Metrics will start updating in 5 seconds...")

# Keep the process alive
Process.sleep(:infinity)