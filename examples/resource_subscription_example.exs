# Example: Resource Subscriptions in ExMCP
#
# This example demonstrates how to use resource subscriptions
# to monitor changes to specific resources.

defmodule ResourceSubscriptionExample do
  @moduledoc """
  Example server that demonstrates resource subscription capabilities.
  
  Clients can subscribe to specific resources and receive notifications
  when those resources change.
  """
  
  defmodule Handler do
    use ExMCP.Server.Handler
    require Logger

    @impl true
    def init(_args) do
      # Start with some initial resources and no subscriptions
      {:ok, %{
        resources: %{
          "file:///config.json" => %{
            content: ~s({"version": "1.0.0", "debug": false}),
            mimeType: "application/json",
            subscribers: MapSet.new()
          },
          "file:///status.txt" => %{
            content: "System status: OK",
            mimeType: "text/plain",
            subscribers: MapSet.new()
          }
        },
        client_subscriptions: %{}  # Track which client subscribed to what
      }}
    end

    @impl true
    def handle_initialize(_params, state) do
      {:ok, %{
        name: "subscription-example",
        version: "1.0.0",
        capabilities: %{
          resources: %{
            subscribe: true  # Enable subscription support
          }
        }
      }, state}
    end

    @impl true
    def handle_list_resources(state) do
      resources = Enum.map(state.resources, fn {uri, resource} ->
        %{
          uri: uri,
          name: Path.basename(uri),
          mimeType: resource.mimeType
        }
      end)
      
      {:ok, resources, state}
    end

    @impl true
    def handle_read_resource(uri, state) do
      case Map.get(state.resources, uri) do
        nil ->
          {:error, "Resource not found", state}
          
        resource ->
          content = %{
            uri: uri,
            mimeType: resource.mimeType,
            text: resource.content
          }
          {:ok, content, state}
      end
    end

    @impl true
    def handle_subscribe_resource(uri, state) do
      Logger.info("Client subscribing to resource: #{uri}")
      
      case Map.get(state.resources, uri) do
        nil ->
          {:error, "Resource not found", state}
          
        resource ->
          # In a real implementation, you'd track the client ID
          client_id = self()  # Simplified for example
          
          # Add subscriber to resource
          updated_resource = %{resource | 
            subscribers: MapSet.put(resource.subscribers, client_id)
          }
          
          # Update state
          new_resources = Map.put(state.resources, uri, updated_resource)
          
          # Track subscription for this client
          client_subs = Map.get(state.client_subscriptions, client_id, MapSet.new())
          new_client_subs = MapSet.put(client_subs, uri)
          new_client_subscriptions = Map.put(state.client_subscriptions, client_id, new_client_subs)
          
          new_state = %{state | 
            resources: new_resources,
            client_subscriptions: new_client_subscriptions
          }
          
          {:ok, %{}, new_state}
      end
    end

    @impl true
    def handle_unsubscribe_resource(uri, state) do
      Logger.info("Client unsubscribing from resource: #{uri}")
      
      case Map.get(state.resources, uri) do
        nil ->
          {:error, "Resource not found", state}
          
        resource ->
          client_id = self()
          
          # Remove subscriber from resource
          updated_resource = %{resource | 
            subscribers: MapSet.delete(resource.subscribers, client_id)
          }
          
          # Update state
          new_resources = Map.put(state.resources, uri, updated_resource)
          
          # Update client subscriptions
          client_subs = Map.get(state.client_subscriptions, client_id, MapSet.new())
          new_client_subs = MapSet.delete(client_subs, uri)
          
          new_client_subscriptions = 
            if MapSet.size(new_client_subs) == 0 do
              Map.delete(state.client_subscriptions, client_id)
            else
              Map.put(state.client_subscriptions, client_id, new_client_subs)
            end
          
          new_state = %{state | 
            resources: new_resources,
            client_subscriptions: new_client_subscriptions
          }
          
          {:ok, %{}, new_state}
      end
    end

    # Function to update a resource and notify subscribers
    def update_resource(server, uri, new_content) do
      GenServer.cast(server, {:update_resource, uri, new_content})
    end

    @impl true
    def handle_cast({:update_resource, uri, new_content}, state) do
      case Map.get(state.resources, uri) do
        nil ->
          {:noreply, state}
          
        resource ->
          # Update the resource content
          updated_resource = %{resource | content: new_content}
          new_resources = Map.put(state.resources, uri, updated_resource)
          new_state = %{state | resources: new_resources}
          
          # Notify all subscribers
          if MapSet.size(resource.subscribers) > 0 do
            Logger.info("Notifying #{MapSet.size(resource.subscribers)} subscribers of change to #{uri}")
            ExMCP.Server.notify_resource_updated(self(), uri)
          end
          
          {:noreply, new_state}
      end
    end

    # Required callbacks
    @impl true
    def handle_list_tools(state), do: {:ok, [], state}
    @impl true
    def handle_call_tool(_name, _args, state), do: {:error, "Not implemented", state}
    @impl true
    def handle_list_prompts(state), do: {:ok, [], state}
    @impl true
    def handle_get_prompt(_name, _args, state), do: {:error, "Not implemented", state}
    @impl true
    def handle_list_roots(state), do: {:ok, [], state}
    @impl true
    def handle_create_message(_params, state), do: {:error, "Not implemented", state}
  end
end

# Client usage example
defmodule SubscriptionClient do
  def demo do
    {:ok, client} = ExMCP.Client.start_link(
      transport: :stdio,
      name: "subscription-client"
    )

    # Subscribe to a resource
    uri = "file:///config.json"
    
    case ExMCP.Client.subscribe_resource(client, uri) do
      {:ok, _} ->
        IO.puts("Successfully subscribed to #{uri}")
        
        # In a real app, you'd handle notifications in the client's
        # message handler for "notifications/resource/updated"
        
      {:error, reason} ->
        IO.puts("Failed to subscribe: #{inspect(reason)}")
    end

    # Later, unsubscribe
    case ExMCP.Client.unsubscribe_resource(client, uri) do
      {:ok, _} ->
        IO.puts("Successfully unsubscribed from #{uri}")
        
      {:error, reason} ->
        IO.puts("Failed to unsubscribe: #{inspect(reason)}")
    end
  end
end

# To test:
# 1. Start the server
# {:ok, server} = ExMCP.Server.start_link(
#   handler: ResourceSubscriptionExample.Handler,
#   transport: :stdio
# )
#
# 2. Client subscribes to a resource
# 
# 3. Update the resource to trigger notifications
# ResourceSubscriptionExample.Handler.update_resource(
#   server, 
#   "file:///config.json",
#   ~s({"version": "1.0.1", "debug": true})
# )