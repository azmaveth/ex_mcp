# Example: Using the Roots capability in ExMCP
#
# This example demonstrates how to use the roots capability
# which allows servers to expose URI-based resource boundaries.

defmodule RootsExample do
  @moduledoc """
  Example server that demonstrates the roots capability.
  
  Roots define entry points or boundaries in a server's resource space,
  similar to mount points in a filesystem.
  """
  
  defmodule Handler do
    use ExMCP.Server.Handler
    require Logger

    @impl true
    def init(_args) do
      {:ok, %{
        roots: [
          %{uri: "file:///home/user/documents", name: "Documents"},
          %{uri: "file:///home/user/projects", name: "Projects"},
          %{uri: "https://api.example.com/v1", name: "API Root"}
        ]
      }}
    end

    @impl true
    def handle_initialize(_params, state) do
      {:ok, %{
        name: "roots-example",
        version: "1.0.0",
        capabilities: %{
          roots: %{},  # Enable roots capability
          resources: %{}
        }
      }, state}
    end

    @impl true
    def handle_list_roots(state) do
      Logger.info("Client requested roots list")
      {:ok, state.roots, state}
    end

    @impl true
    def handle_list_resources(state) do
      # List resources under all roots
      resources = Enum.flat_map(state.roots, fn root ->
        case root.uri do
          "file://" <> _path ->
            # For file roots, we could list actual files
            [
              %{
                uri: "#{root.uri}/README.md",
                name: "README",
                mimeType: "text/markdown"
              }
            ]
          
          "https://" <> _url ->
            # For API roots, list available endpoints
            [
              %{
                uri: "#{root.uri}/users",
                name: "Users API",
                mimeType: "application/json"
              },
              %{
                uri: "#{root.uri}/posts", 
                name: "Posts API",
                mimeType: "application/json"
              }
            ]
          
          _ -> []
        end
      end)
      
      {:ok, resources, state}
    end

    @impl true
    def handle_read_resource(uri, state) do
      # Simple example - just return dummy content
      content = %{
        uri: uri,
        mimeType: "text/plain",
        text: "Content for resource: #{uri}"
      }
      {:ok, content, state}
    end

    # Dynamically update roots
    def add_root(server, root) do
      GenServer.cast(server, {:add_root, root})
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
    def handle_subscribe_resource(_uri, state), do: {:ok, %{}, state}
    @impl true
    def handle_unsubscribe_resource(_uri, state), do: {:ok, %{}, state}
    @impl true
    def handle_create_message(_params, state), do: {:error, "Not implemented", state}

    # Handle custom cast for adding roots
    @impl true
    def handle_cast({:add_root, root}, state) do
      new_roots = [root | state.roots]
      new_state = %{state | roots: new_roots}
      
      # Notify clients that roots have changed
      ExMCP.Server.notify_roots_changed(self())
      
      {:noreply, new_state}
    end
  end
end

# Usage example for clients
defmodule RootsClient do
  def demo do
    # Connect to a server
    {:ok, client} = ExMCP.Client.start_link(
      transport: :stdio,
      name: "roots-client"
    )

    # List available roots
    case ExMCP.Client.list_roots(client) do
      {:ok, roots} ->
        IO.puts("Available roots:")
        Enum.each(roots, fn root ->
          IO.puts("  - #{root.name}: #{root.uri}")
        end)
        
      {:error, reason} ->
        IO.puts("Failed to list roots: #{inspect(reason)}")
    end
  end
end

# To run as a server:
# {:ok, server} = ExMCP.Server.start_link(
#   handler: RootsExample.Handler,
#   transport: :stdio
# )

# To test adding a root dynamically:
# RootsExample.Handler.add_root(server, %{
#   uri: "s3://my-bucket/data",
#   name: "S3 Data"
# })