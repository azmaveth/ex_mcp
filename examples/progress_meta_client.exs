#!/usr/bin/env elixir

# Client example for progress tokens and _meta field usage

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule ProgressMetaClient do
  require Logger
  
  def run do
    # Start a test server with our handler
    {:ok, server} = ExMCP.Server.start_link(
      transport: :beam,
      handler: ProgressMetaHandler,
      name: :progress_demo_server
    )
    
    # Connect a client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :progress_demo_server
    )
    
    # Wait for initialization
    Process.sleep(100)
    
    demo_progress_tokens(client)
    demo_metadata_passthrough(client)
    demo_list_operations_with_meta(client)
    
    Logger.info("\nDemo complete!")
  end
  
  defp demo_progress_tokens(client) do
    Logger.info("\n=== Progress Token Demo ===")
    
    # Call a long-running operation with progress token
    token = "operation-#{System.unique_integer([:positive])}"
    
    Logger.info("Calling long_task with progress token: #{token}")
    
    {:ok, result} = ExMCP.Client.call_tool(
      client,
      "long_task",
      %{
        "duration" => 3000,
        "steps" => 3
      },
      meta: %{"progressToken" => token}
    )
    
    Logger.info("Result: #{inspect(result.content |> hd |> Map.get(:text))}")
    
    # Also works with legacy :progress_token option
    Logger.info("\nUsing legacy :progress_token option:")
    {:ok, result2} = ExMCP.Client.call_tool(
      client,
      "long_task",
      %{
        "duration" => 2000,
        "steps" => 2
      },
      progress_token: "legacy-token-123"
    )
    
    Logger.info("Result: #{inspect(result2.content |> hd |> Map.get(:text))}")
  end
  
  defp demo_metadata_passthrough(client) do
    Logger.info("\n=== Metadata Passthrough Demo ===")
    
    # Pass arbitrary metadata
    meta = %{
      "requestId" => "req-#{System.unique_integer([:positive])}",
      "userId" => "user-123",
      "traceId" => "trace-abc-def",
      "customData" => %{
        "source" => "example",
        "priority" => "high"
      }
    }
    
    Logger.info("Calling metadata_echo with custom metadata")
    
    {:ok, result} = ExMCP.Client.call_tool(
      client,
      "metadata_echo",
      %{"message" => "Hello from client!"},
      meta: meta
    )
    
    Logger.info("Response:\n#{result.content |> hd |> Map.get(:text)}")
  end
  
  defp demo_list_operations_with_meta(client) do
    Logger.info("\n=== List Operations with Metadata ===")
    
    # List tools with metadata (useful for tracking/debugging)
    Logger.info("Listing tools with request metadata")
    {:ok, %{tools: tools}} = ExMCP.Client.list_tools(
      client,
      meta: %{"requestId" => "list-tools-123", "debug" => true}
    )
    
    Logger.info("Found #{length(tools)} tools")
    
    # List resources with metadata
    Logger.info("\nListing resources with metadata")
    {:ok, %{resources: resources}} = ExMCP.Client.list_resources(
      client, 
      meta: %{"requestId" => "list-resources-456"}
    )
    
    Logger.info("Found #{length(resources)} resources")
    
    # Get prompt with metadata
    Logger.info("\nGetting prompt with metadata")
    {:ok, prompts} = ExMCP.Client.list_prompts(
      client,
      meta: %{"requestId" => "list-prompts-789"}
    )
    
    Logger.info("Found #{length(prompts.prompts)} prompts")
  end
end

# Handler module (simplified version of the server example)
defmodule ProgressMetaHandler do
  use ExMCP.Server.Handler
  require Logger

  @impl true
  def init(_args), do: {:ok, %{}}

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       protocolVersion: "2025-03-26",
       serverInfo: %{name: "demo-server", version: "1.0.0"},
       capabilities: %{tools: %{}, resources: %{}, prompts: %{}}
     }, state}
  end

  @impl true
  def handle_list_tools(cursor_or_params, state) do
    tools = [
      %{
        name: "long_task",
        description: "Long-running task with progress",
        inputSchema: %{type: "object", properties: %{duration: %{type: "integer"}, steps: %{type: "integer"}}}
      },
      %{
        name: "metadata_echo",
        description: "Echoes metadata",
        inputSchema: %{type: "object", properties: %{message: %{type: "string"}}}
      }
    ]
    {:ok, tools, nil, state}
  end

  @impl true
  def handle_list_resources(_cursor_or_params, state) do
    {:ok, [], nil, state}
  end

  @impl true
  def handle_list_prompts(_cursor_or_params, state) do
    {:ok, [], nil, state}
  end

  @impl true
  def handle_call_tool("long_task", arguments, state) do
    {meta, args} = Map.pop(arguments, "_meta")
    
    if meta && meta["progressToken"] do
      Logger.info("Progress token received: #{meta["progressToken"]}")
    end
    
    result = %{
      type: "text",
      text: "Task started: #{args["steps"]} steps, #{args["duration"]}ms"
    }
    
    {:ok, [result], state}
  end

  def handle_call_tool("metadata_echo", arguments, state) do
    {meta, args} = Map.pop(arguments, "_meta")
    
    result = %{
      type: "text",
      text: """
      Message: #{args["message"]}
      Metadata: #{inspect(meta, pretty: true)}
      """
    }
    
    {:ok, [result], state}
  end
end

# Run the demo
ProgressMetaClient.run()