#!/usr/bin/env elixir

# Progress Token and _meta Field Example
# 
# This example demonstrates how to use progress tokens and the _meta field
# to track long-running operations and pass metadata through MCP requests.

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule ProgressMetaHandler do
  use ExMCP.Server.Handler
  require Logger

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       protocolVersion: "2025-03-26",
       serverInfo: %{
         name: "progress-meta-demo",
         version: "1.0.0"
       },
       capabilities: %{
         tools: %{},
         resources: %{},
         prompts: %{}
       }
     }, state}
  end

  @impl true
  def handle_list_tools(cursor_or_params, state) do
    # Extract _meta if it was passed
    meta = extract_meta(cursor_or_params)
    
    if meta do
      Logger.info("list_tools received metadata: #{inspect(meta)}")
    end
    
    tools = [
      %{
        name: "long_task",
        description: "Simulates a long-running task with progress updates",
        inputSchema: %{
          type: "object",
          properties: %{
            duration: %{type: "integer", minimum: 1000, maximum: 10000},
            steps: %{type: "integer", minimum: 1, maximum: 10}
          },
          required: ["duration", "steps"]
        }
      },
      %{
        name: "metadata_echo",
        description: "Echoes back any metadata passed in _meta field",
        inputSchema: %{
          type: "object",
          properties: %{
            message: %{type: "string"}
          }
        }
      }
    ]

    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool(name, arguments, state) do
    # Extract _meta from arguments
    {meta, args} = Map.pop(arguments, "_meta")
    
    case name do
      "long_task" ->
        handle_long_task(args, meta, state)
        
      "metadata_echo" ->
        handle_metadata_echo(args, meta, state)
        
      _ ->
        {:error, "Unknown tool: #{name}", state}
    end
  end

  defp handle_long_task(%{"duration" => duration, "steps" => steps}, meta, state) do
    Logger.info("Starting long task: #{duration}ms, #{steps} steps")
    
    # Report progress if token provided
    if meta && meta["progressToken"] do
      token = meta["progressToken"]
      Logger.info("Progress token provided: #{token}")
      
      # Start async progress reporting
      Task.start(fn ->
        step_duration = div(duration, steps)
        
        for i <- 1..steps do
          Process.sleep(step_duration)
          # Note: Server.notify_progress would be used in real implementation
          Logger.info("[Progress #{token}] Step #{i}/#{steps}")
        end
      end)
    end
    
    # Return immediately with acknowledgment
    result = %{
      type: "text",
      text: "Started long task with #{steps} steps. Duration: #{duration}ms"
    }
    
    {:ok, [result], state}
  end
  
  defp handle_metadata_echo(%{"message" => message} = _args, meta, state) do
    response = %{
      type: "text",
      text: """
      Message: #{message}
      Metadata received: #{inspect(meta, pretty: true)}
      """
    }
    
    {:ok, [response], state}
  end
  
  defp handle_metadata_echo(_args, meta, state) do
    response = %{
      type: "text",
      text: "Metadata received: #{inspect(meta, pretty: true)}"
    }
    
    {:ok, [response], state}
  end
  
  defp extract_meta(nil), do: nil
  defp extract_meta(cursor) when is_binary(cursor), do: nil
  defp extract_meta(%{"_meta" => meta}), do: meta
  defp extract_meta(_), do: nil
end

# Start the demo
{:ok, server} = ExMCP.Server.start_link(
  transport: :stdio,
  handler: ProgressMetaHandler
)

Logger.info("Progress and _meta demo server started!")
Logger.info("This server demonstrates:")
Logger.info("1. Progress token support for long-running operations")
Logger.info("2. Arbitrary metadata passthrough via _meta field")
Logger.info("3. Backward compatible with clients that don't use _meta")

# Keep the server running
Process.sleep(:infinity)