#!/usr/bin/env elixir

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule ProgressServer do
  @moduledoc """
  Example MCP server demonstrating progress notifications.
  
  This shows how to:
  - Report progress for long-running operations
  - Use progress tokens from client requests
  - Send incremental updates with current/total values
  - Support different progress tracking patterns
  
  ## Usage
  
      # Start the server
      ./examples/progress_demo.exs
      
      # In another terminal, use the client example
      # or connect with any MCP client that supports progress
  """
  
  use ExMCP.Server.Handler
  require Logger

  @impl true
  def init(_args) do
    Logger.info("Progress demo server starting...")
    {:ok, %{}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       protocolVersion: "2025-03-26",
       serverInfo: %{
         name: "progress-demo-server",
         version: "1.0.0"
       },
       capabilities: %{
         tools: %{}
       }
     }, state}
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "analyze_data",
        description: "Analyze large dataset with progress tracking",
        inputSchema: %{
          type: "object",
          properties: %{
            dataset: %{type: "string"},
            rows: %{type: "integer", minimum: 100, maximum: 10000}
          },
          required: ["dataset", "rows"]
        }
      },
      %{
        name: "train_model",
        description: "Train ML model with epoch progress",
        inputSchema: %{
          type: "object",
          properties: %{
            epochs: %{type: "integer", minimum: 1, maximum: 100},
            batch_size: %{type: "integer", default: 32}
          },
          required: ["epochs"]
        }
      },
      %{
        name: "export_report",
        description: "Export report with multiple phases",
        inputSchema: %{
          type: "object",
          properties: %{
            format: %{type: "string", enum: ["pdf", "html", "csv"]},
            sections: %{type: "integer", minimum: 1, maximum: 20}
          },
          required: ["format", "sections"]
        }
      },
      %{
        name: "sync_database",
        description: "Sync database tables with progress",
        inputSchema: %{
          type: "object",
          properties: %{
            tables: %{
              type: "array",
              items: %{type: "string"},
              minItems: 1
            }
          },
          required: ["tables"]
        }
      }
    ]
    
    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool(name, arguments, state) do
    # Extract progress token
    progress_token = get_in(arguments, ["_meta", "progressToken"])
    
    Logger.info("Tool call: #{name} with progress token: #{inspect(progress_token)}")
    
    case name do
      "analyze_data" -> analyze_data(arguments, progress_token, state)
      "train_model" -> train_model(arguments, progress_token, state)
      "export_report" -> export_report(arguments, progress_token, state)
      "sync_database" -> sync_database(arguments, progress_token, state)
      _ -> {:error, "Unknown tool: #{name}", state}
    end
  end

  defp analyze_data(%{"dataset" => dataset, "rows" => rows}, progress_token, state) do
    if progress_token do
      Task.start(fn ->
        # Simulate data analysis in chunks
        chunk_size = max(div(rows, 20), 10)
        chunks = div(rows + chunk_size - 1, chunk_size)
        
        for i <- 1..chunks do
          Process.sleep(200)  # Simulate processing time
          processed = min(i * chunk_size, rows)
          
          ExMCP.Server.notify_progress(self(), progress_token, processed, rows)
          Logger.info("Progress [#{progress_token}]: Analyzed #{processed}/#{rows} rows")
        end
      end)
    end
    
    {:ok, [%{
      type: "text",
      text: "Started analyzing #{dataset} with #{rows} rows. Progress updates will be sent."
    }], state}
  end

  defp train_model(%{"epochs" => epochs} = args, progress_token, state) do
    batch_size = Map.get(args, "batch_size", 32)
    
    if progress_token do
      Task.start(fn ->
        for epoch <- 1..epochs do
          # Simulate epoch training time
          Process.sleep(1000)
          
          # Report epoch completion
          ExMCP.Server.notify_progress(self(), progress_token, epoch, epochs)
          Logger.info("Progress [#{progress_token}]: Completed epoch #{epoch}/#{epochs}")
          
          # Simulate batch progress within epoch (optional sub-progress)
          batches = 10
          for batch <- 1..batches do
            Process.sleep(50)
            # Could send more granular progress here if needed
          end
        end
      end)
    end
    
    {:ok, [%{
      type: "text",
      text: "Started training model for #{epochs} epochs with batch size #{batch_size}"
    }], state}
  end

  defp export_report(%{"format" => format, "sections" => sections}, progress_token, state) do
    if progress_token do
      Task.start(fn ->
        phases = [
          {"Collecting data", 0.2},
          {"Processing data", 0.3},
          {"Generating #{format}", 0.3},
          {"Finalizing", 0.2}
        ]
        
        current_progress = 0
        
        for {phase, weight} <- phases do
          Logger.info("Progress [#{progress_token}]: #{phase}")
          
          # Simulate phase work
          steps = round(sections * weight)
          for i <- 1..max(steps, 1) do
            Process.sleep(100)
            current_progress = current_progress + (100.0 * weight / max(steps, 1))
            
            ExMCP.Server.notify_progress(
              self(), 
              progress_token, 
              round(current_progress), 
              100
            )
          end
        end
        
        # Ensure we report 100% completion
        ExMCP.Server.notify_progress(self(), progress_token, 100, 100)
      end)
    end
    
    {:ok, [%{
      type: "text",
      text: "Exporting #{sections}-section report as #{format}. This will take several phases."
    }], state}
  end

  defp sync_database(%{"tables" => tables}, progress_token, state) do
    total_tables = length(tables)
    
    if progress_token and total_tables > 0 do
      Task.start(fn ->
        tables
        |> Enum.with_index(1)
        |> Enum.each(fn {table, index} ->
          Logger.info("Progress [#{progress_token}]: Syncing table #{table}")
          
          # Simulate table sync with variable time
          sync_time = 500 + :rand.uniform(1000)
          Process.sleep(sync_time)
          
          ExMCP.Server.notify_progress(self(), progress_token, index, total_tables)
          Logger.info("Progress [#{progress_token}]: Completed #{index}/#{total_tables} tables")
        end)
      end)
    end
    
    {:ok, [%{
      type: "text",
      text: "Started syncing #{total_tables} database tables: #{Enum.join(tables, ", ")}"
    }], state}
  end
end

# Start the server
{:ok, _server} = ExMCP.Server.start_link(
  transport: :stdio,
  handler: ProgressServer
)

Logger.info("""
Progress demo server started!

This server demonstrates progress tracking for long-running operations.
Each tool will send progress notifications if a progress token is provided.

Try these example tool calls:
1. analyze_data - Processes data in chunks
2. train_model - Reports epoch-by-epoch progress
3. export_report - Multi-phase operation with weighted progress
4. sync_database - Table-by-table sync progress

Connect with an MCP client to see progress updates in real-time.
""")

# Keep the process alive
Process.sleep(:infinity)