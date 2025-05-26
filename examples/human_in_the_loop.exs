#!/usr/bin/env elixir

# Example: Human-in-the-loop approval for sensitive operations

Mix.install([
  {:ex_mcp, path: "."}
])

defmodule SensitiveToolsServer do
  @behaviour ExMCP.Server.Handler

  @impl true
  def init(_args) do
    {:ok, %{files: %{"data.txt" => "Important data", "config.json" => "{\"key\": \"value\"}"}}}
  end

  @impl true
  def handle_initialize(_params, state) do
    server_info = %{
      "name" => "sensitive-tools-server",
      "version" => "1.0.0",
      "capabilities" => %{
        "tools" => %{},
        "sampling" => %{}
      }
    }
    {:ok, server_info, state}
  end

  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        "name" => "read_file",
        "description" => "Read file contents",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "filename" => %{"type" => "string"}
          },
          "required" => ["filename"]
        }
      },
      %{
        "name" => "delete_file",
        "description" => "Delete a file (DESTRUCTIVE)",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "filename" => %{"type" => "string"}
          },
          "required" => ["filename"]
        },
        "destructiveHint" => true
      },
      %{
        "name" => "analyze_with_ai",
        "description" => "Analyze content with AI",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "content" => %{"type" => "string"}
          },
          "required" => ["content"]
        }
      }
    ]
    {:ok, tools, state}
  end

  @impl true
  def handle_call_tool("read_file", %{"filename" => filename}, state) do
    case Map.get(state.files, filename) do
      nil -> {:error, "File not found", state}
      content -> {:ok, %{"content" => [%{"type" => "text", "text" => content}]}, state}
    end
  end

  def handle_call_tool("delete_file", %{"filename" => filename}, state) do
    if Map.has_key?(state.files, filename) do
      new_files = Map.delete(state.files, filename)
      {:ok, %{"content" => [%{"type" => "text", "text" => "File deleted"}]}, %{state | files: new_files}}
    else
      {:error, "File not found", state}
    end
  end

  def handle_call_tool("analyze_with_ai", %{"content" => content}, state) do
    # Server requests client to analyze with AI
    messages = [
      %{"role" => "system", "content" => "You are a helpful assistant analyzing data."},
      %{"role" => "user", "content" => "Please analyze: #{content}"}
    ]
    
    case ExMCP.Server.create_message(self(), %{"messages" => messages}) do
      {:ok, response} ->
        analysis = response["content"]["text"]
        {:ok, %{"content" => [%{"type" => "text", "text" => analysis}]}, state}
      {:error, error} ->
        {:error, "Analysis failed: #{inspect(error)}", state}
    end
  end

  # Other required callbacks
  def handle_list_resources(state), do: {:ok, [], state}
  def handle_read_resource(_uri, state), do: {:error, "Not found", state}
  def handle_list_prompts(state), do: {:ok, [], state}
  def handle_get_prompt(_name, _args, state), do: {:error, "Not found", state}
  def handle_list_resource_templates(state), do: {:ok, [], state}
  def handle_complete(_ref, _arg, state), do: {:ok, %{}, state}
  def terminate(_reason, _state), do: :ok
end

# Custom approval handler with rules
defmodule CustomApprovalHandler do
  @behaviour ExMCP.Approval

  @impl true
  def request_approval(type, data, _opts) do
    case type do
      :sampling ->
        IO.puts("\n🤖 AI SAMPLING REQUEST")
        IO.puts("Messages: #{inspect(data["messages"], pretty: true)}")
        IO.write("Allow AI to process this request? [y/n]: ")
        
        case IO.gets("") |> String.trim() do
          "y" -> {:approved, data}
          _ -> {:denied, "User denied AI sampling"}
        end
      
      :response ->
        IO.puts("\n📤 AI RESPONSE")
        IO.puts("Response: #{data["content"]["text"]}")
        IO.write("Send this response? [y/n]: ")
        
        case IO.gets("") |> String.trim() do
          "y" -> {:approved, data}
          _ -> {:denied, "User blocked response"}
        end
        
      :tool_call ->
        tool_name = data["name"]
        
        # Auto-approve safe operations
        if tool_name == "read_file" do
          IO.puts("\n✅ Auto-approved: #{tool_name}")
          {:approved, data}
        else
          IO.puts("\n⚠️  TOOL CALL REQUEST: #{tool_name}")
          IO.puts("Arguments: #{inspect(data["arguments"])}")
          IO.write("Execute this tool? [y/n]: ")
          
          case IO.gets("") |> String.trim() do
            "y" -> {:approved, data}
            _ -> {:denied, "User denied tool execution"}
          end
        end
        
      _ ->
        {:approved, data}
    end
  end
end

defmodule HITLExample do
  def run do
    IO.puts("=== Human-in-the-Loop Example ===")
    IO.puts("This example demonstrates approval flows for sensitive operations\n")

    # Start server
    {:ok, server} = ExMCP.Server.start_link(
      transport: :beam,
      name: :hitl_server,
      handler: SensitiveToolsServer
    )

    # Choose approval handler
    IO.write("Use console approval handler? [y/n]: ")
    use_console = IO.gets("") |> String.trim() == "y"
    
    approval_handler = if use_console do
      IO.puts("Using built-in console approval handler")
      ExMCP.Approval.Console
    else
      IO.puts("Using custom approval handler")
      CustomApprovalHandler
    end

    # Start client with approval handler
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :hitl_server,
      handler: {ExMCP.Client.DefaultHandler, [
        approval_handler: approval_handler,
        roots: [%{uri: "file:///secure", name: "Secure Data"}]
      ]}
    )

    Process.sleep(100)

    # Test scenarios
    IO.puts("\n1. Safe operation (should auto-approve with custom handler):")
    case ExMCP.Client.call_tool(client, "read_file", %{"filename" => "data.txt"}) do
      {:ok, result} -> IO.puts("   Result: #{inspect(result)}")
      {:error, error} -> IO.puts("   Error: #{inspect(error)}")
    end

    IO.puts("\n2. Destructive operation (requires approval):")
    case ExMCP.Client.call_tool(client, "delete_file", %{"filename" => "config.json"}) do
      {:ok, result} -> IO.puts("   Result: #{inspect(result)}")
      {:error, error} -> IO.puts("   Error: #{inspect(error)}")
    end

    IO.puts("\n3. AI analysis (requires approval for both request and response):")
    case ExMCP.Client.call_tool(client, "analyze_with_ai", %{"content" => "Sales data: Q1=$1M, Q2=$1.5M"}) do
      {:ok, result} -> IO.puts("   Result: #{inspect(result)}")
      {:error, error} -> IO.puts("   Error: #{inspect(error)}")
    end

    # Cleanup
    Process.sleep(100)
    ExMCP.Client.stop(client)
    GenServer.stop(server)
    
    IO.puts("\n=== Example completed ===")
  end
end

HITLExample.run()