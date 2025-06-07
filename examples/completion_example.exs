#!/usr/bin/env elixir

# Completion Example
# Demonstrates the completion/complete endpoint for argument autocompletion

defmodule CompletionExample do
  @moduledoc """
  Example MCP server that demonstrates argument completion.
  
  This server provides autocompletion for:
  - File paths
  - Tool argument values
  - Resource URIs
  - Prompt parameters
  """
  
  use ExMCP.Server.Handler
  
  @impl true
  def init(_args) do
    state = %{
      # Simulated file system for completion
      files: [
        "/home/user/documents/report.txt",
        "/home/user/documents/invoice.pdf",
        "/home/user/downloads/image.png",
        "/home/user/downloads/archive.zip",
        "/home/user/projects/mcp/README.md",
        "/home/user/projects/mcp/lib/server.ex",
        "/etc/config.yml",
        "/etc/hosts",
        "/var/log/system.log",
        "/var/log/app.log"
      ],
      # Available colors for color picker tool
      colors: ["red", "green", "blue", "yellow", "orange", "purple", "pink", "black", "white"],
      # Available languages
      languages: ["en", "es", "fr", "de", "it", "pt", "zh", "ja", "ko", "ar"]
    }
    
    {:ok, state}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    result = %{
      protocolVersion: "2025-03-26",
      serverInfo: %{
        name: "completion-example-server",
        version: "1.0.0",
        description: "Demonstrates argument completion capabilities"
      },
      capabilities: %{
        tools: %{},
        resources: %{},
        prompts: %{},
        # Advertise completion support
        completion: %{
          hasArguments: true,
          values: true
        }
      }
    }
    
    {:ok, result, state}
  end
  
  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "read_file",
        description: "Read contents of a file",
        inputSchema: %{
          type: "object",
          properties: %{
            path: %{
              type: "string",
              description: "File path to read"
            }
          },
          required: ["path"]
        }
      },
      %{
        name: "set_color",
        description: "Set a color preference",
        inputSchema: %{
          type: "object",
          properties: %{
            color: %{
              type: "string",
              enum: state.colors,
              description: "Color to set"
            }
          },
          required: ["color"]
        }
      },
      %{
        name: "translate",
        description: "Translate text to another language",
        inputSchema: %{
          type: "object",
          properties: %{
            text: %{type: "string"},
            to: %{
              type: "string",
              description: "Target language code"
            }
          },
          required: ["text", "to"]
        }
      }
    ]
    
    {:ok, tools, nil, state}
  end
  
  @impl true
  def handle_complete(ref, params, state) do
    case ref do
      "argument" ->
        handle_argument_completion(params, state)
      
      "resource" ->
        handle_resource_completion(params, state)
      
      _ ->
        {:ok, %{completion: []}, state}
    end
  end
  
  defp handle_argument_completion(%{"name" => name, "value" => value} = params, state) do
    # Get the tool name from params if provided
    tool_name = params["tool"]
    
    completions = case {tool_name, name} do
      {"read_file", "path"} ->
        # Complete file paths
        complete_file_path(value, state.files)
      
      {"set_color", "color"} ->
        # Complete color names
        complete_from_list(value, state.colors)
      
      {"translate", "to"} ->
        # Complete language codes
        complete_from_list(value, state.languages)
      
      _ ->
        # Default: try file path completion
        complete_file_path(value, state.files)
    end
    
    {:ok, %{completion: completions}, state}
  end
  
  defp handle_resource_completion(%{"uri" => partial_uri}, state) do
    # Complete resource URIs
    resources = [
      "file:///home/user/documents/",
      "file:///var/log/",
      "http://example.com/api/",
      "config://settings/",
      "data://cache/"
    ]
    
    completions = 
      resources
      |> Enum.filter(&String.starts_with?(&1, partial_uri))
      |> Enum.take(10)
    
    {:ok, %{completion: completions}, state}
  end
  
  defp complete_file_path(partial, files) do
    files
    |> Enum.filter(&String.starts_with?(&1, partial))
    |> Enum.sort()
    |> Enum.take(10)
  end
  
  defp complete_from_list(partial, items) do
    items
    |> Enum.filter(&String.starts_with?(&1, partial))
    |> Enum.sort()
    |> Enum.take(10)
  end
  
  @impl true
  def handle_call_tool("read_file", %{"path" => path}, state) do
    if path in state.files do
      {:ok, %{content: "Simulated content of #{path}"}, state}
    else
      {:error, "File not found: #{path}", state}
    end
  end
  
  @impl true
  def handle_call_tool("set_color", %{"color" => color}, state) do
    if color in state.colors do
      {:ok, %{message: "Color set to #{color}"}, state}
    else
      {:error, "Invalid color: #{color}", state}
    end
  end
  
  @impl true
  def handle_call_tool("translate", %{"text" => text, "to" => lang}, state) do
    if lang in state.languages do
      {:ok, %{
        original: text,
        translated: "[#{lang}] #{text}",
        language: lang
      }, state}
    else
      {:error, "Unsupported language: #{lang}", state}
    end
  end
  
  @impl true
  def handle_call_tool(_name, _args, state) do
    {:error, "Unknown tool", state}
  end
  
  @impl true
  def handle_list_resources(_cursor, state) do
    {:ok, [], nil, state}
  end
  
  @impl true
  def handle_read_resource(_uri, state) do
    {:error, "Resource not found", state}
  end
  
  @impl true
  def handle_list_prompts(_cursor, state) do
    {:ok, [], nil, state}
  end
  
  @impl true
  def handle_get_prompt(_name, _arguments, state) do
    {:error, "Prompt not found", state}
  end
  
  @impl true
  def handle_list_resource_templates(_cursor, state) do
    {:ok, [], nil, state}
  end
end

# Example client demonstrating completion
defmodule CompletionClient do
  def demo do
    # Start the server
    {:ok, server} = ExMCP.Server.start_link(
      handler: CompletionExample,
      transport: :beam,
      name: :completion_server
    )
    
    # Start a client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :completion_server
    )
    
    # Wait for initialization
    Process.sleep(100)
    
    # Check server capabilities
    {:ok, caps} = ExMCP.Client.server_capabilities(client)
    IO.puts("\nServer capabilities:")
    IO.inspect(caps, pretty: true)
    
    if caps["completion"] do
      IO.puts("\n✅ Server supports completion!")
      IO.puts("  - hasArguments: #{caps["completion"]["hasArguments"]}")
      IO.puts("  - values: #{caps["completion"]["values"]}")
    end
    
    # Example 1: File path completion
    IO.puts("\n1. File path completion for '/home/user/':")
    {:ok, result1} = ExMCP.Client.complete(client, "argument", %{
      "tool" => "read_file",
      "name" => "path",
      "value" => "/home/user/"
    })
    
    IO.puts("Completions:")
    completions = result1["completion"] || []
    Enum.each(completions, &IO.puts("  - #{&1}"))
    
    # Example 2: More specific file path
    IO.puts("\n2. File path completion for '/home/user/doc':")
    {:ok, result2} = ExMCP.Client.complete(client, "argument", %{
      "tool" => "read_file",
      "name" => "path",
      "value" => "/home/user/doc"
    })
    
    IO.puts("Completions:")
    completions = result2["completion"] || []
    Enum.each(completions, &IO.puts("  - #{&1}"))
    
    # Example 3: Color completion
    IO.puts("\n3. Color completion for 'r':")
    {:ok, result3} = ExMCP.Client.complete(client, "argument", %{
      "tool" => "set_color",
      "name" => "color",
      "value" => "r"
    })
    
    IO.puts("Completions:")
    completions = result3["completion"] || []
    Enum.each(completions, &IO.puts("  - #{&1}"))
    
    # Example 4: Language code completion
    IO.puts("\n4. Language completion for 'e':")
    {:ok, result4} = ExMCP.Client.complete(client, "argument", %{
      "tool" => "translate",
      "name" => "to",
      "value" => "e"
    })
    
    IO.puts("Completions:")
    completions = result4["completion"] || []
    Enum.each(completions, &IO.puts("  - #{&1}"))
    
    # Example 5: Resource URI completion
    IO.puts("\n5. Resource URI completion for 'file://':")
    {:ok, result5} = ExMCP.Client.complete(client, "resource", %{
      "uri" => "file://"
    })
    
    IO.puts("Completions:")
    completions = result5["completion"] || []
    Enum.each(completions, &IO.puts("  - #{&1}"))
    
    # Demonstrate using completion in action
    IO.puts("\n6. Using completion to call a tool:")
    
    # Get file path completions
    {:ok, paths} = ExMCP.Client.complete(client, "argument", %{
      "tool" => "read_file",
      "name" => "path",
      "value" => "/home/user/projects/"
    })
    
    path_completions = paths["completion"] || []
    if first_path = List.first(path_completions) do
      IO.puts("Selected path: #{first_path}")
      
      # Call the tool with the completed path
      {:ok, result} = ExMCP.Client.call_tool(client, "read_file", %{
        "path" => first_path
      })
      
      IO.puts("File content: #{result["content"]}")
    end
    
    # Cleanup
    GenServer.stop(client)
    GenServer.stop(server)
  end
end

# Run the demo
CompletionClient.demo()