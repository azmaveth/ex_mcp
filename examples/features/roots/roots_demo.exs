#!/usr/bin/env elixir

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule RootsClient do
  @moduledoc """
  Example MCP client that demonstrates roots functionality.
  
  This shows how to:
  - Implement client roots to expose filesystem access
  - Provide multiple roots with descriptive names
  - Handle server requests for root information
  - Support dynamic root updates
  
  ## Usage
  
      # Run this example to connect to any MCP server
      ./examples/roots_demo.exs
      
      # The client will expose several example roots
      # Any connected server can request these roots
  """
  
  @behaviour ExMCP.Client.Handler
  require Logger

  @impl true
  def init(_opts) do
    Logger.info("Starting roots demo client...")
    
    # Define example roots that this client exposes
    roots = [
      %{
        uri: "file://#{System.user_home()}/Documents",
        name: "Documents"
      },
      %{
        uri: "file://#{System.user_home()}/Desktop", 
        name: "Desktop"
      },
      %{
        uri: "file://#{File.cwd!()}",
        name: "Current Project"
      },
      %{
        uri: "file:///tmp",
        name: "Temporary Files"
      },
      %{
        uri: "file://#{System.user_home()}/.config",
        name: "Configuration"
      }
    ]
    
    Logger.info("Exposed roots:")
    Enum.each(roots, fn root ->
      Logger.info("  • #{root.name}: #{root.uri}")
    end)
    
    {:ok, %{roots: roots, request_count: 0}}
  end

  @impl true
  def handle_ping(state) do
    Logger.info("Received ping from server")
    {:ok, %{}, state}
  end

  @impl true
  def handle_list_roots(state) do
    Logger.info("Server requested roots list (request ##{state.request_count + 1})")
    
    # Track how many times roots have been requested
    new_state = %{state | request_count: state.request_count + 1}
    
    Logger.info("Providing #{length(state.roots)} roots to server")
    {:ok, state.roots, new_state}
  end

  @impl true
  def handle_create_message(params, state) do
    Logger.info("Server requested LLM sampling")
    Logger.info("Messages: #{length(params["messages"] || [])}")
    
    # In a real implementation, this would:
    # 1. Show the request to the user for approval
    # 2. Call an actual LLM if approved
    # 3. Return the response
    
    {:error, "LLM sampling not implemented in demo", state}
  end

  @impl true
  def terminate(_reason, state) do
    Logger.info("Client shutting down after #{state.request_count} root requests")
    :ok
  end
end

defmodule RootsServer do
  @moduledoc """
  Example MCP server that demonstrates requesting and using client roots.
  """
  
  use ExMCP.Server.Handler
  require Logger

  @impl true
  def init(_args) do
    Logger.info("Starting roots demo server...")
    {:ok, %{client_roots: [], last_roots_check: nil}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       protocolVersion: "2025-03-26",
       serverInfo: %{
         name: "roots-demo-server",
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
        name: "check_roots",
        description: "Request and display client's root directories",
        inputSchema: %{
          type: "object",
          properties: %{}
        }
      },
      %{
        name: "analyze_roots",
        description: "Analyze client's root directories for interesting patterns",
        inputSchema: %{
          type: "object",
          properties: %{
            check_permissions: %{type: "boolean", default: false}
          }
        }
      },
      %{
        name: "find_files",
        description: "Search for files within client's root directories",
        inputSchema: %{
          type: "object",
          properties: %{
            pattern: %{type: "string"},
            root_filter: %{type: "string"}
          },
          required: ["pattern"]
        }
      }
    ]
    
    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("check_roots", _arguments, state) do
    Logger.info("Tool call: check_roots")
    
    case ExMCP.Server.list_roots(self(), 5000) do
      {:ok, result} ->
        roots = result["roots"] || []
        new_state = %{state | 
          client_roots: roots,
          last_roots_check: DateTime.utc_now()
        }
        
        if Enum.empty?(roots) do
          {:ok, [%{
            type: "text",
            text: "No roots available from client."
          }], new_state}
        else
          root_summary = 
            roots
            |> Enum.with_index(1)
            |> Enum.map(fn {root, index} ->
              name = Map.get(root, "name", "Unnamed")
              uri = Map.get(root, "uri", "")
              "#{index}. #{name}\n   URI: #{uri}"
            end)
            |> Enum.join("\n")
          
          {:ok, [%{
            type: "text",
            text: "Found #{length(roots)} client roots:\n\n#{root_summary}"
          }], new_state}
        end
        
      {:error, reason} ->
        {:ok, [%{
          type: "text",
          text: "Failed to get roots: #{inspect(reason)}"
        }], state}
    end
  end

  def handle_call_tool("analyze_roots", arguments, state) do
    check_permissions = Map.get(arguments, "check_permissions", false)
    
    if Enum.empty?(state.client_roots) do
      {:ok, [%{
        type: "text",
        text: "No roots available. Please run 'check_roots' first."
      }], state}
    else
      analysis = analyze_root_patterns(state.client_roots, check_permissions)
      
      {:ok, [%{
        type: "text",
        text: "Root Directory Analysis:\n\n#{analysis}"
      }], state}
    end
  end

  def handle_call_tool("find_files", arguments, state) do
    pattern = arguments["pattern"]
    root_filter = Map.get(arguments, "root_filter")
    
    if Enum.empty?(state.client_roots) do
      {:ok, [%{
        type: "text",
        text: "No roots available. Please run 'check_roots' first."
      }], state}
    else
      relevant_roots = filter_roots(state.client_roots, root_filter)
      
      if Enum.empty?(relevant_roots) do
        {:ok, [%{
          type: "text",
          text: "No roots match the filter '#{root_filter || "none"}'"
        }], state}
      else
        search_results = simulate_file_search(relevant_roots, pattern)
        
        {:ok, [%{
          type: "text",
          text: "File Search Results for '#{pattern}':\n\n#{search_results}"
        }], state}
      end
    end
  end

  def handle_call_tool(name, _arguments, state) do
    {:error, "Unknown tool: #{name}", state}
  end

  # Private helper functions

  defp analyze_root_patterns(roots, check_permissions) do
    patterns = []
    
    # Analyze root types
    home_roots = Enum.filter(roots, fn root ->
      uri = Map.get(root, "uri", "")
      String.contains?(uri, "/home/") or String.contains?(uri, "Users/")
    end)
    
    config_roots = Enum.filter(roots, fn root ->
      name = Map.get(root, "name", "")
      String.contains?(String.downcase(name), "config")
    end)
    
    temp_roots = Enum.filter(roots, fn root ->
      uri = Map.get(root, "uri", "")
      String.contains?(uri, "/tmp") or String.contains?(uri, "/temp")
    end)
    
    patterns = [
      "📊 Root Statistics:",
      "  • Total roots: #{length(roots)}",
      "  • Home directories: #{length(home_roots)}",
      "  • Configuration directories: #{length(config_roots)}",
      "  • Temporary directories: #{length(temp_roots)}",
      "",
      "🔍 Root Types:"
    ]
    
    root_types = 
      roots
      |> Enum.map(fn root ->
        name = Map.get(root, "name", "Unnamed")
        uri = Map.get(root, "uri", "")
        type = classify_root_type(uri, name)
        permission_note = if check_permissions, do: " (permissions: #{simulate_permissions()})", else: ""
        "  • #{name}: #{type}#{permission_note}"
      end)
    
    Enum.join(patterns ++ root_types, "\n")
  end

  defp classify_root_type(uri, name) do
    cond do
      String.contains?(uri, "/home/") or String.contains?(uri, "Users/") -> "User Directory"
      String.contains?(uri, "Desktop") -> "Desktop"
      String.contains?(uri, "Documents") -> "Documents"
      String.contains?(uri, "/tmp") or String.contains?(uri, "/temp") -> "Temporary"
      String.contains?(String.downcase(name), "config") -> "Configuration"
      String.contains?(String.downcase(name), "project") -> "Project Root"
      true -> "Generic Directory"
    end
  end

  defp simulate_permissions do
    ~w[r-x rwx r-- rw-] |> Enum.random()
  end

  defp filter_roots(roots, nil), do: roots
  defp filter_roots(roots, filter) do
    Enum.filter(roots, fn root ->
      name = Map.get(root, "name", "")
      uri = Map.get(root, "uri", "")
      
      String.contains?(String.downcase(name), String.downcase(filter)) or
      String.contains?(String.downcase(uri), String.downcase(filter))
    end)
  end

  defp simulate_file_search(roots, pattern) do
    # Simulate finding files matching the pattern
    results = 
      roots
      |> Enum.flat_map(fn root ->
        name = Map.get(root, "name", "Unnamed")
        uri = Map.get(root, "uri", "")
        
        # Generate some fake file matches
        file_count = :rand.uniform(5)
        files = for i <- 1..file_count do
          extension = ~w[.txt .md .json .config .log .py .ex] |> Enum.random()
          filename = "#{pattern}_#{i}#{extension}"
          
          "  📄 #{uri}/#{filename}"
        end
        
        ["📁 #{name} (#{length(files)} matches):"] ++ files ++ [""]
      end)
    
    if Enum.empty?(results) do
      "No files found matching '#{pattern}'"
    else
      Enum.join(results, "\n")
    end
  end
end

# Determine mode from command line args
mode = case System.argv() do
  ["client" | _] -> :client
  ["server" | _] -> :server
  _ -> 
    IO.puts("Usage: #{__ENV__.file} [client|server]")
    IO.puts("  client - Start as MCP client with roots")
    IO.puts("  server - Start as MCP server that requests roots")
    System.halt(1)
end

case mode do
  :client ->
    Logger.info("Starting as MCP client with roots...")
    
    # In a real scenario, this would connect to an existing server
    # For demo purposes, we'll start a simple server internally
    {:ok, demo_server} = ExMCP.Server.start_link(
      transport: :stdio,
      handler: RootsServer
    )
    
    {:ok, _client} = ExMCP.Client.start_link(
      transport: :stdio,
      server: demo_server,
      handler: RootsClient
    )
    
    Logger.info("""
    Client started with roots exposed!
    
    In a real scenario, this client would connect to an external server.
    The server can now request this client's roots using the 'roots/list' method.
    """)
    
  :server ->
    Logger.info("Starting as MCP server...")
    
    {:ok, _server} = ExMCP.Server.start_link(
      transport: :stdio,
      handler: RootsServer
    )
    
    Logger.info("""
    Server started!
    
    This server can request roots from connected clients using these tools:
    • check_roots - Get client's root directories
    • analyze_roots - Analyze root patterns
    • find_files - Search within client roots
    
    Connect an MCP client to see roots functionality in action.
    """)
end

# Keep the process alive
Process.sleep(:infinity)