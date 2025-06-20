#!/usr/bin/env elixir

# Advanced MCP Server using DSL
# 
# This example demonstrates advanced DSL features including:
# - Complex argument schemas with nested objects
# - Resource patterns and subscriptions
# - Meta blocks for organization
# - Content helpers (text, json, image, etc.)
# - Advanced prompt configurations

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule AdvancedServer do
  use ExMCP.Server
  
  @impl true
  def init(_args) do
    # Initialize with some state
    {:ok, %{
      documents: %{
        "readme" => "# Project README\n\nWelcome to the project!",
        "config" => %{api_key: "secret", port: 8080}
      },
      subscriptions: %{}
    }}
  end
  
  # Tools Section
  
  deftool "analyze_data" do
    meta do
      description "Analyzes data with various options"
      category "analytics"
      version "2.0"
    end
    
    args do
      field :data, :array do
        items :number
        description "Array of numbers to analyze"
      end
      
      field :options, :object do
        field :method, :string, 
          enum: ["mean", "median", "mode", "all"], 
          default: "mean",
          description: "Analysis method"
        
        field :precision, :integer,
          minimum: 0,
          maximum: 10,
          default: 2
          
        field :include_metadata, :boolean, default: false
      end
    end
  end
  
  deftool "transform_text" do
    description "Transform text with various operations"
    
    args do
      field :text, :string, required: true, min_length: 1
      field :operations, :array do
        items :object do
          field :type, :string, 
            required: true,
            enum: ["uppercase", "lowercase", "reverse", "base64"]
          field :params, :object
        end
      end
    end
  end
  
  # Resources Section
  
  defresource "doc://project/*" do
    meta do
      name "Project Documents"
      description "Access project documentation"
      mime_type "text/plain"
    end
    
    # Enable pattern matching for dynamic resources
    list_pattern true
    subscribable true
  end
  
  defresource "config://app" do
    name "Application Config"
    description "Current application configuration"
    mime_type "application/json"
    
    # Add custom annotations
    annotations %{
      security: "sensitive",
      cache_ttl: 300
    }
  end
  
  # Prompts Section
  
  defprompt "code_assistant" do
    meta do
      name "Code Writing Assistant"
      description "Helps write code in various languages"
    end
    
    arguments do
      arg :language, required: true, description: "Programming language"
      arg :task, required: true, description: "What to implement"
      arg :style, description: "Coding style preferences"
      arg :constraints, description: "Any constraints or requirements"
    end
  end
  
  defprompt "data_analyst" do
    name "Data Analysis Expert"
    description "Analyzes data and provides insights"
    
    arguments do
      arg :data_description, required: true
      arg :analysis_type
      arg :output_format, description: "Desired output format"
    end
  end
  
  # Handler Implementations
  
  @impl true
  def handle_tool_call("analyze_data", args, state) do
    data = Map.get(args, "data", [])
    options = Map.get(args, "options", %{})
    method = Map.get(options, "method", "mean")
    precision = Map.get(options, "precision", 2)
    include_metadata = Map.get(options, "include_metadata", false)
    
    result = case method do
      "mean" -> calculate_mean(data, precision)
      "median" -> calculate_median(data, precision)
      "mode" -> calculate_mode(data)
      "all" -> %{
        mean: calculate_mean(data, precision),
        median: calculate_median(data, precision),
        mode: calculate_mode(data)
      }
    end
    
    content = if include_metadata do
      [
        text("Analysis complete!"),
        json(%{
          result: result,
          metadata: %{
            count: length(data),
            method: method,
            timestamp: DateTime.utc_now()
          }
        })
      ]
    else
      [json(%{result: result})]
    end
    
    {:ok, %{content: content}, state}
  end
  
  @impl true
  def handle_tool_call("transform_text", %{"text" => text, "operations" => ops}, state) do
    transformed = Enum.reduce(ops, text, fn op, acc ->
      apply_operation(acc, op["type"], op["params"])
    end)
    
    result = %{
      content: [
        text("Transformation complete"),
        json(%{
          original: text,
          transformed: transformed,
          operations_applied: length(ops)
        })
      ]
    }
    
    {:ok, result, state}
  end
  
  @impl true
  def handle_resource_read("doc://project/" <> doc_name, _uri, state) do
    case Map.get(state.documents, doc_name) do
      nil -> 
        {:error, "Document not found: #{doc_name}", state}
      content when is_binary(content) ->
        {:ok, [text(content)], state}
      content ->
        {:ok, [json(content)], state}
    end
  end
  
  @impl true
  def handle_resource_read("config://app", _uri, state) do
    config = Map.get(state.documents, "config", %{})
    {:ok, [json(config)], state}
  end
  
  @impl true
  def handle_resource_list(state) do
    # List all available documents
    doc_resources = state.documents
      |> Map.keys()
      |> Enum.map(fn name ->
        %{
          uri: "doc://project/#{name}",
          name: String.capitalize(name),
          mimeType: if(String.ends_with?(name, ".json"), do: "application/json", else: "text/plain")
        }
      end)
    
    resources = [
      %{
        uri: "config://app",
        name: "Application Config",
        mimeType: "application/json"
      } | doc_resources
    ]
    
    {:ok, resources, state}
  end
  
  @impl true
  def handle_resource_subscribe("doc://project/" <> doc_name = uri, state) do
    # Add subscription
    subscriptions = Map.put(state.subscriptions, uri, true)
    new_state = %{state | subscriptions: subscriptions}
    
    # Send initial notification
    spawn(fn ->
      Process.sleep(1000)
      # In a real implementation, this would notify about changes
      IO.puts("Subscribed to changes for: #{doc_name}")
    end)
    
    {:ok, new_state}
  end
  
  @impl true
  def handle_prompt_get("code_assistant", args, state) do
    language = args["language"]
    task = args["task"]
    style = Map.get(args, "style", "clean and readable")
    constraints = Map.get(args, "constraints", "none")
    
    messages = [
      system("You are an expert #{language} programmer. Write #{style} code."),
      user("""
      Please write #{language} code to: #{task}
      
      Constraints: #{constraints}
      """),
      assistant("I'll help you write #{language} code for that task. Let me create a solution that follows #{style} style guidelines.")
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  @impl true
  def handle_prompt_get("data_analyst", args, state) do
    messages = [
      system("You are a data analysis expert. Provide clear, actionable insights."),
      user("Analyze this data: #{args["data_description"]}"),
      assistant("I'll analyze the data and provide insights in #{Map.get(args, "output_format", "a clear format")}.")
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  # Helper functions
  
  defp calculate_mean([], _precision), do: nil
  defp calculate_mean(data, precision) do
    sum = Enum.sum(data)
    Float.round(sum / length(data), precision)
  end
  
  defp calculate_median([], _precision), do: nil
  defp calculate_median(data, precision) do
    sorted = Enum.sort(data)
    mid = div(length(sorted), 2)
    
    if rem(length(sorted), 2) == 0 do
      Float.round((Enum.at(sorted, mid - 1) + Enum.at(sorted, mid)) / 2, precision)
    else
      Enum.at(sorted, mid)
    end
  end
  
  defp calculate_mode([]), do: nil
  defp calculate_mode(data) do
    frequencies = Enum.frequencies(data)
    max_freq = frequencies |> Map.values() |> Enum.max()
    
    frequencies
    |> Enum.filter(fn {_, freq} -> freq == max_freq end)
    |> Enum.map(fn {val, _} -> val end)
    |> List.first()
  end
  
  defp apply_operation(text, "uppercase", _), do: String.upcase(text)
  defp apply_operation(text, "lowercase", _), do: String.downcase(text)
  defp apply_operation(text, "reverse", _), do: String.reverse(text)
  defp apply_operation(text, "base64", _), do: Base.encode64(text)
  defp apply_operation(text, _, _), do: text
end

# Start the server
defmodule AdvancedServerRunner do
  def run do
    IO.puts("Starting Advanced MCP Server with DSL...")
    IO.puts("This server demonstrates advanced DSL features.\n")
    
    # Start server on stdio transport
    {:ok, _server} = AdvancedServer.start_link(
      transport: :stdio,
      name: :advanced_server
    )
    
    IO.puts("Server is running with advanced features:")
    IO.puts("\nTools:")
    IO.puts("- analyze_data: Statistical analysis with options")
    IO.puts("- transform_text: Text transformation pipeline")
    IO.puts("\nResources:")
    IO.puts("- doc://project/* (pattern-based, subscribable)")
    IO.puts("- config://app (annotated)")
    IO.puts("\nPrompts:")
    IO.puts("- code_assistant: Multi-argument code generation")
    IO.puts("- data_analyst: Data analysis assistance")
    
    # Keep the server running
    Process.sleep(:infinity)
  end
end

# Run if executed directly
if System.get_env("MIX_ENV") != "test" do
  AdvancedServerRunner.run()
end