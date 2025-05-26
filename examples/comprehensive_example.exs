# Comprehensive ExMCP Example
#
# This example demonstrates all major features of ExMCP including:
# - Tools with annotations
# - Resources with subscriptions
# - Prompts
# - Roots
# - Progress notifications
# - Sampling/LLM integration
# - Change notifications

defmodule ComprehensiveServer do
  @moduledoc """
  A comprehensive MCP server demonstrating all protocol features.
  """
  
  use ExMCP.Server.Handler
  require Logger

  defstruct [
    :llm_client,
    tools: %{},
    resources: %{},
    prompts: %{},
    roots: [],
    subscriptions: %{},
    task_progress: %{}
  ]

  @impl true
  def init(opts) do
    state = %__MODULE__{
      llm_client: opts[:llm_client],
      tools: init_tools(),
      resources: init_resources(),
      prompts: init_prompts(),
      roots: init_roots(),
      subscriptions: %{},
      task_progress: %{}
    }
    
    # Start a timer to simulate resource changes
    {:ok, _timer} = :timer.send_interval(30_000, :update_resources)
    
    {:ok, state}
  end

  @impl true
  def handle_initialize(_params, state) do
    server_info = %{
      name: "comprehensive-example",
      version: "1.0.0",
      capabilities: %{
        tools: %{},
        resources: %{subscribe: true},
        prompts: %{},
        roots: %{},
        sampling: %{}
      }
    }
    
    {:ok, server_info, state}
  end

  @impl true
  def handle_list_tools(state) do
    tools = Map.values(state.tools)
    {:ok, tools, state}
  end

  @impl true
  def handle_call_tool(name, args, state) do
    case Map.get(state.tools, name) do
      nil ->
        {:error, "Tool not found: #{name}", state}
        
      _tool ->
        handle_specific_tool(name, args, state)
    end
  end

  @impl true
  def handle_list_resources(state) do
    resources = Map.keys(state.resources) |> Enum.map(fn uri ->
      resource = Map.get(state.resources, uri)
      %{
        uri: uri,
        name: resource.name,
        description: resource.description,
        mimeType: resource.mimeType
      }
    end)
    
    {:ok, resources, state}
  end

  @impl true
  def handle_read_resource(uri, state) do
    case Map.get(state.resources, uri) do
      nil ->
        {:error, "Resource not found: #{uri}", state}
        
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
  def handle_list_prompts(state) do
    prompts = Map.values(state.prompts)
    {:ok, prompts, state}
  end

  @impl true
  def handle_get_prompt(name, args, state) do
    case Map.get(state.prompts, name) do
      nil ->
        {:error, "Prompt not found: #{name}", state}
        
      prompt ->
        messages = generate_prompt_messages(prompt, args)
        {:ok, messages, state}
    end
  end

  @impl true
  def handle_list_roots(state) do
    {:ok, state.roots, state}
  end

  @impl true
  def handle_subscribe_resource(uri, state) do
    Logger.info("Client subscribing to: #{uri}")
    
    if Map.has_key?(state.resources, uri) do
      # In real implementation, track client ID
      client_id = self()
      
      subscriptions = Map.update(
        state.subscriptions,
        uri,
        MapSet.new([client_id]),
        &MapSet.put(&1, client_id)
      )
      
      new_state = %{state | subscriptions: subscriptions}
      {:ok, %{}, new_state}
    else
      {:error, "Resource not found", state}
    end
  end

  @impl true
  def handle_unsubscribe_resource(uri, state) do
    Logger.info("Client unsubscribing from: #{uri}")
    
    client_id = self()
    
    subscriptions = Map.update(
      state.subscriptions,
      uri,
      MapSet.new(),
      &MapSet.delete(&1, client_id)
    )
    
    new_state = %{state | subscriptions: subscriptions}
    {:ok, %{}, new_state}
  end

  @impl true
  def handle_create_message(params, state) do
    messages = params["messages"]
    max_tokens = params["max_tokens"] || 500
    temperature = params["temperature"] || 0.7
    
    # Simulate LLM call
    response = simulate_llm_response(messages, max_tokens, temperature, state)
    
    result = %{
      role: "assistant",
      content: %{
        type: "text",
        text: response
      },
      model: "example-llm",
      stopReason: "stop"
    }
    
    {:ok, result, state}
  end

  @impl true
  def handle_info(:update_resources, state) do
    # Simulate resource updates
    uri = "file:///status.json"
    
    if Map.has_key?(state.resources, uri) do
      # Update resource content
      new_content = Jason.encode!(%{
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
        status: Enum.random(["healthy", "degraded", "maintenance"]),
        metrics: %{
          cpu: :rand.uniform(100),
          memory: :rand.uniform(100),
          requests: :rand.uniform(1000)
        }
      })
      
      new_resources = put_in(state.resources[uri].content, new_content)
      new_state = %{state | resources: new_resources}
      
      # Notify subscribers
      if subscribers = Map.get(state.subscriptions, uri) do
        if MapSet.size(subscribers) > 0 do
          Logger.info("Notifying #{MapSet.size(subscribers)} subscribers about #{uri}")
          ExMCP.Server.notify_resource_updated(self(), uri)
        end
      end
      
      {:noreply, new_state}
    else
      {:noreply, state}
    end
  end

  def handle_info({:task_progress, token, progress, total}, state) do
    # Send progress notification
    ExMCP.Server.notify_progress(self(), token, progress, total)
    
    # Update progress tracking
    new_progress = Map.put(state.task_progress, token, {progress, total})
    {:noreply, %{state | task_progress: new_progress}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # Private functions

  defp init_tools do
    %{
      "analyze_code" => %{
        name: "analyze_code",
        description: "Analyze code for quality, security, and performance issues",
        input_schema: %{
          type: "object",
          properties: %{
            code: %{type: "string", description: "Code to analyze"},
            language: %{type: "string", description: "Programming language"},
            checks: %{
              type: "array",
              items: %{type: "string"},
              description: "Specific checks to run"
            }
          },
          required: ["code", "language"]
        },
        readOnlyHint: true,
        destructiveHint: false,
        costHint: :medium
      },
      "refactor_code" => %{
        name: "refactor_code",
        description: "Refactor code to improve quality",
        input_schema: %{
          type: "object",
          properties: %{
            code: %{type: "string"},
            language: %{type: "string"},
            target: %{type: "string", description: "Refactoring target"}
          },
          required: ["code", "language"]
        },
        readOnlyHint: false,
        destructiveHint: false,
        costHint: :high
      },
      "deploy_application" => %{
        name: "deploy_application",
        description: "Deploy application to production",
        input_schema: %{
          type: "object",
          properties: %{
            environment: %{type: "string"},
            version: %{type: "string"},
            _progressToken: %{type: "string", description: "Progress tracking token"}
          },
          required: ["environment", "version"]
        },
        readOnlyHint: false,
        destructiveHint: true,
        costHint: :high
      }
    }
  end

  defp init_resources do
    %{
      "file:///config.json" => %{
        name: "Configuration",
        description: "Application configuration",
        mimeType: "application/json",
        content: Jason.encode!(%{
          version: "1.0.0",
          features: %{
            analytics: true,
            notifications: true
          }
        })
      },
      "file:///status.json" => %{
        name: "System Status",
        description: "Real-time system status",
        mimeType: "application/json",
        content: Jason.encode!(%{
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
          status: "healthy",
          metrics: %{cpu: 45, memory: 62, requests: 1234}
        })
      },
      "db://users/schema" => %{
        name: "User Schema",
        description: "Database schema for users table",
        mimeType: "text/plain",
        content: """
        CREATE TABLE users (
          id UUID PRIMARY KEY,
          email VARCHAR(255) UNIQUE NOT NULL,
          name VARCHAR(255),
          created_at TIMESTAMP DEFAULT NOW()
        );
        """
      }
    }
  end

  defp init_prompts do
    %{
      "code_review" => %{
        name: "code_review",
        description: "Comprehensive code review prompt",
        arguments: [
          %{name: "language", description: "Programming language", required: true},
          %{name: "focus", description: "Review focus area", required: false}
        ]
      },
      "explain_concept" => %{
        name: "explain_concept",
        description: "Explain a technical concept",
        arguments: [
          %{name: "concept", description: "Concept to explain", required: true},
          %{name: "level", description: "Explanation level (beginner/intermediate/advanced)", required: false}
        ]
      }
    }
  end

  defp init_roots do
    [
      %{uri: "file:///", name: "Local Files"},
      %{uri: "db://", name: "Database"},
      %{uri: "api://v1", name: "API v1"},
      %{uri: "s3://bucket", name: "S3 Storage"}
    ]
  end

  defp handle_specific_tool("analyze_code", args, state) do
    code = args["code"]
    language = args["language"]
    
    # Simulate analysis
    issues = [
      %{type: "style", line: 5, message: "Line too long"},
      %{type: "security", line: 12, message: "Potential SQL injection"},
      %{type: "performance", line: 23, message: "N+1 query detected"}
    ]
    
    result = [%{
      type: "text",
      text: """
      Code Analysis Results for #{language}:
      
      Found #{length(issues)} issues:
      #{Enum.map_join(issues, "\n", fn issue ->
        "- Line #{issue.line}: [#{issue.type}] #{issue.message}"
      end)}
      
      Overall score: B+ (Good with minor issues)
      """
    }]
    
    {:ok, result, state}
  end

  defp handle_specific_tool("refactor_code", args, state) do
    code = args["code"]
    _language = args["language"]
    target = args["target"] || "readability"
    
    # Simulate refactoring
    refactored = String.replace(code, "var", "const")
    |> String.replace("function", "const")
    |> String.replace("==", "===")
    
    result = [
      %{
        type: "text",
        text: "Refactored code for improved #{target}:"
      },
      %{
        type: "text",
        text: refactored
      }
    ]
    
    {:ok, result, state}
  end

  defp handle_specific_tool("deploy_application", args, state) do
    environment = args["environment"]
    version = args["version"]
    progress_token = args["_progressToken"]
    
    # Start async deployment
    server = self()
    
    Task.start(fn ->
      steps = [
        {10, "Validating configuration"},
        {20, "Building application"},
        {40, "Running tests"},
        {60, "Creating deployment package"},
        {80, "Uploading to #{environment}"},
        {90, "Running migrations"},
        {100, "Deployment complete"}
      ]
      
      Enum.each(steps, fn {progress, message} ->
        Logger.info("Deployment progress: #{message}")
        send(server, {:task_progress, progress_token, progress, 100})
        Process.sleep(2000)
      end)
    end)
    
    result = [%{
      type: "text",
      text: "Started deployment of version #{version} to #{environment}. Track progress with token: #{progress_token}"
    }]
    
    {:ok, result, state}
  end

  defp generate_prompt_messages(%{name: "code_review"}, args) do
    language = args["language"]
    focus = args["focus"] || "general"
    
    [
      %{
        role: "system",
        content: %{
          type: "text",
          text: "You are an expert #{language} developer performing a code review."
        }
      },
      %{
        role: "user",
        content: %{
          type: "text",
          text: """
          Please review the following #{language} code with a focus on #{focus}.
          Consider:
          - Code style and conventions
          - Potential bugs and edge cases
          - Performance implications
          - Security concerns
          - Maintainability
          
          Provide specific, actionable feedback.
          """
        }
      }
    ]
  end

  defp generate_prompt_messages(%{name: "explain_concept"}, args) do
    concept = args["concept"]
    level = args["level"] || "intermediate"
    
    [
      %{
        role: "user",
        content: %{
          type: "text",
          text: """
          Explain the concept of "#{concept}" at a #{level} level.
          Include:
          - Clear definition
          - How it works
          - When to use it
          - Practical examples
          - Common pitfalls
          """
        }
      }
    ]
  end

  defp simulate_llm_response(messages, _max_tokens, _temperature, _state) do
    last_message = List.last(messages)
    user_text = last_message["content"]["text"]
    
    # Simple simulation based on content
    cond do
      String.contains?(user_text, "explain") ->
        "I'll explain that concept for you. [Detailed explanation would go here based on actual LLM integration]"
        
      String.contains?(user_text, "review") ->
        "I'll review this code for you. [Code review would go here based on actual analysis]"
        
      true ->
        "I understand your request. [Response would be generated by actual LLM]"
    end
  end
end

# Example client usage
defmodule ComprehensiveClient do
  @moduledoc """
  Example client demonstrating all MCP features.
  """
  
  def demo do
    # Start server
    {:ok, server} = ExMCP.Server.start_link(
      handler: ComprehensiveServer,
      transport: :beam,
      name: :comprehensive_server,
      handler_args: []
    )
    
    # Connect client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :comprehensive_server
    )
    
    # 1. List and use tools
    {:ok, tools} = ExMCP.Client.list_tools(client)
    IO.puts("\n=== Available Tools ===")
    Enum.each(tools, fn tool ->
      hints = []
      hints = if tool[:readOnlyHint], do: ["read-only" | hints], else: hints
      hints = if tool[:destructiveHint], do: ["destructive!" | hints], else: hints
      hints = if tool[:costHint], do: ["cost: #{tool[:costHint]}" | hints], else: hints
      
      IO.puts("- #{tool.name}: #{tool.description} #{inspect(hints)}")
    end)
    
    # 2. Analyze code
    {:ok, result} = ExMCP.Client.call_tool(client, "analyze_code", %{
      code: "var x = 1; if (x == '1') { console.log('equal'); }",
      language: "javascript"
    })
    IO.puts("\n=== Code Analysis ===")
    Enum.each(result, fn content ->
      IO.puts(content.text)
    end)
    
    # 3. List roots
    {:ok, roots} = ExMCP.Client.list_roots(client)
    IO.puts("\n=== Resource Roots ===")
    Enum.each(roots, fn root ->
      IO.puts("- #{root.name}: #{root.uri}")
    end)
    
    # 4. List and read resources
    {:ok, resources} = ExMCP.Client.list_resources(client)
    IO.puts("\n=== Available Resources ===")
    Enum.each(resources, fn resource ->
      IO.puts("- #{resource.uri} (#{resource.mimeType}): #{resource.description}")
    end)
    
    # 5. Subscribe to status updates
    IO.puts("\n=== Subscribing to Status Updates ===")
    {:ok, _} = ExMCP.Client.subscribe_resource(client, "file:///status.json")
    IO.puts("Subscribed to file:///status.json")
    
    # 6. Use prompts
    {:ok, prompts} = ExMCP.Client.list_prompts(client)
    IO.puts("\n=== Available Prompts ===")
    Enum.each(prompts, fn prompt ->
      IO.puts("- #{prompt.name}: #{prompt.description}")
    end)
    
    {:ok, messages} = ExMCP.Client.get_prompt(client, "explain_concept", %{
      concept: "GenServer",
      level: "beginner"
    })
    IO.puts("\n=== Generated Prompt ===")
    Enum.each(messages, fn msg ->
      IO.puts("[#{msg.role}]: #{msg.content.text}")
    end)
    
    # 7. Use sampling/LLM
    IO.puts("\n=== LLM Integration ===")
    {:ok, response} = ExMCP.Client.create_message(client, %{
      messages: [
        %{
          role: "user",
          content: %{type: "text", text: "Please explain the concept of supervision trees"}
        }
      ],
      max_tokens: 200
    })
    IO.puts("Assistant: #{response.content.text}")
    
    # 8. Deploy with progress tracking
    IO.puts("\n=== Deployment with Progress ===")
    {:ok, deploy_result} = ExMCP.Client.call_tool(client, "deploy_application", %{
      environment: "staging",
      version: "v2.1.0",
      "_progressToken" => "deploy-#{:os.system_time()}"
    })
    Enum.each(deploy_result, fn content ->
      IO.puts(content.text)
    end)
    
    # Clean up
    IO.puts("\n=== Cleanup ===")
    {:ok, _} = ExMCP.Client.unsubscribe_resource(client, "file:///status.json")
    IO.puts("Unsubscribed from status updates")
    
    :ok
  end
end

# Run the demo
ComprehensiveClient.demo()