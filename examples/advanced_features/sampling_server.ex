defmodule Examples.AdvancedFeatures.SamplingServer do
  @moduledoc """
  Example MCP server demonstrating the sampling/createMessage feature.
  
  This shows how to integrate LLM capabilities into an MCP server,
  allowing clients to request AI-generated content.
  """
  
  use ExMCP.Server.Handler
  require Logger
  
  defmodule State do
    defstruct conversation_history: [], total_tokens_used: 0
  end
  
  @impl true
  def init(_args) do
    Logger.info("Sampling server starting...")
    {:ok, %State{}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    server_info = %{
      name: "sampling-demo-server",
      version: "1.0.0",
      capabilities: %{
        tools: %{},
        resources: %{},
        prompts: %{},
        sampling: %{}  # Enable sampling capability
      }
    }
    
    {:ok, server_info, state}
  end
  
  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        name: "clear_history",
        description: "Clear the conversation history",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "get_token_count",
        description: "Get total tokens used in this session",
        inputSchema: %{type: "object", properties: %{}}
      }
    ]
    
    {:ok, tools, state}
  end
  
  @impl true
  def handle_call_tool("clear_history", _params, state) do
    new_state = %{state | conversation_history: [], total_tokens_used: 0}
    content = [%{type: "text", text: "Conversation history cleared."}]
    {:ok, content, new_state}
  end
  
  def handle_call_tool("get_token_count", _params, state) do
    content = [%{
      type: "text", 
      text: "Total tokens used: #{state.total_tokens_used}"
    }]
    {:ok, content, state}
  end
  
  @impl true
  def handle_create_message(params, state) do
    Logger.info("Received sampling request")
    
    messages = params["messages"] || []
    model_preferences = params["modelPreferences"] || %{}
    max_tokens = params["maxTokens"] || 1000
    
    # Extract model hints
    model_hints = model_preferences["hints"] || []
    temperature = model_preferences["temperature"] || 0.7
    
    # Log the request
    Logger.debug("Messages: #{inspect(messages)}")
    Logger.debug("Model preferences: #{inspect(model_preferences)}")
    
    # Simulate LLM processing
    # In a real implementation, you would call your LLM provider here
    response = simulate_llm_response(messages, model_hints, temperature, max_tokens)
    
    # Update conversation history
    new_history = state.conversation_history ++ messages ++ [
      %{
        role: "assistant",
        content: %{type: "text", text: response.text}
      }
    ]
    
    # Update token count
    tokens_used = estimate_tokens(messages) + response.tokens
    new_state = %{state | 
      conversation_history: new_history,
      total_tokens_used: state.total_tokens_used + tokens_used
    }
    
    # Return the response in MCP format
    result = %{
      content: %{
        type: "text",
        text: response.text
      },
      model: response.model,
      stopReason: response.stop_reason
    }
    
    {:ok, result, new_state}
  end
  
  @impl true
  def handle_list_prompts(state) do
    prompts = [
      %{
        name: "explain_concept",
        description: "Explain a technical concept simply",
        arguments: [
          %{
            name: "concept",
            description: "The concept to explain",
            required: true
          }
        ]
      },
      %{
        name: "code_review",
        description: "Review code and suggest improvements",
        arguments: [
          %{
            name: "language",
            description: "Programming language",
            required: true
          },
          %{
            name: "code",
            description: "Code to review",
            required: true
          }
        ]
      }
    ]
    
    {:ok, prompts, state}
  end
  
  @impl true
  def handle_get_prompt("explain_concept", args, state) do
    concept = args["concept"] || "quantum computing"
    
    messages = [
      %{
        role: "user",
        content: %{
          type: "text",
          text: "Please explain #{concept} in simple terms that a beginner could understand. Use analogies where helpful."
        }
      }
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  def handle_get_prompt("code_review", args, state) do
    language = args["language"] || "elixir"
    code = args["code"] || "# No code provided"
    
    messages = [
      %{
        role: "user",
        content: %{
          type: "text",
          text: """
          Please review the following #{language} code and provide feedback on:
          1. Code quality and style
          2. Potential bugs or issues
          3. Performance considerations
          4. Suggested improvements
          
          Code:
          ```#{language}
          #{code}
          ```
          """
        }
      }
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  # Simulate LLM response (replace with actual LLM integration)
  defp simulate_llm_response(messages, model_hints, temperature, max_tokens) do
    # Get the last user message
    last_message = List.last(messages) || %{}
    user_text = get_in(last_message, ["content", "text"]) || "Hello"
    
    # Determine model based on hints
    model = case model_hints do
      [%{"name" => model_name} | _] -> model_name
      _ -> "mock-llm-v1"
    end
    
    # Generate a mock response based on the input
    response_text = case user_text do
      text when text =~ ~r/explain/i ->
        "I'll explain that concept for you. [This is a simulated response. " <>
        "In a real implementation, this would be generated by an LLM.] " <>
        "The concept involves several key aspects that work together..."
        
      text when text =~ ~r/code|review/i ->
        "Looking at your code, here are my observations: [Simulated response] " <>
        "1. The code structure looks good overall. " <>
        "2. Consider adding error handling for edge cases. " <>
        "3. The implementation could benefit from more documentation."
        
      text when text =~ ~r/hello/i ->
        "Hello! I'm a simulated LLM response. How can I help you today?"
        
      _ ->
        "I understand you're asking about: #{String.slice(user_text, 0, 50)}... " <>
        "[This is a simulated response demonstrating the sampling capability.]"
    end
    
    # Simulate token usage
    tokens = String.length(response_text) |> div(4)
    
    %{
      text: response_text,
      model: model,
      tokens: tokens,
      stop_reason: if(tokens >= max_tokens, do: "max_tokens", else: "stop")
    }
  end
  
  defp estimate_tokens(messages) do
    messages
    |> Enum.map(fn msg -> 
      get_in(msg, ["content", "text"]) || ""
    end)
    |> Enum.join(" ")
    |> String.length()
    |> div(4)  # Rough estimate: 1 token ≈ 4 characters
  end
end

defmodule Examples.AdvancedFeatures.SamplingClient do
  @moduledoc """
  Example client demonstrating how to use the sampling feature.
  """
  
  require Logger
  
  def demo do
    Logger.info("Starting sampling demo...")
    
    # Start the server
    {:ok, server} = ExMCP.Server.start_link(
      transport: :beam,
      name: :sampling_server,
      handler: Examples.AdvancedFeatures.SamplingServer
    )
    
    # Connect client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :sampling_server
    )
    
    # Wait for initialization
    Process.sleep(100)
    
    # Check server capabilities
    {:ok, info} = ExMCP.Client.server_info(client)
    Logger.info("Server capabilities: #{inspect(info["capabilities"])}")
    
    # Example 1: Direct message
    Logger.info("\n=== Example 1: Simple message ===")
    {:ok, response1} = ExMCP.Client.create_message(client, %{
      messages: [
        %{
          role: "user",
          content: %{type: "text", text: "Hello, can you help me?"}
        }
      ]
    })
    
    Logger.info("Response: #{response1["content"]["text"]}")
    Logger.info("Model used: #{response1["model"]}")
    
    # Example 2: With model preferences
    Logger.info("\n=== Example 2: With model preferences ===")
    {:ok, response2} = ExMCP.Client.create_message(client, %{
      messages: [
        %{
          role: "user",
          content: %{type: "text", text: "Explain how neural networks work"}
        }
      ],
      modelPreferences: %{
        hints: [%{name: "claude-3-opus"}],
        temperature: 0.5,
        costPriority: 0.3
      },
      maxTokens: 500
    })
    
    Logger.info("Response: #{String.slice(response2["content"]["text"], 0, 200)}...")
    
    # Example 3: Using prompts
    Logger.info("\n=== Example 3: Using prompts ===")
    {:ok, %{"prompts" => prompts}} = ExMCP.Client.list_prompts(client)
    Logger.info("Available prompts: #{inspect(Enum.map(prompts, & &1["name"]))}")
    
    # Get a prompt
    {:ok, prompt} = ExMCP.Client.get_prompt(client, "explain_concept", %{
      "concept" => "distributed systems"
    })
    
    # Use the prompt messages
    {:ok, response3} = ExMCP.Client.create_message(client, %{
      messages: prompt["messages"]
    })
    
    Logger.info("Prompt response: #{String.slice(response3["content"]["text"], 0, 200)}...")
    
    # Check token usage
    {:ok, result} = ExMCP.Client.call_tool(client, "get_token_count", %{})
    Logger.info("\n#{result["content"] |> List.first() |> Map.get("text")}")
    
    # Cleanup
    GenServer.stop(client)
    GenServer.stop(server)
    
    Logger.info("\nSampling demo completed!")
  end
end