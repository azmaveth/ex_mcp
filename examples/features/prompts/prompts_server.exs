#!/usr/bin/env elixir

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule PromptsServer do
  @moduledoc """
  Example MCP server that provides various prompts for AI assistance.
  
  This demonstrates:
  - Implementing the prompts capability
  - Providing parameterized prompts
  - Supporting both required and optional arguments
  - Generating context-aware prompt messages
  - Dynamic prompt list changes with notifications
  
  ## Usage
  
      # Start the server
      ./examples/prompts_server.exs
      
      # In another terminal, connect with a client
      ./examples/client.exs stdio
      
      # List available prompts
      iex> {:ok, %{prompts: prompts}} = Client.list_prompts(client)
      iex> Enum.map(prompts, & &1.name)
      ["code_review", "refactor_code", "write_documentation", ...]
      
      # Get a specific prompt
      iex> {:ok, messages} = Client.get_prompt(client, "code_review", %{
        "code" => "def add(a, b), do: a + b",
        "language" => "elixir",
        "focus" => "performance"
      })
  """
  
  use ExMCP.Server.Handler
  require Logger

  @impl true
  def init(_args) do
    Logger.info("Prompts server starting...")
    {:ok, %{prompts: load_prompts()}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       protocolVersion: "2025-03-26",
       serverInfo: %{
         name: "prompts-example-server",
         version: "1.0.0"
       },
       capabilities: %{
         prompts: %{
           listChanged: true  # We support dynamic prompt updates
         }
       }
     }, state}
  end

  @impl true
  def handle_list_prompts(_cursor, state) do
    # For this example, we return all prompts without pagination
    {:ok, state.prompts, nil, state}
  end

  @impl true
  def handle_get_prompt(name, arguments, state) do
    case find_prompt(name, state.prompts) do
      nil ->
        {:error, "Unknown prompt: #{name}", state}
        
      prompt ->
        case validate_arguments(prompt, arguments) do
          :ok ->
            messages = generate_messages(prompt, arguments)
            {:ok, messages, state}
            
          {:error, reason} ->
            {:error, reason, state}
        end
    end
  end

  # Custom function to add new prompts dynamically
  def add_prompt(server, prompt) do
    GenServer.call(server, {:add_prompt, prompt})
  end

  @impl true
  def handle_call({:add_prompt, prompt}, _from, state) do
    new_prompts = [prompt | state.prompts]
    
    # Notify connected clients about the change
    ExMCP.Server.notify_prompts_changed(self())
    
    {:reply, :ok, %{state | prompts: new_prompts}}
  end

  # Private functions

  defp load_prompts do
    [
      %{
        name: "code_review",
        description: "Perform a comprehensive code review",
        arguments: [
          %{
            name: "code",
            description: "The code to review",
            required: true
          },
          %{
            name: "language",
            description: "Programming language (e.g., elixir, python, javascript)",
            required: true
          },
          %{
            name: "focus",
            description: "Review focus (performance, security, readability, all)",
            required: false
          }
        ]
      },
      %{
        name: "refactor_code",
        description: "Suggest refactoring improvements for code",
        arguments: [
          %{
            name: "code",
            description: "The code to refactor",
            required: true
          },
          %{
            name: "language",
            description: "Programming language",
            required: true
          },
          %{
            name: "goal",
            description: "Refactoring goal (simplify, optimize, modernize)",
            required: false
          }
        ]
      },
      %{
        name: "write_documentation",
        description: "Generate documentation for code",
        arguments: [
          %{
            name: "code",
            description: "The code to document",
            required: true
          },
          %{
            name: "style",
            description: "Documentation style (inline, markdown, docstring)",
            required: false
          },
          %{
            name: "detail_level",
            description: "Level of detail (brief, standard, comprehensive)",
            required: false
          }
        ]
      },
      %{
        name: "explain_error",
        description: "Explain an error message and suggest fixes",
        arguments: [
          %{
            name: "error",
            description: "The error message or stack trace",
            required: true
          },
          %{
            name: "context",
            description: "Additional context about when the error occurred",
            required: false
          }
        ]
      },
      %{
        name: "generate_tests",
        description: "Generate unit tests for code",
        arguments: [
          %{
            name: "code",
            description: "The code to test",
            required: true
          },
          %{
            name: "framework",
            description: "Testing framework (exunit, pytest, jest, etc.)",
            required: false
          },
          %{
            name: "coverage",
            description: "Test coverage focus (happy_path, edge_cases, all)",
            required: false
          }
        ]
      },
      %{
        name: "api_design",
        description: "Design an API based on requirements",
        arguments: [
          %{
            name: "requirements",
            description: "Description of what the API should do",
            required: true
          },
          %{
            name: "style",
            description: "API style (rest, graphql, rpc)",
            required: false
          },
          %{
            name: "format",
            description: "Output format (openapi, code, markdown)",
            required: false
          }
        ]
      }
    ]
  end

  defp find_prompt(name, prompts) do
    Enum.find(prompts, fn p -> p.name == name end)
  end

  defp validate_arguments(prompt, arguments) do
    required_args = 
      prompt.arguments
      |> Enum.filter(& &1.required)
      |> Enum.map(& &1.name)
    
    missing = Enum.filter(required_args, fn arg -> 
      not Map.has_key?(arguments, arg)
    end)
    
    case missing do
      [] -> :ok
      _ -> {:error, "Missing required arguments: #{Enum.join(missing, ", ")}"}
    end
  end

  defp generate_messages(prompt, arguments) do
    case prompt.name do
      "code_review" ->
        focus = arguments["focus"] || "all"
        [
          %{
            role: "user",
            content: %{
              type: "text",
              text: """
              Please perform a #{focus} code review for the following #{arguments["language"]} code:

              ```#{arguments["language"]}
              #{arguments["code"]}
              ```

              Focus on:
              #{review_focus_points(focus)}
              """
            }
          }
        ]
        
      "refactor_code" ->
        goal = arguments["goal"] || "improve"
        [
          %{
            role: "user",
            content: %{
              type: "text",
              text: """
              Please refactor the following #{arguments["language"]} code to #{goal}:

              ```#{arguments["language"]}
              #{arguments["code"]}
              ```

              Provide the refactored code with explanations for the changes.
              """
            }
          }
        ]
        
      "write_documentation" ->
        style = arguments["style"] || "markdown"
        detail = arguments["detail_level"] || "standard"
        [
          %{
            role: "user",
            content: %{
              type: "text",
              text: """
              Generate #{detail} #{style} documentation for:

              ```
              #{arguments["code"]}
              ```

              Include:
              #{documentation_requirements(style, detail)}
              """
            }
          }
        ]
        
      "explain_error" ->
        messages = [
          %{
            role: "user",
            content: %{
              type: "text",
              text: """
              Please explain this error and suggest fixes:

              ```
              #{arguments["error"]}
              ```
              """
            }
          }
        ]
        
        if arguments["context"] do
          messages ++ [
            %{
              role: "user",
              content: %{
                type: "text",
                text: "Additional context: #{arguments["context"]}"
              }
            }
          ]
        else
          messages
        end
        
      "generate_tests" ->
        framework = arguments["framework"] || "appropriate framework"
        coverage = arguments["coverage"] || "all"
        [
          %{
            role: "user",
            content: %{
              type: "text",
              text: """
              Generate #{framework} unit tests for:

              ```
              #{arguments["code"]}
              ```

              Test coverage should include: #{test_coverage_description(coverage)}
              """
            }
          }
        ]
        
      "api_design" ->
        style = arguments["style"] || "rest"
        format = arguments["format"] || "markdown"
        [
          %{
            role: "user",
            content: %{
              type: "text",
              text: """
              Design a #{style} API based on these requirements:

              #{arguments["requirements"]}

              Provide the design in #{format} format.
              """
            }
          }
        ]
        
      _ ->
        [
          %{
            role: "user", 
            content: %{
              type: "text",
              text: "Execute prompt: #{prompt.name}"
            }
          }
        ]
    end
  end

  defp review_focus_points("performance"), do: "- Algorithm efficiency\n- Memory usage\n- Potential bottlenecks"
  defp review_focus_points("security"), do: "- Input validation\n- Authentication/authorization\n- Potential vulnerabilities"
  defp review_focus_points("readability"), do: "- Code clarity\n- Naming conventions\n- Documentation"
  defp review_focus_points(_), do: "- Performance\n- Security\n- Readability\n- Best practices"

  defp documentation_requirements("inline", "brief"), do: "- Brief function descriptions"
  defp documentation_requirements("inline", _), do: "- Function descriptions\n- Parameter explanations\n- Return values"
  defp documentation_requirements("markdown", "comprehensive"), do: "- Overview\n- Function documentation\n- Examples\n- Edge cases"
  defp documentation_requirements("markdown", _), do: "- Function documentation\n- Usage examples"
  defp documentation_requirements("docstring", _), do: "- Function purpose\n- Parameters\n- Returns\n- Examples"
  defp documentation_requirements(_, _), do: "- Complete documentation"

  defp test_coverage_description("happy_path"), do: "typical use cases and expected behavior"
  defp test_coverage_description("edge_cases"), do: "boundary conditions, error cases, and unusual inputs"
  defp test_coverage_description(_), do: "comprehensive coverage including happy paths, edge cases, and error conditions"
end

# Start the server
{:ok, _server} = ExMCP.Server.start_link(
  transport: :stdio,
  handler: PromptsServer
)

Logger.info("Prompts server started. Waiting for connections...")

# Keep the process alive
Process.sleep(:infinity)