defmodule ExMCP.Testing.AgentTestServer do
  @moduledoc """
  Comprehensive MCP server for agent simulation tests.

  Provides a rich set of tools, resources, and prompts that exercise
  the full range of MCP capabilities in realistic agent workflows.
  """

  use ExMCP.Server

  # ── Tools ──────────────────────────────────────────────────────────────

  deftool "weather" do
    meta do
      description("Get current weather for a city")

      input_schema(%{
        type: "object",
        properties: %{
          city: %{type: "string", description: "City name"}
        },
        required: ["city"]
      })
    end
  end

  deftool "calculate" do
    meta do
      description("Perform basic math operations")

      input_schema(%{
        type: "object",
        properties: %{
          operation: %{type: "string", enum: ["add", "subtract", "multiply", "divide"]},
          a: %{type: "number"},
          b: %{type: "number"}
        },
        required: ["operation", "a", "b"]
      })
    end
  end

  deftool "search_docs" do
    meta do
      description("Search documentation by query")

      input_schema(%{
        type: "object",
        properties: %{
          query: %{type: "string", description: "Search query"},
          limit: %{type: "integer", description: "Max results"}
        },
        required: ["query"]
      })
    end
  end

  deftool "create_report" do
    meta do
      description("Create a report from data")

      input_schema(%{
        type: "object",
        properties: %{
          title: %{type: "string"},
          data: %{type: "string"},
          format: %{type: "string", enum: ["summary", "detailed", "csv"]}
        },
        required: ["title", "data"]
      })
    end
  end

  # ── Resources ──────────────────────────────────────────────────────────

  defresource "docs://api/users" do
    meta do
      name("API Users Documentation")
      description("User API endpoint documentation")
    end

    mime_type("text/markdown")
  end

  defresource "config://app/settings" do
    meta do
      name("Application Settings")
      description("Current application configuration")
    end

    mime_type("application/json")
  end

  defresource "data://metrics/latest" do
    meta do
      name("Latest Metrics")
      description("Most recent system metrics data")
    end

    mime_type("application/json")
  end

  # ── Prompts ────────────────────────────────────────────────────────────

  defprompt "summarize" do
    meta do
      name("Summarizer")
      description("Summarize content with a given style")
    end

    arguments do
      arg(:style, description: "Summary style (concise, detailed, bullet)")
    end
  end

  defprompt "analyze" do
    meta do
      name("Analyzer")
      description("Analyze data with a specific focus area")
    end

    arguments do
      arg(:focus, required: true, description: "Focus area for analysis")
    end
  end

  # ── Callbacks ──────────────────────────────────────────────────────────

  @impl true
  def handle_initialize(params, state) do
    {:ok,
     %{
       "protocolVersion" => params["protocolVersion"],
       "serverInfo" => %{
         "name" => "agent-test-server",
         "version" => "1.0.0"
       },
       "capabilities" => %{
         "tools" => %{},
         "resources" => %{},
         "prompts" => %{}
       }
     }, state}
  end

  # ── Tool Handlers ─────────────────────────────────────────────────────

  @impl true
  def handle_tool_call("weather", %{"city" => city}, state) do
    weather_data = %{
      "city" => city,
      "temperature" => 72,
      "condition" => "sunny",
      "humidity" => 45,
      "wind_speed" => 8
    }

    {:ok, %{content: [%{"type" => "text", "text" => Jason.encode!(weather_data)}]}, state}
  end

  @impl true
  def handle_tool_call("calculate", %{"operation" => op, "a" => a, "b" => b}, state) do
    result =
      case op do
        "add" -> a + b
        "subtract" -> a - b
        "multiply" -> a * b
        "divide" when b != 0 -> a / b
        "divide" -> {:error, "Division by zero"}
      end

    case result do
      {:error, msg} ->
        {:error, msg, state}

      value ->
        {:ok, %{content: [%{"type" => "text", "text" => "#{value}"}]}, state}
    end
  end

  @impl true
  def handle_tool_call("search_docs", %{"query" => query} = args, state) do
    limit = Map.get(args, "limit", 5)

    results =
      1..min(limit, 3)
      |> Enum.map(fn i ->
        %{
          "title" => "Doc #{i}: #{query}",
          "snippet" => "Result #{i} for query '#{query}' — relevant documentation section.",
          "relevance" => 1.0 - i * 0.1
        }
      end)

    {:ok, %{content: [%{"type" => "text", "text" => Jason.encode!(results)}]}, state}
  end

  @impl true
  def handle_tool_call("create_report", %{"title" => title, "data" => data} = args, state) do
    format = Map.get(args, "format", "summary")

    report =
      case format do
        "summary" ->
          "# #{title}\n\nSummary: #{String.slice(data, 0, 100)}...\n\nGenerated at: #{DateTime.utc_now() |> DateTime.to_iso8601()}"

        "detailed" ->
          "# #{title}\n\n## Full Data\n#{data}\n\n## Analysis\nDetailed analysis of the above data."

        "csv" ->
          "title,data\n\"#{title}\",\"#{String.replace(data, "\"", "\"\"")}\""
      end

    {:ok, %{content: [%{"type" => "text", "text" => report}]}, state}
  end

  @impl true
  def handle_tool_call(name, _args, state) do
    {:error, "Unknown tool: #{name}", state}
  end

  # ── Resource Handlers ─────────────────────────────────────────────────

  @impl true
  def handle_resource_read("docs://api/users", _full_uri, state) do
    content = """
    # Users API

    ## GET /api/users
    Returns a list of users. Supports pagination via `page` and `per_page` params.

    ## POST /api/users
    Create a new user. Requires `name` and `email` fields.

    ## Authentication
    All endpoints require Bearer token authentication.
    """

    {:ok, %{uri: "docs://api/users", mimeType: "text/markdown", text: content}, state}
  end

  @impl true
  def handle_resource_read("config://app/settings", _full_uri, state) do
    config = %{
      "database" => %{"host" => "localhost", "port" => 5432, "pool_size" => 10},
      "cache" => %{"ttl" => 3600, "max_size" => 1000},
      "features" => %{"dark_mode" => true, "beta_features" => false}
    }

    {:ok,
     %{uri: "config://app/settings", mimeType: "application/json", text: Jason.encode!(config)},
     state}
  end

  @impl true
  def handle_resource_read("data://metrics/latest", _full_uri, state) do
    metrics = %{
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "cpu_usage" => 42.5,
      "memory_usage" => 68.2,
      "request_count" => 15234,
      "error_rate" => 0.02
    }

    {:ok,
     %{uri: "data://metrics/latest", mimeType: "application/json", text: Jason.encode!(metrics)},
     state}
  end

  @impl true
  def handle_resource_read(uri, _full_uri, state) do
    {:error, "Resource not found: #{uri}", state}
  end

  # ── Prompt Handlers ───────────────────────────────────────────────────

  @impl true
  def handle_prompt_get("summarize", args, state) do
    style = Map.get(args, "style", "concise")

    {:ok,
     %{
       description: "Summarization prompt with #{style} style",
       messages: [
         %{
           role: "user",
           content: %{
             type: "text",
             text:
               "Please provide a #{style} summary of the given content. Focus on key points and actionable insights."
           }
         }
       ]
     }, state}
  end

  @impl true
  def handle_prompt_get("analyze", args, state) do
    focus = Map.get(args, "focus", "general")

    {:ok,
     %{
       description: "Analysis prompt focused on #{focus}",
       messages: [
         %{
           role: "user",
           content: %{
             type: "text",
             text:
               "Analyze the following data with a focus on #{focus}. Provide insights and recommendations."
           }
         }
       ]
     }, state}
  end

  @impl true
  def handle_prompt_get(name, _args, state) do
    {:error, "Prompt not found: #{name}", state}
  end
end
