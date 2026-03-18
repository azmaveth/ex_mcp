# ExMCP Conformance Test Server
#
# Implements the "everything server" pattern for MCP conformance testing.
# Start with: elixir test/conformance/server.exs [port]
# Then run:   npx @modelcontextprotocol/conformance server --url http://localhost:PORT/mcp
#
# Provides tools, resources, and prompts for the conformance test suite.

Mix.install([{:ex_mcp, path: "."}, {:plug_cowboy, "~> 2.7"}, {:jason, "~> 1.4"}])

defmodule ConformanceServer do
  use ExMCP.Server.Handler

  @impl true
  def init(_args) do
    {:ok, %{subscriptions: MapSet.new()}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       name: "ex_mcp-conformance",
       version: "0.9.0",
       capabilities: %{
         tools: %{listChanged: true},
         resources: %{subscribe: true, listChanged: true},
         prompts: %{listChanged: true},
         logging: %{}
       }
     }, state}
  end

  # ── Tools ──────────────────────────────────────────────────────

  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "echo",
        description: "Echoes back the input text",
        inputSchema: %{
          type: "object",
          properties: %{
            text: %{type: "string", description: "Text to echo"}
          },
          required: ["text"]
        }
      },
      %{
        name: "add",
        description: "Adds two numbers together",
        inputSchema: %{
          type: "object",
          properties: %{
            a: %{type: "number", description: "First number"},
            b: %{type: "number", description: "Second number"}
          },
          required: ["a", "b"]
        }
      },
      %{
        name: "long_running",
        description: "A tool that simulates a long-running operation with progress",
        inputSchema: %{
          type: "object",
          properties: %{
            steps: %{type: "integer", description: "Number of steps", default: 5}
          }
        }
      },
      %{
        name: "sample_llm",
        description: "A tool that requests LLM sampling from the client",
        inputSchema: %{
          type: "object",
          properties: %{
            prompt: %{type: "string", description: "The prompt to send"},
            max_tokens: %{type: "integer", description: "Maximum tokens", default: 100}
          },
          required: ["prompt"]
        }
      }
    ]

    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("echo", %{"text" => text}, state) do
    {:ok, [%{type: "text", text: "Echo: #{text}"}], state}
  end

  def handle_call_tool("add", %{"a" => a, "b" => b}, state) do
    result = a + b
    {:ok, [%{type: "text", text: "#{result}"}], state}
  end

  def handle_call_tool("long_running", args, state) do
    steps = Map.get(args, "steps", 5)

    for i <- 1..steps do
      Process.sleep(100)
      # Progress would be sent via notification in a real implementation
      IO.puts("[conformance] Step #{i}/#{steps}")
    end

    {:ok, [%{type: "text", text: "Completed #{steps} steps"}], state}
  end

  def handle_call_tool("sample_llm", %{"prompt" => prompt} = args, state) do
    max_tokens = Map.get(args, "max_tokens", 100)

    {:ok,
     [
       %{
         type: "text",
         text: "Sample result for: #{prompt} (max_tokens: #{max_tokens})"
       }
     ], state}
  end

  def handle_call_tool(name, _args, state) do
    {:error, "Unknown tool: #{name}", state}
  end

  # ── Resources ──────────────────────────────────────────────────

  @impl true
  def handle_list_resources(_cursor, state) do
    resources = [
      %{
        uri: "test://static/hello",
        name: "Hello Resource",
        description: "A static test resource",
        mimeType: "text/plain"
      },
      %{
        uri: "test://static/json",
        name: "JSON Resource",
        description: "A JSON test resource",
        mimeType: "application/json"
      }
    ]

    {:ok, resources, nil, state}
  end

  @impl true
  def handle_read_resource("test://static/hello", state) do
    {:ok, [%{uri: "test://static/hello", mimeType: "text/plain", text: "Hello, World!"}], state}
  end

  def handle_read_resource("test://static/json", state) do
    json = Jason.encode!(%{message: "Hello from JSON", timestamp: DateTime.utc_now()})

    {:ok, [%{uri: "test://static/json", mimeType: "application/json", text: json}], state}
  end

  def handle_read_resource(uri, state) do
    {:error, "Resource not found: #{uri}", state}
  end

  # ── Resource Templates ─────────────────────────────────────────

  @impl true
  def handle_list_resource_templates(_cursor, state) do
    templates = [
      %{
        uriTemplate: "test://dynamic/{name}",
        name: "Dynamic Resource",
        description: "A dynamically generated resource"
      }
    ]

    {:ok, templates, nil, state}
  end

  # ── Prompts ────────────────────────────────────────────────────

  @impl true
  def handle_list_prompts(_cursor, state) do
    prompts = [
      %{
        name: "simple_prompt",
        description: "A simple test prompt",
        arguments: [
          %{name: "name", description: "Name to greet", required: true}
        ]
      },
      %{
        name: "complex_prompt",
        description: "A prompt with optional arguments",
        arguments: [
          %{name: "topic", description: "Topic to discuss", required: true},
          %{name: "style", description: "Writing style", required: false}
        ]
      }
    ]

    {:ok, prompts, nil, state}
  end

  @impl true
  def handle_get_prompt("simple_prompt", %{"name" => name}, state) do
    {:ok,
     %{
       description: "A greeting prompt",
       messages: [
         %{role: "user", content: %{type: "text", text: "Hello, #{name}!"}}
       ]
     }, state}
  end

  def handle_get_prompt("complex_prompt", args, state) do
    topic = Map.get(args, "topic", "general")
    style = Map.get(args, "style", "casual")

    {:ok,
     %{
       description: "A discussion prompt about #{topic}",
       messages: [
         %{
           role: "user",
           content: %{
             type: "text",
             text: "Discuss #{topic} in a #{style} style."
           }
         }
       ]
     }, state}
  end

  def handle_get_prompt(name, _args, state) do
    {:error, "Unknown prompt: #{name}", state}
  end
end

# Parse port from args
port = String.to_integer(List.first(System.argv(), "3001"))

IO.puts("Starting ExMCP conformance server on port #{port}...")

IO.puts(
  "Test with: npx @modelcontextprotocol/conformance server --url http://localhost:#{port}/mcp"
)

# Start Cowboy with our MCP handler
children = [
  {Plug.Cowboy,
   scheme: :http,
   plug:
     {ExMCP.HttpPlug,
      handler: ConformanceServer,
      server_info: %{name: "ex_mcp-conformance", version: "0.9.0"},
      sse_enabled: true,
      cors_enabled: true},
   options: [port: port]}
]

{:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)

IO.puts("Server ready on http://localhost:#{port}/mcp")

# Keep the process alive
Process.sleep(:infinity)
