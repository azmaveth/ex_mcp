# ExMCP v2 Getting Started Examples

These examples demonstrate the dramatic improvements of the v2 DSL over the legacy v1 approach.

## 📊 Code Reduction Comparison

| Example | v1 Lines | v2 Lines | Reduction |
|---------|----------|----------|-----------|
| Native Server | ~89 | ~15 | 83% |
| stdio Server | ~151 | ~50 | 67% |
| HTTP Server | ~175 | ~80 | 54% |
| SSE Server | ~275 | ~100 | 64% |

## 🚀 v2 DSL Benefits

- **Clean, declarative syntax** - Define tools, resources, and prompts with minimal boilerplate
- **Auto-capability detection** - Server capabilities are automatically determined from definitions
- **Compile-time validation** - Catch errors early with macro validation
- **Built-in JSON Schema** - Automatic schema generation from field definitions
- **Smart content helpers** - `text()`, `json()`, `image()`, `user()`, `system()`, etc.
- **Zero-config transports** - Built-in support for all MCP transport types

## 📁 Example Files

### Servers (Run these first)
- `native_server.exs` - Native BEAM transport with ultra-fast Elixir-to-Elixir communication
- `stdio_server.exs` - Standard input/output transport for subprocess communication
- `simple_http_server.exs` - HTTP transport with built-in CORS and JSON-RPC
- `sse_http_server.exs` - HTTP with Server-Sent Events for real-time streaming

### Client
- `hello_world.exs` - Client that connects to all server types and demonstrates their features

## 🎯 Running the Examples

1. **First compile the project:**
   ```bash
   mix deps.get
   mix compile
   ```

2. **Start a server (in one terminal):**
   ```bash
   # Native BEAM server
   elixir examples/v2/getting_started/native_server.exs
   
   # OR stdio server
   elixir examples/v2/getting_started/stdio_server.exs
   
   # OR HTTP server
   elixir examples/v2/getting_started/simple_http_server.exs
   
   # OR SSE server
   elixir examples/v2/getting_started/sse_http_server.exs
   ```

3. **Run the client (in another terminal):**
   ```bash
   elixir examples/v2/getting_started/hello_world.exs
   ```

## 📝 Example: v2 DSL Server Definition

```elixir
defmodule MyServerV2 do
  use ExMCP.ServerV2
  
  # Define a tool with automatic JSON Schema generation
  deftool "greet" do
    tool_description "Greet someone warmly"
    
    args do
      field :name, :string, required: true, description: "Person to greet"
      field :language, :string, enum: ["en", "es", "fr"], default: "en"
    end
  end
  
  # Define a resource
  defresource "config://settings" do
    resource_name "Server Settings"
    resource_description "Current server configuration"
    mime_type "application/json"
  end
  
  # Define a prompt
  defprompt "assistant" do
    prompt_name "AI Assistant"
    prompt_description "Helpful AI assistant"
    
    arguments do
      arg :task, required: true
      arg :context
    end
  end
  
  # Implement handlers
  @impl true
  def handle_tool_call("greet", %{"name" => name, "language" => lang}, state) do
    greeting = case lang do
      "es" -> "¡Hola, #{name}!"
      "fr" -> "Bonjour, #{name}!"
      _ -> "Hello, #{name}!"
    end
    {:ok, %{content: [text(greeting)]}, state}
  end
end
```

Compare this to the v1 approach which would require:
- Manual capability definitions
- Hand-written JSON Schema
- Boilerplate GenServer code
- Manual protocol handling
- 150+ lines of code!

## 🔧 Transport-Specific Features

### Native BEAM Transport
- Direct process communication
- No JSON serialization overhead
- Auto-discovery via Registry
- Perfect for Elixir-to-Elixir communication

### stdio Transport
- Subprocess communication
- JSON-RPC over stdin/stdout
- Works with any language that can read/write stdio

### HTTP Transport
- RESTful API with JSON-RPC
- Built-in CORS support
- Standard HTTP request/response

### HTTP+SSE Transport
- Real-time streaming
- Server-Sent Events
- Perfect for long-running operations
- Progress updates and live data

## 🎨 Content Helpers

The v2 DSL provides smart content constructors:

```elixir
# Text content
text("Hello, world!")

# JSON content (auto-serialized)
json(%{status: "ok", count: 42})

# Image content
image("base64_data", "image/png")

# Resource reference
resource("file://path/to/file")

# Prompt messages
user("What's the weather?")
system("You are a helpful assistant")
assistant("I'd be happy to help!")
```

## 🚀 Next Steps

1. Try modifying the servers to add your own tools
2. Experiment with different transport types
3. Build your own MCP server using the v2 DSL
4. Check out the [MCP specification](https://github.com/anthropics/mcp) for more details

The v2 DSL makes building MCP servers incredibly simple while providing more functionality than ever before!