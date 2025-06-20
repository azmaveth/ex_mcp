# Getting Started with ExMCP

This directory contains a comprehensive demonstration of all ExMCP transport types and core features.

## Overview

ExMCP supports four different transport mechanisms, each with its own strengths:

1. **STDIO** - Standard input/output for subprocess communication
2. **HTTP** - Traditional HTTP request/response
3. **HTTP+SSE** - HTTP with Server-Sent Events for streaming
4. **Native BEAM** - Direct Erlang process communication

## The Servers

### 1. STDIO Server (`01_stdio_server.exs`)
- **Transport**: STDIO (subprocess)
- **Feature**: Tool - `hello` with multi-language support
- **Use Case**: Simple subprocess-based tools, CLI integration

### 2. HTTP Server (`02_http_server.exs`)
- **Transport**: HTTP without SSE
- **Feature**: Resources - `hello://world` and `hello://stats`
- **Use Case**: RESTful APIs, stateless operations

### 3. HTTP+SSE Server (`03_http_sse_server.exs`)
- **Transport**: HTTP with Server-Sent Events
- **Feature**: Prompts - `hello_generator` and `story_starter`
- **Also**: Subscribable resource `events://greetings`
- **Use Case**: Real-time updates, streaming responses

### 4. Native BEAM Server (`04_beam_server.exs`)
- **Transport**: Native BEAM (Erlang processes)
- **Features**: All three - tool, resources, and prompt
  - Tool: `distributed_hello` - Send messages between nodes
  - Resources: `beam://system/info` and `beam://messages`
  - Prompt: `beam_expert` - BEAM/OTP expertise
- **Use Case**: High-performance, distributed systems

## Running the Demo

### Option 1: Run Everything at Once

```bash
cd examples/getting_started
./run_demo.sh
```

Or run the client directly:
```bash
elixir demo_client.exs
```

This will:
1. Start each server in sequence
2. Connect to it using the appropriate transport
3. Demonstrate its features
4. Clean up and move to the next server

### Option 2: Run Servers Individually

Start any server standalone:

```bash
# Terminal 1: Start STDIO server
elixir 01_stdio_server.exs

# Terminal 2: Start HTTP server
elixir 02_http_server.exs

# Terminal 3: Start HTTP+SSE server
elixir 03_http_sse_server.exs

# Terminal 4: Start BEAM server
elixir 04_beam_server.exs
```

Then connect with your own client or use parts of `demo_client.exs`.

## Key Concepts Demonstrated

### Tools
- Perform actions and computations
- Accept structured arguments
- Return content (text, JSON, etc.)

### Resources
- Provide data that can be read
- Can be subscribable for real-time updates
- Support different MIME types

### Prompts
- Generate conversation templates
- Accept arguments to customize output
- Return message sequences for LLMs

### Transport Features

**STDIO**:
- Simple and universal
- Works with any language that can spawn processes
- Great for CLI tools

**HTTP**:
- Standard web protocols
- Easy to debug and monitor
- Works through firewalls

**HTTP+SSE**:
- Real-time streaming
- Server push capabilities
- Progress updates and live data

**Native BEAM**:
- Zero serialization overhead
- Direct process communication
- Access to OTP features
- Distributed by design

## Understanding the Code

Each server follows the same pattern:

1. **Define features** using DSL macros:
   ```elixir
   deftool "name" do
     description "What it does"
     args do
       field :param, :string
     end
   end
   ```

2. **Implement handlers**:
   ```elixir
   @impl true
   def handle_tool_call("name", args, state) do
     # Process and return result
     {:ok, %{content: [text("Result")]}, state}
   end
   ```

3. **Start the server**:
   ```elixir
   Server.start_link(
     transport: :stdio,  # or :http, :native
     name: :server_name
   )
   ```

## Next Steps

1. **Modify the servers** - Add new tools, resources, or prompts
2. **Create your own** - Use these as templates for your MCP servers
3. **Explore advanced features** - Check the other examples for more complex scenarios
4. **Build something real** - MCP is great for AI tool integration!

## Tips

- Use STDIO for simple tools and CLI integration
- Use HTTP when you need standard web compatibility
- Use HTTP+SSE when you need real-time features
- Use Native BEAM for maximum performance and Elixir/OTP integration
- Always handle errors gracefully
- Keep state management simple and predictable

Happy coding with ExMCP! 🚀