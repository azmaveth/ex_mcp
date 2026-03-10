# ACP Guide

The [Agent Client Protocol (ACP)](https://agentclientprotocol.com/) is a standardized protocol for controlling coding agents programmatically. ExMCP includes a full ACP client implementation, letting you start agent sessions, send prompts, receive streaming updates, and handle permission requests — all from Elixir.

## Overview

ACP uses JSON-RPC 2.0 over stdio (the same wire format as MCP) with methods for session management and bidirectional communication. 24 of 27 coding agents speak ACP natively. For the remaining agents (like Claude Code and Codex), ExMCP provides an adapter system that translates between ACP and the agent's native protocol.

### Architecture

```
Your Elixir App
    │
    ▼
ExMCP.ACP.Client (GenServer)
    │
    ├─── Native ACP agents (Gemini CLI, OpenCode, Qwen Code, ...)
    │       └── stdio JSON-RPC directly
    │
    └─── Adapted agents (Claude Code, Codex, ...)
            └── AdapterBridge → Adapter → agent-native protocol
```

## Quick Start

### Native ACP Agent

```elixir
# Start a client connected to a native ACP agent
{:ok, client} = ExMCP.ACP.start_client(command: ["gemini", "--acp"])

# Create a session rooted at a project directory
{:ok, %{"sessionId" => session_id}} =
  ExMCP.ACP.Client.new_session(client, "/path/to/project")

# Send a prompt and wait for the result
{:ok, %{"stopReason" => reason}} =
  ExMCP.ACP.Client.prompt(client, session_id, "Fix the failing tests")

# Cancel a running prompt
ExMCP.ACP.Client.cancel(client, session_id)

# Clean up
ExMCP.ACP.Client.disconnect(client)
```

### Adapted Agent (Claude Code)

```elixir
{:ok, client} = ExMCP.ACP.start_client(
  command: ["claude"],
  adapter: ExMCP.ACP.Adapters.Claude
)

{:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/my/project")
{:ok, result} = ExMCP.ACP.Client.prompt(client, sid, "Refactor the auth module")
```

### Adapted Agent (Codex)

```elixir
{:ok, client} = ExMCP.ACP.start_client(
  command: ["codex"],
  adapter: ExMCP.ACP.Adapters.Codex
)
```

## Client Options

| Option | Default | Description |
|--------|---------|-------------|
| `:command` | (required) | Command list for the agent subprocess |
| `:adapter` | `nil` | Adapter module for non-native agents |
| `:handler` | `DefaultHandler` | Module implementing `ExMCP.ACP.Client.Handler` |
| `:handler_opts` | `[]` | Options passed to `handler.init/1` |
| `:event_listener` | `nil` | PID to receive `{:acp_session_update, sid, update}` messages |
| `:client_info` | `%{"name" => "ex_mcp", ...}` | Client identification |
| `:capabilities` | `%{}` | Client capabilities map |
| `:protocol_version` | `1` | ACP protocol version (integer) |
| `:name` | `nil` | GenServer name registration |

## Session Lifecycle

ACP sessions represent ongoing conversations with an agent.

```elixir
# Create a new session
{:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/project",
  mcp_servers: [%{"name" => "my-server", "url" => "http://localhost:3000"}]
)

# Resume an existing session
{:ok, %{"sessionId" => ^sid}} = ExMCP.ACP.Client.load_session(client, sid, "/project")

# Send prompts (blocks until agent responds)
{:ok, result} = ExMCP.ACP.Client.prompt(client, sid, "Add error handling")

# Configure the agent at runtime
ExMCP.ACP.Client.set_mode(client, sid, "fast")
ExMCP.ACP.Client.set_config_option(client, sid, "temperature", 0.5)
```

## Handling Session Events

Implement the `ExMCP.ACP.Client.Handler` behaviour to react to streaming updates and agent requests:

```elixir
defmodule MyApp.ACPHandler do
  @behaviour ExMCP.ACP.Client.Handler

  @impl true
  def init(_opts), do: {:ok, %{}}

  @impl true
  def handle_session_update(session_id, %{"sessionUpdate" => "agent_message_chunk"} = update, state) do
    IO.write(update["content"]["text"])
    {:ok, state}
  end

  def handle_session_update(_session_id, %{"sessionUpdate" => "tool_call"} = update, state) do
    IO.puts("Agent calling tool: #{update["title"]}")
    {:ok, state}
  end

  def handle_session_update(_session_id, _update, state) do
    {:ok, state}
  end

  @impl true
  def handle_permission_request(_session_id, tool_call, options, state) do
    # Auto-allow all tool calls (or implement your own approval logic)
    allow_option = Enum.find(options, &(&1["kind"] == "allow_once")) || List.first(options)
    {:ok, %{"outcome" => "selected", "optionId" => allow_option["optionId"]}, state}
  end
end

# Use the custom handler
{:ok, client} = ExMCP.ACP.start_client(
  command: ["gemini", "--acp"],
  handler: MyApp.ACPHandler
)
```

### Event Listener

For simple use cases, receive session updates as process messages instead of implementing a full handler:

```elixir
{:ok, client} = ExMCP.ACP.start_client(
  command: ["gemini", "--acp"],
  event_listener: self()
)

# In your receive loop or GenServer
receive do
  {:acp_session_update, session_id, %{"sessionUpdate" => "agent_message_chunk"} = update} ->
    IO.write(update["content"]["text"])
end
```

## Writing Custom Adapters

To support an agent that doesn't speak ACP natively, implement the `ExMCP.ACP.Adapter` behaviour:

```elixir
defmodule MyApp.CustomAgentAdapter do
  @behaviour ExMCP.ACP.Adapter

  @impl true
  def init(opts) do
    {:ok, %{model: Keyword.get(opts, :model, "default")}}
  end

  @impl true
  def command(opts) do
    {"my-agent", ["--json-mode"]}
  end

  @impl true
  def translate_outbound(%{"method" => "session/prompt"} = msg, state) do
    # Convert ACP prompt to agent's native format
    prompt_text = get_in(msg, ["params", "prompt"])
    native_json = Jason.encode!(%{"action" => "ask", "text" => prompt_text})
    {:ok, [native_json, "\n"], state}
  end

  def translate_outbound(_msg, state) do
    {:ok, :skip, state}
  end

  @impl true
  def translate_inbound(line, state) do
    case Jason.decode(line) do
      {:ok, %{"type" => "response", "text" => text}} ->
        acp_result = %{
          "jsonrpc" => "2.0",
          "id" => state.pending_id,
          "result" => %{"stopReason" => "end_turn"}
        }
        {:messages, [acp_result], state}

      {:ok, %{"type" => "stream", "delta" => delta}} ->
        notification = %{
          "jsonrpc" => "2.0",
          "method" => "session/update",
          "params" => %{
            "sessionId" => "default",
            "update" => %{
              "sessionUpdate" => "agent_message_chunk",
              "content" => %{"type" => "text", "text" => delta}
            }
          }
        }
        {:messages, [notification], state}

      _ ->
        {:skip, state}
    end
  end
end
```

### Adapter Callbacks

| Callback | Required | Description |
|----------|----------|-------------|
| `init/1` | Yes | Initialize adapter state |
| `command/1` | Yes | Return `{executable, args}` or `:one_shot` |
| `translate_outbound/2` | Yes | Convert ACP message to native format |
| `translate_inbound/2` | Yes | Convert native output to ACP messages |
| `post_connect/1` | No | Send initial data after port opens |
| `capabilities/0` | No | Return static agent capabilities |

## Built-in Adapters

### Claude Code (`ExMCP.ACP.Adapters.Claude`)

Translates between ACP and Claude's NDJSON stream-json protocol:
- Launches Claude with `--output-format stream-json --input-format stream-json --verbose`
- Maps `stream_event` text deltas to `session/update` notifications (`agent_message_chunk`)
- Maps `stream_event` thinking deltas to `session/update` notifications (`thinking`)
- Maps `result` events to prompt response results

### Codex (`ExMCP.ACP.Adapters.Codex`)

Translates between ACP and Codex's app-server JSON-RPC protocol:
- Launches `codex app-server` in persistent mode
- Performs its own JSON-RPC initialize handshake
- Maps `thread/start`, `turn/start`, `turn/completed` to ACP session methods
- Maps item deltas and reasoning to `session/update` notifications

## MCP Server Integration

ACP agents can use MCP servers as tool providers. Pass MCP server configurations when creating sessions:

```elixir
{:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/project",
  mcp_servers: [
    %{
      "name" => "my-tools",
      "url" => "http://localhost:4000/mcp",
      "transport" => "http"
    }
  ]
)
```

## API Reference

- `ExMCP.ACP` -- Facade module
- `ExMCP.ACP.Client` -- GenServer client with full session API
- `ExMCP.ACP.Client.Handler` -- Handler behaviour
- `ExMCP.ACP.Protocol` -- ACP JSON-RPC message encoding
- `ExMCP.ACP.Types` -- Type specs and builders
- `ExMCP.ACP.Adapter` -- Adapter behaviour for non-native agents
- `ExMCP.ACP.Adapters.Claude` -- Claude Code adapter
- `ExMCP.ACP.Adapters.Codex` -- Codex adapter
