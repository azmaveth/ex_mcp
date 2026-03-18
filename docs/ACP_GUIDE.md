# ACP Guide

The [Agent Client Protocol (ACP)](https://agentclientprotocol.com/) is a standardized protocol for controlling coding agents programmatically. ExMCP includes a full ACP client implementation, letting you start agent sessions, send prompts, receive streaming updates, and handle permission requests — all from Elixir.

## Overview

ACP uses JSON-RPC 2.0 over stdio (the same wire format as MCP) with methods for session management and bidirectional communication. Most coding agents speak ACP natively. For agents with their own protocols (Claude Code, Codex, Pi), ExMCP provides an adapter system that translates between ACP and the agent's native protocol.

### Architecture

```
Your Elixir App
    │
    ▼
ExMCP.ACP.Client (GenServer)
    │
    ├─── Native ACP agents (Gemini CLI, Hermes, OpenCode, Qwen Code, ...)
    │       └── stdio JSON-RPC directly
    │
    └─── Adapted agents (Claude Code, Codex, Pi)
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
  adapter: ExMCP.ACP.Adapters.Claude,
  adapter_opts: [model: "sonnet", cwd: "/my/project"]
)

{:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/my/project")
{:ok, result} = ExMCP.ACP.Client.prompt(client, sid, "Refactor the auth module")
```

### Adapted Agent (Codex)

```elixir
{:ok, client} = ExMCP.ACP.start_client(
  command: ["codex"],
  adapter: ExMCP.ACP.Adapters.Codex,
  adapter_opts: [model: "gpt-4o"]
)
```

### Adapted Agent (Pi)

```elixir
{:ok, client} = ExMCP.ACP.start_client(
  command: ["pi"],
  adapter: ExMCP.ACP.Adapters.Pi,
  adapter_opts: [
    model: "anthropic/claude-sonnet-4",
    thinking_level: "medium",
    session_path: "/path/to/session.jsonl"  # optional: resume session
  ]
)
```

## Client Options

| Option | Default | Description |
|--------|---------|-------------|
| `:command` | (required) | Command list for the agent subprocess |
| `:adapter` | `nil` | Adapter module for non-native agents |
| `:adapter_opts` | `[]` | Options passed to adapter's `init/1` |
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
{:ok, _} = ExMCP.ACP.Client.load_session(client, sid, "/project")

# List available sessions (if agent supports it)
{:ok, %{"sessions" => sessions}} = ExMCP.ACP.Client.list_sessions(client)

# Send prompts (blocks until agent responds)
{:ok, result} = ExMCP.ACP.Client.prompt(client, sid, "Add error handling")

# Cancel a running prompt
ExMCP.ACP.Client.cancel(client, sid)

# Configure the agent at runtime
ExMCP.ACP.Client.set_mode(client, sid, "code")
ExMCP.ACP.Client.set_config_option(client, sid, "model", "claude-sonnet-4")

# Authenticate (if agent requires it)
ExMCP.ACP.Client.authenticate(client, %{"provider" => "api_key"})
```

## Handling Session Events

Implement the `ExMCP.ACP.Client.Handler` behaviour to react to streaming updates and agent requests:

```elixir
defmodule MyApp.ACPHandler do
  @behaviour ExMCP.ACP.Client.Handler

  @impl true
  def init(_opts), do: {:ok, %{}}

  @impl true
  def handle_session_update(session_id, update, state) do
    case update["sessionUpdate"] do
      "agent_message_chunk" ->
        IO.write(update["content"]["text"])

      "tool_call_update" ->
        status = update["status"]  # "running", "completed", or "failed"
        IO.puts("[#{status}] #{update["title"]}")

        # Rich metadata available for Claude adapter:
        # update["kind"]      — "read", "write", "execute", "search", "think"
        # update["locations"] — [%{"path" => "/src/app.ex", "line" => 10}]
        # update["content"]   — [%{"type" => "diff", "oldText" => ..., "newText" => ...}]

      "plan_update" ->
        for entry <- update["entries"] do
          IO.puts("  [#{entry["status"]}] #{entry["content"]}")
        end

      "thinking" ->
        IO.write(update["content"])

      "usage" ->
        IO.puts("Tokens: #{update["inputTokens"]} in / #{update["outputTokens"]} out")

      _ ->
        :ok
    end

    {:ok, state}
  end

  @impl true
  def handle_permission_request(_session_id, tool_call, options, state) do
    allow_option = Enum.find(options, &(&1["kind"] == "allow_once")) || List.first(options)
    {:ok, %{"outcome" => "selected", "optionId" => allow_option["optionId"]}, state}
  end

  # Optional: handle file read requests from the agent
  def handle_file_read(_session_id, path, _opts, state) do
    case File.read(path) do
      {:ok, content} -> {:ok, content, state}
      {:error, reason} -> {:error, to_string(reason), state}
    end
  end

  # Optional: handle terminal requests from the agent
  def handle_terminal_request(method, params, _id, state) do
    # Handle terminal/create, terminal/output, terminal/kill, etc.
    {:error, "Terminal operations not implemented", state}
  end
end
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
  {:acp_session_update, session_id, %{"sessionUpdate" => type} = update} ->
    IO.puts("#{type}: #{inspect(update)}")
end
```

## Session Update Types

The ACP spec defines these session update types (all supported by ExMCP):

| Type | Description |
|------|-------------|
| `agent_message_chunk` | Streaming text/image content from the agent |
| `user_message_chunk` | Echo of user input |
| `tool_call_update` | Tool call lifecycle (running → completed/failed) |
| `plan_update` | Multi-step execution plan with entry status |
| `available_commands_update` | Slash commands the agent supports |
| `config_option_update` | Runtime config change notification |
| `current_mode_update` | Operational mode change |
| `session_info_update` | Session metadata (name, etc.) |

ExMCP adapters also emit these extension types:

| Type | Description |
|------|-------------|
| `thinking` | Agent reasoning/thinking content |
| `status` | Operational status (compacting, retrying, etc.) |
| `usage` | Token usage tracking |
| `tool_execution` | Tool execution progress (Pi adapter) |
| `tool_result` | Tool output content |

## Writing Custom Adapters

To support an agent that doesn't speak ACP natively, implement the `ExMCP.ACP.Adapter` behaviour:

```elixir
defmodule MyApp.CustomAgentAdapter do
  @behaviour ExMCP.ACP.Adapter

  @impl true
  def init(opts), do: {:ok, %{model: Keyword.get(opts, :model, "default")}}

  @impl true
  def command(_opts), do: {"my-agent", ["--json-mode"]}

  @impl true
  def capabilities, do: %{"streaming" => true}

  # Optional: declare supported modes
  @impl true
  def modes do
    [%{"id" => "fast", "name" => "Fast Mode"}, %{"id" => "quality", "name" => "Quality Mode"}]
  end

  # Optional: declare config options
  @impl true
  def config_options do
    [%{"id" => "model", "name" => "Model", "category" => "model", "type" => "string"}]
  end

  # Optional: list available sessions
  @impl true
  def list_sessions(state) do
    sessions = [%{"sessionId" => "sess-1", "name" => "My Session"}]
    {:ok, sessions, state}
  end

  @impl true
  def translate_outbound(%{"method" => "session/prompt", "params" => params}, state) do
    text = hd(params["prompt"])["text"]
    {:ok, [Jason.encode!(%{"action" => "ask", "text" => text}), "\n"], state}
  end

  def translate_outbound(_msg, state), do: {:ok, :skip, state}

  @impl true
  def translate_inbound(line, state) do
    case Jason.decode(line) do
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
| `modes/0` | No | Return supported operational modes |
| `config_options/0` | No | Return supported config options |
| `list_sessions/1` | No | Return available sessions |

## Built-in Adapters

### Claude Code (`ExMCP.ACP.Adapters.Claude`)

Translates between ACP and Claude's NDJSON stream-json protocol.

**Features:**
- Streaming text and thinking blocks with deduplication
- Multi-turn tool use cycle tracking
- Zed-parity tool introspection: `kind`, `locations` (file:line), `content` (diff/terminal)
- Context-aware tool titles: "Read lib/app.ex (10-29)", "Search: defmodule"
- Project-relative display paths when cwd is known
- Stop reason classification: end_turn, max_tokens, tool_use, error
- Usage tracking with cache token support
- System event and rate limit forwarding

**Config options:** `model`, `thinking_budget`

### Codex (`ExMCP.ACP.Adapters.Codex`)

Translates between ACP and Codex's app-server JSON-RPC protocol.

**Features:**
- Initialize handshake with `post_connect/1`
- Tool call lifecycle: creation, completion, output, patch events
- Command execution streaming (started/outputDelta/completed)
- Web search events
- Session resume via `session/load` → `thread/start` with threadId
- Image content in prompts

**Modes:** suggest, auto-edit, full-auto
**Config options:** `model`

### Pi (`ExMCP.ACP.Adapters.Pi`)

Translates between ACP and Pi's RPC NDJSON protocol.

**Features:**
- All 25 Pi RPC commands (prompt, steer, follow_up, compact, model switching, etc.)
- All 14 Pi event types (text/thinking streaming, tool execution, compaction, retry)
- Extension UI request/response bridge for dialog flows
- Session persistence via `--session` flag
- Session directory scanning for `list_sessions`
- Enhanced tool result parsing (content blocks, diffs, stdout/stderr/exitCode)
- Image support with data-url prefix stripping

**Config options:** `model`, `thinking_level`, `auto_compaction`, `auto_retry`, `steering_mode`, `follow_up_mode`

**Extended commands** (via `pi/*` ACP methods): steer, follow_up, compact, set_thinking_level, set_model, get_state, get_session_stats, switch_session, fork, bash, export_html, and more.

## Content Block Types

ACP supports these content block types in prompts and responses:

```elixir
alias ExMCP.ACP.Types

# Text
Types.text_block("Hello, world!")

# Images
Types.image_block("image/png", "base64data...")

# Audio
Types.audio_block("audio/wav", "base64data...")

# Resource links (references to external resources)
Types.resource_link_block("file:///src/app.ex", name: "app.ex")

# Embedded resources
Types.resource_block("file:///src/app.ex", text: "defmodule App do...")

# Plan entries
Types.plan_entry("Fix the auth bug", "high", "in_progress")

# Plan update notification
Types.plan_update(session_id, [
  Types.plan_entry("Read the code", "high", "completed"),
  Types.plan_entry("Write the fix", "high", "in_progress"),
  Types.plan_entry("Run tests", "medium", "pending")
])
```

## MCP Server Integration

ACP agents can use MCP servers as tool providers. Pass MCP server configurations when creating sessions:

```elixir
{:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/project",
  mcp_servers: [
    %{"uri" => "http://localhost:4000/mcp", "name" => "my-tools"}
  ]
)
```

## API Reference

- `ExMCP.ACP` — Facade module
- `ExMCP.ACP.Client` — GenServer client with full session API
- `ExMCP.ACP.Client.Handler` — Handler behaviour
- `ExMCP.ACP.Protocol` — ACP JSON-RPC message encoding
- `ExMCP.ACP.Types` — Type specs and builders
- `ExMCP.ACP.Adapter` — Adapter behaviour for non-native agents
- `ExMCP.ACP.AdapterBridge` — GenServer bridge managing Port and message queue
- `ExMCP.ACP.Adapters.Claude` — Claude Code adapter
- `ExMCP.ACP.Adapters.Codex` — Codex adapter
- `ExMCP.ACP.Adapters.Pi` — Pi adapter
