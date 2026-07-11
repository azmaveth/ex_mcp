# ACP Guide

The [Agent Client Protocol (ACP)](https://agentclientprotocol.com/) is a standardized protocol for controlling coding agents programmatically. ExMCP includes a full ACP client implementation, letting you start agent sessions, send prompts, receive streaming updates, and handle permission requests — all from Elixir. It also includes `ExMCP.ACP.Agent` for building native Elixir ACP agents.

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

ACP Client
    │
    ▼
ExMCP.ACP.Agent (GenServer)
    │
    └─── Your Elixir handler
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

### Native Elixir ACP Agent

Use `ExMCP.ACP.Agent` when your Elixir application is the agent being controlled by an ACP client:

```elixir
defmodule MyApp.EchoAgent do
  @behaviour ExMCP.ACP.Agent.Handler

  @impl true
  def init(_opts), do: {:ok, %{}}

  @impl true
  def handle_new_session(_params, _ctx, state) do
    {:reply, %{"sessionId" => "sess_" <> Base.encode16(:crypto.strong_rand_bytes(8))}, state}
  end

  @impl true
  def handle_prompt(session_id, prompt, ctx, state) do
    text = prompt |> List.first() |> Map.get("text", "")

    ExMCP.ACP.Agent.agent_message(ctx.agent, session_id, "Echo: " <> text)
    {:reply, %{"stopReason" => "end_turn"}, state}
  end
end

ExMCP.ACP.run_agent(
  handler: MyApp.EchoAgent,
  agent_info: %{"name" => "echo-agent", "version" => "1.0.0"}
)
```

Prompt handlers can also stream updates and finish asynchronously:

```elixir
def handle_prompt(session_id, _prompt, ctx, state) do
  Task.start(fn ->
    ExMCP.ACP.Agent.agent_message(ctx.agent, session_id, "Working...")
    ExMCP.ACP.Agent.finish_prompt(ctx.agent, ctx.prompt_id, "end_turn")
  end)

  {:noreply, state}
end
```

### Adapted Agent (Claude Code)

```elixir
{:ok, client} = ExMCP.ACP.start_client(
  command: ["claude"],
  adapter: ExMCP.ACP.Adapters.ClaudeSDK,
  adapter_opts: [model: "sonnet", cwd: "/my/project"]
)

{:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/my/project")
{:ok, result} = ExMCP.ACP.Client.prompt(client, sid, "Refactor the auth module")
```

Use `ExMCP.ACP.Adapters.ClaudeSDK` for new Claude Code integrations. It speaks
the same SDK-style control protocol used by the official Claude Agent SDK, so it
can bridge permission prompts, partial tool lifecycle events, cancellation,
session setup, model/mode config, and richer status updates.

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
    session_path: "/path/to/session.jsonl"  # optional: resume session
  ]
)

{:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/my/project")
{:ok, _} = ExMCP.ACP.Client.set_config_option(client, sid, "model", "anthropic/claude-sonnet-4")
{:ok, _} = ExMCP.ACP.Client.set_config_option(client, sid, "thought_level", "medium")
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
| `:protocol_version` | `1` | ACP major protocol version (integer). Matches upstream [v1](https://agentclientprotocol.com/protocol/v1/overview); non-breaking features use capability negotiation. |
| `:name` | `nil` | GenServer name registration |

## Session Lifecycle

ACP sessions represent ongoing conversations with an agent.

```elixir
# Create a new session
{:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/project",
  additional_directories: ["/shared/docs"],
  mcp_servers: [
    ExMCP.ACP.Types.http_mcp_server("my-server", "http://localhost:3000/mcp")
  ]
)

# Load an existing session and replay conversation history
{:ok, _} = ExMCP.ACP.Client.load_session(client, sid, "/project")

# Resume an existing session without replaying history (if supported)
{:ok, _} = ExMCP.ACP.Client.resume_session(client, sid, "/project")

# List available sessions with optional filters (if supported)
{:ok, %{"sessions" => sessions}} =
  ExMCP.ACP.Client.list_sessions(client,
    cwd: "/project",
    additional_directories: ["/shared/docs"]
  )

# Delete a session from session history (if supported)
{:ok, %{}} = ExMCP.ACP.Client.delete_session(client, sid)

# Send prompts (blocks until agent responds)
{:ok, result} = ExMCP.ACP.Client.prompt(client, sid, "Add error handling")

# Cancel a running prompt
ExMCP.ACP.Client.cancel(client, sid)

# Cancel a specific JSON-RPC request when you have its request id
ExMCP.ACP.Client.cancel_request(client, request_id)

# Configure the agent at runtime
ExMCP.ACP.Client.set_mode(client, sid, "high")
ExMCP.ACP.Client.set_config_option(client, sid, "model", "anthropic/claude-sonnet-4")
ExMCP.ACP.Client.set_config_option(client, sid, "auto_retry", false)

# Close a session and free agent-side resources (if supported)
ExMCP.ACP.Client.close_session(client, sid)

# Authenticate or logout (if agent requires/supports it)
ExMCP.ACP.Client.authenticate(client, "api-key")
ExMCP.ACP.Client.logout(client)
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
        status = update["status"]  # "pending", "in_progress", "completed", or "failed"
        IO.puts("[#{status}] #{update["title"]}")

        # Rich metadata available for ClaudeSDK/Codex/Pi adapters:
        # update["kind"]      — "read", "edit", "execute", "search", "think"
        # update["locations"] — [%{"path" => "/src/app.ex", "line" => 10}]
        # update["content"]   — [%{"type" => "diff", "oldText" => ..., "newText" => ...}]

      "plan" ->
        for entry <- update["entries"] do
          IO.puts("  [#{entry["status"]}] #{entry["content"]}")
        end

      "agent_thought_chunk" ->
        IO.write(update["content"]["text"])

      "usage" ->
        IO.puts("Tokens: #{update["inputTokens"]} in / #{update["outputTokens"]} out")

      _ ->
        :ok
    end

    {:ok, state}
  end

  @impl true
  def handle_permission_request(_session_id, tool_call, options, state) do
    reject_option = Enum.find(options, &(&1["kind"] == "reject_once")) || List.first(options)

    # Return the direct outcome; ExMCP wraps it as result.outcome on the wire.
    case reject_option do
      nil -> {:ok, %{"outcome" => "cancelled"}, state}
      option -> {:ok, %{"outcome" => "selected", "optionId" => option["optionId"]}, state}
    end
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
| `agent_thought_chunk` | Streaming thought content from the agent |
| `tool_call` | New tool call started |
| `tool_call_update` | Tool call lifecycle (pending → in_progress → completed/failed) |
| `plan` | Multi-step execution plan with entry status |
| `available_commands_update` | Slash commands the agent supports |
| `config_option_update` | Runtime config change notification |
| `current_mode_update` | Operational mode change |
| `session_info_update` | Session metadata such as title and updatedAt |
| `usage_update` | Context window usage and optional cost information |

Adapter-specific status, error, and extension bridge details are attached under
`_meta.ex_mcp` on spec-defined update types, usually `session_info_update`.
Content chunks may include ACP's optional `messageId` field so clients can group
streamed chunks into logical messages.

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
  def capabilities, do: %{}

  # Optional: declare supported modes
  @impl true
  def modes do
    [%{"id" => "fast", "name" => "Fast Mode"}, %{"id" => "quality", "name" => "Quality Mode"}]
  end

  # Optional: declare config options
  @impl true
  def config_options do
    [
      %{
        "id" => "model",
        "name" => "Model",
        "category" => "model",
        "type" => "select",
        "currentValue" => "fast",
        "options" => [
          %{"value" => "fast", "name" => "Fast"},
          %{"value" => "quality", "name" => "Quality"}
        ]
      }
    ]
  end

  # Optional: list available sessions
  @impl true
  def list_sessions(params, state) do
    sessions = [
      %{
        "sessionId" => "sess-1",
        "cwd" => params["cwd"] || state.cwd,
        "title" => "My Session"
      }
    ]

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
| `command/1` | Yes | Return `{executable, args}`, `:one_shot`, or `:adapter_managed` |
| `translate_outbound/2` | Yes | Convert ACP message to native format |
| `translate_inbound/2` | Yes | Convert native output to ACP messages |
| `post_connect/1` | No | Send initial data after port opens |
| `handle_adapter_message/2` | No | Handle Port/process messages for adapter-managed subprocesses |
| `shutdown/1` | No | Clean up adapter-managed resources when the bridge closes |
| `env/1` | No | Return child-process environment variables |
| `capabilities/0` | No | Return static agent capabilities |
| `modes/0` | No | Return supported operational modes |
| `config_options/0` | No | Return supported config options |
| `auth_methods/1` | No | Return initialize `authMethods` for adapter options |
| `list_sessions/2` | No | Return a sessions list or full ACP `session/list` result for decoded params |
| `fork_session/2` | No | Fork an existing session for decoded `session/fork` params |

## Built-in Adapters

### Claude Code SDK (`ExMCP.ACP.Adapters.ClaudeSDK`)

Translates between ACP and Claude Code's SDK-compatible stream-json control
protocol. This is the recommended Claude adapter for new code.

**Features:**
- SDK entrypoint launch environment and `--permission-prompt-tool stdio`, tracking Claude Agent SDK `0.3.198`
- Partial message and pending tool-call lifecycle mapping
- `session/cancel` via SDK `interrupt`
- ACP permission requests bridged from Claude SDK `can_use_tool`, with pending `tool_call` emitted before the permission request
- Runtime mode, model, effort, fast-mode, and agent config controls where supported by the SDK session
- Initialize-aware terminal login auth methods, opt-in gateway auth methods, and ACP `auth.logout`
- Live session setup/load/resume/fork/close ACP surface
- Disk-backed `session/list`, `session/delete`, and `session/fork` for Claude Code's SDK store
- Full `session/load` replay from persisted Claude JSONL transcripts
- FIFO prompt queueing with queued prompt cancellation responses
- Plan updates from `TodoWrite` and task progress events
- Resource links, embedded text resources, HTTP/base64 images, and MCP slash-command prompt rewriting
- Rich tool metadata, Codex-style Bash terminal metadata, result usage updates, and improved stop reasons
- Official ACP `mcpCapabilities` plus ExMCP `_meta` support for BEAM-local MCP transport

`session/list`, `session/load`, `session/fork`, and `session/delete` read and mutate Claude Code's local
`CLAUDE_CONFIG_DIR/projects` JSONL store directly in Elixir, using the same
project-key derivation, UUID validation, sidechain filtering, and title
sanitization rules as the official Claude Agent SDK. `session/load` replays
persisted transcript entries as ACP `session/update` notifications before the
load response; `session/resume` keeps the lighter no-replay behavior.

The adapter advertises official ACP MCP support through `mcpCapabilities`
(`acp`, `http`, and `sse`). ExMCP's BEAM-local MCP transport is intentionally
advertised only as `_meta.ex_mcp.mcpCapabilities.beam`, so other ACP libraries can
ignore it while ExMCP peers can negotiate and validate BEAM-local descriptors.

**Config options:** `mode`, `model`, `effort`, `fast` (when the selected model supports fast mode), and `agent` (when custom main-thread agents are available). The legacy inbound `permission_mode` config id is still accepted as an alias for `mode`.

**Startup options:** `model`, `permission_mode`, `max_thinking_tokens`,
`effort`, `fast_mode`, `agent`, `additional_directories`, `mcp_servers`, `session_id`, `resume`,
`resume_session_at`, `allowed_tools`, `disallowed_tools`, `tools`,
`strict_mcp_config`, `include_partial_messages`, and `cli_path`.

### Codex (`ExMCP.ACP.Adapters.Codex`)

Translates between ACP and Codex's app-server JSON-RPC protocol.

**Features:**
- Initialize handshake with `post_connect/1`
- Model catalog loading from Codex `model/list`, ACP `model` config options, legacy `session/set_model` compatibility, and per-session `models` state
- Tool call lifecycle: creation, completion, output, patch events, and current camelCase app-server item variants
- Command execution streaming with ACP terminal metadata
- Web search, MCP tool, dynamic tool, file change, image view, image generation, guardian review, fuzzy file search, plan, status, goal, usage, and compaction events
- Session list/load/resume/close/delete through Codex app-server thread APIs
- Load-history replay from returned Codex turns when available, including tool history
- Image content, resource links, embedded text/binary resources, and additional workspace directories in prompts/session setup
- Codex slash commands in prompts: `/compact`, `/init`, `/review`, `/review-branch`, `/review-commit`, `/status`, and `/logout`
- ACP HTTP and stdio MCP server descriptors forwarded into Codex session config
- Codex auth methods for `chat-gpt`, `api-key`, and opt-in custom `gateway` auth
- Approval and MCP elicitation requests bridged through ACP `session/request_permission`

**Modes:** `read-only`, `agent`, `agent-full-access`. Legacy `suggest`, `auto-edit`, `auto`, `full-auto`, and `full-access` aliases are no longer accepted.
**Config options:** `mode`, `model`, `reasoning_effort`, and `fast-mode` (when supported by the selected model) are returned with Codex session responses. Runtime changes are kept in adapter session state and applied to subsequent `turn/start` requests.

**Unsupported Codex app-server requests:** Dynamic tool calls, request-user-input prompts, ChatGPT token refresh, and attestation generation are rejected explicitly because ACP does not provide compatible structured responses for those app-server request schemas.

### Pi (`ExMCP.ACP.Adapters.Pi`)

Translates between ACP and Pi's RPC NDJSON protocol.

**Features:**
- Adapter-managed Pi subprocesses for ACP `session/new`, `session/load`, and `session/resume`
- ACP-native `session/new`, `session/load`, `session/resume`, `session/list`, `session/close`, `session/delete`, `session/prompt`, `session/cancel`, `session/set_config_option`, and `session/set_mode`, with legacy `session/set_model` compatibility
- Terminal authentication method advertisement through `authMethods`
- Pi session discovery from JSONL files plus a local ExMCP session map at `~/.ex_mcp/pi/session-map.json`, with cursor pagination and last-cwd default filtering
- Prompt queuing while another Pi turn is active
- Per-session `model` and `thought_level` config options, with ACP config-option sync updates after model/thinking changes
- Global/project Pi settings merge for skill command filtering and quiet startup
- Startup info for Pi version, context, prompts, skills, extensions, and captured CLI prelude; registry update notices are opt-in
- Markdown slash commands loaded from `~/.pi/agent/prompts` and `<cwd>/.pi/prompts`
- Built-in slash commands: `/compact`, `/autocompact`, `/export`, `/session`, `/name`, `/steering`, `/follow-up`, and `/changelog`
- Text/thinking streaming, tool-call streaming, tool execution lifecycle, compaction, retry, and extension UI metadata events
- Enhanced tool result parsing with content blocks, structured edit diffs, stdout/stderr/exitCode formatting, and file locations
- Image support with data-url prefix stripping
- Resource links and embedded text resources folded into Pi prompt text; audio blocks are represented as unsupported markers

**Modes:** `off`, `minimal`, `low`, `medium`, `high`, `xhigh` map to Pi thinking levels through ACP `session/set_mode`.

**Config options:** Session responses include upstream-compatible `model` and `thought_level` selectors, plus ExMCP's existing `auto_compaction`, `auto_retry`, `steering_mode`, and `follow_up_mode` controls. Prefer `set_config_option/4` with config id `model` for model changes; `ExMCP.ACP.Client.set_model/3` is retained for compatibility with older adapters.

**Startup options:** `cli_path`/`pi_command`, `session_path`, `session_dir`, `session_map_path`, `delete_session_files`, and `update_notice`. The live Pi subprocess is started like upstream `pi-acp`, with `--mode rpc --no-themes` and optional `--session <path>`; cwd is applied as the child process working directory. `session/delete` removes ExMCP session-map state by default; backing Pi JSONL files are deleted only when `delete_session_files: true` is set and the file is under the configured Pi session directory. Registry update checks are disabled unless `update_notice: true` or `PI_ACP_UPDATE_NOTICE=true` is set.

**Breaking change:** Pi-specific `_ex_mcp.pi/*` and legacy `pi/*` extension methods are no longer implemented. Use the ACP session methods above or slash commands in prompts.

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

# Plan update notification (emits the stable "plan" update type)
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
  additional_directories: ["/shared/docs"],
  mcp_servers: [
    ExMCP.ACP.Types.stdio_mcp_server("local-tools", "my_mcp_server", args: ["--stdio"]),
    ExMCP.ACP.Types.http_mcp_server("remote-tools", "http://localhost:4000/mcp")
  ]
)
```

## ACP Registry

The public ACP Registry lists ACP-compatible agents and their distribution metadata:

```elixir
{:ok, registry} = ExMCP.ACP.Registry.fetch()

agent = ExMCP.ACP.Registry.get_agent(registry, "codex-acp")
{:ok, command} = ExMCP.ACP.Registry.npx_command(agent)

{:ok, client} = ExMCP.ACP.start_client(command: command)
```

Use `ExMCP.ACP.Registry.find_agents/2` to search the decoded registry by agent id, name, or description.

## API Reference

- `ExMCP.ACP` — Facade module
- `ExMCP.ACP.Agent` — GenServer runtime for native Elixir ACP agents
- `ExMCP.ACP.Agent.Handler` — Agent-side handler behaviour
- `ExMCP.ACP.Client` — GenServer client with full session API
- `ExMCP.ACP.Client.Handler` — Handler behaviour
- `ExMCP.ACP.Protocol` — ACP JSON-RPC message encoding
- `ExMCP.ACP.Types` — Type specs and builders
- `ExMCP.ACP.Registry` — Public ACP Registry fetch and lookup helpers
- `ExMCP.ACP.Adapter` — Adapter behaviour for non-native agents
- `ExMCP.ACP.AdapterBridge` — GenServer bridge managing Port and message queue
- `ExMCP.ACP.Adapters.ClaudeSDK` — Claude Code SDK-protocol adapter
- `ExMCP.ACP.Adapters.Codex` — Codex adapter
- `ExMCP.ACP.Adapters.Pi` — Pi adapter
