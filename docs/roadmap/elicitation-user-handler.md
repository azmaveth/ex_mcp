# Elicitation User Handler

**Priority:** Medium
**Status:** Planned
**Conformance:** Elicitation passing with auto-accept (testing mode). Production needs real user interaction.

## Problem

When an MCP server sends an `elicitation/create` request, it's asking the client to collect user input. Currently:

- **Auto-accept mode** (`elicitation_auto_accept: true`): Accepts with schema defaults. For testing only.
- **Decline mode** (default): Declines all elicitations. No user interaction.
- **Custom handler**: Applications can implement `handle_elicitation_create/3` callback.

There's no built-in way to present elicitations to users in interactive applications (CLI, web, desktop).

## What to Build

### Phase 1: Interactive CLI Handler

An `ElicitationHandler` implementation for terminal-based applications that renders the schema as a form and collects user input via stdin.

```elixir
ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:3000/mcp",
  handler: ExMCP.Client.InteractiveHandler,
  capabilities: %{"elicitation" => %{"form" => %{"applyDefaults" => true}}}
)
```

When the server sends `elicitation/create`:
- Display the message to the user
- Render the requestedSchema as a form (text inputs, number inputs, selects from enums)
- Collect user responses
- Return `{action: "accept", content: user_data}` or `{action: "cancel"}`

### Phase 2: Callback-Based Handler

A handler that fires a callback function, allowing any UI framework to integrate:

```elixir
ExMCP.Client.start_link(
  handler: ExMCP.Client.CallbackHandler,
  handler_opts: [
    on_elicitation: fn message, schema ->
      # Wire to your UI framework (Phoenix LiveView, Scenic, etc.)
      MyApp.UI.prompt_user(message, schema)
    end,
    on_permission: fn tool_call, options ->
      MyApp.UI.approve_tool(tool_call, options)
    end
  ]
)
```

### Phase 3: Phoenix LiveView Integration

For Arbor's dashboard, wire elicitation to the chat interface:
- Agent sends elicitation → signal emitted → LiveView renders form
- User fills form → response sent back via Agent session
- Approval panel already exists (ApprovalGuard) — similar UX

### Schema Rendering

The `requestedSchema` is a JSON Schema object. The handler needs to render:
- `type: "string"` → text input (with optional `default`, `minLength`, `maxLength`)
- `type: "integer"` / `"number"` → number input (with `minimum`, `maximum`)
- `type: "boolean"` → checkbox/yes-no
- `enum: [...]` → select/dropdown
- `type: "array"` → multi-select or repeated input
- Nested `type: "object"` → fieldset/section

The `applyDefaults: true` capability means the SDK should pre-fill defaults from the schema before presenting to the user.

## Current Architecture

```
Server sends elicitation/create via SSE
  → SSEClient forwards to Client GenServer
  → RequestHandler.handle_elicitation_create_request
  → Check: custom handler implements callback?
    YES → delegate to handler.handle_elicitation_create/3
    NO  → check elicitation capability declared?
      YES → ElicitationHandler.handle (decline or auto-accept)
      NO  → return -32601 method not found
```

## Files

- `lib/ex_mcp/client/elicitation_handler.ex` — current default handler (decline/auto-accept)
- `lib/ex_mcp/client/request_handler.ex` — routing logic
- `lib/ex_mcp/client/handler.ex` — behaviour with `handle_elicitation_create/3` callback
- `lib/ex_mcp/testing/schema_generator.ex` — generates defaults from schema (used by auto-accept)
