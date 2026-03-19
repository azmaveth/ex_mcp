# Server Conformance Fixtures

**Priority:** High
**Status:** Planned
**Reference:** `test/conformance/server.exs` (current scaffold), TypeScript SDK `everythingServer.ts`

## Current Status

Server conformance: 7/30 passing. The 24 failures need specific test fixtures that match
what the conformance test suite expects.

## Tools (14 fixtures needed)

| Tool | Description | Complexity |
|------|-------------|-----------|
| `test_simple_text` | Return `"This is a simple text response for testing."` | Simple |
| `test_image_content` | Return base64 PNG image | Simple |
| `test_audio_content` | Return base64 WAV audio | Simple |
| `test_embedded_resource` | Return `{type: "resource", resource: {uri, text}}` | Simple |
| `test_multiple_content_types` | Return text + image + resource | Simple |
| `test_tool_with_logging` | Emit `notifications/message` during execution | Medium |
| `test_tool_with_progress` | Emit `notifications/progress` with progressToken | Medium |
| `test_error_handling` | Throw error → error response | Simple |
| `test_reconnection` | Close SSE stream mid-call (SEP-1699) | Hard |
| `test_sampling` | Send `sampling/createMessage` to client | Medium |
| `test_elicitation` | Send `elicitation/create` to client | Medium |
| `test_elicitation_sep1034_defaults` | Elicitation with default values | Medium |
| `test_elicitation_sep1330_enums` | Elicitation with enum schemas | Medium |
| `json_schema_2020_12_tool` | Tool with JSON Schema 2020-12 features | Simple |

## Resources (4 fixtures needed)

| Resource | URI | Type |
|----------|-----|------|
| Static text | `test://static-text` | `text/plain` |
| Static binary | `test://static-binary` | `image/png` (blob) |
| Template | `test://template/{id}/data` | `application/json` |
| Watched | `test://watched-resource` | Subscribable |

## Prompts (4 fixtures needed)

| Prompt | Arguments |
|--------|-----------|
| `test_simple_prompt` | None |
| `test_prompt_with_arguments` | `arg1`, `arg2` |
| `test_prompt_with_embedded_resource` | `resourceUri` |
| `test_prompt_with_image` | None (returns image content) |

## Other Handlers

- `logging/setLevel` — accept and acknowledge
- `completion/complete` — return empty completion
- `resources/subscribe` / `resources/unsubscribe` — track subscriptions

## Server Architecture Requirements

The conformance tests require a **stateful** server with:
- Session IDs (`Mcp-Session-Id` header)
- SSE responses for POST (Content-Type: text/event-stream)
- GET SSE stream support for server-initiated messages
- DNS rebinding protection (Host header validation)
- Event store for SSE resumability (Last-Event-ID)

This means our `test/conformance/server.exs` needs to use the full
`ExMCP.HttpPlug` with session management, not the stateless approach.

## Test Data

```elixir
# 1x1 red PNG pixel
@test_image_base64 "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg=="

# Minimal WAV file
@test_audio_base64 "UklGRiYAAABXQVZFZm10IBAAAAABAAEAQB8AAAB9AAACABAAZGF0YQIAAAA="
```

## Implementation Order

1. Simple tools (text, image, audio, embedded, mixed, error) — quick wins
2. Resources (static text, static binary, template)
3. Prompts (simple, with args, with resource, with image)
4. Logging/completion handlers
5. Subscribe/unsubscribe
6. Progress notifications
7. Elicitation/sampling (need SSE for server→client requests)
8. Reconnection test (needs SSE stream lifecycle control)
9. DNS rebinding protection
