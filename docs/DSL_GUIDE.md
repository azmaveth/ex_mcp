# ExMCP Server DSL Guide

ExMCP's server DSL defines MCP tools, resources, resource templates, and prompts
next to the functions that handle them. Use it with `ExMCP.Server.Handler`:

```elixir
defmodule MyServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "my-server", version: "1.0.0"

  tool "echo", "Echo back the input" do
    title "Echo"
    param :message, :string, required: true, description: "Message to echo"

    run fn %{message: message}, state ->
      {:ok, "Echo: #{message}", state}
    end
  end
end
```

This generates the standard `ExMCP.Server.Handler` callbacks for listing and
dispatching declared capabilities. The generated `start_link/1` supports
`:beam`, `:test`, `:stdio`, and `:http` transports. Use `transport: :http`
with `sse_enabled: true` when serving HTTP responses with SSE streaming.

## Tools

Tools declare input metadata and a `run` handler:

```elixir
tool "add", "Adds two numbers" do
  title "Add"
  param :a, :number, required: true
  param :b, :number, required: true
  annotations readOnlyHint: true

  output_schema %{
    type: "object",
    properties: %{sum: %{type: "number"}},
    required: ["sum"]
  }

  run fn %{a: a, b: b}, state ->
    sum = a + b
    {:ok, ToolResult.structured("#{sum}", %{sum: sum}), state}
  end
end
```

### Param types

| DSL type | JSON Schema |
|----------|-------------|
| `:string` | `{"type": "string"}` |
| `:integer` | `{"type": "integer"}` |
| `:number` | `{"type": "number"}` |
| `:boolean` | `{"type": "boolean"}` |
| `:object` / `:map` | `{"type": "object"}` |
| `{:array, item_type}` | `{"type": "array", "items": ...}` |

```elixir
param :tags, {:array, :string}, default: []
param :scores, {:array, :number}, required: true
```

Bare `:array` is **not** valid — the item type is required so the generated
`inputSchema` is correct.

You can also pass a full JSON Schema with `input_schema` (DSL instruction,
snake_case). That becomes the MCP `inputSchema` field on the wire.

Declared params are normalized so handlers can use atom keys and defaults.

### Response helpers and normalization

`ToolResult` is an **alias** for `ExMCP.Server.DSL.Result`, injected only inside
modules that `use ExMCP.Server.DSL`. Outside those modules, use the fully
qualified module:

```elixir
ExMCP.Server.DSL.Result.structured("done", %{count: 1})
```

`ToolResult` provides `text/1`, `error/1`, and `structured/2`. The DSL also
normalizes several plain return shapes from `run` / `read` / `render`:

| Return from handler | Normalized result |
|---------------------|-------------------|
| `"hello"` | text content |
| `%{text: "hello"}` | text content |
| `%{content: [...]}` | used as-is (plus structured key cleanup) |
| `ToolResult.structured(text, map)` | text + `structuredContent` |
| `{:error, reason}` | tool/resource/prompt error shape |
| `{:ok, result}` or `{:ok, result, state}` | both accepted |

## Compile-time checks

Invalid DSL declarations fail at **compile time** with file/line and a fix hint:

```elixir
# Missing handler
tool "echo" do
  param :message, :string
end
# => tool "echo" must define `run` or `handle`, e.g. run fn args, state -> ...

# Bare :array
param :data, :array
# => Invalid param type :array ... Use {:array, item_type}, e.g. {:array, :string}

# Wrong instruction for the declaration kind
tool "echo" do
  arg :message   # arg is only valid on prompts
  run fn _, s -> {:ok, "ok", s} end
end

# Duplicates
tool "echo" do ... end
tool "echo" do ... end
# => Duplicate tool "echo" declared 2 times
```

Other checks include unknown instructions (with suggestions for common
mistakes like `inputSchema` → `input_schema`), non-literal types, empty
names/URIs, and using `run`/`read`/`render`/`mime_type` in the wrong block.

## Resources

Static resources use `resource` and a `read` handler:

```elixir
resource "config://app", "Application configuration" do
  title "App Config"
  mime_type "application/json"

  read fn %{uri: uri}, state ->
    {:ok, %{uri: uri, text: Jason.encode!(%{enabled: true})}, state}
  end
end
```

Resource templates use URI variables and optional typed params:

```elixir
resource_template "file:///{path}", "File contents" do
  title "File"
  mime_type "text/plain"
  param :path, :string

  read fn %{path: path}, state ->
    {:ok, "contents for #{path}", state}
  end
end
```

Template variables are available as atom and string keys.

## Prompts

Prompts declare arguments and a `render` handler:

```elixir
prompt "code_review", "Review code" do
  title "Code Review"
  arg :code, required: true, description: "Code to review"

  render fn %{code: code}, state ->
    {:ok,
     %{
       messages: [
         %{role: "user", content: %{type: "text", text: "Review this code:\n#{code}"}}
       ]
     }, state}
  end
end
```

Returning a string creates a single user text message.

## Metadata

The DSL supports spec-aligned metadata on declarations:

```elixir
tool "search", "Search documents" do
  title "Search"
  icons [%{src: "https://example.com/search.svg", mimeType: "image/svg+xml"}]
  annotations readOnlyHint: true
  meta %{"owner" => "docs"}

  param :query, :string, required: true
  run fn %{query: query}, state -> {:ok, "Searching #{query}", state} end
end
```

Use `title` for display names. Custom extension data belongs under `_meta` via
`meta`.

## Starting Servers

For the generated DSL server:

```elixir
{:ok, pid} = MyServer.start_link(transport: :test)
{:ok, pid} = MyServer.start_link(transport: :stdio)
{:ok, pid} = MyServer.start_link(transport: :http, port: 4000)
```

For a hand-written handler without the DSL:

```elixir
{:ok, pid} =
  ExMCP.Server.HandlerServer.start_link(
    transport: :test,
    handler: MyHandler
  )
```

`ExMCP.start_server/1` is also available as a top-level convenience wrapper for
`ExMCP.Server.HandlerServer.start_link/1`.

**Fast verification tip:** After `mix compile`, `mix examples.getting_started` runs a quick in-process demo of the DSL + client patterns shown throughout this guide (and in QUICKSTART.md).

## Migration From The Removed Legacy DSL

The former `use ExMCP.Server` macro and `deftool`, `defresource`, and
`defprompt` declarations have been removed. Migrate by:

1. Replacing `use ExMCP.Server` with `use ExMCP.Server.Handler` and
   `use ExMCP.Server.DSL`.
2. Replacing `deftool` blocks with `tool` blocks and colocated `run` handlers.
3. Replacing `defresource` blocks with `resource` or `resource_template` blocks
   and colocated `read` handlers.
4. Replacing `defprompt` blocks with `prompt` blocks and colocated `render`
   handlers.
5. Replacing the removed `ExMCP.Server.start_link` helper with `MyServer.start_link/1`,
   `ExMCP.Server.HandlerServer.start_link/1`, or `ExMCP.start_server/1`.

Old generated getters such as `get_tools/0`, `get_resources/0`, and
`get_prompts/0` are no longer part of the server API. Use the standard handler
callbacks instead.
