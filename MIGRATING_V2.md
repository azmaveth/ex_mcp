# Migrating to ExMCP v2

This guide helps you migrate from ExMCP v1 to v2. Version 2 introduces several design improvements, enhanced APIs, and better compliance with the MCP specification.

## Overview of Changes

### Breaking Changes

1. **Module Reorganization**
   - All v2 modules are under the `ExMCP` namespace (not `ExMCP.V2`)
   - Old `ExMCP.Plug` renamed to `ExMCP.MessageProcessor`
   - New `ExMCP.HttpPlug` for HTTP transport

2. **DSL Syntax Changes**
   - Tool DSL: `tool_description` → `description`
   - Resource DSL: `resource_name` → `name`, `resource_description` → `description`
   - Prompt DSL: `prompt_name` → `name`, `prompt_description` → `description`

3. **Client API Changes**
   - Clients now return structured responses (`{:ok, %ExMCP.Response{}}` or `{:error, %ExMCP.Error{}}`)
   - Connection options have been standardized

### New Features

1. **HTTP Transport with SSE**
   - Full HTTP/SSE support via `ExMCP.HttpPlug`
   - Backpressure control for Server-Sent Events
   - Last-Event-ID support for connection resumption

2. **Structured Response Types**
   - `ExMCP.Response` for successful operations
   - `ExMCP.Error` for detailed error information

3. **Enhanced Configuration**
   - `ExMCP.ClientConfig` for centralized configuration
   - Transport-agnostic server startup

## Migration Steps

### Step 1: Update Dependencies

```elixir
# mix.exs
def deps do
  [
    {:ex_mcp, "~> 0.6.0"}  # or latest v2 version
  ]
end
```

### Step 2: Update DSL Usage

#### Tools

```elixir
# Old (v1)
deftool "calculate" do
  tool_description "Performs calculations"
  args do
    field :expression, :string, required: true
  end
end

# New (v2)
deftool "calculate" do
  description "Performs calculations"  # Changed from tool_description
  args do
    field :expression, :string, required: true
  end
end
```

#### Resources

```elixir
# Old (v1)
defresource "file://config.json" do
  resource_name "Configuration"
  resource_description "Application config"
end

# New (v2)
defresource "file://config.json" do
  name "Configuration"           # Changed from resource_name
  description "Application config"  # Changed from resource_description
end
```

#### Prompts

```elixir
# Old (v1)
defprompt "code_review" do
  prompt_name "Code Review"
  prompt_description "Reviews code quality"
end

# New (v2)
defprompt "code_review" do
  name "Code Review"           # Changed from prompt_name
  description "Reviews code quality"  # Changed from prompt_description
end
```

### Step 3: Update Client Usage

#### Connection and Requests

```elixir
# Old (v1)
{:ok, client} = ExMCP.V2.Client.connect(transport: :stdio)
{:ok, result} = ExMCP.V2.Client.call_tool(client, "calculate", %{expression: "2+2"})

# New (v2) - Option 1: Using convenience module
{:ok, client} = ExMCP.connect(:stdio)
{:ok, response} = ExMCP.call_tool(client, "calculate", %{expression: "2+2"})
result = ExMCP.Response.text_content(response)

# New (v2) - Option 2: Using client directly
{:ok, client} = ExMCP.Client.connect(transport: :stdio)
{:ok, response} = ExMCP.Client.call_tool(client, "calculate", %{expression: "2+2"})
```

#### Error Handling

```elixir
# Old (v1)
case ExMCP.V2.Client.call_tool(client, "tool", params) do
  {:ok, result} -> handle_result(result)
  {:error, reason} -> handle_error(reason)
end

# New (v2)
case ExMCP.call_tool(client, "tool", params) do
  {:ok, response} ->
    if ExMCP.Response.error?(response) do
      handle_error(response)
    else
      handle_result(ExMCP.Response.text_content(response))
    end
  {:error, %ExMCP.Error{} = error} ->
    Logger.error("Tool error: #{error.message}")
end
```

### Step 4: Update Server Implementation

#### HTTP Transport

```elixir
# Old (v1) - Manual HTTP setup
# Complex manual setup required...

# New (v2) - Built-in HTTP support
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Plug.Cowboy, scheme: :http, plug: ExMCP.HttpPlug, options: [
        port: 4000,
        handler: MyApp.MCPHandler,
        server_info: %{name: "my-app", version: "1.0.0"}
      ]}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

#### Server Handler

```elixir
# Old (v1)
defmodule MyHandler do
  @behaviour ExMCP.V2.Server.Handler
  
  def handle_initialize(params, state) do
    # ...
  end
end

# New (v2) - Same behavior, cleaner namespace
defmodule MyHandler do
  @behaviour ExMCP.Server.Handler
  
  def handle_initialize(params, state) do
    # ...
  end
end
```

### Step 5: Configuration

```elixir
# New (v2) - Centralized configuration
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.with_transport(:stdio)
         |> ExMCP.ClientConfig.with_timeout(30_000)
         |> ExMCP.ClientConfig.with_capabilities(tools: true, resources: true)

{:ok, client} = ExMCP.Client.connect(config)
```

## Backward Compatibility

Version 2 maintains backward compatibility through deprecation warnings:

1. **Deprecated macros still work** but will show warnings with file:line info
2. **Migration can be gradual** - update one module at a time
3. **Both syntaxes supported** during transition period

To see all deprecation warnings in your project:

```bash
mix compile --force 2>&1 | grep deprecated
```

## Common Issues and Solutions

### Issue 1: Module Not Found

If you get errors about modules not being found:

```elixir
# Old
alias ExMCP.V2.Client

# New
alias ExMCP.Client
```

### Issue 2: Response Format Changes

```elixir
# Old (v1) - Direct result
{:ok, "calculated: 4"} = ExMCP.V2.Client.call_tool(...)

# New (v2) - Structured response
{:ok, response} = ExMCP.Client.call_tool(...)
"calculated: 4" = ExMCP.Response.text_content(response)
```

### Issue 3: SSE Connection Errors

If using Server-Sent Events and experiencing connection drops:

1. Ensure your handler implements backpressure control
2. Use `ExMCP.HttpPlug.SSEHandler` for proper flow control
3. Implement Last-Event-ID header support for resumption

## Testing Your Migration

Run your test suite with verbose output to catch any issues:

```bash
# Run tests and check for deprecation warnings
mix test --trace

# Run specific v2 tests
mix test test/ex_mcp_v2/

# Check test coverage
mix coveralls.html
```

## Getting Help

1. Check the [examples directory](examples/) for v2 usage patterns
2. Review the [API documentation](https://hexdocs.pm/ex_mcp)
3. File issues on [GitHub](https://github.com/your-repo/ex_mcp/issues)

## Deprecation Timeline

- **v0.6.0**: Deprecation warnings introduced
- **v0.8.0**: Final warning before removal
- **v1.0.0**: Deprecated APIs removed

Plan your migration accordingly to avoid breaking changes in v1.0.0.