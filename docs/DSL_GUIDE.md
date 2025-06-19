# ExMCP v2 DSL Guide

## Overview

ExMCP v2 provides a powerful Domain-Specific Language (DSL) for defining MCP tools, resources, and prompts. The DSL is designed around three core principles:

1. **Consistency** - Same syntax patterns across all DSL types
2. **Developer Experience** - Clean, intuitive syntax that feels natural in Elixir
3. **Extensibility** - Easy to extend with new metadata fields and features

## Core Design: The Meta Block Pattern

### The Problem We Solved

Early versions of the ExMCP v2 DSL suffered from naming conflicts and inconsistent syntax:

```elixir
# 🚫 POOR DESIGN: Inconsistent and conflicting naming
deftool "say_hello" do
  name "Hello Tool"           # Clean syntax for tools
  description "Says hello"
end

defresource "config://app" do
  Resource.name "App Config"      # Clunky qualified syntax
  Resource.description "Config"   # Different pattern!
end

defprompt "greeting" do
  Prompt.name "Greeting"          # Yet another pattern!
  Prompt.description "Template"
end
```

**Problems with this approach:**

- **Import conflicts** - Multiple DSLs exporting `name` and `description` functions
- **Inconsistent syntax** - Different qualification patterns across DSL types
- **Poor developer experience** - Developers had to remember different patterns
- **Not extensible** - Adding new metadata fields would create more conflicts

### The Solution: Meta Block Pattern

We implemented a **meta block pattern** that provides consistent syntax across all DSL types:

```elixir
# ✅ EXCELLENT DESIGN: Consistent and clean
deftool "say_hello" do
  meta do
    name "Hello Tool"
    description "Says hello to someone"
    version "1.0.0"              # Extensible!
    author "Development Team"
    tags ["greeting", "demo"]
  end
  
  input_schema %{
    type: "object",
    properties: %{name: %{type: "string"}},
    required: ["name"]
  }
end

defresource "config://app" do
  meta do
    name "App Configuration"     # Same syntax everywhere!
    description "Application configuration data"
    version "2.1.0"
    author "System Team"
  end
  
  mime_type "application/json"
  subscribable true
end

defprompt "greeting" do
  meta do
    name "Greeting Template"     # Perfect consistency!
    description "A customizable greeting template"
    version "1.5.0"
    author "UI Team"
  end
  
  arguments do
    arg :style, description: "Greeting style (formal/casual)"
    arg :name, required: true, description: "Person to greet"
  end
end
```

### Benefits of the Meta Block Pattern

#### 1. **Perfect Consistency**

Every DSL type uses the exact same syntax for metadata. Developers learn one pattern and can apply it everywhere.

#### 2. **Zero Import Conflicts**

The `meta` macro creates its own scope where `name`, `description`, etc. are defined. No more qualified imports or namespace conflicts.

#### 3. **Excellent Developer Experience**

- Intuitive syntax that feels natural in Elixir
- IDE autocompletion works perfectly
- Clear separation between metadata and configuration
- Easy to read and maintain

#### 4. **Future Extensible**

Adding new metadata fields is trivial - just add them to the meta block:

```elixir
meta do
  name "Advanced Tool"
  description "Does advanced operations"
  version "3.0.0"
  author "Core Team"
  tags ["advanced", "analytics"]
  deprecated false          # New field - easy to add!
  stability "stable"        # Another new field!
  category "data-processing"
end
```

#### 5. **Follows Elixir Best Practices**

This pattern is common in the Elixir ecosystem:

- **Ecto schemas** use block syntax for field definitions
- **Phoenix routes** use `scope` and `pipeline` blocks
- **GenServer** uses `use` blocks with options

## DSL Reference

### Tool DSL (`deftool`)

Define MCP tools that clients can invoke:

```elixir
deftool "calculate_sum" do
  meta do
    name "Calculator"
    description "Adds two numbers together"
    version "1.0.0"
    author "Math Team"
    tags ["math", "basic"]
  end
  
  # Option 1: Direct JSON Schema (recommended for complex schemas)
  input_schema %{
    type: "object",
    properties: %{
      a: %{type: "number", description: "First number"},
      b: %{type: "number", description: "Second number"}
    },
    required: ["a", "b"]
  }
end

# Option 2: Elixir-native schema syntax (compiles to JSON Schema)
deftool "user_search" do
  meta do
    name "User Search"
    description "Search for users by various criteria"
  end
  
  args do
    field :query, :string, required: true, description: "Search query"
    field :limit, :integer, description: "Maximum results"
    field :include_inactive, :boolean, default: false
    
    # Nested object fields
    field :filters, :object do
      field :department, :string
      field :role, :string
      field :created_after, :string
    end
  end
end
```

**Implementation:**

```elixir
@impl true
def handle_tool_call("calculate_sum", %{"a" => a, "b" => b}, state) do
  result = a + b
  response = text("Result: #{result}")
  {:ok, [response], state}
end
```

### Resource DSL (`defresource`)

Define MCP resources that clients can read:

```elixir
# Static resource
defresource "config://database" do
  meta do
    name "Database Configuration"
    description "Current database connection settings"
    author "Infrastructure Team"
  end
  
  mime_type "application/json"
  annotations %{
    audience: ["admin", "developer"],
    sensitive: true
  }
end

# Pattern-based resource with subscriptions
defresource "logs://app/*.log" do
  meta do
    name "Application Logs"
    description "Real-time application log files"
    tags ["logging", "monitoring"]
  end
  
  mime_type "text/plain"
  list_pattern true        # Supports listing multiple resources
  subscribable true        # Clients can subscribe to changes
  size "variable"         # Indicate resource size characteristics
end

# Resource template for dynamic URIs
defresource_template "users://{user_id}/profile" do
  meta do
    name "User Profile"
    description "Individual user profile data"
  end
  
  mime_type "application/json"
  annotations %{
    cache_ttl: 300,
    privacy: "user-controlled"
  }
end
```

**Implementation:**

```elixir
@impl true
def handle_resource_read("config://database", _uri, state) do
  config = json(%{
    host: "localhost",
    port: 5432,
    database: "myapp_prod"
  })
  {:ok, [config], state}
end

@impl true
def handle_resource_read("logs://app/" <> filename, _uri, state) do
  case File.read("/var/log/app/#{filename}") do
    {:ok, content} -> {:ok, [text(content)], state}
    {:error, _} -> {:error, "Log file not found", state}
  end
end
```

### Prompt DSL (`defprompt`)

Define MCP prompt templates with arguments:

```elixir
# Simple prompt without arguments
defprompt "daily_standup" do
  meta do
    name "Daily Standup Template"
    description "Template for daily standup meetings"
    author "Agile Team"
  end
end

# Complex prompt with typed arguments
defprompt "code_review" do
  meta do
    name "Code Review Assistant"
    description "Comprehensive code review with configurable focus areas"
    version "2.0.0"
    tags ["development", "quality"]
  end
  
  arguments do
    arg :code, required: true, description: "Code to review"
    arg :language, description: "Programming language (auto-detected if not provided)"
    arg :focus, description: "Review focus areas", default: ["security", "performance"]
    arg :style, description: "Review style", enum: ["friendly", "formal", "detailed"]
    arg :include_examples, description: "Include example fixes", type: :boolean, default: false
  end
end
```

**Implementation:**

```elixir
@impl true
def handle_prompt_get("code_review", args, state) do
  code = args["code"]
  language = args["language"] || detect_language(code)
  focus = args["focus"] || ["security", "performance"]
  style = args["style"] || "friendly"
  
  messages = [
    system("You are a #{style} code reviewer focusing on: #{Enum.join(focus, ", ")}"),
    user("Please review this #{language} code:\n\n#{code}")
  ]
  
  {:ok, %{messages: messages}, state}
end
```

## Advanced Patterns

### Metadata Inheritance

While not currently implemented, the meta block pattern makes it easy to add metadata inheritance:

```elixir
# Future feature concept
defmodule MyServer do
  use ExMCP.ServerV2
  
  # Global metadata for all definitions
  default_meta do
    author "Development Team"
    version "1.0.0"
    tags ["internal"]
  end
  
  deftool "example" do
    meta do
      name "Example Tool"
      description "An example tool"
      # author, version, tags inherited from default_meta
      tags ["internal", "example"]  # Merge with default
    end
    
    input_schema %{...}
  end
end
```

### Conditional Metadata

The meta block supports dynamic metadata generation:

```elixir
@environment Application.compile_env(:my_app, :environment)

deftool "debug_info" do
  meta do
    name "Debug Information"
    description "System debugging information"
    
    # Conditional metadata based on compile-time environment
    if @environment == :dev do
      author "Development Team"
      tags ["debug", "development"]
    else
      author "Operations Team"  
      tags ["debug", "production"]
    end
  end
  
  input_schema %{...}
end
```

### Custom Metadata Fields

Add custom fields specific to your application:

```elixir
deftool "analytics_query" do
  meta do
    name "Analytics Query Engine"
    description "Execute analytics queries on user data"
    
    # Standard fields
    version "2.1.0"
    author "Analytics Team"
    
    # Custom application-specific fields
    data_sensitivity "high"
    requires_approval true
    estimated_cost_usd 0.05
    max_execution_time_seconds 30
    supported_data_sources ["postgresql", "bigquery", "snowflake"]
  end
  
  args do
    field :query, :string, required: true
    field :data_source, :string, required: true, 
          enum: ["postgresql", "bigquery", "snowflake"]
  end
end
```

## Migration Guide

### From Legacy Syntax

If you have existing v2 code using the old syntax, migration is straightforward:

**Before:**

```elixir
deftool "old_tool" do
  description "Old style description"  # Legacy
  
  input_schema %{...}
end

defresource "old://resource" do
  name "Old Resource Name"        # Legacy
  description "Old description"   # Legacy
  mime_type "text/plain"
end
```

**After:**

```elixir
deftool "old_tool" do
  meta do
    name "Old Tool"                    # New: add explicit name
    description "Old style description" # New: move to meta block
  end
  
  input_schema %{...}
end

defresource "old://resource" do
  meta do
    name "Old Resource Name"        # New: move to meta block
    description "Old description"   # New: move to meta block
  end
  
  mime_type "text/plain"
end
```

### Benefits of Migration

1. **Consistency** - All your DSL definitions follow the same pattern
2. **Extensibility** - Easy to add version, author, tags later
3. **Future-proof** - Ready for new metadata features
4. **Better tooling** - IDE support and documentation generation

## Design Rationale

### Why We Chose the Meta Block Pattern

We evaluated several approaches for handling metadata in the DSL:

#### Option 1: Unified Vocabulary (Rejected)

```elixir
# Using different terms to avoid conflicts
deftool "example" do
  title "Example Tool"     # Instead of "name"
  summary "Description"    # Instead of "description"
end
```

**Rejected because:** Sacrifices semantic accuracy for technical convenience. `title` isn't the right word for all contexts.

#### Option 2: Context-Specific Vocabulary (Rejected)

```elixir
# Verbose, context-specific naming
deftool "example" do
  tool_name "Example Tool"
  tool_description "Description"
end

defresource "example" do
  resource_name "Example Resource"
  resource_description "Description"
end
```

**Rejected because:** Highly verbose and redundant. Violates DRY principle and creates poor ergonomics.

#### Option 3: Keyword Syntax (Considered)

```elixir
# Function arguments for metadata
deftool "example", name: "Example", description: "Description" do
  input_schema %{...}
end
```

**Considered but not chosen because:** Less flexible for complex metadata and doesn't scale well with many fields.

#### Option 4: Meta Block Pattern (✅ Chosen)

```elixir
# Block-based organization
deftool "example" do
  meta do
    name "Example Tool"
    description "Description"
    version "1.0.0"
  end
  
  input_schema %{...}
end
```

**Chosen because:**

- Solves naming conflicts elegantly
- Excellent organization and readability
- Highly extensible for future fields
- Follows common Elixir DSL patterns
- Creates clear separation between metadata and configuration

### Inspiration from the Elixir Ecosystem

The meta block pattern follows established conventions in Elixir:

**Ecto Schemas:**

```elixir
schema "users" do
  field :name, :string
  field :email, :string
  timestamps()
end
```

**Phoenix Routes:**

```elixir
scope "/api/v1", MyAppWeb do
  pipe_through :api
  
  resources "/users", UserController
end
```

**LiveView:**

```elixir
def mount(_params, _session, socket) do
  socket = assign(socket,
    name: "World",
    count: 0
  )
  
  {:ok, socket}
end
```

All of these use block syntax to create scoped contexts with their own vocabulary, just like our meta blocks.

## Handler Implementation Patterns

### The Separation of Definition and Implementation

ExMCP v2 intentionally separates DSL definitions from handler implementations. This follows Elixir's behaviour pattern and provides several benefits:

1. **Testability** - Handlers are public functions that can be unit tested directly
2. **Organization** - Definitions grouped at the top, implementations below
3. **Flexibility** - Complex handlers can use full Elixir features
4. **Clarity** - Clear separation of "what" (definitions) from "how" (implementations)

### Standard Handler Pattern

After defining tools, resources, and prompts with the DSL, implement handlers using callbacks:

```elixir
defmodule MyServer do
  use ExMCP.ServerV2
  
  # Definitions at the top
  deftool "say_hello" do
    meta do
      name "Hello Tool"
      description "Greets someone"
    end
    
    input_schema %{
      type: "object", 
      properties: %{name: %{type: "string"}},
      required: ["name"]
    }
  end
  
  # Implementations below
  @impl true
  def handle_tool_call("say_hello", %{"name" => name}, state) do
    response = text("Hello, #{name}!")
    {:ok, [response], state}
  end
end
```

### The `defhandler` Macro

To reduce boilerplate while maintaining the separation principle, ExMCP v2 provides the `defhandler` macro:

```elixir
# Instead of:
@impl true
def handle_tool_call("say_hello", %{"name" => name}, state) do
  response = text("Hello, #{name}!")
  {:ok, [response], state}
end

# You can write:
defhandler :tool, "say_hello", %{"name" => name}, state do
  response = text("Hello, #{name}!")
  {:ok, [response], state}
end
```

#### Benefits of `defhandler`

1. **Reduced Boilerplate** - No need to remember function names or add `@impl true`
2. **Explicit Intent** - The handler type (`:tool`, `:resource`, `:prompt`) is clear
3. **Full Elixir Power** - Supports pattern matching, guards, and multiple clauses
4. **Zero Runtime Cost** - Compile-time macro that generates standard functions

#### Using `defhandler`

The macro supports all Elixir function features:

```elixir
# Simple tool handler
defhandler :tool, "echo", %{"message" => msg}, state do
  {:ok, [text(msg)], state}
end

# Pattern matching with guards
defhandler :tool, "divide", %{"a" => a, "b" => b}, state when b != 0 do
  result = a / b
  {:ok, [text("Result: #{result}")], state}
end

defhandler :tool, "divide", %{"a" => _a, "b" => 0}, state do
  {:error, "Division by zero", state}
end

# Resource handler with pattern matching on URI
defhandler :resource, "file://" <> path, _uri, state do
  case File.read(path) do
    {:ok, content} -> {:ok, [text(content)], state}
    {:error, reason} -> {:error, reason, state}
  end
end

# Prompt handler
defhandler :prompt, "greeting", %{"style" => style}, state do
  template = case style do
    "formal" -> "Good day, {name}. How may I assist you?"
    "casual" -> "Hey {name}! What's up?"
    _ -> "Hello, {name}!"
  end
  {:ok, %{template: template}, state}
end
```

#### Escape Hatches

The `defhandler` macro is optional. You can always use standard function definitions:

```elixir
# Mix both styles in the same module
defhandler :tool, "simple", args, state do
  {:ok, [text("Simple response")], state}
end

# Use standard def for complex handlers
@impl true
def handle_tool_call("complex", args, state) do
  with {:ok, validated} <- validate_args(args),
       {:ok, result} <- complex_operation(validated, state),
       {:ok, formatted} <- format_result(result) do
    {:ok, [formatted], state}
  else
    {:error, reason} -> {:error, reason, state}
  end
end
```

### Handler Design Analysis

#### Why Separate Handlers from DSL?

We carefully evaluated whether to include handlers directly in the DSL (inline handlers) versus keeping them separate. The decision to keep them separate was based on:

**Technical Reasons:**

1. **Testability** - Inline handlers would create untestable closures. Separate handlers are public functions that can be unit tested directly.
2. **Debugging** - Stack traces from `def handle_tool_call` are clear. Traces from macro-generated lambdas are cryptic.
3. **Compilation** - Simple data registration is faster than AST manipulation for inline handlers.
4. **Hot Code Reloading** - Can update handler implementations without recompiling DSL definitions.

**Design Reasons:**

1. **Elixir Idioms** - Behaviours with `@callback` and `@impl true` are the standard pattern.
2. **Separation of Concerns** - DSL defines metadata, handlers implement behavior.
3. **Scalability** - Easy to organize handlers in large modules or split into separate modules.
4. **Flexibility** - Full access to Elixir features without macro limitations.

### Example: Why Separation Works Better

```elixir
# ❌ BAD: Inline handler (not supported)
deftool "process_data" do
  meta do
    name "Data Processor"
    description "Processes data with retries"
  end
  
  input_schema %{...}
  
  # This would be hard to test, debug, and extend
  handler do |args, state|
    # Complex logic buried in macro...
  end
end

# ✅ GOOD: Separate handler (recommended)
deftool "process_data" do
  meta do
    name "Data Processor"
    description "Processes data with retries"
  end
  
  input_schema %{...}
end

# Clear, testable, debuggable
@impl true
def handle_tool_call("process_data", args, state) do
  with {:ok, data} <- validate_data(args),
       {:ok, result} <- process_with_retry(data, 3),
       {:ok, stored} <- store_result(result, state) do
    {:ok, [json(stored)], state}
  else
    {:error, :validation_failed} = err -> 
      {:error, "Invalid data format", state}
    {:error, reason} -> 
      Logger.error("Processing failed: #{inspect(reason)}")
      {:error, "Processing failed", state}
  end
end

# Easy to test
test "process_data handles validation errors" do
  result = MyServer.handle_tool_call("process_data", %{invalid: true}, %{})
  assert {:error, "Invalid data format", _} = result
end
```

## Conclusion

The ExMCP v2 DSL provides a comprehensive toolkit for building MCP servers:

1. **Meta Block Pattern** - Consistent syntax for all DSL types
2. **Separated Handlers** - Clear distinction between definition and implementation
3. **`defhandler` Macro** - Optional convenience for reducing boilerplate
4. **Full Elixir Power** - No limitations on what you can implement

This design ensures that ExMCP v2's DSL feels natural to Elixir developers while providing a solid foundation for future enhancements.
