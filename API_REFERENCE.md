# ExMCP API Reference

## Module Index

- [`ExMCP`](#exmcp) - Main module and version information
- [`ExMCP.Client`](#exmcpclient) - MCP client implementation
- [`ExMCP.Client.Handler`](#exmcpclienthandler) - Client handler behaviour
- [`ExMCP.Client.DefaultHandler`](#exmcpclientdefaulthandler) - Default client handler
- [`ExMCP.Server`](#exmcpserver) - MCP server implementation
- [`ExMCP.Server.Handler`](#exmcpserverhandler) - Server handler behaviour
- [`ExMCP.Protocol`](#exmcpprotocol) - Protocol encoding/decoding
- [`ExMCP.Transport`](#exmcptransport) - Transport behaviour
- [`ExMCP.Approval`](#exmcpapproval) - Human-in-the-loop approval behaviour
- [`ExMCP.Approval.Console`](#exmcpapprovalconsole) - Console-based approval handler
- [`ExMCP.Authorization.TokenManager`](#exmcpauthorizationtokenmanager) - OAuth token management
- [`ExMCP.Authorization.ErrorHandler`](#exmcpauthorizationerrorhandler) - Authorization error handling
- [`ExMCP.Authorization.Interceptor`](#exmcpauthorizationinterceptor) - Request authorization middleware
- [`ExMCP.SecureServer`](#exmcpsecureserver) - Security-enhanced MCP server
- [`ExMCP.Security.TokenValidator`](#exmcpsecuritytokenvalidator) - Token validation and audience checking
- [`ExMCP.Security.ConsentManager`](#exmcpsecurityconsentmanager) - Dynamic client consent management
- [`ExMCP.Security.ClientRegistry`](#exmcpsecurityclientregistry) - Client accountability and audit trails
- [`ExMCP.ServerManager`](#exmcpservermanager) - Multiple server management
- [`ExMCP.Discovery`](#exmcpdiscovery) - Server discovery utilities
- [`ExMCP.Types`](#exmcptypes) - Type definitions

---

## ExMCP

Main module providing version information and library overview.

### Functions

#### `protocol_version/0`
Returns the MCP protocol version implemented by this library.

```elixir
@spec protocol_version() :: String.t()

# Example
ExMCP.protocol_version()
# => "2025-03-26"
```

#### `version/0`
Returns the ExMCP library version.

```elixir
@spec version() :: String.t()

# Example
ExMCP.version()
# => "0.2.0"
```

---

## ExMCP.Client

MCP client for connecting to servers.

### Functions

#### `start_link/1`
Starts a new MCP client process.

```elixir
@spec start_link(keyword()) :: GenServer.on_start()

# Options:
# - transport: atom() - Transport type (:stdio, :http, :beam)
# - handler: module() | {module(), args} - Optional client handler
# - handler_state: map() - Initial handler state (deprecated, use tuple form)
# - auth_config: map() - Authorization configuration (optional)
# - Transport-specific options (see below)
# - name: term() - Optional GenServer name

# stdio transport
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["python", "server.py"],
  args: ["--verbose"],
  env: [{"DEBUG", "1"}],
  cd: "/path/to/dir"
)

# Streamable HTTP transport (with SSE)
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8080/mcp",
  headers: [{"Authorization", "Bearer token"}]
)

# BEAM transport
{:ok, client} = ExMCP.Client.start_link(
  transport: :beam,
  server: :my_server  # or {:global, :name} or {node, :name}
)

# With client handler for bi-directional communication
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: MyClientHandler
)

# With handler and initialization args
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: {ExMCP.Client.DefaultHandler, [
    approval_handler: ExMCP.Approval.Console,
    roots: [%{uri: "file:///home", name: "Home"}]
  ]}
)

# With OAuth authorization
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  auth_config: %{
    client_id: "my-client",
    client_secret: "secret",
    token_endpoint: "https://auth.example.com/token",
    initial_token: %{
      "access_token" => "current-token",
      "refresh_token" => "refresh-token",
      "expires_in" => 3600
    }
  }
)
```

#### `list_tools/2`
Lists available tools from the server.

```elixir
@spec list_tools(GenServer.server(), timeout()) :: 
  {:ok, [tool()]} | {:error, term()}

# Example
{:ok, tools} = ExMCP.Client.list_tools(client)
# => {:ok, [%{name: "search", description: "Search the web", ...}]}
```

#### `call_tool/4`
Calls a tool on the server.

```elixir
@spec call_tool(GenServer.server(), String.t(), map(), timeout()) :: 
  {:ok, [content()]} | {:error, term()}

# Example
{:ok, result} = ExMCP.Client.call_tool(
  client, 
  "search", 
  %{query: "Elixir"},
  5000
)
# => {:ok, [%{type: "text", text: "Results..."}]}

# With progress token
{:ok, result} = ExMCP.Client.call_tool(
  client,
  "process",
  %{data: "...", "_progressToken" => "task-123"}
)
```

#### `list_resources/2`
Lists available resources from the server.

```elixir
@spec list_resources(GenServer.server(), timeout()) :: 
  {:ok, [resource()]} | {:error, term()}

# Example
{:ok, resources} = ExMCP.Client.list_resources(client)
# => {:ok, [%{uri: "file:///data.json", name: "Data", ...}]}
```

#### `read_resource/3`
Reads a specific resource.

```elixir
@spec read_resource(GenServer.server(), String.t(), timeout()) :: 
  {:ok, resource_content()} | {:error, term()}

# Example
{:ok, content} = ExMCP.Client.read_resource(client, "file:///data.json")
# => {:ok, %{uri: "file:///data.json", text: "{...}", mimeType: "application/json"}}
```

#### `list_prompts/2`
Lists available prompts from the server.

```elixir
@spec list_prompts(GenServer.server(), timeout()) :: 
  {:ok, [prompt()]} | {:error, term()}

# Example
{:ok, prompts} = ExMCP.Client.list_prompts(client)
# => {:ok, [%{name: "code_review", description: "Review code", ...}]}
```

#### `get_prompt/4`
Gets a specific prompt with arguments.

```elixir
@spec get_prompt(GenServer.server(), String.t(), map(), timeout()) :: 
  {:ok, prompt_messages()} | {:error, term()}

# Example
{:ok, messages} = ExMCP.Client.get_prompt(
  client,
  "code_review",
  %{language: "elixir"}
)
# => {:ok, [%{role: "user", content: %{type: "text", text: "..."}}]}
```

#### `list_roots/2`
Lists available roots (URI boundaries).

```elixir
@spec list_roots(GenServer.server(), timeout()) :: 
  {:ok, [root()]} | {:error, term()}

# Example
{:ok, roots} = ExMCP.Client.list_roots(client)
# => {:ok, [%{uri: "file:///home", name: "Home"}]}
```

#### `subscribe_resource/3`
Subscribes to resource change notifications.

```elixir
@spec subscribe_resource(GenServer.server(), String.t(), timeout()) :: 
  {:ok, map()} | {:error, term()}

# Example
{:ok, _} = ExMCP.Client.subscribe_resource(client, "file:///config.json")
```

#### `unsubscribe_resource/3`
Unsubscribes from resource change notifications.

```elixir
@spec unsubscribe_resource(GenServer.server(), String.t(), timeout()) :: 
  {:ok, map()} | {:error, term()}

# Example
{:ok, _} = ExMCP.Client.unsubscribe_resource(client, "file:///config.json")
```

#### `create_message/3`
Creates a message using the server's LLM capabilities.

```elixir
@spec create_message(GenServer.server(), map(), timeout()) :: 
  {:ok, message()} | {:error, term()}

# Example
{:ok, response} = ExMCP.Client.create_message(client, %{
  messages: [
    %{role: "user", content: %{type: "text", text: "Hello"}}
  ],
  max_tokens: 500,
  temperature: 0.7
})
# => {:ok, %{role: "assistant", content: %{type: "text", text: "Hi!"}}}
```

#### `batch_request/3`
Sends multiple requests as a batch for better performance.

```elixir
@spec batch_request(GenServer.server(), list({atom(), list()}), timeout() | nil) ::
  {:ok, list(any())} | {:error, any()}

# Example
requests = [
  {:list_tools, []},
  {:list_resources, []},
  {:read_resource, ["file:///config.json"]},
  {:call_tool, ["search", %{query: "elixir"}]}
]

{:ok, [tools, resources, config, search_results]} = 
  ExMCP.Client.batch_request(client, requests)

# Supported request types:
# - {:list_tools, []}
# - {:call_tool, [name, arguments]} or {:call_tool, [name, arguments, progress_token]}
# - {:list_resources, []}
# - {:read_resource, [uri]}
# - {:list_prompts, []}
# - {:get_prompt, [name, arguments]}
# - {:create_message, [messages, options]}
# - {:list_roots, []}
# - {:list_resource_templates, []}
# - {:ping, []}
# - {:complete, [ref, argument]}
```

#### `send_cancelled/3`
Sends a cancellation notification for an in-flight request.

```elixir
@spec send_cancelled(GenServer.server(), request_id(), String.t() | nil) :: :ok

# Example - Cancel with reason
:ok = ExMCP.Client.send_cancelled(client, "req_123", "User cancelled")

# Example - Cancel without reason
:ok = ExMCP.Client.send_cancelled(client, "req_123")

# Note: Per MCP spec, initialize requests cannot be cancelled
```

#### `get_pending_requests/1`
Returns a list of currently pending request IDs.

```elixir
@spec get_pending_requests(GenServer.server()) :: [request_id()]

# Example
pending_ids = ExMCP.Client.get_pending_requests(client)
# => ["req_123", "req_124"]

# Cancel all pending requests during shutdown
for id <- pending_ids do
  ExMCP.Client.send_cancelled(client, id, "Shutdown")
end
```

#### `stop/1`
Stops the client gracefully.

```elixir
@spec stop(GenServer.server()) :: :ok

# Example
:ok = ExMCP.Client.stop(client)
```

### Cancellation Support

ExMCP implements the MCP cancellation protocol, allowing either party to cancel in-flight requests:

**Client-side cancellation:**
- Use `send_cancelled/3` to cancel a pending request
- The client will receive `{:error, :cancelled}` for cancelled requests
- Track pending requests with `get_pending_requests/1`

**Server-side cancellation:**
- Servers automatically handle incoming cancellation notifications
- Cancelled requests are removed from processing
- No response is sent for cancelled requests (as per MCP spec)

**Behavior notes:**
- The `initialize` request cannot be cancelled (per MCP spec)
- Unknown or completed requests are ignored when cancelled
- Cancellation notifications may arrive after processing completes due to network latency
- Both parties handle race conditions gracefully

---

## ExMCP.Server

MCP server implementation.

### Functions

#### `start_link/1`
Starts a new MCP server process.

```elixir
@spec start_link(keyword()) :: GenServer.on_start()

# Options:
# - handler: module() - Module implementing Server.Handler behaviour
# - transport: atom() - Transport type (:stdio, :sse, :beam)
# - handler_args: term() - Arguments passed to handler init/1
# - Transport-specific options
# - name: term() - Optional GenServer name

# Example
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :stdio,
  handler_args: [db_url: "postgres://..."]
)
```

#### `notify_resources_changed/1`
Notifies clients that the resources list has changed.

```elixir
@spec notify_resources_changed(GenServer.server()) :: :ok

# Example
ExMCP.Server.notify_resources_changed(server)
```

#### `notify_resource_updated/2`
Notifies clients that a specific resource was updated.

```elixir
@spec notify_resource_updated(GenServer.server(), String.t()) :: :ok

# Example
ExMCP.Server.notify_resource_updated(server, "file:///data.json")
```

#### `notify_tools_changed/1`
Notifies clients that the tools list has changed.

```elixir
@spec notify_tools_changed(GenServer.server()) :: :ok

# Example
ExMCP.Server.notify_tools_changed(server)
```

#### `notify_prompts_changed/1`
Notifies clients that the prompts list has changed.

```elixir
@spec notify_prompts_changed(GenServer.server()) :: :ok

# Example
ExMCP.Server.notify_prompts_changed(server)
```

#### `notify_roots_changed/1`
Notifies clients that the roots list has changed.

```elixir
@spec notify_roots_changed(GenServer.server()) :: :ok

# Example
ExMCP.Server.notify_roots_changed(server)
```

#### `notify_progress/4` and `notify_progress/5`
Sends progress notifications for long-running operations.

```elixir
@spec notify_progress(GenServer.server(), String.t(), number(), number() | nil, String.t() | nil) :: :ok

# Example with percentage
ExMCP.Server.notify_progress(server, "task-123", 50, 100)

# Example with message
ExMCP.Server.notify_progress(server, "task-123", 50, 100, "Processing batch 1 of 2")

# Example without total but with message
ExMCP.Server.notify_progress(server, "task-123", 1024, nil, "Processed 1024 records")
```

#### `ping/2`
Pings the client to check connectivity.

```elixir
@spec ping(GenServer.server(), timeout()) :: {:ok, map()} | {:error, any()}

# Example
{:ok, _} = ExMCP.Server.ping(server)
```

#### `list_roots/2`
Requests the list of roots from the client.

```elixir
@spec list_roots(GenServer.server(), timeout()) :: {:ok, [map()]} | {:error, any()}

# Example
{:ok, roots} = ExMCP.Server.list_roots(server)
# => {:ok, [%{uri: "file:///home", name: "Home"}]}
```

#### `create_message/3`
Requests the client to sample an LLM.

```elixir
@spec create_message(GenServer.server(), map(), timeout()) :: {:ok, map()} | {:error, any()}

# Example
{:ok, response} = ExMCP.Server.create_message(server, %{
  "messages" => [
    %{"role" => "user", "content" => "What is MCP?"}
  ],
  "modelPreferences" => %{
    "hints" => ["gpt-4", "claude-3"],
    "temperature" => 0.7
  }
})
# => {:ok, %{"role" => "assistant", "content" => %{"type" => "text", "text" => "..."}}}
```

---

## ExMCP.Server.Handler

Behaviour for implementing MCP server handlers.

### Callbacks

#### `init/1`
Initializes the handler state.

```elixir
@callback init(args :: term()) :: {:ok, state()} | {:error, reason :: term()}

# Example
@impl true
def init(args) do
  db_url = Keyword.get(args, :db_url)
  {:ok, conn} = Database.connect(db_url)
  {:ok, %{db: conn, cache: %{}}}
end
```

#### `handle_initialize/2`
Handles the initialize request from a client.

```elixir
@callback handle_initialize(params :: map(), state()) :: 
  {:ok, server_info :: map(), state()} | {:error, reason :: term(), state()}

# Example
@impl true
def handle_initialize(_params, state) do
  {:ok, %{
    name: "my-server",
    version: "1.0.0",
    capabilities: %{
      tools: %{},
      resources: %{subscribe: true},
      roots: %{},
      sampling: %{}
    }
  }, state}
end
```

#### `handle_list_tools/1`
Returns the list of available tools.

```elixir
@callback handle_list_tools(state()) :: 
  {:ok, [tool()], state()} | {:error, reason :: term(), state()}

# Example
@impl true
def handle_list_tools(state) do
  tools = [
    %{
      name: "query",
      description: "Query the database",
      input_schema: %{
        type: "object",
        properties: %{
          sql: %{type: "string", description: "SQL query"}
        },
        required: ["sql"]
      },
      readOnlyHint: true,
      destructiveHint: false,
      costHint: :medium
    }
  ]
  {:ok, tools, state}
end
```

#### `handle_call_tool/3`
Executes a tool with the given arguments.

```elixir
@callback handle_call_tool(name :: String.t(), arguments :: map(), state()) :: 
  {:ok, [content()], state()} | {:error, reason :: term(), state()}

# Example
@impl true
def handle_call_tool("query", %{"sql" => sql}, state) do
  case Database.query(state.db, sql) do
    {:ok, results} ->
      content = [%{
        type: "text",
        text: format_results(results)
      }]
      {:ok, content, state}
    
    {:error, reason} ->
      {:error, "Query failed: #{reason}", state}
  end
end
```

#### `handle_list_resources/1`
Returns the list of available resources.

```elixir
@callback handle_list_resources(state()) :: 
  {:ok, [resource()], state()} | {:error, reason :: term(), state()}
```

#### `handle_read_resource/2`
Reads a specific resource by URI.

```elixir
@callback handle_read_resource(uri :: String.t(), state()) :: 
  {:ok, resource_content(), state()} | {:error, reason :: term(), state()}
```

#### `handle_list_prompts/1`
Returns the list of available prompts.

```elixir
@callback handle_list_prompts(state()) :: 
  {:ok, [prompt()], state()} | {:error, reason :: term(), state()}
```

#### `handle_get_prompt/3`
Gets a specific prompt with arguments.

```elixir
@callback handle_get_prompt(name :: String.t(), arguments :: map(), state()) :: 
  {:ok, prompt_messages(), state()} | {:error, reason :: term(), state()}
```

#### `handle_list_roots/1`
Returns the list of available roots.

```elixir
@callback handle_list_roots(state()) :: 
  {:ok, [root()], state()} | {:error, reason :: term(), state()}
```

#### `handle_subscribe_resource/2`
Handles resource subscription requests.

```elixir
@callback handle_subscribe_resource(uri :: String.t(), state()) :: 
  {:ok, map(), state()} | {:error, reason :: term(), state()}
```

#### `handle_unsubscribe_resource/2`
Handles resource unsubscription requests.

```elixir
@callback handle_unsubscribe_resource(uri :: String.t(), state()) :: 
  {:ok, map(), state()} | {:error, reason :: term(), state()}
```

#### `handle_create_message/2`
Handles LLM message creation requests.

```elixir
@callback handle_create_message(params :: map(), state()) :: 
  {:ok, message(), state()} | {:error, reason :: term(), state()}
```

#### `terminate/2`
Called when the server is shutting down.

```elixir
@callback terminate(reason :: term(), state()) :: term()

# Example
@impl true
def terminate(_reason, state) do
  Database.disconnect(state.db)
end
```

#### `handle_cast/2` (optional)
Handles custom async messages.

```elixir
@callback handle_cast(msg :: term(), state()) :: {:noreply, state()}
```

#### `handle_info/2` (optional)
Handles other messages.

```elixir
@callback handle_info(msg :: term(), state()) :: {:noreply, state()}
```

---

## ExMCP.Protocol

Protocol encoding and decoding functions.

### Functions

#### `encode_initialize/1`
Encodes an initialize request.

```elixir
@spec encode_initialize(client_info :: map()) :: map()
```

#### `encode_list_tools/0`
Encodes a tools/list request.

```elixir
@spec encode_list_tools() :: map()
```

#### `encode_call_tool/2`
Encodes a tools/call request.

```elixir
@spec encode_call_tool(name :: String.t(), arguments :: map()) :: map()
```

#### `encode_list_roots/0`
Encodes a roots/list request.

```elixir
@spec encode_list_roots() :: map()
```

#### `encode_subscribe_resource/1`
Encodes a resources/subscribe request.

```elixir
@spec encode_subscribe_resource(uri :: String.t()) :: map()
```

#### `encode_response/2`
Encodes a successful response.

```elixir
@spec encode_response(result :: any(), id :: any()) :: map()
```

#### `encode_error/4`
Encodes an error response.

```elixir
@spec encode_error(code :: integer(), message :: String.t(), data :: any(), id :: any()) :: map()
```

#### `encode_notification/2`
Encodes a notification message.

```elixir
@spec encode_notification(method :: String.t(), params :: map()) :: map()
```

#### `parse_message/1`
Parses an incoming JSON-RPC message.

```elixir
@spec parse_message(String.t() | map()) :: 
  {:request, method :: String.t(), params :: map(), id :: any()} |
  {:notification, method :: String.t(), params :: map()} |
  {:result, result :: any(), id :: any()} |
  {:error, error :: map(), id :: any()} |
  {:error, :invalid_message}
```

---

## ExMCP.Transport

Behaviour for implementing custom transports.

### Callbacks

#### `connect/1`
Establishes a connection.

```elixir
@callback connect(opts :: keyword()) :: {:ok, state :: term()} | {:error, reason :: term()}
```

#### `accept/1`
Accepts an incoming connection (server-side).

```elixir
@callback accept(opts :: keyword()) :: {:ok, state :: term()} | {:error, reason :: term()}
```

#### `send_message/2`
Sends a message through the transport.

```elixir
@callback send_message(message :: String.t(), state :: term()) :: 
  {:ok, new_state :: term()} | {:error, reason :: term()}
```

#### `receive_message/1`
Receives a message from the transport (blocking).

```elixir
@callback receive_message(state :: term()) :: 
  {:ok, message :: String.t() | nil, new_state :: term()} | 
  {:error, reason :: term()}
```

#### `close/1`
Closes the transport connection.

```elixir
@callback close(state :: term()) :: :ok
```

---

## ExMCP.ServerManager

Manages multiple MCP server connections.

### Functions

#### `start_link/1`
Starts the server manager.

```elixir
@spec start_link(keyword()) :: GenServer.on_start()

# Example
{:ok, manager} = ExMCP.ServerManager.start_link(name: :mcp_manager)
```

#### `add_server/2`
Adds a new server connection.

```elixir
@spec add_server(GenServer.server(), map()) :: {:ok, pid()} | {:error, term()}

# Example
{:ok, client} = ExMCP.ServerManager.add_server(manager, %{
  name: "filesystem",
  transport: :stdio,
  command: ["mcp-server-filesystem", "/home"]
})
```

#### `remove_server/2`
Removes a server connection.

```elixir
@spec remove_server(GenServer.server(), String.t()) :: :ok | {:error, :not_found}

# Example
:ok = ExMCP.ServerManager.remove_server(manager, "filesystem")
```

#### `list_servers/1`
Lists all managed servers.

```elixir
@spec list_servers(GenServer.server()) :: [server_info()]

# Example
servers = ExMCP.ServerManager.list_servers(manager)
# => [%{name: "filesystem", pid: #PID<...>, status: :connected}]
```

#### `get_server/2`
Gets a specific server client by name.

```elixir
@spec get_server(GenServer.server(), String.t()) :: {:ok, pid()} | {:error, :not_found}

# Example
{:ok, client} = ExMCP.ServerManager.get_server(manager, "filesystem")
```

---

## ExMCP.Discovery

Server discovery utilities.

### Functions

#### `discover_servers/1`
Discovers available MCP servers using various methods.

```elixir
@spec discover_servers(keyword()) :: [server_config()]

# Options:
# - methods: [:env, :config, :well_known, :npm]

# Example
servers = ExMCP.Discovery.discover_servers()
# => [%{name: "filesystem", transport: "stdio", command: [...]}]
```

#### `discover_from_env/1`
Discovers servers from environment variables.

```elixir
@spec discover_from_env(list()) :: [server_config()]
```

#### `discover_npm_packages/0`
Discovers globally installed npm packages with MCP servers.

```elixir
@spec discover_npm_packages() :: [server_config()]
```

#### `discover_from_well_known/1`
Discovers servers from well-known directories.

```elixir
@spec discover_from_well_known(list()) :: [server_config()]
```

#### `test_server/1`
Tests if a discovered server is reachable.

```elixir
@spec test_server(server_config()) :: boolean()

# Example
ExMCP.Discovery.test_server(%{
  transport: "stdio",
  command: ["mcp-server"]
})
# => true
```

---

## ExMCP.Types

Type definitions used throughout the library.

### Types

```elixir
@type tool :: %{
  required(:name) => String.t(),
  required(:description) => String.t(),
  optional(:input_schema) => json_schema(),
  optional(:readOnlyHint) => boolean(),
  optional(:destructiveHint) => boolean(),
  optional(:costHint) => :low | :medium | :high,
  optional(:annotations) => map()
}

@type resource :: %{
  required(:uri) => String.t(),
  required(:name) => String.t(),
  optional(:description) => String.t(),
  optional(:mimeType) => String.t()
}

@type prompt :: %{
  required(:name) => String.t(),
  optional(:description) => String.t(),
  optional(:arguments) => [prompt_argument()]
}

@type root :: %{
  required(:uri) => String.t(),
  optional(:name) => String.t()
}

@type content :: text_content() | image_content() | resource_content()

@type text_content :: %{
  required(:type) => String.t(),
  required(:text) => String.t()
}

@type image_content :: %{
  required(:type) => String.t(),
  required(:data) => String.t(),
  required(:mimeType) => String.t()
}

@type message :: %{
  required(:role) => String.t(),
  required(:content) => content()
}

@type capabilities :: %{
  optional(:tools) => map(),
  optional(:resources) => map(),
  optional(:prompts) => map(),
  optional(:logging) => map(),
  optional(:sampling) => map(),
  optional(:roots) => map()
}
```

---

## Error Codes

ExMCP uses standard JSON-RPC 2.0 error codes:

- `-32700` - Parse error
- `-32600` - Invalid request
- `-32601` - Method not found
- `-32602` - Invalid params
- `-32603` - Internal error

Custom MCP errors use codes in the `-32000` to `-32099` range.

---

## Transport-Specific Options

### stdio Transport

- `:command` (required) - Command to execute
- `:args` - Command arguments (default: [])
- `:env` - Environment variables (default: [])
- `:cd` - Working directory

### Streamable HTTP Transport

- `:url` (required) - HTTP endpoint URL for Streamable HTTP with SSE
- `:headers` - HTTP headers (default: [])
- `:timeout` - Connection timeout in ms (default: 5000)

### BEAM Transport

Client options:
- `:server` (required) - Server process (pid, name, or {node, name})

Server options:
- `:name` - Optional process registration

---

## ExMCP.Client.Handler

Behaviour for handling server-to-client requests.

### Callbacks

#### `init/1`
Initialize the handler state.

```elixir
@callback init(args :: any()) :: {:ok, state :: any()}

# Example
@impl true
def init(args) do
  {:ok, %{roots: args[:roots] || [], model: "gpt-4"}}
end
```

#### `handle_ping/1`
Handle a ping request from the server.

```elixir
@callback handle_ping(state) :: 
  {:ok, map(), state} | {:error, error_info, state}

# Example
@impl true
def handle_ping(state) do
  {:ok, %{}, state}
end
```

#### `handle_list_roots/1`
Handle a request for the client's roots.

```elixir
@callback handle_list_roots(state) :: 
  {:ok, [map()], state} | {:error, error_info, state}

# Example
@impl true
def handle_list_roots(state) do
  {:ok, state.roots, state}
end
```

#### `handle_create_message/2`
Handle a request to sample an LLM.

```elixir
@callback handle_create_message(params :: map(), state) ::
  {:ok, map(), state} | {:error, error_info, state}

# Example
@impl true
def handle_create_message(params, state) do
  messages = params["messages"]
  response = MyLLM.chat(messages, model: state.model)
  
  result = %{
    "role" => "assistant",
    "content" => %{"type" => "text", "text" => response},
    "model" => state.model
  }
  
  {:ok, result, state}
end
```

---

## ExMCP.Client.DefaultHandler

Default implementation of ExMCP.Client.Handler with approval support.

### Usage

```elixir
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: {ExMCP.Client.DefaultHandler, [
    approval_handler: ExMCP.Approval.Console,
    roots: [%{uri: "file:///home", name: "Home"}],
    model_selector: fn _params -> "claude-3" end
  ]}
)
```

### Options

- `:approval_handler` - Module implementing ExMCP.Approval behaviour
- `:roots` - List of root directories to expose
- `:model_selector` - Function to select which model to use

---

## ExMCP.Approval

Behaviour for implementing human-in-the-loop approval flows.

### Callbacks

#### `request_approval/3`
Request user approval for an operation.

```elixir
@callback request_approval(
  type :: approval_type(), 
  data :: any(), 
  opts :: keyword()
) :: approval_result()

@type approval_type :: :sampling | :response | :tool_call | :resource_access | atom()
@type approval_result :: {:approved, any()} | {:denied, String.t()} | {:modified, any()}

# Example
@impl true
def request_approval(:sampling, params, _opts) do
  if user_approves?(params) do
    {:approved, params}
  else
    {:denied, "User rejected the request"}
  end
end
```

---

## ExMCP.Approval.Console

Console-based implementation of the approval behaviour.

### Usage

```elixir
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: {ExMCP.Client.DefaultHandler, [
    approval_handler: ExMCP.Approval.Console
  ]}
)
```

This handler will:
- Display approval prompts in the terminal
- Show details about sampling requests and responses
- Allow users to approve, deny, or (in future) modify requests
- Support approval for tool calls and resource access

---

## ExMCP.Authorization.TokenManager

OAuth token lifecycle management with automatic refresh.

### Functions

#### `start_link/1`
Starts a TokenManager process.

```elixir
@spec start_link(keyword()) :: GenServer.on_start()

# Options:
# - :auth_config - Authorization configuration map
# - :initial_token - Initial OAuth token (optional)
# - :refresh_window - Seconds before expiry to refresh (default: 300)

{:ok, manager} = TokenManager.start_link(
  auth_config: %{
    client_id: "my-client",
    client_secret: "secret",
    token_endpoint: "https://auth.example.com/token"
  },
  initial_token: %{
    "access_token" => "token123",
    "refresh_token" => "refresh123",
    "expires_in" => 3600
  }
)
```

#### `get_token/1`
Gets the current access token.

```elixir
@spec get_token(GenServer.server()) :: {:ok, String.t()} | {:error, atom()}

{:ok, token} = TokenManager.get_token(manager)
```

#### `refresh_now/1`
Forces an immediate token refresh.

```elixir
@spec refresh_now(GenServer.server()) :: {:ok, String.t()} | {:error, any()}

{:ok, new_token} = TokenManager.refresh_now(manager)
```

#### `subscribe/1`
Subscribes to token update notifications.

```elixir
@spec subscribe(GenServer.server()) :: :ok

TokenManager.subscribe(manager)
# Will receive:
# - {:token_refreshed, new_token}
# - {:token_refresh_failed, reason}
```

---

## ExMCP.Authorization.ErrorHandler

Handles OAuth authorization errors (401/403 responses).

### Functions

#### `handle_auth_error/4`
Processes authorization errors and determines appropriate action.

```elixir
@spec handle_auth_error(
  status_code :: integer(),
  headers :: list(),
  body :: String.t(),
  state :: map()
) :: {:retry, map()} | {:error, atom()} | :ok

case ErrorHandler.handle_auth_error(401, headers, body, state) do
  {:retry, %{action: :refresh_token}} ->
    # Refresh token and retry
  {:error, :unauthorized_no_auth_info} ->
    # No auth info in response
  :ok ->
    # Continue (not an auth error)
end
```

---

## ExMCP.Authorization.Interceptor

Request interceptor for automatic authorization header injection.

### Functions

#### `add_auth_headers/2`
Adds authorization headers to requests.

```elixir
@spec add_auth_headers(map() | keyword(), keyword()) :: 
  {:ok, map() | keyword()} | {:error, any()}

{:ok, auth_request} = Interceptor.add_auth_headers(
  request, 
  token_manager: manager
)
```

#### `wrap_request_fn/2`
Creates an authorization-aware request function.

```elixir
@spec wrap_request_fn(
  (map() -> {:ok, any()} | {:error, any()}),
  keyword()
) :: (map() -> {:ok, any()} | {:error, any()})

auth_request_fn = Interceptor.wrap_request_fn(
  original_request_fn,
  token_manager: manager
)

# Now auth_request_fn will:
# 1. Add auth headers automatically
# 2. Handle 401/403 responses
# 3. Retry with refreshed token if needed
```

---

## ExMCP.SecureServer

Security-enhanced MCP server with built-in security best practices.

### Usage

```elixir
{:ok, server} = ExMCP.SecureServer.start_link(
  handler: MyHandler,
  transport: :stdio,
  server_id: "my-secure-server",
  security: %{
    require_auth: true,
    trusted_issuers: ["https://auth.example.com"],
    introspection_endpoint: "https://auth.example.com/introspect",
    approval_handler: MyApprovalHandler
  }
)
```

### Security Features

- **Token Validation**: Validates all tokens were issued for this specific server
- **Client Registration**: Tracks all clients with audit trails
- **Consent Management**: Requires user consent for dynamic clients
- **Trust Boundaries**: Enforces token audience separation

---

## ExMCP.Security.TokenValidator

Implements strict token validation per MCP security requirements.

### Functions

#### `validate_token_for_server/2`
Validates a token was explicitly issued for this MCP server.

```elixir
@spec validate_token_for_server(String.t(), keyword()) :: 
  {:ok, map()} | {:error, atom()}

{:ok, token_info} = TokenValidator.validate_token_for_server(
  token,
  server_id: "my-server",
  trusted_issuers: ["https://auth.example.com"],
  required_scopes: ["mcp:read"]
)
```

#### `validate_dynamic_client_consent/2`
Ensures user consent for dynamic client registration.

```elixir
{:ok, :approved} = TokenValidator.validate_dynamic_client_consent(
  client_metadata,
  approval_handler
)
```

---

## ExMCP.Security.ConsentManager

Manages user consent for dynamic client registrations.

### Functions

#### `request_consent/2`
Requests user consent for a dynamic client.

```elixir
@spec request_consent(map(), String.t()) :: 
  {:ok, consent_record()} | {:error, atom()}

{:ok, consent} = ConsentManager.request_consent(
  %{
    client_id: "dynamic-client",
    client_name: "Third Party App",
    scope: ["mcp:read", "mcp:write"]
  },
  "user-123"
)
```

#### `check_consent/3`
Verifies valid consent exists.

```elixir
{:ok, :valid} = ConsentManager.check_consent(
  "client-id",
  "user-id", 
  ["mcp:read"]
)
```

---

## ExMCP.Security.ClientRegistry

Maintains client accountability and audit trails.

### Functions

#### `register_client/2`
Registers a client with the MCP server.

```elixir
@spec register_client(map(), :static | :dynamic) :: 
  {:ok, client_info()} | {:error, atom()}

{:ok, client} = ClientRegistry.register_client(
  %{
    client_id: "my-client",
    name: "My Application",
    version: "1.0.0"
  },
  :static
)
```

#### `record_request/3`
Records client requests for audit trail.

```elixir
ClientRegistry.record_request(
  "client-id",
  "tools/call",
  "request-123"
)
```

---

## Examples

See the [examples](examples/) directory for complete working examples:

- `basic_server.exs` - Simple MCP server
- `basic_client.exs` - Simple MCP client
- `batch_requests.exs` - Batch request functionality
- `bidirectional_communication.exs` - Server-to-client requests
- `human_in_the_loop.exs` - Approval flows for sensitive operations
- `oauth_authorization_example.exs` - OAuth 2.1 authorization example
- `secure_server_example.exs` - Security best practices implementation
- `beam_transport/` - BEAM transport examples
- `roots_example.exs` - Roots capability example
- `resource_subscription_example.exs` - Subscription example
- `progress_example.exs` - Progress notification example
- `sampling_example.exs` - LLM integration example