# ExMCP API Reference

## Module Index

### Core Modules
- [`ExMCP`](#exmcp) - Main module with convenience functions
- [`ExMCP.Client`](#exmcpclient) - MCP client implementation
- [`ExMCP.Client.Handler`](#exmcpclienthandler) - Client handler behaviour
- [`ExMCP.Client.DefaultHandler`](#exmcpclientdefaulthandler) - Default client handler
- [`ExMCP.Server`](#exmcpserver) - MCP server implementation
- [`ExMCP.Server.Handler`](#exmcpserverhandler) - Server handler behaviour
- [`ExMCP.Transport`](#exmcptransport) - Transport behaviour

### Native Service Dispatcher
- [`ExMCP.Native`](#exmcpnative) - High-performance service dispatcher
- [`ExMCP.Service`](#exmcpservice) - Service macro for automatic registration

### Authorization (Optional MCP Feature)
- [`ExMCP.Authorization`](#exmcpauthorization) - OAuth 2.1 authorization flows
- [`ExMCP.Authorization.TokenManager`](#exmcpauthorizationtokenmanager) - Automatic token management
- [`ExMCP.Authorization.Interceptor`](#exmcpauthorizationinterceptor) - Request authorization middleware

### Supporting Modules
- [`ExMCP.Content`](#exmcpcontent) - Content type helpers
- [`ExMCP.Types`](#exmcptypes) - Type definitions

---

## ExMCP

Main module providing convenience functions and version information.

### Functions

#### `start_client/1`
Convenience function to start an MCP client.

```elixir
@spec start_client(keyword()) :: {:ok, pid()} | {:error, term()}

# stdio transport
{:ok, client} = ExMCP.start_client(
  transport: :stdio,
  command: ["python", "mcp-server.py"]
)

# HTTP transport  
{:ok, client} = ExMCP.start_client(
  transport: :http,
  url: "https://api.example.com"
)
```

#### `start_server/1`
Convenience function to start an MCP server.

```elixir
@spec start_server(keyword()) :: {:ok, pid()} | {:error, term()}

{:ok, server} = ExMCP.start_server(
  handler: MyApp.Handler,
  transport: :stdio
)
```

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
# => "0.5.0"
```

#### `supported_versions/0`
Returns information about supported protocol versions.

```elixir
@spec supported_versions() :: [String.t()]

# Example
ExMCP.supported_versions()
# => ["2024-11-05", "2025-03-26", "draft"]
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
# - transport: atom() - Transport type (:stdio, :http)
# - handler: module() | {module(), args} - Optional client handler
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
  url: "https://api.example.com",
  endpoint: "/mcp/v1",
  use_sse: true,
  security: %{
    auth: {:bearer, "token"}
  }
)
```

#### `initialize/3`
Initializes the MCP connection with the server.

```elixir
@spec initialize(GenServer.server(), map(), timeout()) :: 
  {:ok, server_info()} | {:error, term()}

# Example
{:ok, server_info} = ExMCP.Client.initialize(client, %{
  protocolVersion: "2025-03-26",
  clientInfo: %{
    name: "my-client",
    version: "1.0.0"
  },
  capabilities: %{
    roots: %{},
    sampling: %{}
  }
})

# Server info contains:
# - protocolVersion
# - capabilities
# - serverInfo (name, version)
```

#### `list_tools/2`
Lists available tools from the server.

```elixir
@spec list_tools(GenServer.server(), timeout()) :: 
  {:ok, tools_list()} | {:error, term()}

# Example
{:ok, %{tools: tools}} = ExMCP.Client.list_tools(client)
# => {:ok, %{tools: [%{name: "search", description: "Search the web"}]}}
```

#### `call_tool/4`
Calls a tool on the server.

```elixir
@spec call_tool(GenServer.server(), String.t(), map(), timeout() | keyword()) :: 
  {:ok, list(content())} | {:error, term()}

# Simple call
{:ok, content} = ExMCP.Client.call_tool(client, "search", %{
  query: "elixir programming"
})

# With progress tracking
{:ok, content} = ExMCP.Client.call_tool(
  client, 
  "process_data",
  %{dataset: "large"},
  progress_token: "op-123"
)
```

#### `list_resources/2`
Lists available resources from the server.

```elixir
@spec list_resources(GenServer.server(), timeout()) :: 
  {:ok, resources_list()} | {:error, term()}

# Example
{:ok, %{resources: resources}} = ExMCP.Client.list_resources(client)
```

#### `read_resource/3`
Reads a specific resource.

```elixir
@spec read_resource(GenServer.server(), String.t(), timeout()) :: 
  {:ok, list(content())} | {:error, term()}

# Example
{:ok, contents} = ExMCP.Client.read_resource(client, "file:///config.json")
```

#### `subscribe_resource/3`
Subscribes to resource updates.

```elixir
@spec subscribe_resource(GenServer.server(), String.t(), timeout()) :: 
  {:ok, map()} | {:error, term()}

# Example
{:ok, _} = ExMCP.Client.subscribe_resource(client, "file:///config.json")
# Will receive notifications via client handler
```

#### `unsubscribe_resource/3`
Unsubscribes from resource updates.

> **Note:** This is an ExMCP extension. The MCP specification does not define `resources/unsubscribe`.

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

#### `set_log_level/3`
Sets the minimum log level for the server.

```elixir
@spec set_log_level(GenServer.server(), String.t(), timeout()) :: 
  {:ok, map()} | {:error, term()}

# Example
{:ok, _} = ExMCP.Client.set_log_level(client, "debug")
{:ok, _} = ExMCP.Client.set_log_level(client, "warning")

# Valid levels: "debug", "info", "notice", "warning", "error", "critical", "alert", "emergency"
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
  ExMCP.Client.send_cancelled(client, id, "Shutting down")
end
```

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

Default implementation of ExMCP.Client.Handler.

### Usage

```elixir
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["mcp-server"],
  handler: {ExMCP.Client.DefaultHandler, [
    roots: [%{uri: "file:///home", name: "Home"}],
    model_selector: fn _params -> "claude-3" end
  ]}
)
```

### Options

- `:roots` - List of root directories to expose
- `:model_selector` - Function to select which model to use

---

## ExMCP.Server

MCP server implementation.

### Functions

#### `start_link/1`
Starts a new MCP server process.

```elixir
@spec start_link(keyword()) :: GenServer.on_start()

# Options:
# - handler: module() | {module(), args} - Server handler module
# - transport: atom() - Transport type (:stdio, :http)
# - Transport-specific options
# - name: term() - Optional GenServer name

# stdio transport server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :stdio
)

# HTTP transport server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  port: 4000,
  bind: {127, 0, 0, 1},
  endpoint: "/mcp"
)
```

#### `send_notification/3`
Sends a notification to the connected client.

```elixir
@spec send_notification(GenServer.server(), String.t(), map()) :: :ok

# Resource update notification
:ok = ExMCP.Server.send_notification(server, "notifications/resources/updated", %{
  uri: "file:///config.json"
})

# Tool list changed notification
:ok = ExMCP.Server.send_notification(server, "notifications/tools/list_changed", %{})
```

#### `send_progress/3`
Sends a progress notification for long-running operations.

```elixir
@spec send_progress(GenServer.server(), progress_token(), progress_info()) :: :ok

# Example
:ok = ExMCP.Server.send_progress(server, "op-123", %{
  progress: 0.5,
  total: 100
})
```

#### `get_client_info/1`
Gets information about the connected client.

```elixir
@spec get_client_info(GenServer.server()) :: {:ok, client_info()} | {:error, :not_initialized}

# Example
{:ok, client_info} = ExMCP.Server.get_client_info(server)
# => {:ok, %{name: "my-client", version: "1.0.0", ...}}
```

---

## ExMCP.Server.Handler

Behaviour for implementing MCP server handlers.

### Callbacks

#### `init/1`
Initializes the handler state.

```elixir
@callback init(args :: any()) :: {:ok, state :: any()} | {:error, reason :: any()}

# Example
@impl true
def init(_args) do
  {:ok, %{tools: [], resources: []}}
end
```

#### `handle_initialize/2`
Handles the initialize request from the client.

```elixir
@callback handle_initialize(params :: map(), state) ::
  {:ok, result :: map(), new_state} | {:error, reason :: any(), state}

# Example
@impl true
def handle_initialize(_params, state) do
  {:ok, %{
    name: "my-server",
    version: "1.0.0",
    capabilities: %{
      tools: %{},
      resources: %{subscribe: true}
    }
  }, state}
end
```

#### `handle_list_tools/1`
Lists available tools.

```elixir
@callback handle_list_tools(state) ::
  {:ok, [tool()], state} | {:error, reason :: any(), state}

# Example
@impl true
def handle_list_tools(state) do
  tools = [
    %{
      name: "search",
      description: "Search the web",
      input_schema: %{
        type: "object",
        properties: %{
          query: %{type: "string", description: "Search query"}
        },
        required: ["query"]
      }
    }
  ]
  {:ok, tools, state}
end
```

#### `handle_call_tool/3`
Executes a tool call.

```elixir
@callback handle_call_tool(name :: String.t(), arguments :: map(), state) ::
  {:ok, [content()], new_state} | {:error, reason :: any(), state}

# Example
@impl true
def handle_call_tool("search", %{"query" => query}, state) do
  results = perform_search(query)
  content = [%{type: "text", text: results}]
  {:ok, content, state}
end
```

### Optional Callbacks

Additional callbacks are available for resources, prompts, sampling, and more. See the module documentation for details.

---

## ExMCP.Transport

Behaviour for implementing custom transports.

### Built-in Transports

- **`:stdio`** - Standard I/O communication (MCP specification)
- **`:http`** - HTTP with optional SSE streaming (MCP specification)  
- **`:test`** - In-memory transport for testing

### Callbacks

#### `connect/1`
Establishes a connection.

```elixir
@callback connect(opts :: keyword()) :: {:ok, state :: term()} | {:error, reason :: term()}
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
  {:ok, message :: String.t(), new_state :: term()} | 
  {:error, reason :: term()}
```

#### `close/1`
Closes the transport connection.

```elixir
@callback close(state :: term()) :: :ok
```

---

## ExMCP.Native

High-performance service dispatcher for direct process communication within Elixir clusters.

### Functions

#### `register_service/1`
Registers a service with the distributed registry.

```elixir
@spec register_service(atom()) :: :ok

# Example
ExMCP.Native.register_service(:my_tools)
# => :ok
```

#### `unregister_service/1`
Unregisters a service from the distributed registry.

```elixir
@spec unregister_service(atom()) :: :ok

# Example
ExMCP.Native.unregister_service(:my_tools)
# => :ok
```

#### `call/4`
Calls a service method with the given parameters.

```elixir
@spec call(service_id(), method(), params(), keyword()) :: {:ok, result()} | {:error, term()}

# Examples
{:ok, tools} = ExMCP.Native.call(:my_tools, "list_tools", %{})

{:ok, result} = ExMCP.Native.call(
  :calculator,
  "add", 
  %{"a" => 1, "b" => 2},
  timeout: 10_000,
  meta: %{"trace_id" => "abc123"}
)

# Cross-node call
{:ok, result} = ExMCP.Native.call(
  {:data_service, :"worker@cluster.local"},
  "process_data",
  %{"dataset" => "large_data"}
)
```

#### `notify/3`
Sends a notification to a service (fire-and-forget).

```elixir
@spec notify(service_id(), method(), params()) :: :ok | {:error, term()}

# Example
:ok = ExMCP.Native.notify(:event_service, "resource_updated", %{
  "uri" => "file:///config.json",
  "type" => "modified"
})
```

#### `list_services/0`
Lists all services registered in the distributed registry.

```elixir
@spec list_services() :: [{atom(), pid(), map()}]

# Example
services = ExMCP.Native.list_services()
# => [{:calculator, #PID<0.123.0>, %{registered_at: ~U[...]}}, ...]
```

#### `service_available?/1`
Checks if a service is available in the distributed registry.

```elixir
@spec service_available?(atom()) :: boolean()

# Example
if ExMCP.Native.service_available?(:calculator) do
  {:ok, result} = ExMCP.Native.call(:calculator, "add", %{"a" => 1, "b" => 2})
end
```

### Performance

- **Local calls**: ~15μs latency
- **Cross-node calls**: ~50μs latency
- **Zero serialization**: Direct term passing for local calls

---

## ExMCP.Service

Macro for creating MCP services with automatic registration.

### Usage

```elixir
defmodule MyToolService do
  use ExMCP.Service, name: :my_tools

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "search",
        "description" => "Search the web",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "query" => %{"type" => "string"}
          }
        }
      }
    ]
    {:ok, %{"tools" => tools}, state}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "search", "arguments" => args}, state) do
    results = search(args["query"])
    {:ok, %{"content" => [%{"type" => "text", "text" => results}]}, state}
  end
end
```

### Callbacks

#### `handle_mcp_request/3`
Handles an MCP request.

```elixir
@callback handle_mcp_request(method :: String.t(), params :: map(), state :: term()) ::
            {:ok, result :: term(), new_state :: term()}
            | {:error, error :: term(), new_state :: term()}
```

### Automatic Features

- **Service Registration**: Automatically registers with `ExMCP.Native` on startup
- **Service Unregistration**: Automatically unregisters on termination
- **GenServer Integration**: Full GenServer callbacks available

---

## ExMCP.Authorization

OAuth 2.1 authorization support for MCP.

### Functions

#### `client_credentials_flow/1`
Performs OAuth 2.1 client credentials flow.

```elixir
@spec client_credentials_flow(map()) :: {:ok, token_response()} | {:error, term()}

# Example
{:ok, token_response} = ExMCP.Authorization.client_credentials_flow(%{
  client_id: "my-client",
  client_secret: "my-secret",
  token_endpoint: "https://auth.example.com/token",
  scope: "mcp:read mcp:write"
})
# => {:ok, %{
#      "access_token" => "eyJ...",
#      "token_type" => "Bearer",
#      "expires_in" => 3600
#    }}
```

#### `authorization_code_flow/1`
Performs OAuth 2.1 authorization code flow with PKCE.

```elixir
@spec authorization_code_flow(map()) :: {:ok, authorization_url()} | {:error, term()}

# Step 1: Generate authorization URL
{:ok, auth_url, state} = ExMCP.Authorization.authorization_code_flow(%{
  client_id: "my-client",
  authorization_endpoint: "https://auth.example.com/authorize",
  redirect_uri: "http://localhost:8080/callback",
  scope: "mcp:read"
})

# Step 2: After user authorizes and you receive the code
{:ok, token_response} = ExMCP.Authorization.exchange_code(%{
  code: "received_code",
  code_verifier: state.code_verifier,
  client_id: "my-client",
  token_endpoint: "https://auth.example.com/token",
  redirect_uri: "http://localhost:8080/callback"
})
```

#### `refresh_token/1`
Refreshes an access token using a refresh token.

```elixir
@spec refresh_token(map()) :: {:ok, token_response()} | {:error, term()}

# Example
{:ok, new_token_response} = ExMCP.Authorization.refresh_token(%{
  refresh_token: "existing_refresh_token",
  client_id: "my-client",
  client_secret: "my-secret",
  token_endpoint: "https://auth.example.com/token"
})
```

#### `generate_pkce_challenge/0`
Generates PKCE challenge and verifier for authorization code flow.

```elixir
@spec generate_pkce_challenge() :: {:ok, String.t(), String.t()} | {:error, term()}

# Example
{:ok, code_verifier, code_challenge} = ExMCP.Authorization.generate_pkce_challenge()
```

---

## ExMCP.Authorization.TokenManager

Manages OAuth tokens with automatic refresh.

### Functions

#### `start_link/1`
Starts a TokenManager process.

```elixir
@spec start_link(keyword()) :: GenServer.on_start()

# Options:
# - :auth_config - Authorization configuration map
# - :initial_token - Initial OAuth token (optional)
# - :refresh_window - Seconds before expiry to refresh (default: 300)

{:ok, manager} = ExMCP.Authorization.TokenManager.start_link(
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

{:ok, token} = ExMCP.Authorization.TokenManager.get_token(manager)
```

#### `refresh_now/1`
Forces an immediate token refresh.

```elixir
@spec refresh_now(GenServer.server()) :: {:ok, String.t()} | {:error, any()}

{:ok, new_token} = ExMCP.Authorization.TokenManager.refresh_now(manager)
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

{:ok, auth_request} = ExMCP.Authorization.Interceptor.add_auth_headers(
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

auth_request_fn = ExMCP.Authorization.Interceptor.wrap_request_fn(
  original_request_fn,
  token_manager: manager
)

# Now auth_request_fn will:
# 1. Add auth headers automatically
# 2. Handle 401/403 responses
# 3. Retry with refreshed token if needed
```

---

## ExMCP.Content

Content type helpers for MCP.

### Functions

#### `text/1`
Creates a text content object.

```elixir
@spec text(String.t()) :: map()

# Example
content = ExMCP.Content.text("Hello, world!")
# => %{type: "text", text: "Hello, world!"}
```

#### `image/2`
Creates an image content object.

```elixir
@spec image(binary(), String.t()) :: map()

# Example
{:ok, data} = File.read("image.png")
content = ExMCP.Content.image(data, "image/png")
# => %{type: "image", data: "base64...", mimeType: "image/png"}
```

#### `resource/1`
Creates a resource content object.

```elixir
@spec resource(String.t()) :: map()

# Example
content = ExMCP.Content.resource("file:///data.json")
# => %{type: "resource", uri: "file:///data.json"}
```

---

## ExMCP.Types

Type definitions used throughout the library.

### Core Types

```elixir
@type tool :: %{
  required(:name) => String.t(),
  required(:description) => String.t(),
  optional(:input_schema) => json_schema()
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

- `:url` (required) - HTTP base URL
- `:endpoint` - MCP endpoint path (default: "/mcp/v1")
- `:headers` - HTTP headers (default: [])
- `:timeout` - Connection timeout in ms (default: 5000)
- `:use_sse` - Use Server-Sent Events for responses (default: true)
- `:security` - Security configuration (see Security Guide)

---

## Examples

See the [examples](../examples/) directory for complete working examples:

- `basic_server.exs` - Simple MCP server
- `basic_client.exs` - Simple MCP client
- `batch_requests.exs` - Batch request functionality
- `bidirectional_communication.exs` - Server-to-client requests
- `oauth_authorization_example.exs` - OAuth 2.1 authorization
- `beam_transport/` - Native Service Dispatcher examples
- `roots_example.exs` - Roots capability example
- `resource_subscription_example.exs` - Subscription example
- `progress_example.exs` - Progress notification example
- `sampling_example.exs` - LLM integration example