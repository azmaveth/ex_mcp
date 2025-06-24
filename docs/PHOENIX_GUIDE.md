# Phoenix Integration Guide

ExMCP provides seamless integration with Phoenix applications through the `ExMCP.HttpPlug` module, which implements the standard Plug behavior. This allows you to easily add MCP (Model Context Protocol) server capabilities to your existing Phoenix applications.

## Quick Setup

### 1. Add ExMCP to Your Phoenix Project

```elixir
# In mix.exs
defp deps do
  [
    {:ex_mcp, "~> 0.6.0"},
    # ... your other dependencies
  ]
end
```

### 2. Create an MCP Handler

Create a handler module that implements your MCP server logic:

```elixir
# lib/my_app/mcp_handler.ex
defmodule MyApp.MCPHandler do
  use ExMCP.Server.Handler
  
  @impl true
  def init(_args), do: {:ok, %{}}
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: Application.get_env(:my_app, :app_name, "my-phoenix-app"),
      version: Application.spec(:my_app, :vsn) |> to_string(),
      capabilities: %{
        tools: %{},
        resources: %{}
      }
    }, state}
  end
  
  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        name: "get_user_count",
        description: "Get the total number of registered users",
        input_schema: %{
          type: "object",
          properties: %{}
        }
      },
      %{
        name: "search_posts",
        description: "Search blog posts",
        input_schema: %{
          type: "object",
          properties: %{
            query: %{type: "string", description: "Search query"},
            limit: %{type: "integer", minimum: 1, maximum: 50, default: 10}
          },
          required: ["query"]
        }
      }
    ]
    {:ok, tools, state}
  end
  
  @impl true
  def handle_call_tool("get_user_count", _args, state) do
    count = MyApp.Accounts.count_users()
    
    result = [
      %{
        type: "text", 
        text: "Total registered users: #{count}"
      }
    ]
    
    {:ok, result, state}
  end
  
  def handle_call_tool("search_posts", args, state) do
    query = Map.get(args, "query")
    limit = Map.get(args, "limit", 10)
    
    posts = MyApp.Blog.search_posts(query, limit: limit)
    
    results = Enum.map(posts, fn post ->
      %{
        type: "text",
        text: "**#{post.title}**\n#{post.excerpt}\nPublished: #{post.published_at}"
      }
    end)
    
    {:ok, results, state}
  end
  
  def handle_call_tool(tool_name, _args, state) do
    error = %{
      code: -32601,
      message: "Unknown tool: #{tool_name}"
    }
    {:error, error, state}
  end
  
  # Implement other required callbacks
  @impl true
  def handle_list_resources(state), do: {:ok, [], state}
  
  @impl true
  def handle_read_resource(_uri, state) do
    error = %{code: -32601, message: "Resources not implemented"}
    {:error, error, state}
  end
  
  @impl true
  def handle_list_prompts(state), do: {:ok, [], state}
  
  @impl true
  def handle_get_prompt(_name, _args, state) do
    error = %{code: -32601, message: "Prompts not implemented"}
    {:error, error, state}
  end
end
```

### 3. Add to Your Phoenix Router

```elixir
# lib/my_app_web/router.ex
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  # ... your existing pipelines

  pipeline :mcp do
    plug :accepts, ["json"]
    # Add authentication if needed:
    # plug MyAppWeb.Plugs.Authenticate
  end

  # ... your existing routes

  scope "/api" do
    pipe_through :mcp
    
    # Mount MCP server at /api/mcp
    forward "/mcp", ExMCP.HttpPlug,
      handler: MyApp.MCPHandler,
      server_info: %{
        name: "my-phoenix-app",
        version: "1.0.0"
      },
      sse_enabled: true,    # Enable Server-Sent Events for real-time communication
      cors_enabled: true    # Enable CORS for web clients
  end
end
```

### 4. Test Your Integration

Start your Phoenix server and test the MCP endpoint:

```bash
# Start Phoenix server
mix phx.server

# Test with curl
curl -X POST http://localhost:4000/api/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/list",
    "params": {}
  }'

# Expected response:
# {
#   "jsonrpc": "2.0",
#   "id": 1,
#   "result": {
#     "tools": [
#       {
#         "name": "get_user_count",
#         "description": "Get the total number of registered users",
#         "input_schema": {"type": "object", "properties": {}}
#       },
#       {
#         "name": "search_posts",
#         "description": "Search blog posts",
#         "input_schema": {
#           "type": "object",
#           "properties": {
#             "query": {"type": "string", "description": "Search query"},
#             "limit": {"type": "integer", "minimum": 1, "maximum": 50, "default": 10}
#           },
#           "required": ["query"]
#         }
#       }
#     ]
#   }
# }
```

## Advanced Configuration

### Authentication & Authorization

Integrate MCP with your existing Phoenix authentication:

```elixir
# lib/my_app_web/plugs/mcp_auth.ex
defmodule MyAppWeb.Plugs.MCPAuth do
  import Plug.Conn
  
  def init(opts), do: opts
  
  def call(conn, _opts) do
    case get_req_header(conn, "authorization") do
      ["Bearer " <> token] ->
        case MyApp.Auth.verify_token(token) do
          {:ok, user} ->
            assign(conn, :current_user, user)
          
          {:error, _reason} ->
            conn
            |> put_status(401)
            |> Phoenix.Controller.json(%{error: "Invalid token"})
            |> halt()
        end
      
      _ ->
        conn
        |> put_status(401)
        |> Phoenix.Controller.json(%{error: "Authorization required"})
        |> halt()
    end
  end
end

# In your router:
pipeline :mcp_authenticated do
  plug :accepts, ["json"]
  plug MyAppWeb.Plugs.MCPAuth
end

scope "/api" do
  pipe_through :mcp_authenticated
  
  forward "/mcp", ExMCP.HttpPlug,
    handler: MyApp.AuthenticatedMCPHandler,
    server_info: %{name: "secure-app", version: "1.0.0"}
end
```

### Accessing Request Context

Access the current user and other Phoenix context in your MCP handler:

```elixir
defmodule MyApp.AuthenticatedMCPHandler do
  use ExMCP.Server.Handler
  
  @impl true
  def handle_call_tool("get_my_posts", _args, state) do
    # Access current user from the request context
    user = get_current_user(state)
    
    posts = MyApp.Blog.list_user_posts(user.id)
    
    results = Enum.map(posts, fn post ->
      %{type: "text", text: "#{post.title}: #{post.excerpt}"}
    end)
    
    {:ok, results, state}
  end
  
  # Helper to extract user from state (you'll need to modify the HttpPlug to pass this)
  defp get_current_user(state) do
    # This would require modifying ExMCP.HttpPlug to pass request context
    # For now, you can access it via Process.get/1 if set by a plug
    Process.get(:current_user)
  end
end
```

### Server-Sent Events (SSE)

ExMCP supports real-time communication via SSE. Clients can connect to the SSE endpoint for live updates:

```javascript
// JavaScript client example
const eventSource = new EventSource('http://localhost:4000/api/mcp/sse');

eventSource.onmessage = function(event) {
  const response = JSON.parse(event.data);
  console.log('Received MCP response:', response);
};

// Send MCP requests via regular HTTP POST
fetch('http://localhost:4000/api/mcp', {
  method: 'POST',
  headers: {'Content-Type': 'application/json'},
  body: JSON.stringify({
    jsonrpc: '2.0',
    id: 1,
    method: 'tools/call',
    params: {name: 'get_user_count', arguments: {}}
  })
});
// Response will arrive via SSE connection
```

### Resource Integration

Expose your Phoenix application data as MCP resources:

```elixir
@impl true
def handle_list_resources(state) do
  resources = [
    %{
      uri: "phoenix://users",
      name: "User List",
      description: "List of all registered users",
      mimeType: "application/json"
    },
    %{
      uri: "phoenix://posts/recent",
      name: "Recent Posts",
      description: "Most recent blog posts",
      mimeType: "application/json"
    }
  ]
  {:ok, resources, state}
end

@impl true
def handle_read_resource("phoenix://users", state) do
  users = MyApp.Accounts.list_users()
  
  data = Enum.map(users, fn user ->
    %{id: user.id, email: user.email, name: user.name}
  end)
  
  content = [%{
    type: "text",
    text: Jason.encode!(data, pretty: true),
    mimeType: "application/json"
  }]
  
  {:ok, content, state}
end

def handle_read_resource("phoenix://posts/recent", state) do
  posts = MyApp.Blog.list_recent_posts(limit: 10)
  
  content = [%{
    type: "text", 
    text: Jason.encode!(posts, pretty: true),
    mimeType: "application/json"
  }]
  
  {:ok, content, state}
end
```

## Production Considerations

### Performance

- **Connection Pooling**: Use a connection pool for database operations in your MCP handlers
- **Caching**: Cache frequently requested data to reduce database load
- **Rate Limiting**: Implement rate limiting for MCP endpoints if needed

```elixir
# Rate limiting example with Hammer
pipeline :mcp_rate_limited do
  plug :accepts, ["json"]
  plug MyAppWeb.Plugs.RateLimit, bucket_name: "mcp_api"
end
```

### Security

- **Input Validation**: Always validate tool arguments and resource URIs
- **Authorization**: Check user permissions before executing tools or reading resources
- **Audit Logging**: Log all MCP operations for security auditing

```elixir
@impl true
def handle_call_tool(tool_name, args, state) do
  # Log the operation
  Logger.info("MCP tool called", tool: tool_name, args: args, user: get_current_user_id())
  
  # Validate permissions
  case check_tool_permission(tool_name, get_current_user()) do
    :ok -> 
      # Execute tool
      do_call_tool(tool_name, args, state)
    
    {:error, reason} ->
      error = %{code: -32000, message: "Permission denied: #{reason}"}
      {:error, error, state}
  end
end
```

### Monitoring

Monitor your MCP endpoints like any other Phoenix endpoint:

```elixir
# Add Telemetry events for MCP operations
defmodule MyApp.MCPTelemetry do
  def track_tool_call(tool_name, duration, success) do
    :telemetry.execute(
      [:my_app, :mcp, :tool_call],
      %{duration: duration},
      %{tool: tool_name, success: success}
    )
  end
end

# In your handler:
@impl true
def handle_call_tool(tool_name, args, state) do
  start_time = System.monotonic_time()
  
  result = do_call_tool(tool_name, args, state)
  
  duration = System.monotonic_time() - start_time
  success = case result do
    {:ok, _, _} -> true
    _ -> false
  end
  
  MyApp.MCPTelemetry.track_tool_call(tool_name, duration, success)
  
  result
end
```

## Examples and Use Cases

### E-commerce Integration

```elixir
# Expose product search and order management
@impl true
def handle_list_tools(state) do
  tools = [
    %{
      name: "search_products",
      description: "Search for products in the catalog",
      input_schema: %{
        type: "object",
        properties: %{
          query: %{type: "string"},
          category: %{type: "string"},
          price_max: %{type: "number"}
        }
      }
    },
    %{
      name: "get_order_status", 
      description: "Get the status of an order",
      input_schema: %{
        type: "object",
        properties: %{
          order_id: %{type: "string"}
        },
        required: ["order_id"]
      }
    }
  ]
  {:ok, tools, state}
end
```

### Analytics Dashboard

```elixir
# Expose analytics data as MCP tools
@impl true
def handle_call_tool("get_dashboard_metrics", args, state) do
  timeframe = Map.get(args, "timeframe", "last_7_days")
  
  metrics = MyApp.Analytics.get_metrics(timeframe)
  
  result = [%{
    type: "text",
    text: """
    📊 Dashboard Metrics (#{timeframe}):
    
    👥 Active Users: #{metrics.active_users}
    📈 Page Views: #{metrics.page_views}
    💰 Revenue: $#{metrics.revenue}
    📊 Conversion Rate: #{metrics.conversion_rate}%
    """
  }]
  
  {:ok, result, state}
end
```

## Troubleshooting

### Common Issues

1. **CORS Errors**: Ensure `cors_enabled: true` in your HttpPlug configuration
2. **Authentication Issues**: Verify your authentication pipeline is working correctly
3. **SSE Connection Drops**: Check your load balancer timeout settings
4. **JSON Parsing Errors**: Validate your tool arguments schema

### Debug Mode

Enable debug logging to troubleshoot issues:

```elixir
# In config/dev.exs
config :logger, level: :debug

# In your handler:
require Logger

@impl true
def handle_call_tool(tool_name, args, state) do
  Logger.debug("MCP tool call: #{tool_name} with args: #{inspect(args)}")
  
  result = do_call_tool(tool_name, args, state)
  
  Logger.debug("MCP tool result: #{inspect(result)}")
  
  result
end
```

## Next Steps

1. **Read the [ExMCP Documentation](https://hexdocs.pm/ex_mcp)** for complete API reference
2. **Check out [Examples](../examples/)** for more implementation patterns  
3. **Review [Security Guide](SECURITY.md)** for production deployment best practices
4. **Join the Community** - contribute to the project or ask questions in Issues

---

**Ready to make your Phoenix app AI-ready?** Start with the Quick Setup above and begin exposing your application's capabilities to AI models through the standardized MCP protocol.