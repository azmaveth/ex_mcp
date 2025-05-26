# Advanced MCP Features Examples

This directory contains examples demonstrating the advanced features of the Model Context Protocol:

## 1. Sampling/LLM Integration (`sampling_server.ex`)

Demonstrates how to implement the `sampling/createMessage` feature for LLM integration:

- Handling `createMessage` requests with messages and model preferences
- Managing conversation history
- Token counting and limits
- Integration with prompt templates
- Model selection based on hints

Run the example:
```elixir
iex> c "examples/advanced_features/sampling_server.ex"
iex> Examples.AdvancedFeatures.SamplingClient.demo()
```

Key concepts:
- Enable sampling in server capabilities: `sampling: %{}`
- Implement `handle_create_message/2` callback
- Return response with content, model, and stopReason
- Support model preferences (hints, temperature, etc.)

## 2. Notifications (`notifications_server.ex`)

Demonstrates all types of MCP notifications:

### Progress Notifications
- Track long-running operations
- Send incremental progress updates
- Use progress tokens for correlation

### Change Notifications
- **Resource changes**: Notify when resources are added/removed
- **Resource updates**: Notify when specific resource content changes
- **Tool changes**: Notify when tools are dynamically added/removed
- **Prompt changes**: Notify when prompts are modified

Run the example:
```elixir
iex> c "examples/advanced_features/notifications_server.ex"
iex> Examples.AdvancedFeatures.NotificationsClient.demo()
```

Key concepts:
- Use `ExMCP.Server.notify_*` functions to send notifications
- Progress tokens are passed in tool parameters as `_progressToken`
- Notifications are fire-and-forget (no response expected)
- Clients automatically log notifications

## Integration Tips

### Progress Tracking Pattern

```elixir
def handle_call_tool("long_operation", params, state) do
  if token = params["_progressToken"] do
    Task.start(fn ->
      for i <- 1..100 do
        do_work()
        ExMCP.Server.notify_progress(self(), token, i, 100)
      end
    end)
  end
  
  {:ok, [%{type: "text", text: "Started"}], state}
end
```

### Dynamic Content Pattern

```elixir
def add_resource(server, resource) do
  # Update your state
  GenServer.call(server, {:add_resource, resource})
  
  # Notify clients
  ExMCP.Server.notify_resources_changed(server)
end
```

### LLM Integration Pattern

```elixir
def handle_create_message(params, state) do
  # Extract parameters
  messages = params["messages"]
  model_prefs = params["modelPreferences"]
  
  # Call your LLM provider
  case MyLLM.generate(messages, model_prefs) do
    {:ok, response} ->
      result = %{
        content: %{type: "text", text: response.text},
        model: response.model,
        stopReason: "stop"
      }
      {:ok, result, state}
      
    {:error, reason} ->
      {:error, reason, state}
  end
end
```

## Testing Notifications

The default ExMCP.Client logs all notifications at the info level. To handle them programmatically, you can:

1. Implement a custom client that processes notifications
2. Use Logger metadata to filter notification logs
3. Monitor the client process for specific notification patterns

## Best Practices

1. **Progress Tokens**: Always check for `_progressToken` in tool parameters
2. **Async Operations**: Use Task or GenServer for long-running operations
3. **Notification Timing**: Add small delays between related notifications
4. **Error Handling**: Notifications are best-effort; don't rely on delivery
5. **State Management**: Keep server state consistent when sending change notifications