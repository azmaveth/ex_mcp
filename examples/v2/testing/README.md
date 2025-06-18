# ExMCP v2 Testing Framework

This directory contains comprehensive examples for using the ExMCP v2 testing framework, which provides specialized tools for testing MCP (Model Context Protocol) applications.

## Quick Start

```elixir
defmodule MyMCPTest do
  use ExMCP.TestCase, async: true
  
  test "basic tool functionality" do
    with_mock_server(tools: [sample_tool()]) do |client|
      result = call_tool(client, "sample_tool", %{input: "test"})
      assert_success(result)
      assert_content_type(result, :text)
      assert_content_contains(result, "test")
    end
  end
end
```

## Testing Framework Components

### 1. ExMCP.TestCase

The foundation of the testing framework, providing:
- Custom test macros and utilities
- Automatic cleanup and resource management
- Performance testing capabilities
- Temporary file management

### 2. ExMCP.Testing.Assertions

MCP-specific assertions for validating:
- Protocol compliance
- Content structure and types
- Tool and resource definitions
- Performance characteristics

### 3. ExMCP.Testing.Builders

Test data generators for creating:
- Valid MCP content (text, image, audio, etc.)
- Tool and resource definitions
- Protocol messages and responses
- JSON schemas and validation rules

### 4. ExMCP.Testing.MockServer

Comprehensive mock MCP server for:
- Simulating server behavior
- Testing client implementations
- Error injection and latency simulation
- Call tracking and state management

## Examples by Category

### Basic Testing
- [Getting Started](getting_started_test.exs) - Basic testing patterns
- [Content Testing](content_testing_test.exs) - Testing content protocols
- [Tool Testing](tool_testing_test.exs) - Testing tool implementations

### Advanced Testing
- [Integration Testing](integration_testing_test.exs) - End-to-end workflows
- [Performance Testing](performance_testing_test.exs) - Benchmarks and load testing
- [Error Handling](error_handling_test.exs) - Robust error testing

### Specialized Testing
- [Property-Based Testing](property_based_test.exs) - Property-based test patterns
- [Mock Server Usage](mock_server_usage_test.exs) - Advanced mock server features
- [Custom Assertions](custom_assertions_test.exs) - Building custom test assertions

## Testing Patterns

### 1. Unit Testing Pattern

```elixir
test "content validation" do
  content = Protocol.text("Hello world")
  assert_valid_content(content)
  assert_content_type(content, :text)
end
```

### 2. Integration Testing Pattern

```elixir
test "full workflow" do
  with_mock_server(tools: [my_tool()]) do |client|
    # 1. Discover tools
    tools = list_tools(client)
    assert_has_tool(tools, "my_tool")
    
    # 2. Execute tool
    result = call_tool(client, "my_tool", %{input: "test"})
    assert_success(result)
    
    # 3. Validate result
    assert_valid_tool_result(result)
  end
end
```

### 3. Performance Testing Pattern

```elixir
test "response time" do
  assert_performance fn ->
    heavy_operation()
  end, max_time: 1000
end
```

### 4. Error Testing Pattern

```elixir
test "error handling" do
  with_mock_server(error_rate: 0.5) do |client|
    result = call_tool(client, "tool", %{})
    # Should handle errors gracefully
    case result do
      {:ok, _} -> assert_valid_tool_result(result)
      {:error, _} -> assert_error(result)
    end
  end
end
```

## Test Configuration

### Test Tags

Use appropriate tags to categorize tests:

```elixir
@moduletag :unit          # Unit tests
@moduletag :integration   # Integration tests
@moduletag :performance   # Performance tests
@moduletag :stress        # Stress tests
@moduletag :property      # Property-based tests
```

### Running Specific Test Categories

```bash
# Run all tests
mix test

# Run only unit tests
mix test --include unit

# Run performance tests
mix test --include performance

# Run integration tests (may be slower)
mix test --include integration

# Exclude stress tests (default)
mix test --exclude stress
```

### Test Environment Configuration

```elixir
# config/test.exs
config :ex_mcp,
  test_timeout: 30_000,
  mock_latency: 0,
  performance_thresholds: %{
    content_creation: 100,      # microseconds
    serialization: 200,
    validation: 50
  }
```

## Best Practices

### 1. Test Structure

```elixir
defmodule MyFeatureTest do
  use ExMCP.TestCase, async: true
  
  describe "feature group" do
    setup do
      # Common setup for this group
      %{test_data: create_test_data()}
    end
    
    test "specific behavior", %{test_data: data} do
      # Test implementation
    end
  end
end
```

### 2. Data Generation

```elixir
# Use builders for consistent test data
content = text_content("Test message")
tool = tool("test_tool", schema: object_schema())
resource = resource("file://test.txt", "Test Resource")
```

### 3. Mock Server Configuration

```elixir
# Configure mock server for specific test needs
config = [
  tools: [my_tool()],
  resources: [my_resource()],
  latency: 10,           # Simulate network latency
  error_rate: 0.1,       # 10% error rate
  capabilities: %{...}   # Custom capabilities
]

with_mock_server(config) do |client|
  # Test implementation
end
```

### 4. Assertion Patterns

```elixir
# Use specific assertions for better error messages
assert_success(result)
assert_valid_tool_result(result)
assert_content_type(result, :text)
assert_content_contains(result, "expected text")

# Chain assertions for comprehensive validation
result
|> assert_success()
|> assert_valid_tool_result()
|> assert_content_type(:text)
```

### 5. Performance Testing

```elixir
# Measure and assert performance
{result, time_ms} = measure_time do
  expensive_operation()
end

assert time_ms < 1000, "Operation took #{time_ms}ms"
assert result == :expected
```

### 6. Error Testing

```elixir
# Test error conditions explicitly
assert_error(invalid_operation())

# Test error recovery
assert_eventually fn ->
  system_recovered?()
end, timeout: 5000
```

## Debugging Tests

### 1. Verbose Output

```elixir
# Enable detailed output in tests
IO.inspect(result, label: "Tool result")
IO.puts("Processing #{length(items)} items")
```

### 2. Test Context

```elixir
# Use test context for debugging
test "complex operation", %{test_id: id} do
  context = create_test_context(%{test_id: id})
  
  context = add_cleanup(context, fn ->
    IO.puts("Cleaning up test #{id}")
  end)
  
  # Test implementation
end
```

### 3. Conditional Testing

```elixir
# Skip expensive tests in development
@tag :slow
test "expensive operation" do
  # Only runs when explicitly included
end
```

## Common Patterns

### Testing Tool Implementations

```elixir
defmodule MyToolTest do
  use ExMCP.TestCase, async: true
  
  test "tool execution" do
    # Test with various input types
    inputs = [
      %{text: "simple input"},
      %{text: "complex input", options: %{format: "json"}},
      %{data: base64_data(), type: "binary"}
    ]
    
    Enum.each(inputs, fn input ->
      result = MyTool.execute(input)
      assert_success(result)
      assert_valid_tool_result(result)
    end)
  end
  
  test "tool validation" do
    definition = MyTool.definition()
    assert_valid_tool(definition)
    
    # Test schema validation
    valid_input = %{text: "valid"}
    invalid_input = %{invalid: "field"}
    
    assert MyTool.validate_input(valid_input) == :ok
    assert {:error, _} = MyTool.validate_input(invalid_input)
  end
end
```

### Testing Content Handling

```elixir
defmodule ContentHandlerTest do
  use ExMCP.TestCase, async: true
  
  test "handles all content types" do
    contents = [
      text_content("Test text"),
      image_content(),
      audio_content(),
      resource_content("file://test.txt"),
      annotation_content("test")
    ]
    
    Enum.each(contents, fn content ->
      result = ContentHandler.process(content)
      assert_success(result)
      assert_content_type(result, content.type)
    end)
  end
  
  test "content transformation" do
    input = text_content("Hello World", format: :plain)
    
    result = ContentHandler.transform(input, :markdown)
    
    assert_content_type(result, :text)
    assert result.format == :markdown
    assert_content_contains(result, "Hello World")
  end
end
```

### Testing Error Conditions

```elixir
defmodule ErrorHandlingTest do
  use ExMCP.TestCase, async: true
  
  test "handles network errors" do
    with_mock_server(error_rate: 1.0) do |client|
      result = call_tool(client, "tool", %{})
      assert_error(result)
      
      # Should contain error information
      assert_error(result, fn error ->
        assert Map.has_key?(error, "code")
        assert Map.has_key?(error, "message")
      end)
    end
  end
  
  test "timeout handling" do
    with_mock_server(latency: 5000) do |client|
      assert_raise(RuntimeError, ~r/timeout/i, fn ->
        with_timeout 1000 do
          call_tool(client, "slow_tool", %{})
        end
      end)
    end
  end
end
```

## Troubleshooting

### Common Issues

1. **Tests hanging**: Check for missing cleanup or infinite loops
2. **Assertion failures**: Use more specific assertions with better error messages
3. **Performance failures**: Adjust thresholds or optimize implementation
4. **Mock server issues**: Verify configuration and request format

### Debug Strategies

1. **Add logging**: Use `IO.puts` and `IO.inspect` for debugging
2. **Isolate failures**: Run single tests to identify specific issues
3. **Check test data**: Verify test data generation and validation
4. **Monitor resources**: Check memory and process usage during tests

For more detailed examples, see the individual test files in this directory.