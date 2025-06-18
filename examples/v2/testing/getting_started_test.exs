defmodule ExMCP.Examples.GettingStartedTest do
  @moduledoc """
  Getting started examples for the ExMCP v2 testing framework.
  
  This file demonstrates the basic patterns and utilities available
  in the ExMCP testing framework for writing comprehensive tests
  for MCP (Model Context Protocol) applications.
  """
  
  use ExMCP.TestCase, async: true
  
  alias ExMCP.Content.Protocol
  alias ExMCP.Testing.{Builders, MockServer}
  
  describe "basic content testing" do
    test "creating and validating text content" do
      # Create text content using the Protocol module
      content = Protocol.text("Hello, MCP World!")
      
      # Validate the content structure
      assert_valid_content(content)
      assert_content_type(content, :text)
      assert_content_contains(content, "MCP World")
      
      # Test serialization and deserialization
      serialized = Protocol.serialize(content)
      {:ok, deserialized} = Protocol.deserialize(serialized)
      
      assert deserialized.text == content.text
      assert deserialized.type == content.type
    end
    
    test "creating content with builders" do
      # Use builders for convenient test data creation
      text_content = Builders.text_content("Builder-generated text")
      image_content = Builders.image_content()
      audio_content = Builders.audio_content()
      
      # All builder-generated content should be valid
      assert_valid_content(text_content)
      assert_valid_content(image_content)
      assert_valid_content(audio_content)
      
      # Check content types
      assert_content_type(text_content, :text)
      assert_content_type(image_content, :image)
      assert_content_type(audio_content, :audio)
    end
    
    test "random content generation" do
      # Generate random content for property-based testing
      random_text = Builders.text_content(nil, random: true, size: 100)
      random_image = Builders.image_content(random: true, size: 1024)
      
      assert_valid_content(random_text)
      assert_valid_content(random_image)
      
      # Random content should be different each time
      another_text = Builders.text_content(nil, random: true, size: 100)
      assert random_text.text != another_text.text
    end
  end
  
  describe "tool testing with mock server" do
    test "basic tool discovery and execution" do
      # Create a mock tool for testing
      test_tool = MockServer.sample_tool(
        name: "echo_tool",
        description: "Echoes the input text"
      )
      
      # Start mock server with the tool
      MockServer.with_server([tools: [test_tool]]) do |client|
        # In a real test, you would use the client to make actual calls
        # For this example, we'll demonstrate the testing pattern
        
        # 1. Verify tool is available (simulated)
        available_tools = [test_tool]  # Would be: list_tools(client)
        assert_has_tool(available_tools, "echo_tool")
        
        # 2. Validate tool definition
        echo_tool = Enum.find(available_tools, &(&1["name"] == "echo_tool"))
        assert_valid_tool(echo_tool)
        
        # 3. Test tool execution (simulated)
        # result = call_tool(client, "echo_tool", %{input: "test message"})
        # assert_success(result)
        # assert_content_contains(result, "test message")
        
        # For this example, we'll just verify the test pattern works
        assert client != nil
      end
    end
    
    test "testing tool with different input types" do
      tools = [
        MockServer.sample_tool(name: "text_processor"),
        MockServer.sample_tool(name: "image_analyzer"),
        MockServer.sample_tool(name: "data_converter")
      ]
      
      MockServer.with_server([tools: tools]) do |client|
        # Test with text content
        text_input = Builders.text_content("Process this text")
        test_tool_with_content(client, "text_processor", text_input)
        
        # Test with image content
        image_input = Builders.image_content()
        test_tool_with_content(client, "image_analyzer", image_input)
        
        # Test with structured data
        json_input = %{
          "operation" => "convert",
          "format" => "json",
          "data" => [1, 2, 3, 4, 5]
        }
        test_tool_with_structured_data(client, "data_converter", json_input)
      end
    end
    
    test "error handling and recovery" do
      # Test with a server that sometimes returns errors
      MockServer.with_server([error_rate: 0.3]) do |client|
        # Make multiple requests and handle both success and error cases
        results = Enum.map(1..10, fn i ->
          # Simulate tool call that might fail
          if rem(i, 3) == 0 do
            {:error, "Simulated error"}
          else
            {:ok, Builders.tool_result("Success #{i}")}
          end
        end)
        
        # Verify we get both successes and errors
        successes = Enum.count(results, &match?({:ok, _}, &1))
        errors = Enum.count(results, &match?({:error, _}, &1))
        
        assert successes > 0, "Should have some successful requests"
        assert errors > 0, "Should have some failed requests"
        
        # Test error assertions
        error_result = {:error, "Test error"}
        assert_error(error_result)
        
        # Test success assertions
        success_result = {:ok, Builders.tool_result("Success")}
        assert_success(success_result)
      end
    end
  end
  
  describe "resource testing" do
    test "resource discovery and access" do
      resources = [
        MockServer.sample_resource(
          uri: "file://config.json",
          name: "Configuration File"
        ),
        MockServer.sample_resource(
          uri: "file://data.csv",
          name: "Dataset"
        )
      ]
      
      MockServer.with_server([resources: resources]) do |client|
        # Test resource discovery (simulated)
        available_resources = resources  # Would be: list_resources(client)
        
        assert_has_resource(available_resources, "file://config.json")
        assert_has_resource(available_resources, "file://data.csv")
        
        # Validate resource definitions
        Enum.each(available_resources, &assert_valid_resource/1)
        
        # Test resource access (simulated)
        # config_content = read_resource(client, "file://config.json")
        # assert_success(config_content)
        
        assert client != nil
      end)
    end
  end
  
  describe "performance testing" do
    test "measuring operation performance" do
      # Test that content creation is fast
      assert_performance(fn ->
        Protocol.text("Performance test content")
      end, max_time: 100)  # Should complete in under 100ms
      
      # Test batch operations
      assert_performance(fn ->
        Enum.each(1..100, fn i ->
          Protocol.text("Batch content #{i}")
        end)
      end, max_time: 1000)  # 100 operations in under 1 second
    end
    
    test "measuring response time distribution" do
      # Measure multiple operations to check consistency
      times = Enum.map(1..50, fn _ ->
        {_result, time_ms} = measure_time do
          Builders.text_content("Timing test")
        end
        time_ms
      end)
      
      avg_time = Enum.sum(times) / length(times)
      max_time = Enum.max(times)
      
      assert avg_time < 10, "Average time #{avg_time}ms too high"
      assert max_time < 50, "Max time #{max_time}ms too high"
    end
    
    test "concurrent operations performance" do
      # Test that operations can run concurrently
      start_time = System.monotonic_time(:millisecond)
      
      # Run operations in parallel
      tasks = Enum.map(1..10, fn i ->
        Task.async(fn ->
          Process.sleep(10)  # Simulate work
          Builders.text_content("Concurrent test #{i}")
        end)
      end)
      
      results = Task.await_many(tasks, 5000)
      end_time = System.monotonic_time(:millisecond)
      
      elapsed = end_time - start_time
      
      # Should complete much faster than sequential (10 * 10ms = 100ms)
      assert elapsed < 50, "Concurrent operations took #{elapsed}ms"
      assert length(results) == 10
    end
  end
  
  describe "advanced testing patterns" do
    test "property-based testing with generated data" do
      # Test that all generated content is valid
      Enum.each(1..50, fn _i ->
        # Generate random content
        content = case :rand.uniform(4) do
          1 -> Builders.text_content(nil, random: true)
          2 -> Builders.image_content(random: true)
          3 -> Builders.audio_content(random: true)
          4 -> Builders.resource_content(nil, random: true)
        end
        
        # All generated content should be valid
        assert_valid_content(content)
        
        # Serialization roundtrip should preserve type
        serialized = Protocol.serialize(content)
        {:ok, deserialized} = Protocol.deserialize(serialized)
        assert content.type == deserialized.type
      end)
    end
    
    test "testing with temporary files" do
      with_temp_file("Test file content", ".txt") do
        # File should exist during the test
        assert File.exists?(file_path)
        assert File.read!(file_path) == "Test file content"
        assert String.ends_with?(file_path, ".txt")
        
        # Can use the file for testing
        content = Protocol.resource("file://" <> file_path, 
          text: File.read!(file_path),
          mime_type: "text/plain"
        )
        
        assert_valid_content(content)
        assert content.resource.uri == "file://" <> file_path
      end
      # File is automatically cleaned up after the block
    end
    
    test "testing with timeout handling" do
      # Test that operations complete within expected time
      with_timeout 1000 do
        # Fast operation should complete
        Builders.text_content("Quick operation")
      end
      
      # Test timeout failure
      assert_raise ExUnit.AssertionError, ~r/timed out/, fn ->
        with_timeout 50 do
          Process.sleep(100)  # This should timeout
        end
      end
    end
    
    test "testing eventual consistency" do
      # Simulate a system that becomes ready over time
      ready_time = System.monotonic_time(:millisecond) + 100
      
      assert_eventually(fn ->
        System.monotonic_time(:millisecond) >= ready_time
      end, timeout: 200)
    end
    
    test "flakiness detection with repeated tests" do
      # Run a test multiple times to detect flakiness
      repeat_test 5 do
        # This test should pass consistently
        content = Protocol.text("Stability test")
        assert_valid_content(content)
        assert content.text == "Stability test"
      end
    end
  end
  
  # Helper functions for examples
  
  defp test_tool_with_content(_client, tool_name, content) do
    # In a real implementation, this would:
    # 1. Serialize the content
    # 2. Call the tool with the content
    # 3. Validate the result
    
    serialized = Protocol.serialize(content)
    
    # Simulate tool call
    simulated_result = %{
      "content" => [serialized],
      "tool" => tool_name,
      "status" => "success"
    }
    
    # Validate the pattern works
    assert is_map(simulated_result)
    assert simulated_result["tool"] == tool_name
  end
  
  defp test_tool_with_structured_data(_client, tool_name, data) do
    # Simulate structured data processing
    assert is_map(data)
    assert is_binary(tool_name)
    
    # In real implementation, would make actual tool call
    # result = call_tool(client, tool_name, data)
    # assert_success(result)
  end
end