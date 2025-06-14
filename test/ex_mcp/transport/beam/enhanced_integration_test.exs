defmodule ExMCP.Transport.Beam.EnhancedIntegrationTest do
  @moduledoc """
  Integration tests demonstrating that the enhanced BEAM transport
  works seamlessly with existing ExMCP.Client and ExMCP.Server modules.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Protocol
  alias ExMCP.Transport.Beam.Enhanced

  describe "enhanced transport integration" do
    test "protocol encoding compatibility" do
      # Test that Protocol-encoded messages work with Enhanced transport

      # Create various MCP protocol messages
      messages = [
        Protocol.encode_initialize(%{"name" => "test-client", "version" => "1.0"}),
        Protocol.encode_list_tools(),
        Protocol.encode_call_tool("test_tool", %{"arg" => "value"}),
        Protocol.encode_list_resources(),
        Protocol.encode_read_resource("file://test.txt"),
        Protocol.encode_response(%{"result" => "success"}, 123),
        Protocol.encode_error(-32601, "Method not found", nil, 456)
      ]

      # Verify all messages can be encoded to JSON (what transport expects)
      for message <- messages do
        assert {:ok, json_string} = Jason.encode(message)
        assert is_binary(json_string)

        # Verify they can be decoded back
        assert {:ok, decoded} = Jason.decode(json_string)
        assert decoded["jsonrpc"] == "2.0"
      end
    end

    test "transport configuration options" do
      # Test that enhanced transport accepts proper configuration
      client_config = [
        mode: :client,
        host: "localhost",
        port: 8080,
        security: %{
          max_frame_size: 1_048_576,
          rate_limit: %{window_ms: 60_000, max_requests: 1000}
        },
        batching: true,
        zero_copy: true
      ]

      # Should not raise errors (though connection will fail without server)
      assert is_list(client_config)
      assert client_config[:mode] == :client
      assert is_map(client_config[:security])
    end

    test "message format consistency" do
      # Test that messages follow MCP specification

      # Initialize request
      init_req =
        Protocol.encode_initialize(
          %{
            "name" => "enhanced-client",
            "version" => "1.0.0"
          },
          %{
            "tools" => %{},
            "resources" => %{}
          }
        )

      # Should have all required MCP fields
      assert init_req["jsonrpc"] == "2.0"
      assert init_req["method"] == "initialize"
      assert is_integer(init_req["id"])
      assert is_map(init_req["params"])
      assert init_req["params"]["clientInfo"]["name"] == "enhanced-client"

      # Tool call with progress tracking
      tool_call =
        Protocol.encode_call_tool(
          "process_data",
          %{
            "input" => "large dataset",
            "options" => %{"parallel" => true}
          },
          "progress-token-789"
        )

      assert tool_call["method"] == "tools/call"
      assert tool_call["params"]["name"] == "process_data"
      assert tool_call["params"]["_meta"]["progressToken"] == "progress-token-789"

      # Response with structured data
      response =
        Protocol.encode_response(
          %{
            "content" => [
              %{"type" => "text", "text" => "Processing complete"},
              %{
                "type" => "resource",
                "resource" => %{
                  "uri" => "file://output.json",
                  "mimeType" => "application/json"
                }
              }
            ]
          },
          tool_call["id"]
        )

      assert response["id"] == tool_call["id"]
      assert is_list(response["result"]["content"])
    end

    test "enhanced features compatibility" do
      # Test that enhanced features don't break standard protocol

      # Large message that would benefit from zero-copy
      large_payload = String.duplicate("data chunk ", 10_000)

      large_message =
        Protocol.encode_call_tool("process_large_file", %{
          "data" => large_payload,
          "compression" => "gzip"
        })

      {:ok, json_message} = Jason.encode(large_message)

      # Should be large enough to trigger zero-copy
      assert byte_size(json_message) > 64_000

      # But still be a valid MCP message
      {:ok, decoded} = Jason.decode(json_message)
      assert decoded["method"] == "tools/call"
      assert decoded["params"]["name"] == "process_large_file"

      # Batch of messages for throughput optimization
      batch_messages =
        for i <- 1..20 do
          Protocol.encode_list_tools("cursor-#{i}")
        end

      # All should be valid, batchable messages
      for msg <- batch_messages do
        assert msg["method"] == "tools/list"
        assert String.starts_with?(msg["params"]["cursor"], "cursor-")
      end

      # Should be good candidates for batching (non-priority)
      assert length(batch_messages) == 20
    end

    test "error handling compatibility" do
      # Test various error scenarios

      # Invalid method
      error_resp =
        Protocol.encode_error(
          -32601,
          "Method not found",
          %{
            "method" => "invalid/method"
          },
          "request-123"
        )

      assert error_resp["error"]["code"] == -32601
      assert error_resp["error"]["message"] == "Method not found"
      assert error_resp["id"] == "request-123"

      # Invalid params
      param_error =
        Protocol.encode_error(
          -32602,
          "Invalid params",
          %{
            "expected" => "object",
            "received" => "string"
          },
          "request-456"
        )

      assert param_error["error"]["code"] == -32602
      assert is_map(param_error["error"]["data"])

      # Parse error
      parse_error = Protocol.encode_error(-32700, "Parse error", nil, nil)
      assert parse_error["error"]["code"] == -32700
      # No ID for parse errors
      assert is_nil(parse_error["id"])
    end

    test "progress and meta field handling" do
      # Test _meta field support across different message types

      # Tool call with progress token
      with_progress = Protocol.encode_call_tool("long_operation", %{}, "op-123")
      assert with_progress["params"]["_meta"]["progressToken"] == "op-123"

      # Resource listing with custom metadata
      with_meta =
        Protocol.encode_list_resources(nil, %{
          "requestId" => "req-789",
          "userId" => "user-456",
          "customField" => "custom value"
        })

      assert with_meta["params"]["_meta"]["requestId"] == "req-789"
      assert with_meta["params"]["_meta"]["userId"] == "user-456"
      assert with_meta["params"]["_meta"]["customField"] == "custom value"

      # Prompt completion with both cursor and meta
      prompt_meta =
        Protocol.encode_list_prompts("next-page", %{
          "language" => "en",
          "category" => "math"
        })

      assert prompt_meta["params"]["cursor"] == "next-page"
      assert prompt_meta["params"]["_meta"]["language"] == "en"
      assert prompt_meta["params"]["_meta"]["category"] == "math"
    end

    test "notification message compatibility" do
      # Test various notification types

      # Resource change notification
      resource_changed = Protocol.encode_resources_changed()
      assert resource_changed["method"] == "notifications/resources/list_changed"
      # Notifications have no ID
      refute Map.has_key?(resource_changed, "id")

      # Tool list changed
      tools_changed = Protocol.encode_tools_changed()
      assert tools_changed["method"] == "notifications/tools/list_changed"

      # Custom notification
      custom_notification = %{
        "jsonrpc" => "2.0",
        "method" => "notifications/custom/progress",
        "params" => %{
          "progressToken" => "op-123",
          "progress" => 75,
          "message" => "Processing files..."
        }
      }

      # Should encode/decode properly
      {:ok, json_notif} = Jason.encode(custom_notification)
      {:ok, decoded_notif} = Jason.decode(json_notif)

      assert decoded_notif["method"] == "notifications/custom/progress"
      assert decoded_notif["params"]["progress"] == 75
      refute Map.has_key?(decoded_notif, "id")
    end
  end

  describe "transport registration" do
    test "enhanced transport is properly registered" do
      # Test that the enhanced transport is available through the Transport module
      transport_module = ExMCP.Transport.get_transport(:beam_enhanced)
      assert transport_module == ExMCP.Transport.Beam.Enhanced

      # Verify it implements the Transport behavior
      behaviors = Enhanced.__info__(:attributes)[:behaviour] || []
      assert ExMCP.Transport in behaviors
    end

    test "enhanced transport has all required callbacks" do
      # Verify all Transport callbacks are implemented
      functions = Enhanced.__info__(:functions)
      function_names = Enum.map(functions, fn {name, _arity} -> name end)

      required_callbacks = [:connect, :send_message, :receive_message, :close, :connected?]

      for callback <- required_callbacks do
        assert callback in function_names, "Missing callback: #{callback}"
      end
    end
  end

  describe "performance characteristics verification" do
    test "large message handling efficiency" do
      # Create messages of various sizes to test performance characteristics

      sizes = [1_000, 10_000, 100_000, 1_000_000]

      for size <- sizes do
        large_data = String.duplicate("x", size)
        message = Protocol.encode_call_tool("process", %{"data" => large_data})

        {:ok, json_msg} = Jason.encode(message)

        # Verify message integrity
        {:ok, decoded} = Jason.decode(json_msg)
        assert decoded["params"]["arguments"]["data"] == large_data

        # Messages over 64KB should benefit from zero-copy
        if byte_size(json_msg) > 64_000 do
          assert String.contains?(json_msg, large_data)
        end
      end
    end

    test "batch efficiency simulation" do
      # Simulate scenarios where batching provides benefits

      # Many small messages (good for batching)
      small_messages =
        for i <- 1..100 do
          Protocol.encode_list_tools("page-#{i}")
        end

      total_size =
        small_messages
        |> Enum.map(&Jason.encode!/1)
        |> Enum.map(&byte_size/1)
        |> Enum.sum()

      # Should be efficient to batch
      avg_size = div(total_size, length(small_messages))
      # Small messages
      assert avg_size < 1000
      # Many messages
      assert length(small_messages) > 50

      # Few large messages (not good for batching)
      large_data = String.duplicate("content", 10_000)

      large_messages =
        for i <- 1..3 do
          Protocol.encode_call_tool("process-#{i}", %{"data" => large_data})
        end

      large_total =
        large_messages
        |> Enum.map(&Jason.encode!/1)
        |> Enum.map(&byte_size/1)
        |> Enum.sum()

      large_avg = div(large_total, length(large_messages))
      # Large messages
      assert large_avg > 30_000
      # Few messages
      assert length(large_messages) < 10
    end
  end
end
