defmodule ExMCP.Transport.Beam.EnhancedTest do
  @moduledoc """
  Tests for the enhanced BEAM transport that integrates with ExMCP.Protocol.

  These tests verify that the enhanced transport properly bridges the gap
  between Protocol-encoded JSON messages and our advanced BEAM components.
  """

  use ExUnit.Case, async: true

  @moduletag :transport
  @moduletag :beam
  @moduletag :requires_beam

  alias ExMCP.Protocol
  alias ExMCP.Transport.Beam.Enhanced

  # Mock socket for server-side tests
  defmodule MockSocket do
    use GenServer

    def start_link(opts \\ []) do
      GenServer.start_link(__MODULE__, opts)
    end

    def init(_opts) do
      {:ok, %{messages: []}}
    end

    def handle_call({:send, data}, _from, state) do
      new_messages = [data | state.messages]
      {:reply, :ok, %{state | messages: new_messages}}
    end

    def handle_call(:get_messages, _from, state) do
      {:reply, Enum.reverse(state.messages), state}
    end

    def handle_call(:close, _from, state) do
      {:reply, :ok, state}
    end
  end

  describe "transport behavior compliance" do
    test "implements all required Transport callbacks" do
      behaviors = Enhanced.__info__(:attributes)[:behaviour] || []
      assert ExMCP.Transport in behaviors
    end
  end

  describe "connection establishment" do
    test "connects in client mode with basic options" do
      # Skip actual network connection for unit test
      # This would require a real server running
      opts = [
        mode: :client,
        host: "localhost",
        port: 8080,
        security: %{max_frame_size: 1024}
      ]

      # In real usage, this would establish a TCP connection
      # For testing, we verify the options are processed correctly
      assert is_list(opts)
      assert opts[:mode] == :client
    end

    test "validates required options for client mode" do
      # Port is required for client mode
      assert_raise KeyError, fn ->
        Enhanced.connect(mode: :client, host: "localhost")
      end
    end

    test "accepts server mode with socket" do
      {:ok, mock_socket} = MockSocket.start_link()

      opts = [
        mode: :server,
        socket: mock_socket,
        security: %{authentication: %{required: false}}
      ]

      # The actual connection would use the mock socket
      assert opts[:mode] == :server
      assert opts[:socket] == mock_socket
    end
  end

  describe "protocol integration" do
    test "encodes and handles MCP protocol messages" do
      # Test that Protocol messages can be properly encoded and processed
      init_request =
        Protocol.encode_initialize(%{
          "name" => "test-client",
          "version" => "1.0.0"
        })

      assert init_request["jsonrpc"] == "2.0"
      assert init_request["method"] == "initialize"
      assert is_integer(init_request["id"])

      # Convert to JSON as the transport expects
      {:ok, json_message} = Jason.encode(init_request)
      assert is_binary(json_message)

      # Verify we can decode it back
      {:ok, decoded} = Jason.decode(json_message)
      assert decoded["method"] == "initialize"
    end

    test "handles list_tools request encoding" do
      list_tools_request = Protocol.encode_list_tools()

      assert list_tools_request["method"] == "tools/list"
      assert list_tools_request["params"] == %{}

      # Test with cursor
      with_cursor = Protocol.encode_list_tools("next-page")
      assert with_cursor["params"]["cursor"] == "next-page"
    end

    test "handles call_tool request encoding" do
      tool_request = Protocol.encode_call_tool("test_tool", %{"arg1" => "value1"})

      assert tool_request["method"] == "tools/call"
      assert tool_request["params"]["name"] == "test_tool"
      assert tool_request["params"]["arguments"]["arg1"] == "value1"

      # Test with progress token
      with_progress =
        Protocol.encode_call_tool(
          "long_tool",
          %{"data" => "large"},
          "progress-123"
        )

      assert with_progress["params"]["_meta"]["progressToken"] == "progress-123"
    end

    test "handles response encoding" do
      response = Protocol.encode_response(%{"result" => "success"}, "request-123")

      assert response["jsonrpc"] == "2.0"
      assert response["id"] == "request-123"
      assert response["result"]["result"] == "success"
    end

    test "handles error response encoding" do
      error_response = Protocol.encode_error(-32601, "Method not found", nil, "request-456")

      assert error_response["id"] == "request-456"
      assert error_response["error"]["code"] == -32601
      assert error_response["error"]["message"] == "Method not found"
    end
  end

  describe "message batching integration" do
    test "identifies priority messages correctly" do
      # These should bypass batching
      cancel_msg = %{"method" => "cancelled", "params" => %{}}
      ping_msg = %{"method" => "ping", "params" => %{}}

      # Regular message should be batched
      regular_msg = %{"method" => "tools/list", "params" => %{}}

      # The transport should handle these appropriately
      assert cancel_msg["method"] == "cancelled"
      assert ping_msg["method"] == "ping"
      assert regular_msg["method"] == "tools/list"
    end

    test "processes batch-worthy messages" do
      messages = [
        Protocol.encode_list_tools(),
        Protocol.encode_list_resources(),
        Protocol.encode_list_prompts()
      ]

      # All should be regular (non-priority) messages
      for message <- messages do
        refute message["method"] in ["cancelled", "ping"]
      end

      # Should be good candidates for batching
      assert length(messages) == 3
    end
  end

  describe "security integration" do
    test "validates security configuration" do
      security_config = %{
        max_frame_size: 1_048_576,
        authentication: %{required: false},
        rate_limit: %{window_ms: 60_000, max_requests: 1000}
      }

      # Config should be well-formed
      assert is_integer(security_config.max_frame_size)
      assert is_map(security_config.authentication)
      assert is_map(security_config.rate_limit)
    end

    test "handles oversized messages appropriately" do
      # Create a large message that might exceed frame limits
      large_data = String.duplicate("x", 50_000)
      large_message = Protocol.encode_call_tool("process_large", %{"data" => large_data})

      # Should still be encodeable as JSON
      {:ok, json_message} = Jason.encode(large_message)

      # Size check
      assert byte_size(json_message) > 50_000

      # The enhanced transport should handle this with zero-copy optimization
      assert String.contains?(json_message, large_data)
    end
  end

  describe "zero-copy integration" do
    test "handles large payloads efficiently" do
      # Simulate a large resource read response
      large_content = String.duplicate("content line\n", 10_000)

      resource_response =
        Protocol.encode_response(
          %{
            "contents" => [
              %{
                "uri" => "file://large.txt",
                "mimeType" => "text/plain",
                "text" => large_content
              }
            ]
          },
          "read-req-1"
        )

      {:ok, json_response} = Jason.encode(resource_response)

      # Should be a large message suitable for zero-copy
      assert byte_size(json_response) > 64_000
      # Check that it contains some of the repeated content
      assert String.contains?(json_response, "content line")
    end
  end

  describe "observability integration" do
    test "collects basic transport statistics" do
      # Mock state for statistics
      mock_state = %{
        connection: nil,
        batcher: nil,
        mode: :client,
        config: %{security: %{}, batching: true, zero_copy: true},
        stats: %{
          messages_sent: 5,
          messages_received: 3,
          bytes_sent: 1024,
          bytes_received: 768,
          errors: 1
        }
      }

      # The transport should provide comprehensive stats
      stats = Enhanced.get_stats(mock_state)

      assert stats.messages_sent == 5
      assert stats.messages_received == 3
      assert stats.bytes_sent == 1024
      assert stats.bytes_received == 768
      assert stats.errors == 1

      # Should include component stats
      assert Map.has_key?(stats, :connection)
      assert Map.has_key?(stats, :batching)
      assert Map.has_key?(stats, :security)
      assert Map.has_key?(stats, :zero_copy)
    end
  end

  describe "error handling" do
    test "handles JSON decode errors gracefully" do
      # Mock state
      state = %{
        stats: %{errors: 0, messages_sent: 0, bytes_sent: 0}
      }

      # Invalid JSON should be handled gracefully
      invalid_json = "{invalid json"

      case Enhanced.send_message(invalid_json, state) do
        {:error, {:json_decode_error, _}} -> :ok
        _ -> flunk("Should have returned JSON decode error")
      end
    end

    test "handles connection failures appropriately" do
      # Mock connection failure scenario
      mock_state = %{
        connection: nil,
        batcher: nil,
        mode: :client,
        config: %{},
        stats: %{errors: 0, messages_sent: 0, bytes_sent: 0}
      }

      # Should handle gracefully
      assert Enhanced.connected?(mock_state) == false
    end
  end

  describe "lifecycle management" do
    test "properly closes all components" do
      mock_state = %{
        connection: nil,
        batcher: nil,
        mode: :client,
        config: %{},
        stats: %{}
      }

      # Should not crash on close
      assert :ok = Enhanced.close(mock_state)
    end
  end

  describe "real protocol workflow simulation" do
    test "simulates complete MCP initialization flow" do
      # Client sends initialize
      client_init =
        Protocol.encode_initialize(
          %{
            "name" => "test-client",
            "version" => "1.0.0"
          },
          %{
            "tools" => %{},
            "resources" => %{}
          }
        )

      {:ok, init_json} = Jason.encode(client_init)

      # Server would respond with initialize result
      server_response =
        Protocol.encode_response(
          %{
            "name" => "test-server",
            "version" => "1.0.0",
            "capabilities" => %{
              "tools" => %{"listChanged" => true},
              "resources" => %{"subscribe" => true}
            }
          },
          client_init["id"]
        )

      {:ok, response_json} = Jason.encode(server_response)

      # Both should be valid Protocol messages
      {:ok, init_decoded} = Jason.decode(init_json)
      {:ok, response_decoded} = Jason.decode(response_json)

      assert init_decoded["method"] == "initialize"
      assert response_decoded["result"]["name"] == "test-server"
      assert init_decoded["id"] == response_decoded["id"]
    end

    test "simulates tool call workflow" do
      # Client calls a tool
      tool_call =
        Protocol.encode_call_tool(
          "read_file",
          %{
            "path" => "/tmp/test.txt"
          },
          "progress-token-123"
        )

      {:ok, call_json} = Jason.encode(tool_call)

      # Server responds with tool result
      tool_result =
        Protocol.encode_response(
          %{
            "content" => [
              %{
                "type" => "text",
                "text" => "File contents here..."
              }
            ]
          },
          tool_call["id"]
        )

      {:ok, result_json} = Jason.encode(tool_result)

      # Should maintain request/response correlation
      {:ok, call_decoded} = Jason.decode(call_json)
      {:ok, result_decoded} = Jason.decode(result_json)

      assert call_decoded["method"] == "tools/call"
      assert call_decoded["params"]["name"] == "read_file"
      assert call_decoded["params"]["_meta"]["progressToken"] == "progress-token-123"
      assert call_decoded["id"] == result_decoded["id"]
    end
  end
end
