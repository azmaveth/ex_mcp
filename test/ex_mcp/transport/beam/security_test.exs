defmodule ExMCP.Transport.Beam.SecurityTest do
  @moduledoc """
  Tests for comprehensive security validation in BEAM transport.

  These tests verify that security measures properly protect against
  various attack vectors and abuse scenarios.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Transport.Beam.{Frame, Security}

  describe "frame validation" do
    setup do
      security_config = %{
        max_frame_size: 1024,
        max_batch_size: 10,
        authentication: %{required: false}
      }

      %{config: security_config}
    end

    test "validates normal frames", %{config: config} do
      message = %{"method" => "test", "params" => %{}}
      {:ok, frame_data} = Frame.encode(message, 1, :rpc_request)

      assert :ok = Security.validate_frame(frame_data, config)
    end

    test "rejects oversized frames", %{config: config} do
      # Create a large message that exceeds the limit
      large_data = String.duplicate("x", 2000)
      message = %{"method" => "test", "params" => %{"data" => large_data}}
      {:ok, frame_data} = Frame.encode(message, 1, :rpc_request)

      assert {:error, :frame_too_large} = Security.validate_frame(frame_data, config)
    end

    test "rejects malformed frames", %{config: config} do
      # Invalid frame structure
      bad_frame = <<1, 2, 3, 4>>

      assert {:error, :invalid_frame_structure} = Security.validate_frame(bad_frame, config)
    end

    test "rejects frames with length mismatches", %{config: config} do
      # Create frame with incorrect length field
      message = %{"method" => "test"}
      {:ok, frame_data} = Frame.encode(message, 1, :rpc_request)

      # Corrupt the length field
      <<_length::32, rest::binary>> = frame_data
      corrupted_frame = <<999::32, rest::binary>>

      assert {:error, :frame_length_mismatch} = Security.validate_frame(corrupted_frame, config)
    end

    test "validates batch frames within limits", %{config: config} do
      messages =
        for i <- 1..5 do
          %{"method" => "test", "params" => %{"index" => i}}
        end

      batch = %{"type" => "batch", "messages" => messages, "count" => 5}
      {:ok, frame_data} = Frame.encode(batch, 1, :batch)

      assert :ok = Security.validate_frame(frame_data, config)
    end

    test "rejects oversized batches", %{config: config} do
      # Create batch exceeding max_batch_size
      messages =
        for i <- 1..15 do
          %{"method" => "test", "params" => %{"index" => i}}
        end

      batch = %{"type" => "batch", "messages" => messages, "count" => 15}
      {:ok, frame_data} = Frame.encode(batch, 1, :batch)

      assert {:error, :batch_too_large} = Security.validate_frame(frame_data, config)
    end
  end

  describe "connection validation" do
    setup do
      opts = [
        max_connections: 100,
        auth_required: false,
        rate_limit_window: 1000,
        rate_limit_requests: 10
      ]

      {:ok, security} = start_supervised({Security, opts})

      config = %{
        max_connections: 100,
        authentication: %{required: false},
        rate_limit: %{window_ms: 1000, max_requests: 10}
      }

      %{security: security, config: config}
    end

    test "allows valid connections", %{config: config} do
      connection_info = %{
        id: "conn_1",
        remote_ip: "192.168.1.100",
        user_agent: "ExMCP Client/1.0"
      }

      assert :ok = Security.validate_connection(connection_info, config)
    end

    test "rejects connections without authentication when required" do
      config = %{
        authentication: %{required: true, methods: [:bearer_token]}
      }

      connection_info = %{id: "conn_1", remote_ip: "192.168.1.100"}

      assert {:error, :authentication_required} =
               Security.validate_connection(connection_info, config)
    end

    test "validates bearer token authentication" do
      config = %{
        authentication: %{required: true, methods: [:bearer_token]}
      }

      # Valid token
      connection_info = %{
        id: "conn_1",
        credentials: %{bearer_token: "valid_token_with_sufficient_length"}
      }

      assert :ok = Security.validate_connection(connection_info, config)

      # Invalid token (too short)
      connection_info_bad = %{
        id: "conn_2",
        credentials: %{bearer_token: "short"}
      }

      assert {:error, :invalid_token} =
               Security.validate_connection(connection_info_bad, config)
    end
  end

  describe "rate limiting" do
    setup do
      # Very restrictive for testing
      opts = [rate_limit_window: 100, rate_limit_requests: 3]
      {:ok, security} = start_supervised({Security, opts})

      config = %{
        rate_limit: %{window_ms: 100, max_requests: 3}
      }

      %{security: security, config: config}
    end

    test "allows requests within limit", %{config: config} do
      connection_id = "rate_test_1"

      # Should allow first 3 requests
      assert :ok = Security.check_rate_limit(connection_id, config)
      assert :ok = Security.check_rate_limit(connection_id, config)
      assert :ok = Security.check_rate_limit(connection_id, config)
    end

    test "rejects requests exceeding limit", %{config: config} do
      connection_id = "rate_test_2"

      # Fill up the rate limit
      assert :ok = Security.check_rate_limit(connection_id, config)
      assert :ok = Security.check_rate_limit(connection_id, config)
      assert :ok = Security.check_rate_limit(connection_id, config)

      # Fourth request should be rejected
      assert {:error, :rate_limit_exceeded} =
               Security.check_rate_limit(connection_id, config)
    end

    test "resets limit after window expires", %{config: config} do
      connection_id = "rate_test_3"

      # Fill up the rate limit
      assert :ok = Security.check_rate_limit(connection_id, config)
      assert :ok = Security.check_rate_limit(connection_id, config)
      assert :ok = Security.check_rate_limit(connection_id, config)

      # Wait for window to reset
      :timer.sleep(150)

      # Should allow new requests
      assert :ok = Security.check_rate_limit(connection_id, config)
    end
  end

  describe "message content validation" do
    setup do
      config = %{
        max_frame_size: 10_000,
        max_batch_size: 100
      }

      %{config: config}
    end

    test "validates normal message structure", %{config: config} do
      # Valid RPC request
      message = %{
        "id" => 1,
        "method" => "test",
        "params" => %{"arg" => "value"}
      }

      assert :ok = Security.validate_message_content(message, config)

      # Valid notification
      notification = %{
        "method" => "notify",
        "params" => %{"event" => "update"}
      }

      assert :ok = Security.validate_message_content(notification, config)
    end

    test "rejects malicious content", %{config: config} do
      # Script injection
      malicious_message = %{
        "method" => "test",
        "params" => %{"data" => "<script>alert('xss')</script>"}
      }

      assert {:error, {:malicious_content, :script_injection}} =
               Security.validate_message_content(malicious_message, config)

      # Path traversal
      path_traversal_message = %{
        "method" => "read_file",
        "params" => %{"path" => "../../../etc/passwd"}
      }

      assert {:error, {:malicious_content, :path_traversal}} =
               Security.validate_message_content(path_traversal_message, config)
    end

    test "rejects oversized messages", %{config: config} do
      # Create message that's too large
      large_data = String.duplicate("x", 20_000)

      oversized_message = %{
        "method" => "test",
        "params" => %{"data" => large_data}
      }

      assert {:error, :message_too_large} =
               Security.validate_message_content(oversized_message, config)
    end

    test "rejects deeply nested messages", %{config: config} do
      # Create deeply nested structure
      deeply_nested =
        Enum.reduce(1..15, %{"base" => "value"}, fn _i, acc ->
          %{"nested" => acc}
        end)

      assert {:error, :message_too_deep} =
               Security.validate_message_content(deeply_nested, config)
    end

    test "validates message structure requirements", %{config: config} do
      # Missing method field
      invalid_request = %{
        "id" => 1,
        "params" => %{}
      }

      assert {:error, :invalid_message_structure} =
               Security.validate_message_content(invalid_request, config)

      # Invalid method type
      invalid_method = %{
        # Should be string
        "method" => 123,
        "params" => %{}
      }

      assert {:error, :invalid_method_type} =
               Security.validate_message_content(invalid_method, config)
    end
  end

  describe "security statistics" do
    setup do
      {:ok, security} = start_supervised({Security, []})
      %{security: security}
    end

    test "tracks validation statistics", %{security: _security} do
      initial_stats = Security.get_security_stats()

      # Perform some validations
      config = %{max_frame_size: 1024}
      message = %{"method" => "test"}
      {:ok, frame_data} = Frame.encode(message, 1, :rpc_request)

      Security.validate_frame(frame_data, config)
      # Invalid frame
      Security.validate_frame(<<1, 2, 3>>, config)

      # Wait for async cast messages to be processed
      :timer.sleep(10)

      final_stats = Security.get_security_stats()

      assert final_stats.frames_validated > initial_stats.frames_validated
      assert final_stats.frames_rejected > initial_stats.frames_rejected
    end

    test "records security events", %{security: _security} do
      event = %{
        type: :test_event,
        severity: :medium,
        connection_id: "test_conn",
        details: %{reason: "test"},
        timestamp: System.system_time(:millisecond)
      }

      Security.record_security_event(event)

      # Wait for event to be processed
      :timer.sleep(10)

      stats = Security.get_security_stats()
      assert stats.recent_security_events > 0
    end
  end

  describe "anomaly detection" do
    test "detects suspicious user agents" do
      config = %{
        anomaly_detection: %{enabled: true, threshold: 1}
      }

      # Suspicious connection
      suspicious_connection = %{
        id: "suspicious_1",
        user_agent: "malicious_bot_scanner",
        remote_ip: "192.168.1.100"
      }

      assert {:error, :suspicious_activity} =
               Security.validate_connection(suspicious_connection, config)

      # Normal connection
      normal_connection = %{
        id: "normal_1",
        user_agent: "ExMCP Client/1.0",
        remote_ip: "192.168.1.100"
      }

      assert :ok = Security.validate_connection(normal_connection, config)
    end

    test "detects invalid IP addresses" do
      config = %{
        anomaly_detection: %{enabled: true}
      }

      # Invalid IP format
      invalid_ip_connection = %{
        id: "invalid_ip",
        remote_ip: "not.an.ip.address",
        user_agent: "Valid Client"
      }

      assert {:error, :suspicious_activity} =
               Security.validate_connection(invalid_ip_connection, config)
    end
  end

  describe "configuration updates" do
    setup do
      {:ok, security} = start_supervised({Security, []})
      %{security: security}
    end

    test "allows configuration updates", %{security: _security} do
      new_config = %{
        max_frame_size: 2_048_576,
        rate_limit: %{window_ms: 30_000, max_requests: 2000}
      }

      assert :ok = Security.update_config(new_config)
    end
  end

  describe "error handling" do
    test "handles invalid inputs gracefully" do
      config = %{max_frame_size: 1024}

      # Invalid frame data
      assert {:error, :invalid_input} = Security.validate_frame(nil, config)
      assert {:error, :invalid_input} = Security.validate_frame("not binary", config)

      # Invalid connection info
      assert {:error, :invalid_input} = Security.validate_connection("not map", config)

      # Invalid message content
      assert {:error, :invalid_input} = Security.validate_message_content("not map", config)
    end

    test "handles edge cases in message validation" do
      config = %{max_frame_size: 1024}

      # Empty message
      empty_message = %{}

      assert {:error, :invalid_message_structure} =
               Security.validate_message_content(empty_message, config)

      # Message with invalid structure
      weird_message = %{"random" => "fields", "no" => "structure"}

      assert {:error, :invalid_message_structure} =
               Security.validate_message_content(weird_message, config)
    end
  end

  describe "integration with existing components" do
    test "works with Frame module encoding" do
      config = %{max_frame_size: 1024, max_batch_size: 10}

      # Test with various message types
      messages = [
        %{"method" => "ping"},
        %{"id" => 1, "method" => "test", "params" => %{}},
        %{"id" => 1, "result" => "success"},
        %{"method" => "notify", "params" => %{"event" => "test"}}
      ]

      for message <- messages do
        {:ok, frame_data} = Frame.encode(message, 1, :rpc_request)
        assert :ok = Security.validate_frame(frame_data, config)
      end
    end

    test "handles complex nested messages" do
      config = %{max_frame_size: 10_000, max_batch_size: 100}

      complex_message = %{
        "method" => "complex_operation",
        "params" => %{
          "config" => %{
            "nested" => %{
              "deep" => %{
                "values" => [1, 2, 3, %{"key" => "value"}]
              }
            }
          },
          "data" => "some_data_payload"
        }
      }

      assert :ok = Security.validate_message_content(complex_message, config)
    end
  end
end
