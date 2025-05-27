defmodule ExMCP.Transport.NotificationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Protocol
  alias ExMCP.Transport.Beam

  describe "notification handling" do
    @notification %{
      "jsonrpc" => "2.0",
      "method" => "notifications/initialized",
      "params" => %{}
    }

    test "notifications have no ID field" do
      notification = Protocol.encode_notification("test/method", %{"foo" => "bar"})

      assert notification["jsonrpc"] == "2.0"
      assert notification["method"] == "test/method"
      assert notification["params"] == %{"foo" => "bar"}
      refute Map.has_key?(notification, "id")
    end

    test "parse_message recognizes notifications" do
      json = Jason.encode!(@notification)

      assert {:notification, "notifications/initialized", %{}} = Protocol.parse_message(json)
    end

    test "BEAM transport handles JSON encoding/decoding for notifications" do
      # Test that the BEAM transport properly encodes/decodes JSON
      # when sending notifications

      # Create a mock server process
      server_pid =
        spawn(fn ->
          receive do
            {:beam_connect, client_mailbox} ->
              send(client_mailbox, {:mcp_connected, self()})

              # Wait for notification
              receive do
                {:mcp_message, message} ->
                  # Echo back the message
                  send(client_mailbox, {:mcp_message, message})
              end

            {:beam_connect, client_mailbox, _auth_info} ->
              # Handle new format with auth info
              send(client_mailbox, {:mcp_connected, self()})

              # Wait for notification
              receive do
                {:mcp_message, message} ->
                  # Echo back the message
                  send(client_mailbox, {:mcp_message, message})
              end
          end
        end)

      # Connect to server
      {:ok, state} = Beam.connect(server: server_pid)

      # Send notification as JSON string
      notification_json = Jason.encode!(@notification)
      {:ok, _state} = Beam.send_message(notification_json, state)

      # Receive echoed message
      {:ok, response_json, _state} = Beam.receive_message(state)

      # Parse response
      {:ok, response} = Jason.decode(response_json)

      # Verify the notification was properly handled
      assert response["jsonrpc"] == "2.0"
      assert response["method"] == "notifications/initialized"
      assert response["params"] == %{}
      refute Map.has_key?(response, "id")
    end

    test "BEAM transport handles map notifications directly" do
      # Test that the BEAM transport can also handle maps directly

      server_pid =
        spawn(fn ->
          receive do
            {:beam_connect, client_mailbox} ->
              send(client_mailbox, {:mcp_connected, self()})

              receive do
                {:mcp_message, message} ->
                  # Verify we received a map
                  assert is_map(message)
                  assert message["method"] == "notifications/initialized"

                  # Send back a notification
                  notification = %{
                    "jsonrpc" => "2.0",
                    "method" => "notifications/resources/list_changed",
                    "params" => %{}
                  }

                  send(client_mailbox, {:mcp_message, notification})
              end

            {:beam_connect, client_mailbox, _auth_info} ->
              # Handle new format with auth info
              send(client_mailbox, {:mcp_connected, self()})

              receive do
                {:mcp_message, message} ->
                  # Verify we received a map
                  assert is_map(message)
                  assert message["method"] == "notifications/initialized"

                  # Send back a notification
                  notification = %{
                    "jsonrpc" => "2.0",
                    "method" => "notifications/resources/list_changed",
                    "params" => %{}
                  }

                  send(client_mailbox, {:mcp_message, notification})
              end
          end
        end)

      {:ok, state} = Beam.connect(server: server_pid, format: :native)

      # Send notification as map
      {:ok, _state} = Beam.send_message(@notification, state)

      # Receive response
      {:ok, response, _state} = Beam.receive_message(state)

      assert response["method"] == "notifications/resources/list_changed"
      refute Map.has_key?(response, "id")
    end
  end

  describe "bidirectional messaging" do
    test "BEAM transport supports bidirectional notifications" do
      # Test that both client and server can send notifications

      _client_notifications = []
      _server_notifications = []

      # Create a more complex server that tracks notifications
      server_pid =
        spawn(fn ->
          client_handler = fn client_mailbox ->
            # Server sends a notification to client
            server_notif = %{
              "jsonrpc" => "2.0",
              "method" => "notifications/tools/list_changed",
              "params" => %{}
            }

            send(client_mailbox, {:mcp_message, server_notif})

            # Receive notification from client
            receive do
              {:mcp_message, client_notif} ->
                assert client_notif["method"] == "notifications/initialized"

                # Send another notification
                final_notif = %{
                  "jsonrpc" => "2.0",
                  "method" => "notifications/progress",
                  "params" => %{"progressToken" => "test", "progress" => 50}
                }

                send(client_mailbox, {:mcp_message, final_notif})
            end
          end

          receive do
            {:beam_connect, client_mailbox} ->
              send(client_mailbox, {:mcp_connected, self()})
              client_handler.(client_mailbox)

            {:beam_connect, client_mailbox, _auth_info} ->
              send(client_mailbox, {:mcp_connected, self()})
              client_handler.(client_mailbox)
          end
        end)

      {:ok, state} = Beam.connect(server: server_pid, format: :native)

      # Receive first notification from server
      {:ok, notif1, state} = Beam.receive_message(state)
      assert notif1["method"] == "notifications/tools/list_changed"

      # Send notification to server
      {:ok, state} = Beam.send_message(@notification, state)

      # Receive second notification from server
      {:ok, notif2, _state} = Beam.receive_message(state)
      assert notif2["method"] == "notifications/progress"
      assert notif2["params"]["progress"] == 50
    end
  end

  describe "error handling" do
    test "BEAM transport handles invalid JSON gracefully" do
      server_pid =
        spawn(fn ->
          receive do
            {:beam_connect, client_mailbox} ->
              send(client_mailbox, {:mcp_connected, self()})

              receive do
                {:mcp_message, message} ->
                  # Should receive error map
                  assert message["error"] == "Invalid JSON"
              end

            {:beam_connect, client_mailbox, _auth_info} ->
              send(client_mailbox, {:mcp_connected, self()})

              receive do
                {:mcp_message, message} ->
                  # Should receive error map
                  assert message["error"] == "Invalid JSON"
              end
          end
        end)

      {:ok, state} = Beam.connect(server: server_pid)

      # Send invalid JSON
      {:ok, _state} = Beam.send_message("invalid json {", state)
    end
  end
end
