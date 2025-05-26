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
            {:mcp_connect, client_pid} ->
              send(client_pid, {:mcp_connected, self()})

              # Wait for notification
              receive do
                {:mcp_message, ^client_pid, message} ->
                  # Echo back the message
                  send(client_pid, {:mcp_response, self(), message})
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
            {:mcp_connect, client_pid} ->
              send(client_pid, {:mcp_connected, self()})

              receive do
                {:mcp_message, ^client_pid, message} ->
                  # Verify we received a map
                  assert is_map(message)
                  assert message["method"] == "notifications/initialized"

                  # Send back a notification
                  notification = %{
                    "jsonrpc" => "2.0",
                    "method" => "notifications/resources/list_changed",
                    "params" => %{}
                  }

                  send(client_pid, {:mcp_response, self(), notification})
              end
          end
        end)

      {:ok, state} = Beam.connect(server: server_pid)

      # Send notification as map
      {:ok, _state} = Beam.send_message(@notification, state)

      # Receive response
      {:ok, response_json, _state} = Beam.receive_message(state)
      {:ok, response} = Jason.decode(response_json)

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
          receive do
            {:mcp_connect, client_pid} ->
              send(client_pid, {:mcp_connected, self()})

              # Server sends a notification to client
              server_notif = %{
                "jsonrpc" => "2.0",
                "method" => "notifications/tools/list_changed",
                "params" => %{}
              }

              send(client_pid, {:mcp_message, self(), server_notif})

              # Receive notification from client
              receive do
                {:mcp_message, ^client_pid, client_notif} ->
                  assert client_notif["method"] == "notifications/initialized"

                  # Send another notification
                  final_notif = %{
                    "jsonrpc" => "2.0",
                    "method" => "notifications/progress",
                    "params" => %{"progressToken" => "test", "progress" => 50}
                  }

                  send(client_pid, {:mcp_message, self(), final_notif})
              end
          end
        end)

      {:ok, state} = Beam.connect(server: server_pid)

      # Receive first notification from server
      {:ok, notif1_json, state} = Beam.receive_message(state)
      {:ok, notif1} = Jason.decode(notif1_json)
      assert notif1["method"] == "notifications/tools/list_changed"

      # Send notification to server
      {:ok, state} = Beam.send_message(Jason.encode!(@notification), state)

      # Receive second notification from server
      {:ok, notif2_json, _state} = Beam.receive_message(state)
      {:ok, notif2} = Jason.decode(notif2_json)
      assert notif2["method"] == "notifications/progress"
      assert notif2["params"]["progress"] == 50
    end
  end

  describe "error handling" do
    test "BEAM transport handles invalid JSON gracefully" do
      server_pid =
        spawn(fn ->
          receive do
            {:mcp_connect, client_pid} ->
              send(client_pid, {:mcp_connected, self()})

              receive do
                {:mcp_message, ^client_pid, message} ->
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
