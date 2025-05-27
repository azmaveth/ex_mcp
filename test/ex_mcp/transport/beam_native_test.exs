defmodule ExMCP.Transport.BeamNativeTest do
  use ExUnit.Case, async: true

  alias ExMCP.Transport.Beam

  describe "native format" do
    test "sends and receives Elixir terms directly in native mode" do
      # Create a mock server
      server_pid =
        spawn(fn ->
          receive do
            {:beam_connect, client_mailbox} ->
              send(client_mailbox, {:mcp_connected, self()})

              receive do
                {:mcp_message, message} ->
                  # In native mode, we receive the raw Elixir term
                  assert is_map(message)
                  assert message.method == "test/native"
                  assert message.params == %{foo: "bar"}

                  # Send back a native response
                  response = %{
                    jsonrpc: "2.0",
                    id: message.id,
                    result: %{native: true, atoms: :allowed}
                  }

                  send(client_mailbox, {:mcp_message, response})
              end

            {:beam_connect, client_mailbox, _auth_info} ->
              send(client_mailbox, {:mcp_connected, self()})

              receive do
                {:mcp_message, message} ->
                  # In native mode, we receive the raw Elixir term
                  assert is_map(message)
                  assert message.method == "test/native"
                  assert message.params == %{foo: "bar"}

                  # Send back a native response
                  response = %{
                    jsonrpc: "2.0",
                    id: message.id,
                    result: %{native: true, atoms: :allowed}
                  }

                  send(client_mailbox, {:mcp_message, response})
              end
          end
        end)

      # Connect with native format
      {:ok, state} = Beam.connect(server: server_pid, format: :native)

      # Send a native Elixir map
      request = %{
        jsonrpc: "2.0",
        method: "test/native",
        params: %{foo: "bar"},
        id: 1
      }

      {:ok, state} = Beam.send_message(request, state)

      # Receive native response
      {:ok, response, _state} = Beam.receive_message(state)

      # In native mode, we get the raw term back
      assert is_map(response)
      assert response.result.native == true
      assert response.result.atoms == :allowed
    end

    test "JSON format encodes and decodes messages" do
      # Create a mock server
      server_pid =
        spawn(fn ->
          receive do
            {:beam_connect, client_mailbox} ->
              send(client_mailbox, {:mcp_connected, self()})

              receive do
                {:mcp_message, message} ->
                  # In JSON mode, we should receive a JSON string
                  assert is_binary(message)
                  {:ok, decoded} = Jason.decode(message)
                  assert decoded["method"] == "test/json"

                  # Send back a JSON response
                  response =
                    Jason.encode!(%{
                      "jsonrpc" => "2.0",
                      "id" => decoded["id"],
                      "result" => %{"format" => "json"}
                    })

                  send(client_mailbox, {:mcp_message, response})
              end

            {:beam_connect, client_mailbox, _auth_info} ->
              send(client_mailbox, {:mcp_connected, self()})

              receive do
                {:mcp_message, message} ->
                  # In JSON mode, we should receive a JSON string
                  assert is_binary(message)
                  {:ok, decoded} = Jason.decode(message)
                  assert decoded["method"] == "test/json"

                  # Send back a JSON response
                  response =
                    Jason.encode!(%{
                      "jsonrpc" => "2.0",
                      "id" => decoded["id"],
                      "result" => %{"format" => "json"}
                    })

                  send(client_mailbox, {:mcp_message, response})
              end
          end
        end)

      # Connect with JSON format (default)
      {:ok, state} = Beam.connect(server: server_pid, format: :json)

      # Send a map that will be converted to JSON
      request = %{
        "jsonrpc" => "2.0",
        "method" => "test/json",
        "params" => %{"test" => true},
        "id" => 1
      }

      {:ok, state} = Beam.send_message(request, state)

      # Receive JSON response
      {:ok, response_json, _state} = Beam.receive_message(state)

      # In JSON mode, we get a JSON string
      assert is_binary(response_json)
      {:ok, decoded} = Jason.decode(response_json)
      assert decoded["result"]["format"] == "json"
    end

    test "handles mixed message types in JSON mode" do
      server_pid =
        spawn(fn ->
          receive do
            {:beam_connect, client_mailbox} ->
              send(client_mailbox, {:mcp_connected, self()})

              # Server sends a map (will be converted to JSON)
              send(client_mailbox, {:mcp_message, %{"notification" => "test"}})

            {:beam_connect, client_mailbox, _auth_info} ->
              send(client_mailbox, {:mcp_connected, self()})

              # Server sends a map (will be converted to JSON)
              send(client_mailbox, {:mcp_message, %{"notification" => "test"}})
          end
        end)

      {:ok, state} = Beam.connect(server: server_pid, format: :json)

      # Receive message
      {:ok, message, _state} = Beam.receive_message(state)

      # Should receive as JSON string
      assert is_binary(message)
      {:ok, decoded} = Jason.decode(message)
      assert decoded["notification"] == "test"
    end

    test "default format is JSON" do
      server_pid =
        spawn(fn ->
          receive do
            {:beam_connect, client_mailbox} ->
              send(client_mailbox, {:mcp_connected, self()})

            {:beam_connect, client_mailbox, _auth_info} ->
              send(client_mailbox, {:mcp_connected, self()})
          end
        end)

      # Connect without specifying format
      {:ok, state} = Beam.connect(server: server_pid)

      # Format should default to :json
      assert state.format == :json
    end

    test "raises error for invalid format" do
      assert_raise ArgumentError, ~r/format must be :json or :native/, fn ->
        Beam.connect(server: self(), format: :invalid)
      end
    end
  end
end
