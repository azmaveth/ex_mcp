defmodule ExMCP.ClientV2Test do
  use ExUnit.Case, async: true

  alias ExMCP.ClientV2
  alias ExMCP.Internal.Protocol

  # Mock transport for testing
  defmodule MockTransport do
    @behaviour ExMCP.Transport

    def connect(opts) do
      if Keyword.get(opts, :fail_connect) do
        {:error, :connection_refused}
      else
        {:ok, %{opts: opts, messages: [], connected: true}}
      end
    end

    def send(%{connected: false}, _data) do
      {:error, :not_connected}
    end

    def send(state, data) do
      # Store the message and immediately prepare a response
      new_state = %{state | messages: [data | state.messages]}

      # Check if this is an initialize request
      case Protocol.parse_message(data) do
        {:notification, "notifications/initialized", _} ->
          # Just acknowledge initialized notification
          {:ok, new_state}

        _ ->
          {:ok, new_state}
      end
    end

    def recv(%{connected: false}, _timeout) do
      {:error, :closed}
    end

    def recv(state, timeout) do
      # For testing, we'll simulate responses based on the request
      receive do
        # Never matches, just for timeout
        :never -> :ok
      after
        # Small delay to simulate network
        10 ->
          case state.messages do
            [last_sent | rest] ->
              # Parse the request and generate appropriate response
              case Protocol.parse_message(last_sent) do
                {:request, "initialize", _params, id} ->
                  response =
                    Protocol.encode_response(
                      %{
                        "protocolVersion" => "2024-11-05",
                        "capabilities" => %{
                          "tools" => %{"listChanged" => true}
                        },
                        "serverInfo" => %{
                          "name" => "MockServer",
                          "version" => "1.0.0"
                        }
                      },
                      id
                    )

                  # Remove the processed message
                  {:ok, Jason.encode!(response), %{state | messages: rest}}

                {:request, "tools/list", _params, id} ->
                  response =
                    Protocol.encode_response(
                      %{
                        "tools" => [
                          %{
                            "name" => "test_tool",
                            "description" => "A test tool",
                            "inputSchema" => %{
                              "type" => "object",
                              "properties" => %{}
                            }
                          }
                        ]
                      },
                      id
                    )

                  {:ok, Jason.encode!(response), %{state | messages: rest}}

                _ ->
                  # Unknown request, timeout
                  if timeout > 10 do
                    recv(state, timeout - 10)
                  else
                    {:error, :timeout}
                  end
              end

            [] ->
              # No messages to process
              if timeout > 10 do
                recv(state, timeout - 10)
              else
                {:error, :timeout}
              end
          end
      end
    end

    def close(state) do
      {:ok, %{state | connected: false}}
    end

    def controlling_process(_state, _pid) do
      :ok
    end

    # Add missing behavior functions
    def send_message(state, message) do
      {:ok, %{state | messages: [message | state.messages]}}
    end

    def receive_message(_state) do
      {:error, :not_implemented}
    end
  end

  describe "start_link/1" do
    test "successfully connects and completes handshake" do
      # Start client with mock transport
      assert {:ok, client} =
               ClientV2.start_link(
                 transport: MockTransport,
                 name: :test_client_1
               )

      # Verify client is ready
      assert {:ok, status} = ClientV2.get_status(client)
      assert status.connection_status == :ready
      assert status.server_info["name"] == "MockServer"

      # Clean up
      GenServer.stop(client)
    end

    test "returns error when connection fails" do
      # This should fail to start
      assert {:error, {:transport_connect_failed, :connection_refused}} =
               ClientV2.start_link(
                 transport: MockTransport,
                 fail_connect: true
               )
    end
  end

  describe "tool operations" do
    setup do
      {:ok, client} =
        ClientV2.start_link(
          transport: MockTransport,
          name: :test_client_tools
        )

      on_exit(fn -> GenServer.stop(client) end)

      %{client: client}
    end

    test "list_tools returns tools", %{client: client} do
      assert {:ok, result} = ClientV2.list_tools(client)
      assert %{"tools" => [tool]} = result
      assert tool["name"] == "test_tool"
    end
  end

  describe "connection lifecycle" do
    test "client is immediately ready after start_link" do
      start_time = System.monotonic_time(:millisecond)

      {:ok, client} =
        ClientV2.start_link(
          transport: MockTransport,
          name: :test_client_lifecycle
        )

      # This should work immediately without any sleep
      assert {:ok, _} = ClientV2.list_tools(client)

      # Verify it was fast (no artificial delays)
      elapsed = System.monotonic_time(:millisecond) - start_time
      assert elapsed < 1000, "Client took too long to initialize: #{elapsed}ms"

      GenServer.stop(client)
    end
  end

  describe "reconnection" do
    @describetag :skip
    test "placeholder" do
      # TODO: Add tests for reconnection logic
      assert true
    end
  end

  describe "error handling" do
    @describetag :skip
    test "placeholder" do
      # TODO: Add tests for various error scenarios
      assert true
    end
  end
end
