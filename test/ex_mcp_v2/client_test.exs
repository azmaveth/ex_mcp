defmodule ExMCP.ClientV2Test do
  use ExUnit.Case, async: true

  @moduletag :integration

  alias ExMCP.ClientV2
  alias ExMCP.Internal.Protocol

  # Mock transport for testing
  defmodule MockTransport do
    @behaviour ExMCP.Transport

    import Kernel, except: [send: 2]

    def connect(opts) do
      if Keyword.get(opts, :fail_connect) do
        {:error, :connection_refused}
      else
        # Start an agent to handle the message queue
        {:ok, agent} = Agent.start_link(fn -> %{responses: []} end)
        {:ok, %{agent: agent, opts: opts, connected: true}}
      end
    end

    # V2 transport interface - used by SimpleClient
    def send(%{connected: false}, _data) do
      {:error, :not_connected}
    end

    def send(%{agent: agent} = state, data) do
      # Parse the request and immediately queue the response
      case Jason.decode!(data) do
        %{"method" => "initialize", "id" => id} ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "result" => %{
              "protocolVersion" => "2024-11-05",
              "capabilities" => %{
                "tools" => %{"listChanged" => true}
              },
              "serverInfo" => %{
                "name" => "MockServer",
                "version" => "1.0.0"
              }
            }
          }

          Agent.update(agent, fn state ->
            %{state | responses: [Jason.encode!(response) | state.responses]}
          end)

        %{"method" => "tools/list", "id" => id} ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "result" => %{
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
            }
          }

          Agent.update(agent, fn state ->
            %{state | responses: [Jason.encode!(response) | state.responses]}
          end)

        %{"method" => "notifications/initialized"} ->
          # Just acknowledge - no response needed
          :ok

        _ ->
          # Unknown request - no response
          :ok
      end

      {:ok, state}
    end

    def recv(%{connected: false}, _timeout) do
      {:error, :closed}
    end

    def recv(%{agent: agent} = state, timeout) do
      # Get the next response with timeout handling
      start_time = System.monotonic_time(:millisecond)

      case poll_for_response(agent, timeout, start_time) do
        nil -> {:error, :timeout}
        response -> {:ok, response, state}
      end
    end

    defp poll_for_response(agent, timeout, start_time) do
      case Agent.get_and_update(agent, fn %{responses: responses} ->
             case responses do
               [response | rest] -> {response, %{responses: rest}}
               [] -> {nil, %{responses: []}}
             end
           end) do
        nil ->
          # Check if we've exceeded timeout
          elapsed = System.monotonic_time(:millisecond) - start_time

          if elapsed >= timeout do
            nil
          else
            # Wait a bit and try again
            Process.sleep(10)
            poll_for_response(agent, timeout, start_time)
          end

        response ->
          response
      end
    end

    def close(%{agent: agent} = state) do
      if Process.alive?(agent) do
        Agent.stop(agent)
      end

      {:ok, %{state | connected: false}}
    end

    def controlling_process(_state, _pid) do
      :ok
    end

    # V1 transport interface - for compatibility
    def send_message(message, state) do
      case send(state, Jason.encode!(message)) do
        {:ok, new_state} -> {:ok, new_state}
        {:error, reason} -> {:error, reason}
      end
    end

    def receive_message(state) do
      case recv(state, 100) do
        {:ok, data, new_state} -> {:ok, Jason.decode!(data), new_state}
        {:error, reason} -> {:error, reason}
      end
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
      # This should fail to start with an EXIT signal because GenServer.start_link 
      # links the calling process to the GenServer, and when init returns {:stop, reason},
      # the GenServer exits and causes the caller to exit too
      Process.flag(:trap_exit, true)

      result =
        ClientV2.start_link(
          transport: MockTransport,
          fail_connect: true
        )

      # Should get an error tuple since the GenServer should stop during init
      assert {:error, {:transport_connect_failed, :connection_refused}} = result
    end
  end

  describe "tool operations" do
    setup do
      {:ok, client} =
        ClientV2.start_link(
          transport: MockTransport,
          name: :test_client_tools
        )

      on_exit(fn ->
        if Process.alive?(client) do
          GenServer.stop(client)
        end
      end)

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
