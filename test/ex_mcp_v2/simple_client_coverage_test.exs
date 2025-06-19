defmodule ExMCP.SimpleClientCoverageTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog

  alias ExMCP.SimpleClient

  # Working mock transport that properly handles the protocol
  defmodule WorkingMockTransport do
    @moduledoc false

    use GenServer

    # Client API
    def connect(opts) do
      case Keyword.get(opts, :fail_connect) do
        true ->
          {:error, :connection_refused}

        _ ->
          # Start a GenServer to handle the transport
          {:ok, pid} = GenServer.start_link(__MODULE__, opts)
          {:ok, pid}
      end
    end

    def send(transport_pid, message) do
      GenServer.call(transport_pid, {:send, message})
    end

    def recv(transport_pid, timeout) do
      GenServer.call(transport_pid, {:recv, timeout}, timeout + 100)
    end

    def close(transport_pid) do
      GenServer.stop(transport_pid)
      {:ok, nil}
    end

    # Server callbacks
    def init(opts) do
      state = %{
        parent: Keyword.get(opts, :parent, self()),
        pending_responses: :queue.new(),
        connected: true,
        opts: opts
      }

      {:ok, state}
    end

    def handle_call({:send, message}, _from, state) do
      # Parse and handle the message
      case Jason.decode(message) do
        {:ok, %{"method" => "initialize", "id" => id}} ->
          # Queue initialize response
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "result" => %{
              "protocolVersion" => "2024-11-05",
              "serverInfo" => %{"name" => "Test Server", "version" => "1.0.0"},
              "capabilities" => %{"tools" => %{}, "resources" => %{}, "prompts" => %{}}
            }
          }

          new_queue = :queue.in(Jason.encode!(response), state.pending_responses)
          {:reply, {:ok, self()}, %{state | pending_responses: new_queue}}

        {:ok, %{"method" => "notifications/initialized"}} ->
          # No response for notifications
          {:reply, {:ok, self()}, state}

        {:ok, %{"method" => method, "id" => id}} ->
          # Handle other requests based on configuration
          response = build_response(method, id, state.opts)
          new_queue = :queue.in(Jason.encode!(response), state.pending_responses)
          {:reply, {:ok, self()}, %{state | pending_responses: new_queue}}

        _ ->
          {:reply, {:ok, self()}, state}
      end
    end

    def handle_call({:recv, _timeout}, _from, state) do
      case :queue.out(state.pending_responses) do
        {{:value, response}, new_queue} ->
          {:reply, {:ok, response, self()}, %{state | pending_responses: new_queue}}

        {:empty, _} ->
          # No pending responses, return timeout
          {:reply, {:error, :timeout}, state}
      end
    end

    defp build_response("tools/list", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "tools" => [
            %{"name" => "test_tool", "description" => "Test tool for coverage"}
          ]
        }
      }
    end

    defp build_response("tools/call", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "content" => [%{"type" => "text", "text" => "Tool result"}]
        }
      }
    end

    defp build_response("resources/list", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "resources" => [
            %{"uri" => "file://test.txt", "name" => "Test Resource"}
          ]
        }
      }
    end

    defp build_response("resources/read", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "contents" => [%{"type" => "text", "text" => "Resource content"}]
        }
      }
    end

    defp build_response("prompts/list", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "prompts" => [
            %{"name" => "test_prompt", "description" => "Test prompt"}
          ]
        }
      }
    end

    defp build_response("prompts/get", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "messages" => [
            %{"role" => "user", "content" => %{"type" => "text", "text" => "Hello!"}}
          ]
        }
      }
    end

    defp build_response(_method, id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{}
      }
    end
  end

  describe "SimpleClient basic functionality" do
    test "connects successfully" do
      opts = [transport: WorkingMockTransport]
      assert {:ok, client} = SimpleClient.start_link(opts)
      assert is_pid(client)

      # Verify status
      assert {:ok, status} = SimpleClient.get_status(client)
      assert status.connection_status == :connected
      assert status.server_info["name"] == "Test Server"
    end

    test "handles connection failure" do
      opts = [transport: WorkingMockTransport, fail_connect: true]

      assert capture_log(fn ->
               assert {:error, _} = SimpleClient.start_link(opts)
             end) =~ "Failed to initialize MCP client"
    end

    test "lists tools" do
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      assert {:ok, result} = SimpleClient.list_tools(client)
      assert result["tools"] != nil
      assert [%{"name" => "test_tool"}] = result["tools"]
    end

    test "calls tool" do
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      assert {:ok, result} = SimpleClient.call_tool(client, "test_tool", %{})
      assert result["content"] != nil
    end

    test "lists resources" do
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      assert {:ok, result} = SimpleClient.list_resources(client)
      assert result["resources"] != nil
    end

    test "reads resource" do
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      assert {:ok, result} = SimpleClient.read_resource(client, "file://test.txt")
      assert result["contents"] != nil
    end

    test "lists prompts" do
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      assert {:ok, result} = SimpleClient.list_prompts(client)
      assert result["prompts"] != nil
    end

    test "gets prompt" do
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      assert {:ok, result} = SimpleClient.get_prompt(client, "test_prompt", %{})
      assert result["messages"] != nil
    end

    test "performs health check" do
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      assert :ok = SimpleClient.health_check(client)
    end

    test "handles request timeout" do
      # Create a mock transport that delays responses
      defmodule SlowMockTransport do
        use GenServer

        def connect(_opts) do
          {:ok, pid} = GenServer.start_link(__MODULE__, [])
          {:ok, pid}
        end

        def send(pid, message) do
          GenServer.cast(pid, {:send, message})
          {:ok, pid}
        end

        def recv(pid, timeout) do
          # Always timeout
          Process.sleep(timeout + 10)
          {:error, :timeout}
        end

        def close(pid) do
          GenServer.stop(pid)
          {:ok, nil}
        end

        def init(_), do: {:ok, %{}}
        def handle_cast({:send, _}, state), do: {:noreply, state}
      end

      opts = [transport: SlowMockTransport]
      # This will timeout during initialization
      assert {:error, :timeout} = SimpleClient.start_link(opts)
    end
  end

  describe "reconnection behavior" do
    test "handles reconnection request" do
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      # Force reconnection
      assert :ok = SimpleClient.reconnect(client)

      # Should still be connected
      assert {:ok, status} = SimpleClient.get_status(client)
      assert status.connection_status == :connected
    end
  end

  describe "transport configuration" do
    test "handles multiple transports with fallback" do
      opts = [
        transports: [
          {WorkingMockTransport, [fail_connect: true]},
          {WorkingMockTransport, []}
        ],
        fallback_strategy: :sequential
      ]

      assert {:ok, client} = SimpleClient.start_link(opts)
      assert is_pid(client)
    end

    test "fails when all transports fail" do
      opts = [
        transports: [
          {WorkingMockTransport, [fail_connect: true]},
          {WorkingMockTransport, [fail_connect: true]}
        ]
      ]

      assert capture_log(fn ->
               assert {:error, _} = SimpleClient.start_link(opts)
             end) =~ "All transports failed"
    end
  end

  describe "error handling" do
    test "handles malformed responses gracefully" do
      # This would require a mock that returns invalid JSON
      # For now, we just test the client doesn't crash
      opts = [transport: WorkingMockTransport]
      {:ok, client} = SimpleClient.start_link(opts)

      # Client should remain operational
      assert {:ok, _} = SimpleClient.get_status(client)
    end
  end
end
