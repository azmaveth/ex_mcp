defmodule ExMCP.Transport.BeamTest do
  use ExUnit.Case, async: true

  @moduletag :transport
  @moduletag :beam
  @moduletag :requires_beam

  import ExUnit.CaptureLog

  alias ExMCP.{Client, Server}

  defmodule TestHandler do
    use ExMCP.Server.Handler

    @impl true
    def handle_initialize(_params, state) do
      result = %{
        protocolVersion: "2024-11-05",
        serverInfo: %{
          name: "test-beam-server",
          version: "1.0.0"
        },
        capabilities: %{
          tools: %{},
          resources: %{},
          prompts: %{}
        }
      }

      {:ok, result, state}
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      tools = [
        %{
          name: "echo",
          description: "Echoes back the input"
        }
      ]

      {:ok, tools, nil, state}
    end

    @impl true
    def handle_call_tool("echo", %{"message" => msg}, state) do
      # Return just the content array, server will wrap it
      result = [%{type: "text", text: "Echo: #{msg}"}]
      {:ok, result, state}
    end

    @impl true
    def handle_call_tool(_, _, state) do
      {:error, "Unknown tool", state}
    end

    @impl true
    def handle_list_resources(_cursor, state) do
      resources = [
        %{
          uri: "test://resource",
          name: "Test Resource",
          description: "A test resource"
        }
      ]

      {:ok, resources, nil, state}
    end

    @impl true
    def handle_read_resource("test://resource", state) do
      content = %{
        uri: "test://resource",
        mimeType: "text/plain",
        text: "Test resource content"
      }

      {:ok, content, state}
    end

    @impl true
    def handle_read_resource(_, state) do
      {:error, "Resource not found", state}
    end
  end

  describe "BEAM transport basics" do
    test "client can connect to server" do
      # Start server
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_1
        )

      # Start client
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_1
        )

      # Verify connection
      assert {:ok, server_info} = Client.server_info(client)
      assert server_info["name"] == "test-beam-server"

      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "client and server can exchange messages" do
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_2
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_2
        )

      # List tools
      {:ok, %{tools: tools}} = Client.list_tools(client)
      assert length(tools) == 1
      assert hd(tools).name == "echo"

      # Call tool
      {:ok, result} = Client.call_tool(client, "echo", %{"message" => "Hello"})
      assert result == %{content: [%{type: "text", text: "Echo: Hello"}]}

      GenServer.stop(client)
      GenServer.stop(server)
    end
  end

  describe "bidirectional notifications" do
    test "server can send resource change notifications" do
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_3
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_3,
          handler: ExMCP.Client.DefaultHandler,
          handler_state: []
        )

      # Wait for client to initialize
      Process.sleep(100)

      # Capture logs to verify notification was received
      log =
        capture_log([level: :info], fn ->
          Server.notify_resources_changed(server)
          Process.sleep(100)
        end)

      assert log =~ "Resources list changed"

      if Process.alive?(client), do: GenServer.stop(client)
      if Process.alive?(server), do: GenServer.stop(server)
    end

    test "server can send tool change notifications" do
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_4
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_4,
          handler: ExMCP.Client.DefaultHandler,
          handler_state: []
        )

      # Wait for client to initialize
      Process.sleep(100)

      log =
        capture_log([level: :info], fn ->
          Server.notify_tools_changed(server)
          Process.sleep(100)
        end)

      assert log =~ "Tools list changed"

      if Process.alive?(client), do: GenServer.stop(client)
      if Process.alive?(server), do: GenServer.stop(server)
    end

    test "server can send progress notifications" do
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_5
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_5
        )

      # Wait for client to be fully initialized
      Process.sleep(50)

      log =
        capture_log([level: :info], fn ->
          Server.notify_progress(server, "test-op", 50, 100)
          # Increased from 50ms to ensure notification is processed
          Process.sleep(100)
        end)

      assert log =~ "Progress [test-op]: 50/100"

      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "server can send resource updated notifications" do
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_6
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_6,
          handler: ExMCP.Client.DefaultHandler,
          handler_state: []
        )

      # Wait for client to initialize
      Process.sleep(100)

      log =
        capture_log([level: :info], fn ->
          Server.notify_resource_updated(server, "test://resource")
          Process.sleep(100)
        end)

      assert log =~ "Resource updated: test://resource"

      if Process.alive?(client), do: GenServer.stop(client)
      if Process.alive?(server), do: GenServer.stop(server)
    end

    test "server can send prompts change notifications" do
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_7
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_7,
          handler: ExMCP.Client.DefaultHandler,
          handler_state: []
        )

      # Wait for client to initialize
      Process.sleep(100)

      log =
        capture_log([level: :info], fn ->
          Server.notify_prompts_changed(server)
          Process.sleep(100)
        end)

      assert log =~ "Prompts list changed"

      if Process.alive?(client), do: GenServer.stop(client)
      if Process.alive?(server), do: GenServer.stop(server)
    end
  end

  describe "error handling" do
    test "client handles server shutdown gracefully" do
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_8
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_8
        )

      # Stop server
      GenServer.stop(server)
      Process.sleep(50)

      # Client should detect disconnection
      assert {:error, _} = Client.list_tools(client)

      GenServer.stop(client)
    end

    test "server not found error" do
      # Client will start but fail to connect
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :nonexistent_server
        )

      # Try to make a request, which should fail since we're not connected
      assert {:error, :not_connected} = Client.list_tools(client, timeout: 1000)

      GenServer.stop(client)
    end
  end

  describe "transport internals" do
    test "mailbox processes are cleaned up on shutdown" do
      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: :beam,
          name: :test_beam_server_9
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_beam_server_9
        )

      # Get mailbox PIDs
      client_state = :sys.get_state(client)
      server_state = :sys.get_state(server)

      client_mailbox = client_state.transport_state.mailbox_pid
      server_mailbox = server_state.transport_state.mailbox_pid

      assert Process.alive?(client_mailbox)
      assert Process.alive?(server_mailbox)

      # Stop client and server
      GenServer.stop(client)
      GenServer.stop(server)

      Process.sleep(50)

      # Mailboxes should be cleaned up
      refute Process.alive?(client_mailbox)
      refute Process.alive?(server_mailbox)
    end
  end
end
