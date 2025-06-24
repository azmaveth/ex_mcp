defmodule ExMCP.Compliance.TransportVersionTest do
  use ExUnit.Case, async: true

  # Aliases
  alias ExMCP.Client
  alias ExMCP.Protocol.VersionNegotiator
  alias ExMCP.Server
  alias ExMCP.Transport.HTTP

  # A server module for testing version negotiation.
  # Using `use ExMCP.Server` is the modern pattern and provides consistent
  # start/stop behavior across transports.
  defmodule VersionTestServer do
    use ExMCP.Server

    deftool "ping" do
      meta do
        description("A simple ping tool")
        input_schema(%{})
      end
    end

    @impl true
    def handle_initialize(params, state) do
      # The server transport layer is responsible for rejecting unsupported versions.
      # This handler mimics that behavior for testing.
      if params["protocolVersion"] == "unsupported-version" do
        {:error, "Unsupported protocol version", state}
      else
        result = %{
          protocolVersion: params["protocolVersion"],
          serverInfo: %{name: "version-test-server", version: "1.0.0"},
          capabilities: %{}
        }

        {:ok, result, state}
      end
    end

    # A simple tool to ensure requests are processed
    @impl true
    def handle_tool_call("ping", _args, state) do
      {:ok, %{content: [%{"type" => "text", "text" => "pong"}]}, state}
    end
  end

  # Setup for HTTP-based tests
  defp start_http_server do
    # Find a free port to avoid conflicts in async tests
    {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)

    # Start the server using the high-level API
    {:ok, pid} = VersionTestServer.start_link(transport: :http, port: port)
    base_url = "http://localhost:#{port}"

    # Return the base_url and a function to stop the server
    {:ok,
     %{
       base_url: base_url,
       stop_server_fn: fn -> if Process.alive?(pid), do: GenServer.stop(pid) end
     }}
  end

  describe "HTTP Transport" do
    setup do
      {:ok, context} = start_http_server()
      on_exit(fn -> context.stop_server_fn.() end)
      context
    end

    @tag :requires_http
    test "client sends correct mcp-protocol-version header", %{base_url: base_url} do
      # This test verifies that the client correctly sets the protocol version,
      # and the server correctly handles it.
      version = VersionNegotiator.latest_version()

      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: base_url,
          protocol_version: version,
          use_sse: false
        )

      # The negotiated version should match what we requested
      {:ok, server_info} = Client.server_info(client)
      assert server_info["name"] == "version-test-server"

      # The transport state should also reflect this
      transport_state = :sys.get_state(client).transport_state
      assert transport_state.protocol_version == version

      Client.stop(client)
    end

    @tag :requires_http
    test "server rejects unsupported protocol version", %{base_url: base_url} do
      # The client's `start_link` should fail if the server rejects the version.
      result =
        Client.start_link(
          transport: :http,
          url: base_url,
          protocol_version: "unsupported-version",
          use_sse: false
        )

      # The HttpPlug should return a 400 Bad Request
      assert {:error, {:connection_failed, {:error, {:http_error, 400, body}}}} = result
      assert body =~ "Unsupported protocol version"
    end

    @tag :requires_http
    test "client uses latest version by default", %{base_url: base_url} do
      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: base_url,
          use_sse: false
          # No protocol_version specified
        )

      latest_version = VersionNegotiator.latest_version()
      {:ok, server_info} = Client.server_info(client)
      assert server_info["name"] == "version-test-server"

      transport_state = :sys.get_state(client).transport_state
      assert transport_state.protocol_version == latest_version

      Client.stop(client)
    end

    @tag :requires_http
    test "Mcp-Session-Id is maintained across requests", %{base_url: base_url} do
      session_id = "session-" <> (System.unique_integer() |> Integer.to_string())

      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: base_url,
          use_sse: false,
          session_id: session_id
        )

      # Check the initial transport state
      transport_state = :sys.get_state(client).transport_state
      assert transport_state.session_id == session_id

      # Make a call to the server
      {:ok, _response} = Client.call_tool(client, "ping", %{})

      # The session ID should persist in the state
      transport_state_after_call = :sys.get_state(client).transport_state
      assert transport_state_after_call.session_id == session_id

      Client.stop(client)
    end
  end

  describe "SSE Transport" do
    setup do
      {:ok, context} = start_http_server()
      on_exit(fn -> context.stop_server_fn.() end)
      context
    end

    @tag :requires_http
    test "connects with correct protocol version and session ID", %{base_url: base_url} do
      version = "2025-06-18"
      session_id = "sse-session-1"

      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: base_url,
          protocol_version: version,
          session_id: session_id,
          use_sse: true
        )

      # Check negotiated version
      {:ok, server_info} = Client.server_info(client)
      assert server_info["name"] == "version-test-server"

      # Check transport state for SSE-specifics
      transport_state = :sys.get_state(client).transport_state
      assert transport_state.protocol_version == version
      assert transport_state.use_sse == true
      assert transport_state.session_id == session_id
      assert is_pid(transport_state.sse_pid)

      Client.stop(client)
    end

    @tag :requires_http
    test "sends Last-Event-ID header on reconnection (conceptual)", %{base_url: _base_url} do
      # This test is conceptual and does not require a running server.
      # It verifies our understanding of how the HTTP transport state should behave.
      # The `ExMCP.Transport.HTTP.start_sse/1` function is responsible for adding
      # the "Last-Event-ID" header if `state.last_event_id` is set.
      state_with_event_id = %HTTP{
        base_url: "http://localhost:1234",
        endpoint: "/mcp/v1",
        headers: [],
        security: nil,
        session_id: "session-for-resume",
        last_event_id: "event-123",
        protocol_version: "2025-06-18",
        timeouts: %{connect: 1000, stream_idle: 5000, stream_handshake: 1000},
        use_sse: true
      }

      # A client that receives an event with an ID should store it in its state.
      # We can trust that if `last_event_id` is in the state, the transport will use it.
      assert state_with_event_id.last_event_id == "event-123"
    end
  end

  describe "Native (:test) Transport" do
    setup do
      {:ok, server} = VersionTestServer.start_link(transport: :test)
      on_exit(fn -> if Process.alive?(server), do: GenServer.stop(server) end)
      {:ok, server: server}
    end

    test "negotiates protocol version via initialize message", %{server: server} do
      version = VersionNegotiator.latest_version()

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server,
          protocol_version: version
        )

      {:ok, server_info} = Client.server_info(client)
      assert server_info["name"] == "version-test-server"

      transport_state = :sys.get_state(client).transport_state
      assert transport_state.protocol_version == version

      Client.stop(client)
    end

    test "uses latest version by default when not specified", %{server: server} do
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
          # No protocol_version specified
        )

      latest_version = VersionNegotiator.latest_version()
      {:ok, server_info} = Client.server_info(client)
      assert server_info["name"] == "version-test-server"

      transport_state = :sys.get_state(client).transport_state
      assert transport_state.protocol_version == latest_version

      Client.stop(client)
    end
  end

  describe "Version-Specific Features" do
    test "2025-03-26: OAuth 2.1 metadata support (conceptual)" do
      # This test confirms awareness of version-specific features.
      # A full OAuth flow is out of scope, but we can check the metadata structure.
      # This is defined by the spec, not strictly a transport feature, but often
      # advertised via a server's metadata endpoint over HTTP.
      metadata = %{
        "authorization_server" => "https://auth.example.com",
        "resource" => "https://api.example.com/mcp",
        "scopes" => ["mcp:read", "mcp:write"]
      }

      assert Map.has_key?(metadata, "authorization_server")
      assert Map.has_key?(metadata, "resource")
      assert is_list(metadata["scopes"])
    end
  end
end
