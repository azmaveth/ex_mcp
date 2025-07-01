defmodule DebugVersionNegotiationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client

  defmodule TestServerV2024 do
    use ExMCP.Server

    @impl true
    def handle_initialize(params, state) do
      client_version = params["protocolVersion"]

      if client_version == "2024-11-05" do
        result = %{
          "protocolVersion" => "2024-11-05",
          "serverInfo" => %{
            "name" => "debug-server-v2024",
            "version" => "1.0.0"
          },
          "capabilities" => %{}
        }

        {:ok, result, state}
      else
        error_msg = "Unsupported protocol version: #{client_version}"
        {:error, error_msg, state}
      end
    end
  end

  defmodule TestServerV2025 do
    use ExMCP.Server

    @impl true
    def handle_initialize(params, state) do
      client_version = params["protocolVersion"]

      if client_version == "2025-06-18" do
        result = %{
          "protocolVersion" => "2025-06-18",
          "serverInfo" => %{
            "name" => "debug-server-v2025",
            "version" => "1.0.0"
          },
          "capabilities" => %{}
        }

        {:ok, result, state}
      else
        error_msg = "Unsupported protocol version: #{client_version}"
        {:error, error_msg, state}
      end
    end
  end

  test "debug version negotiation failure" do
    # Start server that only supports "2024-11-05"
    {:ok, server} = TestServerV2024.start_link(transport: :test)

    # Try to connect client requesting "2025-06-18" - this SHOULD fail
    # Client.start_link should crash with version mismatch
    Process.flag(:trap_exit, true)

    result =
      Client.start_link(
        transport: :test,
        server: server,
        protocol_version: "2025-06-18"
      )

    # The client should fail to start due to version mismatch
    case result do
      {:error, {:initialize_error, %{"code" => -32600}}} ->
        IO.puts("✅ Connection correctly failed with version negotiation error")

      other ->
        flunk("Expected version negotiation failure, got: #{inspect(other)}")
    end

    GenServer.stop(server)
  end

  test "debug version negotiation success" do
    # Start server that supports "2025-06-18"
    {:ok, server} = TestServerV2025.start_link(transport: :test)

    # Connect client requesting the same version - this SHOULD succeed
    {:ok, client} =
      Client.start_link(
        transport: :test,
        server: server,
        protocol_version: "2025-06-18"
      )

    # This is expected - connection should succeed
    {:ok, status} = Client.get_status(client)
    assert status.connection_status == :ready
    Client.stop(client)
    IO.puts("✅ Connection correctly succeeded")

    GenServer.stop(server)
  end
end
