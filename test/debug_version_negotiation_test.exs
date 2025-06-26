defmodule DebugVersionNegotiationTest do
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Server}

  defmodule DebugVersionHandler do
    use ExMCP.Server.Handler

    @impl true
    def init(args) do
      supported_version = Keyword.get(args, :supported_version, "2024-11-05")
      {:ok, %{supported_version: supported_version}}
    end

    @impl true
    def handle_initialize(params, state) do
      client_version = params["protocolVersion"]

      if client_version == state.supported_version do
        result = %{
          "protocolVersion" => state.supported_version,
          "serverInfo" => %{
            "name" => "debug-server",
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

    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [], state}
    @impl true
    def handle_tool_call(_name, _arguments, state), do: {:error, :not_implemented, state}
    @impl true
    def handle_list_resources(_cursor, state), do: {:ok, [], state}
    @impl true
    def handle_resource_read(_uri, state), do: {:error, :not_implemented, state}
    @impl true
    def handle_list_prompts(_cursor, state), do: {:ok, [], state}
    @impl true
    def handle_prompt_get(_name, _arguments, state), do: {:error, :not_implemented, state}
  end

  test "debug version negotiation failure" do
    # Start server that only supports "2024-11-05"
    {:ok, server} =
      Server.start_link(
        transport: :test,
        handler: DebugVersionHandler,
        handler_args: [supported_version: "2024-11-05"]
      )

    # Try to connect client requesting "2025-06-18" - this SHOULD fail
    result =
      Client.start_link(
        transport: :test,
        server: server,
        protocol_version: "2025-06-18"
      )

    case result do
      {:ok, client} ->
        # If we get here, the connection succeeded when it should have failed
        # Get status to understand what happened
        {:ok, status} = Client.get_status(client)
        Client.stop(client)

        flunk("""
        Expected client connection to fail but it succeeded!
        Client status: #{inspect(status)}

        This indicates the version negotiation is not working correctly.
        """)

      {:error, reason} ->
        # This is what we expect - connection should fail
        assert reason != nil
        IO.puts("✅ Connection correctly failed with reason: #{inspect(reason)}")
    end

    GenServer.stop(server)
  end

  test "debug version negotiation success" do
    # Start server that supports "2025-06-18"
    {:ok, server} =
      Server.start_link(
        transport: :test,
        handler: DebugVersionHandler,
        handler_args: [supported_version: "2025-06-18"]
      )

    # Connect client requesting the same version - this SHOULD succeed
    result =
      Client.start_link(
        transport: :test,
        server: server,
        protocol_version: "2025-06-18"
      )

    case result do
      {:ok, client} ->
        # This is expected - connection should succeed
        {:ok, status} = Client.get_status(client)
        assert status.connection_status == :ready
        Client.stop(client)
        IO.puts("✅ Connection correctly succeeded")

      {:error, reason} ->
        flunk("Expected connection to succeed but it failed with: #{inspect(reason)}")
    end

    GenServer.stop(server)
  end
end
