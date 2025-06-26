#!/usr/bin/env elixir

# Simple debug script to test version negotiation behavior
# Run with: elixir debug_version_test.exs

Mix.install([
  {:ex_mcp, path: "."}
])

defmodule DebugVersionHandler do
  use ExMCP.Server.Handler

  @impl true
  def init(args) do
    supported_version = Keyword.get(args, :supported_version, "2024-11-05")
    IO.puts("Debug handler initialized with supported version: #{supported_version}")
    {:ok, %{supported_version: supported_version}}
  end

  @impl true
  def handle_initialize(params, state) do
    client_version = params["protocolVersion"]
    IO.puts("Debug handler received initialize with version: #{inspect(client_version)}")
    IO.puts("Handler supports version: #{state.supported_version}")

    if client_version == state.supported_version do
      result = %{
        "protocolVersion" => state.supported_version,
        "serverInfo" => %{
          "name" => "debug-server",
          "version" => "1.0.0"
        },
        "capabilities" => %{}
      }
      IO.puts("Debug handler returning success: #{inspect(result)}")
      {:ok, result, state}
    else
      error_msg = "Unsupported protocol version: #{client_version}"
      IO.puts("Debug handler returning error: #{error_msg}")
      {:error, error_msg, state}
    end
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    {:ok, [], state}
  end

  @impl true
  def handle_tool_call(_name, _arguments, state) do
    {:error, :not_implemented, state}
  end

  @impl true
  def handle_list_resources(_cursor, state) do
    {:ok, [], state}
  end

  @impl true
  def handle_resource_read(_uri, state) do
    {:error, :not_implemented, state}
  end

  @impl true
  def handle_list_prompts(_cursor, state) do
    {:ok, [], state}
  end

  @impl true
  def handle_prompt_get(_name, _arguments, state) do
    {:error, :not_implemented, state}
  end
end

defmodule DebugTest do
  def run do
    IO.puts("Starting debug test...")

    # Start server that only supports "2024-11-05"
    IO.puts("Starting server...")
    {:ok, server} =
      ExMCP.Server.start_link(
        transport: :test,
        handler: DebugVersionHandler,
        handler_args: [supported_version: "2024-11-05"]
      )

    IO.puts("Server started with PID: #{inspect(server)}")

    # Try to connect client requesting "2025-06-18"
    IO.puts("Attempting client connection with unsupported version...")
    result = ExMCP.Client.start_link(
      transport: :test,
      server: server,
      protocol_version: "2025-06-18"
    )

    IO.puts("Client connection result: #{inspect(result)}")

    case result do
      {:ok, client} ->
        IO.puts("❌ ERROR: Client connection succeeded when it should have failed!")
        IO.puts("Client PID: #{inspect(client)}")
        
        # Check client status
        status = ExMCP.Client.get_status(client)
        IO.puts("Client status: #{inspect(status)}")
        
        # Clean up
        ExMCP.Client.stop(client)
      
      {:error, reason} ->
        IO.puts("✅ SUCCESS: Client connection failed as expected")
        IO.puts("Error reason: #{inspect(reason)}")
    end

    # Clean up server
    GenServer.stop(server)
    IO.puts("Debug test completed.")
  end
end

DebugTest.run()