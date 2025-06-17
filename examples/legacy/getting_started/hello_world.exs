#!/usr/bin/env elixir

# Hello World Example - Demonstrates all four transport types in ExMCP

Mix.install([
  {:ex_mcp, path: "../.."}
])

# This example shows how to connect to MCP servers using different transports:
# 1. Native Service Dispatcher - Ultra-fast Elixir-to-Elixir communication
# 2. stdio - Standard MCP communication via stdin/stdout
# 3. HTTP without SSE - Network communication using HTTP responses
# 4. HTTP with SSE - Network communication with Server-Sent Events

defmodule HelloWorldDemo do
  def run do
    IO.puts("""
    ========================================
    ExMCP Hello World - All Transport Types
    ========================================
    """)

    # 1. Native Service Dispatcher Demo
    demo_native_service()

    # 2. stdio Transport Demo
    demo_stdio_transport()

    # 3. HTTP Transport Demo (without SSE)
    demo_http_transport()

    # 4. HTTP with SSE Transport Demo
    demo_http_sse_transport()

    IO.puts("""

    ========================================
    Demo Complete!

    You've seen all four transport types:
    - Native: Direct Elixir process communication
    - stdio: JSON-RPC over stdin/stdout
    - HTTP: REST with regular HTTP responses
    - HTTP+SSE: REST with Server-Sent Events

    Choose the right transport for your use case!
    ========================================
    """)
  end

  defp demo_native_service do
    IO.puts("\n1. Native Service Dispatcher Demo")
    IO.puts("   (Ultra-fast Elixir-to-Elixir communication)")
    IO.puts("   " <> String.duplicate("-", 40))

    # Define and start a native service
    defmodule NativeHelloService do
      use ExMCP.Service, name: :native_hello_service

      @impl true
      def handle_mcp_request("list_tools", _params, state) do
        tools = [
          %{
            "name" => "say_hello",
            "description" => "Say hello via Native Service Dispatcher",
            "inputSchema" => %{
              "type" => "object",
              "properties" => %{
                "name" => %{"type" => "string", "description" => "Name to greet"}
              },
              "required" => ["name"]
            }
          }
        ]

        {:ok, %{"tools" => tools}, state}
      end

      @impl true
      def handle_mcp_request("tools/call", %{"name" => "say_hello", "arguments" => params}, state) do
        name = params["name"]
        content = [%{"type" => "text", "text" => "Hello, #{name}! Welcome to ExMCP via Native Service Dispatcher! ⚡"}]
        {:ok, %{"content" => content}, state}
      end

      def handle_mcp_request(_method, _params, state) do
        {:error, %{"code" => -32601, "message" => "Method not found"}, state}
      end
    end

    {:ok, _service} = NativeHelloService.start_link()
    Process.sleep(100)

    # Call the service
    {:ok, %{"content" => content}} = ExMCP.Native.call(:native_hello_service, "tools/call", %{
      "name" => "say_hello",
      "arguments" => %{"name" => "Developer"}
    })

    for %{"text" => text} <- content do
      IO.puts("   Response: #{text}")
    end
  end

  defp demo_stdio_transport do
    IO.puts("\n2. stdio Transport Demo")
    IO.puts("   (Standard MCP communication via subprocess)")
    IO.puts("   " <> String.duplicate("-", 40))

    # Check if stdio server script exists
    stdio_server_path = Path.join([__DIR__, "stdio_server.exs"])

    if File.exists?(stdio_server_path) do
      # Start a client that connects to the stdio server
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["elixir", stdio_server_path]
      )

      # Wait for initialization
      Process.sleep(500)

      # Call the tool
      case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "Developer"}) do
        {:ok, result} ->
          # Handle both possible response formats
          content = result["content"] || result[:content]
          if content do
            for item <- content do
              text = item["text"] || item[:text]
              if text, do: IO.puts("   Response: #{text}")
            end
          else
            IO.puts("   Response: #{inspect(result)}")
          end
        {:error, reason} ->
          IO.puts("   Error: #{inspect(reason)}")
      end

      # Stop the client
      GenServer.stop(client)
    else
      IO.puts("   Note: Run stdio_server.exs first to see this demo")
      IO.puts("   Command: elixir examples/getting_started/stdio_server.exs")
    end
  end

  defp demo_http_transport do
    IO.puts("\n3. HTTP Transport Demo (without SSE)")
    IO.puts("   (Network communication using HTTP responses)")
    IO.puts("   " <> String.duplicate("-", 40))

    # Try to connect to HTTP server on port 8321
    case ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:8321",
      endpoint: "/",
      use_sse: false
    ) do
      {:ok, client} ->
        # Wait for connection and initialization
        Process.sleep(1000)

        # Check if client is connected before calling
        case Process.info(client, :dictionary) do
          nil ->
            IO.puts("   Error: Client process died")
            IO.puts("   Note: HTTP server not running on port 8321")
            IO.puts("   To see this demo, run in another terminal:")
            IO.puts("   $ elixir examples/getting_started/simple_http_server.exs")
          _ ->
            # Call the tool
            case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "Developer"}) do
          {:ok, result} ->
            # Handle both possible response formats
            content = result["content"] || result[:content]
            if content do
              for item <- content do
                text = item["text"] || item[:text]
                if text, do: IO.puts("   Response: #{text}")
              end
            else
              IO.puts("   Response: #{inspect(result)}")
            end
          {:error, :not_connected} ->
            IO.puts("   Error: Failed to connect to HTTP server")
            IO.puts("   Make sure the server is running on port 8321")
          {:error, reason} ->
            IO.puts("   Error: #{inspect(reason)}")
            end
        end

        # Stop the client if it's still alive
        if Process.alive?(client), do: GenServer.stop(client)

      {:error, reason} ->
        IO.puts("   Note: HTTP server not running on port 8321")
        IO.puts("   To see this demo, run in another terminal:")
        IO.puts("   $ elixir examples/getting_started/simple_http_server.exs")
        IO.puts("   Error details: #{inspect(reason)}")
    end
  end

  defp demo_http_sse_transport do
    IO.puts("\n4. HTTP Transport Demo (with SSE)")
    IO.puts("   (Network communication using Server-Sent Events)")
    IO.puts("   " <> String.duplicate("-", 40))

    try do
      # Try to connect to SSE-enabled HTTP server on port 8322 with SSE enabled
      case ExMCP.Client.start_link(
        transport: :http,
        url: "http://localhost:8322",
        endpoint: "/",
        use_sse: true
      ) do
        {:ok, client} ->
          # Wait for connection and initialization
          Process.sleep(8000)

          # Check if client is connected before calling
          case Process.info(client, :dictionary) do
            nil ->
              IO.puts("   Error: Client process died")
              IO.puts("   Note: SSE server not running on port 8322")
              IO.puts("   To see this demo, run in another terminal:")
              IO.puts("   $ elixir examples/getting_started/sse_http_server.exs")
          _ ->
            # Call the tool
            case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "Developer"}) do
          {:ok, result} ->
            # Handle both possible response formats
            content = result["content"] || result[:content]
            if content do
              for item <- content do
                text = item["text"] || item[:text]
                if text, do: IO.puts("   Response: #{text}")
              end
            else
              IO.puts("   Response: #{inspect(result)}")
            end
          {:error, :not_connected} ->
            IO.puts("   Error: Failed to connect to HTTP server")
            IO.puts("   Make sure the server is running on port 8322")
          {:error, reason} ->
            IO.puts("   Error: #{inspect(reason)}")
            end
        end

        # Stop the client if it's still alive
        if Process.alive?(client), do: GenServer.stop(client)

      {:error, reason} ->
        IO.puts("   Note: SSE server not running on port 8322")
        IO.puts("   To see this demo, run in another terminal:")
        IO.puts("   $ elixir examples/getting_started/sse_http_server.exs")
        IO.puts("   Error details: #{inspect(reason)}")
      end
    catch
      :exit, reason ->
        IO.puts("   Note: SSE connection failed")
        IO.puts("   (Client process crashed due to connection issues)")
        IO.puts("   Error details: #{inspect(reason)}")
    end
  end
end

# Run the demo
HelloWorldDemo.run()
