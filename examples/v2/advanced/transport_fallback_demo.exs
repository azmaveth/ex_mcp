#!/usr/bin/env elixir

# Transport Fallback Demo
# 
# This demonstrates the v2 transport abstraction and fallback mechanisms.
# Shows how the client can automatically fall back to alternative transports
# when the primary transport fails.

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule TransportFallbackDemo do
  def run do
    IO.puts("🚀 ExMCP v2 Transport Fallback Demo\n")
    
    demonstrate_single_transport()
    IO.puts("\n" <> String.duplicate("=", 50) <> "\n")
    demonstrate_transport_fallback()
    IO.puts("\n" <> String.duplicate("=", 50) <> "\n")
    demonstrate_parallel_fallback()
  end
  
  defp demonstrate_single_transport do
    IO.puts("📡 Single Transport Mode (Legacy Compatible)")
    IO.puts("Connecting with single HTTP transport...")
    
    # Start a test server first
    {:ok, server_pid} = start_test_server(8766)
    Process.sleep(200)
    
    try do
      # Single transport - works exactly like before
      {:ok, client} = ExMCP.SimpleClient.start_link(
        transport: :http,
        url: "http://localhost:8766"
      )
      
      {:ok, tools} = ExMCP.SimpleClient.list_tools(client)
      IO.puts("✅ Connected and found #{length(tools["tools"] || [])} tools")
      
      GenServer.stop(client)
    after
      GenServer.stop(server_pid)
    end
  end
  
  defp demonstrate_transport_fallback do
    IO.puts("🔄 Sequential Transport Fallback")
    IO.puts("Trying HTTP (will fail) → Stdio (will succeed)")
    
    # Start only the stdio mock server
    {:ok, server_pid} = start_test_server(8767)
    Process.sleep(200)
    
    try do
      # Multiple transports with fallback - HTTP will fail, stdio will work
      {:ok, client} = ExMCP.SimpleClient.start_link(
        transports: [
          # This will fail (wrong port)
          {ExMCP.Transport.HTTP, [url: "http://localhost:9999"]},
          # This will succeed  
          {DemoTransport, [port: 8767]}
        ],
        fallback_strategy: :sequential,
        max_retries: 1
      )
      
      {:ok, tools} = ExMCP.SimpleClient.list_tools(client)
      IO.puts("✅ Successfully fell back and found #{length(tools["tools"] || [])} tools")
      
      # Check which transport was actually used
      {:ok, status} = ExMCP.SimpleClient.get_status(client)
      IO.puts("📊 Final transport: #{inspect(status.server_info["name"])}")
      
      GenServer.stop(client)
    after
      GenServer.stop(server_pid)
    end
  end
  
  defp demonstrate_parallel_fallback do
    IO.puts("⚡ Parallel Transport Selection (Fastest Wins)")
    IO.puts("Racing multiple transports to find the fastest...")
    
    # Start test servers on different ports
    {:ok, server1} = start_test_server(8768)
    {:ok, server2} = start_test_server(8769)
    Process.sleep(200)
    
    try do
      start_time = System.monotonic_time(:millisecond)
      
      # All transports should work, fastest wins
      {:ok, client} = ExMCP.SimpleClient.start_link(
        transports: [
          {DemoTransport, [port: 8768, delay: 100]},  # Slower
          {DemoTransport, [port: 8769, delay: 10]},   # Faster
          {DemoTransport, [port: 8768, delay: 200]}   # Slowest
        ],
        fallback_strategy: :parallel,
        health_check_timeout: 2_000
      )
      
      elapsed = System.monotonic_time(:millisecond) - start_time
      IO.puts("✅ Connected in #{elapsed}ms (parallel selection)")
      
      {:ok, tools} = ExMCP.SimpleClient.list_tools(client)
      IO.puts("✅ Found #{length(tools["tools"] || [])} tools")
      
      GenServer.stop(client)
    after
      GenServer.stop(server1)
      GenServer.stop(server2)
    end
  end
  
  defp start_test_server(port) do
    Task.start_link(fn ->
      {:ok, listen_socket} = :gen_tcp.listen(port, [:binary, packet: :line, active: false, reuseaddr: true])
      IO.puts("🎯 Test server listening on port #{port}")
      accept_loop(listen_socket)
    end)
  end
  
  defp accept_loop(listen_socket) do
    {:ok, socket} = :gen_tcp.accept(listen_socket)
    spawn(fn -> handle_client(socket) end)
    accept_loop(listen_socket)
  end
  
  defp handle_client(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, data} ->
        # Simple echo server for demo
        :gen_tcp.send(socket, data)
        handle_client(socket)
      {:error, _} ->
        :gen_tcp.close(socket)
    end
  end
end

# Demo transport that can simulate delays and different behaviors
defmodule DemoTransport do
  @behaviour ExMCP.Transport
  
  def connect(opts) do
    port = Keyword.get(opts, :port, 8080)
    delay = Keyword.get(opts, :delay, 0)
    
    if delay > 0 do
      Process.sleep(delay)
    end
    
    # Simulate connection
    Agent.start_link(fn -> %{port: port, connected: true, messages: []} end)
  end
  
  def send(agent, data) do
    Agent.update(agent, fn state ->
      %{state | messages: [data | state.messages]}
    end)
    {:ok, agent}
  end
  
  def recv(agent, _timeout) do
    # Simple mock responses
    response = case Agent.get(agent, & &1.messages) do
      [msg | _] when is_binary(msg) ->
        case Jason.decode(msg) do
          {:ok, %{"method" => "initialize", "id" => id}} ->
            Jason.encode!(%{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "protocolVersion" => "2024-11-05",
                "capabilities" => %{},
                "serverInfo" => %{"name" => "DemoTransport", "version" => "1.0"}
              }
            })
          {:ok, %{"method" => "tools/list", "id" => id}} ->
            Jason.encode!(%{
              "jsonrpc" => "2.0", 
              "id" => id,
              "result" => %{"tools" => [%{"name" => "demo_tool", "description" => "Demo tool"}]}
            })
          _ ->
            nil
        end
      _ ->
        nil
    end
    
    if response do
      {:ok, response, agent}
    else
      {:error, :no_data}
    end
  end
  
  def close(agent) do
    Agent.stop(agent)
    {:ok, nil}
  end
  
  def controlling_process(_agent, _pid), do: :ok
  def send_message(_agent, _msg), do: {:error, :not_implemented}
  def receive_message(_agent), do: {:error, :not_implemented}
end

# Run the demo
TransportFallbackDemo.run()