defmodule ExMCP.Server.TransportTest do
  use ExUnit.Case, async: true

  alias ExMCP.Server.Transport

  defmodule TestServer do
    use ExMCP.ServerV2

    deftool "test_tool" do
      tool_description("A test tool")
      args do
        field(:message, :string, required: true)
      end
    end

    @impl true
    def handle_tool_call("test_tool", %{"message" => message}, state) do
      result = %{content: [%{"type" => "text", "text" => "Echo: #{message}"}]}
      {:ok, result, state}
    end
  end

  describe "start_server/4" do
    test "starts native transport by default" do
      {:ok, pid} = Transport.start_server(TestServer, %{name: "test", version: "1.0.0"}, [], 
        transport: :native, name: :test_native_server)

      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    test "returns error for unsupported transport" do
      assert {:error, {:unsupported_transport, :invalid}} = 
        Transport.start_server(TestServer, %{}, [], transport: :invalid)
    end

    @tag :requires_http
    test "starts HTTP transport" do
      # This test requires Cowboy to be available
      case Code.ensure_loaded?(Plug.Cowboy) do
        {:module, _} ->
          {:ok, pid} = Transport.start_server(TestServer, %{name: "test", version: "1.0.0"}, [], 
            transport: :http, port: 0)  # Use port 0 for auto-assignment
          
          assert is_pid(pid)
          Plug.Cowboy.shutdown(pid)

        {:error, _} ->
          # Skip if Cowboy not available
          :skip
      end
    end

    @tag :requires_http
    test "starts SSE transport" do
      case Code.ensure_loaded?(Plug.Cowboy) do
        {:module, _} ->
          {:ok, pid} = Transport.start_server(TestServer, %{name: "test", version: "1.0.0"}, [], 
            transport: :sse, port: 0)
          
          assert is_pid(pid)
          Plug.Cowboy.shutdown(pid)

        {:error, _} ->
          :skip
      end
    end

    test "starts stdio transport with fallback" do
      # This should fall back to basic GenServer since StdioServer likely isn't available
      {:ok, pid} = Transport.start_server(TestServer, %{name: "test", version: "1.0.0"}, [], 
        transport: :stdio, name: :test_stdio_server)

      assert Process.alive?(pid)
      GenServer.stop(pid)
    end
  end

  describe "individual transport functions" do
    test "start_native_server/4" do
      {:ok, pid} = Transport.start_native_server(TestServer, %{}, [], name: :test_native_individual)
      
      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    test "start_stdio_server/4 with fallback" do
      {:ok, pid} = Transport.start_stdio_server(TestServer, %{}, [], name: :test_stdio_individual)
      
      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    @tag :requires_http
    test "start_http_server/4" do
      case Code.ensure_loaded?(Plug.Cowboy) do
        {:module, _} ->
          {:ok, pid} = Transport.start_http_server(TestServer, %{name: "test", version: "1.0.0"}, [], 
            port: 0)
          
          assert is_pid(pid)
          Plug.Cowboy.shutdown(pid)

        {:error, _} ->
          :skip
      end
    end
  end

  describe "server management" do
    test "stop_server/1 with pid" do
      {:ok, pid} = Transport.start_native_server(TestServer, %{}, [], name: :test_stop_pid)
      
      assert Process.alive?(pid)
      assert :ok = Transport.stop_server(pid)
      refute Process.alive?(pid)
    end

    test "stop_server/1 with atom" do
      {:ok, pid} = Transport.start_native_server(TestServer, %{}, [], name: :test_stop_atom)
      
      # Wait for process to be registered
      Process.sleep(10)
      assert Process.whereis(:test_stop_atom) == pid
      assert :ok = Transport.stop_server(:test_stop_atom)
      
      # Wait for process to stop
      Process.sleep(10)
      assert Process.whereis(:test_stop_atom) == nil
    end

    test "stop_server/1 with non-existent process" do
      assert :ok = Transport.stop_server(:non_existent_server)
    end

    test "server_info/1" do
      {:ok, pid} = Transport.start_native_server(TestServer, %{}, [], name: :test_info)
      
      # The server info depends on the implementation
      # For now, just test that it doesn't crash
      result = Transport.server_info(pid)
      assert is_tuple(result)
      
      GenServer.stop(pid)
    end
  end

  describe "list_transports/0" do
    test "returns available transports" do
      transports = Transport.list_transports()
      
      assert is_map(transports)
      assert Map.has_key?(transports, :stdio)
      assert Map.has_key?(transports, :http)
      assert Map.has_key?(transports, :sse)
      assert Map.has_key?(transports, :native)
      
      # Native should always be available
      assert transports.native.available == true
      
      # Others depend on dependencies
      assert is_boolean(transports.stdio.available)
      assert is_boolean(transports.http.available)
      assert is_boolean(transports.sse.available)
    end
  end

  describe "ServerV2 integration" do
    test "start_link with transport: :native" do
      {:ok, pid} = TestServer.start_link(transport: :native, name: :test_server_native)
      
      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    @tag :requires_http  
    test "start_link with transport: :http" do
      case Code.ensure_loaded?(Plug.Cowboy) do
        {:module, _} ->
          {:ok, pid} = TestServer.start_link(transport: :http, port: 0)
          
          assert is_pid(pid)
          Plug.Cowboy.shutdown(pid)

        {:error, _} ->
          :skip
      end
    end

    test "start_link defaults to native transport" do
      {:ok, pid} = TestServer.start_link(name: :test_server_default)
      
      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    test "child_spec/1" do
      spec = TestServer.child_spec(transport: :native)
      
      assert spec.id == TestServer
      assert spec.start == {TestServer, :start_link, [[transport: :native]]}
      assert spec.type == :worker
      assert spec.restart == :permanent
      assert spec.shutdown == 500
    end
  end
end