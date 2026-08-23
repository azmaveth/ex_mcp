defmodule ExMCP.Server.TransportTest do
  use ExUnit.Case, async: false

  alias ExMCP.Server.Transport

  setup do
    # Save original configuration before any tests that might use STDIO transport
    original_level = Logger.level()
    original_stdio_mode = Application.get_env(:ex_mcp, :stdio_mode, false)

    on_exit(fn ->
      # Restore original logger configuration after STDIO tests
      Logger.configure(level: original_level)
      Application.put_env(:ex_mcp, :stdio_mode, original_stdio_mode)
    end)

    :ok
  end

  defmodule TestServer do
    use ExMCP.Server.Handler
    use ExMCP.Server.DSL, name: "test", version: "1.0.0"

    tool "test_tool", "A test tool" do
      input_schema(%{
        type: "object",
        properties: %{
          message: %{type: "string"}
        },
        required: ["message"]
      })

      run(fn %{"message" => message}, state ->
        {:ok, %{content: [%{"type" => "text", "text" => "Echo: #{message}"}]}, state}
      end)
    end
  end

  describe "start_server/4" do
    test "starts BEAM transport" do
      {:ok, pid} =
        Transport.start_server(TestServer, %{name: "test", version: "1.0.0"}, [],
          transport: :beam,
          name: :test_beam_server
        )

      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    test "returns error for unsupported transport" do
      assert {:error, {:unsupported_transport, :invalid}} =
               Transport.start_server(TestServer, %{}, [], transport: :invalid)
    end

    test "starts stdio transport with fallback" do
      # This should fall back to basic GenServer since StdioServer likely isn't available
      {:ok, pid} =
        Transport.start_server(TestServer, %{name: "test", version: "1.0.0"}, [],
          transport: :stdio,
          name: :test_stdio_server
        )

      assert Process.alive?(pid)
      GenServer.stop(pid)
    end
  end

  describe "individual transport functions" do
    test "start_beam_server/4" do
      {:ok, pid} =
        Transport.start_beam_server(TestServer, %{}, [], name: :test_beam_individual)

      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    test "start_stdio_server/4 with fallback" do
      {:ok, pid} = Transport.start_stdio_server(TestServer, %{}, [], name: :test_stdio_individual)

      assert Process.alive?(pid)
      GenServer.stop(pid)
    end
  end

  for adapter <- ExMCP.Test.HTTPAdapter.adapters() do
    describe "HTTP transport (#{adapter})" do
      @tag :requires_http
      test "starts HTTP transport" do
        {:ok, handle} =
          ExMCP.Test.HTTPAdapter.start_mcp_http(TestServer,
            adapter: unquote(adapter),
            server_info: %{name: "test", version: "1.0.0"}
          )

        assert is_pid(handle.pid)
        assert Process.alive?(handle.pid)
      end

      @tag :requires_http
      test "starts HTTP transport with SSE enabled" do
        {:ok, handle} =
          ExMCP.Test.HTTPAdapter.start_mcp_http(TestServer,
            adapter: unquote(adapter),
            server_info: %{name: "test", version: "1.0.0"},
            sse_enabled: true
          )

        assert is_pid(handle.pid)
      end

      @tag :requires_http
      test "start_http_server/4" do
        {:ok, handle} =
          ExMCP.Test.HTTPAdapter.start_mcp_http(TestServer,
            adapter: unquote(adapter),
            server_info: %{name: "test", version: "1.0.0"}
          )

        assert is_pid(handle.pid)
      end

      @tag :requires_http
      test "start_link with transport: :http" do
        {:ok, handle} =
          ExMCP.Test.HTTPAdapter.start_mcp_http(TestServer, adapter: unquote(adapter))

        assert is_pid(handle.pid)
      end
    end
  end

  describe "Cowboy listener identity" do
    @tag :requires_http
    test "default Cowboy start is a named singleton" do
      info = %{name: "test", version: "1.0.0"}
      port1 = ExMCP.Test.HTTPAdapter.free_port()
      port2 = ExMCP.Test.HTTPAdapter.free_port()

      {:ok, pid1} =
        Transport.start_http_server(TestServer, info, [], adapter: :cowboy, port: port1)

      on_exit(fn -> shutdown_cowboy(ExMCP.HttpPlug.HTTP) end)

      {:ok, pid2} =
        Transport.start_http_server(TestServer, info, [], adapter: :cowboy, port: port2)

      assert pid2 == pid1
      assert Process.alive?(pid1)
    end

    @tag :requires_http
    test "distinct ranch_ref values start isolated Cowboy listeners" do
      info = %{name: "test", version: "1.0.0"}
      port1 = ExMCP.Test.HTTPAdapter.free_port()
      port2 = ExMCP.Test.HTTPAdapter.free_port()
      ref1 = {:ex_mcp_test_cowboy, System.unique_integer([:positive])}
      ref2 = {:ex_mcp_test_cowboy, System.unique_integer([:positive])}

      {:ok, pid1} =
        Transport.start_http_server(TestServer, info, [],
          adapter: :cowboy,
          port: port1,
          ranch_ref: ref1
        )

      {:ok, pid2} =
        Transport.start_http_server(TestServer, info, [],
          adapter: :cowboy,
          port: port2,
          ranch_ref: ref2
        )

      on_exit(fn ->
        shutdown_cowboy(ref1)
        shutdown_cowboy(ref2)
      end)

      assert Process.alive?(pid1)
      assert Process.alive?(pid2)
      assert pid1 != pid2
      ExMCP.TestHelpers.wait_until(fn -> listening?(port1) end)
      ExMCP.TestHelpers.wait_until(fn -> listening?(port2) end)
    end
  end

  describe "Bandit listener identity" do
    @tag :requires_http
    test "ranch_ref is ignored and still starts" do
      {:ok, pid} =
        Transport.start_http_server(TestServer, %{name: "test", version: "1.0.0"}, [],
          adapter: :bandit,
          port: ExMCP.Test.HTTPAdapter.free_port(),
          ranch_ref: :ignored
        )

      on_exit(fn -> Transport.stop_http_server(pid) end)

      assert Process.alive?(pid)
    end
  end

  describe "server management" do
    test "stop_server/1 with pid" do
      {:ok, pid} = Transport.start_beam_server(TestServer, %{}, [], name: :test_stop_pid)

      assert Process.alive?(pid)
      assert :ok = Transport.stop_server(pid)
      refute Process.alive?(pid)
    end

    test "stop_server/1 with atom" do
      {:ok, pid} = Transport.start_beam_server(TestServer, %{}, [], name: :test_stop_atom)

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
      {:ok, pid} = Transport.start_beam_server(TestServer, %{}, [], name: :test_info)

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
      assert Map.has_key?(transports, :beam)

      # BEAM should always be available
      assert transports.beam.available == true

      # Others depend on dependencies
      assert is_boolean(transports.stdio.available)
      assert transports.http.available == true
    end

    test "prefers Bandit when both adapters are loaded" do
      assert {:ok, :bandit} = Transport.resolve_http_adapter(adapter: :auto)
      assert {:ok, :bandit} = Transport.resolve_http_adapter(adapter: :bandit)
      assert {:ok, :cowboy} = Transport.resolve_http_adapter(adapter: :cowboy)

      assert {:error, {:invalid_http_adapter, :ftp}} =
               Transport.resolve_http_adapter(adapter: :ftp)
    end
  end

  describe "Server integration" do
    test "start_link with transport: :beam" do
      {:ok, pid} = TestServer.start_link(transport: :beam, name: :test_server_beam)

      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    test "start_link defaults to BEAM transport" do
      {:ok, pid} = TestServer.start_link(name: :test_server_default)

      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    test "child_spec/1" do
      spec = TestServer.child_spec(transport: :beam)

      assert spec.id == TestServer
      assert spec.start == {TestServer, :start_link, [[transport: :beam]]}
      assert spec.type == :worker
      assert spec.restart == :permanent
      assert spec.shutdown == 500
    end
  end

  defp shutdown_cowboy(ref) do
    Plug.Cowboy.shutdown(ref)
  catch
    :exit, _ -> :ok
  end

  defp listening?(port) do
    case :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, active: false], 200) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        true

      {:error, _} ->
        false
    end
  end
end
