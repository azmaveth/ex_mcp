defmodule ExMCP.Client.EstablishDeadlineTest do
  @moduledoc """
  A host that accepts the connection and then answers nothing must not hold
  `ExMCP.Client.start_link/1` past its timeouts: `:era_probe_timeout` bounds
  the probe exchange and `:handshake_timeout` the initialize exchange, a
  synchronous HTTP POST included, and `:establish_timeout` bounds the whole
  establishment. A failed attempt closes the transport it opened.
  """

  use ExUnit.Case, async: true

  import ExMCP.TestHelpers, only: [wait_until: 2]

  alias ExMCP.Client

  # Far below the 30 s a synchronous POST was allowed before, and loose
  # enough for a loaded CI host.
  @bound 5_000

  setup do
    Process.flag(:trap_exit, true)
    :ok
  end

  describe "a silent HTTP host" do
    setup do
      {port, connections} = start_silent_listener()
      %{url: "http://127.0.0.1:#{port}/mcp", connections: connections}
    end

    test "legacy_only returns within :handshake_timeout", %{url: url} do
      {elapsed, result} =
        timed(fn -> start_client(url, protocol_mode: :legacy_only, handshake_timeout: 300) end)

      assert {:error, :handshake_timeout} = result
      assert elapsed < @bound
    end

    test "prefer_modern bounds the probe by :era_probe_timeout, then the fallback", %{
      url: url,
      connections: connections
    } do
      {elapsed, result} =
        timed(fn ->
          start_client(url,
            protocol_mode: :prefer_modern,
            era_probe_timeout: 200,
            handshake_timeout: 300
          )
        end)

      assert {:error, _reason} = result
      assert elapsed < @bound
      # The probe timed out on its own bound and counted as fallback
      # evidence, so initialize was tried on a second connection.
      wait_until(fn -> Agent.get(connections, & &1) == 2 end, timeout: 2_000)
    end

    test "modern_only returns within :era_probe_timeout", %{url: url, connections: connections} do
      {elapsed, result} =
        timed(fn ->
          start_client(url, protocol_mode: :modern_only, era_probe_timeout: 200)
        end)

      assert {:error, _reason} = result
      assert elapsed < @bound
      wait_until(fn -> Agent.get(connections, & &1) == 1 end, timeout: 2_000)
    end

    test ":establish_timeout bounds the whole establishment", %{url: url} do
      {elapsed, result} =
        timed(fn ->
          start_client(url,
            protocol_mode: :legacy_only,
            handshake_timeout: 30_000,
            establish_timeout: 300
          )
        end)

      assert {:error, :establish_timeout} = result
      assert elapsed < @bound
    end

    test ":establish_timeout covers connection retries too", %{url: url} do
      {elapsed, result} =
        timed(fn ->
          start_client(url,
            protocol_mode: :legacy_only,
            handshake_timeout: 30_000,
            establish_timeout: 400,
            retry_policy: [max_attempts: 5, initial_delay: 50, jitter: false]
          )
        end)

      assert {:error, :establish_timeout} = result
      assert elapsed < @bound
    end
  end

  test "an invalid :establish_timeout is refused" do
    assert {:error, {:invalid_establish_timeout, -1}} =
             Client.start_link(
               transport: :test,
               establish_timeout: -1,
               health_check_interval: nil,
               reconnect: false
             )
  end

  @tag :tmp_dir
  test "a stdio handshake that times out stops the child it spawned", %{tmp_dir: tmp_dir} do
    pid_file = Path.join(tmp_dir, "child.pid")

    # The child ignores stdin, so closing the port alone would not stop it.
    command = ["sh", "-c", "echo $$ > #{pid_file}; exec sleep 30"]

    assert {:error, _reason} =
             Client.start_link(
               transport: :stdio,
               command: command,
               protocol_mode: :legacy_only,
               handshake_timeout: 300,
               health_check_interval: nil,
               reconnect: false
             )

    os_pid = pid_file |> File.read!() |> String.trim()
    refute os_process_alive?(os_pid)
  end

  defp start_client(url, opts) do
    Client.start_link(
      [
        transport: :http,
        url: url,
        use_sse: false,
        health_check_interval: nil,
        reconnect: false
      ] ++ opts
    )
  end

  defp timed(fun) do
    started = System.monotonic_time(:millisecond)
    result = fun.()
    {System.monotonic_time(:millisecond) - started, result}
  end

  defp os_process_alive?(os_pid) do
    {_output, status} = System.cmd("kill", ["-0", os_pid], stderr_to_stdout: true)
    status == 0
  end

  # Accepts every connection and reads from it, but never writes a byte.
  defp start_silent_listener do
    {:ok, listener} =
      :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true, ip: {127, 0, 0, 1}])

    {:ok, port} = :inet.port(listener)
    {:ok, connections} = Agent.start_link(fn -> 0 end)

    acceptor =
      spawn_link(fn -> accept_loop(listener, connections, []) end)

    :ok = :gen_tcp.controlling_process(listener, acceptor)

    on_exit(fn ->
      Process.exit(acceptor, :kill)
      :gen_tcp.close(listener)
    end)

    {port, connections}
  end

  defp accept_loop(listener, connections, sockets) do
    case :gen_tcp.accept(listener) do
      {:ok, socket} ->
        Agent.update(connections, &(&1 + 1))
        accept_loop(listener, connections, [socket | sockets])

      {:error, _closed} ->
        :ok
    end
  end
end
