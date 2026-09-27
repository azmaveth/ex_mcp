defmodule ExMCP.Transport.StdioProcessGroupTest do
  @moduledoc """
  ERTS starts every port program as the leader of its own process group.
  With `process_group: true` the stdio transport signals that whole group, so
  a server's own children stop with it; by default only the server process is
  signalled. The command is resolved against the child's `PATH`.
  """

  use ExUnit.Case, async: true

  import ExMCP.TestHelpers, only: [wait_until: 2]

  alias ExMCP.Transport.Stdio

  @moduletag :tmp_dir

  setup %{tmp_dir: tmp_dir} do
    if match?({:win32, _}, :os.type()), do: raise("process groups are a Unix feature")
    %{pid_file: Path.join(tmp_dir, "child.pid")}
  end

  describe "close/1" do
    test "by default signals the server only, and its child outlives it", %{pid_file: pid_file} do
      {:ok, state} = Stdio.connect(command: server_with_child(pid_file))
      child = await_pid(pid_file)
      on_exit(fn -> kill(child) end)

      :ok = Stdio.close(state)

      refute alive?(state.os_pid)
      assert alive?(child)
    end

    test "with process_group: true stops the server's children too", %{pid_file: pid_file} do
      {:ok, state} = Stdio.connect(command: server_with_child(pid_file), process_group: true)
      child = await_pid(pid_file)
      on_exit(fn -> kill(child) end)

      :ok = Stdio.close(state)

      refute alive?(state.os_pid)
      refute alive?(child)
    end

    test "with process_group: true kills a child that ignores SIGTERM", %{pid_file: pid_file} do
      command = sh("(trap '' TERM; exec sleep 30) & echo $! > \"$1\"; wait", pid_file)
      {:ok, state} = Stdio.connect(command: command, process_group: true)
      child = await_pid(pid_file)
      on_exit(fn -> kill(child) end)

      :ok = Stdio.close(state)

      refute alive?(child)
    end
  end

  describe "a server that exits on its own with process_group: true" do
    test "has the children it left behind stopped (push)", %{pid_file: pid_file} do
      command = sh(orphaning_server(), pid_file)
      {:ok, state} = Stdio.connect(command: command, process_group: true)
      {:ok, _state} = Stdio.subscribe(self(), state)
      child = await_pid(pid_file)
      on_exit(fn -> kill(child) end)

      assert_receive {:transport_closed, {:process_exited, 0}}, 2_000
      wait_until(fn -> not alive?(child) end, timeout: 2_000)
    end

    test "has the children it left behind stopped (pull)", %{pid_file: pid_file} do
      command = sh(orphaning_server(), pid_file)
      {:ok, state} = Stdio.connect(command: command, process_group: true)
      child = await_pid(pid_file)
      on_exit(fn -> kill(child) end)

      assert {:error, _exited} = Stdio.receive_message(state, 2_000)
      wait_until(fn -> not alive?(child) end, timeout: 2_000)
    end
  end

  test "rejects a non-boolean process_group" do
    assert {:error, {:invalid_process_group, :yes}} =
             Stdio.connect(command: ["true"], process_group: :yes)
  end

  test "resolves the command against the child's PATH", %{tmp_dir: tmp_dir} do
    bin = Path.join(tmp_dir, "bin")
    File.mkdir_p!(bin)
    tool = Path.join(bin, "ex-mcp-path-probe")
    File.write!(tool, "#!/bin/sh\nexit 7\n")
    File.chmod!(tool, 0o755)

    # Not on the VM's PATH, only on the child's.
    assert System.find_executable("ex-mcp-path-probe") == nil

    assert {:ok, %Stdio{port: port}} =
             Stdio.connect(
               command: ["ex-mcp-path-probe"],
               env: [{"PATH", "#{bin}:/usr/bin:/bin"}]
             )

    assert_receive {^port, {:exit_status, 7}}, 2_000
  end

  defp server_with_child(pid_file), do: sh("sleep 30 & echo $! > \"$1\"; wait", pid_file)

  # The child lets go of the server's stdout: ERTS reports the server's exit
  # only once that pipe reaches EOF, so a child still holding it keeps the
  # connection open (and is then, in effect, the server).
  defp orphaning_server, do: "sleep 30 </dev/null >/dev/null 2>&1 & echo $! > \"$1\"; exit 0"

  # The pid file's path (derived from the test name) goes in as $1, not into
  # the script text, so no character in it can change the script.
  defp sh(script, pid_file), do: ["sh", "-c", script, "sh", pid_file]

  defp await_pid(pid_file) do
    wait_until(fn -> match?({:ok, <<_, _::binary>>}, File.read(pid_file)) end, timeout: 2_000)
    pid_file |> File.read!() |> String.trim() |> String.to_integer()
  end

  defp alive?(os_pid) do
    {_output, status} = System.cmd("kill", ["-0", "#{os_pid}"], stderr_to_stdout: true)
    status == 0
  end

  defp kill(os_pid), do: System.cmd("kill", ["-KILL", "#{os_pid}"], stderr_to_stdout: true)
end
