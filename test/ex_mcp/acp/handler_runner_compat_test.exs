defmodule ExMCP.ACP.Client.HandlerRunnerCompatTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Client.HandlerRunner

  defmodule CaptureHandler do
    @behaviour ExMCP.ACP.Client.Handler

    @impl true
    def init(opts), do: {:ok, %{pid: Keyword.fetch!(opts, :pid)}}

    @impl true
    def handle_session_update(session_id, update, state) do
      send(state.pid, {:session_update, session_id, update})
      {:ok, state}
    end

    @impl true
    def handle_permission_request(_session_id, _tool_call, _options, state),
      do: {:ok, %{"outcome" => "cancelled"}, state}

    @impl true
    def handle_file_read(_session_id, _path, _opts, state), do: {:error, :not_supported, state}

    @impl true
    def handle_file_write(_session_id, _path, _content, state),
      do: {:error, :not_supported, state}

    @impl true
    def handle_terminal_request(_method, _params, _id, state), do: {:error, :not_supported, state}

    @impl true
    def terminate(_reason, _state), do: :ok
  end

  defmodule SlowHandler do
    @behaviour ExMCP.ACP.Client.Handler

    @impl true
    def init(_opts), do: {:ok, %{}}

    @impl true
    def handle_session_update(_session_id, _update, state) do
      Process.sleep(200)
      {:ok, state}
    end

    @impl true
    def handle_permission_request(_session_id, _tool_call, _options, state),
      do: {:ok, %{"outcome" => "cancelled"}, state}

    @impl true
    def handle_file_read(_session_id, _path, _opts, state), do: {:error, :not_supported, state}

    @impl true
    def handle_file_write(_session_id, _path, _content, state),
      do: {:error, :not_supported, state}

    @impl true
    def handle_terminal_request(_method, _params, _id, state), do: {:error, :not_supported, state}

    @impl true
    def terminate(_reason, _state), do: :ok
  end

  test "session_update/3 remains as an rc.6 compatibility wrapper" do
    {:ok, pid} = HandlerRunner.start_link(CaptureHandler, [pid: self()], self())

    assert :ok =
             HandlerRunner.session_update(pid, "s1", %{"sessionUpdate" => "agent_message_chunk"})

    assert_receive {:session_update, "s1", %{"sessionUpdate" => "agent_message_chunk"}}
  end

  test "session_update/3 returns :ok when the runner is already dead" do
    {:ok, pid} = HandlerRunner.start_link(CaptureHandler, [pid: self()], self())
    Process.exit(pid, :kill)
    ref = Process.monitor(pid)
    assert_receive {:DOWN, ^ref, :process, ^pid, _}

    assert :dropped =
             HandlerRunner.session_update(pid, "s1", %{"sessionUpdate" => "dead"}, 32, 8_388_608)

    assert :ok = HandlerRunner.session_update(pid, "s1", %{"sessionUpdate" => "dead"})
  end

  test "session_update/3 returns :ok when bounded /5 delivery would drop" do
    {:ok, pid} = HandlerRunner.start_link(SlowHandler, [], self())
    update = %{"sessionUpdate" => "agent_message_chunk", "content" => %{"text" => "x"}}

    # Fill the mailbox while the handler is blocked in handle_cast.
    assert :ok = HandlerRunner.session_update(pid, "s1", update, 1, 64)
    assert :ok = HandlerRunner.session_update(pid, "s1", update, 1, 64)
    assert :dropped = HandlerRunner.session_update(pid, "s1", update, 1, 64)
    assert :ok = HandlerRunner.session_update(pid, "s1", update)
  end
end
