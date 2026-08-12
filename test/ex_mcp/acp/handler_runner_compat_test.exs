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
    def handle_file_write(_session_id, _path, _content, state), do: {:error, :not_supported, state}

    @impl true
    def handle_terminal_request(_method, _params, _id, state), do: {:error, :not_supported, state}

    @impl true
    def terminate(_reason, _state), do: :ok
  end

  test "session_update/3 remains as an rc.6 compatibility wrapper" do
    {:ok, pid} = HandlerRunner.start_link(CaptureHandler, [pid: self()], self())

    assert :ok = HandlerRunner.session_update(pid, "s1", %{"sessionUpdate" => "agent_message_chunk"})
    assert_receive {:session_update, "s1", %{"sessionUpdate" => "agent_message_chunk"}}
  end
end
