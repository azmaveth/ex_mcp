defmodule ExMCP.WirePrivacyTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias ExMCP.ACP.{Agent, Client}
  alias ExMCP.Client.RequestHandler
  alias ExMCP.HttpPlug.SSEHandler
  alias ExMCP.MessageProcessor
  alias ExMCP.Protocol.RequestTracker
  alias ExMCP.Server.{HandlerServer, StdioServer}

  defmodule FailingNotificationHandler do
    def handle_log_message(_request_id, _params, state),
      do: {:error, state.secret, state}
  end

  defmodule RaisingNotificationHandler do
    def handle_log_message(_request_id, _params, state), do: raise(state.secret)
  end

  defmodule CaptureConn do
    @behaviour ExMCP.HttpPlug.SSEConnection

    defstruct [:test_pid]

    @impl true
    def chunk(%__MODULE__{test_pid: test_pid} = conn, data) do
      send(test_pid, {:sse_chunk, data})
      {:ok, conn}
    end

    @impl true
    def get_req_header(_conn, _header), do: []
  end

  setup do
    previous_level = Logger.level()
    Logger.configure(level: :debug)
    on_exit(fn -> Logger.configure(level: previous_level) end)
    :ok
  end

  test "peer-controlled IDs and malformed frames are summarized in logs" do
    secret = "wire-log-secret-#{System.unique_integer([:positive])}"

    log =
      capture_log([level: :debug], fn ->
        notification = %{"jsonrpc" => "2.0", "method" => secret, "params" => secret}
        _conn = MessageProcessor.process(MessageProcessor.new(notification), %{})

        state = RequestTracker.cancel_request(secret, RequestTracker.init())
        assert {:noreply, _state} = RequestTracker.handle_cancellation(secret, state)

        assert {:noreply, _state} =
                 HandlerServer.handle_info({:cancelled, secret}, %{pending_requests: %{}})

        assert {:noreply, _state} = StdioServer.handle_info({:stdin_line, secret}, %{})

        agent_state = %Agent{pending_client_requests: %{}}

        assert {:noreply, _state} =
                 Agent.handle_info(
                   {:transport_message,
                    Jason.encode!(%{"jsonrpc" => "2.0", "id" => secret, "result" => %{}})},
                   agent_state
                 )

        client_state = %Client{pending_requests: %{}}

        assert {:noreply, _state} =
                 Client.handle_info(
                   {:transport_message,
                    Jason.encode!(%{"jsonrpc" => "2.0", "id" => secret, "result" => %{}})},
                   client_state
                 )

        assert {:noreply, _state} = SSEHandler.handle_info({:unexpected, secret}, %SSEHandler{})
      end)

    refute log =~ secret
    assert log =~ "Request not found in pending requests"
    assert log =~ "unknown client request"
  end

  test "client notification handler errors and exception messages stay out of logs" do
    secret = "handler-log-secret-#{System.unique_integer([:positive])}"

    log =
      capture_log([level: :debug], fn ->
        error_state = %ExMCP.Client{
          client_handler: {FailingNotificationHandler, %{secret: secret}}
        }

        assert {:noreply, _state} =
                 RequestHandler.handle_request_stream_message(
                   secret,
                   %{"method" => "notifications/message", "params" => %{"secret" => secret}},
                   error_state
                 )

        raising_state = %ExMCP.Client{
          client_handler: {RaisingNotificationHandler, %{secret: secret}}
        }

        assert {:noreply, _state} =
                 RequestHandler.handle_request_stream_message(
                   secret,
                   %{"method" => "notifications/message", "params" => %{"secret" => secret}},
                   raising_state
                 )
      end)

    refute log =~ secret
    assert log =~ "Client request notification handler failed"
    assert log =~ "Client request notification handler raised"
  end

  test "arbitrary SSE error terms are replaced before reaching the peer" do
    secret = "sse-peer-secret-#{System.unique_integer([:positive])}"

    {:ok, handler} =
      SSEHandler.start_link(%CaptureConn{test_pid: self()}, "session", %{conn_module: CaptureConn})

    assert_receive {:sse_chunk, _connected}
    ref = Process.monitor(handler)
    SSEHandler.send_error(handler, {:transport_error, secret})

    assert_receive {:sse_chunk, error_chunk}
    assert error_chunk =~ "transport_error: internal error"
    refute error_chunk =~ secret
    assert_receive {:DOWN, ^ref, :process, ^handler, _reason}
  end
end
