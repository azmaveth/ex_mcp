defmodule ExMCP.ACP.NullRequestIdTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.{Agent, Client, Envelope, Protocol}
  alias ExMCP.ACP.Agent.Transport.Memory
  alias ExMCP.ACP.Client.Handler, as: ClientHandler

  defmodule PendingPromptAgent do
    @behaviour ExMCP.ACP.Agent.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_new_session(_params, _ctx, state), do: {:reply, "null-id-session", state}

    @impl true
    def handle_prompt(_session_id, _prompt, ctx, state) do
      send(state.test_pid, {:null_prompt_started, ctx.prompt_id})
      {:noreply, state}
    end
  end

  defmodule BlockingClientHandler do
    @behaviour ClientHandler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_session_update(session_id, update, state) do
      send(state.test_pid, {:null_id_update, session_id, update})
      {:ok, state}
    end

    @impl true
    def handle_permission_request(_session_id, _tool_call, _options, state) do
      send(state.test_pid, {:null_permission_started, self()})

      receive do
        {:finish_null_permission, outcome} -> {:ok, outcome, state}
      end
    end
  end

  describe "ACP RequestId parsing" do
    test "distinguishes an explicit null request id from an absent notification id" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "session/cancel",
        "params" => %{"sessionId" => "s"},
        "id" => nil
      }

      notification = Map.delete(request, "id")

      assert {:request, "session/cancel", %{"sessionId" => "s"}, nil} =
               Protocol.parse_message(request)

      assert {:notification, "session/cancel", %{"sessionId" => "s"}} =
               Protocol.parse_message(notification)

      assert {:result, %{"ok" => true}, nil} =
               Protocol.parse_message(%{
                 "jsonrpc" => "2.0",
                 "result" => %{"ok" => true},
                 "id" => nil
               })

      assert Map.has_key?(Envelope.request("test", %{}, nil), "id")
      refute Map.has_key?(Envelope.notification("test"), "id")
    end
  end

  describe "native Agent null request ids" do
    test "responds with null, rejects a concurrent duplicate, and preserves notifications" do
      {agent, transport, peer} = start_raw_agent()

      send_raw_request(transport, nil, "initialize", initialize_params())
      assert %{"id" => nil, "result" => %{"protocolVersion" => 1}} = receive_raw(transport)

      send_raw_notification(transport, "initialized", %{})
      wait_until_processed(agent)
      assert outbound_queue_length(peer, :client) == 0

      prompt = %{
        "sessionId" => "null-id-session",
        "prompt" => [%{"type" => "text", "text" => "wait"}]
      }

      send_raw_request(transport, nil, "session/prompt", prompt)
      assert_receive {:null_prompt_started, nil}

      send_raw_request(transport, nil, "session/prompt", prompt)

      assert %{"id" => nil, "error" => %{"code" => -32_600}} = receive_raw(transport)
      refute_receive {:null_prompt_started, nil}, 50

      assert :ok = Agent.finish_prompt(agent, nil, "end_turn")
      assert %{"id" => nil, "result" => %{"stopReason" => "end_turn"}} = receive_raw(transport)

      state = :sys.get_state(agent)
      assert state.pending_callbacks == %{}
      assert state.pending_prompts == %{}
      assert state.active_prompts == %{}
    end

    test "a null prompt id is released after timeout and may be reused" do
      {agent, transport, _peer} = start_raw_agent(pending_request_timeout: 30)

      send_raw_request(transport, 1, "initialize", initialize_params())
      assert %{"id" => 1, "result" => %{}} = receive_raw(transport)

      prompt = %{
        "sessionId" => "null-id-session",
        "prompt" => [%{"type" => "text", "text" => "wait"}]
      }

      send_raw_request(transport, nil, "session/prompt", prompt)
      assert_receive {:null_prompt_started, nil}

      assert %{"id" => nil, "error" => %{"message" => "Prompt timed out"}} =
               receive_raw(transport)

      state = :sys.get_state(agent)
      assert state.pending_prompts == %{}
      assert state.active_prompts == %{}

      send_raw_request(transport, nil, "session/prompt", prompt)
      assert_receive {:null_prompt_started, nil}
      assert :ok = Agent.finish_prompt(agent, nil, "end_turn")
      assert %{"id" => nil, "result" => %{}} = receive_raw(transport)
    end

    test "an outbound null request resolves, rejects duplicates, and cleans up on timeout" do
      {agent, transport, peer} = start_raw_agent(pending_request_timeout: 30)

      send_raw_request(transport, 1, "initialize", initialize_params())
      assert %{"id" => 1, "result" => %{}} = receive_raw(transport)

      request =
        Envelope.request(
          "session/request_permission",
          %{
            "sessionId" => "null-id-session",
            "toolCall" => %{"toolCallId" => "tool-1"},
            "options" => []
          },
          nil
        )

      first = Task.async(fn -> GenServer.call(agent, {:client_request, request, :permission}) end)
      assert %{"id" => nil, "method" => "session/request_permission"} = receive_raw(transport)

      assert {:error, :duplicate_request_id} =
               GenServer.call(agent, {:client_request, request, :permission})

      assert outbound_queue_length(peer, :client) == 0

      send_raw_response(transport, nil, %{"outcome" => %{"outcome" => "cancelled"}})

      assert {:ok, %{"outcome" => %{"outcome" => "cancelled"}}} = Task.await(first)
      assert :sys.get_state(agent).pending_client_requests == %{}

      timed_out =
        Task.async(fn ->
          GenServer.call(agent, {:client_request, request, :permission}, 1_000)
        end)

      assert %{"id" => nil, "method" => "session/request_permission"} = receive_raw(transport)
      assert {:error, :request_timeout} = Task.await(timed_out)

      assert %{
               "method" => "$/cancel_request",
               "params" => %{"requestId" => nil}
             } = receive_raw(transport)

      assert :sys.get_state(agent).pending_client_requests == %{}
    end
  end

  describe "native Client null request ids" do
    test "responds with null, rejects a concurrent duplicate, and accepts absent-id updates" do
      {client, transport, peer} = start_raw_client()
      authorize_session(client)

      request = permission_request(nil)
      send_raw(transport, request)
      assert_receive {:null_permission_started, handler_pid}

      send_raw(transport, request)
      assert %{"id" => nil, "error" => %{"code" => -32_600}} = receive_raw(transport)
      refute_receive {:null_permission_started, _pid}, 50

      send(handler_pid, {:finish_null_permission, %{"outcome" => "cancelled"}})

      assert %{"id" => nil, "result" => %{"outcome" => %{"outcome" => "cancelled"}}} =
               receive_raw(transport)

      assert :sys.get_state(client).pending_agent_requests == %{}

      update = %{
        "jsonrpc" => "2.0",
        "method" => "session/update",
        "params" => %{
          "sessionId" => "null-id-session",
          "update" => %{
            "sessionUpdate" => "agent_message_chunk",
            "content" => %{"type" => "text", "text" => "x"}
          }
        }
      }

      send_raw(transport, update)

      assert_receive {:null_id_update, "null-id-session",
                      %{"sessionUpdate" => "agent_message_chunk"}}

      assert outbound_queue_length(peer, :agent) == 0
    end

    test "a timed-out null handler request is removed and can be reused" do
      {client, transport, _peer} = start_raw_client(handler_request_timeout: 30)
      authorize_session(client)

      send_raw(transport, permission_request(nil))
      assert_receive {:null_permission_started, handler_pid}

      assert %{"id" => nil, "error" => %{"message" => "Client handler timed out"}} =
               receive_raw(transport)

      assert :sys.get_state(client).pending_agent_requests == %{}

      send(handler_pid, {:finish_null_permission, %{"outcome" => "cancelled"}})
      :sys.get_state(handler_pid)

      send_raw(transport, permission_request(nil))
      assert_receive {:null_permission_started, next_handler_pid}
      send(next_handler_pid, {:finish_null_permission, %{"outcome" => "cancelled"}})

      assert %{"id" => nil, "result" => %{"outcome" => %{"outcome" => "cancelled"}}} =
               receive_raw(transport)

      assert :sys.get_state(client).pending_agent_requests == %{}
    end
  end

  defp start_raw_agent(opts \\ []) do
    {:ok, peer} = Memory.new_pair()

    {:ok, agent} =
      Agent.start_link(
        Keyword.merge(
          [
            handler: PendingPromptAgent,
            handler_opts: [test_pid: self()],
            transport: {:memory, peer}
          ],
          opts
        )
      )

    {:ok, transport} = Memory.connect(peer: peer, role: :client)
    {agent, transport, peer}
  end

  defp start_raw_client(opts \\ []) do
    {:ok, peer} = Memory.new_pair()
    {:ok, transport} = Memory.connect(peer: peer, role: :agent)
    test_pid = self()

    initialize_task =
      Task.async(fn ->
        assert %{"id" => initialize_id, "method" => "initialize"} = receive_raw(transport)

        send_raw_response(transport, initialize_id, %{
          "agentInfo" => %{"name" => "raw", "version" => "1"},
          "agentCapabilities" => %{},
          "authMethods" => [],
          "protocolVersion" => 1
        })
      end)

    assert {:ok, client} =
             Client.start_link(
               Keyword.merge(
                 [
                   transport_mod: Memory,
                   peer: peer,
                   role: :client,
                   handler: BlockingClientHandler,
                   handler_opts: [test_pid: test_pid],
                   event_listener: test_pid
                 ],
                 opts
               )
             )

    assert {:ok, %Memory{}} = Task.await(initialize_task)
    {client, transport, peer}
  end

  defp authorize_session(client) do
    :sys.replace_state(client, fn state ->
      %{state | sessions: %{"null-id-session" => %{roots: ["/tmp"]}}}
    end)
  end

  defp initialize_params do
    %{"protocolVersion" => 1, "clientCapabilities" => %{}}
  end

  defp permission_request(id) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/request_permission",
      "params" => %{
        "sessionId" => "null-id-session",
        "toolCall" => %{"toolCallId" => "tool-1"},
        "options" => []
      },
      "id" => id
    }
  end

  defp send_raw_request(transport, id, method, params) do
    send_raw(transport, %{"jsonrpc" => "2.0", "id" => id, "method" => method, "params" => params})
  end

  defp send_raw_notification(transport, method, params) do
    send_raw(transport, %{"jsonrpc" => "2.0", "method" => method, "params" => params})
  end

  defp send_raw_response(transport, id, result) do
    send_raw(transport, %{"jsonrpc" => "2.0", "id" => id, "result" => result})
  end

  defp send_raw(transport, message) do
    assert {:ok, _transport} = Memory.send_message(Jason.encode!(message), transport)
  end

  defp receive_raw(transport) do
    assert {:ok, message, _transport} = Memory.receive_message(transport)
    Jason.decode!(message)
  end

  defp outbound_queue_length(peer, role) do
    peer
    |> :sys.get_state()
    |> get_in([:queues, role])
    |> :queue.len()
  end

  defp wait_until_processed(pid) do
    _state = :sys.get_state(pid)
    Process.sleep(10)
  end
end
