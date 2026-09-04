defmodule ExMCP.ACP.ClientMessageContextTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Agent.Transport.Memory
  alias ExMCP.ACP.Client

  defmodule ContextHandler do
    @behaviour ExMCP.ACP.Client.Handler

    @impl true
    def init(opts), do: {:ok, %{parent: Keyword.fetch!(opts, :parent), sequence: 0}}

    @impl true
    def handle_session_update(session_id, update, state) do
      send(state.parent, {:legacy_update, session_id, update})
      {:ok, state}
    end

    @impl true
    def handle_session_update(session_id, update, message, state) do
      send(state.parent, {:context_update, session_id, update, message, state.sequence})
      {:ok, %{state | sequence: state.sequence + 1}}
    end

    @impl true
    def handle_permission_request(_session_id, _tool_call, _options, state) do
      send(state.parent, :legacy_permission)
      {:ok, %{"outcome" => "cancelled"}, state}
    end

    @impl true
    def handle_permission_request(session_id, tool_call, options, message, state) do
      send(state.parent, {:context_permission, self(), session_id, tool_call, options, message})

      receive do
        {:permission_outcome, outcome} -> {:ok, outcome, state}
      after
        2_000 -> {:ok, %{"outcome" => "cancelled"}, state}
      end
    end
  end

  test "preserves complete decoded messages and callback ordering without changing event listeners" do
    {client, _transport} = start_client(event_listener: self())
    first = update_message("first")
    second = update_message("second")
    update = first["params"]["update"]

    send(client, {:transport_message, Jason.encode!(first)})
    send(client, {:transport_message, second})

    assert_receive {:context_update, "context-session", ^update, ^first, 0}
    assert_receive {:context_update, "context-session", _, ^second, 1}
    assert_receive {:acp_session_update, "context-session", ^update}
    refute_receive {:legacy_update, _, _}
  end

  test "preserves permission context and response correlation while rejecting duplicate IDs" do
    {_client, transport} = start_client()
    request = permission_message("original-id")
    send_raw(transport, request)
    assert_receive {:context_permission, handler, "context-session", tool_call, options, ^request}
    assert tool_call == request["params"]["toolCall"]
    assert options == request["params"]["options"]

    send_raw(transport, request)
    assert %{"id" => "original-id", "error" => %{"code" => -32_600}} = receive_raw(transport)
    refute_receive {:context_permission, _, _, _, _, _}
    refute_receive :legacy_permission

    outcome = %{"outcome" => "selected", "optionId" => "allow"}
    send(handler, {:permission_outcome, outcome})
    assert %{"id" => "original-id", "result" => %{"outcome" => ^outcome}} = receive_raw(transport)
  end

  test "context-aware permission handlers retain timeout and late-response behavior" do
    {client, transport} = start_client(handler_request_timeout: 50)
    request = permission_message(nil)
    send_raw(transport, request)
    assert_receive {:context_permission, handler, _, _, _, ^request}

    assert %{"id" => nil, "error" => %{"message" => "Client handler timed out"}} =
             receive_raw(transport)

    assert :sys.get_state(client).pending_agent_requests == %{}

    send(handler, {:permission_outcome, %{"outcome" => "cancelled"}})
    :sys.get_state(handler)
    state = :sys.get_state(client)
    assert state.pending_agent_requests == %{}
    assert :queue.is_empty(:sys.get_state(transport.peer).queues.agent)
  end

  test "malformed and unauthorized messages never reach context callbacks" do
    {client, transport} = start_client()
    valid = update_message("valid")

    for invalid <- [
          "{invalid",
          Jason.encode!(Jason.encode!(valid)),
          Map.put(valid, "jsonrpc", "1.0"),
          put_in(valid, ["params", "sessionId"], "unknown"),
          put_in(valid, ["params", "update"], %{})
        ] do
      send(client, {:transport_message, invalid})
    end

    request = put_in(permission_message(7), ["params", "sessionId"], "unknown")
    send_raw(transport, request)
    assert %{"id" => 7, "error" => %{"code" => -32_602}} = receive_raw(transport)
    refute_receive {:context_update, _, _, _, _}
    refute_receive {:context_permission, _, _, _, _, _}
  end

  test "handler queue byte limits include unknown envelope fields" do
    {client, _transport} = start_client(max_update_queue: 100, max_update_queue_bytes: 2_000)
    handler = :sys.get_state(client).handler_pid
    :ok = :sys.suspend(handler)

    message = Map.put(update_message("small update"), "extension", String.duplicate("x", 1_000))
    send(client, {:transport_message, message})
    :sys.get_state(client)
    assert {:message_queue_len, 1} = Process.info(handler, :message_queue_len)

    for _ <- 1..5, do: send(client, {:transport_message, message})
    :sys.get_state(client)
    assert {:message_queue_len, 1} = Process.info(handler, :message_queue_len)
    :ok = :sys.resume(handler)
    assert_receive {:context_update, _, _, ^message, 0}
  end

  test "context updates retain the handler queue count limit" do
    {client, _transport} = start_client(max_update_queue: 2)
    handler = :sys.get_state(client).handler_pid
    :ok = :sys.suspend(handler)
    for index <- 1..10, do: send(client, {:transport_message, update_message(to_string(index))})
    :sys.get_state(client)
    assert {:message_queue_len, 2} = Process.info(handler, :message_queue_len)
    :ok = :sys.resume(handler)
    assert_receive {:context_update, _, _, _, 0}
    assert_receive {:context_update, _, _, _, 1}
    refute_receive {:context_update, _, _, _, 2}
  end

  defp start_client(opts \\ []) do
    {:ok, peer} = Memory.new_pair()
    {:ok, transport} = Memory.connect(peer: peer, role: :agent)

    handshake =
      Task.async(fn ->
        %{"id" => id, "method" => "initialize"} = receive_raw(transport)

        send_raw(transport, %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{
            "protocolVersion" => 1,
            "agentCapabilities" => %{},
            "authMethods" => []
          }
        })

        %{"id" => id, "method" => "session/new"} = receive_raw(transport)

        send_raw(transport, %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{"sessionId" => "context-session"}
        })
      end)

    {:ok, client} =
      Client.start_link(
        Keyword.merge(
          [
            transport_mod: Memory,
            peer: peer,
            role: :client,
            handler: ContextHandler,
            handler_opts: [parent: self()]
          ],
          opts
        )
      )

    assert {:ok, %{"sessionId" => "context-session"}} = Client.new_session(client, "/tmp")
    Task.await(handshake)
    {client, transport}
  end

  defp update_message(text) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "extension" => %{"trace" => text},
      "params" => %{
        "sessionId" => "context-session",
        "extra" => [1, 2],
        "update" => %{
          "sessionUpdate" => "agent_message_chunk",
          "content" => %{"type" => "text", "text" => text},
          "providerExtension" => true
        }
      }
    }
  end

  defp permission_message(id) do
    %{
      "jsonrpc" => "2.0",
      "id" => id,
      "method" => "session/request_permission",
      "extension" => %{"trace" => "permission"},
      "params" => %{
        "sessionId" => "context-session",
        "extra" => "retained",
        "toolCall" => %{"toolCallId" => "tool-1"},
        "options" => [%{"optionId" => "allow", "name" => "Allow", "kind" => "allow_once"}]
      }
    }
  end

  defp send_raw(transport, message) do
    assert {:ok, _} = Memory.send_message(Jason.encode!(message), transport)
  end

  defp receive_raw(transport) do
    assert {:ok, message, _} = Memory.receive_message(transport)
    Jason.decode!(message)
  end
end
