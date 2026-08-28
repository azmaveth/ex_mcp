defmodule ExMCP.ACP.Adapters.Codex.ProtocolTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex
  alias ExMCP.ACP.Adapters.Codex.Protocol

  setup do
    {:ok, state} =
      Codex.init(
        workspace_roots: ["/tmp"],
        authorize_mcp_server: fn _server, _context -> true end,
        trust_authorized_workspaces: true
      )

    %{state: state}
  end

  describe "initialize and session lifecycle app-server envelopes" do
    test "post_connect sends initialize without a jsonrpc field and tracks the request", %{
      state: state
    } do
      {:ok, data, new_state} = Codex.post_connect(state)
      {binary, msg} = decode_envelope(data)

      assert String.ends_with?(binary, "\n")
      refute Map.has_key?(msg, "jsonrpc")
      assert msg["id"] == 1
      assert msg["method"] == "initialize"

      assert msg["params"] == %{
               "clientInfo" => %{"name" => "ex_mcp", "version" => "1.0.0"}
             }

      assert new_state.next_id == 2
      assert new_state.pending_requests[1] == %{type: :initialize, acp_id: nil, meta: %{}}
    end

    test "initialize response writes initialized then model/list and correlates model_list", %{
      state: state
    } do
      state = %{
        state
        | next_id: 2,
          pending_requests: %{1 => %{type: :initialize, acp_id: nil, meta: %{}}}
      }

      line = Jason.encode!(%{"id" => 1, "result" => %{"capabilities" => %{}}})
      assert {:skip_and_write, data, new_state} = Codex.translate_inbound(line, state)
      [initialized, model_list] = decode_envelopes(data)

      refute Map.has_key?(initialized, "jsonrpc")
      refute Map.has_key?(initialized, "id")
      assert initialized == %{"method" => "initialized"}

      refute Map.has_key?(model_list, "jsonrpc")
      assert model_list["id"] == 2
      assert model_list["method"] == "model/list"
      assert model_list["params"] == %{"includeHidden" => false}

      assert new_state.pending_requests[2] == %{type: :model_list, acp_id: nil, meta: %{}}
    end

    test "session/new sends thread/start and correlates thread_start", %{state: state} do
      msg = %{
        "method" => "session/new",
        "id" => "acp-new",
        "params" => %{"cwd" => "/tmp/project", "model" => "gpt-5"}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      {_binary, envelope} = decode_envelope(data)

      refute Map.has_key?(envelope, "jsonrpc")
      assert envelope["id"] == 1
      assert envelope["method"] == "thread/start"
      assert envelope["params"]["cwd"] == "/tmp/project"
      assert envelope["params"]["model"] == "gpt-5"

      assert new_state.pending_requests[1] == %{
               type: :thread_start,
               acp_id: "acp-new",
               meta: %{mode_id: "agent", additional_directories: []}
             }
    end

    test "session/load sends thread/resume with an initial turns page", %{state: state} do
      msg = %{
        "method" => "session/load",
        "id" => "acp-load",
        "params" => %{"sessionId" => "thread-1", "cwd" => "/tmp/project"}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      {_binary, envelope} = decode_envelope(data)

      assert envelope["method"] == "thread/resume"
      assert envelope["params"]["threadId"] == "thread-1"
      assert envelope["params"]["initialTurnsPage"] == %{"limit" => 100, "itemsView" => "full"}
      assert envelope["params"]["modelProvider"] == "openai"
      assert new_state.pending_requests[1].type == :thread_resume
      assert new_state.pending_requests[1].acp_id == "acp-load"
    end

    test "session/resume sends thread/resume with excludeTurns", %{state: state} do
      msg = %{
        "method" => "session/resume",
        "id" => "acp-resume",
        "params" => %{"sessionId" => "thread-1", "cwd" => "/tmp/project"}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      {_binary, envelope} = decode_envelope(data)

      assert envelope["method"] == "thread/resume"
      assert envelope["params"]["threadId"] == "thread-1"
      assert envelope["params"]["excludeTurns"] == true
      refute Map.has_key?(envelope["params"], "initialTurnsPage")
      assert new_state.pending_requests[1].type == :thread_resume
      assert new_state.pending_requests[1].acp_id == "acp-resume"
    end

    test "session/list sends thread/list and correlates session_list", %{state: state} do
      msg = %{
        "method" => "session/list",
        "id" => "acp-list",
        "params" => %{"cwd" => "/tmp/project", "cursor" => "next", "limit" => 10}
      }

      assert {:ok, data, new_state} = Codex.translate_outbound(msg, state)
      {_binary, envelope} = decode_envelope(data)

      assert envelope["method"] == "thread/list"

      assert envelope["params"] == %{
               "cwd" => "/tmp/project",
               "cursor" => "next",
               "limit" => 10,
               "archived" => false
             }

      assert new_state.pending_requests[1] == %{
               type: :session_list,
               acp_id: "acp-list",
               meta: %{}
             }
    end

    test "session/close unsubscribes and optionally interrupts the active turn", %{state: state} do
      state = put_test_session(state, "thread-1", %{turn_id: "turn-1"})

      msg = %{
        "method" => "session/close",
        "id" => "acp-close",
        "params" => %{"sessionId" => "thread-1"}
      }

      assert {:messages_and_write, [], data, new_state} = Codex.translate_outbound(msg, state)
      [interrupt, unsubscribe] = decode_envelopes(data)

      assert interrupt["method"] == "turn/interrupt"
      assert interrupt["params"] == %{"threadId" => "thread-1", "turnId" => "turn-1"}
      assert unsubscribe["method"] == "thread/unsubscribe"
      assert unsubscribe["params"] == %{"threadId" => "thread-1"}

      assert new_state.pending_requests[1].type == :thread_unsubscribe
      assert new_state.pending_requests[2].type == :turn_interrupt
      assert new_state.pending_requests[1].meta == %{session_id: "thread-1"}
      assert new_state.pending_requests[2].meta == %{session_id: "thread-1"}
    end

    test "session/delete interrupts, unsubscribes, and archives a live session", %{state: state} do
      state = put_test_session(state, "thread-1", %{turn_id: "turn-1"})

      msg = %{
        "method" => "session/delete",
        "id" => "acp-delete",
        "params" => %{"sessionId" => "thread-1"}
      }

      assert {:messages_and_write, [], data, new_state} = Codex.translate_outbound(msg, state)
      [interrupt, unsubscribe, archive] = decode_envelopes(data)

      assert interrupt["method"] == "turn/interrupt"
      assert interrupt["params"] == %{"threadId" => "thread-1", "turnId" => "turn-1"}
      assert unsubscribe["method"] == "thread/unsubscribe"
      assert unsubscribe["params"] == %{"threadId" => "thread-1"}
      assert archive["method"] == "thread/archive"
      assert archive["params"] == %{"threadId" => "thread-1"}

      types =
        new_state.pending_requests
        |> Map.values()
        |> Enum.map(& &1.type)
        |> Enum.sort()

      assert types == [:thread_archive, :thread_unsubscribe, :turn_interrupt]
    end

    test "session/delete still archives an unknown session id", %{state: state} do
      msg = %{
        "method" => "session/delete",
        "id" => "acp-delete",
        "params" => %{"sessionId" => "missing"}
      }

      assert {:messages_and_write, [], data, new_state} = Codex.translate_outbound(msg, state)
      [archive] = decode_envelopes(data)

      assert archive["method"] == "thread/archive"
      assert archive["params"] == %{"threadId" => "missing"}

      assert new_state.pending_requests[1] == %{
               type: :thread_archive,
               acp_id: nil,
               meta: %{session_id: "missing"}
             }
    end

    test "authenticate and logout emit account envelopes and correlation types", %{state: state} do
      auth = %{
        "method" => "authenticate",
        "id" => "acp-auth",
        "params" => %{"methodId" => "chat-gpt"}
      }

      assert {:ok, auth_data, auth_state} = Codex.translate_outbound(auth, state)
      {_binary, auth_envelope} = decode_envelope(auth_data)

      assert auth_envelope["method"] == "account/login/start"
      assert auth_envelope["params"] == %{"type" => "chatgpt"}

      assert auth_state.pending_requests[1] == %{
               type: :authenticate,
               acp_id: "acp-auth",
               meta: %{method_id: "chat-gpt"}
             }

      assert {:reply_and_write, %{}, logout_data, logout_state} =
               Codex.translate_outbound(%{"method" => "logout"}, state)

      {_binary, logout_envelope} = decode_envelope(logout_data)
      assert logout_envelope["method"] == "account/logout"
      assert logout_envelope["params"] == %{}
      assert logout_state.pending_requests[1] == %{type: :logout, acp_id: nil, meta: %{}}
    end
  end

  describe "inbound classification and request-id correlation" do
    test "invalid JSON and unknown maps are skipped", %{state: state} do
      assert {:skip, ^state} = Codex.translate_inbound("not-json", state)
      assert {:skip, ^state} = Codex.translate_inbound(~s({"foo":1}), state)
    end

    test "unknown response ids are skipped as uncorrelated", %{state: state} do
      assert {:skip, ^state} =
               Codex.translate_inbound(~s({"id":99,"result":{"ok":true}}), state)

      assert {:skip, ^state} =
               Codex.translate_inbound(
                 ~s({"id":99,"error":{"code":-1,"message":"nope"}}),
                 state
               )
    end

    test "error replies classify against the pending request id", %{state: state} do
      state = %{
        state
        | pending_requests: %{7 => %{type: :thread_start, acp_id: "acp-7", meta: %{}}}
      }

      line = Jason.encode!(%{"id" => 7, "error" => %{"code" => -32_000, "message" => "boom"}})
      assert {:messages, [error], new_state} = Codex.translate_inbound(line, state)

      assert error["id"] == "acp-7"
      assert error["error"]["message"] == "boom"
      assert new_state.pending_requests == %{}
    end

    test "server requests encode a native error envelope for unsupported methods", %{state: state} do
      line =
        Jason.encode!(%{
          "id" => "req-1",
          "method" => "item/tool/call",
          "params" => %{}
        })

      assert {:skip_and_write, data, ^state} = Codex.translate_inbound(line, state)
      {binary, envelope} = decode_envelope(data)

      assert String.ends_with?(binary, "\n")
      refute Map.has_key?(envelope, "jsonrpc")
      assert envelope["id"] == "req-1"

      assert envelope["error"] == %{
               "code" => -32_601,
               "message" => "Unsupported app-server request: item/tool/call"
             }
    end

    test "notifications keep method and params and do not require an id", %{state: state} do
      state = put_test_session(state, "thread-1")

      line =
        Jason.encode!(%{
          "method" => "item/agentMessage/delta",
          "params" => %{"threadId" => "thread-1", "delta" => "hi"}
        })

      assert {:messages, [chunk], _state} = Codex.translate_inbound(line, state)
      assert chunk["method"] == "session/update"
      assert chunk["params"]["update"]["sessionUpdate"] == "agent_message_chunk"
      assert chunk["params"]["update"]["content"]["text"] == "hi"
    end

    test "request ids increment and stay correlated across sequential writes", %{state: state} do
      {:ok, first, state} = Codex.post_connect(state)
      {_binary, initialize} = decode_envelope(first)

      msg = %{
        "method" => "session/new",
        "id" => "acp-new",
        "params" => %{"cwd" => "/tmp/project"}
      }

      assert {:ok, second, state} = Codex.translate_outbound(msg, state)
      {_binary, thread_start} = decode_envelope(second)

      assert initialize["id"] == 1
      assert thread_start["id"] == 2
      assert state.next_id == 3
      assert Map.keys(state.pending_requests) |> Enum.sort() == [1, 2]
      assert state.pending_requests[1].type == :initialize
      assert state.pending_requests[2].type == :thread_start
    end
  end

  describe "ExMCP.ACP.Adapters.Codex.Protocol shapes" do
    test "method names for initialize and session lifecycle" do
      assert Protocol.method(:initialize) == "initialize"
      assert Protocol.method(:initialized) == "initialized"
      assert Protocol.method(:model_list) == "model/list"
      assert Protocol.method(:account_login_start) == "account/login/start"
      assert Protocol.method(:account_logout) == "account/logout"
      assert Protocol.method(:account_login_cancel) == "account/login/cancel"
      assert Protocol.method(:thread_start) == "thread/start"
      assert Protocol.method(:thread_resume) == "thread/resume"
      assert Protocol.method(:thread_list) == "thread/list"
      assert Protocol.method(:thread_unsubscribe) == "thread/unsubscribe"
      assert Protocol.method(:thread_archive) == "thread/archive"
      assert Protocol.method(:thread_compact_start) == "thread/compact/start"
      assert Protocol.method(:turn_start) == "turn/start"
      assert Protocol.method(:turn_interrupt) == "turn/interrupt"
      assert Protocol.method(:review_start) == "review/start"
    end

    test "request/response/error/notification envelopes omit jsonrpc and keep NDJSON" do
      request =
        Protocol.request(1, Protocol.method(:initialize), %{"clientInfo" => %{"name" => "ex_mcp"}})

      response = Protocol.response(1, %{"ok" => true})
      error = Protocol.error("req-1", -32_601, "Unsupported app-server request: item/tool/call")
      notification = Protocol.notification(Protocol.method(:initialized))
      empty_notification = Protocol.notification("warning", %{})

      assert request == %{
               "id" => 1,
               "method" => "initialize",
               "params" => %{"clientInfo" => %{"name" => "ex_mcp"}}
             }

      assert response == %{"id" => 1, "result" => %{"ok" => true}}

      assert error == %{
               "id" => "req-1",
               "error" => %{
                 "code" => -32_601,
                 "message" => "Unsupported app-server request: item/tool/call"
               }
             }

      assert notification == %{"method" => "initialized"}
      assert empty_notification == %{"method" => "warning"}

      encoded = Protocol.encode_request(1, Protocol.method(:thread_start), %{"threadId" => "t"})
      binary = IO.iodata_to_binary(encoded)
      assert String.ends_with?(binary, "\n")
      refute Map.has_key?(Jason.decode!(String.trim(binary)), "jsonrpc")

      assert Protocol.encode_request(3, Protocol.method(:turn_start), nil) ==
               Protocol.line(%{"id" => 3, "method" => "turn/start", "params" => %{}})
    end

    test "classifies inbound result, error, request, notification, and unknown maps" do
      assert Protocol.classify_inbound(%{"id" => 1, "result" => %{"ok" => true}}) ==
               {:response, 1, {:ok, %{"ok" => true}}}

      assert Protocol.classify_inbound(%{"id" => 1, "error" => %{"code" => -1, "message" => "x"}}) ==
               {:response, 1, {:error, %{"code" => -1, "message" => "x"}}}

      assert Protocol.classify_inbound(%{
               "id" => "req-1",
               "method" => "item/tool/call",
               "params" => %{"x" => 1}
             }) == {:request, "req-1", "item/tool/call", %{"x" => 1}}

      assert Protocol.classify_inbound(%{
               "method" => "item/agentMessage/delta",
               "params" => %{"delta" => "hi"}
             }) == {:notification, "item/agentMessage/delta", %{"delta" => "hi"}}

      assert Protocol.classify_inbound(%{"method" => "initialized"}) ==
               {:notification, "initialized", %{}}

      assert Protocol.classify_inbound(%{"id" => 1, "method" => "foo"}) ==
               {:notification, "foo", %{}}

      assert Protocol.classify_inbound(%{"foo" => 1}) == :unknown
      assert Protocol.decode_line("not-json") == :unknown

      assert Protocol.decode_line(~s({"id":2,"result":{}})) == {:response, 2, {:ok, %{}}}
    end

    test "request-id correlation shapes increment ids and build pending entries" do
      assert Protocol.next_id(1) == {1, 2}
      assert Protocol.next_id(41) == {41, 42}

      assert Protocol.request_entry(:initialize, nil) == %{
               type: :initialize,
               acp_id: nil,
               meta: %{}
             }

      assert Protocol.request_entry(:thread_start, "acp-new", %{mode_id: "agent"}) == %{
               type: :thread_start,
               acp_id: "acp-new",
               meta: %{mode_id: "agent"}
             }
    end
  end

  defp put_test_session(state, session_id, attrs \\ %{}) do
    session =
      %{
        id: session_id,
        cwd: "/tmp/project",
        model: nil,
        model_id: nil,
        mode_id: "agent",
        reasoning_effort: "medium",
        accumulated_text: [],
        accumulated_thinking: [],
        accumulated_usage: nil,
        turn_id: nil,
        active_prompt_acp_id: nil
      }
      |> Map.merge(attrs)

    %{state | sessions: Map.put(state.sessions, session_id, session)}
  end

  defp decode_envelope(data) do
    binary = IO.iodata_to_binary(data)
    {binary, binary |> String.trim() |> Jason.decode!()}
  end

  defp decode_envelopes(data) do
    data
    |> IO.iodata_to_binary()
    |> String.split("\n", trim: true)
    |> Enum.map(&Jason.decode!/1)
  end
end
