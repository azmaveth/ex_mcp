defmodule ExMCP.ACP.AgentTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Agent
  alias ExMCP.ACP.Agent.Transport.{Memory, Stdio}
  alias ExMCP.ACP.Client
  alias ExMCP.ACP.Client.Handler, as: ClientHandler

  defmodule EchoAgent do
    @behaviour ExMCP.ACP.Agent.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_new_session(params, _ctx, state) do
      send(state.test_pid, {:new_session, params})
      {:reply, %{"sessionId" => "sess_echo"}, state}
    end

    @impl true
    def handle_prompt(session_id, prompt, ctx, state) do
      text = prompt |> List.first() |> Map.get("text")

      :ok = Agent.agent_message(ctx.agent, session_id, "echo: ")
      :ok = Agent.agent_message(ctx.agent, session_id, text)

      {:reply, %{"stopReason" => "end_turn"}, state}
    end

    @impl true
    def handle_delete_session(session_id, ctx, state) do
      send(state.test_pid, {:delete_session, session_id, ctx.session_id})
      {:reply, %{}, state}
    end
  end

  defmodule AsyncAgent do
    @behaviour ExMCP.ACP.Agent.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_new_session(_params, _ctx, state), do: {:reply, "sess_async", state}

    @impl true
    def handle_prompt(session_id, _prompt, ctx, state) do
      send(state.test_pid, {:prompt_started, ctx.prompt_id})

      Task.start(fn ->
        Agent.agent_message(ctx.agent, session_id, "streamed")
        Agent.finish_prompt(ctx.agent, ctx.prompt_id, %{"stopReason" => "end_turn"})
      end)

      {:noreply, state}
    end
  end

  defmodule CancelAgent do
    @behaviour ExMCP.ACP.Agent.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_new_session(_params, _ctx, state), do: {:reply, "sess_cancel", state}

    @impl true
    def handle_prompt(_session_id, _prompt, ctx, state) do
      send(state.test_pid, {:prompt_waiting, ctx.prompt_id})
      {:noreply, state}
    end

    @impl true
    def handle_cancel(session_id, ctx, state) do
      send(state.test_pid, {:cancelled, session_id, ctx.prompt_id})
      {:reply, "cancelled", state}
    end
  end

  defmodule CloseCancelAgent do
    @behaviour ExMCP.ACP.Agent.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_new_session(_params, _ctx, state), do: {:reply, "sess_close", state}

    @impl true
    def handle_prompt(session_id, _prompt, ctx, state) do
      send(state.test_pid, {:close_prompt_waiting, session_id, ctx.prompt_id})
      {:noreply, state}
    end

    @impl true
    def handle_cancel(session_id, ctx, state) do
      send(state.test_pid, {:close_cancelled, session_id, ctx.prompt_id})
      {:reply, "cancelled", state}
    end

    @impl true
    def handle_close_session(session_id, _ctx, state) do
      send(state.test_pid, {:closed, session_id})
      {:reply, %{}, state}
    end
  end

  defmodule RequestingAgent do
    @behaviour ExMCP.ACP.Agent.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_new_session(_params, _ctx, state), do: {:reply, "sess_requests", state}

    @impl true
    def handle_prompt(session_id, _prompt, ctx, state) do
      {:ok, %{"outcome" => %{"outcome" => "selected", "optionId" => "allow"}}} =
        Agent.request_permission(
          ctx.agent,
          session_id,
          %{"toolName" => "read", "toolCallId" => "tool_1"},
          [%{"optionId" => "allow", "name" => "Allow", "kind" => "allow_once"}]
        )

      {:ok, %{"content" => content}} =
        Agent.read_text_file(ctx.agent, session_id, "/tmp/project/a.txt")

      {:ok, _} =
        Agent.write_text_file(ctx.agent, session_id, "/tmp/project/b.txt", "updated")

      {:ok, %{"terminalId" => terminal_id}} = Agent.terminal_create(ctx.agent, session_id, "mix")
      {:ok, %{"output" => "compiled"}} = Agent.terminal_output(ctx.agent, session_id, terminal_id)
      {:ok, %{"exitCode" => 0}} = Agent.terminal_wait_for_exit(ctx.agent, session_id, terminal_id)
      {:ok, _} = Agent.terminal_release(ctx.agent, session_id, terminal_id)

      send(state.test_pid, :client_requests_completed)

      {:reply, %{"stopReason" => "end_turn", "text" => content}, state}
    end
  end

  defmodule CapabilityAgent do
    @behaviour ExMCP.ACP.Agent.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_new_session(_params, _ctx, state), do: {:reply, "sess_caps", state}

    @impl true
    def handle_prompt(session_id, _prompt, ctx, state) do
      send(state.test_pid, Agent.read_text_file(ctx.agent, session_id, "/tmp/a.txt"))
      {:reply, "refusal", state}
    end
  end

  defmodule HangingAgent do
    @behaviour ExMCP.ACP.Agent.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_new_session(_params, _ctx, state) do
      send(state.test_pid, :hanging_handler_started)
      Process.sleep(:infinity)
    end

    @impl true
    def handle_prompt(_session_id, _prompt, _ctx, state), do: {:noreply, state}
  end

  defmodule RequestClientHandler do
    @behaviour ClientHandler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_session_update(_session_id, _update, state), do: {:ok, state}

    @impl true
    def handle_permission_request(session_id, tool_call, options, state) do
      send(state.test_pid, {:permission_request, session_id, tool_call, options})
      {:ok, %{"outcome" => "selected", "optionId" => "allow"}, state}
    end

    @impl true
    def handle_file_read(session_id, path, opts, state) do
      send(state.test_pid, {:file_read, session_id, path, opts})
      {:ok, "file body", state}
    end

    @impl true
    def handle_file_write(session_id, path, content, state) do
      send(state.test_pid, {:file_write, session_id, path, content})
      {:ok, state}
    end

    @impl true
    def handle_terminal_request(method, params, _id, state) do
      send(state.test_pid, {:terminal_request, method, params})

      result =
        case method do
          "terminal/create" -> %{"terminalId" => "term_1"}
          "terminal/output" -> %{"output" => "compiled"}
          "terminal/wait_for_exit" -> %{"exitCode" => 0}
          "terminal/release" -> %{}
        end

      {:ok, result, state}
    end
  end

  describe "client interoperability over memory transport" do
    test "initializes, creates a session, streams text, and returns final prompt result" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: EchoAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer},
          agent_info: %{"name" => "echo", "version" => "1.0.0"}
        )

      {:ok, client} =
        Client.start_link(
          transport_mod: Memory,
          peer: peer,
          role: :client,
          event_listener: self()
        )

      assert {:ok, %{"sessionId" => "sess_echo"}} = Client.new_session(client, "/tmp/project")
      assert_receive {:new_session, %{"cwd" => "/tmp/project"}}

      assert {:ok, %{"stopReason" => "end_turn", "text" => "echo: hello"}} =
               Client.prompt(client, "sess_echo", "hello")

      assert_receive {:acp_session_update, "sess_echo",
                      %{"sessionUpdate" => "agent_message_chunk"}}
    end

    test "supports async prompt completion" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: AsyncAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer}
        )

      {:ok, client} =
        Client.start_link(transport_mod: Memory, peer: peer, role: :client)

      {:ok, %{"sessionId" => session_id}} = Client.new_session(client, "/tmp/project")

      assert {:ok, %{"stopReason" => "end_turn", "text" => "streamed"}} =
               Client.prompt(client, session_id, "work")

      assert_receive {:prompt_started, _prompt_id}
    end

    test "completes cancellation with cancelled stop reason" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: CancelAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer}
        )

      {:ok, client} =
        Client.start_link(transport_mod: Memory, peer: peer, role: :client)

      {:ok, %{"sessionId" => session_id}} = Client.new_session(client, "/tmp/project")

      task = Task.async(fn -> Client.prompt(client, session_id, "wait") end)
      assert_receive {:prompt_waiting, prompt_id}

      :ok = Client.cancel(client, session_id)

      assert {:ok, %{"stopReason" => "cancelled"}} = Task.await(task)
      assert_receive {:cancelled, ^session_id, ^prompt_id}
    end

    test "completes request-scoped cancellation with cancelled stop reason" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: CancelAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer}
        )

      {:ok, client} =
        Client.start_link(transport_mod: Memory, peer: peer, role: :client)

      {:ok, %{"sessionId" => session_id}} = Client.new_session(client, "/tmp/project")

      task = Task.async(fn -> Client.prompt(client, session_id, "wait") end)
      assert_receive {:prompt_waiting, prompt_id}

      :ok = Client.cancel_request(client, prompt_id)

      assert {:ok, %{"stopReason" => "cancelled"}} = Task.await(task)
      assert_receive {:cancelled, ^session_id, ^prompt_id}
    end

    test "agent can request permission, filesystem, and terminal operations from client" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: RequestingAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer}
        )

      {:ok, client} =
        Client.start_link(
          transport_mod: Memory,
          peer: peer,
          role: :client,
          handler: RequestClientHandler,
          handler_opts: [test_pid: self()],
          capabilities: %{
            "fs" => %{"readTextFile" => true, "writeTextFile" => true},
            "terminal" => true
          }
        )

      {:ok, %{"sessionId" => session_id}} = Client.new_session(client, "/tmp/project")

      assert {:ok, %{"stopReason" => "end_turn", "text" => "file body"}} =
               Client.prompt(client, session_id, "use client")

      assert_receive {:permission_request, ^session_id, %{"toolName" => "read"}, [_]}
      assert_receive {:file_read, ^session_id, "/tmp/project/a.txt", %{}}
      assert_receive {:file_write, ^session_id, "/tmp/project/b.txt", "updated"}
      assert_receive {:terminal_request, "terminal/create", %{"command" => "mix"}}
      assert_receive {:terminal_request, "terminal/output", %{"terminalId" => "term_1"}}
      assert_receive {:terminal_request, "terminal/wait_for_exit", %{"terminalId" => "term_1"}}
      assert_receive {:terminal_request, "terminal/release", %{"terminalId" => "term_1"}}
      assert_receive :client_requests_completed
    end

    test "filesystem helpers fail before the client advertises support" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: CapabilityAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer}
        )

      {:ok, client} =
        Client.start_link(transport_mod: Memory, peer: peer, role: :client, capabilities: %{})

      {:ok, %{"sessionId" => session_id}} = Client.new_session(client, "/tmp/project")
      assert {:ok, %{"stopReason" => "refusal"}} = Client.prompt(client, session_id, "read")

      assert_receive {:error, {:unsupported_client_capability, :fs_read}}
    end

    test "supports session/delete when advertised" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: EchoAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer},
          agent_capabilities: %{"sessionCapabilities" => %{"delete" => %{}}}
        )

      {:ok, client} =
        Client.start_link(transport_mod: Memory, peer: peer, role: :client)

      assert {:ok, %{}} = Client.delete_session(client, "sess_echo")
      assert_receive {:delete_session, "sess_echo", "sess_echo"}
    end

    test "does not accept optional callbacks when capability is explicitly suppressed" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: EchoAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer},
          agent_capabilities: %{}
        )

      {:ok, client} =
        Client.start_link(transport_mod: Memory, peer: peer, role: :client)

      assert {:error, {:unsupported_capability, :session_delete}} =
               Client.delete_session(client, "sess_echo")

      refute_receive {:delete_session, _, _}, 100
    end

    test "session/close cancels an active prompt before closing" do
      {:ok, peer} = Memory.new_pair()

      {:ok, _agent} =
        Agent.start_link(
          handler: CloseCancelAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer},
          agent_capabilities: %{"sessionCapabilities" => %{"close" => %{}}}
        )

      {:ok, client} =
        Client.start_link(transport_mod: Memory, peer: peer, role: :client)

      {:ok, %{"sessionId" => session_id}} = Client.new_session(client, "/tmp/project")

      task = Task.async(fn -> Client.prompt(client, session_id, "wait") end)
      assert_receive {:close_prompt_waiting, ^session_id, prompt_id}, 5_000

      assert {:ok, %{}} = Client.close_session(client, session_id)
      assert {:ok, %{"stopReason" => "cancelled"}} = Task.await(task, 5_000)
      assert_receive {:close_cancelled, ^session_id, ^prompt_id}
      assert_receive {:closed, ^session_id}
    end

    test "agent stops normally when the transport closes" do
      {:ok, peer} = Memory.new_pair()

      {:ok, agent} =
        Agent.start_link(
          handler: EchoAgent,
          handler_opts: [test_pid: self()],
          transport: {:memory, peer}
        )

      {:ok, client} = Client.start_link(transport_mod: Memory, peer: peer, role: :client)
      assert {:ok, %{"sessionId" => "sess_echo"}} = Client.new_session(client, "/tmp/project")

      ref = Process.monitor(agent)
      :ok = Client.disconnect(client)

      assert_receive {:DOWN, ^ref, :process, ^agent, :normal}
    end
  end

  describe "native agent protocol hardening" do
    test "rejects requests before initialize" do
      {_agent, transport} = start_raw_agent(EchoAgent)

      send_raw_request(transport, 1, "session/new", %{"cwd" => "/tmp", "mcpServers" => []})

      assert %{"id" => 1, "error" => %{"code" => -32_002}} = receive_raw(transport)
    end

    test "requires protocolVersion and permits a valid initialize after invalid params" do
      {_agent, transport} = start_raw_agent(EchoAgent)

      send_raw_request(transport, 1, "initialize", %{"clientCapabilities" => %{}})
      assert %{"id" => 1, "error" => %{"code" => -32_602}} = receive_raw(transport)

      initialize_raw(transport, 2)
      assert %{"id" => 2, "result" => %{"protocolVersion" => 1}} = receive_raw(transport)
    end

    test "initialize succeeds exactly once" do
      {_agent, transport} = start_raw_agent(EchoAgent)

      initialize_raw(transport, 1)
      assert %{"id" => 1, "result" => %{}} = receive_raw(transport)

      initialize_raw(transport, 2)
      assert %{"id" => 2, "error" => %{"code" => -32_600}} = receive_raw(transport)
    end

    test "rejects a duplicate outstanding JSON-RPC id without replacing the prompt" do
      {agent, transport} = start_raw_agent(CancelAgent)

      initialize_raw(transport, 1)
      assert %{"result" => %{}} = receive_raw(transport)

      prompt = %{
        "sessionId" => "session-one",
        "prompt" => [%{"type" => "text", "text" => "wait"}]
      }

      send_raw_request(transport, "duplicate", "session/prompt", prompt)
      assert_receive {:prompt_waiting, "duplicate"}

      send_raw_request(transport, "duplicate", "session/prompt", prompt)

      assert %{"id" => "duplicate", "error" => %{"code" => -32_600}} =
               receive_raw(transport)

      assert :ok = Agent.finish_prompt(agent, "duplicate", "end_turn")

      assert %{"id" => "duplicate", "result" => %{"stopReason" => "end_turn"}} =
               receive_raw(transport)
    end

    test "expires a handler callback and removes it from pending state" do
      {agent, transport} = start_raw_agent(HangingAgent, handler_request_timeout: 20)

      initialize_raw(transport, 1)
      assert %{"id" => 1, "result" => %{}} = receive_raw(transport)

      send_raw_request(transport, 2, "session/new", %{"cwd" => "/tmp", "mcpServers" => []})
      assert_receive :hanging_handler_started

      assert %{
               "id" => 2,
               "error" => %{"code" => -32_603, "message" => "Agent handler timed out"}
             } = receive_raw(transport)

      assert :sys.get_state(agent).pending_callbacks == %{}
    end
  end

  describe "native agent stdio framing" do
    test "custom IO devices do not mutate the global logger level" do
      level = :logger.get_primary_config()[:level]
      {:ok, input} = StringIO.open("")

      assert {:ok, _transport} =
               Stdio.connect(input: input, output: input)

      assert :logger.get_primary_config()[:level] == level
    end

    test "aborts an oversized frame without a newline using bounded reads" do
      {:ok, input} = StringIO.open(String.duplicate("x", 65))

      {:ok, transport} =
        Stdio.connect(
          input: input,
          output: input,
          max_frame_bytes: 64
        )

      assert {:error, :frame_too_large} =
               Stdio.receive_message(transport)
    end

    test "retains additional newline-delimited frames read in the same bounded chunk" do
      {:ok, input} = StringIO.open(~s({"first":true}\n{"second":true}\n))

      {:ok, transport} =
        Stdio.connect(
          input: input,
          output: input,
          max_frame_bytes: 64
        )

      assert {:ok, "{\"first\":true}", transport} =
               Stdio.receive_message(transport)

      assert {:ok, "{\"second\":true}", _transport} =
               Stdio.receive_message(transport)
    end
  end

  defp start_raw_agent(handler, opts \\ []) do
    {:ok, peer} = Memory.new_pair()

    {:ok, agent} =
      Agent.start_link(
        Keyword.merge(
          [handler: handler, handler_opts: [test_pid: self()], transport: {:memory, peer}],
          opts
        )
      )

    {:ok, transport} = Memory.connect(peer: peer, role: :client)
    {agent, transport}
  end

  defp initialize_raw(transport, id) do
    send_raw_request(transport, id, "initialize", %{
      "protocolVersion" => 1,
      "clientCapabilities" => %{}
    })
  end

  defp send_raw_request(transport, id, method, params) do
    message =
      Jason.encode!(%{"jsonrpc" => "2.0", "id" => id, "method" => method, "params" => params})

    assert {:ok, _transport} = Memory.send_message(message, transport)
  end

  defp receive_raw(transport) do
    assert {:ok, message, _transport} = Memory.receive_message(transport)
    Jason.decode!(message)
  end
end
