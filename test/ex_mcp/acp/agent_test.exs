defmodule ExMCP.ACP.AgentTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Agent
  alias ExMCP.ACP.Agent.Transport.Memory
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

      {:ok, %{"content" => content}} = Agent.read_text_file(ctx.agent, session_id, "/tmp/a.txt")
      {:ok, _} = Agent.write_text_file(ctx.agent, session_id, "/tmp/b.txt", "updated")
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
      assert_receive {:file_read, ^session_id, "/tmp/a.txt", %{}}
      assert_receive {:file_write, ^session_id, "/tmp/b.txt", "updated"}
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
end
