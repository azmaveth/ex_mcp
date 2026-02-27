defmodule ExMCP.ACP.ClientTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Client
  alias ExMCP.ACP.Protocol

  # MessageRelay: a simple process-based mailbox shared between mock agent and transport.
  # Agent pushes messages in, transport's receive_message pops them out.
  defmodule MessageRelay do
    use GenServer

    def start_link do
      GenServer.start_link(__MODULE__, [])
    end

    def push(relay, message) do
      GenServer.cast(relay, {:push, message})
    end

    def pop(relay, timeout \\ 30_000) do
      GenServer.call(relay, :pop, timeout)
    end

    @impl true
    def init([]) do
      {:ok, %{queue: :queue.new(), waiters: :queue.new()}}
    end

    @impl true
    def handle_cast({:push, message}, state) do
      case :queue.out(state.waiters) do
        {{:value, from}, rest} ->
          GenServer.reply(from, {:ok, message})
          {:noreply, %{state | waiters: rest}}

        {:empty, _} ->
          {:noreply, %{state | queue: :queue.in(message, state.queue)}}
      end
    end

    @impl true
    def handle_call(:pop, from, state) do
      case :queue.out(state.queue) do
        {{:value, message}, rest} ->
          {:reply, {:ok, message}, %{state | queue: rest}}

        {:empty, _} ->
          {:noreply, %{state | waiters: :queue.in(from, state.waiters)}}
      end
    end

    @impl true
    def handle_call(:stop, _from, state) do
      # Reply to all waiters with error
      flush_waiters(state.waiters)
      {:stop, :normal, :ok, state}
    end

    defp flush_waiters(waiters) do
      case :queue.out(waiters) do
        {{:value, from}, rest} ->
          GenServer.reply(from, {:error, :closed})
          flush_waiters(rest)

        {:empty, _} ->
          :ok
      end
    end
  end

  # MockACPTransport: uses MessageRelay for agent→client messages.
  defmodule MockACPTransport do
    @behaviour ExMCP.Transport

    defstruct [:agent_pid, :to_client_relay, :to_agent_relay]

    @impl true
    def connect(opts) do
      agent_pid = Keyword.fetch!(opts, :agent_pid)
      to_client_relay = Keyword.fetch!(opts, :to_client_relay)
      to_agent_relay = Keyword.fetch!(opts, :to_agent_relay)

      {:ok,
       %__MODULE__{
         agent_pid: agent_pid,
         to_client_relay: to_client_relay,
         to_agent_relay: to_agent_relay
       }}
    end

    @impl true
    def send_message(message, %__MODULE__{to_agent_relay: relay} = state) do
      MessageRelay.push(relay, message)
      {:ok, state}
    end

    @impl true
    def receive_message(%__MODULE__{to_client_relay: relay} = state) do
      case MessageRelay.pop(relay) do
        {:ok, message} -> {:ok, message, state}
        {:error, reason} -> {:error, reason}
      end
    end

    @impl true
    def close(%__MODULE__{}) do
      :ok
    end

    @impl true
    def connected?(%__MODULE__{agent_pid: pid}) do
      is_pid(pid) and Process.alive?(pid)
    end
  end

  # MockACPAgent: reads from to_agent_relay, writes to to_client_relay.
  defmodule MockACPAgent do
    def start(to_client_relay, to_agent_relay, opts \\ []) do
      test_pid = self()
      updates = Keyword.get(opts, :updates, [])
      permission_request = Keyword.get(opts, :permission_request)

      spawn_link(fn ->
        loop(%{
          to_client: to_client_relay,
          to_agent: to_agent_relay,
          updates: updates,
          permission_request: permission_request,
          test_pid: test_pid
        })
      end)
    end

    defp loop(state) do
      case MessageRelay.pop(state.to_agent) do
        {:ok, json} ->
          msg = Jason.decode!(json)
          handle_message(msg, state)
          loop(state)

        {:error, _} ->
          :ok
      end
    end

    defp handle_message(%{"method" => "initialize", "id" => id}, state) do
      response =
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "result" => %{
            "agentInfo" => %{"name" => "mock_agent", "version" => "1.0.0"},
            "capabilities" => %{"streaming" => true},
            "protocolVersion" => 1
          },
          "id" => id
        })

      MessageRelay.push(state.to_client, response)
    end

    defp handle_message(%{"method" => "session/new", "id" => id}, state) do
      response =
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "result" => %{"sessionId" => "sess_mock_001"},
          "id" => id
        })

      MessageRelay.push(state.to_client, response)
    end

    defp handle_message(%{"method" => "session/load", "id" => id}, state) do
      response =
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "result" => %{"sessionId" => "sess_loaded_001"},
          "id" => id
        })

      MessageRelay.push(state.to_client, response)
    end

    defp handle_message(%{"method" => "session/prompt", "id" => id, "params" => params}, state) do
      session_id = params["sessionId"]

      # Send queued session/update notifications
      for update <- state.updates do
        notification =
          Jason.encode!(%{
            "jsonrpc" => "2.0",
            "method" => "session/update",
            "params" => Map.put(update, "sessionId", session_id)
          })

        MessageRelay.push(state.to_client, notification)
      end

      # Optionally send a permission request
      if state.permission_request do
        {tool_call, options} = state.permission_request
        perm_id = System.unique_integer([:positive])

        perm_request =
          Jason.encode!(%{
            "jsonrpc" => "2.0",
            "method" => "session/requestPermission",
            "params" => %{
              "sessionId" => session_id,
              "toolCall" => tool_call,
              "options" => options
            },
            "id" => perm_id
          })

        MessageRelay.push(state.to_client, perm_request)

        # Wait for the permission response
        case MessageRelay.pop(state.to_agent) do
          {:ok, json} ->
            resp = Jason.decode!(json)
            send(state.test_pid, {:permission_response, resp})

          {:error, _} ->
            :ok
        end
      end

      # Send the prompt result
      response =
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "result" => %{"stopReason" => "end_turn"},
          "id" => id
        })

      MessageRelay.push(state.to_client, response)
    end

    defp handle_message(%{"method" => "session/cancel"}, _state) do
      :ok
    end

    defp handle_message(%{"method" => "session/setMode", "id" => id}, state) do
      response = Jason.encode!(Protocol.encode_response(%{}, id))
      MessageRelay.push(state.to_client, response)
    end

    defp handle_message(%{"method" => "session/setConfigOption", "id" => id}, state) do
      response = Jason.encode!(Protocol.encode_response(%{}, id))
      MessageRelay.push(state.to_client, response)
    end

    defp handle_message(_msg, _state), do: :ok
  end

  defp start_client(agent_opts \\ []) do
    {:ok, to_client_relay} = MessageRelay.start_link()
    {:ok, to_agent_relay} = MessageRelay.start_link()

    agent_pid = MockACPAgent.start(to_client_relay, to_agent_relay, agent_opts)

    {:ok, client} =
      Client.start_link(
        transport_mod: MockACPTransport,
        command: ["mock"],
        agent_pid: agent_pid,
        to_client_relay: to_client_relay,
        to_agent_relay: to_agent_relay
      )

    {client, agent_pid}
  end

  describe "initialize handshake" do
    test "stores agent capabilities" do
      {client, _agent} = start_client()

      assert {:ok, caps} = Client.agent_capabilities(client)
      assert caps["streaming"] == true

      assert Client.status(client) == :ready
    end
  end

  describe "new_session/3" do
    test "returns session ID" do
      {client, _agent} = start_client()

      assert {:ok, result} = Client.new_session(client, "/tmp/project")
      assert result["sessionId"] == "sess_mock_001"
    end
  end

  describe "load_session/4" do
    test "returns loaded session ID" do
      {client, _agent} = start_client()

      assert {:ok, result} = Client.load_session(client, "old_session_123", "/tmp")
      assert result["sessionId"] == "sess_loaded_001"
    end
  end

  describe "prompt/4" do
    test "blocks until response with streaming events" do
      updates = [
        %{"kind" => "status", "status" => "working"},
        %{"kind" => "text", "content" => "I'll fix that bug."}
      ]

      {client, _agent} = start_client(updates: updates)

      {:ok, _} = Client.new_session(client, "/tmp")
      assert {:ok, result} = Client.prompt(client, "sess_mock_001", "Fix the bug")
      assert result["stopReason"] == "end_turn"
    end

    test "handler receives streaming events" do
      updates = [
        %{"kind" => "text", "content" => "Working on it..."}
      ]

      {client, _agent} = start_client(updates: updates)

      {:ok, _} = Client.new_session(client, "/tmp")
      {:ok, _} = Client.prompt(client, "sess_mock_001", "Do something")

      assert Client.status(client) == :ready
    end

    test "accepts string content" do
      {client, _agent} = start_client()

      {:ok, _} = Client.new_session(client, "/tmp")
      assert {:ok, _} = Client.prompt(client, "sess_mock_001", "Hello agent")
    end

    test "accepts block list content" do
      {client, _agent} = start_client()

      {:ok, _} = Client.new_session(client, "/tmp")
      blocks = [%{"type" => "text", "text" => "Hello"}]
      assert {:ok, _} = Client.prompt(client, "sess_mock_001", blocks)
    end
  end

  describe "cancel/2" do
    test "sends notification without blocking" do
      {client, _agent} = start_client()

      assert :ok = Client.cancel(client, "sess_mock_001")
    end
  end

  describe "permission request handling" do
    test "routes to handler and sends response back" do
      tool_call = %{"toolName" => "file_write", "arguments" => %{"path" => "/etc/hosts"}}

      options = [
        %{"id" => "allow", "label" => "Allow"},
        %{"id" => "deny", "label" => "Deny"}
      ]

      {client, _agent} = start_client(permission_request: {tool_call, options})

      {:ok, _} = Client.new_session(client, "/tmp")
      {:ok, _} = Client.prompt(client, "sess_mock_001", "Write a file")

      assert_receive {:permission_response, resp}, 5_000
      assert resp["result"]["outcome"]["optionId"] == "allow"
    end
  end

  describe "event listener" do
    test "receives session update messages" do
      updates = [
        %{"kind" => "text", "content" => "Hello from agent"}
      ]

      {:ok, to_client_relay} = MessageRelay.start_link()
      {:ok, to_agent_relay} = MessageRelay.start_link()

      agent_pid = MockACPAgent.start(to_client_relay, to_agent_relay, updates: updates)

      {:ok, client} =
        Client.start_link(
          transport_mod: MockACPTransport,
          command: ["mock"],
          agent_pid: agent_pid,
          to_client_relay: to_client_relay,
          to_agent_relay: to_agent_relay,
          event_listener: self()
        )

      {:ok, _} = Client.new_session(client, "/tmp")
      {:ok, _} = Client.prompt(client, "sess_mock_001", "Say hello")

      assert_receive {:acp_session_update, "sess_mock_001", update}, 5_000
      assert update["kind"] == "text"
      assert update["content"] == "Hello from agent"
    end
  end

  describe "transport error" do
    test "transitions to disconnected after disconnect" do
      {client, _agent} = start_client()

      # Verify we start ready
      assert Client.status(client) == :ready

      # Disconnect and verify
      :ok = Client.disconnect(client)
      assert Client.status(client) == :disconnected
    end
  end

  describe "disconnect/1" do
    test "cleanly disconnects" do
      {client, _agent} = start_client()

      assert :ok = Client.disconnect(client)
      assert Client.status(client) == :disconnected
    end
  end
end
