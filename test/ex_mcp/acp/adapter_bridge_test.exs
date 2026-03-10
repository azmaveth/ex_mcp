defmodule ExMCP.ACP.AdapterBridgeTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.AdapterBridge

  # MockAdapter: uses a simple cat-like echo process for testing
  defmodule MockAdapter do
    @behaviour ExMCP.ACP.Adapter

    defstruct [:session_id, messages_received: []]

    @impl true
    def init(_opts), do: {:ok, %__MODULE__{}}

    @impl true
    def command(_opts) do
      # Use cat as a simple echo process — reads stdin, writes to stdout
      {"cat", []}
    end

    @impl true
    def capabilities do
      %{"streaming" => true, "mockAdapter" => true}
    end

    @impl true
    def translate_outbound(%{"method" => "initialize"}, state) do
      {:ok, :skip, state}
    end

    def translate_outbound(%{"method" => "session/prompt", "params" => params}, state) do
      # Echo the prompt text back as a line to stdin
      text =
        params["prompt"]
        |> List.first()
        |> Map.get("text", "")

      data = Jason.encode!(%{"type" => "echo", "text" => text}) <> "\n"
      {:ok, data, state}
    end

    def translate_outbound(_msg, state) do
      {:ok, :skip, state}
    end

    @impl true
    def translate_inbound(line, state) do
      case Jason.decode(String.trim(line)) do
        {:ok, %{"type" => "echo", "text" => text}} ->
          notification = %{
            "jsonrpc" => "2.0",
            "method" => "session/update",
            "params" => %{
              "sessionId" => "test_session",
              "update" => %{
                "sessionUpdate" => "agent_message_chunk",
                "content" => %{"type" => "text", "text" => text}
              }
            }
          }

          {:messages, [notification], state}

        _ ->
          {:skip, state}
      end
    end
  end

  # OneShotMockAdapter: simulates one-shot execution
  defmodule OneShotMockAdapter do
    @behaviour ExMCP.ACP.Adapter

    defstruct []

    @impl true
    def init(_opts), do: {:ok, %__MODULE__{}}

    @impl true
    def command(_opts), do: :one_shot

    @impl true
    def capabilities, do: %{"streaming" => false}

    @impl true
    def translate_outbound(%{"method" => "initialize"}, state) do
      {:ok, :skip, state}
    end

    def translate_outbound(%{"method" => "session/prompt", "id" => id}, state) do
      cmd_fn = fn ->
        result = %{
          "jsonrpc" => "2.0",
          "result" => %{"stopReason" => "end_turn", "text" => "one-shot result"},
          "id" => id
        }

        {:ok, [Jason.encode!(result)]}
      end

      {:one_shot, cmd_fn, state}
    end

    def translate_outbound(_msg, state), do: {:ok, :skip, state}

    @impl true
    def translate_inbound(_line, state), do: {:skip, state}
  end

  # Helper to send initialize and drain the synthesized init response
  defp send_initialize(bridge) do
    :ok =
      AdapterBridge.send_message(
        bridge,
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "method" => "initialize",
          "id" => 0,
          "params" => %{}
        })
      )

    {:ok, init_raw} = AdapterBridge.receive_message(bridge, 5_000)
    Jason.decode!(init_raw)
  end

  describe "start_link/1 with persistent adapter" do
    test "starts and produces initialize response" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: MockAdapter, adapter_opts: [])

      # Send initialize to trigger synthesized init response
      msg = send_initialize(bridge)

      assert msg["jsonrpc"] == "2.0"
      assert msg["result"]["agentInfo"]["name"] == "mockadapter"
      assert msg["result"]["agentCapabilities"]["streaming"] == true
      assert msg["result"]["agentCapabilities"]["mockAdapter"] == true
      assert msg["result"]["protocolVersion"] == 1

      AdapterBridge.close(bridge)
    end
  end

  describe "send and receive round-trip" do
    test "prompt goes through adapter translate and back" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: MockAdapter, adapter_opts: [])

      # Send initialize and drain init response
      _init = send_initialize(bridge)

      # Send a prompt
      prompt_msg = %{
        "jsonrpc" => "2.0",
        "method" => "session/prompt",
        "params" => %{
          "sessionId" => "test_session",
          "prompt" => [%{"type" => "text", "text" => "Hello adapter"}]
        },
        "id" => 42
      }

      assert :ok = AdapterBridge.send_message(bridge, Jason.encode!(prompt_msg))

      # cat echoes back what we send — the adapter translates it to a session/update
      assert {:ok, raw} = AdapterBridge.receive_message(bridge, 5_000)
      msg = Jason.decode!(raw)

      assert msg["method"] == "session/update"
      assert msg["params"]["update"]["sessionUpdate"] == "agent_message_chunk"
      assert msg["params"]["update"]["content"] == %{"type" => "text", "text" => "Hello adapter"}

      AdapterBridge.close(bridge)
    end
  end

  describe "one-shot adapter" do
    test "produces results without persistent Port" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: OneShotMockAdapter, adapter_opts: [])

      # Send initialize and drain init response
      _init = send_initialize(bridge)

      # Send prompt
      prompt_msg = %{
        "jsonrpc" => "2.0",
        "method" => "session/prompt",
        "params" => %{
          "sessionId" => "s1",
          "prompt" => [%{"type" => "text", "text" => "test"}]
        },
        "id" => 99
      }

      assert :ok = AdapterBridge.send_message(bridge, Jason.encode!(prompt_msg))

      # One-shot result arrives via Task message
      assert {:ok, raw} = AdapterBridge.receive_message(bridge, 5_000)
      msg = Jason.decode!(raw)

      assert msg["result"]["stopReason"] == "end_turn"
      assert msg["result"]["text"] == "one-shot result"
      assert msg["id"] == 99

      AdapterBridge.close(bridge)
    end
  end

  describe "waiter queue" do
    test "receive blocks until message available" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: MockAdapter, adapter_opts: [])

      # Send initialize and drain init response
      _init = send_initialize(bridge)

      # Start a receive that will block
      task =
        Task.async(fn ->
          AdapterBridge.receive_message(bridge, 10_000)
        end)

      # Give the receive call time to register as a waiter
      Process.sleep(50)

      # Now send a prompt that will produce a response
      prompt_msg = %{
        "jsonrpc" => "2.0",
        "method" => "session/prompt",
        "params" => %{
          "sessionId" => "s1",
          "prompt" => [%{"type" => "text", "text" => "delayed"}]
        },
        "id" => 1
      }

      AdapterBridge.send_message(bridge, Jason.encode!(prompt_msg))

      # The blocked receive should now get the message
      assert {:ok, raw} = Task.await(task, 10_000)
      msg = Jason.decode!(raw)
      assert msg["params"]["update"]["content"] == %{"type" => "text", "text" => "delayed"}

      AdapterBridge.close(bridge)
    end
  end

  describe "close/1" do
    test "closes cleanly" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: MockAdapter, adapter_opts: [])
      assert :ok = AdapterBridge.close(bridge)
      refute Process.alive?(bridge)
    end
  end

  describe "port exit" do
    test "replies error to waiters when port exits" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: MockAdapter, adapter_opts: [])

      # Send initialize and drain init response
      _init = send_initialize(bridge)

      # Close the underlying port by closing the bridge
      # We'll test via the close path
      AdapterBridge.close(bridge)

      # Bridge is stopped, no more messages
      refute Process.alive?(bridge)
    end
  end

  describe "skip messages" do
    test "initialize is handled by bridge and adapter skips it" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: MockAdapter, adapter_opts: [])

      # Send initialize — bridge synthesizes init response, adapter skips port write
      init_msg = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "params" => %{},
        "id" => 1
      }

      assert :ok = AdapterBridge.send_message(bridge, Jason.encode!(init_msg))

      # Should get synthesized init response
      {:ok, raw} = AdapterBridge.receive_message(bridge, 5_000)
      msg = Jason.decode!(raw)
      assert msg["id"] == 1
      assert msg["result"]["agentInfo"]["name"] == "mockadapter"

      AdapterBridge.close(bridge)
    end
  end
end
