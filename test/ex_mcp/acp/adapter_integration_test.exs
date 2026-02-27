defmodule ExMCP.ACP.AdapterIntegrationTest do
  @moduledoc """
  Integration tests for the full adapter pipeline:
  AdapterTransport → AdapterBridge → Adapter → Port subprocess → back

  Uses `cat` as a real subprocess to verify bidirectional message flow
  through the entire stack.
  """
  use ExUnit.Case, async: true

  alias ExMCP.ACP.AdapterBridge
  alias ExMCP.ACP.AdapterTransport

  # Full-featured mock adapter using `cat` for round-trip testing.
  # Tracks request IDs so inbound echoes produce proper JSON-RPC responses.
  defmodule IntegrationAdapter do
    @behaviour ExMCP.ACP.Adapter

    defstruct [
      :pending_new_id,
      :pending_prompt_id,
      session_id: "test-session"
    ]

    @impl true
    def init(_opts), do: {:ok, %__MODULE__{}}

    @impl true
    def command(_opts), do: {"cat", []}

    @impl true
    def capabilities, do: %{"streaming" => true, "test" => true}

    @impl true
    def translate_outbound(%{"method" => "initialize"}, state) do
      {:ok, :skip, state}
    end

    def translate_outbound(%{"method" => "session/new", "id" => id}, state) do
      state = %{state | pending_new_id: id}
      data = Jason.encode!(%{"_echo" => "new_session", "_id" => id}) <> "\n"
      {:ok, data, state}
    end

    def translate_outbound(
          %{"method" => "session/prompt", "id" => id, "params" => params},
          state
        ) do
      text = extract_text(params["content"])
      state = %{state | pending_prompt_id: id}
      data = Jason.encode!(%{"_echo" => "prompt", "_text" => text, "_id" => id}) <> "\n"
      {:ok, data, state}
    end

    def translate_outbound(_msg, state), do: {:ok, :skip, state}

    @impl true
    def translate_inbound(line, state) do
      case Jason.decode(String.trim(line)) do
        {:ok, %{"_echo" => "new_session"}} ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => state.pending_new_id,
            "result" => %{"sessionId" => state.session_id}
          }

          {:messages, [response], %{state | pending_new_id: nil}}

        {:ok, %{"_echo" => "prompt", "_text" => text}} ->
          notification = %{
            "jsonrpc" => "2.0",
            "method" => "session/update",
            "params" => %{
              "sessionId" => state.session_id,
              "kind" => "text",
              "content" => text
            }
          }

          response = %{
            "jsonrpc" => "2.0",
            "id" => state.pending_prompt_id,
            "result" => %{
              "stopReason" => "end_turn",
              "text" => text,
              "sessionId" => state.session_id
            }
          }

          {:messages, [notification, response], %{state | pending_prompt_id: nil}}

        _ ->
          {:skip, state}
      end
    end

    defp extract_text(nil), do: ""

    defp extract_text(blocks) when is_list(blocks) do
      blocks
      |> Enum.filter(&(&1["type"] == "text"))
      |> Enum.map_join("\n", &(&1["text"] || ""))
    end

    defp extract_text(text) when is_binary(text), do: text
  end

  # Adapter that uses post_connect to perform a handshake with the subprocess.
  # Verifies the skip_and_write return path.
  defmodule HandshakeAdapter do
    @behaviour ExMCP.ACP.Adapter

    defstruct phase: :initializing, handshake_done: false

    @impl true
    def init(_opts), do: {:ok, %__MODULE__{}}

    @impl true
    def command(_opts), do: {"cat", []}

    @impl true
    def capabilities, do: %{"handshake" => true}

    @impl true
    def post_connect(state) do
      request =
        Jason.encode!(%{
          "id" => 0,
          "method" => "initialize",
          "params" => %{"clientInfo" => %{"name" => "test"}}
        }) <> "\n"

      {:ok, request, state}
    end

    @impl true
    def translate_outbound(%{"method" => "initialize"}, state) do
      {:ok, :skip, state}
    end

    def translate_outbound(_msg, state), do: {:ok, :skip, state}

    @impl true
    def translate_inbound(line, state) do
      case Jason.decode(String.trim(line)) do
        {:ok, %{"id" => 0, "method" => "initialize"}} ->
          # cat echoed our initialize request — complete the handshake
          state = %{state | phase: :ready, handshake_done: true}
          initialized = Jason.encode!(%{"method" => "initialized"}) <> "\n"
          {:skip_and_write, initialized, state}

        {:ok, %{"method" => "initialized"}} ->
          # cat echoed the initialized notification — ignore
          {:skip, state}

        _ ->
          {:skip, state}
      end
    end
  end

  # Adapter that uses messages_and_write return to send data back while producing messages.
  defmodule WriteBackAdapter do
    @behaviour ExMCP.ACP.Adapter

    defstruct ack_count: 0

    @impl true
    def init(_opts), do: {:ok, %__MODULE__{}}

    @impl true
    def command(_opts), do: {"cat", []}

    @impl true
    def translate_outbound(%{"method" => "initialize"}, state), do: {:ok, :skip, state}

    def translate_outbound(%{"method" => "session/prompt", "params" => params}, state) do
      text = get_in(params, ["content", Access.at(0), "text"]) || ""
      data = Jason.encode!(%{"_trigger" => "ack", "_text" => text}) <> "\n"
      {:ok, data, state}
    end

    def translate_outbound(_msg, state), do: {:ok, :skip, state}

    @impl true
    def translate_inbound(line, state) do
      case Jason.decode(String.trim(line)) do
        {:ok, %{"_trigger" => "ack", "_text" => text}} ->
          state = %{state | ack_count: state.ack_count + 1}

          notification = %{
            "jsonrpc" => "2.0",
            "method" => "session/update",
            "params" => %{"kind" => "text", "content" => text}
          }

          # Write an ack back to the subprocess while also producing a message
          ack = Jason.encode!(%{"_ack" => state.ack_count}) <> "\n"
          {:messages_and_write, [notification], ack, state}

        {:ok, %{"_ack" => _count}} ->
          # Ignore the echo of our ack
          {:skip, state}

        _ ->
          {:skip, state}
      end
    end
  end

  describe "AdapterTransport full lifecycle" do
    test "connect + init + session/new + prompt round-trip" do
      {:ok, transport} = AdapterTransport.connect(adapter: IntegrationAdapter, adapter_opts: [])

      # 1. Receive synthesized init response
      {:ok, init_raw, transport} = AdapterTransport.receive_message(transport)
      init_msg = Jason.decode!(init_raw)
      assert init_msg["result"]["agentInfo"]["name"] == "integrationadapter"
      assert init_msg["result"]["capabilities"]["streaming"] == true

      # 2. Send session/new
      {:ok, transport} =
        AdapterTransport.send_message(
          Jason.encode!(%{
            "jsonrpc" => "2.0",
            "method" => "session/new",
            "id" => 1,
            "params" => %{}
          }),
          transport
        )

      {:ok, new_raw, transport} = AdapterTransport.receive_message(transport)
      new_resp = Jason.decode!(new_raw)
      assert new_resp["id"] == 1
      assert new_resp["result"]["sessionId"] == "test-session"

      # 3. Send prompt
      {:ok, transport} =
        AdapterTransport.send_message(
          Jason.encode!(%{
            "jsonrpc" => "2.0",
            "method" => "session/prompt",
            "id" => 2,
            "params" => %{
              "sessionId" => "test-session",
              "content" => [%{"type" => "text", "text" => "Hello from integration"}]
            }
          }),
          transport
        )

      # 4. Receive notification + response
      {:ok, update_raw, transport} = AdapterTransport.receive_message(transport)
      update = Jason.decode!(update_raw)
      assert update["method"] == "session/update"
      assert update["params"]["kind"] == "text"
      assert update["params"]["content"] == "Hello from integration"

      {:ok, resp_raw, transport} = AdapterTransport.receive_message(transport)
      resp = Jason.decode!(resp_raw)
      assert resp["id"] == 2
      assert resp["result"]["stopReason"] == "end_turn"
      assert resp["result"]["text"] == "Hello from integration"

      AdapterTransport.close(transport)
    end

    test "multiple sequential prompts maintain correct state" do
      {:ok, transport} = AdapterTransport.connect(adapter: IntegrationAdapter, adapter_opts: [])

      # Drain init
      {:ok, _, transport} = AdapterTransport.receive_message(transport)

      # session/new
      {:ok, transport} =
        AdapterTransport.send_message(
          Jason.encode!(%{
            "jsonrpc" => "2.0",
            "method" => "session/new",
            "id" => 1,
            "params" => %{}
          }),
          transport
        )

      {:ok, _, transport} = AdapterTransport.receive_message(transport)

      # Prompt 1
      {:ok, transport} =
        AdapterTransport.send_message(
          Jason.encode!(%{
            "jsonrpc" => "2.0",
            "method" => "session/prompt",
            "id" => 2,
            "params" => %{
              "sessionId" => "test-session",
              "content" => [%{"type" => "text", "text" => "First"}]
            }
          }),
          transport
        )

      {:ok, u1, transport} = AdapterTransport.receive_message(transport)
      {:ok, r1, transport} = AdapterTransport.receive_message(transport)
      assert Jason.decode!(u1)["params"]["content"] == "First"
      assert Jason.decode!(r1)["id"] == 2

      # Prompt 2
      {:ok, transport} =
        AdapterTransport.send_message(
          Jason.encode!(%{
            "jsonrpc" => "2.0",
            "method" => "session/prompt",
            "id" => 3,
            "params" => %{
              "sessionId" => "test-session",
              "content" => [%{"type" => "text", "text" => "Second"}]
            }
          }),
          transport
        )

      {:ok, u2, transport} = AdapterTransport.receive_message(transport)
      {:ok, r2, _transport} = AdapterTransport.receive_message(transport)
      assert Jason.decode!(u2)["params"]["content"] == "Second"
      assert Jason.decode!(r2)["id"] == 3
      assert Jason.decode!(r2)["result"]["text"] == "Second"

      AdapterTransport.close(transport)
    end

    test "connected? tracks bridge liveness" do
      {:ok, transport} = AdapterTransport.connect(adapter: IntegrationAdapter, adapter_opts: [])
      assert AdapterTransport.connected?(transport)

      AdapterTransport.close(transport)
      refute AdapterTransport.connected?(transport)
    end

    test "close is idempotent" do
      {:ok, transport} = AdapterTransport.connect(adapter: IntegrationAdapter, adapter_opts: [])
      assert :ok = AdapterTransport.close(transport)
      assert :ok = AdapterTransport.close(transport)
    end
  end

  describe "post_connect handshake" do
    test "adapter performs initialize handshake via cat echo" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: HandshakeAdapter, adapter_opts: [])

      # Receive synthesized init (from bridge, not from handshake)
      {:ok, init_raw} = AdapterBridge.receive_message(bridge, 5_000)
      init_msg = Jason.decode!(init_raw)
      assert init_msg["result"]["capabilities"]["handshake"] == true

      # The handshake happened internally:
      # 1. post_connect wrote initialize request to cat
      # 2. cat echoed it back
      # 3. translate_inbound processed it, sent "initialized" via skip_and_write
      # 4. cat echoed "initialized" back (ignored by translate_inbound)

      # Verify bridge is still functional
      assert :ok =
               AdapterBridge.send_message(
                 bridge,
                 Jason.encode!(%{
                   "jsonrpc" => "2.0",
                   "method" => "initialize",
                   "id" => 1,
                   "params" => %{}
                 })
               )

      AdapterBridge.close(bridge)
    end
  end

  describe "messages_and_write return type" do
    test "produces messages and writes data back to port" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: WriteBackAdapter, adapter_opts: [])

      # Drain init
      {:ok, _} = AdapterBridge.receive_message(bridge, 5_000)

      # Send a prompt that triggers the write-back flow
      prompt_msg = %{
        "jsonrpc" => "2.0",
        "method" => "session/prompt",
        "id" => 1,
        "params" => %{
          "sessionId" => "s1",
          "content" => [%{"type" => "text", "text" => "write-back test"}]
        }
      }

      :ok = AdapterBridge.send_message(bridge, Jason.encode!(prompt_msg))

      # Receive the notification produced by messages_and_write
      {:ok, raw} = AdapterBridge.receive_message(bridge, 5_000)
      msg = Jason.decode!(raw)
      assert msg["method"] == "session/update"
      assert msg["params"]["content"] == "write-back test"

      # The ack was written back to cat, which echoed it.
      # translate_inbound skips the ack echo, so no more messages.

      AdapterBridge.close(bridge)
    end
  end

  describe "error scenarios" do
    test "send to closed bridge returns error" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: IntegrationAdapter, adapter_opts: [])
      AdapterBridge.close(bridge)

      # Bridge process is stopped — GenServer.call will exit
      assert catch_exit(
               AdapterBridge.send_message(
                 bridge,
                 Jason.encode!(%{"method" => "test", "id" => 1})
               )
             )
    end

    test "receive from closed bridge returns error" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: IntegrationAdapter, adapter_opts: [])

      # Drain init
      {:ok, _} = AdapterBridge.receive_message(bridge, 5_000)

      AdapterBridge.close(bridge)

      assert catch_exit(AdapterBridge.receive_message(bridge, 1_000))
    end

    test "invalid JSON sent to bridge returns decode error" do
      {:ok, bridge} = AdapterBridge.start_link(adapter: IntegrationAdapter, adapter_opts: [])

      assert {:error, {:decode_error, _}} =
               AdapterBridge.send_message(bridge, "not valid json {{{")

      AdapterBridge.close(bridge)
    end
  end

  describe "Codex adapter translate chain" do
    @moduletag :codex_translate

    alias ExMCP.ACP.Adapters.Codex

    test "full outbound→inbound flow for thread lifecycle" do
      {:ok, state} = Codex.init(model: "gpt-4o")

      # post_connect sends initialize
      {:ok, init_data, state} = Codex.post_connect(state)
      init_json = IO.iodata_to_binary(init_data)
      {:ok, init_msg} = Jason.decode(init_json)
      assert init_msg["method"] == "initialize"

      # Simulate Codex response
      init_response = Jason.encode!(%{"id" => 1, "result" => %{"capabilities" => %{}}})
      {:skip_and_write, initialized_data, state} = Codex.translate_inbound(init_response, state)
      assert state.phase == :ready
      initialized_json = IO.iodata_to_binary(initialized_data)
      assert Jason.decode!(initialized_json)["method"] == "initialized"

      # session/new → thread/start
      {:ok, thread_data, state} =
        Codex.translate_outbound(
          %{"method" => "session/new", "id" => 10, "params" => %{}},
          state
        )

      thread_json = IO.iodata_to_binary(thread_data)
      {:ok, thread_msg} = Jason.decode(thread_json)
      assert thread_msg["method"] == "thread/start"
      assert thread_msg["params"]["model"] == "gpt-4o"

      # Simulate Codex thread/start response
      thread_response =
        Jason.encode!(%{
          "id" => thread_msg["id"],
          "result" => %{"thread" => %{"id" => "thread-42"}}
        })

      {:messages, [new_resp], state} = Codex.translate_inbound(thread_response, state)
      assert new_resp["id"] == 10
      assert new_resp["result"]["sessionId"] == "thread-42"
      assert state.thread_id == "thread-42"

      # session/prompt → turn/start
      {:ok, turn_data, state} =
        Codex.translate_outbound(
          %{
            "method" => "session/prompt",
            "id" => 11,
            "params" => %{
              "sessionId" => "thread-42",
              "content" => [%{"type" => "text", "text" => "Fix the tests"}]
            }
          },
          state
        )

      turn_json = IO.iodata_to_binary(turn_data)
      {:ok, turn_msg} = Jason.decode(turn_json)
      assert turn_msg["method"] == "turn/start"
      assert turn_msg["params"]["threadId"] == "thread-42"
      assert turn_msg["params"]["input"] == "Fix the tests"

      # Simulate turn/start response
      turn_response =
        Jason.encode!(%{
          "id" => turn_msg["id"],
          "result" => %{"turn" => %{"id" => "turn-99"}}
        })

      {:skip, state} = Codex.translate_inbound(turn_response, state)
      assert state.turn_id == "turn-99"

      # Simulate streaming text delta
      delta1 =
        Jason.encode!(%{
          "method" => "item/agentMessage/delta",
          "params" => %{"delta" => "I'll fix ", "threadId" => "thread-42", "turnId" => "turn-99"}
        })

      {:messages, [upd1], state} = Codex.translate_inbound(delta1, state)
      assert upd1["params"]["kind"] == "text"
      assert upd1["params"]["content"] == "I'll fix "

      delta2 =
        Jason.encode!(%{
          "method" => "item/agentMessage/delta",
          "params" => %{"delta" => "the tests.", "threadId" => "thread-42", "turnId" => "turn-99"}
        })

      {:messages, [upd2], state} = Codex.translate_inbound(delta2, state)
      assert upd2["params"]["content"] == "the tests."

      # Simulate turn/completed
      completed =
        Jason.encode!(%{
          "method" => "turn/completed",
          "params" => %{
            "turn" => %{"id" => "turn-99", "status" => "completed"},
            "threadId" => "thread-42"
          }
        })

      {:messages, [final], _state} = Codex.translate_inbound(completed, state)
      assert final["id"] == 11
      assert final["result"]["text"] == "I'll fix the tests."
      assert final["result"]["stopReason"] == "end_turn"
      assert final["result"]["sessionId"] == "thread-42"
    end
  end

  describe "Claude adapter translate chain" do
    @moduletag :claude_translate

    alias ExMCP.ACP.Adapters.Claude

    test "full outbound→inbound flow for session lifecycle" do
      {:ok, state} = Claude.init(model: "sonnet")

      # session/new → skip (Claude doesn't have explicit sessions)
      {:ok, :skip, state} =
        Claude.translate_outbound(
          %{"method" => "session/new", "id" => 1, "params" => %{}},
          state
        )

      # session/prompt → stream-json stdin format
      {:ok, prompt_data, state} =
        Claude.translate_outbound(
          %{
            "method" => "session/prompt",
            "id" => 2,
            "params" => %{
              "sessionId" => "default",
              "content" => [%{"type" => "text", "text" => "Hello Claude"}]
            }
          },
          state
        )

      prompt_json = IO.iodata_to_binary(prompt_data)
      {:ok, prompt_msg} = Jason.decode(prompt_json)
      assert prompt_msg["type"] == "user"
      assert prompt_msg["message"]["content"] == "Hello Claude"

      # Simulate text streaming (Claude wraps events in stream_event envelope)
      delta_line =
        Jason.encode!(%{
          "type" => "stream_event",
          "event" => %{
            "type" => "content_block_delta",
            "delta" => %{"type" => "text_delta", "text" => "Hi there!"}
          }
        })

      {:messages, [upd], state} = Claude.translate_inbound(delta_line, state)
      assert upd["params"]["kind"] == "text"
      assert upd["params"]["content"] == "Hi there!"

      # Simulate result (usage is at top level of the result event)
      result_line =
        Jason.encode!(%{
          "type" => "result",
          "usage" => %{"input_tokens" => 10, "output_tokens" => 5}
        })

      {:messages, msgs, _state} = Claude.translate_inbound(result_line, state)
      # Result produces status update + response (2 messages)
      resp = Enum.find(msgs, &(&1["id"] == 2))
      assert resp["result"]["usage"]["inputTokens"] == 10
    end
  end
end
