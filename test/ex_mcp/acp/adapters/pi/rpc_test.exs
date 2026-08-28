defmodule ExMCP.ACP.Adapters.Pi.RPCTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Pi
  alias ExMCP.ACP.Adapters.Pi.RPC

  setup do
    tmp_dir = Path.join(System.tmp_dir!(), "pi_rpc_test_#{System.unique_integer([:positive])}")
    session_dir = Path.join(tmp_dir, "sessions")
    session_map_path = Path.join(tmp_dir, "session-map.json")

    File.mkdir_p!(session_dir)

    {:ok, state} =
      Pi.init(
        cwd: tmp_dir,
        session_dir: session_dir,
        session_map_path: session_map_path,
        managed: false
      )

    on_exit(fn -> File.rm_rf!(tmp_dir) end)

    %{
      state: state,
      tmp_dir: tmp_dir,
      session_dir: session_dir,
      session_map_path: session_map_path
    }
  end

  describe "RPC envelopes for new, load, resume, fork, close, delete, and prompt" do
    test "session/new emits NDJSON control envelopes with unique pi-* correlation ids", %{
      state: state,
      tmp_dir: tmp_dir
    } do
      msg = %{"method" => "session/new", "id" => "acp-new", "params" => %{"cwd" => tmp_dir}}

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      {binary, envelopes} = decode_envelopes(data)

      assert String.ends_with?(binary, "\n")
      assert length(envelopes) == 4

      assert Enum.map(envelopes, & &1["type"]) == [
               "new_session",
               "get_state",
               "get_available_models",
               "get_commands"
             ]

      ids = Enum.map(envelopes, & &1["id"])
      assert Enum.all?(ids, &match?("pi-" <> _, &1))
      assert Enum.uniq(ids) == ids

      for envelope <- envelopes do
        refute Map.has_key?(envelope, "jsonrpc")
        assert map_size(envelope) == 2
      end

      assert map_size(new_state.pending_controls) == 4
      assert MapSet.new(Map.keys(new_state.pending_controls)) == MapSet.new(ids)

      kinds =
        new_state.pending_controls
        |> Map.values()
        |> Enum.map(& &1.kind)
        |> Enum.sort()

      assert kinds == [:commands, :models, :new_session, :state]
      assert Enum.all?(Map.values(new_state.pending_controls), &(&1.rpc_id in ids))
    end

    test "session/load emits switch_session plus replay and catalog envelopes", %{
      state: state,
      tmp_dir: tmp_dir,
      session_map_path: session_map_path
    } do
      session_file = write_mapped_session(session_map_path, tmp_dir, "mapped-session")

      msg = %{
        "method" => "session/load",
        "id" => "acp-load",
        "params" => %{"sessionId" => "mapped-session", "cwd" => tmp_dir}
      }

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      {_binary, envelopes} = decode_envelopes(data)

      assert Enum.map(envelopes, & &1["type"]) == [
               "switch_session",
               "get_messages",
               "get_state",
               "get_available_models",
               "get_commands"
             ]

      assert hd(envelopes)["sessionPath"] == session_file
      assert Enum.all?(envelopes, &match?("pi-" <> _, &1["id"]))
      assert Enum.uniq(Enum.map(envelopes, & &1["id"])) == Enum.map(envelopes, & &1["id"])
      assert map_size(new_state.pending_controls) == 5

      kinds =
        new_state.pending_controls
        |> Map.values()
        |> Enum.map(& &1.kind)
        |> Enum.sort()

      assert kinds == [:commands, :messages, :models, :state, :switch]
    end

    test "session/resume emits switch_session without get_messages", %{
      state: state,
      tmp_dir: tmp_dir,
      session_map_path: session_map_path
    } do
      session_file = write_mapped_session(session_map_path, tmp_dir, "mapped-session")

      msg = %{
        "method" => "session/resume",
        "id" => "acp-resume",
        "params" => %{"sessionId" => "mapped-session", "cwd" => tmp_dir}
      }

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      {_binary, envelopes} = decode_envelopes(data)

      assert Enum.map(envelopes, & &1["type"]) == [
               "switch_session",
               "get_state",
               "get_available_models",
               "get_commands"
             ]

      refute Enum.any?(envelopes, &(&1["type"] == "get_messages"))
      assert hd(envelopes)["sessionPath"] == session_file
      assert map_size(new_state.pending_controls) == 4

      kinds =
        new_state.pending_controls
        |> Map.values()
        |> Enum.map(& &1.kind)
        |> Enum.sort()

      assert kinds == [:commands, :models, :state, :switch]
    end

    test "session/fork is not a Pi RPC command and is skipped", %{state: state} do
      msg = %{
        "method" => "session/fork",
        "id" => "acp-fork",
        "params" => %{"sessionId" => "s1", "cwd" => "/tmp"}
      }

      assert {:ok, :skip, ^state} = Pi.translate_outbound(msg, state)
      assert state.pending_controls == %{}
    end

    test "session/close and session/delete emit no native RPC envelopes", %{
      state: state,
      tmp_dir: tmp_dir,
      session_map_path: session_map_path
    } do
      write_mapped_session(session_map_path, tmp_dir, "mapped-session")
      state = %{state | session_id: "s1"}

      assert {:reply, %{}, close_state} =
               Pi.translate_outbound(
                 %{
                   "method" => "session/close",
                   "id" => "acp-close",
                   "params" => %{"sessionId" => "s1"}
                 },
                 state
               )

      assert close_state.pending_controls == %{}

      assert {:reply, %{}, delete_state} =
               Pi.translate_outbound(
                 %{
                   "method" => "session/delete",
                   "id" => "acp-delete",
                   "params" => %{"sessionId" => "mapped-session"}
                 },
                 state
               )

      assert delete_state.pending_controls == %{}
    end

    test "session/prompt emits a compact prompt envelope correlated as msg-N", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => "acp-prompt",
        "params" => %{"sessionId" => "s1", "prompt" => "Hello"}
      }

      assert {:ok, data, new_state} = Pi.translate_outbound(msg, state)
      {binary, [envelope]} = decode_envelopes(data)

      assert String.ends_with?(binary, "\n")
      refute Map.has_key?(envelope, "jsonrpc")
      assert envelope["type"] == "prompt"
      assert envelope["id"] == "msg-1"
      assert envelope["message"] == "Hello"
      refute Map.has_key?(envelope, "images")
      refute Map.has_key?(envelope, "streamingBehavior")

      assert new_state.pending_prompt == %{
               acp_id: "acp-prompt",
               msg_id: "msg-1",
               cancel_requested: false
             }

      assert new_state.msg_counter == 1
    end

    test "session/prompt increments msg-N ids and omits empty optional fields", %{state: state} do
      first = %{
        "method" => "session/prompt",
        "id" => "acp-1",
        "params" => %{"sessionId" => "s1", "prompt" => "one"}
      }

      assert {:ok, first_data, state} = Pi.translate_outbound(first, state)
      {_binary, [first_envelope]} = decode_envelopes(first_data)
      assert first_envelope["id"] == "msg-1"

      assert {:messages, _queued, state} =
               Pi.translate_outbound(
                 %{
                   "method" => "session/prompt",
                   "id" => "acp-2",
                   "params" => %{"sessionId" => "s1", "prompt" => "two"}
                 },
                 state
               )

      assert {:messages_and_write, _messages, second_data, new_state} =
               Pi.translate_inbound(Jason.encode!(%{"type" => "agent_settled"}), state)

      {_binary, [second_envelope]} = decode_envelopes(second_data)
      assert second_envelope["type"] == "prompt"
      assert second_envelope["id"] == "msg-2"
      assert second_envelope["message"] == "two"
      assert new_state.pending_prompt.msg_id == "msg-2"
      assert new_state.msg_counter == 2
    end

    test "session/cancel writes a compact abort envelope without a correlation id", %{
      state: state
    } do
      state = %{
        state
        | session_id: "s1",
          pending_prompt: %{acp_id: 1, msg_id: "msg-1", cancel_requested: false}
      }

      assert {:messages_and_write, _messages, data, _new_state} =
               Pi.translate_outbound(%{"method" => "session/cancel"}, state)

      {binary, [envelope]} = decode_envelopes(data)
      assert String.ends_with?(binary, "\n")
      assert envelope == %{"type" => "abort"}
      refute Map.has_key?(envelope, "id")
    end
  end

  describe "inbound classification and request-id correlation" do
    test "empty lines, invalid JSON, and unknown maps are skipped", %{state: state} do
      assert {:skip, ^state} = Pi.translate_inbound("", state)
      assert {:skip, ^state} = Pi.translate_inbound("   \n", state)
      assert {:skip, ^state} = Pi.translate_inbound("not-json", state)
      assert {:skip, ^state} = Pi.translate_inbound(~s({"foo":1}), state)
    end

    test "unknown response ids are skipped as uncorrelated", %{state: state} do
      assert {:skip, ^state} =
               Pi.translate_inbound(
                 Jason.encode!(%{
                   "type" => "response",
                   "id" => "pi-missing",
                   "command" => "get_state",
                   "success" => true,
                   "data" => %{}
                 }),
                 state
               )

      assert {:skip, ^state} =
               Pi.translate_inbound(
                 Jason.encode!(%{
                   "type" => "response",
                   "id" => "pi-missing",
                   "command" => "get_state",
                   "success" => false,
                   "error" => "nope"
                 }),
                 state
               )
    end

    test "control-group responses classify success and error against the pending rpc id", %{
      state: state,
      tmp_dir: tmp_dir
    } do
      msg = %{"method" => "session/new", "id" => "acp-new", "params" => %{"cwd" => tmp_dir}}
      assert {:ok, data, state} = Pi.translate_outbound(msg, state)
      ids_by_type = data |> decode_envelopes() |> elem(1) |> Map.new(&{&1["type"], &1["id"]})

      assert {:skip, state} =
               Pi.translate_inbound(
                 Jason.encode!(%{
                   "type" => "response",
                   "id" => ids_by_type["new_session"],
                   "command" => "new_session",
                   "success" => true,
                   "data" => %{}
                 }),
                 state
               )

      refute Map.has_key?(state.pending_controls, ids_by_type["new_session"])
      assert map_size(state.pending_controls) == 3

      assert {:messages, [error], new_state} =
               Pi.translate_inbound(
                 Jason.encode!(%{
                   "type" => "response",
                   "id" => ids_by_type["get_state"],
                   "command" => "get_state",
                   "success" => false,
                   "error" => "boom"
                 }),
                 state
               )

      assert error["id"] == "acp-new"
      assert error["error"]["message"] == "boom"
      assert new_state.pending_controls == %{}
      assert new_state.control_groups == %{}
    end

    test "prompt response errors classify against the pending msg id", %{state: state} do
      msg = %{
        "method" => "session/prompt",
        "id" => "acp-prompt",
        "params" => %{"sessionId" => "s1", "prompt" => "Hello"}
      }

      assert {:ok, data, state} = Pi.translate_outbound(msg, state)
      {_binary, [envelope]} = decode_envelopes(data)

      assert {:messages, [error], new_state} =
               Pi.translate_inbound(
                 Jason.encode!(%{
                   "type" => "response",
                   "id" => envelope["id"],
                   "command" => "prompt",
                   "success" => false,
                   "error" => "model failed"
                 }),
                 state
               )

      assert error["id"] == "acp-prompt"
      assert error["error"]["message"] == "model failed"
      assert new_state.pending_prompt == nil
    end

    test "typed stream events keep their type and do not require a correlation id", %{
      state: state
    } do
      line =
        Jason.encode!(%{
          "type" => "message_update",
          "assistantMessageEvent" => %{"type" => "text_delta", "delta" => "hi"}
        })

      assert {:messages, [chunk], _state} = Pi.translate_inbound(line, state)
      assert chunk["method"] == "session/update"
      assert chunk["params"]["update"]["sessionUpdate"] == "agent_message_chunk"
      assert chunk["params"]["update"]["content"]["text"] == "hi"
    end
  end

  describe "ExMCP.ACP.Adapters.Pi.RPC shapes" do
    test "method names for session lifecycle and prompt control" do
      assert RPC.method(:new_session) == "new_session"
      assert RPC.method(:switch_session) == "switch_session"
      assert RPC.method(:get_state) == "get_state"
      assert RPC.method(:get_messages) == "get_messages"
      assert RPC.method(:get_available_models) == "get_available_models"
      assert RPC.method(:get_commands) == "get_commands"
      assert RPC.method(:prompt) == "prompt"
      assert RPC.method(:abort) == "abort"
      assert RPC.method(:set_thinking_level) == "set_thinking_level"
      assert RPC.method(:set_model) == "set_model"
      assert RPC.method(:set_auto_compaction) == "set_auto_compaction"
      assert RPC.method(:set_auto_retry) == "set_auto_retry"
      assert RPC.method(:set_steering_mode) == "set_steering_mode"
      assert RPC.method(:set_follow_up_mode) == "set_follow_up_mode"
      assert RPC.method(:compact) == "compact"
      assert RPC.method(:export_html) == "export_html"
      assert RPC.method(:get_session_stats) == "get_session_stats"
      assert RPC.method(:set_session_name) == "set_session_name"
      assert RPC.method(:extension_ui_response) == "extension_ui_response"
    end

    test "request and notification envelopes omit jsonrpc, drop nils, and stay NDJSON" do
      request = RPC.request("pi-1", RPC.method(:new_session), %{})
      notification = RPC.notification(RPC.method(:abort))

      compacted =
        RPC.request("pi-2", RPC.method(:switch_session), %{
          "sessionPath" => "/tmp/s.jsonl",
          "extra" => nil
        })

      assert request == %{"id" => "pi-1", "type" => "new_session"}
      assert notification == %{"type" => "abort"}

      assert compacted == %{
               "id" => "pi-2",
               "type" => "switch_session",
               "sessionPath" => "/tmp/s.jsonl"
             }

      refute Map.has_key?(compacted, "extra")
      refute Map.has_key?(request, "jsonrpc")

      encoded = RPC.encode_request("pi-3", RPC.method(:get_state))
      assert String.ends_with?(encoded, "\n")
      refute Map.has_key?(Jason.decode!(String.trim(encoded)), "jsonrpc")

      many =
        RPC.encode_many([
          RPC.request("pi-1", RPC.method(:new_session)),
          RPC.request("pi-2", RPC.method(:get_state))
        ])

      assert String.split(many, "\n", trim: true) |> length() == 2
      assert String.ends_with?(many, "\n")
    end

    test "classifies inbound result, error, typed events, and unknown maps" do
      ok = %{"type" => "response", "id" => "pi-1", "success" => true, "data" => %{"ok" => true}}
      err = %{"type" => "response", "id" => "pi-1", "success" => false, "error" => "boom"}
      event = %{"type" => "message_update", "assistantMessageEvent" => %{"type" => "text_delta"}}

      assert RPC.classify_inbound(ok) == {:response, "pi-1", {:ok, ok}}
      assert RPC.classify_inbound(err) == {:response, "pi-1", {:error, err}}
      assert RPC.classify_inbound(event) == {:event, "message_update", event}
      assert RPC.classify_inbound(%{"foo" => 1}) == :unknown
      assert RPC.decode_line("not-json") == :unknown
      assert RPC.decode_line("   ") == :unknown
      assert RPC.decode_line(Jason.encode!(ok)) == {:response, "pi-1", {:ok, ok}}
    end

    test "request-id correlation shapes format pi-* and msg-* ids" do
      assert RPC.rpc_id(7) == "pi-7"
      assert RPC.prompt_id(3) == "msg-3"
      assert RPC.next_prompt_id(0) == {"msg-1", 1}
      assert RPC.next_prompt_id(4) == {"msg-5", 5}

      assert RPC.control_entry("pi-7", :state, "group-1") == %{
               rpc_id: "pi-7",
               kind: :state,
               group_id: "group-1"
             }
    end
  end

  defp write_mapped_session(session_map_path, tmp_dir, session_id) do
    session_file = Path.join(tmp_dir, "#{session_id}.jsonl")
    File.write!(session_file, "")

    File.write!(
      session_map_path,
      Jason.encode!(%{
        "version" => 1,
        "sessions" => %{
          session_id => %{
            "sessionId" => session_id,
            "cwd" => tmp_dir,
            "sessionFile" => session_file
          }
        }
      })
    )

    session_file
  end

  defp decode_envelopes(data) do
    binary = IO.iodata_to_binary(data)

    envelopes =
      binary
      |> String.split("\n", trim: true)
      |> Enum.map(&Jason.decode!/1)

    {binary, envelopes}
  end
end
