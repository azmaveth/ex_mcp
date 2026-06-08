defmodule ExMCP.ACP.Adapters.ClaudeSDKTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.ClaudeSDK
  alias ExMCP.ACP.Adapters.ClaudeSDK.SessionStore
  alias ExMCP.ACP.PromptQueue

  setup do
    {:ok, state} = ClaudeSDK.init(cwd: "/tmp/project", model: "sonnet")
    %{state: state}
  end

  describe "command/1 and env/1" do
    test "uses SDK-compatible Claude Code flags by default" do
      {cmd, args} = ClaudeSDK.command([])

      assert cmd == "claude"
      assert ["--output-format", "stream-json"] = Enum.slice(args, 0, 2)
      assert "--input-format" in args
      assert "--verbose" in args
      assert "--permission-prompt-tool" in args
      assert "stdio" in args
      assert "--include-partial-messages" in args
      assert "--permission-mode" in args
      assert "default" in args
    end

    test "exposes SDK entrypoint env" do
      assert ClaudeSDK.env([]) == %{
               "CLAUDE_CODE_ENTRYPOINT" => "sdk-ts",
               "CLAUDE_AGENT_SDK_VERSION" => "0.3.165"
             }
    end

    test "passes session and mcp options through to Claude Code" do
      {_cmd, args} =
        ClaudeSDK.command(
          model: "opus",
          additional_directories: ["/tmp/shared"],
          mcp_servers: %{"docs" => %{"command" => "docs-mcp"}},
          resume: "sess_1"
        )

      assert "--model" in args
      assert "opus" in args
      assert "--add-dir" in args
      assert "/tmp/shared" in args
      assert "--mcp-config" in args
      assert Enum.any?(args, &String.contains?(&1, "\"docs\""))
      assert "--resume" in args
      assert "sess_1" in args
    end

    test "bypass permission mode requires the explicit dangerous opt-in flag" do
      {_cmd, args} = ClaudeSDK.command(permission_mode: :bypass)

      assert "--permission-mode" in args
      assert "bypassPermissions" in args
      assert "--allow-dangerously-skip-permissions" in args
    end
  end

  describe "capabilities/0" do
    test "advertises disk-backed session store operations" do
      capabilities = ClaudeSDK.capabilities()
      session_capabilities = capabilities["sessionCapabilities"]

      assert Map.has_key?(session_capabilities, "list")
      assert Map.has_key?(session_capabilities, "delete")
      assert Map.has_key?(session_capabilities, "resume")
      assert Map.has_key?(session_capabilities, "close")
      assert Map.has_key?(session_capabilities, "fork")
      assert capabilities["mcpCapabilities"]["acp"] == true
      assert capabilities["mcpCapabilities"]["http"] == true
      assert capabilities["mcpCapabilities"]["sse"] == true

      assert get_in(capabilities, [
               "mcpCapabilities",
               "_meta",
               "ex_mcp.mcpCapabilities",
               "native"
             ]) == true
    end
  end

  describe "post_connect/1" do
    test "sends SDK initialize control request", %{state: state} do
      assert {:ok, line, state} = ClaudeSDK.post_connect(state)
      decoded = Jason.decode!(line)

      assert decoded["type"] == "control_request"
      assert decoded["request"]["subtype"] == "initialize"
      assert Map.has_key?(state.pending_controls, decoded["request_id"])
    end
  end

  describe "session lifecycle" do
    test "session/new returns adapter-provided session setup", %{state: state} do
      msg = %{
        "id" => 1,
        "method" => "session/new",
        "params" => %{"cwd" => "/tmp/project"}
      }

      assert {:reply, result, state} = ClaudeSDK.translate_outbound(msg, state)

      assert result["sessionId"] == state.session_id
      assert result["modes"]["currentModeId"] == "default"
      assert Enum.any?(result["configOptions"], &(&1["id"] == "model"))
    end

    test "session/list reads Claude SDK sessions from disk" do
      {config_dir, cwd, session_id} = write_store_fixture("list me")
      {:ok, state} = ClaudeSDK.init(cwd: cwd, claude_config_dir: config_dir)

      assert {:ok, [session], _state} = ClaudeSDK.list_sessions(%{"cwd" => cwd}, state)
      assert session["sessionId"] == session_id
      assert session["cwd"] == cwd
      assert session["title"] == "list me"
    end

    test "session/delete removes Claude SDK sessions from disk" do
      {config_dir, cwd, session_id} = write_store_fixture("delete me")

      {:ok, state} =
        ClaudeSDK.init(cwd: cwd, claude_config_dir: config_dir, session_id: session_id)

      msg = %{
        "id" => 11,
        "method" => "session/delete",
        "params" => %{"sessionId" => session_id, "cwd" => cwd}
      }

      assert {:reply, %{}, state} = ClaudeSDK.translate_outbound(msg, state)
      assert state.session_id == nil
      assert {:ok, [], _state} = ClaudeSDK.list_sessions(%{"cwd" => cwd}, state)
    end

    test "session/load replays persisted transcript before replying" do
      {config_dir, cwd, session_id} =
        write_store_fixture([
          %{
            "type" => "user",
            "uuid" => "user-1",
            "cwd" => cwd_placeholder(),
            "message" => %{"role" => "user", "content" => "hello"}
          },
          %{
            "type" => "assistant",
            "uuid" => "assistant-1",
            "session_id" => session_id_placeholder(),
            "message" => %{
              "role" => "assistant",
              "content" => [%{"type" => "text", "text" => "hi there"}]
            }
          }
        ])

      {:ok, state} = ClaudeSDK.init(cwd: cwd, claude_config_dir: config_dir)

      msg = %{
        "id" => 12,
        "method" => "session/load",
        "params" => %{"sessionId" => session_id, "cwd" => cwd}
      }

      assert {:messages_and_reply, messages, result, state} =
               ClaudeSDK.translate_outbound(msg, state)

      assert result["sessionId"] == session_id

      updates = Enum.map(messages, &get_in(&1, ["params", "update", "sessionUpdate"]))
      assert "user_message_chunk" in updates
      assert "agent_message_chunk" in updates
      assert Map.has_key?(state.message_ids, "user-1")
    end

    test "session/fork copies persisted transcript and returns a new session id" do
      {config_dir, cwd, session_id} = write_store_fixture("fork me")
      {:ok, state} = ClaudeSDK.init(cwd: cwd, claude_config_dir: config_dir)

      assert {:ok, result, state} =
               ClaudeSDK.fork_session(%{"sessionId" => session_id, "cwd" => cwd}, state)

      forked_id = result["sessionId"]
      assert forked_id != session_id
      assert state.session_id == forked_id

      assert {:ok, [_entry]} =
               SessionStore.read_session_messages(forked_id,
                 claude_config_dir: config_dir,
                 cwd: cwd
               )
    end

    test "session/cancel writes SDK interrupt control request", %{state: state} do
      msg = %{"method" => "session/cancel", "params" => %{"sessionId" => "s1"}}

      assert {:ok, line, state} = ClaudeSDK.translate_outbound(msg, state)
      decoded = Jason.decode!(line)

      assert decoded["type"] == "control_request"
      assert decoded["request"]["subtype"] == "interrupt"
      assert Map.has_key?(state.pending_controls, decoded["request_id"])
    end
  end

  describe "prompt translation" do
    test "converts ACP prompt blocks into SDK user message", %{state: state} do
      msg = %{
        "id" => 10,
        "method" => "session/prompt",
        "params" => %{
          "sessionId" => "s1",
          "prompt" => [
            %{"type" => "text", "text" => "hello"},
            %{"type" => "resource_link", "uri" => "file:///tmp/project/lib/a.ex"}
          ]
        }
      }

      assert {:ok, line, state} = ClaudeSDK.translate_outbound(msg, state)
      decoded = Jason.decode!(line)

      assert decoded["type"] == "user"
      assert decoded["session_id"] == "s1"
      assert get_in(decoded, ["message", "content", Access.at(0), "text"]) == "hello"

      assert get_in(decoded, ["message", "content", Access.at(1), "text"]) =~
               "file:///tmp/project/lib/a.ex"

      assert state.pending_prompt_id == 10
    end

    test "queues a prompt while another prompt is active and drains it after result", %{
      state: state
    } do
      first = %{
        "id" => 10,
        "method" => "session/prompt",
        "params" => %{"sessionId" => "s1", "prompt" => [%{"type" => "text", "text" => "one"}]}
      }

      second = %{
        "id" => 11,
        "method" => "session/prompt",
        "params" => %{"sessionId" => "s1", "prompt" => [%{"type" => "text", "text" => "two"}]}
      }

      assert {:ok, _line, state} = ClaudeSDK.translate_outbound(first, state)
      assert {:ok, :skip, state} = ClaudeSDK.translate_outbound(second, state)
      assert PromptQueue.len(state.prompt_queue) == 1

      result = %{
        "type" => "result",
        "session_id" => "s1",
        "stop_reason" => "end_turn",
        "usage" => %{},
        "result" => "done"
      }

      assert {:messages_and_write, messages, [write], state} =
               ClaudeSDK.translate_inbound(Jason.encode!(result), state)

      assert Enum.any?(messages, &(&1["id"] == 10))
      assert Jason.decode!(write)["message"]["content"] == [%{"type" => "text", "text" => "two"}]
      assert state.pending_prompt_id == 11
      assert PromptQueue.empty?(state.prompt_queue)
    end
  end

  describe "permission control bridge" do
    test "maps SDK can_use_tool request to ACP permission request and response", %{state: state} do
      event = %{
        "type" => "control_request",
        "request_id" => "perm_1",
        "request" => %{
          "subtype" => "can_use_tool",
          "tool_name" => "Bash",
          "tool_use_id" => "toolu_1",
          "input" => %{"command" => "mix test"},
          "decision_reason" => "command needs approval"
        }
      }

      assert {:messages, [permission], state} =
               ClaudeSDK.translate_inbound(Jason.encode!(event), state)

      assert permission["method"] == "session/request_permission"
      assert permission["params"]["toolCall"]["toolCallId"] == "toolu_1"
      assert Enum.any?(permission["params"]["options"], &(&1["kind"] == "allow_once"))

      response = %{
        "id" => permission["id"],
        "result" => %{"outcome" => %{"outcome" => "selected", "optionId" => "allow_once"}}
      }

      assert {:ok, line, _state} = ClaudeSDK.translate_outbound(response, state)
      decoded = Jason.decode!(line)

      assert decoded["type"] == "control_response"
      assert decoded["response"]["request_id"] == "perm_1"
      assert decoded["response"]["response"]["behavior"] == "allow"
    end
  end

  describe "SDK event mapping" do
    test "emits pending tool_call from partial tool start", %{state: state} do
      event = %{
        "type" => "stream_event",
        "session_id" => "s1",
        "event" => %{
          "type" => "content_block_start",
          "content_block" => %{
            "type" => "tool_use",
            "id" => "toolu_read",
            "name" => "Read",
            "input" => %{"file_path" => "/tmp/project/lib/a.ex"}
          }
        }
      }

      assert {:messages, [message], state} =
               ClaudeSDK.translate_inbound(Jason.encode!(event), state)

      update = message["params"]["update"]

      assert message["params"]["sessionId"] == "s1"
      assert update["sessionUpdate"] == "tool_call"
      assert update["status"] == "pending"
      assert update["kind"] == "read"
      assert Map.has_key?(state.tool_calls, "toolu_read")
    end

    test "maps TodoWrite to plan update", %{state: state} do
      event = %{
        "type" => "assistant",
        "session_id" => "s1",
        "message" => %{
          "content" => [
            %{
              "type" => "tool_use",
              "id" => "todo_1",
              "name" => "TodoWrite",
              "input" => %{
                "todos" => [
                  %{"content" => "Read code", "status" => "completed"},
                  %{"content" => "Patch adapter", "status" => "in_progress"}
                ]
              }
            }
          ]
        }
      }

      assert {:messages, messages, _state} =
               ClaudeSDK.translate_inbound(Jason.encode!(event), state)

      plan = Enum.find(messages, &(get_in(&1, ["params", "update", "sessionUpdate"]) == "plan"))

      assert get_in(plan, ["params", "update", "entries", Access.at(0), "status"]) == "completed"

      assert get_in(plan, ["params", "update", "entries", Access.at(1), "status"]) ==
               "in_progress"
    end

    test "does not treat assistant message id as session id", %{state: state} do
      event = %{
        "type" => "assistant",
        "session_id" => "s1",
        "message" => %{
          "id" => "msg_123",
          "content" => [%{"type" => "text", "text" => "hello"}]
        }
      }

      assert {:messages, [message], state} =
               ClaudeSDK.translate_inbound(Jason.encode!(event), state)

      assert message["params"]["sessionId"] == "s1"
      assert state.session_id == "s1"
    end

    test "final result produces ACP prompt response with stop reason and usage", %{state: state} do
      state = %{state | pending_prompt_id: 123, session_id: "s1", text_acc: ["world", "hello "]}

      event = %{
        "type" => "result",
        "subtype" => "success",
        "session_id" => "s1",
        "stop_reason" => "max_tokens",
        "usage" => %{"input_tokens" => 1, "output_tokens" => 2},
        "result" => "ignored when text_acc exists"
      }

      assert {:messages, messages, state} =
               ClaudeSDK.translate_inbound(Jason.encode!(event), state)

      response = Enum.find(messages, &(&1["id"] == 123))

      assert response["result"]["stopReason"] == "max_tokens"
      assert response["result"]["usage"]["inputTokens"] == 1
      assert get_in(response, ["result", "_meta", "ex_mcp.claude_sdk", "text"]) == "hello world"
      assert state.pending_prompt_id == nil
    end
  end

  defp write_store_fixture(summary) when is_binary(summary) do
    write_store_fixture([%{"type" => "summary", "summary" => summary}])
  end

  defp write_store_fixture(entries) when is_list(entries) do
    root =
      System.tmp_dir!()
      |> Path.join("ex_mcp_claude_sdk_adapter_#{System.unique_integer([:positive])}")

    config_dir = Path.join(root, "claude")
    cwd = Path.join(root, "workspace")
    session_id = "123e4567-e89b-12d3-a456-426614174000"

    File.mkdir_p!(cwd)

    project_dir =
      config_dir
      |> Path.join("projects")
      |> Path.join(SessionStore.project_key(cwd))

    File.mkdir_p!(project_dir)

    entries =
      Enum.map(entries, fn entry ->
        entry
        |> replace_placeholder(cwd_placeholder(), cwd)
        |> replace_placeholder(session_id_placeholder(), session_id)
        |> Map.put_new("cwd", cwd)
      end)

    project_dir
    |> Path.join("#{session_id}.jsonl")
    |> File.write!(Enum.map_join(entries, "\n", &Jason.encode!/1) <> "\n")

    on_exit(fn -> File.rm_rf!(root) end)

    {config_dir, cwd, session_id}
  end

  defp replace_placeholder(value, placeholder, replacement) when is_map(value) do
    value
    |> Enum.map(fn {key, nested} ->
      {key, replace_placeholder(nested, placeholder, replacement)}
    end)
    |> Map.new()
  end

  defp replace_placeholder(value, placeholder, replacement) when is_list(value),
    do: Enum.map(value, &replace_placeholder(&1, placeholder, replacement))

  defp replace_placeholder(value, placeholder, replacement) when value == placeholder,
    do: replacement

  defp replace_placeholder(value, _placeholder, _replacement), do: value

  defp cwd_placeholder, do: "__cwd__"
  defp session_id_placeholder, do: "__session_id__"
end
