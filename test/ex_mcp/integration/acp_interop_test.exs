defmodule ExMCP.Integration.ACPInteropTest do
  @moduledoc """
  Cross-language ACP interop tests between ExMCP and the official TypeScript ACP SDK.

  These tests verify both protocol roles over real stdio transport:

  1. ExMCP ACP Client -> TypeScript ACP Agent
  2. TypeScript ACP Client -> ExMCP ACP Agent

  Run with: mix test test/ex_mcp/integration/acp_interop_test.exs --include interop
  """

  use ExUnit.Case, async: false

  alias ExMCP.ACP.Client

  @moduletag :interop
  @moduletag :integration
  @moduletag timeout: 60_000

  @interop_dir Path.expand("../../interop", __DIR__)
  @ts_agent_script Path.join(@interop_dir, "acp_ts_agent.mjs")
  @ts_client_script Path.join(@interop_dir, "acp_ts_client.mjs")
  @ts_everything_agent_script Path.join(@interop_dir, "acp_everything_ts_agent.mjs")
  @ts_everything_client_script Path.join(@interop_dir, "acp_everything_ts_client.mjs")
  @node_modules Path.join(@interop_dir, "node_modules")
  @acp_sdk_package Path.join(@node_modules, "@agentclientprotocol/sdk/package.json")

  defmodule EverythingClientHandler do
    @behaviour ExMCP.ACP.Client.Handler

    @impl true
    def init(opts), do: {:ok, %{test_pid: Keyword.fetch!(opts, :test_pid)}}

    @impl true
    def handle_session_update(_session_id, _update, state), do: {:ok, state}

    @impl true
    def handle_permission_request(session_id, tool_call, options, state) do
      send(state.test_pid, {:everything_client_request, "session/request_permission", session_id})
      option = Enum.find(options, &(&1["kind"] == "allow_once")) || List.first(options)

      assert tool_call["toolCallId"]
      {:ok, %{"outcome" => "selected", "optionId" => option["optionId"]}, state}
    end

    @impl true
    def handle_file_read(session_id, path, _opts, state) do
      send(state.test_pid, {:everything_client_request, "fs/read_text_file", session_id})
      {:ok, "mock file content for #{path}", state}
    end

    @impl true
    def handle_file_write(session_id, _path, _content, state) do
      send(state.test_pid, {:everything_client_request, "fs/write_text_file", session_id})
      {:ok, state}
    end

    @impl true
    def handle_terminal_request(method, params, _id, state) do
      send(state.test_pid, {:everything_client_request, method, params["sessionId"]})

      result =
        case method do
          "terminal/create" ->
            %{"terminalId" => "term_elixir_everything"}

          "terminal/output" ->
            %{
              "output" => "terminal output from ExMCP client",
              "truncated" => false,
              "exitStatus" => %{"exitCode" => 0}
            }

          "terminal/wait_for_exit" ->
            %{"exitCode" => 0}

          "terminal/kill" ->
            %{}

          "terminal/release" ->
            %{}
        end

      {:ok, result, state}
    end
  end

  setup_all do
    case System.find_executable("node") do
      nil ->
        ExUnit.configure(exclude: [interop: true])
        :ok

      node_path ->
        ensure_npm_deps!()
        Application.ensure_all_started(:ex_mcp)
        {:ok, node_path: node_path}
    end
  end

  describe "ExMCP ACP Client -> TypeScript ACP Agent" do
    test "connects, prompts, and folds streamed text into the prompt result", context do
      skip_without_node(context)

      {:ok, client} =
        ExMCP.ACP.start_client(
          command: [context.node_path, @ts_agent_script],
          cd: @interop_dir,
          event_listener: self(),
          client_info: %{"name" => "ex-mcp-acp-interop-client", "version" => "1.0.0"}
        )

      try do
        {:ok, %{"sessionId" => session_id}} =
          Client.new_session(client, File.cwd!(), timeout: 10_000)

        {:ok, result} =
          Client.prompt(client, session_id, "hello from Elixir client", timeout: 10_000)

        assert result["stopReason"] == "end_turn"
        assert result["text"] == "Hello from TypeScript ACP agent: hello from Elixir client"

        assert_receive {:acp_session_update, ^session_id,
                        %{
                          "sessionUpdate" => "agent_message_chunk",
                          "content" => %{"text" => "Hello from "}
                        }},
                       5_000

        assert_receive {:acp_session_update, ^session_id,
                        %{
                          "sessionUpdate" => "agent_message_chunk",
                          "content" => %{
                            "text" => "TypeScript ACP agent: hello from Elixir client"
                          }
                        }},
                       5_000

        {:ok, %{"sessions" => sessions}} = Client.list_sessions(client, timeout: 10_000)
        assert Enum.any?(sessions, &(&1["sessionId"] == session_id))

        assert {:ok, _} = Client.close_session(client, session_id, timeout: 10_000)
      after
        Client.disconnect(client)
      end
    end
  end

  describe "TypeScript ACP Client -> ExMCP ACP Agent" do
    test "connects, prompts, and receives streamed updates", context do
      skip_without_node(context)

      mix_path = System.find_executable("mix")
      project_dir = Path.expand("../../..", __DIR__)

      {output, exit_code} =
        System.cmd(
          "node",
          [@ts_client_script, mix_path, "acp.interop_agent"],
          cd: project_dir,
          stderr_to_stdout: true,
          env: [{"MIX_ENV", "test"}]
        )

      results = parse_ts_client_results(output)

      assert results["connected"] == true,
             "TS ACP client failed to connect: #{inspect(results)}"

      assert results["success"] == true,
             "TS ACP client operations failed: #{inspect(results)}"

      assert results["stopReason"] == "end_turn"
      assert results["text"] =~ "Hello from ExMCP ACP agent"
      assert "agent_message_chunk" in results["updates"]
      assert results["sessionId"] in results["sessions"]

      assert exit_code == 0,
             "TS ACP client exited with code #{exit_code}: #{output}"
    end
  end

  describe "everything-style ACP interop" do
    test "ExMCP client covers the TypeScript everything agent stable surface", context do
      skip_without_node(context)

      {:ok, client} =
        ExMCP.ACP.start_client(
          command: [context.node_path, @ts_everything_agent_script],
          cd: @interop_dir,
          event_listener: self(),
          handler: EverythingClientHandler,
          handler_opts: [test_pid: self()],
          client_info: %{"name" => "ex-mcp-acp-everything-client", "version" => "1.0.0"},
          capabilities: %{
            "fs" => %{"readTextFile" => true, "writeTextFile" => true},
            "terminal" => true
          }
        )

      try do
        assert {:ok, %{}} = Client.authenticate(client, "agent", timeout: 10_000)

        {:ok, %{"sessionId" => session_id} = session} =
          Client.new_session(client, File.cwd!(), timeout: 10_000)

        assert get_in(session, ["modes", "currentModeId"]) == "code"
        assert Enum.any?(session["configOptions"], &(&1["id"] == "model"))

        assert {:ok, _} = Client.load_session(client, session_id, File.cwd!(), timeout: 10_000)
        assert {:ok, _} = Client.resume_session(client, session_id, File.cwd!(), timeout: 10_000)

        {:ok, %{"sessions" => sessions}} = Client.list_sessions(client, timeout: 10_000)
        assert Enum.any?(sessions, &(&1["sessionId"] == session_id))

        assert {:ok, %{}} = Client.set_mode(client, session_id, "plan")

        assert {:ok, %{"configOptions" => config_options}} =
                 Client.set_config_option(client, session_id, "model", "deep")

        assert Enum.find(config_options, &(&1["id"] == "model"))["currentValue"] == "deep"

        {:ok, result} =
          Client.prompt(
            client,
            session_id,
            [
              %{"type" => "text", "text" => "hello from Elixir everything client"},
              %{"type" => "image", "mimeType" => "image/png", "data" => "aW1hZ2U="},
              %{"type" => "audio", "mimeType" => "audio/wav", "data" => "YXVkaW8="},
              %{"type" => "resource_link", "uri" => "file:///README.md", "name" => "README.md"},
              %{
                "type" => "resource",
                "resource" => %{
                  "uri" => "file:///inline.txt",
                  "mimeType" => "text/plain",
                  "text" => "inline resource"
                }
              }
            ],
            timeout: 10_000
          )

        assert result["stopReason"] == "end_turn"
        assert result["text"] == "Hello from TypeScript everything agent"

        request_methods = collect_everything_client_requests(session_id, 8)

        assert Enum.sort(request_methods) ==
                 Enum.sort([
                   "session/request_permission",
                   "fs/read_text_file",
                   "fs/write_text_file",
                   "terminal/create",
                   "terminal/output",
                   "terminal/wait_for_exit",
                   "terminal/kill",
                   "terminal/release"
                 ])

        update_types = collect_update_types(session_id, 11)

        for type <- [
              "user_message_chunk",
              "agent_message_chunk",
              "agent_thought_chunk",
              "tool_call",
              "tool_call_update",
              "plan",
              "available_commands_update",
              "current_mode_update",
              "config_option_update",
              "session_info_update",
              "usage_update"
            ] do
          assert type in update_types
        end

        cancel_task = Task.async(fn -> Client.prompt(client, session_id, "cancel-me") end)

        assert_receive {:acp_session_update, ^session_id,
                        %{"content" => %{"text" => "waiting for cancel"}}},
                       5_000

        :ok = Client.cancel(client, session_id)
        assert {:ok, %{"stopReason" => "cancelled"}} = Task.await(cancel_task, 10_000)

        assert {:ok, _} = Client.close_session(client, session_id, timeout: 10_000)
        assert {:ok, %{}} = Client.logout(client, timeout: 10_000)
      after
        Client.disconnect(client)
      end
    end

    test "TypeScript client covers the ExMCP everything agent stable surface", context do
      skip_without_node(context)

      mix_path = System.find_executable("mix")
      project_dir = Path.expand("../../..", __DIR__)

      {output, exit_code} =
        System.cmd(
          "node",
          [@ts_everything_client_script, mix_path, "acp.everything_agent"],
          cd: project_dir,
          stderr_to_stdout: true,
          env: [{"MIX_ENV", "test"}]
        )

      results = parse_ts_client_results(output)

      assert results["connected"] == true,
             "TS ACP everything client failed to connect: #{inspect(results)}"

      assert results["success"] == true,
             "TS ACP everything operations failed: #{inspect(results)}"

      assert results["stopReason"] == "end_turn"
      assert results["cancelStopReason"] == "cancelled"
      assert results["text"] =~ "Hello from ExMCP everything agent"

      for method <- [
            "session/request_permission",
            "fs/read_text_file",
            "fs/write_text_file",
            "terminal/create",
            "terminal/output",
            "terminal/wait_for_exit",
            "terminal/kill",
            "terminal/release"
          ] do
        assert method in results["clientRequests"]
      end

      for type <- [
            "user_message_chunk",
            "agent_message_chunk",
            "agent_thought_chunk",
            "tool_call",
            "tool_call_update",
            "plan",
            "available_commands_update",
            "current_mode_update",
            "config_option_update",
            "session_info_update",
            "usage_update"
          ] do
        assert type in results["updates"]
      end

      assert "session/load" in results["lifecycle"]
      assert "session/resume" in results["lifecycle"]
      assert "session/set_mode" in results["lifecycle"]
      assert "session/set_config_option" in results["lifecycle"]
      assert "session/delete" in results["lifecycle"]
      assert "logout" in results["lifecycle"]

      assert exit_code == 0,
             "TS ACP everything client exited with code #{exit_code}: #{output}"
    end
  end

  defp ensure_npm_deps! do
    unless File.dir?(@node_modules) and File.exists?(@acp_sdk_package) do
      {output, 0} = System.cmd("npm", ["install"], cd: @interop_dir, stderr_to_stdout: true)
      output
    end
  end

  defp skip_without_node(context) do
    unless Map.has_key?(context, :node_path) do
      ExUnit.Assertions.flunk("Node.js not available, skipping ACP interop tests")
    end
  end

  defp parse_ts_client_results(output) do
    output
    |> String.split("\n")
    |> Enum.reverse()
    |> Enum.find_value(%{}, fn line ->
      line = String.trim(line)

      if String.starts_with?(line, "{") do
        case Jason.decode(line) do
          {:ok, map} -> map
          _ -> nil
        end
      end
    end)
  end

  defp collect_everything_client_requests(session_id, count, acc \\ []) do
    if length(acc) == count do
      acc
    else
      receive do
        {:everything_client_request, method, ^session_id} ->
          collect_everything_client_requests(session_id, count, [method | acc])
      after
        5_000 ->
          Enum.reverse(acc)
      end
    end
  end

  defp collect_update_types(session_id, expected_count, acc \\ []) do
    if length(Enum.uniq(acc)) >= expected_count do
      acc
    else
      receive do
        {:acp_session_update, ^session_id, %{"sessionUpdate" => type}} ->
          collect_update_types(session_id, expected_count, [type | acc])
      after
        5_000 ->
          Enum.reverse(acc)
      end
    end
  end
end
