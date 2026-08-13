defmodule ExMCP.Integration.ACPAdapterCLIInteropTest do
  @moduledoc """
  Credential-free smoke tests against the actual CLIs behind all built-in ACP adapters.

  The tests exercise process startup, each vendor's native control protocol, ACP
  initialization, session creation/listing/close, and clean shutdown. They never
  send `session/prompt`, so no LLM request is made. Pi receives an isolated dummy
  OpenAI-compatible model definition because it requires a configured model even
  to create a session; the deliberately unreachable endpoint is never contacted.

  Run all three explicitly with:

      mix test --only interop_acp_cli

  Executables are resolved from PATH or `CLAUDE_CODE_EXECUTABLE`, `CODEX_PATH`,
  and `PI_ACP_PI_COMMAND`. The suite fails when a requested CLI is unavailable
  instead of silently skipping coverage.
  """

  use ExUnit.Case, async: false

  alias ExMCP.ACP.Client
  alias ExMCP.ACP.Adapters.{ClaudeSDK, Codex, Pi}

  @moduletag :external
  @moduletag :integration
  @moduletag :interop_acp_cli
  @moduletag timeout: 90_000

  setup context do
    root =
      Path.join(
        System.tmp_dir!(),
        "ex_mcp_acp_cli_#{context.test}_#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(root)
    on_exit(fn -> cleanup_root(root, 20) end)
    %{root: root}
  end

  test "Claude Code SDK adapter completes a no-model ACP lifecycle", %{root: root} do
    home = mkdir!(root, "home")
    config_dir = mkdir!(root, "claude")

    exercise_adapter(
      ClaudeSDK,
      [
        cli_path: executable!(:claude),
        cwd: root,
        claude_config_dir: config_dir,
        extra_args: ["--bare", "--no-session-persistence"],
        env: [{"HOME", home}, {"CLAUDE_CONFIG_DIR", config_dir}]
      ],
      root
    )
  end

  test "Codex app-server adapter completes a no-model ACP lifecycle", %{root: root} do
    home = mkdir!(root, "home")
    codex_home = mkdir!(root, "codex")

    exercise_adapter(
      Codex,
      [
        codex_path: executable!(:codex),
        cwd: root,
        workspace_roots: [root],
        no_browser: true,
        env: [{"HOME", home}, {"CODEX_HOME", codex_home}, {"NO_BROWSER", "1"}]
      ],
      root
    )
  end

  test "Pi RPC adapter completes a no-model ACP lifecycle", %{root: root} do
    home = mkdir!(root, "home")
    agent_dir = mkdir!(root, "pi-agent")
    session_dir = mkdir!(root, "pi-sessions")

    write_json!(Path.join(agent_dir, "models.json"), %{
      "providers" => %{
        "ex-mcp-interop" => %{
          "api" => "openai-completions",
          "apiKey" => "interop-placeholder",
          "baseUrl" => "http://127.0.0.1:1/v1",
          "models" => [
            %{
              "id" => "no-model-call",
              "name" => "No Model Call",
              "reasoning" => false,
              "input" => ["text"],
              "contextWindow" => 4_096,
              "maxTokens" => 1_024,
              "cost" => %{"input" => 0, "output" => 0, "cacheRead" => 0, "cacheWrite" => 0}
            }
          ]
        }
      }
    })

    write_json!(Path.join(agent_dir, "settings.json"), %{
      "defaultProvider" => "ex-mcp-interop",
      "defaultModel" => "no-model-call",
      "sessionDir" => session_dir,
      "quietStartup" => true
    })

    exercise_adapter(
      Pi,
      [
        cli_path: executable!(:pi),
        cwd: root,
        agent_dir: agent_dir,
        session_dir: session_dir,
        session_map_path: Path.join(root, "pi-session-map.json"),
        env: [
          {"HOME", home},
          {"PI_CODING_AGENT_DIR", agent_dir},
          {"PI_OFFLINE", "1"}
        ]
      ],
      root
    )
  end

  defp exercise_adapter(adapter, adapter_opts, root) do
    assert {:ok, client} =
             ExMCP.ACP.start_client(
               transport_mod: ExMCP.ACP.AdapterTransport,
               adapter: adapter,
               adapter_opts: adapter_opts,
               initialize_timeout: 30_000,
               client_info: %{"name" => "ex-mcp-cli-interop", "version" => "1.0.0"}
             )

    try do
      assert Client.status(client) == :ready

      assert {:ok, %{"sessionCapabilities" => %{"close" => %{}}}} =
               Client.agent_capabilities(client)

      assert {:ok, %{"sessionId" => session_id}} =
               Client.new_session(client, root, timeout: 30_000)

      assert is_binary(session_id) and session_id != ""

      assert {:ok, %{"sessions" => sessions}} =
               Client.list_sessions(client, cwd: root, timeout: 30_000)

      assert is_list(sessions)
      assert {:ok, %{}} = Client.close_session(client, session_id, timeout: 30_000)
    after
      if Process.alive?(client), do: Client.disconnect(client)
    end
  end

  defp executable!(:claude),
    do: resolve_executable!("Claude Code", "CLAUDE_CODE_EXECUTABLE", "claude", [])

  defp executable!(:codex), do: resolve_executable!("Codex", "CODEX_PATH", "codex", [])
  defp executable!(:pi), do: resolve_executable!("Pi", "PI_ACP_PI_COMMAND", "pi", [])

  defp resolve_executable!(name, env_name, command, fallbacks) do
    candidates = [System.get_env(env_name), System.find_executable(command) | fallbacks]

    case Enum.find(candidates, &(is_binary(&1) and &1 != "" and File.regular?(&1))) do
      nil ->
        flunk(
          "#{name} CLI is required for :interop_acp_cli; install `#{command}` or set #{env_name}"
        )

      path ->
        Path.expand(path)
    end
  end

  defp mkdir!(root, name) do
    path = Path.join(root, name)
    File.mkdir_p!(path)
    path
  end

  defp write_json!(path, value), do: File.write!(path, Jason.encode!(value, pretty: true))

  defp cleanup_root(root, attempts) do
    case File.rm_rf(root) do
      {:ok, _removed} ->
        :ok

      {:error, _reason, _file} when attempts > 0 ->
        Process.sleep(25)
        cleanup_root(root, attempts - 1)

      {:error, reason, file} ->
        raise File.Error,
          reason: reason,
          action: "remove files and directories recursively from",
          path: file
    end
  end
end
