defmodule ExMCP.Integration.ACPEcosystemCLIInteropTest do
  @moduledoc """
  Credential-free smoke test for one reviewed native ACP command.

  The selected command comes from `test/interop/acp_compatibility.json`; remote
  catalog data never becomes executable input. Scheduled CI selects one entry
  per isolated matrix job with `ACP_ECOSYSTEM_AGENT_ID`.

      ACP_ECOSYSTEM_AGENT_ID=gemini \
        mix test --only interop_acp_ecosystem
  """

  use ExUnit.Case, async: false

  alias ExMCP.ACP.Client
  alias ExMCP.ACPCompat

  @moduletag :external
  @moduletag :interop_acp_ecosystem
  @moduletag timeout: 120_000

  setup_all do
    agent_id =
      System.get_env("ACP_ECOSYSTEM_AGENT_ID") ||
        flunk("Set ACP_ECOSYSTEM_AGENT_ID to a reviewed manifest entry")

    manifest =
      case ACPCompat.load_manifest() do
        {:ok, manifest} -> manifest
        {:error, reason} -> flunk("Cannot load ACP compatibility manifest: #{inspect(reason)}")
      end

    agent =
      case ACPCompat.interop_agent(manifest, agent_id) do
        {:ok, agent} -> agent
        {:error, :unknown_agent} -> flunk("Unknown reviewed ACP agent: #{agent_id}")
      end

    {:ok, agent: agent}
  end

  setup context do
    root =
      Path.join(
        System.tmp_dir!(),
        "ex_mcp_acp_ecosystem_#{context.agent["id"]}_#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(root)
    on_exit(fn -> cleanup_root(root, 20) end)
    {:ok, root: root}
  end

  test "reviewed agent completes its declared credential-free tier", %{agent: agent, root: root} do
    home = mkdir!(root, "home")
    config_home = mkdir!(root, "config")
    cache_home = mkdir!(root, "cache")
    npm_cache = mkdir!(root, "npm-cache")

    env =
      [
        {"HOME", home},
        {"XDG_CONFIG_HOME", config_home},
        {"XDG_CACHE_HOME", cache_home},
        {"NPM_CONFIG_CACHE", npm_cache},
        {"CI", "1"},
        {"NO_COLOR", "1"}
      ] ++ manifest_env(agent["env"])

    assert {:ok, client} =
             ExMCP.ACP.start_client(
               command: agent["command"],
               cd: root,
               env: env,
               environment_policy: :isolated,
               initialize_timeout: 90_000,
               client_info: %{
                 "name" => "ex_mcp-ecosystem-interop",
                 "version" => "1.0.0"
               }
             )

    try do
      assert Client.status(client) == :ready
      assert {:ok, capabilities} = Client.agent_capabilities(client)
      assert is_map(capabilities)
      assert {:ok, auth_methods} = Client.auth_methods(client)
      assert is_list(auth_methods)

      if agent["tier"] == "session" do
        exercise_session_tier(client, capabilities, root)
      end
    after
      if Process.alive?(client), do: Client.disconnect(client)
    end
  end

  defp exercise_session_tier(client, capabilities, root) do
    assert {:ok, %{"sessionId" => session_id}} =
             Client.new_session(client, root, timeout: 30_000)

    assert is_binary(session_id) and session_id != ""
    session_capabilities = capabilities["sessionCapabilities"] || %{}

    if is_map(session_capabilities["list"]) do
      assert {:ok, %{"sessions" => sessions}} =
               Client.list_sessions(client, cwd: root, timeout: 30_000)

      assert is_list(sessions)
    end

    if is_map(session_capabilities["close"]) do
      assert {:ok, %{}} = Client.close_session(client, session_id, timeout: 30_000)
    end
  end

  defp manifest_env(nil), do: []

  defp manifest_env(env) when is_map(env) do
    Enum.map(env, fn {name, value} -> {name, value} end)
  end

  defp mkdir!(root, name) do
    path = Path.join(root, name)
    File.mkdir_p!(path)
    path
  end

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
