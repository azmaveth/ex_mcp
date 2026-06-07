defmodule ExMCP.ACP.Adapters.ClaudeSDK.SessionStoreTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.ClaudeSDK.SessionStore

  @session_id "123e4567-e89b-12d3-a456-426614174000"
  @other_session_id "123e4567-e89b-12d3-a456-426614174001"

  setup do
    root =
      System.tmp_dir!()
      |> Path.join("ex_mcp_claude_store_#{System.unique_integer([:positive])}")

    config_dir = Path.join(root, "claude")
    cwd = Path.join(root, "workspace")

    File.mkdir_p!(config_dir)
    File.mkdir_p!(cwd)

    on_exit(fn -> File.rm_rf!(root) end)

    %{config_dir: config_dir, cwd: cwd}
  end

  describe "project_key/1" do
    test "uses SDK-compatible sanitization for unresolved paths" do
      assert SessionStore.project_key("/not-real/My Project") == "-not-real-My-Project"
    end

    test "hashes keys longer than the SDK limit" do
      long_path = "/not-real/" <> String.duplicate("segment/", 40)
      key = SessionStore.project_key(long_path)

      assert String.length(key) > 200
      assert String.match?(key, ~r/^[a-zA-Z0-9-]+-[a-z0-9]+$/)
    end
  end

  describe "list_acp_sessions/1" do
    test "returns ACP sessions from the matching Claude project directory", %{
      config_dir: config_dir,
      cwd: cwd
    } do
      write_session(config_dir, cwd, @session_id, [
        %{
          "type" => "user",
          "timestamp" => "2026-01-01T00:00:00.000Z",
          "cwd" => cwd,
          "gitBranch" => "main",
          "message" => %{
            "role" => "user",
            "content" => [%{"type" => "text", "text" => "first prompt"}]
          }
        },
        %{"type" => "summary", "summary" => "Summary\nTitle", "gitBranch" => "feature/acp"}
      ])

      assert {:ok, [session]} =
               SessionStore.list_acp_sessions(claude_config_dir: config_dir, cwd: cwd)

      assert session["sessionId"] == @session_id
      assert session["cwd"] == cwd
      assert session["title"] == "Summary Title"
      assert session["updatedAt"] =~ "T"
      assert get_in(session, ["_meta", "ex_mcp.claude_sdk", "fileSize"]) > 0
      assert get_in(session, ["_meta", "ex_mcp.claude_sdk", "firstPrompt"]) == "first prompt"
      assert get_in(session, ["_meta", "ex_mcp.claude_sdk", "gitBranch"]) == "feature/acp"
    end

    test "filters sidechains, invalid names, empty files, and untitled sessions", %{
      config_dir: config_dir,
      cwd: cwd
    } do
      project_dir = project_dir(config_dir, cwd)
      File.mkdir_p!(project_dir)

      write_session(config_dir, cwd, @session_id, [
        %{
          "type" => "user",
          "cwd" => cwd,
          "message" => %{"role" => "user", "content" => "visible prompt"}
        }
      ])

      write_session(config_dir, cwd, @other_session_id, [
        %{"isSidechain" => true, "summary" => "sidechain", "cwd" => cwd}
      ])

      File.write!(Path.join(project_dir, "not-a-uuid.jsonl"), ~s({"summary":"invalid"}\n))
      File.write!(Path.join(project_dir, "123e4567-e89b-12d3-a456-426614174002.jsonl"), "")
      File.write!(Path.join(project_dir, "123e4567-e89b-12d3-a456-426614174003.jsonl"), "{}\n")

      assert {:ok, [session]} =
               SessionStore.list_acp_sessions(claude_config_dir: config_dir, cwd: cwd)

      assert session["sessionId"] == @session_id
      assert session["title"] == "visible prompt"
    end

    test "supports numeric cursor and limit for local pagination", %{
      config_dir: config_dir,
      cwd: cwd
    } do
      write_session(config_dir, cwd, @session_id, [
        %{"type" => "summary", "summary" => "first", "cwd" => cwd}
      ])

      write_session(config_dir, cwd, @other_session_id, [
        %{"type" => "summary", "summary" => "second", "cwd" => cwd}
      ])

      assert {:ok, [session]} =
               SessionStore.list_acp_sessions(
                 claude_config_dir: config_dir,
                 cwd: cwd,
                 cursor: "1",
                 limit: 1
               )

      assert session["sessionId"] in [@session_id, @other_session_id]
    end
  end

  describe "delete_session/2" do
    test "removes the jsonl file and sibling artifact directory", %{
      config_dir: config_dir,
      cwd: cwd
    } do
      path =
        write_session(config_dir, cwd, @session_id, [
          %{"type" => "summary", "summary" => "delete me", "cwd" => cwd}
        ])

      sibling_dir = path |> Path.dirname() |> Path.join(@session_id)
      File.mkdir_p!(sibling_dir)
      File.write!(Path.join(sibling_dir, "artifact.txt"), "artifact")

      assert :ok =
               SessionStore.delete_session(@session_id, claude_config_dir: config_dir, cwd: cwd)

      refute File.exists?(path)
      refute File.exists?(sibling_dir)
    end

    test "rejects non-UUID session ids", %{config_dir: config_dir, cwd: cwd} do
      assert {:error, message} =
               SessionStore.delete_session("../bad", claude_config_dir: config_dir, cwd: cwd)

      assert message =~ "Invalid Claude sessionId"
    end
  end

  defp write_session(config_dir, cwd, session_id, entries) do
    project_dir = project_dir(config_dir, cwd)
    File.mkdir_p!(project_dir)

    path = Path.join(project_dir, "#{session_id}.jsonl")

    entries
    |> Enum.map_join("\n", &Jason.encode!/1)
    |> then(&File.write!(path, &1 <> "\n"))

    path
  end

  defp project_dir(config_dir, cwd) do
    config_dir
    |> Path.join("projects")
    |> Path.join(SessionStore.project_key(cwd))
  end
end
