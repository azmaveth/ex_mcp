defmodule ExMCP.ACP.Adapters.Codex.LifecycleGoldenTest do
  @moduledoc """
  Characterization gate for the Codex ACP adapter's initialize and session
  lifecycle wire behavior (see `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Codex
  adapter restructuring" / "Characterization gate").

  Each test drives `ExMCP.ACP.Adapters.Codex` through `ExMCP.Test.CodexGolden`
  and compares the recorded transcript (app-server writes, ACP messages,
  replies, and errors, with generated ids normalized) against a committed
  fixture under `test/fixtures/acp/codex/lifecycle/`. The fixtures pin the
  adapter's current behavior so that the planned internal modularization can
  only change them deliberately.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CODEX_GOLDEN=update`; that run rewrites the fixture and fails on
  purpose, so review the diff and re-run without the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.CodexGolden

  @area "lifecycle"

  test "initialize_handshake" do
    transcript = CodexGolden.assert_golden(@area, "initialize_handshake", handshake_steps())

    assert [
             %{"id" => 1, "method" => "initialize"},
             %{"method" => "initialized"},
             %{"id" => 2, "method" => "model/list"}
           ] =
             CodexGolden.writes(transcript)

    assert CodexGolden.messages(transcript) == []
    assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
  end

  test "session_new_thread_start" do
    steps =
      handshake_steps() ++
        [
          {:note, "ACP client opens a session inside the authorized workspace root"},
          {:outbound,
           %{
             "method" => "session/new",
             "id" => 10,
             "params" => %{"cwd" => "/tmp/project", "mcpServers" => []}
           }},
          {:inbound,
           %{
             "id" => 3,
             "result" => %{
               "model" => "gpt-5",
               "thread" => %{
                 "id" => "thread-abc",
                 "cwd" => "/tmp/project",
                 "updatedAt" => 1_700_000_000
               }
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_thread_start", steps)

    assert [_initialize, _initialized, _model_list, %{"id" => 3, "method" => "thread/start"}] =
             CodexGolden.writes(transcript)

    assert [%{"id" => 10, "result" => %{"sessionId" => "thread-abc"}}] =
             CodexGolden.messages(transcript)

    assert %{tag: :messages} = CodexGolden.last_result(transcript)
  end

  test "session_new_rejects_unknown_workspace" do
    steps =
      handshake_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/new",
             "id" => 11,
             "params" => %{"cwd" => "/srv/elsewhere", "mcpServers" => []}
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "session_new_rejects_unknown_workspace", steps)

    assert length(CodexGolden.writes(transcript)) == 3

    assert %{tag: :error, error: "Workspace path is not authorized"} =
             CodexGolden.last_result(transcript)
  end

  defp handshake_steps do
    [
      :post_connect,
      {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}},
      {:inbound, %{"id" => 2, "result" => %{"data" => catalog_models(), "nextCursor" => nil}}}
    ]
  end

  # Uses the Codex app-server v2 `model/list` shape, where each entry of
  # `supportedReasoningEfforts` is `%{"reasoningEffort" => ..., "description" => ...}`.
  # The older helper in test/ex_mcp/acp/adapters/codex_test.exs uses
  # `%{"value" => ..., "name" => ..., "description" => ...}` instead; the adapter
  # accepts both, so neither shape is wrong.
  defp catalog_models do
    [
      %{
        "id" => "codex-mini",
        "model" => "gpt-5-codex",
        "displayName" => "Codex Mini",
        "description" => "Fast coding model",
        "hidden" => false,
        "defaultReasoningEffort" => "medium",
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "medium", "description" => "Balanced"},
          %{"reasoningEffort" => "high", "description" => "Deep"}
        ]
      },
      %{
        "id" => "gpt-5",
        "model" => "gpt-5",
        "displayName" => "GPT-5",
        "description" => "General purpose model",
        "hidden" => false,
        "defaultReasoningEffort" => "high",
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "low", "description" => "Quick"},
          %{"reasoningEffort" => "high", "description" => "Deep"}
        ]
      }
    ]
  end
end
