defmodule ExMCP.ACP.Adapters.Codex.ConfigTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex.Config

  test "normalizes current mode ids" do
    assert Config.normalize_mode_id(nil) == "agent"
    assert Config.normalize_mode_id("read-only") == "read-only"
    assert Config.normalize_mode_id("agent") == "agent"
    assert Config.normalize_mode_id("agent-full-access") == "agent-full-access"
  end

  test "validates requested modes" do
    assert Config.normalize_requested_mode("agent-full-access") == {:ok, "agent-full-access"}
    assert {:error, reason} = Config.normalize_requested_mode("unknown")
    assert reason =~ "Unsupported Codex mode"
    assert {:error, legacy_reason} = Config.normalize_requested_mode("full-auto")
    assert legacy_reason =~ "Unsupported Codex mode"
  end

  test "lists modes with 1.7.0 names and _meta.kind" do
    assert Config.modes() == [
             %{
               "id" => "read-only",
               "name" => "Ask for approval",
               "description" => "Always ask to edit external files and use the internet",
               "_meta" => %{"kind" => "standard"}
             },
             %{
               "id" => "agent",
               "name" => "Approve for me",
               "description" => "Only ask for actions detected as potentially unsafe",
               "_meta" => %{"kind" => "auto_review"}
             },
             %{
               "id" => "agent-full-access",
               "name" => "Full access",
               "description" =>
                 "Unrestricted access to the internet and any file on your computer",
               "_meta" => %{"kind" => "full_access"}
             }
           ]
  end

  test "merges mode wire params" do
    assert Config.merge_mode_wire_params(%{"model" => "gpt-5"}, "read-only") == %{
             "model" => "gpt-5",
             "sandbox" => "workspace-write",
             "approvalPolicy" => "on-request"
           }

    assert Config.merge_mode_wire_params(%{}, "unknown") == %{}
  end

  test "turn/start mode params include approvalsReviewer and remapped sandbox" do
    assert Config.merge_turn_mode_wire_params(%{"threadId" => "t1"}, "read-only") == %{
             "threadId" => "t1",
             "sandboxPolicy" => %{
               "type" => "workspaceWrite",
               "writableRoots" => [],
               "networkAccess" => false,
               "excludeTmpdirEnvVar" => false,
               "excludeSlashTmp" => false
             },
             "approvalPolicy" => "on-request",
             "approvalsReviewer" => "user"
           }

    assert Config.merge_turn_mode_wire_params(%{}, "agent")["approvalsReviewer"] == "auto_review"

    assert Config.merge_turn_mode_wire_params(%{}, "agent-full-access") == %{
             "sandboxPolicy" => %{"type" => "dangerFullAccess"},
             "approvalPolicy" => "never",
             "approvalsReviewer" => "user"
           }
  end

  test "maps active permission profiles back to ACP mode ids" do
    assert Config.mode_id_from_result(%{"activePermissionProfile" => %{"id" => ":workspace"}}) ==
             "agent"

    assert Config.mode_id_from_result(%{
             "settings" => %{"activePermissionProfile" => %{"id" => ":danger-no-sandbox"}}
           }) == "agent-full-access"

    assert Config.mode_id_from_result(%{
             "sandboxPolicy" => %{"type" => "workspaceWrite"},
             "approvalsReviewer" => "user"
           }) == "read-only"

    assert Config.mode_id_from_result(%{
             "sandboxPolicy" => %{"type" => "workspaceWrite"},
             "approvalsReviewer" => "auto_review"
           }) == "agent"

    assert Config.mode_id_from_result(%{}) == nil
  end
end
