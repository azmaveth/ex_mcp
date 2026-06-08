defmodule ExMCP.ACP.Adapters.Codex.ConfigTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex.Config

  test "normalizes current mode ids" do
    assert Config.normalize_mode_id(nil) == "auto"
    assert Config.normalize_mode_id("read-only") == "read-only"
    assert Config.normalize_mode_id("auto") == "auto"
    assert Config.normalize_mode_id("full-access") == "full-access"
  end

  test "validates requested modes" do
    assert Config.normalize_requested_mode("full-access") == {:ok, "full-access"}
    assert {:error, reason} = Config.normalize_requested_mode("unknown")
    assert reason =~ "Unsupported Codex mode"
    assert {:error, legacy_reason} = Config.normalize_requested_mode("full-auto")
    assert legacy_reason =~ "Unsupported Codex mode"
  end

  test "merges mode wire params" do
    assert Config.merge_mode_wire_params(%{"model" => "gpt-5"}, "read-only") == %{
             "model" => "gpt-5",
             "permissions" => ":read-only",
             "approvalPolicy" => "on-request"
           }

    assert Config.merge_mode_wire_params(%{}, "unknown") == %{}
  end

  test "maps active permission profiles back to ACP mode ids" do
    assert Config.mode_id_from_result(%{"activePermissionProfile" => %{"id" => ":workspace"}}) ==
             "auto"

    assert Config.mode_id_from_result(%{
             "settings" => %{"activePermissionProfile" => %{"id" => ":danger-no-sandbox"}}
           }) == "full-access"

    assert Config.mode_id_from_result(%{}) == nil
  end
end
