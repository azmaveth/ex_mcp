defmodule ExMCP.ACP.Adapters.Codex.SlashCommandsTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex.SlashCommands

  test "parses supported slash commands" do
    assert SlashCommands.parse([%{"type" => "text", "text" => "/compact"}]) ==
             {:ok, {:compact, ""}}

    assert SlashCommands.parse([%{"type" => "text", "text" => "/review src"}]) ==
             {:ok, {:review, "src"}}

    assert SlashCommands.parse([%{"type" => "text", "text" => "  /review-commit HEAD"}]) ==
             {:ok, {:"review-commit", "HEAD"}}
  end

  test "ignores unsupported input" do
    assert SlashCommands.parse([%{"type" => "text", "text" => "not a command"}]) == :error
    assert SlashCommands.parse([%{"type" => "image", "data" => "..."}]) == :error
    assert SlashCommands.parse([]) == :error
  end

  test "builds init input items" do
    assert [%{"type" => "text", "text" => text}] = SlashCommands.init_input_items()
    assert text =~ "AGENTS.md"
    assert text =~ "Repository Guidelines"
  end
end
