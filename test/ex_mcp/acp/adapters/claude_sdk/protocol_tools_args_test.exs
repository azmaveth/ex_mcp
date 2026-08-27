defmodule ExMCP.ACP.Adapters.ClaudeSDK.ProtocolToolsArgsTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.ClaudeSDK.Protocol

  defp args(opts), do: elem(Protocol.command(Keyword.merge([cli_path: "claude"], opts)), 1)

  defp flag_value(args, flag) do
    case Enum.find_index(args, &(&1 == flag)) do
      nil -> :absent
      i -> Enum.at(args, i + 1)
    end
  end

  test "tools: [] disables every built-in tool (--tools \"\")" do
    assert flag_value(args(tools: []), "--tools") == ""
  end

  test "tools: [names] restricts the built-in set" do
    assert flag_value(args(tools: ["Read", "Grep"]), "--tools") == "Read,Grep"
  end

  test "empty allowed/disallowed lists are omitted (no opinion)" do
    a = args(allowed_tools: [], disallowed_tools: [])
    assert flag_value(a, "--allowedTools") == :absent
    assert flag_value(a, "--disallowedTools") == :absent
  end

  test "allowed and disallowed lists are passed as CSV" do
    a = args(allowed_tools: ["Read"], disallowed_tools: ["Bash", "Edit"])
    assert flag_value(a, "--allowedTools") == "Read"
    assert flag_value(a, "--disallowedTools") == "Bash,Edit"
  end
end
