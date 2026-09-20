defmodule ExMCP.ACP.Adapters.Pi.SlashCommandsGoldenTest do
  @moduledoc """
  Characterization gate for the Pi ACP adapter's slash commands (area P6 of
  `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Pi adapter restructuring" /
  "Characterization gate": slash-command expansion and available-command
  notifications).

  Each test drives `ExMCP.ACP.Adapters.Pi` through `ExMCP.Test.PiGolden` and
  compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/pi/slash_commands/`. The fixtures pin:

    * the built-in commands and their RPC requests plus result text:
      `/compact [instructions]`, `/autocompact on|off|<toggle>`, `/export`
      (output path derived from the sanitized session id, result text and
      `resource_link` chunk), `/session` (stats lines or JSON fallback),
      `/name` (usage, `set_session_name`, `session_info_update` title),
      `/steering`, `/follow-up`, `/model`, `/thinking` notices and
      `/changelog` (executable missing, changelog missing, changelog found
      next to the executable);
    * argument parsing (quotes, repeated whitespace, tabs) and the texts
      that are *not* slash commands (`/ x`, `/1abc`, a prompt with images);
    * file commands from the agent `prompts/` and project `.pi/prompts/`
      directories: `$1`/`$2`/`$@` substitution, missing arguments, nested
      directories and frontmatter in the advertised description, and an
      unknown `/command` being forwarded to Pi as prompt text;
    * `available_commands_update` composition after `session/new`, `load`
      and `resume`: Pi commands first (extension sources dropped, skill
      sources dropped when `enableSkillCommands` is false in the agent or
      project settings, `input` normalized to `{"hint": ...}`, missing
      descriptions defaulting to `(command)`, both `commands` and
      `data.commands` payload shapes), then file commands, then built-ins,
      de-duplicated by name.

  Slash-command control-group failures are characterized by the
  control_groups area, and queued slash commands by the prompt_flow area.

  Mutation check (2026-09-20): substituting `$@` with the first argument
  only in `SlashCommands.substitute_args/2` (`slash_commands.ex`) fails
  `file_commands_expand_positional_and_all_arguments`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `PI_GOLDEN=update mix test <this file>[:line]`; that run rewrites the
  fixture and fails on purpose, so review the diff and re-run without the
  variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.PiGolden
  alias ExMCP.Test.PiGolden.Flows

  @area "slash_commands"

  defp chunk_texts(transcript) do
    transcript
    |> PiGolden.messages()
    |> Enum.map(&get_in(&1, ["params", "update", "content", "text"]))
    |> Enum.reject(&is_nil/1)
  end

  defp prompt_writes(transcript),
    do: transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "prompt"))

  defp command_names(transcript) do
    transcript
    |> PiGolden.messages()
    |> Enum.map(&get_in(&1, ["params", "update", "availableCommands"]))
    |> Enum.reject(&is_nil/1)
    |> Enum.map(fn commands -> Enum.map(commands, & &1["name"]) end)
  end

  describe "built-in commands" do
    test "compact_with_and_without_instructions" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "/compact keep the failing tests"),
            {:respond, "compact", %{"summary" => "Kept tests", "tokensBefore" => 1200}},
            Flows.prompt(3, "/compact"),
            {:respond, "compact", %{"tokensBefore" => "n/a"}},
            Flows.prompt(4, "/compact   "),
            {:respond, "compact", "done"}
          ]

      transcript = PiGolden.assert_golden(@area, "compact_with_and_without_instructions", steps)

      assert [
               %{"type" => "compact", "customInstructions" => "keep the failing tests"},
               %{"type" => "compact"},
               %{"type" => "compact"}
             ] = transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "compact"))

      refute Map.has_key?(Enum.at(PiGolden.writes(transcript), 5), "customInstructions")

      assert [
               "Compaction completed.\nTokens before: 1200\nKept tests",
               "Compaction completed.",
               "Compaction completed."
             ] =
               chunk_texts(transcript)
    end

    test "autocompact_on_off_and_toggle_variants" do
      ons = ~w(on true enable enabled)
      offs = ~w(off false disable disabled)

      steps =
        Flows.open_session(1) ++
          Enum.flat_map(Enum.with_index(ons ++ offs, 2), fn {arg, acp_id} ->
            [Flows.prompt(acp_id, "/autocompact #{arg}"), {:respond, "set_auto_compaction", %{}}]
          end) ++
          [
            Flows.prompt(20, "/autocompact maybe"),
            {:respond, "get_state", %{"autoCompactionEnabled" => 1}},
            {:respond, "set_auto_compaction", %{}},
            Flows.prompt(21, "/autocompact"),
            {:respond, "get_state", %{}},
            {:respond, "set_auto_compaction", %{}}
          ]

      transcript = PiGolden.assert_golden(@area, "autocompact_on_off_and_toggle_variants", steps)

      assert [true, true, true, true, false, false, false, false, false, true] =
               transcript
               |> PiGolden.writes()
               |> Enum.filter(&(&1["type"] == "set_auto_compaction"))
               |> Enum.map(& &1["enabled"])

      assert ["Auto-compaction disabled.", "Auto-compaction enabled."] =
               transcript |> chunk_texts() |> Enum.take(-2)
    end

    test "export_derives_the_output_path_and_links_the_result" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "/export"),
            {:respond, "export_html",
             %{"path" => "<sandbox>/project/pi-session-pi-session.html"}},
            Flows.prompt(3, "/export", %{"sessionId" => "team/alpha beta"}),
            {:respond, "export_html", %{}},
            Flows.prompt(4, "/export", %{"sessionId" => nil}),
            {:respond, "export_html", %{"path" => ""}}
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "export_derives_the_output_path_and_links_the_result",
          steps
        )

      assert [
               %{"outputPath" => "<sandbox>/project/pi-session-pi-session.html"},
               %{"outputPath" => "<sandbox>/project/pi-session-team_alpha_beta.html"},
               %{"outputPath" => "<sandbox>/project/pi-session-pi-session.html"}
             ] = transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "export_html"))

      assert %{
               messages: [
                 %{
                   "params" => %{
                     "update" => %{
                       "content" => %{
                         "text" =>
                           "Session exported: file://<sandbox>/project/pi-session-pi-session.html"
                       }
                     }
                   }
                 },
                 %{
                   "params" => %{
                     "update" => %{
                       "content" => %{
                         "type" => "resource_link",
                         "name" => "pi-session-pi-session.html"
                       }
                     }
                   }
                 },
                 %{"id" => 2}
               ]
             } = Enum.at(transcript, 7).result

      assert ["Session export completed.", "Session export completed."] =
               transcript |> chunk_texts() |> Enum.take(-2)
    end

    test "session_stats_lines_and_json_fallback" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "/session"),
            {:respond, "get_session_stats",
             %{
               "sessionId" => "pi-session",
               "sessionFile" => "<sandbox>/sessions/pi-session.jsonl",
               "totalMessages" => 12,
               "cost" => 0.5
             }},
            Flows.prompt(3, "/session"),
            {:respond, "get_session_stats", %{"tokens" => 42}},
            Flows.prompt(4, "/session"),
            {:respond, "get_session_stats", "not a map"}
          ]

      transcript = PiGolden.assert_golden(@area, "session_stats_lines_and_json_fallback", steps)

      assert [
               "Session: pi-session\nSession file: <sandbox>/sessions/pi-session.jsonl\nMessages: 12\nCost: 0.5",
               "Session stats:\n{\n  \"tokens\": 42\n}",
               "Command completed."
             ] = chunk_texts(transcript)
    end

    test "name_usage_set_and_title_update" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "/name"),
            Flows.prompt(3, "/name My Project Session"),
            {:respond, "set_session_name", %{}},
            Flows.prompt(4, ~s(/name "quoted  name" 'and more')),
            {:respond, "set_session_name", %{"name" => "ignored"}}
          ]

      transcript = PiGolden.assert_golden(@area, "name_usage_set_and_title_update", steps)

      assert %{tag: :messages_and_reply, reply: %{"stopReason" => "end_turn"}} =
               Enum.at(transcript, 6).result

      assert [%{"name" => "My Project Session"}, %{"name" => "quoted  name and more"}] =
               transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "set_session_name"))

      assert %{
               messages: [
                 %{
                   "params" => %{
                     "update" => %{
                       "sessionUpdate" => "session_info_update",
                       "title" => "My Project Session",
                       "updatedAt" => "<now>"
                     }
                   }
                 },
                 %{
                   "params" => %{
                     "update" => %{
                       "content" => %{"text" => "Session name set: My Project Session"}
                     }
                   }
                 },
                 %{"id" => 3}
               ]
             } = Enum.at(transcript, 8).result
    end

    test "model_and_thinking_notices" do
      steps =
        Flows.open_session(1) ++
          [Flows.prompt(2, "/model gpt-5.1"), Flows.prompt(3, "/thinking high")]

      transcript = PiGolden.assert_golden(@area, "model_and_thinking_notices", steps)

      assert [
               %{tag: :messages_and_reply, reply: %{"stopReason" => "end_turn"}},
               %{tag: :messages_and_reply, reply: %{"stopReason" => "end_turn"}}
             ] = transcript |> Enum.take(-2) |> Enum.map(&Map.take(&1.result, [:tag, :reply]))

      assert [
               "Use the ACP model selector to change models.",
               "Use the ACP mode selector to change thinking level."
             ] =
               chunk_texts(transcript)
    end

    test "changelog_resolves_relative_to_the_pi_executable" do
      steps =
        [{:init, cli_path: "<sandbox>/bin/missing-pi"}] ++
          Flows.open_session(1) ++
          [
            Flows.prompt(2, "/changelog"),
            {:init, []}
          ] ++
          Flows.open_session(3) ++
          [
            Flows.prompt(4, "/changelog"),
            {:write_file, "<sandbox>/CHANGELOG.md", "# Changelog\n\n## 1.0.0\n- first\n"},
            Flows.prompt(5, "/changelog")
          ]

      transcript =
        PiGolden.assert_golden(@area, "changelog_resolves_relative_to_the_pi_executable", steps)

      assert [
               "Pi executable not found",
               "Pi changelog not found",
               "# Changelog\n\n## 1.0.0\n- first\n"
             ] =
               chunk_texts(transcript)
    end
  end

  describe "parsing" do
    test "argument_parsing_and_non_commands" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "  /compact   keep\t\"the  tests\" 'and docs'  "),
            {:respond, "compact", %{}},
            Flows.prompt(3, "/ not a command"),
            Flows.agent_settled(),
            Flows.prompt(4, "/1abc"),
            Flows.agent_settled(),
            Flows.prompt(5, "text /compact in the middle"),
            Flows.agent_settled(),
            Flows.prompt(6, [
              %{"type" => "text", "text" => "/compact"},
              %{"type" => "image", "mimeType" => "image/png", "data" => "abc"}
            ]),
            Flows.agent_settled(),
            Flows.prompt(7, "/compact\nsecond line"),
            {:respond, "compact", %{}}
          ]

      transcript = PiGolden.assert_golden(@area, "argument_parsing_and_non_commands", steps)

      assert [
               %{"customInstructions" => "keep the  tests and docs"},
               %{"customInstructions" => "second line"}
             ] =
               transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "compact"))

      assert ["/ not a command", "/1abc", "text /compact in the middle", "/compact"] =
               transcript |> prompt_writes() |> Enum.map(& &1["message"])
    end
  end

  describe "file commands" do
    test "file_commands_expand_positional_and_all_arguments" do
      steps =
        [
          {:write_file, "<sandbox>/project/.pi/prompts/review.md",
           "Review $1 against $2; all: $@"},
          {:write_file, "<sandbox>/agent/prompts/fix.md",
           "---\ndescription: Fix a bug\n---\nFix $1 now"}
        ] ++
          Flows.open_session(1) ++
          [
            Flows.prompt(2, "/review src/a.ex src/b.ex extra"),
            Flows.agent_settled(),
            Flows.prompt(3, "/review only"),
            Flows.agent_settled(),
            Flows.prompt(4, "/fix"),
            Flows.agent_settled(),
            Flows.prompt(5, "/unknown foo bar"),
            Flows.agent_settled(),
            Flows.prompt(6, "/unknown")
          ]

      transcript =
        PiGolden.assert_golden(@area, "file_commands_expand_positional_and_all_arguments", steps)

      assert [
               "Review src/a.ex against src/b.ex; all: src/a.ex src/b.ex extra",
               "Review only against ; all: only",
               "Fix  now",
               "/unknown foo bar",
               "/unknown"
             ] = transcript |> prompt_writes() |> Enum.map(& &1["message"])
    end

    test "file_commands_are_advertised_with_source_labels" do
      steps =
        [
          {:write_file, "<sandbox>/project/.pi/prompts/review.md",
           "Review the change carefully and thoroughly, checking every edge case you can find"},
          {:write_file, "<sandbox>/project/.pi/prompts/nested/deep/plan.md",
           "---\ndescription: Plan work\nextra: ignored\n---\n\nPlan $1"},
          {:write_file, "<sandbox>/agent/prompts/review.md", "User-level review"},
          {:write_file, "<sandbox>/agent/prompts/compact.md", "Shadows a builtin"},
          {:write_file, "<sandbox>/agent/prompts/empty.md", ""},
          {:write_file, "<sandbox>/agent/prompts/notes.txt", "not a command"}
        ] ++ Flows.open_session(1)

      transcript =
        PiGolden.assert_golden(@area, "file_commands_are_advertised_with_source_labels", steps)

      assert [
               [
                 "compact",
                 "empty",
                 "review",
                 "plan",
                 "autocompact",
                 "export",
                 "session",
                 "name",
                 "steering",
                 "follow-up",
                 "changelog"
               ]
             ] = command_names(transcript)

      [commands] =
        transcript
        |> PiGolden.messages()
        |> Enum.map(&get_in(&1, ["params", "update", "availableCommands"]))
        |> Enum.reject(&is_nil/1)

      assert %{"description" => "User-level review (user)"} =
               Enum.find(commands, &(&1["name"] == "review"))

      assert %{"description" => "Plan work (project:nested:deep)"} =
               Enum.find(commands, &(&1["name"] == "plan"))

      assert %{"description" => "Shadows a builtin (user)"} =
               Enum.find(commands, &(&1["name"] == "compact"))

      assert %{"description" => "(user) (user)"} = Enum.find(commands, &(&1["name"] == "empty"))
    end
  end

  describe "available commands" do
    test "pi_commands_merge_with_file_and_builtin_commands" do
      pi_commands = [
        %{"name" => "model", "description" => "Model picker", "source" => "extension"},
        %{
          "name" => "skill-x",
          "description" => "A skill",
          "source" => "skill",
          "input" => "topic"
        },
        %{
          "name" => "compact",
          "description" => "Pi's own compact",
          "input" => %{"hint" => "hint", "unsupported" => 1}
        },
        %{"name" => "nodesc", "input" => ["bad"]},
        %{"name" => "", "description" => "blank name"},
        %{"description" => "no name"},
        %{"name" => "dup", "description" => "first"},
        %{"name" => "dup", "description" => "second"},
        %{"name" => "review", "description" => "Pi review wins over the file command"}
      ]

      steps =
        [{:write_file, "<sandbox>/project/.pi/prompts/review.md", "Review $@"}] ++
          Flows.open_session(1, commands: pi_commands) ++
          [
            Flows.prompt(2, "/review this"),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(@area, "pi_commands_merge_with_file_and_builtin_commands", steps)

      assert [
               [
                 "skill-x",
                 "compact",
                 "nodesc",
                 "dup",
                 "review",
                 "autocompact",
                 "export",
                 "session",
                 "name",
                 "steering",
                 "follow-up",
                 "changelog"
               ]
             ] = command_names(transcript)

      [commands] =
        transcript
        |> PiGolden.messages()
        |> Enum.map(&get_in(&1, ["params", "update", "availableCommands"]))
        |> Enum.reject(&is_nil/1)

      assert %{"description" => "Pi's own compact", "input" => %{"hint" => "hint"}} =
               Enum.find(commands, &(&1["name"] == "compact"))

      assert %{"description" => "(command)"} =
               nodesc = Enum.find(commands, &(&1["name"] == "nodesc"))

      refute Map.has_key?(nodesc, "input")
      assert %{"description" => "first"} = Enum.find(commands, &(&1["name"] == "dup"))

      assert [%{"message" => "Review this"}] = prompt_writes(transcript)
    end

    test "skill_commands_follow_the_enable_skill_commands_setting" do
      pi_commands = [
        %{"name" => "skill-a", "description" => "skill", "source" => "skill"},
        %{"name" => "plain", "description" => "plain"}
      ]

      steps =
        [
          {:write_file, "<sandbox>/agent/settings.json",
           %{"quietStartup" => true, "enableSkillCommands" => false}}
        ] ++
          Flows.open_session(1, commands: pi_commands) ++
          [
            {:write_file, "<sandbox>/agent/settings.json",
             %{"quietStartup" => true, "skills" => %{"enableSkillCommands" => false}}}
          ] ++
          Flows.open_session(2, commands: pi_commands) ++
          [
            {:write_file, "<sandbox>/project/.pi/settings.json",
             %{"skills" => %{"enableSkillCommands" => true}}}
          ] ++
          Flows.open_session(3, commands: pi_commands) ++
          [
            {:write_file, "<sandbox>/agent/settings.json",
             %{"quietStartup" => true, "enableSkillCommands" => "false"}},
            {:write_file, "<sandbox>/project/.pi/settings.json", "{not json"}
          ] ++
          [
            Flows.session_new(4),
            {:respond, "new_session", %{}},
            {:respond, "get_state", Flows.state_data()},
            {:respond, "get_available_models", %{"models" => Flows.models()}},
            {:respond, "get_commands", %{"data" => %{"commands" => pi_commands}}}
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "skill_commands_follow_the_enable_skill_commands_setting",
          steps
        )

      assert [
               ["plain" | _],
               ["plain" | _],
               ["skill-a", "plain" | _],
               ["skill-a", "plain" | _]
             ] = command_names(transcript)
    end

    test "available_commands_are_sent_after_load_and_resume" do
      steps =
        [
          Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}]),
          {:write_file, "<sandbox>/project/.pi/prompts/local.md", "Local prompt"},
          Flows.session_load(1, "mapped"),
          {:respond, "switch_session", %{}},
          {:respond, "get_messages", %{"messages" => []}},
          {:respond, "get_state", Flows.state_data(%{"sessionId" => "mapped"})},
          {:respond, "get_available_models", %{"models" => Flows.models()}},
          {:respond, "get_commands",
           %{"commands" => [%{"name" => "pi-cmd", "description" => "from pi"}]}},
          Flows.session_resume(2, "mapped"),
          {:respond, "switch_session", %{}},
          {:respond, "get_state", Flows.state_data(%{"sessionId" => "mapped"})},
          {:respond, "get_available_models", %{"models" => Flows.models()}},
          {:respond, "get_commands", %{}}
        ]

      transcript =
        PiGolden.assert_golden(@area, "available_commands_are_sent_after_load_and_resume", steps)

      assert [["pi-cmd", "local" | _], ["local" | _]] = command_names(transcript)
    end
  end

  describe "routing edge cases" do
    test "command_names_are_case_sensitive_and_unknown_names_are_forwarded" do
      steps =
        [{:write_file, "<sandbox>/project/.pi/prompts/Review.md", "Capitalized $@"}] ++
          Flows.open_session(1) ++
          [
            Flows.prompt(2, "/COMPACT now"),
            Flows.agent_settled(),
            Flows.prompt(3, "/Review it"),
            Flows.agent_settled(),
            Flows.prompt(4, "/review it"),
            Flows.agent_settled(),
            Flows.prompt(5, "/name:sub value"),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "command_names_are_case_sensitive_and_unknown_names_are_forwarded",
          steps
        )

      assert ["/COMPACT now", "Capitalized it", "/review it", "/name:sub value"] =
               transcript |> prompt_writes() |> Enum.map(& &1["message"])
    end

    test "slash_notices_before_a_session_use_the_request_or_default_session" do
      steps = [
        Flows.prompt(1, "/model", %{"sessionId" => "requested"}),
        Flows.prompt(2, "/thinking", %{"sessionId" => nil}),
        Flows.prompt(3, "/name", %{"sessionId" => nil}),
        Flows.prompt(4, "/export", %{"sessionId" => nil})
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "slash_notices_before_a_session_use_the_request_or_default_session",
          steps
        )

      assert ["requested", "default", "default"] =
               transcript |> PiGolden.messages() |> Enum.map(& &1["params"]["sessionId"])

      assert %{
               writes: [
                 %{
                   "type" => "export_html",
                   "outputPath" => "<sandbox>/project/pi-session-default.html"
                 }
               ]
             } =
               PiGolden.last_result(transcript)
    end
  end
end
