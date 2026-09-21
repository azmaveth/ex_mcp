defmodule ExMCP.ACP.Adapters.Pi.SessionSafetyGoldenTest do
  @moduledoc """
  Characterization gate for the Pi ACP adapter's session-map and backing
  JSONL safety rules (area P7 of `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Pi
  adapter restructuring" / "Characterization gate").

  Each test drives `ExMCP.ACP.Adapters.Pi` through `ExMCP.Test.PiGolden` and
  compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/pi/session_safety/`, reading the session map and
  session files back through `{:read_file, path}` steps. The fixtures pin:

    * the session map entry (`sessionId`, `cwd`, `sessionFile`,
      `updatedAt`) written when `session/new`, `load` and `resume`
      complete, keyed by the requested id, accumulating across sessions,
      and never written without a session file;
    * session file resolution for `load` / `resume`: the map entry wins
      over a JSONL scan, an entry without a string `sessionFile` falls back
      to the scan, and a map with the wrong version or invalid JSON is
      treated as empty and rewritten with only the new entry;
    * `session/delete` removing the map entry while leaving the backing
      file unless `delete_session_files: true`, and even then deleting only
      files inside the session directory: files elsewhere, `..` traversal
      out of it, and sibling directories sharing its name prefix are
      refused; without a `sessionId` the active session's own file is the
      candidate;
    * `session/delete` for the active session dropping its pending prompt
      silently, and for another session leaving the active prompt running;
    * the auth-failure cleanup after an empty model catalog honoring the
      same file rules;
    * `session_dir` falling back to the agent settings' `sessionDir` for
      listing while deletion still measures against `<agent_dir>/sessions`;
    * `list_sessions/2` reading titles (`session_info` name, else the first
      user message truncated to 80 characters), `updatedAt` (last message
      timestamp, then any timestamp, then the file mtime), the `cwd`
      filter defaulting to the last session's cwd, cursor parsing, nested
      directories, and malformed or non-session JSONL being skipped, never
      exposing `sessionFile`.

  Mutation check (2026-09-20): dropping the trailing `/` from
  `root_prefix` in `safe_delete_session_file/2` (so a sibling
  `sessions-other/` directory matches) fails
  `delete_refuses_files_outside_the_session_directory`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `PI_GOLDEN=update mix test <this file>[:line]`; that run rewrites the
  fixture and fails on purpose, so review the diff and re-run without the
  variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.PiGolden
  alias ExMCP.Test.PiGolden.Flows

  @area "session_safety"
  @map "<sandbox>/session-map.json"

  defp delete(acp_id, session_id) do
    params = if session_id, do: %{"sessionId" => session_id}, else: %{}
    {:outbound, %{"method" => "session/delete", "id" => acp_id, "params" => params}}
  end

  defp load_steps(acp_id, session_id, file, opts \\ []) do
    [
      Flows.session_resume(acp_id, session_id),
      {:respond, "switch_session", %{}},
      {:respond, "get_state",
       Flows.state_data(
         Map.merge(
           %{"sessionId" => session_id, "sessionFile" => file},
           Keyword.get(opts, :state, %{})
         )
       )},
      {:respond, "get_available_models",
       %{"models" => Keyword.get(opts, :models, Flows.models())}},
      {:respond, "get_commands", %{"commands" => []}}
    ]
  end

  defp file_results(transcript) do
    transcript
    |> Enum.filter(&(&1.step.kind == :read_file))
    |> Enum.map(&{&1.step.path, &1.result})
  end

  describe "session map" do
    test "session_new_upserts_a_map_entry_keyed_by_session_id" do
      steps =
        [{:read_file, @map}] ++
          Flows.open_session(1) ++
          [{:read_file, @map}] ++
          Flows.open_session(2,
            state: %{
              "sessionId" => "second",
              "sessionFile" => "<sandbox>/sessions/second.jsonl",
              "cwd" => "<sandbox>/other"
            }
          ) ++
          [
            {:read_file, @map},
            Flows.session_new(3),
            {:respond, "new_session", %{}},
            {:respond, "get_state",
             Flows.state_data(%{"sessionId" => "third", "sessionFile" => nil})},
            {:respond, "get_available_models", %{"models" => Flows.models()}},
            {:respond, "get_commands", %{"commands" => []}},
            {:read_file, @map}
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "session_new_upserts_a_map_entry_keyed_by_session_id",
          steps
        )

      assert [
               {_, %{exists: false}},
               {_, %{content: %{"sessions" => %{"pi-session" => %{"updatedAt" => "<now>"}}}}},
               {_,
                %{
                  content: %{
                    "sessions" => %{"pi-session" => _, "second" => %{"cwd" => "<sandbox>/other"}}
                  }
                }},
               {_, %{content: %{"sessions" => sessions}}}
             ] = file_results(transcript)

      assert Map.keys(sessions) == ["pi-session", "second"]
    end

    test "map_entry_wins_over_a_jsonl_scan" do
      steps =
        [
          Flows.session_jsonl("shared", path: "<sandbox>/sessions/scanned.jsonl"),
          Flows.session_map([{"shared", "<sandbox>/project", "<sandbox>/sessions/mapped.jsonl"}]),
          Flows.session_resume(1, "shared"),
          {:write_file, @map,
           %{
             "version" => 1,
             "sessions" => %{"shared" => %{"sessionId" => "shared", "sessionFile" => 7}}
           }},
          Flows.session_resume(2, "shared"),
          {:write_file, @map, %{"version" => 1, "sessions" => %{}}},
          Flows.session_resume(3, "shared")
        ]

      transcript = PiGolden.assert_golden(@area, "map_entry_wins_over_a_jsonl_scan", steps)

      assert [
               "<sandbox>/sessions/mapped.jsonl",
               "<sandbox>/sessions/scanned.jsonl",
               "<sandbox>/sessions/scanned.jsonl"
             ] =
               transcript
               |> PiGolden.writes()
               |> Enum.filter(&(&1["type"] == "switch_session"))
               |> Enum.map(& &1["sessionPath"])
    end

    test "unreadable_or_mismatched_map_is_treated_as_empty_and_rewritten" do
      steps =
        [
          {:write_file, @map,
           %{
             "version" => 2,
             "sessions" => %{
               "old" => %{
                 "sessionId" => "old",
                 "cwd" => "<sandbox>/project",
                 "sessionFile" => "<sandbox>/sessions/old.jsonl"
               }
             }
           }},
          Flows.session_resume(1, "old")
        ] ++
          Flows.open_session(2) ++
          [
            {:read_file, @map},
            {:write_file, @map, "{not json"},
            Flows.session_resume(3, "pi-session"),
            {:write_file, @map, %{"version" => 1, "sessions" => "not-a-map"}},
            Flows.session_resume(4, "pi-session")
          ] ++
          Flows.open_session(5) ++
          [{:read_file, @map}]

      transcript =
        PiGolden.assert_golden(
          @area,
          "unreadable_or_mismatched_map_is_treated_as_empty_and_rewritten",
          steps
        )

      assert [
               %{error: "Unknown sessionId: old"},
               %{error: "Unknown sessionId: pi-session"},
               %{error: "Unknown sessionId: pi-session"}
             ] =
               transcript |> Enum.map(& &1.result) |> Enum.filter(&Map.has_key?(&1, :error))

      assert [
               {_, %{content: %{"version" => 1, "sessions" => %{"pi-session" => _}}}},
               {_, %{content: %{"version" => 1, "sessions" => sessions}}}
             ] =
               file_results(transcript)

      assert Map.keys(sessions) == ["pi-session"]
    end

    test "load_and_resume_refresh_the_map_entry" do
      steps =
        [
          Flows.session_jsonl("scan-1", cwd: "<sandbox>/project"),
          {:read_file, @map}
        ] ++
          load_steps(1, "scan-1", "<sandbox>/sessions/scan-1.jsonl",
            state: %{"cwd" => "<sandbox>/moved"}
          ) ++
          [{:read_file, @map}]

      transcript = PiGolden.assert_golden(@area, "load_and_resume_refresh_the_map_entry", steps)

      assert [
               {_, %{exists: false}},
               {_,
                %{
                  content: %{
                    "sessions" => %{
                      "scan-1" => %{
                        "cwd" => "<sandbox>/moved",
                        "sessionFile" => "<sandbox>/sessions/scan-1.jsonl"
                      }
                    }
                  }
                }}
             ] = file_results(transcript)
    end
  end

  describe "session/delete" do
    test "delete_keeps_the_backing_file_by_default" do
      steps =
        [
          Flows.session_jsonl("mapped"),
          Flows.session_map([{"mapped", "<sandbox>/project", Flows.session_file("mapped")}])
        ] ++
          [
            delete(1, "mapped"),
            {:read_file, Flows.session_file("mapped")},
            {:read_file, @map},
            delete(2, "mapped"),
            {:read_file, @map}
          ]

      transcript =
        PiGolden.assert_golden(@area, "delete_keeps_the_backing_file_by_default", steps)

      assert [
               {_, %{exists: true}},
               {_, %{content: %{"sessions" => %{}}}},
               {_, %{content: %{"sessions" => %{}}}}
             ] =
               file_results(transcript)
    end

    test "delete_removes_files_inside_the_session_directory_when_enabled" do
      steps =
        [
          {:init, delete_session_files: true},
          Flows.session_jsonl("inside"),
          Flows.session_jsonl("nested", path: "<sandbox>/sessions/2026/nested.jsonl"),
          Flows.session_map([
            {"inside", "<sandbox>/project", Flows.session_file("inside")},
            {"nested", "<sandbox>/project", "<sandbox>/sessions/2026/nested.jsonl"},
            {"gone", "<sandbox>/project", "<sandbox>/sessions/gone.jsonl"}
          ]),
          delete(1, "inside"),
          delete(2, "nested"),
          delete(3, "gone"),
          {:read_file, Flows.session_file("inside")},
          {:read_file, "<sandbox>/sessions/2026/nested.jsonl"},
          {:read_file, @map}
        ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "delete_removes_files_inside_the_session_directory_when_enabled",
          steps
        )

      assert [{_, %{exists: false}}, {_, %{exists: false}}, {_, %{content: %{"sessions" => %{}}}}] =
               file_results(transcript)
    end

    test "delete_refuses_files_outside_the_session_directory" do
      steps = [
        {:init, delete_session_files: true},
        {:write_file, "<sandbox>/project/elsewhere.jsonl", "keep\n"},
        {:write_file, "<sandbox>/project/escape.jsonl", "keep\n"},
        {:write_file, "<sandbox>/sessions-other/sibling.jsonl", "keep\n"},
        {:write_file, "<sandbox>/sessions/../project/traversal.jsonl", "keep\n"},
        Flows.session_map([
          {"elsewhere", "<sandbox>/project", "<sandbox>/project/elsewhere.jsonl"},
          {"escape", "<sandbox>/project", "<sandbox>/sessions/../project/escape.jsonl"},
          {"sibling", "<sandbox>/project", "<sandbox>/sessions-other/sibling.jsonl"},
          {"traversal", "<sandbox>/project", "<sandbox>/sessions/../project/traversal.jsonl"},
          {"relative", "<sandbox>/project", "sessions/relative.jsonl"}
        ]),
        delete(1, "elsewhere"),
        delete(2, "escape"),
        delete(3, "sibling"),
        delete(4, "traversal"),
        delete(5, "relative"),
        {:read_file, "<sandbox>/project/elsewhere.jsonl"},
        {:read_file, "<sandbox>/project/escape.jsonl"},
        {:read_file, "<sandbox>/sessions-other/sibling.jsonl"},
        {:read_file, "<sandbox>/project/traversal.jsonl"},
        {:read_file, @map}
      ]

      transcript =
        PiGolden.assert_golden(@area, "delete_refuses_files_outside_the_session_directory", steps)

      assert [
               {_, %{exists: true}},
               {_, %{exists: true}},
               {_, %{exists: true}},
               {_, %{exists: true}},
               {_, %{content: %{"sessions" => %{}}}}
             ] = file_results(transcript)
    end

    test "delete_without_session_id_targets_the_active_session_file" do
      steps =
        [{:init, delete_session_files: true}, Flows.session_jsonl("pi-session")] ++
          Flows.open_session(1) ++
          [
            Flows.prompt(2, "working"),
            delete(3, nil),
            {:read_file, Flows.session_file("pi-session")},
            {:read_file, @map},
            {:note, "The dropped prompt is never answered"},
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "delete_without_session_id_targets_the_active_session_file",
          steps
        )

      assert [{_, %{exists: false}}, {_, %{content: %{"sessions" => %{"pi-session" => _}}}}] =
               file_results(transcript)

      assert %{tag: :skip} = PiGolden.last_result(transcript)
    end

    test "delete_of_another_session_keeps_the_active_prompt_running" do
      steps =
        [Flows.session_map([{"other", "<sandbox>/project", "<sandbox>/sessions/other.jsonl"}])] ++
          Flows.open_session(1) ++
          [
            Flows.prompt(2, "working"),
            delete(3, "other"),
            Flows.text_delta("still "),
            Flows.agent_settled(),
            Flows.prompt(4, "again"),
            delete(5, "pi-session"),
            Flows.agent_settled(),
            {:read_file, @map}
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "delete_of_another_session_keeps_the_active_prompt_running",
          steps
        )

      assert [%{"id" => 1}, %{"id" => 2, "result" => %{"stopReason" => "end_turn"}}] =
               transcript |> PiGolden.messages() |> Enum.filter(&Map.has_key?(&1, "id"))

      assert [{_, %{content: %{"sessions" => %{}}}}] = file_results(transcript)
    end
  end

  describe "auth failure cleanup" do
    test "empty_catalog_cleanup_honors_the_file_rules" do
      steps =
        [
          {:init, delete_session_files: true},
          Flows.session_jsonl("inside"),
          {:write_file, "<sandbox>/project/outside.jsonl", "keep\n"}
        ] ++
          load_steps(1, "inside", Flows.session_file("inside"), models: []) ++
          [
            {:read_file, Flows.session_file("inside")},
            Flows.session_new(2),
            {:respond, "new_session", %{}},
            {:respond, "get_state",
             Flows.state_data(%{"sessionFile" => "<sandbox>/project/outside.jsonl"})},
            {:respond, "get_available_models", %{"models" => []}},
            {:respond, "get_commands", %{"commands" => []}},
            {:read_file, "<sandbox>/project/outside.jsonl"},
            {:init, delete_session_files: false},
            Flows.session_jsonl("kept")
          ] ++
          load_steps(3, "kept", Flows.session_file("kept"), models: []) ++
          [{:read_file, Flows.session_file("kept")}, {:read_file, @map}]

      transcript =
        PiGolden.assert_golden(@area, "empty_catalog_cleanup_honors_the_file_rules", steps)

      assert [
               {_, %{exists: false}},
               {_, %{exists: true}},
               {_, %{exists: true}},
               {_, %{exists: false}}
             ] =
               file_results(transcript)
    end
  end

  describe "session directory resolution" do
    test "agent_settings_session_dir_is_used_for_listing_but_not_deletion" do
      steps = [
        {:init, session_dir: nil, delete_session_files: true},
        {:write_file, "<sandbox>/agent/settings.json",
         %{"quietStartup" => true, "sessionDir" => "alt-sessions"}},
        Flows.session_jsonl("alt", path: "<sandbox>/agent/alt-sessions/alt.jsonl"),
        Flows.session_jsonl("default-dir", path: "<sandbox>/agent/sessions/default-dir.jsonl"),
        {:list_sessions, %{"cwd" => "<sandbox>/project"}},
        Flows.session_resume(1, "alt"),
        {:respond, "switch_session", %{}},
        {:respond, "get_state",
         Flows.state_data(%{
           "sessionId" => "alt",
           "sessionFile" => "<sandbox>/agent/alt-sessions/alt.jsonl"
         })},
        {:respond, "get_available_models", %{"models" => Flows.models()}},
        {:respond, "get_commands", %{"commands" => []}},
        delete(2, "alt"),
        {:read_file, "<sandbox>/agent/alt-sessions/alt.jsonl"},
        {:write_file, "<sandbox>/agent/settings.json",
         %{"quietStartup" => true, "sessionDir" => "<sandbox>/abs-sessions"}},
        Flows.session_jsonl("abs", path: "<sandbox>/abs-sessions/abs.jsonl"),
        {:list_sessions, %{"cwd" => "<sandbox>/project"}},
        {:write_file, "<sandbox>/agent/settings.json", %{"quietStartup" => true}},
        {:list_sessions, %{"cwd" => "<sandbox>/project"}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "agent_settings_session_dir_is_used_for_listing_but_not_deletion",
          steps
        )

      assert [["alt"], ["abs"], ["default-dir"]] =
               transcript
               |> Enum.filter(&(&1.step.kind == :list_sessions))
               |> Enum.map(fn entry ->
                 Enum.map(entry.result.reply["sessions"], & &1["sessionId"])
               end)

      assert [{_, %{exists: true}}] = file_results(transcript)
    end
  end

  describe "list_sessions" do
    test "list_sessions_reads_titles_and_timestamps_without_exposing_files" do
      long = String.duplicate("word ", 30)

      steps = [
        Flows.session_jsonl("named", name: "Project session", first_prompt: "Hello"),
        Flows.session_jsonl("untitled", first_prompt: long),
        Flows.session_jsonl("blocks",
          first_prompt: nil,
          extra: [
            %{
              "type" => "message",
              "timestamp" => "2026-02-01T00:00:00Z",
              "message" => %{
                "role" => "user",
                "content" => [%{"type" => "image"}, %{"type" => "text", "text" => "From blocks"}]
              }
            }
          ]
        ),
        Flows.session_jsonl("blank-name", name: "   ", first_prompt: "Fallback title"),
        Flows.session_jsonl("no-messages",
          first_prompt: nil,
          extra: [
            %{
              "type" => "session_info",
              "name" => "Renamed later",
              "timestamp" => "2026-03-01T00:00:00Z"
            }
          ]
        ),
        Flows.session_jsonl("no-timestamps",
          first_prompt: nil,
          path: "<sandbox>/sessions/no-timestamps.jsonl",
          extra: []
        ),
        {:write_file, "<sandbox>/sessions/no-timestamps.jsonl",
         [%{"type" => "session", "id" => "no-timestamps", "cwd" => "<sandbox>/project"}]},
        Flows.session_jsonl("nested",
          path: "<sandbox>/sessions/2026/01/nested.jsonl",
          extra: [
            %{
              "type" => "message",
              "timestamp" => "2026-04-01T00:00:00Z",
              "message" => %{"role" => "assistant", "content" => "later"}
            }
          ]
        ),
        {:write_file, "<sandbox>/sessions/broken.jsonl", "{not json\n"},
        {:write_file, "<sandbox>/sessions/no-cwd.jsonl",
         [%{"type" => "session", "id" => "no-cwd"}]},
        {:write_file, "<sandbox>/sessions/not-session.jsonl",
         [%{"type" => "message", "id" => "x", "cwd" => "<sandbox>/project"}]},
        {:write_file, "<sandbox>/sessions/empty.jsonl", ""},
        {:write_file, "<sandbox>/sessions/notes.txt", "ignored"},
        {:list_sessions, %{"cwd" => "<sandbox>/project"}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "list_sessions_reads_titles_and_timestamps_without_exposing_files",
          steps
        )

      %{reply: %{"sessions" => sessions}} = PiGolden.last_result(transcript)

      assert [
               "no-timestamps",
               "nested",
               "no-messages",
               "blocks",
               "blank-name",
               "named",
               "untitled"
             ] =
               Enum.map(sessions, & &1["sessionId"])

      assert Enum.all?(sessions, &(not Map.has_key?(&1, "sessionFile")))

      assert %{"title" => "Project session", "name" => "Project session"} =
               Enum.find(sessions, &(&1["sessionId"] == "named"))

      assert %{"title" => nil, "name" => "no-timestamps", "updatedAt" => "<now>"} =
               Enum.find(sessions, &(&1["sessionId"] == "no-timestamps"))

      assert %{"title" => "Fallback title"} =
               Enum.find(sessions, &(&1["sessionId"] == "blank-name"))

      assert %{"title" => "From blocks"} = Enum.find(sessions, &(&1["sessionId"] == "blocks"))

      assert %{"updatedAt" => "2026-03-01T00:00:00Z", "title" => "Renamed later"} =
               Enum.find(sessions, &(&1["sessionId"] == "no-messages"))

      assert 80 == String.length(Enum.find(sessions, &(&1["sessionId"] == "untitled"))["title"])
    end

    test "list_sessions_filters_by_cwd_and_pages_with_cursors" do
      steps =
        [
          Flows.session_jsonl("here-1",
            cwd: "<sandbox>/project",
            extra: [
              %{
                "type" => "message",
                "timestamp" => "2026-01-03T00:00:00Z",
                "message" => %{"role" => "user", "content" => "c"}
              }
            ]
          ),
          Flows.session_jsonl("here-2",
            cwd: "<sandbox>/project",
            extra: [
              %{
                "type" => "message",
                "timestamp" => "2026-01-02T00:00:00Z",
                "message" => %{"role" => "user", "content" => "b"}
              }
            ]
          ),
          Flows.session_jsonl("there", cwd: "<sandbox>/elsewhere"),
          {:note, "Before any session the init cwd is the default filter"},
          {:list_sessions, %{}},
          {:list_sessions, %{"cwd" => "<sandbox>/elsewhere"}},
          {:list_sessions, %{"cwd" => "<sandbox>/nowhere"}},
          {:list_sessions, %{"cursor" => "1"}},
          {:list_sessions, %{"cursor" => "abc"}},
          {:list_sessions, %{"cursor" => "-1"}},
          {:list_sessions, %{"cursor" => 5}},
          {:list_sessions, %{"cursor" => "99"}}
        ] ++
          Flows.open_session(1, state: %{"cwd" => "<sandbox>/elsewhere"}) ++
          [
            {:note, "After a session the last session cwd is the default filter"},
            {:list_sessions, %{}}
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "list_sessions_filters_by_cwd_and_pages_with_cursors",
          steps
        )

      assert [
               ["here-1", "here-2"],
               ["there"],
               [],
               ["here-2"],
               ["here-1", "here-2"],
               ["here-1", "here-2"],
               ["here-1", "here-2"],
               [],
               ["there"]
             ] =
               transcript
               |> Enum.filter(&(&1.step.kind == :list_sessions))
               |> Enum.map(fn entry ->
                 Enum.map(entry.result.reply["sessions"], & &1["sessionId"])
               end)

      assert Enum.all?(
               Enum.filter(transcript, &(&1.step.kind == :list_sessions)),
               &(not Map.has_key?(&1.result.reply, "nextCursor"))
             )
    end

    test "list_sessions_scans_files_and_ignores_map_only_entries" do
      steps = [
        Flows.session_map([
          {"map-only", "<sandbox>/project", "<sandbox>/sessions/map-only.jsonl"},
          {"renamed-in-map", "<sandbox>/project", Flows.session_file("on-disk")}
        ]),
        Flows.session_jsonl("on-disk"),
        {:list_sessions, %{"cwd" => "<sandbox>/project"}}
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "list_sessions_scans_files_and_ignores_map_only_entries",
          steps
        )

      assert %{reply: %{"sessions" => [%{"sessionId" => "on-disk", "name" => "First prompt"}]}} =
               PiGolden.last_result(transcript)
    end
  end

  describe "session/close" do
    test "close_leaves_the_map_and_backing_file_untouched" do
      steps =
        [{:init, delete_session_files: true}, Flows.session_jsonl("pi-session")] ++
          Flows.open_session(1) ++
          [
            {:outbound,
             %{"method" => "session/close", "id" => 2, "params" => %{"sessionId" => "pi-session"}}},
            {:outbound, %{"method" => "session/close", "id" => 3, "params" => %{}}},
            :shutdown,
            {:read_file, Flows.session_file("pi-session")},
            {:read_file, @map}
          ]

      transcript =
        PiGolden.assert_golden(@area, "close_leaves_the_map_and_backing_file_untouched", steps)

      assert [{_, %{exists: true}}, {_, %{content: %{"sessions" => %{"pi-session" => _}}}}] =
               file_results(transcript)
    end
  end
end
