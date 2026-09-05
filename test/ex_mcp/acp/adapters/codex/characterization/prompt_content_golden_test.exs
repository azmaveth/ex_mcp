defmodule ExMCP.ACP.Adapters.Codex.PromptContentGoldenTest do
  @moduledoc """
  Characterization gate for the Codex ACP adapter's prompt-content wire
  behavior (area A2 `prompt_content`; see `docs/POST_1_0_MAINTENANCE_PLAN.md`,
  "Codex adapter restructuring" / "Characterization gate").

  Each test drives `ExMCP.ACP.Adapters.Codex` through `ExMCP.Test.CodexGolden`
  and compares the recorded transcript (app-server writes, ACP messages,
  replies, and errors, with generated ids normalized) against a committed
  fixture under `test/fixtures/acp/codex/prompt_content/`. The scenarios pin
  how `session/prompt` becomes `turn/start`: every content block shape the
  adapter converts (text, images, embedded resources, resource links, mixed
  and empty prompts), the slash commands and their native requests or
  synthesized replies, the mode / reasoning-effort / service-tier / cwd /
  additional-directory wire parameters and how `session/set_mode`,
  `session/set_model` and `session/set_config_option` change them, the error
  replies for missing, unknown, and closed sessions, prompting while a turn is
  active (and what a new prompt resets: accumulated text, per-item streamed
  deltas, usage, rate limits, and the prompt-activity mark that gates the
  -32029 rate_limit_exhausted failure of an output-less turn), which prompt a
  late `turn/start` reply re-pins, and the ACP reply when the app-server
  answers `turn/start` with an error (code and message relayed, `data`
  dropped, missing code -> -1).

  To regenerate a fixture after an intentional behavior change, run the test
  with `CODEX_GOLDEN=update`:

      CODEX_GOLDEN=update mix test test/ex_mcp/acp/adapters/codex/characterization/prompt_content_golden_test.exs

  That run rewrites the fixtures and fails on purpose, so review the diff and
  re-run without the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.CodexGolden

  @area "prompt_content"
  @session_id "thread-abc"
  @cwd "/tmp/project"
  @init_opts [workspace_roots: ["/tmp"], cwd: @cwd]

  # -- content block shapes ---------------------------------------------------

  test "text_block" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("Summarize the repository layout.")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "text_block", steps)

    assert %{
             "threadId" => @session_id,
             "summary" => "auto",
             "model" => "gpt-5",
             "effort" => "medium",
             "cwd" => @cwd,
             "approvalPolicy" => "on-request",
             "approvalsReviewer" => "auto_review",
             "sandboxPolicy" => %{"type" => "workspaceWrite", "writableRoots" => []},
             "input" => [
               %{
                 "type" => "text",
                 "text" => "Summarize the repository layout.",
                 "text_elements" => []
               }
             ]
           } = last_turn_start(transcript)

    assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
  end

  test "image_by_url" do
    steps =
      session_steps() ++
        [
          prompt(20, [
            text("What is in this picture?"),
            image(%{"uri" => "https://example.com/a.png"})
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "image_by_url", steps)

    assert [_text, %{"type" => "image", "url" => "https://example.com/a.png"}] =
             last_turn_start(transcript)["input"]
  end

  test "image_data_with_mime_type" do
    steps =
      session_steps() ++
        [prompt(20, [image(%{"data" => "aGVsbG8=", "mimeType" => "image/jpeg"})])]

    transcript = CodexGolden.assert_golden(@area, "image_data_with_mime_type", steps)

    assert [%{"type" => "image", "url" => "data:image/jpeg;base64,aGVsbG8="}] =
             last_turn_start(transcript)["input"]
  end

  test "image_data_without_mime_type" do
    steps = session_steps() ++ [prompt(20, [image(%{"data" => "aGVsbG8="})])]

    transcript = CodexGolden.assert_golden(@area, "image_data_without_mime_type", steps)

    assert [%{"type" => "image", "url" => "data:image/png;base64,aGVsbG8="}] =
             last_turn_start(transcript)["input"]
  end

  test "image_without_uri_or_data" do
    steps =
      session_steps() ++
        [
          {:note, "An image block with neither uri nor data is forwarded with an empty url"},
          prompt(20, [image(%{"mimeType" => "image/png"})])
        ]

    transcript = CodexGolden.assert_golden(@area, "image_without_uri_or_data", steps)
    assert [%{"type" => "image", "url" => ""}] = last_turn_start(transcript)["input"]
  end

  test "image_empty_uri_falls_back_to_data" do
    steps =
      session_steps() ++
        [
          {:note,
           "An empty-string uri is not used as the url: with data the data URL is sent, " <>
             "without data the url is empty"},
          prompt(20, [
            image(%{"uri" => "", "data" => "aGVsbG8=", "mimeType" => "image/jpeg"}),
            image(%{"uri" => ""})
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "image_empty_uri_falls_back_to_data", steps)

    assert [
             %{"type" => "image", "url" => "data:image/jpeg;base64,aGVsbG8="},
             %{"type" => "image", "url" => ""}
           ] = last_turn_start(transcript)["input"]
  end

  test "image_uri_and_data_prefers_uri" do
    steps =
      session_steps() ++
        [
          {:note,
           "An image block carrying both a uri and inline data (the ACP shape for an attached " <>
             "local file) is sent by uri; the data and mimeType are ignored"},
          prompt(20, [
            image(%{
              "uri" => "file:///tmp/project/shot.png",
              "data" => "aGVsbG8=",
              "mimeType" => "image/png"
            })
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "image_uri_and_data_prefers_uri", steps)

    assert [%{"type" => "image", "url" => "file:///tmp/project/shot.png"}] =
             last_turn_start(transcript)["input"]
  end

  test "embedded_resource_text" do
    steps =
      session_steps() ++
        [
          prompt(20, [
            text("Use this context:"),
            resource(%{"uri" => "file:///tmp/project/NOTES.md", "text" => "line one\nline two"})
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "embedded_resource_text", steps)

    assert [_text, %{"type" => "text", "text" => context}] = last_turn_start(transcript)["input"]

    assert context ==
             "[@NOTES.md](file:///tmp/project/NOTES.md)\n" <>
               "<context ref=\"file:///tmp/project/NOTES.md\">\nline one\nline two\n</context>"
  end

  test "embedded_resource_blob_image" do
    steps =
      session_steps() ++
        [
          prompt(20, [
            resource(%{
              "uri" => "file:///tmp/project/diagram.png",
              "mimeType" => "image/png",
              "blob" => "iVBORw0KGgo="
            })
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "embedded_resource_blob_image", steps)

    assert [%{"type" => "image", "url" => "data:image/png;base64,iVBORw0KGgo="}] =
             last_turn_start(transcript)["input"]
  end

  test "embedded_resource_blob_non_image" do
    steps =
      session_steps() ++
        [
          prompt(20, [
            resource(%{
              "uri" => "file:///tmp/project/spec.pdf",
              "mimeType" => "application/pdf",
              "blob" => "JVBERi0xLjQ="
            })
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "embedded_resource_blob_non_image", steps)

    assert [%{"type" => "text", "text" => context}] = last_turn_start(transcript)["input"]

    assert context ==
             "[@spec.pdf](file:///tmp/project/spec.pdf)\n" <>
               ~s(<context ref="file:///tmp/project/spec.pdf" mimeType="application/pdf" encoding="base64">\n) <>
               "JVBERi0xLjQ=\n</context>"
  end

  test "embedded_resource_blob_nil_mime_type" do
    steps =
      session_steps() ++
        [
          {:note,
           "A blob resource whose mimeType key is present but null defaults to octet-stream"},
          prompt(20, [
            resource(%{
              "uri" => "file:///tmp/project/data.bin",
              "mimeType" => nil,
              "blob" => "AAEC"
            })
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "embedded_resource_blob_nil_mime_type", steps)

    assert [%{"type" => "text", "text" => context}] = last_turn_start(transcript)["input"]
    assert context =~ ~s(mimeType="application/octet-stream" encoding="base64")
  end

  test "embedded_resource_blob_missing_mime_type_dropped" do
    steps =
      session_steps() ++
        [
          {:note,
           "A blob resource without any mimeType key matches no conversion clause and is dropped, " <>
             "leaving the empty-text fallback"},
          prompt(20, [resource(%{"uri" => "file:///tmp/project/data.bin", "blob" => "AAEC"})])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "embedded_resource_blob_missing_mime_type_dropped", steps)

    assert [%{"type" => "text", "text" => "", "text_elements" => []}] =
             last_turn_start(transcript)["input"]
  end

  test "embedded_resource_text_and_blob_prefers_text" do
    steps =
      session_steps() ++
        [
          {:note,
           "A resource carrying both text and an image blob is rendered as the text <context> " <>
             "form; the blob and mimeType are ignored"},
          prompt(20, [
            resource(%{
              "uri" => "file:///tmp/project/diagram.png",
              "mimeType" => "image/png",
              "blob" => "iVBORw0KGgo=",
              "text" => "alt: architecture diagram"
            })
          ])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "embedded_resource_text_and_blob_prefers_text", steps)

    assert [%{"type" => "text", "text" => context}] = last_turn_start(transcript)["input"]

    assert context ==
             "[@diagram.png](file:///tmp/project/diagram.png)\n" <>
               "<context ref=\"file:///tmp/project/diagram.png\">\nalt: architecture diagram\n</context>"
  end

  test "embedded_resource_text_without_uri_dropped" do
    steps =
      session_steps() ++
        [
          {:note,
           "A text resource without a uri key matches no conversion clause and is dropped, " <>
             "leaving the empty-text fallback (like the blob-without-mimeType case)"},
          prompt(20, [resource(%{"text" => "orphan text"})])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "embedded_resource_text_without_uri_dropped", steps)

    assert [%{"type" => "text", "text" => "", "text_elements" => []}] =
             last_turn_start(transcript)["input"]
  end

  test "resource_link_with_name" do
    steps =
      session_steps() ++
        [
          prompt(20, [
            text("Review "),
            resource_link(%{"name" => "lib.ex", "uri" => "file:///tmp/project/lib/lib.ex"})
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "resource_link_with_name", steps)

    assert [_text, %{"type" => "text", "text" => "[@lib.ex](file:///tmp/project/lib/lib.ex)"}] =
             last_turn_start(transcript)["input"]
  end

  test "resource_link_title_ignored_name_wins" do
    steps =
      session_steps() ++
        [
          {:note,
           "A resource_link carrying the optional title/description/mimeType fields " <>
             "(Zed-style mention) is rendered by name; title is ignored"},
          prompt(20, [
            resource_link(%{
              "name" => "lib.ex",
              "title" => "Library",
              "description" => "Main library module",
              "mimeType" => "text/x-elixir",
              "uri" => "file:///tmp/project/lib.ex"
            })
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "resource_link_title_ignored_name_wins", steps)

    assert [%{"type" => "text", "text" => "[@lib.ex](file:///tmp/project/lib.ex)"}] =
             last_turn_start(transcript)["input"]
  end

  test "resource_link_without_name_file_uri" do
    steps =
      session_steps() ++
        [prompt(20, [resource_link(%{"uri" => "file:///tmp/project/lib/nested/mod.ex"})])]

    transcript = CodexGolden.assert_golden(@area, "resource_link_without_name_file_uri", steps)

    assert [%{"type" => "text", "text" => "[@mod.ex](file:///tmp/project/lib/nested/mod.ex)"}] =
             last_turn_start(transcript)["input"]
  end

  test "resource_link_without_name_http_uri" do
    steps =
      session_steps() ++
        [prompt(20, [resource_link(%{"uri" => "https://example.com/docs/guide"})])]

    transcript = CodexGolden.assert_golden(@area, "resource_link_without_name_http_uri", steps)

    assert [%{"type" => "text", "text" => "https://example.com/docs/guide"}] =
             last_turn_start(transcript)["input"]
  end

  test "resource_link_without_uri" do
    steps =
      session_steps() ++
        [
          {:note,
           "A named resource_link with no uri renders a link with an empty target; " <>
             "an unnamed one becomes an empty text item"},
          prompt(20, [resource_link(%{"name" => "orphan"}), resource_link(%{})])
        ]

    transcript = CodexGolden.assert_golden(@area, "resource_link_without_uri", steps)

    assert [%{"type" => "text", "text" => "[@orphan]()"}, %{"type" => "text", "text" => ""}] =
             last_turn_start(transcript)["input"]
  end

  test "resource_link_empty_name_falls_back_to_uri" do
    steps =
      session_steps() ++
        [
          {:note,
           "An empty-string name is treated like a missing one: file URIs render the basename, " <>
             "other URIs are forwarded raw"},
          prompt(20, [
            resource_link(%{"name" => "", "uri" => "file:///tmp/project/lib/mod.ex"}),
            resource_link(%{"name" => "", "uri" => "https://example.com/docs/guide"})
          ])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "resource_link_empty_name_falls_back_to_uri", steps)

    assert [
             %{"type" => "text", "text" => "[@mod.ex](file:///tmp/project/lib/mod.ex)"},
             %{"type" => "text", "text" => "https://example.com/docs/guide"}
           ] = last_turn_start(transcript)["input"]
  end

  test "text_block_nil_text_is_empty_item" do
    steps =
      session_steps() ++
        [
          {:note,
           "A text block whose text is null is skipped by slash parsing (which needs a string) " <>
             "and becomes an empty text item that keeps its position"},
          prompt(20, [%{"type" => "text", "text" => nil}, text("hello")])
        ]

    transcript = CodexGolden.assert_golden(@area, "text_block_nil_text_is_empty_item", steps)

    assert [
             %{"type" => "text", "text" => "", "text_elements" => []},
             %{"type" => "text", "text" => "hello"}
           ] = last_turn_start(transcript)["input"]
  end

  test "mixed_blocks_preserve_order" do
    steps =
      session_steps() ++
        [
          {:note,
           "Unsupported block types (audio) are dropped; every other item keeps its position"},
          prompt(20, [
            text("Compare "),
            resource_link(%{"name" => "a.ex", "uri" => "file:///tmp/project/a.ex"}),
            text(" with "),
            resource(%{"uri" => "file:///tmp/project/b.ex", "text" => "defmodule B do\nend"}),
            %{"type" => "audio", "data" => "AAAA", "mimeType" => "audio/wav"},
            image(%{"uri" => "https://example.com/shot.png"}),
            text(" and explain.")
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "mixed_blocks_preserve_order", steps)

    assert [
             %{"type" => "text", "text" => "Compare "},
             %{"type" => "text", "text" => "[@a.ex](file:///tmp/project/a.ex)"},
             %{"type" => "text", "text" => " with "},
             %{"type" => "text", "text" => "[@b.ex](file:///tmp/project/b.ex)\n<context" <> _},
             %{"type" => "image", "url" => "https://example.com/shot.png"},
             %{"type" => "text", "text" => " and explain."}
           ] = last_turn_start(transcript)["input"]
  end

  test "empty_prompt_list" do
    steps = session_steps() ++ [prompt(20, [])]
    transcript = CodexGolden.assert_golden(@area, "empty_prompt_list", steps)

    assert [%{"type" => "text", "text" => "", "text_elements" => []}] =
             last_turn_start(transcript)["input"]
  end

  test "unsupported_blocks_only" do
    steps =
      session_steps() ++
        [prompt(20, [%{"type" => "audio", "data" => "AAAA", "mimeType" => "audio/wav"}])]

    transcript = CodexGolden.assert_golden(@area, "unsupported_blocks_only", steps)

    assert [%{"type" => "text", "text" => "", "text_elements" => []}] =
             last_turn_start(transcript)["input"]
  end

  test "prompt_missing" do
    steps =
      session_steps() ++
        [
          {:note,
           "A session/prompt without a prompt key sends one empty text item without text_elements"},
          {:outbound,
           %{"method" => "session/prompt", "id" => 20, "params" => %{"sessionId" => @session_id}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "prompt_missing", steps)
    assert [%{"type" => "text", "text" => ""} = item] = last_turn_start(transcript)["input"]
    refute Map.has_key?(item, "text_elements")
  end

  test "prompt_as_string" do
    steps = session_steps() ++ [prompt(20, "plain string prompt")]
    transcript = CodexGolden.assert_golden(@area, "prompt_as_string", steps)

    assert [%{"type" => "text", "text" => "plain string prompt", "text_elements" => []}] =
             last_turn_start(transcript)["input"]
  end

  # -- slash commands ---------------------------------------------------------

  test "slash_compact_ok_reply_skipped" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("/compact")]),
          {:inbound, %{"id" => 4, "result" => %{}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_compact_ok_reply_skipped", steps)

    assert [
             %{
               "id" => 4,
               "method" => "thread/compact/start",
               "params" => %{"threadId" => @session_id}
             }
           ] =
             non_handshake_writes(transcript)

    assert %{tag: :skip, skipped: true} = CodexGolden.last_result(transcript)
  end

  test "slash_init_uses_agents_md_prompt" do
    steps =
      session_steps() ++
        [prompt(20, [text("/init")], %{"summary" => "concise", "model" => "gpt-5-codex"})]

    transcript = CodexGolden.assert_golden(@area, "slash_init_uses_agents_md_prompt", steps)

    assert %{
             "summary" => "concise",
             "model" => "gpt-5-codex",
             "input" => [%{"text" => init_text}]
           } =
             last_turn_start(transcript)

    assert init_text =~ "Generate a file named AGENTS.md"
    assert init_text =~ "Repository Guidelines"
  end

  test "slash_review_uncommitted_changes" do
    steps = session_steps() ++ [prompt(20, [text("/review")])]
    transcript = CodexGolden.assert_golden(@area, "slash_review_uncommitted_changes", steps)

    assert [
             %{
               "method" => "review/start",
               "params" => %{
                 "threadId" => @session_id,
                 "delivery" => "inline",
                 "target" => %{"type" => "uncommittedChanges"}
               }
             }
           ] = non_handshake_writes(transcript)
  end

  test "slash_review_custom_instructions" do
    steps = session_steps() ++ [prompt(20, [text("/review focus on error handling in lib/")])]
    transcript = CodexGolden.assert_golden(@area, "slash_review_custom_instructions", steps)

    assert [%{"params" => %{"target" => target}}] = non_handshake_writes(transcript)
    assert target == %{"type" => "custom", "instructions" => "focus on error handling in lib/"}
  end

  test "slash_review_custom_instructions_trimmed" do
    steps =
      session_steps() ++
        [
          {:note, "Trailing whitespace after the argument is trimmed from the instructions"},
          prompt(20, [text("/review fix it  ")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "slash_review_custom_instructions_trimmed", steps)

    assert [%{"method" => "review/start", "params" => %{"target" => target}}] =
             non_handshake_writes(transcript)

    assert target == %{"type" => "custom", "instructions" => "fix it"}
  end

  test "slash_review_multiple_spaces_before_argument" do
    steps =
      session_steps() ++
        [
          {:note,
           "Extra spaces between the command and its argument do not reach the " <>
             "instructions (see slash_review_newline_after_command for a non-space separator)"},
          prompt(20, [text("/review   fix")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "slash_review_multiple_spaces_before_argument", steps)

    assert [%{"method" => "review/start", "params" => %{"target" => target}}] =
             non_handshake_writes(transcript)

    assert target == %{"type" => "custom", "instructions" => "fix"}
  end

  test "slash_review_newline_after_command" do
    steps =
      session_steps() ++
        [
          {:note,
           "A newline (any whitespace, not only a space) separates the command name from its " <>
             "argument, so /review followed by a newline still starts a custom review"},
          prompt(20, [text("/review\nfocus on error handling")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_review_newline_after_command", steps)

    assert [%{"method" => "review/start", "params" => %{"target" => target}}] =
             non_handshake_writes(transcript)

    assert target == %{"type" => "custom", "instructions" => "focus on error handling"}
  end

  test "slash_review_multiline_instructions" do
    steps =
      session_steps() ++
        [
          {:note, "The argument may span lines; the inner newline is kept in the instructions"},
          prompt(20, [text("/review focus on\nerror handling")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_review_multiline_instructions", steps)

    assert [%{"method" => "review/start", "params" => %{"target" => target}}] =
             non_handshake_writes(transcript)

    assert target == %{"type" => "custom", "instructions" => "focus on\nerror handling"}
  end

  test "slash_review_branch" do
    steps = session_steps() ++ [prompt(20, [text("/review-branch main")])]
    transcript = CodexGolden.assert_golden(@area, "slash_review_branch", steps)

    assert [%{"method" => "review/start", "params" => %{"target" => target}}] =
             non_handshake_writes(transcript)

    assert target == %{"type" => "baseBranch", "branch" => "main"}
  end

  test "slash_review_branch_argument_trimmed" do
    steps =
      session_steps() ++
        [
          {:note, "Trailing spaces and a trailing newline after the branch name are trimmed"},
          prompt(20, [text("/review-branch main  \n")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_review_branch_argument_trimmed", steps)

    assert [%{"method" => "review/start", "params" => %{"target" => target}}] =
             non_handshake_writes(transcript)

    assert target == %{"type" => "baseBranch", "branch" => "main"}
  end

  test "slash_review_branch_without_branch" do
    steps =
      session_steps() ++
        [
          {:note,
           "Without a branch the command is forwarded as a plain turn whose text keeps a trailing " <>
             "space; the caller's summary/model/cwd params are ignored (session cwd is used)"},
          prompt(20, [text("/review-branch")], %{
            "summary" => "concise",
            "model" => "o3",
            "cwd" => "/tmp/project/sub"
          })
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_review_branch_without_branch", steps)

    assert %{
             "summary" => "auto",
             "model" => "gpt-5",
             "cwd" => @cwd,
             "input" => [%{"text" => "/review-branch "}]
           } = last_turn_start(transcript)
  end

  test "slash_review_commit" do
    steps = session_steps() ++ [prompt(20, [text("/review-commit abc123")])]
    transcript = CodexGolden.assert_golden(@area, "slash_review_commit", steps)

    assert [%{"method" => "review/start", "params" => %{"target" => target}}] =
             non_handshake_writes(transcript)

    assert target == %{"type" => "commit", "sha" => "abc123", "title" => nil}
  end

  test "slash_review_commit_argument_trimmed" do
    steps =
      session_steps() ++
        [
          {:note, "A trailing space after the sha is trimmed"},
          prompt(20, [text("/review-commit abc123 ")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_review_commit_argument_trimmed", steps)

    assert [%{"method" => "review/start", "params" => %{"target" => target}}] =
             non_handshake_writes(transcript)

    assert target == %{"type" => "commit", "sha" => "abc123", "title" => nil}
  end

  test "slash_review_commit_without_sha" do
    steps =
      session_steps() ++
        [
          {:note,
           "Without a sha the command is forwarded as a plain turn whose text keeps a trailing " <>
             "space; like /review-branch, the caller's summary/model/cwd params are ignored " <>
             "(session cwd is used)"},
          prompt(20, [text("/review-commit")], %{
            "summary" => "concise",
            "model" => "o3",
            "cwd" => "/tmp/project/sub"
          })
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_review_commit_without_sha", steps)

    assert [
             %{
               "method" => "turn/start",
               "params" => %{
                 "summary" => "auto",
                 "model" => "gpt-5",
                 "cwd" => @cwd,
                 "input" => [%{"text" => "/review-commit "}]
               }
             }
           ] = non_handshake_writes(transcript)
  end

  test "slash_review_start_error_replies_error" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("/review")]),
          {:inbound,
           %{"id" => 4, "error" => %{"code" => -32_000, "message" => "review already running"}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_review_start_error_replies_error", steps)

    assert %{
             tag: :messages,
             messages: [
               %{
                 "id" => 20,
                 "error" => %{"code" => -32_000, "message" => "review already running"}
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "slash_logout" do
    steps = session_steps() ++ [prompt(20, [text("/logout")])]
    transcript = CodexGolden.assert_golden(@area, "slash_logout", steps)

    assert %{
             tag: :reply_and_write,
             reply: %{
               "stopReason" => "refusal",
               "_meta" => %{"ex_mcp" => %{"authRequired" => true}}
             },
             writes: [%{"id" => 4, "method" => "account/logout", "params" => %{}}]
           } = CodexGolden.last_result(transcript)
  end

  test "slash_status_without_usage" do
    steps = session_steps() ++ [prompt(20, [text("/status")])]
    transcript = CodexGolden.assert_golden(@area, "slash_status_without_usage", steps)

    assert %{
             tag: :messages_and_reply,
             reply: %{
               "stopReason" => "end_turn",
               "_meta" => %{"ex_mcp" => %{"command" => "status"}}
             },
             messages: [
               %{
                 "params" => %{
                   "update" => %{
                     "sessionUpdate" => "agent_message_chunk",
                     "content" => %{"text" => status}
                   }
                 }
               }
             ]
           } = CodexGolden.last_result(transcript)

    assert status =~ "**Model:** gpt-5/high"
    assert status =~ "**Sandbox:** workspace-write"
    assert status =~ "**Token usage:** data not available yet"
    refute Map.has_key?(CodexGolden.last_result(transcript).reply, "usage")
  end

  test "slash_status_with_usage" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("Hello")]),
          {:inbound,
           %{
             "method" => "turn/started",
             "params" => %{"threadId" => @session_id, "turn" => %{"id" => "turn-1"}}
           }},
          {:inbound,
           %{
             "method" => "thread/tokenUsage/updated",
             "params" => %{
               "threadId" => @session_id,
               "tokenUsage" => %{
                 "last" => %{"inputTokens" => 4, "outputTokens" => 1},
                 "modelContextWindow" => 100,
                 "total" => %{"inputTokens" => 10, "outputTokens" => 5, "cachedInputTokens" => 2}
               }
             }
           }},
          {:note,
           "/status issued while the turn is still running reports the usage accumulated so far"},
          prompt(21, [text("/status")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_status_with_usage", steps)

    assert %{
             reply: %{
               "usage" => %{"inputTokens" => 10, "outputTokens" => 5, "cachedInputTokens" => 2}
             },
             messages: [%{"params" => %{"update" => %{"content" => %{"text" => status}}}}]
           } = CodexGolden.last_result(transcript)

    assert status =~ "**Token usage:** 15 total (10 input + 2 cached input, 5 output)"
  end

  test "slash_status_read_only_mode" do
    steps =
      session_steps(new_params: %{"modeId" => "read-only"}) ++
        [
          prompt(20, [text("/status")]),
          {:note,
           "/status renders the read-only profile, but the next turn/start still sends a " <>
             "workspaceWrite sandbox with approvalsReviewer user"},
          prompt(21, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_status_read_only_mode", steps)

    assert status_text(transcript, 20) =~
             "**Approval:** on-request  \n**Sandbox:** read-only  \n**Session:** `thread-abc`"

    assert %{"approvalsReviewer" => "user", "sandboxPolicy" => %{"type" => "workspaceWrite"}} =
             last_turn_start(transcript)
  end

  test "slash_status_agent_full_access_mode" do
    steps =
      session_steps(new_params: %{"modeId" => "agent-full-access"}) ++
        [prompt(20, [text("/status")]), prompt(21, [text("hi")])]

    transcript = CodexGolden.assert_golden(@area, "slash_status_agent_full_access_mode", steps)

    assert status_text(transcript, 20) =~
             "**Approval:** never  \n**Sandbox:** danger-full-access  \n"

    assert %{"approvalPolicy" => "never", "sandboxPolicy" => %{"type" => "dangerFullAccess"}} =
             last_turn_start(transcript)
  end

  test "slash_status_after_set_mode_read_only" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/set_mode",
             "id" => 11,
             "params" => %{"sessionId" => @session_id, "modeId" => "read-only"}
           }},
          prompt(20, [text("/status")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_status_after_set_mode_read_only", steps)
    assert status_text(transcript, 20) =~ "**Sandbox:** read-only"
  end

  test "slash_status_model_without_catalog" do
    steps =
      session_steps(catalog: []) ++
        [
          {:note,
           "With an empty model/list catalog the status line shows the raw wire model " <>
             "without an effort suffix"},
          prompt(20, [text("/status")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_status_model_without_catalog", steps)
    assert status_text(transcript, 20) =~ "**Model:** gpt-5  \n"
  end

  test "slash_status_model_default_without_any_model" do
    steps =
      session_steps(thread_result: %{"model" => nil}) ++
        [
          {:note,
           "No init :model and a thread/start result without a model: /status reports " <>
             "\"default\" and turn/start omits the model key"},
          prompt(20, [text("/status")]),
          prompt(21, [text("hi")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "slash_status_model_default_without_any_model", steps)

    assert status_text(transcript, 20) =~ "**Model:** default  \n"
    refute Map.has_key?(last_turn_start(transcript), "model")
  end

  test "slash_status_without_any_cwd" do
    steps =
      session_steps(
        init_opts: [workspace_roots: ["/tmp"]],
        thread_result: %{"thread" => %{"id" => @session_id, "updatedAt" => 1_700_000_000}}
      ) ++
        [
          {:note,
           "No init :cwd (deliberately omitted here) and a thread/start result without " <>
             "thread.cwd: the session/new cwd only shapes thread/start, so /status renders an " <>
             "empty Directory line and turn/start omits the cwd key"},
          prompt(20, [text("/status")]),
          prompt(21, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_status_without_any_cwd", steps)

    assert %{"cwd" => @cwd} = thread_start(transcript)
    assert status_text(transcript, 20) =~ "**Model:** gpt-5/high  \n**Directory:**   \n"
    refute Map.has_key?(last_turn_start(transcript), "cwd")
  end

  test "status_after_new_prompt_resets_usage" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          {:inbound,
           %{
             "method" => "turn/started",
             "params" => %{"threadId" => @session_id, "turn" => %{"id" => "turn-1"}}
           }},
          token_usage_update(),
          {:note,
           "A new prompt clears the usage accumulated by the previous turn before it completes"},
          prompt(21, [text("second")]),
          prompt(22, [text("/status")]),
          {:inbound, %{"id" => 5, "result" => %{"turn" => %{"id" => "turn-2"}}}},
          {:inbound,
           %{
             "method" => "turn/completed",
             "params" => %{
               "threadId" => @session_id,
               "turn" => %{"id" => "turn-2", "status" => "completed"}
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "status_after_new_prompt_resets_usage", steps)

    assert %{reply: reply} = acp_result(transcript, 22)
    assert status_text(transcript, 22) =~ "**Token usage:** data not available yet"
    refute Map.has_key?(reply, "usage")

    assert %{messages: [_update, %{"id" => 21, "result" => result}]} =
             CodexGolden.last_result(transcript)

    refute Map.has_key?(result, "usage")
  end

  test "status_after_compact_resets_usage" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          token_usage_update(),
          {:note, "A slash command that starts a native request also clears the accumulators"},
          prompt(21, [text("/compact")]),
          prompt(22, [text("/status")])
        ]

    transcript = CodexGolden.assert_golden(@area, "status_after_compact_resets_usage", steps)

    assert [%{"method" => "turn/start"}, %{"method" => "thread/compact/start"}] =
             non_handshake_writes(transcript)

    assert status_text(transcript, 22) =~ "**Token usage:** data not available yet"
    refute Map.has_key?(acp_result(transcript, 22).reply, "usage")
  end

  test "status_after_logout_resets_usage" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          token_usage_update(),
          {:note,
           "/logout clears the accumulators too: the following /status no longer reports " <>
             "the usage of the still-running turn"},
          prompt(21, [text("/logout")]),
          prompt(22, [text("/status")])
        ]

    transcript = CodexGolden.assert_golden(@area, "status_after_logout_resets_usage", steps)

    assert [%{"method" => "turn/start"}, %{"id" => 5, "method" => "account/logout"}] =
             non_handshake_writes(transcript)

    assert status_text(transcript, 22) =~ "**Token usage:** data not available yet"
    refute Map.has_key?(acp_result(transcript, 22).reply, "usage")
  end

  test "slash_unknown_command" do
    steps = session_steps() ++ [prompt(20, [text("/deploy prod")])]
    transcript = CodexGolden.assert_golden(@area, "slash_unknown_command", steps)

    assert %{
             tag: :messages_and_reply,
             reply: %{
               "stopReason" => "end_turn",
               "_meta" => %{"ex_mcp" => %{"command" => "unknown"}}
             },
             messages: [%{"params" => %{"update" => %{"content" => %{"text" => message}}}}]
           } = CodexGolden.last_result(transcript)

    assert message =~ ~s(Unknown command "/deploy".)
    assert message =~ "- /logout"
    assert non_handshake_writes(transcript) == []
  end

  test "slash_command_lookup_is_case_sensitive" do
    steps =
      session_steps() ++
        [
          {:note,
           "Command names are matched case-sensitively, so /Compact is an unknown command"},
          prompt(20, [text("/Compact")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_command_lookup_is_case_sensitive", steps)

    assert %{
             reply: %{"_meta" => %{"ex_mcp" => %{"command" => "unknown"}}},
             messages: [%{"params" => %{"update" => %{"content" => %{"text" => message}}}}]
           } = CodexGolden.last_result(transcript)

    assert message =~ ~s(Unknown command "/Compact".)
    assert non_handshake_writes(transcript) == []
  end

  test "slash_bare_slash_is_plain_prompt" do
    steps =
      session_steps() ++
        [
          {:note, "A lone slash has no command name and is forwarded as ordinary prompt text"},
          prompt(20, [text("/")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_bare_slash_is_plain_prompt", steps)

    assert [%{"method" => "turn/start", "params" => %{"input" => [%{"text" => "/"}]}}] =
             non_handshake_writes(transcript)
  end

  test "slash_leading_digit_is_plain_prompt" do
    steps =
      session_steps() ++
        [
          {:note, "A command name must start with a letter, so /1abc is ordinary prompt text"},
          prompt(20, [text("/1abc")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_leading_digit_is_plain_prompt", steps)

    assert [%{"method" => "turn/start", "params" => %{"input" => [%{"text" => "/1abc"}]}}] =
             non_handshake_writes(transcript)
  end

  test "slash_command_not_first_block_is_plain_prompt" do
    steps =
      session_steps() ++
        [
          {:note,
           "Slash detection only inspects the first block, so a leading image hides the command"},
          prompt(20, [image(%{"uri" => "https://example.com/a.png"}), text("/compact")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "slash_command_not_first_block_is_plain_prompt", steps)

    assert [
             %{
               "method" => "turn/start",
               "params" => %{"input" => [%{"type" => "image"}, %{"text" => "/compact"}]}
             }
           ] =
             non_handshake_writes(transcript)
  end

  test "slash_command_ignores_trailing_blocks" do
    steps =
      session_steps() ++
        [
          {:note,
           "Only the first block decides: a leading /review still starts an uncommitted-changes " <>
             "review and the image and text blocks after it are dropped"},
          prompt(20, [
            text("/review"),
            image(%{"uri" => "https://example.com/a.png"}),
            text("and look at this")
          ])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_command_ignores_trailing_blocks", steps)

    assert [
             %{
               "method" => "review/start",
               "params" => %{"target" => %{"type" => "uncommittedChanges"}}
             }
           ] = non_handshake_writes(transcript)
  end

  test "slash_compact_and_init_ignore_argument" do
    steps =
      session_steps() ++
        [
          {:note, "/compact and /init take no argument; any trailing text is discarded"},
          prompt(20, [text("/compact extra words")]),
          prompt(21, [text("/init extra words")])
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_compact_and_init_ignore_argument", steps)

    assert [
             %{"method" => "thread/compact/start", "params" => %{"threadId" => @session_id}},
             %{"method" => "turn/start", "params" => %{"input" => [%{"text" => init_text}]}}
           ] = non_handshake_writes(transcript)

    assert init_text =~ "Generate a file named AGENTS.md"
    refute init_text =~ "extra words"
  end

  test "slash_command_with_leading_whitespace" do
    steps = session_steps() ++ [prompt(20, [text("   \n/compact")])]
    transcript = CodexGolden.assert_golden(@area, "slash_command_with_leading_whitespace", steps)
    assert [%{"method" => "thread/compact/start"}] = non_handshake_writes(transcript)
  end

  # -- mode, effort, model, and directory wire params -------------------------

  test "mode_read_only_turn_params" do
    steps = session_steps(new_params: %{"modeId" => "read-only"}) ++ [prompt(20, [text("hi")])]
    transcript = CodexGolden.assert_golden(@area, "mode_read_only_turn_params", steps)

    assert %{
             "approvalPolicy" => "on-request",
             "approvalsReviewer" => "user",
             "sandboxPolicy" => %{
               "type" => "workspaceWrite",
               "writableRoots" => [],
               "networkAccess" => false,
               "excludeTmpdirEnvVar" => false,
               "excludeSlashTmp" => false
             }
           } = last_turn_start(transcript)
  end

  test "mode_agent_full_access_turn_params" do
    steps =
      session_steps(new_params: %{"modeId" => "agent-full-access"}) ++ [prompt(20, [text("hi")])]

    transcript = CodexGolden.assert_golden(@area, "mode_agent_full_access_turn_params", steps)

    assert %{
             "approvalPolicy" => "never",
             "approvalsReviewer" => "user",
             "sandboxPolicy" => %{"type" => "dangerFullAccess"}
           } = last_turn_start(transcript)
  end

  test "mode_unknown_omits_policy_params" do
    steps =
      session_steps(new_params: %{"modeId" => "yolo"}) ++
        [
          {:note,
           "An unrecognized modeId is accepted as-is: thread/start and turn/start carry no " <>
             "sandbox/approval keys and the session reports currentModeId yolo"},
          prompt(20, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "mode_unknown_omits_policy_params", steps)

    params = last_turn_start(transcript)
    refute Map.has_key?(params, "sandboxPolicy")
    refute Map.has_key?(params, "approvalPolicy")
    refute Map.has_key?(params, "approvalsReviewer")
  end

  test "set_mode_read_only_before_prompt" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/set_mode",
             "id" => 11,
             "params" => %{"sessionId" => @session_id, "modeId" => "read-only"}
           }},
          prompt(20, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "set_mode_read_only_before_prompt", steps)

    assert %{
             tag: :messages_and_reply,
             reply: %{},
             messages: [
               %{
                 "params" => %{
                   "update" => %{
                     "sessionUpdate" => "current_mode_update",
                     "currentModeId" => "read-only"
                   }
                 }
               }
             ]
           } = acp_result(transcript, 11)

    assert %{"approvalsReviewer" => "user", "approvalPolicy" => "on-request"} =
             last_turn_start(transcript)
  end

  test "set_config_option_mode_full_access_before_prompt" do
    steps =
      session_steps() ++
        [
          set_config_option(12, "mode", "agent-full-access"),
          prompt(20, [text("hi")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_mode_full_access_before_prompt", steps)

    assert %{"approvalPolicy" => "never", "sandboxPolicy" => %{"type" => "dangerFullAccess"}} =
             last_turn_start(transcript)
  end

  test "set_config_option_reasoning_effort_before_prompt" do
    steps =
      session_steps() ++
        [
          set_config_option(12, "reasoning_effort", "high"),
          prompt(20, [text("hi")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "set_config_option_reasoning_effort_before_prompt", steps)

    assert %{"model" => "gpt-5", "effort" => "high"} = last_turn_start(transcript)
  end

  test "set_config_option_model_before_prompt" do
    steps =
      session_steps() ++
        [
          set_config_option(12, "model", "codex-mini"),
          prompt(20, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "set_config_option_model_before_prompt", steps)

    assert %{"model" => "gpt-5-codex", "effort" => "medium"} = last_turn_start(transcript)
  end

  test "set_model_catalog_id_before_prompt" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/set_model",
             "id" => 13,
             "params" => %{"sessionId" => @session_id, "modelId" => "codex-mini/high"}
           }},
          prompt(20, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "set_model_catalog_id_before_prompt", steps)
    assert %{"model" => "gpt-5-codex", "effort" => "high"} = last_turn_start(transcript)
  end

  test "fast_mode_adds_service_tier" do
    steps =
      session_steps() ++
        [
          {:note, "gpt-5 advertises additionalSpeedTiers [fast] in the catalog"},
          set_config_option(12, "fast-mode", "on"),
          prompt(20, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "fast_mode_adds_service_tier", steps)
    assert %{"model" => "gpt-5", "serviceTier" => "fast"} = last_turn_start(transcript)
  end

  test "fast_mode_unsupported_model_omits_service_tier" do
    steps =
      session_steps() ++
        [
          set_config_option(12, "model", "codex-mini"),
          set_config_option(13, "fast-mode", "on"),
          prompt(20, [text("hi")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "fast_mode_unsupported_model_omits_service_tier", steps)

    params = last_turn_start(transcript)
    assert params["model"] == "gpt-5-codex"
    refute Map.has_key?(params, "serviceTier")
  end

  test "additional_directories_in_writable_roots" do
    steps =
      session_steps(
        new_params: %{"additionalDirectories" => ["/tmp/shared", "/tmp/shared", "/tmp/vendor"]}
      ) ++
        [prompt(20, [text("hi")])]

    transcript =
      CodexGolden.assert_golden(@area, "additional_directories_in_writable_roots", steps)

    assert %{
             "sandboxPolicy" => %{
               "type" => "workspaceWrite",
               "writableRoots" => ["/tmp/shared", "/tmp/vendor"]
             }
           } =
             last_turn_start(transcript)
  end

  test "additional_directories_ignored_under_full_access" do
    steps =
      session_steps(
        new_params: %{"modeId" => "agent-full-access", "additionalDirectories" => ["/tmp/shared"]}
      ) ++
        [prompt(20, [text("hi")])]

    transcript =
      CodexGolden.assert_golden(@area, "additional_directories_ignored_under_full_access", steps)

    assert %{"sandboxPolicy" => %{"type" => "dangerFullAccess"}} = last_turn_start(transcript)
  end

  test "thread_start_result_settings_flow_to_turn" do
    steps =
      session_steps(thread_result: %{"reasoningEffort" => "high", "serviceTier" => "fast"}) ++
        [prompt(20, [text("hi")])]

    transcript =
      CodexGolden.assert_golden(@area, "thread_start_result_settings_flow_to_turn", steps)

    assert %{"model" => "gpt-5", "effort" => "high", "serviceTier" => "fast"} =
             last_turn_start(transcript)
  end

  test "init_opts_defaults_flow_to_turn" do
    init_opts =
      @init_opts ++ [model: "gpt-5-codex", reasoning_effort: "high", mode_id: "read-only"]

    steps =
      session_steps(init_opts: init_opts, thread_result: %{"model" => nil}) ++
        [
          {:note, "thread/start answered without a model: the init :model option is used"},
          prompt(20, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "init_opts_defaults_flow_to_turn", steps)

    assert %{"model" => "gpt-5-codex", "effort" => "high", "approvalsReviewer" => "user"} =
             last_turn_start(transcript)
  end

  test "model_session_result_beats_init_default" do
    steps =
      session_steps(init_opts: @init_opts ++ [model: "gpt-5-codex"]) ++
        [
          {:note,
           "thread/start is asked for the init :model, but the model the app-server answers " <>
             "with wins for turn/start"},
          prompt(20, [text("hi")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "model_session_result_beats_init_default", steps)

    assert %{"model" => "gpt-5-codex"} = thread_start(transcript)
    assert %{"model" => "gpt-5"} = last_turn_start(transcript)
  end

  test "cwd_session_beats_init_option" do
    steps =
      session_steps(init_opts: [workspace_roots: ["/tmp"], cwd: "/tmp"]) ++
        [prompt(20, [text("/status")]), prompt(21, [text("hi")])]

    transcript = CodexGolden.assert_golden(@area, "cwd_session_beats_init_option", steps)

    assert status_text(transcript, 20) =~ "**Directory:** /tmp/project  \n"
    assert %{"cwd" => "/tmp/project"} = last_turn_start(transcript)
  end

  test "cwd_init_option_fallback_without_session_cwd" do
    steps =
      session_steps(
        init_opts: [workspace_roots: ["/tmp"], cwd: "/tmp"],
        thread_result: %{"thread" => %{"id" => @session_id, "updatedAt" => 1_700_000_000}}
      ) ++
        [
          {:note,
           "The session cwd comes from the thread/start result, not from session/new: " <>
             "without thread.cwd the init :cwd option is used"},
          prompt(20, [text("/status")]),
          prompt(21, [text("hi")])
        ]

    transcript =
      CodexGolden.assert_golden(@area, "cwd_init_option_fallback_without_session_cwd", steps)

    assert %{"cwd" => "/tmp/project"} = thread_start(transcript)
    assert status_text(transcript, 20) =~ "**Directory:** /tmp  \n"
    assert %{"cwd" => "/tmp"} = last_turn_start(transcript)
  end

  test "cwd_top_level_result_beats_thread_cwd" do
    steps =
      session_steps(thread_result: %{"cwd" => "/tmp/top"}) ++
        [
          {:note,
           "A top-level cwd in the thread/start result takes precedence over thread.cwd " <>
             "(/tmp/project) for both /status and turn/start"},
          prompt(20, [text("/status")]),
          prompt(21, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "cwd_top_level_result_beats_thread_cwd", steps)

    assert status_text(transcript, 20) =~ "**Directory:** /tmp/top  \n"
    assert %{"cwd" => "/tmp/top"} = last_turn_start(transcript)
  end

  test "prompt_cwd_outside_workspace_roots_forwarded" do
    steps =
      session_steps() ++
        [
          {:note,
           "session/prompt params.cwd is forwarded to turn/start without any workspace " <>
             "authorization"},
          prompt(20, [text("hi")], %{"cwd" => "/opt/elsewhere"})
        ]

    transcript =
      CodexGolden.assert_golden(@area, "prompt_cwd_outside_workspace_roots_forwarded", steps)

    assert %{"cwd" => "/opt/elsewhere"} = last_turn_start(transcript)
    assert %{tag: :ok} = CodexGolden.last_result(transcript)
  end

  test "prompt_params_override_model_summary_cwd" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("hi")], %{
            "model" => "o3",
            "summary" => "detailed",
            "cwd" => "/tmp/project/sub"
          })
        ]

    transcript =
      CodexGolden.assert_golden(@area, "prompt_params_override_model_summary_cwd", steps)

    assert %{
             "model" => "o3",
             "summary" => "detailed",
             "cwd" => "/tmp/project/sub",
             "effort" => "medium"
           } =
             last_turn_start(transcript)
  end

  # -- session preconditions and turn/start replies ---------------------------

  test "prompt_missing_session_id" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{"method" => "session/prompt", "id" => 20, "params" => %{"prompt" => [text("hi")]}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "prompt_missing_session_id", steps)
    assert %{tag: :error, error: "sessionId is required"} = CodexGolden.last_result(transcript)
  end

  test "prompt_empty_session_id" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/prompt",
             "id" => 20,
             "params" => %{"sessionId" => "", "prompt" => [text("hi")]}
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "prompt_empty_session_id", steps)
    assert %{tag: :error, error: "sessionId is required"} = CodexGolden.last_result(transcript)
  end

  test "prompt_unknown_session" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/prompt",
             "id" => 20,
             "params" => %{"sessionId" => "thread-nope", "prompt" => [text("hi")]}
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "prompt_unknown_session", steps)

    assert %{tag: :error, error: "Unknown Codex session: thread-nope"} =
             CodexGolden.last_result(transcript)
  end

  test "slash_command_unknown_session" do
    steps =
      session_steps() ++
        [
          {:outbound,
           %{
             "method" => "session/prompt",
             "id" => 20,
             "params" => %{"sessionId" => "thread-nope", "prompt" => [text("/status")]}
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "slash_command_unknown_session", steps)

    assert %{tag: :error, error: "Unknown Codex session: thread-nope"} =
             CodexGolden.last_result(transcript)
  end

  test "prompt_closed_session" do
    steps =
      session_steps() ++
        [
          {:outbound, %{"method" => "session/close", "params" => %{"sessionId" => @session_id}}},
          {:note, "A closed session is forgotten, so prompting it reports it as unknown"},
          prompt(20, [text("hi")])
        ]

    transcript = CodexGolden.assert_golden(@area, "prompt_closed_session", steps)

    assert %{tag: :error, error: "Unknown Codex session: thread-abc"} =
             CodexGolden.last_result(transcript)
  end

  test "prompt_while_turn_active" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          {:inbound,
           %{
             "method" => "turn/started",
             "params" => %{"threadId" => @session_id, "turn" => %{"id" => "turn-1"}}
           }},
          {:note,
           "A second prompt while turn-1 is active starts another turn; prompt 20 is never answered"},
          prompt(21, [text("second")]),
          {:inbound, %{"id" => 5, "result" => %{"turn" => %{"id" => "turn-2"}}}},
          {:inbound,
           %{
             "method" => "turn/completed",
             "params" => %{
               "threadId" => @session_id,
               "turn" => %{"id" => "turn-2", "status" => "completed"}
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "prompt_while_turn_active", steps)

    assert [%{"id" => 4, "method" => "turn/start"}, %{"id" => 5, "method" => "turn/start"}] =
             non_handshake_writes(transcript)

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "session/update"},
               %{"id" => 21, "result" => %{"stopReason" => "end_turn"}}
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "prompt_late_turn_start_reply_repins_active_prompt" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          prompt(21, [text("second")]),
          {:note,
           "The app-server answers the two turn/start requests out of order. Each ok reply " <>
             "re-pins the ACP request it belongs to as the active prompt, so after the late " <>
             "reply to id 4 the completion of turn-1 answers prompt 20, not the newer prompt 21"},
          {:inbound, %{"id" => 5, "result" => %{"turn" => %{"id" => "turn-2"}}}},
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          turn_completed("turn-1")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "prompt_late_turn_start_reply_repins_active_prompt", steps)

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "session/update"},
               %{
                 "id" => 20,
                 "result" => %{
                   "stopReason" => "end_turn",
                   "_meta" => %{"ex_mcp" => %{"turnId" => "turn-1"}}
                 }
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "prompt_while_turn_active_resets_accumulated_text" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          turn_started("turn-1"),
          agent_message_delta("turn-1", "item-1", "partial"),
          {:note,
           "A second prompt while turn-1 is still streaming clears the accumulated text, so " <>
             "the completion of turn-2 reports empty text rather than \"partial\""},
          prompt(21, [text("second")]),
          {:inbound, %{"id" => 5, "result" => %{"turn" => %{"id" => "turn-2"}}}},
          turn_completed("turn-2")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "prompt_while_turn_active_resets_accumulated_text", steps)

    assert %{
             messages: [%{"params" => %{"update" => %{"content" => %{"text" => "partial"}}}}]
           } = inbound_result(transcript, "item/agentMessage/delta")

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "session/update"},
               %{
                 "id" => 21,
                 "result" => %{
                   "stopReason" => "end_turn",
                   "_meta" => %{"ex_mcp" => %{"text" => ""}}
                 }
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "prompt_while_turn_active_resets_streamed_items" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          turn_started("turn-1"),
          {:note,
           "A delta without an itemId is tracked in the shared :current slot; the second " <>
             "prompt clears it, so turn-2's completed agent message is emitted in full " <>
             "(\"partial done\") instead of only the unstreamed remainder (\" done\")"},
          agent_message_delta("turn-1", nil, "partial"),
          prompt(21, [text("second")]),
          {:inbound, %{"id" => 5, "result" => %{"turn" => %{"id" => "turn-2"}}}},
          turn_started("turn-2"),
          {:inbound,
           %{
             "method" => "item/completed",
             "params" => %{
               "threadId" => @session_id,
               "turnId" => "turn-2",
               "item" => %{"type" => "agentMessage", "id" => "item-2", "text" => "partial done"}
             }
           }},
          turn_completed("turn-2")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "prompt_while_turn_active_resets_streamed_items", steps)

    assert %{
             messages: [
               %{
                 "params" => %{
                   "update" => %{
                     "sessionUpdate" => "agent_message_chunk",
                     "content" => %{"text" => "partial done"},
                     "_meta" => %{"ex_mcp" => %{"final" => true}}
                   }
                 }
               }
             ]
           } = inbound_result(transcript, "item/completed")

    assert %{
             messages: [
               %{"method" => "session/update"},
               %{"id" => 21, "result" => %{"_meta" => %{"ex_mcp" => %{"text" => "partial done"}}}}
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "prompt_while_turn_active_resets_rate_limits" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          turn_started("turn-1"),
          rate_limits_exhausted(),
          {:note,
           "A second prompt while turn-1 is active clears the exhausted rate limits observed " <>
             "during turn-1, so turn-2 completing with no text is a plain end_turn instead of " <>
             "the -32029 rate_limit_exhausted error pinned by " <>
             "prompt_after_completed_turn_resets_prompt_activity"},
          prompt(21, [text("second")]),
          {:inbound, %{"id" => 5, "result" => %{"turn" => %{"id" => "turn-2"}}}},
          turn_completed("turn-2")
        ]

    transcript =
      CodexGolden.assert_golden(@area, "prompt_while_turn_active_resets_rate_limits", steps)

    assert %{
             messages: [
               %{
                 "params" => %{
                   "update" => %{
                     "sessionUpdate" => "session_info_update",
                     "_meta" => %{
                       "ex_mcp" => %{"rateLimits" => %{"rateLimitReachedType" => "primary"}}
                     }
                   }
                 }
               }
             ]
           } = inbound_result(transcript, "account/rateLimits/updated")

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "session/update"},
               %{"id" => 21, "result" => %{"stopReason" => "end_turn"} = result}
             ]
           } = CodexGolden.last_result(transcript)

    refute Map.has_key?(result, "error")
  end

  test "prompt_after_completed_turn_resets_prompt_activity" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          turn_started("turn-1"),
          agent_message_delta("turn-1", "item-1", "hi"),
          turn_completed("turn-1"),
          {:note,
           "turn-1 produced output, which marks prompt activity; turn/completed does not " <>
             "clear that mark, only the next session/prompt does. So when turn-2 then hits " <>
             "exhausted rate limits and completes with no text, prompt 21 fails with -32029 " <>
             "rate_limit_exhausted instead of inheriting turn-1's activity as an end_turn"},
          prompt(21, [text("second")]),
          {:inbound, %{"id" => 5, "result" => %{"turn" => %{"id" => "turn-2"}}}},
          turn_started("turn-2"),
          rate_limits_exhausted(),
          turn_completed("turn-2")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "prompt_after_completed_turn_resets_prompt_activity",
        steps
      )

    assert %{
             messages: [
               %{"method" => "session/update"},
               %{
                 "id" => 20,
                 "result" => %{
                   "stopReason" => "end_turn",
                   "_meta" => %{"ex_mcp" => %{"text" => "hi"}}
                 }
               }
             ]
           } = inbound_result(transcript, "turn/completed")

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "session/update"},
               %{
                 "id" => 21,
                 "error" => %{
                   "code" => -32_029,
                   "message" => "Codex rate limit exhausted before the model produced a response",
                   "data" => %{
                     "kind" => "rate_limit_exhausted",
                     "provider" => "codex",
                     "rateLimits" => %{"rateLimitReachedType" => "primary"}
                   }
                 }
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "turn_output_under_exhausted_rate_limits_is_end_turn" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("first")]),
          {:inbound, %{"id" => 4, "result" => %{"turn" => %{"id" => "turn-1"}}}},
          turn_started("turn-1"),
          rate_limits_exhausted(),
          {:note,
           "The -32029 capacity failure only fires for a turn that produced nothing: a " <>
             "streamed delta after the exhausted rate limits makes the completion a plain " <>
             "end_turn carrying the streamed text"},
          agent_message_delta("turn-1", "item-1", "hi"),
          turn_completed("turn-1")
        ]

    transcript =
      CodexGolden.assert_golden(
        @area,
        "turn_output_under_exhausted_rate_limits_is_end_turn",
        steps
      )

    assert %{
             tag: :messages,
             messages: [
               %{"method" => "session/update"},
               %{
                 "id" => 20,
                 "result" => %{
                   "stopReason" => "end_turn",
                   "_meta" => %{"ex_mcp" => %{"text" => "hi"}}
                 }
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "turn_start_error_replies_error" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("hi")]),
          {:inbound,
           %{"id" => 4, "error" => %{"code" => -32_602, "message" => "unsupported model: gpt-5"}}}
        ]

    transcript = CodexGolden.assert_golden(@area, "turn_start_error_replies_error", steps)

    assert %{
             tag: :messages,
             messages: [
               %{
                 "id" => 20,
                 "jsonrpc" => "2.0",
                 "error" => %{"code" => -32_602, "message" => "unsupported model: gpt-5"}
               }
             ]
           } = CodexGolden.last_result(transcript)
  end

  test "turn_start_error_without_code_defaults_minus_one" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("hi")]),
          {:note,
           "An app-server error object without a code is relayed with code -1 " <>
             "(JSON-RPC requires a code, so the app-server payload is degenerate)"},
          {:inbound, %{"id" => 4, "error" => %{"message" => "boom"}}}
        ]

    transcript =
      CodexGolden.assert_golden(@area, "turn_start_error_without_code_defaults_minus_one", steps)

    assert %{
             tag: :messages,
             messages: [%{"id" => 20, "error" => %{"code" => -1, "message" => "boom"}}]
           } = CodexGolden.last_result(transcript)
  end

  test "turn_start_error_data_dropped" do
    steps =
      session_steps() ++
        [
          prompt(20, [text("hi")]),
          {:note,
           "Only code and message of an app-server error object are relayed; its data " <>
             "field is dropped from the ACP error"},
          {:inbound,
           %{
             "id" => 4,
             "error" => %{
               "code" => -32_000,
               "message" => "turn rejected",
               "data" => %{"reason" => "thread busy", "threadId" => @session_id}
             }
           }}
        ]

    transcript = CodexGolden.assert_golden(@area, "turn_start_error_data_dropped", steps)

    assert %{
             tag: :messages,
             messages: [%{"id" => 20, "error" => error}]
           } = CodexGolden.last_result(transcript)

    assert error == %{"code" => -32_000, "message" => "turn rejected"}
  end

  # -- step helpers -----------------------------------------------------------

  # initialize -> initialized + model/list -> catalog. App-server ids 1 and 2.
  defp handshake_steps(init_opts, catalog) do
    [
      {:init, init_opts},
      :post_connect,
      {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}},
      {:inbound, %{"id" => 2, "result" => %{"data" => catalog, "nextCursor" => nil}}}
    ]
  end

  # Handshake, then session/new (ACP id 10) answered by thread/start (app-server
  # id 3) so that the next app-server request id is 4.
  #
  # The default init :cwd and session/new cwd are the same path on purpose
  # (most scenarios are not about cwd precedence); the cwd_* scenarios pass
  # distinct paths so the two sources are distinguishable.
  #
  #   * `:init_opts` - overrides the adapter init options
  #   * `:new_params` - merged into the session/new params
  #   * `:thread_result` - merged into the thread/start result
  #   * `:catalog` - replaces the model/list catalog
  defp session_steps(opts \\ []) do
    new_params =
      Map.merge(%{"cwd" => @cwd, "mcpServers" => []}, Keyword.get(opts, :new_params, %{}))

    thread_result =
      Map.merge(
        %{
          "model" => "gpt-5",
          "thread" => %{"id" => @session_id, "cwd" => @cwd, "updatedAt" => 1_700_000_000}
        },
        Keyword.get(opts, :thread_result, %{})
      )

    handshake_steps(
      Keyword.get(opts, :init_opts, @init_opts),
      Keyword.get(opts, :catalog, catalog_models())
    ) ++
      [
        {:outbound, %{"method" => "session/new", "id" => 10, "params" => new_params}},
        {:inbound, %{"id" => 3, "result" => thread_result}}
      ]
  end

  # thread/tokenUsage/updated for the current turn with a total of 10 input
  # (2 cached) and 5 output tokens.
  defp token_usage_update do
    {:inbound,
     %{
       "method" => "thread/tokenUsage/updated",
       "params" => %{
         "threadId" => @session_id,
         "tokenUsage" => %{
           "last" => %{"inputTokens" => 4, "outputTokens" => 1},
           "modelContextWindow" => 100,
           "total" => %{"inputTokens" => 10, "outputTokens" => 5, "cachedInputTokens" => 2}
         }
       }
     }}
  end

  # account/rateLimits/updated with the primary window exhausted, no credits,
  # and rateLimitReachedType primary: the shape that makes an output-less turn
  # fail with -32029 rate_limit_exhausted.
  defp rate_limits_exhausted do
    {:inbound,
     %{
       "method" => "account/rateLimits/updated",
       "params" => %{
         "rateLimits" => %{
           "limitId" => "codex",
           "planType" => "plus",
           "primary" => %{
             "usedPercent" => 100,
             "windowDurationMins" => 300,
             "resetsAt" => 1_700_003_600
           },
           "secondary" => %{
             "usedPercent" => 37,
             "windowDurationMins" => 10_080,
             "resetsAt" => 1_700_600_000
           },
           "credits" => %{"hasCredits" => false, "unlimited" => false},
           "rateLimitReachedType" => "primary"
         }
       }
     }}
  end

  defp turn_started(turn_id) do
    {:inbound,
     %{
       "method" => "turn/started",
       "params" => %{"threadId" => @session_id, "turn" => %{"id" => turn_id}}
     }}
  end

  defp turn_completed(turn_id) do
    {:inbound,
     %{
       "method" => "turn/completed",
       "params" => %{
         "threadId" => @session_id,
         "turn" => %{"id" => turn_id, "status" => "completed"}
       }
     }}
  end

  # item/agentMessage/delta; a nil item_id omits the itemId key entirely.
  defp agent_message_delta(turn_id, item_id, delta) do
    params =
      %{"threadId" => @session_id, "turnId" => turn_id, "delta" => delta}
      |> then(&if(item_id, do: Map.put(&1, "itemId", item_id), else: &1))

    {:inbound, %{"method" => "item/agentMessage/delta", "params" => params}}
  end

  defp prompt(acp_id, prompt, extra_params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/prompt",
       "id" => acp_id,
       "params" => Map.merge(%{"sessionId" => @session_id, "prompt" => prompt}, extra_params)
     }}
  end

  defp set_config_option(acp_id, config_id, value) do
    {:outbound,
     %{
       "method" => "session/set_config_option",
       "id" => acp_id,
       "params" => %{"sessionId" => @session_id, "configId" => config_id, "value" => value}
     }}
  end

  defp text(text), do: %{"type" => "text", "text" => text}
  defp image(fields), do: Map.put(fields, "type", "image")
  defp resource(fields), do: %{"type" => "resource", "resource" => fields}
  defp resource_link(fields), do: Map.put(fields, "type", "resource_link")

  # Codex app-server v2 `model/list` shape. gpt-5 advertises the fast service
  # tier so that fast-mode scenarios can observe `serviceTier`.
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
        "additionalSpeedTiers" => ["fast"],
        "supportedReasoningEfforts" => [
          %{"reasoningEffort" => "low", "description" => "Quick"},
          %{"reasoningEffort" => "high", "description" => "Deep"}
        ]
      }
    ]
  end

  # -- transcript helpers -----------------------------------------------------

  # Writes after the initialize / initialized / model/list / thread/start prefix.
  defp non_handshake_writes(transcript) do
    transcript
    |> CodexGolden.writes()
    |> Enum.reject(&(&1["method"] in ["initialize", "initialized", "model/list", "thread/start"]))
  end

  defp thread_start(transcript) do
    %{"method" => "thread/start", "params" => params} =
      transcript |> CodexGolden.writes() |> Enum.find(&(&1["method"] == "thread/start"))

    params
  end

  # The result recorded for the outbound ACP request with id `acp_id`.
  defp acp_result(transcript, acp_id) do
    %CodexGolden.Entry{result: result} =
      Enum.find(transcript, fn entry ->
        match?(%{kind: :outbound, message: %{"id" => ^acp_id}}, entry.step)
      end)

    result
  end

  # The result recorded for the first inbound app-server notification `method`.
  defp inbound_result(transcript, method) do
    %CodexGolden.Entry{result: result} =
      Enum.find(transcript, fn entry ->
        match?(%{kind: :inbound, message: %{"method" => ^method}}, entry.step)
      end)

    result
  end

  # The agent_message_chunk text of the /status reply to ACP request `acp_id`.
  defp status_text(transcript, acp_id) do
    %{
      tag: :messages_and_reply,
      messages: [%{"params" => %{"update" => %{"content" => %{"text" => status}}}}]
    } = acp_result(transcript, acp_id)

    status
  end

  defp last_turn_start(transcript) do
    %{"method" => "turn/start", "params" => params} =
      transcript
      |> CodexGolden.writes()
      |> Enum.filter(&(&1["method"] == "turn/start"))
      |> List.last()

    params
  end
end
