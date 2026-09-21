defmodule ExMCP.ACP.Adapters.ClaudeSDK.PromptContentGoldenTest do
  @moduledoc """
  Characterization gate for the Claude SDK adapter's prompt content
  conversion (`docs/POST_1_0_MAINTENANCE_PLAN.md`, "Claude adapter
  characterization gate": prompt content conversion for text, images,
  resources, and resource links).

  Each test drives `ExMCP.ACP.Adapters.ClaudeSDK` through
  `ExMCP.Test.ClaudeGolden` and compares the recorded transcript against a
  committed fixture under `test/fixtures/acp/claude/prompt_content/`. The
  fixtures pin the `type: "user"` SDK message a `session/prompt` writes for:

    * a bare string prompt, a `nil` prompt, an empty block list, a text
      block without its `text` key, and a bare string inside the list;
    * the `/mcp:<server>:<command>` rewrite (with and without arguments)
      and the shapes it deliberately leaves alone;
    * images by URL (`http`/`https`) and by base64 payload, including the
      `mimeType` / `media_type` precedence and the `image/png` default for
      a source with neither;
    * resource links rendered as `[@name](uri)`, the `file://` basename
      fallback (including a trailing slash), a nested `resource.uri`, a
      non-file URI rendered bare, and a link with no URI at all;
    * embedded resources emitting the link inline while their
      `<context ref="...">` block is appended after *all* inline content,
      with `resource.text`, a block-level `text`, `resource.blob`, and the
      `"resource"` URI fallback;
    * the errors for an unsupported block type, a non-map block, and a
      prompt that is neither a string nor a list - including a prompt that
      fails while another prompt is active, which is refused rather than
      queued;
    * the `session_id` stamped on the message: the open session, an
      explicitly different session (which the adapter adopts), and `""`
      when no session is open.

  Prompt scheduling (queueing, cancellation, settlement) belongs to the
  faults area; this file pins only what a prompt turns into on the wire.

  Mutation check (2026-09-21): in `claude_sdk/protocol.ex`, appending the
  deferred context blocks *before* the inline content in `prompt_content/1`
  (swapping the two halves of `Enum.reverse(content) ++
  Enum.reverse(context)`) fails `resource_context_is_appended_after_content`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CLAUDE_GOLDEN=update mix test <this file>[:line]`; that run rewrites
  the fixture and fails on purpose, so review the diff and re-run without
  the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.ClaudeGolden
  alias ExMCP.Test.ClaudeGolden.Flows

  @area "prompt_content"

  describe "text" do
    test "string_prompt_becomes_one_text_block" do
      transcript = golden("string_prompt_becomes_one_text_block", "Summarize the repo")

      assert [
               %{
                 "message" => %{
                   "content" => [%{"type" => "text", "text" => "Summarize the repo"}]
                 }
               }
             ] =
               ClaudeGolden.writes(transcript)
    end

    test "nil_prompt_becomes_an_empty_text_block" do
      golden("nil_prompt_becomes_an_empty_text_block", nil)
    end

    test "empty_block_list_becomes_empty_content" do
      transcript = golden("empty_block_list_becomes_empty_content", [])

      assert [%{"message" => %{"content" => []}}] = ClaudeGolden.writes(transcript)
    end

    test "text_block_is_converted" do
      golden("text_block_is_converted", [%{"type" => "text", "text" => "hello"}])
    end

    test "text_block_without_text_becomes_empty_string" do
      golden("text_block_without_text_becomes_empty_string", [%{"type" => "text"}])
    end

    test "bare_string_block_is_converted" do
      golden("bare_string_block_is_converted", ["raw string block"])
    end

    test "multiple_text_blocks_keep_their_order" do
      golden("multiple_text_blocks_keep_their_order", [
        %{"type" => "text", "text" => "first"},
        %{"type" => "text", "text" => "second"},
        %{"type" => "text", "text" => "third"}
      ])
    end
  end

  describe "mcp slash commands" do
    test "mcp_slash_command_is_rewritten" do
      transcript =
        golden("mcp_slash_command_is_rewritten", [
          %{"type" => "text", "text" => "/mcp:docs:search"}
        ])

      assert [%{"message" => %{"content" => [%{"text" => "/docs:search (MCP)"}]}}] =
               ClaudeGolden.writes(transcript)
    end

    test "mcp_slash_command_keeps_its_arguments" do
      golden("mcp_slash_command_keeps_its_arguments", [
        %{"type" => "text", "text" => "/mcp:docs:search elixir protocols"}
      ])
    end

    test "non_mcp_slash_commands_are_untouched" do
      golden("non_mcp_slash_commands_are_untouched", [
        %{"type" => "text", "text" => "/review"},
        %{"type" => "text", "text" => "/mcp:docs"},
        %{"type" => "text", "text" => "leading /mcp:docs:search"}
      ])
    end
  end

  describe "images" do
    test "http_image_uri_becomes_a_url_source" do
      golden("http_image_uri_becomes_a_url_source", [
        %{"type" => "image", "uri" => "http://example.test/a.png"}
      ])
    end

    test "https_image_uri_becomes_a_url_source" do
      golden("https_image_uri_becomes_a_url_source", [
        %{"type" => "image", "uri" => "https://example.test/a.png"}
      ])
    end

    test "base64_image_uses_mime_type" do
      golden("base64_image_uses_mime_type", [
        %{"type" => "image", "mimeType" => "image/jpeg", "data" => "QUFB"}
      ])
    end

    test "base64_image_falls_back_to_media_type" do
      golden("base64_image_falls_back_to_media_type", [
        %{"type" => "image", "media_type" => "image/webp", "data" => "QUFB"}
      ])
    end

    test "base64_image_defaults_to_png_and_empty_data" do
      golden("base64_image_defaults_to_png_and_empty_data", [%{"type" => "image"}])
    end

    test "data_uri_image_is_treated_as_base64" do
      golden("data_uri_image_is_treated_as_base64", [
        %{"type" => "image", "uri" => "data:image/png;base64,QUFB", "data" => "QUFB"}
      ])
    end
  end

  describe "resource links" do
    test "resource_link_uses_its_name" do
      transcript =
        golden("resource_link_uses_its_name", [
          %{"type" => "resource_link", "name" => "README", "uri" => "file:///tmp/README.md"}
        ])

      assert [%{"message" => %{"content" => [%{"text" => "[@README](file:///tmp/README.md)"}]}}] =
               ClaudeGolden.writes(transcript)
    end

    test "resource_link_falls_back_to_its_title" do
      golden("resource_link_falls_back_to_its_title", [
        %{"type" => "resource_link", "title" => "Notes", "uri" => "file:///tmp/notes.md"}
      ])
    end

    test "resource_link_without_name_uses_the_file_basename" do
      golden("resource_link_without_name_uses_the_file_basename", [
        %{"type" => "resource_link", "uri" => "file:///tmp/project/lib/app.ex"}
      ])
    end

    test "resource_link_strips_a_trailing_slash_before_the_basename" do
      golden("resource_link_strips_a_trailing_slash_before_the_basename", [
        %{"type" => "resource_link", "uri" => "file:///tmp/project/lib/"}
      ])
    end

    test "resource_link_reads_a_nested_resource_uri" do
      golden("resource_link_reads_a_nested_resource_uri", [
        %{"type" => "resource_link", "resource" => %{"uri" => "file:///tmp/nested.md"}}
      ])
    end

    test "resource_link_with_a_non_file_uri_is_rendered_bare" do
      golden("resource_link_with_a_non_file_uri_is_rendered_bare", [
        %{"type" => "resource_link", "uri" => "https://example.test/doc"}
      ])
    end

    test "resource_link_without_a_uri_becomes_empty_text" do
      golden("resource_link_without_a_uri_becomes_empty_text", [
        %{"type" => "resource_link"}
      ])
    end

    test "resource_link_with_a_blank_name_uses_the_uri" do
      golden("resource_link_with_a_blank_name_uses_the_uri", [
        %{"type" => "resource_link", "name" => "", "uri" => "file:///tmp/blank.md"}
      ])
    end
  end

  describe "embedded resources" do
    test "resource_emits_a_link_and_a_context_block" do
      transcript =
        golden("resource_emits_a_link_and_a_context_block", [
          %{
            "type" => "resource",
            "resource" => %{"uri" => "file:///tmp/a.ex", "text" => "defmodule A do\nend"}
          }
        ])

      assert [%{"message" => %{"content" => [link, context]}}] = ClaudeGolden.writes(transcript)
      assert link["text"] == "[@a.ex](file:///tmp/a.ex)"
      assert context["text"] =~ ~s(<context ref="file:///tmp/a.ex">)
    end

    test "resource_uses_a_block_level_text" do
      golden("resource_uses_a_block_level_text", [
        %{"type" => "resource", "uri" => "file:///tmp/b.ex", "text" => "block level"}
      ])
    end

    test "resource_falls_back_to_its_blob" do
      golden("resource_falls_back_to_its_blob", [
        %{"type" => "resource", "resource" => %{"uri" => "file:///tmp/c.bin", "blob" => "QUFB"}}
      ])
    end

    test "resource_without_a_uri_uses_the_resource_literal" do
      golden("resource_without_a_uri_uses_the_resource_literal", [
        %{"type" => "resource", "resource" => %{"text" => "orphan"}}
      ])
    end

    test "resource_without_any_text_emits_an_empty_context" do
      golden("resource_without_any_text_emits_an_empty_context", [
        %{"type" => "resource", "resource" => %{"uri" => "file:///tmp/d.ex"}}
      ])
    end

    test "resource_context_is_appended_after_content" do
      transcript =
        golden("resource_context_is_appended_after_content", [
          %{"type" => "text", "text" => "look at"},
          %{"type" => "resource", "resource" => %{"uri" => "file:///tmp/a.ex", "text" => "AAA"}},
          %{"type" => "text", "text" => "and"},
          %{"type" => "resource", "resource" => %{"uri" => "file:///tmp/b.ex", "text" => "BBB"}}
        ])

      assert [%{"message" => %{"content" => content}}] = ClaudeGolden.writes(transcript)

      assert Enum.map(content, & &1["text"]) == [
               "look at",
               "[@a.ex](file:///tmp/a.ex)",
               "and",
               "[@b.ex](file:///tmp/b.ex)",
               "\n<context ref=\"file:///tmp/a.ex\">\nAAA\n</context>",
               "\n<context ref=\"file:///tmp/b.ex\">\nBBB\n</context>"
             ]
    end

    test "mixed_blocks_keep_inline_order" do
      golden("mixed_blocks_keep_inline_order", [
        %{"type" => "text", "text" => "before"},
        %{"type" => "image", "mimeType" => "image/png", "data" => "QUFB"},
        %{"type" => "resource_link", "uri" => "file:///tmp/link.md"},
        %{"type" => "resource", "resource" => %{"uri" => "file:///tmp/e.ex", "text" => "EEE"}},
        %{"type" => "text", "text" => "after"}
      ])
    end
  end

  describe "rejected prompts" do
    test "unsupported_block_type_errors" do
      transcript =
        golden("unsupported_block_type_errors", [%{"type" => "audio", "data" => "QUFB"}])

      assert %{tag: :error} = ClaudeGolden.last_result(transcript)
    end

    test "non_map_block_errors" do
      golden("non_map_block_errors", [42])
    end

    test "map_prompt_errors" do
      golden("map_prompt_errors", %{"type" => "text", "text" => "not a list"})
    end

    test "the_first_bad_block_halts_the_conversion" do
      golden("the_first_bad_block_halts_the_conversion", [
        %{"type" => "text", "text" => "fine"},
        %{"type" => "video"},
        %{"type" => "audio"}
      ])
    end

    test "a_bad_queued_prompt_errors_instead_of_queueing" do
      steps =
        [Flows.session_new(), Flows.prompt("acp-prompt-1", "first")] ++
          [
            {:note, "A second prompt is queued, but only if its content converts"},
            Flows.prompt("acp-prompt-2", [%{"type" => "audio"}])
          ]

      transcript =
        ClaudeGolden.assert_golden(@area, "a_bad_queued_prompt_errors_instead_of_queueing", steps)

      assert %{tag: :error} = ClaudeGolden.last_result(transcript)
    end
  end

  describe "session id on the message" do
    test "prompt_without_a_session_sends_an_empty_session_id" do
      steps = [
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-prompt",
           "method" => "session/prompt",
           "params" => %{"sessionId" => nil, "prompt" => "no session yet"}
         }}
      ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "prompt_without_a_session_sends_an_empty_session_id",
          steps
        )

      assert [%{"session_id" => ""}] = ClaudeGolden.writes(transcript)
    end

    test "prompt_for_another_session_adopts_that_id" do
      steps = [
        Flows.session_new(),
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-prompt",
           "method" => "session/prompt",
           "params" => %{"sessionId" => "other-session", "prompt" => "hi"}
         }},
        {:note, "The adopted id is the one a later session/close must name"},
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-close",
           "method" => "session/close",
           "params" => %{"sessionId" => "other-session"}
         }}
      ]

      ClaudeGolden.assert_golden(@area, "prompt_for_another_session_adopts_that_id", steps)
    end
  end

  defp golden(name, prompt) do
    ClaudeGolden.assert_golden(@area, name, [
      Flows.session_new(),
      Flows.prompt("acp-prompt", prompt)
    ])
  end
end
