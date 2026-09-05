defmodule ExMCP.ACP.Adapters.Codex.McpConfigGoldenTest do
  @moduledoc """
  Characterization gate for the Codex ACP adapter's session configuration
  wire behavior: the `config` (and `modelProvider`) carried by `thread/start`
  and `thread/resume` (see `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Codex adapter
  restructuring" / "Characterization gate"), area `mcp_config`.

  The scenarios pin how `ExMCP.ACP.Adapters.Codex` turns ACP `mcpServers`
  into `config.mcp_servers` (stdio, http, sse, acp, untyped, malformed and
  duplicate entries), how a server is authorized (`authorize_mcp_server`
  callback, `trusted_mcp_servers` list or `:all`, secure default), how the
  session `cwd` and `additionalDirectories` are contained inside the
  workspace roots (or delegated to `authorize_workspace`), what
  `trust_authorized_workspaces` adds to `config.projects`, and how the
  `codex_config` / `model_provider` init options and a gateway login are
  layered into the same config.

  Each test drives the adapter through `ExMCP.Test.CodexGolden` and compares
  the recorded transcript against a fixture under
  `test/fixtures/acp/codex/mcp_config/`. To regenerate a fixture after an
  intentional behavior change, run the test with `CODEX_GOLDEN=update`; that
  run rewrites the fixture and fails on purpose, so review the diff and re-run
  without the variable to confirm:

      CODEX_GOLDEN=update mix test test/ex_mcp/acp/adapters/codex/characterization/mcp_config_golden_test.exs

  Deliberately unpinned: `ExMCP.Internal.WorkspacePath.within?/2` resolves
  symlinks with `:file.read_link/1`, so a cwd that is a symlink escaping the
  roots is only observable through a real symlink on disk. Creating one under
  `/tmp` in a `setup` block would write shared filesystem state outside the
  repository and could collide between concurrent runs, so this file leaves
  that branch to `test/ex_mcp/internal/workspace_path_test.exs`.
  """

  use ExUnit.Case, async: true

  import ExUnit.CaptureLog, only: [with_log: 1]

  alias ExMCP.ACP.Adapters.Codex
  alias ExMCP.Test.CodexGolden

  @area "mcp_config"

  @cwd "/tmp/project"

  @http_server %{
    "type" => "http",
    "name" => "remote tools",
    "url" => "https://mcp.example.test/mcp",
    "headers" => [%{"name" => "Authorization", "value" => "Bearer token"}]
  }

  @stdio_server %{
    "type" => "stdio",
    "name" => "local tools",
    "command" => "/usr/local/bin/tools-mcp",
    "args" => ["--stdio", "--verbose"],
    "env" => [%{"name" => "TOOLS_TOKEN", "value" => "secret"}]
  }

  # -- mcpServers conversion ------------------------------------------------

  describe "mcpServers conversion" do
    test "stdio_server_maps_command_args_env" do
      server = Map.put(@stdio_server, "cwd", "/tmp/project/tools")

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "The stdio server carries a cwd; the adapter only forwards command/args/env and snake_cases the name"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript = CodexGolden.assert_golden(@area, "stdio_server_maps_command_args_env", steps)

      assert %{
               "local_tools" => %{
                 "command" => "/usr/local/bin/tools-mcp",
                 "args" => ["--stdio", "--verbose"],
                 "env" => %{"TOOLS_TOKEN" => "secret"}
               }
             } = thread_config(transcript)["mcp_servers"]

      refute Map.has_key?(thread_config(transcript)["mcp_servers"]["local_tools"], "cwd")
    end

    test "stdio_server_empty_args_and_env_omit_keys" do
      server = %{@stdio_server | "args" => [], "env" => []}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(@area, "stdio_server_empty_args_and_env_omit_keys", steps)

      assert thread_config(transcript)["mcp_servers"] == %{
               "local_tools" => %{"command" => "/usr/local/bin/tools-mcp"}
             }
    end

    test "untyped_server_with_command_is_treated_as_stdio" do
      server = Map.delete(@stdio_server, "type")

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(@area, "untyped_server_with_command_is_treated_as_stdio", steps)

      assert %{"local_tools" => %{"command" => "/usr/local/bin/tools-mcp"}} =
               thread_config(transcript)["mcp_servers"]
    end

    test "padded_server_name_is_trimmed_before_snake_casing" do
      server = %{@stdio_server | "name" => "  local tools  "}

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "Leading and trailing whitespace is trimmed before inner whitespace runs become underscores"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "padded_server_name_is_trimmed_before_snake_casing",
          steps
        )

      assert Map.keys(thread_config(transcript)["mcp_servers"]) == ["local_tools"]
    end

    test "server_name_inner_whitespace_run_collapses_to_one_underscore" do
      server = %{@http_server | "name" => "remote \t  tools"}

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "A run of mixed inner whitespace (space, tab, space, space) becomes a single underscore, not one per character"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "server_name_inner_whitespace_run_collapses_to_one_underscore",
          steps
        )

      assert Map.keys(thread_config(transcript)["mcp_servers"]) == ["remote_tools"]
    end

    test "server_name_case_and_punctuation_are_preserved" do
      http = %{@http_server | "name" => "GitHub-Tools.v2 Beta", "headers" => []}
      stdio = %{@stdio_server | "name" => "Local Tools 3", "args" => [], "env" => []}

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "Sanitizing only trims and replaces whitespace runs: upper-case letters, hyphens, dots and digits survive into the mcp_servers key"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [http, stdio]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "server_name_case_and_punctuation_are_preserved", steps)

      assert Map.keys(thread_config(transcript)["mcp_servers"]) ==
               ["GitHub-Tools.v2_Beta", "Local_Tools_3"]
    end

    test "http_server_maps_url_and_headers" do
      steps =
        opened(authorizing_all()) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})]

      transcript = CodexGolden.assert_golden(@area, "http_server_maps_url_and_headers", steps)

      assert thread_config(transcript)["mcp_servers"] == %{
               "remote_tools" => %{
                 "url" => "https://mcp.example.test/mcp",
                 "http_headers" => %{"Authorization" => "Bearer token"}
               }
             }
    end

    test "http_server_empty_headers_omit_http_headers" do
      server = %{@http_server | "headers" => []}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(@area, "http_server_empty_headers_omit_http_headers", steps)

      assert thread_config(transcript)["mcp_servers"] == %{
               "remote_tools" => %{"url" => "https://mcp.example.test/mcp"}
             }
    end

    test "http_server_plain_http_scheme_is_accepted" do
      server = %{@http_server | "url" => "http://localhost:4000/mcp", "headers" => []}

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "A plain http:// URL (the local dev-server case) passes validation like https:// and is forwarded unchanged"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "http_server_plain_http_scheme_is_accepted", steps)

      assert thread_config(transcript)["mcp_servers"] == %{
               "remote_tools" => %{"url" => "http://localhost:4000/mcp"}
             }
    end

    test "http_server_url_is_forwarded_verbatim" do
      remote = %{
        @http_server
        | "name" => "Remote API",
          "url" => "https://mcp.example.test:8443/mcp?token=abc&v=2"
      }

      local = %{
        @http_server
        | "name" => "local dev",
          "url" => "http://localhost:4000/",
          "headers" => []
      }

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "Validation parses the URL but the emitted url is the original string: explicit port, query string and trailing slash are all kept byte for byte"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [remote, local]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "http_server_url_is_forwarded_verbatim", steps)

      assert thread_config(transcript)["mcp_servers"] == %{
               "Remote_API" => %{
                 "url" => "https://mcp.example.test:8443/mcp?token=abc&v=2",
                 "http_headers" => %{"Authorization" => "Bearer token"}
               },
               "local_dev" => %{"url" => "http://localhost:4000/"}
             }
    end

    test "duplicate_header_and_env_names_last_wins" do
      http = %{
        @http_server
        | "headers" => [
            %{"name" => "Authorization", "value" => "Bearer old"},
            %{"name" => "X-Team", "value" => "platform"},
            %{"name" => "Authorization", "value" => "Bearer new"}
          ]
      }

      stdio = %{
        @stdio_server
        | "env" => [
            %{"name" => "TOOLS_TOKEN", "value" => "secret"},
            %{"name" => "TOOLS_TOKEN", "value" => "rotated"}
          ]
      }

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "Headers and env lists are folded into maps in list order, so a repeated name keeps the last value"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [http, stdio]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "duplicate_header_and_env_names_last_wins", steps)

      assert thread_config(transcript)["mcp_servers"] == %{
               "remote_tools" => %{
                 "url" => "https://mcp.example.test/mcp",
                 "http_headers" => %{"Authorization" => "Bearer new", "X-Team" => "platform"}
               },
               "local_tools" => %{
                 "command" => "/usr/local/bin/tools-mcp",
                 "args" => ["--stdio", "--verbose"],
                 "env" => %{"TOOLS_TOKEN" => "rotated"}
               }
             }
    end

    test "http_and_stdio_servers_share_one_mcp_servers_map" do
      steps =
        opened(authorizing_all()) ++
          [
            session_new(%{
              "cwd" => @cwd,
              "model" => "gpt-5",
              "mcpServers" => [@http_server, @stdio_server]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "http_and_stdio_servers_share_one_mcp_servers_map",
          steps
        )

      assert [%{"method" => "thread/start", "params" => %{"model" => "gpt-5"}}] =
               Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

      assert Map.keys(thread_config(transcript)["mcp_servers"]) == ["local_tools", "remote_tools"]
    end

    test "whitespace_only_server_name_replies_invalid" do
      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "Validation requires a non-blank name, so the adapter's mcp_server fallback name is unreachable from the wire"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [%{@http_server | "name" => "   "}]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "whitespace_only_server_name_replies_invalid", steps)

      assert last_error(transcript) == "Invalid HTTP MCP server configuration"
    end

    test "sse_server_replies_unsupported_error" do
      server = %{@http_server | "type" => "sse", "name" => "events"}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript = CodexGolden.assert_golden(@area, "sse_server_replies_unsupported_error", steps)

      assert last_error(transcript) == "Codex doesn't support MCP SSE transport protocol"
      assert length(CodexGolden.writes(transcript)) == 3
    end

    test "acp_server_replies_unsupported_error" do
      server = %{"type" => "acp", "name" => "agent", "command" => "/usr/local/bin/agent"}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript = CodexGolden.assert_golden(@area, "acp_server_replies_unsupported_error", steps)

      assert last_error(transcript) == "Codex doesn't support MCP ACP transport protocol"
    end

    test "untyped_server_without_command_is_unsupported_transport" do
      server = %{"name" => "mystery", "url" => "https://mcp.example.test/mcp"}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "untyped_server_without_command_is_unsupported_transport",
          steps
        )

      assert last_error(transcript) == "Unsupported MCP server transport"
    end

    test "unknown_server_type_falls_back_on_command_presence" do
      test_pid = self()

      callback = fn server, context ->
        send(test_pid, {:mcp_authorize, server, context})
        true
      end

      streamable = %{
        "type" => "streamable-http",
        "name" => "Remote Streams",
        "url" => "https://mcp.example.test/mcp",
        "headers" => []
      }

      websocket = %{
        "type" => "websocket",
        "name" => "ws-bridge",
        "command" => "/usr/bin/ws-mcp",
        "args" => [],
        "env" => []
      }

      steps =
        opened(authorize_mcp_server: callback) ++
          [
            {:note,
             "A type the adapter does not know is handled like a missing type: without a command it is an unsupported transport (not an invalid server), with a command the type is silently rewritten to stdio before authorization and the server is emitted as a stdio server"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [streamable]}),
            session_new(%{"cwd" => @cwd, "mcpServers" => [websocket]}, 11)
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "unknown_server_type_falls_back_on_command_presence",
          steps
        )

      assert [{10, %{error: "Unsupported MCP server transport"}}, {11, %{tag: :ok}}] =
               outbound_results(transcript, "session/new")

      assert_receive {:mcp_authorize, %{"type" => "stdio", "name" => "ws-bridge"},
                      %{cwd: @cwd, transport: "stdio", adapter: Codex}}

      assert thread_config(transcript)["mcp_servers"] == %{
               "ws-bridge" => %{"command" => "/usr/bin/ws-mcp"}
             }
    end

    test "non_map_server_entry_replies_invalid_server" do
      steps =
        opened(authorizing_all()) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => ["remote tools"]})]

      transcript =
        CodexGolden.assert_golden(@area, "non_map_server_entry_replies_invalid_server", steps)

      assert last_error(transcript) == "Invalid MCP server"
    end

    test "mcp_servers_not_a_list_replies_error" do
      steps =
        opened(authorizing_all()) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => %{"remote_tools" => @http_server}})]

      transcript = CodexGolden.assert_golden(@area, "mcp_servers_not_a_list_replies_error", steps)

      assert last_error(transcript) == "mcpServers must be a list"
    end

    test "duplicate_sanitized_names_reply_error" do
      steps =
        opened(authorizing_all()) ++
          [
            {:note, "\"remote tools\" and \"remote_tools\" both sanitize to remote_tools"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [@http_server, %{@stdio_server | "name" => "remote_tools"}]
            })
          ]

      transcript =
        CodexGolden.assert_golden(@area, "duplicate_sanitized_names_reply_error", steps)

      assert last_error(transcript) == "MCP server names must be unique"
    end

    test "http_server_missing_url_replies_invalid" do
      server = Map.delete(@http_server, "url")

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(@area, "http_server_missing_url_replies_invalid", steps)

      assert last_error(transcript) == "Invalid HTTP MCP server configuration"
    end

    test "http_server_non_http_scheme_replies_invalid" do
      server = %{@http_server | "url" => "ftp://mcp.example.test/mcp"}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(@area, "http_server_non_http_scheme_replies_invalid", steps)

      assert last_error(transcript) == "Invalid HTTP MCP server configuration"
    end

    test "http_server_empty_host_replies_invalid" do
      server = %{@http_server | "url" => "https:///mcp"}

      steps =
        opened(authorizing_all()) ++
          [
            {:note, "The scheme is fine but the URL has no host"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "http_server_empty_host_replies_invalid", steps)

      assert last_error(transcript) == "Invalid HTTP MCP server configuration"
    end

    test "http_server_url_without_authority_replies_invalid" do
      server = %{@http_server | "url" => "https:mcp"}

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "A URL with a scheme but no authority parses to a nil host, which is rejected like the empty host"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "http_server_url_without_authority_replies_invalid",
          steps
        )

      assert last_error(transcript) == "Invalid HTTP MCP server configuration"
      assert thread_write(transcript) == nil
    end

    test "http_server_missing_headers_replies_invalid" do
      server = Map.delete(@http_server, "headers")

      steps =
        opened(authorizing_all()) ++
          [
            {:note, "headers is mandatory: an absent list is rejected, an empty list is fine"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "http_server_missing_headers_replies_invalid", steps)

      assert last_error(transcript) == "Invalid HTTP MCP server configuration"
    end

    test "http_server_malformed_header_entry_replies_invalid" do
      server = %{@http_server | "headers" => [%{"Authorization" => "Bearer token"}]}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "http_server_malformed_header_entry_replies_invalid",
          steps
        )

      assert last_error(transcript) == "Invalid HTTP MCP server configuration"
    end

    test "name_value_entry_non_string_value_replies_invalid" do
      http = %{@http_server | "headers" => [%{"name" => "X-Retry", "value" => 3}]}
      stdio = %{@stdio_server | "env" => [%{"name" => "TOOLS_DEBUG", "value" => true}]}

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "Header and env entries need a string value as well as a string name; a number or boolean value fails the transport's own validation message"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [http]}),
            session_new(%{"cwd" => @cwd, "mcpServers" => [stdio]}, 11)
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "name_value_entry_non_string_value_replies_invalid",
          steps
        )

      assert [
               {10, %{error: "Invalid HTTP MCP server configuration"}},
               {11, %{error: "Invalid stdio MCP server configuration"}}
             ] = outbound_results(transcript, "session/new")

      assert thread_write(transcript) == nil
    end

    test "stdio_server_blank_name_replies_invalid" do
      server = %{@stdio_server | "name" => ""}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(@area, "stdio_server_blank_name_replies_invalid", steps)

      assert last_error(transcript) == "Invalid stdio MCP server configuration"
    end

    test "stdio_server_relative_command_replies_invalid" do
      server = %{@stdio_server | "command" => "tools-mcp"}

      steps =
        opened(authorizing_all()) ++
          [
            {:note,
             "The command must be an absolute path; it is not checked against the workspace roots"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "stdio_server_relative_command_replies_invalid", steps)

      assert last_error(transcript) == "Invalid stdio MCP server configuration"
    end

    test "stdio_server_missing_args_replies_invalid" do
      server = Map.delete(@stdio_server, "args")

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(@area, "stdio_server_missing_args_replies_invalid", steps)

      assert last_error(transcript) == "Invalid stdio MCP server configuration"
    end

    test "stdio_server_non_string_arg_replies_invalid" do
      server = %{@stdio_server | "args" => ["--stdio", 1]}

      steps =
        opened(authorizing_all()) ++
          [
            {:note, "Every args entry must be a string"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "stdio_server_non_string_arg_replies_invalid", steps)

      assert last_error(transcript) == "Invalid stdio MCP server configuration"
    end

    test "stdio_server_env_not_a_list_replies_invalid" do
      server = %{@stdio_server | "env" => %{"TOOLS_TOKEN" => "secret"}}

      steps =
        opened(authorizing_all()) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [server]})]

      transcript =
        CodexGolden.assert_golden(@area, "stdio_server_env_not_a_list_replies_invalid", steps)

      assert last_error(transcript) == "Invalid stdio MCP server configuration"
    end

    test "empty_mcp_servers_list_omits_config" do
      steps = opened() ++ [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript = CodexGolden.assert_golden(@area, "empty_mcp_servers_list_omits_config", steps)

      assert %{"method" => "thread/start", "params" => params} = thread_write(transcript)
      refute Map.has_key?(params, "config")
    end

    test "absent_mcp_servers_omits_config" do
      steps = opened() ++ [session_new(%{"cwd" => @cwd})]

      transcript = CodexGolden.assert_golden(@area, "absent_mcp_servers_omits_config", steps)

      assert %{"method" => "thread/start", "params" => params} = thread_write(transcript)
      refute Map.has_key?(params, "config")
    end

    test "validation_runs_before_authorization" do
      server = %{@http_server | "url" => "not a url"}

      steps =
        opened() ++
          [
            {:note,
             "No authorization is configured, yet the malformed server fails validation first"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [server]})
          ]

      transcript = CodexGolden.assert_golden(@area, "validation_runs_before_authorization", steps)

      assert last_error(transcript) == "Invalid HTTP MCP server configuration"
    end

    test "second_server_error_rejects_whole_list" do
      sse = %{@http_server | "type" => "sse", "name" => "events"}

      steps =
        opened(authorizing_all()) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@stdio_server, sse]})]

      transcript =
        CodexGolden.assert_golden(@area, "second_server_error_rejects_whole_list", steps)

      assert last_error(transcript) == "Codex doesn't support MCP SSE transport protocol"
      assert thread_write(transcript) == nil
    end
  end

  # -- MCP server authorization ---------------------------------------------

  describe "MCP server authorization" do
    test "authorize_callback_true_accepts_server" do
      test_pid = self()

      callback = fn server, context ->
        send(test_pid, {:mcp_authorize, server, context})
        true
      end

      steps =
        opened(authorize_mcp_server: callback) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})]

      transcript =
        CodexGolden.assert_golden(@area, "authorize_callback_true_accepts_server", steps)

      assert_receive {:mcp_authorize, @http_server,
                      %{cwd: @cwd, transport: "http", adapter: Codex}}

      assert %{"remote_tools" => %{"url" => "https://mcp.example.test/mcp"}} =
               thread_config(transcript)["mcp_servers"]
    end

    test "authorize_callback_receives_injected_stdio_type_for_untyped_server" do
      test_pid = self()

      callback = fn server, context ->
        send(test_pid, {:mcp_authorize, server, context})
        true
      end

      steps =
        opened(authorize_mcp_server: callback) ++
          [
            {:note,
             "An untyped server with a command is rewritten to type stdio before the callback sees it"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [Map.delete(@stdio_server, "type")]})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "authorize_callback_receives_injected_stdio_type_for_untyped_server",
          steps
        )

      assert_receive {:mcp_authorize, @stdio_server,
                      %{cwd: @cwd, transport: "stdio", adapter: Codex}}

      assert %{"local_tools" => %{"command" => "/usr/local/bin/tools-mcp"}} =
               thread_config(transcript)["mcp_servers"]
    end

    test "authorize_callback_false_rejects_server" do
      steps =
        opened(authorize_mcp_server: fn _server, _context -> false end) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})]

      transcript =
        CodexGolden.assert_golden(@area, "authorize_callback_false_rejects_server", steps)

      assert last_error(transcript) == "MCP server is not authorized"
      assert thread_write(transcript) == nil
    end

    test "authorize_callback_raise_rejects_server" do
      steps =
        opened(authorize_mcp_server: fn _server, _context -> raise "policy backend down" end) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@stdio_server]})]

      {transcript, log} =
        with_log(fn ->
          CodexGolden.assert_golden(@area, "authorize_callback_raise_rejects_server", steps)
        end)

      assert last_error(transcript) == "MCP server is not authorized"
      assert log =~ "Codex authorization callback failed"
    end

    test "authorize_callback_throw_rejects_server" do
      steps =
        opened(authorize_mcp_server: fn _server, _context -> throw(:policy_unavailable) end) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})]

      transcript =
        CodexGolden.assert_golden(@area, "authorize_callback_throw_rejects_server", steps)

      assert last_error(transcript) == "MCP server is not authorized"
      assert thread_write(transcript) == nil
    end

    test "authorize_callback_arity_one_raise_rejects_server" do
      steps =
        opened(authorize_mcp_server: fn _server -> raise "policy backend down" end) ++
          [
            {:note,
             "The arity-1 callback path rescues like the arity-2 one: the exception becomes a denial and the same warning is logged"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [@stdio_server]})
          ]

      {transcript, log} =
        with_log(fn ->
          CodexGolden.assert_golden(
            @area,
            "authorize_callback_arity_one_raise_rejects_server",
            steps
          )
        end)

      assert last_error(transcript) == "MCP server is not authorized"
      assert log =~ "Codex authorization callback failed"
    end

    test "authorize_callback_arity_one_throw_rejects_server" do
      steps =
        opened(authorize_mcp_server: fn _server -> throw(:policy_unavailable) end) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "authorize_callback_arity_one_throw_rejects_server",
          steps
        )

      assert last_error(transcript) == "MCP server is not authorized"
      assert thread_write(transcript) == nil
    end

    test "authorize_callback_arity_one_ok_tuple_accepts_server" do
      steps =
        opened(authorize_mcp_server: fn server -> {:ok, server["name"]} end) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@stdio_server]})]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "authorize_callback_arity_one_ok_tuple_accepts_server",
          steps
        )

      assert %{"local_tools" => _} = thread_config(transcript)["mcp_servers"]
    end

    test "authorize_callback_non_boolean_result_rejects_server" do
      steps =
        opened(authorize_mcp_server: fn _server, _context -> "approved" end) ++
          [
            {:note, "Only :ok, true and {:ok, _} authorize; any other value is a denial"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "authorize_callback_non_boolean_result_rejects_server",
          steps
        )

      assert last_error(transcript) == "MCP server is not authorized"
    end

    test "authorize_callback_not_a_function_rejects_server" do
      steps =
        opened(authorize_mcp_server: :allow_everything) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "authorize_callback_not_a_function_rejects_server",
          steps
        )

      assert last_error(transcript) == "MCP server is not authorized"
    end

    test "no_callback_and_no_trusted_list_rejects_server" do
      steps = opened() ++ [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})]

      transcript =
        CodexGolden.assert_golden(@area, "no_callback_and_no_trusted_list_rejects_server", steps)

      assert last_error(transcript) == "MCP server is not authorized"
    end

    test "trusted_mcp_servers_exact_map_accepts_server" do
      steps =
        opened(trusted_mcp_servers: [@http_server]) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})]

      transcript =
        CodexGolden.assert_golden(@area, "trusted_mcp_servers_exact_map_accepts_server", steps)

      assert %{"remote_tools" => %{"http_headers" => %{"Authorization" => "Bearer token"}}} =
               thread_config(transcript)["mcp_servers"]
    end

    test "untyped_server_exact_untyped_trusted_map_is_rejected" do
      untyped = Map.delete(@stdio_server, "type")

      steps =
        opened(trusted_mcp_servers: [untyped]) ++
          [
            {:note,
             "The wire server gains \"type\" => \"stdio\" before the trust check, so a trusted entry mirroring the untyped wire shape no longer matches"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [untyped]})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "untyped_server_exact_untyped_trusted_map_is_rejected",
          steps
        )

      assert last_error(transcript) == "MCP server is not authorized"
      assert thread_write(transcript) == nil
    end

    test "untyped_server_matches_typed_stdio_trusted_map" do
      steps =
        opened(trusted_mcp_servers: [@stdio_server]) ++
          [
            {:note, "The typed stdio trusted entry equals the rewritten untyped wire server"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [Map.delete(@stdio_server, "type")]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "untyped_server_matches_typed_stdio_trusted_map", steps)

      assert %{
               "local_tools" => %{
                 "command" => "/usr/local/bin/tools-mcp",
                 "args" => ["--stdio", "--verbose"]
               }
             } = thread_config(transcript)["mcp_servers"]
    end

    test "trusted_mcp_servers_mismatched_headers_rejects_server" do
      steps =
        opened(trusted_mcp_servers: [%{@http_server | "headers" => []}]) ++
          [
            {:note, "Trust is exact map equality: extra caller-supplied headers break the match"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "trusted_mcp_servers_mismatched_headers_rejects_server",
          steps
        )

      assert last_error(transcript) == "MCP server is not authorized"
    end

    test "trusted_mcp_servers_name_only_rejects_server" do
      steps =
        opened(trusted_mcp_servers: ["remote tools", "local tools"]) ++
          [
            {:note, "A trusted name never authorizes caller-controlled connection details"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]}),
            session_new(%{"cwd" => @cwd, "mcpServers" => [Map.delete(@stdio_server, "type")]}, 11)
          ]

      transcript =
        CodexGolden.assert_golden(@area, "trusted_mcp_servers_name_only_rejects_server", steps)

      assert [%{error: "MCP server is not authorized"}, %{error: "MCP server is not authorized"}] =
               transcript |> Enum.take(-2) |> Enum.map(& &1.result)
    end

    test "trusted_mcp_servers_map_value_rejects_server" do
      steps =
        opened(trusted_mcp_servers: %{"remote_tools" => @http_server}) ++
          [
            {:note,
             "Only a list of exact server maps or :all grants trust; a map keyed by name is not a trusted list and denies every server"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "trusted_mcp_servers_map_value_rejects_server", steps)

      assert last_error(transcript) == "MCP server is not authorized"
      assert thread_write(transcript) == nil
    end

    test "trusted_mcp_servers_string_all_rejects_server" do
      steps =
        opened(trusted_mcp_servers: "all") ++
          [
            {:note,
             "Only the atom :all grants blanket trust; the string \"all\" is neither a list nor :all and denies every server"},
            session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server]})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "trusted_mcp_servers_string_all_rejects_server", steps)

      assert last_error(transcript) == "MCP server is not authorized"
      assert thread_write(transcript) == nil
    end

    test "trusted_mcp_servers_all_accepts_any_server" do
      steps =
        opened(trusted_mcp_servers: :all) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => [@http_server, @stdio_server]})]

      transcript =
        CodexGolden.assert_golden(@area, "trusted_mcp_servers_all_accepts_any_server", steps)

      assert Map.keys(thread_config(transcript)["mcp_servers"]) == ["local_tools", "remote_tools"]
    end
  end

  # -- workspace containment: cwd ------------------------------------------

  describe "cwd containment" do
    test "cwd_missing_replies_absolute_error" do
      steps =
        opened() ++
          [
            {:note, "No cwd in params and no :cwd init option"},
            session_new(%{"mcpServers" => []})
          ]

      transcript = CodexGolden.assert_golden(@area, "cwd_missing_replies_absolute_error", steps)

      assert last_error(transcript) == "Workspace paths must be absolute"
    end

    test "cwd_relative_replies_absolute_error" do
      steps = opened() ++ [session_new(%{"cwd" => "project", "mcpServers" => []})]

      transcript = CodexGolden.assert_golden(@area, "cwd_relative_replies_absolute_error", steps)

      assert last_error(transcript) == "Workspace paths must be absolute"
    end

    test "cwd_path_traversal_escapes_roots" do
      steps =
        opened() ++
          [
            {:note, "/tmp/project/../../etc expands to /etc, outside the /tmp root"},
            session_new(%{"cwd" => "/tmp/project/../../etc", "mcpServers" => []})
          ]

      transcript = CodexGolden.assert_golden(@area, "cwd_path_traversal_escapes_roots", steps)

      assert last_error(transcript) == "Workspace path is not authorized"
    end

    test "cwd_equal_to_root_is_authorized" do
      steps = opened() ++ [session_new(%{"cwd" => "/tmp", "mcpServers" => []})]

      transcript = CodexGolden.assert_golden(@area, "cwd_equal_to_root_is_authorized", steps)

      assert %{"method" => "thread/start", "params" => %{"cwd" => "/tmp"}} =
               thread_write(transcript)
    end

    test "cwd_under_second_root_is_authorized" do
      steps =
        opened(workspace_roots: ["/srv/other", "/tmp"]) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript = CodexGolden.assert_golden(@area, "cwd_under_second_root_is_authorized", steps)

      assert %{"params" => %{"cwd" => @cwd}} = thread_write(transcript)
    end

    test "workspace_roots_single_string_root_is_accepted" do
      steps =
        opened(workspace_roots: "/tmp") ++ [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript =
        CodexGolden.assert_golden(@area, "workspace_roots_single_string_root_is_accepted", steps)

      assert %{"params" => %{"cwd" => @cwd}} = thread_write(transcript)
    end

    test "workspace_roots_relative_root_is_ignored" do
      steps =
        opened(workspace_roots: ["tmp", "/srv/other"]) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript =
        CodexGolden.assert_golden(@area, "workspace_roots_relative_root_is_ignored", steps)

      assert last_error(transcript) == "Workspace path is not authorized"
    end

    test "workspace_roots_empty_list_rejects_every_cwd" do
      steps =
        opened(workspace_roots: [], cwd: @cwd) ++
          [
            {:note,
             "An empty list is an empty root set: the :cwd option still supplies the default cwd but never becomes a fallback root, so even the option's own directory is rejected"},
            session_new(%{"cwd" => @cwd, "mcpServers" => []}),
            session_new(%{"mcpServers" => []}, 11),
            session_new(%{"cwd" => "/tmp", "mcpServers" => []}, 12)
          ]

      transcript =
        CodexGolden.assert_golden(@area, "workspace_roots_empty_list_rejects_every_cwd", steps)

      assert [
               {10, %{error: "Workspace path is not authorized"}},
               {11, %{error: "Workspace path is not authorized"}},
               {12, %{error: "Workspace path is not authorized"}}
             ] = outbound_results(transcript, "session/new")

      assert thread_write(transcript) == nil
    end

    test "workspace_roots_relative_root_is_ignored_even_when_it_expands_inside" do
      relative_root = String.duplicate("../", 40) <> "tmp"

      steps =
        opened(workspace_roots: [relative_root]) ++
          [
            {:note,
             "A relative root is skipped before any path comparison; it is never expanded against the process cwd, although expanding this one would yield /tmp"},
            session_new(%{"cwd" => @cwd, "mcpServers" => []})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "workspace_roots_relative_root_is_ignored_even_when_it_expands_inside",
          steps
        )

      # Documents the precondition the note relies on, not the fixture: the
      # 40-level `../` prefix climbs to `/` from any test process cwd at most
      # 40 directories deep, so this fails loudly (never silently) elsewhere.
      assert Path.expand(relative_root) == "/tmp"
      assert last_error(transcript) == "Workspace path is not authorized"
    end

    test "workspace_root_prefix_sibling_is_rejected" do
      steps =
        opened(workspace_roots: [@cwd]) ++
          [
            {:note,
             "Containment is per path component: /tmp/project-other and /tmp/projectx share the /tmp/project string prefix but are siblings of the root, not inside it; only /tmp/project/sub is"},
            session_new(%{"cwd" => "/tmp/project-other", "mcpServers" => []}),
            session_new(
              %{"cwd" => @cwd, "mcpServers" => [], "additionalDirectories" => ["/tmp/projectx"]},
              11
            ),
            session_new(%{"cwd" => "/tmp/project/sub", "mcpServers" => []}, 12)
          ]

      transcript =
        CodexGolden.assert_golden(@area, "workspace_root_prefix_sibling_is_rejected", steps)

      assert [
               {10, %{error: "Workspace path is not authorized"}},
               {11, %{error: "Workspace path is not authorized"}},
               {12, %{tag: :ok}}
             ] = outbound_results(transcript, "session/new")

      assert [%{"params" => %{"cwd" => "/tmp/project/sub"}}] =
               Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))
    end

    test "cwd_authorization_precedes_mcp_validation" do
      malformed = %{@http_server | "url" => "not a url"}

      steps =
        opened() ++
          [
            {:note,
             "session_config checks cwd, then the additionalDirectories shape, then their authorization, and only then mcpServers: the same malformed, unauthorized server list is carried by every request but is reported only once the workspace checks pass. Requests 14 and 15 pair the bad cwd with a malformed additionalDirectories value, so the cwd error is reported before the shape error; request 16 puts an unauthorized absolute directory ahead of a relative one, so the shape error of the whole list is reported before any directory is authorized"},
            session_new(%{"cwd" => "/srv/elsewhere", "mcpServers" => [malformed]}),
            session_new(
              %{
                "cwd" => @cwd,
                "mcpServers" => [malformed],
                "additionalDirectories" => ["vendor"]
              },
              11
            ),
            session_new(
              %{
                "cwd" => @cwd,
                "mcpServers" => [malformed],
                "additionalDirectories" => ["/srv/elsewhere"]
              },
              12
            ),
            session_new(
              %{
                "cwd" => @cwd,
                "mcpServers" => [malformed],
                "additionalDirectories" => ["/tmp/vendor"]
              },
              13
            ),
            session_new(
              %{
                "cwd" => "/srv/elsewhere",
                "mcpServers" => [malformed],
                "additionalDirectories" => ["vendor"]
              },
              14
            ),
            session_new(
              %{
                "cwd" => "/srv/elsewhere",
                "mcpServers" => [malformed],
                "additionalDirectories" => "vendor"
              },
              15
            ),
            session_new(
              %{
                "cwd" => @cwd,
                "mcpServers" => [malformed],
                "additionalDirectories" => ["/srv/elsewhere", "vendor"]
              },
              16
            )
          ]

      transcript =
        CodexGolden.assert_golden(@area, "cwd_authorization_precedes_mcp_validation", steps)

      assert [
               {10, %{error: "Workspace path is not authorized"}},
               {11, %{error: "additionalDirectories entries must be absolute paths"}},
               {12, %{error: "Workspace path is not authorized"}},
               {13, %{error: "Invalid HTTP MCP server configuration"}},
               {14, %{error: "Workspace path is not authorized"}},
               {15, %{error: "Workspace path is not authorized"}},
               {16, %{error: "additionalDirectories entries must be absolute paths"}}
             ] = outbound_results(transcript, "session/new")

      assert thread_write(transcript) == nil
    end

    test "cwd_option_is_default_workspace_and_root" do
      steps =
        [{:init, [cwd: @cwd]} | handshake_steps()] ++
          [
            {:note,
             "Without workspace_roots the :cwd option is both the default cwd and the only root"},
            session_new(%{"mcpServers" => []}),
            session_new(%{"cwd" => "/tmp/project/sub", "mcpServers" => []}, 11),
            session_new(%{"cwd" => "/tmp/other", "mcpServers" => []}, 12)
          ]

      transcript =
        CodexGolden.assert_golden(@area, "cwd_option_is_default_workspace_and_root", steps)

      assert [%{"params" => %{"cwd" => @cwd}}, %{"params" => %{"cwd" => "/tmp/project/sub"}}] =
               Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

      assert last_error(transcript) == "Workspace path is not authorized"
    end

    test "authorize_workspace_callback_true_bypasses_roots" do
      test_pid = self()

      callback = fn path, context ->
        send(test_pid, {:workspace_authorize, path, context})
        true
      end

      steps =
        opened(authorize_workspace: callback) ++
          [session_new(%{"cwd" => "/srv/elsewhere", "mcpServers" => []})]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "authorize_workspace_callback_true_bypasses_roots",
          steps
        )

      assert_receive {:workspace_authorize, "/srv/elsewhere", %{kind: :cwd, adapter: Codex}}
      assert %{"params" => %{"cwd" => "/srv/elsewhere"}} = thread_write(transcript)
    end

    test "authorize_workspace_callback_false_rejects_cwd" do
      steps =
        opened(authorize_workspace: fn _path -> false end) ++
          [
            {:note, "An arity-1 callback denies even a cwd inside the configured roots"},
            session_new(%{"cwd" => @cwd, "mcpServers" => []})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "authorize_workspace_callback_false_rejects_cwd", steps)

      assert last_error(transcript) == "Workspace path is not authorized"
    end

    test "authorize_workspace_callback_throw_rejects_cwd" do
      steps =
        opened(authorize_workspace: fn _path, _context -> throw(:policy_unavailable) end) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript =
        CodexGolden.assert_golden(@area, "authorize_workspace_callback_throw_rejects_cwd", steps)

      assert last_error(transcript) == "Workspace path is not authorized"
    end

    test "authorize_workspace_arity_one_raise_rejects_cwd" do
      steps =
        opened(authorize_workspace: fn _path -> raise "policy backend down" end) ++
          [
            {:note,
             "The arity-1 workspace callback shares the rescue with the MCP one: an exception is a denial and logs the same warning"},
            session_new(%{"cwd" => @cwd, "mcpServers" => []})
          ]

      {transcript, log} =
        with_log(fn ->
          CodexGolden.assert_golden(
            @area,
            "authorize_workspace_arity_one_raise_rejects_cwd",
            steps
          )
        end)

      assert last_error(transcript) == "Workspace path is not authorized"
      assert log =~ "Codex authorization callback failed"
    end

    test "authorize_workspace_arity_one_throw_rejects_cwd" do
      steps =
        opened(authorize_workspace: fn _path -> throw(:policy_unavailable) end) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript =
        CodexGolden.assert_golden(@area, "authorize_workspace_arity_one_throw_rejects_cwd", steps)

      assert last_error(transcript) == "Workspace path is not authorized"
      assert thread_write(transcript) == nil
    end

    test "authorize_workspace_not_a_function_replies_invalid_callback" do
      steps =
        opened(authorize_workspace: true) ++ [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "authorize_workspace_not_a_function_replies_invalid_callback",
          steps
        )

      assert last_error(transcript) == "Invalid workspace authorization callback"
    end
  end

  # -- workspace containment: additionalDirectories -------------------------

  describe "additionalDirectories" do
    test "additional_directories_extend_sandbox_projects_and_turn" do
      steps =
        opened(trust_authorized_workspaces: true) ++
          [
            {:note,
             "Entries are trimmed and deduplicated; one equal to cwd is dropped. The surviving directories reach config.sandbox_workspace_write, config.projects, and the next turn/start"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor", @cwd, " /tmp/vendor ", "/tmp/shared"]
            }),
            thread_start_reply(),
            prompt("thread-abc", "list the vendored packages")
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "additional_directories_extend_sandbox_projects_and_turn",
          steps
        )

      assert thread_config(transcript) == %{
               "projects" => %{
                 "/tmp/project" => %{"trust_level" => "trusted"},
                 "/tmp/shared" => %{"trust_level" => "trusted"},
                 "/tmp/vendor" => %{"trust_level" => "trusted"}
               },
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor", "/tmp/shared"]}
             }

      assert %{"method" => "turn/start", "params" => %{"sandboxPolicy" => sandbox_policy}} =
               thread_write(transcript, "turn/start")

      assert sandbox_policy["writableRoots"] == ["/tmp/vendor", "/tmp/shared"]
    end

    test "meta_additional_roots_fallback_is_used" do
      steps =
        opened() ++
          [
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "_meta" => %{"additionalRoots" => ["/tmp/vendor"]}
            })
          ]

      transcript =
        CodexGolden.assert_golden(@area, "meta_additional_roots_fallback_is_used", steps)

      assert thread_config(transcript) == %{
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor"]}
             }
    end

    test "additional_directories_only_cwd_emits_no_sandbox_section" do
      steps =
        opened() ++
          [
            {:note,
             "Entries equal to the cwd (after trimming) are dropped, so a list made only of them collapses to nothing: no sandbox_workspace_write section and, with nothing else configured, no config key at all"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => [@cwd, " /tmp/project "]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "additional_directories_only_cwd_emits_no_sandbox_section",
          steps
        )

      assert %{"method" => "thread/start", "params" => %{"cwd" => @cwd} = params} =
               thread_write(transcript)

      refute Map.has_key?(params, "config")
    end

    test "meta_additional_roots_errors_use_additional_directories_messages" do
      steps =
        opened() ++
          [
            {:note,
             "The _meta.additionalRoots alias goes through the same shape checks and reports them with the additionalDirectories wording"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "_meta" => %{"additionalRoots" => "/tmp/vendor"}
            }),
            session_new(
              %{"cwd" => @cwd, "mcpServers" => [], "_meta" => %{"additionalRoots" => ["vendor"]}},
              11
            ),
            session_new(
              %{"cwd" => @cwd, "mcpServers" => [], "_meta" => %{"additionalRoots" => ["  "]}},
              12
            ),
            session_new(
              %{
                "cwd" => @cwd,
                "mcpServers" => [],
                "_meta" => %{"additionalRoots" => [%{"path" => "/tmp/vendor"}]}
              },
              13
            )
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "meta_additional_roots_errors_use_additional_directories_messages",
          steps
        )

      assert [
               {10, %{error: "additionalDirectories must be a list of absolute paths"}},
               {11, %{error: "additionalDirectories entries must be absolute paths"}},
               {12, %{error: "additionalDirectories entries must not be empty"}},
               {13, %{error: "additionalDirectories entries must be strings"}}
             ] = outbound_results(transcript, "session/new")

      assert thread_write(transcript) == nil
    end

    test "repeated_additional_directory_is_authorized_once" do
      test_pid = self()

      callback = fn path, context ->
        send(test_pid, {:workspace_authorize, path, context})
        true
      end

      steps =
        opened(authorize_workspace: callback, trust_authorized_workspaces: true) ++
          [
            {:note,
             "Repeated entries are collapsed before authorization: the callback sees /tmp/vendor once, and the single surviving directory reaches config and turn/start"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor", "/tmp/vendor", " /tmp/vendor "]
            }),
            thread_start_reply(),
            prompt("thread-abc", "list the vendored packages")
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "repeated_additional_directory_is_authorized_once",
          steps
        )

      assert_receive {:workspace_authorize, @cwd, %{kind: :cwd}}
      assert_receive {:workspace_authorize, "/tmp/vendor", %{kind: {:additional_directory, @cwd}}}
      refute_received {:workspace_authorize, "/tmp/vendor", _}

      assert thread_config(transcript) == %{
               "projects" => %{
                 "/tmp/project" => %{"trust_level" => "trusted"},
                 "/tmp/vendor" => %{"trust_level" => "trusted"}
               },
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor"]}
             }

      assert %{"params" => %{"sandboxPolicy" => %{"writableRoots" => ["/tmp/vendor"]}}} =
               thread_write(transcript, "turn/start")
    end

    test "additional_directories_take_precedence_over_meta_additional_roots" do
      steps =
        opened() ++
          [
            {:note,
             "additionalDirectories wins over _meta.additionalRoots whenever it is present, even as an empty list; the surviving directories are what the next turn/start carries"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor"],
              "_meta" => %{"additionalRoots" => ["/tmp/shared"]}
            }),
            thread_start_reply(),
            prompt("thread-abc", "list the vendored packages"),
            session_new(
              %{
                "cwd" => @cwd,
                "mcpServers" => [],
                "additionalDirectories" => [],
                "_meta" => %{"additionalRoots" => ["/tmp/shared"]}
              },
              11
            )
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "additional_directories_take_precedence_over_meta_additional_roots",
          steps
        )

      assert [%{"params" => %{"config" => config}}, %{"params" => second_params}] =
               Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

      assert config == %{"sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor"]}}
      refute Map.has_key?(second_params, "config")

      assert %{"params" => %{"sandboxPolicy" => %{"writableRoots" => ["/tmp/vendor"]}}} =
               thread_write(transcript, "turn/start")
    end

    test "additional_directories_not_a_list_replies_error" do
      steps =
        opened() ++
          [
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => "/tmp/vendor"
            })
          ]

      transcript =
        CodexGolden.assert_golden(@area, "additional_directories_not_a_list_replies_error", steps)

      assert last_error(transcript) == "additionalDirectories must be a list of absolute paths"
    end

    test "additional_directories_empty_entry_replies_error" do
      steps =
        opened() ++
          [
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor", "  "]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "additional_directories_empty_entry_replies_error",
          steps
        )

      assert last_error(transcript) == "additionalDirectories entries must not be empty"
    end

    test "additional_directories_relative_entry_replies_error" do
      steps =
        opened() ++
          [
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["vendor"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "additional_directories_relative_entry_replies_error",
          steps
        )

      assert last_error(transcript) == "additionalDirectories entries must be absolute paths"
    end

    test "additional_directories_non_string_entry_replies_error" do
      steps =
        opened() ++
          [
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => [%{"path" => "/tmp/vendor"}]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "additional_directories_non_string_entry_replies_error",
          steps
        )

      assert last_error(transcript) == "additionalDirectories entries must be strings"
    end

    test "additional_directories_outside_roots_replies_unauthorized" do
      test_pid = self()

      steps =
        opened(
          authorize_workspace: fn path, context ->
            send(test_pid, {:workspace_authorize, path, context})
            String.starts_with?(path, "/tmp/")
          end
        ) ++
          [
            {:note,
             "Directories are authorized one by one with kind {:additional_directory, cwd}; the first failure wins"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor", "/srv/elsewhere", "/tmp/shared"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "additional_directories_outside_roots_replies_unauthorized",
          steps
        )

      assert last_error(transcript) == "Workspace path is not authorized"
      assert_receive {:workspace_authorize, @cwd, %{kind: :cwd}}
      assert_receive {:workspace_authorize, "/tmp/vendor", %{kind: {:additional_directory, @cwd}}}

      assert_receive {:workspace_authorize, "/srv/elsewhere",
                      %{kind: {:additional_directory, @cwd}}}

      refute_received {:workspace_authorize, "/tmp/shared", _}
    end

    test "additional_directories_outside_configured_roots_replies_unauthorized" do
      steps =
        opened() ++
          [
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/srv/elsewhere"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "additional_directories_outside_configured_roots_replies_unauthorized",
          steps
        )

      assert last_error(transcript) == "Workspace path is not authorized"
    end
  end

  # -- session/load and session/resume --------------------------------------

  describe "thread/resume config" do
    test "session_load_carries_config_and_defaults_model_provider" do
      steps =
        opened(authorizing_all() ++ [trust_authorized_workspaces: true]) ++
          [
            session_load(%{
              "sessionId" => "thread-abc",
              "cwd" => @cwd,
              "mcpServers" => [@http_server],
              "additionalDirectories" => ["/tmp/vendor"]
            }),
            thread_resume_reply()
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_load_carries_config_and_defaults_model_provider",
          steps
        )

      assert %{
               "method" => "thread/resume",
               "params" => %{
                 "threadId" => "thread-abc",
                 "modelProvider" => "openai",
                 "initialTurnsPage" => %{"limit" => 100, "itemsView" => "full"},
                 "config" => %{"mcp_servers" => %{"remote_tools" => _}}
               }
             } = thread_write(transcript, "thread/resume")

      assert [
               %{"method" => "session/update", "params" => %{"update" => replayed}},
               %{"id" => 20, "result" => %{"sessionId" => "thread-abc"}}
             ] = CodexGolden.messages(transcript)

      assert replayed["sessionUpdate"] == "agent_message_chunk"
    end

    test "session_resume_carries_config_with_exclude_turns" do
      steps =
        opened(authorizing_all()) ++
          [
            session_resume(%{
              "sessionId" => "thread-abc",
              "cwd" => @cwd,
              "mcpServers" => [@stdio_server]
            }),
            thread_resume_reply()
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_resume_carries_config_with_exclude_turns",
          steps
        )

      assert %{
               "method" => "thread/resume",
               "params" => %{
                 "excludeTurns" => true,
                 "modelProvider" => "openai",
                 "config" => %{"mcp_servers" => %{"local_tools" => _}}
               }
             } = thread_write(transcript, "thread/resume")
    end

    test "session_load_additional_directories_reach_turn_start" do
      steps =
        opened() ++
          [
            session_load(%{
              "sessionId" => "thread-abc",
              "cwd" => @cwd,
              "additionalDirectories" => ["/tmp/vendor"]
            }),
            thread_resume_reply(),
            prompt("thread-abc", "list the vendored packages")
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_load_additional_directories_reach_turn_start",
          steps
        )

      assert thread_config(transcript, "thread/resume") == %{
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor"]}
             }

      assert %{"params" => %{"sandboxPolicy" => %{"writableRoots" => ["/tmp/vendor"]}}} =
               thread_write(transcript, "turn/start")
    end

    test "session_resume_additional_directories_reach_turn_start" do
      steps =
        opened() ++
          [
            session_resume(%{
              "sessionId" => "thread-abc",
              "cwd" => @cwd,
              "additionalDirectories" => ["/tmp/vendor", "/tmp/shared"]
            }),
            thread_resume_reply(),
            prompt("thread-abc", "list the shared packages")
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_resume_additional_directories_reach_turn_start",
          steps
        )

      assert %{"params" => %{"excludeTurns" => true, "config" => config}} =
               thread_write(transcript, "thread/resume")

      assert config == %{
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor", "/tmp/shared"]}
             }

      assert %{
               "params" => %{
                 "sandboxPolicy" => %{"writableRoots" => ["/tmp/vendor", "/tmp/shared"]}
               }
             } =
               thread_write(transcript, "turn/start")
    end

    test "session_load_and_resume_meta_additional_roots_fallback" do
      steps =
        opened() ++
          [
            {:note,
             "Both resume-style handlers read _meta.additionalRoots when additionalDirectories is absent, exactly like session/new"},
            session_load(%{
              "sessionId" => "thread-abc",
              "cwd" => @cwd,
              "_meta" => %{"additionalRoots" => ["/tmp/vendor"]}
            }),
            thread_resume_reply(),
            session_resume(%{
              "sessionId" => "thread-def",
              "cwd" => @cwd,
              "_meta" => %{"additionalRoots" => ["/tmp/shared"]}
            }),
            thread_resume_reply(4, "thread-def")
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_load_and_resume_meta_additional_roots_fallback",
          steps
        )

      assert [
               %{
                 "params" => %{
                   "threadId" => "thread-abc",
                   "config" => %{
                     "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor"]}
                   }
                 }
               },
               %{
                 "params" => %{
                   "threadId" => "thread-def",
                   "excludeTurns" => true,
                   "config" => %{
                     "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/shared"]}
                   }
                 }
               }
             ] = Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/resume"))

      assert [{20, %{tag: :ok}}] = outbound_results(transcript, "session/load")
      assert [{21, %{tag: :ok}}] = outbound_results(transcript, "session/resume")
    end

    test "session_load_and_resume_default_cwd_from_option" do
      steps =
        [{:init, [cwd: @cwd]} | handshake_steps()] ++
          [
            {:note,
             "Without a cwd param both handlers fall back to the :cwd option, which is also their only workspace root"},
            session_load(%{"sessionId" => "thread-abc"}),
            session_resume(%{"sessionId" => "thread-def"}),
            session_load(%{"sessionId" => "thread-abc", "cwd" => "/tmp/other"}, 22)
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_load_and_resume_default_cwd_from_option",
          steps
        )

      assert [
               %{
                 "params" => %{"threadId" => "thread-abc", "cwd" => @cwd, "initialTurnsPage" => _}
               },
               %{"params" => %{"threadId" => "thread-def", "cwd" => @cwd, "excludeTurns" => true}}
             ] = Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/resume"))

      assert [{20, %{tag: :ok}}, {22, %{error: "Workspace path is not authorized"}}] =
               outbound_results(transcript, "session/load")
    end

    test "model_provider_option_reaches_thread_resume" do
      steps =
        opened(model_provider: "azure") ++
          [
            {:note, "The configured provider replaces the \"openai\" resume default"},
            session_load(%{"sessionId" => "thread-abc", "cwd" => @cwd}),
            session_resume(%{"sessionId" => "thread-abc", "cwd" => @cwd})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "model_provider_option_reaches_thread_resume", steps)

      assert [
               %{"params" => %{"modelProvider" => "azure"} = load_params},
               %{
                 "params" => %{"modelProvider" => "azure", "excludeTurns" => true} = resume_params
               }
             ] = Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/resume"))

      refute Map.has_key?(load_params, "config")
      refute Map.has_key?(resume_params, "config")
    end

    test "gateway_provider_reaches_thread_resume" do
      steps =
        opened(gateway_auth: true) ++
          [
            {:note,
             "A gateway login sets both the thread/resume modelProvider and the matching config.model_providers entry"},
            gateway_authenticate(),
            session_resume(%{"sessionId" => "thread-abc", "cwd" => @cwd}),
            session_load(%{"sessionId" => "thread-abc", "cwd" => @cwd})
          ]

      transcript =
        CodexGolden.assert_golden(@area, "gateway_provider_reaches_thread_resume", steps)

      assert [
               %{"params" => %{"modelProvider" => "custom-gateway", "config" => resume_config}},
               %{"params" => %{"modelProvider" => "custom-gateway", "config" => load_config}}
             ] = Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/resume"))

      assert resume_config == load_config

      assert %{
               "model_providers" => %{
                 "custom-gateway" => %{
                   "name" => "Example Gateway",
                   "base_url" => "https://gateway.example.test/v1",
                   "wire_api" => "responses"
                 }
               }
             } = resume_config
    end

    test "session_load_and_resume_callbacks_receive_context" do
      test_pid = self()

      steps =
        opened(
          authorize_workspace: fn path, context ->
            send(test_pid, {:workspace_authorize, path, context})
            true
          end,
          authorize_mcp_server: fn server, context ->
            send(test_pid, {:mcp_authorize, server, context})
            true
          end
        ) ++
          [
            {:note,
             "session/load and session/resume run the same workspace and MCP callbacks as session/new, with the request cwd in every context; the callbacks bypass the configured roots"},
            session_load(%{
              "sessionId" => "thread-abc",
              "cwd" => "/srv/load",
              "mcpServers" => [@http_server],
              "additionalDirectories" => ["/srv/load/vendor"]
            }),
            session_resume(%{
              "sessionId" => "thread-abc",
              "cwd" => "/srv/resume",
              "mcpServers" => [@stdio_server],
              "additionalDirectories" => ["/srv/resume/vendor"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_load_and_resume_callbacks_receive_context",
          steps
        )

      assert_receive {:workspace_authorize, "/srv/load", %{kind: :cwd, adapter: Codex}}

      assert_receive {:workspace_authorize, "/srv/load/vendor",
                      %{kind: {:additional_directory, "/srv/load"}, adapter: Codex}}

      assert_receive {:mcp_authorize, @http_server,
                      %{cwd: "/srv/load", transport: "http", adapter: Codex}}

      assert_receive {:workspace_authorize, "/srv/resume", %{kind: :cwd, adapter: Codex}}

      assert_receive {:workspace_authorize, "/srv/resume/vendor",
                      %{kind: {:additional_directory, "/srv/resume"}, adapter: Codex}}

      assert_receive {:mcp_authorize, @stdio_server,
                      %{cwd: "/srv/resume", transport: "stdio", adapter: Codex}}

      assert [
               %{
                 "params" => %{
                   "cwd" => "/srv/load",
                   "config" => %{
                     "mcp_servers" => %{"remote_tools" => _},
                     "sandbox_workspace_write" => %{"writable_roots" => ["/srv/load/vendor"]}
                   }
                 }
               },
               %{
                 "params" => %{
                   "cwd" => "/srv/resume",
                   "config" => %{
                     "mcp_servers" => %{"local_tools" => _},
                     "sandbox_workspace_write" => %{"writable_roots" => ["/srv/resume/vendor"]}
                   }
                 }
               }
             ] = Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/resume"))
    end

    test "session_load_cwd_outside_roots_replies_unauthorized" do
      steps =
        opened() ++
          [session_load(%{"sessionId" => "thread-abc", "cwd" => "/srv/elsewhere"})]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_load_cwd_outside_roots_replies_unauthorized",
          steps
        )

      assert last_error(transcript) == "Workspace path is not authorized"
    end

    test "session_load_missing_session_id_is_checked_first" do
      steps =
        opened() ++
          [
            {:note, "sessionId validation precedes the workspace check"},
            session_load(%{"cwd" => "/srv/elsewhere"}),
            session_resume(%{"sessionId" => "", "cwd" => "/srv/elsewhere"})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "session_load_missing_session_id_is_checked_first",
          steps
        )

      assert [%{error: "sessionId is required"}, %{error: "sessionId is required"}] =
               transcript |> Enum.take(-2) |> Enum.map(& &1.result)
    end
  end

  # -- config layers ----------------------------------------------------------

  describe "config layers" do
    test "trust_authorized_workspaces_marks_cwd_trusted" do
      steps =
        opened(trust_authorized_workspaces: true) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript =
        CodexGolden.assert_golden(@area, "trust_authorized_workspaces_marks_cwd_trusted", steps)

      assert thread_config(transcript) == %{
               "projects" => %{"/tmp/project" => %{"trust_level" => "trusted"}}
             }
    end

    test "trust_authorized_workspaces_false_omits_projects" do
      steps =
        opened(trust_authorized_workspaces: false) ++
          [
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "trust_authorized_workspaces_false_omits_projects",
          steps
        )

      assert thread_config(transcript) == %{
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor"]}
             }
    end

    test "trust_authorized_workspaces_overrides_preconfigured_project_entry" do
      codex_config = %{
        "projects" => %{
          "/tmp/project" => %{"trust_level" => "untrusted"},
          "/tmp/vendor" => %{"trust_level" => "untrusted"},
          "/tmp/legacy" => %{"trust_level" => "untrusted"}
        }
      }

      steps =
        opened(codex_config: codex_config, trust_authorized_workspaces: true) ++
          [
            {:note,
             "The session cwd and additional directories overwrite a preconfigured projects entry for the same path; unrelated entries keep their own trust_level"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "trust_authorized_workspaces_overrides_preconfigured_project_entry",
          steps
        )

      assert thread_config(transcript) == %{
               "projects" => %{
                 "/tmp/legacy" => %{"trust_level" => "untrusted"},
                 "/tmp/project" => %{"trust_level" => "trusted"},
                 "/tmp/vendor" => %{"trust_level" => "trusted"}
               },
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor"]}
             }
    end

    test "codex_config_keyword_list_is_ignored" do
      steps =
        opened(
          codex_config: [model_reasoning_summary: "detailed"],
          trust_authorized_workspaces: true
        ) ++
          [
            {:note,
             "Only a map or a JSON object string counts as a codex_config; any other term is treated as no preconfigured config"},
            session_new(%{"cwd" => @cwd, "mcpServers" => []})
          ]

      transcript = CodexGolden.assert_golden(@area, "codex_config_keyword_list_is_ignored", steps)

      assert thread_config(transcript) == %{
               "projects" => %{"/tmp/project" => %{"trust_level" => "trusted"}}
             }
    end

    test "model_provider_option_sets_thread_model_provider" do
      steps =
        opened(model_provider: "azure") ++
          [
            {:note, "modelProvider is a thread/start param, not part of config"},
            session_new(%{"cwd" => @cwd, "mcpServers" => []})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "model_provider_option_sets_thread_model_provider",
          steps
        )

      assert %{"method" => "thread/start", "params" => %{"modelProvider" => "azure"} = params} =
               thread_write(transcript)

      refute Map.has_key?(params, "config")
    end

    test "codex_config_map_is_base_layer_merged_with_session" do
      codex_config = %{
        "model_reasoning_summary" => "detailed",
        "projects" => %{"/tmp/legacy" => %{"trust_level" => "untrusted"}},
        "sandbox_workspace_write" => %{
          "writable_roots" => ["/tmp/cache", "/tmp/vendor"],
          "network_access" => true
        },
        "mcp_servers" => %{"preconfigured" => %{"command" => "/usr/local/bin/pre-mcp"}}
      }

      steps =
        opened(
          authorizing_all() ++ [codex_config: codex_config, trust_authorized_workspaces: true]
        ) ++
          [
            {:note,
             "projects and writable_roots are merged (writable_roots deduplicated); session mcpServers replace the whole preconfigured mcp_servers map"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [@http_server],
              "additionalDirectories" => ["/tmp/vendor", "/tmp/shared"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "codex_config_map_is_base_layer_merged_with_session",
          steps
        )

      config = thread_config(transcript)
      assert config["model_reasoning_summary"] == "detailed"

      assert Map.keys(config["projects"]) == [
               "/tmp/legacy",
               "/tmp/project",
               "/tmp/shared",
               "/tmp/vendor"
             ]

      assert config["sandbox_workspace_write"] == %{
               "network_access" => true,
               "writable_roots" => ["/tmp/cache", "/tmp/vendor", "/tmp/shared"]
             }

      assert Map.keys(config["mcp_servers"]) == ["remote_tools"]
    end

    test "preconfigured_mcp_servers_survive_without_session_servers" do
      codex_config = %{
        "mcp_servers" => %{"preconfigured" => %{"command" => "/usr/local/bin/pre-mcp"}}
      }

      steps =
        opened(codex_config: codex_config) ++
          [
            {:note,
             "Absent and empty mcpServers both leave the preconfigured mcp_servers map in place"},
            session_new(%{"cwd" => @cwd}),
            session_new(%{"cwd" => @cwd, "mcpServers" => []}, 11)
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "preconfigured_mcp_servers_survive_without_session_servers",
          steps
        )

      assert [%{"params" => %{"config" => config}}, %{"params" => %{"config" => config}}] =
               Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

      assert config == codex_config
    end

    test "writable_roots_string_is_wrapped_into_list" do
      codex_config = %{"sandbox_workspace_write" => %{"writable_roots" => "/tmp/cache"}}

      steps =
        opened(codex_config: codex_config) ++
          [
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(@area, "writable_roots_string_is_wrapped_into_list", steps)

      assert thread_config(transcript) == %{
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/cache", "/tmp/vendor"]}
             }
    end

    test "writable_roots_non_string_entries_are_dropped_only_when_merged" do
      codex_config = %{
        "sandbox_workspace_write" => %{
          "writable_roots" => ["/tmp/cache", 42, nil, %{"path" => "/tmp/other"}]
        }
      }

      steps =
        opened(codex_config: codex_config) ++
          [
            {:note,
             "Non-string entries are filtered out when additionalDirectories are merged in; without any, the section passes through untouched"},
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor"]
            }),
            session_new(%{"cwd" => @cwd, "mcpServers" => []}, 11)
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "writable_roots_non_string_entries_are_dropped_only_when_merged",
          steps
        )

      assert [%{"params" => %{"config" => merged}}, %{"params" => %{"config" => untouched}}] =
               Enum.filter(CodexGolden.writes(transcript), &(&1["method"] == "thread/start"))

      assert merged == %{
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/cache", "/tmp/vendor"]}
             }

      assert untouched == codex_config
    end

    test "codex_config_json_string_is_decoded" do
      steps =
        opened(
          codex_config: ~s({"model_reasoning_summary":"concise","tui":{"notifications":true}})
        ) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript = CodexGolden.assert_golden(@area, "codex_config_json_string_is_decoded", steps)

      assert thread_config(transcript) == %{
               "model_reasoning_summary" => "concise",
               "tui" => %{"notifications" => true}
             }
    end

    test "codex_config_invalid_json_is_ignored" do
      steps =
        opened(codex_config: ~s({"model_reasoning_summary": ), trust_authorized_workspaces: true) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript = CodexGolden.assert_golden(@area, "codex_config_invalid_json_is_ignored", steps)

      assert thread_config(transcript) == %{
               "projects" => %{"/tmp/project" => %{"trust_level" => "trusted"}}
             }
    end

    test "codex_config_json_array_is_ignored" do
      steps =
        opened(codex_config: ~s(["not","an","object"])) ++
          [session_new(%{"cwd" => @cwd, "mcpServers" => []})]

      transcript = CodexGolden.assert_golden(@area, "codex_config_json_array_is_ignored", steps)

      refute Map.has_key?(thread_write(transcript)["params"], "config")
    end

    test "codex_config_malformed_sections_are_replaced" do
      codex_config = %{
        "projects" => "trusted",
        "sandbox_workspace_write" => ["/tmp/cache"],
        "model_providers" => "openai"
      }

      steps =
        opened(codex_config: codex_config, trust_authorized_workspaces: true, gateway_auth: true) ++
          [
            {:note,
             "A gateway login stores a custom model provider; non-map projects/sandbox/model_providers sections from codex_config are replaced rather than merged"},
            gateway_authenticate(),
            session_new(%{
              "cwd" => @cwd,
              "mcpServers" => [],
              "additionalDirectories" => ["/tmp/vendor"]
            })
          ]

      transcript =
        CodexGolden.assert_golden(@area, "codex_config_malformed_sections_are_replaced", steps)

      assert %{"params" => %{"modelProvider" => "custom-gateway"}} = thread_write(transcript)

      assert thread_config(transcript) == %{
               "model_providers" => %{
                 "custom-gateway" => %{
                   "name" => "Example Gateway",
                   "base_url" => "https://gateway.example.test/v1",
                   "http_headers" => %{"X-Client-Feature-ID" => "codex", "X-Team" => "platform"},
                   "wire_api" => "responses"
                 }
               },
               "projects" => %{
                 "/tmp/project" => %{"trust_level" => "trusted"},
                 "/tmp/vendor" => %{"trust_level" => "trusted"}
               },
               "sandbox_workspace_write" => %{"writable_roots" => ["/tmp/vendor"]}
             }
    end

    test "gateway_config_merges_into_preconfigured_model_providers" do
      other = %{
        "name" => "Other Provider",
        "base_url" => "https://other.example.test/v1",
        "wire_api" => "chat"
      }

      steps =
        opened(codex_config: %{"model_providers" => %{"other" => other}}, gateway_auth: true) ++
          [
            {:note,
             "The gateway provider is added next to the preconfigured providers rather than replacing them"},
            gateway_authenticate(),
            session_new(%{"cwd" => @cwd, "mcpServers" => []})
          ]

      transcript =
        CodexGolden.assert_golden(
          @area,
          "gateway_config_merges_into_preconfigured_model_providers",
          steps
        )

      assert %{"params" => %{"modelProvider" => "custom-gateway"}} = thread_write(transcript)

      assert %{
               "custom-gateway" => %{"base_url" => "https://gateway.example.test/v1"},
               "other" => ^other
             } =
               thread_config(transcript)["model_providers"]

      assert Map.keys(thread_config(transcript)["model_providers"]) == ["custom-gateway", "other"]
    end
  end

  # -- step helpers ------------------------------------------------------------

  # Init with only the workspace root configured, plus `overrides`, followed
  # by the app-server handshake. Authorization of MCP servers is deliberately
  # not configured here so each scenario states its own policy.
  defp opened(overrides \\ []) do
    [{:init, Keyword.merge([workspace_roots: ["/tmp"]], overrides)} | handshake_steps()]
  end

  defp authorizing_all, do: [authorize_mcp_server: fn _server, _context -> true end]

  defp handshake_steps do
    [
      :post_connect,
      {:inbound, %{"id" => 1, "result" => %{"capabilities" => %{}}}},
      {:inbound, %{"id" => 2, "result" => %{"data" => catalog_models(), "nextCursor" => nil}}}
    ]
  end

  defp session_new(params, id \\ 10) do
    {:outbound, %{"method" => "session/new", "id" => id, "params" => params}}
  end

  defp session_load(params, id \\ 20) do
    {:outbound, %{"method" => "session/load", "id" => id, "params" => params}}
  end

  defp session_resume(params, id \\ 21) do
    {:outbound, %{"method" => "session/resume", "id" => id, "params" => params}}
  end

  # A gateway login that stores the "custom-gateway" model provider.
  defp gateway_authenticate(id \\ 5) do
    {:outbound,
     %{
       "method" => "authenticate",
       "id" => id,
       "params" => %{
         "methodId" => "gateway",
         "_meta" => %{
           "gateway" => %{
             "baseUrl" => "https://gateway.example.test/v1",
             "providerName" => "Example Gateway",
             "headers" => %{"X-Team" => "platform"}
           }
         }
       }
     }}
  end

  defp prompt(session_id, text, id \\ 30) do
    {:outbound,
     %{
       "method" => "session/prompt",
       "id" => id,
       "params" => %{
         "sessionId" => session_id,
         "prompt" => [%{"type" => "text", "text" => text}]
       }
     }}
  end

  # The handshake consumes native ids 1 and 2, so the first session request
  # is id 3.
  defp thread_start_reply(native_id \\ 3, thread_id \\ "thread-abc") do
    {:inbound,
     %{
       "id" => native_id,
       "result" => %{
         "model" => "gpt-5",
         "thread" => %{"id" => thread_id, "cwd" => @cwd, "updatedAt" => 1_700_000_000}
       }
     }}
  end

  defp thread_resume_reply(native_id \\ 3, thread_id \\ "thread-abc") do
    {:inbound,
     %{
       "id" => native_id,
       "result" => %{
         "model" => "gpt-5",
         "thread" => %{
           "id" => thread_id,
           "cwd" => @cwd,
           "updatedAt" => 1_700_000_000,
           "turns" => [
             %{
               "id" => "turn-0",
               "items" => [%{"type" => "agent_message", "text" => "Earlier reply"}]
             }
           ]
         }
       }
     }}
  end

  # Same v2 `model/list` shape as the lifecycle golden test.
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

  # -- transcript helpers ------------------------------------------------------

  defp thread_write(transcript, method \\ "thread/start") do
    transcript |> CodexGolden.writes() |> Enum.find(&(&1["method"] == method))
  end

  defp thread_config(transcript, method \\ "thread/start") do
    get_in(thread_write(transcript, method), ["params", "config"])
  end

  defp last_error(transcript) do
    assert %{tag: :error, error: error} = CodexGolden.last_result(transcript)
    error
  end

  # `{acp_id, result}` for every outbound step with `method`, in transcript
  # order, so multi-request scenarios anchor each assertion on the request
  # that produced the reply rather than on the transcript tail.
  defp outbound_results(transcript, method) do
    for %{step: %{kind: :outbound, message: %{"method" => ^method, "id" => id}}, result: result} <-
          transcript,
        do: {id, result}
  end
end
