defmodule ExMCP.ACP.Adapters.ClaudeSDK.MCPConfigGoldenTest do
  @moduledoc """
  Characterization gate for the Claude SDK adapter's process configuration
  and authorization (`docs/POST_1_0_MAINTENANCE_PLAN.md`, "Claude adapter
  characterization gate": MCP server normalization, native config output,
  and authorization failures).

  Each test drives `ExMCP.ACP.Adapters.ClaudeSDK` through
  `ExMCP.Test.ClaudeGolden` and compares the recorded transcript against a
  committed fixture under `test/fixtures/acp/claude/mcp_config/`. The
  fixtures pin:

    * the `--mcp-config` payload `command/1` builds - one entry per stdio,
      HTTP and SSE server, atom keys stringified through
      `ExMCP.ACP.Maps.stringify_keys/1` - and the absence of the flag for
      an empty or missing server map, plus `--strict-mcp-config`;
    * the rest of the Claude Code command line in its exact order: the
      stream-json flags, thinking (`--thinking`, `--max-thinking-tokens`,
      and `0` meaning disabled), effort and budget limits, model and
      fallback model, agent, the stdio permission prompt tool,
      allow/deny/`--tools` CSV rules (including `tools: []` meaning "no
      built-in tools" while an empty allow list means "no opinion"),
      permission mode (with `bypassPermissions` pulling in
      `--allow-dangerously-skip-permissions`), partial messages,
      `--add-dir` repetition, resume/fork/session flags and `extra_args`;
    * `cli_path` resolution and the constant SDK entrypoint `env/1`;
    * the static `capabilities/0` map, including the MCP transports the
      adapter advertises;
    * `auth_methods/1,2`: the empty list before `initialize`, the terminal
      login methods a `auth.terminal` client capability unlocks, the
      `_meta.terminal-auth` block a `_meta["terminal-auth"]` capability
      adds, and the gateway methods that need both the client capability
      and the adapter's `gateway_auth` option;
    * `authenticate` for each supported method id, its error for an
      unsupported one and for a request without `methodId`, and `logout`
      with the CLI call disabled, succeeding, and failing (the exit status
      and trimmed output are part of the error).

  The remote-login branch of `auth_methods/2` (`NO_BROWSER`, `SSH_*`,
  `CLAUDE_CODE_REMOTE`) is not characterized: it reads the OS environment
  directly, and setting those variables would race every other async test.
  The harness fails an `:auth_methods` step when one of them is set.

  Mutation check (2026-09-21): in `claude_sdk/protocol.ex`, making
  `append_csv/4` treat `tools: []` like every other empty list (dropping
  the `[] when key == :tools` clause) fails
  `command_with_an_empty_tools_list_disables_built_ins`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `CLAUDE_GOLDEN=update mix test <this file>[:line]`; that run rewrites
  the fixture and fails on purpose, so review the diff and re-run without
  the variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.ClaudeGolden
  alias ExMCP.Test.ClaudeGolden.Flows

  @area "mcp_config"

  describe "mcp servers" do
    test "command_without_mcp_servers_has_no_config_flag" do
      transcript = command("command_without_mcp_servers_has_no_config_flag", [])

      refute "--mcp-config" in args(transcript)
    end

    test "command_with_an_empty_server_map_has_no_config_flag" do
      command("command_with_an_empty_server_map_has_no_config_flag", mcp_servers: %{})
    end

    test "command_with_every_mcp_transport" do
      transcript =
        command("command_with_every_mcp_transport",
          mcp_servers: %{
            "docs" => %{
              "type" => "stdio",
              "command" => "docs-mcp",
              "args" => ["--stdio"],
              "env" => %{"TOKEN" => "abc"}
            },
            "api" => %{
              "type" => "http",
              "url" => "https://mcp.example.test/",
              "headers" => %{"Authorization" => "Bearer x"}
            },
            "events" => %{"type" => "sse", "url" => "https://mcp.example.test/sse"}
          }
        )

      assert "--mcp-config" in args(transcript)
    end

    test "command_stringifies_atom_keyed_servers" do
      transcript =
        command("command_stringifies_atom_keyed_servers",
          mcp_servers: %{docs: %{command: "docs-mcp", args: ["--stdio"]}}
        )

      assert Enum.any?(args(transcript), &String.contains?(&1, ~s("docs")))
    end

    test "command_with_strict_mcp_config" do
      command("command_with_strict_mcp_config",
        mcp_servers: %{"docs" => %{"command" => "docs-mcp"}},
        strict_mcp_config: true
      )
    end
  end

  describe "command line" do
    test "command_default_flags" do
      transcript = command("command_default_flags", [])

      assert Enum.take(args(transcript), 5) == [
               "--output-format",
               "stream-json",
               "--verbose",
               "--input-format",
               "stream-json"
             ]
    end

    test "command_default_cli_path_is_claude" do
      transcript = command("command_default_cli_path_is_claude", cli_path: nil)

      assert %{reply: %{executable: "claude"}} = ClaudeGolden.last_result(transcript)
    end

    test "command_with_thinking_levels" do
      steps = [
        {:command, thinking: :disabled},
        {:command, thinking: :adaptive},
        {:command, thinking: "disabled"},
        {:command, thinking: "adaptive"},
        {:command, thinking: 12_000}
      ]

      ClaudeGolden.assert_golden(@area, "command_with_thinking_levels", steps)
    end

    test "command_with_max_thinking_tokens" do
      steps = [
        {:note, "A zero budget is the documented way to disable thinking"},
        {:command, max_thinking_tokens: 0},
        {:command, max_thinking_tokens: 8_000}
      ]

      ClaudeGolden.assert_golden(@area, "command_with_max_thinking_tokens", steps)
    end

    test "command_with_limits_and_models" do
      command("command_with_limits_and_models",
        effort: "high",
        max_turns: 12,
        max_budget_usd: "2.50",
        model: "opus",
        fallback_model: "sonnet",
        agent: "reviewer"
      )
    end

    test "command_without_the_permission_prompt_tool" do
      transcript =
        command("command_without_the_permission_prompt_tool", permission_prompt: false)

      refute "--permission-prompt-tool" in args(transcript)
    end

    test "command_with_tool_rules" do
      command("command_with_tool_rules",
        allowed_tools: ["Read", "Bash"],
        disallowed_tools: ["WebFetch"],
        tools: ["Read"]
      )
    end

    test "command_with_empty_tool_rules" do
      transcript =
        command("command_with_empty_tool_rules", allowed_tools: [], disallowed_tools: [])

      refute "--allowedTools" in args(transcript)
    end

    test "command_with_an_empty_tools_list_disables_built_ins" do
      transcript =
        command("command_with_an_empty_tools_list_disables_built_ins", tools: [])

      args = args(transcript)
      assert Enum.at(args, Enum.find_index(args, &(&1 == "--tools")) + 1) == ""
    end

    test "command_with_the_default_tools_keyword" do
      command("command_with_the_default_tools_keyword", tools: "default")
    end

    test "command_with_permission_modes" do
      steps = [
        {:command, permission_mode: :default},
        {:command, permission_mode: :accept_edits},
        {:command, permission_mode: :plan},
        {:command, permission_mode: :auto},
        {:command, permission_mode: :dont_ask},
        {:command, permission_mode: "custom"}
      ]

      ClaudeGolden.assert_golden(@area, "command_with_permission_modes", steps)
    end

    test "command_with_bypass_permission_mode_adds_the_dangerous_flag" do
      transcript =
        command("command_with_bypass_permission_mode_adds_the_dangerous_flag",
          permission_mode: :bypass
        )

      assert "--allow-dangerously-skip-permissions" in args(transcript)
    end

    test "command_without_a_permission_mode" do
      transcript = command("command_without_a_permission_mode", permission_mode: nil)

      refute "--permission-mode" in args(transcript)
    end

    test "command_without_partial_messages" do
      transcript = command("command_without_partial_messages", include_partial_messages: false)

      refute "--include-partial-messages" in args(transcript)
    end

    test "command_with_additional_directories" do
      command("command_with_additional_directories",
        additional_directories: ["/tmp/shared", nil, "/tmp/other"]
      )
    end

    test "command_with_a_single_additional_directory" do
      command("command_with_a_single_additional_directory", additional_directories: "/tmp/only")
    end

    test "command_with_session_flags" do
      command("command_with_session_flags",
        resume: "123e4567-e89b-12d3-a456-426614174000",
        resume_session_at: "msg-7",
        fork_session: true,
        session_id: "123e4567-e89b-12d3-a456-426614174001"
      )
    end

    test "command_with_extra_args" do
      command("command_with_extra_args", extra_args: ["--debug", :verbose, 7])
    end

    test "command_with_non_list_extra_args_is_ignored" do
      command("command_with_non_list_extra_args_is_ignored", extra_args: "--debug")
    end
  end

  describe "environment and capabilities" do
    test "env_is_the_sdk_entrypoint" do
      steps = [{:env, []}, {:env, model: "opus"}]

      transcript = ClaudeGolden.assert_golden(@area, "env_is_the_sdk_entrypoint", steps)

      assert %{reply: %{"CLAUDE_CODE_ENTRYPOINT" => "sdk-ts"}} =
               ClaudeGolden.last_result(transcript)
    end

    test "capabilities_advertise_the_mcp_transports" do
      transcript =
        ClaudeGolden.assert_golden(@area, "capabilities_advertise_the_mcp_transports", [
          :capabilities
        ])

      assert %{reply: %{"mcpCapabilities" => mcp}} = ClaudeGolden.last_result(transcript)
      assert mcp["acp"] == true
      assert mcp["http"] == true
      assert mcp["sse"] == true
    end
  end

  describe "auth methods" do
    test "auth_methods_are_empty_before_initialize" do
      steps = [{:auth_methods, []}, {:auth_methods_fresh, gateway_auth: true}]

      transcript =
        ClaudeGolden.assert_golden(@area, "auth_methods_are_empty_before_initialize", steps)

      assert %{reply: []} = ClaudeGolden.last_result(transcript)
    end

    test "terminal_capability_unlocks_the_login_methods" do
      steps = [
        Flows.initialize("acp-init", %{"auth" => %{"terminal" => true}}),
        {:auth_methods, []}
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "terminal_capability_unlocks_the_login_methods", steps)

      assert %{reply: methods} = ClaudeGolden.last_result(transcript)
      assert Enum.map(methods, & &1["id"]) == ["claude-ai-login", "console-login"]
      refute Enum.any?(methods, &Map.has_key?(&1, "_meta"))
    end

    test "terminal_auth_meta_adds_the_command_block" do
      steps = [
        Flows.initialize("acp-init", %{"_meta" => %{"terminal-auth" => true}}),
        {:auth_methods, []}
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "terminal_auth_meta_adds_the_command_block", steps)

      assert %{reply: methods} = ClaudeGolden.last_result(transcript)
      assert Enum.all?(methods, &Map.has_key?(&1, "_meta"))
    end

    test "gateway_methods_need_the_capability_and_the_option" do
      steps = [
        Flows.initialize("acp-init", %{
          "auth" => %{"terminal" => true, "_meta" => %{"gateway" => true}}
        }),
        {:note, "Without the adapter option the gateway methods are not offered"},
        {:auth_methods, []},
        {:auth_methods, gateway_auth: true}
      ]

      transcript =
        ClaudeGolden.assert_golden(
          @area,
          "gateway_methods_need_the_capability_and_the_option",
          steps
        )

      assert %{reply: methods} = ClaudeGolden.last_result(transcript)

      assert Enum.map(methods, & &1["id"]) == [
               "claude-ai-login",
               "console-login",
               "gateway",
               "gateway-bedrock"
             ]
    end

    test "gateway_capability_alone_offers_nothing_without_terminal_support" do
      steps = [
        Flows.initialize("acp-init", %{"auth" => %{"_meta" => %{"gateway" => true}}}),
        {:auth_methods, gateway_auth: true}
      ]

      ClaudeGolden.assert_golden(
        @area,
        "gateway_capability_alone_offers_nothing_without_terminal_support",
        steps
      )
    end
  end

  describe "authenticate and logout" do
    test "authenticate_accepts_every_terminal_method" do
      steps = [
        authenticate("acp-auth-1", "claude-login"),
        authenticate("acp-auth-2", "claude-ai-login"),
        authenticate("acp-auth-3", "console-login")
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "authenticate_accepts_every_terminal_method", steps)

      assert %{tag: :reply, reply: %{}} = ClaudeGolden.last_result(transcript)
    end

    test "authenticate_stores_the_gateway_configuration" do
      steps = [
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-auth",
           "method" => "authenticate",
           "params" => %{
             "methodId" => "gateway",
             "_meta" => %{"gateway" => %{"baseUrl" => "https://gateway.example.test"}}
           }
         }},
        authenticate("acp-auth-2", "gateway-bedrock")
      ]

      ClaudeGolden.assert_golden(@area, "authenticate_stores_the_gateway_configuration", steps)
    end

    test "authenticate_rejects_an_unsupported_method" do
      steps = [authenticate("acp-auth", "carrier-pigeon")]

      transcript =
        ClaudeGolden.assert_golden(@area, "authenticate_rejects_an_unsupported_method", steps)

      assert %{tag: :error, error: "Unsupported Claude auth method: carrier-pigeon"} =
               ClaudeGolden.last_result(transcript)
    end

    test "authenticate_without_a_method_id_errors" do
      steps = [
        {:outbound,
         %{
           "jsonrpc" => "2.0",
           "id" => "acp-auth",
           "method" => "authenticate",
           "params" => %{}
         }},
        {:outbound, %{"jsonrpc" => "2.0", "id" => "acp-auth-2", "method" => "authenticate"}}
      ]

      ClaudeGolden.assert_golden(@area, "authenticate_without_a_method_id_errors", steps)
    end

    test "logout_without_the_cli_call_just_replies" do
      steps = [{:init, logout_cli: false}, logout("acp-logout")]

      transcript =
        ClaudeGolden.assert_golden(@area, "logout_without_the_cli_call_just_replies", steps)

      assert %{tag: :reply, reply: %{}} = ClaudeGolden.last_result(transcript)
    end

    test "logout_runs_the_cli" do
      steps = [
        {:note, "The sandbox `claude` prints one line and exits 0"},
        logout("acp-logout")
      ]

      transcript = ClaudeGolden.assert_golden(@area, "logout_runs_the_cli", steps)

      assert %{tag: :reply, reply: %{}} = ClaudeGolden.last_result(transcript)
    end

    test "a_failing_logout_reports_the_status_and_output" do
      steps = [
        {:init, cli_path: "<sandbox>/bin/claude-failing"},
        logout("acp-logout")
      ]

      transcript =
        ClaudeGolden.assert_golden(@area, "a_failing_logout_reports_the_status_and_output", steps)

      assert %{tag: :error, error: error} = ClaudeGolden.last_result(transcript)
      assert error == "claude auth logout failed with status 3: not authenticated"
    end

    test "logout_clears_the_gateway_configuration" do
      steps = [
        {:init, logout_cli: false},
        authenticate("acp-auth", "gateway"),
        logout("acp-logout"),
        {:note, "The gateway credentials are gone; the methods are re-offered"},
        Flows.initialize("acp-init", %{
          "auth" => %{"terminal" => true, "_meta" => %{"gateway" => true}}
        }),
        {:auth_methods, gateway_auth: true}
      ]

      ClaudeGolden.assert_golden(@area, "logout_clears_the_gateway_configuration", steps)
    end
  end

  # -- helpers ---------------------------------------------------------------

  defp command(name, opts) do
    ClaudeGolden.assert_golden(@area, name, [{:command, opts}])
  end

  defp args(transcript) do
    %{reply: %{args: args}} = ClaudeGolden.last_result(transcript)
    args
  end

  defp authenticate(acp_id, method_id) do
    {:outbound,
     %{
       "jsonrpc" => "2.0",
       "id" => acp_id,
       "method" => "authenticate",
       "params" => %{"methodId" => method_id}
     }}
  end

  defp logout(acp_id) do
    {:outbound, %{"jsonrpc" => "2.0", "id" => acp_id, "method" => "logout", "params" => %{}}}
  end
end
