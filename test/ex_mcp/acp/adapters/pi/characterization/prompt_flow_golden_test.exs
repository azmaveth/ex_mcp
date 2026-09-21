defmodule ExMCP.ACP.Adapters.Pi.PromptFlowGoldenTest do
  @moduledoc """
  Characterization gate for the Pi ACP adapter's prompt scheduling (area P4
  of `docs/POST_1_0_MAINTENANCE_PLAN.md`, "Pi adapter restructuring" /
  "Characterization gate": prompt queue, steering, follow-up, cancellation,
  and subprocess-exit behavior).

  Each test drives `ExMCP.ACP.Adapters.Pi` through `ExMCP.Test.PiGolden` and
  compares the recorded transcript against a committed fixture under
  `test/fixtures/acp/pi/prompt_flow/`. The fixtures pin:

    * a prompt settling on `agent_settled` with `stopReason: "end_turn"`,
      the accumulated text and the last `agent_end` usage;
    * prompts arriving while one is active being queued with a
      `Queued message (position N).` notice and a `queueDepth` info update,
      the next queued prompt starting on settlement (with its own notice,
      `prompt` write and `msg-N` id) and queued slash commands being sent
      verbatim as prompt text rather than routed;
    * `session/cancel` writing `abort`, answering every queued prompt with
      `stopReason: "cancelled"`, emitting the `Cleared queued prompts.`
      notice only when something was queued, and settling the active prompt
      as `cancelled`; a cancel with nothing active still writes `abort` and
      the flag does not leak into the next prompt;
    * a failed `prompt` response answering the ACP request with an error
      and leaving queued prompts stranded until the next settlement, and a
      stray `agent_settled` with no pending prompt being skipped;
    * `/steering` and `/follow-up` slash commands (get, set, usage) and the
      `steering_mode` / `follow_up_mode` config options writing the
      matching `set_*_mode` messages, and a steering command issued while a
      prompt is active being queued like any other prompt;
    * subprocess exit (`exit_status` and `:closed`) failing the active
      prompt and every pending control group with `-32603 Pi process
      exited: <reason>` and cancelling queued prompts, in that order, with
      nothing emitted when nothing was pending; in managed mode the same
      through a real fake-`pi` exit, after which prompts fail with
      `:no_active_pi_session` until a new session respawns the process,
      and a buffered partial frame at exit being dropped;
    * `session/close` and `shutdown` discarding the active prompt and queue
      silently while the `msg-N` counter keeps counting, and unrelated
      adapter messages being skipped.

  Stream-event conversion is characterized by the stream_events area and
  slash-command expansion by the slash_commands area.

  Mutation check (2026-09-20): changing the settled `stopReason` for a
  cancel-requested prompt from `"cancelled"` to `"end_turn"` fails
  `cancel_with_active_prompt_settles_as_cancelled`.

  To regenerate a fixture after an intentional behavior change, run the test
  with `PI_GOLDEN=update mix test <this file>[:line]`; that run rewrites the
  fixture and fails on purpose, so review the diff and re-run without the
  variable to confirm.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Test.PiGolden
  alias ExMCP.Test.PiGolden.Flows

  @area "prompt_flow"

  defp responses(transcript) do
    transcript |> PiGolden.messages() |> Enum.filter(&Map.has_key?(&1, "id"))
  end

  defp chunk_texts(transcript) do
    transcript
    |> PiGolden.messages()
    |> Enum.map(&get_in(&1, ["params", "update", "content", "text"]))
    |> Enum.reject(&is_nil/1)
  end

  defp close_session(acp_id),
    do:
      {:outbound,
       %{"method" => "session/close", "id" => acp_id, "params" => %{"sessionId" => "pi-session"}}}

  describe "prompt queue" do
    test "prompt_settles_with_end_turn_text_and_usage" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "hello"),
            Flows.text_delta("Hi "),
            Flows.text_delta("there"),
            Flows.agent_end(),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(@area, "prompt_settles_with_end_turn_text_and_usage", steps)

      assert %{
               messages: [
                 %{
                   "id" => 2,
                   "result" => %{
                     "stopReason" => "end_turn",
                     "usage" => %{"inputTokens" => 10, "outputTokens" => 4},
                     "_meta" => %{
                       "ex_mcp" => %{"text" => "Hi there", "sessionId" => "pi-session"}
                     }
                   }
                 }
               ]
             } = PiGolden.last_result(transcript)
    end

    test "prompts_while_active_are_queued_and_started_in_order" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "first"),
            Flows.prompt(3, "second"),
            Flows.prompt(4, "third"),
            Flows.text_delta("one"),
            Flows.agent_settled(),
            Flows.text_delta("two"),
            Flows.agent_settled(),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "prompts_while_active_are_queued_and_started_in_order",
          steps
        )

      assert [
               "Queued message (position 1).",
               "Queued message (position 2).",
               "one",
               "Starting queued message. (1 remaining)",
               "two",
               "Starting queued message. (0 remaining)"
             ] = chunk_texts(transcript)

      assert [
               %{"id" => "msg-1", "message" => "first"},
               %{"id" => "msg-2", "message" => "second"},
               %{"id" => "msg-3", "message" => "third"}
             ] = transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "prompt"))

      assert [%{"id" => 1}, %{"id" => 2}, %{"id" => 3}, %{"id" => 4}] = responses(transcript)
    end

    test "queued_slash_command_is_sent_verbatim_when_started" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "first"),
            Flows.prompt(3, "/compact keep tests"),
            Flows.prompt(4, [
              %{"type" => "text", "text" => "look"},
              %{"type" => "image", "mimeType" => "image/png", "data" => "abc"}
            ]),
            Flows.agent_settled(),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(@area, "queued_slash_command_is_sent_verbatim_when_started", steps)

      assert [
               %{"id" => "msg-1", "message" => "first"},
               %{"id" => "msg-2", "message" => "/compact keep tests"},
               %{"id" => "msg-3", "message" => "look", "images" => [%{"mimeType" => "image/png"}]}
             ] = transcript |> PiGolden.writes() |> Enum.filter(&(&1["type"] == "prompt"))
    end

    test "failed_prompt_leaves_queued_prompts_stranded" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "first"),
            Flows.prompt(3, "second"),
            {:respond_error, "prompt", "model failed"},
            {:note, "No settlement follows a failed prompt, so the queue is not drained"},
            Flows.agent_settled(),
            Flows.prompt(4, "third"),
            Flows.agent_settled(),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(@area, "failed_prompt_leaves_queued_prompts_stranded", steps)

      assert [
               %{"id" => 2, "error" => %{"message" => "model failed"}},
               %{"id" => 4, "result" => %{"stopReason" => "end_turn"}},
               %{"id" => 3, "result" => %{"stopReason" => "end_turn"}}
             ] = responses(transcript) |> Enum.drop(1)
    end

    test "settlement_without_a_pending_prompt_is_skipped" do
      steps =
        Flows.open_session(1) ++
          [Flows.agent_settled(), Flows.agent_end(), Flows.agent_settled()]

      transcript =
        PiGolden.assert_golden(@area, "settlement_without_a_pending_prompt_is_skipped", steps)

      assert Enum.all?(Enum.take(transcript, -3), &match?(%{tag: :skip}, &1.result))
    end
  end

  describe "cancellation" do
    test "cancel_with_active_prompt_settles_as_cancelled" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "hello"),
            Flows.text_delta("partial"),
            Flows.cancel(),
            Flows.agent_end(),
            Flows.agent_settled(),
            {:note, "The cancel flag does not leak into the next prompt"},
            Flows.prompt(3, "again"),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(@area, "cancel_with_active_prompt_settles_as_cancelled", steps)

      assert %{tag: :messages_and_write, messages: [], writes: [%{"type" => "abort"}]} =
               Enum.at(transcript, 8).result

      assert [
               %{
                 "id" => 2,
                 "result" => %{
                   "stopReason" => "cancelled",
                   "_meta" => %{"ex_mcp" => %{"text" => "partial"}}
                 }
               },
               %{"id" => 3, "result" => %{"stopReason" => "end_turn"}}
             ] = responses(transcript) |> Enum.drop(1)
    end

    test "cancel_clears_the_queue_and_answers_queued_prompts" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "first"),
            Flows.prompt(3, "second"),
            Flows.prompt(4, "third"),
            Flows.cancel(),
            Flows.agent_settled(),
            Flows.cancel()
          ]

      transcript =
        PiGolden.assert_golden(@area, "cancel_clears_the_queue_and_answers_queued_prompts", steps)

      assert %{
               tag: :messages_and_write,
               messages: [
                 %{"id" => 3, "result" => %{"stopReason" => "cancelled"}},
                 %{"id" => 4, "result" => %{"stopReason" => "cancelled"}},
                 %{
                   "params" => %{
                     "update" => %{"content" => %{"text" => "Cleared queued prompts."}}
                   }
                 },
                 %{
                   "params" => %{
                     "update" => %{
                       "_meta" => %{
                         "ex_mcp" => %{"pi" => %{"queueDepth" => 0, "running" => true}}
                       }
                     }
                   }
                 }
               ],
               writes: [%{"type" => "abort"}]
             } = Enum.at(transcript, 9).result

      assert %{messages: [%{"id" => 2, "result" => %{"stopReason" => "cancelled"}}]} =
               Enum.at(transcript, 10).result

      assert %{tag: :messages_and_write, messages: [], writes: [%{"type" => "abort"}]} =
               PiGolden.last_result(transcript)
    end

    test "cancel_before_any_session_writes_abort_with_default_session" do
      steps = [
        Flows.cancel(%{}),
        Flows.prompt(1, "x", %{"sessionId" => nil}),
        Flows.prompt(2, "y", %{"sessionId" => nil}),
        Flows.cancel(%{})
      ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "cancel_before_any_session_writes_abort_with_default_session",
          steps
        )

      assert %{messages: [%{"id" => 2}, %{"params" => %{"sessionId" => "default"}}, _]} =
               PiGolden.last_result(transcript)
    end
  end

  describe "steering and follow-up" do
    test "steering_slash_command_get_set_and_usage" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "/steering"),
            {:respond, "get_state", %{"steeringMode" => "one-at-a-time"}},
            Flows.prompt(3, "/steering"),
            {:respond, "get_state", %{}},
            Flows.prompt(4, "/steering one-at-a-time"),
            {:respond, "set_steering_mode", %{}},
            Flows.prompt(5, "/steering all extra"),
            {:respond, "set_steering_mode", %{}},
            Flows.prompt(6, "/steering sometimes")
          ]

      transcript =
        PiGolden.assert_golden(@area, "steering_slash_command_get_set_and_usage", steps)

      assert [
               "Steering mode: one-at-a-time",
               "Steering mode: unknown",
               "Steering mode set to: one-at-a-time",
               "Steering mode set to: all",
               "Usage: /steering all | /steering one-at-a-time"
             ] = chunk_texts(transcript)

      assert [%{"mode" => "one-at-a-time"}, %{"mode" => "all"}] =
               transcript
               |> PiGolden.writes()
               |> Enum.filter(&(&1["type"] == "set_steering_mode"))
    end

    test "follow_up_slash_command_get_set_and_usage" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "/follow-up"),
            {:respond, "get_state", %{"followUpMode" => "all"}},
            Flows.prompt(3, "/follow-up one-at-a-time"),
            {:respond, "set_follow_up_mode", %{}},
            Flows.prompt(4, "/follow-up never")
          ]

      transcript =
        PiGolden.assert_golden(@area, "follow_up_slash_command_get_set_and_usage", steps)

      assert [
               "Follow-up mode: all",
               "Follow-up mode set to: one-at-a-time",
               "Usage: /follow-up all | /follow-up one-at-a-time"
             ] = chunk_texts(transcript)
    end

    test "steering_and_follow_up_config_options_write_mode_notifications" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.set_config(2, "steering_mode", "one-at-a-time"),
            Flows.set_config(3, "steering_mode", "all"),
            Flows.set_config(4, "follow_up_mode", "one-at-a-time"),
            Flows.set_config(5, "steering_mode", "never"),
            Flows.set_config(6, "follow_up_mode", true)
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "steering_and_follow_up_config_options_write_mode_notifications",
          steps
        )

      assert [
               %{tag: :ok, writes: [%{"type" => "set_steering_mode", "mode" => "one-at-a-time"}]},
               %{tag: :ok, writes: [%{"type" => "set_steering_mode", "mode" => "all"}]},
               %{
                 tag: :ok,
                 writes: [%{"type" => "set_follow_up_mode", "mode" => "one-at-a-time"}]
               },
               %{tag: :error, error: "Unknown Pi config option: steering_mode"},
               %{tag: :error, error: "Unknown Pi config option: follow_up_mode"}
             ] = Enum.map(Enum.take(transcript, -5), & &1.result)
    end

    test "steering_command_while_a_prompt_is_active_is_queued" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "working"),
            Flows.prompt(3, "/steering one-at-a-time"),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "steering_command_while_a_prompt_is_active_is_queued",
          steps
        )

      assert %{"message" => "/steering one-at-a-time", "type" => "prompt"} =
               transcript |> PiGolden.writes() |> List.last()
    end
  end

  describe "subprocess exit" do
    test "exit_fails_active_prompt_and_control_groups_then_cancels_queue" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "working"),
            Flows.prompt(3, "queued one"),
            Flows.prompt(4, "queued two"),
            Flows.session_new(5),
            Flows.session_new(6),
            {:port_exit, 137},
            {:note, "Nothing is pending any more"},
            {:port_exit, 1},
            Flows.agent_settled(),
            {:respond, "get_state", Flows.state_data()}
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "exit_fails_active_prompt_and_control_groups_then_cancels_queue",
          steps
        )

      assert %{
               messages: [
                 %{
                   "id" => 2,
                   "error" => %{"code" => -32_603, "message" => "Pi process exited: 137"}
                 },
                 %{"id" => 5, "error" => %{"message" => "Pi process exited: 137"}},
                 %{"id" => 6, "error" => %{"message" => "Pi process exited: 137"}},
                 %{"id" => 3, "result" => %{"stopReason" => "cancelled"}},
                 %{"id" => 4, "result" => %{"stopReason" => "cancelled"}}
               ]
             } = Enum.at(transcript, 11).result

      assert Enum.all?(Enum.take(transcript, -3), &match?(%{tag: :skip}, &1.result))
    end

    test "port_closed_fails_the_active_prompt" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "working"),
            :port_closed,
            :port_closed,
            {:adapter_message, :unrelated},
            {:adapter_message, {:other_port, {:data, "{}\n"}}}
          ]

      transcript = PiGolden.assert_golden(@area, "port_closed_fails_the_active_prompt", steps)

      assert [
               %{
                 messages: [%{"id" => 2, "error" => %{"message" => "Pi process exited: :closed"}}]
               },
               %{tag: :skip},
               %{tag: :skip},
               %{tag: :skip}
             ] = Enum.map(Enum.take(transcript, -4), & &1.result)
    end

    test "managed_exit_fails_prompt_and_respawn_restores_service" do
      steps =
        [{:init, managed: true}] ++
          Flows.open_session(1) ++
          [
            Flows.prompt(2, "working"),
            {:port_data,
             ~s({"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":"par)},
            {:port_exit, 3},
            Flows.prompt(3, "after exit"),
            Flows.cancel(),
            Flows.set_config(4, "auto_retry", false)
          ] ++
          Flows.open_session(5) ++
          [
            Flows.prompt(6, "alive again"),
            {:port_data, ~s({"type":"agent_settled"}\n)},
            :shutdown
          ]

      transcript =
        PiGolden.assert_golden(
          @area,
          "managed_exit_fails_prompt_and_respawn_restores_service",
          steps
        )

      assert %{
               messages: [%{"id" => 2, "error" => %{"message" => "Pi process exited: 3"}}],
               port: :closed
             } =
               Enum.at(transcript, 8).result

      assert [
               %{tag: :error, error: ":no_active_pi_session"},
               %{tag: :error, error: ":no_active_pi_session"},
               %{tag: :error, error: ":no_active_pi_session"}
             ] = Enum.map(Enum.slice(transcript, 9..11), & &1.result)

      assert %{port: :spawned, pending: true, port_writes: [%{"type" => "new_session"} | _]} =
               Enum.at(transcript, 12).result

      assert %{messages: [%{"id" => 6, "result" => %{"stopReason" => "end_turn"}}]} =
               Enum.at(transcript, 18).result

      assert %{port: :closed} = PiGolden.last_result(transcript)
    end

    test "managed_session_new_replaces_the_running_process" do
      steps =
        [{:init, managed: true}] ++
          Flows.open_session(1) ++
          [Flows.prompt(2, "working")] ++
          Flows.open_session(3) ++
          [
            {:note, "The first session's prompt was dropped with the old process"},
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(@area, "managed_session_new_replaces_the_running_process", steps)

      assert %{port: :respawned, pending: true} = Enum.at(transcript, 7).result
      assert %{tag: :skip} = PiGolden.last_result(transcript)
    end
  end

  describe "close and shutdown" do
    test "close_discards_active_prompt_and_queue_silently" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "working"),
            Flows.prompt(3, "queued"),
            close_session(4),
            Flows.agent_settled(),
            {:note, "The prompt counter is not reset by close"},
            Flows.prompt(5, "fresh"),
            Flows.agent_settled()
          ]

      transcript =
        PiGolden.assert_golden(@area, "close_discards_active_prompt_and_queue_silently", steps)

      assert [%{"id" => 1}, %{"id" => 5}] = responses(transcript)

      assert %{"id" => "msg-2", "message" => "fresh"} =
               transcript |> PiGolden.writes() |> List.last()
    end

    test "shutdown_discards_pending_work_silently" do
      steps =
        Flows.open_session(1) ++
          [
            Flows.prompt(2, "working"),
            Flows.session_new(3),
            :shutdown,
            Flows.agent_settled(),
            {:respond, "get_state", Flows.state_data()},
            {:port_exit, 0}
          ]

      transcript = PiGolden.assert_golden(@area, "shutdown_discards_pending_work_silently", steps)

      assert Enum.all?(Enum.take(transcript, -3), &match?(%{tag: :skip}, &1.result))
    end
  end
end
