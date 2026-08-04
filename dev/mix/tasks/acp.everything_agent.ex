defmodule Mix.Tasks.Acp.EverythingAgent do
  @moduledoc """
  Runs an ACP agent that exercises the stable protocol surface for interop tests.

      mix acp.everything_agent
  """

  use Mix.Task

  @shortdoc "Runs an everything-style ACP agent for interop testing"

  def run(_args) do
    Application.put_env(:ex_mcp, :stdio_mode, true)
    Logger.configure(level: :emergency)
    :logger.set_primary_config(:level, :emergency)

    Mix.Task.run("app.start")

    {:ok, _agent} =
      ExMCP.ACP.Agent.start_link(
        handler: Mix.Tasks.Acp.EverythingAgent.Handler,
        agent_info: %{"name" => "elixir-acp-everything-agent", "version" => "1.0.0"},
        auth_methods: [
          %{
            "id" => "agent",
            "name" => "Agent-managed auth",
            "description" => "Interop auth method handled by the test agent"
          }
        ],
        capabilities:
          ExMCP.ACP.Types.agent_capabilities(
            load_session: true,
            image: true,
            audio: true,
            embedded_context: true,
            session_list: true,
            session_resume: true,
            session_close: true,
            session_delete: true,
            session_additional_directories: true,
            logout: true
          )
      )

    Process.sleep(:infinity)
  end
end

defmodule Mix.Tasks.Acp.EverythingAgent.Handler do
  @moduledoc false

  @behaviour ExMCP.ACP.Agent.Handler

  alias ExMCP.ACP.Agent

  @updated_at "2026-05-29T00:00:00Z"

  @impl true
  def init(_opts) do
    {:ok, %{sessions: %{}, authenticated?: false, mode_id: "code", model: "fast"}}
  end

  @impl true
  def handle_authenticate(%{"methodId" => "agent"}, _ctx, state) do
    {:reply, %{}, %{state | authenticated?: true}}
  end

  def handle_authenticate(_params, _ctx, state) do
    {:error, {-32602, "Unknown auth method", nil}, state}
  end

  @impl true
  def handle_logout(_ctx, state) do
    {:reply, %{}, %{state | authenticated?: false}}
  end

  @impl true
  def handle_new_session(params, _ctx, state) do
    session_id = "sess_elixir_everything"
    cwd = params["cwd"] || File.cwd!()

    session = %{
      "sessionId" => session_id,
      "cwd" => cwd,
      "title" => "Elixir ACP everything session",
      "updatedAt" => @updated_at
    }

    state = put_in(state, [:sessions, session_id], session)
    {:reply, Map.put(session_response(state), "sessionId", session_id), state}
  end

  @impl true
  def handle_load_session(%{"sessionId" => session_id}, ctx, state) do
    send_loaded_history(ctx.agent, session_id)
    {:reply, session_response(state), state}
  end

  @impl true
  def handle_resume_session(%{"sessionId" => _session_id}, _ctx, state) do
    {:reply, session_response(state), state}
  end

  @impl true
  def handle_list_sessions(_params, _ctx, state) do
    {:reply, Map.values(state.sessions), state}
  end

  @impl true
  def handle_close_session(session_id, _ctx, state) do
    {:reply, %{}, %{state | sessions: Map.delete(state.sessions, session_id)}}
  end

  @impl true
  def handle_delete_session(session_id, _ctx, state) do
    {:reply, %{}, %{state | sessions: Map.delete(state.sessions, session_id)}}
  end

  @impl true
  def handle_set_mode(session_id, mode_id, ctx, state) do
    :ok = Agent.current_mode(ctx.agent, session_id, mode_id)
    {:reply, %{}, %{state | mode_id: mode_id}}
  end

  @impl true
  def handle_set_config_option(session_id, "model", model, ctx, state) when is_binary(model) do
    state = %{state | model: model}
    :ok = Agent.config_options(ctx.agent, session_id, config_options(state))
    {:reply, %{"configOptions" => config_options(state)}, state}
  end

  def handle_set_config_option(_session_id, config_id, _value, _ctx, state) do
    {:error, {-32602, "Unknown config option: #{config_id}", nil}, state}
  end

  @impl true
  def handle_prompt(session_id, prompt, ctx, state) do
    text = prompt_text(prompt)

    if String.contains?(text, "cancel-me") do
      :ok = Agent.agent_message(ctx.agent, session_id, "waiting for cancel")
      {:noreply, state}
    else
      :ok =
        Agent.session_update(ctx.agent, session_id, %{
          "sessionUpdate" => "user_message_chunk",
          "content" => %{"type" => "text", "text" => text}
        })

      :ok = Agent.agent_thought(ctx.agent, session_id, "thinking from ExMCP")

      :ok =
        Agent.plan(ctx.agent, session_id, [
          %{"content" => "Inspect request", "priority" => "high", "status" => "completed"},
          %{"content" => "Exercise client APIs", "priority" => "high", "status" => "in_progress"}
        ])

      :ok =
        Agent.available_commands(ctx.agent, session_id, [
          %{"name" => "test", "description" => "Run the ACP everything fixture"}
        ])

      :ok = Agent.current_mode(ctx.agent, session_id, state.mode_id)
      :ok = Agent.config_options(ctx.agent, session_id, config_options(state))

      :ok =
        Agent.session_info(ctx.agent, session_id, %{
          "title" => "Elixir ACP everything prompt",
          "updatedAt" => @updated_at
        })

      :ok =
        Agent.tool_call(ctx.agent, session_id, %{
          "toolCallId" => "tool_elixir_1",
          "title" => "Read interop file",
          "kind" => "read",
          "status" => "pending",
          "locations" => [%{"path" => readme_path(), "line" => 1}],
          "rawInput" => %{"path" => readme_path()}
        })

      {:ok, permission} =
        Agent.request_permission(
          ctx.agent,
          session_id,
          %{
            "toolCallId" => "tool_elixir_1",
            "title" => "Read interop file",
            "kind" => "read",
            "status" => "pending",
            "locations" => [%{"path" => readme_path()}],
            "rawInput" => %{"path" => readme_path()}
          },
          [%{"kind" => "allow_once", "name" => "Allow once", "optionId" => "allow"}]
        )

      {:ok, %{"content" => file_content}} =
        Agent.read_text_file(ctx.agent, session_id, readme_path(), line: 1, limit: 20)

      {:ok, _} =
        Agent.write_text_file(ctx.agent, session_id, write_path(), "updated")

      {:ok, %{"terminalId" => terminal_id}} =
        Agent.terminal_create(ctx.agent, session_id, %{
          "command" => "echo",
          "args" => ["hello"],
          "cwd" => File.cwd!()
        })

      {:ok, %{"output" => terminal_output}} =
        Agent.terminal_output(ctx.agent, session_id, terminal_id)

      {:ok, %{"exitCode" => 0}} = Agent.terminal_wait_for_exit(ctx.agent, session_id, terminal_id)
      {:ok, _} = Agent.terminal_kill(ctx.agent, session_id, terminal_id)
      {:ok, _} = Agent.terminal_release(ctx.agent, session_id, terminal_id)

      :ok =
        Agent.tool_call_update(ctx.agent, session_id, %{
          "toolCallId" => "tool_elixir_1",
          "status" => "completed",
          "content" => [
            %{"type" => "content", "content" => %{"type" => "text", "text" => file_content}},
            %{"type" => "terminal", "terminalId" => terminal_id}
          ],
          "rawOutput" => %{
            "permission" => permission,
            "terminalOutput" => terminal_output
          }
        })

      :ok = Agent.usage(ctx.agent, session_id, 42, 100)

      :ok = Agent.agent_message(ctx.agent, session_id, "Hello from ExMCP everything agent")
      {:reply, %{"stopReason" => "end_turn"}, state}
    end
  end

  @impl true
  def handle_cancel(_session_id, _ctx, state) do
    {:reply, %{"stopReason" => "cancelled"}, state}
  end

  defp send_loaded_history(agent, session_id) do
    :ok =
      Agent.session_update(agent, session_id, %{
        "sessionUpdate" => "user_message_chunk",
        "content" => %{"type" => "text", "text" => "loaded user message"}
      })

    Agent.agent_message(agent, session_id, "loaded agent message")
  end

  defp prompt_text(prompt) do
    prompt
    |> Enum.filter(&(&1["type"] == "text"))
    |> Enum.map_join("", &Map.get(&1, "text", ""))
  end

  defp readme_path, do: Path.expand("README.md")
  defp write_path, do: Path.expand("tmp/acp_everything.txt")

  defp session_response(state) do
    %{"modes" => mode_state(state), "configOptions" => config_options(state)}
  end

  defp mode_state(state) do
    %{
      "availableModes" => [
        %{"id" => "code", "name" => "Code", "description" => "Make code changes"},
        %{"id" => "plan", "name" => "Plan", "description" => "Plan before editing"}
      ],
      "currentModeId" => state.mode_id
    }
  end

  defp config_options(state) do
    [
      %{
        "id" => "model",
        "name" => "Model",
        "category" => "model",
        "type" => "select",
        "currentValue" => state.model,
        "options" => [
          %{"value" => "fast", "name" => "Fast"},
          %{"value" => "deep", "name" => "Deep"}
        ]
      }
    ]
  end
end
