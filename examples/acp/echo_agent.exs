#!/usr/bin/env elixir

Application.put_env(:ex_mcp, :stdio_mode, true)
Logger.configure(level: :emergency)
:logger.set_primary_config(:level, :emergency)

defmodule EchoAgent do
  @behaviour ExMCP.ACP.Agent.Handler

  @impl true
  def init(_opts), do: {:ok, %{sessions: %{}}}

  @impl true
  def handle_new_session(params, _ctx, state) do
    session_id = "sess_" <> Base.encode16(:crypto.strong_rand_bytes(8), case: :lower)
    name = Path.basename(params["cwd"] || File.cwd!())

    state = put_in(state, [:sessions, session_id], %{name: name, cwd: params["cwd"]})

    {:reply, %{"sessionId" => session_id}, state}
  end

  @impl true
  def handle_prompt(session_id, prompt, ctx, state) do
    text =
      prompt
      |> Enum.filter(&(&1["type"] == "text"))
      |> Enum.map_join("", &Map.get(&1, "text", ""))

    :ok = ExMCP.ACP.Agent.agent_message(ctx.agent, session_id, "Echo agent received: ")
    :ok = ExMCP.ACP.Agent.agent_message(ctx.agent, session_id, text)

    {:reply, %{"stopReason" => "end_turn"}, state}
  end

  @impl true
  def handle_cancel(_session_id, _ctx, state) do
    {:reply, %{"stopReason" => "cancelled"}, state}
  end
end

ExMCP.ACP.run_agent(
  handler: EchoAgent,
  agent_info: %{"name" => "ex-mcp-echo-agent", "version" => "1.0.0"},
  capabilities: %{"sessionCapabilities" => %{"close" => true}}
)
