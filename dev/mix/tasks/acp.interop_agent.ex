defmodule Mix.Tasks.Acp.InteropAgent do
  @moduledoc """
  Runs a minimal stdio ACP agent for cross-language interop tests.

      mix acp.interop_agent
  """

  use Mix.Task

  @shortdoc "Runs a stdio ACP agent for interop testing"

  def run(_args) do
    Application.put_env(:ex_mcp, :stdio_mode, true)
    Logger.configure(level: :emergency)
    :logger.set_primary_config(:level, :emergency)

    Mix.Task.run("app.start")

    Code.eval_string(~S"""
    defmodule AcpInteropAgentHandler do
      @behaviour ExMCP.ACP.Agent.Handler

      @impl true
      def init(_opts), do: {:ok, %{sessions: MapSet.new()}}

      @impl true
      def handle_new_session(_params, _ctx, state) do
        session_id = "sess_elixir_interop"
        {:reply, %{"sessionId" => session_id}, %{state | sessions: MapSet.put(state.sessions, session_id)}}
      end

      @impl true
      def handle_prompt(session_id, prompt, ctx, state) do
        text =
          prompt
          |> Enum.filter(&(&1["type"] == "text"))
          |> Enum.map_join("", &Map.get(&1, "text", ""))

        :ok = ExMCP.ACP.Agent.agent_message(ctx.agent, session_id, "Hello from ")
        :ok = ExMCP.ACP.Agent.agent_message(ctx.agent, session_id, "ExMCP ACP agent: #{text}")

        {:reply, %{"stopReason" => "end_turn"}, state}
      end

      @impl true
      def handle_list_sessions(_params, _ctx, state) do
        sessions =
          Enum.map(state.sessions, fn session_id ->
            %{"sessionId" => session_id, "name" => "Elixir ACP interop session"}
          end)

        {:reply, sessions, state}
      end

      @impl true
      def handle_close_session(session_id, _ctx, state) do
        {:reply, %{}, %{state | sessions: MapSet.delete(state.sessions, session_id)}}
      end

      @impl true
      def handle_cancel(_session_id, _ctx, state) do
        {:reply, %{"stopReason" => "cancelled"}, state}
      end
    end
    """)

    {:ok, _agent} =
      ExMCP.ACP.Agent.start_link(
        handler: AcpInteropAgentHandler,
        agent_info: %{"name" => "elixir-acp-interop-agent", "version" => "1.0.0"},
        capabilities: %{
          "sessionCapabilities" => %{
            "list" => %{},
            "close" => %{}
          }
        }
      )

    Process.sleep(:infinity)
  end
end
