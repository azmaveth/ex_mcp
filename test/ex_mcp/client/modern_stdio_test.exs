defmodule ExMCP.Client.ModernStdioTest do
  use ExUnit.Case, async: false

  alias ExMCP.Client

  @fixture Path.expand("../../../priv/test/modern_stdio_server.exs", __DIR__)
  @project_root Path.expand("../../..", __DIR__)

  defmodule Handler do
    @behaviour ExMCP.Client.Handler

    @impl true
    def init(opts), do: {:ok, %{owner: Keyword.fetch!(opts, :owner)}}

    @impl true
    def handle_ping(state), do: {:ok, %{}, state}

    @impl true
    def handle_list_roots(state), do: {:ok, [], state}

    @impl true
    def handle_create_message(_params, state), do: {:error, "not configured", state}

    @impl true
    def handle_elicitation_create(message, _schema, state) do
      send(state.owner, {:stdio_elicitation, message})
      {:ok, %{"action" => "accept", "content" => %{"name" => "Mina"}}, state}
    end
  end

  test "modern client completes discovery and tool calls over stdio without initialize" do
    mix = System.find_executable("mix") || flunk("mix executable is required")

    {:ok, client} =
      Client.start_link(
        transport: :stdio,
        command: [mix, "run", "--no-compile", "--no-start", @fixture],
        cd: @project_root,
        env: [{"MIX_ENV", "test"}],
        protocol_mode: :modern_only,
        capabilities: %{"elicitation" => %{"form" => %{}}},
        handler: {Handler, [owner: self()]},
        era_probe_timeout: 10_000,
        health_check_interval: nil
      )

    on_exit(fn ->
      try do
        Client.disconnect(client)
      catch
        :exit, _reason -> :ok
      end
    end)

    assert {:ok, "2026-07-28"} = Client.negotiated_version(client)

    assert {:ok, %{"name" => "modern-stdio-server", "version" => "1.0.0"}} =
             Client.server_info(client)

    assert {:ok, %{"resultType" => "complete", "tools" => tools}} =
             Client.list_tools(client, format: :map, timeout: 5_000)

    tool = Enum.find(tools, &(&1["name"] == "echo"))
    assert tool["name"] == "echo"

    assert {:ok, result} =
             Client.call_tool(client, "echo", %{"text" => "stdio"},
               format: :map,
               timeout: 5_000
             )

    assert result["resultType"] == "complete"
    assert result["content"] == [%{"type" => "text", "text" => "stdio"}]

    assert {:ok, mrtr_result} =
             Client.call_tool(client, "onboard", %{}, format: :map, timeout: 5_000)

    assert mrtr_result["resultType"] == "complete"
    assert mrtr_result["content"] == [%{"type" => "text", "text" => "Mina:stdio"}]
    assert_receive {:stdio_elicitation, "Choose a stdio display name"}
  end
end
