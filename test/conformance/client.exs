# ExMCP Conformance Test Client
#
# Minimal wrapper around ExMCP.Client for the MCP conformance framework.
# All protocol logic lives in the library — this script just connects,
# exercises the API, and disconnects.
#
# Spawned by the conformance framework:
#   npx @modelcontextprotocol/conformance client \
#     --command "elixir test/conformance/client.exs" \
#     --scenario initialize
#
# The server URL is passed as the last argument.
# MCP_CONFORMANCE_SCENARIO env var indicates which scenario to run.
# MCP_CONFORMANCE_CONTEXT env var has scenario-specific data (JSON).

Mix.install([{:ex_mcp, path: "."}, {:jason, "~> 1.4"}])

defmodule ConformanceClient do
  require Logger

  def run do
    server_url = List.last(System.argv()) || raise "No server URL provided"
    scenario = System.get_env("MCP_CONFORMANCE_SCENARIO", "")

    context =
      case System.get_env("MCP_CONFORMANCE_CONTEXT") do
        nil -> %{}
        "" -> %{}
        json -> Jason.decode!(json)
      end

    Logger.info("Conformance client: scenario=#{scenario} url=#{server_url}")

    # Enable elicitation auto-accept for conformance testing
    Application.put_env(:ex_mcp, :elicitation_auto_accept, true)

    # All scenarios follow the same pattern: connect, exercise the API, disconnect.
    # The conformance framework validates protocol behavior by observing the wire traffic.
    # Auth scenarios pass credentials via context; the transport handles 401→OAuth automatically.
    run_scenario(server_url, scenario, context)
  end

  defp run_scenario(server_url, scenario, context) do
    opts = build_connect_opts(scenario, context)

    case ExMCP.Client.start_link([url: server_url] ++ opts) do
      {:ok, client} ->
        exercise_api(client, scenario)
        ExMCP.Client.disconnect(client)

      {:error, reason} ->
        Logger.error("Connect failed: #{inspect(reason)}")
    end
  end

  # Build connection options. Auth config comes from conformance context;
  # everything else is standard.
  defp build_connect_opts(_scenario, context) do
    # Per MCP Streamable HTTP spec: always POST, parse SSE responses from POST.
    # SSE GET stream opened automatically when server provides a session ID.
    # use_sse: true enables this — the transport falls back gracefully when
    # no session ID is provided (stateless servers).
    base = [
      transport: :http,
      use_sse: true,
      client_info: %{"name" => "ex_mcp-conformance-client", "version" => "0.9.0"},
      capabilities: %{"sampling" => %{}, "elicitation" => %{}}
    ]

    case build_auth_from_context(context) do
      nil -> base
      auth -> Keyword.put(base, :auth, auth)
    end
  end

  defp build_auth_from_context(%{"client_id" => client_id} = ctx) do
    config = %{client_id: client_id}

    case ctx["client_secret"] do
      nil -> config
      secret -> Map.merge(config, %{client_secret: secret, auth_method: :client_secret})
    end
  end

  defp build_auth_from_context(_), do: nil

  # Exercise the server API based on what the scenario tests.
  # Most scenarios just need connect + list + call tools.
  defp exercise_api(client, "initialize") do
    # Initialize already happened during connect — nothing more needed.
    Process.sleep(200)
  end

  defp exercise_api(client, _scenario) do
    # Default: list tools and call each one. This covers tools_call, auth,
    # elicitation, and most other scenarios. The conformance framework
    # validates the protocol interactions, not our scenario routing.
    case ExMCP.Client.list_tools(client) do
      {:ok, result} ->
        tools = result["tools"] || []
        Logger.info("Listed #{length(tools)} tools")

        for tool <- tools do
          name = tool["name"]
          args = ExMCP.Testing.SchemaGenerator.generate_args(tool["inputSchema"])
          Logger.info("Calling tool: #{name}")

          case ExMCP.Client.call_tool(client, name, args) do
            {:ok, _} -> Logger.info("Tool #{name}: OK")
            {:error, reason} -> Logger.warning("Tool #{name} failed: #{inspect(reason)}")
          end
        end

      {:error, reason} ->
        Logger.warning("list_tools failed: #{inspect(reason)}")
    end
  end
end

ConformanceClient.run()
