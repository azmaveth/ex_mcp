# ExMCP Conformance Test Client
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

    case scenario do
      "initialize" -> run_initialize(server_url)
      "tools_call" -> run_tools_call(server_url)
      "sse-retry" -> run_sse_retry(server_url)
      "elicitation-sep1034-client-defaults" -> run_elicitation(server_url, context)
      "auth/" <> _ -> run_auth(server_url, scenario, context)
      other -> run_default(server_url, other)
    end
  end

  defp run_initialize(server_url) do
    {:ok, client} = connect(server_url)
    # The conformance framework checks that we sent initialize and received a valid response.
    # Just connecting is enough — ExMCP.Client does the handshake automatically.
    Process.sleep(500)
    ExMCP.Client.disconnect(client)
  end

  defp run_tools_call(server_url) do
    Logger.info("Connecting to #{server_url}")

    case connect(server_url) do
      {:ok, client} ->
        list_and_call_tools(client)
        ExMCP.Client.disconnect(client)

      {:error, reason} ->
        Logger.error("Connect failed: #{inspect(reason)}")
    end
  end

  defp list_and_call_tools(client) do
    Logger.info("Connected, listing tools...")

    case ExMCP.Client.list_tools(client) do
      {:ok, result} ->
        tools = result["tools"] || result[:tools] || []
        call_each_tool(client, tools)

      {:error, reason} ->
        Logger.error("list_tools failed: #{inspect(reason)}")
    end
  end

  defp call_each_tool(client, tools) do
    for tool <- tools do
      name = tool["name"] || tool[:name]
      schema = tool["inputSchema"] || tool[:inputSchema]
      args = build_args_from_schema(schema)
      Logger.info("Calling tool #{name} with args: #{inspect(args)}")

      case ExMCP.Client.call_tool(client, name, args) do
        {:ok, result} -> Logger.info("Tool #{name} result: #{inspect(result)}")
        {:error, reason} -> Logger.warning("Tool #{name} failed: #{inspect(reason)}")
      end
    end
  end

  defp run_sse_retry(server_url) do
    # Test SSE reconnection behavior
    {:ok, client} = connect(server_url)
    Process.sleep(1000)
    ExMCP.Client.disconnect(client)
  end

  defp run_elicitation(server_url, _context) do
    {:ok, client} = connect(server_url)

    # List and call tools — the framework checks elicitation handling
    {:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client)

    for tool <- tools do
      name = tool["name"]
      args = build_args_from_schema(tool["inputSchema"])
      ExMCP.Client.call_tool(client, name, args)
    end

    ExMCP.Client.disconnect(client)
  end

  defp run_auth(server_url, _scenario, _context) do
    # Auth scenarios — try to connect with auth headers
    {:ok, client} = connect(server_url)
    Process.sleep(500)
    ExMCP.Client.disconnect(client)
  end

  defp run_default(server_url, scenario) do
    Logger.info("Running default scenario: #{scenario}")
    {:ok, client} = connect(server_url)
    Process.sleep(500)
    ExMCP.Client.disconnect(client)
  end

  # Connect to the conformance test server via HTTP (Streamable HTTP with SSE).
  # The framework passes a full URL like http://localhost:PORT/mcp — we need to
  # split into base_url + endpoint so the HTTP transport posts to the right path.
  defp connect(server_url) do
    uri = URI.parse(server_url)
    base_url = "#{uri.scheme}://#{uri.host}:#{uri.port}"
    endpoint = uri.path || ""

    ExMCP.Client.start_link(
      transport: :http,
      url: base_url,
      endpoint: endpoint,
      use_sse: false,
      client_info: %{
        "name" => "ex_mcp-conformance-client",
        "version" => "0.9.0"
      }
    )
  end

  # Build minimal valid args from JSON Schema
  defp build_args_from_schema(nil), do: %{}

  defp build_args_from_schema(%{"properties" => props} = schema) when is_map(props) do
    required = Map.get(schema, "required", [])

    Map.new(props, fn {name, prop_schema} ->
      value =
        if name in required do
          generate_value(prop_schema)
        else
          generate_value(prop_schema)
        end

      {name, value}
    end)
  end

  defp build_args_from_schema(_), do: %{}

  defp generate_value(%{"type" => "string"}), do: "test"
  defp generate_value(%{"type" => "integer"}), do: 1
  defp generate_value(%{"type" => "number"}), do: 1.0
  defp generate_value(%{"type" => "boolean"}), do: true
  defp generate_value(%{"type" => "array"}), do: []
  defp generate_value(%{"type" => "object"}), do: %{}
  defp generate_value(_), do: "test"
end

ConformanceClient.run()
