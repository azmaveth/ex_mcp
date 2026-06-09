defmodule ExMCP.DocRegressionTest do
  @moduledoc """
  Regression tests for documentation shapes.

  These ensure that the example code in guides, README, and moduledocs
  remains valid as the library evolves. They exercise the exact callback
  signatures and return shapes that we document for new users.
  """

  use ExUnit.Case, async: true

  # This test would have caught the pre-audit 1-arg list_* and incomplete
  # initialize shapes in PHOENIX_GUIDE, USER_GUIDE, lib/ex_mcp.ex, etc.
  test "documented raw Handler callback shapes work (2-arity lists, full initialize, proper returns)" do
    defmodule DocRegressionRawHandler do
      use ExMCP.Server.Handler

      @impl true
      def init(_args), do: {:ok, %{}}

      @impl true
      def handle_initialize(_params, state) do
        {:ok,
         %{
           protocolVersion: ExMCP.protocol_version(),
           serverInfo: %{name: "doc-regression", version: "1.0.0"},
           capabilities: %{tools: %{}, resources: %{}}
         }, state}
      end

      @impl true
      def handle_list_tools(_cursor, state) do
        tools = [
          %{
            name: "echo",
            description: "Echo input",
            inputSchema: %{type: "object", properties: %{}}
          }
        ]

        {:ok, tools, nil, state}
      end

      @impl true
      def handle_call_tool("echo", args, state) do
        msg = Map.get(args, "message", "hello")
        {:ok, %{content: [%{type: "text", text: "echo: #{msg}"}]}, state}
      end

      # Minimal implementations for other callbacks used in docs
      @impl true
      def handle_list_resources(_cursor, state), do: {:ok, [], nil, state}

      @impl true
      def handle_read_resource(uri, state),
        do: {:ok, %{uri: uri, text: "ok"}, state}

      @impl true
      def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}

      @impl true
      def handle_get_prompt(_name, _args, state),
        do: {:ok, %{messages: []}, state}
    end

    {:ok, server} =
      ExMCP.Server.HandlerServer.start_link(
        handler: DocRegressionRawHandler,
        transport: :test
      )

    {:ok, client} = ExMCP.Client.start_link(transport: :test, server: server)

    assert {:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client, format: :map)
    assert Enum.any?(tools, &(&1["name"] == "echo" || &1[:name] == "echo"))

    assert {:ok, result} =
             ExMCP.Client.call_tool(client, "echo", %{"message" => "world"}, format: :map)

    assert result["content"] |> hd() |> Map.get("text") =~ "echo: world"

    # Also exercise a resource to cover documented shapes
    assert {:ok, _} = ExMCP.Client.list_resources(client, format: :map)

    ExMCP.Client.stop(client)
    GenServer.stop(server)
  end

  test "no 1-arity list_* callbacks or other known-bad patterns remain in live docs" do
    bad_patterns = [
      ~r/handle_list_tools\(state\)/,
      ~r/handle_list_resources\(state\)/,
      ~r/handle_list_prompts\(state\)/
    ]

    doc_files =
      Path.wildcard("docs/**/*.md") ++
        Path.wildcard("README.md") ++
        Path.wildcard("docs/getting-started/**/*.md")

    for file <- doc_files, pattern <- bad_patterns do
      content = File.read!(file)

      refute Regex.match?(pattern, content),
             "Found outdated 1-arity callback pattern in #{file} matching #{inspect(pattern)}"
    end
  end
end
