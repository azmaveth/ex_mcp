# Fast, mix-context version of the getting-started demo.
# Run via: mix examples.getting_started
#
# This exercises the core documented patterns:
#   - use ExMCP.Server.Handler + use ExMCP.Server.DSL
#   - server.start_link(transport: :test)
#   - ExMCP.Client.start_link(transport: :test, server: ...)
#   - list_tools / call_tool / list_resources
#
# For the full cross-transport experience (including real stdio sub-processes
# that trigger Mix.install on first run), use:
#   cd examples/getting_started && elixir demo_client.exs
# or ./run_demo.sh

IO.puts("ExMCP Getting Started Demo (fast mode — using compiled code)")
IO.puts("This exercises the DSL + Client + start_link patterns from the docs.")

IO.puts(
  "For the full cross-transport demo (including stdio): cd examples/getting_started && elixir demo_client.exs"
)

IO.puts("---")

defmodule GettingStartedDemoServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "getting-started-demo", version: "1.0.0"

  tool "hello", "Says hello in a requested language" do
    param(:name, :string, required: true)
    param(:language, :string, default: "english")

    run(fn %{name: name, language: language}, state ->
      greeting =
        case language do
          "spanish" -> "Hola, #{name}."
          "french" -> "Bonjour, #{name}."
          "japanese" -> "Konnichiwa, #{name}."
          _ -> "Hello, #{name}."
        end

      {:ok, %{content: [%{type: "text", text: greeting}]}, state}
    end)
  end

  resource "demo://info", "Demo resource" do
    read(fn %{uri: uri}, state ->
      {:ok, %{uri: uri, text: "Getting Started Demo Resource"}, state}
    end)
  end
end

{:ok, server} = GettingStartedDemoServer.start_link(transport: :test)
{:ok, client} = ExMCP.Client.start_link(transport: :test, server: server)

{:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client, format: :map)
IO.puts("Tools: #{Enum.map(tools, & &1["name"]) |> Enum.join(", ")}")

{:ok, result} =
  ExMCP.Client.call_tool(client, "hello", %{"name" => "World", "language" => "english"},
    format: :map
  )

text = get_in(result, ["content", Access.at(0), "text"]) || inspect(result)
IO.puts("Tool result: #{text}")

{:ok, %{"resources" => resources}} = ExMCP.Client.list_resources(client, format: :map)
IO.puts("Resources: #{Enum.map(resources, & &1["uri"]) |> Enum.join(", ")}")

ExMCP.Client.stop(client)
GenServer.stop(server)

IO.puts("---")

IO.puts(
  "Demo complete. All patterns match the documented DSL + Client usage in QUICKSTART.md, DSL_GUIDE.md, and examples/."
)
