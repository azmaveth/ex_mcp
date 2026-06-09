#!/usr/bin/env elixir

# Unified demo client for stdio, HTTP, HTTP+SSE, and BEAM-local transports.

System.put_env("ELIXIR_LOG_LEVEL", "error")
:logger.set_primary_config(:level, :error)
Application.put_env(:logger, :level, :error)

Mix.install(
  [
    {:ex_mcp, path: Path.expand("../..", __DIR__)}
  ],
  verbose: false
)

Logger.configure(level: :error)

defmodule DemoHttpServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "demo-http-server", version: "1.0.0"

  resource "hello://world", "HTTP demo greeting" do
    title("HTTP Greeting")
    mime_type("text/plain")

    read(fn %{uri: uri}, state ->
      {:ok, %{uri: uri, text: "Hello from the HTTP transport."}, state}
    end)
  end
end

defmodule DemoSseServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "demo-sse-server", version: "1.0.0"

  prompt "hello_generator", "Generates a greeting prompt" do
    title("Hello Generator")
    arg(:recipient, required: true)

    render(fn %{recipient: recipient}, state ->
      {:ok, "Write a concise greeting for #{recipient}.", state}
    end)
  end
end

defmodule DemoBeamServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "demo-beam-server", version: "1.0.0"

  tool "beam_hello", "Returns a greeting from the same BEAM VM" do
    title("BEAM Hello")
    param(:message, :string, default: "Hello from BEAM.")

    run(fn %{message: message}, state ->
      {:ok, ToolResult.structured(message, %{transport: "beam"}), state}
    end)
  end
end

defmodule DemoClient do
  alias ExMCP.Client

  def run do
    IO.puts("ExMCP getting-started transport demo")
    IO.puts(String.duplicate("=", 40))

    demo_stdio()
    demo_http()
    demo_http_sse()
    demo_beam()

    IO.puts("\nDemo completed.")
  end

  defp demo_stdio do
    IO.puts("\n1. STDIO transport")
    server_path = Path.join(__DIR__, "01_stdio_server.exs")

    with {:ok, client} <-
           Client.start_link(
             transport: :stdio,
             command: ["elixir", server_path],
             name: :demo_stdio_client
           ),
         {:ok, %{"tools" => tools}} <- Client.list_tools(client, format: :map, timeout: 15_000),
         {:ok, result} <-
           Client.call_tool(
             client,
             "hello",
             %{"name" => "World", "language" => "english"},
             format: :map
           ) do
      IO.puts("Tools: #{Enum.map_join(tools, ", ", &field(&1, :name))}")
      IO.puts("Tool result: #{tool_text(result)}")
      Client.stop(client)
    else
      error -> IO.puts("STDIO demo skipped: #{inspect(error)}")
    end
  end

  defp demo_http do
    IO.puts("\n2. HTTP transport")
    port = open_port()
    ref = :"demo_http_#{port}"

    {:ok, _server} =
      DemoHttpServer.start_link(
        transport: :http,
        port: port,
        use_sse: false,
        ranch_ref: ref
      )

    try do
      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: "http://localhost:#{port}",
          use_sse: false,
          name: :demo_http_client
        )

      {:ok, %{"resources" => resources}} = Client.list_resources(client, format: :map)

      {:ok, %{"contents" => [content]}} =
        Client.read_resource(client, "hello://world", format: :map)

      IO.puts("Resources: #{Enum.map_join(resources, ", ", &field(&1, :uri))}")
      IO.puts("Resource text: #{field(content, :text)}")
      Client.stop(client)
    after
      Plug.Cowboy.shutdown(ref)
    end
  end

  defp demo_http_sse do
    IO.puts("\n3. HTTP+SSE transport")
    port = open_port()
    ref = :"demo_sse_#{port}"

    {:ok, _server} =
      DemoSseServer.start_link(
        transport: :http,
        port: port,
        use_sse: true,
        ranch_ref: ref
      )

    try do
      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: "http://localhost:#{port}",
          use_sse: false,
          name: :demo_sse_client
        )

      {:ok, %{"prompts" => prompts}} = Client.list_prompts(client, format: :map)

      {:ok, %{"messages" => [message]}} =
        Client.get_prompt(client, "hello_generator", %{"recipient" => "Alice"}, format: :map)

      IO.puts("Prompts: #{Enum.map_join(prompts, ", ", &field(&1, :name))}")
      IO.puts("Prompt text: #{message |> field(:content) |> field(:text)}")
      Client.stop(client)
    after
      Plug.Cowboy.shutdown(ref)
    end
  end

  defp demo_beam do
    IO.puts("\n4. BEAM-local transport")

    {:ok, server} = DemoBeamServer.start_link(transport: :beam)
    {:ok, client} = Client.start_link(transport: :beam, server: server, name: :demo_beam_client)

    {:ok, %{"tools" => tools}} = Client.list_tools(client, format: :map)

    {:ok, result} =
      Client.call_tool(client, "beam_hello", %{"message" => "Hello from a local pid."},
        format: :map
      )

    IO.puts("Tools: #{Enum.map_join(tools, ", ", &field(&1, :name))}")
    IO.puts("Tool result: #{tool_text(result)}")

    Client.stop(client)
    GenServer.stop(server)
  end

  defp tool_text(%{"content" => [%{"text" => text} | _]}), do: text
  defp tool_text(%{content: [%{text: text} | _]}), do: text
  defp tool_text(result), do: inspect(result)

  defp field(map, key) when is_map(map),
    do: Map.get(map, Atom.to_string(key)) || Map.get(map, key)

  defp open_port do
    {:ok, socket} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, {_address, port}} = :inet.sockname(socket)
    :gen_tcp.close(socket)
    port
  end
end

if System.get_env("MCP_ENV") != "test" do
  DemoClient.run()
end
