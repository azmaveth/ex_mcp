defmodule ExMCP.Server.StdioServerI18nTest do
  @moduledoc """
  Launches a real stdio server in a subprocess under several locales and
  drives it with non-ASCII traffic.

  The locale is the only place the environment reaches the transport, and it
  only reaches the stdio transport, so this is the one test that carries a
  locale matrix. Every run uses a tool that generates its own non-ASCII text:
  an echo tool cannot catch the defect, because the write reverses the read's
  corruption exactly.
  """

  use ExUnit.Case, async: false

  alias ExMCP.Test.I18nCorpus

  @moduletag :integration
  @moduletag timeout: 120_000

  @locales [
    {"unset", nil},
    {"C", "C"},
    {"en_US.UTF-8", "en_US.UTF-8"},
    {"ja_JP.eucJP", "ja_JP.eucJP"}
  ]

  setup_all do
    dir = Path.join(System.tmp_dir!(), "ex-mcp-stdio-i18n-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)

    script = Path.join(dir, "server.exs")
    File.write!(script, server_script())

    input = Path.join(dir, "input.jsonl")
    File.write!(input, input_bytes())

    %{script: script, input: input}
  end

  for {label, locale} <- @locales do
    test "moves non-ASCII frames byte-exact under LANG=#{label}", %{script: script, input: input} do
      {output, stderr, code} = run_server(script, input, unquote(locale))
      assert code == 0, "exit #{code}\nstdout:\n#{output}\nstderr:\n#{stderr}"

      responses =
        output
        |> String.split("\n", trim: true)
        |> Enum.map(&Jason.decode!/1)

      assert Enum.map(responses, & &1["id"]) == [1, 2, 3, 4]

      generated = text_of(Enum.at(responses, 1))
      assert generated == "generated=" <> I18nCorpus.all_text()

      echoed = text_of(Enum.at(responses, 2))

      assert echoed ==
               "echoed=" <>
                 I18nCorpus.all_text() <>
                 " bytes=" <> Integer.to_string(byte_size(I18nCorpus.all_text()))

      assert %{"id" => 4, "result" => %{}} = Enum.at(responses, 3)
    end
  end

  # The input stream starts with a byte-order mark, carries the corpus both as
  # a request argument and as a `\u`-escaped surrogate pair, includes one line
  # that is not valid UTF-8, and ends with a ping that proves the server
  # survived all of it.
  defp input_bytes do
    all = I18nCorpus.all_text()

    requests = [
      Jason.encode!(%{
        "jsonrpc" => "2.0",
        "id" => 1,
        "method" => "initialize",
        "params" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "clientInfo" => %{"name" => "i18n", "version" => "1"}
        }
      }),
      Jason.encode!(%{"jsonrpc" => "2.0", "method" => "notifications/initialized"}),
      Jason.encode!(%{
        "jsonrpc" => "2.0",
        "id" => 2,
        "method" => "tools/call",
        "params" => %{"name" => "generate", "arguments" => %{}}
      }),
      I18nCorpus.surrogate_escaped_json(%{
        "jsonrpc" => "2.0",
        "id" => 3,
        "method" => "tools/call",
        "params" => %{"name" => "echo", "arguments" => %{"text" => all}}
      })
    ]

    IO.iodata_to_binary([
      I18nCorpus.bom(),
      Enum.map(requests, &[&1, "\n"]),
      I18nCorpus.invalid_utf8_frame(),
      Jason.encode!(%{"jsonrpc" => "2.0", "id" => 4, "method" => "ping"}),
      "\n"
    ])
  end

  defp run_server(script, input, locale) do
    paths = :code.get_path() |> Enum.flat_map(&["-pa", to_string(&1)])

    locale_env =
      case locale do
        nil -> [{"LANG", nil}, {"LC_ALL", nil}, {"LC_CTYPE", nil}]
        value -> [{"LANG", value}, {"LC_ALL", value}]
      end

    stderr_path = input <> ".stderr"

    {output, code} =
      System.cmd(
        "sh",
        [
          "-c",
          ~s(exec "$@" < "$I18N_INPUT" 2> "$I18N_STDERR"),
          "stdio-i18n",
          System.find_executable("elixir")
        ] ++ paths ++ [script],
        env:
          locale_env ++ [{"MIX_ENV", "test"}, {"I18N_INPUT", input}, {"I18N_STDERR", stderr_path}]
      )

    {output, File.read!(stderr_path), code}
  end

  defp text_of(%{"result" => %{"content" => [%{"type" => "text", "text" => text}]}}), do: text

  defp server_script do
    all = I18nCorpus.all_text()

    ~s"""
    # A bare `elixir` VM with the test code path: start only what the stdio
    # server itself needs, not every application on the path.
    Logger.configure(level: :error)
    {:ok, _apps} = Application.ensure_all_started(:jason)
    {:ok, _apps} = Application.ensure_all_started(:telemetry)

    defmodule I18nStdioServer do
      use ExMCP.Server.Handler
      use ExMCP.Server.DSL, name: "i18n", version: "1"

      tool "generate", "Generates non-ASCII text of its own" do
        run(fn _args, state -> {:ok, "generated=" <> #{inspect(all)}, state} end)
      end

      tool "echo", "Echoes text and reports its byte length" do
        param(:text, :string, required: true)

        run(fn args, state ->
          {:ok, "echoed=" <> args.text <> " bytes=" <> Integer.to_string(byte_size(args.text)), state}
        end)
      end
    end

    {:ok, pid} = I18nStdioServer.start_link(transport: :stdio)
    ref = Process.monitor(pid)

    receive do
      {:DOWN, ^ref, :process, ^pid, :normal} -> :ok
      {:DOWN, ^ref, :process, ^pid, reason} -> exit({:server_down, reason})
    after
      30_000 -> exit(:eof_did_not_stop_server)
    end
    """
  end
end
