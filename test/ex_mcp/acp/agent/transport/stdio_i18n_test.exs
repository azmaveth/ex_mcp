defmodule ExMCP.ACP.Agent.Transport.StdioI18nTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Agent.Transport.Stdio
  alias ExMCP.Test.I18nCorpus

  # Embedders pass their own devices. A developer shell gives the transport a
  # unicode-mode stdio; a locale-less service gives it latin1. Both must move
  # frames byte-exact, so every case runs against devices opened in each mode.
  for mode <- [:unicode, :latin1] do
    describe "with devices opened in #{mode} mode" do
      @mode mode

      test "receives every corpus frame byte-exact" do
        for {label, text} <- I18nCorpus.strings() do
          frame =
            Jason.encode!(%{"jsonrpc" => "2.0", "method" => "note", "params" => %{"t" => text}})

          {:ok, state} = connect(@mode, frame <> "\n")

          assert {:ok, received, _state} = Stdio.receive_message(state)
          assert received == frame, label
          assert Jason.decode!(received)["params"]["t"] == text, label
        end
      end

      test "sends every corpus frame byte-exact" do
        for {label, text} <- I18nCorpus.strings() do
          frame =
            Jason.encode!(%{"jsonrpc" => "2.0", "method" => "note", "params" => %{"t" => text}})

          {:ok, state} = connect(@mode, "")

          assert {:ok, _state} = Stdio.send_message(frame, state)
          {_input, output} = StringIO.contents(state.output)
          assert output == frame <> "\n", label
        end
      end

      test "accepts a surrogate-escaped astral character from another SDK" do
        frame =
          I18nCorpus.surrogate_escaped_json(%{
            "jsonrpc" => "2.0",
            "method" => "note",
            "params" => %{"t" => "🪁"}
          })

        refute frame =~ "🪁"
        {:ok, state} = connect(@mode, frame <> "\n")

        assert {:ok, received, _state} = Stdio.receive_message(state)
        assert Jason.decode!(received)["params"]["t"] == "🪁"
      end

      test "strips a byte-order mark from the first frame only" do
        first = ~S({"jsonrpc":"2.0","method":"first"})
        second = ~S({"jsonrpc":"2.0","method":"second"})
        {:ok, state} = connect(@mode, I18nCorpus.bom() <> first <> "\n" <> second <> "\n")

        assert {:ok, ^first, state} = Stdio.receive_message(state)
        assert {:ok, ^second, _state} = Stdio.receive_message(state)
      end

      test "accepts CRLF-terminated frames" do
        frame =
          Jason.encode!(%{"jsonrpc" => "2.0", "method" => "note", "params" => %{"t" => "café"}})

        {:ok, state} = connect(@mode, frame <> "\r\n")

        assert {:ok, ^frame, _state} = Stdio.receive_message(state)
      end

      test "counts the frame limit in bytes, not characters" do
        limit = 64
        exact = I18nCorpus.multibyte_of_size(limit)
        assert byte_size(exact) == limit and String.length(exact) < limit

        {:ok, state} = connect(@mode, exact <> "\n", max_frame_bytes: limit)
        assert {:ok, ^exact, _state} = Stdio.receive_message(state)

        {:ok, state} = connect(@mode, exact <> "x\n", max_frame_bytes: limit)
        assert {:error, :frame_too_large} = Stdio.receive_message(state)
      end
    end
  end

  defp connect(mode, input_bytes, opts \\ []) do
    {:ok, input} = StringIO.open(input_bytes, encoding: mode)
    {:ok, output} = StringIO.open("", encoding: mode)
    Stdio.connect([input: input, output: output] ++ opts)
  end
end
