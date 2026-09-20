defmodule ExMCP.Internal.StdioFramingTest do
  use ExUnit.Case, async: true

  alias ExMCP.Internal.StdioFraming
  alias ExMCP.Test.I18nCorpus

  for mode <- [:unicode, :latin1] do
    describe "on a device opened in #{mode} mode" do
      @mode mode

      test "a pinned device writes every corpus frame byte-exact" do
        for {label, text} <- I18nCorpus.strings() do
          {:ok, device} = StringIO.open("", encoding: @mode)
          :ok = StdioFraming.pin_byte_mode(device)
          frame = Jason.encode!(%{"t" => text})

          assert :ok = StdioFraming.write_frame(device, frame)
          {_input, output} = StringIO.contents(device)
          assert output == frame <> "\n", label
        end
      end

      test "a pinned device reads every corpus frame byte-exact" do
        for {label, text} <- I18nCorpus.strings() do
          frame = Jason.encode!(%{"t" => text})
          {:ok, device} = StringIO.open(frame <> "\n", encoding: @mode)
          :ok = StdioFraming.pin_byte_mode(device)

          assert {:ok, line} = StdioFraming.read_line(device)
          assert line == frame <> "\n", label
          assert :eof = StdioFraming.read_line(device)
        end
      end
    end
  end

  test "byte writes on an unpinned unicode device are not byte-exact, which is why the pin exists" do
    {:ok, device} = StringIO.open("", encoding: :unicode)
    frame = Jason.encode!(%{"t" => "café"})

    :ok = StdioFraming.write_frame(device, frame)
    {_input, output} = StringIO.contents(device)
    refute output == frame <> "\n"
  end

  test "strips a byte-order mark only from the start of a frame" do
    frame = ~S({"a":1})
    assert StdioFraming.strip_bom(I18nCorpus.bom() <> frame) == frame
    assert StdioFraming.strip_bom(frame) == frame
    assert StdioFraming.strip_bom("") == ""
    assert StdioFraming.strip_bom(frame <> I18nCorpus.bom()) == frame <> I18nCorpus.bom()
  end
end
