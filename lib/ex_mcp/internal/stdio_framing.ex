defmodule ExMCP.Internal.StdioFraming do
  @moduledoc false

  # The one place that knows how a JSON-RPC frame crosses an IO device.
  #
  # MCP and ACP frames are UTF-8 bytes, one per line. An Erlang IO device has
  # a character encoding of its own, picked from the process locale at VM
  # start: `:unicode` under a UTF-8 locale, `:latin1` under anything else,
  # including no locale at all. Reading or writing *characters* through such a
  # device lets that mode translate the frame: latin1 mode double-encodes
  # incoming UTF-8 and escapes outgoing codepoints above U+00FF, unicode mode
  # turns byte-sized reads into decoded codepoints. Neither is acceptable for
  # protocol traffic, and the locale is not something a transport controls.
  #
  # So the transport pins each device it owns to byte mode once, before any
  # read or write, and from then on only moves bytes. `IO.binwrite/2` and
  # `IO.binread/2` on a latin1-mode device are exact byte copies; on a
  # unicode-mode device they are not, which is why the pin is not optional.
  #
  # Both stdio transports use this module so the rule lives in one place.

  @bom <<0xEF, 0xBB, 0xBF>>

  @doc """
  Pins a device to byte mode. Must run before any read or write on it.
  """
  @spec pin_byte_mode(IO.device()) :: :ok | {:error, term()}
  def pin_byte_mode(device) do
    # Encoding and binary mode are separate options, and not every device
    # accepts both in one call (StringIO rejects the pair with :enotsup), so
    # set the encoding first and only touch binary mode where it is off.
    # :stdio is an Elixir alias that Erlang's :io does not know.
    device = map_device(device)

    with :ok <- :io.setopts(device, encoding: :latin1) do
      case :io.getopts(device) do
        opts when is_list(opts) ->
          if Keyword.get(opts, :binary, true), do: :ok, else: :io.setopts(device, binary: true)

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  @doc """
  Writes one frame followed by a newline, as raw bytes.
  """
  @spec write_frame(IO.device(), iodata()) :: :ok | {:error, term()}
  def write_frame(device, frame) do
    # :file.write/2 is what IO.binwrite/2 wraps, minus the raise on error;
    # a transport wants the error back so it can report a dead pipe.
    :file.write(map_device(device), [frame, ?\n])
  end

  defp map_device(:stdio), do: :standard_io
  defp map_device(device), do: device

  @doc """
  Reads one line as raw bytes, newline included.
  """
  @spec read_line(IO.device()) :: {:ok, binary()} | :eof | {:error, term()}
  def read_line(device) do
    case IO.binread(map_device(device), :line) do
      line when is_binary(line) -> {:ok, line}
      :eof -> :eof
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Removes a UTF-8 byte-order mark from the start of the first frame.

  Some hosts and editors emit one at the start of a stream. It is not part
  of the frame, and a frame that starts with it is not valid JSON.
  """
  @spec strip_bom(binary()) :: binary()
  def strip_bom(@bom <> rest), do: rest
  def strip_bom(frame) when is_binary(frame), do: frame
end
