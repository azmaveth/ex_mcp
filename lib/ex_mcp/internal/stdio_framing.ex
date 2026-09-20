defmodule ExMCP.Internal.StdioFraming do
  @moduledoc false

  # The one place that knows how a JSON-RPC frame crosses an IO device.
  #
  # MCP and ACP frames are UTF-8 bytes, one per line. An Erlang IO device has
  # a character encoding of its own, picked from the process locale at VM
  # start: `:unicode` under a UTF-8 locale, `:latin1` under anything else,
  # including no locale at all. Mixing the two views corrupts frames: byte
  # reads on a unicode device return decoded codepoints as single bytes,
  # character reads on a latin1 device double-encode UTF-8, byte writes on a
  # unicode device double-encode, and character writes on a latin1 device
  # escape everything above U+00FF.
  #
  # Each view is byte-exact for valid UTF-8 as long as it matches the device:
  # characters on a unicode device, bytes on a latin1 device. So the transport
  # asks the device which it is and uses the matching calls. It does not try
  # to change the device: OTP 27 cannot switch a unicode-mode stdin to latin1
  # once the VM has started (reads then fail with `no_translation`), and the
  # device is shared with the rest of the VM anyway.
  #
  # The mode is consulted on every read and every write, not cached, because
  # the VM changes it behind the transport's back: when a unicode-mode stdio
  # meets input it cannot decode, OTP switches that device to latin1 for the
  # rest of its life (seen on OTP 27 and 28). One bad line from a peer would
  # otherwise corrupt every frame written after it. The query is a single
  # message to the device's owner, negligible next to encoding a frame.
  #
  # A read with the wrong view is not recoverable: the io server answers
  # `no_translation` and then reports end-of-file for the rest of the stream
  # (seen on OTP 27 and 28). So the view is chosen from the reported mode
  # and never probed.
  #
  # Both stdio transports use this module so the rule lives in one place.

  @bom <<0xEF, 0xBB, 0xBF>>

  @type mode :: :unicode | :latin1

  @doc """
  The encoding view to use for a device. Unknown or unqueryable devices are
  treated as byte devices.
  """
  @spec mode(IO.device()) :: mode()
  def mode(device) do
    case :io.getopts(map_device(device)) do
      opts when is_list(opts) ->
        case Keyword.get(opts, :encoding) do
          :unicode -> :unicode
          :utf8 -> :unicode
          _other -> :latin1
        end

      _error ->
        :latin1
    end
  end

  @doc "Writes one frame followed by a newline, byte-exact for valid UTF-8."
  @spec write_frame(IO.device(), iodata()) :: :ok | {:error, term()}
  def write_frame(device, frame), do: write_frame(device, mode(device), frame)

  @doc false
  @spec write_frame(IO.device(), mode(), iodata()) :: :ok | {:error, term()}
  def write_frame(device, :unicode, frame) do
    # :io.put_chars/2 is what IO.write/2 wraps; it may return {:error, _}
    # (a dead pipe, for instance) or raise on a bad device.
    :io.put_chars(map_device(device), [frame, ?\n])
  catch
    :error, reason -> {:error, reason}
    :exit, reason -> {:error, {:exit, reason}}
  end

  def write_frame(device, :latin1, frame) do
    :file.write(map_device(device), [frame, ?\n])
  end

  @doc "Reads one line, newline included, byte-exact for valid UTF-8."
  @spec read_line(IO.device()) :: {:ok, binary()} | :eof | {:error, term()}
  def read_line(device), do: read_line(device, mode(device))

  @doc false
  @spec read_line(IO.device(), mode()) :: {:ok, binary()} | :eof | {:error, term()}
  def read_line(device, :unicode), do: normalize_read(IO.read(map_device(device), :line))
  def read_line(device, :latin1), do: normalize_read(IO.binread(map_device(device), :line))

  @doc """
  Reads one unit: a whole character on a unicode device (one to four bytes),
  one byte on a latin1 device. A newline is always the single byte `"\\n"`.
  Callers that read a frame unit by unit query `mode/1` once per frame.
  """
  @spec read_unit(IO.device(), mode()) :: {:ok, binary()} | :eof | {:error, term()}
  def read_unit(device, :unicode), do: normalize_read(IO.read(map_device(device), 1))
  def read_unit(device, :latin1), do: normalize_read(IO.binread(map_device(device), 1))

  @doc """
  Removes a UTF-8 byte-order mark from the start of the first frame.

  Some hosts and editors emit one at the start of a stream. It is not part
  of the frame, and a frame that starts with it is not valid JSON.
  """
  @spec strip_bom(binary()) :: binary()
  def strip_bom(@bom <> rest), do: rest
  def strip_bom(frame) when is_binary(frame), do: frame

  defp normalize_read(data) when is_binary(data), do: {:ok, data}
  defp normalize_read(:eof), do: :eof
  defp normalize_read({:error, reason}), do: {:error, reason}
  # A device in list mode hands back charlists; keep the bytes.
  defp normalize_read(data) when is_list(data), do: {:ok, IO.iodata_to_binary(data)}

  # :stdio is an Elixir alias that Erlang's :io does not know.
  defp map_device(:stdio), do: :standard_io
  defp map_device(device), do: device
end
