defmodule ExMCP.ACP.Agent.Transport.Stdio do
  @moduledoc """
  Server-side stdio transport for ACP agents.

  This transport reads JSON-RPC lines from this process' stdin and writes
  JSON-RPC lines to stdout. Logs and diagnostics must go to stderr.

  Frames are UTF-8 bytes. Each device is either a character device or a byte
  device, decided by how it was opened (for stdio, by the process locale)
  and subject to change by the VM, and the transport reads and writes it the
  way it is configured at that moment so nothing translates the frames.
  Embedders may pass their own `:input` and `:output` devices.
  """

  @behaviour ExMCP.ACP.Agent.Transport

  alias ExMCP.Internal.{Options, StdioFraming, StdioLoggerConfig}

  @default_max_frame_bytes 1_048_576
  @collector_chunk_bytes 4_096

  defstruct input: :stdio,
            output: :stdio,
            max_frame_bytes: @default_max_frame_bytes,
            closed?: false,
            first_frame?: true

  @impl true
  def connect(opts) do
    output = Keyword.get(opts, :output, :stdio)
    input = Keyword.get(opts, :input, :stdio)

    if output in [:stdio, :standard_io] do
      StdioLoggerConfig.configure()
    end

    {:ok,
     %__MODULE__{
       input: input,
       output: output,
       max_frame_bytes: Options.positive_integer(opts, :max_frame_bytes, @default_max_frame_bytes)
     }}
  end

  @impl true
  def send_message(message, %__MODULE__{max_frame_bytes: limit} = _state)
      when is_binary(message) and byte_size(message) > limit,
      do: {:error, :frame_too_large}

  def send_message(message, %__MODULE__{output: output} = state)
      when is_binary(message) do
    case StdioFraming.write_frame(output, message) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def receive_message(%__MODULE__{input: input} = state) do
    # The device's mode is consulted once per frame; see StdioFraming.
    read_frame(state, StdioFraming.mode(input), [], [], 0, 0)
  end

  # IO devices satisfy a fixed-size read only after receiving the requested
  # count. Reading a large chunk therefore deadlocks on a short NDJSON frame
  # while the peer keeps the pipe open. Single-unit reads (one byte on a byte
  # device, one character on a character device) preserve streaming semantics
  # and impose the limit before any unbounded line allocation. The collector
  # batches units into bounded binary chunks so the frame itself is built in
  # linear space; the limit and the chunking count bytes.
  defp read_frame(%__MODULE__{input: input} = state, mode, chunks, chunk, chunk_size, size) do
    case StdioFraming.read_unit(input, mode) do
      :eof ->
        finish_eof(state, chunks, chunk, size)

      {:error, reason} ->
        {:error, reason}

      {:ok, "\n"} ->
        finish_line(collect_frame(chunks, chunk), state)

      {:ok, unit} ->
        size = size + byte_size(unit)

        if size > state.max_frame_bytes do
          {:error, :frame_too_large}
        else
          chunk = [unit | chunk]
          chunk_size = chunk_size + byte_size(unit)

          if chunk_size >= @collector_chunk_bytes do
            completed_chunk = chunk |> Enum.reverse() |> IO.iodata_to_binary()
            read_frame(state, mode, [completed_chunk | chunks], [], 0, size)
          else
            read_frame(state, mode, chunks, chunk, chunk_size, size)
          end
        end
    end
  end

  # The first frame may carry a byte-order mark from the host; later ones cannot.
  defp finish_line(line, %__MODULE__{first_frame?: true} = state) do
    finish_line(StdioFraming.strip_bom(line), %{state | first_frame?: false})
  end

  defp finish_line(line, state) do
    case String.trim(line) do
      "" -> receive_message(state)
      message -> {:ok, message, state}
    end
  end

  defp finish_eof(_state, [], [], 0), do: {:error, :closed}

  defp finish_eof(%__MODULE__{} = state, chunks, chunk, _size) do
    finish_line(collect_frame(chunks, chunk), state)
  end

  defp collect_frame(chunks, chunk) do
    partial = chunk |> Enum.reverse() |> IO.iodata_to_binary()
    IO.iodata_to_binary(Enum.reverse([partial | chunks]))
  end

  @impl true
  def close(%__MODULE__{}), do: :ok

  @impl true
  def connected?(%__MODULE__{closed?: closed?}), do: not closed?
end
