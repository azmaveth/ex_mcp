defmodule ExMCP.ACP.Agent.Transport.Stdio do
  @moduledoc """
  Server-side stdio transport for ACP agents.

  This transport reads JSON-RPC lines from this process' stdin and writes
  JSON-RPC lines to stdout. Logs and diagnostics must go to stderr.
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

    # Frames are UTF-8 bytes. Pin both devices to byte mode before the first
    # read or write so the process locale cannot translate them. Embedders
    # and tests may pass their own devices; StringIO and files accept this.
    with :ok <- StdioFraming.pin_byte_mode(input),
         :ok <- pin_output(output, input) do
      {:ok,
       %__MODULE__{
         input: input,
         output: output,
         max_frame_bytes:
           Options.positive_integer(opts, :max_frame_bytes, @default_max_frame_bytes)
       }}
    else
      {:error, reason} -> {:error, {:byte_mode_unavailable, reason}}
    end
  end

  defp pin_output(output, input) when output == input, do: :ok
  defp pin_output(output, _input), do: StdioFraming.pin_byte_mode(output)

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
  def receive_message(%__MODULE__{} = state) do
    read_frame(state, [], [], 0, 0)
  end

  # IO devices satisfy a fixed-size read only after receiving the requested byte
  # count. Reading a large chunk therefore deadlocks on a short NDJSON frame while
  # the peer keeps the pipe open. One-byte reads preserve streaming semantics and
  # impose the limit before any unbounded line allocation. The collector batches
  # bytes into bounded binary chunks so the frame itself is built in linear space.
  defp read_frame(%__MODULE__{input: input} = state, chunks, chunk, chunk_size, size) do
    case IO.binread(input, 1) do
      :eof ->
        finish_eof(state, chunks, chunk, size)

      {:error, reason} ->
        {:error, reason}

      "\n" ->
        finish_line(collect_frame(chunks, chunk), state)

      byte when is_binary(byte) ->
        size = size + 1

        if size > state.max_frame_bytes do
          {:error, :frame_too_large}
        else
          chunk = [byte | chunk]
          chunk_size = chunk_size + 1

          if chunk_size == @collector_chunk_bytes do
            completed_chunk = chunk |> Enum.reverse() |> IO.iodata_to_binary()
            read_frame(state, [completed_chunk | chunks], [], 0, size)
          else
            read_frame(state, chunks, chunk, chunk_size, size)
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
