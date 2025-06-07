defmodule ExMCP.Transport.Beam.Stream do
  @moduledoc """
  Stream management for BEAM transport large payload handling.

  Provides streaming capabilities for handling large MCP messages:
  - Stream initialization and metadata
  - Data chunking with configurable sizes
  - Flow control and backpressure handling
  - Resource streaming support
  """

  alias ExMCP.Transport.Beam.StreamChunk

  defstruct [
    :id,
    :content_type,
    :total_size,
    :chunk_size,
    :window_size,
    :timeout_ms,
    :resource_uri,
    :status,
    :created_at
  ]

  @type t :: %__MODULE__{
          id: String.t(),
          content_type: String.t(),
          total_size: non_neg_integer() | nil,
          chunk_size: pos_integer(),
          window_size: pos_integer(),
          timeout_ms: pos_integer(),
          resource_uri: String.t() | nil,
          status: :initialized | :streaming | :completed | :error | :timeout,
          created_at: integer()
        }

  @default_chunk_size 1024
  @default_window_size 5
  @default_timeout_ms 30_000

  @doc """
  Creates a new stream with the given options.

  ## Options

  - `:content_type` - MIME type of the content (required)
  - `:total_size` - Total size in bytes (optional)
  - `:chunk_size` - Size of each chunk in bytes (default: 1024)
  - `:window_size` - Flow control window size (default: 5)
  - `:timeout_ms` - Stream timeout in milliseconds (default: 30000)

  ## Examples

      {:ok, stream} = Stream.new(%{
        content_type: "application/json",
        total_size: 5000,
        chunk_size: 512
      })
  """
  @spec new(map()) :: {:ok, t()} | {:error, atom()}
  def new(opts) when is_map(opts) do
    with :ok <- validate_options(opts) do
      stream = %__MODULE__{
        id: generate_stream_id(),
        content_type: Map.fetch!(opts, :content_type),
        total_size: Map.get(opts, :total_size),
        chunk_size: Map.get(opts, :chunk_size, @default_chunk_size),
        window_size: Map.get(opts, :window_size, @default_window_size),
        timeout_ms: Map.get(opts, :timeout_ms, @default_timeout_ms),
        resource_uri: Map.get(opts, :resource_uri),
        status: :initialized,
        created_at: System.system_time(:millisecond)
      }

      {:ok, stream}
    end
  end

  @doc """
  Creates a new resource stream for streaming large resource content.

  ## Options

  - `:content_type` - MIME type of the resource (required)
  - `:resource_uri` - URI of the resource being streamed (required)
  - `:total_size` - Total size in bytes (optional)
  - `:chunk_size` - Size of each chunk in bytes (default: 1024)

  ## Examples

      {:ok, stream} = Stream.new_resource_stream(%{
        content_type: "application/yaml",
        resource_uri: "config://app/settings.yaml",
        total_size: 8192
      })
  """
  @spec new_resource_stream(map()) :: {:ok, t()} | {:error, atom()}
  def new_resource_stream(opts) when is_map(opts) do
    with :ok <- validate_resource_options(opts) do
      new(opts)
    end
  end

  @doc """
  Chunks data into a list of StreamChunk structs.

  ## Examples

      {:ok, stream} = Stream.new(%{content_type: "text/plain", chunk_size: 10})
      {:ok, chunks} = Stream.chunk_data(stream, "Hello, World!")
      
      # Returns list of chunks with sequence numbers
  """
  @spec chunk_data(t(), binary()) :: {:ok, [StreamChunk.t()]} | {:error, term()}
  def chunk_data(%__MODULE__{} = stream, data) when is_binary(data) do
    binary_chunks = chunk_binary(data, stream.chunk_size)
    total_chunks = length(binary_chunks)

    chunks =
      binary_chunks
      |> Enum.with_index(1)
      |> Enum.map(fn {chunk_data, sequence} ->
        is_final = sequence == total_chunks

        {:ok, chunk} = StreamChunk.new(stream.id, sequence, chunk_data, is_final)

        # Add resource URI if this is a resource stream
        if stream.resource_uri do
          %{chunk | resource_uri: stream.resource_uri}
        else
          chunk
        end
      end)

    {:ok, chunks}
  end

  @doc """
  Sends chunks with backpressure control.

  Returns the current window state and any pending data.
  """
  @spec send_chunks_with_backpressure(t(), binary()) ::
          {:ok, map()} | {:blocked, map()}
  def send_chunks_with_backpressure(%__MODULE__{} = stream, data) do
    {:ok, chunks} = chunk_data(stream, data)

    window_state = %{
      stream_id: stream.id,
      sent_count: 0,
      acked_count: 0,
      window_available: stream.window_size,
      pending_chunks: chunks,
      pending_data: nil
    }

    # Send chunks up to window size
    {sendable, remaining} = Enum.split(chunks, stream.window_size)

    updated_state = %{
      window_state
      | sent_count: length(sendable),
        window_available: stream.window_size - length(sendable),
        pending_chunks: remaining
    }

    if length(remaining) > 0 do
      {:blocked, %{updated_state | pending_data: data}}
    else
      {:ok, updated_state}
    end
  end

  @doc """
  Acknowledges a chunk and updates the window state.
  """
  @spec acknowledge_chunk(map(), pos_integer()) :: {:ok, map()}
  def acknowledge_chunk(window_state, sequence) when is_integer(sequence) do
    updated_state = %{
      window_state
      | acked_count: window_state.acked_count + 1,
        window_available: window_state.window_available + 1
    }

    {:ok, updated_state}
  end

  @doc """
  Handles stream errors by creating an error chunk.
  """
  @spec handle_stream_error(t(), atom()) :: {:error, atom(), StreamChunk.t()}
  def handle_stream_error(%__MODULE__{} = stream, error_type) do
    {:ok, error_chunk} = StreamChunk.new(stream.id, 0, "", true)
    error_chunk = %{error_chunk | error_type: error_type}

    {:error, error_type, error_chunk}
  end

  @doc """
  Checks the current status of a stream, including timeout detection.
  """
  @spec check_status(t()) :: {:ok, atom()} | {:error, atom()}
  def check_status(%__MODULE__{} = stream) do
    current_time = System.system_time(:millisecond)
    elapsed = current_time - stream.created_at

    if elapsed > stream.timeout_ms do
      {:error, :stream_timeout}
    else
      {:ok, stream.status}
    end
  end

  # Private functions

  defp generate_stream_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end

  defp validate_options(opts) do
    cond do
      not Map.has_key?(opts, :content_type) ->
        {:error, :missing_content_type}

      Map.get(opts, :chunk_size, @default_chunk_size) <= 0 ->
        {:error, :invalid_chunk_size}

      true ->
        :ok
    end
  end

  defp validate_resource_options(opts) do
    if not Map.has_key?(opts, :resource_uri) do
      {:error, :missing_resource_uri}
    else
      validate_options(opts)
    end
  end

  defp chunk_binary(data, chunk_size) do
    chunk_binary_helper(data, chunk_size, [])
  end

  defp chunk_binary_helper(<<>>, _chunk_size, acc), do: Enum.reverse(acc)

  defp chunk_binary_helper(data, chunk_size, acc) when byte_size(data) <= chunk_size do
    Enum.reverse([data | acc])
  end

  defp chunk_binary_helper(data, chunk_size, acc) do
    <<chunk::binary-size(chunk_size), rest::binary>> = data
    chunk_binary_helper(rest, chunk_size, [chunk | acc])
  end
end
