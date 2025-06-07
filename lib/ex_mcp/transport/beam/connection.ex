defmodule ExMCP.Transport.Beam.Connection do
  @moduledoc """
  Connection management for BEAM transport streaming.

  Manages multiple concurrent streams over a single BEAM transport connection:
  - Stream lifecycle management
  - Resource limits and cleanup
  - Memory management
  """

  alias ExMCP.Transport.Beam.Stream

  defstruct [
    :id,
    :max_concurrent_streams,
    :active_streams,
    :completed_streams,
    :created_at
  ]

  @type t :: %__MODULE__{
          id: String.t(),
          max_concurrent_streams: pos_integer(),
          active_streams: map(),
          completed_streams: MapSet.t(),
          created_at: integer()
        }

  @default_max_streams 10

  @doc """
  Creates a new connection with stream management.

  ## Options

  - `:max_concurrent_streams` - Maximum number of concurrent streams (default: 10)

  ## Examples

      {:ok, connection} = Connection.new(%{max_concurrent_streams: 5})
  """
  @spec new(map()) :: {:ok, t()}
  def new(opts \\ %{}) do
    connection = %__MODULE__{
      id: generate_connection_id(),
      max_concurrent_streams: Map.get(opts, :max_concurrent_streams, @default_max_streams),
      active_streams: %{},
      completed_streams: MapSet.new(),
      created_at: System.system_time(:millisecond)
    }

    {:ok, connection}
  end

  @doc """
  Creates a new stream within this connection.

  Returns an error if the maximum number of concurrent streams is exceeded.
  """
  @spec create_stream(t(), map()) :: {:ok, Stream.t(), t()} | {:error, atom()}
  def create_stream(%__MODULE__{} = connection, stream_opts) do
    if map_size(connection.active_streams) >= connection.max_concurrent_streams do
      {:error, :max_streams_exceeded}
    else
      case Stream.new(stream_opts) do
        {:ok, stream} ->
          updated_connection = %{
            connection
            | active_streams: Map.put(connection.active_streams, stream.id, stream)
          }

          {:ok, stream, updated_connection}

        error ->
          error
      end
    end
  end

  @doc """
  Marks a stream as completed and cleans it up.
  """
  @spec complete_stream(t(), String.t()) :: {:ok, t()}
  def complete_stream(%__MODULE__{} = connection, stream_id) do
    updated_connection = %{
      connection
      | active_streams: Map.delete(connection.active_streams, stream_id),
        completed_streams: MapSet.put(connection.completed_streams, stream_id)
    }

    {:ok, updated_connection}
  end

  @doc """
  Gets information about a specific stream.
  """
  @spec get_stream(t(), String.t()) :: {:ok, Stream.t()} | {:error, :stream_not_found}
  def get_stream(%__MODULE__{} = connection, stream_id) do
    case Map.get(connection.active_streams, stream_id) do
      nil -> {:error, :stream_not_found}
      stream -> {:ok, stream}
    end
  end

  @doc """
  Lists all active streams in the connection.
  """
  @spec list_active_streams(t()) :: [Stream.t()]
  def list_active_streams(%__MODULE__{} = connection) do
    Map.values(connection.active_streams)
  end

  @doc """
  Cleans up timed out streams.
  """
  @spec cleanup_timed_out_streams(t()) :: {:ok, t(), [String.t()]}
  def cleanup_timed_out_streams(%__MODULE__{} = connection) do
    current_time = System.system_time(:millisecond)

    {timed_out, active} =
      Enum.split_with(connection.active_streams, fn {_id, stream} ->
        elapsed = current_time - stream.created_at
        elapsed > stream.timeout_ms
      end)

    timed_out_ids = Enum.map(timed_out, fn {id, _stream} -> id end)

    updated_connection = %{
      connection
      | active_streams: Map.new(active),
        completed_streams:
          Enum.reduce(timed_out_ids, connection.completed_streams, &MapSet.put(&2, &1))
    }

    {:ok, updated_connection, timed_out_ids}
  end

  @doc """
  Gets connection statistics.
  """
  @spec get_stats(t()) :: map()
  def get_stats(%__MODULE__{} = connection) do
    %{
      connection_id: connection.id,
      active_streams: map_size(connection.active_streams),
      completed_streams: MapSet.size(connection.completed_streams),
      max_concurrent_streams: connection.max_concurrent_streams,
      utilization: map_size(connection.active_streams) / connection.max_concurrent_streams,
      uptime_ms: System.system_time(:millisecond) - connection.created_at
    }
  end

  # Private functions

  defp generate_connection_id do
    :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
  end
end
