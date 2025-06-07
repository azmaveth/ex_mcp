defmodule ExMCP.Transport.Beam.StreamChunk do
  @moduledoc """
  Represents a chunk of data in a BEAM transport stream.

  Each chunk contains:
  - Stream identification
  - Sequence number for ordering
  - Data payload
  - Metadata (timestamps, resource info, etc.)
  """

  defstruct [
    :stream_id,
    :sequence,
    :data,
    :is_final,
    :timestamp,
    :resource_uri,
    :error_type
  ]

  @type t :: %__MODULE__{
          stream_id: String.t(),
          sequence: non_neg_integer(),
          data: binary(),
          is_final: boolean(),
          timestamp: integer(),
          resource_uri: String.t() | nil,
          error_type: atom() | nil
        }

  @doc """
  Creates a new stream chunk.

  ## Parameters

  - `stream_id` - ID of the parent stream
  - `sequence` - Sequence number (starting from 1)
  - `data` - Binary data for this chunk
  - `is_final` - Whether this is the last chunk in the stream

  ## Examples

      {:ok, chunk} = StreamChunk.new("abc123", 1, "Hello", false)
      {:ok, final_chunk} = StreamChunk.new("abc123", 5, "World!", true)
  """
  @spec new(String.t(), non_neg_integer(), binary(), boolean()) ::
          {:ok, t()} | {:error, atom()}
  def new(stream_id, sequence, data, is_final)
      when is_binary(stream_id) and is_integer(sequence) and
             is_binary(data) and is_boolean(is_final) do
    chunk = %__MODULE__{
      stream_id: stream_id,
      sequence: sequence,
      data: data,
      is_final: is_final,
      timestamp: System.system_time(:millisecond),
      resource_uri: nil,
      error_type: nil
    }

    {:ok, chunk}
  end

  @doc """
  Validates that a chunk's data size is within limits.
  """
  @spec validate_size(t(), pos_integer()) :: :ok | {:error, :chunk_too_large}
  def validate_size(%__MODULE__{data: data}, max_size) do
    if byte_size(data) <= max_size do
      :ok
    else
      {:error, :chunk_too_large}
    end
  end

  @doc """
  Creates an acknowledgment message for a chunk.
  """
  @spec create_ack(t()) :: map()
  def create_ack(%__MODULE__{} = chunk) do
    %{
      "type" => "chunk_ack",
      "stream_id" => chunk.stream_id,
      "sequence" => chunk.sequence,
      "timestamp" => System.system_time(:millisecond)
    }
  end

  @doc """
  Converts a chunk to a wire format message.
  """
  @spec to_message(t()) :: map()
  def to_message(%__MODULE__{} = chunk) do
    base_message = %{
      "type" => "stream_chunk",
      "stream_id" => chunk.stream_id,
      "sequence" => chunk.sequence,
      "data" => Base.encode64(chunk.data),
      "is_final" => chunk.is_final,
      "timestamp" => chunk.timestamp
    }

    # Add optional fields
    base_message
    |> maybe_add_field("resource_uri", chunk.resource_uri)
    |> maybe_add_field("error_type", chunk.error_type)
  end

  @doc """
  Creates a chunk from a wire format message.
  """
  @spec from_message(map()) :: {:ok, t()} | {:error, atom()}
  def from_message(message) when is_map(message) do
    case Base.decode64(message["data"]) do
      {:ok, data} ->
        chunk = %__MODULE__{
          stream_id: message["stream_id"],
          sequence: message["sequence"],
          data: data,
          is_final: message["is_final"],
          timestamp: message["timestamp"],
          resource_uri: message["resource_uri"],
          error_type: message["error_type"] && String.to_atom(message["error_type"])
        }

        {:ok, chunk}

      :error ->
        {:error, :invalid_data_encoding}
    end
  end

  # Private functions

  defp maybe_add_field(map, _key, nil), do: map
  defp maybe_add_field(map, key, value), do: Map.put(map, key, value)
end
