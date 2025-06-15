defmodule ExMCP.Transport.Beam.Frame do
  @moduledoc """
  Frame protocol implementation for BEAM transport.

  Provides reliable message framing over TCP with length prefixes and version support.
  Format: | 4-byte Length | 1-byte Version | 1-byte MsgType | N-byte Payload |

  ## Security Features
  - Maximum frame size validation (prevents DoS attacks)
  - Version field for protocol evolution
  - Type field for message categorization

  ## Message Types
  - `:rpc_request` - Request requiring response
  - `:rpc_response` - Response to request
  - `:notification` - One-way message
  - `:heartbeat` - Keep-alive ping/pong
  - `:batch` - Multiple messages batched together
  """

  # 1MB maximum frame size
  @max_frame_size 1_048_576
  @current_version 1

  @type message_type :: :rpc_request | :rpc_response | :notification | :heartbeat | :batch
  @type frame :: %{
          version: integer(),
          type: message_type(),
          payload: binary()
        }

  @doc """
  Encodes a message into a framed binary.

  ## Examples

      iex> Frame.encode(%{method: "ping"}, 1, :heartbeat)
      {:ok, <<payload_size::32, 1::8, 3::8, encoded_payload::binary>>}
      
      iex> Frame.encode(%{very: :large_message}, 1, :rpc_request)
      {:error, :frame_too_large}
  """
  @spec encode(term(), integer(), message_type()) :: {:ok, binary()} | {:error, atom()}
  def encode(message, version \\ @current_version, type \\ :rpc_request) do
    with {:ok, payload} <- serialize_message(message),
         :ok <- validate_frame_size(payload) do
      type_byte = encode_message_type(type)
      # +2 for version and type bytes
      length = byte_size(payload) + 2

      frame = <<length::32, version::8, type_byte::8, payload::binary>>
      {:ok, frame}
    end
  end

  @doc """
  Decodes a framed binary into a message.

  ## Examples

      iex> Frame.decode(<<20::32, 1::8, 1::8, payload::binary>>)
      {:ok, %{version: 1, type: :rpc_request, payload: decoded_message}}
  """
  @spec decode(binary()) :: {:ok, frame()} | {:error, atom()}
  def decode(<<length::32, version::8, type_byte::8, payload::binary>>) do
    # Subtract version and type bytes
    expected_payload_size = length - 2

    cond do
      length > @max_frame_size ->
        {:error, :frame_too_large}

      byte_size(payload) != expected_payload_size ->
        {:error, :incomplete_frame}

      true ->
        case {decode_message_type(type_byte), deserialize_message(payload)} do
          {:unknown, _} ->
            {:error, :unknown_message_type}

          {_type, {:error, reason}} ->
            {:error, {:deserialization_failed, reason}}

          {decoded_type, {:ok, message}} ->
            {:ok, %{version: version, type: decoded_type, payload: message}}
        end
    end
  end

  def decode(_binary), do: {:error, :invalid_frame_format}

  @doc """
  Validates that a frame size is within acceptable limits.
  """
  @spec validate_frame_size(binary()) :: :ok | {:error, :frame_too_large}
  def validate_frame_size(payload) when byte_size(payload) > @max_frame_size do
    {:error, :frame_too_large}
  end

  def validate_frame_size(_), do: :ok

  @doc """
  Gets the maximum allowed frame size.
  """
  @spec max_frame_size() :: integer()
  def max_frame_size, do: @max_frame_size

  @doc """
  Gets the current protocol version.
  """
  @spec current_version() :: integer()
  def current_version, do: @current_version

  # Private functions

  defp serialize_message(message) do
    binary = :erlang.term_to_binary(message, [:compressed])
    {:ok, binary}
  rescue
    error -> {:error, {:serialization_exception, error}}
  end

  defp deserialize_message(binary) when is_binary(binary) do
    term = :erlang.binary_to_term(binary, [:safe])
    {:ok, term}
  rescue
    error -> {:error, {:deserialization_exception, error}}
  end

  defp encode_message_type(:rpc_request), do: 1
  defp encode_message_type(:rpc_response), do: 2
  defp encode_message_type(:notification), do: 3
  defp encode_message_type(:heartbeat), do: 4
  defp encode_message_type(:batch), do: 5

  defp decode_message_type(1), do: :rpc_request
  defp decode_message_type(2), do: :rpc_response
  defp decode_message_type(3), do: :notification
  defp decode_message_type(4), do: :heartbeat
  defp decode_message_type(5), do: :batch
  defp decode_message_type(_), do: :unknown
end
