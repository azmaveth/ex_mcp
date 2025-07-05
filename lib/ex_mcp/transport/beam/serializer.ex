defmodule ExMCP.Transport.Beam.Serializer do
  @moduledoc """
  Serialization/deserialization for BEAM transport.

  Supports multiple serialization formats optimized for different scenarios:
  - ETF (Erlang Term Format): Maximum performance for BEAM-to-BEAM communication
  - JSON: Cross-platform compatibility when needed
  - Protobuf: Future enhancement for schema evolution

  ## ETF Advantages
  - Native BEAM optimization provides fastest serialization
  - Zero schema compilation required
  - Supports all Erlang/Elixir data types natively
  - Leverages "free serialization" in BEAM VM

  ## Security
  - Uses `:safe` and `:used` options for ETF deserialization to prevent code injection
  - Enforces strict size limits (10MB max) to prevent resource exhaustion
  - Monitors atom table usage to prevent atom exhaustion attacks
  - Detects trailing data in ETF binaries to identify potential attacks
  - Handles serialization errors gracefully with detailed error reporting
  """

  @type format :: :etf | :json | :protobuf
  @type serialization_result :: {:ok, binary()} | {:error, term()}
  @type deserialization_result :: {:ok, term()} | {:error, term()}

  # Security limits for ETF deserialization
  # 10MB limit for ETF binaries
  @max_etf_size 10 * 1024 * 1024
  # Limit new atoms created per deserialization
  @max_atoms_per_deserialize 100

  @doc """
  Serializes a message using the specified format.

  ## Examples

      iex> Serializer.serialize(%{method: "ping"}, :etf)
      {:ok, <<131, 116, 0, 0, 0, 1, 100, 0, 6, 109, 101, 116, 104, 111, 100, 109, 0, 0, 0, 4, 112, 105, 110, 103>>}

      iex> Serializer.serialize(%{method: "ping"}, :json)
      {:ok, "{\"method\":\"ping\"}"}
  """
  @spec serialize(term(), format()) :: serialization_result()
  def serialize(message, :etf) do
    binary = :erlang.term_to_binary(message, [:compressed])
    {:ok, binary}
  rescue
    error -> {:error, {:etf_serialization_failed, error}}
  end

  def serialize(message, :json) do
    case Jason.encode(message) do
      {:ok, json} -> {:ok, json}
      {:error, reason} -> {:error, {:json_serialization_failed, reason}}
    end
  end

  def serialize(_message, :protobuf) do
    # Future enhancement - placeholder for now
    {:error, :protobuf_not_implemented}
  end

  @doc """
  Deserializes a binary using the specified format.

  ## Examples

      iex> Serializer.deserialize(binary, :etf)
      {:ok, %{method: "ping"}}

      iex> Serializer.deserialize("{\"method\":\"ping\"}", :json)
      {:ok, %{"method" => "ping"}}
  """
  @spec deserialize(binary(), format()) :: deserialization_result()
  def deserialize(binary, :etf) when is_binary(binary) do
    with :ok <- validate_etf_size(binary),
         {:ok, initial_atom_count} <- get_current_atom_count(),
         {:ok, term} <- safe_etf_deserialize(binary),
         :ok <- validate_atom_usage(initial_atom_count) do
      {:ok, term}
    end
  end

  def deserialize(json, :json) when is_binary(json) do
    case Jason.decode(json) do
      {:ok, term} -> {:ok, term}
      {:error, reason} -> {:error, {:json_deserialization_failed, reason}}
    end
  end

  def deserialize(_binary, :protobuf) do
    # Future enhancement - placeholder for now
    {:error, :protobuf_not_implemented}
  end

  def deserialize(_, format) do
    {:error, {:invalid_input_for_format, format}}
  end

  @doc """
  Determines the best serialization format for the given scenario.

  ## Examples

      iex> Serializer.optimal_format(:local, :beam_to_beam)
      :etf

      iex> Serializer.optimal_format(:distributed, :cross_platform)
      :json
  """
  @spec optimal_format(atom(), atom()) :: format()
  def optimal_format(:local, :beam_to_beam), do: :etf
  def optimal_format(:local, :cross_platform), do: :json
  def optimal_format(:distributed, _), do: :json
  def optimal_format(_, _), do: :json

  @doc """
  Estimates the serialized size of a message without actually serializing it.
  Useful for optimization decisions (zero-copy thresholds, etc.).
  """
  @spec estimate_size(term(), format()) :: integer()
  def estimate_size(message, :etf) do
    # ETF size estimation - rough approximation
    case message do
      map when is_map(map) ->
        map_size(map) * 20 +
          Enum.reduce(map, 0, fn {k, v}, acc ->
            acc + estimate_term_size(k) + estimate_term_size(v)
          end)

      list when is_list(list) ->
        length(list) * 10 +
          Enum.reduce(list, 0, fn item, acc ->
            acc + estimate_term_size(item)
          end)

      term ->
        estimate_term_size(term)
    end
  end

  def estimate_size(message, :json) do
    # JSON size estimation
    case Jason.encode(message) do
      {:ok, json} -> byte_size(json)
      _ -> 0
    end
  end

  def estimate_size(_, :protobuf), do: 0

  @doc """
  Checks if a format is available and properly configured.
  """
  @spec format_available?(format()) :: boolean()
  def format_available?(:etf), do: true
  def format_available?(:json), do: Code.ensure_loaded?(Jason)
  # Not implemented yet
  def format_available?(:protobuf), do: false

  @doc """
  Lists all available serialization formats.
  """
  @spec available_formats() :: [format()]
  def available_formats do
    [:etf, :json, :protobuf]
    |> Enum.filter(&format_available?/1)
  end

  # Private helper functions

  defp estimate_term_size(binary) when is_binary(binary), do: byte_size(binary)
  defp estimate_term_size(atom) when is_atom(atom), do: byte_size(Atom.to_string(atom))
  defp estimate_term_size(integer) when is_integer(integer), do: 8
  defp estimate_term_size(float) when is_float(float), do: 8
  defp estimate_term_size(pid) when is_pid(pid), do: 12
  defp estimate_term_size(ref) when is_reference(ref), do: 16
  # Default estimate
  defp estimate_term_size(_), do: 8

  # Security helper functions for ETF deserialization

  defp validate_etf_size(binary) do
    if byte_size(binary) <= @max_etf_size do
      :ok
    else
      {:error, {:etf_binary_too_large, byte_size(binary), @max_etf_size}}
    end
  end

  defp get_current_atom_count do
    {:ok, :erlang.system_info(:atom_count)}
  rescue
    # Fallback if atom count unavailable
    _ -> {:ok, 0}
  end

  defp safe_etf_deserialize(binary) do
    # Use both :safe and :used options for maximum security
    # :safe prevents code injection, :used detects trailing data
    case :erlang.binary_to_term(binary, [:safe, :used]) do
      {term, bytes_used} when bytes_used == byte_size(binary) ->
        {:ok, term}

      {_term, bytes_used} ->
        # Trailing data detected - potential attack or corruption
        {:error, {:etf_trailing_data, bytes_used, byte_size(binary)}}
    end
  rescue
    error -> {:error, {:etf_deserialization_failed, error}}
  end

  defp validate_atom_usage(initial_atom_count) do
    current_atom_count = :erlang.system_info(:atom_count)
    atoms_created = current_atom_count - initial_atom_count

    if atoms_created <= @max_atoms_per_deserialize do
      :ok
    else
      {:error, {:too_many_atoms_created, atoms_created, @max_atoms_per_deserialize}}
    end
  rescue
    # Fallback if atom count check fails
    _ -> :ok
  end
end
