defmodule ExMCP.Content.SchemaPolicy do
  @moduledoc """
  Fail-closed resource policy for JSON Schema compilation and validation.

  Cross-document `$ref` resolution is disabled before `ExJsonSchema` sees a
  schema, even if the host application configured ExJsonSchema's global remote
  resolver. Local fragment references remain supported.

  Schema size, structural depth, composition depth, total subschema count,
  resolution time, and validation time are bounded. Defaults can be adjusted
  with `config :ex_mcp, :json_schema, ...` or per call.

  Safe network schema fetching is deliberately not implemented here. Enabling
  it requires redirect and DNS/IP revalidation, an allowlist, response limits,
  and an explicit proxy policy; until that complete boundary exists, remote
  references remain unavailable.
  """

  @composition_keywords MapSet.new(~w(allOf anyOf oneOf not if then else))
  @literal_keywords MapSet.new(~w(const default enum examples))
  @reference_keywords MapSet.new(~w($ref $recursiveRef $dynamicRef))

  @defaults [
    max_schema_bytes: 262_144,
    max_schema_depth: 64,
    max_subschemas: 1_000,
    max_composition_depth: 16,
    resolve_timeout_ms: 1_000,
    validation_timeout_ms: 100
  ]

  @type policy_error ::
          :network_ref_forbidden
          | {:schema_limit_exceeded, atom(), non_neg_integer()}
          | {:schema_resolution_timeout, non_neg_integer()}
          | {:schema_validation_timeout, non_neg_integer()}
          | {:invalid_schema_policy_option, atom()}
          | {:invalid_schema, String.t()}
          | {:schema_validation_failed, String.t()}

  @type compile_result :: {:ok, term()} | {:error, policy_error()}
  @type validation_result :: :ok | {:error, term()}

  @doc "Returns the effective schema-policy options."
  @spec options(keyword()) :: keyword()
  def options(overrides \\ []) when is_list(overrides) do
    configured = Application.get_env(:ex_mcp, :json_schema, [])
    configured = if is_list(configured), do: configured, else: []

    @defaults
    |> Keyword.merge(Keyword.take(configured, Keyword.keys(@defaults)))
    |> Keyword.merge(Keyword.take(overrides, Keyword.keys(@defaults)))
  end

  @doc "Normalizes atom-keyed schema terms to their JSON wire representation."
  @spec json_compatible(term()) :: term()
  def json_compatible(map) when is_map(map) and not is_struct(map) do
    Map.new(map, fn {key, value} -> {json_key(key), json_compatible(value)} end)
  end

  def json_compatible(list) when is_list(list), do: Enum.map(list, &json_compatible/1)

  def json_compatible(value) when is_atom(value) and value not in [true, false, nil],
    do: Atom.to_string(value)

  def json_compatible(value), do: value

  @doc "Checks a raw schema without resolving references."
  @spec preflight(map() | boolean(), keyword()) :: :ok | {:error, policy_error()}
  def preflight(schema, opts \\ [])

  def preflight(schema, opts)
      when (is_map(schema) and not is_struct(schema)) or is_boolean(schema) do
    schema = json_compatible(schema)
    opts = options(opts)

    with :ok <- validate_options(opts),
         :ok <- check_encoded_size(schema, opts),
         {:ok, _count} <- walk(schema, 0, 0, 0, opts) do
      :ok
    end
  end

  def preflight(_schema, _opts),
    do: {:error, {:invalid_schema, "schema must be an object or boolean"}}

  @doc "Preflights and resolves a schema within the configured deadline."
  @spec compile(map() | boolean(), keyword()) :: compile_result()
  def compile(schema, opts \\ []) do
    schema = json_compatible(schema)
    opts = options(opts)

    with :ok <- preflight(schema, opts) do
      run_bounded(
        fn ->
          try do
            {:ok, ExJsonSchema.Schema.resolve(schema)}
          rescue
            exception -> {:error, {:invalid_schema, Exception.message(exception)}}
          catch
            _kind, _reason -> {:error, {:invalid_schema, "schema resolution failed"}}
          end
        end,
        opts[:resolve_timeout_ms],
        {:schema_resolution_timeout, opts[:resolve_timeout_ms]}
      )
    end
  end

  @doc "Validates data with a resolved or raw schema within a hard deadline."
  @spec validate(term(), term(), keyword()) :: validation_result()
  def validate(data, schema, opts \\ []) do
    opts = options(opts)

    with :ok <- validate_options(opts),
         {:ok, resolved} <- ensure_compiled(schema, opts) do
      run_bounded(
        fn ->
          try do
            ExJsonSchema.Validator.validate(resolved, data)
          rescue
            exception ->
              {:error, {:schema_validation_failed, Exception.message(exception)}}
          catch
            _kind, _reason ->
              {:error, {:schema_validation_failed, "schema validation failed"}}
          end
        end,
        opts[:validation_timeout_ms],
        {:schema_validation_timeout, opts[:validation_timeout_ms]}
      )
    end
  end

  @doc "Formats a policy failure without including remote-reference values."
  @spec format_error(policy_error()) :: String.t()
  def format_error(:network_ref_forbidden),
    do: "network and cross-document JSON Schema references are disabled"

  def format_error({:schema_limit_exceeded, limit, value}),
    do: "JSON Schema exceeds #{limit} (observed #{value})"

  def format_error({:schema_resolution_timeout, timeout}),
    do: "JSON Schema resolution exceeded #{timeout}ms"

  def format_error({:schema_validation_timeout, timeout}),
    do: "JSON Schema validation exceeded #{timeout}ms"

  def format_error({:invalid_schema_policy_option, option}),
    do: "JSON Schema policy option #{option} is invalid"

  def format_error({:invalid_schema, message}), do: "invalid JSON Schema: #{message}"

  def format_error({:schema_validation_failed, message}),
    do: "JSON Schema validation failed: #{message}"

  defp ensure_compiled(%ExJsonSchema.Schema.Root{} = root, _opts), do: {:ok, root}
  defp ensure_compiled(schema, opts), do: compile(schema, opts)

  defp validate_options(opts) do
    Enum.reduce_while(@defaults, :ok, fn {key, _default}, :ok ->
      value = Keyword.get(opts, key)

      valid? = is_integer(value) and value >= 0

      if valid?,
        do: {:cont, :ok},
        else: {:halt, {:error, {:invalid_schema_policy_option, key}}}
    end)
  end

  defp check_encoded_size(schema, opts) do
    case Jason.encode(schema) do
      {:ok, encoded} ->
        size = byte_size(encoded)
        limit = opts[:max_schema_bytes]

        if size <= limit,
          do: :ok,
          else: {:error, {:schema_limit_exceeded, :max_schema_bytes, size}}

      {:error, _reason} ->
        invalid_json_schema()
    end
  rescue
    _exception -> invalid_json_schema()
  end

  defp invalid_json_schema, do: {:error, {:invalid_schema, "schema is not JSON-compatible"}}

  defp walk(value, depth, composition_depth, count, opts) when is_map(value) do
    count = count + 1

    cond do
      depth > opts[:max_schema_depth] ->
        {:error, {:schema_limit_exceeded, :max_schema_depth, depth}}

      composition_depth > opts[:max_composition_depth] ->
        {:error, {:schema_limit_exceeded, :max_composition_depth, composition_depth}}

      count > opts[:max_subschemas] ->
        {:error, {:schema_limit_exceeded, :max_subschemas, count}}

      true ->
        walk_map_children(value, depth, composition_depth, count, opts)
    end
  end

  defp walk(values, depth, composition_depth, count, opts) when is_list(values) do
    if depth > opts[:max_schema_depth] do
      {:error, {:schema_limit_exceeded, :max_schema_depth, depth}}
    else
      Enum.reduce_while(values, {:ok, count}, fn child, {:ok, current_count} ->
        case walk(child, depth + 1, composition_depth, current_count, opts) do
          {:ok, next_count} -> {:cont, {:ok, next_count}}
          {:error, _reason} = error -> {:halt, error}
        end
      end)
    end
  end

  defp walk(_value, _depth, _composition_depth, count, _opts), do: {:ok, count}

  defp walk_map_children(value, depth, composition_depth, count, opts) do
    Enum.reduce_while(value, {:ok, count}, fn {key, child}, {:ok, current_count} ->
      cond do
        MapSet.member?(@literal_keywords, key) ->
          {:cont, {:ok, current_count}}

        MapSet.member?(@reference_keywords, key) and external_ref?(child) ->
          {:halt, {:error, :network_ref_forbidden}}

        true ->
          next_composition_depth =
            if MapSet.member?(@composition_keywords, key),
              do: composition_depth + 1,
              else: composition_depth

          case walk(child, depth + 1, next_composition_depth, current_count, opts) do
            {:ok, next_count} -> {:cont, {:ok, next_count}}
            {:error, _reason} = error -> {:halt, error}
          end
      end
    end)
  end

  defp external_ref?(ref) when is_binary(ref), do: ref != "" and not String.starts_with?(ref, "#")
  defp external_ref?(_ref), do: true

  defp run_bounded(fun, timeout, timeout_error) do
    task = Task.async(fun)

    case Task.yield(task, timeout) do
      {:ok, result} ->
        result

      {:exit, _reason} ->
        {:error, {:schema_validation_failed, "schema worker exited"}}

      nil ->
        Task.shutdown(task, :brutal_kill)
        {:error, timeout_error}
    end
  end

  defp json_key(key) when is_atom(key), do: Atom.to_string(key)
  defp json_key(key) when is_binary(key), do: key
  defp json_key(key), do: key
end
