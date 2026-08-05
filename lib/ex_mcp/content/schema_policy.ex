defmodule ExMCP.Content.SchemaPolicy do
  @moduledoc """
  Fail-closed resource policy for JSON Schema compilation and validation.

  Cross-document `$ref` resolution is disabled by default before `ExJsonSchema`
  sees a schema, even if the host application configured ExJsonSchema's global
  remote resolver. Local fragment references remain supported. Network
  references can be enabled only through ExMCP's allowlisted, IP-pinned resolver.

  Schema size, structural depth, composition depth, total subschema count,
  resolution time, and validation time are bounded. Defaults can be adjusted
  with `config :ex_mcp, :json_schema, ...` or per call.

  The opt-in resolver revalidates every redirect and DNS result, rejects
  non-public addresses, pins the connection to an approved address, rejects
  compressed responses and proxies, and bounds response bytes, aggregate bytes,
  document count, reference depth, redirects, DNS, connection, and request time.
  Fetched schemas are scoped to one compilation and are never globally cached.
  """

  alias ExMCP.Content.{SchemaDNS, SchemaHTTPClient, SchemaRemoteResolver}

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

  @network_defaults [
    enabled: false,
    allowed_hosts: [],
    allow_http: false,
    max_redirects: 3,
    max_documents: 16,
    max_reference_depth: 8,
    max_response_bytes: 262_144,
    max_decompressed_bytes: 262_144,
    max_aggregate_bytes: 1_048_576,
    dns_timeout_ms: 1_000,
    connect_timeout_ms: 2_000,
    request_timeout_ms: 3_000,
    proxy: :disabled,
    trust_partition: "application",
    dns_resolver: SchemaDNS,
    http_client: SchemaHTTPClient
  ]

  @network_integer_options ~w(
    max_redirects
    max_documents
    max_reference_depth
    max_response_bytes
    max_decompressed_bytes
    max_aggregate_bytes
    dns_timeout_ms
    connect_timeout_ms
    request_timeout_ms
  )a

  @type policy_error ::
          :network_ref_forbidden
          | {:schema_limit_exceeded, atom(), non_neg_integer()}
          | {:schema_resolution_timeout, non_neg_integer()}
          | {:schema_validation_timeout, non_neg_integer()}
          | {:invalid_schema_policy_option, atom()}
          | {:invalid_schema, String.t()}
          | {:schema_validation_failed, String.t()}
          | {:network_schema_error, atom()}

  @type compile_result :: {:ok, term()} | {:error, policy_error()}
  @type validation_result :: :ok | {:error, term()}

  @doc "Returns the effective schema-policy options."
  @spec options(keyword()) :: keyword()
  def options(overrides \\ []) when is_list(overrides) do
    configured = Application.get_env(:ex_mcp, :json_schema, [])
    configured = if is_list(configured), do: configured, else: []

    network_options = merge_network_options(configured, overrides)

    @defaults
    |> Keyword.merge(Keyword.take(configured, Keyword.keys(@defaults)))
    |> Keyword.merge(Keyword.take(overrides, Keyword.keys(@defaults)))
    |> Keyword.put(:network_refs, network_options)
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
            resolve_schema(schema, opts)
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

  def format_error({:network_schema_error, reason}),
    do: "remote JSON Schema resolution failed: #{network_error_message(reason)}"

  defp ensure_compiled(%ExJsonSchema.Schema.Root{} = root, _opts), do: {:ok, root}
  defp ensure_compiled(schema, opts), do: compile(schema, opts)

  defp validate_options(opts) do
    with :ok <- validate_integer_options(opts, Keyword.keys(@defaults)) do
      validate_network_options(opts[:network_refs])
    end
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
          if key == "$ref" and network_refs_enabled?(opts),
            do: {:cont, {:ok, current_count}},
            else: {:halt, {:error, :network_ref_forbidden}}

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

  defp resolve_schema(schema, opts) do
    if network_refs_enabled?(opts),
      do: SchemaRemoteResolver.resolve(schema, opts),
      else: {:ok, ExJsonSchema.Schema.resolve(schema)}
  end

  defp network_refs_enabled?(opts), do: opts[:network_refs][:enabled] == true

  defp validate_integer_options(opts, keys) do
    Enum.reduce_while(keys, :ok, fn key, :ok ->
      value = Keyword.get(opts, key)

      if is_integer(value) and value >= 0,
        do: {:cont, :ok},
        else: {:halt, {:error, {:invalid_schema_policy_option, key}}}
    end)
  end

  defp validate_network_options(network_opts) when is_list(network_opts) do
    with :ok <- validate_integer_options(network_opts, @network_integer_options),
         :ok <- validate_boolean_option(network_opts, :enabled),
         :ok <- validate_boolean_option(network_opts, :allow_http),
         :ok <- validate_allowed_hosts(network_opts[:allowed_hosts]),
         :ok <- require_allowed_host_when_enabled(network_opts),
         :ok <- validate_fixed_option(network_opts, :proxy, :disabled),
         :ok <- validate_trust_partition(network_opts[:trust_partition]),
         :ok <- validate_callback(network_opts, :dns_resolver, 2) do
      validate_callback(network_opts, :http_client, 3)
    end
  end

  defp validate_network_options(_network_opts),
    do: {:error, {:invalid_schema_policy_option, :network_refs}}

  defp validate_boolean_option(opts, key) do
    if is_boolean(opts[key]), do: :ok, else: {:error, {:invalid_schema_policy_option, key}}
  end

  defp validate_allowed_hosts(hosts) when is_list(hosts) do
    valid? =
      Enum.all?(hosts, fn host ->
        is_binary(host) and host != "" and byte_size(host) <= 255 and
          String.match?(host, ~r/^(\*\.)?[A-Za-z0-9.:-]+$/)
      end)

    if valid?, do: :ok, else: {:error, {:invalid_schema_policy_option, :allowed_hosts}}
  end

  defp validate_allowed_hosts(_hosts),
    do: {:error, {:invalid_schema_policy_option, :allowed_hosts}}

  defp require_allowed_host_when_enabled(network_opts) do
    if network_opts[:enabled] and network_opts[:allowed_hosts] == [],
      do: {:error, {:invalid_schema_policy_option, :allowed_hosts}},
      else: :ok
  end

  defp validate_fixed_option(opts, key, expected) do
    if opts[key] == expected, do: :ok, else: {:error, {:invalid_schema_policy_option, key}}
  end

  defp validate_trust_partition(partition) when is_binary(partition) and partition != "", do: :ok

  defp validate_trust_partition(partition) when is_atom(partition) and not is_nil(partition),
    do: :ok

  defp validate_trust_partition(_partition),
    do: {:error, {:invalid_schema_policy_option, :trust_partition}}

  defp validate_callback(opts, key, arity) do
    callback = opts[key]

    valid? =
      is_function(callback, arity) or
        (is_atom(callback) and Code.ensure_loaded?(callback) and
           function_exported?(callback, callback_function(key), arity))

    if valid?,
      do: :ok,
      else: {:error, {:invalid_schema_policy_option, key}}
  end

  defp callback_function(:dns_resolver), do: :resolve
  defp callback_function(:http_client), do: :get

  defp merge_network_options(configured, overrides) do
    with {:ok, network_config} <- nested_options(configured, :network_refs),
         {:ok, network_overrides} <- nested_options(overrides, :network_refs) do
      @network_defaults
      |> Keyword.merge(Keyword.take(network_config, Keyword.keys(@network_defaults)))
      |> Keyword.merge(Keyword.take(network_overrides, Keyword.keys(@network_defaults)))
    else
      {:error, :invalid} -> :invalid
    end
  end

  defp nested_options(options, key) do
    case Keyword.fetch(options, key) do
      :error -> {:ok, []}
      {:ok, nested} when is_list(nested) -> {:ok, nested}
      {:ok, _invalid} -> {:error, :invalid}
    end
  end

  defp network_error_message(:host_not_allowed), do: "host is not allowlisted"
  defp network_error_message(:non_public_address), do: "target resolved to a non-public address"
  defp network_error_message(:scheme_not_allowed), do: "URI scheme is not allowed"
  defp network_error_message(:userinfo_forbidden), do: "URI userinfo is forbidden"
  defp network_error_message(:proxy_forbidden), do: "proxy use is disabled"
  defp network_error_message(:dns_timeout), do: "DNS resolution timed out"
  defp network_error_message(:dns_failed), do: "DNS resolution failed"
  defp network_error_message(:response_too_large), do: "response exceeded the byte limit"

  defp network_error_message(:decompressed_response_too_large),
    do: "response exceeded the decompressed-byte limit"

  defp network_error_message(:aggregate_response_too_large),
    do: "responses exceeded the aggregate-byte limit"

  defp network_error_message(:compressed_response), do: "compressed responses are rejected"
  defp network_error_message(:document_limit), do: "document count exceeded the limit"
  defp network_error_message(:reference_depth), do: "reference depth exceeded the limit"
  defp network_error_message(:reference_cycle), do: "cross-document reference cycle detected"
  defp network_error_message(:redirect_limit), do: "redirect count exceeded the limit"
  defp network_error_message(:redirect_cycle), do: "redirect cycle detected"
  defp network_error_message(:redirect_downgrade), do: "HTTPS-to-HTTP redirect was rejected"
  defp network_error_message(:unexpected_status), do: "server returned an unexpected status"
  defp network_error_message(:invalid_json_schema), do: "response was not a JSON Schema"
  defp network_error_message(:invalid_remote_schema), do: "remote schema was invalid"

  defp network_error_message(:relative_reference_without_base),
    do: "relative reference has no absolute base URI"

  defp network_error_message(_reason), do: "request was rejected"

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
