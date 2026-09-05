defmodule ExMCP.Test.CodexGolden do
  @moduledoc """
  Golden-transcript harness for characterizing `ExMCP.ACP.Adapters.Codex`.

  The Codex adapter is a pure translation layer: every step below drives one
  public adapter function (`init/1`, `post_connect/1`, `translate_outbound/2`,
  `translate_inbound/2`) with the state threaded from the previous step. The
  harness records what the adapter *did* at each step (writes to the
  app-server, ACP messages, replies, errors) and never records the adapter
  state itself, so fixtures pin wire behavior rather than internal state
  layout.

  ## Steps

    * `{:init, opts}` - `Codex.init(opts)`. When the first step is not `:init`
      the default options from `default_init_opts/0` are used.
    * `:post_connect` - `Codex.post_connect(state)`.
    * `{:outbound, map | fun}` - `Codex.translate_outbound(map, state)`.
    * `{:inbound, map | fun}` - `Codex.translate_inbound(Jason.encode!(map), state)`.
    * `{:inbound_raw, string}` - `Codex.translate_inbound(string, state)` verbatim.
    * `{:note, text}` - recorded verbatim, no adapter call.

  A `fun` receives the *raw* transcript recorded so far (real ids intact) and
  returns the map to use; the resolved map is what gets recorded.

  ## Transcript entries

  Each entry is an `ExMCP.Test.CodexGolden.Entry` struct (a map with `:step`
  and `:result`), so fixtures render the cause (`step`) before its effect
  (`result`). `:result` holds only the keys that apply: `tag` (the adapter's
  return tag), `writes`, `messages`, `reply`, `error`, `skipped`.

  ## Wire framing

  App-server writes are recorded as JSON-decoded maps for readability, but
  the NDJSON framing produced by `ExMCP.ACP.Adapters.Codex.Protocol.line/1`
  is enforced on every write before decoding: the iodata must be empty or
  consist of JSON objects each terminated by exactly one `"\\n"`. A write
  whose final object lacks its terminator, or that contains an empty line,
  raises rather than silently producing the same transcript, because the
  app-server would not see such a message until the next line arrived.

  The `ExMCP.ACP.Adapter` behaviour also allows `{:ok, :pending, state}`,
  `{:one_shot, fun, state}`, and `{:partial, state}` results. The Codex
  adapter never returns any of them: it is not adapter-managed, and
  `ExMCP.ACP.AdapterBridge` buffers and splits the subprocess output so
  `translate_inbound/2` always receives one complete line. `normalize_result/1`
  deliberately has no clauses for them (Dialyzer rejects unreachable ones); a
  step that produced one would raise a `FunctionClauseError` here, which is
  the right signal to extend the harness (a `one_shot` fun owns a subprocess
  lifecycle and would have to be observed rather than invoked).

  ## Normalization

  The adapter mints ACP request ids with `System.unique_integer/1`
  (`"codex-permission-123"`, `"codex-elicitation-456"`, ...). Before a
  transcript is returned, written, or compared, every such id is replaced by
  a stable placeholder that preserves identity within the transcript: the
  first distinct permission id becomes `"codex-permission-<1>"`, the next
  `"codex-permission-<2>"`, and the same real id always maps to the same
  placeholder. Entries are walked in order, each entry's `step` before its
  `result`, and inside a map the (sorted) keys are walked as well as the
  values, so placeholders are assigned in a deterministic order and a
  generated id used as a key cannot leak. Anonymous functions in recorded
  init options are replaced by `:__fun__` so fixtures stay evaluable.

  ## Environment

  `MODEL_PROVIDER`, `CODEX_CONFIG` and `NO_BROWSER` change the adapter's wire
  output and cannot be overridden through init options, so `run/2` fails with
  a clear message when any of them is set. Scenarios that need those
  behaviors pass `model_provider:`, `codex_config:` or `no_browser:` to
  `{:init, opts}` instead.

  ## Fixtures

  `assert_golden/4` compares the transcript with
  `test/fixtures/acp/codex/<area>/<name>.term`. Run the test with
  `CODEX_GOLDEN=update` to (re)write the fixture; that run always fails so
  regeneration is a deliberate, reviewed act.
  """

  import ExUnit.Assertions, only: [assert: 1, flunk: 1]

  alias ExMCP.ACP.Adapters.Codex

  defmodule Entry do
    @moduledoc "One transcript entry: the step as executed and the adapter's normalized result."
    defstruct [:step, :result]

    @type t :: %__MODULE__{step: map(), result: map()}
  end

  @fixture_root Path.expand("../../fixtures/acp/codex", __DIR__)

  # Environment variables the adapter reads directly (with no init-option
  # override precedence) that change its wire output.
  @leaky_env_vars ~w(MODEL_PROVIDER CODEX_CONFIG NO_BROWSER)

  # Real ids as minted by the adapter (digits only); used for normalization.
  @generated_id_pattern ~r/(?<![\w-])codex-(auth-elicitation|permission|elicitation|user-input|login)-(\d+)(?![\w-])/

  # Real ids or their `<n>` placeholders; used by `generated_ids/1` so it works
  # on raw and normalized transcripts alike.
  @any_id_pattern ~r/(?<![\w-])codex-(?:auth-elicitation|permission|elicitation|user-input|login)-(?:\d+|<\d+>)(?![\w-])/

  @type step ::
          {:init, keyword()}
          | :post_connect
          | {:outbound, map() | (transcript() -> map())}
          | {:inbound, map() | (transcript() -> map())}
          | {:inbound_raw, String.t()}
          | {:note, String.t()}

  @type entry :: Entry.t()
  @type transcript :: [entry()]

  @doc "Default `Codex.init/1` options used when a scenario does not start with `{:init, opts}`."
  @spec default_init_opts() :: keyword()
  def default_init_opts do
    [
      workspace_roots: ["/tmp"],
      authorize_mcp_server: fn _server, _context -> true end,
      trust_authorized_workspaces: true
    ]
  end

  @doc """
  Runs `steps` against a fresh adapter and returns the normalized transcript.

  `opts` is reserved for future harness options; no keys are recognized yet
  and passing any raises, so callers do not mistake it for init overrides
  (use an `{:init, opts}` step for those).
  """
  @spec run([step()], keyword()) :: transcript()
  def run(steps, opts \\ []) when is_list(steps) do
    Keyword.validate!(opts, [])
    ensure_clean_env!()

    steps =
      case steps do
        [{:init, _} | _] -> steps
        _ -> [{:init, default_init_opts()} | steps]
      end

    {transcript, _state} =
      Enum.reduce(steps, {[], nil}, fn step, {acc, state} ->
        {recorded_step, result, state} = execute(step, state, Enum.reverse(acc))
        {[%Entry{step: recorded_step, result: result} | acc], state}
      end)

    transcript |> Enum.reverse() |> normalize()
  end

  @doc """
  Runs the scenario and compares it with the committed fixture for
  `area`/`name`, returning the transcript for inline sanity assertions.
  """
  @spec assert_golden(String.t(), String.t(), [step()], keyword()) :: transcript()
  def assert_golden(area, name, steps, opts \\ []) do
    transcript = run(steps, opts)
    path = fixture_path(area, name)

    cond do
      System.get_env("CODEX_GOLDEN") == "update" ->
        File.mkdir_p!(Path.dirname(path))
        File.write!(path, render(transcript))

        flunk(
          "Codex golden fixture (re)written at #{path}. " <>
            "Re-run the test without CODEX_GOLDEN to compare against it."
        )

      not File.exists?(path) ->
        flunk(
          "Missing Codex golden fixture #{path}. " <>
            "Run this test with CODEX_GOLDEN=update to generate it, then review and commit it."
        )

      true ->
        {expected, _binding} = path |> File.read!() |> Code.eval_string([], file: path)
        assert transcript == expected
        transcript
    end
  end

  @doc "Every app-server write in the transcript, in order."
  @spec writes(transcript()) :: [map()]
  def writes(transcript), do: Enum.flat_map(transcript, &Map.get(&1.result, :writes, []))

  @doc "Every ACP message emitted by the adapter, in order."
  @spec messages(transcript()) :: [map()]
  def messages(transcript), do: Enum.flat_map(transcript, &Map.get(&1.result, :messages, []))

  @doc "The result map of the most recent entry, or `nil` for an empty transcript."
  @spec last_result(transcript()) :: map() | nil
  def last_result([]), do: nil
  def last_result(transcript), do: List.last(transcript).result

  @doc """
  Distinct adapter-generated ACP request ids in order of first appearance.

  Inside a step function the transcript is raw, so this returns the real ids
  (for example to answer a `session/request_permission` request); on a
  transcript returned by `run/2` it returns the placeholders instead.
  """
  @spec generated_ids(transcript()) :: [String.t()]
  def generated_ids(transcript) do
    transcript
    |> Enum.reduce([], fn %Entry{step: step, result: result}, acc ->
      acc = collect_generated_ids(step, acc)
      collect_generated_ids(result, acc)
    end)
    |> Enum.reverse()
    |> Enum.uniq()
  end

  @doc "Path of the fixture file for `area`/`name`."
  @spec fixture_path(String.t(), String.t()) :: Path.t()
  def fixture_path(area, name), do: Path.join([@fixture_root, area, name <> ".term"])

  # -- environment guard ----------------------------------------------------

  defp ensure_clean_env! do
    case Enum.filter(@leaky_env_vars, &System.get_env/1) do
      [] ->
        :ok

      set ->
        flunk(
          "Codex golden transcripts depend on the environment: #{Enum.join(set, ", ")} " <>
            "is set and would change the adapter's wire output. Unset it to run golden " <>
            "scenarios (pass model_provider:/codex_config:/no_browser: to {:init, opts} " <>
            "when a scenario needs that behavior)."
        )
    end
  end

  # -- step execution -------------------------------------------------------

  defp execute({:init, opts}, _state, _transcript) do
    {:ok, state} = Codex.init(opts)
    {%{kind: :init, opts: opts}, %{tag: :ok}, state}
  end

  defp execute(:post_connect, state, _transcript) do
    {:ok, data, state} = Codex.post_connect(state)
    {%{kind: :post_connect}, %{tag: :ok, writes: decode_writes(data)}, state}
  end

  defp execute({:outbound, message}, state, transcript) do
    message = resolve(message, transcript)
    {result, state} = message |> Codex.translate_outbound(state) |> normalize_result()
    {%{kind: :outbound, message: message}, result, state}
  end

  defp execute({:inbound, message}, state, transcript) do
    message = resolve(message, transcript)

    {result, state} =
      message |> Jason.encode!() |> Codex.translate_inbound(state) |> normalize_result()

    {%{kind: :inbound, message: message}, result, state}
  end

  defp execute({:inbound_raw, line}, state, _transcript) when is_binary(line) do
    {result, state} = line |> Codex.translate_inbound(state) |> normalize_result()
    {%{kind: :inbound_raw, line: line}, result, state}
  end

  defp execute({:note, text}, state, _transcript) when is_binary(text) do
    {%{kind: :note, text: text}, %{}, state}
  end

  defp resolve(fun, transcript) when is_function(fun, 1), do: fun.(transcript)
  defp resolve(message, _transcript) when is_map(message), do: message

  # -- result normalization -------------------------------------------------

  defp normalize_result({:ok, :skip, state}), do: {%{tag: :ok, skipped: true}, state}
  defp normalize_result({:ok, data, state}), do: {%{tag: :ok, writes: decode_writes(data)}, state}
  defp normalize_result({:reply, result, state}), do: {%{tag: :reply, reply: result}, state}

  defp normalize_result({:messages, messages, state}),
    do: {%{tag: :messages, messages: messages}, state}

  defp normalize_result({:messages_and_reply, messages, result, state}),
    do: {%{tag: :messages_and_reply, messages: messages, reply: result}, state}

  defp normalize_result({:messages_and_write, messages, data, state}),
    do: {%{tag: :messages_and_write, messages: messages, writes: decode_writes(data)}, state}

  defp normalize_result({:reply_and_write, result, data, state}),
    do: {%{tag: :reply_and_write, reply: result, writes: decode_writes(data)}, state}

  defp normalize_result({:skip_and_write, data, state}),
    do: {%{tag: :skip_and_write, skipped: true, writes: decode_writes(data)}, state}

  defp normalize_result({:error, reason, state}), do: {%{tag: :error, error: reason}, state}

  defp normalize_result({:skip, state}), do: {%{tag: :skip, skipped: true}, state}

  # Decodes an NDJSON batch while enforcing its framing: every object,
  # including the last, must be terminated by exactly one "\n".
  defp decode_writes(data) do
    case IO.iodata_to_binary(data) do
      "" ->
        []

      bin ->
        unless String.ends_with?(bin, "\n") do
          raise ArgumentError,
                "app-server write is not newline-terminated NDJSON: #{inspect(bin)}"
        end

        bin
        |> binary_part(0, byte_size(bin) - 1)
        |> String.split("\n")
        |> Enum.map(&decode_line(&1, bin))
    end
  end

  defp decode_line("", bin) do
    raise ArgumentError, "app-server write contains an empty NDJSON line: #{inspect(bin)}"
  end

  defp decode_line(line, _bin), do: Jason.decode!(line)

  # -- id normalization -----------------------------------------------------

  defp normalize(transcript) do
    {entries, _mapping} =
      Enum.map_reduce(transcript, %{ids: %{}, counters: %{}}, fn
        %Entry{step: step, result: result}, acc ->
          {step, acc} = walk(step, acc)
          {result, acc} = walk(result, acc)
          {%Entry{step: step, result: result}, acc}
      end)

    entries
  end

  defp walk(binary, acc) when is_binary(binary) do
    acc =
      @generated_id_pattern
      |> Regex.scan(binary)
      |> Enum.reduce(acc, fn [id, kind, _n], acc -> register_id(acc, id, kind) end)

    {Regex.replace(@generated_id_pattern, binary, fn id, _kind, _n -> acc.ids[id] end), acc}
  end

  defp walk(list, acc) when is_list(list) do
    {items, acc} = Enum.map_reduce(list, acc, &walk/2)
    {items, acc}
  end

  defp walk(tuple, acc) when is_tuple(tuple) do
    {items, acc} = tuple |> Tuple.to_list() |> walk(acc)
    {List.to_tuple(items), acc}
  end

  defp walk(%{__struct__: _} = struct, acc), do: {struct, acc}

  defp walk(map, acc) when is_map(map) do
    {pairs, acc} =
      map
      |> Map.to_list()
      |> Enum.sort()
      |> Enum.map_reduce(acc, fn {key, value}, acc ->
        {key, acc} = walk(key, acc)
        {value, acc} = walk(value, acc)
        {{key, value}, acc}
      end)

    {Map.new(pairs), acc}
  end

  defp walk(fun, acc) when is_function(fun), do: {:__fun__, acc}
  defp walk(other, acc), do: {other, acc}

  defp register_id(%{ids: ids} = acc, id, _kind) when is_map_key(ids, id), do: acc

  defp register_id(%{ids: ids, counters: counters} = acc, id, kind) do
    n = Map.get(counters, kind, 0) + 1
    %{acc | ids: Map.put(ids, id, "codex-#{kind}-<#{n}>"), counters: Map.put(counters, kind, n)}
  end

  defp collect_generated_ids(binary, acc) when is_binary(binary) do
    @any_id_pattern
    |> Regex.scan(binary)
    |> Enum.reduce(acc, fn [id], acc -> [id | acc] end)
  end

  defp collect_generated_ids(list, acc) when is_list(list),
    do: Enum.reduce(list, acc, &collect_generated_ids/2)

  defp collect_generated_ids(tuple, acc) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> collect_generated_ids(acc)

  defp collect_generated_ids(%{__struct__: _}, acc), do: acc

  defp collect_generated_ids(map, acc) when is_map(map) do
    map
    |> Map.to_list()
    |> Enum.sort()
    |> Enum.reduce(acc, fn {k, v}, acc ->
      acc = collect_generated_ids(k, acc)
      collect_generated_ids(v, acc)
    end)
  end

  defp collect_generated_ids(_other, acc), do: acc

  # -- fixture rendering ----------------------------------------------------

  # sort_maps: atom-keyed maps otherwise render in atom-creation order, which
  # differs between VM runs and would make regenerated fixtures churn.
  defp render(transcript) do
    inspect(transcript,
      pretty: true,
      limit: :infinity,
      printable_limit: :infinity,
      width: 98,
      custom_options: [sort_maps: true]
    ) <> "\n"
  end
end
