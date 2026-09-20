defmodule ExMCP.Test.PiGolden do
  @moduledoc """
  Golden-transcript harness for characterizing `ExMCP.ACP.Adapters.Pi`.

  Every step drives one public adapter callback (`init/1`,
  `translate_outbound/2`, `translate_inbound/2`, `handle_adapter_message/2`,
  `list_sessions/2`, `shutdown/1`) with the state threaded from the previous
  step. The harness records what the adapter *did* at each step (RPC lines
  written to Pi, ACP messages, replies, errors, subprocess lifecycle) and
  never records the adapter state itself, so fixtures pin wire behavior
  rather than internal state layout.

  ## Sandbox

  `run/2` creates a private sandbox directory per run and removes it when the
  test exits. Its layout is:

      <sandbox>/bin/pi            fake Pi executable (see "Managed mode")
      <sandbox>/agent/            Pi agent directory (`agent_dir`), with a
                                  settings.json of `{"quietStartup": true}`
      <sandbox>/sessions/         Pi session directory (`session_dir`)
      <sandbox>/session-map.json  adapter session map (`session_map_path`)
      <sandbox>/project/          working directory (`cwd`)

  The literal string `"<sandbox>"` may appear anywhere in a step (init
  options, messages, file paths); it is substituted with the real sandbox
  path before the step executes and normalized back when the transcript is
  rendered. `default_init_opts/0` points every location-bearing option into
  the sandbox and `{:init, opts}` is merged over those defaults. The
  harness refuses (`flunk/1`) any `agent_dir`, `session_dir`,
  `session_map_path` or `cli_path` that does not live inside the sandbox,
  so no scenario can read the developer's real Pi settings, prompts,
  models, or sessions (`docs/POST_1_0_MAINTENANCE_PLAN.md`, "Pi completion
  criteria").

  ## Steps

    * `{:init, opts}` - `Pi.init(Keyword.merge(default_init_opts(), opts))`.
      When the first step is not `:init` the defaults are used.
    * `{:outbound, map | fun}` - `Pi.translate_outbound(map, state)`.
    * `{:inbound, map | fun}` - `Pi.translate_inbound(Jason.encode!(map), state)`.
    * `{:inbound_raw, string}` - `Pi.translate_inbound(string, state)` verbatim.
    * `{:respond, type, data}` / `{:respond_error, type, error}` - an
      `:inbound` success/failure response correlated with the most recent
      RPC request of that `type` written so far (`request_id/2`); the step
      is recorded as `:inbound` with the resolved message.
    * `{:port_data, string}` - `Pi.handle_adapter_message({port, {:data, string}}, state)`
      with the adapter's current port (`nil` in unmanaged mode, which still
      matches the adapter's own `%{port: port}` clause).
    * `{:port_exit, code}` - in managed mode the fake Pi is told to exit
      with `code` and the real `{port, {:exit_status, code}}` message is
      awaited; in both modes it is then passed to `handle_adapter_message/2`.
    * `:port_closed` - `Pi.handle_adapter_message({port, :closed}, state)`.
    * `{:adapter_message, term}` - `Pi.handle_adapter_message(term, state)` verbatim.
    * `{:list_sessions, params}` - `Pi.list_sessions(params, state)`.
    * `:shutdown` - `Pi.shutdown(state)`.
    * `{:write_file, path, content}` - writes a sandbox file before the next
      adapter call (`content` is a string, a map rendered as JSON, or a list
      of maps rendered as JSONL); parent directories are created.
    * `{:read_file, path}` - records `%{exists: boolean, content: term}`
      where JSON content is decoded and anything else kept verbatim.
    * `{:note, text}` - recorded verbatim, no adapter call.

  A `fun` receives the *raw* transcript recorded so far (real ids intact)
  and returns the map to use; the resolved map is what gets recorded.

  ## Transcript entries

  Each entry is an `ExMCP.Test.PiGolden.Entry` struct (a map with `:step`
  and `:result`), so fixtures render the cause (`step`) before its effect
  (`result`). `:result` holds only the keys that apply: `tag` (the adapter's
  return tag), `writes` (RPC lines returned to the bridge), `port_writes`
  (RPC lines that reached the fake Pi in managed mode), `messages`,
  `reply`, `error`, `skipped`, `pending`, `port` (`:spawned`, `:closed`,
  or `:respawned`), `exists`, `content`.

  ## Wire framing

  RPC writes are recorded as JSON-decoded maps for readability, but the
  NDJSON framing produced by `ExMCP.ACP.Adapters.Pi.RPC.line/1` is enforced
  on every write before decoding: the iodata must be empty or consist of
  JSON objects each terminated by exactly one `"\\n"`. A write whose final
  object lacks its terminator, or that contains an empty line, raises rather
  than silently producing the same transcript.

  The `ExMCP.ACP.Adapter` behaviour also allows `{:one_shot, fun, state}`,
  `{:partial, state}` and `{:reply_and_write, result, data, state}`. The Pi
  adapter never returns them and `normalize_result/1` deliberately has no
  clauses for them; a step that produced one raises a `FunctionClauseError`
  here, which is the right signal to extend the harness.

  ## Managed mode

  With `{:init, managed: true}` the adapter owns a real Port. The sandbox's
  `bin/pi` is a shell script that echoes every stdin line back on stdout and
  exits with `N` when it reads `__exit__ N`. After each step the harness
  writes a sentinel line and collects everything echoed before it, so the
  RPC lines the adapter pushed into the subprocess are recorded as
  `port_writes` in the same order Pi would have read them. Managed
  scenarios must keep `quietStartup` enabled in the sandbox agent settings
  (the harness checks the agent settings file at `:init`): Pi's startup
  banner inventories `~/.pi` and `~/.agents`, which is neither sandboxed nor
  deterministic, so it is deliberately not characterized.

  ## Normalization

  Before a transcript is returned, written, or compared, entries are walked
  in order (each entry's `step` before its `result`; inside a map the sorted
  keys as well as the values) and:

    * the sandbox path is replaced by `"<sandbox>"` and the user's home
      directory by `"<home>"` (the latter is a tripwire: it should never
      appear in a fixture);
    * RPC correlation ids and fallback session ids minted from
      `System.unique_integer/1` (`"pi-123"`) are replaced by placeholders
      that preserve identity within the transcript: the first distinct id
      becomes `"pi-<1>"`, the next `"pi-<2>"`, and the same real id always
      maps to the same placeholder. The `"id"` of an RPC envelope (a
      recorded write or a `"type": "response"` message) is tracked
      separately from ids found anywhere else, so a minted session id that
      textually collides with a correlation id (the adapter's monotonic and
      non-monotonic `unique_integer` sequences overlap) still gets its own
      placeholder. Prompt ids (`"msg-N"`) and extension-UI request ids
      (`"pi-extension-N"`) come from the adapter's own counter and are kept
      verbatim because they are part of the wire contract;
    * replayed tool-call ids minted for `toolResult` messages without one
      (`"tool-456"`) become `"tool-<n>"` the same way;
    * ISO-8601 timestamps within one day of the run (the adapter's and the
      session store's `DateTime.utc_now/0`, and JSONL files that fall back
      to their mtime) become `"<now>"`; timestamps written by scenarios
      must therefore be dated in the past;
    * anonymous functions are replaced by `:__fun__` so fixtures stay
      evaluable.

  ## Fixtures

  `assert_golden/4` compares the transcript with
  `test/fixtures/acp/pi/<area>/<name>.term`. Run the test with
  `PI_GOLDEN=update` to (re)write the fixture; that run always fails so
  regeneration is a deliberate, reviewed act.
  """

  import ExUnit.Assertions, only: [flunk: 1, assert: 1]

  alias ExMCP.ACP.Adapters.Pi

  defmodule Entry do
    @moduledoc "One transcript entry: the step as executed and the adapter's normalized result."
    defstruct [:step, :result]

    @type t :: %__MODULE__{step: map(), result: map()}
  end

  @fixture_root Path.expand("../../fixtures/acp/pi", __DIR__)
  @sandbox_placeholder "<sandbox>"
  @port_timeout 5_000

  @fake_pi_script """
  #!/bin/sh
  # Fake `pi` for ExMCP.Test.PiGolden: echoes every stdin line back on stdout
  # so the harness can observe what the adapter wrote; "__exit__ N" exits N.
  while IFS= read -r line; do
    case "$line" in
      "__exit__ "*) exit "${line#__exit__ }" ;;
    esac
    printf '%s\\n' "$line"
  done
  """

  @generated_id_pattern ~r/(?<![\w-])(pi|tool)-(\d+)(?![\w-])/
  @any_id_pattern ~r/(?<![\w-])(?:pi|tool)-(?:\d+|<\d+>)(?![\w-])/
  @timestamp_pattern ~r/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,6})?(?:Z|[+-]\d{2}:\d{2})/
  @now_window_seconds 86_400

  @type step ::
          {:init, keyword()}
          | {:outbound, map() | (transcript() -> map())}
          | {:inbound, map() | (transcript() -> map())}
          | {:inbound_raw, String.t()}
          | {:respond, String.t(), term()}
          | {:respond_error, String.t(), term()}
          | {:port_data, String.t()}
          | {:port_exit, integer()}
          | :port_closed
          | {:adapter_message, term()}
          | {:list_sessions, map()}
          | :shutdown
          | {:write_file, Path.t(), term()}
          | {:read_file, Path.t()}
          | {:note, String.t()}

  @type entry :: Entry.t()
  @type transcript :: [entry()]

  @doc "Default `Pi.init/1` options; `{:init, opts}` steps are merged over them."
  @spec default_init_opts() :: keyword()
  def default_init_opts do
    [
      cwd: "<sandbox>/project",
      agent_dir: "<sandbox>/agent",
      session_dir: "<sandbox>/sessions",
      session_map_path: "<sandbox>/session-map.json",
      cli_path: "<sandbox>/bin/pi",
      update_notice: false,
      managed: false
    ]
  end

  @doc """
  Runs `steps` against a fresh adapter in a fresh sandbox and returns the
  normalized transcript.

  `opts` is reserved for future harness options; no keys are recognized yet
  and passing any raises, so callers do not mistake it for init overrides
  (use an `{:init, opts}` step for those).
  """
  @spec run([step()], keyword()) :: transcript()
  def run(steps, opts \\ []) when is_list(steps) do
    Keyword.validate!(opts, [])
    sandbox = create_sandbox!()

    steps =
      case steps do
        [{:init, _} | _] -> steps
        _ -> [{:init, []} | steps]
      end

    ctx = %{sandbox: sandbox}

    {transcript, _state} =
      Enum.reduce(steps, {[], nil}, fn step, {acc, state} ->
        {recorded_step, result, state} = execute(step, state, Enum.reverse(acc), ctx)
        {[%Entry{step: recorded_step, result: result} | acc], state}
      end)

    transcript |> Enum.reverse() |> normalize(sandbox)
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
      System.get_env("PI_GOLDEN") == "update" ->
        File.mkdir_p!(Path.dirname(path))
        File.write!(path, render(transcript))

        flunk(
          "Pi golden fixture (re)written at #{path}. " <>
            "Re-run the test without PI_GOLDEN to compare against it."
        )

      not File.exists?(path) ->
        flunk(
          "Missing Pi golden fixture #{path}. " <>
            "Run this test with PI_GOLDEN=update to generate it, then review and commit it."
        )

      true ->
        {expected, _binding} = path |> File.read!() |> Code.eval_string([], file: path)
        assert transcript == expected
        transcript
    end
  end

  @doc "Every RPC line the adapter produced (returned writes and managed port writes), in order."
  @spec writes(transcript()) :: [map()]
  def writes(transcript) do
    Enum.flat_map(transcript, fn %Entry{result: result} ->
      Map.get(result, :writes, []) ++ Map.get(result, :port_writes, [])
    end)
  end

  @doc "Every ACP message emitted by the adapter, in order."
  @spec messages(transcript()) :: [map()]
  def messages(transcript), do: Enum.flat_map(transcript, &Map.get(&1.result, :messages, []))

  @doc "The result map of the most recent entry, or `nil` for an empty transcript."
  @spec last_result(transcript()) :: map() | nil
  def last_result([]), do: nil
  def last_result(transcript), do: List.last(transcript).result

  @doc "Ids of every RPC request of `type` written so far, in order (real ids inside a step fun)."
  @spec request_ids(transcript(), String.t()) :: [String.t()]
  def request_ids(transcript, type) do
    for %{"type" => ^type, "id" => id} <- writes(transcript), do: id
  end

  @doc "Id of the most recent RPC request of `type`, or `nil`."
  @spec request_id(transcript(), String.t()) :: String.t() | nil
  def request_id(transcript, type), do: transcript |> request_ids(type) |> List.last()

  @doc """
  Distinct adapter-generated ids in order of first appearance.

  Inside a step function the transcript is raw, so this returns the real ids;
  on a transcript returned by `run/2` it returns the placeholders instead.
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

  # -- sandbox --------------------------------------------------------------

  defp create_sandbox! do
    sandbox =
      Path.join(System.tmp_dir!(), "pi_golden_#{System.unique_integer([:positive])}")

    for dir <- ["bin", "agent", "sessions", "project"] do
      File.mkdir_p!(Path.join(sandbox, dir))
    end

    pi = Path.join([sandbox, "bin", "pi"])
    File.write!(pi, @fake_pi_script)
    File.chmod!(pi, 0o755)

    File.write!(
      Path.join([sandbox, "agent", "settings.json"]),
      Jason.encode!(%{"quietStartup" => true})
    )

    ExUnit.Callbacks.on_exit(fn -> File.rm_rf(sandbox) end)
    sandbox
  end

  defp substitute(binary, sandbox) when is_binary(binary),
    do: String.replace(binary, @sandbox_placeholder, sandbox)

  defp substitute(list, sandbox) when is_list(list), do: Enum.map(list, &substitute(&1, sandbox))

  defp substitute(tuple, sandbox) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> substitute(sandbox) |> List.to_tuple()

  defp substitute(%{__struct__: _} = struct, _sandbox), do: struct

  defp substitute(map, sandbox) when is_map(map),
    do: Map.new(map, fn {k, v} -> {substitute(k, sandbox), substitute(v, sandbox)} end)

  defp substitute(other, _sandbox), do: other

  defp ensure_sandboxed!(opts, sandbox) do
    prefix = sandbox <> "/"

    for key <- [:agent_dir, :session_map_path, :cli_path] do
      value = Keyword.get(opts, key)

      unless is_binary(value) and String.starts_with?(value, prefix) do
        flunk(
          "Pi golden scenarios must keep #{key} inside the sandbox " <>
            "(got #{inspect(value)}); the developer's real Pi configuration is off limits."
        )
      end
    end

    case Keyword.get(opts, :session_dir) do
      nil -> :ok
      dir when is_binary(dir) -> assert String.starts_with?(dir, prefix)
      other -> flunk("session_dir must be nil or a sandbox path, got #{inspect(other)}")
    end

    if Keyword.get(opts, :managed, true), do: ensure_quiet_startup!(opts)
    :ok
  end

  defp ensure_quiet_startup!(opts) do
    path = Path.join(Keyword.fetch!(opts, :agent_dir), "settings.json")

    with {:ok, raw} <- File.read(path),
         {:ok, %{"quietStartup" => true}} <- Jason.decode(raw) do
      :ok
    else
      _ ->
        flunk(
          "Managed Pi golden scenarios require {\"quietStartup\": true} in #{path}: " <>
            "Pi's startup banner inventories the developer's home directory."
        )
    end
  end

  # -- step execution -------------------------------------------------------

  defp execute({:init, opts}, _state, _transcript, ctx) do
    opts = default_init_opts() |> Keyword.merge(opts) |> substitute(ctx.sandbox)
    ensure_sandboxed!(opts, ctx.sandbox)
    {:ok, state} = Pi.init(opts)
    {%{kind: :init, opts: opts}, %{tag: :ok}, state}
  end

  defp execute({:outbound, message}, state, transcript, ctx) do
    message = resolve(message, transcript, ctx)
    {result, state} = message |> Pi.translate_outbound(state) |> after_step(state)
    {%{kind: :outbound, message: message}, result, state}
  end

  defp execute({:inbound, message}, state, transcript, ctx) do
    message = resolve(message, transcript, ctx)

    {result, state} =
      message |> Jason.encode!() |> Pi.translate_inbound(state) |> after_step(state)

    {%{kind: :inbound, message: message}, result, state}
  end

  defp execute({:inbound_raw, line}, state, _transcript, _ctx) when is_binary(line) do
    {result, state} = line |> Pi.translate_inbound(state) |> after_step(state)
    {%{kind: :inbound_raw, line: line}, result, state}
  end

  defp execute({:respond, type, data}, state, transcript, ctx) do
    execute({:inbound, response(transcript, type, true, "data", data)}, state, transcript, ctx)
  end

  defp execute({:respond_error, type, error}, state, transcript, ctx) do
    execute({:inbound, response(transcript, type, false, "error", error)}, state, transcript, ctx)
  end

  defp execute({:port_data, data}, state, _transcript, _ctx) when is_binary(data) do
    {result, state} =
      {state.port, {:data, data}} |> Pi.handle_adapter_message(state) |> after_step(state)

    {%{kind: :port_data, data: data}, result, state}
  end

  defp execute({:port_exit, code}, state, _transcript, _ctx) when is_integer(code) do
    port = state.port
    if is_port(port) and Port.info(port), do: exit_fake_pi(port, code)

    {result, state} =
      {port, {:exit_status, code}} |> Pi.handle_adapter_message(state) |> after_step(state)

    {%{kind: :port_exit, code: code}, result, state}
  end

  defp execute(:port_closed, state, _transcript, _ctx) do
    {result, state} =
      {state.port, :closed} |> Pi.handle_adapter_message(state) |> after_step(state)

    {%{kind: :port_closed}, result, state}
  end

  defp execute({:adapter_message, message}, state, _transcript, _ctx) do
    {result, state} = message |> Pi.handle_adapter_message(state) |> after_step(state)
    {%{kind: :adapter_message, message: message}, result, state}
  end

  defp execute({:list_sessions, params}, state, _transcript, ctx) when is_map(params) do
    params = substitute(params, ctx.sandbox)
    {:ok, result, state} = Pi.list_sessions(params, state)
    {%{kind: :list_sessions, params: params}, %{tag: :ok, reply: result}, state}
  end

  defp execute(:shutdown, state, _transcript, _ctx) do
    before = state
    state = Pi.shutdown(state)
    {%{kind: :shutdown}, %{} |> put_port_transition(before, state), state}
  end

  defp execute({:write_file, path, content}, state, _transcript, ctx) do
    path = substitute(path, ctx.sandbox)
    content = substitute(content, ctx.sandbox)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, render_file(content))
    {%{kind: :write_file, path: path, content: content}, %{}, state}
  end

  defp execute({:read_file, path}, state, _transcript, ctx) do
    path = substitute(path, ctx.sandbox)

    result =
      case File.read(path) do
        {:ok, raw} -> %{exists: true, content: decode_file(raw)}
        {:error, _} -> %{exists: false}
      end

    {%{kind: :read_file, path: path}, result, state}
  end

  defp execute({:note, text}, state, _transcript, _ctx) when is_binary(text) do
    {%{kind: :note, text: text}, %{}, state}
  end

  defp resolve(fun, transcript, ctx) when is_function(fun, 1),
    do: fun.(transcript) |> substitute(ctx.sandbox)

  defp resolve(message, _transcript, ctx) when is_map(message),
    do: substitute(message, ctx.sandbox)

  defp response(transcript, type, success?, key, payload) do
    case request_id(transcript, type) do
      nil ->
        flunk("No #{inspect(type)} RPC request has been written yet, nothing to respond to")

      id ->
        %{
          "type" => "response",
          "id" => id,
          "command" => type,
          "success" => success?,
          key => payload
        }
    end
  end

  defp render_file(content) when is_binary(content), do: content
  defp render_file(content) when is_map(content), do: Jason.encode!(content, pretty: true)

  defp render_file(content) when is_list(content),
    do: Enum.map_join(content, "", &(Jason.encode!(&1) <> "\n"))

  defp decode_file(raw) do
    case Jason.decode(raw) do
      {:ok, decoded} -> decoded
      {:error, _} -> raw
    end
  end

  # -- result normalization -------------------------------------------------

  defp after_step(result, before) do
    {result, state} = normalize_result(result)

    result =
      result
      |> put_port_writes(state)
      |> put_port_transition(before, state)

    {result, state}
  end

  defp normalize_result({:ok, :skip, state}), do: {%{tag: :ok, skipped: true}, state}
  defp normalize_result({:ok, :pending, state}), do: {%{tag: :ok, pending: true}, state}
  defp normalize_result({:ok, data, state}), do: {%{tag: :ok, writes: decode_writes(data)}, state}
  defp normalize_result({:reply, result, state}), do: {%{tag: :reply, reply: result}, state}

  defp normalize_result({:messages, messages, state}),
    do: {%{tag: :messages, messages: messages}, state}

  defp normalize_result({:messages_and_reply, messages, result, state}),
    do: {%{tag: :messages_and_reply, messages: messages, reply: result}, state}

  defp normalize_result({:messages_and_write, messages, data, state}),
    do: {%{tag: :messages_and_write, messages: messages, writes: decode_writes(data)}, state}

  defp normalize_result({:skip_and_write, data, state}),
    do: {%{tag: :skip_and_write, skipped: true, writes: decode_writes(data)}, state}

  defp normalize_result({:error, reason, state}), do: {%{tag: :error, error: reason}, state}
  defp normalize_result({:skip, state}), do: {%{tag: :skip, skipped: true}, state}

  defp put_port_transition(result, %{port: before}, %{port: after_port}) do
    cond do
      before == after_port -> result
      is_nil(before) -> Map.put(result, :port, :spawned)
      is_nil(after_port) -> Map.put(result, :port, :closed)
      true -> Map.put(result, :port, :respawned)
    end
  end

  defp put_port_writes(result, %{port: port}) when is_port(port) do
    case collect_port_writes(port) do
      [] -> result
      port_writes -> Map.put(result, :port_writes, port_writes)
    end
  end

  defp put_port_writes(result, _state), do: result

  # -- fake pi I/O ----------------------------------------------------------

  defp collect_port_writes(port) do
    if Port.info(port) do
      sentinel = "__pi_golden_sentinel_#{System.unique_integer([:positive])}\n"
      Port.command(port, sentinel)
      collect_until(port, sentinel, "")
    else
      []
    end
  end

  defp collect_until(port, sentinel, acc) do
    receive do
      {^port, {:data, data}} ->
        acc = acc <> data

        case String.split(acc, sentinel, parts: 2) do
          [before, ""] -> decode_writes(before)
          [before, rest] -> flunk("fake pi echoed #{inspect(rest)} after the sentinel: #{before}")
          [_] -> collect_until(port, sentinel, acc)
        end
    after
      @port_timeout ->
        flunk("fake pi did not echo the harness sentinel within #{@port_timeout}ms")
    end
  end

  defp exit_fake_pi(port, code) do
    Port.command(port, "__exit__ #{code}\n")

    receive do
      {^port, {:exit_status, ^code}} -> :ok
    after
      @port_timeout -> flunk("fake pi did not exit with #{code} within #{@port_timeout}ms")
    end
  end

  # Decodes an NDJSON batch while enforcing its framing: every object,
  # including the last, must be terminated by exactly one "\n".
  defp decode_writes(data) do
    case IO.iodata_to_binary(data) do
      "" ->
        []

      bin ->
        unless String.ends_with?(bin, "\n") do
          raise ArgumentError, "Pi RPC write is not newline-terminated NDJSON: #{inspect(bin)}"
        end

        bin
        |> binary_part(0, byte_size(bin) - 1)
        |> String.split("\n")
        |> Enum.map(&decode_line(&1, bin))
    end
  end

  defp decode_line("", bin) do
    raise ArgumentError, "Pi RPC write contains an empty NDJSON line: #{inspect(bin)}"
  end

  defp decode_line(line, _bin), do: Jason.decode!(line)

  # -- transcript normalization ---------------------------------------------

  # Ids are normalized per role so that a minted fallback session id that
  # happens to equal an RPC correlation id (both come from
  # System.unique_integer/1, whose monotonic and non-monotonic sequences
  # overlap) still renders deterministically: the "id" of an RPC envelope
  # (a write, or a response step) lives in the :rpc namespace, everything
  # else in the :other namespace; both share one counter per prefix.
  defp normalize(transcript, sandbox) do
    acc = %{
      ids: %{},
      counters: %{},
      sandbox: sandbox,
      home: System.user_home!(),
      now: DateTime.utc_now()
    }

    {entries, _acc} =
      Enum.map_reduce(transcript, acc, fn %Entry{step: step, result: result}, acc ->
        {step, acc} = walk(step, acc, :other)
        {result, acc} = walk(result, acc, :other)
        {%Entry{step: step, result: result}, acc}
      end)

    entries
  end

  defp walk(binary, acc, :rpc_id) when is_binary(binary) do
    case Regex.run(~r/^(pi)-(\d+)$/, binary) do
      [_, kind, _n] ->
        acc = register_id(acc, {:rpc, binary}, kind)
        {acc.ids[{:rpc, binary}], acc}

      nil ->
        walk(binary, acc, :other)
    end
  end

  defp walk(binary, acc, _role) when is_binary(binary) do
    binary =
      binary
      |> String.replace(acc.sandbox, @sandbox_placeholder)
      |> String.replace(acc.home, "<home>")
      |> normalize_timestamps(acc.now)

    acc =
      @generated_id_pattern
      |> Regex.scan(binary)
      |> Enum.reduce(acc, fn [id, kind, _n], acc -> register_id(acc, {:other, id}, kind) end)

    {Regex.replace(@generated_id_pattern, binary, fn id, _kind, _n -> acc.ids[{:other, id}] end),
     acc}
  end

  defp walk(list, acc, role) when is_list(list),
    do: Enum.map_reduce(list, acc, &walk(&1, &2, role))

  defp walk(tuple, acc, role) when is_tuple(tuple) do
    {items, acc} = tuple |> Tuple.to_list() |> walk(acc, role)
    {List.to_tuple(items), acc}
  end

  defp walk(%{__struct__: _} = struct, acc, _role), do: {struct, acc}

  defp walk(map, acc, role) when is_map(map) do
    envelope? = role == :rpc_envelopes or map["type"] == "response"

    {pairs, acc} =
      map
      |> Map.to_list()
      |> Enum.sort()
      |> Enum.map_reduce(acc, fn {key, value}, acc ->
        {key, acc} = walk(key, acc, :other)
        {value, acc} = walk(value, acc, child_role(key, envelope?))
        {{key, value}, acc}
      end)

    {Map.new(pairs), acc}
  end

  defp walk(fun, acc, _role) when is_function(fun), do: {:__fun__, acc}
  defp walk(other, acc, _role), do: {other, acc}

  defp child_role(key, _envelope?) when key in [:writes, :port_writes], do: :rpc_envelopes
  defp child_role("id", true), do: :rpc_id
  defp child_role(_key, _envelope?), do: :other

  defp normalize_timestamps(binary, now) do
    Regex.replace(@timestamp_pattern, binary, fn stamp ->
      case DateTime.from_iso8601(stamp) do
        {:ok, dt, _offset} ->
          if abs(DateTime.diff(dt, now, :second)) < @now_window_seconds, do: "<now>", else: stamp

        _ ->
          stamp
      end
    end)
  end

  defp register_id(%{ids: ids} = acc, key, _kind) when is_map_key(ids, key), do: acc

  defp register_id(%{ids: ids, counters: counters} = acc, key, kind) do
    n = Map.get(counters, kind, 0) + 1
    %{acc | ids: Map.put(ids, key, "#{kind}-<#{n}>"), counters: Map.put(counters, kind, n)}
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
