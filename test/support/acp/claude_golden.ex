defmodule ExMCP.Test.ClaudeGolden do
  @moduledoc """
  Golden-transcript harness for characterizing `ExMCP.ACP.Adapters.ClaudeSDK`.

  Every step drives one public adapter callback (`init/1`, `command/1`,
  `env/1`, `post_connect/1`, `auth_methods/1,2`, `capabilities/0`, `modes/0`,
  `config_options/0`, `list_sessions/2`, `fork_session/2`,
  `translate_outbound/2`, `translate_inbound/2`) with the state threaded from
  the previous step. The harness records what the adapter *did* at each step
  (SDK lines written to Claude Code, ACP messages, replies, errors) and never
  records the adapter state itself, so fixtures pin wire behavior rather than
  internal state layout.

  The Claude adapter is not adapter-managed: `command/1` returns an
  executable and args for the bridge to spawn, and the adapter implements
  neither `handle_adapter_message/2` nor `shutdown/1`. Subprocess exit and
  transport teardown are owned by `ExMCP.ACP.AdapterBridge` and are therefore
  not reachable from this harness (see the faults area moduledoc).

  ## Sandbox

  `run/2` creates a private sandbox directory per run and removes it when the
  test exits. Its layout is:

      <sandbox>/bin/claude          fake `claude` executable (see "Fake CLI")
      <sandbox>/bin/claude-failing  fake `claude` that exits non-zero
      <sandbox>/claude/             Claude config dir (`claude_config_dir`),
                                    holding `projects/<key>/<uuid>.jsonl`
      <sandbox>/project/            working directory (`cwd`)

  Two placeholders may appear anywhere in a step (init options, messages,
  file paths) and are substituted with real values before the step executes,
  then normalized back when the transcript is rendered:

    * `"<sandbox>"` - the sandbox directory;
    * `"<sandbox-key>"` - the sandbox directory in the SDK's project-key
      form (every non-alphanumeric character replaced by `-`), so a session
      store path is written as
      `"<sandbox>/claude/projects/<sandbox-key>-project/<uuid>.jsonl"`.

  `default_init_opts/0` points `cwd`, `claude_config_dir` and `cli_path` into
  the sandbox and `{:init, opts}` is merged over those defaults. The harness
  refuses (`flunk/1`) any of those three options, any `CLAUDE_CONFIG_DIR`
  passed through `:env`, and any `params["cwd"]` of an outbound request that
  does not live inside the sandbox, so no scenario can read the developer's
  real Claude configuration, credentials, sessions, or projects. The
  normalizer additionally rewrites the user's home directory to `"<home>"`
  and `assert_golden/4` fails if that placeholder ever reaches a fixture.

  ## Environment

  `CLAUDE_CODE_EXECUTABLE` (the `cli_path` fallback) and `CLAUDE_CONFIG_DIR`
  (the session store fallback) change the adapter's behavior without an
  option override, so `run/2` fails with a clear message when either is set.
  `auth_methods/1,2` additionally branches on `NO_BROWSER`, `SSH_CONNECTION`,
  `SSH_CLIENT`, `SSH_TTY` and `CLAUDE_CODE_REMOTE`; an `:auth_methods` step
  fails when any of them is set, and the remote branch is deliberately not
  characterized (it is only reachable by mutating the OS environment, which
  would race every other async test).

  ## Steps

    * `{:init, opts}` - `ClaudeSDK.init(Keyword.merge(default_init_opts(), opts))`.
      When the first step is not `:init` the defaults are used.
    * `:post_connect` - `ClaudeSDK.post_connect(state)`.
    * `{:outbound, map | fun}` - `ClaudeSDK.translate_outbound(map, state)`.
    * `{:inbound, map | fun}` - `ClaudeSDK.translate_inbound(Jason.encode!(map), state)`.
    * `{:inbound_raw, string}` - `ClaudeSDK.translate_inbound(string, state)` verbatim.
    * `{:respond_control, subtype, response}` / `{:respond_control_error,
      subtype, error}` - an `:inbound` `control_response` correlated with the
      most recent control request of that `subtype` written so far
      (`control_request_id/2`); the step is recorded as `:inbound` with the
      resolved message.
    * `{:list_sessions, params}` - `ClaudeSDK.list_sessions(params, state)`.
    * `{:fork_session, params}` - `ClaudeSDK.fork_session(params, state)`.
    * `{:command, opts}` / `{:env, opts}` - `ClaudeSDK.command/1` and
      `ClaudeSDK.env/1` over `Keyword.merge(default_cli_opts(), opts)`.
    * `{:auth_methods, opts}` - `ClaudeSDK.auth_methods(opts, state)`;
      `{:auth_methods_fresh, opts}` - `ClaudeSDK.auth_methods(opts)`.
    * `:capabilities`, `:modes`, `:config_options` - the static callbacks.
    * `{:write_file, path, content}` - writes a sandbox file before the next
      adapter call (`content` is a string, a map rendered as JSON, or a list
      of maps rendered as JSONL); parent directories are created.
    * `{:read_file, path}` - records `%{exists: boolean, content: term}`
      where JSONL content is decoded line by line and anything else is kept
      verbatim.
    * `{:note, text}` - recorded verbatim, no adapter call.

  A `fun` (accepted by `:outbound`, `:inbound`, `:fork_session`,
  `:write_file` and `:read_file`) receives the *raw* transcript recorded so
  far (real ids intact) and returns the map or path to use; the resolved value
  is what gets recorded. A `:fork_session` fun is how a scenario forks at a
  `messageId` an earlier step actually put on the wire.

  ## Request ids

  ACP requests the adapter sends to the client (`session/request_permission`,
  `elicitation/create`, `fs/read_text_file`) carry an id minted by
  `ExMCP.Internal.JSONRPC.generate_id/0`, a bare monotonic integer that
  starts at 1 in a fresh VM. To keep scenario ids and adapter ids apart, an
  `"id"` supplied by a step must be a string unless it is an integer the
  adapter already minted (which is how a reply step answers a request);
  anything else is refused with `flunk/1`.

  ## Transcript entries

  Each entry is an `ExMCP.Test.ClaudeGolden.Entry` struct (a map with `:step`
  and `:result`), so fixtures render the cause (`step`) before its effect
  (`result`). `:result` holds only the keys that apply: `tag` (the adapter's
  return tag), `writes` (SDK lines written to Claude Code), `messages`,
  `reply` (a JSON-RPC result, or the value of a plain callback such as
  `capabilities/0`), `error`, `skipped`, `exists`, `content`.

  ## Wire framing

  SDK writes are recorded as JSON-decoded maps for readability, but the
  NDJSON framing produced by `ExMCP.ACP.Adapters.ClaudeSDK.Protocol.line/1`
  is enforced on every write before decoding: the iodata must be empty or
  consist of JSON objects each terminated by exactly one `"\\n"`. A write
  whose final object lacks its terminator, or that contains an empty line,
  raises rather than silently producing the same transcript.

  The `ExMCP.ACP.Adapter` behaviour also allows `{:ok, :pending, state}`,
  `{:one_shot, fun, state}` and `{:partial, state}`. The Claude adapter never
  returns them and `normalize_result/1` deliberately has no clauses for them;
  a step that produced one raises a `FunctionClauseError` here, which is the
  right signal to extend the harness.

  ## Normalization

  Before a transcript is returned, written, or compared, entries are walked
  in order (each entry's `step` before its `result`; inside a map the sorted
  keys as well as the values) and:

    * the sandbox path (in both its literal and symlink-resolved form, which
      differ on macOS) is replaced by `"<sandbox>"`, its project-key form by
      `"<sandbox-key>"`, and the user's home directory by `"<home>"` (the
      last is a tripwire: it must never appear in a fixture);
    * control request ids minted by the adapter
      (`"ex_mcp_set_permission_mode_12"`), generated ACP session ids
      (`"claude_sdk_13"`) and the tool-call id minted for a permission
      request that carries none (`"tool_14"`) are replaced by placeholders
      that preserve identity within the transcript: the first distinct id of
      a kind becomes `..._<1>`, the next `..._<2>`, and the same real id
      always maps to the same placeholder;
    * an integer under an `"id"` key - always adapter-minted, see "Request
      ids" - becomes `"acp-<1>"`, `"acp-<2>"`, ... the same way;
    * a UUID first seen in a step is a scenario literal and is kept verbatim;
      a UUID first seen in a result was minted by the adapter
      (`SessionStore.fork_session/2`) and becomes `"<forked-1>"`;
    * a `"fileSize"` becomes `"<bytes>"`: a session transcript embeds the
      sandbox path, whose length differs per run and per machine, so the
      byte count is pinned only as present;
    * ISO-8601 timestamps within one day of the run (session `updatedAt`
      values derived from file mtimes) become `"<now>"`; timestamps written
      by scenarios must therefore be dated in the past;
    * anonymous functions are replaced by `:__fun__` so fixtures stay
      evaluable.

  ## Fake CLI

  `<sandbox>/bin/claude` is a shell script that prints `Logged out.` and
  exits 0, and `<sandbox>/bin/claude-failing` prints `not authenticated` and
  exits 3. `session/logout` runs `cli_path auth logout` unless the adapter
  was started with `logout_cli: false`, so both outcomes are characterized
  without any chance of invoking the developer's real `claude` binary.

  ## Fixtures

  `assert_golden/4` compares the transcript with
  `test/fixtures/acp/claude/<area>/<name>.term`. Run the test with
  `CLAUDE_GOLDEN=update` to (re)write the fixture; that run always fails so
  regeneration is a deliberate, reviewed act.
  """

  import ExUnit.Assertions, only: [assert: 1, flunk: 1]

  alias ExMCP.ACP.Adapters.ClaudeSDK

  defmodule Entry do
    @moduledoc "One transcript entry: the step as executed and the adapter's normalized result."
    defstruct [:step, :result]

    @type t :: %__MODULE__{step: map(), result: map()}
  end

  @fixture_root Path.expand("../../fixtures/acp/claude", __DIR__)
  @sandbox_placeholder "<sandbox>"
  @sandbox_key_placeholder "<sandbox-key>"

  # Environment variables the adapter reads directly, with no option override.
  @leaky_env_vars ~w(CLAUDE_CODE_EXECUTABLE CLAUDE_CONFIG_DIR)
  @remote_env_vars ~w(NO_BROWSER SSH_CONNECTION SSH_CLIENT SSH_TTY CLAUDE_CODE_REMOTE)

  @fake_cli_script """
  #!/bin/sh
  # Fake `claude` for ExMCP.Test.ClaudeGolden: the adapter only ever runs
  # `claude auth logout`, so this succeeds with a fixed line of output.
  echo "Logged out."
  exit 0
  """

  @failing_cli_script """
  #!/bin/sh
  # Fake `claude` that refuses to log out, for the failure transcript.
  echo "not authenticated"
  exit 3
  """

  @generated_id_pattern ~r/(?<![\w])(ex_mcp_[a-z_]+|claude_sdk|tool)_(\d+)(?![\w])/
  @any_id_pattern ~r/(?<![\w])(?:ex_mcp_[a-z_]+|claude_sdk|tool)_(?:\d+|<\d+>)(?![\w])/
  @uuid_pattern ~r/\b[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\b/i
  @timestamp_pattern ~r/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,6})?(?:Z|[+-]\d{2}:\d{2})/
  @now_window_seconds 86_400

  @type step ::
          {:init, keyword()}
          | :post_connect
          | {:outbound, map() | (transcript() -> map())}
          | {:inbound, map() | (transcript() -> map())}
          | {:inbound_raw, String.t()}
          | {:respond_control, String.t(), map()}
          | {:respond_control_error, String.t(), String.t()}
          | {:list_sessions, map()}
          | {:fork_session, map() | (transcript() -> map())}
          | {:command, keyword()}
          | {:env, keyword()}
          | {:auth_methods, keyword()}
          | {:auth_methods_fresh, keyword()}
          | :capabilities
          | :modes
          | :config_options
          | {:write_file, Path.t() | (transcript() -> Path.t()), term()}
          | {:read_file, Path.t() | (transcript() -> Path.t())}
          | {:note, String.t()}

  @type entry :: Entry.t()
  @type transcript :: [entry()]

  @doc "Default `ClaudeSDK.init/1` options; `{:init, opts}` steps are merged over them."
  @spec default_init_opts() :: keyword()
  def default_init_opts do
    [
      cwd: "<sandbox>/project",
      claude_config_dir: "<sandbox>/claude",
      cli_path: "<sandbox>/bin/claude"
    ]
  end

  @doc "Default options for the `:command`, `:env` and `:auth_methods` steps."
  @spec default_cli_opts() :: keyword()
  def default_cli_opts, do: [cli_path: "<sandbox>/bin/claude"]

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
    ensure_clean_env!()
    ctx = create_sandbox!()

    steps =
      case steps do
        [{:init, _} | _] -> steps
        _ -> [{:init, []} | steps]
      end

    {transcript, _state} =
      Enum.reduce(steps, {[], nil}, fn step, {acc, state} ->
        {recorded_step, result, state} = execute(step, state, Enum.reverse(acc), ctx)
        {[%Entry{step: recorded_step, result: result} | acc], state}
      end)

    transcript |> Enum.reverse() |> normalize(ctx)
  end

  @doc """
  Runs the scenario and compares it with the committed fixture for
  `area`/`name`, returning the transcript for inline sanity assertions.
  """
  @spec assert_golden(String.t(), String.t(), [step()], keyword()) :: transcript()
  def assert_golden(area, name, steps, opts \\ []) do
    transcript = run(steps, opts)
    path = fixture_path(area, name)
    rendered = render(transcript)
    ensure_no_home!(rendered, path)

    cond do
      System.get_env("CLAUDE_GOLDEN") == "update" ->
        File.mkdir_p!(Path.dirname(path))
        File.write!(path, rendered)

        flunk(
          "Claude golden fixture (re)written at #{path}. " <>
            "Re-run the test without CLAUDE_GOLDEN to compare against it."
        )

      not File.exists?(path) ->
        flunk(
          "Missing Claude golden fixture #{path}. " <>
            "Run this test with CLAUDE_GOLDEN=update to generate it, then review and commit it."
        )

      true ->
        {expected, _binding} = path |> File.read!() |> Code.eval_string([], file: path)
        assert transcript == expected
        transcript
    end
  end

  @doc "Every SDK line the adapter wrote, in order."
  @spec writes(transcript()) :: [map()]
  def writes(transcript), do: Enum.flat_map(transcript, &Map.get(&1.result, :writes, []))

  @doc "Every ACP message emitted by the adapter, in order."
  @spec messages(transcript()) :: [map()]
  def messages(transcript), do: Enum.flat_map(transcript, &Map.get(&1.result, :messages, []))

  @doc "Every `session/update` notification the adapter emitted, in order."
  @spec updates(transcript()) :: [map()]
  def updates(transcript) do
    transcript |> messages() |> Enum.filter(&(&1["method"] == "session/update"))
  end

  @doc "The `sessionUpdate` type of every `session/update` notification, in order."
  @spec update_types(transcript()) :: [String.t()]
  def update_types(transcript) do
    transcript |> updates() |> Enum.map(&get_in(&1, ["params", "update", "sessionUpdate"]))
  end

  @doc "The result map of the most recent entry, or `nil` for an empty transcript."
  @spec last_result(transcript()) :: map() | nil
  def last_result([]), do: nil
  def last_result(transcript), do: List.last(transcript).result

  @doc "Ids of every SDK control request of `subtype` written so far, in order."
  @spec control_request_ids(transcript(), String.t()) :: [term()]
  def control_request_ids(transcript, subtype) do
    for %{"type" => "control_request", "request_id" => id, "request" => %{"subtype" => ^subtype}} <-
          writes(transcript),
        do: id
  end

  @doc "Id of the most recent SDK control request of `subtype`, or `nil`."
  @spec control_request_id(transcript(), String.t()) :: term() | nil
  def control_request_id(transcript, subtype),
    do: transcript |> control_request_ids(subtype) |> List.last()

  @doc """
  Ids of the ACP requests the adapter sent to the client, in order.

  Inside a step function the transcript is raw, so this returns the real
  integer ids (which is how a reply step answers a request); on a transcript
  returned by `run/2` it returns the `"acp-<n>"` placeholders instead.
  """
  @spec request_ids(transcript()) :: [term()]
  def request_ids(transcript) do
    for message <- messages(transcript),
        is_map(message),
        Map.has_key?(message, "method"),
        Map.has_key?(message, "id"),
        do: message["id"]
  end

  @doc "Distinct adapter-generated string ids in order of first appearance."
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
          "Claude golden transcripts depend on the environment: #{Enum.join(set, ", ")} " <>
            "is set and would change the adapter's CLI path or session store. Unset it to " <>
            "run golden scenarios (pass cli_path:/claude_config_dir: to {:init, opts} " <>
            "instead; the harness already points both into the sandbox)."
        )
    end
  end

  defp ensure_local_env! do
    case Enum.filter(@remote_env_vars, &System.get_env/1) do
      [] ->
        :ok

      set ->
        flunk(
          "ClaudeSDK.auth_methods/2 branches on #{Enum.join(set, ", ")}, which is set in " <>
            "this environment. The remote-login branch is deliberately not characterized " <>
            "(it is only reachable by mutating the OS environment); unset it to run the " <>
            "auth scenarios."
        )
    end
  end

  defp ensure_no_home!(rendered, path) do
    if String.contains?(rendered, "<home>") do
      flunk(
        "Claude golden transcript for #{path} reached the developer's home directory. " <>
          "Every path a scenario touches must live inside the run's sandbox."
      )
    end
  end

  # -- sandbox --------------------------------------------------------------

  defp create_sandbox! do
    sandbox = Path.join(System.tmp_dir!(), "claude_golden_#{System.unique_integer([:positive])}")

    for dir <- ["bin", "claude", "project"] do
      File.mkdir_p!(Path.join(sandbox, dir))
    end

    write_script!(Path.join([sandbox, "bin", "claude"]), @fake_cli_script)
    write_script!(Path.join([sandbox, "bin", "claude-failing"]), @failing_cli_script)

    ExUnit.Callbacks.on_exit(fn -> File.rm_rf(sandbox) end)

    aliases = [sandbox, realpath(sandbox)] |> Enum.uniq() |> longest_first()

    %{
      sandbox: sandbox,
      aliases: aliases,
      # The session store resolves symlinks before deriving a project key, so
      # the key a scenario writes under is the one of the resolved path.
      sandbox_key: sandbox |> realpath() |> project_key_form(),
      key_aliases: aliases |> Enum.map(&project_key_form/1) |> longest_first()
    }
  end

  defp write_script!(path, contents) do
    File.write!(path, contents)
    File.chmod!(path, 0o755)
  end

  defp project_key_form(path), do: Regex.replace(~r/[^a-zA-Z0-9]/, path, "-")

  # `/var` is a symlink to `/private/var` on macOS, so one alias is a suffix
  # of the other; replacing the longest first keeps the shorter one from
  # leaving a `-private` stub behind.
  defp longest_first(paths), do: Enum.sort_by(paths, &byte_size/1, :desc)

  # Resolves every symlink in `path` without changing the process working
  # directory (`/var` is a symlink to `/private/var` on macOS, so the adapter
  # reports paths the raw sandbox string does not match).
  defp realpath(path) do
    path
    |> Path.expand()
    |> Path.split()
    |> Enum.reduce("/", fn
      "/", acc ->
        acc

      part, acc ->
        joined = Path.join(acc, part)

        case File.read_link(joined) do
          {:ok, target} -> Path.expand(target, acc)
          {:error, _reason} -> joined
        end
    end)
  end

  defp substitute(binary, ctx) when is_binary(binary) do
    binary
    |> String.replace(@sandbox_key_placeholder, ctx.sandbox_key)
    |> String.replace(@sandbox_placeholder, ctx.sandbox)
  end

  defp substitute(list, ctx) when is_list(list), do: Enum.map(list, &substitute(&1, ctx))

  defp substitute(tuple, ctx) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> substitute(ctx) |> List.to_tuple()

  defp substitute(%{__struct__: _} = struct, _ctx), do: struct

  defp substitute(map, ctx) when is_map(map),
    do: Map.new(map, fn {k, v} -> {substitute(k, ctx), substitute(v, ctx)} end)

  defp substitute(other, _ctx), do: other

  defp ensure_sandboxed!(opts, ctx) do
    for key <- [:cwd, :claude_config_dir, :cli_path] do
      ensure_inside_sandbox!(Keyword.get(opts, key), ctx, "#{key} of ClaudeSDK.init/1")
    end

    case Keyword.get(opts, :env) do
      nil -> :ok
      env -> ensure_inside_sandbox!(env_config_dir(env), ctx, "CLAUDE_CONFIG_DIR in :env")
    end

    :ok
  end

  defp env_config_dir(env) when is_map(env), do: Map.get(env, "CLAUDE_CONFIG_DIR")

  defp env_config_dir(env) when is_list(env) do
    Keyword.get(env, :CLAUDE_CONFIG_DIR) ||
      case List.keyfind(env, "CLAUDE_CONFIG_DIR", 0) do
        {_key, value} -> value
        nil -> nil
      end
  end

  defp env_config_dir(_env), do: nil

  defp ensure_inside_sandbox!(nil, _ctx, _what), do: :ok

  defp ensure_inside_sandbox!(value, ctx, what) when is_binary(value) do
    if Enum.any?(ctx.aliases, &String.starts_with?(value, &1 <> "/")) do
      :ok
    else
      flunk(
        "Claude golden scenarios must keep the #{what} inside the sandbox " <>
          "(got #{inspect(value)}); the developer's real Claude configuration, " <>
          "credentials, sessions and projects are off limits."
      )
    end
  end

  defp ensure_inside_sandbox!(value, _ctx, what),
    do: flunk("The #{what} must be a sandbox path, got #{inspect(value)}")

  defp ensure_step_id!(%{"id" => id}, _transcript) when is_binary(id), do: :ok

  defp ensure_step_id!(%{"id" => id} = message, transcript) when is_integer(id) do
    if id in request_ids(transcript) do
      :ok
    else
      flunk(
        "Step #{inspect(message)} carries the integer ACP id #{id}, which the adapter " <>
          "never minted. Adapter ids are bare monotonic integers starting at 1, so a " <>
          "scenario must use a string id (answer a real request with a step function and " <>
          "ExMCP.Test.ClaudeGolden.request_ids/1)."
      )
    end
  end

  defp ensure_step_id!(_message, _transcript), do: :ok

  defp ensure_step_cwd!(message, ctx) do
    case get_in(message, ["params", "cwd"]) do
      nil -> :ok
      cwd -> ensure_inside_sandbox!(cwd, ctx, "params.cwd of #{inspect(message["method"])}")
    end
  end

  # -- step execution -------------------------------------------------------

  defp execute({:init, opts}, _state, _transcript, ctx) do
    opts = default_init_opts() |> Keyword.merge(opts) |> substitute(ctx)
    ensure_sandboxed!(opts, ctx)
    {:ok, state} = ClaudeSDK.init(opts)
    {%{kind: :init, opts: opts}, %{tag: :ok}, state}
  end

  defp execute(:post_connect, state, _transcript, _ctx) do
    {:ok, data, state} = ClaudeSDK.post_connect(state)
    {%{kind: :post_connect}, %{tag: :ok, writes: decode_writes(data)}, state}
  end

  defp execute({:outbound, message}, state, transcript, ctx) do
    message = resolve(message, transcript, ctx)
    ensure_step_id!(message, transcript)
    ensure_step_cwd!(message, ctx)
    {result, state} = message |> ClaudeSDK.translate_outbound(state) |> normalize_result()
    {%{kind: :outbound, message: message}, result, state}
  end

  defp execute({:inbound, message}, state, transcript, ctx) do
    message = resolve(message, transcript, ctx)

    {result, state} =
      message |> Jason.encode!() |> ClaudeSDK.translate_inbound(state) |> normalize_result()

    {%{kind: :inbound, message: message}, result, state}
  end

  defp execute({:inbound_raw, line}, state, _transcript, _ctx) when is_binary(line) do
    {result, state} = line |> ClaudeSDK.translate_inbound(state) |> normalize_result()
    {%{kind: :inbound_raw, line: line}, result, state}
  end

  defp execute({:respond_control, subtype, response}, state, transcript, ctx) do
    message = control_response(transcript, subtype, %{"response" => response})
    execute({:inbound, message}, state, transcript, ctx)
  end

  defp execute({:respond_control_error, subtype, error}, state, transcript, ctx) do
    message = control_response(transcript, subtype, %{"error" => error})
    execute({:inbound, message}, state, transcript, ctx)
  end

  defp execute({:list_sessions, params}, state, _transcript, ctx) do
    params = substitute(params, ctx)

    {result, state} =
      case ClaudeSDK.list_sessions(params, state) do
        {:ok, sessions, state} -> {%{tag: :ok, reply: sessions}, state}
        {:error, reason, state} -> {%{tag: :error, error: reason}, state}
      end

    {%{kind: :list_sessions, params: params}, result, state}
  end

  defp execute({:fork_session, params}, state, transcript, ctx) do
    params = resolve(params, transcript, ctx)

    {result, state} =
      case ClaudeSDK.fork_session(params, state) do
        {:ok, reply, state} -> {%{tag: :ok, reply: reply}, state}
        {:error, reason, state} -> {%{tag: :error, error: reason}, state}
      end

    {%{kind: :fork_session, params: params}, result, state}
  end

  defp execute({:command, opts}, state, _transcript, ctx) do
    opts = cli_opts(opts, ctx)
    {executable, args} = ClaudeSDK.command(opts)

    {%{kind: :command, opts: opts}, %{tag: :ok, reply: %{executable: executable, args: args}},
     state}
  end

  defp execute({:env, opts}, state, _transcript, ctx) do
    opts = cli_opts(opts, ctx)
    {%{kind: :env, opts: opts}, %{tag: :ok, reply: ClaudeSDK.env(opts)}, state}
  end

  defp execute({:auth_methods, opts}, state, _transcript, ctx) do
    ensure_local_env!()
    opts = cli_opts(opts, ctx)

    {%{kind: :auth_methods, opts: opts}, %{tag: :ok, reply: ClaudeSDK.auth_methods(opts, state)},
     state}
  end

  defp execute({:auth_methods_fresh, opts}, state, _transcript, ctx) do
    ensure_local_env!()
    opts = cli_opts(opts, ctx)

    {%{kind: :auth_methods_fresh, opts: opts}, %{tag: :ok, reply: ClaudeSDK.auth_methods(opts)},
     state}
  end

  defp execute(:capabilities, state, _transcript, _ctx) do
    {%{kind: :capabilities}, %{tag: :ok, reply: ClaudeSDK.capabilities()}, state}
  end

  defp execute(:modes, state, _transcript, _ctx) do
    {%{kind: :modes}, %{tag: :ok, reply: ClaudeSDK.modes()}, state}
  end

  defp execute(:config_options, state, _transcript, _ctx) do
    {%{kind: :config_options}, %{tag: :ok, reply: ClaudeSDK.config_options()}, state}
  end

  defp execute({:write_file, path, content}, state, transcript, ctx) do
    execute({:write_file, path, content, []}, state, transcript, ctx)
  end

  # `mtime:` pins the file's modification time. The session store reads mtime at
  # posix (whole second) granularity, so files written in the same second tie on
  # `lastModified`; the sort is stable, which leaves the tie broken by directory
  # listing order, and that differs between filesystems. Any scenario whose
  # expected output depends on session ordering must set distinct mtimes or it
  # will pass on one platform and fail on another.
  defp execute({:write_file, path, content, opts}, state, transcript, ctx) do
    path = resolve(path, transcript, ctx)
    content = substitute(content, ctx)
    ensure_inside_sandbox!(path, ctx, "path of a :write_file step")
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, render_file(content))

    step =
      case Keyword.fetch(opts, :mtime) do
        {:ok, mtime} when is_integer(mtime) ->
          File.touch!(path, mtime)
          %{kind: :write_file, path: path, content: content, mtime: mtime}

        :error ->
          %{kind: :write_file, path: path, content: content}
      end

    {step, %{}, state}
  end

  defp execute({:read_file, path}, state, transcript, ctx) do
    path = resolve(path, transcript, ctx)
    ensure_inside_sandbox!(path, ctx, "path of a :read_file step")

    result =
      case File.read(path) do
        {:ok, raw} -> %{exists: true, content: decode_file(raw)}
        {:error, _reason} -> %{exists: false}
      end

    {%{kind: :read_file, path: path}, result, state}
  end

  defp execute({:note, text}, state, _transcript, _ctx) when is_binary(text) do
    {%{kind: :note, text: text}, %{}, state}
  end

  defp cli_opts(opts, ctx) do
    default_cli_opts()
    |> Keyword.merge(opts)
    |> substitute(ctx)
    |> tap(fn opts ->
      ensure_inside_sandbox!(Keyword.get(opts, :cli_path), ctx, "cli_path of a CLI step")
    end)
  end

  defp resolve(fun, transcript, ctx) when is_function(fun, 1),
    do: fun.(transcript) |> substitute(ctx)

  defp resolve(value, _transcript, ctx), do: substitute(value, ctx)

  defp control_response(transcript, subtype, payload) do
    case control_request_id(transcript, subtype) do
      nil ->
        flunk("No #{inspect(subtype)} control request has been written yet, nothing to answer")

      request_id ->
        response =
          %{"subtype" => if(Map.has_key?(payload, "error"), do: "error", else: "success")}
          |> Map.put("request_id", request_id)
          |> Map.merge(payload)

        %{"type" => "control_response", "response" => response}
    end
  end

  defp render_file(content) when is_binary(content), do: content
  defp render_file(content) when is_map(content), do: Jason.encode!(content, pretty: true)

  defp render_file(content) when is_list(content),
    do: Enum.map_join(content, "", &(Jason.encode!(&1) <> "\n"))

  defp decode_file(raw) do
    lines = String.split(raw, "\n", trim: true)
    decoded = Enum.map(lines, &Jason.decode/1)

    if decoded != [] and Enum.all?(decoded, &match?({:ok, _}, &1)) do
      Enum.map(decoded, fn {:ok, value} -> value end)
    else
      raw
    end
  end

  # -- result normalization -------------------------------------------------

  defp normalize_result({:ok, :skip, state}), do: {%{tag: :ok, skipped: true}, state}
  defp normalize_result({:ok, data, state}), do: {%{tag: :ok, writes: decode_writes(data)}, state}
  defp normalize_result({:reply, result, state}), do: {%{tag: :reply, reply: result}, state}

  defp normalize_result({:reply_and_write, result, data, state}),
    do: {%{tag: :reply_and_write, reply: result, writes: decode_writes(data)}, state}

  defp normalize_result({:messages, messages, state}),
    do: {%{tag: :messages, messages: messages}, state}

  defp normalize_result({:messages_and_reply, messages, result, state}),
    do: {%{tag: :messages_and_reply, messages: messages, reply: result}, state}

  defp normalize_result({:messages_and_reply_and_write, messages, result, data, state}),
    do:
      {%{
         tag: :messages_and_reply_and_write,
         messages: messages,
         reply: result,
         writes: decode_writes(data)
       }, state}

  defp normalize_result({:messages_and_write, messages, data, state}),
    do: {%{tag: :messages_and_write, messages: messages, writes: decode_writes(data)}, state}

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
                "Claude SDK write is not newline-terminated NDJSON: #{inspect(bin)}"
        end

        bin
        |> binary_part(0, byte_size(bin) - 1)
        |> String.split("\n")
        |> Enum.map(&decode_line(&1, bin))
    end
  end

  defp decode_line("", bin) do
    raise ArgumentError, "Claude SDK write contains an empty NDJSON line: #{inspect(bin)}"
  end

  defp decode_line(line, _bin), do: Jason.decode!(line)

  # -- transcript normalization ---------------------------------------------

  defp normalize(transcript, ctx) do
    acc = %{
      ids: %{},
      counters: %{},
      uuids: %{},
      ctx: ctx,
      home: System.user_home!(),
      now: DateTime.utc_now(),
      phase: :step
    }

    {entries, _acc} =
      Enum.map_reduce(transcript, acc, fn %Entry{step: step, result: result}, acc ->
        {step, acc} = walk(step, %{acc | phase: :step}, :other)
        {result, acc} = walk(result, %{acc | phase: :result}, :other)
        {%Entry{step: step, result: result}, acc}
      end)

    entries
  end

  defp walk(binary, acc, _role) when is_binary(binary) do
    binary =
      acc.ctx.key_aliases
      |> Enum.reduce(binary, &String.replace(&2, &1, @sandbox_key_placeholder))
      |> then(fn text ->
        Enum.reduce(acc.ctx.aliases, text, &String.replace(&2, &1, @sandbox_placeholder))
      end)
      |> String.replace(acc.home, "<home>")
      |> normalize_timestamps(acc.now)

    acc =
      @generated_id_pattern
      |> Regex.scan(binary)
      |> Enum.reduce(acc, fn [id, kind, _n], acc -> register_id(acc, id, kind) end)

    binary =
      Regex.replace(@generated_id_pattern, binary, fn id, _kind, _n -> acc.ids[id] end)

    acc =
      @uuid_pattern
      |> Regex.scan(binary)
      |> Enum.reduce(acc, fn [uuid], acc -> register_uuid(acc, uuid) end)

    {Regex.replace(@uuid_pattern, binary, fn uuid -> acc.uuids[uuid] end), acc}
  end

  # A session transcript contains the sandbox path, whose length differs
  # between runs and machines, so the byte count is pinned only as present.
  defp walk(integer, acc, :file_size) when is_integer(integer), do: {"<bytes>", acc}

  defp walk(integer, acc, :acp_id) when is_integer(integer) do
    acc = register_id(acc, integer, "acp")
    {acc.ids[integer], acc}
  end

  defp walk(list, acc, role) when is_list(list),
    do: Enum.map_reduce(list, acc, &walk(&1, &2, role))

  defp walk(tuple, acc, role) when is_tuple(tuple) do
    {items, acc} = tuple |> Tuple.to_list() |> walk(acc, role)
    {List.to_tuple(items), acc}
  end

  defp walk(%{__struct__: _} = struct, acc, _role), do: {struct, acc}

  defp walk(map, acc, _role) when is_map(map) do
    {pairs, acc} =
      map
      |> Map.to_list()
      |> Enum.sort()
      |> Enum.map_reduce(acc, fn {key, value}, acc ->
        {key, acc} = walk(key, acc, :other)
        {value, acc} = walk(value, acc, child_role(key))
        {{key, value}, acc}
      end)

    {Map.new(pairs), acc}
  end

  defp walk(fun, acc, _role) when is_function(fun), do: {:__fun__, acc}
  defp walk(other, acc, _role), do: {other, acc}

  defp child_role("id"), do: :acp_id
  defp child_role("fileSize"), do: :file_size
  defp child_role(_key), do: :other

  defp normalize_timestamps(binary, now) do
    Regex.replace(@timestamp_pattern, binary, fn stamp ->
      case DateTime.from_iso8601(stamp) do
        {:ok, datetime, _offset} ->
          if abs(DateTime.diff(datetime, now, :second)) < @now_window_seconds,
            do: "<now>",
            else: stamp

        _ ->
          stamp
      end
    end)
  end

  defp register_id(%{ids: ids} = acc, key, _kind) when is_map_key(ids, key), do: acc

  defp register_id(%{ids: ids, counters: counters} = acc, key, kind) do
    n = Map.get(counters, kind, 0) + 1
    placeholder = if kind == "acp", do: "acp-<#{n}>", else: "#{kind}_<#{n}>"
    %{acc | ids: Map.put(ids, key, placeholder), counters: Map.put(counters, kind, n)}
  end

  # A UUID first seen in a step is a scenario literal and stays verbatim; one
  # first seen in a result was minted by SessionStore.fork_session/2.
  defp register_uuid(%{uuids: uuids} = acc, uuid) when is_map_key(uuids, uuid), do: acc

  defp register_uuid(%{phase: :step, uuids: uuids} = acc, uuid),
    do: %{acc | uuids: Map.put(uuids, uuid, uuid)}

  defp register_uuid(%{uuids: uuids, counters: counters} = acc, uuid) do
    n = Map.get(counters, "forked", 0) + 1

    %{
      acc
      | uuids: Map.put(uuids, uuid, "<forked-#{n}>"),
        counters: Map.put(counters, "forked", n)
    }
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
    |> Enum.reduce(acc, fn {key, value}, acc ->
      acc = collect_generated_ids(key, acc)
      collect_generated_ids(value, acc)
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
