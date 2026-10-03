# ExMCP v2 Release Assessment

- **Reviewed:** 2026-10-03
- **Released baseline:** `v1.5.0`
- **Integrated source baseline:** `e4d2fc3`
- **Status:** Assessment and proposed release scope; no package or namespace decision accepted
- **Canonical plan:** [V2_ROADMAP.md](./V2_ROADMAP.md)

## Release conclusion

The library has a stable protocol baseline and useful split preparation, but
neither the package cutover nor the architectural v2 roadmap is complete.
Choose the release scope before treating any remaining roadmap phase as
optional. The existing full roadmap remains authoritative until that decision
is recorded.

**Proposed:** a focused v2 containing the MCP/ACP split, any accepted Arbor
rename, optional HTTP server dependencies, documented API removals, and release
qualification. This uses the early-release option already recorded in roadmap
§10.2 item 8; it does not mark the runtime/scheduler work complete or deferred.

ACP **protocol** v2 is a separate upstream effort. Its implementation is not a
library-v2 release requirement; retain the gates in
[ACP_V2_TRACKING.md](./ACP_V2_TRACKING.md).

## Work already integrated

| Area | Evidence and current outcome |
|---|---|
| MCP wire baseline | Modern MCP and retained legacy revisions, MRTR, subscriptions, Tasks, conformance and official-SDK lanes exist. The completed [migration plan](./MCP_2026_07_28_MIGRATION_PLAN.md) is release history, not unfinished v2 scope. |
| Store foundations | Accepted [store ADR](./STORE_ADAPTER.md), internal `SessionStore` behaviour, default ETS, opt-in DETS and a common contract suite. |
| Runtime characterization | `test/ex_mcp/server/runtime_characterization_test.exs` pins callback PID/links, cancellation, timeout, state ordering and basic sibling-server isolation. It does not implement a runtime or scheduler. |
| ACP adapter foundations | Pi/Codex internal boundaries and Claude/Pi/Codex golden-transcript suites provide a comparison baseline for extraction. |
| ACP split preparation | PR #71 narrowed helper documentation, promoted `AdapterEvents`, and added `mix acp.gen_shims`; the task is inert until an ACP dependency is installed. |
| Post-1.5 adapter behavior | Claude mode kinds/quota, message-specific forks and chunk message IDs, plus Pi correlation/order fixes are integrated. |
| Post-1.5 security/lifecycle | Trust-store deadlines, client shutdown/link/deadline/delivery behavior, metadata checks, redaction and raised Mint/Cowlib floors are integrated. See `CHANGELOG.md` for migration effects. |
| Claude MCP launch fix | `e4d2fc3` stops claiming unsupported session-supplied MCP transports and keeps launch-time MCP configuration out of argv through a private configuration file. Extraction must include this commit. |
| Pi subprocess test cleanup | Adapted the remaining stdout suppression from `014beb6` into the managed-model confirmation test. #71 already made the child exit on stdin EOF; suppressing its output also avoids EPIPE noise during teardown. |

Local validation of the integrated tree passed compilation with warnings as
errors, formatting of changed code/tests, strict Credo, documentation generation,
and the default test suite: 20 doctests, 34 properties, 5,330 tests, no failures
(218 excluded). External conformance, vendor CLI and other excluded lanes are
still release qualification work. `mix.exs` still identifies one `:ex_mcp`
package at `1.5.0` and requires Cowboy/Cowlib.

## Branch reconciliation

`origin` was fetched with pruning. GitHub has only `master`; the old remote
feature branches were already deleted. The following local branches were
removed after verifying their changes:

| Removed branch | Tip | Preservation evidence |
|---|---|---|
| `acp-split-prep` | `109b78d` | Complete tree equals squash merge `b087aa3` (#71). |
| `fix/cacerts-deadline` | `e1d311a` | Complete tree equals squash merge `5b0e116` (#74). |
| `claude/fervent-swanson-44cd3d` | `15c33f8` | Complete tree equals squash merge `fc61f84` (#72). |
| `claude/wizardly-cerf-11dfd4` | `e4d2fc3` | Fast-forwarded into `master`. |
| `claude/heuristic-driscoll-2fc2f7` | `014beb6` | Orphan-child correction already in #71; remaining stdout suppression adapted and tested. |
| `co` | `3294d12` | Already an ancestor of `master`. |

Two clean Claude worktrees were removed. Their ignored local settings were
preserved under `tmp/branch-cleanup-2026-10-03/` in the main checkout. The
remaining local branches are `master` and `spike/acp-cutover`; the latter and
its dirty worktree remain intact. Draft PR #21 remains open for future work.

## Existing extraction and preserved spike

A sibling repository exists at `/Users/azmaveth/code/ex_acp`: clean local
commit `58dee1d`, with no Git remote configured. It is a concrete extraction
prototype, not a published package or completed cutover. Its project declares
`:ex_acp` version `0.1.0`, with Jason and telemetry as runtime dependencies.

The dirty `spike/acp-cutover` worktree is preserved separately. Do not discard
it or overwrite its dependency/shim edits while evaluating the clean sibling.
Neither prototype should be merged wholesale without reconciling it with the
integrated MCP source and the final package identity.

Known extraction work remains:

- Refresh ACP sources, fixtures and tests through `e4d2fc3`; the clean sibling
  predates later fixes, including the private Claude MCP configuration work.
- Replace or formally govern copied security-sensitive helpers and stdio
  overlays. `scripts/extract_from_ex_mcp.sh` currently copies JSON-RPC, line
  buffering, port-environment, framing and diagnostics helpers into ACP.
  Stale duplicate implementations can miss lifecycle/security fixes.
- Fix `test/ex_acp/integration/acp_interop_test.exs`, which still calls
  `Application.ensure_all_started(:ex_mcp)` in its subprocess bootstrap.
  An ACP-only interoperability run must start the ACP application.
- Complete independent CI, docs, package contents and interoperability
  qualification before replacing the canonical implementation.
- Keep regeneration one-way while the monolith is canonical; explicitly
  retire the extraction script when ACP becomes the source of truth.

The source footprint measured at historical audit snapshot `fc61f84` was
60 ACP files including its facade and 26,692 of 101,085 library lines (26.4%).
These figures exclude `e4d2fc3` and are not a current cutover measurement.
Remeasure archive size, clean compile time and consumer dependency/app count;
line counts alone do not establish the value of the split.

## Decisions required before moving the public contract

| Decision | Required record |
|---|---|
| Identity | Accept or reject `arbor_mcp` / `arbor_acp`; separately choose GitHub paths, Hex names, modules, OTP apps, configuration and telemetry namespaces. A GitHub redirect does not migrate the other identities. |
| Repository topology | Choose one multi-package repository or separate repositories, release owners/order and compatibility ranges. The roadmap initially preferred one repository; the local spike uses two and this decision remains open. |
| Shared mechanics | Decide whether a small neutral framing/subprocess contract merits a shared package; avoid making ACP depend on the full MCP package. Trivial helpers alone do not justify a third package. |
| MCP integration | Keep ACP `mcpServers` descriptors as ACP-owned data; place MCP runtime integration and BEAM-specific extensions in an optional bridge. |
| Migration | Decide whether a final 1.x release supplies forwarding modules, their support period, and which compatibility names disappear in v2. |
| v2 size | Explicitly accept focused v2 or retain the full architectural release. Record how unfinished breaking work is scheduled. |

The current shim generator hardcodes `ExACP`, `:ex_acp` and corresponding
telemetry prefixes. Adapt it after naming is decided. It forwards functions,
types and callbacks, but cannot preserve `%ExMCP.ACP.*{}` struct identity.
Document affected patterns and runtime state inspection; also review wire
extension keys, generated identifiers and Pi's persisted session-map path.

## Proposed focused-v2 implementation checklist

1. **Freeze the migration baseline.** Generate a reproducible API manifest
   from the final 1.x release, covering modules/exports/callbacks/types/structs,
   options, configuration, telemetry, process names and observable defaults.
2. **Complete the package cutover.** Refresh and qualify the extraction,
   establish one canonical ACP implementation, adapt shims if retained, and
   demonstrate MCP-only, ACP-only and combined consumers without cycles.
3. **Make HTTP listeners optional.** Rework draft PR #21 against the current
   source on the v2 branch. It conflicts; despite its title/body, its current
   dependency diff keeps Cowboy required and only adds optional Bandit. It is
   useful groundwork, not the completed optional-server change. Provide Cowboy
   and Bandit adapters, missing-adapter diagnostics, listener ownership and
   shutdown behavior, retaining Cowboy `:ranch_ref` where selected.
4. **Remove accepted deprecated APIs.** Verify replacements before removing
   `Server.Tools` and companions, `HTTPServer` / `HTTPServerWithVersion`,
   content transformation stubs and agreed aliases. Every removal needs an
   API-diff entry and a before/after migration example.
5. **Update artifacts and guides.** Package names/dependencies, docs URLs,
   examples, CI/release credentials, telemetry/config migration, and any
   transitional `ex_mcp` package must agree with the accepted topology.
6. **Qualify and soak.** Run the applicable gates below against packaged
   artifacts, publish at least one RC, and record durable release evidence.

## Full-roadmap work still outstanding

| Phase | Remaining implementation |
|---|---|
| 1: contracts | Proposed removal/replacement manifest; runtime/state/concurrency/legacy-support decisions; validated runtime configuration and reducer/effect contracts. Existing client builders are not this completed contract. |
| 2: runtime | Explicit server supervision subtree and reference; scoped session/subscription/cancellation/replay ownership; crash/restart/stop isolation. `ExMCP.Application` still starts singleton owners. |
| 3: dispatch/scheduler | One request pipeline and bounded supervised work, queue policy, cancellation/deadline propagation and state-commit guarantees. Stdio/test share `Server.Dispatch`; HTTP retains separate `MessageProcessor` lifecycle semantics. |
| 4: stores | Deliberate public contracts, runtime-owned adapter lifecycle and payload-safe store telemetry. The internal ETS/DETS seam is groundwork, not the whole target. |
| 5: public API | Unified `Server.Result`, selected DSL constraints/composition, and bracketed client connection ownership. No `with_connection` helper or unified result facade exists. |
| 6–7: migration/release | Accepted removals, final API diff/guide, cross-transport equivalence, runtime pressure/isolation/upgrade evidence and v2 RC/soak. |

If focused v2 is accepted, later 2.x work must remain compatible or opt-in.
Changing callback PID/links, state order, default cancellation, runtime ownership
or restart behavior requires another major unless included in the accepted v2
contract. Existing tests show a HandlerServer client timeout leaves its callback
running, while HTTP `MessageProcessor` has temporary-handler timeout semantics;
a common scheduler must resolve that difference explicitly.

## Release evidence required

- Clean independent consumer builds and package inspection, with measured
  archive/compile/dependency results and lowest/newest dependency contracts.
- MCP legacy compliance, modern external conformance and official-SDK lanes;
  ACP SDK, adapter golden transcripts and credential-free real-CLI lifecycle.
- Cowboy, Bandit and Phoenix-mounted HTTP coverage; stdio/ACP builds without
  Cowboy/Cowlib; retained security, framing, Unicode and replay contracts.
- Namespace/shim/config/telemetry/struct migration checks and combined-package
  loading without duplicate modules or application-name collisions.
- Performance budgets against the final 1.x artifact; applicable isolation,
  cancellation, pressure, persistence and upgrade gates for accepted changes.
- RC artifact, soak duration, release owner and durable evidence for each gate.

## Proposed GitHub and Hex migration

If separate Arbor repositories are accepted, transfer the existing repository
to `trust-arbor/arbor_mcp` so its history, issues, pull requests and stars remain
with the MCP lineage. Create `trust-arbor/arbor_acp` from the reconciled ACP
extraction. GitHub's transfer/rename redirect has one successor; it cannot
route the old repository to both packages. Put a split notice linking ACP in
the destination README.

The GitHub owner/admin uses repository **Settings → Danger Zone → Transfer**,
selects `trust-arbor`, and supplies the new repository name when permitted;
otherwise transfer first and rename in the receiving repository's settings.
Then update the clone's remote:

```sh
git remote set-url origin git@github.com:trust-arbor/arbor_mcp.git
```

GitHub automatically redirects ordinary repository URLs and Git operations.
Do not recreate `azmaveth/ex_mcp`: that replaces the redirect. Project-site
URLs and consumers of repository-hosted Actions require separate handling.
See GitHub's [transfer documentation](https://docs.github.com/en/repositories/creating-and-managing-repositories/transferring-a-repository)
and [rename documentation](https://docs.github.com/en/repositories/creating-and-managing-repositories/renaming-a-repository).

Treat `arbor_mcp` and `arbor_acp` as new Hex package identities with explicit
dependency migration; a GitHub redirect does not change a Mix dependency on
`ex_mcp`. Choose the OTP apps and module namespaces separately, update package
links/docs/release metadata, and qualify coexistence with any transitional
package. Hex's [publishing guide](https://hex.pm/docs/publish) distinguishes
package name from application name, and its [usage guide](https://hex.pm/docs/usage)
documents the `:hex` override. Hex ownership is also separate from GitHub;
[mix hex.owner](https://hex.hexdocs.pm/Mix.Tasks.Hex.Owner.html) manages it.

Preserve wire/storage identifiers such as `_meta.ex_mcp`, the BEAM capability
extension and Pi's persisted session-map path unless a separate migration is
accepted. Renaming a library does not justify silently changing those contracts.
No repository transfer, package publication or namespace rename was performed
as part of this assessment.
