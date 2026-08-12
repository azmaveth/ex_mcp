# ExMCP 1.0.0-rc.7

`1.0.0-rc.7` is the modern-preferred soak candidate that follows published
`1.0.0-rc.6`. It keeps the dual-era MCP 2026-07-28 default while packaging the
post-rc.6 transport lifecycle, ACP, and security hardening that cannot promote
directly from rc.6 to stable 1.0.

## What changed since rc.6

- Legacy Streamable HTTP GET SSE streams persist events before delivery, retain
  the MCP session across ordinary connection gaps, and replay from
  `Last-Event-ID` without duplication under bounded retention.
- ACP Pi managed event handling correctly processes multi-chunk agent streams,
  and adapter/MCP subprocesses isolate their environments by default.
- The 2026-08-12 security harden enforces protocol lifecycle, identity,
  authorization, network, resource, and privacy boundaries across MCP and ACP.
  See [SECURITY_AUDIT_2026-08-12.md](SECURITY_AUDIT_2026-08-12.md).
- CI and compatibility follow-ups keep Elixir 1.17 docs/tooling green, fail
  closed on invalid MRTR identities, and align fixtures with server-issued
  sessions and strict schema/privacy behavior.

`:prefer_modern` remains the application default. `:legacy_only` preserves the
exact rc.5 connection and wire policy. See the
[migration guide](getting-started/MIGRATION.md) for rollout steps and the
[API diff](API_DIFF_RC5_TO_1_0.md) for the public compatibility audit, including
the rc.7 delta note.

> **Why another soak:** The SSE disconnect lifecycle intentionally retains a
> session after an ordinary GET-stream disconnect, and the security harden
> changes session issuance, outbound network policy, and subprocess defaults.
> Those observable compatibility changes restart the stable-1.0 clock. See the
> [2.0 roadmap backport lane](V2_ROADMAP.md#8-the-1x-backport-lane).

## Installation

```elixir
{:ex_mcp, "~> 1.0.0-rc.7"}
```

Pin the rollout policy when it must not change with a package upgrade:

```elixir
config :ex_mcp, protocol_mode: :prefer_modern
# Emergency rollback / exact rc.5 wire path:
# config :ex_mcp, protocol_mode: :legacy_only
```

## Security and dependencies

The release retains the patched Mint, Mint WebSocket, Plug, Plug Cowboy,
Cowboy, Cowlib, HPAX, Decimal, and related runtime dependency set from rc.6.
The locked dependency set has no critical or high-severity advisories.

Cowlib 2.19.0, the newest compatible release, retains two upstream advisories:

- `EEF-CVE-2026-43966` (medium) is mitigated because Plug rejects CR/LF/NUL in
  response-header values and Cowboy independently terminates invalid response
  headers before serialization.
- `EEF-CVE-2026-43969` (low) affects `cow_cookie:cookie/1`, which ExMCP and its
  Plug/Cowboy response path do not call.

These acknowledgements are exact IDs in the Hex audit configuration. CI still
fails on every unacknowledged advisory and builds the real Hex archive.

## Qualification evidence and ownership

The release tag identifies the exact source commit. Its GitHub Actions run is
the durable evidence record; the external-conformance job uploads the complete
runner output as `mcp-2026-07-28-conformance`.

| Gate | Owner | rc.7 evidence |
|---|---|---|
| Legacy byte fixtures | Protocol maintainer | `mix test.suite compliance`; version characterization and cross-version suites |
| Seven-row era matrix | Protocol maintainer | stdio/HTTP compatibility tests in the full CI and coverage jobs |
| Modern conformance and SDK v2 interop | Release maintainer | external conformance artifact plus six isolated legacy/modern Node.js lanes |
| MRTR retry, tamper, replay, expiry, and rotation | Security maintainer | MRTR, request-state, replay-cache, integration, and coverage suites |
| Subscription pressure, isolation, reconnect, and cluster fanout | Runtime maintainer | subscription, PubSub, multi-node, performance, stress, and coverage suites |
| OAuth/CIMD/SSRF/credential isolation | Security maintainer | authorization matrix, redirect leakage, metadata fetch, and security suites |
| Modern-preferred soak | Release maintainer | begins when rc.7 is published; minimum seven calendar days |
| Security, resource bounds, and public API | Release maintainer | `mix hex.audit`, Sobelow, performance/stress CI, API diff, and package build |
| Mixed-cluster rollback drill | Deployment operator | required before stable 1.0; record the operator run in the stable release evidence |

The external suite is pinned to
`@modelcontextprotocol/conformance@0.2.0-alpha.10` because no stable
2026-07-28-aware runner is published; its CI lane pins Node.js 22 because the
harness imports `fs.globSync`. Official TypeScript SDK v2 interop pins
`@modelcontextprotocol/client`, `server`, and `node` to `2.0.0`. The legacy
conformance baseline remains pinned independently to `0.1.16`.

## Load and regression budget

The qualifying workload is `mix test.suite performance` on OTP 27 / Elixir
1.17. It covers the committed microbenchmarks, concurrent serialization,
high-volume tool request preparation, large payloads, a ten-second sustained
load interval, security checks, schema compilation, subscription queue
pressure, and slow-consumer behavior.

The rc.7 gate is:

- zero errors or timeouts in the performance and stress suites;
- no committed absolute performance threshold may regress;
- shared legacy workloads may not regress more than 20% in median runtime or
  10% in retained memory versus the rc.5 baseline on the same runner;
- process and mailbox counts must return to their bounded post-cleanup state;
- slow subscribers must coalesce or disconnect according to policy without
  cross-principal delivery.

Any unexplained breach blocks the release. Runner noise must be resolved by
repeating both revisions on the same machine, not by widening a threshold in
the release commit.

## Stable 1.0 gates still open

Publishing rc.7 restarts, but does not complete, the stable-release clock.
Stable `1.0.0` requires:

1. at least seven calendar days of final modern-preferred RC use without a
   release-blocking regression; and
2. a successful mixed-version cluster rollback drill with active
   subscriptions and in-flight MRTR operations.

Any wire-design, public-API, or documented lifecycle change requires another
RC and restarts the soak. Current `master` packaged as rc.7 meets that
condition because of the legacy SSE persistence/disconnect-lifecycle fix and
the 2026-08-12 security harden described above.
