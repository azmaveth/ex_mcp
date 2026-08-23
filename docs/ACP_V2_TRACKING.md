# ACP Protocol v2 Tracking

- **Status:** Draft monitoring and architecture preparation; no production v2 support
- **Reviewed SDK:** `@agentclientprotocol/sdk` `1.4.0`
- **Last updated:** 2026-08-22
- **Upstream:** [ACP v2 draft announcement](https://agentclientprotocol.com/announcements/acp-v2-draft),
  [migration guide](https://agentclientprotocol.com/protocol/v2/migration)

This document tracks **ACP protocol v2**. It is separate from ExMCP's own
[`2.0` roadmap](./V2_ROADMAP.md); the two version numbers describe independent
compatibility boundaries.

## Current decision

ExMCP remains an ACP v1 implementation and advertises only
`protocolVersion: 1`. ACP v2 is still Draft, and its maintainers explicitly
require version negotiation, feature flags, and continued v1 support. Merely
adding `2` to the supported-version lists would falsely claim support for a
different initialization shape, prompt lifecycle, update model, capability
layout, and baseline session contract.

Draft work is limited to monitoring, interoperability probes, internal design,
and feedback. It must not change production negotiation or default behavior.

## Reviewed draft assumptions

The compatibility probes currently track these architectural assumptions:

- `session/prompt` acknowledges acceptance; `state_update` reports running,
  required-action, idle, and completion state;
- messages, tool calls, and plans use ID-keyed upserts where omission, `null`,
  replacement, and append are distinct operations;
- client filesystem and terminal-execution APIs are v1-only, while agent-owned
  terminal output is a display surface;
- client-provided tools move through MCP server configuration;
- initialization uses role-neutral `info` and `capabilities` fields;
- `session/list`, `session/resume`, and `session/close` are part of the v2
  session baseline; and
- v1 and v2 remain available side by side, selected per connection.

SDK 1.4.0 also introduced an explicitly **UNSTABLE** compaction experiment in
the v1 and v2 session-update unions. V1 requires clients to advertise
`session.compaction: {}` before an agent may send ID-addressed compaction
updates. ExMCP neither advertises that capability nor emits those update
variants; the probe records the draft shape without promoting it into the
supported API. The same release removed the former experimental `env_var`
authentication schema. ExMCP's Codex legacy-auth compatibility option remains
disabled by default and is not part of its conforming default surface.

## Compatibility evidence

The repository keeps three independent guards:

1. `test/interop/acp_v2_draft_probe.mjs` checks the reviewed official SDK v1
   and v2 API and schema decisions.
2. `test/interop/acp_v2_contract.json` pins both reviewed schema digests.
   The normal ACP SDK interop lane checks this deterministic pin.
3. The scheduled `ACP ecosystem compatibility` workflow installs the newest
   official SDK and runs the same probe. A draft schema change fails that
   scheduled workflow for review but does not gate pull requests.

The native-agent tests also prove that a v2-shaped initialize request is
negotiated down to v1, and official SDK interop proves that an ExMCP v1 client
selects the v1 side of the SDK's dual-version agent.

After reviewing an upstream schema change, update the pinned SDK normally,
run:

```bash
cd test/interop
node acp_v2_draft_probe.mjs --print-baseline
```

Then update `acp_v2_contract.json`, add or change focused assertions for any
semantic difference, and run the complete ACP interop suite. Never update the
digest without reviewing the migration guide and schema diff.

## Target implementation boundaries

When implementation begins, keep two thin protocol surfaces around shared
application logic:

- v1 and v2 codecs, validation, capability maps, and fixtures remain separate;
- a version-neutral session core owns normalized messages, tool calls, plans,
  configuration, and lifecycle transitions;
- pure reducers receive normalized events and return state plus tagged effects;
- OTP processes retain ownership of ports, timers, cancellation, telemetry,
  and callback execution; and
- internal patch values represent absent, explicit `null`, and concrete values
  distinctly. A plain lookup that collapses those states is not sufficient for
  v2 upserts.

Generate and retain internal message IDs now where practical, but do not alter
v1 wire behavior. Keep v1 filesystem and terminal-execution dispatch isolated
so a v2 codec cannot advertise or invoke it accidentally. Keep MCP server
configuration in shared internal code because both ACP versions use it and v2
depends on MCP more heavily for client-side tools.

## Adoption gates

| ACP stage | ExMCP action |
|---|---|
| Draft | Monitor schema changes, maintain downgrade/routing probes, design boundaries, and run disposable spikes only. |
| Preview | Begin feature-flagged implementation after an official conformance path and at least one supported real agent are available. Keep v1 as the default. |
| Stable | Qualify both protocol versions through official SDK, conformance, real-agent, security, and soak coverage before enabling production support. |

ACP v1 support does not end when v2 stabilizes. Any future removal requires a
separate ExMCP major-version decision with ecosystem usage evidence and a
migration period.
