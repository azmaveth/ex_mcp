// Tracks the reviewed ACP v2 draft surface without enabling it in ExMCP.
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import * as v2 from "@agentclientprotocol/sdk/experimental/v2";

const packagePath = new URL(
  "./node_modules/@agentclientprotocol/sdk/package.json",
  import.meta.url,
);
const v1SchemaPath = new URL(
  "./node_modules/@agentclientprotocol/sdk/schema/schema.json",
  import.meta.url,
);
const v2SchemaPath = new URL(
  "./node_modules/@agentclientprotocol/sdk/schema/v2/schema.unstable.json",
  import.meta.url,
);
const baselinePath = new URL("./acp_v2_contract.json", import.meta.url);

const [packageJson, v1SchemaText, v2SchemaText, baseline] = await Promise.all([
  readJson(packagePath),
  readFile(v1SchemaPath, "utf8"),
  readFile(v2SchemaPath, "utf8"),
  readJson(baselinePath),
]);
const v1Defs = JSON.parse(v1SchemaText).$defs;
const v2Defs = JSON.parse(v2SchemaText).$defs;
const v1SchemaSha256 = sha256(v1SchemaText);
const v2SchemaSha256 = sha256(v2SchemaText);

// These are the draft decisions that influence ExMCP's versioned architecture.
// A failure means the draft changed and requires review; it does not mean ExMCP
// should advertise protocolVersion 2.
assert.equal(v2.PROTOCOL_VERSION, 2);
assert.equal(v2.methods.agent.initialize, "initialize");
assert.equal(v2.methods.agent.session.prompt, "session/prompt");
assert.equal(v2.methods.agent.session.resume, "session/resume");
assert.equal(v2.methods.agent.session.close, "session/close");
assert.equal(v2.methods.client.session.update, "session/update");
assert.equal(v2.methods.protocol.cancelRequest, "$/cancel_request");
assert.equal(typeof v2.agentProtocolRouter, "function");
assert.equal(typeof v2.batchRequest, "function");

assert.ok(v1Defs, "v1 schema must contain $defs");
assert.ok(v2Defs, "v2 schema must contain $defs");
assert.deepEqual(
  [...v2Defs.InitializeRequest.required].sort(),
  ["info", "protocolVersion"],
);
assert.deepEqual(
  [...v2Defs.InitializeResponse.required].sort(),
  ["info", "protocolVersion"],
);
assert.equal(v2Defs.PromptResponse.properties.stopReason, undefined);

const stateUpdate = JSON.stringify(v2Defs.StateUpdate);
for (const state of ["RunningStateUpdate", "RequiresActionStateUpdate", "IdleStateUpdate"]) {
  assert.ok(stateUpdate.includes(state), `StateUpdate must include ${state}`);
}

const stdioMcpServer = v2Defs.McpServer.anyOf.find(
  (variant) => variant.properties?.type?.const === "stdio",
);
assert.ok(stdioMcpServer, "McpServer must include the stdio variant");
assert.ok(stdioMcpServer.required.includes("type"));
assert.ok(JSON.stringify(stdioMcpServer).includes("McpServerStdio"));

// SDK 1.4.0 added an explicitly unstable compaction experiment to both schema
// generations. V1 agents must gate it on this client capability; ExMCP does
// not advertise the capability or emit these updates yet.
assert.ok(v1Defs.ClientSessionCapabilities.properties.compaction);
for (const defs of [v1Defs, v2Defs]) {
  const sessionUpdate = JSON.stringify(defs.SessionUpdate);
  assert.ok(sessionUpdate.includes("CompactionUpdate"));
  assert.ok(sessionUpdate.includes("CompactionSummaryChunk"));
  assert.equal(defs.AuthMethodEnvVar, undefined);
}

if (process.argv.includes("--print-baseline")) {
  process.stdout.write(
    JSON.stringify(
      { reviewedSdkVersion: packageJson.version, v1SchemaSha256, v2SchemaSha256 },
      null,
      2,
    ) + "\n",
  );
  process.exit(0);
}

assert.equal(
  packageJson.version,
  baseline.reviewedSdkVersion,
  `ACP SDK ${packageJson.version} has not been reviewed; inspect both schemas and run this probe with --print-baseline after accepting the new contract`,
);
assert.equal(v1SchemaSha256, baseline.v1SchemaSha256, schemaDriftMessage("v1"));
assert.equal(v2SchemaSha256, baseline.v2SchemaSha256, schemaDriftMessage("v2 draft"));

process.stdout.write(
  JSON.stringify({
    ok: true,
    installedSdkVersion: packageJson.version,
    reviewedSdkVersion: baseline.reviewedSdkVersion,
    v1SchemaSha256,
    v2SchemaSha256,
  }) + "\n",
);

async function readJson(url) {
  return JSON.parse(await readFile(url, "utf8"));
}

function sha256(value) {
  return createHash("sha256").update(value).digest("hex");
}

function schemaDriftMessage(version) {
  return `ACP ${version} schema changed in SDK ${packageJson.version}; review the schema diff and migration guide, then run this probe with --print-baseline after accepting the new contract`;
}
