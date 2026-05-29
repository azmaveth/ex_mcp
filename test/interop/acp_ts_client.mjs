// TypeScript ACP client for interop testing against an ExMCP ACP agent.
// Spawns the agent process, runs initialize/session/prompt, and prints JSON.
import { spawn } from "node:child_process";
import { Readable, Writable } from "node:stream";
import * as acp from "@agentclientprotocol/sdk";

const agentCommand = process.argv[2];
const agentArgs = process.argv.slice(3);

if (!agentCommand) {
  process.stderr.write(
    JSON.stringify({ error: "Usage: node acp_ts_client.mjs <command> [args...]" }) +
      "\n",
  );
  process.exit(1);
}

const results = {
  connected: false,
  success: false,
  updates: [],
  text: "",
};

class TestClient {
  async requestPermission(params) {
    const option =
      params.options.find((item) => item.kind === "allow_once") ?? params.options[0];

    return {
      outcome: {
        outcome: option ? "selected" : "cancelled",
        optionId: option?.optionId,
      },
    };
  }

  async sessionUpdate(params) {
    results.updates.push(params.update.sessionUpdate);

    if (
      params.update.sessionUpdate === "agent_message_chunk" &&
      params.update.content?.type === "text"
    ) {
      results.text += params.update.content.text;
    }
  }

  async readTextFile(_params) {
    return { content: "mock file content" };
  }

  async writeTextFile(_params) {
    return {};
  }
}

const agentProcess = spawn(agentCommand, agentArgs, {
  stdio: ["pipe", "pipe", "pipe"],
});

agentProcess.stderr.on("data", (chunk) => {
  process.stderr.write(chunk);
});

const input = Writable.toWeb(agentProcess.stdin);
const output = Readable.toWeb(agentProcess.stdout);
const stream = acp.ndJsonStream(input, output);
const connection = new acp.ClientSideConnection((_agent) => new TestClient(), stream);

try {
  const initResult = await connection.initialize({
    protocolVersion: acp.PROTOCOL_VERSION,
    clientInfo: { name: "ts-acp-test-client", version: "1.0.0" },
    clientCapabilities: {
      fs: {
        readTextFile: true,
        writeTextFile: true,
      },
    },
  });

  results.connected = true;
  results.protocolVersion = initResult.protocolVersion;

  const sessionResult = await connection.newSession({
    cwd: process.cwd(),
    mcpServers: [],
  });
  results.sessionId = sessionResult.sessionId;

  const listResult = await connection.listSessions({ cwd: process.cwd() });
  results.sessions = listResult.sessions?.map((session) => session.sessionId) ?? [];

  const promptResult = await connection.prompt({
    sessionId: sessionResult.sessionId,
    prompt: [{ type: "text", text: "hello from TypeScript client" }],
  });
  results.stopReason = promptResult.stopReason;

  await connection.closeSession({ sessionId: sessionResult.sessionId });

  results.success =
    results.connected &&
    results.stopReason === "end_turn" &&
    results.text.includes("ExMCP ACP agent");
} catch (error) {
  results.error = error?.message ?? String(error);
} finally {
  agentProcess.kill();
}

process.stderr.write(JSON.stringify(results) + "\n");
process.exit(results.success ? 0 : 1);
