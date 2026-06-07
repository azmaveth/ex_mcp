// Everything-style TypeScript ACP client for interop testing against ExMCP agents.
import { spawn } from "node:child_process";
import { Readable, Writable } from "node:stream";
import * as acp from "@agentclientprotocol/sdk";

const agentCommand = process.argv[2];
const agentArgs = process.argv.slice(3);

if (!agentCommand) {
  process.stderr.write(
    JSON.stringify({
      error: "Usage: node acp_everything_ts_client.mjs <command> [args...]",
    }) + "\n",
  );
  process.exit(1);
}

const results = {
  connected: false,
  success: false,
  updates: [],
  text: "",
  clientRequests: [],
  lifecycle: [],
};

class EverythingClient {
  async requestPermission(params) {
    results.clientRequests.push("session/request_permission");
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

  async readTextFile(params) {
    results.clientRequests.push("fs/read_text_file");
    return { content: `mock file content for ${params.path}` };
  }

  async writeTextFile(_params) {
    results.clientRequests.push("fs/write_text_file");
    return {};
  }

  async createTerminal(_params) {
    results.clientRequests.push("terminal/create");
    return { terminalId: "term_ts_everything" };
  }

  async terminalOutput(_params) {
    results.clientRequests.push("terminal/output");
    return {
      output: "terminal output from TypeScript client",
      truncated: false,
      exitStatus: { exitCode: 0 },
    };
  }

  async waitForTerminalExit(_params) {
    results.clientRequests.push("terminal/wait_for_exit");
    return { exitCode: 0 };
  }

  async killTerminal(_params) {
    results.clientRequests.push("terminal/kill");
    return {};
  }

  async releaseTerminal(_params) {
    results.clientRequests.push("terminal/release");
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
const connection = new acp.ClientSideConnection((_agent) => new EverythingClient(), stream);

try {
  const initResult = await connection.initialize({
    protocolVersion: acp.PROTOCOL_VERSION,
    clientInfo: { name: "ts-acp-everything-client", version: "1.0.0" },
    clientCapabilities: {
      fs: {
        readTextFile: true,
        writeTextFile: true,
      },
      terminal: true,
    },
  });

  results.connected = true;
  results.protocolVersion = initResult.protocolVersion;
  results.lifecycle.push("initialize");

  await connection.authenticate({ methodId: "agent" });
  results.lifecycle.push("authenticate");

  const sessionResult = await connection.newSession({
    cwd: process.cwd(),
    mcpServers: [],
  });
  results.sessionId = sessionResult.sessionId;
  results.modes = sessionResult.modes?.availableModes?.map((mode) => mode.id) ?? [];
  results.configOptions =
    sessionResult.configOptions?.map((option) => option.id) ?? [];
  results.lifecycle.push("session/new");

  await connection.loadSession({
    sessionId: sessionResult.sessionId,
    cwd: process.cwd(),
    mcpServers: [],
  });
  results.lifecycle.push("session/load");

  await connection.resumeSession({
    sessionId: sessionResult.sessionId,
    cwd: process.cwd(),
    mcpServers: [],
  });
  results.lifecycle.push("session/resume");

  const listResult = await connection.listSessions({ cwd: process.cwd() });
  results.sessions = listResult.sessions?.map((session) => session.sessionId) ?? [];
  results.lifecycle.push("session/list");

  await connection.setSessionMode({
    sessionId: sessionResult.sessionId,
    modeId: "plan",
  });
  results.lifecycle.push("session/set_mode");

  const configResult = await connection.setSessionConfigOption({
    sessionId: sessionResult.sessionId,
    configId: "model",
    value: "deep",
  });
  results.configAfterSet =
    configResult.configOptions?.find((option) => option.id === "model")
      ?.currentValue ?? null;
  results.lifecycle.push("session/set_config_option");

  const promptResult = await connection.prompt({
    sessionId: sessionResult.sessionId,
    prompt: [
      { type: "text", text: "hello from TypeScript everything client" },
      {
        type: "image",
        mimeType: "image/png",
        data: "aW1hZ2U=",
      },
      {
        type: "audio",
        mimeType: "audio/wav",
        data: "YXVkaW8=",
      },
      {
        type: "resource_link",
        uri: "file:///README.md",
        name: "README.md",
      },
      {
        type: "resource",
        resource: {
          uri: "file:///inline.txt",
          mimeType: "text/plain",
          text: "inline resource",
        },
      },
    ],
  });
  results.stopReason = promptResult.stopReason;
  results.lifecycle.push("session/prompt");

  const cancelPrompt = connection.prompt({
    sessionId: sessionResult.sessionId,
    prompt: [{ type: "text", text: "cancel-me" }],
  });
  await new Promise((resolve) => setTimeout(resolve, 25));
  await connection.cancel({ sessionId: sessionResult.sessionId });
  const cancelResult = await cancelPrompt;
  results.cancelStopReason = cancelResult.stopReason;
  results.lifecycle.push("session/cancel");

  await connection.closeSession({ sessionId: sessionResult.sessionId });
  results.lifecycle.push("session/close");

  await connection.deleteSession({ sessionId: sessionResult.sessionId });
  results.lifecycle.push("session/delete");

  await connection.logout({});
  results.lifecycle.push("logout");

  const expectedUpdates = [
    "user_message_chunk",
    "agent_message_chunk",
    "agent_thought_chunk",
    "tool_call",
    "tool_call_update",
    "plan",
    "available_commands_update",
    "current_mode_update",
    "config_option_update",
    "session_info_update",
    "usage_update",
  ];

  const expectedClientRequests = [
    "session/request_permission",
    "fs/read_text_file",
    "fs/write_text_file",
    "terminal/create",
    "terminal/output",
    "terminal/wait_for_exit",
    "terminal/kill",
    "terminal/release",
  ];

  results.success =
    results.connected &&
    results.stopReason === "end_turn" &&
    results.cancelStopReason === "cancelled" &&
    expectedUpdates.every((type) => results.updates.includes(type)) &&
    expectedClientRequests.every((method) => results.clientRequests.includes(method)) &&
    results.sessions.includes(results.sessionId) &&
    results.text.includes("ExMCP everything agent");
} catch (error) {
  results.error = error?.stack ?? error?.message ?? String(error);
} finally {
  agentProcess.kill();
}

process.stderr.write(JSON.stringify(results) + "\n");
process.exit(results.success ? 0 : 1);
