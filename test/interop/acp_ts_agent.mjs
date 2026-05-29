// Minimal TypeScript ACP agent for interop testing.
// Streams text over session/update, then returns a prompt stop reason.
import * as acp from "@agentclientprotocol/sdk";
import { randomUUID } from "node:crypto";
import { Readable, Writable } from "node:stream";

class TestAgent {
  constructor(connection) {
    this.connection = connection;
    this.sessions = new Map();
  }

  async initialize(_params) {
    return {
      protocolVersion: acp.PROTOCOL_VERSION,
      agentCapabilities: {
        loadSession: false,
        sessionCapabilities: {
          list: true,
          close: true,
        },
      },
    };
  }

  async newSession(params) {
    const sessionId = `sess_ts_${randomUUID()}`;
    this.sessions.set(sessionId, { cwd: params.cwd ?? null });
    return { sessionId };
  }

  async listSessions(_params) {
    return {
      sessions: Array.from(this.sessions.keys()).map((sessionId) => ({
        sessionId,
        name: "TypeScript ACP interop session",
      })),
    };
  }

  async closeSession(params) {
    this.sessions.delete(params.sessionId);
    return {};
  }

  async authenticate(_params) {
    return {};
  }

  async prompt(params) {
    const text = params.prompt
      .filter((block) => block.type === "text")
      .map((block) => block.text)
      .join("");

    await this.connection.sessionUpdate({
      sessionId: params.sessionId,
      update: {
        sessionUpdate: "agent_message_chunk",
        content: { type: "text", text: "Hello from " },
      },
    });

    await this.connection.sessionUpdate({
      sessionId: params.sessionId,
      update: {
        sessionUpdate: "agent_message_chunk",
        content: { type: "text", text: `TypeScript ACP agent: ${text}` },
      },
    });

    return { stopReason: "end_turn" };
  }

  async cancel(_params) {
    return;
  }
}

const input = Writable.toWeb(process.stdout);
const output = Readable.toWeb(process.stdin);
const stream = acp.ndJsonStream(input, output);

new acp.AgentSideConnection((connection) => new TestAgent(connection), stream);
