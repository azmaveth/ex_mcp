// Everything-style TypeScript ACP agent for ExMCP interop testing.
import * as acp from "@agentclientprotocol/sdk";
import { randomUUID } from "node:crypto";
import { Readable, Writable } from "node:stream";

const UPDATED_AT = "2026-05-29T00:00:00Z";
const README_PATH = new URL("../../README.md", import.meta.url).pathname;
const WRITE_PATH = new URL("../../tmp/acp_everything.txt", import.meta.url).pathname;

class EverythingAgent {
  constructor(connection) {
    this.connection = connection;
    this.sessions = new Map();
    this.modeId = "code";
    this.model = "fast";
  }

  async initialize(_params) {
    return {
      protocolVersion: acp.PROTOCOL_VERSION,
      agentInfo: { name: "ts-acp-everything-agent", version: "1.0.0" },
      agentCapabilities: {
        auth: { logout: {} },
        loadSession: true,
        promptCapabilities: {
          audio: true,
          embeddedContext: true,
          image: true,
        },
        sessionCapabilities: {
          list: {},
          resume: {},
          close: {},
          delete: {},
        },
      },
      authMethods: [
        {
          id: "agent",
          name: "Agent-managed auth",
          description: "Interop auth method handled by the test agent",
        },
      ],
    };
  }

  async authenticate(params) {
    if (params.methodId !== "agent") {
      throw new Error(`unknown auth method: ${params.methodId}`);
    }
    return {};
  }

  async logout(_params) {
    return {};
  }

  async newSession(params) {
    const sessionId = `sess_ts_everything_${randomUUID()}`;
    this.sessions.set(sessionId, {
      sessionId,
      cwd: params.cwd,
      title: "TypeScript ACP everything session",
      updatedAt: UPDATED_AT,
    });

    return {
      sessionId,
      ...this.sessionState(),
    };
  }

  async loadSession(params) {
    this.ensureSession(params.sessionId, params.cwd);
    await this.sendLoadedHistory(params.sessionId);
    return this.sessionState();
  }

  async resumeSession(params) {
    this.ensureSession(params.sessionId, params.cwd);
    return this.sessionState();
  }

  async listSessions(_params) {
    return {
      sessions: Array.from(this.sessions.values()),
    };
  }

  async closeSession(params) {
    this.sessions.delete(params.sessionId);
    return {};
  }

  async deleteSession(params) {
    this.sessions.delete(params.sessionId);
    return {};
  }

  async setSessionMode(params) {
    this.modeId = params.modeId;
    await this.connection.sessionUpdate({
      sessionId: params.sessionId,
      update: { sessionUpdate: "current_mode_update", currentModeId: this.modeId },
    });
    return {};
  }

  async setSessionConfigOption(params) {
    if (params.configId !== "model") {
      throw new Error(`unknown config option: ${params.configId}`);
    }

    this.model = params.value;
    const configOptions = this.configOptions();

    await this.connection.sessionUpdate({
      sessionId: params.sessionId,
      update: { sessionUpdate: "config_option_update", configOptions },
    });

    return { configOptions };
  }

  async prompt(params) {
    const text = params.prompt
      .filter((block) => block.type === "text")
      .map((block) => block.text)
      .join("");

    if (text.includes("cancel-me")) {
      await this.connection.sessionUpdate({
        sessionId: params.sessionId,
        update: {
          sessionUpdate: "agent_message_chunk",
          content: { type: "text", text: "waiting for cancel" },
        },
      });

      return new Promise((resolve) => {
        const session = this.ensureSession(params.sessionId, process.cwd());
        session.cancelPrompt = () => resolve({ stopReason: "cancelled" });
      });
    }

    await this.sendEverythingTurn(params.sessionId, text);
    return { stopReason: "end_turn" };
  }

  async cancel(params) {
    const session = this.sessions.get(params.sessionId);
    session?.cancelPrompt?.();
    if (session) {
      session.cancelPrompt = null;
    }
  }

  ensureSession(sessionId, cwd) {
    if (!this.sessions.has(sessionId)) {
      this.sessions.set(sessionId, {
        sessionId,
        cwd,
        title: "TypeScript ACP everything session",
        updatedAt: UPDATED_AT,
      });
    }

    return this.sessions.get(sessionId);
  }

  async sendLoadedHistory(sessionId) {
    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "user_message_chunk",
        content: { type: "text", text: "loaded user message" },
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "agent_message_chunk",
        content: { type: "text", text: "loaded agent message" },
      },
    });
  }

  async sendEverythingTurn(sessionId, text) {
    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "user_message_chunk",
        content: { type: "text", text },
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "agent_thought_chunk",
        content: { type: "text", text: "thinking from TypeScript" },
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "plan",
        entries: [
          { content: "Inspect request", priority: "high", status: "completed" },
          { content: "Exercise client APIs", priority: "high", status: "in_progress" },
        ],
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "available_commands_update",
        availableCommands: [
          { name: "test", description: "Run the ACP everything fixture" },
        ],
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: { sessionUpdate: "current_mode_update", currentModeId: this.modeId },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "config_option_update",
        configOptions: this.configOptions(),
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "session_info_update",
        title: "TypeScript ACP everything prompt",
        updatedAt: UPDATED_AT,
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "tool_call",
        toolCallId: "tool_ts_1",
        title: "Read interop file",
        kind: "read",
        status: "pending",
        locations: [{ path: README_PATH, line: 1 }],
        rawInput: { path: README_PATH },
      },
    });

    const permission = await this.connection.requestPermission({
      sessionId,
      toolCall: {
        toolCallId: "tool_ts_1",
        title: "Read interop file",
        kind: "read",
        status: "pending",
        locations: [{ path: README_PATH }],
        rawInput: { path: README_PATH },
      },
      options: [{ kind: "allow_once", name: "Allow once", optionId: "allow" }],
    });

    const read = await this.connection.readTextFile({
      sessionId,
      path: README_PATH,
      line: 1,
      limit: 20,
    });

    await this.connection.writeTextFile({
      sessionId,
      path: WRITE_PATH,
      content: "updated",
    });

    const terminal = await this.connection.createTerminal({
      sessionId,
      command: "echo",
      args: ["hello"],
      cwd: process.cwd(),
    });

    const terminalOutput = await terminal.currentOutput();
    await terminal.waitForExit();
    await terminal.kill();
    await terminal.release();

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "tool_call_update",
        toolCallId: "tool_ts_1",
        status: "completed",
        content: [
          {
            type: "content",
            content: { type: "text", text: read.content },
          },
          {
            type: "terminal",
            terminalId: terminal.id,
          },
        ],
        rawOutput: { permission, terminalOutput },
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "usage_update",
        used: 42,
        size: 100,
      },
    });

    await this.connection.sessionUpdate({
      sessionId,
      update: {
        sessionUpdate: "agent_message_chunk",
        content: { type: "text", text: "Hello from TypeScript everything agent" },
      },
    });
  }

  sessionState() {
    return {
      modes: {
        availableModes: [
          { id: "code", name: "Code", description: "Make code changes" },
          { id: "plan", name: "Plan", description: "Plan before editing" },
        ],
        currentModeId: this.modeId,
      },
      configOptions: this.configOptions(),
    };
  }

  configOptions() {
    return [
      {
        id: "model",
        name: "Model",
        category: "model",
        type: "select",
        currentValue: this.model,
        options: [
          { value: "fast", name: "Fast" },
          { value: "deep", name: "Deep" },
        ],
      },
    ];
  }
}

const input = Writable.toWeb(process.stdout);
const output = Readable.toWeb(process.stdin);
const stream = acp.ndJsonStream(input, output);

new acp.AgentSideConnection((connection) => new EverythingAgent(connection), stream);
