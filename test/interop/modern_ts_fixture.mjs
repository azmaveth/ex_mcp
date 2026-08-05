import {
  McpServer,
  acceptedContent,
  inputRequired,
} from "@modelcontextprotocol/server";
import { z } from "zod/v4";

/** Build the common SDK v2 server surface used by stdio and HTTP interop. */
export const createModernServer = ({ publishToolsChanged } = {}) => {
  const server = new McpServer(
    {
      name: "ts-modern-interop-server",
      version: "2.0.0",
    },
    {
      capabilities: {
        tools: { listChanged: true },
        resources: { listChanged: true, subscribe: true },
        prompts: { listChanged: true },
      },
      cacheHints: {
        "server/discover": { ttlMs: 1_000, cacheScope: "private" },
        "tools/list": { ttlMs: 0, cacheScope: "private" },
        "resources/list": { ttlMs: 0, cacheScope: "private" },
        "resources/read": { ttlMs: 0, cacheScope: "private" },
        "prompts/list": { ttlMs: 0, cacheScope: "private" },
      },
    }
  );

  server.registerTool(
    "echo",
    {
      title: "Echo",
      description: "Echo text supplied by the caller",
      inputSchema: z.object({ text: z.string() }),
      annotations: {
        readOnlyHint: true,
        destructiveHint: false,
        idempotentHint: true,
        openWorldHint: false,
      },
    },
    async ({ text }) => ({
      content: [{ type: "text", text: `Echo: ${text}` }],
    })
  );

  server.registerTool(
    "add",
    {
      title: "Add",
      description: "Add two numbers",
      inputSchema: z.object({ a: z.number(), b: z.number() }),
      outputSchema: z.object({ sum: z.number() }),
      annotations: {
        readOnlyHint: true,
        destructiveHint: false,
        idempotentHint: true,
        openWorldHint: false,
      },
    },
    async ({ a, b }) => {
      const output = { sum: a + b };
      return {
        content: [{ type: "text", text: String(output.sum) }],
        structuredContent: output,
      };
    }
  );

  server.registerTool(
    "inspect_context",
    {
      title: "Inspect MCP Context",
      description: "Return the validated MCP request envelope for interop assertions",
      inputSchema: z.object({}),
      annotations: {
        readOnlyHint: true,
        destructiveHint: false,
        idempotentHint: true,
        openWorldHint: false,
      },
    },
    async (_arguments, ctx) => {
      const envelope = ctx.mcpReq.envelope ?? {};
      const httpHeaders = ctx.http?.req
        ? Object.fromEntries(ctx.http.req.headers.entries())
        : undefined;
      const observed = {
        ...envelope,
        ...(httpHeaders ? { httpHeaders } : {}),
      };

      return {
        content: [{ type: "text", text: JSON.stringify(observed) }],
        structuredContent: observed,
      };
    }
  );

  server.registerTool(
    "onboard",
    {
      title: "Onboard",
      description: "Exercise a modern multi-round-trip elicitation",
      inputSchema: z.object({}),
      annotations: {
        readOnlyHint: false,
        destructiveHint: false,
        idempotentHint: false,
        openWorldHint: false,
      },
    },
    async (_arguments, ctx) => {
      const profile = acceptedContent(ctx.mcpReq.inputResponses, "profile");

      if (!profile || typeof profile.name !== "string") {
        return inputRequired({
          inputRequests: {
            profile: inputRequired.elicit({
              message: "Choose a TypeScript interop display name",
              requestedSchema: {
                type: "object",
                properties: { name: { type: "string" } },
                required: ["name"],
              },
            }),
          },
          requestState: "ts-modern-interop",
        });
      }

      return {
        content: [
          {
            type: "text",
            text: `${profile.name}:${ctx.mcpReq.requestState()}`,
          },
        ],
      };
    }
  );

  server.registerTool(
    "publish_tools_changed",
    {
      title: "Publish Tools Changed",
      description: "Publish a tools list-changed notification to active subscriptions",
      inputSchema: z.object({}),
      annotations: {
        readOnlyHint: false,
        destructiveHint: false,
        idempotentHint: false,
        openWorldHint: false,
      },
    },
    async () => {
      if (publishToolsChanged) publishToolsChanged();
      else server.sendToolListChanged();

      return { content: [{ type: "text", text: "published" }] };
    }
  );

  server.registerResource(
    "greeting",
    "test://modern-greeting",
    {
      title: "Modern Greeting",
      description: "A static resource served by the TypeScript SDK v2 fixture",
      mimeType: "text/plain",
      cacheHint: { ttlMs: 0, cacheScope: "private" },
    },
    async (uri) => ({
      contents: [
        {
          uri: uri.href,
          mimeType: "text/plain",
          text: "Hello from TypeScript MCP 2026-07-28!",
        },
      ],
    })
  );

  server.registerPrompt(
    "modern_prompt",
    {
      title: "Modern Prompt",
      description: "A prompt served by the TypeScript SDK v2 fixture",
      argsSchema: z.object({ subject: z.string().optional() }),
    },
    async ({ subject }) => ({
      messages: [
        {
          role: "user",
          content: {
            type: "text",
            text: `Modern prompt about ${subject ?? "interop"}`,
          },
        },
      ],
    })
  );

  return server;
};
