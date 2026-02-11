// Minimal TypeScript MCP Server for interop testing
// Exposes tools, resources, and prompts over stdio transport
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";

const server = new McpServer({
  name: "ts-test-server",
  version: "1.0.0",
});

// Tool: echo - echoes input text
server.tool("echo", { text: z.string() }, async ({ text }) => ({
  content: [{ type: "text", text: `Echo: ${text}` }],
}));

// Tool: add - adds two numbers
server.tool("add", { a: z.number(), b: z.number() }, async ({ a, b }) => ({
  content: [{ type: "text", text: String(a + b) }],
}));

// Resource: test://greeting (name, uri, callback)
server.resource("greeting", "test://greeting", async (uri) => ({
  contents: [
    {
      uri: uri.href,
      mimeType: "text/plain",
      text: "Hello from TypeScript!",
    },
  ],
}));

// Prompt: simple_prompt
server.prompt("simple_prompt", async () => ({
  messages: [
    {
      role: "user",
      content: { type: "text", text: "This is a test prompt from TS" },
    },
  ],
}));

const transport = new StdioServerTransport();
await server.connect(transport);
