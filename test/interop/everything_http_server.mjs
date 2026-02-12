/**
 * Launches the MCP "everything" server on streamable HTTP transport.
 * Listens on a random available port and prints the port number to stdout
 * so the Elixir test process can read it.
 *
 * Uses the official @modelcontextprotocol/server-everything package.
 */

import { StreamableHTTPServerTransport } from "@modelcontextprotocol/sdk/server/streamableHttp.js";
import { InMemoryEventStore } from "@modelcontextprotocol/sdk/examples/shared/inMemoryEventStore.js";
import { createServer } from "@modelcontextprotocol/server-everything/dist/everything.js";
import { randomUUID } from "node:crypto";
import { createServer as createHttpServer } from "node:http";

// We need express from the everything server's dependencies
import express from "express";

const app = express();

// NOTE: Do NOT use express.json() here. The SDK's StreamableHTTPServerTransport
// reads and parses the request body directly from the stream. Adding body-parsing
// middleware would consume the stream before the transport can read it.

// Enable CORS for all routes
app.use((req, res, next) => {
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS");
  res.setHeader(
    "Access-Control-Allow-Headers",
    "Content-Type, mcp-session-id, Last-Event-ID, mcp-protocol-version"
  );
  res.setHeader(
    "Access-Control-Expose-Headers",
    "mcp-session-id, Last-Event-ID, mcp-protocol-version"
  );
  if (req.method === "OPTIONS") {
    res.status(204).end();
    return;
  }
  next();
});

// Track transports by session ID
const transports = new Map();

app.post("/mcp", async (req, res) => {
  try {
    const sessionId = req.headers["mcp-session-id"];
    let transport;

    if (sessionId && transports.has(sessionId)) {
      transport = transports.get(sessionId);
    } else if (!sessionId) {
      const { server, cleanup, startNotificationIntervals } = createServer();

      const eventStore = new InMemoryEventStore();
      transport = new StreamableHTTPServerTransport({
        sessionIdGenerator: () => randomUUID(),
        eventStore,
        onsessioninitialized: (sid) => {
          transports.set(sid, transport);
        },
      });

      server.onclose = async () => {
        const sid = transport.sessionId;
        if (sid && transports.has(sid)) {
          transports.delete(sid);
          await cleanup();
        }
      };

      await server.connect(transport);
      await transport.handleRequest(req, res);
      startNotificationIntervals(transport.sessionId);
      return;
    } else {
      res.status(400).json({
        jsonrpc: "2.0",
        error: { code: -32000, message: "Bad Request: No valid session ID" },
        id: req?.body?.id,
      });
      return;
    }

    await transport.handleRequest(req, res);
  } catch (error) {
    console.error("Error handling POST:", error);
    if (!res.headersSent) {
      res.status(500).json({
        jsonrpc: "2.0",
        error: { code: -32603, message: "Internal server error" },
        id: req?.body?.id,
      });
    }
  }
});

app.get("/mcp", async (req, res) => {
  const sessionId = req.headers["mcp-session-id"];
  if (!sessionId || !transports.has(sessionId)) {
    res.status(400).json({
      jsonrpc: "2.0",
      error: { code: -32000, message: "Bad Request: No valid session ID" },
    });
    return;
  }

  const transport = transports.get(sessionId);
  await transport.handleRequest(req, res);
});

app.delete("/mcp", async (req, res) => {
  const sessionId = req.headers["mcp-session-id"];
  if (!sessionId || !transports.has(sessionId)) {
    res.status(400).json({
      jsonrpc: "2.0",
      error: { code: -32000, message: "Bad Request: No valid session ID" },
    });
    return;
  }

  const transport = transports.get(sessionId);
  await transport.handleRequest(req, res);
});

// Listen on port 0 for random available port
const httpServer = createHttpServer(app);
httpServer.listen(0, "127.0.0.1", () => {
  const port = httpServer.address().port;
  // Print port to stdout so Elixir can read it
  process.stdout.write(`PORT:${port}\n`);
});

// Handle shutdown
process.on("SIGTERM", async () => {
  for (const [sid, transport] of transports) {
    try {
      await transport.close();
      transports.delete(sid);
    } catch (_) {}
  }
  httpServer.close();
  process.exit(0);
});

process.on("SIGINT", async () => {
  for (const [sid, transport] of transports) {
    try {
      await transport.close();
      transports.delete(sid);
    } catch (_) {}
  }
  httpServer.close();
  process.exit(0);
});
