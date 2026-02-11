// TypeScript MCP Client for interop testing
// Connects to a server via stdio, runs operations, outputs JSON results to stderr
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";

const serverCommand = process.argv[2];
const serverArgs = process.argv.slice(3);

if (!serverCommand) {
  process.stderr.write(
    JSON.stringify({ error: "Usage: node ts_client.mjs <command> [args...]" }) +
      "\n"
  );
  process.exit(1);
}

const results = {};

try {
  const transport = new StdioClientTransport({
    command: serverCommand,
    args: serverArgs,
  });

  const client = new Client({
    name: "ts-test-client",
    version: "1.0.0",
  });

  await client.connect(transport);
  results.connected = true;

  // List and call tools
  try {
    const toolsResult = await client.listTools();
    results.tools = toolsResult.tools.map((t) => t.name);
  } catch (e) {
    results.tools_error = e.message;
  }

  // Call echo tool
  try {
    const echoResult = await client.callTool({
      name: "echo",
      arguments: { message: "hello from TS" },
    });
    results.echo = echoResult;
  } catch (e) {
    results.echo_error = e.message;
  }

  // Call add tool (if available)
  try {
    const addResult = await client.callTool({
      name: "add",
      arguments: { a: 10, b: 20 },
    });
    results.add = addResult;
  } catch (e) {
    results.add_error = e.message;
  }

  // List resources
  try {
    const resourcesResult = await client.listResources();
    results.resources = resourcesResult.resources.map((r) => r.uri);
  } catch (e) {
    results.resources_error = e.message;
  }

  // List prompts
  try {
    const promptsResult = await client.listPrompts();
    results.prompts = promptsResult.prompts.map((p) => p.name);
  } catch (e) {
    results.prompts_error = e.message;
  }

  // Ping
  try {
    await client.ping();
    results.ping = true;
  } catch (e) {
    results.ping_error = e.message;
  }

  await client.close();
  results.success = true;
} catch (e) {
  results.error = e.message;
  results.success = false;
}

// Output results to stderr (stdout is used by stdio transport)
process.stderr.write(JSON.stringify(results) + "\n");
process.exit(results.success ? 0 : 1);
