# Getting Started with ExMCP v2

This directory contains examples to get you started with ExMCP v2.

## Quick Start

**Recommended: Feature Demo**
```bash
./run_simple_demo.sh
```

This demonstrates v2 features without complex server setup:
- Structured Response types (text, JSON, error)
- Error handling and categories
- ClientConfig builder pattern
- JSON-RPC format conversion

## Examples

### Core v2 Features

**`working_demo.exs`** - Interactive demo of v2 Response and Error types
```bash
elixir working_demo.exs
```

**`hello_server_simple.exs`** - Basic server using handler callbacks
```bash
elixir hello_server_simple.exs
```

### Transport Examples (Advanced)

⚠️ **Note**: The DSL and full transport examples require additional setup and may need debugging. Start with the working demo above.

**STDIO Transport:**
- `hello_server_stdio.exs` - STDIO server (needs fixing)
- `hello_client_stdio.exs` - STDIO client

**HTTP Transport:**
- `hello_server_http.exs` - HTTP server with Mix.install
- `hello_client_http.exs` - HTTP client
- `hello_server_sse.exs` - HTTP with SSE streaming

**Native Transport:**
- `hello_server_native.exs` - BEAM transport (distributed mode)

**Universal Client:**
- `hello_client_all.exs` - Tests all transport types

## Demo Scripts

**Simple Feature Demo (Recommended):**
```bash
./run_simple_demo.sh     # Shows v2 features that work
```

**Full Transport Demo (Experimental):**
```bash
./run_demo.sh           # Attempts to test all transports
```

## Current Status

✅ **Working v2 Features:**
- Response and Error types
- Content extraction functions
- ClientConfig builder
- JSON-RPC format conversion
- Handler callback pattern

⚠️ **In Development:**
- DSL macros (deftool, defresource, etc.)
- Full transport server examples
- Distributed client-server communication

## What Each Example Shows

All examples implement the same basic functionality:
- A "hello" tool that greets users
- A resource providing server information
- Proper error handling

The examples are intentionally minimal to focus on transport setup and basic communication.

## Next Steps

After mastering these basics, explore:
- `/advanced/` - Complete servers with multiple tools, resources, and prompts
- `/utilities/` - Helper examples for responses, errors, and configuration
- `/migration_guide/` - Migrating from v1 to v2