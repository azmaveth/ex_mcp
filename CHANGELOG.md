# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-05-25

### Added
- Initial release of ExMCP
- Complete Model Context Protocol implementation
- Protocol encoder/decoder for JSON-RPC messages
- Client implementation with automatic reconnection
- Server implementation with handler behaviour
- stdio transport for process communication
- SSE (Server-Sent Events) transport for HTTP streaming
- BEAM transport for native Erlang/Elixir communication
- Tool discovery and execution
- Resource listing and reading
- Prompt management
- Server manager for multiple connections
- Server discovery (npm packages, local directories)
- Request/response correlation
- Concurrent request handling
- Error handling and validation
- Type specifications throughout

### Features
- Full MCP specification compliance
- Multiple transport layer support (stdio, SSE, BEAM)
- Both client and server implementations
- Extensible architecture
- Supervision tree integration