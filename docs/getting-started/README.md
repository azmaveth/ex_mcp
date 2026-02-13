# Getting Started with ExMCP

Welcome to ExMCP! This section contains everything you need to get up and running quickly.

## Documentation Overview

```
📚 Getting Started
├── 🚀 QUICKSTART.md      - 5-minute setup guide
├── 🔄 MIGRATION.md       - Version upgrade guide
└── 📋 README.md          - This overview (you are here)
```

## Choose Your Path

### 🆕 New to ExMCP?
**Start here:** [QUICKSTART.md](QUICKSTART.md)
- Complete setup in 5 minutes
- Working examples for all transport types
- Basic client and server patterns

### 🔄 Upgrading ExMCP?
**Check:** [MIGRATION.md](MIGRATION.md)
- Step-by-step upgrade instructions
- Breaking changes and solutions
- Version-specific migration guides

## What is ExMCP?

ExMCP is a production-ready Elixir implementation of the Model Context Protocol (MCP), enabling AI models to securely interact with external tools and resources.

### Key Features

- **🚀 Ultra-fast**: Native BEAM transport with ~15μs local calls
- **🔌 Transport flexible**: HTTP, stdio, and native BEAM support  
- **🔐 Secure**: Complete OAuth 2.1 Resource Server implementation
- **🎯 MCP compliant**: Full support for MCP 2025-06-18 specification
- **🧪 Well tested**: Comprehensive test suite with 500+ tests
- **📚 Well documented**: Extensive guides and examples

### Transport Performance Comparison

```
Transport     Latency    Best For
─────────────────────────────────────────────
Native BEAM   ~15μs      Internal services
stdio         ~1-5ms     External tools  
HTTP          ~5-20ms    Network clients
```

## Common Use Cases

### 🔧 Tool Integration
Connect AI models to external tools and APIs:
- File system operations
- Database queries  
- API calls to external services
- Custom business logic

### 📊 Resource Access
Provide AI models with contextual information:
- Configuration files
- Documentation
- Real-time data feeds
- Search results

### 💬 Prompt Templates
Create reusable prompt patterns:
- Conversation starters
- Task-specific templates
- Multi-step workflows
- Parameterized prompts

## Architecture Overview

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   AI Model      │    │   ExMCP Client  │    │   ExMCP Server  │
│                 │◄──►│                 │◄──►│                 │
│ (Claude, GPT,   │    │ (Your App)      │    │ (Tools/Data)    │
│  etc.)          │    │                 │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘
        │                       │                       │
        └───────────────────────┼───────────────────────┘
                    MCP Protocol
```

**Flow:**
1. AI model requests available tools/resources via MCP client
2. MCP client communicates with your ExMCP server
3. Server executes operations and returns structured results
4. Results flow back to AI model for processing

## Next Steps

1. **📖 Read the [QUICKSTART.md](QUICKSTART.md)** - Get your first MCP server running
2. **📚 Explore the full [USER_GUIDE.md](../guides/USER_GUIDE.md)** - Deep dive into advanced features
3. **💡 Check out [examples/](../../examples/)** - Real-world implementation patterns

## Getting Help

- **💬 Issues**: [GitHub Issues](https://github.com/example/ex_mcp/issues)
- **📖 Documentation**: Full guides in the `docs/` directory
- **🔧 Troubleshooting**: [TROUBLESHOOTING.md](../TROUBLESHOOTING.md)
- **🛡️ Security**: [SECURITY.md](../SECURITY.md)

Welcome to the ExMCP community! 🎉