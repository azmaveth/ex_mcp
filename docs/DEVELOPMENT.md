# ExMCP Development Guide

This guide covers everything you need to know for developing, testing, and contributing to ExMCP.

## Table of Contents

1. [Development Setup](#development-setup)
2. [Code Quality Tools](#code-quality-tools)
3. [Testing Strategy](#testing-strategy)
4. [Test Process Cleanup](#test-process-cleanup)
5. [Contributing](#contributing)
6. [Release Process](#release-process)

## Development Setup

### Prerequisites

- Elixir 1.14+ and Erlang/OTP 25+
- Git with hooks support
- Make (for using Makefile shortcuts)

### Initial Setup

```bash
# Clone the repository
git clone https://github.com/azmaveth/ex_mcp.git
cd ex_mcp

# Install dependencies and set up git hooks
make setup

# Verify setup
make quality
```

### Essential Development Commands

```bash
# Dependencies and compilation
mix deps.get          # Install dependencies
mix compile           # Compile the project
mix compile --warnings-as-errors  # Compile with strict warnings

# Code quality
mix format            # Format code (required before committing)
mix credo             # Static code analysis
mix dialyzer          # Type checking (run after significant changes)
mix sobelow --skip    # Security analysis

# Testing
mix test              # Run all tests
mix test test/ex_mcp/protocol_test.exs  # Run specific test file
mix coveralls.html    # Generate coverage report
MIX_ENV=test mix compile              # Compile for test environment

# Documentation
mix docs              # Generate documentation
iex -S mix            # Start interactive shell with project loaded

# Using Makefile shortcuts
make setup                       # Initial setup with git hooks
make quality                     # Run format check, credo, and compile checks
make all                         # Run all quality checks, tests, and dialyzer
make coverage                    # Generate HTML coverage report
make test                        # Run tests with automatic cleanup
make docs                        # Generate documentation
```

## Code Quality Tools

ExMCP uses a comprehensive set of code quality tools to ensure maintainable, reliable code:

### Formatter
- **Tool**: Elixir's built-in code formatter
- **Usage**: `mix format`
- **Purpose**: Consistent code formatting across the project
- **Required**: Yes, before every commit

### Credo
- **Tool**: Static code analysis
- **Usage**: `mix credo` or `mix credo --strict`
- **Purpose**: Code readability, consistency, and best practices
- **Configuration**: See `.credo.exs`
- **Thresholds**: 
  - Cyclomatic complexity max: 11
  - Function length max: reasonable (enforced by review)

### Dialyzer
- **Tool**: Type checking and static analysis
- **Usage**: `mix dialyzer`
- **Purpose**: Find type inconsistencies and potential runtime errors
- **When to run**: After significant changes or before releases
- **PLT location**: `_dialyzer/` (gitignored)

### Sobelow
- **Tool**: Security analysis
- **Usage**: `mix sobelow --skip`
- **Purpose**: Identify security vulnerabilities
- **Focus**: Input validation, SQL injection, XSS prevention

### ExCoveralls
- **Tool**: Test coverage analysis
- **Usage**: `mix coveralls.html`
- **Purpose**: Ensure comprehensive test coverage
- **Target**: Aim for >80% coverage on core modules

### Git Hooks
- **Pre-commit**: Runs formatter, credo, and compile checks
- **Pre-push**: Runs full test suite
- **Setup**: Automatically configured with `make setup`
- **Bypass**: Use `--no-verify` only in emergencies

## Testing Strategy

ExMCP uses a sophisticated test tagging strategy for efficient test execution across different scenarios.

### Test Categories

#### Core Test Suites

```bash
# Fast unit tests (default, ~5s)
mix test.suite unit

# MCP specification compliance tests
mix test.suite compliance

# Integration tests with real components
mix test.suite integration

# CI-appropriate tests (excludes slow tests)
mix test.suite ci

# All tests including slow ones
mix test.suite all
```

#### Transport-Specific Tests

```bash
# BEAM transport tests
mix test --only beam

# HTTP transport tests (Streamable HTTP with SSE)
mix test --only http

# stdio transport tests
mix test --only stdio
```

#### Feature-Specific Tests

```bash
# Security and authentication tests
mix test --only security

# Progress notification tests
mix test --only progress

# Resource management tests
mix test --only resources

# Performance and benchmarking tests
mix test --only performance
```

#### Development Workflows

```bash
# Include normally excluded slow tests
mix test --include slow

# Skip integration tests for faster feedback
mix test --exclude integration

# Skip external dependencies
mix test --exclude external

# List all available tags
mix test.tags
```

### Test Organization

Tests follow a clear structure mirroring the source code:

```
test/
├── ex_mcp/                    # Core module tests
│   ├── client/               # Client implementation tests
│   ├── server/               # Server implementation tests
│   ├── transport/            # Transport layer tests
│   └── protocol_test.exs     # Protocol tests
├── integration/              # Cross-component integration tests
├── compliance/               # MCP specification compliance tests
└── support/                  # Test helpers and utilities
```

### Test Patterns

#### Unit Tests
- Test individual functions and modules in isolation
- Use mocks for external dependencies
- Fast execution (typically <100ms per test)
- Tagged with `:unit` (default)

#### Integration Tests
- Test component interactions
- May use real external services or subprocess
- Slower execution
- Tagged with `:integration`

#### Compliance Tests
- Verify MCP specification adherence
- Test protocol message formats
- Cross-version compatibility
- Tagged with `:compliance`

#### Property-Based Tests
- Used for protocol encoding/decoding
- Input validation testing
- Edge case discovery
- Uses PropCheck library

### Writing Tests

Follow these patterns when writing tests:

```elixir
defmodule ExMCP.SomeModuleTest do
  use ExUnit.Case, async: true  # Use async: false for shared state
  
  # Add appropriate tags
  @moduletag :unit
  @moduletag :some_feature
  
  # Use descriptive test names
  describe "function_name/2" do
    test "handles valid input correctly" do
      # Arrange
      input = %{valid: "data"}
      
      # Act
      result = SomeModule.function_name(input, [])
      
      # Assert
      assert {:ok, expected} = result
    end
    
    test "returns error for invalid input" do
      # Test error conditions
      assert {:error, _reason} = SomeModule.function_name(nil, [])
    end
  end
end
```

## Test Process Cleanup

Tests that start servers can sometimes leave processes running if they crash. ExMCP provides several tools to clean up these stray processes:

### Automatic Cleanup

```bash
# Clean up before running tests (automatic with make test)
mix test.cleanup

# Manual cleanup with verbose output
mix test.cleanup --verbose

# Dry run to see what would be cleaned
mix test.cleanup --dry-run

# Alternative bash script
./scripts/cleanup_tests.sh

# Clean up as part of test run
make test  # Automatically runs cleanup first

# Skip automatic cleanup if needed
SKIP_TEST_CLEANUP=true mix test
```

### What Gets Cleaned

The cleanup tools will:
- Stop any Cowboy listeners from tests
- Kill registered test processes
- Free up commonly used test ports (8080-8085, 9000-9002)
- Clean up stray beam.smp processes from test runs

### Manual Process Investigation

If you encounter persistent process issues:

```bash
# Check for running beam processes
ps aux | grep beam

# Check for listening ports
lsof -i :8080-8085

# Kill specific processes if needed
pkill -f "beam.*test"
```

## Contributing

### Contribution Workflow

1. **Fork the repository** on GitHub
2. **Create a feature branch** from `master`:
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. **Make your changes** following the coding standards
4. **Run quality checks**:
   ```bash
   make quality  # Format, credo, compile checks
   make test     # Full test suite
   ```
5. **Commit your changes** with conventional commit messages:
   ```bash
   git commit -m "feat: add new transport option"
   git commit -m "fix: resolve connection timeout issue"
   git commit -m "docs: update configuration examples"
   ```
6. **Push to your fork** and create a pull request

### Code Standards

#### Formatting and Style
- **Always run `mix format`** before committing
- Follow Elixir community conventions
- Use descriptive variable and function names
- Add appropriate documentation to public functions

#### Documentation
- Add `@doc` to all public functions
- Include examples in documentation when helpful
- Update guides when adding new features
- Ensure examples work with current codebase

#### Testing
- Write tests for all new functionality
- Maintain or improve test coverage
- Add appropriate test tags
- Test both success and error cases

#### Git Commit Messages
Follow [Conventional Commits](https://conventionalcommits.org/):

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

Types:
- `feat`: New features
- `fix`: Bug fixes
- `docs`: Documentation changes
- `test`: Test additions or modifications
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `chore`: Maintenance tasks

### Pull Request Guidelines

#### Before Submitting
- [ ] All tests pass locally
- [ ] Code is formatted with `mix format`
- [ ] No credo warnings
- [ ] Documentation is updated if needed
- [ ] CHANGELOG.md is updated for user-facing changes

#### PR Description
Include:
- Clear description of the change
- Motivation and context
- Breaking changes (if any)
- Testing approach
- Related issues

### Code Review Process

1. **Automated checks** must pass (CI, formatting, tests)
2. **Manual review** by maintainers
3. **Feedback incorporation** and iteration
4. **Final approval** and merge

## Release Process

### Version Management

ExMCP follows [Semantic Versioning](https://semver.org/):

- **Patch** (`0.6.1`): Bug fixes, documentation updates
- **Minor** (`0.7.0`): New features, non-breaking changes
- **Major** (`1.0.0`): Breaking changes

### Release Checklist

#### Pre-Release
- [ ] All tests pass on CI
- [ ] Documentation is up to date
- [ ] CHANGELOG.md is updated
- [ ] Version is bumped in `mix.exs`
- [ ] Security audit passes

#### Release
- [ ] Tag release: `git tag v0.6.1`
- [ ] Push tag: `git push origin v0.6.1`
- [ ] GitHub release with changelog
- [ ] Publish to Hex: `mix hex.publish`

#### Post-Release
- [ ] Announce release
- [ ] Update documentation sites
- [ ] Close related issues/milestones

### Hotfix Process

For critical bugs in production releases:

1. Create hotfix branch from release tag
2. Apply minimal fix
3. Update version (patch bump)
4. Release immediately
5. Merge back to master

## Getting Help

### Development Questions
- **GitHub Discussions**: For general development questions
- **GitHub Issues**: For bug reports and feature requests
- **Code Review**: In pull requests

### Documentation
- **This guide**: Development setup and processes
- **[User Guide](guides/USER_GUIDE.md)**: Feature usage and examples  
- **[Architecture Guide](ARCHITECTURE.md)**: Internal design decisions
- **[API Docs](https://hexdocs.pm/ex_mcp)**: Complete API reference

### Community
- **Elixir Forum**: For general Elixir questions
- **ExMCP Community**: Growing community of contributors and users

---

Thank you for contributing to ExMCP! Your contributions help make MCP implementation in Elixir more robust and accessible to the community.