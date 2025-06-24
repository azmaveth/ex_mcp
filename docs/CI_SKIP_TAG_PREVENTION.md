# CI Skip Tag Prevention

This document describes the CI pipeline enhancement that prevents committing tests with `@tag :skip`, which can hide compliance issues and create false test coverage confidence.

## Overview

The ExMCP project now includes automated checks to prevent tests marked with `@tag :skip` from being committed. This is enforced at three levels:

1. **Git Pre-commit Hook** - Checks staged files before commit
2. **CI Pipeline** - Validates all test files on every push
3. **Mix Task** - Provides manual checking capability

## Implementation

### Script: `scripts/check_skip_tags.sh`

A bash script that scans test files for `@tag :skip` annotations. Features:
- Supports different modes: all files, staged files, or branch differences
- Provides detailed output showing which files and lines contain skip tags
- Returns appropriate exit codes for CI/hook integration
- Works on both macOS and Linux

### Mix Task: `mix check_skip_tags`

Elixir wrapper around the bash script for better integration:
```bash
# Check all test files
mix check_skip_tags

# Check only staged files
mix check_skip_tags --staged

# Check files changed vs main branch
mix check_skip_tags --branch main

# Run quietly (exit code only)
mix check_skip_tags --quiet
```

### Git Hook Configuration

In `config/config.exs`, the pre-commit hook includes:
```elixir
{:cmd, "./scripts/check_skip_tags.sh staged"}
```

This runs automatically before each commit when git hooks are installed.

### CI Workflow

The GitHub Actions workflow (`/.github/workflows/ci.yml`) includes:
```yaml
- name: Check for skipped tests
  run: ./scripts/check_skip_tags.sh all
```

This ensures no skipped tests can be merged into main branches.

## Rationale

Based on the SPEC_ALIGNMENT_PLAN.md analysis, 38 compliance tests were marked with `@tag :skip`, creating a false sense of security about MCP specification compliance. These skipped tests:

- Hide actual compliance gaps
- Create misleading test coverage metrics
- Allow specification violations to go unnoticed
- Accumulate technical debt

By preventing `@tag :skip` in the CI pipeline, we ensure that:
1. All tests either run or are removed entirely
2. Compliance issues are visible and must be addressed
3. Test coverage metrics accurately reflect actual testing

## Usage Guidelines

### For Developers

When you encounter a failing test:
1. **Fix the implementation** - This is the preferred approach
2. **Remove the test** - If the test is no longer relevant
3. **Create a GitHub issue** - If the fix requires significant work

**Never use `@tag :skip`** - It hides problems rather than solving them.

### For CI/CD

The check will fail the build if any test files contain `@tag :skip`. The error message will show:
- Which files contain skip tags
- The exact line numbers
- A reminder about why skipping tests is problematic

### Exceptions

If there's a legitimate need to temporarily skip a test (e.g., external dependency issues):
1. Create a GitHub issue documenting why
2. Use `@tag :pending` with a comment linking to the issue
3. Set up monitoring to ensure pending tests don't accumulate

## Migration Strategy

To comply with this new policy:

1. **Audit existing skip tags**:
   ```bash
   mix check_skip_tags
   ```

2. **For each skipped test**:
   - Determine why it was skipped
   - Either fix the underlying issue or remove the test
   - Document any removed tests in CHANGELOG.md

3. **Update documentation**:
   - Add notes to contributor guidelines
   - Update test writing documentation

## Benefits

1. **Improved Compliance** - All MCP specification requirements are actively tested
2. **Accurate Metrics** - Test coverage reflects actual testing
3. **Early Detection** - Issues caught before merging to main
4. **Technical Debt Prevention** - Forces immediate resolution of test failures
5. **Better Visibility** - No hidden failures in the test suite

## Troubleshooting

### Script Not Found
```
Error: Skip tag check script not found
```
Ensure `scripts/check_skip_tags.sh` exists and is executable:
```bash
chmod +x scripts/check_skip_tags.sh
```

### Permission Denied
The script needs execute permissions. Run:
```bash
chmod +x scripts/check_skip_tags.sh
```

### Git Hooks Not Running
Install git hooks:
```bash
mix git_hooks.install
```

## Future Enhancements

1. **Granular Control** - Allow specific skip reasons with automated tracking
2. **Metrics Dashboard** - Track skip tag usage over time
3. **Auto-fix Tool** - Convert skip tags to GitHub issues automatically
4. **Integration Tests** - Extend to integration test suites