# Aider Configuration Rules

## Working Directory

When using aider, remember that its current working directory is `/Users/azmaveth/code`. 

**ALWAYS prefix file paths with `ex_mcp/`** so aider writes code in the correct directory.

## File Path Examples

**Correct:**
- `ex_mcp/lib/ex_mcp/consent_handler.ex`
- `ex_mcp/test/ex_mcp/compliance/security_compliance_test.exs`
- `ex_mcp/config/config.exs`

**Incorrect:**
- `lib/ex_mcp/consent_handler.ex` (missing ex_mcp/ prefix)
- `test/ex_mcp/compliance/security_compliance_test.exs` (missing ex_mcp/ prefix)
- `config/config.exs` (missing ex_mcp/ prefix)

## Implementation Notes

- All file paths in aider commands must include the `ex_mcp/` prefix
- This ensures files are created in the correct ExMCP project directory
- Verify file locations after creation to confirm proper placement
