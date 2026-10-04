#!/usr/bin/env python3
"""One-time MCP migration in an isolated checkout; never edits the 1.x source.

Usage: python3 scripts/v2/migrate_mcp.py /absolute/path/to/v2-checkout
ACP must first be extracted from the reconciled 1.x source. This tool does not
change wire extension keys, protocol method names, or persisted ACP paths.
"""
import re
import shutil
import sys
from pathlib import Path


def migrate(root: Path) -> None:
    if root == Path(__file__).resolve().parents[2]:
        raise SystemExit("Use an isolated checkout; the supported 1.x source must remain intact")
    if not (root / "lib/ex_mcp.ex").exists():
        raise SystemExit("Expected an untouched ExMCP checkout with lib/ex_mcp.ex")

    # ACP implementation, developer tooling and tests move to the ACP repository.
    removed = [
        "lib/ex_mcp/acp", "lib/ex_mcp/acp.ex", "examples/acp",
        "test/ex_mcp/acp", "test/support/acp", "test/fixtures/acp",
        "test/interop/acp_compatibility.json", ".github/workflows/acp-ecosystem.yml",
        "dev/ex_mcp/acp_compat.ex", "dev/ex_mcp/acp_shim_generator.ex",
        "docs/ACP_GUIDE.md", "docs/ACP_V2_TRACKING.md",
    ]
    removed += [str(p.relative_to(root)) for p in (root / "dev/mix/tasks").glob("acp.*.ex")]
    removed += [str(p.relative_to(root)) for p in (root / "test/ex_mcp/integration").glob("acp*_test.exs")]
    for relative in removed:
        path = root / relative
        shutil.rmtree(path) if path.is_dir() else path.unlink(missing_ok=True)

    facade = root / "lib/ex_mcp.ex"
    text = facade.read_text()
    text = text.replace("  - `ExMCP.ACP` - Agent Client Protocol client and native agent helpers\n", "")
    text = re.sub(r'  @doc """\n  Starts an ACP client.*?  defdelegate start_acp_client\(opts\), to: ExMCP.ACP, as: :start_client\n\n', "", text, flags=re.S)
    facade.write_text(text)

    privacy = root / "test/ex_mcp/wire_privacy_test.exs"
    text = privacy.read_text().replace("  alias ExMCP.ACP.{Agent, Client}\n", "")
    text = re.sub(r'        agent_state = %Agent.*?(?=        assert \{:noreply, _state\} = SSEHandler)', "", text, flags=re.S)
    text = text.replace('    assert log =~ "unknown client request"\n', "")
    privacy.write_text(text)

    codes = root / "test/ex_mcp/protocol/error_code_characterization_test.exs"
    text = codes.read_text().replace("  alias ExMCP.ACP.Types, as: ACPTypes\n", "")
    text = re.sub(r'\n    assert %\{\n             auth_required: ACPTypes.*?request_cancelled: -32800\n           \}\n', "\n", text, flags=re.S)
    codes.write_text(text)

    for base in ["lib", "dev", "test"]:
        old = root / base / "ex_mcp"
        if old.exists():
            old.rename(root / base / "arbor_mcp")
    facade.rename(root / "lib/arbor_mcp.ex")

    shared = {"JSONRPC", "StdioFraming", "PortEnvironment", "LogSummary", "LineBuffer"}
    for name in ["jsonrpc", "stdio_framing", "port_environment", "log_summary", "line_buffer"]:
        (root / "lib/arbor_mcp/internal" / f"{name}.ex").unlink()

    def rpc_alias(match):
        names = [item.strip() for item in match.group(1).split(",")]
        own = [name for name in names if name not in shared]
        rpc = [name for name in names if name in shared and name != "LineBuffer"]
        lines = []
        if own:
            lines.append("alias Arbor.MCP.Internal.{" + ", ".join(own) + "}")
        if rpc:
            lines.append("alias Arbor.RPC.{" + ", ".join(rpc) + "}")
        if "LineBuffer" in names:
            lines.append("alias Arbor.RPC.Internal.LineBuffer")
        return "\n  ".join(lines)

    # Rewrite code and current examples/guides; historical API inventories and
    # release records retain their original identities and source references.
    for path in root.rglob("*"):
        relative = path.relative_to(root)
        if not path.is_file() or any(part in {".git", "deps", "_build", "tmp", "node_modules"} for part in relative.parts):
            continue
        if path.suffix not in {".ex", ".exs", ".md", ".sh", ".yml", ".yaml"}:
            continue
        if path.name == "CHANGELOG.md" or str(relative).startswith("docs/V2_"):
            continue
        text = path.read_text()
        text = re.sub(r"\bExMCP\b", "Arbor.MCP", text)
        text = re.sub(r":ex_mcp\b", ":arbor_mcp", text)
        text = text.replace("lib/ex_mcp", "lib/arbor_mcp").replace("test/ex_mcp", "test/arbor_mcp").replace("dev/ex_mcp", "dev/arbor_mcp")
        text = re.sub(r"alias Arbor\.MCP\.Internal\.\{([^}]+)\}", rpc_alias, text)
        for name in shared:
            replacement = f"Arbor.RPC.Internal.{name}" if name == "LineBuffer" else f"Arbor.RPC.{name}"
            text = text.replace(f"Arbor.MCP.Internal.{name}", replacement)
        text = text.replace("https://github.com/azmaveth/ex_mcp", "https://github.com/trust-arbor/arbor_mcp")
        path.write_text(text)

    print("MCP namespace and pure-helper ownership migrated; package metadata and release gates remain.")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit(__doc__)
    migrate(Path(sys.argv[1]).resolve())
