#!/usr/bin/env bash
set -euo pipefail
# Apply packaging/docs patches for 1.0.0-rc.7 when MCP payload size
# prevented committing the full files remotely.
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"
for p in docs/patches/rc7/*.patch; do
  echo "Applying $p"
  patch -p1 < "$p"
done
echo "Done. Review git status before committing."
