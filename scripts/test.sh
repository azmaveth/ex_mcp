#!/usr/bin/env bash
# Wrapper around mix test that always saves output to a log file.
# Usage: ./scripts/test.sh [mix test args...]
# Output saved to: tmp/test_output.txt (overwritten each run)

set -euo pipefail

mkdir -p tmp

echo "Running: mix test $@"
mix test "$@" 2>&1 | tee tmp/test_output.txt
exit_code=${PIPESTATUS[0]}

# Summary
echo ""
echo "--- Test output saved to tmp/test_output.txt ---"

if [ $exit_code -ne 0 ]; then
  echo "--- Failures: ---"
  grep -A 20 "^  [0-9]\+)" tmp/test_output.txt 2>/dev/null || true
fi

exit $exit_code
