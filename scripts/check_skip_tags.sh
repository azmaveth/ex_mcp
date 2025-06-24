#!/bin/bash
#
# Check for @tag :skip in test files
# This script prevents committing tests that are marked as skipped,
# which can hide compliance issues and create false test coverage.
#
# Exit codes:
# 0 - No @tag :skip found
# 1 - Found @tag :skip in test files
# 2 - Error during execution

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo -e "${RED}Error: Not in a git repository${NC}"
    exit 2
fi

# Function to check files for @tag :skip
check_files() {
    local files=("$@")
    local found_skip=false
    local skip_files=()
    
    for file in "${files[@]}"; do
        # Skip if file doesn't exist (might be deleted)
        [ -f "$file" ] || continue
        
        # Check if file contains @tag :skip
        if grep -q "@tag :skip" "$file" 2>/dev/null; then
            found_skip=true
            skip_files+=("$file")
        fi
    done
    
    if [ "$found_skip" = true ]; then
        echo -e "${RED}✗ Found @tag :skip in the following test files:${NC}"
        for file in "${skip_files[@]}"; do
            echo -e "  ${YELLOW}$file${NC}"
            # Show the lines with @tag :skip for context
            grep -n "@tag :skip" "$file" | while IFS= read -r line; do
                echo -e "    ${RED}$line${NC}"
            done
        done
        echo ""
        echo -e "${RED}Tests marked with @tag :skip will not run and can hide compliance issues.${NC}"
        echo -e "${YELLOW}Please implement the tests or remove them entirely instead of skipping.${NC}"
        return 1
    else
        return 0
    fi
}

# Main execution
main() {
    local mode="${1:-all}"
    local files_to_check=()
    
    case "$mode" in
        "staged")
            # Check only staged files (for pre-commit hook)
            files_to_check=($(git diff --cached --name-only --diff-filter=ACM | grep -E "test/.*\.exs$" || true))
            if [ ${#files_to_check[@]} -eq 0 ]; then
                echo -e "${GREEN}✓ No test files staged for commit${NC}"
                exit 0
            fi
            ;;
        "branch")
            # Check files changed in current branch (for CI)
            local base_branch="${2:-main}"
            files_to_check=($(git diff "$base_branch"...HEAD --name-only --diff-filter=ACM | grep -E "test/.*\.exs$" || true))
            if [ ${#files_to_check[@]} -eq 0 ]; then
                echo -e "${GREEN}✓ No test files changed in this branch${NC}"
                exit 0
            fi
            ;;
        "all"|*)
            # Check all test files
            files_to_check=($(find test -name "*.exs" -type f 2>/dev/null || true))
            if [ ${#files_to_check[@]} -eq 0 ]; then
                echo -e "${GREEN}✓ No test files found${NC}"
                exit 0
            fi
            ;;
    esac
    
    echo -e "${YELLOW}Checking ${#files_to_check[@]} test file(s) for @tag :skip...${NC}"
    
    if check_files "${files_to_check[@]}"; then
        echo -e "${GREEN}✓ No @tag :skip found in test files${NC}"
        exit 0
    else
        exit 1
    fi
}

# Handle script arguments
if [ $# -gt 0 ]; then
    main "$@"
else
    main "all"
fi