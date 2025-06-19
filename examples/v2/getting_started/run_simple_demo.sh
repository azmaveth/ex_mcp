#!/bin/bash

# ExMCP v2 Simple Demo Script
# Runs the working demo that showcases v2 features

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

# Change to project root so _build directory is available
cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}==========================================
ExMCP v2 Feature Demo
==========================================${NC}"

echo -e "\nThis demo showcases v2 features without complex server setup:"
echo -e "• Structured Response types"
echo -e "• Error handling and categories"  
echo -e "• ClientConfig builder pattern"
echo -e "• JSON-RPC format conversion"

echo -e "\n${YELLOW}🚀 Running v2 feature demo...${NC}"
echo -e "${BLUE}----------------------------------------${NC}"

if elixir examples/v2/getting_started/working_demo.exs; then
    echo -e "${BLUE}----------------------------------------${NC}"
    echo -e "${GREEN}✓ Demo completed successfully${NC}"
else
    echo -e "${BLUE}----------------------------------------${NC}"
    echo -e "${RED}✗ Demo failed${NC}"
    exit 1
fi

echo -e "\n${YELLOW}🧪 Running utility examples...${NC}"

echo -e "\n• Structured Responses:"
if elixir examples/v2/utilities/structured_responses.exs > /dev/null 2>&1; then
    echo -e "  ${GREEN}✓ Passed${NC}"
else
    echo -e "  ${RED}✗ Failed${NC}"
fi

echo -e "\n• Error Handling:"
if elixir examples/v2/utilities/error_handling.exs > /dev/null 2>&1; then
    echo -e "  ${GREEN}✓ Passed${NC}"
else
    echo -e "  ${RED}✗ Failed${NC}"
fi

echo -e "\n• Client Config:"
if elixir examples/v2/utilities/client_config.exs > /dev/null 2>&1; then
    echo -e "  ${GREEN}✓ Passed${NC}"
else
    echo -e "  ${RED}✗ Failed${NC}"
fi

echo -e "\n${GREEN}✅ All demos complete!${NC}"

echo -e "\n${YELLOW}📚 Next Steps:${NC}"
echo -e "• Explore examples/v2/advanced/ for complete server implementations"
echo -e "• Check examples/v2/utilities/ for detailed feature examples"  
echo -e "• See MIGRATING_V2.md for migration from v1 to v2"
echo -e "• Review the main v2 README for comprehensive documentation"