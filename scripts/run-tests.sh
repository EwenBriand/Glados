#!/bin/sh

# Define ANSI color codes
GREEN='\033[0;32m'  # Green text color
RED='\033[0;31m'    # Red text color
NC='\033[0m'        # No color (reset)

# Run 'stack clean' to remove any previous build artifacts
stack clean

# Run 'stack test' to execute the tests
stack test

# Check the exit code of the 'stack test' command
if [ $? -eq 0 ]; then
  # If the exit code is 0, the tests were successful
  echo -e "${GREEN}Tests passed successfully!${NC}"
else
  # If the exit code is not 0, the tests failed
  echo -e "${RED}Tests failed.${NC}"
  exit 1
fi
