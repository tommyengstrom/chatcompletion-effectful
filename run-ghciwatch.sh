#!/usr/bin/env bash

# Exit on error
set -x
set -e

# Configuration
LOG_FILE="ghciwatch.log"
COMPONENT="chatcompletion-effectful:test:test-dev"

echo "Starting ghciwatch for component: $COMPONENT"
echo "Logging to: $LOG_FILE"
echo "----------------------------------------"

# Run ghciwatch with:
# - hpack before each startup/restart to regenerate .cabal files
# - cabal repl with the specific test-dev component
# - output redirected to log file
ghciwatch \
  --command "cabal repl $COMPONENT " \
  --before-startup-shell "hpack" \
  --watch src \
  --watch test \
  --watch package.yaml \
  --restart-glob "package.yaml" \
  --restart-glob "cabal.project" \
  --error-file $LOG_FILE
