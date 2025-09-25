#!/usr/bin/env bash

# Exit on error
set -x
set -e

# Configuration
LOG_FILE="ghciwatch.log"

echo "Starting ghciwatch"
echo "Logging to: $LOG_FILE"
echo "----------------------------------------"

# Run ghciwatch with:
# - hpack before each startup/restart to regenerate .cabal files
# - cabal repl with the specific test-dev component
# - output redirected to log file
ghcid \
  --command "hpack;cabal v2-repl --enable-multi-repl lib:chatcompletion-effectful test:chatcompletion-effectful-test" \
  --restart "cabal.project" \
  --restart "chatcompletion-effectful.cabal" \
  -o $LOG_FILE
