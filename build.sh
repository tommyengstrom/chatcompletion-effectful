#!/usr/bin/env bash
set -e

echo "=== Running hpack to generate .cabal file from package.yaml ==="
hpack

echo ""
echo "=== Building project with cabal ==="
cabal build all

echo ""
echo "=== Build completed successfully ==="