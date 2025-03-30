#!/bin/bash
set -e

# Check if jq is installed
if ! [ -x "$(command -v jq)" ]; then
    echo "jq is not installed" >& 2
    exit 1
fi

# Clean previous packages
if [ -d "pkg" ]; then
    rm -rf pkg
fi

# Build for browser only
export RUSTFLAGS='--cfg getrandom_backend="wasm_js"'
wasm-pack build -t browser -d pkg crates/js