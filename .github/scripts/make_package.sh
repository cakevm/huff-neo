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

if [ -d "pkg-node" ]; then
    rm -rf pkg-node
fi

# Build for both targets
export RUSTFLAGS='--cfg getrandom_backend="wasm_js"'
wasm-pack build -t nodejs -d pkg-node crates/js
wasm-pack build -t browser -d pkg crates/js

# Get the package name
PKG_NAME=$(jq -r .name crates/js/pkg/package.json | sed 's/\-/_/g')

# Give the packages a version tag
if [ -n "$VERSION_TAG" ]; then
    # Overwrite the version in the package.json
    jq --arg v $VERSION_TAG '.version = $v' crates/js/pkg/package.json > temp.json && mv temp.json crates/js/pkg/package.json
    jq --arg v $VERSION_TAG '.version = $v' crates/js/pkg-node/package.json > temp.json && mv temp.json crates/js/pkg-node/package.json
fi