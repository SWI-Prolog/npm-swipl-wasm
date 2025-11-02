#!/bin/bash
# Read build-config.json and export as environment variables
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG=$(cat "$SCRIPT_DIR/../build-config.json")
export SWIPL_VERSION=$(echo "$CONFIG" | jq -r '.swipl.version')
export SWIPL_COMMIT=$(echo "$CONFIG" | jq -r '.swipl.commit')
export SWIPL_NAME=$(echo "$CONFIG" | jq -r '.swipl.name')
export EMSDK_VERSION=$(echo "$CONFIG" | jq -r '.emsdk.version')
export EMSDK_COMMIT=$(echo "$CONFIG" | jq -r '.emsdk.commit')
export EMSDK_NAME=$(echo "$CONFIG" | jq -r '.emsdk.name')
export ZLIB_VERSION=$(echo "$CONFIG" | jq -r '.zlib.version')
export PCRE2_VERSION=$(echo "$CONFIG" | jq -r '.pcre2.version')
export PCRE2_COMMIT=$(echo "$CONFIG" | jq -r '.pcre2.commit')
export PCRE2_NAME=$(echo "$CONFIG" | jq -r '.pcre2.name')
