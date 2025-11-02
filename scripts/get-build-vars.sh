#!/bin/bash
# Read build-config.json and export as environment variables
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/../build-config.json"

if [ ! -f "$CONFIG_FILE" ]; then
  echo "Error: build-config.json not found at $CONFIG_FILE" >&2
  exit 1
fi

CONFIG=$(cat "$CONFIG_FILE")
if [ -z "$CONFIG" ]; then
  echo "Error: Failed to read build-config.json" >&2
  exit 1
fi
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

# Validate required variables are not null or empty
if [ "$SWIPL_VERSION" = "null" ] || [ -z "$SWIPL_VERSION" ]; then
  echo "Error: Missing or invalid swipl.version in build-config.json" >&2
  exit 1
fi
if [ "$EMSDK_VERSION" = "null" ] || [ -z "$EMSDK_VERSION" ]; then
  echo "Error: Missing or invalid emsdk.version in build-config.json" >&2
  exit 1
fi
if [ "$ZLIB_VERSION" = "null" ] || [ -z "$ZLIB_VERSION" ]; then
  echo "Error: Missing or invalid zlib.version in build-config.json" >&2
  exit 1
fi
if [ "$PCRE2_NAME" = "null" ] || [ -z "$PCRE2_NAME" ]; then
  echo "Error: Missing or invalid pcre2.name in build-config.json" >&2
  exit 1
fi
