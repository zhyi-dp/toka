#!/bin/bash

# tool/tokac.sh
# Usage: ./tool/tokac.sh source.tk [output_binary]
# This script compiles a Toka source file to a binary executable using tokac and clang.

# Environment Setup
SCRIPT_DIR="$(dirname "$0")"
# Try to resolve project root based on script location (assuming tool/ is one level deep)
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TOKAC="$PROJECT_ROOT/build/src/tokac"
CLANG="clang"

# Default to "tokac" in PATH if build version not found
if [ ! -f "$TOKAC" ]; then
    if command -v tokac &> /dev/null; then
        TOKAC="tokac"
    else
        echo "Error: tokac executable not found at $TOKAC and not in PATH."
        echo "Please build the project first or check your path."
        exit 1
    fi
fi

# Argument Parsing
if [ -z "$1" ]; then
    echo "Usage: $0 <source.tk> [output_binary_name]"
    exit 1
fi

SOURCE_FILE="$1"
if [ ! -f "$SOURCE_FILE" ]; then
    echo "Error: Source file '$SOURCE_FILE' not found."
    exit 1
fi

# Determine Output Name
BASENAME=$(basename "$SOURCE_FILE" .tk)
DIRNAME=$(dirname "$SOURCE_FILE")

if [ -z "$2" ]; then
    # Default: Output to current directory with .bin extension
    OUTPUT_BIN="./${BASENAME}.bin"
else
    OUTPUT_BIN="$2"
fi

LL_FILE="${DIRNAME}/${BASENAME}.ll"

# Step 1: Compile to LLVM IR
echo "[tokac.sh] Compiling $SOURCE_FILE to IR..."
"$TOKAC" "$SOURCE_FILE" > "$LL_FILE" 2> /dev/null
COMPILE_STATUS=$?

if [ $COMPILE_STATUS -ne 0 ]; then
    echo "Error: Toka compilation failed."
    # Re-run to show error
    "$TOKAC" "$SOURCE_FILE" > /dev/null
    rm -f "$LL_FILE"
    exit 1
fi

# Step 2: Link to Binary
echo "[tokac.sh] Linking to binary $OUTPUT_BIN..."
# -Wno-override-module suppresses warnings about ModuleID mismatch if any
"$CLANG" "$LL_FILE" -o "$OUTPUT_BIN" -Wno-override-module -lm

LINK_STATUS=$?
if [ $LINK_STATUS -eq 0 ]; then
    echo -e "\033[0;32mSuccess! Executable created: $OUTPUT_BIN\033[0m"
    # Cleanup intermediate IR file
    rm -f "$LL_FILE"
    exit 0
else
    echo "Error: Linking failed."
    rm -f "$LL_FILE"
    exit 1
fi
