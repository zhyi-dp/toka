#!/bin/bash

# tool/test_single.sh - Run a single Toka test case

if [ -z "$1" ]; then
  echo "Usage: $0 <tk_file>"
  echo "Example: $0 tests/pass/test_small.tk"
  exit 1
fi

TK_FILE="$1"

if [ ! -f "$TK_FILE" ]; then
    echo "Error: File '$TK_FILE' not found."
    exit 1
fi

BASE_NAME="${TK_FILE%.*}"
LL_FILE="${BASE_NAME}.ll"
LOG_FILE="${BASE_NAME}.log"
# Configuration
TOKAC="./build/src/tokac"
LLI=$(which lli || which lli-17 || echo "/usr/local/Cellar/llvm@17/17.0.6/bin/lli")

echo "[TEST] Running $TK_FILE"
echo "  - Compiling..."

# Compile
"$TOKAC" "$TK_FILE" > "$LL_FILE" 2> "$LOG_FILE"
COMPILE_STATUS=$?

if [ $COMPILE_STATUS -ne 0 ]; then
    echo "  - Compilation FAILED (Exit Code: $COMPILE_STATUS)"
    echo "  - Error Log:"
    cat "$LOG_FILE"
    exit 1
fi

echo "  - Running ($LLI)..."
# Run with lli
"$LLI" "$LL_FILE"
RUN_STATUS=$?

echo "  - Finished with Exit Code: $RUN_STATUS"

# Cleanup
# rm -f "$LL_FILE" "$LOG_FILE"

exit $RUN_STATUS
