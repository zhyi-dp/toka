#!/bin/bash

# Auto-Compile Compiler if needed
make -C build -j8
if [ $? -ne 0 ]; then
    echo "Compiler Build Failed"
    exit 1
fi

# Configuration
VERIFIER="tool/test_verify_fail.py"

if [ ! -f "$VERIFIER" ]; then
    echo "Error: Verifier script not found at $VERIFIER"
    exit 1
fi

# Delegate to the Python script
python3 "$VERIFIER"
exit $?
