#!/bin/bash

# Configuration
TOKAC="./build/src/tokac"
LLI=$(which lli || which lli-17 || echo "/usr/local/Cellar/llvm@17/17.0.6/bin/lli")

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

pass_count=0
fail_count=0

if [ ! -f "$TOKAC" ]; then
    echo "Error: Compiler not found at $TOKAC"
    echo "Please run this script from the project root and ensure the project is built."
    exit 1
fi

echo "Starting Toka 'PASS' Test Suite..."
echo "---------------------------------"

# Targeted test collection: ONLY tests/pass/
files=(tests/pass/*.tk)

if [ ${#files[@]} -eq 0 ]; then
    echo "No .tk files found in tests/pass/"
    exit 0
fi

for test_file in "${files[@]}"; do
    # Check existence
    [ -e "$test_file" ] || continue

    test_name=$(basename "$test_file")
    printf "Testing %-35s ... " "$test_name"

    # Temporary files
    ll_file="${test_name}.ll"
    log_file="${test_name}.log"

    # 1. Compile
    if $TOKAC "$test_file" > "$ll_file" 2> "$log_file"; then
        
        # 2. Run with lli
        if $LLI "$ll_file" >> "$log_file" 2>&1; then
             echo -e "${GREEN}PASS${NC}"
             ((pass_count++))
             rm -f "$ll_file" "$log_file"
        else
             echo -e "${RED}FAIL (Runtime)${NC}"
             ((fail_count++))
             echo -e "${RED}    Runtime error details in ${log_file}${NC}"
        fi
    else
        echo -e "${RED}FAIL (Compile)${NC}"
        ((fail_count++))
        echo -e "${RED}    Compilation error details in ${log_file}${NC}"
    fi
done

echo "---------------------------------"
echo "Summary:"
echo -e "  Passed: ${GREEN}$pass_count${NC}"
echo -e "  Failed: ${RED}$fail_count${NC}"

if [ $fail_count -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
