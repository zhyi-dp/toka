#!/bin/bash

# Auto-Compile Compiler if needed
make -C build -j8
if [ $? -ne 0 ]; then
    echo "Compiler Build Failed"
    exit 1
fi

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
    #printf "Testing %-35s ... " "$test_name"

    # Temporary files
    ll_file="${test_name}.ll"
    log_file="${test_name}.log"

    # 1. Compile
    if $TOKAC "$test_file" > "$ll_file" 2> "$log_file"; then
        
        # Check for EXPECT_PANIC logic
        panic_line=$(grep "// EXPECT_PANIC:" "$test_file" | head -n 1 | sed 's/.*EXPECT_PANIC: \([0-9]*\).*/\1/')
        
        if [ -n "$panic_line" ]; then
            # 2a. Run with lli and expect FAILURE, then verify log
            $LLI "$ll_file" >> "$log_file" 2>&1
            # We don't check exit code here, but we must find the panic message location
            if grep -q "at .*$(basename "$test_file"):$panic_line" "$log_file"; then
                 ((pass_count++))
                 rm -f "$ll_file" "$log_file"
            else
                 printf "Testing %-35s ... " "$test_name"
                 echo -e "${RED}FAIL (Panic Mismatch)${NC}"
                 ((fail_count++))
                 echo -e "${RED}    Expected panic at line $panic_line not found in ./${log_file}${NC}"
            fi
        else
            # 2b. Run with lli (Original logic)
            if $LLI "$ll_file" >> "$log_file" 2>&1; then
                 ((pass_count++))
                 rm -f "$ll_file" "$log_file"
            else
                 printf "Testing %-35s ... " "$test_name"
                 echo -e "${RED}FAIL (Runtime)${NC}"
                 ((fail_count++))
                 echo -e "${RED}    Runtime error details in ./${log_file}${NC}"
            fi
        fi
    else
        echo -e "${RED}FAIL (Compile)${NC}"
        ((fail_count++))
        echo -e "${RED}    Compilation error details in ./${log_file}${NC}"
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
