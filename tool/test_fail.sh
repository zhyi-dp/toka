#!/bin/bash

# Auto-Compile Compiler if needed
make -C build -j8
if [ $? -ne 0 ]; then
    echo "Compiler Build Failed"
    exit 1
fi

# Configuration
TOKAC="./build/src/tokac"
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

pass_count=0
fail_count=0

if [ ! -f "$TOKAC" ]; then
    echo "Error: Compiler not found at $TOKAC"
    exit 1
fi

echo "Starting Toka 'REJECT' Test Suite..."
echo "---------------------------------"

# Targeted test collection: ONLY tests/fail/
files=(tests/fail/*.tk)

if [ ${#files[@]} -eq 0 ]; then
    echo "No .tk files found in tests/fail/"
    exit 0
fi

for test_file in "${files[@]}"; do
    [ -e "$test_file" ] || continue
    test_name=$(basename "$test_file")
    #printf "Testing %-35s ... " "$test_name"

    log_file="${test_name}.log"

    # We EXPECT compilation to FAIL
    if $TOKAC "$test_file" > /dev/null 2> "$log_file"; then
        printf "Testing %-35s ... " "$test_name"
        echo -e "${RED}FAIL (Unexpectedly Passed)${NC} details in ./${log_file}${NC}"
        ((fail_count++))
    else
        # It failed, which is what we want.
        # Ideally we'd check the error message too, but for now just exit code != 0 is success for rejection tests
        #printf "Testing %-35s ... " "$test_name"
        #echo -e "${GREEN}PASS (Rejected)${NC}"
        ((pass_count++))
        rm -f "$log_file"
    fi
done

echo "---------------------------------"
echo "Summary:"
echo -e "  Passed (Correctly Rejected): ${GREEN}$pass_count${NC}"
echo -e "  Failed (Unexpectedly Passed): ${RED}$fail_count${NC}"

if [ $fail_count -eq 0 ]; then
    exit 0
else
    exit 1
fi
