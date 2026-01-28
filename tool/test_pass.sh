#!/bin/bash

# --- Configuration ---
TOKAC="./build/src/tokac"
LLI=$(which lli || which lli-17 || echo "/usr/local/Cellar/llvm@17/17.0.6/bin/lli")

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

pass_count=0
fail_count=0

# --- Diagnostics ---

log_fail() {
    local test_name="$1"
    local error_type="$2"
    local click_link="$3"
    local extra_msg="$4"
    local log_file="$5"
    
    printf "Testing %-35s ... " "$test_name"
    echo -e "${RED}FAIL ($error_type)${NC}"
    echo -e "    ${RED}$click_link: error: $extra_msg${NC}"
    
    # If it's a runtime/unexpected error, show the tail of the log
    if [ -f "$log_file" ] && [[ "$error_type" == *"Panic"* || "$error_type" == *"Runtime"* ]]; then
        echo -e "    --- Last 5 lines of $log_file ---"
        tail -n 5 "$log_file" | sed 's/^/    | /'
    fi
}

log_pass() {
    ((pass_count++))
}

# --- Logic ---

# 1. Build
make -C build -j8 || { echo "Compiler Build Failed"; exit 1; }

echo "Starting Toka 'PASS' Test Suite..."
echo "---------------------------------"

for test_path in tests/pass/*.tk; do
    [ -e "$test_path" ] || continue
    
    file_name=$(basename "$test_path")
    ll_file="${file_name}.ll"
    log_file="${file_name}.log"

    # Step 1: Compile
    if ! "$TOKAC" "$test_path" > "$ll_file" 2> "$log_file"; then
        log_fail "$file_name" "Compile" "$test_path:1" "Check compilation log" "$log_file"
        ((fail_count++))
        continue
    fi

    # Step 2: Run
    # Use a subshell to avoid "Abort trap" spam in some shells
    ( "$LLI" "$ll_file" >> "$log_file" 2>&1 )
    exit_code=$?

    # Step 3: Extract Info
    # Extract actual panic message and line from log
    # We look specifically for the block after "Panic:" to avoid catching compiler notes
    panic_info=$(grep -A 5 "Panic:" "$log_file" | tail -n 5)
    actual_line=$(echo "$panic_info" | grep -oE "[^[:space:]]*${file_name}:[0-9]+" | head -n 1 | cut -d':' -f2)
    actual_msg=$(echo "$panic_info" | grep "Panic:" | sed 's/.*Panic: //')
    
    # expected_markers: Find all active (not commented out) markers
    # We want to match:
    #   - // EXPECT_PANIC: 38
    #   - auto x = ... // EXPECT_PANIC
    # But ignore:
    #   - // auto x = ... // EXPECT_PANIC
    active_markers=$(grep -n "// EXPECT_PANIC" "$test_path" | grep -vE '^[0-9]+:[[:space:]]*//[^[:space:]E]')
    primary_marker_line=$(echo "$active_markers" | head -n 1 | cut -d: -f1)

    # Step 4: Verification
    if [ $exit_code -eq 0 ]; then
        # Should NOT have had a marker
        if [ -n "$active_markers" ]; then
            log_fail "$file_name" "Panic Missing" "$test_path:$primary_marker_line" "Expected panic did not occur" "$log_file"
            ((fail_count++))
        else
            log_pass
            rm -f "$ll_file" "$log_file"
        fi
    else
        # Fault occurred
        if [ -n "$actual_line" ]; then
            # Rigorous Check: The ACTUAL panic must match the FIRST expected marker in the file.
            # If we skip an earlier marker and hit a later one, it's a mismatch.
            if [ "$actual_line" == "$primary_marker_line" ]; then
                log_pass
                rm -f "$ll_file" "$log_file"
            else
                log_fail "$file_name" "Panic Mismatch" "$test_path:$actual_line" "Expected panic at line $primary_marker_line, but crashed here instead (Panic message: $actual_msg)" "$log_file"
                ((fail_count++))
            fi
        else
            # No line info found in log
            fail_ptr="${test_path}:${primary_marker_line:-1}"
            log_fail "$file_name" "Runtime Error ($exit_code)" "$fail_ptr" "Process crashed without panic info" "$log_file"
            ((fail_count++))
        fi
    fi
done

# --- Report ---
echo "---------------------------------"
echo "Summary:"
echo -e "  Passed: ${GREEN}$pass_count${NC}"
echo -e "  Failed: ${RED}$fail_count${NC}"

[ $fail_count -eq 0 ] || exit 1
