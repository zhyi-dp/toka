#!/bin/bash

# --- Configuration ---
TOKAC="./build/src/tokac"
LLI=$(which lli || which lli-17 || echo "/usr/local/Cellar/llvm@17/17.0.6/bin/lli")

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'
GRAY='\033[0;90m'

pass_count=0
fail_count=0

# --- Helper Functions ---

# Print log context (Panic line or last few lines), called only once per file
print_log_context() {
    local log_file="$1"
    local exit_code="$2"
    
    if [ -f "$log_file" ]; then
        # Try to capture the critical Panic line
        local panic_line=$(grep "runtime error: Panic with" "$log_file" | head -n 1)
        
        echo -e "${GRAY}    --------------------------------------------------${NC}"
        if [ -n "$panic_line" ]; then
             echo -e "    | Exit Code: $exit_code"
             echo -e "    | $panic_line"
        else
             # If not a standard Panic (e.g. Segfault), show the last 3 lines
             echo -e "    | Exit Code: $exit_code (No panic info found)"
             echo -e "    | Last logs:"
             tail -n 3 "$log_file" | sed 's/^/    | /'
        fi
        echo -e "${GRAY}    --------------------------------------------------${NC}"
    fi
}

# --- Main Logic ---

# Ensure compiler builds first
make -C build -j8 > /dev/null || { echo "Compiler Build Failed"; exit 1; }

echo "Starting Toka 'PASS' Test Suite..."
echo "---------------------------------"

for test_path in tests/pass/*.tk; do
    [ -e "$test_path" ] || continue
    
    file_name=$(basename "$test_path")
    ll_file="${file_name}.ll"
    log_file="${file_name}.log"
    
    # Array to collect all errors for the current file
    errors=()

    # Step 1: Compile
    if ! "$TOKAC" "$test_path" > "$ll_file" 2> "$log_file"; then
        printf "[FAIL] %-35s\n" "$file_name"
        echo -e "    ${RED}$test_path:1: error: Compilation failed${NC}"
        # Print tail of log to help debug syntax errors
        tail -n 5 "$log_file" | sed 's/^/    | /'
        ((fail_count++))
        continue
    fi

    # Step 2: Run
    # Trick: Use { ... } 2>&1 to capture output, and pipe to grep -v to silence 
    # the shell's "Abort trap" message (common on macOS).
    # We use PIPESTATUS to get the exit code of lli, not grep.
    { "$LLI" "$ll_file" >> "$log_file" 2>&1; } 2>&1 | grep -v "Abort trap"
    exit_code=${PIPESTATUS[0]}

    # Step 3: Extract Info
    # Extract actual Panic line number and message
    panic_log_line=$(grep "runtime error: Panic with" "$log_file" | head -n 1)
    if [ -n "$panic_log_line" ]; then
        actual_line=$(echo "$panic_log_line" | grep -oE ":[0-9]+ runtime" | grep -oE "[0-9]+")
        actual_msg=$(echo "$panic_log_line" | sed -n 's/.*Panic with "\(.*\)" \*\*\*/\1/p')
    else
        actual_line=""
        actual_msg=""
    fi
    
    # Get all expected line numbers
    all_expected_lines=$(grep -n "// EXPECT_PANIC" "$test_path" | cut -d: -f1 | tr '\n' ' ')

    # Step 4: Verification
    
    # --- Case A: Program finished successfully (Exit 0) ---
    if [ $exit_code -eq 0 ]; then
        for exp_line in $all_expected_lines; do
            errors+=("${RED}$test_path:$exp_line: error: Expected panic did not trigger (program finished successfully).${NC}")
        done

    # --- Case B: Program crashed (Exit != 0) ---
    else
        if [ -n "$actual_line" ]; then
            
            # 1. Check for MISSED expected panics (lines before the crash)
            for exp_line in $all_expected_lines; do
                if [ "$exp_line" -lt "$actual_line" ]; then
                    errors+=("${RED}$test_path:$exp_line: error: Expected panic did not trigger (execution passed this line).${NC}")
                fi
            done

            # 2. Check the CRASH location
            # Read the source code of the crashed line
            crashed_source_code=$(sed "${actual_line}q;d" "$test_path")
            
            # Sub-check: Is this an ASSERTION failure?
            # Asserts are logical bugs and should FAIL even if marked with EXPECT_PANIC.
            if [[ "$crashed_source_code" == *"assert"* ]]; then
                 errors+=("${RED}$test_path:$actual_line: error: Assertion failed: \"$actual_msg\" ${NC}")
            
            # Sub-check: Is this an UNEXPECTED runtime panic?
            else
                # Check if the line has the marker
                is_actual_expected=$(echo "$crashed_source_code" | grep "// EXPECT_PANIC")
                
                if [ -z "$is_actual_expected" ]; then
                    errors+=("${RED}$test_path:$actual_line: error: Unexpected panic: \"$actual_msg\"${NC}")
                fi
            fi

        else
            # Runtime crash without standard panic info (e.g. Segfault)
            errors+=("${RED}$test_path:1: error: Runtime crash ($exit_code) without standard panic info.${NC}")
        fi
    fi

    # Step 5: Reporting
    if [ ${#errors[@]} -eq 0 ]; then
        ((pass_count++))
        
        # If it passed BUT crashed (Expected Panic), show context for verification
        if [ $exit_code -ne 0 ]; then
             printf "[PASS with Expected Panic] %-35s\n" "$file_name"
             print_log_context "$log_file" "$exit_code"
        fi
        
        rm -f "$ll_file" "$log_file"
    else
        ((fail_count++))
        printf "[FAIL] %-35s\n" "$file_name"
        for err_msg in "${errors[@]}"; do
            echo -e "    $err_msg"
        done
        print_log_context "$log_file" "$exit_code"
    fi
done

# --- Report ---
echo "---------------------------------"
echo "Summary:"
echo -e "  Passed: ${GREEN}$pass_count${NC}"
echo -e "  Failed: ${RED}$fail_count${NC}"

[ $fail_count -eq 0 ] || exit 1