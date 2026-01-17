#!/usr/bin/env python3
import sys
import os
import subprocess
import glob

# Configuration
TOKAC = "./build/src/tokac"
FAIL_TEST_DIR = "tests/fail"

GREEN = '\033[0;32m'
RED = '\033[0;31m'
YELLOW = '\033[0;33m'
NC = '\033[0m' # No Color

def main():
    if not os.path.exists(TOKAC):
        print(f"{RED}Error: Compiler not found at {TOKAC}{NC}")
        print(f"Please build it first: make -C build -j8")
        sys.exit(1)

    # Determine which files to test
    if len(sys.argv) > 1:
        files = sys.argv[1:]
    else:
        files = glob.glob(os.path.join(FAIL_TEST_DIR, "*.tk"))
        if not files:
            print(f"{YELLOW}No .tk files found in {FAIL_TEST_DIR}{NC}")
            sys.exit(0)

    files.sort()
    
    total_run = 0
    total_passed = 0
    total_failed = 0

    print("Starting Toka 'VERIFY FAIL' Test Suite...")
    print("---------------------------------------")

    for test_file in files:
        if not os.path.exists(test_file):
            print(f"{YELLOW}Skipping missing file: {test_file}{NC}")
            continue
            
        total_run += 1
        test_name = os.path.basename(test_file)
        
        # 1. Parse Expectations
        expected_errors = []
        with open(test_file, 'r') as f:
            for line in f:
                if '// EXPECT:' in line:
                    parts = line.split('// EXPECT:', 1)
                    if len(parts) > 1:
                        msg = parts[1].strip()
                        if msg:
                            expected_errors.append(msg)
        
        # 2. Run Compiler
        # We assume fail tests are standalone (no complex imports for now)
        try:
            result = subprocess.run(
                [TOKAC, test_file],
                capture_output=True,
                text=True
            )
        except Exception as e:
            print(f"Testing {test_name:<35} {RED}ERROR (Execution Failed){NC}: {e}")
            total_failed += 1
            continue

        compiler_output = result.stderr + result.stdout
        exit_code = result.returncode

        # 3. Verification Logic
        
        # Case A: Unexpected Pass (Exit Code 0)
        if exit_code == 0:
            print(f"Testing {test_name:<35} {RED}FAIL (Unexpectedly Passed){NC}")
            total_failed += 1
            continue
            
        # Case B: No Expectations defined -> Default to "Fail is Good"
        if not expected_errors:
            # For backward compatibility, if no expectations are set, any failure is a pass
            # But we might want to warn about this
            print(f"Testing {test_name:<35} {GREEN}PASS (Rejected - No Checks){NC}")
            total_passed += 1
            continue

        # Case C: Check Expectations
        missing_expectations = []
        for expect in expected_errors:
            if expect not in compiler_output:
                missing_expectations.append(expect)
        
        if missing_expectations:
            print(f"Testing {test_name:<35} {RED}FAIL (Missing Expected Errors){NC}")
            print(f"  {YELLOW}Expected but not found:{NC}")
            for m in missing_expectations:
                print(f"    - '{m}'")
            print(f"  {YELLOW}Actual Output Snippet:{NC}")
            print("\n".join(compiler_output.splitlines()[:10])) # Show first 10 lines
            total_failed += 1
        else:
            print(f"Testing {test_name:<35} {GREEN}PASS (Verified){NC}")
            total_passed += 1

    print("---------------------------------------")
    print("Summary:")
    print(f"  Passed: {GREEN}{total_passed}{NC}")
    print(f"  Failed: {RED}{total_failed}{NC}")
    
    if total_failed > 0:
        sys.exit(1)
    else:
        sys.exit(0)

if __name__ == "__main__":
    main()
