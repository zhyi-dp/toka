#!/usr/bin/env python3
import sys
import os
import subprocess
import re

def run_test(file_path):
    print(f"Testing panic: {os.path.basename(file_path)}...")
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Look for // EXPECT_PANIC: line_number
    match = re.search(r'// EXPECT_PANIC: (\d+)', content)
    if not match:
        print(f"  SKIP: No EXPECT_PANIC found in {file_path}")
        return True

    expected_line = match.group(1)
    file_name = os.path.basename(file_path)

    # 1. Compile
    ll_file = "panic_test_temp.ll"
    try:
        # Assuming tokac supports -o or redirection. 
        # The common pattern in test_single.sh is redirection or it creates .tk.ll
        # To be safe, we'll run tokac and then locate the generated file 
        # or better, use the known behavior from test_single.sh
        subprocess.check_output(f"./build/src/tokac {file_path} > {ll_file}", shell=True, stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
        print(f"  FAIL: Compilation failed\n{e.output.decode()}")
        return False

    # 2. Run with lli
    try:
        cmd = ["/usr/local/opt/llvm@17/bin/lli", ll_file]
        # We expect a non-zero exit code (abort)
        proc = subprocess.run(cmd, capture_output=True, text=True)
        
        output = proc.stdout + proc.stderr
        
        # 3. Verify output
        # Expectation: Panic: ... at file.tk:line
        # Use re.DOTALL because there might be newlines between Panic and at
        panic_pattern = rf"Panic:.*?at .*?{file_name}:{expected_line}"
        if re.search(panic_pattern, output, re.MULTILINE | re.DOTALL):
            print(f"  PASS: Caught expected panic at line {expected_line}")
            return True
        else:
            print(f"  FAIL: Panic captured but location mismatch or missing.")
            print(f"  Expected line: {expected_line}")
            print(f"  Command: {' '.join(cmd)}")
            print(f"  Actual Output:\n{output}")
            return False

    except Exception as e:
        print(f"  FAIL: Execution error: {str(e)}")
        return False

def main():
    test_files = sys.argv[1:]
    if not test_files:
        # Default to checking member_parsing.tk if it exists
        test_files = ["tests/pass/member_parsing.tk"]

    all_pass = True
    for f in test_files:
        if not run_test(f):
            all_pass = False
    
    sys.exit(0 if all_pass else 1)

if __name__ == "__main__":
    main()
