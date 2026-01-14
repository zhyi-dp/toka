
import sys
import re

def check_structure(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()

    stack = []
    
    # Simple regex to ignore comments and strings/chars
    # This is a naive parser.
    
    for i, line in enumerate(lines):
        line_num = i + 1
        
        # Remove simplistic comments
        line = re.sub(r'//.*', '', line)
        
        # Remove strings (naive)
        line = re.sub(r'"(?:[^"\\]|\\.)*"', '""', line)
        line = re.sub(r"'(?:[^'\\]|\\.)*'", "''", line)

        for char in line:
            if char == '{':
                stack.append(line_num)
            elif char == '}':
                if not stack:
                    print(f"Error: Unexpected '}}' at line {line_num}")
                    return
                stack.pop()

    if stack:
        print(f"Error: Unclosed '{{' at line {stack[-1]}")
        # Print the whole stack if needed, but last one is usually the culprit
        if len(stack) > 1:
             print(f"Total open braces: {len(stack)}")
             print(f"First open brace at line {stack[0]}")

if __name__ == "__main__":
    check_structure(sys.argv[1])
