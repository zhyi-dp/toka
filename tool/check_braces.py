import sys

def check_braces(file_path):
    with open(file_path, 'r') as f:
        lines = f.readlines()

    depth = 0
    in_string = False
    stack = []
    
    for i, line in enumerate(lines):
        orig_line = line
        j = 0
        while j < len(orig_line):
            char = orig_line[j]
            
            if char == '"' and (j == 0 or orig_line[j-1] != '\\'):
                in_string = not in_string
            
            if not in_string:
                if orig_line[j:j+2] == '//':
                    break
                
                if char == '{':
                    depth += 1
                    stack.append((i+1, orig_line.strip()))
                elif char == '}':
                    if depth > 0:
                        start_line, start_text = stack.pop()
                        if depth <= 2: 
                            print(f"Block: Lines {start_line}-{i+1} (Depth {depth}) -> {start_text}")
                    depth -= 1
                    if depth < 0:
                        print(f"ERROR: Extra closing brace at line {i+1}: {orig_line.strip()}")
            j += 1
            
    print(f"Final Depth: {depth}")
    if depth != 0:
        print(f"FAILURE: Final brace depth is {depth}. Unclosed blocks:")
        for start_line, start_text in stack:
            print(f"  Line {start_line}: {start_text}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 check_braces.py <file_path>")
    else:
        check_braces(sys.argv[1])
