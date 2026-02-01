import re
import sys

def check_file(filepath):
    print(f"Checking {filepath}...")
    try:
        with open(filepath, 'r') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"Error reading file: {e}")
        return

    # Regex patterns based on extension.js
    # Note: Javascript \s includes newlines, but here we process line by line.
    # We use re.match to Anchor to start of string (like ^)
    
    fn_pattern = re.compile(r'^\s*(?:pub\s+)?fn\s+([a-zA-Z_]\w*)')
    shape_pattern = re.compile(r'^\s*(?:pub\s+)?shape\s+([a-zA-Z_]\w*)')
    impl_pattern = re.compile(r'^\s*impl\s+(?:([a-zA-Z_]\w*)\s+for\s+)?([a-zA-Z_]\w*)')
    alias_pattern = re.compile(r'^\s*(?:pub\s+)?alias\s+([a-zA-Z_]\w*)')

    symbols = []

    for i, line in enumerate(lines):
        # Python's re.match checks from the beginning of the string
        
        m = fn_pattern.match(line)
        if m:
            print(f"Line {i+1}: Function '{m.group(1)}'")
            symbols.append(('Function', m.group(1), i+1))
            continue
            
        m = shape_pattern.match(line)
        if m:
            print(f"Line {i+1}: Struct '{m.group(1)}'")
            symbols.append(('Struct', m.group(1), i+1))
            continue
            
        m = impl_pattern.match(line)
        if m:
            name = f"{m.group(1)} for {m.group(2)}" if m.group(1) else m.group(2)
            print(f"Line {i+1}: Class '{name}'")
            symbols.append(('Class', name, i+1))
            continue
            
        m = alias_pattern.match(line)
        if m:
            print(f"Line {i+1}: Variable '{m.group(1)}'")
            symbols.append(('Variable', m.group(1), i+1))
            continue

    print(f"\nTotal symbols found: {len(symbols)}")
    if len(symbols) == 0:
        print("NO SYMBOLS FOUND.")

if __name__ == "__main__":
    target = sys.argv[1] if len(sys.argv) > 1 else "lib/std/fs.tk"
    check_file(target)
