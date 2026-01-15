import sys
import os

class BraceChecker:
    def __init__(self, filename):
        # 获取绝对路径，确保 IDE 能正确跳转
        self.filename = os.path.abspath(filename)
        self.raw_lines = []
        self.clean_lines = [] 
        self.stack = [] # (line_idx, col_idx, indent_level)

    def load_and_clean(self):
        if not os.path.exists(self.filename):
            print(f"{self.filename}:0:0: error: File not found")
            sys.exit(1)

        with open(self.filename, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
            self.raw_lines = content.splitlines()

        # === 内存清洗 (保留换行，抹除注释/字符串) ===
        code_list = list(content)
        length = len(code_list)
        i = 0
        
        # 状态常量
        NORMAL = 0
        STRING = 1
        CHAR = 2
        LINE_COMMENT = 3
        BLOCK_COMMENT = 4
        
        state = NORMAL
        
        while i < length:
            char = code_list[i]
            
            if state == NORMAL:
                if char == '"': state = STRING; code_list[i] = ' '
                elif char == "'": state = CHAR; code_list[i] = ' '
                elif char == '/' and i+1 < length:
                    if code_list[i+1] == '/': state = LINE_COMMENT; code_list[i] = ' '; code_list[i+1] = ' '; i+=1
                    elif code_list[i+1] == '*': state = BLOCK_COMMENT; code_list[i] = ' '; code_list[i+1] = ' '; i+=1
            
            elif state == STRING:
                if char == '\\': code_list[i] = ' '; code_list[i+1] = ' '; i+=1
                elif char == '"': state = NORMAL; code_list[i] = ' '
                else: code_list[i] = ' '
                
            elif state == CHAR:
                if char == '\\': code_list[i] = ' '; code_list[i+1] = ' '; i+=1
                elif char == "'": state = NORMAL; code_list[i] = ' '
                else: code_list[i] = ' '
            
            elif state == LINE_COMMENT:
                if char == '\n': state = NORMAL
                else: code_list[i] = ' '
                
            elif state == BLOCK_COMMENT:
                if char == '*' and i+1 < length and code_list[i+1] == '/':
                    state = NORMAL; code_list[i] = ' '; code_list[i+1] = ' '; i+=1
                elif char != '\n':
                    code_list[i] = ' '
            
            i += 1
            
        self.clean_lines = "".join(code_list).splitlines()

    def get_indent(self, line):
        expanded = line.replace('\t', '    ')
        return len(expanded) - len(expanded.lstrip())

    def run(self):
        # 扫描所有括号
        for line_idx, line in enumerate(self.clean_lines):
            for col_idx, char in enumerate(line):
                if char == '{':
                    indent = self.get_indent(self.raw_lines[line_idx])
                    # 列号从1开始，所以 col_idx + 1
                    self.stack.append((line_idx, col_idx + 1, indent))
                
                elif char == '}':
                    if not self.stack:
                        print(f"{self.filename}:{line_idx + 1}:{col_idx + 1}: error: unexpected '}}'")
                        return
                    self.stack.pop()

        if not self.stack:
            return # 完美，无输出

        # === 发现错误，生成专业报告 ===
        
        # 1. 取出最后一个未闭合的 '{'
        last_idx, last_col, last_indent = self.stack[-1]
        
        # 输出主要错误 (Clang/GCC 格式)
        # 格式: file:line:col: error: message
        print(f"{self.filename}:{last_idx + 1}:{last_col}: error: unclosed '{{' found here")
        print(f"    {self.raw_lines[last_idx].strip()}")
        # 打印指示箭头 (计算除去前导空格后的偏移)
        raw_line = self.raw_lines[last_idx]
        stripped_len = len(raw_line.strip())
        leading_spaces = len(raw_line) - stripped_len
        # 箭头位置 = 原始列号 - 前导空格数 + 4个缩进显示位 - 1
        # 简单处理：直接指向最后一个字符附近
        print(f"    ^")

        # 2. 扫描建议位置 (Note)
        found_hint = False
        for i in range(last_idx + 1, len(self.clean_lines)):
            line_content = self.clean_lines[i].strip()
            if not line_content: continue 
            
            curr_indent = self.get_indent(self.raw_lines[i])
            
            # 如果缩进回退到了同级或更浅，且该行不是以 } 开头
            if curr_indent <= last_indent:
                if line_content.startswith('}'):
                    continue
                
                print(f"{self.filename}:{i + 1}:1: note: indentation suggests the missing '}}' might belong before this line")
                print(f"    {self.raw_lines[i].strip()}")
                print(f"    ^")
                found_hint = True
                break
        
        if not found_hint:
             # 如果直到文件末尾都没找到缩进回退，说明可能是在最后漏了
             total_lines = len(self.raw_lines)
             print(f"{self.filename}:{total_lines}:1: note: reached end of file without closing the block")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 check_braces.py <filename>")
    else:
        checker = BraceChecker(sys.argv[1])
        checker.load_and_clean()
        checker.run()