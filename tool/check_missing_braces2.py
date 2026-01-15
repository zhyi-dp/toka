import sys
import os
from pygments import lex
from pygments.lexers import CppLexer
from pygments.token import Token

def check_braces_with_pygments(filename):
    if not os.path.exists(filename):
        print(f"{filename}:0:0: error: File not found")
        return

    with open(filename, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()

    # stack 存储: (行号, 缩进, 这一行的内容)
    stack = []
    
    # 获取 C++ 词法分析器
    lexer = CppLexer()
    
    # 追踪当前的行号
    # pygments 返回的 token 包含位置信息，但我们自己算行号更直观
    line_num = 1
    
    # 将代码切分为 Token 流
    # lexer.get_tokens(content) 返回 (Token类型, Token内容)
    tokens = lex(content, lexer)

    # 简单的缩进获取辅助函数
    lines = content.splitlines()
    def get_indent(ln):
        if ln <= len(lines):
            line_str = lines[ln-1]
            return len(line_str) - len(line_str.lstrip())
        return 0

    for token_type, value in tokens:
        # Pygments 会把换行符单独作为 Token 此时行号+1
        num_newlines = value.count('\n')
        if num_newlines > 0:
            line_num += num_newlines
            continue
            
        # 我们只关心 Punctuation (标点) 中的 { 和 }
        # Pygments 自动忽略了 Comment 和 String 中的括号，这正是我们要的！
        if token_type in Token.Punctuation:
            if '{' in value:
                indent = get_indent(line_num)
                stack.append((line_num, indent))
            elif '}' in value:
                if not stack:
                    print(f"{filename}:{line_num}:1: error: unexpected '}}'")
                    return
                stack.pop()

    # 检查结果
    if stack:
        last_line, last_indent = stack[-1]
        print(f"{filename}:{last_line}:1: error: unclosed '{{' (found by Pygments)")
        
        # 打印建议位置
        found_hint = False
        for i in range(last_line, len(lines)):
            curr_indent = get_indent(i + 1)
            line_content = lines[i].strip()
            
            # 缩进回退逻辑
            if line_content and curr_indent <= last_indent and not line_content.startswith('}'):
                 print(f"{filename}:{i+1}:1: note: indentation mismatch suggests missing '}}' before here")
                 found_hint = True
                 break
        
        if not found_hint:
            print(f"{filename}:{len(lines)}:1: note: reached end of file")

if __name__ == "__main__":
    check_braces_with_pygments(sys.argv[1])