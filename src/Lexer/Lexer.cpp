// Copyright (c) 2025 YiZhonghua<zhyi@dpai.com>. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
#include "toka/Lexer.h"
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

namespace toka {

static std::unordered_map<std::string, TokenType> Keywords = {
    {"auto", TokenType::KwAuto},
    {"mut", TokenType::KwMut},
    {"type", TokenType::KwType},
    {"alias", TokenType::KwAlias},
    {"const", TokenType::KwConst},
    {"shape", TokenType::KwShape},
    {"packed", TokenType::KwPacked},
    {"trait", TokenType::KwTrait},
    {"impl", TokenType::KwImpl},
    {"fn", TokenType::KwFn},
    {"new", TokenType::KwNew},
    {"dyn", TokenType::KwDyn},
    {"move", TokenType::KwMove},
    {"where", TokenType::KwWhere},
    {"default", TokenType::KwDefault},
    {"delete", TokenType::KwDelete},
    {"del", TokenType::KwDelete},
    {"final", TokenType::KwFinal},
    {"Fn", TokenType::KwFnType},
    {"if", TokenType::KwIf},
    {"else", TokenType::KwElse},
    {"match", TokenType::KwMatch},
    {"case", TokenType::KwCase},
    {"for", TokenType::KwFor},
    {"while", TokenType::KwWhile},
    {"loop", TokenType::KwLoop},
    {"pass", TokenType::KwPass},
    {"to", TokenType::KwTo},
    {"or", TokenType::KwOr},
    {"band", TokenType::KwBand},
    {"bor", TokenType::KwBor},
    {"bxor", TokenType::KwBxor},
    {"bnot", TokenType::KwBnot},
    {"bshl", TokenType::KwBshl},
    {"bshr", TokenType::KwBshr},
    {"break", TokenType::KwBreak},
    {"continue", TokenType::KwContinue},
    {"return", TokenType::KwReturn},
    {"yield", TokenType::KwYield},
    {"Task", TokenType::KwTask},
    {"suspend", TokenType::KwSuspend},
    {"async", TokenType::KwAsync},
    {"cancel", TokenType::KwCancel},
    {"await", TokenType::KwAwait},
    {"Channel", TokenType::KwChannel},
    {"import", TokenType::KwImport},
    {"pub", TokenType::KwPub},
    {"as", TokenType::KwAs},
    {"is", TokenType::KwIs},
    {"in", TokenType::KwIn},
    {"self", TokenType::KwSelf},
    {"Self", TokenType::KwUpperSelf},
    {"true", TokenType::KwTrue},
    {"false", TokenType::KwFalse},
    {"none", TokenType::KwNone},
    {"nullptr", TokenType::KwNull},
    {"defer", TokenType::KwDefer},
    {"main", TokenType::KwMain},
    {"extern", TokenType::KwExtern},
    {"crate", TokenType::KwCrate},
    {"unsafe", TokenType::KwUnsafe},
    {"alloc", TokenType::KwAlloc},
    {"free", TokenType::KwFree},
    {"unset", TokenType::KwUnset},
    {"variant", TokenType::KwVariant},
    {"union", TokenType::KwUnion}};

Lexer::Lexer(const char *source, SourceLocation startLoc)
    : m_Source(source), m_Current(source), m_StartLoc(startLoc) {}

std::vector<Token> Lexer::tokenize() {
  std::vector<Token> tokens;
  while (true) {
    Token t = nextToken();
    tokens.push_back(t);
    if (t.Kind == TokenType::EndOfFile)
      break;
  }
  return tokens;
}

void Lexer::skipWhitespace() {
  m_HasNewline = false;
  while (true) {
    char c = peek();
    if (c == ' ' || c == '\r' || c == '\t' || c == '\n') {
      if (c == '\n')
        m_HasNewline = true;
      advance();
    } else if (c == '/' && peekNext() == '/') {
      // Comment
      while (peek() != '\n' && peek() != '\0')
        advance();
    } else if (c == '/' && peekNext() == '*') {
      // Multi-line Comment
      advance(); // consume '/'
      advance(); // consume '*'
      while (peek() != '\0') {
        if (peek() == '*' && peekNext() == '/') {
          advance(); // consume '*'
          advance(); // consume '/'
          break;
        }
        if (peek() == '\n') {
          m_HasNewline = true;
        }
        advance();
      }
    } else {
      break;
    }
  }
}

Token Lexer::nextToken() {
  skipWhitespace();
  const char *startPtr = m_Current;

  Token t;
  bool hadNewline = m_HasNewline;

  if (peek() == '\0') {
    t = Token{TokenType::EndOfFile, "", m_Line, m_Column};
  } else if (isDigit(peek())) {
    t = number();
  } else if (isAlpha(peek())) {
    t = identifier();
  } else {
    t = punctuation();
  }

  // Calculate SourceLocation relative to start
  ptrdiff_t offset = startPtr - m_Source;
  if (m_StartLoc.isValid()) {
    t.Loc = SourceLocation(m_StartLoc.getRawEncoding() + (uint32_t)offset);
  } else {
    // If StartLoc is invalid (e.g. testing strings), we use 0-based offset?
    // Or just invalid Loc?
    // Let's use 0-based offset so strict checks don't fail if they rely on ID
    // uniqueness, but ID=0 is Invalid.
    // If m_StartLoc is 0 (Invalid), adding offset makes it non-zero (Valid)?
    // No, 0 + offset = offset. Only offset 0 is invalid.
    // But usually SourceManager assigns base ID > 0.
    // For now:
    t.Loc = SourceLocation(m_StartLoc.getRawEncoding() + (uint32_t)offset);
  }

  t.HasNewlineBefore = hadNewline;
  return t;
}

Token Lexer::identifier() {
  const char *start = m_Current;
  int startCol = m_Column;
  int startLine = m_Line;

  while (isAlpha(peek()) || isDigit(peek())) {
    advance();
  }
  std::string text(start, m_Current);

  // Check keyword
  TokenType kind = TokenType::Identifier;
  if (Keywords.find(text) != Keywords.end()) {
    kind = Keywords[text];
  }

  Token t{kind, text, startLine, startCol};

  // Check for Attributes suffix (ONLY for identifiers, NOT keywords generally,
  // although Val might be special but usually Val doesn't have suffix.
  // Identifier does). Spec: "val x# = ..."
  if (kind == TokenType::Identifier || kind == TokenType::KwSelf ||
      kind == TokenType::KwUpperSelf) {
    if (match('#'))
      t.HasWrite = true;
    else if (match('?'))
      t.HasNull = true;
    else if (match('!')) {
      t.HasWrite = true;
      t.HasNull = true;
    } else if (match('$')) { /* Default */
    }

    // Update text to include suffix for debugging?
    // Or keep text as raw identifier and flags separate.
    // Let's append suffix to text for now for clarity in debug
    // We keep Text as pure identifier for symbol lookup.
    // if (t.HasWrite) t.Text += "#";
  }

  return t;
}

#include <cctype>

Token Lexer::number() {
  const char *start = m_Current;
  int line = m_Line;
  int col = m_Column;

  // Hex: 0x...
  if (peek() == '0' && (peekNext() == 'x' || peekNext() == 'X')) {
    advance(); // 0
    advance(); // x
    while (isxdigit(peek()))
      advance();
    return Token{TokenType::Integer, std::string(start, m_Current), line, col};
  }

  while (isDigit(peek()))
    advance();

  // Float Part
  bool isFloat = false;
  if (peek() == '.' && isDigit(peekNext())) {
    isFloat = true;
    advance();
    while (isDigit(peek()))
      advance();
  }

  // Scientific Notation (e.g. 1e10, 1.5e-5)
  if (peek() == 'e' || peek() == 'E') {
    isFloat = true;
    advance(); // e
    if (peek() == '+' || peek() == '-')
      advance();
    while (isDigit(peek()))
      advance();
  }

  TokenType type = isFloat ? TokenType::Float : TokenType::Integer;
  return Token{type, std::string(start, m_Current), line, col};
}

Token Lexer::punctuation() {
  char c = advance();
  int line = m_Line;
  int col = m_Column - 1; // Since we advanced

  // Check for caret special handling
  if (c == '^') {
    Token t{TokenType::Caret, "^", line, col};
    // Check attributes for Pointer
    if (match('#')) {
      t.IsSwappablePtr = true;
      t.Text += "#";
    } else if (match('?')) {
      t.HasNull = true;
      t.Text += "?";
    } // Pointer itself nullable
    else if (match('!')) {
      t.IsSwappablePtr = true;
      t.HasNull = true;
      t.Text += "!";
    }
    return t;
  }

  switch (c) {
  case '(':
    return Token{TokenType::LParen, "(", line, col};
  case ')':
    return Token{TokenType::RParen, ")", line, col};
  case '[':
    return Token{TokenType::LBracket, "[", line, col};
  case ']':
    return Token{TokenType::RBracket, "]", line, col};
  case '{':
    return Token{TokenType::LBrace, "{", line, col};
  case '}':
    return Token{TokenType::RBrace, "}", line, col};
  case ',':
    return Token{TokenType::Comma, ",", line, col};
  case ';':
    return Token{TokenType::Semicolon, ";", line, col};
  case ':':
    return Token{TokenType::Colon, ":", line, col};
  case '&':
    if (peek() == '&') {
      advance();
      return Token{TokenType::And, "&&", line, col};
    }
    {
      Token t{TokenType::Ampersand, "&", line, col};
      if (match('#')) {
        t.IsSwappablePtr = true;
        t.Text += "#";
      } else if (match('?')) {
        t.HasNull = true;
        t.Text += "?";
      } else if (match('!')) {
        t.IsSwappablePtr = true;
        t.HasNull = true;
        t.Text += "!";
      }
      return t;
    }
  case '~': {
    Token t{TokenType::Tilde, "~", line, col};
    if (match('#')) {
      t.IsSwappablePtr = true;
      t.Text += "#";
    } else if (match('?')) {
      t.HasNull = true;
      t.Text += "?";
    } else if (match('!')) {
      t.IsSwappablePtr = true;
      t.HasNull = true;
      t.Text += "!";
    }
    return t;
  }
  case '@':
    return Token{TokenType::At, "@", line, col};
  case '|':
    if (peek() == '|') {
      advance();
      return Token{TokenType::Or, "||", line, col};
    }
    return Token{TokenType::Pipe, "|", line, col};
  case '+':
    if (peek() == '=') {
      advance();
      return Token{TokenType::PlusEqual, "+=", line, col};
    }
    if (peek() == '+') {
      advance();
      return Token{TokenType::PlusPlus, "++", line, col};
    }
    return Token{TokenType::Plus, "+", line, col};
  case '-':
    if (peek() == '=') {
      advance();
      return Token{TokenType::MinusEqual, "-=", line, col};
    }
    if (peek() == '-') {
      advance();
      return Token{TokenType::MinusMinus, "--", line, col};
    }
    if (peek() == '>') {
      advance();
      return Token{TokenType::Arrow, "->", line, col};
    }
    return Token{TokenType::Minus, "-", line, col};
  case '*':
    if (peek() == '=') {
      advance();
      return Token{TokenType::StarEqual, "*=", line, col};
    }
    {
      Token t{TokenType::Star, "*", line, col};
      if (match('#')) {
        t.IsSwappablePtr = true;
        t.Text += "#";
      } else if (match('?')) {
        t.HasNull = true;
        t.Text += "?";
      } else if (match('!')) {
        t.IsSwappablePtr = true;
        t.HasNull = true;
        t.Text += "!";
      }
      return t;
    }
  case '/':
    if (peek() == '=') {
      advance();
      return Token{TokenType::SlashEqual, "/=", line, col};
    }
    return Token{TokenType::Slash, "/", line, col};
  case '=':
    if (peek() == '=') {
      advance();
      return Token{TokenType::DoubleEqual, "==", line, col};
    }
    if (peek() == '>') {
      advance();
      return Token{TokenType::FatArrow, "=>", line, col};
    }
    return Token{TokenType::Equal, "=", line, col};
  case '!':
    if (peek() == '=') {
      advance();
      return Token{TokenType::Neq, "!=", line, col};
    }
    return Token{TokenType::Bang, "!", line, col};
  case '<': {
    if (peek() == '-') {
      advance();
      return Token{TokenType::Dependency, "<-", line, col};
    }
    if (peek() == '=') {
      advance();
      return Token{TokenType::LessEqual, "<=", line, col};
    }
    // Toka Generic Rule: < followed by space is Less, otherwise GenericLT
    char nextC = peek();
    if (nextC == ' ' || nextC == '\t' || nextC == '\n' || nextC == '\r') {
      return Token{TokenType::Less, "<", line, col};
    }
    return Token{TokenType::GenericLT, "<", line, col};
  }
  case '>':
    if (peek() == '=') {
      advance();
      return Token{TokenType::GreaterEqual, ">=", line, col};
    }
    return Token{TokenType::Greater, ">", line, col};
  case '.':
    if (peek() == '.' && peekNext() == '.') {
      advance();
      advance();
      return Token{TokenType::DotDotDot, "...", line, col};
    }
    return Token{TokenType::Dot, ".", line, col};
  // How to handle standalone # ?
  case '#':
    return Token{TokenType::TokenWrite, "#", line, col};
  case '?':
    return Token{TokenType::TokenNull, "?", line, col};
  case '$':
    return Token{TokenType::TokenNone, "$", line, col};

  case '"':
    return string(); // Call string handler
  default:
    return Token{TokenType::EndOfFile, "UNEXPECTED", line, col};
  }
}

Token Lexer::string() {
  // Already consumed opening quote
  std::string text = "";
  while (peek() != '"' && peek() != '\0') {
    char c = advance();
    if (c == '\\') {
      char next = advance();
      switch (next) {
      case 'n':
        text += '\n';
        break;
      case 't':
        text += '\t';
        break;
      case '\\':
        text += '\\';
        break;
      case '"':
        text += '"';
        break;
      default:
        text += next;
        break;
      }
    } else {
      text += c;
    }
  }

  if (peek() == '"')
    advance(); // Consume closing

  return Token{TokenType::String, text, m_Line, m_Column};
}

} // namespace toka
