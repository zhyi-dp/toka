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
#pragma once

#include "toka/SourceLocation.h"
#include "toka/Token.h"
#include <string_view>
#include <vector>

namespace toka {

class Lexer {
public:
  Lexer(const char *source, SourceLocation startLoc = SourceLocation());

  // Returns a vector of all tokens (simple approach for now)
  std::vector<Token> tokenize();

private:
  const char *m_Source;
  const char *m_Current;
  SourceLocation m_StartLoc;
  int m_Line = 1;
  int m_Column = 1;
  bool m_HasNewline = false;

  void skipWhitespace();
  Token nextToken();
  Token identifier();
  Token number();
  Token string();
  Token charLiteral();
  Token punctuation();

  char peek() const { return *m_Current; }
  char peekNext() const {
    if (*m_Current == '\0')
      return '\0';
    return *(m_Current + 1);
  }
  char advance() {
    char c = *m_Current;
    if (c == '\n') {
      m_Line++;
      m_Column = 1;
    } else {
      m_Column++;
    }
    m_Current++;
    return c;
  }
  bool match(char expected) {
    if (*m_Current == expected) {
      advance();
      return true;
    }
    return false;
  }

  bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
  }
  bool isDigit(char c) { return c >= '0' && c <= '9'; }
  bool isAttributes(char c) {
    return c == '#' || c == '?' || c == '!' || c == '$';
  }
};

} // namespace toka
