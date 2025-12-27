#pragma once

#include "toka/Token.h"
#include <string_view>
#include <vector>

namespace toka {

class Lexer {
public:
  Lexer(const char *source);

  // Returns a vector of all tokens (simple approach for now)
  std::vector<Token> tokenize();

private:
  const char *m_Source;
  const char *m_Current;
  int m_Line = 1;
  int m_Column = 1;

  void skipWhitespace();
  Token nextToken();
  Token identifier();
  Token number();
  Token string();
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
