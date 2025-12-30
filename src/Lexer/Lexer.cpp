#include "toka/Lexer.h"
#include <unordered_map>

namespace toka {

static std::unordered_map<std::string, TokenType> Keywords = {
    {"let", TokenType::KwLet},        {"type", TokenType::KwType},
    {"const", TokenType::KwConst},    {"struct", TokenType::KwStruct},
    {"trait", TokenType::KwTrait},    {"impl", TokenType::KwImpl},
    {"fn", TokenType::KwFn},          {"new", TokenType::KwNew},
    {"dyn", TokenType::KwDyn},        {"move", TokenType::KwMove},
    {"where", TokenType::KwWhere},    {"default", TokenType::KwDefault},
    {"delete", TokenType::KwDelete},  {"del", TokenType::KwDelete},
    {"final", TokenType::KwFinal},    {"Fn", TokenType::KwFnType},
    {"if", TokenType::KwIf},          {"else", TokenType::KwElse},
    {"match", TokenType::KwMatch},    {"case", TokenType::KwCase},
    {"for", TokenType::KwFor},        {"while", TokenType::KwWhile},
    {"break", TokenType::KwBreak},    {"continue", TokenType::KwContinue},
    {"return", TokenType::KwReturn},  {"yield", TokenType::KwYield},
    {"Task", TokenType::KwTask},      {"suspend", TokenType::KwSuspend},
    {"async", TokenType::KwAsync},    {"cancel", TokenType::KwCancel},
    {"await", TokenType::KwAwait},    {"Channel", TokenType::KwChannel},
    {"import", TokenType::KwImport},  {"pub", TokenType::KwPub},
    {"as", TokenType::KwAs},          {"is", TokenType::KwIs},
    {"in", TokenType::KwIn},          {"self", TokenType::KwSelf},
    {"Self", TokenType::KwUpperSelf}, {"true", TokenType::KwTrue},
    {"false", TokenType::KwFalse},    {"none", TokenType::KwNone},
    {"null", TokenType::KwNull},      {"defer", TokenType::KwDefer},
    {"main", TokenType::KwMain},      {"extern", TokenType::KwExtern},
    {"option", TokenType::KwOption}};

Lexer::Lexer(const char *source) : m_Source(source), m_Current(source) {}

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
  while (true) {
    char c = peek();
    if (c == ' ' || c == '\r' || c == '\t' || c == '\n') {
      advance();
    } else if (c == '/' && peekNext() == '/') {
      // Comment
      while (peek() != '\n' && peek() != '\0')
        advance();
    } else {
      break;
    }
  }
}

Token Lexer::nextToken() {
  skipWhitespace();

  if (peek() == '\0') {
    return Token{TokenType::EndOfFile, "", m_Line, m_Column};
  }

  // Numbers
  if (isDigit(peek()))
    return number();

  // Identifiers & Keywords
  if (isAlpha(peek()))
    return identifier();

  // Punctuation & Operators & Caret
  return punctuation();
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
  if (kind == TokenType::Identifier) {
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

Token Lexer::number() {
  const char *start = m_Current;
  int line = m_Line;
  int col = m_Column;

  while (isDigit(peek()))
    advance();
  if (peek() == '.' && isDigit(peekNext())) {
    advance();
    while (isDigit(peek()))
      advance();
    return Token{TokenType::Float, std::string(start, m_Current), line, col};
  }

  // Handle specific type suffix like 10:u64 ? No, that's done in Parser
  // (colon). But number literal might have suffix like 10L? Not in Toka spec
  // yet.

  return Token{TokenType::Integer, std::string(start, m_Current), line, col};
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
    return Token{TokenType::Ampersand, "&", line, col};
  case '~':
    return Token{TokenType::Tilde, "~", line, col};
  case '@':
    return Token{TokenType::At, "@", line, col};
  case '|':
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
  case '<':
    if (peek() == '-') {
      advance();
      return Token{TokenType::Dependency, "<-", line, col};
    }
    return Token{TokenType::Less, "<", line, col};
  case '>':
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
