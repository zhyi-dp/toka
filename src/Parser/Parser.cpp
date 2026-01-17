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
#include "toka/Parser.h"
#include "toka/DiagnosticEngine.h"
#include <algorithm>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace toka {

const Token &Parser::peek() const {
  if (m_Pos >= m_Tokens.size())
    return m_Tokens.back(); // EOF
  return m_Tokens[m_Pos];
}

const Token &Parser::peekAt(int offset) const {
  if (m_Pos + offset >= m_Tokens.size())
    return m_Tokens.back();
  return m_Tokens[m_Pos + offset];
}

const Token &Parser::previous() const { return m_Tokens[m_Pos - 1]; }

Token Parser::advance() {
  if (m_Pos < m_Tokens.size())
    m_Pos++;
  return previous();
}

bool Parser::check(TokenType type) const { return peek().Kind == type; }

bool Parser::checkAt(int offset, TokenType type) const {
  if (peekAt(offset).Kind == TokenType::EndOfFile)
    return false;
  return peekAt(offset).Kind == type;
}

bool Parser::match(TokenType type) {
  if (check(type)) {
    advance();
    return true;
  }
  return false;
}

Token Parser::consume(TokenType type, const std::string &message) {
  if (check(type))
    return advance();
  error(peek(), message);
  return peek();
}

void Parser::expectEndOfStatement() {
  if (isEndOfStatement()) {
    if (match(TokenType::Semicolon)) {
      // Consumed
    }
    return;
  }
  DiagnosticEngine::report(peek().Loc, DiagID::ERR_EXPECTED_SEMI);
  std::exit(1);
}

bool Parser::isEndOfStatement() {
  if (check(TokenType::Semicolon))
    return true;
  if (check(TokenType::RBrace))
    return true;
  if (peek().Kind == TokenType::EndOfFile)
    return true;

  // Newline rule
  if (peek().HasNewlineBefore) {
    // If previous was an operator, it's not a terminator
    TokenType prev = previous().Kind;
    switch (prev) {
    case TokenType::Plus:
    case TokenType::Minus:
    case TokenType::Star:
    case TokenType::Slash:
    case TokenType::Equal:
    case TokenType::PlusEqual:
    case TokenType::MinusEqual:
    case TokenType::StarEqual:
    case TokenType::SlashEqual:
    case TokenType::DoubleEqual:
    case TokenType::Neq:
    case TokenType::Less:
    case TokenType::Greater:
    case TokenType::And:
    case TokenType::Or:
    case TokenType::Dot:
    case TokenType::Arrow:
    case TokenType::Comma:
    case TokenType::Colon:
    case TokenType::At:
    case TokenType::Dependency:
    case TokenType::LParen:
    case TokenType::LBracket:
    case TokenType::LBrace:
    case TokenType::Ampersand:
    case TokenType::Pipe:
    case TokenType::Caret:
    case TokenType::Tilde:
      return false;
    default:
      return true;
    }
  }

  return false;
}

void Parser::error(const Token &tok, const std::string &message) {
  DiagnosticEngine::report(tok.Loc, DiagID::ERR_GENERIC_PARSE, message);
  std::exit(1);
}

std::unique_ptr<Module> Parser::parseModule() {
  auto module = std::make_unique<Module>();
  // module->FileName = m_CurrentFile;
  if (peek().Kind != TokenType::EndOfFile) {
    module->Loc = peek().Loc;
  } else {
    // Empty file, usage might be tricky, but we should still have valid loc
    // from EOF token
    module->Loc = peek().Loc;
  }

  while (peek().Kind != TokenType::EndOfFile) {
    std::cerr << "Parsing Top Level: " << peek().toString() << " at line "
              << peek().Line << "\n";
    bool isPub = false;
    if (match(TokenType::KwPub)) {
      isPub = true;
    }

    if (check(TokenType::KwImport)) {
      module->Imports.push_back(parseImport(isPub));
    } else if (check(TokenType::KwFn)) {
      module->Functions.push_back(parseFunctionDecl(isPub));
    } else if (check(TokenType::KwLet) || check(TokenType::KwAuto) ||
               check(TokenType::KwConst)) {
      module->Globals.push_back(parseVariableDecl(isPub));
    } else if (check(TokenType::KwType) || check(TokenType::KwAlias)) {
      module->TypeAliases.push_back(parseTypeAliasDecl(isPub));
    } else if (check(TokenType::KwExtern)) {
      if (isPub) {
        DiagnosticEngine::report(peek().Loc, DiagID::ERR_EXTERN_PUB);
        std::exit(1);
      }
      module->Externs.push_back(parseExternDecl());
    } else if (check(TokenType::KwImpl)) {
      if (isPub) {
        DiagnosticEngine::report(peek().Loc, DiagID::ERR_IMPL_PUB);
        std::exit(1);
      }
      module->Impls.push_back(parseImpl());
    } else if (check(TokenType::KwTrait)) {
      module->Traits.push_back(parseTrait(isPub));
    } else if (check(TokenType::KwShape) || check(TokenType::KwPacked)) {
      module->Shapes.push_back(parseShape(isPub));
    } else if (check(TokenType::Identifier) && checkAt(1, TokenType::Equal)) {
      // Legacy or alternate struct init?
      module->Shapes.push_back(parseShape(isPub));
    } else {
      if (isPub) {
        DiagnosticEngine::report(peek().Loc, DiagID::ERR_EXPECTED_DECL);
        std::exit(1);
      }
      std::cerr << "Unexpected Top Level Token: " << peek().toString() << "\n";
      advance();
    }
  }
  return module;
}

} // namespace toka
