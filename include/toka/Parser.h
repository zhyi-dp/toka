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

#include "toka/AST.h"
#include "toka/Lexer.h"
#include <memory>
#include <string>
#include <vector>

namespace toka {

class Parser {
public:
  Parser(const std::vector<Token> &tokens, const std::string &fileName = "")
      : m_Tokens(tokens), m_Pos(0), m_CurrentFile(fileName) {}

  // Top level
  std::unique_ptr<Module> parseModule();

private:
  const std::vector<Token> &m_Tokens;
  size_t m_Pos;
  std::string m_CurrentFile;

  // Helpers
  const Token &peek() const;
  const Token &peekAt(int offset) const;
  const Token &previous() const;
  Token advance();
  bool check(TokenType type) const;
  bool checkAt(int offset, TokenType type) const;
  bool match(TokenType type);
  Token consume(TokenType type, const std::string &message);
  void expectEndOfStatement();
  bool isEndOfStatement();
  void error(const Token &tok, const std::string &message);

  // Recursive Descent Methods
  std::unique_ptr<FunctionDecl> parseFunctionDecl(bool isPub = false);
  std::unique_ptr<Stmt> parseVariableDecl(bool isPub = false);
  std::unique_ptr<ExternDecl> parseExternDecl();
  std::unique_ptr<ImportDecl> parseImport(bool isPub = false);
  std::unique_ptr<TypeAliasDecl> parseTypeAliasDecl(bool isPub = false);
  std::unique_ptr<ShapeDecl> parseShape(bool isPub = false);
  std::unique_ptr<ImplDecl> parseImpl();
  std::unique_ptr<TraitDecl> parseTrait(bool isPub = false);
  std::unique_ptr<Expr> parseMatchExpr();
  std::unique_ptr<MatchArm::Pattern> parsePattern();

  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<Expr> parseIf();
  std::unique_ptr<Expr> parseWhile();
  std::unique_ptr<Expr> parseLoop();
  std::unique_ptr<Expr> parseForExpr();
  std::unique_ptr<Expr> parseBreak();
  std::unique_ptr<Expr> parseContinue();
  std::unique_ptr<Expr> parsePass();
  std::unique_ptr<BlockStmt> parseBlock();
  std::unique_ptr<Stmt> parseDeleteStmt();
  std::unique_ptr<Stmt> parseUnsafeStmt();
  std::unique_ptr<Expr> parseUnsafeExpr();
  std::unique_ptr<Stmt> parseFreeStmt();
  std::unique_ptr<Expr> parseAllocExpr();
  std::unique_ptr<ReturnStmt> parseReturn();

  std::unique_ptr<Expr> parseExpr(int minPrec = 0);
  std::unique_ptr<Expr> parsePrimary();

  // ... Add more precedence helpers here
};

} // namespace toka
