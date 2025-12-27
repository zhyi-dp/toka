#pragma once

#include "toka/AST.h"
#include "toka/Lexer.h"
#include <memory>
#include <vector>

namespace toka {

class Parser {
public:
  Parser(const std::vector<Token> &tokens) : m_Tokens(tokens), m_Pos(0) {}

  // Top level
  std::unique_ptr<Module> parseModule();

private:
  const std::vector<Token> &m_Tokens;
  size_t m_Pos;

  // Helpers
  const Token &peek() const;
  const Token &previous() const;
  Token advance();
  bool check(TokenType type) const;
  bool match(TokenType type);
  Token consume(TokenType type, const std::string &message);

  // Recursive Descent Methods
  std::unique_ptr<FunctionDecl> parseFunctionDecl();
  std::unique_ptr<VariableDecl> parseVariableDecl();
  std::unique_ptr<ExternDecl> parseExternDecl();
  std::unique_ptr<ImportDecl> parseImport();
  std::unique_ptr<StructDecl> parseStruct();

  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<Stmt> parseIf();
  std::unique_ptr<Stmt> parseWhile();
  std::unique_ptr<BlockStmt> parseBlock();
  std::unique_ptr<ReturnStmt> parseReturn();

  std::unique_ptr<Expr> parseExpr(int minPrec = 0);
  int getPrecedence(TokenType type);
  std::unique_ptr<Expr> parsePrimary();

  // ... Add more precedence helpers here
};

} // namespace toka
