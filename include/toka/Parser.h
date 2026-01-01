#pragma once

#include "toka/AST.h"
#include "toka/Lexer.h"
#include <memory>
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
  std::unique_ptr<FunctionDecl> parseFunctionDecl();
  std::unique_ptr<Stmt> parseVariableDecl();
  std::unique_ptr<ExternDecl> parseExternDecl();
  std::unique_ptr<ImportDecl> parseImport();
  std::unique_ptr<TypeAliasDecl> parseTypeAliasDecl();
  std::unique_ptr<StructDecl> parseStruct();
  std::unique_ptr<OptionDecl> parseOptionDecl(); // ADT
  std::unique_ptr<ImplDecl> parseImpl();
  std::unique_ptr<TraitDecl> parseTrait();
  std::unique_ptr<Stmt> parseMatchStmt();

  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<Stmt> parseIf();
  std::unique_ptr<Stmt> parseWhile();
  std::unique_ptr<BlockStmt> parseBlock();
  std::unique_ptr<Stmt> parseDeleteStmt();
  std::unique_ptr<ReturnStmt> parseReturn();

  std::unique_ptr<Expr> parseExpr(int minPrec = 0);
  int getPrecedence(TokenType type);
  std::unique_ptr<Expr> parsePrimary();

  // ... Add more precedence helpers here
};

} // namespace toka
