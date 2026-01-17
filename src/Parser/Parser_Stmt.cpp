#include "toka/Parser.h"
#include <algorithm>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace toka {

std::unique_ptr<Stmt> Parser::parseVariableDecl(bool isPub) {
  bool isConst = match(TokenType::KwConst);
  if (!isConst) {
    match(TokenType::KwLet);
    if (previous().Kind != TokenType::KwLet)
      match(TokenType::KwAuto);
  }

  bool isRef = match(TokenType::Ampersand);
  bool hasPointer = false;
  bool isUnique = false;

  bool isShared = false;
  bool isRebindable = false;
  bool isPtrNullable = false;
  if (match(TokenType::Caret)) {
    isUnique = true;
    Token tok = previous();
    isRebindable = tok.IsSwappablePtr;
    isPtrNullable = tok.HasNull;
  } else if (match(TokenType::Star)) {
    hasPointer = true;
    Token tok = previous();
    isRebindable = tok.IsSwappablePtr;
    isPtrNullable = tok.HasNull;
  } else if (match(TokenType::Tilde)) {
    isShared = true;
    Token tok = previous();
    isRebindable = tok.IsSwappablePtr;
    isPtrNullable = tok.HasNull;
  }

  // Check for positional destructuring: let Type(v1, v2) = ...
  if (check(TokenType::Identifier) && checkAt(1, TokenType::LParen)) {
    Token typeName = advance();
    consume(TokenType::LParen, "Expected '(' for destructuring");
    std::vector<DestructuredVar> vars;
    while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
      Token varName = consume(TokenType::Identifier, "Expected variable name");
      vars.push_back({varName.Text, varName.HasWrite, varName.HasNull});
      if (!match(TokenType::Comma))
        break;
    }
    consume(TokenType::RParen, "Expected ')' after destructuring");
    consume(TokenType::Equal, "Expected '=' for destructuring");
    auto init = parseExpr();
    expectEndOfStatement();
    auto node = std::make_unique<DestructuringDecl>(
        typeName.Text, std::move(vars), std::move(init));
    node->setLocation(typeName, m_CurrentFile);
    return node;
  }

  Token name = consume(TokenType::Identifier, "Expected variable name");

  std::string typeName = "";
  if (match(TokenType::Colon)) {
    int depth = 0;
    while (!check(TokenType::EndOfFile)) {
      if (check(TokenType::LBracket))
        depth++;
      if (check(TokenType::RBracket))
        depth--;

      if (depth == 0 && (isEndOfStatement() || check(TokenType::Equal) ||
                         check(TokenType::Comma) || check(TokenType::RParen) ||
                         check(TokenType::LBrace)))
        break;
      typeName += advance().Text;
    }
  }

  std::unique_ptr<Expr> init;
  if (match(TokenType::Equal)) {
    init = parseExpr();
  }

  auto node = std::make_unique<VariableDecl>(name.Text, std::move(init));
  node->setLocation(name, m_CurrentFile);
  node->HasPointer = hasPointer;
  node->IsUnique = isUnique;
  node->IsShared = isShared;
  node->IsReference = isRef;
  node->IsPub = isPub;
  node->IsConst = isConst;
  // node->IsMutable = name.HasWrite; // Deprecated
  // node->IsNullable = name.HasNull; // Deprecated
  // Explicit properties mapping
  node->IsValueMutable = name.HasWrite;
  node->IsValueNullable = name.HasNull;
  node->IsRebindable = isRebindable;
  node->IsPointerNullable = isPtrNullable;
  node->TypeName = typeName;

  expectEndOfStatement();
  return node;
}

std::unique_ptr<Stmt> Parser::parseStmt() {
  if (check(TokenType::LBrace))
    return parseBlock();
  if (check(TokenType::KwIf))
    return std::make_unique<ExprStmt>(parseIf());
  if (match(TokenType::KwMatch))
    return std::make_unique<ExprStmt>(parseMatchExpr());
  if (check(TokenType::KwWhile))
    return std::make_unique<ExprStmt>(parseWhile());
  if (check(TokenType::KwLoop))
    return std::make_unique<ExprStmt>(parseLoop());
  if (check(TokenType::KwFor))
    return std::make_unique<ExprStmt>(parseForExpr());
  if (check(TokenType::KwReturn))
    return parseReturn();
  if (check(TokenType::KwLet) || check(TokenType::KwAuto))
    return parseVariableDecl(false);
  if (check(TokenType::KwDelete))
    return parseDeleteStmt();
  if (check(TokenType::KwUnsafe))
    return parseUnsafeStmt();
  if (check(TokenType::KwFree))
    return parseFreeStmt();

  // ExprStmt
  auto expr = parseExpr();
  if (expr) {
    expectEndOfStatement();
    return std::make_unique<ExprStmt>(std::move(expr));
  }
  return nullptr;
}

std::unique_ptr<BlockStmt> Parser::parseBlock() {
  Token tok = consume(TokenType::LBrace, "Expected '{'");
  auto block = std::make_unique<BlockStmt>();
  block->setLocation(tok, m_CurrentFile);

  while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
    auto stmt = parseStmt();
    if (stmt)
      block->Statements.push_back(std::move(stmt));
    else
      advance(); // Avoid infinite loop if null
  }

  consume(TokenType::RBrace, "Expected '}'");
  return block;
}

std::unique_ptr<ReturnStmt> Parser::parseReturn() {
  Token tok = consume(TokenType::KwReturn, "Expected 'return'");
  std::unique_ptr<Expr> val;
  if (!isEndOfStatement()) {
    val = parseExpr();
  }
  expectEndOfStatement();
  auto node = std::make_unique<ReturnStmt>(std::move(val));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Stmt> Parser::parseDeleteStmt() {
  Token kw = consume(TokenType::KwDelete, "Expected 'del' or 'delete'");
  auto expr = parseExpr();
  expectEndOfStatement();
  auto node = std::make_unique<DeleteStmt>(std::move(expr));
  node->setLocation(kw, m_CurrentFile);
  return node;
}

std::unique_ptr<Stmt> Parser::parseUnsafeStmt() {
  Token tok = consume(TokenType::KwUnsafe, "Expected 'unsafe'");
  if (check(TokenType::LBrace)) {
    auto block = parseBlock();
    auto node = std::make_unique<UnsafeStmt>(std::move(block));
    node->setLocation(tok, m_CurrentFile);
    return node;
  }
  if (check(TokenType::KwFree)) {
    auto freeStmt = parseFreeStmt();
    auto node = std::make_unique<UnsafeStmt>(std::move(freeStmt));
    node->setLocation(tok, m_CurrentFile);
    return node;
  }
  // 行级 unsafe: unsafe p#[0] = 1
  auto stmt = parseStmt();
  auto node = std::make_unique<UnsafeStmt>(std::move(stmt));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Stmt> Parser::parseFreeStmt() {
  Token tok = consume(TokenType::KwFree, "Expected 'free'");
  std::unique_ptr<Expr> count = nullptr;
  if (match(TokenType::LBracket)) {
    count = parseExpr();
    consume(TokenType::RBracket, "Expected ']'");
  }
  auto expr = parseExpr();
  expectEndOfStatement();
  auto node = std::make_unique<FreeStmt>(std::move(expr), std::move(count));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

} // namespace toka
