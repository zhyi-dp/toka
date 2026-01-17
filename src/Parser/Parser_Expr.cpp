// Auto-refactored split
#include "toka/Parser.h"
#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <algorithm>


namespace toka {


std::unique_ptr<Expr> Parser::parseIf() {
  Token tok = previous(); // consumed by match(KwIf)
  if (tok.Kind != TokenType::KwIf)
    tok = consume(TokenType::KwIf, "Expected 'if'");
  bool hasParen = match(TokenType::LParen);
  auto cond = parseExpr();
  if (hasParen)
    consume(TokenType::RParen, "Expected ')'");
  auto thenStmt = parseStmt();
  std::unique_ptr<Stmt> elseStmt;
  if (match(TokenType::KwElse)) {
    elseStmt = parseStmt();
  }
  auto node = std::make_unique<IfExpr>(std::move(cond), std::move(thenStmt),
                                       std::move(elseStmt));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Expr> Parser::parseWhile() {
  Token tok = previous();
  if (tok.Kind != TokenType::KwWhile)
    tok = consume(TokenType::KwWhile, "Expected 'while'");
  bool hasParen = match(TokenType::LParen);
  auto cond = parseExpr();
  if (hasParen)
    consume(TokenType::RParen, "Expected ')'");
  auto body = parseStmt();
  std::unique_ptr<Stmt> elseBody;
  if (match(TokenType::KwOr)) {
    elseBody = parseBlock();
  }
  auto node = std::make_unique<WhileExpr>(std::move(cond), std::move(body),
                                          std::move(elseBody));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Expr> Parser::parseLoop() {
  Token tok = previous();
  if (tok.Kind != TokenType::KwLoop)
    tok = consume(TokenType::KwLoop, "Expected 'loop'");
  auto body = parseStmt();
  auto node = std::make_unique<LoopExpr>(std::move(body));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Expr> Parser::parseForExpr() {
  Token tok = previous();
  if (tok.Kind != TokenType::KwFor)
    tok = consume(TokenType::KwFor, "Expected 'for'");

  bool isMut = match(TokenType::KwLet);
  if (!isMut)
    match(TokenType::KwAuto); // Allow optional auto
  bool isRef = match(TokenType::Ampersand);
  Token varName =
      consume(TokenType::Identifier, "Expected variable name in for");
  consume(TokenType::KwIn, "Expected 'in' in for loop");
  auto collection = parseExpr();
  auto body = parseStmt();
  std::unique_ptr<Stmt> elseBody;
  if (match(TokenType::KwOr)) {
    elseBody = parseBlock();
  }
  auto node = std::make_unique<ForExpr>(varName.Text, isRef, isMut,
                                        std::move(collection), std::move(body),
                                        std::move(elseBody));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Expr> Parser::parseBreak() {
  Token tok = previous();
  std::string label = "";
  if (match(TokenType::KwTo)) {
    label = consume(TokenType::Identifier, "Expected label after 'to'").Text;
  }
  std::unique_ptr<Expr> val;
  if (!isEndOfStatement() && !check(TokenType::RBrace)) {
    val = parseExpr();
  }
  auto node = std::make_unique<BreakExpr>(label, std::move(val));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Expr> Parser::parseContinue() {
  Token tok = previous();
  std::string label = "";
  if (match(TokenType::KwTo)) {
    label = consume(TokenType::Identifier, "Expected label after 'to'").Text;
  }
  auto node = std::make_unique<ContinueExpr>(label);
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Expr> Parser::parsePass() {
  Token tok = previous();
  auto val = parseExpr();
  auto node = std::make_unique<PassExpr>(std::move(val));
  node->setLocation(tok, m_CurrentFile);
  return node;
}



} // namespace toka