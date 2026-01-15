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
#include <iostream>

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
  error(peek(), "Expected ';' or newline at end of statement");
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
  std::cerr << m_CurrentFile << ":" << tok.Line << ":" << tok.Column
            << ": error: " << message << "\n";
  std::exit(1);
}

std::unique_ptr<Module> Parser::parseModule() {
  auto module = std::make_unique<Module>();
  module->FileName = m_CurrentFile;

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
      if (isPub)
        error(peek(), "Extern blocks cannot be marked public");
      module->Externs.push_back(parseExternDecl());
    } else if (check(TokenType::KwImpl)) {
      if (isPub)
        error(peek(), "Impl blocks cannot be marked public");
      module->Impls.push_back(parseImpl());
    } else if (check(TokenType::KwTrait)) {
      module->Traits.push_back(parseTrait(isPub));
    } else if (check(TokenType::KwShape) || check(TokenType::KwPacked)) {
      module->Shapes.push_back(parseShape(isPub));
    } else if (check(TokenType::Identifier) && checkAt(1, TokenType::Equal)) {
      // Legacy or alternate struct init?
      module->Shapes.push_back(parseShape(isPub));
    } else {
      if (isPub)
        error(peek(), "Expected declaration after 'pub'");
      std::cerr << "Unexpected Top Level Token: " << peek().toString() << "\n";
      advance();
    }
  }
  return module;
}

std::unique_ptr<MatchArm::Pattern> Parser::parsePattern() {
  bool isMut = false;
  bool isRef = false;
  match(TokenType::KwAuto); // skip auto if present

  if (match(TokenType::Ampersand)) {
    isRef = true;
    if (match(TokenType::KwMut))
      isMut = true;
  }

  if (check(TokenType::Integer) || check(TokenType::String) ||
      check(TokenType::KwTrue) || check(TokenType::KwFalse)) {
    auto p = std::make_unique<MatchArm::Pattern>(MatchArm::Pattern::Literal);
    p->Name = advance().Text;
    if (previous().Kind == TokenType::Integer) {
      try {
        p->LiteralVal = std::stoull(previous().Text, nullptr, 0);
      } catch (...) {
        p->LiteralVal = 0;
      }
    }
    return p;
  }

  if (check(TokenType::Identifier)) {
    if (peek().Text == "_") {
      advance();
      return std::make_unique<MatchArm::Pattern>(MatchArm::Pattern::Wildcard);
    }

    Token nameTok = advance();
    std::string name = nameTok.Text;

    // Handle Path::Variant
    if (check(TokenType::Colon) && checkAt(1, TokenType::Colon)) {
      consume(TokenType::Colon, "");
      consume(TokenType::Colon, "");
      name +=
          "::" + consume(TokenType::Identifier, "Expected variant name").Text;
    }

    if (match(TokenType::LParen)) {
      std::vector<std::unique_ptr<MatchArm::Pattern>> subs;
      while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
        subs.push_back(parsePattern());
        if (!check(TokenType::RParen))
          match(TokenType::Comma);
      }
      consume(TokenType::RParen, "Expected ')' after subpatterns");
      auto p = std::make_unique<MatchArm::Pattern>(MatchArm::Pattern::Decons);
      p->Name = name;
      p->SubPatterns = std::move(subs);
      p->IsReference = isRef;
      return p;
    }

    auto p = std::make_unique<MatchArm::Pattern>(MatchArm::Pattern::Variable);
    p->Name = name;
    p->IsReference = isRef;
    p->IsMutable = nameTok.HasWrite;
    return p;
  }

  if (match(TokenType::KwDefault)) {
    return std::make_unique<MatchArm::Pattern>(MatchArm::Pattern::Wildcard);
  }

  error(peek(), "Expected pattern");
  return nullptr;
}

std::unique_ptr<Expr> Parser::parseMatchExpr() {
  Token matchTok = previous(); // KwMatch or peek()
  auto target = parseExpr();
  consume(TokenType::LBrace, "Expected '{' after match expression");

  std::vector<std::unique_ptr<MatchArm>> arms;
  while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
    auto pat = parsePattern();
    std::unique_ptr<Expr> guard = nullptr;
    if (match(TokenType::KwIf)) {
      guard = parseExpr();
    }
    consume(TokenType::FatArrow, "Expected '=>'");
    auto body = parseStmt();
    arms.push_back(std::make_unique<MatchArm>(std::move(pat), std::move(guard),
                                              std::move(body)));
  }
  consume(TokenType::RBrace, "Expected '}' after match arms");
  auto matched =
      std::make_unique<MatchExpr>(std::move(target), std::move(arms));
  matched->setLocation(matchTok, m_CurrentFile);
  return matched;
}

std::unique_ptr<ShapeDecl> Parser::parseShape(bool isPub) {
  match(TokenType::KwShape); // Optional
  match(TokenType::KwPacked);
  bool packed = previous().Kind == TokenType::KwPacked;
  if (packed)
    consume(TokenType::KwShape, "Expected 'shape' after 'packed'");

  Token name = consume(TokenType::Identifier, "Expected shape name");
  ShapeKind kind = ShapeKind::Struct;
  std::vector<ShapeMember> members;
  int64_t arraySize = 0;

  match(TokenType::Equal);

  if (match(TokenType::LBracket)) {
    kind = ShapeKind::Array;
    std::string elemTy = advance().Text;
    consume(TokenType::Semicolon, "Expected ';'");
    arraySize = std::stoull(consume(TokenType::Integer, "Expected size").Text,
                            nullptr, 0);
    consume(TokenType::RBracket, "Expected ']'");
    ShapeMember m;
    m.Name = "0";
    m.Type = elemTy;
    members.push_back(std::move(m));
  } else if (match(TokenType::LParen)) {
    bool isEnum = false;
    int depth = 0;
    for (int i = 0;; ++i) {
      TokenType t = peekAt(i).Kind;
      if (t == TokenType::EndOfFile || (t == TokenType::RParen && depth == 0))
        break;
      if (depth == 0 && (t == TokenType::Pipe || t == TokenType::Equal)) {
        isEnum = true;
        break;
      }
      if (t == TokenType::LParen)
        depth++;
      else if (t == TokenType::RParen)
        depth--;
    }

    if (isEnum) {
      kind = ShapeKind::Enum;
      while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
        ShapeMember v;
        v.Name = consume(TokenType::Identifier, "Expected variant").Text;
        if (match(TokenType::LParen)) {
          v.SubKind = ShapeKind::Tuple;
          while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
            ShapeMember field;
            field.Type = advance().Text;
            v.SubMembers.push_back(std::move(field));
            if (!check(TokenType::RParen))
              match(TokenType::Comma);
          }
          consume(TokenType::RParen, "Expected ')'");
        }
        if (match(TokenType::Equal)) {
          v.TagValue = std::stoull(
              consume(TokenType::Integer, "Expected tag").Text, nullptr, 0);
        }
        members.push_back(std::move(v));
        if (!check(TokenType::RParen))
          match(TokenType::Pipe);
      }
    } else {
      for (int i = 0;; ++i) {
        if (peekAt(i).Kind == TokenType::EndOfFile ||
            peekAt(i).Kind == TokenType::RParen)
          break;
        if (peekAt(i).Kind == TokenType::Colon) {
          kind = ShapeKind::Struct;
          break;
        }
        if (peekAt(i).Kind == TokenType::Comma) {
          kind = ShapeKind::Tuple;
          break;
        }
      }
      int idx = 0;
      while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
        ShapeMember m;
        if (kind == ShapeKind::Struct) {
          std::string prefixType = "";
          if (match(TokenType::Star)) {
            prefixType = "*";
            if (previous().HasNull)
              prefixType += "?";
            else if (previous().IsSwappablePtr)
              prefixType += "!";
          } else if (match(TokenType::Caret)) {
            prefixType = "^";
            if (previous().HasNull)
              prefixType += "?";
            else if (previous().IsSwappablePtr)
              prefixType += "!";
          } else if (match(TokenType::Tilde)) {
            prefixType = "~";
            if (previous().HasNull)
              prefixType += "?";
            else if (previous().IsSwappablePtr)
              prefixType += "!";
          } else if (match(TokenType::Ampersand)) {
            prefixType = "&";
            if (previous().HasNull)
              prefixType += "?";
            else if (previous().IsSwappablePtr)
              prefixType += "!";
          }

          m.Name = consume(TokenType::Identifier, "Expected field name").Text;
          consume(TokenType::Colon, "Expected ':'");

          m.Type = prefixType; // Start with prefix
        } else {
          m.Name = std::to_string(idx++);
          m.Type = "";
        }

        if (kind == ShapeKind::Struct && m.Type.empty()) {
          // If we didn't have a prefix, initiate empty
          m.Type = "";
        }
        int bracketDepth = 0;
        while (!check(TokenType::EndOfFile)) {
          if (check(TokenType::LBracket))
            bracketDepth++;
          if (check(TokenType::RBracket))
            bracketDepth--;

          if (bracketDepth == 0 &&
              (check(TokenType::Comma) || check(TokenType::RParen)))
            break;

          m.Type += advance().Text;
        }
        members.push_back(std::move(m));
        if (!check(TokenType::RParen))
          match(TokenType::Comma);
      }
    }
    match(TokenType::RParen);
  } else {
    error(peek(), "Expected '(' or '[' after shape name");
  }

  auto decl = std::make_unique<ShapeDecl>(isPub, name.Text, kind,
                                          std::move(members), packed);
  decl->ArraySize = arraySize;
  decl->FileName = m_CurrentFile;
  decl->setLocation(name, m_CurrentFile);
  return decl;
}

std::unique_ptr<FunctionDecl> Parser::parseFunctionDecl(bool isPub) {
  if (match(TokenType::KwPub))
    isPub = true;
  consume(TokenType::KwFn, "Expected 'fn'");
  Token name;
  if (check(TokenType::KwMain)) {
    name = advance();
    name.Kind = TokenType::Identifier;
  } else if (check(TokenType::KwNew)) {
    name = advance();
    name.Text = "new";
    name.Kind = TokenType::Identifier;
  } else if (check(TokenType::Identifier) || check(TokenType::KwFree) ||
             check(TokenType::KwAlloc)) {
    name = advance();
  } else {
    error(peek(), "Expected function name");
    return nullptr;
  }
  consume(TokenType::LParen, "Expected '('");
  std::vector<FunctionDecl::Arg> args;
  bool isVariadic = false;
  bool firstArg = true;
  if (!check(TokenType::RParen)) {
    do {
      if (check(TokenType::DotDotDot))
        break;

      if (firstArg && match(TokenType::KwSelf)) {
        FunctionDecl::Arg arg;
        arg.Name = "self";
        arg.Type = "Self";
        arg.HasPointer = false;
        // Capture mutability from token (e.g. self#)
        if (previous().HasWrite) {
          arg.IsMutable = true;
          arg.IsValueMutable = true;
        }
        args.push_back(arg);
        firstArg = false;
        continue;
      }
      firstArg = false;

      bool isRef = match(TokenType::Ampersand);
      bool hasPointer = false;
      bool isUnique = false;
      bool isShared = false;

      bool isRebindable = false;
      bool isPtrNullable = false;

      if (match(TokenType::Caret)) {
        isUnique = true;
        Token t = previous();
        isRebindable = t.IsSwappablePtr;
        isPtrNullable = t.HasNull;
      } else if (check(TokenType::Star)) {
        Token t = advance();
        hasPointer = true;
        isRebindable = t.IsSwappablePtr;
        isPtrNullable = t.HasNull;
        if (t.IsSwappablePtr)
          isRef = true; // Legacy support? *# implies rebindable pointer,
                        // usually by reference
      } else if (match(TokenType::Tilde)) {
        isShared = true;
        Token t = previous();
        isRebindable = t.IsSwappablePtr;
        isPtrNullable = t.HasNull;
      }
      Token argName = consume(TokenType::Identifier, "Expected argument name");
      std::string argType = "i64"; // fallback
      if (match(TokenType::Colon)) {
        argType = "";
        while (!check(TokenType::Comma) && !check(TokenType::RParen) &&
               !check(TokenType::EndOfFile)) {
          argType += advance().Text;
        }
      }
      FunctionDecl::Arg arg;
      arg.Name = argName.Text;
      arg.Type = argType;
      arg.HasPointer = hasPointer;
      arg.IsReference = isRef;
      arg.IsMutable = argName.HasWrite;
      arg.IsNullable = argName.HasNull;

      // New Permissions
      arg.IsUnique = isUnique;
      arg.IsShared = isShared;
      arg.IsRebindable = isRebindable; // Captured from token
      arg.IsPointerNullable = isPtrNullable;
      arg.IsValueMutable = argName.HasWrite;
      arg.IsValueNullable = argName.HasNull;

      std::cerr << "DEBUG: Parsed Arg " << arg.Name
                << " IsRebindable=" << arg.IsRebindable << "\n";

      args.push_back(std::move(arg));
    } while (match(TokenType::Comma));
  }
  if (match(TokenType::DotDotDot)) {
    isVariadic = true;
  }
  consume(TokenType::RParen, "Expected ')'");

  // Return Type
  std::string retType = "void"; // default
  if (match(TokenType::Arrow)) {
    retType = "";
    while (!check(TokenType::LBrace) && !isEndOfStatement() &&
           !check(TokenType::EndOfFile)) {
      retType += advance().Text;
    }
  }

  std::unique_ptr<BlockStmt> body = nullptr;
  if (check(TokenType::LBrace)) {
    body = parseBlock();
  } else {
    expectEndOfStatement();
  }
  auto decl = std::make_unique<FunctionDecl>(isPub, name.Text, args,
                                             std::move(body), retType);
  decl->IsVariadic = isVariadic;
  decl->setLocation(name, m_CurrentFile);
  return decl;
}

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
  node->IsMutable = name.HasWrite; // Value Mutable
  node->IsNullable = name.HasNull; // Value Nullable
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

// getPrecedence moved to Token.h

std::unique_ptr<Expr> Parser::parseExpr(int minPrec) {
  auto lhs = parsePrimary();
  if (!lhs)
    return nullptr;

  while (true) {
    if (check(TokenType::Colon) || check(TokenType::KwAs) ||
        (peek().Kind == TokenType::Identifier && peek().Text == "as")) {
      advance(); // consume ':' or 'as'
      std::string typeName = "";
      int depth = 0;
      while (true) {
        if (depth == 0) {
          if (check(TokenType::Comma) || check(TokenType::RParen) ||
              check(TokenType::RBrace) || isEndOfStatement() ||
              check(TokenType::Plus) || check(TokenType::Minus) ||
              // check(TokenType::Star) || // Don't break on * for types
              check(TokenType::Slash) || check(TokenType::Equal) ||
              check(TokenType::DoubleEqual) || check(TokenType::Neq) ||
              check(TokenType::Less) || check(TokenType::Greater) ||
              check(TokenType::KwIs) || check(TokenType::And) ||
              check(TokenType::Or) || check(TokenType::LBrace) ||
              check(TokenType::EndOfFile)) {
            break;
          }
        }
        Token t = advance();
        typeName += t.Text;
        if (t.Kind == TokenType::LBracket || t.Kind == TokenType::LParen)
          depth++;
        else if (t.Kind == TokenType::RBracket || t.Kind == TokenType::RParen)
          depth--;
      }
      Token tok = previous();
      auto node = std::make_unique<CastExpr>(std::move(lhs), typeName);
      node->setLocation(tok, m_CurrentFile);
      lhs = std::move(node);
      continue;
    }

    int prec = getPrecedence(peek().Kind);
    if (prec < minPrec)
      break;

    // Rule: Binary operators must be on the previous line to continue
    if (peek().HasNewlineBefore)
      break;

    Token op = advance();
    auto rhs = parseExpr(prec + 1);
    if (!rhs) {
      std::cerr << "Parser Error: Expected expression after operator\n";
      break;
    }

    auto node =
        std::make_unique<BinaryExpr>(op.Text, std::move(lhs), std::move(rhs));
    node->setLocation(op, m_CurrentFile);
    lhs = std::move(node);
  }

  return lhs;
}

std::unique_ptr<Expr> Parser::parsePrimary() {
  std::unique_ptr<Expr> expr = nullptr;
  if (match(TokenType::Bang) || match(TokenType::Minus) ||
      match(TokenType::PlusPlus) || match(TokenType::MinusMinus) ||
      match(TokenType::Caret) || match(TokenType::Tilde) ||
      match(TokenType::Star) || match(TokenType::Ampersand) ||
      match(TokenType::At)) {
    Token tok = previous();
    TokenType op = tok.Kind;
    auto sub = parsePrimary();
    auto node = std::make_unique<UnaryExpr>(op, std::move(sub));
    node->HasNull = tok.HasNull;
    node->IsRebindable = tok.IsSwappablePtr;
    node->IsValueMutable = tok.HasWrite;
    node->IsValueNullable = tok.HasNull;
    node->setLocation(tok, m_CurrentFile);
    return node;
  }

  if (match(TokenType::Integer)) {
    Token tok = previous();
    auto node = std::make_unique<NumberExpr>(std::stoull(tok.Text, nullptr, 0));
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::Float)) {
    Token tok = previous();
    auto node = std::make_unique<FloatExpr>(std::stod(tok.Text));
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::KwTrue)) {
    Token tok = previous();
    auto node = std::make_unique<BoolExpr>(true);
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::KwFalse)) {
    Token tok = previous();
    auto node = std::make_unique<BoolExpr>(false);
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::KwNull)) {
    Token tok = previous();
    auto node = std::make_unique<NullExpr>();
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::KwNone)) {
    Token tok = previous();
    auto node = std::make_unique<NoneExpr>();
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::String)) {
    Token tok = previous();
    auto node = std::make_unique<StringExpr>(tok.Text);
    node->setLocation(tok, m_CurrentFile);
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::KwIf)) {
    expr = parseIf();
  } else if (match(TokenType::KwWhile)) {
    expr = parseWhile();
  } else if (match(TokenType::KwLoop)) {
    expr = parseLoop();
  } else if (match(TokenType::KwFor)) {
    expr = parseForExpr();
  } else if (match(TokenType::KwMatch)) {
    expr = parseMatchExpr();
  } else if (match(TokenType::KwBreak)) {
    expr = parseBreak();
  } else if (match(TokenType::KwContinue)) {
    expr = parseContinue();
  } else if (match(TokenType::KwPass)) {
    expr = parsePass();
  } else if (match(TokenType::KwYield)) {
    // Treat yield as pass for now, or keep it separate if needed
    Token tok = previous();
    auto val = parseExpr();
    auto node = std::make_unique<PassExpr>(std::move(val));
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::KwSelf)) {
    Token tok = previous();
    auto node = std::make_unique<VariableExpr>("self");
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::KwUnsafe)) {
    expr = parseUnsafeExpr();
  } else if (match(TokenType::KwAlloc)) {
    expr = parseAllocExpr();
  } else if (match(TokenType::KwNew)) {
    Token kw = previous();
    Token typeName =
        consume(TokenType::Identifier, "Expected type after 'new'");
    std::unique_ptr<Expr> init = nullptr;
    if (check(TokenType::LBrace)) {
      advance(); // LBrace
      std::vector<std::pair<std::string, std::unique_ptr<Expr>>> fields;
      while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
        Token fieldName = consume(TokenType::Identifier, "Expected field name");
        consume(TokenType::Equal, "Expected '=' after field name");
        fields.push_back({fieldName.Text, parseExpr()});
        match(TokenType::Comma);
      }
      consume(TokenType::RBrace, "Expected '}'");
      auto node =
          std::make_unique<InitStructExpr>(typeName.Text, std::move(fields));
      node->setLocation(typeName, m_CurrentFile);
      init = std::move(node);
    } else if (check(TokenType::LParen)) {
      // new Type(...) -> treat as CallExpr for constructor
      consume(TokenType::LParen, "Expected '('");
      std::vector<std::unique_ptr<Expr>> args;
      if (!check(TokenType::RParen)) {
        do {
          args.push_back(parseExpr());
        } while (match(TokenType::Comma));
      }
      consume(TokenType::RParen, "Expected ')'");
      auto node = std::make_unique<CallExpr>(typeName.Text, std::move(args));
      node->setLocation(typeName, m_CurrentFile);
      init = std::move(node);
    } else {
      error(kw, "Expected '{' or '(' initializer for new expression");
      return nullptr;
    }
    auto node = std::make_unique<NewExpr>(typeName.Text, std::move(init));
    node->setLocation(kw, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::LBracket)) {
    // Array literal [1, 2, 3]
    std::vector<std::unique_ptr<Expr>> elements;
    if (!check(TokenType::RBracket)) {
      do {
        elements.push_back(parseExpr());
      } while (match(TokenType::Comma));
    }
    consume(TokenType::RBracket, "Expected ']' after array elements");
    expr = std::make_unique<ArrayExpr>(std::move(elements));
  } else if (check(TokenType::LParen)) {
    Token tok = peek();

    // Anonymous Record Detection: ( key = val ... )
    bool isAnonRecord = false;
    if (checkAt(1, TokenType::Identifier) && checkAt(2, TokenType::Equal)) {
      isAnonRecord = true;
    }

    consume(TokenType::LParen, "Expected '('");

    if (isAnonRecord) {
      std::vector<std::pair<std::string, std::unique_ptr<Expr>>> fields;
      while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
        Token key = consume(TokenType::Identifier, "Expected field name");
        consume(TokenType::Equal, "Expected '='");
        auto val = parseExpr();
        fields.push_back({key.Text, std::move(val)});
        if (!check(TokenType::RParen)) {
          consume(TokenType::Comma, "Expected ',' or ')'");
        }
      }
      consume(TokenType::RParen, "Expected ')'");
      auto node = std::make_unique<AnonymousRecordExpr>(std::move(fields));
      node->setLocation(tok, m_CurrentFile);
      expr = std::move(node);
    } else {
      // Grouping or Tuple
      std::vector<std::unique_ptr<Expr>> elements;
      bool isTuple = false;
      if (!check(TokenType::RParen)) {
        elements.push_back(parseExpr());
        if (match(TokenType::Comma)) {
          isTuple = true;
          while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
            elements.push_back(parseExpr());
            match(TokenType::Comma);
          }
        }
      }
      consume(TokenType::RParen, "Expected ')'");
      if (isTuple || elements.empty()) {
        auto node = std::make_unique<TupleExpr>(std::move(elements));
        node->setLocation(tok, m_CurrentFile);
        expr = std::move(node);
      } else {
        expr = std::move(elements[0]);
      }
    }
  } else if (match(TokenType::Identifier)) {
    Token name = previous();
    bool isStructInit = false;
    if (check(TokenType::LBrace)) {
      // Disambiguate against match block or other blocks starting with brace
      if (checkAt(1, TokenType::RBrace))
        isStructInit = true;
      else if (checkAt(1, TokenType::Identifier) &&
               checkAt(2, TokenType::Equal))
        isStructInit = true;
    }

    if (isStructInit) {
      // Struct Init
      advance();
      std::vector<std::pair<std::string, std::unique_ptr<Expr>>> fields;
      while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
        Token fieldName = consume(TokenType::Identifier, "Expected field name");
        consume(TokenType::Equal, "Expected '=' after field name");
        fields.push_back({fieldName.Text, parseExpr()});
        match(TokenType::Comma);
      }
      consume(TokenType::RBrace, "Expected '}'");
      expr = std::make_unique<InitStructExpr>(name.Text, std::move(fields));
      expr->setLocation(name, m_CurrentFile);
    } else if (match(TokenType::LParen)) {
      // Function Call
      std::vector<std::unique_ptr<Expr>> args;
      if (!check(TokenType::RParen)) {
        do {
          args.push_back(parseExpr());
        } while (match(TokenType::Comma));
      }
      consume(TokenType::RParen, "Expected ')' after arguments");
      auto node = std::make_unique<CallExpr>(name.Text, std::move(args));
      node->setLocation(name, m_CurrentFile);
      expr = std::move(node);
    } else {
      // Check for Scope Resolution (State::On)
      if (check(TokenType::Colon) && checkAt(1, TokenType::Colon)) {
        consume(TokenType::Colon, "");
        consume(TokenType::Colon, "");
        Token member =
            consume(TokenType::Identifier, "Expected member after ::");

        auto obj = std::make_unique<VariableExpr>(name.Text);
        obj->setLocation(name, m_CurrentFile);
        expr = std::make_unique<MemberExpr>(std::move(obj), member.Text, false,
                                            true);
        expr->setLocation(name, m_CurrentFile);

        if (match(TokenType::LParen)) {
          // Function Call on Member
          std::vector<std::unique_ptr<Expr>> args;
          if (!check(TokenType::RParen)) {
            do {
              args.push_back(parseExpr());
            } while (match(TokenType::Comma));
          }
          consume(TokenType::RParen, "Expected ')' after arguments");
          auto node = std::make_unique<CallExpr>(name.Text + "::" + member.Text,
                                                 std::move(args));
          node->setLocation(name, m_CurrentFile);
          expr = std::move(node);
          return expr;
        }
        return expr;
      }

      auto var = std::make_unique<VariableExpr>(name.Text);
      var->setLocation(name, m_CurrentFile);
      var->IsMutable = name.HasWrite;
      var->IsNullable = name.HasNull;
      expr = std::move(var);
    }
  } else {
    error(peek(), "Expected expression");
    return nullptr;
  }

  // Suffixes: .member, [index], etc.
  while (expr) {
    if (match(TokenType::Dot)) {
      Token dotTok = previous();

      std::string prefix = "";
      if (match(TokenType::Star))
        prefix = "*";
      else if (match(TokenType::Caret))
        prefix = "^";
      else if (match(TokenType::Tilde))
        prefix = "~";
      else if (match(TokenType::Ampersand))
        prefix = "&";

      if (match(TokenType::Identifier)) {
        std::string memberName = prefix + previous().Text;
        // Method Call check
        if (match(TokenType::LParen)) {
          std::vector<std::unique_ptr<Expr>> args;
          if (!check(TokenType::RParen)) {
            do {
              args.push_back(parseExpr());
            } while (match(TokenType::Comma));
          }
          consume(TokenType::RParen, "Expected ')' after method arguments");
          auto node = std::make_unique<MethodCallExpr>(
              std::move(expr), memberName, std::move(args));
          node->setLocation(dotTok, m_CurrentFile);
          expr = std::move(node);
        } else {
          // Member Access
          auto node = std::make_unique<MemberExpr>(std::move(expr), memberName);
          node->setLocation(dotTok, m_CurrentFile);
          expr = std::move(node);
        }
      } else if (prefix.empty() && match(TokenType::Integer)) {
        auto node =
            std::make_unique<MemberExpr>(std::move(expr), previous().Text);
        node->setLocation(dotTok, m_CurrentFile);
        expr = std::move(node);
      } else {
        error(peek(), "Expected member name or index after '.'");
      }
    } else if (match(TokenType::Arrow)) {
      Token arrowTok = previous();
      if (match(TokenType::Identifier) || match(TokenType::Integer)) {
        auto node = std::make_unique<MemberExpr>(std::move(expr),
                                                 previous().Text, true);
        node->setLocation(arrowTok, m_CurrentFile);
        expr = std::move(node);
      } else {
        error(peek(), "Expected member name or index after '->'");
      }
    } else if (match(TokenType::LBracket)) {
      std::vector<std::unique_ptr<Expr>> indices;
      if (!check(TokenType::RBracket)) {
        do {
          indices.push_back(parseExpr());
        } while (match(TokenType::Comma));
      }
      consume(TokenType::RBracket, "Expected ']' after index");
      expr =
          std::make_unique<ArrayIndexExpr>(std::move(expr), std::move(indices));
    } else if (match(TokenType::PlusPlus)) {
      expr =
          std::make_unique<PostfixExpr>(TokenType::PlusPlus, std::move(expr));
    } else if (match(TokenType::MinusMinus)) {
      expr =
          std::make_unique<PostfixExpr>(TokenType::MinusMinus, std::move(expr));
    } else {
      break;
    }
  }

  return expr;
}

std::unique_ptr<ExternDecl> Parser::parseExternDecl() {
  consume(TokenType::KwExtern, "Expected 'extern'");
  consume(TokenType::KwFn, "Expected 'fn'");
  Token name;
  if (check(TokenType::Identifier) || check(TokenType::KwFree) ||
      check(TokenType::KwAlloc)) {
    name = advance();
  } else {
    error(peek(), "Expected external function name");
    return nullptr;
  }
  consume(TokenType::LParen, "Expected '('");
  std::vector<ExternDecl::Arg> args;
  bool isVariadic = false;
  if (!check(TokenType::RParen)) {
    do {
      if (check(TokenType::DotDotDot))
        break;
      bool hasPointer = match(TokenType::Caret) || match(TokenType::Star);
      Token argName = consume(TokenType::Identifier, "Expected argument name");
      std::string argType = "i64";
      if (match(TokenType::Colon)) {
        argType = "";
        while (!check(TokenType::Comma) && !check(TokenType::RParen) &&
               !check(TokenType::EndOfFile)) {
          argType += advance().Text;
        }
      }
      ExternDecl::Arg arg;
      arg.Name = argName.Text;
      arg.Type = argType;
      arg.HasPointer = hasPointer;
      arg.IsMutable = argName.HasWrite;
      arg.IsNullable = argName.HasNull;
      args.push_back(std::move(arg));
    } while (match(TokenType::Comma));
  }
  if (match(TokenType::DotDotDot)) {
    isVariadic = true;
  }
  consume(TokenType::RParen, "Expected ')'");

  std::string retType = "void";
  if (match(TokenType::Arrow)) {
    retType = "";
    while (!isEndOfStatement() && !check(TokenType::EndOfFile)) {
      retType += advance().Text;
    }
  }
  expectEndOfStatement();

  auto node = std::make_unique<ExternDecl>(name.Text, std::move(args), retType);
  node->setLocation(name, m_CurrentFile);
  node->IsVariadic = isVariadic;
  return node;
}

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

std::unique_ptr<ImportDecl> Parser::parseImport(bool isPub) {
  Token importTok = consume(TokenType::KwImport, "Expected 'import'");
  std::string physicalPath;

  // 1. Parse Physical Path (Segments)
  while (true) {
    if (peek().HasNewlineBefore)
      break;

    if (check(TokenType::KwAs))
      break;

    bool consumed = false;
    if (check(TokenType::Identifier) || (peek().Kind >= TokenType::KwLet &&
                                         peek().Kind <= TokenType::KwCrate)) {
      physicalPath += advance().Text;
      consumed = true;
    } else if (match(TokenType::Dot)) {
      physicalPath += ".";
      consumed = true;
    } else if (match(TokenType::Slash)) {
      physicalPath += "/";
      consumed = true;
    }

    if (!consumed)
      break;
  }

  std::vector<ImportItem> items;
  std::string moduleAlias;

  // 2. Parse Logical Items (::)
  // Check for :: (colon colon)
  if (check(TokenType::Colon) && checkAt(1, TokenType::Colon)) {
    advance(); // :
    advance(); // :

    if (match(TokenType::Star)) {
      items.push_back({"*", ""});
    } else if (match(TokenType::LBrace)) {
      while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
        std::string symName;
        if (match(TokenType::At)) {
          // Consume @ but don't include in name for lookup
        }
        symName += consume(TokenType::Identifier, "Expected symbol name").Text;
        std::string alias;
        if (match(TokenType::KwAs)) {
          alias = consume(TokenType::Identifier, "Expected alias").Text;
        }
        items.push_back({symName, alias});
        if (!match(TokenType::Comma))
          break;
      }
      consume(TokenType::RBrace, "Expected '}'");
    } else {
      std::string symName;
      if (match(TokenType::At)) {
        // Consume @ but don't include in name for lookup
      }
      symName += consume(TokenType::Identifier, "Expected symbol name").Text;
      std::string alias;
      if (match(TokenType::KwAs)) {
        alias = consume(TokenType::Identifier, "Expected alias").Text;
      }
      items.push_back({symName, alias});
    }
  } else {
    // 3. Optional Module Alias (only if no logical items were parsed)
    if (match(TokenType::KwAs)) {
      moduleAlias =
          consume(TokenType::Identifier, "Expected module alias").Text;
    }
  }

  // Special handling: 'import ... :: *' ends with *, which isEndOfStatement
  // thinks is a binary op. We manually allow newline or semicolon here to avoid
  // that check.
  if (peek().HasNewlineBefore || check(TokenType::Semicolon) ||
      check(TokenType::EndOfFile) || check(TokenType::RBrace)) {
    match(TokenType::Semicolon);
  } else {
    expectEndOfStatement();
  }

  auto decl =
      std::make_unique<ImportDecl>(isPub, physicalPath, moduleAlias, items);
  decl->setLocation(importTok, m_CurrentFile);
  return decl;
}

std::unique_ptr<TypeAliasDecl> Parser::parseTypeAliasDecl(bool isPub) {
  bool isStrong = false;
  if (match(TokenType::KwType)) {
    isStrong = true;
  } else {
    consume(TokenType::KwAlias, "Expected 'alias' or 'type'");
  }
  Token name = consume(TokenType::Identifier, "Expected type alias name");
  consume(TokenType::Equal, "Expected '='");

  std::string targetType = "";
  int depth = 0;
  while ((depth > 0 || !isEndOfStatement()) && !check(TokenType::EndOfFile)) {
    Token t = advance();
    targetType += t.Text;
    if (t.Kind == TokenType::LBracket || t.Kind == TokenType::LParen)
      depth++;
    else if (t.Kind == TokenType::RBracket || t.Kind == TokenType::RParen)
      depth--;
  }
  expectEndOfStatement();

  auto decl =
      std::make_unique<TypeAliasDecl>(isPub, name.Text, targetType, isStrong);
  decl->setLocation(name, m_CurrentFile);
  return decl;
}

std::unique_ptr<Stmt> Parser::parseDeleteStmt() {
  Token kw = consume(TokenType::KwDelete, "Expected 'del' or 'delete'");
  auto expr = parseExpr();
  expectEndOfStatement();
  auto node = std::make_unique<DeleteStmt>(std::move(expr));
  node->setLocation(kw, m_CurrentFile);
  return node;
}

std::unique_ptr<ImplDecl> Parser::parseImpl() {
  consume(TokenType::KwImpl, "Expected 'impl'");
  Token firstIdent = consume(TokenType::Identifier, "Expected identifier");

  std::string traitName;
  std::string typeName;

  if (match(TokenType::At)) {
    // impl Type@Trait
    typeName = firstIdent.Text;
    Token traitToken =
        consume(TokenType::Identifier, "Expected trait name after '@'");
    traitName = traitToken.Text;
  } else if (match(TokenType::KwFor)) {
    // impl Trait for Type
    traitName = firstIdent.Text;
    typeName =
        consume(TokenType::Identifier, "Expected type name after 'for'").Text;
  } else {
    // impl Type
    typeName = firstIdent.Text;
  }

  consume(TokenType::LBrace, "Expected '{'");

  std::vector<std::unique_ptr<FunctionDecl>> methods;
  std::vector<EncapEntry> encapEntries;

  if (traitName == "encap") {
    while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
      if ((check(TokenType::KwFn)) ||
          (check(TokenType::KwPub) && checkAt(1, TokenType::KwFn))) {
        // Parse as function
        bool isPub = false;
        if (match(TokenType::KwPub)) {
          isPub = true;
        }
        methods.push_back(parseFunctionDecl(isPub));
      } else if (match(TokenType::KwPub)) {
        EncapEntry entry;
        entry.Level = EncapEntry::Global;

        if (match(TokenType::LParen)) {
          if (match(TokenType::KwCrate)) {
            entry.Level = EncapEntry::Crate;
          } else {
            entry.Level = EncapEntry::Path;
            // Parse targeted path (simple logic for now)
            while (check(TokenType::Identifier) || check(TokenType::Slash) ||
                   check(TokenType::Colon)) {
              if (match(TokenType::Slash)) {
                entry.TargetPath += "/";
              } else if (match(TokenType::Colon)) {
                entry.TargetPath += ":";
              } else {
                entry.TargetPath += advance().Text;
              }
            }
          }
          consume(TokenType::RParen, "Expected ')'");
        }

        if (match(TokenType::Star)) {
          entry.IsExclusion = true;
          match(TokenType::Bang); // Optional !
          while (check(TokenType::Identifier)) {
            entry.Fields.push_back(advance().Text);
            if (!match(TokenType::Comma))
              break;
          }
        } else {
          // One or more fields
          while (check(TokenType::Identifier)) {
            entry.Fields.push_back(advance().Text);
            if (!match(TokenType::Comma))
              break;
          }
        }
        encapEntries.push_back(std::move(entry));
      } else if (check(TokenType::KwFn)) {
        // Non-pub function (private to trait impl?)
        methods.push_back(parseFunctionDecl(false));
      } else {
        error(peek(), "Expected 'pub' or 'fn' inside @encap block");
        advance();
      }
    }
  } else {
    while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
      bool isPub = false;
      if (match(TokenType::KwPub)) {
        isPub = true;
      }

      if (check(TokenType::KwFn)) {
        methods.push_back(parseFunctionDecl(isPub));
      } else {
        error(peek(), "Expected method in impl block");
        advance();
      }
    }
  }
  consume(TokenType::RBrace, "Expected '}'");

  auto decl =
      std::make_unique<ImplDecl>(typeName, std::move(methods), traitName);
  decl->EncapEntries = std::move(encapEntries);
  decl->setLocation(firstIdent, m_CurrentFile);
  return decl;
}

std::unique_ptr<TraitDecl> Parser::parseTrait(bool isPub) {
  consume(TokenType::KwTrait, "Expected 'trait'");
  if (match(TokenType::At)) {
    // Optional @ prefix
  }
  Token name = consume(TokenType::Identifier, "Expected trait name");
  consume(TokenType::LBrace, "Expected '{'");

  std::vector<std::unique_ptr<FunctionDecl>> methods;
  while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
    bool isPub = false;
    if (match(TokenType::KwPub)) {
      isPub = true;
    }
    if (check(TokenType::KwFn)) {
      methods.push_back(parseFunctionDecl(isPub));
    } else {
      error(peek(), "Expected method prototype in trait");
    }
  }
  consume(TokenType::RBrace, "Expected '}'");
  return std::make_unique<TraitDecl>(isPub, name.Text, std::move(methods));
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

std::unique_ptr<Expr> Parser::parseUnsafeExpr() {
  Token tok = previous(); // assume KwUnsafe matched
  auto expr = parseExpr();
  auto node = std::make_unique<UnsafeExpr>(std::move(expr));
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

std::unique_ptr<Expr> Parser::parseAllocExpr() {
  Token tok = previous(); // assume KwAlloc matched
  bool isArray = false;
  std::unique_ptr<Expr> arraySize = nullptr;

  if (match(TokenType::LBracket)) {
    isArray = true;
    arraySize = parseExpr();
    consume(TokenType::RBracket, "Expected ']'");
  }

  Token typeTok =
      consume(TokenType::Identifier, "Expected type name after 'alloc'");
  std::string typeName = typeTok.Text;

  std::unique_ptr<Expr> init = nullptr;
  if (match(TokenType::LParen)) {
    // Check if it's named field initialization: Hero(id = 1, hp = 2)
    if (check(TokenType::Identifier) && checkAt(1, TokenType::Equal)) {
      std::vector<std::pair<std::string, std::unique_ptr<Expr>>> fields;
      while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
        Token fieldName = consume(TokenType::Identifier, "Expected field name");
        consume(TokenType::Equal, "Expected '=' after field name");
        fields.push_back({fieldName.Text, parseExpr()});
        match(TokenType::Comma);
      }
      init = std::make_unique<InitStructExpr>(typeName, std::move(fields));
    } else if (!check(TokenType::RParen)) {
      // Positional args
      std::vector<std::unique_ptr<Expr>> args;
      do {
        args.push_back(parseExpr());
      } while (match(TokenType::Comma));
      init = std::make_unique<CallExpr>(typeName, std::move(args));
    }
    consume(TokenType::RParen, "Expected ')'");
  }

  auto node = std::make_unique<AllocExpr>(typeName, std::move(init), isArray,
                                          std::move(arraySize));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

} // namespace toka
