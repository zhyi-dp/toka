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


} // namespace toka
