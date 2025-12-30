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

bool Parser::check(TokenType type) const {
  if (peek().Kind == TokenType::EndOfFile)
    return false;
  return peek().Kind == type;
}

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

void Parser::error(const Token &tok, const std::string &message) {
  std::cerr << m_CurrentFile << ":" << tok.Line << ":" << tok.Column
            << ": error: " << message << "\n";
  std::exit(1);
}

std::unique_ptr<Module> Parser::parseModule() {
  auto module = std::make_unique<Module>();

  while (peek().Kind != TokenType::EndOfFile) {
    if (check(TokenType::KwImport)) {
      module->Imports.push_back(parseImport());
    } else if (check(TokenType::KwFn)) {
      module->Functions.push_back(parseFunctionDecl());
    } else if (check(TokenType::KwLet)) {
      module->Globals.push_back(parseVariableDecl());
    } else if (check(TokenType::KwType)) {
      module->TypeAliases.push_back(parseTypeAliasDecl());
    } else if (check(TokenType::KwExtern)) {
      module->Externs.push_back(parseExternDecl());
    } else if (check(TokenType::KwImpl)) {
      module->Impls.push_back(parseImpl());
    } else if (check(TokenType::KwTrait)) {
      module->Traits.push_back(parseTrait());
    } else if (check(TokenType::KwStruct)) {
      module->Structs.push_back(parseStruct());
    } else if (check(TokenType::KwOption)) {
      module->Options.push_back(parseOptionDecl());
    } else if (match(TokenType::Identifier) &&
               peek().Text ==
                   "=") { // This condition seems to be for a different struct
                          // syntax, but parseStruct() expects KwStruct. Keeping
                          // it as per instruction, but it might cause issues.
      module->Structs.push_back(parseStruct());
    } else {
      // Error or unknown
      std::cerr << "Unexpected Top Level Token: " << peek().toString() << "\n";
      advance();
    }
  }
  return module;
}

std::unique_ptr<StructDecl> Parser::parseStruct() {
  consume(TokenType::KwStruct, "Expected 'struct'");
  Token name = consume(TokenType::Identifier, "Expected struct name");
  consume(TokenType::LBrace, "Expected '{'");

  std::vector<StructField> fields;
  while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
    Token fieldName = consume(TokenType::Identifier, "Expected field name");
    consume(TokenType::Colon, "Expected ':' after field name");

    bool hasPointer = match(TokenType::Caret);
    std::string typeName = "";
    while (!check(TokenType::Comma) && !check(TokenType::RBrace) &&
           !check(TokenType::EndOfFile)) {
      // For now simple type name parsing until we see comma or brace
      // This is a bit loose but works for basic types
      typeName += advance().Text;
    }
    match(TokenType::Comma); // optional comma

    fields.push_back({fieldName.Text, typeName, hasPointer});
  }
  consume(TokenType::RBrace, "Expected '}'");

  auto node = std::make_unique<StructDecl>(name.Text, std::move(fields));
  node->setLocation(name, m_CurrentFile);
  return node;
}

std::unique_ptr<OptionDecl> Parser::parseOptionDecl() {
  consume(TokenType::KwOption, "Expected 'option'");
  Token name = consume(TokenType::Identifier, "Expected option name");
  consume(TokenType::LBrace, "Expected '{' after option name");

  std::vector<OptionVariant> variants;
  while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
    Token varName = consume(TokenType::Identifier, "Expected variant name");
    consume(TokenType::Equal, "Expected '=' after variant name");
    consume(TokenType::LParen, "Expected '(' for variant fields");

    std::vector<VariantField> fields;
    while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
      VariantField field;
      if (check(TokenType::Identifier) && checkAt(1, TokenType::Colon)) {
        field.Name = advance().Text;
        consume(TokenType::Colon, "Expected ':' after field name");
      }

      // Handle decorators
      if (match(TokenType::Star))
        field.HasPointer = true;
      else if (match(TokenType::Caret))
        field.IsUnique = true;
      else if (match(TokenType::Tilde))
        field.IsShared = true;
      else if (match(TokenType::Ampersand))
        field.IsReference = true;

      field.Type = consume(TokenType::Identifier, "Expected field type").Text;
      fields.push_back(field);

      if (!check(TokenType::RParen)) {
        consume(TokenType::Comma, "Expected ',' between fields");
      }
    }
    consume(TokenType::RParen, "Expected ')' after variant fields");
    variants.push_back({varName.Text, std::move(fields)});

    if (!check(TokenType::RBrace)) {
      match(TokenType::Comma); // Optional comma between variants
    }
  }
  consume(TokenType::RBrace, "Expected '}' after variants");

  auto decl = std::make_unique<OptionDecl>(name.Text, std::move(variants));
  decl->setLocation(name, m_CurrentFile);
  return decl;
}

std::unique_ptr<Stmt> Parser::parseMatchStmt() {
  consume(TokenType::KwMatch, "Expected 'match'");
  auto target = parseExpr();
  consume(TokenType::LBrace, "Expected '{' after match expression");

  std::vector<MatchCase> cases;

  while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
    MatchCase c;

    if (match(TokenType::KwLet)) {
      // let Variant(a, b)
      Token varName = consume(TokenType::Identifier, "Expected variant name");
      c.VariantName = varName.Text;
      if (match(TokenType::LParen)) {
        // Positional binding
        while (!check(TokenType::RParen) && !check(TokenType::EndOfFile)) {
          c.BindingNames.push_back(
              consume(TokenType::Identifier, "Expected binding name").Text);
          if (!check(TokenType::RParen))
            consume(TokenType::Comma, "Expected ','");
        }
        consume(TokenType::RParen, "Expected ')'");
      }
    } else if (match(TokenType::KwDefault)) {
      c.IsWildcard = true;
    } else if (check(TokenType::Identifier)) {
      if (peek().Text == "_") {
        advance();
        c.IsWildcard = true;
      } else {
        c.VariantName = advance().Text;
      }
    } else {
      // Stop if we don't see a pattern start (likely RBrace or error)
      if (check(TokenType::RBrace))
        break;
      error(peek(), "Expected pattern in match case");
    }

    if (match(TokenType::KwIf)) {
      c.Guard = parseExpr();
    }

    consume(TokenType::FatArrow, "Expected '=>'");
    c.Body = parseStmt();
    cases.push_back(std::move(c));
  }

  consume(TokenType::RBrace, "Expected '}'");
  return std::make_unique<MatchStmt>(std::move(target), std::move(cases));
}

std::unique_ptr<FunctionDecl> Parser::parseFunctionDecl() {
  consume(TokenType::KwFn, "Expected 'fn'");
  Token name;
  if (check(TokenType::KwMain)) {
    name = advance();                  // Consume main
    name.Kind = TokenType::Identifier; // Treat as identifier
  } else {
    name = consume(TokenType::Identifier, "Expected function name");
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
        arg.HasPointer = true;
        args.push_back(arg);
        firstArg = false;
        continue;
      }
      firstArg = false;

      bool isRef = match(TokenType::Ampersand);
      bool hasPointer = false;
      bool isUnique = false;
      bool isShared = false;
      if (match(TokenType::Caret)) {
        isUnique = true;
      } else if (match(TokenType::Star)) {
        hasPointer = true;
      } else if (match(TokenType::Tilde)) {
        isShared = true;
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
    retType = advance().Text;
  }

  std::unique_ptr<BlockStmt> body = nullptr;
  if (check(TokenType::LBrace)) {
    body = parseBlock();
  } else {
    match(TokenType::Semicolon);
  }
  auto decl =
      std::make_unique<FunctionDecl>(name.Text, args, std::move(body), retType);
  decl->IsVariadic = isVariadic;
  return decl;
}

std::unique_ptr<Stmt> Parser::parseVariableDecl() {
  consume(TokenType::KwLet, "Expected 'let'");
  bool isRef = match(TokenType::Ampersand);
  bool hasPointer = false;
  bool isUnique = false;
  bool isShared = false;
  if (match(TokenType::Caret)) {
    isUnique = true;
  } else if (match(TokenType::Star)) {
    hasPointer = true;
  } else if (match(TokenType::Tilde)) {
    isShared = true;
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
    consume(TokenType::Semicolon, "Expected ';' after destructuring");
    auto node = std::make_unique<DestructuringDecl>(
        typeName.Text, std::move(vars), std::move(init));
    node->setLocation(typeName, m_CurrentFile);
    return node;
  }

  Token name = consume(TokenType::Identifier, "Expected variable name");

  std::string typeName = "";
  if (match(TokenType::Colon)) {
    while (!check(TokenType::Semicolon) && !check(TokenType::Equal) &&
           !check(TokenType::EndOfFile) && !check(TokenType::Comma) &&
           !check(TokenType::RParen)) {
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
  node->IsMutable = name.HasWrite;
  node->IsNullable = name.HasNull;
  node->TypeName = typeName;

  consume(TokenType::Semicolon, "Expected ';' after variable declaration");
  return node;
}

std::unique_ptr<Stmt> Parser::parseStmt() {
  if (check(TokenType::LBrace))
    return parseBlock();
  if (check(TokenType::KwIf))
    return parseIf();
  if (check(TokenType::KwMatch))
    return parseMatchStmt();
  if (check(TokenType::KwWhile))
    return parseWhile();
  if (check(TokenType::KwReturn))
    return parseReturn();
  if (check(TokenType::KwLet))
    return parseVariableDecl();
  if (check(TokenType::KwDelete))
    return parseDeleteStmt();

  // ExprStmt
  auto expr = parseExpr();
  if (expr) {
    consume(TokenType::Semicolon, "Expected ';' after expression statement");
    return std::make_unique<ExprStmt>(std::move(expr));
  }
  return nullptr;
}

std::unique_ptr<BlockStmt> Parser::parseBlock() {
  consume(TokenType::LBrace, "Expected '{'");
  auto block = std::make_unique<BlockStmt>();

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
  if (!check(TokenType::Semicolon)) {
    val = parseExpr();
  }
  consume(TokenType::Semicolon, "Expected ';' after return value");
  auto node = std::make_unique<ReturnStmt>(std::move(val));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

int Parser::getPrecedence(TokenType type) {
  switch (type) {
  case TokenType::Plus:
  case TokenType::Minus:
    return 10;
  case TokenType::Star:
  case TokenType::Slash:
    return 20;
  case TokenType::Equal:
  case TokenType::PlusEqual:
  case TokenType::MinusEqual:
  case TokenType::StarEqual:
  case TokenType::SlashEqual:
    return 1; // Assignment
  case TokenType::DoubleEqual:
  case TokenType::Neq:
  case TokenType::Less:
  case TokenType::Greater:
    return 5;
  default:
    return -1;
  }
}

std::unique_ptr<Expr> Parser::parseExpr(int minPrec) {
  auto lhs = parsePrimary();
  if (!lhs)
    return nullptr;

  while (true) {
    if (check(TokenType::Colon)) {
      advance(); // consume ':'
      std::string typeName = "";
      while (!check(TokenType::Comma) && !check(TokenType::RParen) &&
             !check(TokenType::Semicolon) && !check(TokenType::Plus) &&
             !check(TokenType::Minus) && !check(TokenType::Star) &&
             !check(TokenType::Slash) && !check(TokenType::Equal) &&
             !check(TokenType::DoubleEqual) && !check(TokenType::Neq) &&
             !check(TokenType::Less) && !check(TokenType::Greater) &&
             !check(TokenType::EndOfFile)) {
        typeName += advance().Text;
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
  if (match(TokenType::Ampersand)) {
    Token tok = previous();
    auto sub = parsePrimary();
    auto node = std::make_unique<AddressOfExpr>(std::move(sub));
    node->setLocation(tok, m_CurrentFile);
    return node;
  }
  if (match(TokenType::Star)) {
    Token tok = previous();
    auto sub = parsePrimary();
    auto node = std::make_unique<DereferenceExpr>(std::move(sub));
    node->setLocation(tok, m_CurrentFile);
    return node;
  }
  if (match(TokenType::Bang) || match(TokenType::Minus) ||
      match(TokenType::PlusPlus) || match(TokenType::MinusMinus) ||
      match(TokenType::Caret) || match(TokenType::Tilde)) {
    Token tok = previous();
    TokenType op = tok.Kind;
    auto sub = parsePrimary();
    auto node = std::make_unique<UnaryExpr>(op, std::move(sub));
    node->setLocation(tok, m_CurrentFile);
    return node;
  }

  if (match(TokenType::Integer)) {
    Token tok = previous();
    auto node = std::make_unique<NumberExpr>(std::stoll(tok.Text));
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
  } else if (match(TokenType::String)) {
    Token tok = previous();
    auto node = std::make_unique<StringExpr>(tok.Text);
    node->setLocation(tok, m_CurrentFile);
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
  } else if (match(TokenType::KwSelf)) {
    Token tok = previous();
    auto node = std::make_unique<VariableExpr>("self");
    node->setLocation(tok, m_CurrentFile);
    expr = std::move(node);
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
    } else {
      error(kw, "Expected '{' initializer for new expression");
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
    consume(TokenType::LParen, "Expected '('");
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
        name.Text += "::" + member.Text;

        if (match(TokenType::LParen)) {
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
          return expr; // Early return as scope resolution usually ends a
                       // primary
        }
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
      if (match(TokenType::Identifier)) {
        std::string memberName = previous().Text;
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
      } else if (match(TokenType::Integer)) {
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
      std::unique_ptr<Expr> index = parseExpr();
      consume(TokenType::RBracket, "Expected ']' after index");
      expr =
          std::make_unique<ArrayIndexExpr>(std::move(expr), std::move(index));
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
  Token name =
      consume(TokenType::Identifier, "Expected external function name");
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
    while (!check(TokenType::Semicolon) && !check(TokenType::EndOfFile)) {
      retType += advance().Text;
    }
  }
  consume(TokenType::Semicolon, "Expected ';' after extern declaration");

  auto node = std::make_unique<ExternDecl>(name.Text, std::move(args), retType);
  node->setLocation(name, m_CurrentFile);
  node->IsVariadic = isVariadic;
  return node;
}

std::unique_ptr<Stmt> Parser::parseIf() {
  Token tok = consume(TokenType::KwIf, "Expected 'if'");
  consume(TokenType::LParen, "Expected '('");
  auto cond = parseExpr();
  consume(TokenType::RParen, "Expected ')'");
  auto thenStmt = parseStmt();
  std::unique_ptr<Stmt> elseStmt;
  if (match(TokenType::KwElse)) {
    elseStmt = parseStmt();
  }
  auto node = std::make_unique<IfStmt>(std::move(cond), std::move(thenStmt),
                                       std::move(elseStmt));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<Stmt> Parser::parseWhile() {
  Token tok = consume(TokenType::KwWhile, "Expected 'while'");
  consume(TokenType::LParen, "Expected '('");
  auto cond = parseExpr();
  consume(TokenType::RParen, "Expected ')'");
  auto body = parseStmt();
  auto node = std::make_unique<WhileStmt>(std::move(cond), std::move(body));
  node->setLocation(tok, m_CurrentFile);
  return node;
}

std::unique_ptr<ImportDecl> Parser::parseImport() {
  consume(TokenType::KwImport, "Expected 'import'");
  Token path = consume(TokenType::String, "Expected string path for import");
  consume(TokenType::Semicolon, "Expected ';' after import");
  return std::make_unique<ImportDecl>(path.Text);
}

std::unique_ptr<TypeAliasDecl> Parser::parseTypeAliasDecl() {
  consume(TokenType::KwType, "Expected 'type'");
  Token name = consume(TokenType::Identifier, "Expected type alias name");
  consume(TokenType::Equal, "Expected '='");

  std::string targetType = "";
  int depth = 0;
  while ((depth > 0 || !check(TokenType::Semicolon)) &&
         !check(TokenType::EndOfFile)) {
    Token t = advance();
    targetType += t.Text;
    if (t.Kind == TokenType::LBracket || t.Kind == TokenType::LParen)
      depth++;
    else if (t.Kind == TokenType::RBracket || t.Kind == TokenType::RParen)
      depth--;
  }
  match(TokenType::Semicolon);

  auto node = std::make_unique<TypeAliasDecl>(name.Text, targetType);
  node->setLocation(name, m_CurrentFile);
  return node;
}

std::unique_ptr<Stmt> Parser::parseDeleteStmt() {
  Token kw = consume(TokenType::KwDelete, "Expected 'del' or 'delete'");
  auto expr = parseExpr();
  consume(TokenType::Semicolon, "Expected ';' after delete statement");
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
  while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
    if (check(TokenType::KwFn)) {
      methods.push_back(parseFunctionDecl());
    } else {
      error(peek(), "Expected method in impl block");
    }
  }
  consume(TokenType::RBrace, "Expected '}'");
  auto decl =
      std::make_unique<ImplDecl>(typeName, std::move(methods), traitName);
  decl->setLocation(firstIdent, m_CurrentFile);
  return decl;
}

std::unique_ptr<TraitDecl> Parser::parseTrait() {
  consume(TokenType::KwTrait, "Expected 'trait'");
  if (match(TokenType::At)) {
    // Optional @ prefix
  }
  Token name = consume(TokenType::Identifier, "Expected trait name");
  consume(TokenType::LBrace, "Expected '{'");

  std::vector<std::unique_ptr<FunctionDecl>> methods;
  while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
    if (check(TokenType::KwFn)) {
      methods.push_back(parseFunctionDecl());
    } else {
      error(peek(), "Expected method prototype in trait");
    }
  }
  consume(TokenType::RBrace, "Expected '}'");
  return std::make_unique<TraitDecl>(name.Text, std::move(methods));
}

} // namespace toka
