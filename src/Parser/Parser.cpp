#include "toka/Parser.h"
#include <iostream>

namespace toka {

const Token &Parser::peek() const {
  if (m_Pos >= m_Tokens.size())
    return m_Tokens.back(); // EOF
  return m_Tokens[m_Pos];
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
  std::cerr << "Parser Error: " << message << " at " << peek().toString()
            << "\n";
  // For now simple error recovery: return EOF or garbage
  return peek();
}

std::unique_ptr<Module> Parser::parseModule() {
  auto module = std::make_unique<Module>();

  while (peek().Kind != TokenType::EndOfFile) {
    if (check(TokenType::KwFn)) {
      module->Functions.push_back(parseFunctionDecl());
    } else if (check(TokenType::KwLet)) {
      module->Globals.push_back(parseVariableDecl());
    } else if (check(TokenType::KwType)) {
      module->TypeAliases.push_back(parseTypeAliasDecl());
    } else if (check(TokenType::KwExtern)) {
      module->Externs.push_back(parseExternDecl());
    } else if (check(TokenType::KwImport)) {
      module->Imports.push_back(parseImport());
    } else if (check(TokenType::KwStruct)) {
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

  return std::make_unique<StructDecl>(name.Text, std::move(fields));
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
  if (!check(TokenType::RParen)) {
    do {
      if (check(TokenType::DotDotDot))
        break;
      bool hasPointer = match(TokenType::Caret);
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

  std::unique_ptr<BlockStmt> body = parseBlock();
  auto decl =
      std::make_unique<FunctionDecl>(name.Text, args, std::move(body), retType);
  decl->IsVariadic = isVariadic;
  return decl;
}

std::unique_ptr<VariableDecl> Parser::parseVariableDecl() {
  consume(TokenType::KwLet, "Expected 'let'");

  bool hasPointer = match(TokenType::Caret);
  Token name = consume(TokenType::Identifier, "Expected variable name");
  bool isMutable = name.HasWrite; // Check attribute token

  // Type annotation : Type
  std::string typeName = "";
  if (match(TokenType::Colon)) {
    int depth = 0;
    while ((depth > 0 ||
            (!check(TokenType::Equal) && !check(TokenType::Semicolon))) &&
           !check(TokenType::EndOfFile)) {
      Token t = advance();
      typeName += t.Text;
      if (t.Kind == TokenType::LBracket || t.Kind == TokenType::LParen)
        depth++;
      else if (t.Kind == TokenType::RBracket || t.Kind == TokenType::RParen)
        depth--;
    }
  }

  std::unique_ptr<Expr> init = nullptr;
  if (match(TokenType::Equal)) {
    init = parseExpr();
  }

  consume(TokenType::Semicolon, "Expected ';'");

  auto decl = std::make_unique<VariableDecl>(name.Text, std::move(init));
  decl->HasPointer = hasPointer;
  decl->IsMutable = isMutable;
  decl->IsNullable = name.HasNull;
  decl->TypeName = typeName;
  return decl;
}

std::unique_ptr<Stmt> Parser::parseStmt() {
  if (check(TokenType::LBrace))
    return parseBlock();
  if (match(TokenType::KwIf))
    return parseIf();
  if (match(TokenType::KwWhile))
    return parseWhile();
  if (match(TokenType::KwReturn))
    return parseReturn();
  if (check(TokenType::KwLet))
    return parseVariableDecl();

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
  std::unique_ptr<Expr> value = nullptr;
  if (!check(TokenType::Semicolon)) {
    value = parseExpr();
  }
  consume(TokenType::Semicolon, "Expected ';'");
  return std::make_unique<ReturnStmt>(std::move(value));
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
      lhs = std::make_unique<CastExpr>(std::move(lhs), typeName);
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

    lhs = std::make_unique<BinaryExpr>(op.Text, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

std::unique_ptr<Expr> Parser::parsePrimary() {
  std::unique_ptr<Expr> expr = nullptr;

  if (match(TokenType::Integer)) {
    expr = std::make_unique<NumberExpr>(std::stoll(previous().Text));
  } else if (match(TokenType::Float)) {
    expr = std::make_unique<FloatExpr>(std::stod(previous().Text));
  } else if (match(TokenType::KwTrue)) {
    expr = std::make_unique<BoolExpr>(true);
  } else if (match(TokenType::KwFalse)) {
    expr = std::make_unique<BoolExpr>(false);
  } else if (match(TokenType::String)) {
    expr = std::make_unique<StringExpr>(previous().Text);
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
  } else if (match(TokenType::LParen)) {
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
      expr = std::make_unique<TupleExpr>(std::move(elements));
    } else {
      expr = std::move(elements[0]);
    }
  } else {
    bool hasPointer = match(TokenType::Caret);
    if (match(TokenType::Identifier)) {
      Token name = previous();
      if (check(TokenType::LBrace)) {
        // Struct Init
        advance();
        std::vector<std::pair<std::string, std::unique_ptr<Expr>>> fields;
        while (!check(TokenType::RBrace) && !check(TokenType::EndOfFile)) {
          Token fieldName =
              consume(TokenType::Identifier, "Expected field name");
          consume(TokenType::Equal, "Expected '=' after field name");
          fields.push_back({fieldName.Text, parseExpr()});
          match(TokenType::Comma);
        }
        consume(TokenType::RBrace, "Expected '}'");
        expr = std::make_unique<InitStructExpr>(name.Text, std::move(fields));
      } else if (match(TokenType::LParen)) {
        // Function Call
        std::vector<std::unique_ptr<Expr>> args;
        if (!check(TokenType::RParen)) {
          do {
            args.push_back(parseExpr());
          } while (match(TokenType::Comma));
        }
        consume(TokenType::RParen, "Expected ')' after arguments");
        expr = std::make_unique<CallExpr>(name.Text, std::move(args));
      } else {
        auto var = std::make_unique<VariableExpr>(name.Text);
        var->HasPointer = hasPointer;
        var->IsMutable = name.HasWrite;
        var->IsNullable = name.HasNull;
        expr = std::move(var);
      }
    }
  }

  // Suffixes: .member, [index], etc.
  while (expr) {
    if (match(TokenType::Dot)) {
      if (match(TokenType::Identifier) || match(TokenType::Integer)) {
        expr = std::make_unique<MemberExpr>(std::move(expr), previous().Text);
      } else {
        std::cerr << "Parser Error: Expected member name or index after '.' at "
                  << peek().Text << "\n";
        std::exit(1);
      }
    } else if (match(TokenType::LBracket)) {
      std::unique_ptr<Expr> index = parseExpr();
      consume(TokenType::RBracket, "Expected ']' after index");
      expr =
          std::make_unique<ArrayIndexExpr>(std::move(expr), std::move(index));
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
      bool hasPointer = match(TokenType::Caret);
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
    retType = advance().Text;
  }
  consume(TokenType::Semicolon, "Expected ';'");
  auto decl = std::make_unique<ExternDecl>(name.Text, std::move(args), retType);
  decl->IsVariadic = isVariadic;
  return decl;
}

std::unique_ptr<Stmt> Parser::parseIf() {
  consume(TokenType::LParen, "Expected '(' after 'if'");
  auto cond = parseExpr();
  consume(TokenType::RParen, "Expected ')' after condition");

  auto thenStmt = parseStmt();
  std::unique_ptr<Stmt> elseStmt = nullptr;

  if (match(TokenType::KwElse)) {
    elseStmt = parseStmt();
  }

  return std::make_unique<IfStmt>(std::move(cond), std::move(thenStmt),
                                  std::move(elseStmt));
}

std::unique_ptr<Stmt> Parser::parseWhile() {
  consume(TokenType::LParen, "Expected '(' after 'while'");
  auto cond = parseExpr();
  consume(TokenType::RParen, "Expected ')' after condition");

  auto body = parseStmt();

  return std::make_unique<WhileStmt>(std::move(cond), std::move(body));
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

  std::string typeName = "";
  int depth = 0;
  while ((depth > 0 || !check(TokenType::Semicolon)) &&
         !check(TokenType::EndOfFile)) {
    Token t = advance();
    typeName += t.Text;
    if (t.Kind == TokenType::LBracket || t.Kind == TokenType::LParen)
      depth++;
    else if (t.Kind == TokenType::RBracket || t.Kind == TokenType::RParen)
      depth--;
  }
  consume(TokenType::Semicolon, "Expected ';'");
  return std::make_unique<TypeAliasDecl>(name.Text, typeName);
}

} // namespace toka
