// Auto-refactored split
#include "toka/Parser.h"
#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <algorithm>


namespace toka {


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