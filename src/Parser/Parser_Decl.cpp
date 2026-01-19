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
#include <algorithm>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace toka {

std::unique_ptr<ShapeDecl> Parser::parseShape(bool isPub) {
  match(TokenType::KwShape); // Optional
  match(TokenType::KwPacked);
  bool packed = previous().Kind == TokenType::KwPacked;
  if (packed)
    consume(TokenType::KwShape, "Expected 'shape' after 'packed'");

  Token name = consume(TokenType::Identifier, "Expected shape name");

  // Parse Generic Parameters: Name<T, U> or Name<T, N_: usize>
  std::vector<GenericParam> genericParams;
  if (match(TokenType::GenericLT)) {
    do {
      GenericParam gp;
      gp.Name =
          consume(TokenType::Identifier, "Expected generic parameter name")
              .Text;
      if (match(TokenType::Colon)) {
        gp.Type = advance().Text; // e.g. usize
        gp.IsConst = true;
      }
      genericParams.push_back(gp);
    } while (match(TokenType::Comma));
    consume(TokenType::Greater, "Expected '>' to close generic parameters");
  }

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
        m.Type += parseTypeString();
        members.push_back(std::move(m));
        if (!check(TokenType::RParen))
          match(TokenType::Comma);
      }
    }
    match(TokenType::RParen);
  } else {
    error(peek(), "Expected '(' or '[' after shape name");
  }

  auto decl = std::make_unique<ShapeDecl>(isPub, name.Text, genericParams, kind,
                                          std::move(members), packed);
  decl->ArraySize = arraySize;
  // decl->FileName = m_CurrentFile;
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

  // Parse Generic Parameters: <T, N_: usize>
  std::vector<GenericParam> genericParams;
  if (match(TokenType::GenericLT)) {
    do {
      GenericParam gp;
      gp.Name =
          consume(TokenType::Identifier, "Expected generic parameter name")
              .Text;
      if (match(TokenType::Colon)) {
        gp.Type = parseTypeString();
        gp.IsConst = true;
      }
      genericParams.push_back(gp);
    } while (match(TokenType::Comma));
    consume(TokenType::Greater, "Expected '>' to close generic parameters");
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
          // arg.IsMutable = true; // Deprecated
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
        argType = parseTypeString();
      }
      FunctionDecl::Arg arg;
      arg.Name = argName.Text;
      arg.Type = argType;
      arg.HasPointer = hasPointer;
      arg.IsReference = isRef;
      // arg.IsMutable = argName.HasWrite; // Deprecated
      // arg.IsNullable = argName.HasNull; // Deprecated

      // New Permissions
      arg.IsUnique = isUnique;
      arg.IsShared = isShared;
      arg.IsRebindable = isRebindable; // Captured from token
      arg.IsPointerNullable = isPtrNullable;
      arg.IsValueMutable = argName.HasWrite;
      arg.IsValueNullable = argName.HasNull;

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
    retType = parseTypeString();
  }

  std::unique_ptr<BlockStmt> body = nullptr;
  if (check(TokenType::LBrace)) {
    body = parseBlock();
  } else {
    expectEndOfStatement();
  }
  auto decl = std::make_unique<FunctionDecl>(
      isPub, name.Text, args, std::move(body), retType, genericParams);
  decl->IsVariadic = isVariadic;
  decl->setLocation(name, m_CurrentFile);
  return decl;
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
        argType = parseTypeString();
      }
      ExternDecl::Arg arg;
      arg.Name = argName.Text;
      arg.Type = argType;
      arg.HasPointer = hasPointer;
      arg.IsValueMutable = argName.HasWrite;
      arg.IsValueNullable = argName.HasNull;
      args.push_back(std::move(arg));
    } while (match(TokenType::Comma));
  }
  if (match(TokenType::DotDotDot)) {
    isVariadic = true;
  }
  consume(TokenType::RParen, "Expected ')'");

  std::string retType = "void";
  if (match(TokenType::Arrow)) {
    retType = parseTypeString();
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

  std::string targetType = parseTypeString();
  expectEndOfStatement();

  auto decl =
      std::make_unique<TypeAliasDecl>(isPub, name.Text, targetType, isStrong);
  decl->setLocation(name, m_CurrentFile);
  return decl;
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

} // namespace toka
