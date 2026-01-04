#pragma once

#include <cstdint>
#include <iostream>
#include <string>
#include <string_view>

namespace toka {

enum class TokenType {
  // End of file
  EndOfFile,

  // Identifiers & Literals
  Identifier,
  Integer,
  Float,
  String,

  // Keywords
  KwLet,
  KwAuto,
  KwMut,
  KwType,
  KwConst,
  KwShape,
  KwPacked,
  KwTrait,
  KwImpl,
  KwFn,
  KwNew,
  KwDyn,
  KwMove,
  KwWhere,
  KwDefault,
  KwDelete,
  KwFinal,
  KwFnType, // Fn

  KwIf,
  KwElse,
  KwMatch,
  KwCase,
  KwFor,
  KwWhile,
  KwBreak,
  KwContinue,
  KwReturn,
  KwYield,
  KwLoop,
  KwPass,
  KwTo,
  KwOr,

  KwTask,
  KwSuspend,
  KwAsync,
  KwCancel,
  KwAwait,
  KwChannel,
  KwImport,
  KwPub,
  KwAs,
  KwIs,
  KwIn,
  KwSelf,
  KwUpperSelf,
  KwTrue,
  KwFalse,
  KwNone,
  KwNull,
  KwDefer,
  KwMain,
  KwExtern,
  KwCrate,

  // Attribute Tokens (When separate, though usually parsed as part of Ident or
  // Type)
  // We treat attached tokens as properties of the Identifier token in the
  // Lexer,
  // but they might appear standalone or in types.
  TokenWrite,     // #
  TokenNull,      // ?
  TokenWriteNull, // !
  TokenNone,      // $

  // Operators & Symbols
  LParen,
  RParen, // ( )
  LBracket,
  RBracket, // [ ]
  LBrace,
  RBrace,     // { }
  Caret,      // ^
  Comma,      // ,
  Dot,        // .
  Colon,      // :
  Semicolon,  // ;
  Arrow,      // ->
  FatArrow,   // =>
  Dependency, // <-
  Pipe,       // |
  DotDotDot,  // ...
  Ampersand,  // &

  // Arithmetic/Logic (Basic set)
  Plus,
  Minus,
  Star,
  Slash,
  Equal,
  DoubleEqual,
  PlusEqual,
  MinusEqual,
  StarEqual,
  SlashEqual,
  Bang,
  Neq,
  Less,
  Greater,
  PlusPlus,
  MinusMinus,
  Tilde, // ~
  At,    // @
  And,   // &&
  Or     // ||
};

struct Token {
  TokenType Kind;
  std::string Text; // Or string_view if source is kept alive
  int Line;
  int Column;

  // Attribute flags for identifiers (e.g. if identifier is "x#")
  bool HasWrite = false;
  bool HasNull = false;
  bool IsSwappablePtr = false;   // "swappable" property for pointers
  bool HasNewlineBefore = false; // Support optional semicolons

  std::string toString() const {
    // Debug helper
    return "Token(" + std::to_string((int)Kind) + ", " + Text + ")";
  }
};

} // namespace toka
