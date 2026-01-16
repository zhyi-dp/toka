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
#pragma once

#include "toka/Token.h"
#include "toka/Type.h" // Added for ResolvedType
#include <llvm/IR/Value.h>
#include <memory>
#include <string>
#include <vector>

namespace toka {

class ASTNode {
public:
  int Line = 0;
  int Column = 0;
  std::string FileName;

  virtual ~ASTNode() = default;
  virtual std::string toString() const = 0;

  void setLocation(const Token &tok, const std::string &file = "") {
    Line = tok.Line;
    Column = tok.Column;
    FileName = file;
  }
};

class Expr : public ASTNode {
public:
  std::shared_ptr<Type> ResolvedType;
};
class Stmt : public ASTNode {};

// --- Expressions ---

class NumberExpr : public Expr {
public:
  uint64_t Value;
  NumberExpr(uint64_t val) : Value(val) {}
  std::string toString() const override {
    return "Number(" + std::to_string(Value) + ")";
  }
};

class FloatExpr : public Expr {
public:
  double Value;
  FloatExpr(double val) : Value(val) {}
  std::string toString() const override {
    return "Float(" + std::to_string(Value) + ")";
  }
};

class BoolExpr : public Expr {
public:
  bool Value;
  BoolExpr(bool val) : Value(val) {}
  std::string toString() const override { return Value ? "true" : "false"; }
};

class NullExpr : public Expr {
public:
  NullExpr() {}
  std::string toString() const override { return "nullptr"; }
};

class NoneExpr : public Expr {
public:
  NoneExpr() {}
  std::string toString() const override { return "none"; }
};

class VariableExpr : public Expr {
public:
  std::string Name;
  bool HasPointer = false;
  bool IsUnique = false;
  bool IsShared = false;
  bool IsMutable = false;
  bool IsNullable = false;

  VariableExpr(const std::string &name) : Name(name) {}
  std::string toString() const override {
    return std::string("Var(") + (HasPointer ? "^" : "") + Name +
           (IsMutable ? "#" : "") + ")";
  }
};

class StringExpr : public Expr {
public:
  std::string Value;
  StringExpr(const std::string &val) : Value(val) {}
  std::string toString() const override { return "String(\"" + Value + "\")"; }
};

class DereferenceExpr : public Expr {
public:
  std::unique_ptr<Expr> Expression;
  DereferenceExpr(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override {
    return std::string("Dereference(") + Expression->toString() + ")";
  }
};

class BinaryExpr : public Expr {
public:
  std::string Op;
  std::unique_ptr<Expr> LHS, RHS;
  BinaryExpr(const std::string &op, std::unique_ptr<Expr> lhs,
             std::unique_ptr<Expr> rhs)
      : Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
  std::string toString() const override {
    return "Binary(" + Op + ", " + LHS->toString() + ", " + RHS->toString() +
           ")";
  }
};

class UnaryExpr : public Expr {
public:
  TokenType Op;
  std::unique_ptr<Expr> RHS;
  bool HasNull = false;      // For ^? or *?
  bool IsRebindable = false; // For ^# or *#
  bool IsValueMutable =
      false; // For identifier# (unlikely in Unary op token but consistent)
  bool IsValueNullable = false; // For identifier?
  // Actually UnaryExpr covers ^, *, ~, etc.

  UnaryExpr(TokenType op, std::unique_ptr<Expr> rhs)
      : Op(op), RHS(std::move(rhs)) {}
  std::string toString() const override {
    return "Unary(" + std::to_string((int)Op) + (HasNull ? "?" : "") +
           (IsRebindable ? "#" : "") + ", " + RHS->toString() + ")";
  }
};

class PostfixExpr : public Expr {
public:
  TokenType Op;
  std::unique_ptr<Expr> LHS;
  PostfixExpr(TokenType op, std::unique_ptr<Expr> lhs)
      : Op(op), LHS(std::move(lhs)) {}
  std::string toString() const override {
    return "Postfix(" + std::to_string((int)Op) + ", " + LHS->toString() + ")";
  }
};

class CastExpr : public Expr {
public:
  std::unique_ptr<Expr> Expression;
  std::string TargetType;
  CastExpr(std::unique_ptr<Expr> expr, const std::string &type)
      : Expression(std::move(expr)), TargetType(type) {}
  std::string toString() const override {
    return "Cast(" + Expression->toString() + " as " + TargetType + ")";
  }
};

class AddressOfExpr : public Expr {
public:
  std::unique_ptr<Expr> Expression;
  AddressOfExpr(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override { return "&" + Expression->toString(); }
};

class MemberExpr : public Expr {
public:
  std::unique_ptr<Expr> Object;
  std::string Member;
  bool IsArrow;
  bool IsStatic;
  MemberExpr(std::unique_ptr<Expr> obj, const std::string &member,
             bool isArrow = false, bool isStatic = false)
      : Object(std::move(obj)), Member(member), IsArrow(isArrow),
        IsStatic(isStatic) {}
  std::string toString() const override {
    if (IsStatic)
      return Object->toString() + "::" + Member;
    return Object->toString() + (IsArrow ? "->" : ".") + Member;
  }
};

class ArrayIndexExpr : public Expr {
public:
  std::unique_ptr<Expr> Array;
  std::vector<std::unique_ptr<Expr>> Indices;

  ArrayIndexExpr(std::unique_ptr<Expr> arr,
                 std::vector<std::unique_ptr<Expr>> idxs)
      : Array(std::move(arr)), Indices(std::move(idxs)) {}
  std::string toString() const override {
    std::string s = Array->toString() + "[";
    for (size_t i = 0; i < Indices.size(); ++i) {
      if (i > 0)
        s += ", ";
      s += Indices[i]->toString();
    }
    s += "]";
    return s;
  }
};

class ArrayExpr : public Expr {
public:
  std::vector<std::unique_ptr<Expr>> Elements;
  ArrayExpr(std::vector<std::unique_ptr<Expr>> elems)
      : Elements(std::move(elems)) {}
  std::string toString() const override {
    std::string s = "[";
    for (size_t i = 0; i < Elements.size(); ++i) {
      if (i > 0)
        s += ", ";
      s += Elements[i]->toString();
    }
    s += "]";
    return s;
  }
};

class UnsafeExpr : public Expr {
public:
  std::unique_ptr<Expr> Expression;
  UnsafeExpr(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override {
    return "Unsafe(" + Expression->toString() + ")";
  }
};

class AllocExpr : public Expr {
public:
  std::string TypeName;
  std::unique_ptr<Expr> Initializer;
  bool IsArray = false;
  std::unique_ptr<Expr> ArraySize;

  AllocExpr(const std::string &type, std::unique_ptr<Expr> init = nullptr,
            bool isArray = false, std::unique_ptr<Expr> size = nullptr)
      : TypeName(type), Initializer(std::move(init)), IsArray(isArray),
        ArraySize(std::move(size)) {}

  std::string toString() const override {
    return std::string("Alloc(") + (IsArray ? "[]" : "") + TypeName + ")";
  }
};

class TupleExpr : public Expr {
public:
  std::vector<std::unique_ptr<Expr>> Elements;
  TupleExpr(std::vector<std::unique_ptr<Expr>> elems)
      : Elements(std::move(elems)) {}
  std::string toString() const override {
    std::string s = "(";
    for (size_t i = 0; i < Elements.size(); ++i) {
      if (i > 0)
        s += ", ";
      s += Elements[i]->toString();
    }
    s += ")";
    return s;
  }
};

class InitStructExpr : public Expr {
public:
  std::string ShapeName;
  std::vector<std::pair<std::string, std::unique_ptr<Expr>>> Members;
  InitStructExpr(
      const std::string &name,
      std::vector<std::pair<std::string, std::unique_ptr<Expr>>> members)
      : ShapeName(name), Members(std::move(members)) {}
  std::string toString() const override { return "Init(" + ShapeName + ")"; }
};

class AnonymousRecordExpr : public Expr {
public:
  std::vector<std::pair<std::string, std::unique_ptr<Expr>>> Fields;
  std::string AssignedTypeName; // Filled by Sema, used by CodeGen

  AnonymousRecordExpr(
      std::vector<std::pair<std::string, std::unique_ptr<Expr>>> fields)
      : Fields(std::move(fields)) {}

  std::string toString() const override {
    std::string s = "AnonRecord(";
    if (!AssignedTypeName.empty())
      s += "[" + AssignedTypeName + "] ";
    for (size_t i = 0; i < Fields.size(); ++i) {
      if (i > 0)
        s += ", ";
      s += Fields[i].first + "=" + Fields[i].second->toString();
    }
    s += ")";
    return s;
  }
};

class FunctionDecl;
class ExternDecl;
class ShapeDecl;

class CallExpr : public Expr {
public:
  std::string Callee;
  std::vector<std::unique_ptr<Expr>> Args;

  // Semantic Resolution Cache
  FunctionDecl *ResolvedFn = nullptr;
  ExternDecl *ResolvedExtern = nullptr;
  ShapeDecl *ResolvedShape = nullptr;

  CallExpr(const std::string &callee, std::vector<std::unique_ptr<Expr>> args)
      : Callee(callee), Args(std::move(args)) {}

  std::string toString() const override { return "Call(" + Callee + ")"; }
};

class MethodCallExpr : public Expr {
public:
  std::unique_ptr<Expr> Object;
  std::string Method;
  std::vector<std::unique_ptr<Expr>> Args;

  MethodCallExpr(std::unique_ptr<Expr> obj, const std::string &method,
                 std::vector<std::unique_ptr<Expr>> args)
      : Object(std::move(obj)), Method(method), Args(std::move(args)) {}

  std::string toString() const override { return "MethodCall(" + Method + ")"; }
};

class NewExpr : public Expr {
public:
  std::string Type;
  std::unique_ptr<Expr> Initializer;
  NewExpr(const std::string &type, std::unique_ptr<Expr> init)
      : Type(type), Initializer(std::move(init)) {}
  std::string toString() const override {
    return "New(" + Type + ", " + (Initializer ? Initializer->toString() : "") +
           ")";
  }
};

class PassExpr : public Expr {
public:
  std::unique_ptr<Expr> Value;
  PassExpr(std::unique_ptr<Expr> val) : Value(std::move(val)) {}
  std::string toString() const override {
    return "Pass(" + (Value ? Value->toString() : "none") + ")";
  }
};

class MatchArm {
public:
  struct Pattern : public ASTNode {
    enum Kind { Literal, Variable, Decons, Wildcard };
    Kind PatternKind;
    std::string Name;        // For Variable/Decons (e.g., "Maybe::One")
    uint64_t LiteralVal = 0; // For Literal
    bool IsReference = false;
    bool IsMutable = false;
    std::vector<std::unique_ptr<Pattern>> SubPatterns; // For Decons

    Pattern(Kind k) : PatternKind(k) {}
    std::string toString() const {
      switch (PatternKind) {
      case Literal:
        return std::to_string(LiteralVal);
      case Variable:
        return (IsReference ? "&" : "") + Name + (IsMutable ? "#" : "");
      case Decons: {
        std::string s = Name + "(";
        for (size_t i = 0; i < SubPatterns.size(); ++i) {
          if (i > 0)
            s += ", ";
          s += SubPatterns[i]->toString();
        }
        s += ")";
        return s;
      }
      case Wildcard:
        return "_";
      }
      return "";
    }
  };

  std::unique_ptr<Pattern> Pat;
  std::unique_ptr<Expr> Guard;
  std::unique_ptr<Stmt> Body;

  MatchArm(std::unique_ptr<Pattern> p, std::unique_ptr<Expr> g,
           std::unique_ptr<Stmt> b)
      : Pat(std::move(p)), Guard(std::move(g)), Body(std::move(b)) {}
};

class MatchExpr : public Expr {
public:
  std::unique_ptr<Expr> Target;
  std::vector<std::unique_ptr<MatchArm>> Arms;

  MatchExpr(std::unique_ptr<Expr> target,
            std::vector<std::unique_ptr<MatchArm>> arms)
      : Target(std::move(target)), Arms(std::move(arms)) {}

  std::string toString() const override { return "Match(...)"; }
};

class BreakExpr : public Expr {
public:
  std::string TargetLabel;
  std::unique_ptr<Expr> Value;
  BreakExpr(std::string label, std::unique_ptr<Expr> val)
      : TargetLabel(std::move(label)), Value(std::move(val)) {}
  std::string toString() const override { return "Break"; }
};

class ContinueExpr : public Expr {
public:
  std::string TargetLabel;
  ContinueExpr(std::string label) : TargetLabel(std::move(label)) {}
  std::string toString() const override { return "Continue"; }
};

// --- Statements ---

class BlockStmt : public Stmt {
public:
  std::vector<std::unique_ptr<Stmt>> Statements;
  BlockStmt() = default;
  BlockStmt(std::vector<std::unique_ptr<Stmt>> stmts)
      : Statements(std::move(stmts)) {}
  std::string toString() const override { return "Block"; }
};

class ReturnStmt : public Stmt {
public:
  std::unique_ptr<Expr> ReturnValue;
  ReturnStmt(std::unique_ptr<Expr> val) : ReturnValue(std::move(val)) {}
  std::string toString() const override { return "Return"; }
};

class ExprStmt : public Stmt {
public:
  std::unique_ptr<Expr> Expression;
  ExprStmt(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override { return "ExprStmt"; }
};

class DeleteStmt : public Stmt {
public:
  std::unique_ptr<Expr> Expression;
  DeleteStmt(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override { return "Delete"; }
};

class UnsafeStmt : public Stmt {
public:
  std::unique_ptr<Stmt> Statement;
  UnsafeStmt(std::unique_ptr<Stmt> stmt) : Statement(std::move(stmt)) {}
  std::string toString() const override { return "UnsafeStmt"; }
};

class FreeStmt : public Stmt {
public:
  std::unique_ptr<Expr> Expression;
  std::unique_ptr<Expr> Count;
  FreeStmt(std::unique_ptr<Expr> expr, std::unique_ptr<Expr> count = nullptr)
      : Expression(std::move(expr)), Count(std::move(count)) {}
  std::string toString() const override { return "FreeStmt"; }
};

class IfExpr : public Expr {
public:
  std::unique_ptr<Expr> Condition;
  std::unique_ptr<Stmt> Then;
  std::unique_ptr<Stmt> Else;

  IfExpr(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> thenStmt,
         std::unique_ptr<Stmt> elseStmt)
      : Condition(std::move(cond)), Then(std::move(thenStmt)),
        Else(std::move(elseStmt)) {}

  std::string toString() const override { return "If(...)"; }
};

class WhileExpr : public Expr {
public:
  std::unique_ptr<Expr> Condition;
  std::unique_ptr<Stmt> Body;
  std::unique_ptr<Stmt> ElseBody;

  WhileExpr(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> body,
            std::unique_ptr<Stmt> elseBody = nullptr)
      : Condition(std::move(cond)), Body(std::move(body)),
        ElseBody(std::move(elseBody)) {}

  std::string toString() const override { return "While(...)"; }
};

class LoopExpr : public Expr {
public:
  std::unique_ptr<Stmt> Body;
  LoopExpr(std::unique_ptr<Stmt> body) : Body(std::move(body)) {}

  std::string toString() const override { return "Loop"; }
};

class ForExpr : public Expr {
public:
  std::string VarName;
  bool IsReference = false;
  bool IsMutable = false;
  std::unique_ptr<Expr> Collection;
  std::unique_ptr<Stmt> Body;
  std::unique_ptr<Stmt> ElseBody;

  ForExpr(const std::string &varName, bool isRef, bool isMut,
          std::unique_ptr<Expr> coll, std::unique_ptr<Stmt> body,
          std::unique_ptr<Stmt> elseBody = nullptr)
      : VarName(varName), IsReference(isRef), IsMutable(isMut),
        Collection(std::move(coll)), Body(std::move(body)),
        ElseBody(std::move(elseBody)) {}

  std::string toString() const override { return "For(" + VarName + ")"; }
};

// Deprecated: MatchStmt is replaced by MatchExpr since match is now an
// expression.
using MatchStmt = MatchExpr;

struct DestructuredVar {
  std::string Name;
  bool IsMutable = false;
  bool IsNullable = false;
};

class DestructuringDecl : public Stmt {
public:
  std::string TypeName;
  std::vector<DestructuredVar> Variables;
  std::unique_ptr<Expr> Init;

  DestructuringDecl(const std::string &typeName,
                    std::vector<DestructuredVar> vars,
                    std::unique_ptr<Expr> init)
      : TypeName(typeName), Variables(std::move(vars)), Init(std::move(init)) {}

  std::string toString() const override { return "Destructuring " + TypeName; }
};

class VariableDecl : public Stmt {
public:
  std::string Name;
  std::unique_ptr<Expr> Init;
  std::string TypeName;
  bool HasPointer = false;
  bool IsUnique = false;
  bool IsShared = false;
  bool IsReference = false;
  bool IsPub = false;
  bool IsConst = false;
  // Permissions (Dual-Location Attributes)
  bool IsRebindable = false;      // Pointer Attribute # (^#p)
  bool IsValueMutable = false;    // Identifier Attribute # (p#)
  bool IsPointerNullable = false; // Pointer Attribute ? (^?p)
  bool IsValueNullable = false;   // Identifier Attribute ? (p?)

  // Deprecated/Legacy
  bool IsMutable = false;
  bool IsNullable = false;

  VariableDecl(const std::string &name, std::unique_ptr<Expr> init)
      : Name(name), Init(std::move(init)) {}

  std::string toString() const override { return "Val " + Name; }

  std::shared_ptr<Type> ResolvedType;
};

// --- High-level Declarations ---

class TypeAliasDecl : public ASTNode {
public:
  bool IsPub = false;
  std::string Name;
  std::string TargetType;
  bool IsStrong = false;
  TypeAliasDecl(bool isPub, const std::string &name, const std::string &target,
                bool isStrong = false)
      : IsPub(isPub), Name(name), TargetType(target), IsStrong(isStrong) {}
  std::string toString() const override {
    return std::string(IsPub ? "Pub" : "") + "TypeAlias(" + Name + " = " +
           TargetType + ")";
  }
};

enum class ShapeKind { Struct, Tuple, Array, Enum, Union };

struct ShapeMember {
  std::string Name; // Member or Variant name
  std::string Type;
  int64_t TagValue = -1; // Specific value for Tagged Union variants (= 1)
  bool HasPointer = false;
  bool IsUnique = false;
  bool IsShared = false;
  bool IsReference = false;

  // For Bare Union (as ...)
  std::vector<ShapeMember> SubMembers;
  ShapeKind SubKind = ShapeKind::Struct;
};

class ShapeDecl : public ASTNode {
public:
  bool IsPub = false;
  bool IsPacked = false;
  std::string Name;
  ShapeKind Kind;
  std::vector<ShapeMember> Members;
  int64_t ArraySize = 0; // For Array kind

  ShapeDecl(bool isPub, const std::string &name, ShapeKind kind,
            std::vector<ShapeMember> members, bool packed = false)
      : IsPub(isPub), IsPacked(packed), Name(name), Kind(kind),
        Members(std::move(members)) {}

  std::string toString() const override {
    return std::string(IsPub ? "Pub " : "") + "Shape(" + Name + ")";
  }
};

// Deprecated: Use ShapeDecl
using StructDecl = ShapeDecl;
using OptionDecl = ShapeDecl;

struct ImportItem {
  std::string Symbol; // Name of symbol, or "*" for wildcard
  std::string Alias;  // Optional alias
};

class ImportDecl : public ASTNode {
public:
  bool IsPub = false;
  std::string PhysicalPath;
  std::string Alias;             // Module alias (e.g. import path as alias)
  std::vector<ImportItem> Items; // If empty, it's a module import (import path)

  ImportDecl(bool isPub, const std::string &path, const std::string &alias = "",
             std::vector<ImportItem> items = {})
      : IsPub(isPub), PhysicalPath(path), Alias(alias),
        Items(std::move(items)) {}

  std::string toString() const override {
    std::string s = IsPub ? "PubImport(" : "Import(";
    s += PhysicalPath;
    if (!Items.empty()) {
      s += " :: {";
      for (size_t i = 0; i < Items.size(); ++i) {
        if (i > 0)
          s += ", ";
        s += Items[i].Symbol;
        if (!Items[i].Alias.empty())
          s += " as " + Items[i].Alias;
      }
      s += "}";
    }
    s += ")";
    return s;
  }
};

class FunctionDecl : public ASTNode {
public:
  struct Arg {
    std::string Name;
    std::string Type;
    bool HasPointer = false;
    bool IsUnique = false;
    bool IsShared = false;
    bool IsReference = false;
    bool IsMutable = false;  // Deprecated
    bool IsNullable = false; // Deprecated

    // New Permissions
    bool IsRebindable = false;
    bool IsValueMutable = false;
    bool IsPointerNullable = false;
    bool IsValueNullable = false;

    std::shared_ptr<toka::Type> ResolvedType;
  };

  bool IsPub = false;
  std::string Name;
  std::vector<Arg> Args;
  std::string ReturnType;
  std::unique_ptr<BlockStmt> Body;
  bool IsVariadic = false;

  FunctionDecl(bool isPub, const std::string &name, std::vector<Arg> args,
               std::unique_ptr<BlockStmt> body, const std::string &retType)
      : IsPub(isPub), Name(name), Args(std::move(args)), ReturnType(retType),
        Body(std::move(body)) {}
  std::string toString() const override {
    return std::string(IsPub ? "Pub" : "") + "Fn(" + Name + ")";
  }
};

class ExternDecl : public ASTNode {
public:
  struct Arg {
    std::string Name;
    std::string Type;
    bool HasPointer = false;
    bool IsReference = false;
    bool IsMutable = false;
    bool IsNullable = false;

    // New Permissions match FunctionDecl
    bool IsUnique = false;
    bool IsShared = false;
    bool IsRebindable = false;
    bool IsValueMutable = false;
    bool IsPointerNullable = false;
    bool IsValueNullable = false;
  };
  std::string Name;
  std::vector<Arg> Args;
  std::string ReturnType;
  bool IsVariadic = false;

  ExternDecl(const std::string &name, std::vector<Arg> args,
             std::string retType)
      : Name(name), Args(std::move(args)), ReturnType(retType) {}
  std::string toString() const override { return "Extern(" + Name + ")"; }
};

struct EncapEntry {
  enum Visibility { Global, Crate, Path, Private };
  Visibility Level;
  std::string TargetPath;
  std::vector<std::string> Fields;
  bool IsExclusion = false; // For pub * ! ...
};

class ImplDecl : public ASTNode {
public:
  std::string TypeName;
  std::string TraitName;
  std::vector<std::unique_ptr<FunctionDecl>> Methods;
  std::vector<EncapEntry> EncapEntries;

  ImplDecl(const std::string &name,
           std::vector<std::unique_ptr<FunctionDecl>> methods,
           const std::string &traitName = "")
      : TypeName(name), Methods(std::move(methods)), TraitName(traitName) {}
  std::string toString() const override {
    return "Impl(" + (TraitName.empty() ? "" : TraitName + " for ") + TypeName +
           ")";
  }
};

class TraitDecl : public ASTNode {
public:
  bool IsPub = false;
  std::string Name;
  std::vector<std::unique_ptr<FunctionDecl>> Methods;

  TraitDecl(bool isPub, const std::string &name,
            std::vector<std::unique_ptr<FunctionDecl>> methods)
      : IsPub(isPub), Name(name), Methods(std::move(methods)) {}
  std::string toString() const override {
    return std::string(IsPub ? "Pub" : "") + "Trait(" + Name + ")";
  }
};

class Module {
public:
  std::string FileName;
  std::vector<std::unique_ptr<ImportDecl>> Imports;
  std::vector<std::unique_ptr<TypeAliasDecl>> TypeAliases;
  std::vector<std::unique_ptr<ShapeDecl>> Shapes;
  std::vector<std::unique_ptr<Stmt>> Globals;
  std::vector<std::unique_ptr<ImplDecl>> Impls;
  std::vector<std::unique_ptr<TraitDecl>> Traits;
  std::vector<std::unique_ptr<ExternDecl>> Externs;
  std::vector<std::unique_ptr<FunctionDecl>> Functions;
};

} // namespace toka
