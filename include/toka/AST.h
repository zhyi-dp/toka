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

class ASTNode;

struct GenericParam {
  std::string Name;
  std::string Type; // Empty if it's a type parameter
  bool IsConst = false;
};

class ASTNode {
public:
  SourceLocation Loc;

  virtual ~ASTNode() = default;
  virtual std::string toString() const = 0;

  virtual std::unique_ptr<ASTNode> clone() const {
    return nullptr;
  } // TODO: Make pure virtual after implementing for all

  void setLocation(const Token &tok, const std::string &file = "") {
    Loc = tok.Loc;
  }
};

// Helper for deep copying unique_ptr<T> where T : ASTNode
template <typename T>
std::unique_ptr<T> cloneNode(const std::unique_ptr<T> &node) {
  if (!node)
    return nullptr;
  // clone() returns unique_ptr<ASTNode>, we cast it back to unique_ptr<T>
  // This assumes the clone() implementation returns the correct type.
  return std::unique_ptr<T>(static_cast<T *>(node->clone().release()));
}

// Helper for deep copying vector of unique_ptr<T>
template <typename T>
std::vector<std::unique_ptr<T>>
cloneVec(const std::vector<std::unique_ptr<T>> &vec) {
  std::vector<std::unique_ptr<T>> res;
  res.reserve(vec.size());
  for (const auto &el : vec) {
    res.push_back(cloneNode(el));
  }
  return res;
}

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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<NumberExpr>(Value);
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class FloatExpr : public Expr {
public:
  double Value;
  FloatExpr(double val) : Value(val) {}
  std::string toString() const override {
    return "Float(" + std::to_string(Value) + ")";
  }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<FloatExpr>(Value);
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class BoolExpr : public Expr {
public:
  bool Value;
  BoolExpr(bool val) : Value(val) {}
  std::string toString() const override { return Value ? "true" : "false"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<BoolExpr>(Value);
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
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

class UnsetExpr : public Expr {
public:
  UnsetExpr() {}
  std::string toString() const override { return "unset"; }
};

class VariableExpr : public Expr {
public:
  std::string Name;
  bool HasPointer = false;
  bool IsUnique = false;
  bool IsShared = false;
  bool IsValueMutable = false;
  bool IsValueNullable = false;
  bool HasConstantValue = false;
  uint64_t ConstantValue = 0;

  VariableExpr(const std::string &name) : Name(name) {}
  std::string toString() const override {
    return std::string("Var(") + (HasPointer ? "^" : "") + Name +
           (IsValueMutable ? "#" : "") + ")";
  }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<VariableExpr>(Name);
    n->HasPointer = HasPointer;
    n->IsUnique = IsUnique;
    n->IsShared = IsShared;
    n->IsValueMutable = IsValueMutable;
    n->IsValueNullable = IsValueNullable;
    n->HasConstantValue = HasConstantValue;
    n->ConstantValue = ConstantValue;
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class StringExpr : public Expr {
public:
  std::string Value;
  StringExpr(const std::string &val) : Value(val) {}
  std::string toString() const override { return "String(\"" + Value + "\")"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<StringExpr>(Value);
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class DereferenceExpr : public Expr {
public:
  std::unique_ptr<Expr> Expression;
  DereferenceExpr(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override {
    return std::string("Dereference(") + Expression->toString() + ")";
  }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<DereferenceExpr>(cloneNode(Expression));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<BinaryExpr>(Op, cloneNode(LHS), cloneNode(RHS));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<UnaryExpr>(Op, cloneNode(RHS));
    n->HasNull = HasNull;
    n->IsRebindable = IsRebindable;
    n->IsValueMutable = IsValueMutable;
    n->IsValueNullable = IsValueNullable;
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<PostfixExpr>(Op, cloneNode(LHS));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<CastExpr>(cloneNode(Expression), TargetType);
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class AddressOfExpr : public Expr {
public:
  std::unique_ptr<Expr> Expression;
  AddressOfExpr(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override { return "&" + Expression->toString(); }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<AddressOfExpr>(cloneNode(Expression));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
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
    return Object->toString() + (IsArrow ? "->" : ".") + Member;
  }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<MemberExpr>(cloneNode(Object), Member, IsArrow,
                                          IsStatic);
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n =
        std::make_unique<ArrayIndexExpr>(cloneNode(Array), cloneVec(Indices));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<ArrayExpr>(cloneVec(Elements));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class RepeatedArrayExpr : public Expr {
public:
  std::unique_ptr<Expr> Value;
  std::unique_ptr<Expr> Count;
  RepeatedArrayExpr(std::unique_ptr<Expr> val, std::unique_ptr<Expr> count)
      : Value(std::move(val)), Count(std::move(count)) {}
  std::string toString() const override { return "RepeatedArray"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n =
        std::make_unique<RepeatedArrayExpr>(cloneNode(Value), cloneNode(Count));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class UnsafeExpr : public Expr {
public:
  std::unique_ptr<Expr> Expression;
  UnsafeExpr(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override {
    return "Unsafe(" + Expression->toString() + ")";
  }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<UnsafeExpr>(cloneNode(Expression));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<AllocExpr>(TypeName, cloneNode(Initializer),
                                         IsArray, cloneNode(ArraySize));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<TupleExpr>(cloneVec(Elements));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
  std::unique_ptr<ASTNode> clone() const override {
    std::vector<std::pair<std::string, std::unique_ptr<Expr>>> members;
    for (const auto &p : Members) {
      members.emplace_back(p.first, cloneNode(p.second));
    }
    auto n = std::make_unique<InitStructExpr>(ShapeName, std::move(members));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
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
  std::unique_ptr<ASTNode> clone() const override {
    std::vector<std::pair<std::string, std::unique_ptr<Expr>>> fields;
    for (const auto &p : Fields) {
      fields.emplace_back(p.first, cloneNode(p.second));
    }
    auto n = std::make_unique<AnonymousRecordExpr>(std::move(fields));
    n->AssignedTypeName = AssignedTypeName;
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class FunctionDecl;
class ExternDecl;
class ShapeDecl;

class CallExpr : public Expr {
public:
  std::string Callee;
  std::vector<std::unique_ptr<Expr>> Args;
  std::vector<std::string> GenericArgs; // [NEW]

  // Semantic Resolution Cache
  FunctionDecl *ResolvedFn = nullptr;
  ExternDecl *ResolvedExtern = nullptr;
  ShapeDecl *ResolvedShape = nullptr;
  int MatchedMemberIdx = -1; // For Union variant selection

  CallExpr(const std::string &callee, std::vector<std::unique_ptr<Expr>> args,
           std::vector<std::string> genericArgs = {})
      : Callee(callee), Args(std::move(args)),
        GenericArgs(std::move(genericArgs)) {}

  std::string toString() const override {
    std::string s = "Call(" + Callee;
    if (!GenericArgs.empty()) {
      s += "<";
      for (size_t i = 0; i < GenericArgs.size(); ++i) {
        if (i > 0)
          s += ", ";
        s += GenericArgs[i];
      }
      s += ">";
    }
    s += ")";
    return s;
  }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<CallExpr>(Callee, cloneVec(Args), GenericArgs);
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    n->ResolvedFn = nullptr; // Reset sema cache
    return n;
  }
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<MethodCallExpr>(cloneNode(Object), Method,
                                              cloneVec(Args));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<NewExpr>(Type, cloneNode(Initializer));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class PassExpr : public Expr {
public:
  std::unique_ptr<Expr> Value;
  PassExpr(std::unique_ptr<Expr> val) : Value(std::move(val)) {}
  std::string toString() const override {
    return "Pass(" + (Value ? Value->toString() : "none") + ")";
  }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<PassExpr>(cloneNode(Value));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
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
    bool IsValueMutable = false;
    std::vector<std::unique_ptr<Pattern>> SubPatterns; // For Decons

    Pattern(Kind k) : PatternKind(k) {}
    std::string toString() const {
      switch (PatternKind) {
      case Literal:
        return std::to_string(LiteralVal);
      case Variable:
        return (IsReference ? "&" : "") + Name + (IsValueMutable ? "#" : "");
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

  std::unique_ptr<MatchArm> clone() const {
    // Pattern is NOT ASTNode compatible with cloneNode?
    // Wait, Pattern inherits ASTNode (Line 551). So cloneNode<Pattern> works.
    auto n = std::make_unique<MatchArm>(cloneNode(Pat), cloneNode(Guard),
                                        cloneNode(Body));
    return n;
  }
};

class MatchExpr : public Expr {
public:
  std::unique_ptr<Expr> Target;
  std::vector<std::unique_ptr<MatchArm>> Arms;

  MatchExpr(std::unique_ptr<Expr> target,
            std::vector<std::unique_ptr<MatchArm>> arms)
      : Target(std::move(target)), Arms(std::move(arms)) {}

  std::string toString() const override { return "Match(...)"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<MatchExpr>(cloneNode(Target), cloneVec(Arms));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class BreakExpr : public Expr {
public:
  std::string TargetLabel;
  std::unique_ptr<Expr> Value;
  BreakExpr(std::string label, std::unique_ptr<Expr> val)
      : TargetLabel(std::move(label)), Value(std::move(val)) {}
  std::string toString() const override { return "Break"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<BreakExpr>(TargetLabel, cloneNode(Value));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class ContinueExpr : public Expr {
public:
  std::string TargetLabel;
  ContinueExpr(std::string label) : TargetLabel(std::move(label)) {}
  std::string toString() const override { return "Continue"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<ContinueExpr>(TargetLabel);
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

// --- Statements ---

class BlockStmt : public Stmt {
public:
  std::vector<std::unique_ptr<Stmt>> Statements;
  BlockStmt() = default;
  BlockStmt(std::vector<std::unique_ptr<Stmt>> stmts)
      : Statements(std::move(stmts)) {}
  std::string toString() const override { return "Block"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<BlockStmt>(cloneVec(Statements));
    n->Loc = Loc;
    return n;
  }
};

class ReturnStmt : public Stmt {
public:
  std::unique_ptr<Expr> ReturnValue;
  ReturnStmt(std::unique_ptr<Expr> val) : ReturnValue(std::move(val)) {}
  std::string toString() const override { return "Return"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<ReturnStmt>(cloneNode(ReturnValue));
    n->Loc = Loc;
    return n;
  }
};

class ExprStmt : public Stmt {
public:
  std::unique_ptr<Expr> Expression;
  ExprStmt(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override { return "ExprStmt"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<ExprStmt>(cloneNode(Expression));
    n->Loc = Loc;
    return n;
  }
};

class DeleteStmt : public Stmt {
public:
  std::unique_ptr<Expr> Expression;
  DeleteStmt(std::unique_ptr<Expr> expr) : Expression(std::move(expr)) {}
  std::string toString() const override { return "Delete"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<DeleteStmt>(cloneNode(Expression));
    n->Loc = Loc;
    return n;
  }
};

class UnsafeStmt : public Stmt {
public:
  std::unique_ptr<Stmt> Statement;
  UnsafeStmt(std::unique_ptr<Stmt> stmt) : Statement(std::move(stmt)) {}
  std::string toString() const override { return "UnsafeStmt"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<UnsafeStmt>(cloneNode(Statement));
    n->Loc = Loc;
    return n;
  }
};

class FreeStmt : public Stmt {
public:
  std::unique_ptr<Expr> Expression;
  std::unique_ptr<Expr> Count;
  FreeStmt(std::unique_ptr<Expr> expr, std::unique_ptr<Expr> count = nullptr)
      : Expression(std::move(expr)), Count(std::move(count)) {}
  std::string toString() const override { return "FreeStmt"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n =
        std::make_unique<FreeStmt>(cloneNode(Expression), cloneNode(Count));
    n->Loc = Loc;
    return n;
  }
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<IfExpr>(cloneNode(Condition), cloneNode(Then),
                                      cloneNode(Else));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<WhileExpr>(cloneNode(Condition), cloneNode(Body),
                                         cloneNode(ElseBody));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

class LoopExpr : public Expr {
public:
  std::unique_ptr<Stmt> Body;
  LoopExpr(std::unique_ptr<Stmt> body) : Body(std::move(body)) {}

  std::string toString() const override { return "Loop"; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<LoopExpr>(cloneNode(Body));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<ForExpr>(VarName, IsReference, IsMutable,
                                       cloneNode(Collection), cloneNode(Body),
                                       cloneNode(ElseBody));
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }
};

// Deprecated: MatchStmt is replaced by MatchExpr since match is now an
// expression.
using MatchStmt = MatchExpr;

struct DestructuredVar {
  std::string Name;
  bool IsValueMutable = false;
  bool IsValueNullable = false;
  bool IsReference = false;
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
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<DestructuringDecl>(TypeName, Variables,
                                                 cloneNode(Init));
    n->Loc = Loc;
    return n;
  }
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

  VariableDecl(const std::string &name, std::unique_ptr<Expr> init)
      : Name(name), Init(std::move(init)) {}

  std::string toString() const override { return "Val " + Name; }
  std::unique_ptr<ASTNode> clone() const override {
    auto n = std::make_unique<VariableDecl>(Name, cloneNode(Init));
    n->TypeName = TypeName;
    n->HasPointer = HasPointer;
    n->IsUnique = IsUnique;
    n->IsShared = IsShared;
    n->IsReference = IsReference;
    n->IsPub = IsPub;
    n->IsConst = IsConst;
    n->IsRebindable = IsRebindable;
    n->IsValueMutable = IsValueMutable; // VariableDecl has this field
    n->IsPointerNullable = IsPointerNullable;
    n->IsValueNullable = IsValueNullable;
    n->Loc = Loc;
    n->ResolvedType = ResolvedType;
    return n;
  }

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

  // Resolution Cache from Sema
  std::shared_ptr<toka::Type> ResolvedType = nullptr;
};

class ShapeDecl : public ASTNode {
public:
  bool IsPub = false;
  bool IsPacked = false;
  std::string Name;
  // struct GenericParam moved to top-level
  std::vector<GenericParam> GenericParams; // [UPDATED] e.g. <T, N_: usize>
  ShapeKind Kind;
  std::vector<ShapeMember> Members;
  int64_t ArraySize = 0; // For Array kind
  uint64_t MaxAlign = 1; // For Union/Enum alignment persistence

  ShapeDecl(bool isPub, const std::string &name,
            std::vector<GenericParam> generics, ShapeKind kind,
            std::vector<ShapeMember> members, bool packed = false)
      : IsPub(isPub), IsPacked(packed), Name(name),
        GenericParams(std::move(generics)), Kind(kind),
        Members(std::move(members)) {}

  std::string toString() const override {
    std::string s = std::string(IsPub ? "Pub " : "") + "Shape(" + Name;
    if (!GenericParams.empty()) {
      s += "<";
      for (size_t i = 0; i < GenericParams.size(); ++i) {
        if (i > 0)
          s += ", ";
        s += GenericParams[i].Name;
        if (GenericParams[i].IsConst)
          s += ": " + GenericParams[i].Type;
      }
      s += ">";
    }
    s += ")";
    return s;
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

    // Permissions
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
  std::vector<std::string> LifeDependencies; // [NEW] e.g., <- x|y
  std::unique_ptr<BlockStmt> Body;

  bool IsVariadic = false;
  std::vector<GenericParam> GenericParams; // [NEW] e.g. <T>

  FunctionDecl(bool isPub, const std::string &name, std::vector<Arg> args,
               std::unique_ptr<BlockStmt> body, const std::string &retType,
               std::vector<GenericParam> generics = {},
               std::vector<std::string> lifeDeps = {})
      : IsPub(isPub), Name(name), Args(std::move(args)), ReturnType(retType),
        Body(std::move(body)), GenericParams(std::move(generics)),
        LifeDependencies(std::move(lifeDeps)) {}
  std::string toString() const override {
    return std::string(IsPub ? "Pub" : "") + "Fn(" + Name + ")";
  }
  std::unique_ptr<ASTNode> clone() const override {
    // Manually deep copy args (since they contain strings/bools and shared_ptr)
    // shared_ptr copy is shallow for resolved type, which is what we want?
    // Wait, generic template args have unresolved types usually? Or generic
    // types? We copy whatever is there.
    std::vector<Arg> clonedArgs = Args;

    // Deep copy body
    auto clonedBody =
        Body ? std::unique_ptr<BlockStmt>(
                   static_cast<BlockStmt *>(Body->clone().release()))
             : nullptr;

    auto n = std::make_unique<FunctionDecl>(IsPub, Name, clonedArgs,
                                            std::move(clonedBody), ReturnType,
                                            GenericParams, LifeDependencies);
    n->IsVariadic = IsVariadic;
    n->Loc = Loc;
    // FunctionDecl is NOT an Expr, does not have ResolvedType?
    // FunctionDecl inherits ASTNode directly (Line 993).
    // ASTNode doesn't have ResolvedType.
    return n;
  }
};

class ExternDecl : public ASTNode {
public:
  struct Arg {
    std::string Name;
    std::string Type;
    bool HasPointer = false;
    bool IsReference = false;

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
  std::vector<GenericParam> GenericParams; // [NEW] e.g. <T>

  ImplDecl(const std::string &name,
           std::vector<std::unique_ptr<FunctionDecl>> methods,
           const std::string &traitName = "",
           std::vector<GenericParam> generics = {})
      : TypeName(name), Methods(std::move(methods)), TraitName(traitName),
        GenericParams(std::move(generics)) {}
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

class Module : public ASTNode {
public:
  std::vector<std::unique_ptr<ImportDecl>> Imports;
  std::vector<std::unique_ptr<TypeAliasDecl>> TypeAliases;
  std::vector<std::unique_ptr<ShapeDecl>> Shapes;
  std::vector<std::unique_ptr<Stmt>> Globals;
  std::vector<std::unique_ptr<ImplDecl>> Impls;
  std::vector<std::unique_ptr<TraitDecl>> Traits;
  std::vector<std::unique_ptr<ExternDecl>> Externs;
  std::vector<std::unique_ptr<FunctionDecl>> Functions;

  std::string toString() const override { return "Module"; }
};

} // namespace toka
