#pragma once

#include "toka/Token.h"
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

class Expr : public ASTNode {};
class Stmt : public ASTNode {};

// --- Expressions ---

class NumberExpr : public Expr {
public:
  int64_t Value;
  NumberExpr(int64_t val) : Value(val) {}
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
  UnaryExpr(TokenType op, std::unique_ptr<Expr> rhs)
      : Op(op), RHS(std::move(rhs)) {}
  std::string toString() const override {
    return "Unary(" + std::to_string((int)Op) + ", " + RHS->toString() + ")";
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
  MemberExpr(std::unique_ptr<Expr> obj, const std::string &member,
             bool isArrow = false)
      : Object(std::move(obj)), Member(member), IsArrow(isArrow) {}
  std::string toString() const override {
    return Object->toString() + (IsArrow ? "->" : ".") + Member;
  }
};

class ArrayIndexExpr : public Expr {
public:
  std::unique_ptr<Expr> Array;
  std::unique_ptr<Expr> Index;

  ArrayIndexExpr(std::unique_ptr<Expr> arr, std::unique_ptr<Expr> idx)
      : Array(std::move(arr)), Index(std::move(idx)) {}
  std::string toString() const override {
    return Array->toString() + "[" + Index->toString() + "]";
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
  std::string StructName;
  std::vector<std::pair<std::string, std::unique_ptr<Expr>>> Fields;
  InitStructExpr(
      const std::string &name,
      std::vector<std::pair<std::string, std::unique_ptr<Expr>>> flds)
      : StructName(name), Fields(std::move(flds)) {}
  std::string toString() const override { return "Init(" + StructName + ")"; }
};

class CallExpr : public Expr {
public:
  std::string Callee;
  std::vector<std::unique_ptr<Expr>> Args;

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

class IfStmt : public Stmt {
public:
  std::unique_ptr<Expr> Condition;
  std::unique_ptr<Stmt> Then;
  std::unique_ptr<Stmt> Else;

  IfStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> thenStmt,
         std::unique_ptr<Stmt> elseStmt)
      : Condition(std::move(cond)), Then(std::move(thenStmt)),
        Else(std::move(elseStmt)) {}

  std::string toString() const override { return "If(...)"; }
};

class WhileStmt : public Stmt {
public:
  std::unique_ptr<Expr> Condition;
  std::unique_ptr<Stmt> Body;

  WhileStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> body)
      : Condition(std::move(cond)), Body(std::move(body)) {}

  std::string toString() const override { return "While(...)"; }
};

struct MatchCase {
  // Pattern info
  std::string VariantName; // e.g. "Some" or "Data" or "None"
  std::vector<std::string>
      BindingNames; // For destructured vars: let Some(a, b) -> ["a", "b"]
  bool IsWildcard = false; // _ => ...

  std::unique_ptr<Expr> Guard; // if condition
  std::unique_ptr<Stmt> Body;
};

class MatchStmt : public Stmt {
public:
  std::unique_ptr<Expr> Target; // match target
  std::vector<MatchCase> Cases;

  MatchStmt(std::unique_ptr<Expr> target, std::vector<MatchCase> cases)
      : Target(std::move(target)), Cases(std::move(cases)) {}

  std::string toString() const override { return "Match(...)"; }
};

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
  bool IsMutable = false;
  bool IsNullable = false;

  VariableDecl(const std::string &name, std::unique_ptr<Expr> init)
      : Name(name), Init(std::move(init)) {}

  std::string toString() const override { return "Val " + Name; }
};

// --- High-level Declarations ---

struct StructField {
  std::string Name;
  std::string Type;
  bool HasPointer = false;
};

class TypeAliasDecl : public ASTNode {
public:
  std::string Name;
  std::string TargetType;
  TypeAliasDecl(const std::string &name, const std::string &target)
      : Name(name), TargetType(target) {}
  std::string toString() const override {
    return "TypeAlias(" + Name + " = " + TargetType + ")";
  }
};

struct VariantField {
  std::string Name; // Optional name
  std::string Type;
  bool HasPointer = false;
  bool IsUnique = false;
  bool IsShared = false;
  bool IsReference = false;
};

struct OptionVariant {
  std::string Name;
  std::vector<VariantField> Fields;
};

class OptionDecl : public ASTNode {
public:
  std::string Name;
  std::vector<OptionVariant> Variants;
  OptionDecl(const std::string &name, std::vector<OptionVariant> variants)
      : Name(name), Variants(std::move(variants)) {}
  std::string toString() const override { return "Option(" + Name + ")"; }
};

class StructDecl : public ASTNode {
public:
  std::string Name;
  std::vector<StructField> Fields;
  StructDecl(const std::string &name, std::vector<StructField> flds)
      : Name(name), Fields(std::move(flds)) {}
  std::string toString() const override { return "Struct(" + Name + ")"; }
};

class ImportDecl : public ASTNode {
public:
  std::string Path;
  ImportDecl(const std::string &path) : Path(path) {}
  std::string toString() const override { return "Import(" + Path + ")"; }
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
    bool IsMutable = false;
    bool IsNullable = false;
  };

  std::string Name;
  std::vector<Arg> Args;
  std::string ReturnType;
  std::unique_ptr<BlockStmt> Body;
  bool IsVariadic = false;

  FunctionDecl(const std::string &name, std::vector<Arg> args,
               std::unique_ptr<BlockStmt> body, const std::string &retType)
      : Name(name), Args(std::move(args)), ReturnType(retType),
        Body(std::move(body)) {}
  std::string toString() const override { return "Fn(" + Name + ")"; }
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

class ImplDecl : public ASTNode {
public:
  std::string TypeName;
  std::vector<std::unique_ptr<FunctionDecl>> Methods;

  ImplDecl(const std::string &name,
           std::vector<std::unique_ptr<FunctionDecl>> methods)
      : TypeName(name), Methods(std::move(methods)) {}
  std::string toString() const override { return "Impl(" + TypeName + ")"; }
};

class Module {
public:
  std::vector<std::unique_ptr<ImportDecl>> Imports;
  std::vector<std::unique_ptr<TypeAliasDecl>> TypeAliases;
  std::vector<std::unique_ptr<StructDecl>> Structs;
  std::vector<std::unique_ptr<OptionDecl>> Options;
  std::vector<std::unique_ptr<Stmt>> Globals;
  std::vector<std::unique_ptr<ImplDecl>> Impls;
  std::vector<std::unique_ptr<ExternDecl>> Externs;
  std::vector<std::unique_ptr<FunctionDecl>> Functions;
};

} // namespace toka
