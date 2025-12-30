#pragma once

#include "toka/AST.h"
#include <map>
#include <set>
#include <string>
#include <vector>

namespace toka {

struct SymbolInfo {
  std::string Type;
  bool IsMutable = false;
  bool IsNullable = false;
  bool IsReference = false;
};

class Scope {
public:
  Scope *Parent = nullptr;
  std::map<std::string, SymbolInfo> Symbols;

  Scope(Scope *P = nullptr) : Parent(P) {}

  void define(const std::string &Name, const SymbolInfo &Info) {
    Symbols[Name] = Info;
  }

  bool lookup(const std::string &Name, SymbolInfo &OutInfo) {
    if (Symbols.count(Name)) {
      OutInfo = Symbols[Name];
      return true;
    }
    if (Parent)
      return Parent->lookup(Name, OutInfo);
    return false;
  }
};

class Sema {
public:
  Sema() = default;

  /// \brief Run semantic analysis on the module.
  /// \return true if success, false if errors found.
  bool checkModule(Module &M);

  bool hasErrors() const { return HasError; }

private:
  bool HasError = false;
  Scope *CurrentScope = nullptr;
  std::map<std::string, FunctionDecl *> FunctionMap;
  std::map<std::string, ExternDecl *> ExternMap;
  std::map<std::string, StructDecl *> StructMap;
  std::map<std::string, std::string> TypeAliasMap;
  // TypeName -> {MethodName -> ReturnType}
  std::map<std::string, std::map<std::string, std::string>> MethodMap;
  std::string CurrentFunctionReturnType;

  void error(ASTNode *Node, const std::string &Msg);

  // Scope management
  void enterScope();
  void exitScope();

  // Passes
  void registerGlobals(Module &M);
  void checkFunction(FunctionDecl *Fn);
  void checkStmt(Stmt *S);
  std::string checkExpr(Expr *E); // Returns type name

  // Helpers
  std::string resolveType(const std::string &Type);
  bool isTypeCompatible(const std::string &Target, const std::string &Source);
};

} // namespace toka
