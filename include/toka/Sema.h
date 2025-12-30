#pragma once

#include "toka/AST.h"
#include <map>
#include <set>
#include <string>
#include <vector>

namespace toka {

struct SymbolInfo {
  std::string Type;       // Soul Type (e.g., "Point")
  std::string Morphology; // "", "^", "~", "*", "&"

  // Dual-Location Attributes
  bool IsRebindable = false;      // # on ^, ~, *
  bool IsValueMutable = false;    // # on identifier
  bool IsPointerNullable = false; // ? on ^, ~, *
  bool IsValueNullable = false;   // ? on identifier

  bool Moved = false;

  // Borrow Tracking
  int ImmutableBorrowCount = 0;
  bool IsMutablyBorrowed = false;
  std::string BorrowedFrom =
      ""; // If this is a reference, name of the source variable

  // Legacy/Helpers
  bool IsMutable() const { return IsValueMutable; }
  bool IsReference() const { return Morphology == "&"; }
  bool IsUnique() const { return Morphology == "^"; }
  bool IsShared() const { return Morphology == "~"; }
};

class Scope {
public:
  Scope *Parent = nullptr;
  std::map<std::string, SymbolInfo> Symbols;

  // Track which variables in THIS scope level are references borrowing from
  // elsewhere Map: ReferenceName -> {SourceVarName, IsMutable}
  std::map<std::string, std::pair<std::string, bool>> ActiveBorrows;

  Scope(Scope *P = nullptr) : Parent(P) {}

  void define(const std::string &Name, const SymbolInfo &Info) {
    Symbols[Name] = Info;
    if (!Info.BorrowedFrom.empty()) {
      ActiveBorrows[Name] = {Info.BorrowedFrom,
                             Info.Morphology == "&" && Info.IsValueMutable};
    }
  }

  // Find symbol and its owning scope
  bool findSymbol(const std::string &Name, SymbolInfo *&OutInfo) {
    if (Symbols.count(Name)) {
      OutInfo = &Symbols[Name];
      return true;
    }
    if (Parent)
      return Parent->findSymbol(Name, OutInfo);
    return false;
  }

  bool lookup(const std::string &Name, SymbolInfo &OutInfo) {
    SymbolInfo *ptr = nullptr;
    if (findSymbol(Name, ptr)) {
      OutInfo = *ptr;
      return true;
    }
    return false;
  }

  // Mark a symbol as moved. Returns true if found and updated.
  bool markMoved(const std::string &Name) {
    SymbolInfo *ptr = nullptr;
    if (findSymbol(Name, ptr)) {
      ptr->Moved = true;
      return true;
    }
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
  std::map<std::string, TraitDecl *> TraitMap;
  std::string CurrentFunctionReturnType;
  std::string m_LastBorrowSource;
  // {VarName, IsMutable}
  std::vector<std::pair<std::string, bool>> m_CurrentStmtBorrows;

  void error(ASTNode *Node, const std::string &Msg);

  // Scope management
  void enterScope();
  void exitScope();
  void clearStmtBorrows();

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
