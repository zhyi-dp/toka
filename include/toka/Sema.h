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

#include "toka/AST.h"
#include "toka/Type.h"
#include <map>
#include <set>
#include <string>
#include <vector>

namespace toka {

struct SymbolInfo {
  // New Type Object (Source of Truth)
  std::shared_ptr<toka::Type> TypeObj;

  bool Moved = false;

  // Borrow Tracking
  int ImmutableBorrowCount = 0;
  bool IsMutablyBorrowed = false;
  std::string BorrowedFrom =
      ""; // If this is a reference, name of the source variable

  void *ReferencedModule = nullptr; // Pointer to ModuleScope (opaque here)

  // Helpers redirection to TypeObj
  bool IsMutable() const {
    // Map to TypeObj IsWritable attribute (handles #)
    return TypeObj && TypeObj->IsWritable;
  }

  bool IsReference() const { return TypeObj && TypeObj->isReference(); }

  bool IsUnique() const {
    return TypeObj && TypeObj->typeKind == toka::Type::UniquePtr;
  }

  bool IsShared() const {
    return TypeObj && TypeObj->typeKind == toka::Type::SharedPtr;
  }
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
                             Info.IsReference() && Info.IsMutable()};
      // llvm::errs() requires include. std::cerr works? No, Sema.h included
      // everywhere. Skipping Sema.h debug print to avoid include mess. Rely on
      // Sema.cpp.
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
  // Shape Analysis Caches
  enum class ShapeAnalysisStatus {
    Unvisited,
    Visiting, // Cycle detection
    Analyzed
  };

  struct ShapeProperties {
    bool HasRawPtr = false;
    bool HasDrop = false;
    ShapeAnalysisStatus Status = ShapeAnalysisStatus::Unvisited;
  };

  std::map<std::string, ShapeProperties> m_ShapeProps;

  void analyzeShapes(Module &M);
  void checkShapeSovereignty();
  void computeShapeProperties(const std::string &shapeName, Module &M);

  bool HasError = false;
  Scope *CurrentScope = nullptr;
  std::vector<FunctionDecl *>
      GlobalFunctions; // All functions across all modules
  std::map<std::string, ExternDecl *> ExternMap;
  std::map<std::string, ShapeDecl *> ShapeMap;
  struct AliasInfo {
    std::string Target;
    bool IsStrong;
  };
  std::map<std::string, AliasInfo> TypeAliasMap;
  // TypeName -> {MethodName -> ReturnType}
  std::map<std::string, std::map<std::string, std::string>> MethodMap;
  // TypeName -> {MethodName -> FunctionDecl*}
  std::map<std::string, std::map<std::string, FunctionDecl *>> MethodDecls;
  std::map<std::string, TraitDecl *> TraitMap;
  // Key: "StructName@TraitName" -> {MethodName -> FunctionDecl*}
  std::map<std::string, std::map<std::string, FunctionDecl *>> ImplMap;
  std::map<std::string, std::vector<EncapEntry>> EncapMap;
  std::string CurrentFunctionReturnType;
  std::string m_LastBorrowSource;
  // {VarName, IsMutable}
  std::vector<std::pair<std::string, bool>> m_CurrentStmtBorrows;
  struct ModuleScope {
    std::string Name;
    std::map<std::string, FunctionDecl *> Functions;
    std::map<std::string, ExternDecl *> Externs;
    std::map<std::string, ShapeDecl *> Shapes;
    std::map<std::string, AliasInfo> TypeAliases;
    std::map<std::string, TraitDecl *> Traits;
    std::map<std::string, VariableDecl *> Globals;
  };
  std::map<std::string, ModuleScope> ModuleMap; // FullPath -> Scope

  ModuleScope *getModule(const std::string &Path);

  Module *CurrentModule = nullptr;
  bool m_InUnsafeContext = false;

  struct ControlFlowInfo {
    std::string Label;
    std::string ExpectedType;
    bool IsLoop;
    bool IsReceiver =
        false; // Whether this context expects a 'pass' or 'break' value
  };
  std::vector<ControlFlowInfo> m_ControlFlowStack;

  // Anonymous Records
  int AnonRecordCounter = 0;
  std::vector<std::unique_ptr<ShapeDecl>> SyntheticShapes;

  void error(ASTNode *Node, const std::string &Msg);

  // Scope management
  void enterScope();
  void exitScope();
  void clearStmtBorrows();

  // Passes
  void registerGlobals(Module &M);
  void checkFunction(FunctionDecl *Fn);
  void checkStmt(Stmt *S);

  std::string checkUnaryExprStr(UnaryExpr *Unary); // Legacy
  std::shared_ptr<toka::Type> checkExpr(Expr *E);  // New Object API
  std::shared_ptr<toka::Type>
  checkUnaryExpr(UnaryExpr *Unary); // New Object API
  std::shared_ptr<toka::Type>
  checkBinaryExpr(BinaryExpr *Bin); // New Object API
  std::shared_ptr<toka::Type>
  checkIndexExpr(ArrayIndexExpr *Idx);                       // New Object API
  std::shared_ptr<toka::Type> checkCallExpr(CallExpr *Call); // New Object API
  void checkPattern(MatchArm::Pattern *Pat, const std::string &TargetType,
                    bool SourceIsMutable);

  // Type system helpers
  std::string getCommonType(const std::string &T1, const std::string &T2);

  // Helpers
  std::string resolveType(const std::string &Type);
  std::shared_ptr<toka::Type> resolveType(std::shared_ptr<toka::Type> Type);
  bool isTypeCompatible(const std::string &Target, const std::string &Source);
  bool isTypeCompatible(std::shared_ptr<toka::Type> Target,
                        std::shared_ptr<toka::Type> Source);
};

} // namespace toka
