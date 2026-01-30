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
#include "toka/DiagnosticEngine.h"
#include "toka/Type.h"
#include <map>
#include <set>
#include <string>
#include <vector>

namespace toka {

struct SymbolInfo {
  // New Type Object (Source of Truth)
  std::shared_ptr<toka::Type> TypeObj;

  bool IsTypeAlias = false; // [NEW] For Generic Params (T -> i32)
  bool Moved = false;
  uint64_t InitMask =
      ~0ULL; // 0=unset, 1=set. For shapes, each bit corresponds to a member.

  // "Hot Potato" Tracking
  // If this symbol is a Reference (&T), this mask tracks the InitMask of the
  // REFERENT. If a bit is 0, it means the referent's corresponding field is
  // Unset, and THIS reference is responsible for initializing it (or passing
  // the responsibility).
  uint64_t DirtyReferentMask = ~0ULL; // Default to Clean

  // Borrow Tracking
  int ImmutableBorrowCount = 0;
  bool IsMutablyBorrowed = false;
  std::string MutablyBorrowedBy = ""; // [NEW] Name of the mutable borrower
  std::string BorrowedFrom =
      ""; // If this is a reference, name of the source variable
  std::set<std::string> LifeDependencySet; // [NEW] Shadow Dependency Set

  void *ReferencedModule = nullptr; // Pointer to ModuleScope (opaque here)

  // Helpers redirection to TypeObj
  bool IsMutable() const {
    // Map to TypeObj IsWritable attribute (handles #)
    return TypeObj && TypeObj->IsWritable;
  }

  bool IsReference() const { return TypeObj && TypeObj->isReference(); }

  bool IsBorrowed() const {
    return IsMutablyBorrowed || ImmutableBorrowCount > 0;
  }

  bool IsUnique() const {
    return TypeObj && TypeObj->typeKind == toka::Type::UniquePtr;
  }

  bool IsShared() const {
    return TypeObj && TypeObj->typeKind == toka::Type::SharedPtr;
  }

  bool IsSoulMutable() const {
    if (!TypeObj)
      return false;
    // For pointers/references, we care about whether the POINTED-TO value is
    // mutable.
    if (TypeObj->isPointer()) {
      auto pointee = TypeObj->getPointeeType();
      return pointee && pointee->IsWritable;
    }
    // For non-pointers, identity and soul are at the same level of mutability.
    return TypeObj->IsWritable;
  }

  bool HasConstValue = false;
  uint64_t ConstValue = 0;
  bool IsRebindable = false; // [NEW] prefix '#' or '!' rebind permission
};

class Scope {
public:
  Scope *Parent = nullptr;
  std::map<std::string, SymbolInfo> Symbols;

  // Track which variables in THIS scope level are references borrowing from
  // elsewhere Map: ReferenceName -> {SourceVarName, IsMutable}
  std::map<std::string, std::pair<std::string, bool>> ActiveBorrows;

  int Depth = 0; // [NEW] Scope depth for lifetime comparison
  Scope(Scope *P = nullptr) : Parent(P) {
    if (P)
      Depth = P->Depth + 1;
  }

  void define(const std::string &Name, const SymbolInfo &Info) {
    Symbols[Name] = Info;
    if (!Info.BorrowedFrom.empty()) {
      // Use IsSoulMutable() to determine if this is an exclusive (mutable)
      // borrow. Identity mutability (IsRebindable) doesn't matter for the
      // borrow status of the source.
      ActiveBorrows[Name] = {Info.BorrowedFrom, Info.IsSoulMutable()};
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

  // Clear moved flag. Returns true if found and updated.
  bool resetMoved(const std::string &Name) {
    SymbolInfo *ptr = nullptr;
    if (findSymbol(Name, ptr)) {
      ptr->Moved = false;
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
    bool HasManualDrop = false; // [NEW] Derived from explicit 'drop' impl
    ShapeAnalysisStatus Status = ShapeAnalysisStatus::Unvisited;
  };

  void instantiateGenericImpl(
      ImplDecl *Template, const std::string &ConcreteTypeName,
      const std::vector<std::shared_ptr<toka::Type>> &GenericArgs);

  std::map<std::string, ShapeProperties> m_ShapeProps;

  // Generic Impl Templates (Lazy Instantiation)
  // Key: TypeName (e.g. "Box")
  // Value: Vector of Pointers to the Template ImplDecls (owned by Module)
  std::map<std::string, std::vector<ImplDecl *>> GenericImplMap;

  void analyzeShapes(Module &M);
  void checkShapeSovereignty();
  void computeShapeProperties(const std::string &shapeName, Module &M);

  bool HasError = false;
  uint64_t m_LastInitMask =
      1; // Default to fully initialized (1 for simple var)
  bool m_AllowUnsetUsage = false;
  Scope *CurrentScope = nullptr;
  std::vector<FunctionDecl *>
      GlobalFunctions; // All functions across all modules
  std::map<std::string, ExternDecl *> ExternMap;
  std::map<std::string, ShapeDecl *> ShapeMap;
  struct AliasInfo {
    std::string Target;
    bool IsStrong;
    std::vector<GenericParam> GenericParams; // [NEW]
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
  FunctionDecl *CurrentFunction =
      nullptr; // [NEW] Track current function for dependencies
  std::string m_LastBorrowSource;
  std::set<std::string>
      m_LastLifeDependencies; // [NEW] Track shape dependencies
  std::shared_ptr<toka::Type> m_ExpectedType;
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
  std::string getModuleName(Module *M);

  Module *CurrentModule = nullptr;
  bool m_InUnsafeContext = false;
  bool m_InLHS = false;
  bool m_IsUnsetInitCall = false;     // [NEW] Track .unset() intrinsic
  bool m_DisableSoulCollapse = false; // [NEW] Track context for soul collapse
  bool m_InIntermediatePath =
      false; // [Ch 5] Track if we are in a chain (not leaf)
  bool m_IsAssignmentTarget =
      false; // [Ch 6] Track if we are at the LHS terminal
  TokenType m_OuterPointerSigil =
      TokenType::TokenNone; // [NEW] Track outer pointer sigil for nested member
                            // access

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

  // Path Narrowing
  std::set<std::string> m_NarrowedPaths;

  void error(ASTNode *Node, const std::string &Msg);

  template <typename... Args>
  void error(ASTNode *Node, DiagID ID, Args &&...args) {
    HasError = true;
    DiagnosticEngine::report(Node->Loc, ID, std::forward<Args>(args)...);
  }

  // Scope management
  void enterScope();
  void exitScope();
  void clearStmtBorrows();
  int getScopeDepth(const std::string &
                        Name); // [NEW] Get depth of scope where name is defined

  // Passes
  void registerGlobals(Module &M);
  void checkFunction(FunctionDecl *Fn);
  void registerImpl(ImplDecl *Impl);
  void checkImpl(ImplDecl *Impl);
  void checkStmt(Stmt *S);

  std::string checkUnaryExprStr(UnaryExpr *Unary); // Legacy
  std::shared_ptr<toka::Type>
  checkExprImpl(Expr *E); // New Object API Implementation
  std::shared_ptr<toka::Type>
  checkExpr(Expr *E); // New Object API Wrapper (Annotates AST)
  std::shared_ptr<toka::Type> checkExpr(
      Expr *E,
      std::shared_ptr<toka::Type> expected); // [NEW] Overload for inference
  std::shared_ptr<toka::Type>
  checkUnaryExpr(UnaryExpr *Unary); // New Object API
  std::shared_ptr<toka::Type>
  checkBinaryExpr(BinaryExpr *Bin); // New Object API
  std::shared_ptr<toka::Type> checkMemberExpr(MemberExpr *Memb);
  std::shared_ptr<toka::Type>
  checkIndexExpr(ArrayIndexExpr *Idx);                       // New Object API
  std::shared_ptr<toka::Type> checkCallExpr(CallExpr *Call); // New Object API
  void checkPattern(MatchArm::Pattern *Pat, const std::string &TargetType,
                    bool SourceIsMutable);

  // Decoupled Initialization Helpers
  std::shared_ptr<toka::Type> checkShapeInit(InitStructExpr *Init);
  std::shared_ptr<toka::Type>
  checkStructInit(InitStructExpr *Init, ShapeDecl *SD,
                  const std::string &resolvedName,
                  std::map<std::string, uint64_t> &memberMasks);
  std::shared_ptr<toka::Type>
  checkUnionInit(InitStructExpr *Init, ShapeDecl *SD,
                 const std::string &resolvedName,
                 std::map<std::string, uint64_t> &memberMasks);

  // Control flow helpers
  bool allPathsReturn(Stmt *S);
  bool allPathsJump(Stmt *S);

  // Type system helpers
  bool isLValue(const Expr *expr);
  std::string getCommonType(const std::string &T1, const std::string &T2);

  // Helpers
  std::string resolveType(const std::string &Type, bool force = false);
  std::shared_ptr<toka::Type> resolveType(std::shared_ptr<toka::Type> Type,
                                          bool force = false);
  uint64_t getTypeSize(std::shared_ptr<toka::Type> Type);
  bool checkVisibility(ASTNode *Node, ShapeDecl *SD);
  bool isTypeCompatible(const std::string &Target, const std::string &Source);
  bool isTypeCompatible(std::shared_ptr<toka::Type> Target,
                        std::shared_ptr<toka::Type> Source);

  // [NEW] Deep Inspection for Union Safety
  std::shared_ptr<toka::Type>
  getDeepestUnderlyingType(std::shared_ptr<toka::Type> Type);

  std::shared_ptr<toka::Type>
  instantiateGenericShape(std::shared_ptr<ShapeType> GenericShape);

  FunctionDecl *instantiateGenericFunction(
      FunctionDecl *Template,
      const std::vector<std::shared_ptr<toka::Type>> &Args, CallExpr *CallSite);

  // [NEW] Helper to substitute GenericConst variables with NumberExpr
  std::unique_ptr<Expr> foldGenericConstant(std::unique_ptr<Expr> E);

  // Helper for type synthesis from AST nodes with morphology flags
  // Helper for type synthesis from AST nodes with morphology flags
  template <typename T>
  static std::string synthesizePhysicalType(const T &Arg) {
    std::string Signature = "";

    // 1. Morphologies (Prefix Zone)
    if (Arg.IsUnique) {
      Signature += "^";
    } else if (Arg.IsShared) {
      Signature += "~";
    } else if (Arg.IsReference) {
      Signature += "&";
    } else if (Arg.HasPointer) {
      Signature += "*";
    }

    // 2. Identity Attributes (Prefix Zone)
    if (Arg.IsPointerNullable)
      Signature += "?";
    if (Arg.IsRebindable)
      Signature += "#";
    if (Arg.IsRebindBlocked)
      Signature += "$";

    // 3. Soul Type (Base Name) - Strip everything to avoid
    // double-hatting/double-suffoding
    Signature += toka::Type::stripMorphology(getTypeName(Arg));

    // 4. Soul/Object Attributes (Suffix Zone)
    if (Arg.IsValueNullable)
      Signature += "?";
    if (Arg.IsValueMutable)
      Signature += "#";
    if (Arg.IsValueBlocked)
      Signature += "$";

    return Signature;
  }

  // Specialization for ShapeMember to include morphology flags
  static std::string synthesizePhysicalType(const ShapeMember &Arg) {
    std::string Signature = "";

    // 1. Morphologies (Prefix Zone)
    if (Arg.IsUnique)
      Signature += "^";
    else if (Arg.IsShared)
      Signature += "~";
    else if (Arg.IsReference)
      Signature += "&";
    else if (Arg.HasPointer)
      Signature += "*";

    // 2. Identity Attributes (Prefix Zone)
    if (Arg.IsPointerNullable)
      Signature += "?";
    if (Arg.IsRebindable)
      Signature += "#";
    if (Arg.IsRebindBlocked)
      Signature += "$";

    // 3. Soul Type (Base Name)
    Signature += toka::Type::stripMorphology(Arg.Type);

    // 4. Soul/Object Attributes (Suffix Zone)
    if (Arg.IsValueNullable)
      Signature += "?";
    if (Arg.IsValueMutable)
      Signature += "#";
    if (Arg.IsValueBlocked)
      Signature += "$";

    return Signature;
  }

  // Pointer Morphology Strictness
  enum class MorphKind {
    None,    // No pointer (value type)
    Valid,   // Matches generic valid state (e.g. constructor result)
    Raw,     // *
    Unique,  // ^
    Shared,  // ~
    Ref,     // &
    Address, // & (Synonym for Reference in some contexts, but let's stick to
             // Ref)
    Any      // Wildcard
  };

  MorphKind getSyntacticMorphology(Expr *E);
  bool checkStrictMorphology(ASTNode *Node, MorphKind Target, MorphKind Source,
                             const std::string &TargetName);

private:
  static std::string getTypeName(const FunctionDecl::Arg &A) { return A.Type; }
  static std::string getTypeName(const ExternDecl::Arg &A) { return A.Type; }
  static std::string getTypeName(const VariableDecl &V) { return V.TypeName; }
  static std::string getTypeName(const ShapeMember &M) { return M.Type; }
};

} // namespace toka
