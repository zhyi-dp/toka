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
#include "toka/AST.h"
#include "toka/DiagnosticEngine.h"
#include "toka/Sema.h"
#include "toka/Type.h"
#include <algorithm>
#include <iostream>

namespace toka {

static SourceLocation getLoc(ASTNode *Node) {
  return {Node->FileName.empty() ? "<unknown>" : Node->FileName, Node->Line,
          Node->Column};
}

bool Sema::allPathsReturn(Stmt *S) {
  if (!S)
    return false;
  if (dynamic_cast<ReturnStmt *>(S))
    return true;
  if (auto *B = dynamic_cast<BlockStmt *>(S)) {
    for (const auto &Sub : B->Statements) {
      if (allPathsReturn(Sub.get()))
        return true;
    }
    return false;
  }
  if (auto *Unsafe = dynamic_cast<UnsafeStmt *>(S)) {
    return allPathsReturn(Unsafe->Statement.get());
  }
  // Expressions wrapped in Stmt
  if (auto *ES = dynamic_cast<ExprStmt *>(S)) {
    Expr *E = ES->Expression.get();
    if (auto *If = dynamic_cast<IfExpr *>(E)) {
      if (If->Else && allPathsReturn(If->Then.get()) &&
          allPathsReturn(If->Else.get()))
        return true;
      return false;
    }
    if (auto *Match = dynamic_cast<MatchExpr *>(E)) {
      for (const auto &Arm : Match->Arms) {
        if (!allPathsReturn(Arm->Body.get()))
          return false;
      }
      return true;
    }
    if (auto *Loop = dynamic_cast<LoopExpr *>(E)) {
      if (allPathsReturn(Loop->Body.get()))
        return true;
      return false;
    }
  }
  return false;
}

bool Sema::allPathsJump(Stmt *S) {
  if (!S)
    return false;
  if (dynamic_cast<ReturnStmt *>(S))
    return true;
  if (auto *B = dynamic_cast<BlockStmt *>(S)) {
    for (const auto &Sub : B->Statements) {
      if (allPathsJump(Sub.get()))
        return true;
    }
    return false;
  }
  if (auto *Unsafe = dynamic_cast<UnsafeStmt *>(S)) {
    return allPathsJump(Unsafe->Statement.get());
  }
  if (auto *ES = dynamic_cast<ExprStmt *>(S)) {
    Expr *E = ES->Expression.get();
    if (dynamic_cast<BreakExpr *>(E) || dynamic_cast<ContinueExpr *>(E))
      return true;
    if (auto *If = dynamic_cast<IfExpr *>(E)) {
      if (If->Else && allPathsJump(If->Then.get()) &&
          allPathsJump(If->Else.get()))
        return true;
      return false;
    }
    if (auto *Match = dynamic_cast<MatchExpr *>(E)) {
      for (const auto &Arm : Match->Arms) {
        if (!allPathsJump(Arm->Body.get()))
          return false;
      }
      return true;
    }
    if (auto *Loop = dynamic_cast<LoopExpr *>(E)) {
      // Loop body jumping out counts as jump
      if (allPathsJump(Loop->Body.get()))
        return true;
      return false;
    }
  }
  return false;
}

void Sema::checkStmt(Stmt *S) {
  if (!S)
    return;

  if (auto *Block = dynamic_cast<BlockStmt *>(S)) {
    enterScope();
    for (auto &SubStmt : Block->Statements) {
      checkStmt(SubStmt.get());
    }
    exitScope();
  } else if (auto *Ret = dynamic_cast<ReturnStmt *>(S)) {
    std::string ExprType = "void";
    if (Ret->ReturnValue) {
      m_ControlFlowStack.push_back(
          {"", CurrentFunctionReturnType, false, true});
      auto RetTypeObj = checkExpr(Ret->ReturnValue.get());
      ExprType = RetTypeObj->toString();
      m_ControlFlowStack.pop_back();
    }
    clearStmtBorrows();

    if (!isTypeCompatible(CurrentFunctionReturnType, ExprType)) {
      DiagnosticEngine::report(getLoc(Ret), DiagID::ERR_TYPE_MISMATCH, ExprType,
                               CurrentFunctionReturnType);
      HasError = true;
    }
  } else if (auto *Free = dynamic_cast<FreeStmt *>(S)) {
    auto FreeTypeObj = checkExpr(Free->Expression.get());
    if (!FreeTypeObj->isRawPointer()) {
      std::string ExprType = FreeTypeObj->toString();
      if (FreeTypeObj->isSmartPointer()) {
        DiagnosticEngine::report(getLoc(Free), DiagID::ERR_FREE_SMART,
                                 ExprType);
        HasError = true;
      } else {
        DiagnosticEngine::report(getLoc(Free), DiagID::ERR_FREE_NON_PTR,
                                 ExprType);
        HasError = true;
      }
    }
  } else if (auto *ExprS = dynamic_cast<ExprStmt *>(S)) {
    // Standalone expressions are NOT receivers
    m_ControlFlowStack.push_back({"", "void", false, false});
    checkExpr(ExprS->Expression.get());
    m_ControlFlowStack.pop_back();
    clearStmtBorrows();
  } else if (auto *Var = dynamic_cast<VariableDecl *>(S)) {
    std::string InitType = "";
    if (Var->Init) {
      m_ControlFlowStack.push_back({Var->Name, "void", false, true});
      auto InitTypeObj = checkExpr(Var->Init.get());
      InitType = InitTypeObj->toString();
      m_ControlFlowStack.pop_back();
    }

    // If type not specified, infer from init
    if (Var->TypeName.empty() || Var->TypeName == "auto") {
      if (InitType.empty() || InitType == "void") {
        DiagnosticEngine::report(getLoc(Var), DiagID::ERR_TYPE_REQUIRED,
                                 Var->Name);
        HasError = true;
        Var->TypeName = "unknown";
      } else {
        std::string Inferred = InitType;
        // If the variable declaration HAS morphology, and the initializer
        // ALSO has it, we must strip it from the inferred type to avoid
        // redundancy (e.g. *i32 -> i32)
        if (Inferred == "nullptr" && Var->TypeName.empty()) {
          DiagnosticEngine::report(getLoc(Var), DiagID::ERR_INFER_NULLPTR);
          HasError = true;
          Var->TypeName = "unknown";
          Var->TypeName = "unknown";
        } else {
          std::string Inferred = InitType;
          // If the variable declaration HAS morphology, and the initializer
          // ALSO has it, we must strip it from the inferred type to avoid
          // redundancy (e.g. *i32 -> i32)
          if (Inferred == "nullptr" && Var->TypeName.empty()) {
            DiagnosticEngine::report(
                getLoc(Var), DiagID::ERR_GENERIC_PARSE,
                "Cannot infer type from 'nullptr'. Explicit type annotation "
                "required (e.g., 'auto *p: Data = nullptr').");
            HasError = true;
            Var->TypeName = "unknown";
            return;
          }

          if (Var->HasPointer || Var->IsUnique || Var->IsShared ||
              Var->IsReference) {
            if (!Inferred.empty() &&
                (Inferred[0] == '*' || Inferred[0] == '^' ||
                 Inferred[0] == '~' || Inferred[0] == '&')) {
              Inferred = Inferred.substr(1);
              // Also strip attributes from the inferred prefix
              if (!Inferred.empty() &&
                  (Inferred[0] == '?' || Inferred[0] == '!' ||
                   Inferred[0] == '#')) {
                Inferred = Inferred.substr(1);
              }
            }
          }
          Var->TypeName = Inferred;
        }
      }
    } else {
      // Check compatibility
      std::string DeclFullTy = Var->TypeName;
      std::string Morph = "";
      if (Var->HasPointer)
        Morph = "*";
      else if (Var->IsUnique)
        Morph = "^";
      else if (Var->IsShared)
        Morph = "~";
      else if (Var->IsReference)
        Morph = "&";

      if (!Morph.empty()) {
        if (Var->IsPointerNullable)
          Morph += "?";
        else if (Var->IsRebindable)
          Morph += "!";
        DeclFullTy = Morph + DeclFullTy;
      }

      if (!InitType.empty() && !isTypeCompatible(DeclFullTy, InitType)) {
        DiagnosticEngine::report(getLoc(Var), DiagID::ERR_INIT_TYPE_MISMATCH,
                                 DeclFullTy, InitType);
        HasError = true;
      }
    }

    SymbolInfo Info;

    // Construct Full Type String for Type Object with Legacy Normalization
    std::string morph = "";
    if (Var->HasPointer)
      morph = "*";
    else if (Var->IsUnique)
      morph = "^";
    else if (Var->IsShared)
      morph = "~";
    else if (Var->IsReference)
      morph = "&";

    std::string baseType = Var->TypeName;
    // Legacy behavior: If type implies morphology (e.g. ^Data) and flags
    // imply morphology (IsUnique), we often treat the type string's sigil as
    // the representation of that flag and strip it? Or if flags are set, we
    // PREPEND morphology. The legacy code checked if baseType started with
    // sigils.

    if (baseType.size() > 1 && (baseType[0] == '^' || baseType[0] == '~' ||
                                baseType[0] == '*' || baseType[0] == '&')) {
      if (morph.empty()) {
        morph = std::string(1, baseType[0]);
      }
      // We strip the sigil from base type if we are extracting it to morph,
      // OR if morph is already set (redundancy)
      baseType = baseType.substr(1);
    }

    // Borrow tracking logic using 'morph' local var
    if (morph == "&" && !m_LastBorrowSource.empty()) {
      Info.BorrowedFrom = m_LastBorrowSource;
      // Remove from temporary borrows since it's now persistent
      for (auto it = m_CurrentStmtBorrows.begin();
           it != m_CurrentStmtBorrows.end(); ++it) {
        if (it->first == m_LastBorrowSource) {
          m_CurrentStmtBorrows.erase(it);
          break;
        }
      }
    } else {
      // llvm::errs() << "DEBUG: checkVariableDecl NO_PERSIST Var=" <<
      // Var->Name
      //              << " Morph=" << morph << " LastSrc=" <<
      //              m_LastBorrowSource
      //              << "\n";
    }
    m_LastBorrowSource = ""; // Clear for next var

    // Construct Type Object manually to avoid string ambiguity
    // especially for references to mutable types (e.g. &i32#) being misparsed
    // as mutable references (&i32)#
    auto innerObj = toka::Type::fromString(baseType);

    if (morph == "*") {
      Info.TypeObj = std::make_shared<toka::RawPointerType>(innerObj);
    } else if (morph == "^") {
      Info.TypeObj = std::make_shared<toka::UniquePointerType>(innerObj);
    } else if (morph == "~") {
      Info.TypeObj = std::make_shared<toka::SharedPointerType>(innerObj);
    } else if (morph == "&") {
      Info.TypeObj = std::make_shared<toka::ReferenceType>(innerObj);
    } else {
      Info.TypeObj = innerObj;
    }

    if (Var->IsRebindable || Var->IsValueMutable || Var->IsMutable) {
      Info.TypeObj->IsWritable = true;
    }
    if (Var->IsPointerNullable || Var->IsValueNullable || Var->IsNullable) {
      Info.TypeObj->IsNullable = true;
    }

    // [New] Annotated AST: Populate ResolvedType
    Var->ResolvedType = Info.TypeObj;

    CurrentScope->define(Var->Name, Info);

    // Move Logic: If initializing from a Unique Variable, move it.
    if (Var->Init && Info.IsUnique()) {
      Expr *InitExpr = Var->Init.get();
      // Unwrap unary ^ or ~ or * if it matches
      if (auto *Unary = dynamic_cast<UnaryExpr *>(InitExpr)) {
        InitExpr = Unary->RHS.get();
      }

      if (auto *RHSVar = dynamic_cast<VariableExpr *>(InitExpr)) {
        SymbolInfo *SourceInfoPtr = nullptr;
        if (CurrentScope->findSymbol(RHSVar->Name, SourceInfoPtr)) {
          if (SourceInfoPtr->IsUnique()) {
            if (SourceInfoPtr->IsMutablyBorrowed ||
                SourceInfoPtr->ImmutableBorrowCount > 0) {
              DiagnosticEngine::report(getLoc(Var), DiagID::ERR_MOVE_BORROWED,
                                       RHSVar->Name);
              HasError = true;
            }
            CurrentScope->markMoved(RHSVar->Name);
          }
        }
      }
    }
    clearStmtBorrows();
  } else if (auto *Destruct = dynamic_cast<DestructuringDecl *>(S)) {
    // Stage 1: Resolve Types using the new Type system
    auto initType = checkExpr(Destruct->Init.get());
    auto declType = toka::Type::fromString(Destruct->TypeName);

    // Basic check: declType should match initType
    if (initType->toString() != "unknown" && initType->toString() != "tuple" &&
        !isTypeCompatible(declType, initType)) {
      DiagnosticEngine::report(getLoc(Destruct), DiagID::ERR_TYPE_MISMATCH,
                               initType->toString(), declType->toString());
      HasError = true;
    }

    std::string soulName = Type::stripMorphology(Destruct->TypeName);
    if (ShapeMap.count(soulName)) {
      ShapeDecl *SD = ShapeMap[soulName];
      size_t Limit = std::min(SD->Members.size(), Destruct->Variables.size());
      for (size_t i = 0; i < Limit; ++i) {
        SymbolInfo Info;
        // Simple type for destructuring vars (usually primitives or basic
        // shapes)
        std::string fullType = SD->Members[i].Type;
        if (Destruct->Variables[i].IsMutable)
          fullType += "#";
        if (Destruct->Variables[i].IsNullable)
          fullType += "?";
        Info.TypeObj = toka::Type::fromString(fullType);

        CurrentScope->define(Destruct->Variables[i].Name, Info);
      }
    } else if (TypeAliasMap.count(soulName)) {
      for (const auto &Var : Destruct->Variables) {
        SymbolInfo Info;
        std::string fullType = "i32"; // Fallback
        if (Var.IsMutable)
          fullType += "#";
        if (Var.IsNullable)
          fullType += "?";
        Info.TypeObj = toka::Type::fromString(fullType);

        CurrentScope->define(Var.Name, Info);
      }
    } else {
      for (const auto &Var : Destruct->Variables) {
        SymbolInfo Info;
        Info.TypeObj = toka::Type::fromString("unknown");
        CurrentScope->define(Var.Name, Info);
        CurrentScope->define(Var.Name, Info);
      }
    }
  }
}

} // namespace toka
