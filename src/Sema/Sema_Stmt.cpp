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

static SourceLocation getLoc(ASTNode *Node) { return Node->Loc; }

bool Sema::allPathsReturn(Stmt *S) {
  if (!S)
    return false;
  if (dynamic_cast<ReturnStmt *>(S) || dynamic_cast<UnreachableStmt *>(S))
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
  if (dynamic_cast<ReturnStmt *>(S) || dynamic_cast<UnreachableStmt *>(S))
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

    // SCOPE GUARD: Hot Potato Check
    // Iterate over symbols in the current scope before exiting.
    // If any symbol is a Reference with DirtyReferentMask != Full,
    // we must ensure its Referent (BorrowedFrom) is now Clean.
    // However, the Referent might be in a parent scope. We need to check the
    // Referent's CURRENT state. Wait, simpler model first: If the Ref is marked
    // Dirty, it means it TOOK responsibility. We check if the Ref *itself*
    // thinks it's done? No, the Ref doesn't update its own DirtyMask
    // automatically unless we implement flow sensitive updates to
    // DirtyReferentMask on assignment. BETTER APPROACH based on plan: "Check if
    // the referent (Source) has been fully initialized (Cleaned) within this
    // scope."

    // We need to iterate the Symbols in CurrentScope.
    for (auto const &[name, info] : CurrentScope->Symbols) {
      if (info.IsReference() && info.DirtyReferentMask != ~0ULL) {
        // It was a dirty reference.
        // Check if it's still dirty?
        // Actually, we should check the SOURCE variable's current InitMask.
        SymbolInfo *sourceInfo = nullptr;
        if (!info.BorrowedFrom.empty() &&
            CurrentScope->findSymbol(info.BorrowedFrom, sourceInfo)) {
          bool referentIsShape =
              (sourceInfo->TypeObj && sourceInfo->TypeObj->isShape());
          uint64_t signature = ~0ULL;
          if (referentIsShape) {
            std::string soul = sourceInfo->TypeObj->getSoulName();
            if (ShapeMap.count(soul)) {
              ShapeDecl *SD = ShapeMap[soul];
              signature = (1ULL << SD->Members.size()) - 1;
              if (SD->Members.size() >= 64)
                signature = ~0ULL;
            }
          }

          // Check if Source is now fully initialized
          if ((sourceInfo->InitMask & signature) != signature) {
            DiagnosticEngine::report(getLoc(Block),
                                     DiagID::ERR_DIRTY_REF_ESCAPE, name,
                                     info.BorrowedFrom);
            HasError = true;
          }
        }
      }
    }

    exitScope();
  } else if (auto *Ret = dynamic_cast<ReturnStmt *>(S)) {
    std::string ExprType = "void";
    if (Ret->ReturnValue) {
      Ret->ReturnValue = foldGenericConstant(std::move(Ret->ReturnValue));
      m_ControlFlowStack.push_back(
          {"", CurrentFunctionReturnType, false, true});
      auto RetTypeObj = checkExpr(Ret->ReturnValue.get());
      ExprType = RetTypeObj->toString();
      m_ControlFlowStack.pop_back();

      // Escape Blockade: Check for Dirty Reference
      if (auto *Var = dynamic_cast<VariableExpr *>(Ret->ReturnValue.get())) {
        SymbolInfo *info = nullptr;
        if (CurrentScope->findSymbol(Var->Name, info)) {
          if (info->IsReference() && info->DirtyReferentMask != ~0ULL) {
            DiagnosticEngine::report(getLoc(Ret), DiagID::ERR_ESCAPE_UNSET,
                                     Var->Name);
            HasError = true;
          }
        }
      }

      // [NEW] Lifetime Union Exemption & Escape Analysis
      if (auto *AddrOf = dynamic_cast<UnaryExpr *>(Ret->ReturnValue.get())) {
        if (AddrOf->Op == TokenType::Ampersand) {
          if (auto *Var = dynamic_cast<VariableExpr *>(AddrOf->RHS.get())) {
            bool isParam = false;
            if (CurrentFunction) {
              for (const auto &Arg : CurrentFunction->Args) {
                if (Arg.Name == Var->Name) {
                  isParam = true;
                  break;
                }
              }
            }

            if (isParam) {
              // It's a parameter. Is it in LifeDependencies?
              bool isDependent = false;
              if (CurrentFunction) {
                for (const auto &dep : CurrentFunction->LifeDependencies) {
                  if (dep == Var->Name) {
                    isDependent = true;
                    break;
                  }
                }
              }

              if (!isDependent) {
                // Return of parameter address NOT in LifeDependencies is
                // UNSAFE (Escaping Local)
                DiagnosticEngine::report(getLoc(Ret),
                                         DiagID::ERR_LIFETIME_UNION_REQUIRED,
                                         Var->Name, Var->Name);
                HasError = true;
              }
            } else {
              // It's a local variable (not a parameter). Returning its address
              // is ALWAYS unsafe.
              DiagnosticEngine::report(getLoc(Ret), DiagID::ERR_ESCAPE_LOCAL,
                                       Var->Name);
              HasError = true;
            }
          }
        }
      }
    }
    clearStmtBorrows();

    if (!isTypeCompatible(CurrentFunctionReturnType, ExprType)) {
      DiagnosticEngine::report(getLoc(Ret), DiagID::ERR_TYPE_MISMATCH, ExprType,
                               CurrentFunctionReturnType);
      HasError = true;
    } else {
      // Strict Morphology Check for Return
      MorphKind targetMorph = MorphKind::None;
      if (!CurrentFunctionReturnType.empty()) {
        char c = CurrentFunctionReturnType[0];
        if (c == '*')
          targetMorph = MorphKind::Raw;
        else if (c == '^')
          targetMorph = MorphKind::Unique;
        else if (c == '~')
          targetMorph = MorphKind::Shared;
        else if (c == '&')
          targetMorph = MorphKind::Ref;
      }
      MorphKind sourceMorph = getSyntacticMorphology(Ret->ReturnValue.get());
      checkStrictMorphology(Ret, targetMorph, sourceMorph, "return value");
    }
  } else if (auto *Free = dynamic_cast<FreeStmt *>(S)) {
    Free->Expression = foldGenericConstant(std::move(Free->Expression));
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
  } else if (auto *Unsafe = dynamic_cast<UnsafeStmt *>(S)) {
    bool oldUnsafe = m_InUnsafeContext;
    m_InUnsafeContext = true;
    checkStmt(Unsafe->Statement.get());
    m_InUnsafeContext = oldUnsafe;
  } else if (auto *ExprS = dynamic_cast<ExprStmt *>(S)) {
    // Standalone expressions are NOT receivers
    m_ControlFlowStack.push_back({"", "void", false, false});
    ExprS->Expression = foldGenericConstant(std::move(ExprS->Expression));
    checkExpr(ExprS->Expression.get());
    m_ControlFlowStack.pop_back();
    clearStmtBorrows();
  } else if (auto *Var = dynamic_cast<VariableDecl *>(S)) {
    // [Constitutional 1.3] Adversarial Principle: $ is only for contesting
    // inheritance.
    if (Var->IsValueBlocked || Var->IsRebindBlocked) {
      DiagnosticEngine::report(getLoc(Var), DiagID::ERR_REDUNDANT_BLOCK,
                               Var->Name);
      HasError = true;
    }

    std::string InitType = "";
    std::shared_ptr<toka::Type> InitTypeObj = nullptr;
    if (Var->Init) {
      Var->Init = foldGenericConstant(std::move(Var->Init));
      if (Var->IsReference)
        m_AllowUnsetUsage = true;
      m_ControlFlowStack.push_back({Var->Name, "void", false, true});
      InitTypeObj = checkExpr(Var->Init.get());
      InitType = InitTypeObj->toString();
      m_AllowUnsetUsage = false;
    }

    // 4. If type not specified, infer from init
    if (Var->TypeName.empty() || Var->TypeName == "auto") {
      if (InitType.empty() || InitType == "void") {
        DiagnosticEngine::report(getLoc(Var), DiagID::ERR_TYPE_REQUIRED,
                                 Var->Name);
        HasError = true;
        Var->TypeName = "unknown";
      } else {
        std::string Inferred = InitType;
        if (Inferred == "nullptr") {
          DiagnosticEngine::report(getLoc(Var), DiagID::ERR_INFER_NULLPTR);
          HasError = true;
          Var->TypeName = "unknown";
          return;
        }

        // If variable declares morphology (auto ^p = ...), strip matching
        // morphology from inferred soul
        if (Var->HasPointer || Var->IsUnique || Var->IsShared ||
            Var->IsReference) {
          if (!Inferred.empty() && (Inferred[0] == '*' || Inferred[0] == '^' ||
                                    Inferred[0] == '~' || Inferred[0] == '&')) {
            Inferred = Inferred.substr(1);
            if (!Inferred.empty() && (Inferred[0] == '?' ||
                                      Inferred[0] == '!' || Inferred[0] == '#'))
              Inferred = Inferred.substr(1);
          }
        } else {
          // Strict: No sigil, no pointer, except for "str" (implicit *char)
          if (!Inferred.empty() &&
              (Inferred[0] == '*' || Inferred[0] == '^' || Inferred[0] == '~' ||
               Inferred[0] == '&') &&
              Inferred.substr(0, 3) != "str") {
            std::string sigilStr = std::string(1, Inferred[0]);
            DiagnosticEngine::report(getLoc(Var),
                                     DiagID::ERR_POINTER_SIGIL_MISSING,
                                     Var->Name, Inferred, sigilStr, Var->Name);
            HasError = true;
            Var->TypeName = "unknown";
            return;
          }
        }
        Var->TypeName = Inferred;
      }
    } else {
      // Compatibility Check
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

    // 5. Strict Morphology Check
    if (Var->Init && !Var->IsReference) {
      MorphKind lhsMorph = MorphKind::None;
      if (Var->IsUnique)
        lhsMorph = MorphKind::Unique;
      else if (Var->IsShared)
        lhsMorph = MorphKind::Shared;
      else if (Var->IsReference)
        lhsMorph = MorphKind::Ref;
      else if (Var->HasPointer)
        lhsMorph = MorphKind::Raw;

      MorphKind rhsMorph = getSyntacticMorphology(Var->Init.get());
      checkStrictMorphology(Var, lhsMorph, rhsMorph, Var->Name);
    }

    SymbolInfo Info;
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
    // Strip redundant sigil from baseType if it matches morph
    if (baseType.size() > 1 && (baseType[0] == '^' || baseType[0] == '~' ||
                                baseType[0] == '*' || baseType[0] == '&')) {
      if (morph.empty()) {
        morph = std::string(1, baseType[0]);
        baseType = baseType.substr(1);
      } else if (morph[0] == baseType[0]) {
        baseType = baseType.substr(1);
      }
    }
    if (morph == "&" && !m_LastBorrowSource.empty()) {
      Info.BorrowedFrom = m_LastBorrowSource;
      Info.LifeDependencySet.insert(m_LastBorrowSource);

      // [NEW] Lifetime check: Depth(Me) >= Depth(Src)
      int srcDepth = getScopeDepth(m_LastBorrowSource);
      int myDepth = CurrentScope->Depth;
      if (myDepth < srcDepth) {
        DiagnosticEngine::report(getLoc(Var), DiagID::ERR_BORROW_LIFETIME,
                                 Var->Name, m_LastBorrowSource);
        HasError = true;
      }

      // [Hot Potato] Propagate InitMask from Source to Reference
      SymbolInfo *srcPtr = nullptr;
      if (CurrentScope->findSymbol(m_LastBorrowSource, srcPtr)) {
        // If source is not fully initialized, mark this reference as holding a
        // Hot Potato We use the same 'signature' logic as elsewhere to
        // determine "Full"
        uint64_t fullMask = ~0ULL;
        if (srcPtr->TypeObj && srcPtr->TypeObj->isShape()) {
          std::string soul = srcPtr->TypeObj->getSoulName();
          if (ShapeMap.count(soul)) {
            ShapeDecl *SD = ShapeMap[soul];
            uint64_t bits = (1ULL << SD->Members.size()) - 1;
            if (SD->Members.size() >= 64)
              bits = ~0ULL;
            fullMask = bits;
          }
        }

        if ((srcPtr->InitMask & fullMask) != fullMask) {
          // It's Dirty!
          Info.DirtyReferentMask = srcPtr->InitMask;
        } else {
          Info.DirtyReferentMask = ~0ULL; // Clean
        }
      }

      // [NEW] Register in ActiveBorrows for scope-based cleanup
      bool isMutFromExpr = false;
      for (auto const &b : m_CurrentStmtBorrows) {
        if (b.first == m_LastBorrowSource) {
          isMutFromExpr = b.second;
          break;
        }
      }
      bool isMut = isMutFromExpr || Var->IsValueMutable || Var->IsRebindable;

      // [Reconcile] Registration in ActiveBorrows is sufficient.
      // Borrow counts are handled by checkExpr during traversal,
      // and cleaned up by exitScope using ActiveBorrows metadata.
      Info.BorrowedFrom = m_LastBorrowSource;
      CurrentScope->ActiveBorrows[Var->Name] = {m_LastBorrowSource, isMut};

      // Remove from temporary borrows since it's now persistent
      for (auto it = m_CurrentStmtBorrows.begin();
           it != m_CurrentStmtBorrows.end(); ++it) {
        if (it->first == m_LastBorrowSource) {
          m_CurrentStmtBorrows.erase(it);
          break;
        }
      }
    }

    if (!m_LastLifeDependencies.empty()) {
      for (const auto &dep : m_LastLifeDependencies) {
        Info.LifeDependencySet.insert(dep);
        int srcDepth = getScopeDepth(dep);
        int myDepth = CurrentScope->Depth;
        if (myDepth < srcDepth) {
          DiagnosticEngine::report(getLoc(Var), DiagID::ERR_BORROW_LIFETIME,
                                   Var->Name, dep);
          HasError = true;
        }
      }
      m_LastLifeDependencies.clear();
    }

    m_LastBorrowSource = ""; // Clear for next var

    // [Constitution 1.3] Dual-Attribute Synthesis
    std::string fullType = "";
    // 1. Morphology sigil (Constitutional 1.3 - Leading)
    fullType += morph;

    // 2. Identity/Handle prefix attributes
    if (Var->IsPointerNullable)
      fullType += "?";
    if (Var->IsRebindable)
      fullType += "#";

    // 3. Base Type Name
    fullType += baseType;

    // 4. Soul/Value suffix attributes
    if (Var->IsValueNullable)
      fullType += "?";
    if (Var->IsValueMutable)
      fullType += "#";

    Info.TypeObj = toka::Type::fromString(fullType);
    Info.IsRebindable = Var->IsRebindable;
    Var->ResolvedType = Info.TypeObj;

    if (Var->Init) {
      Info.InitMask = m_LastInitMask;
    } else {
      Info.InitMask = 0;
    }

    // Rule: Numeric Substitution (Constant variables)
    if (Var->Init && Var->TypeName == "i32" && !Var->IsValueMutable) {
      if (auto *Num = dynamic_cast<NumberExpr *>(Var->Init.get())) {
        Info.HasConstValue = true;
        Info.ConstValue = Num->Value;
      }
      // Or if initialized with ANOTHER const variable (like N = M)
      else if (auto *RefVar = dynamic_cast<VariableExpr *>(Var->Init.get())) {
        SymbolInfo RefInfo;
        if (CurrentScope->lookup(RefVar->Name, RefInfo) &&
            RefInfo.HasConstValue) {
          Info.HasConstValue = true;
          Info.ConstValue = RefInfo.ConstValue;
        }
      }
    }

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
      } else if (auto *Memb = dynamic_cast<MemberExpr *>(InitExpr)) {
        // [Move Restriction Rule] Prohibit moving member out of shape that
        // has drop() Rule ONLY applies if we are moving a resource
        // (UniquePtr)
        if (InitTypeObj->isUniquePtr()) {
          auto objType = checkExpr(Memb->Object.get());
          std::shared_ptr<toka::Type> soulType = objType->getSoulType();
          std::string soul = soulType->getSoulName();
          if (m_ShapeProps.count(soul) && m_ShapeProps[soul].HasDrop) {
            error(Var, DiagID::ERR_MOVE_MEMBER_DROP, Memb->Member, soul);
          }
        }
      }
    }
    clearStmtBorrows();
    if (Var->Init) {
      m_ControlFlowStack.pop_back();
    }
  } else if (auto *Destruct = dynamic_cast<DestructuringDecl *>(S)) {
    // Stage 1: Resolve Types using the new Type system
    auto initType = checkExpr(Destruct->Init.get());
    auto declType = toka::Type::fromString(Destruct->TypeName);

    // Basic check: declType should match initType
    if (!Destruct->TypeName.empty() && initType->toString() != "unknown" &&
        initType->toString() != "tuple" &&
        !isTypeCompatible(declType, initType)) {
      DiagnosticEngine::report(getLoc(Destruct), DiagID::ERR_TYPE_MISMATCH,
                               initType->toString(), declType->toString());
      HasError = true;
    }

    std::string soulName = Type::stripMorphology(Destruct->TypeName);
    if (!soulName.empty() && ShapeMap.count(soulName)) {
      ShapeDecl *SD = ShapeMap[soulName];
      size_t Limit = std::min(SD->Members.size(), Destruct->Variables.size());
      for (size_t i = 0; i < Limit; ++i) {
        SymbolInfo Info;
        std::string fullType = SD->Members[i].Type;
        auto baseTypeObj = toka::Type::fromString(fullType);
        // Apply attributes to the SOUL
        auto soulType =
            baseTypeObj->withAttributes(Destruct->Variables[i].IsValueMutable,
                                        Destruct->Variables[i].IsValueNullable,
                                        Destruct->Variables[i].IsValueBlocked);

        if (Destruct->Variables[i].IsReference) {
          Info.TypeObj = std::make_shared<toka::ReferenceType>(soulType);
          Info.BorrowedFrom = m_LastBorrowSource;
          // Register in ActiveBorrows
          bool isMutFromExpr = false;
          for (auto const &b : m_CurrentStmtBorrows) {
            if (b.first == m_LastBorrowSource) {
              isMutFromExpr = b.second;
              break;
            }
          }
          bool isMut = isMutFromExpr || Destruct->Variables[i].IsValueMutable;
          CurrentScope->ActiveBorrows[Destruct->Variables[i].Name] = {
              m_LastBorrowSource, isMut};
        } else {
          Info.TypeObj = soulType;
        }
        CurrentScope->define(Destruct->Variables[i].Name, Info);
      }
    } else if (initType->typeKind == Type::Tuple) {
      auto tt = std::dynamic_pointer_cast<TupleType>(initType);
      size_t Limit = std::min(tt->Elements.size(), Destruct->Variables.size());
      for (size_t i = 0; i < Limit; ++i) {
        SymbolInfo Info;
        auto memType = tt->Elements[i];
        // Apply attributes to the SOUL
        auto soulType =
            memType->withAttributes(Destruct->Variables[i].IsValueMutable,
                                    Destruct->Variables[i].IsValueNullable,
                                    Destruct->Variables[i].IsValueBlocked);

        if (Destruct->Variables[i].IsReference) {
          Info.TypeObj = std::make_shared<toka::ReferenceType>(soulType);
          Info.BorrowedFrom = m_LastBorrowSource;
          // Register in ActiveBorrows
          bool isMutFromExpr = false;
          for (auto const &b : m_CurrentStmtBorrows) {
            if (b.first == m_LastBorrowSource) {
              isMutFromExpr = b.second;
              break;
            }
          }
          bool isMut = isMutFromExpr || Destruct->Variables[i].IsValueMutable;
          CurrentScope->ActiveBorrows[Destruct->Variables[i].Name] = {
              m_LastBorrowSource, isMut};
        } else {
          Info.TypeObj = soulType;
        }
        CurrentScope->define(Destruct->Variables[i].Name, Info);
      }
    } else if (!soulName.empty() && TypeAliasMap.count(soulName)) {
      for (const auto &Var : Destruct->Variables) {
        SymbolInfo Info;
        std::string fullType = "i32"; // Fallback
        auto baseTypeObj = toka::Type::fromString(fullType);
        auto soulType = baseTypeObj->withAttributes(
            Var.IsValueMutable, Var.IsValueNullable, Var.IsValueBlocked);
        if (Var.IsReference) {
          Info.TypeObj = std::make_shared<toka::ReferenceType>(soulType);
          Info.BorrowedFrom = m_LastBorrowSource;
          // Register in ActiveBorrows
          bool isMutFromExpr = false;
          for (auto const &b : m_CurrentStmtBorrows) {
            if (b.first == m_LastBorrowSource) {
              isMutFromExpr = b.second;
              break;
            }
          }
          bool isMut = isMutFromExpr || Var.IsValueMutable;
          CurrentScope->ActiveBorrows[Var.Name] = {m_LastBorrowSource, isMut};
        } else {
          Info.TypeObj = soulType;
        }
        CurrentScope->define(Var.Name, Info);
      }
    } else {
      for (const auto &Var : Destruct->Variables) {
        SymbolInfo Info;
        Info.TypeObj = toka::Type::fromString("unknown");
        CurrentScope->define(Var.Name, Info);
      }
    }
  } else if (auto *Unreachable = dynamic_cast<UnreachableStmt *>(S)) {
    // No-op for now, it's just a marker
  }
}

} // namespace toka
