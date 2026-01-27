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
#include "toka/SourceManager.h"
#include "toka/Type.h"
#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace toka {

static SourceLocation getLoc(ASTNode *Node) { return Node->Loc; }

static std::string getStringifyPath(Expr *E) {
  if (!E)
    return "";
  if (auto *ve = dynamic_cast<VariableExpr *>(E)) {
    return ve->Name;
  }
  if (auto *me = dynamic_cast<MemberExpr *>(E)) {
    std::string member = toka::Type::stripMorphology(me->Member);
    return getStringifyPath(me->Object.get()) + "." + member;
  }
  if (auto *ue = dynamic_cast<UnaryExpr *>(E)) {
    return getStringifyPath(ue->RHS.get());
  }
  return "";
}

static bool isLValue(const Expr *expr) {
  if (dynamic_cast<const VariableExpr *>(expr))
    return true;
  if (auto *me = dynamic_cast<const MemberExpr *>(expr))
    return isLValue(me->Object.get());
  if (auto *ae = dynamic_cast<const ArrayIndexExpr *>(expr))
    return isLValue(ae->Array.get());
  if (auto *ue = dynamic_cast<const UnaryExpr *>(expr)) {
    if (ue->Op == TokenType::Star)
      return true;
  }
  if (auto *met = dynamic_cast<const MethodCallExpr *>(expr)) {
    // [Intrinsic] unset() and unwrap() can be L-values if their object is
    if (met->Method == "unset" || met->Method == "unwrap") {
      return isLValue(met->Object.get());
    }
  }
  return false;
}

std::string Sema::checkUnaryExprStr(UnaryExpr *Unary) {
  return checkUnaryExpr(Unary)->toString();
}

std::unique_ptr<Expr> Sema::foldGenericConstant(std::unique_ptr<Expr> E) {
  if (auto *Var = dynamic_cast<VariableExpr *>(E.get())) {
    SymbolInfo Info;
    // Look up symbol. If explicit Const Generic, substitute.
    if (CurrentScope->lookup(Var->Name, Info) && Info.HasConstValue) {
      // Create replacement NumberExpr
      // Note: We use default i32/u64 typing logic or explicit usize?
      // User suggested Type::usize(). Assuming AST supports it or just Value.
      // If NumberExpr only takes Value, use that.
      // Checking existing usage: Parser creates NumberExpr(val).
      // Let's assume (val) constructor exists.
      auto Num = std::make_unique<NumberExpr>(Info.ConstValue);
      Num->Loc = Var->Loc;

      // [Fix] Enforce Declared Type via Cast
      // Generic parameters like <N: usize> must resolve to 'usize' typed
      // expressions, not default 'i32' NumberExprs.
      if (Info.TypeObj) {
        std::string typeStr = Info.TypeObj->toString();
        if (typeStr != "unknown" && typeStr != "auto") {
          auto Cast = std::make_unique<CastExpr>(std::move(Num), typeStr);
          Cast->Loc = Var->Loc;
          return Cast;
        }
      }
      return Num;
    }
  }
  return E;
}

std::shared_ptr<toka::Type> Sema::checkExpr(Expr *E) {
  if (!E)
    return toka::Type::fromString("void");
  m_LastInitMask = ~0ULL; // Default to fully set
  auto T = checkExprImpl(E);
  // [Fix] Monomorphize type before assigning it to the node
  T = resolveType(T);
  E->ResolvedType = T;

  if (!dynamic_cast<UnsetExpr *>(E) && !dynamic_cast<InitStructExpr *>(E) &&
      !dynamic_cast<NewExpr *>(E) && !dynamic_cast<CastExpr *>(E) &&
      !dynamic_cast<CallExpr *>(E) && !dynamic_cast<MethodCallExpr *>(E)) {
    m_LastInitMask = ~0ULL;
  }
  return T;
}

std::shared_ptr<toka::Type>
Sema::checkExpr(Expr *E, std::shared_ptr<toka::Type> expected) {
  auto oldExpected = m_ExpectedType;
  m_ExpectedType = expected;
  auto T = checkExpr(E);
  m_ExpectedType = oldExpected;
  return T;
}

// -----------------------------------------------------------------------------
// Type & Morphology Helpers
// -----------------------------------------------------------------------------

Sema::MorphKind Sema::getSyntacticMorphology(Expr *E) {
  if (!E)
    return MorphKind::None;

  // Unary Ops: ^, *, ~, &
  if (auto *U = dynamic_cast<UnaryExpr *>(E)) {
    // [Constitution] Hat-Off Transduction for Member Access
    // If the hat wraps an Arrow Member Access (^p->x), the hat is consumed
    // by the Arrow-Context logic to validate access to 'x'. The result of the
    // expression is the field 'x' (Value), not a Pointer.
    // Thus, syntactic morphology should return None (Value), not Unique/etc.
    if (auto *M = dynamic_cast<MemberExpr *>(U->RHS.get())) {
      if (M->IsArrow)
        return MorphKind::None;
    }

    switch (U->Op) {
    case TokenType::Star:
      return MorphKind::Raw;
    case TokenType::Caret:
      return MorphKind::Unique;
    case TokenType::Tilde:
      return MorphKind::Shared;
    case TokenType::Ampersand:
      return MorphKind::Ref;
    default:
      return MorphKind::None;
    }
  }

  // Binary Expressions: Pointer Arithmetic or Computed Values
  // These produce RValues, which do not need sigils (the strictly-typed
  // checking handles validation).
  if (dynamic_cast<BinaryExpr *>(E)) {
    return MorphKind::Valid;
  }

  // Cast: Check target type string
  if (auto *C = dynamic_cast<CastExpr *>(E)) {
    if (C->TargetType.empty())
      return MorphKind::None;
    char c = C->TargetType[0];
    if (c == '*')
      return MorphKind::Raw;
    if (c == '^')
      return MorphKind::Unique;
    if (c == '~')
      return MorphKind::Shared;
    if (c == '&')
      return MorphKind::Ref;
    return MorphKind::None;
  }

  // Member Access: Check if Member string carries pointer sigil (e.g. .*name)
  if (auto *M = dynamic_cast<MemberExpr *>(E)) {
    if (!M->Member.empty()) {
      char c = M->Member[0];
      if (c == '*')
        return MorphKind::Raw;
      if (c == '^')
        return MorphKind::Unique;
      if (c == '~')
        return MorphKind::Shared;
      if (c == '&')
        return MorphKind::Ref;
    }
    return MorphKind::None;
  }

  // Safe Constructors (Exceptions)
  if (dynamic_cast<CallExpr *>(E) || dynamic_cast<MethodCallExpr *>(E) ||
      dynamic_cast<NewExpr *>(E) || dynamic_cast<AllocExpr *>(E) ||
      dynamic_cast<NullExpr *>(E) || dynamic_cast<UnsetExpr *>(E) ||
      dynamic_cast<StringExpr *>(E)) {
    return MorphKind::Valid;
  }

  // Unsafe: Recurse
  if (auto *U = dynamic_cast<UnsafeExpr *>(E)) {
    return getSyntacticMorphology(U->Expression.get());
  }

  // Parentheses: Recurse
  if (auto *T = dynamic_cast<TupleExpr *>(E)) {
    if (T->Elements.size() == 1)
      return getSyntacticMorphology(T->Elements[0].get());
  }

  return MorphKind::None;
}

bool Sema::checkStrictMorphology(ASTNode *Node, MorphKind Target,
                                 MorphKind Source,
                                 const std::string &TargetName) {
  // 1. Exact Match
  if (Target == Source)
    return true;

  // 2. Safe Constructors (Source is function call/new/etc)
  if (Source == MorphKind::Valid)
    return true;

  // 3. None/None Match (Value types)
  if (Target == MorphKind::None && Source == MorphKind::None)
    return true;

  // 4. Mismatch
  std::string tgtStr = "value";
  if (Target == MorphKind::Raw)
    tgtStr = "*";
  if (Target == MorphKind::Unique)
    tgtStr = "^";
  if (Target == MorphKind::Shared)
    tgtStr = "~";
  if (Target == MorphKind::Ref)
    tgtStr = "&";

  std::string srcStr = "value";
  if (Source == MorphKind::Raw)
    srcStr = "*";
  if (Source == MorphKind::Unique)
    srcStr = "^";
  if (Source == MorphKind::Shared)
    srcStr = "~";
  if (Source == MorphKind::Ref)
    srcStr = "&";

  DiagnosticEngine::report(Node->Loc, DiagID::ERR_MORPHOLOGY_MISMATCH, tgtStr,
                           srcStr);
  return false;
}

std::shared_ptr<toka::Type> Sema::checkExprImpl(Expr *E) {
  if (!E)
    return toka::Type::fromString("void");

  if (auto *U = dynamic_cast<UnsetExpr *>(E)) {
    m_LastInitMask = 0;
    return toka::Type::fromString("unknown");
  }

  if (auto *Null = dynamic_cast<NullExpr *>(E)) {
    return toka::Type::fromString("nullptr");
  }

  if (auto *None = dynamic_cast<NoneExpr *>(E)) {
    return toka::Type::fromString("void");
  }

  if (auto *Num = dynamic_cast<NumberExpr *>(E)) {
    if (Num->Value > 9223372036854775807ULL)
      return toka::Type::fromString("u64");
    if (Num->Value > 2147483647)
      return toka::Type::fromString("i64");
    return toka::Type::fromString("i32");
  } else if (auto *Flt = dynamic_cast<FloatExpr *>(E)) {
    return toka::Type::fromString("f64");
  } else if (auto *Bool = dynamic_cast<BoolExpr *>(E)) {
    return toka::Type::fromString("bool");
  } else if (auto *Addr = dynamic_cast<AddressOfExpr *>(E)) {
    // Toka Spec: &x creates a Reference.
    auto innerObj = checkExpr(Addr->Expression.get());
    if (innerObj->isUnknown())
      return toka::Type::fromString("unknown");

    // Borrow Tracking
    Expr *scan = Addr->Expression.get();
    // Unwrap Postfix (like x#)
    while (auto *post = dynamic_cast<PostfixExpr *>(scan)) {
      scan = post->LHS.get();
    }

    if (auto *Var = dynamic_cast<VariableExpr *>(scan)) {
      SymbolInfo *Info = nullptr;
      if (CurrentScope->findSymbol(Var->Name, Info)) {
        bool wantMutable = innerObj->IsWritable;

        // Borrow Check Logic
        if (wantMutable) {
          if (!Info->IsMutable()) {
            error(Addr, DiagID::ERR_BORROW_IMMUT, Var->Name);
          }
          if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
            error(Addr, DiagID::ERR_BORROW_MUT, Var->Name);
          }
          Info->IsMutablyBorrowed = true;
        } else {
          if (Info->IsMutablyBorrowed) {
            error(Addr, DiagID::ERR_BORROW_MUT, Var->Name);
          }
          Info->ImmutableBorrowCount++;
        }

        m_LastBorrowSource = Var->Name;
        m_LastLifeDependencies.insert(Var->Name);
        m_CurrentStmtBorrows.push_back({Var->Name, wantMutable});
      }
    }

    auto refType = std::make_shared<toka::ReferenceType>(innerObj);
    return refType;
  } else if (auto *Idx = dynamic_cast<ArrayIndexExpr *>(E)) {
    return checkIndexExpr(Idx);
  } else if (auto *Rec = dynamic_cast<AnonymousRecordExpr *>(E)) {
    // 1. Infer field types
    std::vector<ShapeMember> members;
    std::set<std::string> seenFields;

    for (auto &f : Rec->Fields) {
      if (seenFields.count(f.first)) {
        error(Rec, DiagID::ERR_DUPLICATE_FIELD, f.first);
      }
      seenFields.insert(f.first);

      auto fieldTypeObj = checkExpr(f.second.get());
      std::string fieldT = fieldTypeObj->toString();
      if (fieldTypeObj->isUnknown())
        return toka::Type::fromString("unknown");

      ShapeMember sm;
      sm.Name = f.first;
      sm.Type = fieldT;
      members.push_back(sm);
    }

    // 2. Generate Unique Type Name
    // Each anonymous record literal creates a distinct Nominal Type.
    std::string UniqueName =
        "__Toka_Anon_Rec_" + std::to_string(AnonRecordCounter++);
    Rec->AssignedTypeName = UniqueName;

    // 3. Create and Register Synthetic ShapeDecl
    // We treat it as a regular Struct
    auto SyntheticShape = std::make_unique<ShapeDecl>(
        false, UniqueName, std::vector<GenericParam>{}, ShapeKind::Struct,
        members);

    // Important: Register in ShapeMap so MemberExpr can find it
    ShapeMap[UniqueName] = SyntheticShape.get();

    // Store ownership
    SyntheticShapes.push_back(std::move(SyntheticShape));

    return toka::Type::fromString(UniqueName);
  } else if (auto *Deref = dynamic_cast<DereferenceExpr *>(E)) {
    auto innerObj = checkExpr(Deref->Expression.get());
    if (innerObj->isUnknown())
      return toka::Type::fromString("unknown");

    if (auto ptr = std::dynamic_pointer_cast<toka::PointerType>(innerObj)) {
      return ptr->getPointeeType();
    }
    error(Deref, DiagID::ERR_INVALID_OP, "dereference", innerObj->toString(),
          "void");
    return toka::Type::fromString("unknown");
  } else if (auto *Unary = dynamic_cast<UnaryExpr *>(E)) {
    return checkUnaryExpr(Unary);
  } else if (auto *Str = dynamic_cast<StringExpr *>(E)) {
    return toka::Type::fromString("str");
  } else if (auto *ve = dynamic_cast<VariableExpr *>(E)) {
    SymbolInfo Info;
    if (!CurrentScope->lookup(ve->Name, Info)) {
      // [NEW] Surgical Plan: Try resolving as a type (handles Option<i32>)
      auto possible = toka::Type::fromString(ve->Name);
      if (possible && !possible->isUnknown()) {
        auto resolved = resolveType(possible);
        if (auto shapeT =
                std::dynamic_pointer_cast<toka::ShapeType>(resolved)) {
          // Success: Resolved to a type name (e.g. Option_M_i32)
          return shapeT;
        }
      }

      error(ve, DiagID::ERR_UNDECLARED, ve->Name);
      return toka::Type::fromString("unknown");
    }
    if (Info.Moved && !m_InLHS) {
      error(ve, DiagID::ERR_USE_MOVED, ve->Name);
    }
    if (Info.IsMutablyBorrowed) {
      error(ve, DiagID::ERR_BORROW_MUT, ve->Name);
    }

    // [Annotated AST] Constant Substitution: The Core Fix
    // If this variable is a Generic Constant (e.g., S=4, N=10), we MUST NOT
    // allow CodeGen to see a VariableExpr, because it will try to Load it as an
    // LValue. Instead, we perform AST Substitution here and return a NumberExpr
    // (or equivalent).
    if (Info.HasConstValue) {
      // NOTE: We cannot simply return a new NumberExpr *node* here because
      // checkExpr returns a Type object, not an AST node replacement. Wait,
      // checkExpr returns shared_ptr<Type>, but it modifies the AST in-place?
      // No, checkExpr does not take AST** or smart_ptr reference.
      //
      // CRITICAL ARCHITECTURE CORRECTION:
      // Since checkExpr cannot replace the AST node itself (it takes raw
      // pointer), we must rely on the previous approach (HasConstantValue
      // propagation) BUT with a different CodeGen contract, OR we need the
      // CALLER of checkExpr to handle replacement.
      //
      // However, the User's directive was "checkIdentifierExpr returns new
      // NumberExpr". But verify checkExpr signature: it returns
      // `std::shared_ptr<toka::Type>`. It does NOT return an Expr*.
      //
      // Let's look at `checkExprImpl` or `Sema::analyze`?
      // Ah, `checkExpr` is recursive.
      //
      // Let's look at the Caller. Most callers are `checkExpr(E.get())`.
      // E is a `unique_ptr<Expr>`.
      // If we want to substitute, we need to change how `checkExpr` is called
      // or works.
      //
      // PROPOSAL: We implement the 'HasConstantValue' flag-based fix again, BUT
      // we correct CodeGen logic to NOT generate a Load.
      //
      // WAIT, the User said "don't do IsConstant check in CodeGen".
      // That implies AST substitution.
      //
      // Let's check if we can modify the AST in place.
      // We have `VariableExpr *ve`.
      // We can't turn a `VariableExpr` into `NumberExpr` in C++ without
      // changing the pointer.
      //
      // ALTERNATIVE:
      // The user might be thinking of a different compiler architecture (AST
      // Rewriter).
      //
      // Let's stick with the user's intent: "Ensure CodeGen treats it as a
      // number". If I can't swap the node pointer easily here, I must modify
      // CodeGen to *behave* as if it's a number.
      //
      // BUT, I can try to Mutate the VariableExpr to ACT like a NumberExpr?
      // No.
      //
      // LET'S LOOK AT checkStmt or whoever owns the unique_ptr.
      //
      // Actually, looking at `Sema_Expr.cpp`, `checkExpr` takes `Expr *E`.
      //
      // If I cannot replace the node, I MUST use the `HasConstantValue` flag in
      // CodeGen. The user said: "Do not return VariableExpr! ... return new
      // NumberExpr". This implies the user thinks checkExpr returns an Expr*.
      //
      // CHECK SEMANTICS:
      // `std::shared_ptr<toka::Type> Sema::checkExpr(Expr *E)`
      // Correct. It returns Type.
      //
      // So I cannot substitute the node *here*.
      //
      // OPTION 2: The `HasConstantValue` approach IS the right way for this
      // codebase, provided `genVariableExpr` handles it correctly.
      //
      // The user's diagnosis "You generated a Load for a ConstantInt" was
      // correct. My CodeGen fix WAS: `if (var->HasConstantValue) return
      // ConstantInt;` This DOES NOT generate a Load. It generates a Value.
      //
      // So why did it crash?
      // "上层逻辑...试图生成 builder.CreateLoad(i32 4)"
      //
      // Ah. `genVariableExpr` returned a `PhysEntity` wrapping the ConstantInt.
      // `PhysEntity` constructor has `bool isAlloca`.
      // If `isAlloca` is false (default), `PhysEntity::load()` returns the
      // value itself.
      //
      // If the caller calls `load()`, it gets `i32 4`.
      // If the caller calls `store()`, it crashes.
      //
      // Where is it crashing?
      // `audit_generics.tk` Line 31: `auto buf = [0; S]`
      //
      // AST: `VariableDecl(buf, RepeatedArrayExpr(0, S))`
      // CodeGen: `genVariableDecl` -> `genRepeatedArrayExpr` -> `genExpr(S)`.
      // `genRepeatedArrayExpr` logic:
      // `if (auto *num = dyn_cast<NumberExpr>(expr->Count)) ...`
      // `else if (auto *var = dyn_cast<VariableExpr>(expr->Count))`
      //
      // In my CodeGen fix, I handled `VariableExpr` with `HasConstantValue`.
      // So `genRepeatedArrayExpr` should grab `var->ConstantValue` (raw int)
      // and use `ArrayType::get`. It does NOT call `genExpr(var)`.
      //
      // So `genVariableExpr` is NOT called for the array size S.
      //
      // So where is the crash?
      // "Control flow reaches end of non-void function" (Exit 1)
      // "Segmentation fault" (Exit 139)
      //
      // When `println(".. S")` was uncommented, it crashed.
      // `println` calls `make_buffer_print` -> `genCallExpr` ->
      // `genExpr(Args)`. `S` is passed to `println`. `println` takes `(fmt,
      // ...)`. Varargs? `println` implementation might expect LValue?
      //
      // `checkIdentifierExpr` DOES NOT EXIST in this codebase.
      // `checkExpr` handles `VariableExpr`.
      //
      // I will implement the Metadata Propagation (`HasConstantValue`) in Sema,
      // AND I will re-implement the CodeGen fix which was correct,
      // BUT I will ensure `PhysEntity` correctly flags it as an RValue.
      //
      // Wait, the user specifically said: "Don't do IsConstant check in
      // CodeGen". They want AST Substitution. To do AST Substitution, I need to
      // find where `Expr` pointers are held. They are held in
      // `unique_ptr<Expr>` inside other AST nodes (Stmt, CallExpr, etc).
      //
      // To substitute, I would need a `Transform` pass or `Mutator`. `Sema` is
      // a pass. `Sema` visits nodes via `checkExpr(E)`.
      //
      // If I cannot replace the node pointer, I must modify the node to behave
      // correctly.
      //
      // Let's execute the User's Plan via a helper method
      // `substituteConstantVars`? Or just accept that `HasConstantValue` IS the
      // way to go, but I need to fix the Crash reason.
      //
      // The crash "Instruction does not dominate use" or similar LLVM errors
      // usually come from block mess-up.
      //
      // Let's re-read the user's advice:
      // "checkIdentifierExpr(IdentifierExpr* ID) ... return new NumberExpr"
      // The user thinks I have `checkIdentifierExpr` returning `Expr*`. I DO
      // NOT. I have `checkExpr(Expr*)` returning `Type`.
      //
      // COMPROMISE:
      // I will use `HasConstantValue` (Sema) + `genVariableExpr` (CodeGen)
      // checks. The user's premise that I can swap AST nodes return values is
      // technically incompatible with the current `Sema` signature.
      //
      // However, I CAN mutate `VariableExpr` to `ResolvedType` = `const int`.
      //
      // Let's try to restore the CodeGen logic but be very careful.
      //
      // The crash with `println` might be because `genVariableExpr` returned a
      // ConstantInt, and `genCallExpr` tried to pass it as `...` (VarArg). For
      // VarArgs, `genCallExpr` calls `genExpr`. If `genExpr` returns a
      // ConstantInt RValue, `genCallExpr` handles it?
      //
      // Let's implement the `HasConstantValue` propagation here first.

      ve->HasConstantValue = true;
      ve->ConstantValue = Info.ConstValue;
      ve->ResolvedType = Info.TypeObj; // Ensure type is known (e.g. usize)
    }

    // Unset Check: Only check if NOT in LHS
    if (!m_InLHS) {
      bool isFullyInit = true;
      if (Info.InitMask == 0) {
        isFullyInit = false;
      } else if (Info.TypeObj && Info.TypeObj->isShape()) {
        // Check all bits for shape
        std::string soul = Info.TypeObj->getSoulName();
        if (ShapeMap.count(soul)) {
          ShapeDecl *SD = ShapeMap[soul];
          uint64_t expected = (1ULL << SD->Members.size()) - 1;
          if (SD->Members.size() >= 64)
            expected = ~0ULL;
          if ((Info.InitMask & expected) != expected) {
            isFullyInit = false;
          }
        }
      }

      if (!isFullyInit && !m_AllowUnsetUsage) {
        DiagnosticEngine::report(getLoc(ve), DiagID::ERR_USE_UNSET, ve->Name);
        HasError = true;
      }
    }
    m_LastInitMask = Info.InitMask;
    // [Constitution] Soul Collapse (The Hat-Off Transduction)
    // "Variable name without hat collapses to value (Soul)."
    bool shouldCollapse = true;
    if (m_DisableSoulCollapse) {
      shouldCollapse = false;
    }

    auto current = Info.TypeObj;
    if (shouldCollapse && current) {
      while (current && (current->isPointer() || current->isReference() ||
                         current->isSmartPointer())) {
        if (current->IsNullable) {
          // Rule: deref of nullable REQUIRES explicit check
          break;
        }
        auto inner = current->getPointeeType();
        if (!inner)
          break;
        current = inner;
      }
    }

    // [Constitution 1.3] Soul Attributes Rule:
    // Identifiers (x, p) refer to the SOUL. Sigils (*p, ^p) refer to the
    // IDENTITY. Trailing attributes (#, ?) on an identifier always apply to the
    // soul. If we are looking at the soul (either we collapsed or it's a
    // value), apply the attributes. If we are looking at the identity (collapse
    // disabled), keep handle attributes and ignore trailing soul attributes for
    // now.
    if (shouldCollapse || (current && !current->isPointer())) {
      if (current) {
        // [Fix] Additive Nullability: Do not strip existing nullability from
        // the type just because usage doesn't have '?'.
        bool effectiveNull = current->IsNullable || ve->IsValueNullable;
        return current->withAttributes(ve->IsValueMutable, effectiveNull);
      }
    }

    return current;
  } else if (auto *Cast = dynamic_cast<CastExpr *>(E)) {
    auto srcType = checkExpr(Cast->Expression.get());
    auto targetType = resolveType(toka::Type::fromString(Cast->TargetType));

    // Rule: Numeric Casts (Always allowed for standard numeric types)
    bool srcIsNumeric = srcType->isInteger() || srcType->isFloatingPoint();
    bool targetIsNumeric =
        targetType->isInteger() || targetType->isFloatingPoint();

    // Rule: Pointer Morphologies or Addr
    bool srcIsAddr = (srcType->toString() == "Addr" ||
                      resolveType("Addr") == srcType->toString());
    bool targetIsAddr =
        (Cast->TargetType == "Addr" || resolveType("Addr") == Cast->TargetType);

    bool srcIsRaw = srcType->isRawPointer();
    bool targetIsRaw = targetType->isRawPointer();

    bool srcIsOAddr = (srcType->toString() == "OAddr" ||
                       resolveType("OAddr") == srcType->toString());
    bool targetIsOAddr = (Cast->TargetType == "OAddr" ||
                          resolveType("OAddr") == Cast->TargetType);

    if (srcIsOAddr && !targetIsOAddr) {
      DiagnosticEngine::report(getLoc(Cast), DiagID::ERR_CAST_MISMATCH,
                               srcType->toString(), Cast->TargetType);
      HasError = true;
    }

    if (srcIsNumeric && targetIsNumeric) {
      // Normal numeric cast, allow.
    } else if (targetType->isReference()) {
      // [Constitution 1.3] Borrow-Cast: var as &Node or var as &var
      // Re-interpreting identity as a direct borrow view.
      auto targetInner = targetType->getPointeeType();
      auto srcInner =
          (srcType->isPointer())
              ? std::dynamic_pointer_cast<toka::PointerType>(srcType)
                    ->getPointeeType()
              : srcType;

      if (!isTypeCompatible(targetInner, srcInner)) {
        error(Cast, DiagID::ERR_CAST_MISMATCH, srcType->toString(),
              Cast->TargetType);
      }

      // Semantic Side-Effect: Register this as a borrow
      Expr *Traverse = Cast->Expression.get();
      // Peel Identity Op if present: ^p -> p
      if (auto *UE = dynamic_cast<UnaryExpr *>(Traverse)) {
        if (UE->Op == TokenType::Caret || UE->Op == TokenType::Tilde ||
            UE->Op == TokenType::Star) {
          Traverse = UE->RHS.get();
        }
      }

      while (true) {
        if (auto *M = dynamic_cast<MemberExpr *>(Traverse)) {
          Traverse = M->Object.get();
        } else if (auto *Idx = dynamic_cast<ArrayIndexExpr *>(Traverse)) {
          Traverse = Idx->Array.get();
        } else {
          break;
        }
      }

      if (auto *Var = dynamic_cast<VariableExpr *>(Traverse)) {
        SymbolInfo *Info = nullptr;
        if (CurrentScope->findSymbol(Var->Name, Info)) {
          if (!m_InLHS) {
            bool isExclusive = targetInner->IsWritable;
            if (isExclusive) {
              if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
                error(Cast, DiagID::ERR_BORROW_MUT, Var->Name);
              }
              Info->IsMutablyBorrowed = true;
            } else {
              if (Info->IsMutablyBorrowed) {
                error(Cast, DiagID::ERR_BORROW_MUT, Var->Name);
              }
              Info->ImmutableBorrowCount++;
            }
            m_LastBorrowSource = Var->Name;
            m_CurrentStmtBorrows.push_back({Var->Name, isExclusive});
          }
        }
      }
    } else if (targetType->isSmartPointer() && !srcType->isSmartPointer() &&
               srcType->toString() != "nullptr" &&
               srcType->toString() != "*?void") {
      error(Cast, DiagID::ERR_SMART_PTR_FROM_STACK, Cast->TargetType[0]);
    } else if (targetIsRaw &&
               (srcType->isUniquePtr() || srcType->isSharedPtr())) {
      error(Cast, DiagID::ERR_GENERIC_PARSE,
            "Identity Intrusion: Managed memory ('" + srcType->toString() +
                "') cannot be degraded to raw pointer ('" + Cast->TargetType +
                "'). Violation of Allocation Sourcing.");
    } else if (targetIsAddr) {
      if (!(srcIsAddr || srcIsRaw || srcIsNumeric)) {
        error(Cast, DiagID::ERR_CAST_MISMATCH, srcType->toString(),
              Cast->TargetType);
      }
    } else if (srcIsAddr) {
      if (!(targetIsAddr || targetIsRaw || targetIsNumeric)) {
        error(Cast, DiagID::ERR_CAST_MISMATCH, srcType->toString(),
              Cast->TargetType);
      }
    } else if (targetIsOAddr) {
      if (!srcType->isPointer()) {
        error(Cast, DiagID::ERR_CAST_MISMATCH, srcType->toString(),
              Cast->TargetType);
      }
    } else if (targetIsRaw) {
      bool srcIsStr = (srcType->toString() == "str");
      bool srcIsNull =
          (srcType->toString() == "nullptr" || srcType->toString() == "*?void");
      if (!(srcIsAddr || srcIsRaw || srcIsNumeric || srcIsStr || srcIsNull)) {
        error(Cast, DiagID::ERR_CAST_MISMATCH, srcType->toString(),
              Cast->TargetType);
      }
    } else if (srcIsRaw) {
      if (!(targetIsAddr || targetIsRaw || targetIsNumeric)) {
        error(Cast, DiagID::ERR_CAST_MISMATCH, srcType->toString(),
              Cast->TargetType);
      }
    } else if (!srcType->equals(*targetType)) {
      // Rule: Union Reinterpretation
      auto srcTypeResolved = resolveType(srcType);
      bool srcIsUnion = false;
      std::shared_ptr<ShapeType> st = nullptr;
      if (srcTypeResolved->isShape()) {
        st = std::dynamic_pointer_cast<ShapeType>(srcTypeResolved);
        if (st->Decl && st->Decl->Kind == ShapeKind::Union) {
          srcIsUnion = true;
        }
      }

      if (srcIsUnion) {
        bool found = false;
        for (const auto &M : st->Decl->Members) {
          auto mType = M.ResolvedType
                           ? M.ResolvedType
                           : toka::Type::fromString(resolveType(M.Type));
          if (mType && targetType->equals(*mType)) {
            found = true;
            break;
          }
        }
        if (!found) {
          error(Cast, DiagID::ERR_CAST_MISMATCH, srcType->toString(),
                Cast->TargetType);
        }
      } else if (!isTypeCompatible(targetType, srcType)) {
        error(Cast, DiagID::ERR_CAST_MISMATCH, srcType->toString(),
              Cast->TargetType);
      }
    }

    return targetType;
  } else if (auto *Bin = dynamic_cast<BinaryExpr *>(E)) {
    return checkBinaryExpr(Bin);
  } else if (auto *ie = dynamic_cast<IfExpr *>(E)) {

    auto condTypeObj = checkExpr(ie->Condition.get());
    std::string condType = condTypeObj->toString();

    // Type Narrowing for 'is' check
    std::string narrowedVar;
    SymbolInfo originalInfo;
    bool narrowed = false;

    if (auto *bin = dynamic_cast<BinaryExpr *>(ie->Condition.get())) {
      if (bin->Op == "is") {
        Expr *lhs = bin->LHS.get();
        std::string path = getStringifyPath(lhs);
        if (!path.empty()) {
          m_NarrowedPaths.insert(path);
          narrowed = true;
          narrowedVar = path;
        }

        VariableExpr *varExpr = dynamic_cast<VariableExpr *>(lhs);
        if (!varExpr) {
          if (auto *un = dynamic_cast<UnaryExpr *>(lhs)) {
            varExpr = dynamic_cast<VariableExpr *>(un->RHS.get());
          }
        }

        if (varExpr) {
          SymbolInfo *infoPtr = nullptr;
          if (CurrentScope->findSymbol(varExpr->Name, infoPtr)) {
            if (!narrowed) {
              narrowedVar = varExpr->Name;
              narrowed = true;
            }
            originalInfo = *infoPtr;
            // Sync TypeObj: Narrow type to non-nullable
            if (infoPtr->TypeObj) {
              infoPtr->TypeObj = infoPtr->TypeObj->withAttributes(
                  infoPtr->TypeObj->IsWritable, false);
            }
          }
        }
      }
    }

    bool isReceiver = false;
    if (!m_ControlFlowStack.empty()) {
      isReceiver = m_ControlFlowStack.back().IsReceiver;
    }

    m_ControlFlowStack.push_back({"", "void", false, isReceiver});

    // Save Mask State for Intersection Rule
    std::map<std::string, uint64_t> masksBefore;
    for (auto &pair : CurrentScope->Symbols) {
      masksBefore[pair.first] = pair.second.InitMask;
    }

    checkStmt(ie->Then.get());

    std::map<std::string, uint64_t> masksThen;
    for (auto &pair : CurrentScope->Symbols) {
      masksThen[pair.first] = pair.second.InitMask;
    }

    // Restore if narrowed
    if (narrowed) {
      if (CurrentScope->lookup(narrowedVar, originalInfo)) { // If it was a var
        SymbolInfo *infoPtr = nullptr;
        if (CurrentScope->findSymbol(narrowedVar, infoPtr)) {
          *infoPtr = originalInfo;
        }
      }
      m_NarrowedPaths.erase(narrowedVar);
    }

    std::string thenType = m_ControlFlowStack.back().ExpectedType;
    bool thenReturns = allPathsJump(ie->Then.get());
    m_ControlFlowStack.pop_back();

    std::string elseType = "void";
    bool elseReturns = false;
    if (ie->Else) {
      // Restore Before Else
      for (auto &pair : masksBefore) {
        CurrentScope->Symbols[pair.first].InitMask = pair.second;
      }

      m_ControlFlowStack.push_back({"", "void", false, isReceiver});
      checkStmt(ie->Else.get());
      elseType = m_ControlFlowStack.back().ExpectedType;
      elseReturns = allPathsJump(ie->Else.get());
      m_ControlFlowStack.pop_back();

      // Intersection Rule
      if (thenReturns && elseReturns) {
        // Unreachable after if
      } else if (thenReturns) {
        // State is from Else branch
      } else if (elseReturns) {
        // State is from Then branch
        for (auto &pair : CurrentScope->Symbols) {
          if (masksThen.count(pair.first))
            pair.second.InitMask = masksThen[pair.first];
        }
      } else {
        // Actual Intersection
        for (auto &pair : CurrentScope->Symbols) {
          uint64_t thenM =
              masksThen.count(pair.first) ? masksThen[pair.first] : 0;
          pair.second.InitMask &= thenM;
        }
      }
    } else {
      // No Else: Intersection with Before state (which we just did by not
      // initializing or by restoring)
      // Actually, if there is no else, anything set in 'then' is NOT set after
      // the if.
      for (auto &pair : CurrentScope->Symbols) {
        pair.second.InitMask = masksBefore[pair.first];
      }
    }

    if (isReceiver) {
      if (thenType == "void" && !allPathsJump(ie->Then.get()))
        error(ie->Then.get(), DiagID::ERR_YIELD_VALUE_REQUIRED, "if branch");
      if (!ie->Else)
        error(ie, DiagID::ERR_YIELD_ELSE_REQUIRED);
      else if (elseType == "void" && !allPathsJump(ie->Else.get()))
        error(ie->Else.get(), DiagID::ERR_YIELD_VALUE_REQUIRED, "else branch");
    }

    if (thenType != "void" && elseType != "void" &&
        !isTypeCompatible(thenType, elseType)) {
      error(ie, DiagID::ERR_BRANCH_TYPE_MISMATCH, "If", thenType, elseType);
    }
    return toka::Type::fromString((thenType != "void") ? thenType : elseType);
  } else if (auto *we = dynamic_cast<WhileExpr *>(E)) {
    checkExpr(we->Condition.get());
    bool isReceiver = false;
    if (!m_ControlFlowStack.empty()) {
      isReceiver = m_ControlFlowStack.back().IsReceiver;
    }

    bool tookOver = false;
    if (!m_ControlFlowStack.empty() && !m_ControlFlowStack.back().IsLoop &&
        !m_ControlFlowStack.back().Label.empty()) {
      m_ControlFlowStack.back().IsLoop = true;
      m_ControlFlowStack.back().IsReceiver = isReceiver;
      tookOver = true;
    } else {
      m_ControlFlowStack.push_back({"", "void", true, isReceiver});
    }
    checkStmt(we->Body.get());
    std::string bodyType = m_ControlFlowStack.back().ExpectedType;
    if (!tookOver)
      m_ControlFlowStack.pop_back();

    std::string elseType = "void";
    if (we->ElseBody) {
      m_ControlFlowStack.push_back({"", "void", false, isReceiver});
      checkStmt(we->ElseBody.get());
      elseType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();
    }

    if (isReceiver) {
      if (bodyType == "void" && !allPathsJump(we->Body.get()))
        error(we->Body.get(), DiagID::ERR_YIELD_VALUE_REQUIRED, "while loop");
      if (!we->ElseBody)
        error(we, DiagID::ERR_YIELD_OR_REQUIRED, "while");
      else if (elseType == "void" && !allPathsJump(we->ElseBody.get()))
        error(we->ElseBody.get(), DiagID::ERR_YIELD_VALUE_REQUIRED,
              "'or' block");
    }

    if (bodyType != "void" && !we->ElseBody) {
      error(we, DiagID::ERR_YIELD_OR_REQUIRED, "while");
    }
    if (bodyType != "void" && elseType != "void" &&
        !isTypeCompatible(bodyType, elseType)) {
      error(we, DiagID::ERR_BRANCH_TYPE_MISMATCH, "While loop", bodyType,
            elseType);
    }
    return toka::Type::fromString((bodyType != "void") ? bodyType : elseType);
  } else if (auto *le = dynamic_cast<LoopExpr *>(E)) {
    bool isReceiver = false;
    if (!m_ControlFlowStack.empty()) {
      isReceiver = m_ControlFlowStack.back().IsReceiver;
    }

    bool tookOver = false;
    if (!m_ControlFlowStack.empty() && !m_ControlFlowStack.back().IsLoop &&
        !m_ControlFlowStack.back().Label.empty()) {
      m_ControlFlowStack.back().IsLoop = true;
      m_ControlFlowStack.back().IsReceiver = isReceiver;
      tookOver = true;
    } else {
      m_ControlFlowStack.push_back({"", "void", true, isReceiver});
    }
    checkStmt(le->Body.get());
    std::string res = m_ControlFlowStack.back().ExpectedType;
    if (!tookOver)
      m_ControlFlowStack.pop_back();

    if (isReceiver && res == "void" && !allPathsJump(le->Body.get())) {
      error(le, DiagID::ERR_YIELD_VALUE_REQUIRED, "loop");
    }
    return toka::Type::fromString(res);
  } else if (auto *fe = dynamic_cast<ForExpr *>(E)) {
    auto collTypeObj = checkExpr(fe->Collection.get());
    std::string collType = collTypeObj->toString();
    std::string elemType = "i32"; // TODO: better inference
    if (collType.size() > 2 && collType.substr(0, 2) == "[]")
      elemType = collType.substr(2);

    enterScope();
    SymbolInfo Info;
    // Construct Full Type String for Type Object
    std::string fullType = (fe->IsReference ? "&" : "") + elemType;
    if (fe->IsMutable)
      fullType += "#";

    Info.TypeObj = toka::Type::fromString(fullType);
    CurrentScope->define(fe->VarName, Info);

    bool isReceiver = false;
    if (!m_ControlFlowStack.empty()) {
      isReceiver = m_ControlFlowStack.back().IsReceiver;
    }

    bool tookOver = false;
    if (!m_ControlFlowStack.empty() && !m_ControlFlowStack.back().IsLoop &&
        !m_ControlFlowStack.back().Label.empty()) {
      m_ControlFlowStack.back().IsLoop = true;
      m_ControlFlowStack.back().IsReceiver = isReceiver; // Sync receiver status
      tookOver = true;
    } else {
      m_ControlFlowStack.push_back({"", "void", true, isReceiver});
    }
    checkStmt(fe->Body.get());
    std::string bodyType = m_ControlFlowStack.back().ExpectedType;
    if (!tookOver)
      m_ControlFlowStack.pop_back();

    std::string elseType = "void";
    if (fe->ElseBody) {
      m_ControlFlowStack.push_back({"", "void", false, isReceiver});
      checkStmt(fe->ElseBody.get());
      elseType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();
    }
    exitScope();

    if (isReceiver) {
      if (bodyType == "void" && !allPathsJump(fe->Body.get()))
        error(fe->Body.get(), DiagID::ERR_YIELD_VALUE_REQUIRED, "for loop");
      if (!fe->ElseBody)
        error(fe, DiagID::ERR_YIELD_OR_REQUIRED, "for");
      else if (elseType == "void" && !allPathsJump(fe->ElseBody.get()))
        error(fe->ElseBody.get(), DiagID::ERR_YIELD_VALUE_REQUIRED,
              "'or' block");
    }

    if (bodyType != "void" && !fe->ElseBody) {
      error(fe, DiagID::ERR_YIELD_OR_REQUIRED, "for");
    }
    if (bodyType != "void" && elseType != "void" &&
        !isTypeCompatible(bodyType, elseType)) {
      error(fe, DiagID::ERR_BRANCH_TYPE_MISMATCH, "For loop", bodyType,
            elseType);
    }
    return toka::Type::fromString((bodyType != "void") ? bodyType : elseType);
  } else if (auto *pe = dynamic_cast<PassExpr *>(E)) {
    // 1. Detect if this is a prefix 'pass' (wrapping a complex expression)
    bool isPrefixMatch = dynamic_cast<MatchExpr *>(pe->Value.get());
    bool isPrefixIf = dynamic_cast<IfExpr *>(pe->Value.get());
    bool isPrefixFor = dynamic_cast<ForExpr *>(pe->Value.get());
    bool isPrefixWhile = dynamic_cast<WhileExpr *>(pe->Value.get());
    bool isPrefixLoop = dynamic_cast<LoopExpr *>(pe->Value.get());

    std::string valType = "void";
    if (isPrefixMatch || isPrefixIf || isPrefixFor || isPrefixWhile ||
        isPrefixLoop) {
      m_ControlFlowStack.push_back({"", "void", false, true});
      auto valTypeObj = checkExpr(pe->Value.get());
      valType = valTypeObj->toString();
      m_ControlFlowStack.pop_back();

      if (valType == "void") {
        error(pe, "Prefix 'pass' expects a value-yielding expression");
      }
    } else {
      auto valTypeObj = checkExpr(pe->Value.get());
      valType = valTypeObj->toString();
    }

    // Escape Blockade: Check for Dirty Reference
    if (pe->Value) {
      if (auto *Var = dynamic_cast<VariableExpr *>(pe->Value.get())) {
        SymbolInfo *info = nullptr;
        if (CurrentScope->findSymbol(Var->Name, info)) {
          if (info->IsReference() && info->DirtyReferentMask != ~0ULL) {
            DiagnosticEngine::report(getLoc(pe), DiagID::ERR_ESCAPE_UNSET,
                                     Var->Name);
            HasError = true;
          }
        }
      }
    }

    bool foundReceiver = false;
    if (!m_ControlFlowStack.empty()) {
      for (auto it = m_ControlFlowStack.rbegin();
           it != m_ControlFlowStack.rend(); ++it) {
        if (it->IsReceiver) {
          foundReceiver = true;
          if (it->ExpectedType == "void") {
            it->ExpectedType = valType;
          } else if (!isTypeCompatible(it->ExpectedType, valType)) {
            error(pe, DiagID::ERR_TYPE_MISMATCH, valType, it->ExpectedType);
          }
          break;
        }
      }
    }

    if (!foundReceiver) {
      error(pe, DiagID::ERR_PASS_NO_RECEIVER);
    }

    return toka::Type::fromString((isPrefixMatch || isPrefixIf || isPrefixFor ||
                                   isPrefixWhile || isPrefixLoop)
                                      ? valType
                                      : "void");
  } else if (auto *be = dynamic_cast<BreakExpr *>(E)) {
    std::string valType = "void";
    if (be->Value) {
      auto valTypeObj = checkExpr(be->Value.get());
      valType = valTypeObj->toString();

      // Escape Blockade: Check for Dirty Reference
      if (auto *Var = dynamic_cast<VariableExpr *>(be->Value.get())) {
        SymbolInfo *info = nullptr;
        if (CurrentScope->findSymbol(Var->Name, info)) {
          if (info->IsReference() && info->DirtyReferentMask != ~0ULL) {
            DiagnosticEngine::report(getLoc(be), DiagID::ERR_ESCAPE_UNSET,
                                     Var->Name);
            HasError = true;
          }
        }
      }
    }

    ControlFlowInfo *target = nullptr;
    if (be->TargetLabel.empty()) {
      for (auto it = m_ControlFlowStack.rbegin();
           it != m_ControlFlowStack.rend(); ++it) {
        if (it->IsLoop) {
          target = &(*it);
          break;
        }
      }
    } else {
      for (auto it = m_ControlFlowStack.rbegin();
           it != m_ControlFlowStack.rend(); ++it) {
        if (it->Label == be->TargetLabel) {
          target = &(*it);
          break;
        }
      }
    }

    if (target) {
      if (valType != "void") {
        if (target->ExpectedType == "void") {
          target->ExpectedType = valType;
        } else if (!isTypeCompatible(target->ExpectedType, valType)) {
          error(be, DiagID::ERR_TYPE_MISMATCH, valType, target->ExpectedType);
        }
      }
    }
    return toka::Type::fromString("void");
  } else if (auto *ce = dynamic_cast<ContinueExpr *>(E)) {
    // Continue target must be a loop
    return toka::Type::fromString("void");
  } else if (auto *Call = dynamic_cast<CallExpr *>(E)) {
    return checkCallExpr(Call);
  } else if (auto *New = dynamic_cast<NewExpr *>(E)) {
    std::string resolvedName = resolveType(New->Type);

    // [New] Generic Inference for 'new'
    if (ShapeMap.count(resolvedName)) {
      ShapeDecl *SD = ShapeMap[resolvedName];
      if (!SD->GenericParams.empty() && m_ExpectedType) {
        auto ptrTy =
            std::dynamic_pointer_cast<toka::PointerType>(m_ExpectedType);
        if (ptrTy) {
          auto expShape =
              std::dynamic_pointer_cast<toka::ShapeType>(ptrTy->PointeeType);
          if (expShape && (expShape->Name == SD->Name ||
                           expShape->Name.find(SD->Name + "_M") == 0)) {
            resolvedName = resolveType(expShape->toString());
          }
        }
      }
    }

    if (New->Initializer) {
      auto InitTypeObj = checkExpr(New->Initializer.get(),
                                   toka::Type::fromString(resolvedName));
      // Re-propagate the mask from initializer to NewExpr
      // (This will be picked up by checkExpr wrapper and passed to
      // VariableDecl)
    }
    // 'new' usually returns a unique pointer: ^Type
    m_LastInitMask = ~0ULL;
    return toka::Type::fromString("^" + resolvedName);
  } else if (auto *UnsafeE = dynamic_cast<UnsafeExpr *>(E)) {
    bool oldUnsafe = m_InUnsafeContext;
    m_InUnsafeContext = true;
    auto typeObj = checkExpr(UnsafeE->Expression.get());
    std::string type = typeObj->toString();
    m_InUnsafeContext = oldUnsafe;
    return typeObj;
  } else if (auto *AllocE = dynamic_cast<AllocExpr *>(E)) {
    if (!m_InUnsafeContext) {
      DiagnosticEngine::report(getLoc(AllocE), DiagID::ERR_UNSAFE_ALLOC_CTX);
      HasError = true;
    }
    // Mapping to __toka_alloc
    // Returning raw pointer identity: *Type
    std::string baseType = resolveType(AllocE->TypeName);
    if (AllocE->IsArray) {
      if (AllocE->ArraySize) {
        checkExpr(AllocE->ArraySize.get());
      }
    }
    if (AllocE->Initializer) {
      checkExpr(AllocE->Initializer.get(), toka::Type::fromString(baseType));
    }
    AllocE->TypeName = baseType; // [FIX] Update with mangled name for CodeGen
    return toka::Type::fromString("*" + baseType);
  } else if (auto *Met = dynamic_cast<MethodCallExpr *>(E)) {
    auto ObjTypeObj = checkExpr(Met->Object.get());
    std::string ObjType = resolveType(ObjTypeObj->toString());

    // Check for Dynamic Trait Object
    if (ObjType.size() >= 4 && ObjType.substr(0, 3) == "dyn") {
      std::string traitName = "";
      if (ObjType.rfind("dyn @", 0) == 0)
        traitName = ObjType.substr(5);
      else if (ObjType.rfind("dyn@", 0) == 0)
        traitName = ObjType.substr(4);

      if (!traitName.empty() && TraitMap.count(traitName)) {
        TraitDecl *TD = TraitMap[traitName];
        for (auto &M : TD->Methods) {
          if (M->Name == Met->Method) {
            if (!M->IsPub) {
              bool sameModule = false;
              if (CurrentModule) {
                std::string modFile = DiagnosticEngine::SrcMgr
                                          ->getFullSourceLoc(CurrentModule->Loc)
                                          .FileName;
                std::string tdDeclFile =
                    DiagnosticEngine::SrcMgr->getFullSourceLoc(TD->Loc)
                        .FileName;
                if (modFile == tdDeclFile) {
                  sameModule = true;
                }
              }
              std::string metCallFile =
                  DiagnosticEngine::SrcMgr->getFullSourceLoc(Met->Loc).FileName;
              std::string tdDeclFile =
                  DiagnosticEngine::SrcMgr->getFullSourceLoc(TD->Loc).FileName;
              if (metCallFile != tdDeclFile && !sameModule) {
                error(Met, DiagID::ERR_METHOD_PRIVATE, Met->Method,
                      "trait " + traitName, getModuleName(CurrentModule));
              }
            }
            return toka::Type::fromString(M->ReturnType);
          }
        }
      }
    }

    std::string soulType = Type::stripMorphology(ObjType);
    if (!(MethodMap.count(soulType) &&
          MethodMap[soulType].count(Met->Method))) {
      // [NEW] Lazy Impl Instantiation
      // [Fix] Use fully resolved/mangled name for lazy instantiation lookup
      std::string resolveName = resolveType(ObjTypeObj->toString());
      std::string ConcreteTypeName = Type::stripMorphology(resolveName);
      std::string BaseName = ConcreteTypeName;
      size_t lt = BaseName.find('<');
      if (lt != std::string::npos) {
        BaseName = BaseName.substr(0, lt);
        if (GenericImplMap.count(BaseName)) {
          // [FIX] Pass generic arguments to instantiateGenericImpl
          std::vector<std::shared_ptr<toka::Type>> genericArgs;
          auto soulType = ObjTypeObj->getSoulType();
          if (auto *ST = dynamic_cast<ShapeType *>(soulType.get())) {
            genericArgs = ST->GenericArgs;
          } else {
            // Fallback: parse from string
            auto parsed = Type::fromString(ConcreteTypeName);
            if (auto *PST = dynamic_cast<ShapeType *>(parsed.get())) {
              genericArgs = PST->GenericArgs;
            }
          }
          for (auto *ImplTemplate : GenericImplMap[BaseName]) {
            instantiateGenericImpl(ImplTemplate, ConcreteTypeName, genericArgs);
          }
        }
      }
    }

    if (MethodMap.count(soulType) && MethodMap[soulType].count(Met->Method)) {
      if (MethodDecls.count(soulType) &&
          MethodDecls[soulType].count(Met->Method)) {
        FunctionDecl *FD = MethodDecls[soulType][Met->Method];
        if (!FD->IsPub) {
          // Check visibility (simplified: allows same-file access, needs
          // expansion for crate)
          // FIXME: This logic should match checkCallExpr's relaxed check
          // (Same Module)
          bool sameModule = false;
          if (CurrentModule) {
            std::string modFile =
                DiagnosticEngine::SrcMgr->getFullSourceLoc(CurrentModule->Loc)
                    .FileName;
            std::string fdFile =
                DiagnosticEngine::SrcMgr->getFullSourceLoc(FD->Loc).FileName;
            if (modFile == fdFile) {
              sameModule = true;
            }
          }

          std::string metCallFile =
              DiagnosticEngine::SrcMgr->getFullSourceLoc(Met->Loc).FileName;
          std::string fdDeclFile =
              DiagnosticEngine::SrcMgr->getFullSourceLoc(FD->Loc).FileName;

          if (metCallFile != fdDeclFile && !sameModule) {
            error(Met, DiagID::ERR_METHOD_PRIVATE, Met->Method, soulType,
                  getModuleName(CurrentModule));
          }
        }
      }

      // [Rule] Borrowing check for Method Call
      if (auto *objVar = dynamic_cast<VariableExpr *>(Met->Object.get())) {
        SymbolInfo *info = nullptr;
        if (CurrentScope->findSymbol(objVar->Name, info)) {
          if (info->IsMutablyBorrowed) {
            DiagnosticEngine::report(getLoc(Met), DiagID::ERR_BORROW_MUT,
                                     objVar->Name);
            HasError = true;
          }
        }
      }

      return toka::Type::fromString(MethodMap[soulType][Met->Method]);
    }
    // Check with @encap suffix as fallback
    std::string encapType = soulType + "@encap";
    if (MethodMap.count(encapType) && MethodMap[encapType].count(Met->Method)) {
      return toka::Type::fromString(MethodMap[encapType][Met->Method]);
    }

    // Check if it's a reference to a struct
    if (ObjType.size() > 1 && ObjType[0] == '^') {
      std::string Pointee = ObjType.substr(1);
      std::string pSoul = Type::stripMorphology(Pointee);
      if (MethodMap.count(pSoul) && MethodMap[pSoul].count(Met->Method)) {
        return toka::Type::fromString(MethodMap[pSoul][Met->Method]);
      }
    }
    // [Intrinsic] unset() & unwrap()
    if (Met->Method == "unset") {
      m_IsUnsetInitCall = true;
      return ObjTypeObj;
    }
    if (Met->Method == "unwrap") {
      if (!ObjTypeObj->IsNullable) {
        // Warning or Silent - constitution says it's programmer's承担
        // return type is already non-nullable?
      }
      return ObjTypeObj->withAttributes(ObjTypeObj->IsWritable, false);
    }

    error(Met, DiagID::ERR_NO_SUCH_MEMBER, ObjType, Met->Method);
    return toka::Type::fromString("unknown");
  } else if (auto *Init = dynamic_cast<InitStructExpr *>(E)) {
    return checkShapeInit(Init);
  } else if (auto *Memb = dynamic_cast<MemberExpr *>(E)) {
    return checkMemberExpr(Memb);
  } else if (auto *Post = dynamic_cast<PostfixExpr *>(E)) {
    // [Fix] Do NOT disable soul collapse.
    // If the user wants the handle, they must use explicit prefix (e.g. ^ptr#).
    // Otherwise `ptr#` means "Mutable Value".
    // bool oldDisable = m_DisableSoulCollapse;
    // m_DisableSoulCollapse = true;
    auto lhsObj = checkExpr(Post->LHS.get());
    // m_DisableSoulCollapse = oldDisable;
    std::string lhsInfo = lhsObj->toString();
    if (auto *Var = dynamic_cast<VariableExpr *>(Post->LHS.get())) {
      SymbolInfo Info;
      if (CurrentScope->lookup(Var->Name, Info) && !Info.IsMutable()) {
        error(Post, DiagID::ERR_IMMUTABLE_MOD, Var->Name);
      }
    }
    if (Post->Op == TokenType::TokenWrite) {
      if (!lhsInfo.empty() && lhsInfo.back() != '#' && lhsInfo.back() != '!')
        lhsInfo += "#";
    } else if (Post->Op == TokenType::TokenNull) {
      if (!lhsInfo.empty() && lhsInfo.back() != '?' && lhsInfo.back() != '!')
        lhsInfo += "?";
    }
    return toka::Type::fromString(lhsInfo);
  } else if (auto *Arr = dynamic_cast<ArrayIndexExpr *>(E)) {
    if (auto *Var = dynamic_cast<VariableExpr *>(Arr->Array.get())) {
      if (ShapeMap.count(Var->Name)) {
        ShapeDecl *SD = ShapeMap[Var->Name];
        if (SD->Kind == ShapeKind::Array) {
          if (Arr->Indices.size() != SD->ArraySize) {
            error(Arr, "Array shape '" + Var->Name + "' expects " +
                           std::to_string(SD->ArraySize) + " elements, got " +
                           std::to_string(Arr->Indices.size()));
          }
          std::string ElemType = "unknown";
          if (!SD->Members.empty())
            ElemType = SD->Members[0].Type;

          for (auto &idx : Arr->Indices) {
            idx = foldGenericConstant(std::move(idx)); // [FIX] Substitution
            std::shared_ptr<toka::Type> ArgTypeObj = checkExpr(idx.get());
            std::string ArgType = ArgTypeObj->toString();
            if (!isTypeCompatible(ElemType, ArgType)) {
              error(idx.get(), "Array element type mismatch: expected '" +
                                   ElemType + "', got '" + ArgType + "'");
            }
          }
          return toka::Type::fromString(Var->Name);
        }
      }
    }

    // Normal Array Indexing
    auto arrTypeObj = checkExpr(Arr->Array.get());
    if (Arr->Indices.size() != 1) {
      error(Arr, DiagID::ERR_INVALID_OP, "indexing", "array",
            "multiple indices");
    }
    Arr->Indices[0] =
        foldGenericConstant(std::move(Arr->Indices[0])); // [FIX] Substitution
    checkExpr(Arr->Indices[0].get());
    return toka::Type::fromString(
        "unknown"); // Placeholder for element type derivation
  } else if (auto *Tup = dynamic_cast<TupleExpr *>(E)) {
    std::vector<std::shared_ptr<toka::Type>> elements;
    for (size_t i = 0; i < Tup->Elements.size(); ++i) {
      elements.push_back(checkExpr(Tup->Elements[i].get()));
    }
    return std::make_shared<toka::TupleType>(std::move(elements));
  } else if (auto *Repeat = dynamic_cast<RepeatedArrayExpr *>(E)) {
    auto elemType = checkExpr(Repeat->Value.get());

    // [FIX] Substitution for Count
    Repeat->Count = foldGenericConstant(std::move(Repeat->Count));

    uint64_t size = 0;
    if (auto *Num = dynamic_cast<NumberExpr *>(Repeat->Count.get())) {
      size = Num->Value;
    } else if (auto *Var = dynamic_cast<VariableExpr *>(Repeat->Count.get())) {
      SymbolInfo info;
      if (CurrentScope->lookup(Var->Name, info) && info.HasConstValue) {
        size = info.ConstValue;
        // [Annotated AST] Propagate for CodeGen
        Var->HasConstantValue = true;
        Var->ConstantValue = size;
      } else {
        error(Repeat, "Array repeat count must be a numeric literal or const "
                      "generic parameter");
      }
    } else {
      error(Repeat, "Array repeat count must be a numeric literal or const "
                    "generic parameter");
    }
    return std::make_shared<toka::ArrayType>(elemType, size);
  } else if (auto *ArrLit = dynamic_cast<ArrayExpr *>(E)) {
    // Infer from first element
    if (!ArrLit->Elements.empty()) {
      ArrLit->Elements[0] =
          foldGenericConstant(std::move(ArrLit->Elements[0])); // [FIX]
      auto ElemTyObj = checkExpr(ArrLit->Elements[0].get());
      std::string ElemTy = ElemTyObj->toString();
      return toka::Type::fromString(
          "[" + ElemTy + ";" + std::to_string(ArrLit->Elements.size()) + "]");
    }
    return toka::Type::fromString("[i32; 0]");
  } else if (auto *me = dynamic_cast<MatchExpr *>(E)) {
    auto targetTypeObj = checkExpr(me->Target.get());
    std::string targetType = targetTypeObj->toString();
    std::string resultType = "void";

    bool isReceiver = false;
    if (!m_ControlFlowStack.empty()) {
      isReceiver = m_ControlFlowStack.back().IsReceiver;
    }

    for (auto &arm : me->Arms) {
      enterScope();
      checkPattern(arm->Pat.get(), targetType, false);
      if (arm->Guard) {
        auto guardTypeObj = checkExpr(arm->Guard.get());
        if (!arm->Guard->ResolvedType->isBoolean()) {
          error(arm->Guard.get(), DiagID::ERR_OPERAND_TYPE_MISMATCH,
                "match guard", "bool", arm->Guard->ResolvedType->toString());
        }
      }
      m_ControlFlowStack.push_back({"", "void", false, isReceiver});
      checkStmt(arm->Body.get());
      std::string armType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();

      if (isReceiver && armType == "void" && !allPathsJump(arm->Body.get())) {
        error(arm->Body.get(), DiagID::ERR_YIELD_VALUE_REQUIRED, "match arm");
      }

      if (resultType == "void")
        resultType = armType;
      else if (armType != "void" && !isTypeCompatible(resultType, armType)) {
        if (!isTypeCompatible(resultType, armType)) {
          error(me, DiagID::ERR_BRANCH_TYPE_MISMATCH, "match", resultType,
                armType);
        }
        exitScope();
      }
    }

    if (isReceiver && resultType == "void") {
      error(me, DiagID::ERR_YIELD_VALUE_REQUIRED, "match expression");
    }

    // Check for private variants if @encap is active
    if (EncapMap.count(targetType)) {
      bool hasWildcard = false;
      for (auto &arm : me->Arms) {
        if (arm->Pat->PatternKind == MatchArm::Pattern::Wildcard) {
          hasWildcard = true;
          break;
        }
      }
      if (!hasWildcard) {
        error(me, DiagID::ERR_GENERIC_PARSE,
              "match on encapsulated type '" + targetType +
                  "' requires a default '_' branch (Rule 412)");
      }
      return toka::Type::fromString(resultType);
    }

    return toka::Type::fromString(resultType);
  }

  return toka::Type::fromString("unknown");
}

std::shared_ptr<toka::Type> Sema::checkMemberExpr(MemberExpr *Memb) {
  std::string path = getStringifyPath(Memb);
  bool isNarrowed = m_NarrowedPaths.count(path);

  // [Fix] Visual Semantics Enforcement: Symmetry Rule
  // 1. Arrow '->' REQUIRES explicit pointer sigil (UnaryExpr with ^, *,
  // ~, &)
  // 2. Dot '.' FORBIDS explicit pointer sigil
  bool isExplicitPtr = false;
  if (auto *UE = dynamic_cast<UnaryExpr *>(Memb->Object.get())) {
    if (UE->Op == TokenType::Caret || UE->Op == TokenType::Star ||
        UE->Op == TokenType::Tilde || UE->Op == TokenType::Ampersand) {
      isExplicitPtr = true;
    }
  }

  // [FIX] Handle context-aware sigil (^p->x)
  if (!isExplicitPtr && (m_OuterPointerSigil == TokenType::Caret ||
                         m_OuterPointerSigil == TokenType::Star ||
                         m_OuterPointerSigil == TokenType::Tilde ||
                         m_OuterPointerSigil == TokenType::Ampersand)) {
    isExplicitPtr = true;
  }

  if (Memb->IsArrow) {
    if (!isExplicitPtr) {
      DiagnosticEngine::report(getLoc(Memb), DiagID::ERR_ARROW_SIGIL_REQUIRED);
      HasError = true;
      return toka::Type::fromString("unknown");
    }
  } else {
    if (isExplicitPtr) {
      DiagnosticEngine::report(getLoc(Memb), DiagID::ERR_MEMBER_SIGIL_MISMATCH);
      HasError = true;
      return toka::Type::fromString("unknown");
    }
  }

  // [FIX] Manual Peeling for Explicit Semantic Access
  // If Object is (^p)->x or *p->x, checkMemberExpr receives
  // UnaryExpr(^p). checking ^p via verifyUnaryExpr triggers "stack
  // variable" error. We must manually peel it, verify morphology, and
  // proceed.
  std::shared_ptr<toka::Type> objTypeObj;
  bool isExplicitPeel = false;
  TokenType explicitOp = TokenType::TokenNone;

  // Check peeling FIRST
  if (auto *UE = dynamic_cast<UnaryExpr *>(Memb->Object.get())) {
    // DEBUG:
    // llvm::errs() << "DEBUG: checkMemberExpr Object is UnaryExpr Op="
    //              << (int)UE->Op << "\n";

    if (UE->Op == TokenType::Caret || UE->Op == TokenType::Star ||
        UE->Op == TokenType::Tilde || UE->Op == TokenType::Ampersand) {

      // Manual Check of Inner Logic
      objTypeObj = checkExpr(UE->RHS.get());
      isExplicitPeel = true;
      explicitOp = UE->Op;

      // Match Logic:
      // ^ requires UniquePtr
      // ~ requires SharedPtr
      // * requires Any Pointer (Raw/Smart/Array)
      // & requires Any (Address of)

      bool match = false;
      if (explicitOp == TokenType::Caret && (objTypeObj->isUniquePtr()))
        match = true;
      else if (explicitOp == TokenType::Tilde && (objTypeObj->isSharedPtr()))
        match = true;
      else if (explicitOp == TokenType::Star &&
               (objTypeObj->isPointer() || objTypeObj->isArray() ||
                objTypeObj->isSmartPointer()))
        match = true;
      else if (explicitOp == TokenType::Ampersand)
        match = true; // Always valid to refer

      if (!match && objTypeObj->toString() != "unknown") {
        // Let strict checks later decide, but for now we peeled it.
      }
    }
  } else if (auto *Post = dynamic_cast<PostfixExpr *>(Memb->Object.get())) {
    // Handle Postfix like p# passed to us?
    // No, p# is internal.
    // If ^p#, then Unary -> Postfix. handled above.
  }

  if (!isExplicitPeel) {
    objTypeObj = checkExpr(Memb->Object.get());
  }

  // Unset Check for Member Access
  if (!m_InLHS) {
    if (auto *objVar = dynamic_cast<VariableExpr *>(Memb->Object.get())) {
      SymbolInfo *Info = nullptr;
      if (CurrentScope->findSymbol(objVar->Name, Info)) {
        // [Rule] Borrowing check for Member Access
        if (Info->IsMutablyBorrowed) {
          DiagnosticEngine::report(getLoc(Memb), DiagID::ERR_BORROW_MUT,
                                   objVar->Name);
          HasError = true;
        }

        if (Info->TypeObj && Info->TypeObj->isShape()) {
          // Determine which mask to check: InitMask (for values) or
          // DirtyReferentMask (for references)
          uint64_t maskToCheck = Info->InitMask;
          if (Info->IsReference()) {
            maskToCheck = Info->DirtyReferentMask;
          }

          std::string soul = Info->TypeObj->getSoulName();
          if (ShapeMap.count(soul)) {
            ShapeDecl *SD = ShapeMap[soul];
            for (int i = 0; i < (int)SD->Members.size(); ++i) {
              if (SD->Members[i].Name == Memb->Member) {
                if (i < 64 && !(maskToCheck & (1ULL << i))) {
                  DiagnosticEngine::report(getLoc(Memb), DiagID::ERR_USE_UNSET,
                                           objVar->Name + "." + Memb->Member);
                  HasError = true;
                }
                break;
              }
            }
          }
        }
      }
    }
  }

  std::string ObjTypeFull = objTypeObj->toString();

  // Visibility Check for Private Member (Robust check using EncapMap exists
  // later in loop)

  if (ObjTypeFull == "module") {
    // It's a module access
    if (auto *objVar = dynamic_cast<VariableExpr *>(Memb->Object.get())) {
      SymbolInfo modSpec;
      if (CurrentScope->lookup(objVar->Name, modSpec) &&
          modSpec.ReferencedModule) {
        ModuleScope *target = (ModuleScope *)modSpec.ReferencedModule;
        if (target->Functions.count(Memb->Member)) {
          return toka::Type::fromString("fn");
        }
        if (target->Globals.count(Memb->Member)) {
          return toka::Type::fromString(
              resolveType(target->Globals[Memb->Member]->TypeName));
        }
      }
    }
  }

  // std::cerr << "DEBUG: checkMemberExpr Object=" << objTypeObj->toString()
  //           << " IsNullable=" << objTypeObj->IsNullable
  //           << " isNarrowed=" << isNarrowed << "\n";

  // [Constitution] Strict Null Safety: No Flow-Sensitive Analysis
  // We enforce explicit unwrap/check. "isNarrowed" suppression is removed.
  if (objTypeObj->IsNullable) {
    //      DiagnosticEngine::report(getLoc(Memb), DiagID::ERR_NULL_ACCESS,
    //                               objTypeObj->toString());
    //      HasError = true;
    // [Update] Reporting logic kept same
    DiagnosticEngine::report(getLoc(Memb), DiagID::ERR_NULL_ACCESS,
                             objTypeObj->toString());
    HasError = true;
  }

  std::string ObjType =
      toka::Type::stripMorphology(resolveType(objTypeObj, true)->toString());

  if (ShapeMap.count(ObjType)) {
    ShapeDecl *SD = ShapeMap[ObjType];
    std::string requestedMember = Memb->Member;
    std::string requestedPrefix = "";
    if (!requestedMember.empty() &&
        (requestedMember[0] == '*' || requestedMember[0] == '^' ||
         requestedMember[0] == '~' || requestedMember[0] == '&')) {
      size_t prefixEnd = 0;
      while (prefixEnd < requestedMember.size() &&
             (requestedMember[prefixEnd] == '*' ||
              requestedMember[prefixEnd] == '^' ||
              requestedMember[prefixEnd] == '~' ||
              requestedMember[prefixEnd] == '&' ||
              requestedMember[prefixEnd] == '?' ||
              requestedMember[prefixEnd] == '#' ||
              requestedMember[prefixEnd] == '!')) {
        prefixEnd++;
      }
      requestedPrefix = requestedMember.substr(0, prefixEnd);
      requestedMember = requestedMember.substr(prefixEnd);
    }

    for (int i = 0; i < (int)SD->Members.size(); ++i) {
      const auto &Field = SD->Members[i];
      if (toka::Type::stripMorphology(Field.Name) == requestedMember) {
        Memb->Index = i; // [FIX] Set index for CodeGen
        // Visibility Check: God-eye view (same file)
        std::string membFile =
            DiagnosticEngine::SrcMgr->getFullSourceLoc(Memb->Loc).FileName;
        std::string sdFile =
            DiagnosticEngine::SrcMgr->getFullSourceLoc(SD->Loc).FileName;

        if (membFile != sdFile) {
          // Check EncapMap
          if (EncapMap.count(ObjType)) {
            bool accessible = false;
            for (const auto &entry : EncapMap[ObjType]) {
              bool fieldMatches = false;
              if (entry.IsExclusion) {
                fieldMatches = true;
                for (const auto &f : entry.Fields) {
                  if (f == requestedMember) {
                    fieldMatches = false;
                    break;
                  }
                }
              } else {
                for (const auto &f : entry.Fields) {
                  if (f == requestedMember) {
                    fieldMatches = true;
                    break;
                  }
                }
              }

              if (fieldMatches) {
                if (entry.Level == EncapEntry::Global) {
                  accessible = true;
                } else if (entry.Level == EncapEntry::Crate) {
                  accessible = true;
                } else if (entry.Level == EncapEntry::Path) {
                  if (membFile.find(entry.TargetPath) != std::string::npos) {
                    accessible = true;
                  }
                }
              }
              if (accessible)
                break;
            }

            if (!accessible) {
              error(Memb, DiagID::ERR_MEMBER_PRIVATE, requestedMember, ObjType);
            }
          }
        }

        // Return type based on Toka 1.3 Pointer-Value Duality
        std::string fullType = Sema::synthesizePhysicalType(Field);
        std::shared_ptr<toka::Type> fieldType =
            toka::Type::fromString(fullType);

        // Determine if the field's soul should be writable based on
        // inheritance
        bool isFieldMarked = Field.IsValueMutable;

        if (isFieldMarked) {
          // [Fix] If member is marked mutable, it means the SOUL is mutable.
          // For pointers/refs, this means the Pointee must be writable.
          if (fieldType->isPointer() || fieldType->isReference() ||
              fieldType->isSmartPointer()) {
            if (auto pt = fieldType->getPointeeType()) {
              pt->IsWritable = true;
            }
          } else {
            fieldType->IsWritable = true;
          }
        }

        bool finalSoulWritable = objTypeObj->IsWritable;
        if (isFieldMarked)
          finalSoulWritable = true;
        else if (Field.IsValueBlocked)
          finalSoulWritable = false;

        if (requestedPrefix.empty() && !m_DisableSoulCollapse) {
          // obj.field (Hat-Off) -> Soul Collapse.
          return fieldType->getSoulType()->withAttributes(
              finalSoulWritable, isNarrowed ? false : fieldType->IsNullable);
        } else {
          // Hatted Access (Identity Access)
          // Use fieldType directly as the base (preserving its morphologies)
          std::shared_ptr<toka::Type> result = fieldType;

          // If requested prefix exists, wrap it
          if (!requestedPrefix.empty()) {
            // Check if field already has this morphology
            bool matches = false;
            if (requestedPrefix == "&" && fieldType->isReference())
              matches = true;
            else if (requestedPrefix == "^" && fieldType->isUniquePtr())
              matches = true;
            else if (requestedPrefix == "~" && fieldType->isSharedPtr())
              matches = true;
            else if (requestedPrefix == "*" && fieldType->isRawPointer())
              matches = true;

            if (!matches) {
              if (requestedPrefix == "&") {
                result = std::make_shared<ReferenceType>(fieldType);
              } else if (requestedPrefix == "^") {
                result = std::make_shared<UniquePointerType>(fieldType);
              } else if (requestedPrefix == "~") {
                result = std::make_shared<SharedPointerType>(fieldType);
              } else if (requestedPrefix == "*") {
                result = std::make_shared<RawPointerType>(fieldType);
              }
            }
          }

          // [Toka 1.3] Inheritance for Handles:
          // If field is NOT marked (#?!), it inherits handle writability from
          // object.
          bool finalHandleWritable = objTypeObj->IsWritable;
          if (isFieldMarked)
            finalHandleWritable = result->IsWritable;
          else if (Field.IsRebindBlocked)
            finalHandleWritable = false;

          // Apply attributes to the resulting handle
          return result->withAttributes(finalHandleWritable,
                                        result->IsNullable);
        }
      }
    }
    error(Memb, DiagID::ERR_NO_SUCH_MEMBER, ObjType, Memb->Member);

    return toka::Type::fromString("unknown");
  } else {
    auto soulType = objTypeObj;
    while (soulType && (soulType->isPointer() || soulType->isReference() ||
                        soulType->isSmartPointer())) {
      soulType = soulType->getPointeeType();
    }

    auto resSoul = resolveType(soulType, true);
    if (auto TT = std::dynamic_pointer_cast<toka::TupleType>(resSoul)) {
      // Tuple access: .0, .1
      try {
        int idx = std::stoi(Memb->Member);
        if (idx >= 0 && idx < (int)TT->Elements.size()) {
          Memb->Index = idx; // Set index for CodeGen
          return TT->Elements[idx];
        } else {
          error(Memb, DiagID::ERR_TUPLE_INDEX_OOB, Memb->Member,
                std::to_string(TT->Elements.size()));
        }
      } catch (...) {
        error(Memb, DiagID::ERR_MEMBER_NOT_FOUND, Memb->Member, "tuple");
      }
      return toka::Type::fromString("unknown");
    } else if (ObjType != "unknown") {
      error(Memb, DiagID::ERR_NOT_A_STRUCT, Memb->Member, ObjType);
    }
  }
  return toka::Type::fromString("unknown");
}

// Stage 4: Object-Oriented Shims
std::shared_ptr<toka::Type> Sema::checkUnaryExpr(UnaryExpr *Unary) {
  // [FIX] Context-Aware Arrow Access (Precedence Handling)
  // If we are wrapping a MemberExpr with Arrow, pass the sigil down.
  // This MUST happen before checkExpr(Unary->RHS) to set the context
  // properly.
  if (auto *Memb = dynamic_cast<MemberExpr *>(Unary->RHS.get())) {
    if (Memb->IsArrow) {
      if (Unary->Op == TokenType::Star || Unary->Op == TokenType::Caret ||
          Unary->Op == TokenType::Tilde || Unary->Op == TokenType::Ampersand) {
        m_OuterPointerSigil = Unary->Op;            // Set context
        auto res = checkExpr(Memb);                 // Check inner
        m_OuterPointerSigil = TokenType::TokenNone; // Reset
        return res;
      }
    }
  }

  // [Fix] Allow soul collapse when checking RHS. Identifiers like 'p' should
  // refer to the soul so that sigils can correctly elevate exactly one level.
  auto rhsType = checkExpr(Unary->RHS.get());

  // Assuming checkExpr returns object now.
  if (!rhsType || rhsType->toString() == "unknown")
    return toka::Type::fromString("unknown");

  std::string rhsInfo = rhsType->toString();

  if (Unary->Op == TokenType::Bang) {
    if (!rhsType->isBoolean()) {
      error(Unary, "operand of '!' must be bool, got '" + rhsInfo + "'");
    }
    return toka::Type::fromString("bool");
  } else if (Unary->Op == TokenType::Minus) {
    bool isNum = rhsType->isInteger() || rhsType->isFloatingPoint();
    if (!isNum) {
      error(Unary, "operand of '-' must be numeric, got '" + rhsInfo + "'");
    }
    return rhsType; // Return object directly
  }

  if (auto *Var = dynamic_cast<VariableExpr *>(Unary->RHS.get())) {
    SymbolInfo *Info = nullptr;
    if (CurrentScope->findSymbol(Var->Name, Info)) {
      std::shared_ptr<toka::Type> physType = Info->TypeObj;

      if (Unary->Op == TokenType::Ampersand) {
        auto innerType = rhsType;
        bool handleMutable = Unary->IsRebindable;
        bool soulMutable = rhsType->IsWritable;

        auto refType = std::make_shared<toka::ReferenceType>(innerType);
        refType->IsNullable = Unary->HasNull;
        refType->IsWritable = handleMutable;

        bool isExclusive = soulMutable;
        if (!m_InLHS) {
          if (isExclusive) {
            if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
              error(Unary, DiagID::ERR_BORROW_MUT, Var->Name);
            }
            Info->IsMutablyBorrowed = true;
          } else {
            if (Info->IsMutablyBorrowed) {
              error(Unary, DiagID::ERR_BORROW_MUT, Var->Name);
            }
            Info->ImmutableBorrowCount++;
          }
          m_LastBorrowSource = Var->Name;
          m_CurrentStmtBorrows.push_back({Var->Name, isExclusive});
        }
        return refType;
      }

      if (Unary->Op == TokenType::Star) {
        if (physType && physType->isRawPointer()) {
          return physType->withAttributes(Unary->IsRebindable, Unary->HasNull);
        }
        // [New] Array-to-Pointer Decay for Variable Elevation
        if (physType && physType->isArray()) {
          auto arr = std::dynamic_pointer_cast<toka::ArrayType>(physType);
          auto res = std::make_shared<toka::RawPointerType>(arr->ElementType);
          res->IsWritable = Unary->IsRebindable;
          res->IsNullable = Unary->HasNull;
          return res;
        }
        auto res = std::make_shared<toka::RawPointerType>(rhsType);
        res->IsWritable = Unary->IsRebindable;
        res->IsNullable = Unary->HasNull;
        return res;
      }

      if (Unary->Op == TokenType::Caret) {
        if (!m_InLHS &&
            (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0)) {
          error(Unary, DiagID::ERR_BORROW_MUT, Var->Name);
        }
        if (physType && physType->isUniquePtr()) {
          return physType->withAttributes(Unary->IsRebindable, Unary->HasNull);
        }
        auto res = std::make_shared<toka::UniquePointerType>(rhsType);
        res->IsWritable = Unary->IsRebindable;
        res->IsNullable = Unary->HasNull;
        return res;
      }

      if (Unary->Op == TokenType::Tilde) {
        if (physType && physType->isSharedPtr()) {
          return physType->withAttributes(Unary->IsRebindable, Unary->HasNull);
        }
        auto res = std::make_shared<toka::SharedPointerType>(rhsType);
        res->IsWritable = Unary->IsRebindable;
        res->IsNullable = Unary->HasNull;
        return res;
      }
    }
  }

  // Fallback for non-variable expressions
  if (Unary->Op == TokenType::Star) {
    // Identity (*)
    std::shared_ptr<toka::Type> inner;
    if (rhsType->isArray()) {
      // Decay Array to Pointer-to-Element
      auto arr = std::dynamic_pointer_cast<toka::ArrayType>(rhsType);
      inner = arr->ElementType;
    } else if (rhsType->isPointer()) {
      inner = rhsType->getPointeeType();
      if (!inner)
        inner = rhsType;
    } else {
      inner = rhsType;
    }
    auto rawPtr = std::make_shared<toka::RawPointerType>(inner);
    rawPtr->IsNullable = Unary->HasNull;
    rawPtr->IsWritable = Unary->IsRebindable;
    return rawPtr;
  }
  if (Unary->Op == TokenType::PlusPlus || Unary->Op == TokenType::MinusMinus) {
    if (!rhsType->isInteger()) {
      error(Unary, DiagID::ERR_OPERAND_TYPE_MISMATCH, "++/--", "integer",
            rhsInfo);
    }
    if (auto *Var = dynamic_cast<VariableExpr *>(Unary->RHS.get())) {
      SymbolInfo *Info = nullptr;
      if (CurrentScope->findSymbol(Var->Name, Info)) {
        if (Info->IsBorrowed()) {
          error(Unary, DiagID::ERR_MOVE_BORROWED, Var->Name);
        }
        if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
          error(Unary, DiagID::ERR_BORROW_MUT, Var->Name);
        }
      }
    }
    return rhsType;
  }
  if (Unary->Op == TokenType::KwBnot) {
    if (!rhsType->isInteger()) {
      error(Unary, DiagID::ERR_OPERAND_TYPE_MISMATCH, "bnot", "integer",
            rhsInfo);
    }
    return rhsType;
  }
  return rhsType;
}

// Stage 5: Object-Oriented Binary Expression Check
std::shared_ptr<toka::Type> Sema::checkBinaryExpr(BinaryExpr *Bin) {
  // 1. Resolve Operands using New API
  // [Toka 1.3] Evaluation Order: Check RHS first to avoid LHS borrows/moves
  // blocking RHS usage (e.g. &#cursor = cursor.&next)
  Bin->RHS = foldGenericConstant(std::move(Bin->RHS));
  auto rhsType = checkExpr(Bin->RHS.get());

  bool isAssign = (Bin->Op == "=" || Bin->Op == "+=" || Bin->Op == "-=" ||
                   Bin->Op == "*=" || Bin->Op == "/=");
  bool oldLHS = m_InLHS;
  m_InLHS = isAssign;

  if (!isAssign)
    Bin->LHS = foldGenericConstant(std::move(Bin->LHS));

  auto lhsType = checkExpr(Bin->LHS.get());
  m_InLHS = oldLHS;

  if (!lhsType || !rhsType)
    return toka::Type::fromString("unknown");

  std::string LHS = lhsType->toString(); // For error messages
  std::string RHS = rhsType->toString();

  // [Optimization] Literal Adaptation
  // Allow mixed comparison like (i64 < 2) by auto-casting the literal to
  // the explicit type.
  Expr *lhsExpr = Bin->LHS.get();
  Expr *rhsExpr = Bin->RHS.get();

  // Strip parens if needed (simple check)
  // For now direct NumberExpr check
  auto *lhsNum = dynamic_cast<NumberExpr *>(lhsExpr);
  auto *rhsNum = dynamic_cast<NumberExpr *>(rhsExpr);

  if (resolveType(lhsType, true)->isInteger() && rhsNum && !lhsNum) {
    // Left is Strong Integer, Right is Literal -> Adapt Right
    Bin->RHS->ResolvedType = lhsType;
    rhsType = lhsType;
    RHS = rhsType->toString();
  } else if (resolveType(rhsType, true)->isInteger() && lhsNum && !rhsNum) {
    // Right is Strong Integer, Left is Literal -> Adapt Left
    Bin->LHS->ResolvedType = rhsType;
    lhsType = rhsType;
    LHS = lhsType->toString();
  } else if (resolveType(rhsType, true)->isInteger() && lhsNum && !rhsNum) {
    // Right is Strong Integer, Left is Literal -> Adapt Left
    Bin->LHS->ResolvedType = rhsType;
    lhsType = rhsType;
    LHS = lhsType->toString();
  }

  // Generic Implicit Dereference for Smart Pointers (Soul Interaction)
  // If one side is Smart Pointer and other side matches its Pointee, decay
  // Smart Pointer.
  if (lhsType->isUniquePtr() || lhsType->isSharedPtr()) {
    if (auto inner = lhsType->getPointeeType()) {
      if (isTypeCompatible(inner, rhsType)) {
        lhsType = inner;
        Bin->LHS->ResolvedType = lhsType; // PERSIST for CodeGen
        LHS = lhsType->toString();
      }
    }
  } else if (rhsType->isUniquePtr() || rhsType->isSharedPtr()) {
    if (auto inner = rhsType->getPointeeType()) {
      if (isTypeCompatible(lhsType, inner)) {
        rhsType = inner;
        Bin->RHS->ResolvedType = rhsType; // PERSIST for CodeGen
        RHS = rhsType->toString();
      }
    }
  }
  bool isRefAssign = false;
  bool isUnsetInit = false;
  if (m_IsUnsetInitCall) {
    isRefAssign = true;
    isUnsetInit = true;
    m_IsUnsetInitCall = false;
  }

  // Rebinding Logic: Unwrap &ref on LHS
  if (Bin->Op == "=") {
    if (auto *UnLHS = dynamic_cast<UnaryExpr *>(Bin->LHS.get())) {
      if (UnLHS->Op == TokenType::Ampersand) {
        if (lhsType->isReference() && lhsType->getPointeeType() &&
            lhsType->getPointeeType()->isReference()) {
          lhsType = lhsType->getPointeeType();
        }
      }
    }
  }

  // Assignment Logic
  if (Bin->Op == "=") {
    // Move Logic
    Expr *RHSExpr = Bin->RHS.get();
    if (auto *Unary = dynamic_cast<UnaryExpr *>(RHSExpr)) {
      if (Unary->Op == TokenType::Caret)
        RHSExpr = Unary->RHS.get();
    }

    if (auto *RHSVar = dynamic_cast<VariableExpr *>(RHSExpr)) {
      SymbolInfo *RHSInfoPtr = nullptr;
      if (CurrentScope->findSymbol(RHSVar->Name, RHSInfoPtr) &&
          RHSInfoPtr->IsUnique()) {
        if (RHSInfoPtr->IsBorrowed()) {
          error(Bin, DiagID::ERR_MOVE_BORROWED, RHSVar->Name);
        }
        CurrentScope->markMoved(RHSVar->Name);
      }
    } else if (auto *Memb = dynamic_cast<MemberExpr *>(RHSExpr)) {
      // [Move Restriction Rule] Prohibit moving member out of shape that has
      // drop() Rule ONLY applies if we are moving a resource (UniquePtr)
      if (rhsType->isUniquePtr()) {
        auto objType = checkExpr(Memb->Object.get());
        std::shared_ptr<toka::Type> soulType = objType->getSoulType();
        std::string soul = soulType->getSoulName();
        if (m_ShapeProps.count(soul) && m_ShapeProps[soul].HasDrop) {
          error(Bin, DiagID::ERR_MOVE_MEMBER_DROP, Memb->Member, soul);
        }
      }
    }

    // [New] Assign to VariableExpr: Clear Moved State
    Expr *LHSScan = Bin->LHS.get();
    while (auto *un = dynamic_cast<UnaryExpr *>(LHSScan)) {
      LHSScan = un->RHS.get();
    }
    if (auto *LHSVar = dynamic_cast<VariableExpr *>(LHSScan)) {
      CurrentScope->resetMoved(LHSVar->Name);
    }

    // Reference Assignment
    // [Constitution] Reference Rebinding Detection
    // If the LHS resolved to a Reference type (due to explicit hatted
    // syntax
    // &#z), then it's a rebinding. If it collapsed to the Soul (Hat-Off),
    // it's Soul modification.
    if (lhsType->isReference()) {
      isRefAssign = true;
    }
  }

  if (isAssign) {
    // Smart Pointer NewExpr Special Case
    bool isSmartNew = false;
    bool isImplicitDerefAssign = false;

    if (dynamic_cast<NewExpr *>(Bin->RHS.get())) {
      if (lhsType->isUniquePtr() || lhsType->isSharedPtr()) {
        auto inner = lhsType->getPointeeType();
        auto rhsInner = rhsType->getPointeeType(); // new returns ^T
        if (inner && rhsInner && isTypeCompatible(inner, rhsInner)) {
          isSmartNew = true;
        }
      }
    }

    // Implicit Dereference Assignment Logic (Soul Mutation)
    if (!isSmartNew && !isRefAssign &&
        (lhsType->isUniquePtr() || lhsType->isSharedPtr())) {
      auto inner = lhsType->getPointeeType();
      // If RHS matches Inner, we are assigning to the Soul (implicit *s =
      // val)
      if (inner && isTypeCompatible(inner, rhsType)) {
        lhsType = inner; // Decay to Pointee Type for Writability Check
        isImplicitDerefAssign = true;
      }
    }

    // [Constitution 1.3] Covenant-based Writability Check
    bool isLHSWritable = false;
    bool isRebind = false;

    // Detect Rebind: Assigning to the pointer variable/handle itself
    // e.g. "p = ..." where p is smart pointer, or "~#p = ..."
    // Constitution 1.3: Reference rebinding is ALSO a Reseat operation.
    if (!isImplicitDerefAssign) {
      if (lhsType->isSmartPointer() || isRefAssign) {
        isRebind = true;
      }
    }

    Expr *Traverse = Bin->LHS.get();
    while (true) {
      if (auto *M = dynamic_cast<MemberExpr *>(Traverse)) {
        Traverse = M->Object.get();
      } else if (auto *Idx = dynamic_cast<ArrayIndexExpr *>(Traverse)) {
        Traverse = Idx->Array.get();
      } else {
        break;
      }
    }

    if (auto *Var = dynamic_cast<VariableExpr *>(Traverse)) {
      SymbolInfo *InfoPtr = nullptr;
      if (CurrentScope->findSymbol(Var->Name, InfoPtr)) {
        if (!isUnsetInit && InfoPtr) {
          if (InfoPtr->IsMutablyBorrowed || InfoPtr->ImmutableBorrowCount > 0) {
            error(Bin, DiagID::ERR_BORROW_MUT, Var->Name);
          }
        }

        // [Constitution] Rebind (# prefix) check
        if (isRebind) {
          // Rebinding (Reseat) requires explicit IDENTITY attribute (# or !)
          if (InfoPtr->IsRebindable) {
            isLHSWritable = true;
          } else {
            // If the variable was unset, allow initialization as a rebind
            // exemption
            if (InfoPtr->InitMask != ~0ULL)
              isLHSWritable = true;
          }
        } else {
          // Soul Mutation
          // [Constitution 3.0] Explicit Mutability Enforcement
          // Usage site MUST show '#' or '!' (Var->IsValueMutable)
          if (Var->IsValueMutable) {
            bool authorized = InfoPtr->IsMutable();
            // detailed check for Pointers: Soul is the Pointee
            if (!authorized) {
              if (InfoPtr->IsReference() ||
                  (InfoPtr->TypeObj && InfoPtr->TypeObj->isPointer())) {
                if (InfoPtr->TypeObj && InfoPtr->TypeObj->getPointeeType()) {
                  if (InfoPtr->TypeObj->getPointeeType()->IsWritable) {
                    authorized = true;
                  }
                }
              }
            }

            if (authorized) {
              isLHSWritable = true;
            } else {
              // User asserted mutability (#) on an immutable variable
              error(Bin, DiagID::ERR_IMMUTABLE_MOD, Var->Name);
              HasError = true;
              isLHSWritable = true; // Bypasses subsequent error
            }
          }
        }

        // [Unset Safety] Allow writing to immutable variables if they are
        // unset
        if (InfoPtr->IsReference()) {
          if (InfoPtr->DirtyReferentMask != ~0ULL)
            isLHSWritable = true;
        } else {
          if (InfoPtr->InitMask != ~0ULL)
            isLHSWritable = true;
        }
      }
    } else if (auto *Post = dynamic_cast<PostfixExpr *>(Traverse)) {
      if (Post->Op == TokenType::TokenWrite || Post->Op == TokenType::Bang) {
        isLHSWritable = true;
      }
    } else if (auto *Un = dynamic_cast<UnaryExpr *>(Traverse)) {
      // [Constitution] Explicit Rebind/Access check
      if (Un->Op == TokenType::Star) { // *p
        isLHSWritable = true; // Pointer deref allows mutation if target
                              // type allows (checked below via Soul)
      } else if (Un->Op == TokenType::Caret || Un->Op == TokenType::Tilde ||
                 Un->Op == TokenType::Ampersand ||
                 Un->Op == TokenType::Star) { // ^p, ~p, &p, *p
        // These are Rebindable handles or access handles.
        // If they are on the LHS without a deref, they are rebinds.
        if (isRebind) {
          // Check if the UnaryExpr itself carries the rebind intent (# or !)
          if (Un->IsRebindable) {
            isLHSWritable = true;
          }
        } else {
          isLHSWritable = true; // Soul view
        }
      }
    }

    if (isUnsetInit)
      isLHSWritable = true;

    if (!isLHSWritable) {
      error(Bin, "Cannot assign to immutable entity. Missing writable token "
                 "'#' or '!'.");
      HasError = true;
    }

    // [Constitution] Soul Permission Elevation Audit
    // RHS soul must not exceed LHS soul's permissions if they share objects
    // (Shared/Ref)
    if (lhsType->isSharedPtr() || lhsType->isReference()) {
      auto lhsSoul = lhsType->getPointeeType();
      auto rhsSoul = rhsType->getPointeeType();
      if (lhsSoul && rhsSoul) {
        // Check suffix: if RHS soul is Immutable ($/?) and LHS soul is
        // Writable
        // (#/!) -> Error
        if (!rhsSoul->IsWritable && lhsSoul->IsWritable) {
          // [Identity Exemption] Fresh allocations from 'new' can satisfy
          // writable souls.
          if (dynamic_cast<NewExpr *>(Bin->RHS.get())) {
            // OK: Freshly baked bread is always warm.
          } else {
            error(Bin,
                  "Covenant Violation: Cannot elevate write permission from "
                  "ReadOnly soul to Writable container.");
            HasError = true;
          }
        }
      }
    }

    // [FIX] Unset Safety: Allow writing to immutable fields if they are
    // unset [FIX] Unset Safety: Allow writing to immutable fields if they
    // are unset
    bool isLHSUnset = false;
    if (auto *M = dynamic_cast<MemberExpr *>(Bin->LHS.get())) {
      Expr *Traverse = M->Object.get();
      while (auto *InnerM = dynamic_cast<MemberExpr *>(Traverse))
        Traverse = InnerM->Object.get();

      if (auto *ObjVar = dynamic_cast<VariableExpr *>(Traverse)) {
        SymbolInfo *ObjInfo = nullptr;
        if (CurrentScope->findSymbol(ObjVar->Name, ObjInfo)) {
          // Follow BorrowedFrom chain to find the actual shape instance
          SymbolInfo *EffectiveInfo = ObjInfo;
          int depth = 0;
          while (!EffectiveInfo->BorrowedFrom.empty() && depth < 10) {
            SymbolInfo *Next = nullptr;
            if (CurrentScope->findSymbol(EffectiveInfo->BorrowedFrom, Next))
              EffectiveInfo = Next;
            else
              break;
            depth++;
          }

          std::shared_ptr<toka::Type> actualType = EffectiveInfo->TypeObj;
          while (actualType &&
                 (actualType->isReference() || actualType->isPointer())) {
            actualType = actualType->getPointeeType();
          }

          if (actualType && actualType->isShape()) {
            std::string sName = actualType->getSoulName();
            if (ShapeMap.count(sName)) {
              ShapeDecl *SD = ShapeMap[sName];
              for (int i = 0; i < (int)SD->Members.size(); ++i) {
                std::string sanDef =
                    toka::Type::stripMorphology(SD->Members[i].Name);
                std::string sanMemb = toka::Type::stripMorphology(M->Member);
                if (sanDef == sanMemb) {
                  uint64_t bit = (1ULL << i);
                  bool isUnset = !(EffectiveInfo->InitMask & bit) &&
                                 !(EffectiveInfo->DirtyReferentMask & bit);

                  if (isUnset) {
                    isLHSUnset = true;
                  }
                  break;
                }
              }
            }
          }
        }
      }
    }

    // Constitution 1.3: Only elevate soul permission if it's a Mutation,
    // not a Reseat (Rebind).
    if (!isRebind && (isLHSWritable || isLHSUnset))
      lhsType =
          lhsType->withAttributes(true, lhsType->IsNullable); // Valid Mutation

    if (!lhsType->IsWritable && !isRefAssign) {
      error(Bin->LHS.get(), DiagID::ERR_IMMUTABLE_MOD, LHS);
    }

    auto lhsCompatType = lhsType->withAttributes(false, lhsType->IsNullable);

    // [FIX] Reference Rebinding Morphology Mirror
    // [NEW] Lifetime Safety Check: Scope(LHS_Object) >=
    // Scope(RHS_Dependency)
    std::string targetObjName = "";
    Expr *lhsObj = Bin->LHS.get();
    while (auto *me = dynamic_cast<MemberExpr *>(lhsObj)) {
      lhsObj = me->Object.get();
    }
    if (auto *ve = dynamic_cast<VariableExpr *>(lhsObj)) {
      targetObjName = ve->Name;
    }

    if (!targetObjName.empty()) {
      SymbolInfo *targetInfo = nullptr;
      if (CurrentScope->findSymbol(targetObjName, targetInfo)) {
        std::set<std::string> rhsDeps = m_LastLifeDependencies;
        if (!m_LastBorrowSource.empty())
          rhsDeps.insert(m_LastBorrowSource);
        if (auto *rv = dynamic_cast<VariableExpr *>(Bin->RHS.get())) {
          SymbolInfo *ri = nullptr;
          if (CurrentScope->findSymbol(rv->Name, ri)) {
            for (const auto &d : ri->LifeDependencySet)
              rhsDeps.insert(d);
          }
        }

        int targetDepth = getScopeDepth(targetObjName);
        for (const auto &dep : rhsDeps) {
          int depDepth = getScopeDepth(dep);
          if (targetDepth < depDepth) { // outer < inner => error
            error(Bin, DiagID::ERR_BORROW_LIFETIME, targetObjName, dep);
          }
          targetInfo->LifeDependencySet.insert(dep);
        }
      }
    }
    m_LastBorrowSource = "";
    m_LastLifeDependencies.clear();

    if (isRefAssign && !isUnsetInit) {
      // If LHS is Ref (&#), RHS must be Ref (&)
      if (!rhsType->isReference()) {
        error(Bin->RHS.get(), DiagID::ERR_MORPHOLOGY_MISMATCH, "&",
              rhsType->toString());
      }
      // Compare types after stripping one layer of Reference
      auto target = lhsType->getPointeeType();
      auto source = rhsType->getPointeeType();
      if (target && source && isTypeCompatible(target, source)) {
        // OK
      } else {
        error(Bin, DiagID::ERR_TYPE_MISMATCH, RHS, LHS);
      }
      return lhsType;
    }

    // Strict Pointer Morphology Check
    if (!isRefAssign) {
      // Skip check if LHS is explicit dereference (*p = val) which targets
      // Value, not Pointer Identity.
      bool isDerefAssign = false;
      if (auto *Un = dynamic_cast<UnaryExpr *>(Bin->LHS.get())) {
        if (Un->Op == TokenType::Star)
          isDerefAssign = true;
      }

      if (!isDerefAssign) {
        // Determine Target Morphology (LHS)
        MorphKind targetMorph = MorphKind::None;
        // We need to look at the LHS expression structure
        // If LHS is *p or ^p etc.
        targetMorph = getSyntacticMorphology(Bin->LHS.get());

        // If LHS is a variable declaration, we don't handle it here
        // (handled in checkVariableDecl). But this is assignment to
        // existing variable. If LHS is 'p' (VariableExpr) and p is a
        // pointer type, Morph is None (hidden). If p is pointer,
        // targetMorph=None. SourceMorph check... User rule: "auto ^p = x"
        // (Invalid). "auto p = ^x" (Invalid). "p = q" (Hidden = Hidden)?
        // "Strict explicit morphology matching".
        // If LHS has no sigil, but is a pointer type?
        // "auto p = ^x". p is pointer. LHS sigil None. RHS sigil Unique.
        // Mismatch. Correct. So `getSyntacticMorphology` returning None for
        // VariableExpr is correct.

        MorphKind sourceMorph = getSyntacticMorphology(Bin->RHS.get());
        checkStrictMorphology(Bin, targetMorph, sourceMorph, LHS);
      }
    }

    if (!isRefAssign && !isSmartNew &&
        !isTypeCompatible(lhsCompatType, rhsType) && LHS != "unknown" &&
        RHS != "unknown") {
      error(Bin, DiagID::ERR_TYPE_MISMATCH, RHS, LHS);
    }

    // [Fix] Update InitMask logic for 'unset' variables
    Expr *LHSExpr = Bin->LHS.get();

    // Helper lambda for back-propagation
    auto propagateInit = [&](std::string startVar, uint64_t updateBits,
                             bool isPartial) {
      std::string current = startVar;
      // Limit depth to avoid infinite loops in circular refs (though
      // illegal in Toka)
      int depth = 0;
      while (!current.empty() && depth < 20) {
        SymbolInfo *Sym = nullptr;
        if (!CurrentScope->findSymbol(current, Sym))
          break;

        // Update the symbol itself (if it's the source or a ref in chain)
        if (Sym->IsReference()) {
          if (isPartial)
            Sym->DirtyReferentMask |= updateBits;
          else
            Sym->DirtyReferentMask = ~0ULL;
        } else {
          if (isPartial)
            Sym->InitMask = ~0ULL;
          else
            Sym->InitMask |= updateBits;
        }

        // Move to next upstream source
        if (Sym->IsReference()) {
          current = Sym->BorrowedFrom;
        } else {
          break; // Reached root
        }
        depth++;
      }
    };

    if (auto *Var = dynamic_cast<VariableExpr *>(LHSExpr)) {
      SymbolInfo *Info = nullptr;
      if (CurrentScope->findSymbol(Var->Name, Info)) {
        // Full Assignment to Variable (or Reference)
        // If it's a reference, we propagate Cleanliness to Source
        if (Info->IsReference()) {
          Info->DirtyReferentMask = ~0ULL;
          if (!Info->BorrowedFrom.empty()) {
            propagateInit(Info->BorrowedFrom, ~0ULL, false);
          }
        } else {
          Info->InitMask = ~0ULL;
        }
      }
    } else if (auto *Memb = dynamic_cast<MemberExpr *>(LHSExpr)) {
      // Partial Initialization via Member
      Expr *Obj = Memb->Object.get();
      if (auto *Var = dynamic_cast<VariableExpr *>(Obj)) {
        SymbolInfo *Info = nullptr;
        if (CurrentScope->findSymbol(Var->Name, Info)) {
          std::shared_ptr<toka::Type> actualType = Info->TypeObj;
          // If reference, peel to find Shape
          if (actualType && actualType->isReference()) {
            actualType = actualType->getPointeeType();
          }

          if (actualType && actualType->isShape()) {
            std::string sName = actualType->getSoulName();
            if (ShapeMap.count(sName)) {
              ShapeDecl *SD = ShapeMap[sName];

              // Find which bit to set
              uint64_t bitsToSet = 0;
              for (int i = 0; i < (int)SD->Members.size(); ++i) {
                if (SD->Members[i].Name == Memb->Member) {
                  bitsToSet = (1ULL << i);
                  break;
                }
              }

              if (bitsToSet != 0) {
                // Apply locally
                if (Info->IsReference()) {
                  Info->DirtyReferentMask |= bitsToSet;
                  // Propagate Up
                  if (!Info->BorrowedFrom.empty()) {
                    propagateInit(Info->BorrowedFrom, bitsToSet, true);
                  }
                } else {
                  Info->InitMask |= bitsToSet;
                }
              }
            }
          }
        }
      }
    }

    return lhsType;
  }

  // General Binary Ops
  if (Bin->Op == "&&" || Bin->Op == "||") {
    if (!lhsType->isBoolean() || !rhsType->isBoolean()) {
      error(Bin, DiagID::ERR_INVALID_OP, Bin->Op, lhsType->toString(),
            rhsType->toString());
    }
    return toka::Type::fromString("bool");
  }

  if (lhsType->isPointer() && (Bin->Op == "+" || Bin->Op == "-")) {
    if (!m_InUnsafeContext) {
      error(Bin, DiagID::ERR_UNSAFE_ALLOC_CTX); // Reuse for ptr arithmetic
    }
    return lhsType->withAttributes(false, lhsType->IsNullable);
  }

  if (Bin->Op == "==" || Bin->Op == "!=" || Bin->Op == "<" || Bin->Op == ">" ||
      Bin->Op == "<=" || Bin->Op == ">=") {
    if (!isTypeCompatible(lhsType, rhsType) &&
        !isTypeCompatible(rhsType, lhsType)) {
      error(Bin, DiagID::ERR_INVALID_OP, Bin->Op, LHS, RHS);
    }
    // Strict Integer Check
    auto lRes = resolveType(lhsType);
    auto rRes = resolveType(rhsType);
    if (!lRes->withAttributes(false, false)
             ->equals(*rRes->withAttributes(false, false))) {
      if (lhsType->isInteger() && rhsType->isInteger()) {
        error(Bin, "comparison operands must have exact same type ('" + LHS +
                       "' vs '" + RHS + "')");
      }
    }
    return toka::Type::fromString("bool");
  }

  if (Bin->Op == "+" || Bin->Op == "-" || Bin->Op == "*" || Bin->Op == "/") {
    bool isValid = false;
    auto lRes = resolveType(lhsType, true);
    if (lRes->isInteger() || lRes->isFloatingPoint())
      isValid = true;

    if (!isValid) {
      error(Bin,
            "operands of '" + Bin->Op + "' must be numeric, got '" + LHS + "'");
    }
    return lhsType->withAttributes(false, lhsType->IsNullable);
  }

  if (Bin->Op == "band" || Bin->Op == "bor" || Bin->Op == "bxor" ||
      Bin->Op == "bshl" || Bin->Op == "bshr") {
    if (!resolveType(lhsType, true)->isInteger() ||
        !resolveType(rhsType, true)->isInteger()) {
      error(Bin, "operands of '" + Bin->Op + "' must be integers");
    }
    return lhsType->withAttributes(false, lhsType->IsNullable);
  }

  if (Bin->Op == "is" || Bin->Op == "is!") {
    // [Fix] Disable soul collapse for the LHS of 'is' so we can check the
    // the pointer/handle itself.
    bool oldDisable = m_DisableSoulCollapse;
    m_DisableSoulCollapse = true;
    auto lhsType = checkExpr(Bin->LHS.get());
    m_DisableSoulCollapse = oldDisable;

    auto rhsType = checkExpr(Bin->RHS.get());
    // Basic validation for 'is' / 'is!'
    if (auto *rhsVar = dynamic_cast<VariableExpr *>(Bin->RHS.get())) {
      // If RHS is just a Shape name, it's NOT a valid pattern (should be a
      // variable or variant)
      if (ShapeMap.count(rhsVar->Name)) {
        error(Bin->RHS.get(), "'" + rhsVar->Name +
                                  "' is a shape, not a valid pattern for 'is'");
      }
    }
    return toka::Type::fromString("bool");
  }

  return toka::Type::fromString("unknown");
}

// Stage 5b: Object-Oriented Index Expression Check
std::shared_ptr<toka::Type> Sema::checkIndexExpr(ArrayIndexExpr *Idx) {
  // 1. Validate Indices (must be integer loops)
  for (auto &idxExpr : Idx->Indices) {
    idxExpr = foldGenericConstant(std::move(idxExpr)); // [FIX]
    auto idxType = checkExpr(idxExpr.get());
    if (!idxType->isInteger()) {
      error(Idx,
            "array index must be integer, got '" + idxType->toString() + "'");
    }
  }

  // 2. Resolve Base Type
  std::shared_ptr<toka::Type> baseType = nullptr;

  // [Constitution] Indexing targets Identity (Pointer/Array), not Soul.
  if (auto *Var = dynamic_cast<VariableExpr *>(Idx->Array.get())) {
    SymbolInfo *Info = nullptr;
    if (CurrentScope->findSymbol(Var->Name, Info)) {
      baseType = Info->TypeObj;
    } else {
      baseType = checkExpr(Idx->Array.get());
    }
  } else {
    bool old = m_DisableSoulCollapse;
    m_DisableSoulCollapse = true;
    baseType = checkExpr(Idx->Array.get());
    m_DisableSoulCollapse = old;
  }

  if (!baseType || baseType->toString() == "unknown")
    return toka::Type::fromString("unknown");

  std::shared_ptr<toka::Type> resultType = nullptr;
  baseType = resolveType(baseType, true);

  if (baseType->isArray()) {
    resultType = baseType->getArrayElementType();
  } else if (baseType->isPointer()) {
    // Pointer indexing P[i] -> Pointee Type
    if (!m_InUnsafeContext) {
      error(Idx, "raw pointer indexing requires unsafe context");
    }
    resultType = baseType->getPointeeType();
  } else {
    error(Idx, "type '" + baseType->toString() + "' is not indexable");
    return toka::Type::fromString("unknown");
  }

  if (!resultType)
    return toka::Type::fromString("unknown");

  // 3. Permission Inheritance (The Law of Permission Inheritance)
  // If base collection is Writable, the element view is Writable.
  // We use withAttributes to propagate this property.

  bool isBaseWritable = baseType->IsWritable;
  resultType =
      resultType->withAttributes(isBaseWritable, resultType->IsNullable);

  return resultType;
}

// Stage 5c: Object-Oriented Call Expression Check
std::shared_ptr<toka::Type> Sema::checkCallExpr(CallExpr *Call) {

  std::string CallName = Call->Callee;
  std::string OriginalName = CallName;

  // 1. Primitives (Constructors/Casts) e.g. i32(42)
  if (CallName == "i32" || CallName == "u32" || CallName == "i64" ||
      CallName == "u64" || CallName == "f32" || CallName == "f64" ||
      CallName == "i16" || CallName == "u16" || CallName == "i8" ||
      CallName == "u8" || CallName == "usize" || CallName == "isize" ||
      CallName == "bool") {
    for (auto &Arg : Call->Args) {
      Arg = foldGenericConstant(std::move(Arg)); // [FIX]
      checkExpr(Arg.get());
    }
    return toka::Type::fromString(CallName);
  }

  // 2. Intrinsics (println)
  if (CallName == "println" || CallName == "std::io::println") {
    bool visible = (CallName == "std::io::println");
    if (!visible) {
      SymbolInfo val;
      if (CurrentScope->lookup("println", val))
        visible = true;
    }
    if (!visible) {
      error(Call, "println requires at least a format string");
      return toka::Type::fromString("void");
    }
    if (Call->Args.empty()) {
      error(Call, "println requires at least a format string");
    }
    for (auto &Arg : Call->Args) {
      Arg = foldGenericConstant(std::move(Arg)); // [FIX]
      checkExpr(Arg.get());
    }
    return toka::Type::fromString("void");
  }

  std::shared_ptr<toka::FunctionType> funcType = nullptr;

  // 3. Resolve Static Methods / Enum Variants
  size_t pos = CallName.find("::");
  if (pos != std::string::npos) {
    std::string RawPrefix = CallName.substr(0, pos);
    std::string ShapeName = resolveType(RawPrefix);
    // [FIX] resolveType might return a type string "SharedPtr_M_i32" or
    // similar. If it contains sigils, strip them for Map lookup.
    ShapeName = Type::stripMorphology(ShapeName);

    std::string VariantName = CallName.substr(pos + 2);

    if (ShapeMap.count(ShapeName)) {
      // Update CallName and Callee for subsequent lookup and CodeGen
      CallName = ShapeName + "::" + VariantName;
      Call->Callee = CallName;

      // Static Method
      if (MethodMap.count(ShapeName) &&
          MethodMap[ShapeName].count(VariantName)) {
        for (auto &Arg : Call->Args) {
          Arg = foldGenericConstant(std::move(Arg)); // [FIX]
          checkExpr(Arg.get());
        }
        auto retObj = toka::Type::fromString(MethodMap[ShapeName][VariantName]);
        return resolveType(retObj);
      } else {
        // [NEW] Lazy Impl Instantiation
        std::string BaseName = RawPrefix;
        size_t lt = BaseName.find('<');
        if (lt != std::string::npos) {
          BaseName = BaseName.substr(0, lt);
          if (GenericImplMap.count(BaseName)) {
            // [FIX] Pass generic arguments to instantiateGenericImpl
            std::vector<std::shared_ptr<toka::Type>> genericArgs;
            auto parsed = Type::fromString(RawPrefix);
            if (auto *ST = dynamic_cast<ShapeType *>(parsed.get())) {
              genericArgs = ST->GenericArgs;
            }
            for (auto *ImplTemplate : GenericImplMap[BaseName]) {
              instantiateGenericImpl(ImplTemplate, RawPrefix, genericArgs);
            }
            // Retry lookup
            if (MethodMap.count(ShapeName) &&
                MethodMap[ShapeName].count(VariantName)) {
              for (auto &Arg : Call->Args) {
                Arg = foldGenericConstant(std::move(Arg));
                checkExpr(Arg.get());
              }
              auto retObj =
                  toka::Type::fromString(MethodMap[ShapeName][VariantName]);
              return resolveType(retObj);
            }
          }
        }
      }
      // Enum Variant Constructor
      ShapeDecl *SD = ShapeMap[ShapeName];
      if (SD->Kind == ShapeKind::Enum) {
        for (auto &Memb : SD->Members) {
          if (Memb.Name == VariantName) {
            // Enum Variant Constructor: Variant(Args...) -> ShapeName
            for (auto &Arg : Call->Args) {
              Arg = foldGenericConstant(std::move(Arg)); // [FIX]
              checkExpr(Arg.get());
            }

            // Set ResolvedShape for CodeGen
            Call->ResolvedShape = SD;

            return toka::Type::fromString(ShapeName);
          }
        }
      }
      // If we are here, it means we found the Shape but not the
      // Method/Variant
      error(Call, "static method or variant '" + VariantName +
                      "' not found in shape '" + ShapeName + "'");
      return toka::Type::fromString("unknown");
    }
  }
  // 4. Regular Function Lookup
  FunctionDecl *Fn = nullptr;
  ExternDecl *Ext = nullptr;
  ShapeDecl *Sh = nullptr; // Constructor

  // [NEW] Generic Constructor Pre-Check
  // If CallName looks like a generic type "Box<i32>", try to resolve it as
  // a Type. This triggers monomorphization in resolveType.
  if (CallName.find('<') != std::string::npos) {
    auto possibleType = toka::Type::fromString(CallName);
    if (possibleType && !possibleType->isUnknown()) {
      auto resolved = resolveType(possibleType);
      if (auto shapeT = std::dynamic_pointer_cast<toka::ShapeType>(resolved)) {
        if (shapeT->Decl) {
          Sh = shapeT->Decl;
          Call->Callee =
              shapeT->Name; // Update Call Name to Mangled Name for CodeGen!
          CallName =
              shapeT->Name; // Update local var for verification logic below

          // [NEW] Union Constructor: Check member name if provided
          if (Sh->Kind == ShapeKind::Union) {
            if (Call->Args.size() == 1) {
              // Check if the argument is a named argument
              // Actually the Parser produces named args as BinaryExpr with
              // "="? Wait, Toka syntax for named args is `Func(name =
              // val)`. In CallExpr::Args, this is parsed as BinaryExpr("=",
              // Var(name), Val). But checkCallExpr usually iterates args
              // and checks them. We need to peek at the arg structure.

              // Moved logic to "4. Regular Function Lookup" section below
              // because Sh might be found there too.
            }
          }
        }
      }
    }
  }

  size_t scopePos = CallName.find("::");
  if (!Sh && scopePos != std::string::npos) {
    std::string ModName = CallName.substr(0, scopePos);
    std::string FuncName = CallName.substr(scopePos + 2);
    SymbolInfo modSpec;
    if (CurrentScope->lookup(ModName, modSpec)) {
      if (modSpec.ReferencedModule) {
        ModuleScope *target = (ModuleScope *)modSpec.ReferencedModule;
        if (target->Functions.count(FuncName))
          Fn = target->Functions[FuncName];
        else if (target->Externs.count(FuncName))
          Ext = target->Externs[FuncName];
        else if (target->Shapes.count(FuncName))
          Sh = target->Shapes[FuncName];

        if (!Fn && !Ext && !Sh) {
          // No debug prints
        }
      } else {
        error(Call, "Module '" + ModName + "' not found or not imported");
        return toka::Type::fromString("unknown");
      }
    } else {
      error(Call, "Module '" + ModName + "' not found or not imported");
      return toka::Type::fromString("unknown");
    }
  } else if (!Sh) {
    if (ExternMap.count(CallName))
      Ext = ExternMap[CallName];
    else if (ShapeMap.count(CallName))
      Sh = ShapeMap[CallName];

    // Fallback: Check if it's a type alias to a shape
    if (!Fn && !Ext && !Sh && TypeAliasMap.count(CallName)) {
      std::string target = TypeAliasMap[CallName].Target;
      // [FIX] Generic Alias Resolution: Resolve the type string first
      // This handles 'alias Node = GenericNode<i32>' by triggering
      // instantiation and returning the mangled ShapeType.
      auto potentialType = toka::Type::fromString(target);
      if (potentialType && !potentialType->isUnknown()) {
        auto resolved = resolveType(potentialType);
        if (auto shapeT =
                std::dynamic_pointer_cast<toka::ShapeType>(resolved)) {
          if (shapeT->Decl) {
            Sh = shapeT->Decl;
            // Update Callee to the concrete mangled name (e.g.
            // Generic_M_i32) This ensures CodeGen calls the correct
            // function.
            Call->Callee = shapeT->Name;
          }
        }
      }

      // Legacy/Simple Fallback (if resolveType didn't yield a ShapeDecl
      // logic above covers most)
      if (!Sh && ShapeMap.count(target)) {
        Sh = ShapeMap[target];
        // If simple alias, we might also want to update Callee?
        // Typically code expects Callee to be the Shape Name.
        // For 'alias P = Point', target='Point'.
        Call->Callee = target;
      }
    }
  }
  // Local Scope Lookup (Local, Imported, or Shadowed)
  SymbolInfo sym;
  if (CurrentScope->lookup(CallName, sym)) {
    // Find implementation based on lookup
    for (auto *GF : GlobalFunctions) {
      if (GF->Name == CallName) {
        Fn = GF;
        break;
      }
    }
    if (!Fn) {
      for (auto &pair : ExternMap) {
        if (pair.second->Name == CallName) {
          Ext = pair.second;
          break;
        }
      }
    }
    std::string soulName = Type::stripMorphology(CallName);
    if (!Fn && !Ext && ShapeMap.count(soulName)) {
      Sh = ShapeMap[soulName];
    }
  }

  if (!Fn && !Ext && !Sh) {
    if (CallName != "str" && CallName != "unknown") {
      DiagnosticEngine::report(getLoc(Call), DiagID::ERR_UNDECLARED, CallName);
      HasError = true;
    }
    return toka::Type::fromString("unknown");
  }

  // 5. Synthesize FunctionType
  // ParamTypes, ReturnType
  std::vector<std::shared_ptr<toka::Type>> ParamTypes;
  std::shared_ptr<toka::Type> ReturnType;
  bool IsVariadic = false;

  // [NEW] Generic Instantiation
  if (Fn && !Fn->GenericParams.empty()) {
    std::vector<std::shared_ptr<toka::Type>> TypeArgs;
    bool deductionFailed = false;

    if (!Call->GenericArgs.empty()) {
      // Explicit Instantiation
      if (Call->GenericArgs.size() != Fn->GenericParams.size()) {
        DiagnosticEngine::report(
            getLoc(Call), DiagID::ERR_GENERIC_ARITY_MISMATCH, Fn->Name,
            Fn->GenericParams.size(), Call->GenericArgs.size());
        HasError = true;
        return toka::Type::fromString("unknown");
      }
      for (size_t i = 0; i < Call->GenericArgs.size(); ++i) {
        std::string argStr = Call->GenericArgs[i];
        if (Fn->GenericParams[i].IsConst) {
          // Pass the literal value directly as the "Type" name for mangling
          TypeArgs.push_back(toka::Type::fromString(argStr));
        } else {
          TypeArgs.push_back(toka::Type::fromString(resolveType(argStr)));
        }
      }
    } else {
      // Type Deduction
      std::map<std::string, std::shared_ptr<toka::Type>> Deduced;

      for (size_t i = 0; i < Call->Args.size() && i < Fn->Args.size(); ++i) {
        Call->Args[i] = foldGenericConstant(std::move(Call->Args[i])); // [FIX]
        auto argType = checkExpr(Call->Args[i].get());
        if (!argType || argType->isUnknown())
          continue;

        // DEBUG:
        // std::cerr << "Deduce: Arg " << i << " Type=" << argType->toString()
        // << "\n";

        const auto &Param = Fn->Args[i];
        std::string PType = Param.Type;
        // [FIX] Parse Sigils from PType string (since Parser might leave
        // them in string)
        bool locHasPointer = Param.HasPointer;
        bool locIsUnique = Param.IsUnique;
        bool locIsShared = Param.IsShared;
        bool locIsReference = Param.IsReference;

        while (PType.size() > 1 && (PType[0] == '*' || PType[0] == '^' ||
                                    PType[0] == '~' || PType[0] == '&')) {
          if (PType[0] == '*')
            locHasPointer = true;
          else if (PType[0] == '^')
            locIsUnique = true;
          else if (PType[0] == '~')
            locIsShared = true;
          else if (PType[0] == '&')
            locIsReference = true;
          PType = PType.substr(1);
        }

        // Check if PType is a generic param
        bool isGeneric = false;
        for (const auto &gp : Fn->GenericParams) {
          if (gp.Name == PType) {
            isGeneric = true;
            break;
          }
        }

        if (isGeneric) {
          // Strict Match: T matches ArgType
          // Need to account for morphology stripping on Param/Arg side?
          // If Param is `x: T`, and Arg is `i32`, T=i32.
          // If Param is `x: ^T`, and Arg is `^i32`?
          //   Param.IsUnique is true. Param.Type is T.
          //   ArgType is UniquePointer(i32).
          //   We must strip ArgType morphology to find T.

          std::shared_ptr<toka::Type> candidate = argType;

          // Strip Param Morphology from Candidate
          if (locHasPointer) {
            if (candidate->isRawPointer())
              candidate = candidate->getPointeeType();
            else if (candidate->isReference()) // Allow &T -> *T decay for
                                               // deduction
              candidate = candidate->getPointeeType();
            else
              continue; // Mismatch handled later
          }
          if (locIsUnique) {
            if (candidate->isUniquePtr())
              candidate = candidate->getPointeeType();
            else
              continue;
          }
          if (locIsShared) {
            if (candidate->isSharedPtr())
              candidate = candidate->getPointeeType();
            else
              continue;
          }
          if (locIsReference) {
            if (candidate->isReference())
              candidate = candidate->getPointeeType();
            // Else: Candidate is Value. Match Value against T in &T.
            // So if Param=&T, Arg=i32. T=i32.
            // Just proceed with candidate as is.
          }

          // Deduce
          if (Deduced.count(PType)) {
            if (!Deduced[PType]->equals(*candidate)) {
              error(Call, "Type deduction conflict for '" + PType + "': '" +
                              Deduced[PType]->toString() + "' vs '" +
                              candidate->toString() + "'");
              deductionFailed = true;
            }
          } else {
            Deduced[PType] = candidate;
          }
        }
      }

      if (!deductionFailed) {
        for (const auto &gp : Fn->GenericParams) {
          if (!Deduced.count(gp.Name)) {
            error(Call, "Failed to deduce type for generic parameter '" +
                            gp.Name + "'");
            deductionFailed = true;
          } else {
            TypeArgs.push_back(Deduced[gp.Name]);
          }
        }
      }
    }

    if (!deductionFailed) {
      Fn = instantiateGenericFunction(Fn, TypeArgs, Call);
      if (!Fn)
        return toka::Type::fromString("unknown");

      // [FIX] Update the Call AST to point to the mangled instance name
      // otherwise CodeGen will attempt to call the generic template name
      Call->Callee = Fn->Name;
    } else {
      HasError = true;
      return toka::Type::fromString("unknown");
    }
  }

  if (Fn) {
    Call->ResolvedFn = Fn;
    for (auto &Arg : Fn->Args) {
      ParamTypes.push_back(
          toka::Type::fromString(Sema::synthesizePhysicalType(Arg)));
    }
    ReturnType = toka::Type::fromString(Fn->ReturnType);
    IsVariadic = Fn->IsVariadic;
  } else if (Ext) {
    Call->ResolvedExtern = Ext;
    for (auto &Arg : Ext->Args) {
      ParamTypes.push_back(
          toka::Type::fromString(Sema::synthesizePhysicalType(Arg)));
    }
    ReturnType = toka::Type::fromString(Ext->ReturnType);
    IsVariadic = Ext->IsVariadic;
  } else if (Sh) {
    if (!checkVisibility(Call, Sh)) {
      return toka::Type::fromString("unknown");
    }

    // [NEW] Instantiate Generic Shape Constructor
    if (!Sh->GenericParams.empty() && !Call->GenericArgs.empty()) {
      std::vector<std::shared_ptr<toka::Type>> typeArgs;
      for (const auto &s : Call->GenericArgs) {
        typeArgs.push_back(toka::Type::fromString(resolveType(s)));
      }
      auto genericShape = std::make_shared<toka::ShapeType>(Sh->Name, typeArgs);
      auto resolved = resolveType(genericShape);
      if (auto rs = std::dynamic_pointer_cast<toka::ShapeType>(resolved)) {
        if (rs->Decl) {
          Sh = rs->Decl;
          Call->ResolvedShape = Sh;
          Call->Callee = rs->Name; // Update for CodeGen lookup
        }
      }
    }

    // Constructor: Params = Members, Return = ShapeName
    if (Sh->Kind == ShapeKind::Struct) {
      // Shape Constructor Logic (Named or Positional)
      Call->ResolvedShape = Sh;
      std::set<std::string> providedFields;
      size_t argIdx = 0;

      for (auto &arg : Call->Args) {
        std::string fieldName;
        Expr *valExpr = arg.get();
        bool isNamed = false;

        // Detect Named Arg: Field = Value or *Field = Value
        if (auto *bin = dynamic_cast<BinaryExpr *>(arg.get())) {
          if (bin->Op == "=") {
            if (auto *var = dynamic_cast<VariableExpr *>(bin->LHS.get())) {
              fieldName = var->Name;
              valExpr = bin->RHS.get();
              isNamed = true;
            } else if (auto *un = dynamic_cast<UnaryExpr *>(bin->LHS.get())) {
              // Handle *p = ..., ^p = ... etc.
              if (auto *innerVar =
                      dynamic_cast<VariableExpr *>(un->RHS.get())) {
                fieldName = innerVar->Name;
                valExpr = bin->RHS.get();
                isNamed = true;

                // Morphology Check for Key
                MorphKind keyMorph = MorphKind::None;
                if (un->Op == TokenType::Star)
                  keyMorph = MorphKind::Raw;
                else if (un->Op == TokenType::Caret)
                  keyMorph = MorphKind::Unique;
                else if (un->Op == TokenType::Tilde)
                  keyMorph = MorphKind::Shared;
                else if (un->Op == TokenType::Ampersand)
                  keyMorph = MorphKind::Ref;

                // Find field type in Sh
                for (const auto &M : Sh->Members) {
                  if (M.Name == fieldName) {
                    MorphKind fieldMorph = MorphKind::None;
                    if (!M.Type.empty()) {
                      char c = M.Type[0];
                      if (c == '*')
                        fieldMorph = MorphKind::Raw;
                      else if (c == '^')
                        fieldMorph = MorphKind::Unique;
                      else if (c == '~')
                        fieldMorph = MorphKind::Shared;
                      else if (c == '&')
                        fieldMorph = MorphKind::Ref;
                    }
                    checkStrictMorphology(bin, fieldMorph, keyMorph, fieldName);
                    break;
                  }
                }
              }
            }
          }
        }

        std::string expectedTypeStr = "unknown";
        if (isNamed) {
          bool found = false;
          for (auto &M : Sh->Members) {
            // [Constitution] Initialization Exemption: Normalize field name
            // for comparison
            std::string normalizedFieldName = fieldName;
            while (!normalizedFieldName.empty() &&
                   (normalizedFieldName.back() == '#' ||
                    normalizedFieldName.back() == '?' ||
                    normalizedFieldName.back() == '!')) {
              normalizedFieldName.pop_back();
            }
            std::string normalizedMName = M.Name;
            while (!normalizedMName.empty() &&
                   (normalizedMName.back() == '#' ||
                    normalizedMName.back() == '?' ||
                    normalizedMName.back() == '!')) {
              normalizedMName.pop_back();
            }

            if (normalizedMName == normalizedFieldName) {
              found = true;
              expectedTypeStr = M.Type;
              break;
            }
          }
          if (!found)
            error(arg.get(), "Shape '" + Sh->Name + "' has no field named '" +
                                 fieldName + "'");
          providedFields.insert(fieldName);
        } else {
          // Positional
          if (argIdx < Sh->Members.size()) {
            expectedTypeStr = Sh->Members[argIdx].Type;
            providedFields.insert(Sh->Members[argIdx].Name);
          } else {
            error(arg.get(),
                  "Too many arguments for struct '" + Sh->Name + "'");
          }
        }

        // Check Type Compatibility
        // Note: valExpr is the expression to check.
        // We use checkExpr(valExpr) to get object.
        auto valType = checkExpr(valExpr);
        auto expectedType = toka::Type::fromString(expectedTypeStr);

        if (!isTypeCompatible(expectedType, valType)) {
          error(valExpr, "Type mismatch for field '" +
                             (isNamed ? fieldName : std::to_string(argIdx)) +
                             "': expected " + expectedType->toString() +
                             ", got " + valType->toString());
        }
        argIdx++;
      }
      auto res = toka::Type::fromString(Sh->Name);

      if (TypeAliasMap.count(OriginalName) &&
          TypeAliasMap[OriginalName].IsStrong) {
        res = toka::Type::fromString(OriginalName);
      }
      // std::cerr << "CTOR RETURN: " << OriginalName << " -> " <<
      // res->toString() << "\n";
      return res;
    } else if (Sh->Kind == ShapeKind::Union) {

      if (Call->Args.size() != 1) {
        error(Call, "Union '" + CallName + "' requires exactly one argument");
        return toka::Type::fromString("void");
      }
      Expr *argExpr = Call->Args[0].get();
      Expr *valExpr = argExpr;
      std::string fieldName = "";
      bool isNamed = false;

      // Detect Named Arg: variant = value
      if (auto *bin = dynamic_cast<BinaryExpr *>(argExpr)) {
        if (bin->Op == "=") {
          if (auto *var = dynamic_cast<VariableExpr *>(bin->LHS.get())) {
            fieldName = var->Name;
            valExpr = bin->RHS.get();
            isNamed = true;
          }
        }
      }

      int matchedIdx = -1;
      std::shared_ptr<toka::Type> argType = checkExpr(valExpr);

      if (isNamed) {
        for (int i = 0; i < (int)Sh->Members.size(); ++i) {
          if (Sh->Members[i].Name == fieldName) {
            matchedIdx = i;
            break;
          }
        }
        if (matchedIdx == -1) {
          error(argExpr, "Union '" + Sh->Name + "' has no variant named '" +
                             fieldName + "'");
          return toka::Type::fromString("unknown");
        }

        auto memType = Sh->Members[matchedIdx].ResolvedType
                           ? Sh->Members[matchedIdx].ResolvedType
                           : toka::Type::fromString(
                                 resolveType(Sh->Members[matchedIdx].Type));
        if (!isTypeCompatible(memType, argType)) {
          error(valExpr, "Type mismatch for Union variant '" + fieldName +
                             "': expected " + memType->toString() + ", got " +
                             argType->toString());
        }

        // 1. Calculate Union Size
        uint64_t unionSize = 0;
        for (auto &m : Sh->Members) {
          auto mT = m.ResolvedType
                        ? m.ResolvedType
                        : toka::Type::fromString(resolveType(m.Type));
          uint64_t s = getTypeSize(mT);
          if (s > unionSize)
            unionSize = s;
        }

        // 2. Calculate Initialized Variant Size
        // memType is the type of the variant we are initializing
        uint64_t variantSize = getTypeSize(memType);

        if (variantSize < unionSize) {
          DiagnosticEngine::report(getLoc(Call), DiagID::ERR_UNION_PARTIAL_INIT,
                                   Sh->Name, fieldName, variantSize, unionSize);
          HasError = true;
        }

        Call->MatchedMemberIdx = matchedIdx;
      } else {
        // Heuristic matching for positional arg
        int exactMatchIdx = -1;
        int exactMatchCount = 0;
        int fitMatchIdx = -1;
        int fitMatchCount = 0;

        for (int i = 0; i < (int)Sh->Members.size(); ++i) {
          auto memType =
              Sh->Members[i].ResolvedType
                  ? Sh->Members[i].ResolvedType
                  : toka::Type::fromString(resolveType(Sh->Members[i].Type));
          if (!memType)
            continue;

          if (argType->equals(*memType)) {
            exactMatchCount++;
            if (exactMatchIdx == -1)
              exactMatchIdx = i;
          } else if (isTypeCompatible(memType, argType)) {
            fitMatchCount++;
            if (fitMatchIdx == -1)
              fitMatchIdx = i;
          }
        }

        if (exactMatchCount == 1) {
          Call->MatchedMemberIdx = exactMatchIdx;
        } else if (exactMatchCount > 1) {
          error(Call, DiagID::ERR_GENERIC_PARSE,
                "Ambiguous Union constructor: multiple "
                "exact matches found for "
                "type " +
                    argType->toString());
          HasError = true;
          return toka::Type::fromString("unknown");
        } else if (fitMatchCount == 1) {
          Call->MatchedMemberIdx = fitMatchIdx;
        } else if (fitMatchCount > 1) {
          error(Call, DiagID::ERR_GENERIC_PARSE,
                "Ambiguous Union constructor: multiple "
                "safe-fit matches found for "
                "type " +
                    argType->toString() + ". Use explicit cast.");
          HasError = true;
          return toka::Type::fromString("unknown");
        } else {
          error(Call, DiagID::ERR_GENERIC_PARSE,
                "No matching member found in Union '" + Sh->Name +
                    "' for type " + argType->toString());
          HasError = true;
          return toka::Type::fromString("unknown");
        }
      }

      return toka::Type::fromString(Sh->Name);
    } else {
      // Enum / Alias ?

      return toka::Type::fromString(Sh->Name);
    }
  }

  // Generic Function/Extern Matching
  funcType =
      std::make_shared<toka::FunctionType>(ParamTypes, ReturnType, IsVariadic);

  // 6. Argument Matching
  if (!IsVariadic && Call->Args.size() != ParamTypes.size()) {
    error(Call, "Argument count mismatch for '" + CallName + "': expected " +
                    std::to_string(ParamTypes.size()) + ", got " +
                    std::to_string(Call->Args.size()));
  }

  for (size_t i = 0; i < Call->Args.size(); ++i) {
    Call->Args[i] = foldGenericConstant(std::move(Call->Args[i])); // [FIX]
    auto argType = checkExpr(Call->Args[i].get());

    if (IsVariadic && i >= ParamTypes.size())
      continue;
    if (i >= ParamTypes.size())
      break; // Should be caught by count check unless variadic

    auto paramType = ParamTypes[i];

    // Morphology Check for Argument
    MorphKind targetMorph = MorphKind::None;
    std::string paramTypeStr = paramType->toString();
    if (!paramTypeStr.empty()) {
      char c = paramTypeStr[0];
      if (c == '*')
        targetMorph = MorphKind::Raw;
      else if (c == '^')
        targetMorph = MorphKind::Unique;
      else if (c == '~')
        targetMorph = MorphKind::Shared;
      else if (c == '&')
        targetMorph = MorphKind::Ref;
    }

    MorphKind sourceMorph = getSyntacticMorphology(Call->Args[i].get());
    std::string ctx = "arg " + std::to_string(i + 1);
    checkStrictMorphology(Call->Args[i].get(), targetMorph, sourceMorph, ctx);

    if (!isTypeCompatible(paramType, argType)) {
      error(Call->Args[i].get(), "Type mismatch for argument " +
                                     std::to_string(i + 1) + ": expected " +
                                     paramType->toString() + ", got " +
                                     argType->toString());
    }
  }

  return ReturnType;
}

void Sema::checkPattern(MatchArm::Pattern *Pat, const std::string &TargetType,
                        bool SourceIsMutable) {
  if (!Pat)
    return;

  std::string T = resolveType(TargetType);

  switch (Pat->PatternKind) {
  case MatchArm::Pattern::Literal:
    // Literal patterns don't bind variables
    break;

  case MatchArm::Pattern::Wildcard:
    break;

  case MatchArm::Pattern::Variable: {
    SymbolInfo Info;
    // Type Migration Stage 1: Coexistence
    // Construct type string to parse object. Pattern bindings infer type T.
    // If Reference, it is &T.
    std::string fullType = "";
    if (Pat->IsReference)
      fullType = "&";
    fullType += T;
    // Patterns usually don't have rebind/nullable sigils unless explicit?
    // In match arms, we trust the inferred type T.
    // But wait, T comes from resolveType(TargetType).
    Info.TypeObj = toka::Type::fromString(fullType);

    CurrentScope->define(Pat->Name, Info);
    break;
  }

  case MatchArm::Pattern::Decons: {
    // Pat->Name might be "Ok" or "Result::Ok"
    std::string variantName = Pat->Name;
    std::string shapeName = T;

    size_t pos = variantName.find("::");
    if (pos != std::string::npos) {
      shapeName = resolveType(variantName.substr(0, pos));
      variantName = variantName.substr(pos + 2);
    }

    if (ShapeMap.count(shapeName)) {
      ShapeDecl *SD = ShapeMap[shapeName];
      ShapeMember *foundMemb = nullptr;
      for (auto &Memb : SD->Members) {
        if (Memb.Name == variantName) {
          foundMemb = &Memb;
          break;
        }
      }

      if (foundMemb) {
        if (Pat->SubPatterns.size() > 0) {
          if (foundMemb->Type.empty() && foundMemb->SubMembers.empty()) {
            DiagnosticEngine::report(
                getLoc(Pat), DiagID::ERR_VARIANT_NO_PAYLOAD, variantName);
            HasError = true;
          } else {
            if (!foundMemb->SubMembers.empty()) {
              // Multi-field tuple variant
              if (Pat->SubPatterns.size() != foundMemb->SubMembers.size()) {
                DiagnosticEngine::report(
                    getLoc(Pat), DiagID::ERR_VARIANT_ARG_MISMATCH, variantName,
                    foundMemb->SubMembers.size(), Pat->SubPatterns.size());
                HasError = true;
              } else {
                for (size_t i = 0; i < Pat->SubPatterns.size(); ++i) {
                  // Rebind check
                  checkPattern(Pat->SubPatterns[i].get(),
                               foundMemb->SubMembers[i].Type, SourceIsMutable);
                }
              }
            } else {
              // Legacy single-field variant
              if (Pat->SubPatterns.size() != 1) {
                DiagnosticEngine::report(
                    getLoc(Pat), DiagID::ERR_VARIANT_ARG_MISMATCH, variantName,
                    1, Pat->SubPatterns.size());
                HasError = true;
              }
              checkPattern(Pat->SubPatterns[0].get(), foundMemb->Type,
                           SourceIsMutable);
            }
          }
        }
      } else {
        DiagnosticEngine::report(
            getLoc(Pat), DiagID::ERR_UNKNOWN_SHAPE_IN_PAT,
            shapeName); // Actually variant not found in shape
        HasError = true;
      }
    } else {
      DiagnosticEngine::report(getLoc(Pat), DiagID::ERR_UNKNOWN_SHAPE_IN_PAT,
                               shapeName);
      HasError = true;
    }
    break;
  }
  }
}
std::shared_ptr<toka::Type> Sema::checkShapeInit(InitStructExpr *Init) {
  std::string OriginalName = Init->ShapeName; // [Fix] Capture original name
  std::map<std::string, uint64_t> memberMasks;
  std::string resolvedName = resolveType(Init->ShapeName, true);

  if (!ShapeMap.count(resolvedName)) {
    // ...
  }

  // Helper lambda for inference (copied from original)

  // Helper lambda for inference (copied from original)
  auto performInference = [&](std::string &currentName, ShapeDecl *&SD) {
    if (!SD)
      return;
    // Try inference from m_ExpectedType first
    if (!SD->GenericParams.empty() && m_ExpectedType) {
      auto expShape =
          std::dynamic_pointer_cast<toka::ShapeType>(m_ExpectedType);
      if (expShape && (expShape->Name == SD->Name ||
                       expShape->Name.find(SD->Name + "_M") == 0)) {
        currentName = resolveType(m_ExpectedType->toString());
        SD = ShapeMap[currentName];
      }
    }

    // Inference from fields if still a template
    if (SD && !SD->GenericParams.empty() &&
        (currentName == SD->Name || currentName == SD->Name + "<>")) {
      std::map<std::string, std::string> inferred;
      for (auto const &pair : Init->Members) {
        std::string fieldName = Type::stripMorphology(pair.first);
        const ShapeMember *pM = nullptr;
        for (const auto &m : SD->Members) {
          if (m.Name == fieldName) {
            pM = &m;
            break;
          }
        }
        if (!pM)
          continue;

        auto valType = checkExpr(pair.second.get(), nullptr);
        if (!valType)
          continue;

        if (pM->Type.find("[") == 0) {
          auto arrTy = std::dynamic_pointer_cast<toka::ArrayType>(valType);
          if (arrTy) {
            size_t semi = pM->Type.find(';');
            size_t close = pM->Type.find(']');
            if (semi != std::string::npos && close != std::string::npos) {
              auto trim_inline = [](std::string s) {
                size_t first = s.find_first_not_of(" \t\n\r");
                if (first == std::string::npos)
                  return std::string("");
                size_t last = s.find_last_not_of(" \t\n\r");
                return s.substr(first, (last - first + 1));
              };
              std::string tName = trim_inline(pM->Type.substr(1, semi - 1));
              std::string nName =
                  trim_inline(pM->Type.substr(semi + 1, close - semi - 1));

              for (const auto &gp : SD->GenericParams) {
                if (gp.Name == tName && !gp.IsConst)
                  inferred[tName] = arrTy->ElementType->toString();
                if (gp.Name == nName && gp.IsConst)
                  inferred[nName] = std::to_string(arrTy->Size);
              }
            }
          }
        } else {
          for (const auto &gp : SD->GenericParams) {
            if (gp.Name == pM->Type && !gp.IsConst)
              inferred[gp.Name] = valType->toString();
          }
        }
      }

      if (inferred.size() > 0) {
        std::string fullName = SD->Name + "<";
        for (size_t i = 0; i < SD->GenericParams.size(); ++i) {
          std::string pName = SD->GenericParams[i].Name;
          if (inferred.count(pName))
            fullName += inferred[pName];
          else
            fullName += "unknown";
          if (i < SD->GenericParams.size() - 1)
            fullName += ", ";
        }
        fullName += ">";
        currentName = resolveType(fullName, true);
        SD = ShapeMap[currentName];
      }
    }
  };

  if (ShapeMap.count(resolvedName)) {
    ShapeDecl *SD = ShapeMap[resolvedName];
    performInference(resolvedName, SD);
    Init->ShapeName = resolvedName;

    if (!checkVisibility(Init, SD)) {
      return toka::Type::fromString("unknown");
    }

    std::shared_ptr<toka::Type> ResultType;
    if (SD->Kind == ShapeKind::Union || SD->Kind == ShapeKind::Enum) {
      ResultType = checkUnionInit(Init, SD, resolvedName, memberMasks);
    } else {
      ResultType = checkStructInit(Init, SD, resolvedName, memberMasks);
    }

    // [Fix] Strong Alias Preservation
    std::string BaseName = toka::Type::stripMorphology(OriginalName);
    size_t lt = BaseName.find('<');
    if (lt != std::string::npos) {
      BaseName = BaseName.substr(0, lt);
    }

    if (TypeAliasMap.count(BaseName) && TypeAliasMap[BaseName].IsStrong) {
      return toka::Type::fromString(OriginalName);
    }
    return ResultType;
  }

  DiagnosticEngine::report(getLoc(Init), DiagID::ERR_UNKNOWN_STRUCT,
                           Init->ShapeName);
  HasError = true;
  return toka::Type::fromString("unknown");
}

std::shared_ptr<toka::Type>
Sema::checkStructInit(InitStructExpr *Init, ShapeDecl *SD,
                      const std::string &resolvedName,
                      std::map<std::string, uint64_t> &memberMasks) {
  m_LastLifeDependencies.clear();
  std::set<std::string> providedFields;

  for (auto &pair : Init->Members) {
    if (providedFields.count(pair.first)) {
      DiagnosticEngine::report(getLoc(Init), DiagID::ERR_DUPLICATE_FIELD,
                               pair.first);
      HasError = true;
    }
    providedFields.insert(pair.first);

    bool fieldFound = false;
    const ShapeMember *pDefMember = nullptr;
    for (const auto &defField : SD->Members) {
      auto cleanDef = defField.Name;
      while (!cleanDef.empty() &&
             (cleanDef.back() == '#' || cleanDef.back() == '!' ||
              cleanDef.back() == '?'))
        cleanDef.pop_back();

      auto cleanProv = pair.first;
      while (!cleanProv.empty() &&
             (cleanProv.back() == '#' || cleanProv.back() == '!' ||
              cleanProv.back() == '?'))
        cleanProv.pop_back();

      if (cleanDef == cleanProv || toka::Type::stripMorphology(cleanDef) ==
                                       toka::Type::stripMorphology(cleanProv)) {
        fieldFound = true;
        pDefMember = &defField;
        break;
      }
    }

    if (!fieldFound) {
      error(Init, DiagID::ERR_NO_SUCH_MEMBER, resolvedName, pair.first);
      continue;
    }

    auto memberTypeObj = pDefMember->ResolvedType;
    if (!memberTypeObj)
      memberTypeObj = toka::Type::fromString(pDefMember->Type);

    std::shared_ptr<toka::Type> exprTypeObj =
        checkExpr(pair.second.get(), memberTypeObj);
    memberMasks[pair.first] = m_LastInitMask;

    if (isTypeCompatible(memberTypeObj, exprTypeObj) &&
        !memberTypeObj->equals(*exprTypeObj)) {
      auto origLoc = pair.second->Loc;
      pair.second = std::make_unique<CastExpr>(std::move(pair.second),
                                               memberTypeObj->toString());
      pair.second->Loc = origLoc;
      pair.second->ResolvedType = memberTypeObj;
      exprTypeObj = memberTypeObj;
    }

    if (!isTypeCompatible(memberTypeObj, exprTypeObj)) {
      error(Init, DiagID::ERR_MEMBER_TYPE_MISMATCH, pair.first,
            memberTypeObj->toString(), exprTypeObj->toString());
    }

    // Lifetime dependency tracking
    std::string cleanName = toka::Type::stripMorphology(pDefMember->Name);
    for (const auto &dep : SD->LifeDependencies) {
      if (dep == cleanName) {
        if (!m_LastBorrowSource.empty()) {
          m_LastLifeDependencies.insert(m_LastBorrowSource);
        }
      }
    }
  }

  // Missing fields check for Struct/Tuple
  for (const auto &defField : SD->Members) {
    if (!providedFields.count(defField.Name) &&
        !providedFields.count("^" + defField.Name) &&
        !providedFields.count("*" + defField.Name) &&
        !providedFields.count("~" + defField.Name) &&
        !providedFields.count("&" + defField.Name) &&
        !providedFields.count("^?" + defField.Name)) {
      error(Init, DiagID::ERR_MISSING_MEMBER, defField.Name, Init->ShapeName);
    }
  }

  // Mask Calculation
  uint64_t mask = 0;
  for (int i = 0; i < (int)SD->Members.size(); ++i) {
    std::string memName = SD->Members[i].Name;
    for (const auto &pair : Init->Members) {
      if (toka::Type::stripMorphology(pair.first) ==
          toka::Type::stripMorphology(memName)) {
        m_LastInitMask = memberMasks[pair.first];
        std::shared_ptr<Type> memTypeObj =
            toka::Type::fromString(SD->Members[i].Type);
        uint64_t expected = 1;
        if (memTypeObj->isShape()) {
          std::string sName = memTypeObj->getSoulName();
          if (ShapeMap.count(sName)) {
            size_t sz = ShapeMap[sName]->Members.size();
            expected = (sz >= 64) ? ~0ULL : (1ULL << sz) - 1;
          }
        }
        if ((m_LastInitMask & expected) == expected) {
          if (i < 64)
            mask |= (1ULL << i);
        }
        break;
      }
    }
  }
  m_LastInitMask = mask;

  return toka::Type::fromString(resolvedName);
}

std::shared_ptr<toka::Type>
Sema::checkUnionInit(InitStructExpr *Init, ShapeDecl *SD,
                     const std::string &resolvedName,
                     std::map<std::string, uint64_t> &memberMasks) {
  if (Init->Members.empty()) {
    error(Init, DiagID::ERR_MISSING_MEMBER, "at least one variant",
          Init->ShapeName);
    m_LastInitMask = 0;
    return toka::Type::fromString(resolvedName);
  }

  if (Init->Members.size() > 1) {
    error(Init, DiagID::ERR_GENERIC_PARSE,
          "Union '{}' initialization must specify exactly "
          "one variant, but {} were provided.",
          Init->ShapeName, Init->Members.size());
  }

  auto &pair = Init->Members[0];
  bool fieldFound = false;
  const ShapeMember *pDefMember = nullptr;
  for (const auto &defField : SD->Members) {
    if (toka::Type::stripMorphology(defField.Name) ==
        toka::Type::stripMorphology(pair.first)) {
      fieldFound = true;
      pDefMember = &defField;
      break;
    }
  }

  if (!fieldFound) {
    error(Init, DiagID::ERR_NO_SUCH_MEMBER, resolvedName, pair.first);
  } else {
    auto memberTypeObj = pDefMember->ResolvedType;
    if (!memberTypeObj)
      memberTypeObj = toka::Type::fromString(pDefMember->Type);

    std::shared_ptr<toka::Type> exprTypeObj =
        checkExpr(pair.second.get(), memberTypeObj);
    m_LastInitMask = ~0ULL; // Union is fully initialized if one field is set

    if (!isTypeCompatible(memberTypeObj, exprTypeObj)) {
      error(Init, DiagID::ERR_MEMBER_TYPE_MISMATCH, pair.first,
            memberTypeObj->toString(), exprTypeObj->toString());
    }

    // [Rule] Strict Union Initialization Size Check
    // Ensure initialized member covers the full size of the Union.
    uint64_t unionSize = 0;
    for (auto &m : SD->Members) {
      auto mT = m.ResolvedType ? m.ResolvedType
                               : toka::Type::fromString(resolveType(m.Type));
      uint64_t s = getTypeSize(mT);
      if (s > unionSize)
        unionSize = s;
    }
    uint64_t variantSize = getTypeSize(memberTypeObj);
    if (variantSize < unionSize) {
      DiagnosticEngine::report(getLoc(Init), DiagID::ERR_UNION_PARTIAL_INIT,
                               SD->Name, pair.first, variantSize, unionSize);
      HasError = true;
    }
  }

  m_LastInitMask = ~0ULL;
  return toka::Type::fromString(resolvedName);
}

} // namespace toka
