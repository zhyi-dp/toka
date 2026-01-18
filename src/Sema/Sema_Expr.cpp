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
  return false;
}

// [Sema.cpp]
// 语义合成器：从 "变量形态(Flags)" 和 "灵魂类型(Type)" 合成完整的物理签名
// 用于在函数调用检查时，生成期望的参数类型字符串
template <typename T> static std::string synthesizePhysicalType(const T &Arg) {
  std::string Signature = "";

  // 1. Morphologies
  if (Arg.IsUnique) {
    Signature += "^";
  } else if (Arg.IsShared) {
    Signature += "~";
  } else if (Arg.IsReference) {
    Signature += "&";
  } else if (Arg.HasPointer) {
    Signature += "*";
  }

  // 2. Pointer Attributes (on prefix level if needed, but Type::fromString
  // expects suffixes) Actually, for consistency with Type::fromString recursive
  // parsing: *Data? means Pointer (isNullable=true) to Data. *Data# means
  // Pointer (isWritable=true) to Data.

  // 3. Soul Type
  Signature += Arg.Type;

  // 4. Value Attributes
  if (Arg.IsPointerNullable || Arg.IsValueNullable) {
    Signature += "?";
  }
  if (Arg.IsRebindable || Arg.IsValueMutable) {
    Signature += "#";
  }

  return Signature;
}

std::string Sema::checkUnaryExprStr(UnaryExpr *Unary) {
  return checkUnaryExpr(Unary)->toString();
}

std::shared_ptr<toka::Type> Sema::checkExpr(Expr *E) {
  if (!E)
    return nullptr;
  m_LastInitMask = ~0ULL; // Default to fully set
  auto T = checkExprImpl(E);
  E->ResolvedType = T;

  if (!dynamic_cast<UnsetExpr *>(E) && !dynamic_cast<InitStructExpr *>(E)) {
    m_LastInitMask = ~0ULL;
  }
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

  // Safe Constructors (Exceptions)
  if (dynamic_cast<CallExpr *>(E) || dynamic_cast<MethodCallExpr *>(E) ||
      dynamic_cast<NewExpr *>(E) || dynamic_cast<AllocExpr *>(E)) {
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
        DiagnosticEngine::report(getLoc(Rec), DiagID::ERR_GENERIC_PARSE,
                                 "duplicate field '{}' in anonymous record",
                                 f.first);
        HasError = true;
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
        false, UniqueName, ShapeKind::Struct, members);

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
    DiagnosticEngine::report(getLoc(Deref), DiagID::ERR_INVALID_OP,
                             "dereference", innerObj->toString(), "void");
    HasError = true;
    return toka::Type::fromString("unknown");
  } else if (auto *Unary = dynamic_cast<UnaryExpr *>(E)) {
    return checkUnaryExpr(Unary);
  } else if (auto *Str = dynamic_cast<StringExpr *>(E)) {
    return toka::Type::fromString("str");
  } else if (auto *ve = dynamic_cast<VariableExpr *>(E)) {
    SymbolInfo Info;
    if (!CurrentScope->lookup(ve->Name, Info)) {
      DiagnosticEngine::report(getLoc(ve), DiagID::ERR_UNDECLARED, ve->Name);
      HasError = true;
      return toka::Type::fromString("unknown");
    }
    if (Info.Moved) {
      DiagnosticEngine::report(getLoc(ve), DiagID::ERR_USE_MOVED, ve->Name);
      HasError = true;
    }
    if (Info.IsMutablyBorrowed) {
      DiagnosticEngine::report(getLoc(ve), DiagID::ERR_BORROW_MUT, ve->Name);
      HasError = true;
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

      if (!isFullyInit) {
        DiagnosticEngine::report(getLoc(ve), DiagID::ERR_USE_UNSET, ve->Name);
        HasError = true;
      }
    }
    m_LastInitMask = Info.InitMask;
    return Info.TypeObj;
  } else if (auto *Null = dynamic_cast<NullExpr *>(E)) {
    return toka::Type::fromString("nullptr");
  } else if (auto *None = dynamic_cast<NoneExpr *>(E)) {
    return toka::Type::fromString("none");
  } else if (auto *Cast = dynamic_cast<CastExpr *>(E)) {
    auto srcType = checkExpr(Cast->Expression.get());
    auto targetType = toka::Type::fromString(Cast->TargetType);

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

    if (srcIsNumeric && targetIsNumeric) {
      // Normal numeric cast, allow.
    } else if (targetType->isSmartPointer() && !srcType->isSmartPointer()) {
      // Rule: Smart Pointer Creation Restriction
      DiagnosticEngine::report(getLoc(Cast), DiagID::ERR_SMART_PTR_FROM_STACK,
                               Cast->TargetType[0]);
      HasError = true;
    } else if (targetIsAddr) {
      // Rule: Addr can be cast from Addr, Raw Pointer, or Numeric
      if (!(srcIsAddr || srcIsRaw || srcIsNumeric)) {
        DiagnosticEngine::report(getLoc(Cast), DiagID::ERR_CAST_MISMATCH,
                                 srcType->toString(), Cast->TargetType);
        HasError = true;
      }
    } else if (srcIsAddr) {
      // Rule: Addr can be cast to Addr, Raw Pointer, or Numeric
      if (!(targetIsAddr || targetIsRaw || targetIsNumeric)) {
        DiagnosticEngine::report(getLoc(Cast), DiagID::ERR_CAST_MISMATCH,
                                 srcType->toString(), Cast->TargetType);
        HasError = true;
      }
    } else if (targetIsRaw) {
      // Rule: Raw Pointer can be cast from Addr or another Raw Pointer
      if (!(srcIsAddr || srcIsRaw)) {
        DiagnosticEngine::report(getLoc(Cast), DiagID::ERR_CAST_MISMATCH,
                                 srcType->toString(), Cast->TargetType);
        HasError = true;
      }
    } else if (srcIsRaw) {
      // Rule: Raw Pointer can be cast to Addr or another Raw Pointer
      if (!(targetIsAddr || targetIsRaw)) {
        DiagnosticEngine::report(getLoc(Cast), DiagID::ERR_CAST_MISMATCH,
                                 srcType->toString(), Cast->TargetType);
        HasError = true;
      }
    } else if (!srcType->equals(*targetType)) {
      // Fallback for other things? E.g. compatible aliases
      if (!isTypeCompatible(targetType, srcType)) {
        DiagnosticEngine::report(getLoc(Cast), DiagID::ERR_CAST_MISMATCH,
                                 srcType->toString(), Cast->TargetType);
        HasError = true;
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
        VariableExpr *varExpr = dynamic_cast<VariableExpr *>(lhs);
        if (!varExpr) {
          if (auto *un = dynamic_cast<UnaryExpr *>(lhs)) {
            varExpr = dynamic_cast<VariableExpr *>(un->RHS.get());
          }
        }

        if (varExpr) {
          SymbolInfo *infoPtr = nullptr;
          if (CurrentScope->findSymbol(varExpr->Name, infoPtr)) {
            narrowedVar = varExpr->Name;
            originalInfo = *infoPtr;
            // Sync TypeObj: Narrow type to non-nullable
            if (infoPtr->TypeObj) {
              infoPtr->TypeObj = infoPtr->TypeObj->withAttributes(
                  infoPtr->TypeObj->IsWritable, false);
            }

            // Sync TypeObj
            if (infoPtr->TypeObj) {
              // Determine what nullability means for this type
              // IsPointerNullable vs IsValueNullable
              // For simplicity in Stage 3, we reconstruct or strip attributes
              bool isPtrNull = false; // Should be false now
              bool isValNull = false; // Should be false now
              // Reconstruct from source of truth properties?
              // Or just use withAttributes(writable, nullable=false)?
              // Nullable means *either* pointer or value nullability in Toka?
              // Narrowing removes nullability.
              infoPtr->TypeObj = infoPtr->TypeObj->withAttributes(
                  infoPtr->TypeObj->IsWritable, false);
            }

            narrowed = true;
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
      SymbolInfo *infoPtr = nullptr;
      if (CurrentScope->findSymbol(narrowedVar, infoPtr)) {
        *infoPtr = originalInfo;
      }
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
        error(ie->Then.get(), "Yielding if branch must pass a value");
      if (!ie->Else)
        error(ie, "Yielding if expression must have an 'else' block");
      else if (elseType == "void" && !allPathsJump(ie->Else.get()))
        error(ie->Else.get(), "Yielding else branch must pass a value");
    }

    if (thenType != "void" && elseType != "void" &&
        !isTypeCompatible(thenType, elseType)) {
      error(ie, "If branches have incompatible types: '" + thenType +
                    "' and '" + elseType + "'");
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
        error(we->Body.get(), "Yielding while loop body must pass a value");
      if (!we->ElseBody)
        error(we, "Yielding while loop must have an 'or' block");
      else if (elseType == "void" && !allPathsJump(we->ElseBody.get()))
        error(we->ElseBody.get(),
              "Yielding while loop 'or' block must pass a value");
    }

    if (bodyType != "void" && !we->ElseBody) {
      error(we, "Yielding while loop must have an 'or' block");
    }
    if (bodyType != "void" && elseType != "void" &&
        !isTypeCompatible(bodyType, elseType)) {
      error(we, "While loop branches have incompatible types");
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
      error(le, "Yielding loop body must pass a value");
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
        error(fe->Body.get(), "Yielding for loop body must pass a value");
      if (!fe->ElseBody)
        error(fe, "Yielding for loop must have an 'or' block");
      else if (elseType == "void" && !allPathsJump(fe->ElseBody.get()))
        error(fe->ElseBody.get(),
              "Yielding for loop 'or' block must pass a value");
    }

    if (bodyType != "void" && !fe->ElseBody) {
      error(fe, "Yielding for loop must have an 'or' block");
    }
    if (bodyType != "void" && elseType != "void" &&
        !isTypeCompatible(bodyType, elseType)) {
      error(fe, "For loop branches have incompatible types");
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
      // 2. Leaf 'pass' - must have a receiver
      auto valTypeObj = checkExpr(pe->Value.get());
      valType = valTypeObj->toString();
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
            error(pe, "Yield type mismatch ('" + it->ExpectedType + "' vs '" +
                          valType + "')");
          }
          break;
        }
      }
    }

    if (!foundReceiver) {
      error(pe, "'pass' used without a receiver. Prefix the expression with "
                "'pass' or assign it to a variable.");
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

    if (!target) {
      error(be, "Target for break not found");
    } else {
      if (valType != "void") {
        if (target->ExpectedType == "void")
          target->ExpectedType = valType;
        else if (!isTypeCompatible(target->ExpectedType, valType))
          error(be, "Break value type mismatch");
      }
    }
    return toka::Type::fromString("void");
  } else if (auto *ce = dynamic_cast<ContinueExpr *>(E)) {
    // Continue target must be a loop
    return toka::Type::fromString("void");
  } else if (auto *Call = dynamic_cast<CallExpr *>(E)) {
    return checkCallExpr(Call);
  } else if (auto *New = dynamic_cast<NewExpr *>(E)) {
    // Return the Type being created.
    // Validating the Initializer matches the Type is good (e.g.
    // InitStructExpr)
    if (New->Initializer) {
      auto InitTypeObj = checkExpr(New->Initializer.get());
      std::string InitType = InitTypeObj->toString();
      if (!isTypeCompatible(New->Type, InitType) && InitType != "unknown") {
        // InitStruct returns "StructName".
        // checkExprStr(InitStruct) returns "StructName".
        // So this should match.
        DiagnosticEngine::report(getLoc(New), DiagID::ERR_TYPE_MISMATCH,
                                 InitType, New->Type);
        HasError = true;
      }
    }
    // 'new' usually returns a unique pointer: ^Type
    return toka::Type::fromString("^" + New->Type);
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
    std::string baseType = AllocE->TypeName;
    if (AllocE->IsArray) {
      if (AllocE->ArraySize) {
        checkExpr(AllocE->ArraySize.get());
      }
    }
    if (AllocE->Initializer) {
      checkExpr(AllocE->Initializer.get());
    }
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
                error(Met, "method '" + Met->Method +
                               "' is private to trait '" + traitName + "'");
              }
            }
            return toka::Type::fromString(M->ReturnType);
          }
        }
      }
    }

    std::string soulType = Type::stripMorphology(ObjType);
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
            error(Met, "method '" + Met->Method + "' is private to '" +
                           fdDeclFile + "'");
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
    error(Met,
          "method '" + Met->Method + "' not found on type '" + ObjType + "'");
    return toka::Type::fromString("unknown");
  } else if (auto *Init = dynamic_cast<InitStructExpr *>(E)) {
    // Validate fields against ShapeMap
    if (ShapeMap.count(Init->ShapeName)) {
      ShapeDecl *SD = ShapeMap[Init->ShapeName];

      // Visibility Check:
      std::string sdFile =
          DiagnosticEngine::SrcMgr->getFullSourceLoc(SD->Loc).FileName;
      std::string initFile =
          DiagnosticEngine::SrcMgr->getFullSourceLoc(Init->Loc).FileName;

      if (!SD->IsPub && sdFile != initFile) {
        // Relaxed privacy check: allow calls within the same module,
        // regardless of file.
        bool sameModule = false;
        if (CurrentModule && !CurrentModule->Shapes.empty()) {
          for (const auto &shapeInModule : CurrentModule->Shapes) {
            if (DiagnosticEngine::SrcMgr->getFullSourceLoc(shapeInModule->Loc)
                    .FileName == sdFile) {
              if (CurrentModule) {
                std::string modFile = DiagnosticEngine::SrcMgr
                                          ->getFullSourceLoc(CurrentModule->Loc)
                                          .FileName;
                std::string fdFile =
                    DiagnosticEngine::SrcMgr->getFullSourceLoc(SD->Loc)
                        .FileName; // Should be SD->Loc, not FD->Loc
                if (modFile == fdFile) {
                  sameModule = true;
                }
              }
              break;
            }
          }
        }
        if (!sameModule) {
          DiagnosticEngine::report(getLoc(Init), DiagID::ERR_PRIVATE_TYPE,
                                   Init->ShapeName, sdFile);
          HasError = true;
        }
      }

      // Check fields exist and types match
      std::set<std::string> providedFields;
      for (const auto &pair : Init->Members) {
        if (providedFields.count(pair.first)) {
          DiagnosticEngine::report(getLoc(Init), DiagID::ERR_DUPLICATE_FIELD,
                                   pair.first);
          HasError = true;
        }
        providedFields.insert(pair.first);

        bool fieldFound = false;
        std::string expectedType;
        for (const auto &defField : SD->Members) {
          if (defField.Name == pair.first) {
            fieldFound = true;
            expectedType = defField.Type;
            break;
          }
        }

        if (!fieldFound) {
          DiagnosticEngine::report(getLoc(Init), DiagID::ERR_NO_SUCH_MEMBER,
                                   Init->ShapeName, pair.first);
          HasError = true;
          continue; // Continue to find more errors
        }

        std::shared_ptr<toka::Type> exprTypeObj = checkExpr(pair.second.get());
        std::string exprType = exprTypeObj->toString();
        if (!isTypeCompatible(expectedType, exprType)) {
          DiagnosticEngine::report(getLoc(Init),
                                   DiagID::ERR_MEMBER_TYPE_MISMATCH, pair.first,
                                   expectedType, exprType);
          HasError = true;
        }
      }

      // Check for missing fields
      for (const auto &defField : SD->Members) {
        // If field is NOT in providedFields
        if (!providedFields.count(defField.Name)) {
          DiagnosticEngine::report(getLoc(Init), DiagID::ERR_MISSING_MEMBER,
                                   defField.Name, Init->ShapeName);
          HasError = true;
        }
      }
    } else {
      DiagnosticEngine::report(getLoc(Init), DiagID::ERR_UNKNOWN_STRUCT,
                               Init->ShapeName);
      HasError = true;
    }

    // Mask Calculation for Struct
    if (ShapeMap.count(Init->ShapeName)) {
      ShapeDecl *SD = ShapeMap[Init->ShapeName];
      uint64_t mask = 0;
      for (int i = 0; i < (int)SD->Members.size(); ++i) {
        std::string memName = SD->Members[i].Name;
        bool found = false;
        for (const auto &pair : Init->Members) {
          if (pair.first == memName) {
            // Check the expression again to get its mask
            // (Optimization: cache this? calling checkExpr twice might be
            // expensive or verify idempotence. checkExpr sets m_LastInitMask)
            checkExpr(pair.second.get());

            uint64_t subMask = 1;
            // Determine expected mask for member type
            // If member is primitive, expected is 1.
            // If member is shape?
            // Current limitation: Flattened masks not fully supported for
            // nested shapes in one integer. But we treat fully initialized
            // nested shape as "1". So if m_LastInitMask indicates full init, we
            // set bit i.

            // How to check full init?
            // If Member is Shape:
            //   Look up Member Shape.
            //   Expected = (1 << Size) - 1.
            //   If (m_LastInitMask & Expected) == Expected -> Set bit i.
            // If Member is Primitive:
            //   Expected = 1.
            //   If (m_LastInitMask & 1) -> Set bit i.

            std::shared_ptr<Type> memTypeObj =
                toka::Type::fromString(SD->Members[i].Type);
            uint64_t expected = 1;
            if (memTypeObj->isShape()) {
              std::string sName = memTypeObj->getSoulName();
              if (ShapeMap.count(sName)) {
                size_t sz = ShapeMap[sName]->Members.size();
                if (sz >= 64)
                  expected = ~0ULL;
                else
                  expected = (1ULL << sz) - 1;
              }
            }

            if ((m_LastInitMask & expected) == expected) {
              if (i < 64)
                mask |= (1ULL << i);
            }
            found = true;
            break;
          }
        }
        // If not found (missing member), bit remains 0. Error reported above.
      }
      m_LastInitMask = mask;
    } else {
      // Unknown struct, assume full init to avoid cascading errors?
      // Or 0?
      m_LastInitMask = 1;
    }

    return toka::Type::fromString(Init->ShapeName);
  } else if (auto *Memb = dynamic_cast<MemberExpr *>(E)) {
    auto objTypeObj = checkExpr(Memb->Object.get());

    // Unset Check for Member Access
    if (!m_InLHS) {
      if (auto *objVar = dynamic_cast<VariableExpr *>(Memb->Object.get())) {
        SymbolInfo Info;
        if (CurrentScope->lookup(objVar->Name, Info)) {
          if (Info.TypeObj && Info.TypeObj->isShape()) {
            std::string soul = Info.TypeObj->getSoulName();
            if (ShapeMap.count(soul)) {
              ShapeDecl *SD = ShapeMap[soul];
              for (int i = 0; i < (int)SD->Members.size(); ++i) {
                if (SD->Members[i].Name == Memb->Member) {
                  if (i < 64 && !(Info.InitMask & (1ULL << i))) {
                    DiagnosticEngine::report(getLoc(Memb),
                                             DiagID::ERR_USE_UNSET,
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

    if (objTypeObj->IsNullable) {
      DiagnosticEngine::report(getLoc(Memb), DiagID::ERR_NULL_ACCESS,
                               ObjTypeFull);
      HasError = true;
    }
    std::string ObjType = resolveType(objTypeObj)->toString();

    // Auto-dereference if it's a pointer/ref (strips ^, &, *, ~)
    if (ObjType.size() > 1 && (ObjType[0] == '^' || ObjType[0] == '&' ||
                               ObjType[0] == '*' || ObjType[0] == '~')) {
      ObjType = ObjType.substr(1);
    }
    // Strip suffix '?' if it was '^?Point' -> '?Point' -> 'Point'
    while (!ObjType.empty() && (ObjType.back() == '?' ||
                                ObjType.back() == '!' || ObjType.back() == '#'))
      ObjType.pop_back();

    // Secondary middle check if prefix icon remained somehow?
    while (!ObjType.empty() &&
           (ObjType[0] == '?' || ObjType[0] == '!' || ObjType[0] == '#'))
      ObjType = ObjType.substr(1);

    if (ShapeMap.count(ObjType)) {
      ShapeDecl *SD = ShapeMap[ObjType];
      std::string requestedMember = Memb->Member;
      std::string requestedPrefix = "";
      if (!requestedMember.empty() &&
          (requestedMember[0] == '*' || requestedMember[0] == '^' ||
           requestedMember[0] == '~' || requestedMember[0] == '&')) {
        requestedPrefix = requestedMember.substr(0, 1);
        requestedMember = requestedMember.substr(1);
      }

      for (const auto &Field : SD->Members) {
        if (Field.Name == requestedMember) {
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
                error(Memb, "field '" + requestedMember + "' of struct '" +
                                ObjType +
                                "' is private and not accessible from this "
                                "context");
              }
            }
          }

          // Return type based on Toka 1.3 Pointer-Value Duality
          std::string fullType = Field.Type;
          std::string requestedMember = Memb->Member;
          std::string requestedPrefix = "";
          if (!requestedMember.empty() &&
              (requestedMember[0] == '*' || requestedMember[0] == '^' ||
               requestedMember[0] == '~' || requestedMember[0] == '&')) {
            requestedPrefix = requestedMember.substr(0, 1);
          }

          if (requestedPrefix.empty()) {
            // obj.field -> Entity (Pointer itself if it's a pointer)
            if (SD->Kind == ShapeKind::Enum) {
              return toka::Type::fromString(ObjType);
            }
            return toka::Type::fromString(fullType);
          } else if (requestedPrefix == "*") {
            // obj.*field -> Identity (Address stored in the pointer)
            return toka::Type::fromString(fullType);
          }

          return toka::Type::fromString(fullType);
        }
      }
      error(Memb, "struct '" + ObjType + "' has no member named '" +
                      Memb->Member + "'");
    } else if (ObjType == "tuple" || ObjType.find("(") == 0) {
      // Tuple access: .0, .1
      // If we knew the tuple type string, e.g. "(i32, str)", we could parse
      // it. For now, accept integer members on anything resembling a tuple.
      // And return "unknown" or "i32" fallback?
      // test.tk uses .0 which is i32, .1 which is string/i32.
      // Let's return "unknown" to suppress error but allow it to pass
      // checks if we are lenient.
      return toka::Type::fromString("unknown");
    } else if (ObjType != "unknown") {
      error(Memb, "member access on non-struct type '" + ObjType + "'");
    }
    return toka::Type::fromString("unknown");
  } else if (auto *Post = dynamic_cast<PostfixExpr *>(E)) {
    auto lhsObj = checkExpr(Post->LHS.get());
    std::string lhsInfo = lhsObj->toString();
    if (auto *Var = dynamic_cast<VariableExpr *>(Post->LHS.get())) {
      SymbolInfo Info;
      if (CurrentScope->lookup(Var->Name, Info) && !Info.IsMutable()) {
        error(Post, "cannot modify immutable variable '" + Var->Name +
                        "' (# suffix required)");
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
      error(Arr, "Array indexing expects exactly 1 index");
    }
    checkExpr(Arr->Indices[0].get());
    return toka::Type::fromString(
        "unknown"); // Placeholder for element type derivation
  } else if (auto *Tup = dynamic_cast<TupleExpr *>(E)) {
    std::string s = "(";
    for (size_t i = 0; i < Tup->Elements.size(); ++i) {
      if (i > 0)
        s += ", ";
      auto elTypeObj = checkExpr(Tup->Elements[i].get());
      s += elTypeObj->toString();
    }
    s += ")";
    return toka::Type::fromString(s);
  } else if (auto *ArrLit = dynamic_cast<ArrayExpr *>(E)) {
    // Infer from first element
    if (!ArrLit->Elements.empty()) {
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
        if (!guardTypeObj->isBoolean())
          error(arm->Guard.get(), "guard must be bool");
      }
      m_ControlFlowStack.push_back({"", "void", false, isReceiver});
      checkStmt(arm->Body.get());
      std::string armType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();

      if (isReceiver && armType == "void" && !allPathsJump(arm->Body.get())) {
        error(arm->Body.get(), "Yielding match arm must pass a value");
      }

      if (resultType == "void")
        resultType = armType;
      else if (armType != "void" && !isTypeCompatible(resultType, armType)) {
        if (resultType != "unknown" && armType != "unknown")
          error(me, "Match arms have incompatible types ('" + resultType +
                        "' vs '" + armType + "')");
      }
      exitScope();
    }

    if (isReceiver && resultType == "void") {
      error(me, "Yielding match expression must pass a value in all arms");
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
        error(me, "match on encapsulated type '" + targetType +
                      "' requires a default '_' branch (Rule 412)");
      }
      return toka::Type::fromString(resultType);
    }

    return toka::Type::fromString(resultType);
  }
  return toka::Type::fromString("unknown");
}

// Stage 4: Object-Oriented Shims
std::shared_ptr<toka::Type> Sema::checkUnaryExpr(UnaryExpr *Unary) {
  auto rhsType = checkExpr(Unary->RHS.get());
  // Assuming checkExpr returns object now.
  if (!rhsType || rhsType->toString() == "unknown") // Or use isUnknown()?
    return toka::Type::fromString("unknown");

  std::string rhsInfo =
      rhsType->toString(); // Keep string for error messages/legacy checks

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
  } else if (Unary->Op == TokenType::Star || Unary->Op == TokenType::Caret ||
             Unary->Op == TokenType::Tilde ||
             Unary->Op == TokenType::Ampersand) {

    if (auto *Var = dynamic_cast<VariableExpr *>(Unary->RHS.get())) {
      SymbolInfo *Info = nullptr;
      if (CurrentScope->findSymbol(Var->Name, Info)) {
        if (Unary->Op == TokenType::Ampersand) {
          bool wantMutable = Var->IsValueMutable;
          if (wantMutable) {
            if (!Info->IsMutable()) {
              error(Unary, "cannot mutably borrow immutable variable '" +
                               Var->Name + "' (# required)");
            }
            if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
              error(Unary, "cannot mutably borrow '" + Var->Name +
                               "' because it is already borrowed");
            }
            Info->IsMutablyBorrowed = true;
          } else {
            if (Info->IsMutablyBorrowed) {
              error(Unary, "cannot borrow '" + Var->Name +
                               "' because it is mutably borrowed");
            }
            Info->ImmutableBorrowCount++;
          }
          m_LastBorrowSource = Var->Name;
          m_CurrentStmtBorrows.push_back({Var->Name, wantMutable});

          // Use TypeObj if available, else generic logic
          auto innerType =
              Info->TypeObj ? Info->TypeObj
                            : toka::Type::fromString(Info->TypeObj->toString());

          // Implicit Dereference for Smart Pointers: &p -> &Pointee
          if (innerType->isUniquePtr() || innerType->isSharedPtr()) {
            if (auto ptrInner = innerType->getPointeeType()) {
              innerType = ptrInner;
            }
          }

          bool innerWritable = Info->IsMutable() || Var->IsValueMutable;
          innerType =
              innerType->withAttributes(innerWritable, innerType->IsNullable);
          auto refType = std::make_shared<toka::ReferenceType>(innerType);
          refType->IsNullable = Unary->HasNull;
          refType->IsWritable = Unary->IsRebindable;
          return refType;
        } else if (Unary->Op == TokenType::Caret) {
          if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
            error(Unary,
                  "cannot move '" + Var->Name + "' while it is borrowed");
          }
          // Move Semantics: Only allowed if already same smart pointer
          // morphology.
          if (rhsType->isUniquePtr())
            return rhsType;
          DiagnosticEngine::report(getLoc(Unary),
                                   DiagID::ERR_SMART_PTR_FROM_STACK, "^");
          HasError = true;
          return std::make_shared<toka::UniquePointerType>(rhsType);
        } else if (Unary->Op == TokenType::Tilde) {
          if (rhsType->isSharedPtr()) {
            return rhsType; // Idempotent
          }
          DiagnosticEngine::report(getLoc(Unary),
                                   DiagID::ERR_SMART_PTR_FROM_STACK, "~");
          HasError = true;
          auto sh = std::make_shared<toka::SharedPointerType>(rhsType);
          sh->IsNullable = Unary->HasNull;
          sh->IsWritable = Unary->IsRebindable;
          return sh;
        } else if (Unary->Op == TokenType::Star) {
          // Identity (*)
          std::shared_ptr<toka::Type> inner;
          if (rhsType->isPointer()) {
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
      }
    }

    if (Unary->Op == TokenType::Star) {
      // Identity (*) on non-variable
      std::shared_ptr<toka::Type> inner;
      if (rhsType->isPointer()) {
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

    if (Unary->Op == TokenType::Ampersand) {
      auto ref = std::make_shared<toka::ReferenceType>(rhsType);
      ref->IsNullable = Unary->HasNull;
      ref->IsWritable = Unary->IsRebindable;
      return ref;
    }
    if (Unary->Op == TokenType::Caret) {
      if (rhsType->isUniquePtr())
        return rhsType;
      DiagnosticEngine::report(getLoc(Unary), DiagID::ERR_SMART_PTR_FROM_STACK,
                               "^");
      HasError = true;
      auto unq = std::make_shared<toka::UniquePointerType>(rhsType);
      unq->IsNullable = Unary->HasNull;
      unq->IsWritable = Unary->IsRebindable;
      return unq;
    }
    if (Unary->Op == TokenType::Tilde) {
      if (rhsType->isSharedPtr())
        return rhsType;
      DiagnosticEngine::report(getLoc(Unary), DiagID::ERR_SMART_PTR_FROM_STACK,
                               "~");
      HasError = true;
      auto sh = std::make_shared<toka::SharedPointerType>(rhsType);
      sh->IsNullable = Unary->HasNull;
      sh->IsWritable = Unary->IsRebindable;
      return sh;
    }
  }

  if (Unary->Op == TokenType::PlusPlus || Unary->Op == TokenType::MinusMinus) {
    if (!rhsType->isInteger()) {
      error(Unary, "operand of increment/decrement must be integer, got '" +
                       rhsInfo + "'");
    }
    if (auto *Var = dynamic_cast<VariableExpr *>(Unary->RHS.get())) {
      SymbolInfo *Info = nullptr;
      if (CurrentScope->findSymbol(Var->Name, Info)) {
        if (!Info->IsMutable()) {
          error(Unary, "cannot modify immutable variable '" + Var->Name +
                           "' (# suffix required)");
        }
        if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
          error(Unary,
                "cannot modify '" + Var->Name + "' while it is borrowed");
        }
      }
    }
    return rhsType;
  }
  if (Unary->Op == TokenType::KwBnot) {
    if (!rhsType->isInteger()) {
      error(Unary, "operand of 'bnot' must be integer, got '" + rhsInfo + "'");
    }
    return rhsType;
  }
  return rhsType;
}

// Stage 5: Object-Oriented Binary Expression Check
std::shared_ptr<toka::Type> Sema::checkBinaryExpr(BinaryExpr *Bin) {
  // 1. Resolve Operands using New API
  m_InLHS = (Bin->Op == "=" || Bin->Op == "+=" || Bin->Op == "-=" ||
             Bin->Op == "*=" || Bin->Op == "/=");
  auto lhsType = checkExpr(Bin->LHS.get());
  m_InLHS = false;
  auto rhsType = checkExpr(Bin->RHS.get());

  if (!lhsType || !rhsType)
    return toka::Type::fromString("unknown");

  std::string LHS = lhsType->toString(); // For error messages
  std::string RHS = rhsType->toString();

  // [Optimization] Literal Adaptation
  // Allow mixed comparison like (i64 < 2) by auto-casting the literal to the
  // explicit type.
  Expr *lhsExpr = Bin->LHS.get();
  Expr *rhsExpr = Bin->RHS.get();

  // Strip parens if needed (simple check)
  // For now direct NumberExpr check
  auto *lhsNum = dynamic_cast<NumberExpr *>(lhsExpr);
  auto *rhsNum = dynamic_cast<NumberExpr *>(rhsExpr);

  if (lhsType->isInteger() && rhsNum && !lhsNum) {
    // Left is Strong Integer, Right is Literal -> Adapt Right
    Bin->RHS->ResolvedType = lhsType;
    rhsType = lhsType;
    RHS = rhsType->toString();
  } else if (rhsType->isInteger() && lhsNum && !rhsNum) {
    // Right is Strong Integer, Left is Literal -> Adapt Left
    Bin->LHS->ResolvedType = rhsType;
    lhsType = rhsType;
    LHS = lhsType->toString();
  } else if (rhsType->isInteger() && lhsNum && !rhsNum) {
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
        if (RHSInfoPtr->IsMutablyBorrowed ||
            RHSInfoPtr->ImmutableBorrowCount > 0) {
          error(Bin, "cannot move '" + RHSVar->Name + "' while it is borrowed");
        }
        CurrentScope->markMoved(RHSVar->Name);
      }
    }

    // Reference Assignment
    if (auto *VarLHS = dynamic_cast<VariableExpr *>(Bin->LHS.get())) {
      SymbolInfo Info;
      if (CurrentScope->lookup(VarLHS->Name, Info)) {
        if (Info.IsReference()) {
          // Check against Pointee
          if (lhsType->isReference()) {
            auto inner =
                lhsType->getPointeeType(); // Correct: Reference's inner
            if (!inner)
              inner = lhsType; // Fallback
            if (!isTypeCompatible(inner, rhsType)) {
              error(Bin, "assignment type mismatch (ref): cannot assign '" +
                             RHS + "' to '" + inner->toString() + "'");
            }
            return inner; // Assign returns value? Or void? Spec says
                          // void/unit usually but legacy returns type.
          }
          isRefAssign = true;
        }
      }
    }
  }

  bool isAssign = (Bin->Op == "=" || Bin->Op == "+=" || Bin->Op == "-=" ||
                   Bin->Op == "*=" || Bin->Op == "/=");

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
      // If RHS matches Inner, we are assigning to the Soul (implicit *s = val)
      if (inner && isTypeCompatible(inner, rhsType)) {
        lhsType = inner; // Decay to Pointee Type for Writability Check
        isImplicitDerefAssign = true;
      }
    }

    // Writability & Borrow Check
    bool isLHSWritable = false;
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
        if (InfoPtr->IsMutablyBorrowed || InfoPtr->ImmutableBorrowCount > 0) {
          error(Bin, "cannot modify '" + Var->Name + "' while it is borrowed");
        }
        if (InfoPtr->IsMutable() || Var->IsValueMutable)
          isLHSWritable = true;
      }
    } else if (auto *Un = dynamic_cast<UnaryExpr *>(Traverse)) {
      if (Un->Op == TokenType::Star || Un->Op == TokenType::Caret ||
          Un->Op == TokenType::Tilde || Un->Op == TokenType::Ampersand) {
        if (auto *Var = dynamic_cast<VariableExpr *>(Un->RHS.get())) {
          SymbolInfo *InfoPtr = nullptr;
          if (CurrentScope->findSymbol(Var->Name, InfoPtr)) {
            // Rebindable check (missing in Type but checked in Symbol)
            // Using generic "IsMutable" proxy if Rebindable not on Type
            isLHSWritable = true;
          }
        }
      } else {
        isLHSWritable = true;
      }
    }

    if (isLHSWritable)
      lhsType =
          lhsType->withAttributes(true, lhsType->IsNullable); // Valid Write

    if (!lhsType->IsWritable && !isRefAssign) {
      error(Bin->LHS.get(), "Cannot assign to immutable view '" + LHS +
                                "'. Missing writable token '#'.");
    }

    auto lhsCompatType = lhsType->withAttributes(false, lhsType->IsNullable);

    // Strict Pointer Morphology Check
    if (!isRefAssign) {
      // Determine Target Morphology (LHS)
      MorphKind targetMorph = MorphKind::None;
      // We need to look at the LHS expression structure
      // If LHS is *p or ^p etc.
      targetMorph = getSyntacticMorphology(Bin->LHS.get());

      // If LHS is a variable declaration, we don't handle it here (handled in
      // checkVariableDecl). But this is assignment to existing variable. If
      // LHS is 'p' (VariableExpr) and p is a pointer type, Morph is None
      // (hidden). If p is pointer, targetMorph=None. SourceMorph check...
      // User rule: "auto ^p = x" (Invalid). "auto p = ^x" (Invalid).
      // "p = q" (Hidden = Hidden)?
      // "Strict explicit morphology matching".
      // If LHS has no sigil, but is a pointer type?
      // "auto p = ^x". p is pointer. LHS sigil None. RHS sigil Unique.
      // Mismatch. So checks apply.

      // However, we need to know if LHS *is* a pointer type to enforce
      // strictness. If LHS is i32, and RHS is i32. None == None. OK. If LHS
      // is *i32 (via `*p` deref? No, `*p` assigns to `i32`). If `p` is
      // `*i32`. `p = q`. LHS `p` has Morph::None. If strictness requires `*p`
      // to be assigned? No, `*p` assigns to the *pointee*. We are assigning
      // TO the pointer `p`. So `p = q` is "Value = Value" syntax. Does user
      // want `*p = *q` for pointer assignment? No, that's partial
      // update/deref assignment. `p = q` rebinds the pointer. If the rule is
      // about *Declarations* majorly (`auto *p = ...`), maybe assignment
      // `p=q` is exempt or `Valid`. The prompt says: "pointer morphology
      // symbols... on both sides of an assignment... must explicitly match."
      // Example: `auto *p = ^q` -> Invalid. `p = q` where p, q are pointers.
      // Sigils are None. None == None. Matches. `p = ^q`. None != Unique.
      // Mismatch. Correct. So `getSyntacticMorphology` returning None for
      // VariableExpr is correct.

      MorphKind sourceMorph = getSyntacticMorphology(Bin->RHS.get());
      checkStrictMorphology(Bin, targetMorph, sourceMorph, LHS);
    }

    if (!isRefAssign && !isSmartNew &&
        !isTypeCompatible(lhsCompatType, rhsType) && LHS != "unknown" &&
        RHS != "unknown") {
      error(Bin, "assignment type mismatch: cannot assign '" + RHS + "' to '" +
                     LHS + "'");
    }

    // [Fix] Update InitMask logic for 'unset' variables
    Expr *LHSExpr = Bin->LHS.get();
    // Unwrap mutation suffix/prefix if any (e.g. x#) -> handled by VariableExpr
    // IsValueMutable? If LHS is VariableExpr, update mask.
    if (auto *Var = dynamic_cast<VariableExpr *>(LHSExpr)) {
      SymbolInfo *Info = nullptr;
      if (CurrentScope->findSymbol(Var->Name, Info)) {
        Info->InitMask = ~0ULL; // Fully initialized
      }
    } else if (auto *Memb = dynamic_cast<MemberExpr *>(LHSExpr)) {
      // Handle Partial Initialization
      Expr *Obj = Memb->Object.get();
      if (auto *Var = dynamic_cast<VariableExpr *>(Obj)) {
        SymbolInfo *Info = nullptr;
        if (CurrentScope->findSymbol(Var->Name, Info)) {
          if (Info->TypeObj && Info->TypeObj->isShape()) {
            std::string sName = Info->TypeObj->getSoulName();
            if (ShapeMap.count(sName)) {
              ShapeDecl *SD = ShapeMap[sName];
              for (int i = 0; i < (int)SD->Members.size(); ++i) {
                if (SD->Members[i].Name == Memb->Member) {
                  Info->InitMask |= (1ULL << i);
                  break;
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
      error(Bin, "operands of '" + Bin->Op + "' must be bool");
    }
    return toka::Type::fromString("bool");
  }

  if (lhsType->isPointer() && (Bin->Op == "+" || Bin->Op == "-")) {
    if (!m_InUnsafeContext) {
      error(Bin, "pointer arithmetic requires unsafe context");
    }
    return lhsType->withAttributes(false, lhsType->IsNullable);
  }

  if (Bin->Op == "==" || Bin->Op == "!=" || Bin->Op == "<" || Bin->Op == ">" ||
      Bin->Op == "<=" || Bin->Op == ">=") {
    if (!isTypeCompatible(lhsType, rhsType) &&
        !isTypeCompatible(rhsType, lhsType)) {
      error(Bin, "operands of '" + Bin->Op + "' must be same type ('" + LHS +
                     "' vs '" + RHS + "')");
    }
    // Strict Integer Check
    if (!lhsType->withAttributes(false, false)
             ->equals(*rhsType->withAttributes(false, false))) {
      if (lhsType->isInteger() && rhsType->isInteger()) {
        error(Bin, "comparison operands must have exact same type ('" + LHS +
                       "' vs '" + RHS + "')");
      }
    }
    return toka::Type::fromString("bool");
  }

  if (Bin->Op == "+" || Bin->Op == "-" || Bin->Op == "*" || Bin->Op == "/") {
    bool isValid = false;
    if (lhsType->isInteger() || lhsType->isFloatingPoint())
      isValid = true;
    // Addr/usize check via string for now or explicit Type check?
    // Type::isPrimitive("Addr")

    if (!isValid) {
      error(Bin,
            "operands of '" + Bin->Op + "' must be numeric, got '" + LHS + "'");
    }
    return lhsType->withAttributes(false, lhsType->IsNullable);
  }

  if (Bin->Op == "band" || Bin->Op == "bor" || Bin->Op == "bxor" ||
      Bin->Op == "bshl" || Bin->Op == "bshr") {
    if (!lhsType->isInteger() || !rhsType->isInteger()) {
      error(Bin, "operands of '" + Bin->Op + "' must be integers");
    }
    return lhsType->withAttributes(false, lhsType->IsNullable);
  }

  if (Bin->Op == "is" || Bin->Op == "is!") {
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
    auto idxType = checkExpr(idxExpr.get());
    if (!idxType->isInteger()) {
      error(Idx,
            "array index must be integer, got '" + idxType->toString() + "'");
    }
  }

  // 2. Resolve Base Type
  auto baseType = checkExpr(Idx->Array.get());
  if (!baseType || baseType->toString() == "unknown")
    return toka::Type::fromString("unknown");

  std::shared_ptr<toka::Type> resultType = nullptr;

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
  llvm::errs() << "DEBUG: checkCallExpr Entry: " << CallName << "\n";

  // 1. Primitives (Constructors/Casts) e.g. i32(42)
  if (CallName == "i32" || CallName == "u32" || CallName == "i64" ||
      CallName == "u64" || CallName == "f32" || CallName == "f64" ||
      CallName == "i16" || CallName == "u16" || CallName == "i8" ||
      CallName == "u8" || CallName == "usize" || CallName == "isize" ||
      CallName == "bool") {
    for (auto &Arg : Call->Args) {
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
      error(Call, "println must be explicitly imported from std/io");
      return toka::Type::fromString("void");
    }
    if (Call->Args.empty()) {
      error(Call, "println requires at least a format string");
    }
    for (auto &Arg : Call->Args) {
      checkExpr(Arg.get());
    }
    return toka::Type::fromString("void");
  }

  std::shared_ptr<toka::FunctionType> funcType = nullptr;

  // 3. Resolve Static Methods / Enum Variants
  size_t pos = CallName.find("::");
  if (pos != std::string::npos) {
    std::string ShapeName = Type::stripMorphology(CallName.substr(0, pos));
    std::string VariantName = CallName.substr(pos + 2);

    if (ShapeMap.count(ShapeName)) {
      // Static Method
      if (MethodMap.count(ShapeName) &&
          MethodMap[ShapeName].count(VariantName)) {
        // We don't have full signature for static methods in MethodMap
        // (just return string) For Stage 5, we check args as expressions
        // but CANNOT verify arity/types strictly yet unless we store Method
        // Decl via visitShapeDecl. Existing legacy logic just returned
        // MethodMap[ShapeName][VariantName].
        for (auto &Arg : Call->Args)
          checkExpr(Arg.get());
        return toka::Type::fromString(MethodMap[ShapeName][VariantName]);
      }
      // Enum Variant Constructor
      ShapeDecl *SD = ShapeMap[ShapeName];
      if (SD->Kind == ShapeKind::Enum) {
        for (auto &Memb : SD->Members) {
          if (Memb.Name == VariantName) {
            // Enum Variant Constructor: Variant(Args...) -> ShapeName
            // We need Memb's type to verify arg?
            // Members in Enum are Variants. If Variant has payload, its
            // Type is the payload type? Checking args...
            for (auto &Arg : Call->Args)
              checkExpr(Arg.get());
            return toka::Type::fromString(ShapeName);
          }
        }
      }
    }
  }

  // 4. Regular Function Lookup
  FunctionDecl *Fn = nullptr;
  ExternDecl *Ext = nullptr;
  ShapeDecl *Sh = nullptr; // Constructor

  size_t scopePos = CallName.find("::");
  if (scopePos != std::string::npos) {
    std::string ModName = CallName.substr(0, scopePos);
    std::string FuncName = CallName.substr(scopePos + 2);
    SymbolInfo modSpec;
    if (CurrentScope->lookup(ModName, modSpec) && modSpec.ReferencedModule) {
      ModuleScope *target = (ModuleScope *)modSpec.ReferencedModule;
      llvm::errs() << "DEBUG: Looking up '" << FuncName << "' in module '"
                   << target->Name << "'\n";
      if (target->Functions.count(FuncName))
        Fn = target->Functions[FuncName];
      else if (target->Externs.count(FuncName))
        Ext = target->Externs[FuncName];
      else if (target->Shapes.count(FuncName))
        Sh = target->Shapes[FuncName];

      if (!Fn && !Ext && !Sh) {
        llvm::errs() << "DEBUG: Symbol not found. Available functions:\n";
        for (auto const &[n, f] : target->Functions)
          llvm::errs() << "  Fn: " << n << "\n";
        for (auto const &[n, f] : target->Externs)
          llvm::errs() << "  Ext: " << n << "\n";
      }
    } else {
      error(Call, "Module '" + ModName + "' not found or not imported");
      return toka::Type::fromString("unknown");
    }
  } else {
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

  if (Fn) {
    Call->ResolvedFn = Fn;
    for (auto &Arg : Fn->Args) {
      ParamTypes.push_back(toka::Type::fromString(synthesizePhysicalType(Arg)));
    }
    ReturnType = toka::Type::fromString(Fn->ReturnType);
    IsVariadic = Fn->IsVariadic;
  } else if (Ext) {
    Call->ResolvedExtern = Ext;
    for (auto &Arg : Ext->Args) {
      ParamTypes.push_back(toka::Type::fromString(synthesizePhysicalType(Arg)));
    }
    ReturnType = toka::Type::fromString(Ext->ReturnType);
    IsVariadic = Ext->IsVariadic;
  } else if (Sh) {
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
            if (M.Name == fieldName) {
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

        if (!valType->isCompatibleWith(*expectedType)) {
          error(valExpr, "Type mismatch for field '" +
                             (isNamed ? fieldName : std::to_string(argIdx)) +
                             "': expected " + expectedType->toString() +
                             ", got " + valType->toString());
        }
        argIdx++;
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

    if (!argType->isCompatibleWith(*paramType)) {
      error(Call->Args[i].get(), "Type mismatch for argument " +
                                     std::to_string(i + 1) + ": expected " +
                                     paramType->toString() + ", got " +
                                     argType->toString());
    }
  }

  return ReturnType;
}

} // namespace toka
