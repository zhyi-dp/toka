#include "toka/Sema.h"
#include "llvm/Support/raw_ostream.h"

namespace toka {

bool Sema::checkModule(Module &M) {
  CurrentModule = &M; // Set context
  // 1. Register all globals (Functions, Structs, etc.)
  registerGlobals(M);

  // 2. Check function bodies
  for (auto &Fn : M.Functions) {
    checkFunction(Fn.get());
  }

  CurrentModule = nullptr;
  return !HasError;
}

void Sema::error(ASTNode *Node, const std::string &Msg) {
  HasError = true;
  llvm::errs() << (Node->FileName.empty() ? "<unknown>" : Node->FileName) << ":"
               << Node->Line << ":" << Node->Column << ": error: " << Msg
               << "\n";
}

void Sema::enterScope() { CurrentScope = new Scope(CurrentScope); }

void Sema::exitScope() {
  Scope *Old = CurrentScope;
  // Cleanup borrows
  for (auto const &[refName, borrowInfo] : Old->ActiveBorrows) {
    std::string sourceName = borrowInfo.first;
    bool isMutable = borrowInfo.second;
    SymbolInfo *sourcePtr = nullptr;
    if (CurrentScope->Parent &&
        CurrentScope->Parent->findSymbol(sourceName, sourcePtr)) {
      if (isMutable)
        sourcePtr->IsMutablyBorrowed = false;
      else
        sourcePtr->ImmutableBorrowCount--;
    }
  }
  CurrentScope = CurrentScope->Parent;
  delete Old;
}
void Sema::clearStmtBorrows() {
  for (auto const &borrow : m_CurrentStmtBorrows) {
    SymbolInfo *info = nullptr;
    if (CurrentScope->findSymbol(borrow.first, info)) {
      if (borrow.second)
        info->IsMutablyBorrowed = false;
      else
        info->ImmutableBorrowCount--;
    }
  }
  m_CurrentStmtBorrows.clear();
  m_LastBorrowSource = "";
}

void Sema::registerGlobals(Module &M) {
  enterScope(); // Global scope

  for (auto &Fn : M.Functions) {
    GlobalFunctions.push_back(Fn.get());
    // Also checking for duplicates within the SAME module could be good, but
    // expensive O(N^2) or need a set. For now, assume Parser prevents
    // duplicates or let it slide.
  }

  for (auto &Ext : M.Externs) {
    if (ExternMap.count(Ext->Name)) {
      error(Ext.get(), "redefinition of extern '" + Ext->Name + "'");
    } else {
      ExternMap[Ext->Name] = Ext.get();
    }
  }

  for (auto &St : M.Shapes) {
    if (ShapeMap.count(St->Name)) {
      error(St.get(), "redefinition of shape '" + St->Name + "'");
    } else {
      ShapeMap[St->Name] = St.get();
    }
  }

  for (auto &Alias : M.TypeAliases) {
    if (TypeAliasMap.count(Alias->Name)) {
      error(Alias.get(), "redefinition of type alias '" + Alias->Name + "'");
    } else {
      TypeAliasMap[Alias->Name] = Alias->TargetType;
    }
  }

  for (auto &Trait : M.Traits) {
    if (TraitMap.count(Trait->Name)) {
      error(Trait.get(), "redefinition of trait '" + Trait->Name + "'");
    } else {
      TraitMap[Trait->Name] = Trait.get();
    }
  }

  for (auto &Impl : M.Impls) {
    std::set<std::string> implemented;
    for (auto &Method : Impl->Methods) {
      MethodMap[Impl->TypeName][Method->Name] = Method->ReturnType;
      implemented.insert(Method->Name);
    }
    // Handle Trait Defaults
    if (!Impl->TraitName.empty()) {
      if (TraitMap.count(Impl->TraitName)) {
        TraitDecl *TD = TraitMap[Impl->TraitName];
        for (auto &Method : TD->Methods) {
          if (implemented.count(Method->Name))
            continue;
          if (Method->Body) {
            // Trait provides a default implementation
            MethodMap[Impl->TypeName][Method->Name] = Method->ReturnType;
          }
        }
      } else {
        error(Impl.get(), "trait '" + Impl->TraitName +
                              "' not found for implementation on '" +
                              Impl->TypeName + "'");
      }
    }
  }
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
    Info.Type = T;
    Info.IsValueMutable = Pat->IsMutable | SourceIsMutable;
    if (Pat->IsReference) {
      Info.Morphology = "&";
    }
    CurrentScope->define(Pat->Name, Info);
    break;
  }

  case MatchArm::Pattern::Decons: {
    // Pat->Name might be "Ok" or "Result::Ok"
    std::string variantName = Pat->Name;
    std::string shapeName = T;

    size_t pos = variantName.find("::");
    if (pos != std::string::npos) {
      shapeName = variantName.substr(0, pos);
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
          if (foundMemb->Type.empty()) {
            error(static_cast<ASTNode *>(Pat),
                  "variant '" + variantName + "' takes no payload");
          } else {
            // For now assume single payload if not tuple
            checkPattern(Pat->SubPatterns[0].get(), foundMemb->Type,
                         SourceIsMutable);
          }
        }
      } else {
        error(static_cast<ASTNode *>(Pat), "variant '" + variantName +
                                               "' not found in shape '" +
                                               shapeName + "'");
      }
    } else {
      error(static_cast<ASTNode *>(Pat),
            "unknown shape '" + shapeName + "' in pattern deconstruction");
    }
    break;
  }
  }
}

void Sema::checkFunction(FunctionDecl *Fn) {
  CurrentFunctionReturnType = Fn->ReturnType;
  enterScope(); // Function scope

  // Register arguments
  for (auto &Arg : Fn->Args) {
    SymbolInfo Info;
    Info.Type = Arg.Type;
    Info.IsValueMutable = Arg.IsMutable;
    Info.IsValueNullable = Arg.IsNullable;
    if (Arg.IsReference)
      Info.Morphology = "&";
    else if (Arg.IsUnique)
      Info.Morphology = "^";
    else if (Arg.IsShared)
      Info.Morphology = "~";
    else if (Arg.HasPointer)
      Info.Morphology = "*";

    CurrentScope->define(Arg.Name, Info);
  }

  if (Fn->Body) {
    checkStmt(Fn->Body.get());
  }

  // TODO: Check if all paths return if return type is not void

  exitScope();
  CurrentFunctionReturnType = "";
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
      ExprType = checkExpr(Ret->ReturnValue.get());
    }
    clearStmtBorrows();

    if (!isTypeCompatible(CurrentFunctionReturnType, ExprType)) {
      error(Ret, "return type mismatch: expected '" +
                     CurrentFunctionReturnType + "', got '" + ExprType + "'");
    }
  } else if (auto *ExprS = dynamic_cast<ExprStmt *>(S)) {
    checkExpr(ExprS->Expression.get());
    clearStmtBorrows();
  } else if (auto *Var = dynamic_cast<VariableDecl *>(S)) {
    std::string InitType = "";
    if (Var->Init) {
      m_ControlFlowStack.push_back({Var->Name, "void", false});
      InitType = checkExpr(Var->Init.get());
      m_ControlFlowStack.pop_back();
    }

    // If type not specified, infer from init
    if (Var->TypeName.empty() || Var->TypeName == "auto") {
      if (InitType.empty() || InitType == "void") {
        error(Var, "variable '" + Var->Name + "' type must be specified");
        Var->TypeName = "unknown";
      } else {
        Var->TypeName = InitType;
        // Strip ? if we want strict auto, but Toka often keeps it
      }
    } else {
      // Check compatibility
      if (!InitType.empty() && !isTypeCompatible(Var->TypeName, InitType)) {
        error(Var, "cannot initialize variable of type '" + Var->TypeName +
                       "' with '" + InitType + "'");
      }
    }

    SymbolInfo Info;
    Info.Type = Var->TypeName;
    Info.IsValueMutable = Var->IsValueMutable || Var->IsMutable;
    Info.IsValueNullable = Var->IsValueNullable || Var->IsNullable;
    Info.IsRebindable = Var->IsRebindable;
    Info.IsPointerNullable = Var->IsPointerNullable;

    if (Var->HasPointer)
      Info.Morphology = "*";
    else if (Var->IsUnique)
      Info.Morphology = "^";
    else if (Var->IsShared)
      Info.Morphology = "~";
    else if (Var->IsReference)
      Info.Morphology = "&";
    else
      Info.Morphology = "";

    // Legacy fallback: If IsMutable is set but IsValueMutable isn't
    if (Var->IsMutable && !Info.IsValueMutable)
      Info.IsValueMutable = true;
    if (Var->IsNullable && !Info.IsValueNullable)
      Info.IsValueNullable = true;

    // Type cleanup: If type is "^T", we often store "T" in Info.Type and "^" in
    // Morphology
    if (Info.Type.size() > 1 && (Info.Type[0] == '^' || Info.Type[0] == '~' ||
                                 Info.Type[0] == '*' || Info.Type[0] == '&')) {
      if (Info.Morphology.empty()) {
        Info.Morphology = std::string(1, Info.Type[0]);
      }
      Info.Type = Info.Type.substr(1);
    }

    // Borrow tracking link
    if (Info.Morphology == "&" && !m_LastBorrowSource.empty()) {
      Info.BorrowedFrom = m_LastBorrowSource;
      // Remove from temporary borrows since it's now persistent
      for (auto it = m_CurrentStmtBorrows.begin();
           it != m_CurrentStmtBorrows.end(); ++it) {
        if (it->first == m_LastBorrowSource) {
          m_CurrentStmtBorrows.erase(it);
          break;
        }
      }
    }
    m_LastBorrowSource = ""; // Clear for next var

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
              error(Var,
                    "cannot move '" + RHSVar->Name + "' while it is borrowed");
            }
            CurrentScope->markMoved(RHSVar->Name);
          }
        }
      }
    }
    clearStmtBorrows();
  } else if (auto *Destruct = dynamic_cast<DestructuringDecl *>(S)) {
    std::string InitType = resolveType(checkExpr(Destruct->Init.get()));
    std::string DeclType = resolveType(Destruct->TypeName);

    // Basic check: DeclType should match InitType (or InitType is tuple/struct
    // compatible)
    if (DeclType != InitType && InitType != "unknown" &&
        InitType != "tuple") { // "tuple" is weak type for now
      error(Destruct, "destructuring type mismatch: expected '" + DeclType +
                          "', got '" + InitType + "'");
    }

    // We need to define the variables.
    // For now, blindly assume the fields match the variables in order/count.
    // In a real compiler we'd check against the Struct/Tuple definition.
    // Real implementation needs to iterate Struct fields and assign types to
    // Variables by index.
    if (ShapeMap.count(DeclType)) {
      ShapeDecl *SD = ShapeMap[DeclType];
      size_t Limit = std::min(SD->Members.size(), Destruct->Variables.size());
      for (size_t i = 0; i < Limit; ++i) {
        SymbolInfo Info;
        Info.Type = SD->Members[i].Type;
        Info.IsValueMutable = Destruct->Variables[i].IsMutable;
        Info.IsValueNullable = Destruct->Variables[i].IsNullable;
        Info.Morphology = "";
        CurrentScope->define(Destruct->Variables[i].Name, Info);
      }
    } else if (TypeAliasMap.count(DeclType)) {
      for (const auto &Var : Destruct->Variables) {
        SymbolInfo Info;
        Info.Type = "i32"; // Fallback
        Info.IsValueMutable = Var.IsMutable;
        Info.IsValueNullable = Var.IsNullable;
        Info.Morphology = "";
        CurrentScope->define(Var.Name, Info);
      }
    } else {
      for (const auto &Var : Destruct->Variables) {
        SymbolInfo Info;
        Info.Type = "unknown"; // Fallback
        Info.IsValueMutable = Var.IsMutable;
        Info.IsValueNullable = Var.IsNullable;
        Info.Morphology = "";
        CurrentScope->define(Var.Name, Info);
      }
    }
  }
}

std::string Sema::checkExpr(Expr *E) {
  if (!E)
    return "void";

  if (auto *Num = dynamic_cast<NumberExpr *>(E)) {
    return "i32"; // Default for now
  } else if (auto *Flt = dynamic_cast<FloatExpr *>(E)) {
    return "f64";
  } else if (auto *Bool = dynamic_cast<BoolExpr *>(E)) {
    return "bool";
  } else if (auto *Addr = dynamic_cast<AddressOfExpr *>(E)) {
    // strict: &T -> &T (Reference)
    // Toka Spec: &x creates a Reference.
    // If x is T, &x is &T.
    std::string inner = checkExpr(Addr->Expression.get());
    if (inner == "unknown")
      return "unknown";
    return "&" + inner;
  } else if (auto *Deref = dynamic_cast<DereferenceExpr *>(E)) {
    std::string inner = checkExpr(Deref->Expression.get());
    if (inner == "unknown")
      return "unknown";

    // Dereference a pointer/reference/handle
    if (inner.size() > 1 && (inner[0] == '^' || inner[0] == '&' ||
                             inner[0] == '*' || inner[0] == '~')) {
      return inner.substr(1);
    }
    // Spec: variable name is the object. dereferencing a non-pointer is error?
    // Morpholopy: *ptr is Identity value (address).
    // Wait, AddressOfExpr is &x. DereferenceExpr is *x.
    // If x is ^i32. *x accesses the raw address? Or the value?
    // CodeGen: dereference load from pointer.
    // So *x returns the pointee type.
    error(Deref, "cannot dereference non-pointer type '" + inner + "'");
    return "unknown";
  } else if (auto *Unary = dynamic_cast<UnaryExpr *>(E)) {
    std::string rhsInfo = checkExpr(Unary->RHS.get());
    if (rhsInfo == "unknown")
      return "unknown";

    if (Unary->Op == TokenType::Bang) { // !
      if (rhsInfo != "bool") {
        error(Unary, "operand of '!' must be bool, got '" + rhsInfo + "'");
      }
      return "bool";
    } else if (Unary->Op == TokenType::Minus) { // -
      if (rhsInfo != "i32" && rhsInfo != "i64" && rhsInfo != "f32" &&
          rhsInfo != "f64") {
        error(Unary, "operand of '-' must be numeric, got '" + rhsInfo + "'");
      }
      return rhsInfo;
    } else if (Unary->Op == TokenType::Star || Unary->Op == TokenType::Caret ||
               Unary->Op == TokenType::Tilde ||
               Unary->Op == TokenType::Ampersand) {
      if (auto *Var = dynamic_cast<VariableExpr *>(Unary->RHS.get())) {
        SymbolInfo *Info = nullptr;
        if (CurrentScope->findSymbol(Var->Name, Info)) {
          if (Unary->Op == TokenType::Ampersand) {
            // Borrowing logic
            bool wantMutable = Var->IsMutable;
            if (wantMutable) {
              if (!Info->IsValueMutable) {
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
          } else if (Unary->Op == TokenType::Caret) {
            // Move check
            if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
              error(Unary,
                    "cannot move '" + Var->Name + "' while it is borrowed");
            }
          }
          std::string Morph = "";
          if (Unary->Op == TokenType::Star)
            Morph = "*";
          else if (Unary->Op == TokenType::Caret)
            Morph = "^";
          else if (Unary->Op == TokenType::Tilde)
            Morph = "~";
          else if (Unary->Op == TokenType::Ampersand)
            Morph = "&";

          bool isNullable = Info->IsPointerNullable || Info->IsValueNullable;
          return Morph + (isNullable ? "?" : "") + Info->Type;
        }
      }
      return (Unary->Op == TokenType::Star
                  ? "*"
                  : (Unary->Op == TokenType::Caret
                         ? "^"
                         : (Unary->Op == TokenType::Tilde ? "~" : "&"))) +
             rhsInfo;
    }
    // Handle ++ -- (Prefix)
    if (Unary->Op == TokenType::PlusPlus ||
        Unary->Op == TokenType::MinusMinus) {
      if (rhsInfo != "i32" && rhsInfo != "i64") {
        error(Unary, "operand of increment/decrement must be integer, got '" +
                         rhsInfo + "'");
      }
      if (auto *Var = dynamic_cast<VariableExpr *>(Unary->RHS.get())) {
        SymbolInfo *Info = nullptr;
        if (CurrentScope->findSymbol(Var->Name, Info)) {
          if (!Info->IsValueMutable) {
            error(Unary, "cannot modify immutable variable '" + Var->Name +
                             "' (# suffix required)");
          }
          if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
            error(Unary,
                  "cannot modify '" + Var->Name + "' while it is borrowed");
          }
        }
      }
      return rhsInfo;
    }
    return rhsInfo;
  } else if (auto *Str = dynamic_cast<StringExpr *>(E)) {
    return "str";
  } else if (auto *Var = dynamic_cast<VariableExpr *>(E)) {
    SymbolInfo Info;
    if (!CurrentScope->lookup(Var->Name, Info)) {
      error(Var, "use of undeclared identifier '" + Var->Name + "'");
      return "unknown";
    }
    if (Info.Moved) {
      error(Var, "use of moved value: '" + Var->Name + "'");
    }
    if (Info.IsMutablyBorrowed) {
      error(Var, "cannot access '" + Var->Name +
                     "' while it is mutably borrowed (Rule 406)");
    }
    std::string baseType = Info.Type;
    if (!baseType.empty() && baseType.back() == '?')
      baseType.pop_back();

    return baseType +
           (Info.IsValueNullable || Info.IsPointerNullable ? "?" : "");
  } else if (auto *Null = dynamic_cast<NullExpr *>(E)) {
    return "null";
  } else if (auto *None = dynamic_cast<NoneExpr *>(E)) {
    return "none";
  } else if (auto *Bin = dynamic_cast<BinaryExpr *>(E)) {
    if (Bin->Op == "is") {
      checkExpr(Bin->LHS.get());
      checkExpr(Bin->RHS.get());
      return "bool";
    }
    std::string LHS = checkExpr(Bin->LHS.get());
    std::string RHS = checkExpr(Bin->RHS.get());

    bool isRefAssign = false;
    // Assignment
    if (Bin->Op == "=") {
      // Move Logic: If RHS is Unique Variable, mark it moved.
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
            error(Bin,
                  "cannot move '" + RHSVar->Name + "' while it is borrowed");
          }
          CurrentScope->markMoved(RHSVar->Name);
        }
      }

      // Logic for Reference Assignment:
      // If LHS is a variable that is a reference, we are assigning to the
      // pointee. LHS type (from checkExpr) returns the type of the symbol. If
      // Symbol.IsReference is true, does checkExpr return T or ^T?
      // ... (Rest of existing logic)
      if (auto *VarLHS = dynamic_cast<VariableExpr *>(Bin->LHS.get())) {
        SymbolInfo Info;
        if (CurrentScope->lookup(VarLHS->Name, Info)) {
          if (Info.IsReference()) {
            // It's a reference variable.
            // We expect RHS to match the Pointee type.
            if (LHS.size() > 1 && LHS[0] == '&') {
              std::string Pointee = LHS.substr(1);
              if (!isTypeCompatible(Pointee, RHS)) {
                error(Bin, "assignment type mismatch (ref): cannot assign '" +
                               RHS + "' to '" + Pointee + "'");
              }
              return Pointee;
            }
            isRefAssign = true;
          }
        }
      }
    }

    bool isAssign = (Bin->Op == "=" || Bin->Op == "+=" || Bin->Op == "-=" ||
                     Bin->Op == "*=" || Bin->Op == "/=");

    if (isAssign) {
      if (!isRefAssign && !isTypeCompatible(LHS, RHS) && LHS != "unknown" &&
          RHS != "unknown") {
        error(Bin, "assignment type mismatch: cannot assign '" + RHS +
                       "' to '" + LHS + "'");
      }

      // Strict Mutability Check
      Expr *LHSExpr = Bin->LHS.get();
      if (auto *Var = dynamic_cast<VariableExpr *>(LHSExpr)) {
        SymbolInfo *InfoPtr = nullptr;
        if (CurrentScope->findSymbol(Var->Name, InfoPtr)) {
          if (InfoPtr->IsMutablyBorrowed || InfoPtr->ImmutableBorrowCount > 0) {
            error(Bin,
                  "cannot modify '" + Var->Name + "' while it is borrowed");
          }
          if (!InfoPtr->Morphology.empty() && InfoPtr->Morphology != "&") {
            if (!InfoPtr->IsRebindable) {
              error(LHSExpr,
                    "cannot reassign fixed pointer '" + Var->Name +
                        "' (requires morphology # or !, e.g. ^# or ^!)");
            }
          } else {
            if (!InfoPtr->IsValueMutable) {
              error(LHSExpr, "cannot assign to immutable variable '" +
                                 Var->Name + "' (# required)");
            }
          }
          if (!Var->IsMutable) {
            error(Var, "cannot modify variable '" + Var->Name +
                           "' without # suffix");
          }
        }
      } else if (auto *Un = dynamic_cast<UnaryExpr *>(LHSExpr)) {
        if (Un->Op == TokenType::Star || Un->Op == TokenType::Caret ||
            Un->Op == TokenType::Tilde) {
          if (auto *Var = dynamic_cast<VariableExpr *>(Un->RHS.get())) {
            SymbolInfo *InfoPtr = nullptr;
            if (CurrentScope->findSymbol(Var->Name, InfoPtr)) {
              if (!InfoPtr->IsRebindable) {
                error(LHSExpr, "cannot reseat fixed pointer identity '" +
                                   Var->Name + "' (morphology # required)");
              }
            }
          }
        }
      } else if (auto *Memb = dynamic_cast<MemberExpr *>(LHSExpr)) {
        Expr *Base = Memb->Object.get();
        while (auto *SubMemb = dynamic_cast<MemberExpr *>(Base)) {
          Base = SubMemb->Object.get();
        }
        if (auto *Var = dynamic_cast<VariableExpr *>(Base)) {
          SymbolInfo Info;
          if (CurrentScope->lookup(Var->Name, Info)) {
            if (!Info.IsValueMutable) {
              error(Memb, "cannot assign to field of immutable variable '" +
                              Var->Name + "' (# required)");
            }
          }
        }
      }
    }

    // Logic
    if (Bin->Op == "&&" || Bin->Op == "||") {
      if (LHS != "bool" || RHS != "bool") {
        error(Bin, "operands of '" + Bin->Op + "' must be bool");
      }
      return "bool";
    }

    // Comparison
    if (Bin->Op == "==" || Bin->Op == "!=" || Bin->Op == "<" ||
        Bin->Op == ">" || Bin->Op == "<=" || Bin->Op == ">=") {
      if (!isTypeCompatible(LHS, RHS)) {
        error(Bin, "operands of '" + Bin->Op + "' must be same type ('" + LHS +
                       "' vs '" + RHS + "')");
      }
      return "bool";
    }

    if (!isTypeCompatible(LHS, RHS) && LHS != "unknown" && RHS != "unknown") {
      error(Bin, "binary operator '" + Bin->Op +
                     "' operands must have same type ('" + LHS + "' vs '" +
                     RHS + "')");
      return "unknown";
    }
    // Arithmetic
    if (Bin->Op == "+" || Bin->Op == "-" || Bin->Op == "*" || Bin->Op == "/") {
      if (LHS != "i32" && LHS != "i64" && LHS != "f32" && LHS != "f64" &&
          LHS != "u32" && LHS != "u64" && LHS != "i8" && LHS != "u8" &&
          LHS != "i16" && LHS != "u16") {
        error(Bin, "operands of '" + Bin->Op + "' must be numeric, got '" +
                       LHS + "'");
      }
    }

    return LHS;
  } else if (auto *ie = dynamic_cast<IfExpr *>(E)) {

    std::string condType = checkExpr(ie->Condition.get());

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
            infoPtr->IsValueNullable = false;
            infoPtr->IsPointerNullable = false;
            if (!infoPtr->Type.empty() && infoPtr->Type[0] == '?')
              infoPtr->Type = infoPtr->Type.substr(1);
            if (!infoPtr->Type.empty() && infoPtr->Type.back() == '?')
              infoPtr->Type.pop_back();
            narrowed = true;
          }
        }
      }
    }

    m_ControlFlowStack.push_back({"", "void", false});
    checkStmt(ie->Then.get());

    // Restore if narrowed
    if (narrowed) {
      SymbolInfo *infoPtr = nullptr;
      if (CurrentScope->findSymbol(narrowedVar, infoPtr)) {
        *infoPtr = originalInfo;
      }
    }

    std::string thenType = m_ControlFlowStack.back().ExpectedType;
    m_ControlFlowStack.pop_back();

    std::string elseType = "void";
    if (ie->Else) {
      m_ControlFlowStack.push_back({"", "void", false});
      checkStmt(ie->Else.get());
      elseType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();
    }

    if (thenType != "void" && elseType != "void" &&
        !isTypeCompatible(thenType, elseType)) {
      error(ie, "If branches have incompatible types: '" + thenType +
                    "' and '" + elseType + "'");
    }
    return (thenType != "void") ? thenType : elseType;
  } else if (auto *we = dynamic_cast<WhileExpr *>(E)) {
    checkExpr(we->Condition.get());
    bool tookOver = false;
    if (!m_ControlFlowStack.empty() && !m_ControlFlowStack.back().IsLoop &&
        !m_ControlFlowStack.back().Label.empty()) {
      m_ControlFlowStack.back().IsLoop = true;
      tookOver = true;
    } else {
      m_ControlFlowStack.push_back({"", "void", true});
    }
    checkStmt(we->Body.get());
    std::string bodyType = m_ControlFlowStack.back().ExpectedType;
    if (!tookOver)
      m_ControlFlowStack.pop_back();

    std::string elseType = "void";
    if (we->ElseBody) {
      m_ControlFlowStack.push_back({"", "void", false});
      checkStmt(we->ElseBody.get());
      elseType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();
    }

    if (bodyType != "void" && !we->ElseBody) {
      error(we, "Yielding while loop must have an 'or' block");
    }
    if (bodyType != "void" && elseType != "void" &&
        !isTypeCompatible(bodyType, elseType)) {
      error(we, "While loop branches have incompatible types");
    }
    return (bodyType != "void") ? bodyType : elseType;
  } else if (auto *le = dynamic_cast<LoopExpr *>(E)) {
    bool tookOver = false;
    if (!m_ControlFlowStack.empty() && !m_ControlFlowStack.back().IsLoop &&
        !m_ControlFlowStack.back().Label.empty()) {
      m_ControlFlowStack.back().IsLoop = true;
      tookOver = true;
    } else {
      m_ControlFlowStack.push_back({"", "void", true});
    }
    checkStmt(le->Body.get());
    std::string res = m_ControlFlowStack.back().ExpectedType;
    if (!tookOver)
      m_ControlFlowStack.pop_back();
    return res;
  } else if (auto *fe = dynamic_cast<ForExpr *>(E)) {
    std::string collType = checkExpr(fe->Collection.get());
    std::string elemType = "i32"; // TODO: better inference
    if (collType.size() > 2 && collType.substr(0, 2) == "[]")
      elemType = collType.substr(2);

    enterScope();
    SymbolInfo Info;
    Info.Type = elemType;
    Info.IsValueMutable = fe->IsMutable;
    Info.Morphology = fe->IsReference ? "&" : "";
    CurrentScope->define(fe->VarName, Info);

    bool tookOver = false;
    if (!m_ControlFlowStack.empty() && !m_ControlFlowStack.back().IsLoop &&
        !m_ControlFlowStack.back().Label.empty()) {
      m_ControlFlowStack.back().IsLoop = true;
      tookOver = true;
    } else {
      m_ControlFlowStack.push_back({"", "void", true});
    }
    checkStmt(fe->Body.get());
    std::string bodyType = m_ControlFlowStack.back().ExpectedType;
    if (!tookOver)
      m_ControlFlowStack.pop_back();

    std::string elseType = "void";
    if (fe->ElseBody) {
      m_ControlFlowStack.push_back({"", "void", false});
      checkStmt(fe->ElseBody.get());
      elseType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();
    }
    exitScope();

    if (bodyType != "void" && !fe->ElseBody) {
      error(fe, "Yielding for loop must have an 'or' block");
    }
    if (bodyType != "void" && elseType != "void" &&
        !isTypeCompatible(bodyType, elseType)) {
      error(fe, "For loop branches have incompatible types");
    }
    return (bodyType != "void") ? bodyType : elseType;
  } else if (auto *pe = dynamic_cast<PassExpr *>(E)) {
    std::string valType = checkExpr(pe->Value.get());
    if (!m_ControlFlowStack.empty()) {
      if (m_ControlFlowStack.back().ExpectedType == "void") {
        m_ControlFlowStack.back().ExpectedType = valType;
      } else if (!isTypeCompatible(m_ControlFlowStack.back().ExpectedType,
                                   valType)) {
        error(pe, "Yield type mismatch");
      }
    }
    return "void";
  } else if (auto *be = dynamic_cast<BreakExpr *>(E)) {
    std::string valType = "void";
    if (be->Value)
      valType = checkExpr(be->Value.get());

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
    return "void";
  } else if (auto *ce = dynamic_cast<ContinueExpr *>(E)) {
    // Continue target must be a loop
    return "void";
  } else if (auto *Call = dynamic_cast<CallExpr *>(E)) {
    // Check GlobalFunctions
    std::string CallName = Call->Callee;
    std::string ShapeName = "";
    std::string VariantName = "";

    // Resolve Shape::Variant
    size_t pos = CallName.find("::");
    if (pos != std::string::npos) {
      ShapeName = CallName.substr(0, pos);
      VariantName = CallName.substr(pos + 2);

      if (ShapeMap.count(ShapeName)) {
        ShapeDecl *SD = ShapeMap[ShapeName];
        if (SD->Kind == ShapeKind::Enum) {
          for (auto &Memb : SD->Members) {
            if (Memb.Name == VariantName) {
              // It's a variant constructor
              for (auto &Arg : Call->Args)
                checkExpr(Arg.get());
              return ShapeName;
            }
          }
        }
      }
    }

    // Regular call
    FunctionDecl *Fn = nullptr;
    std::string FnName = Call->Callee;

    // 1. Check if name is namespaced (e.g. lib::foo)
    size_t scopePos = FnName.find("::");
    if (scopePos != std::string::npos) {
      std::string ModName = FnName.substr(0, scopePos);
      std::string FuncName = FnName.substr(scopePos + 2);

      // Verify ModName is bound in CurrentModule->Imports
      const ImportDecl *BoundImport = nullptr;
      if (CurrentModule) {
        for (const auto &Imp : CurrentModule->Imports) {
          // Rule: To access Mod::Func, 'Mod' must be bound.
          // Only module imports (empty items) bind the module name.
          if (!Imp->Items.empty())
            continue;

          std::string BoundName;
          if (!Imp->Alias.empty()) {
            BoundName = Imp->Alias;
          } else {
            // Derive from path stem
            std::string Stem = Imp->PhysicalPath;
            size_t lastSlash = Stem.find_last_of('/');
            if (lastSlash != std::string::npos)
              Stem = Stem.substr(lastSlash + 1);
            size_t dot = Stem.find_last_of('.');
            if (dot != std::string::npos)
              Stem = Stem.substr(0, dot);
            BoundName = Stem;
          }

          if (BoundName == ModName) {
            BoundImport = Imp.get();
            break;
          }
        }
      }

      if (!BoundImport) {
        error(Call, "Module '" + ModName +
                        "' is not imported or bound in this scope");
        return "";
      }

      // Find matching function
      for (auto *GF : GlobalFunctions) {
        if (GF->Name == FuncName) {
          // Check if Module matches the BoundImport's path.
          // We assume physical path stem matches function file stem.
          // Ideally we should compare full resolved paths, but for now strict
          // stem matching + bound check is enough.

          std::string Stem = GF->FileName;
          size_t lastSlash = Stem.find_last_of('/');
          if (lastSlash != std::string::npos)
            Stem = Stem.substr(lastSlash + 1);
          size_t dot = Stem.find_last_of('.');
          if (dot != std::string::npos)
            Stem = Stem.substr(0, dot);

          // IMPORTANT: If we used an alias (import core/io as sys), ModName is
          // "sys". But the Function's native module name is "io". We must
          // ensure that the function we found actually belongs to the file
          // pointed to by BoundImport.

          // Let's derive the stem from BoundImport->PhysicalPath
          std::string ImportStem = BoundImport->PhysicalPath;
          size_t iSlash = ImportStem.find_last_of('/');
          if (iSlash != std::string::npos)
            ImportStem = ImportStem.substr(iSlash + 1);
          size_t iDot = ImportStem.find_last_of('.');
          if (iDot != std::string::npos)
            ImportStem = ImportStem.substr(0, iDot);

          if (Stem == ImportStem) {
            Fn = GF;
            break;
          }
        }
      }
    } else {
      // 2. Unqualified lookup
      // Priority: Local Module > Imported Explicitly > Global?
      // Spec: Default Private.

      for (auto *GF : GlobalFunctions) {
        if (GF->Name == FnName) {
          // Determine if this function is visible
          bool isVisible = false;

          // Same Module?
          if (CurrentModule && !CurrentModule->Functions.empty() &&
              GF->FileName == CurrentModule->Functions[0]->FileName) {
            isVisible = true;
          } else if (CurrentModule) {
            // It's external. Is it visible?
            // Check imports.

            std::string GFStem = GF->FileName;
            size_t lastSlash = GFStem.find_last_of('/');
            if (lastSlash != std::string::npos)
              GFStem = GFStem.substr(lastSlash + 1);
            size_t dot = GFStem.find_last_of('.');
            if (dot != std::string::npos)
              GFStem = GFStem.substr(0, dot);

            for (const auto &Imp : CurrentModule->Imports) {
              std::string ImpStem = Imp->PhysicalPath;
              size_t iSlash = ImpStem.find_last_of('/');
              if (iSlash != std::string::npos)
                ImpStem = ImpStem.substr(iSlash + 1);

              // Handle "local" imports? ImpStem might be "lib.tk" or just "lib"
              // My parser handles "lib" or "lib.tk" if I patched it?
              // Actually parsing "import std/json" gives "std/json".
              // So ImpStem -> "json".

              if (ImpStem == GFStem) {
                if (!Imp->Items.empty()) {
                  for (const auto &Item : Imp->Items) {
                    if (Item.Symbol == "*" || Item.Symbol == FnName) {
                      isVisible = true;
                      break;
                    }
                  }
                }
              }
              if (isVisible)
                break;
            }
          }

          if (isVisible) {
            Fn = GF;
            break;
          }
        }
      }
    }

    if (Fn) {

      if (!Fn->IsPub && Fn->FileName != Call->FileName) {
        // Relaxed privacy check: allow calls within the same module, regardless
        // of file. This means if a function is private, it can still be called
        // by other functions in the same module, even if they are in different
        // files within that module. The original check was too strict,
        // requiring the *exact* same file.
        bool sameModule = false;
        if (CurrentModule && !CurrentModule->Functions.empty()) {
          // Check if the called function's file belongs to the current module
          // This is a heuristic, assuming functions in the same module share a
          // common base path or are explicitly listed. A more robust check
          // would involve a module-level function registry.
          for (const auto &funcInModule : CurrentModule->Functions) {
            if (funcInModule->FileName == Fn->FileName) {
              sameModule = true;
              break;
            }
          }
        }
        if (!sameModule) {
          error(Call, "Function '" + Fn->Name + "' is private to '" +
                          Fn->FileName + "'");
        }
      }

      if (Fn->Args.size() != Call->Args.size() && !Fn->IsVariadic) {
        error(Call, "incorrect number of arguments for function '" +
                        Call->Callee + "'");
      }
      for (size_t i = 0; i < Call->Args.size(); ++i) {
        std::string ArgType = checkExpr(Call->Args[i].get());
        if (i < Fn->Args.size()) {
          if (Fn->Args[i].IsReference) {
            if (ArgType == "&" + Fn->Args[i].Type ||
                ArgType == "^" + Fn->Args[i].Type) {
              continue;
            }
          }

          if (!isTypeCompatible(Fn->Args[i].Type, ArgType)) {
            error(Call->Args[i].get(), "argument type mismatch: expected '" +
                                           Fn->Args[i].Type + "', got '" +
                                           ArgType + "'");
          }
        }
      }
      return Fn->ReturnType;
    }

    // Check ExternMap
    if (ExternMap.count(Call->Callee)) {
      ExternDecl *Fn = ExternMap[Call->Callee];
      if (Fn->Args.size() != Call->Args.size() && !Fn->IsVariadic) {
        // If variadic, we need at least fixed args
        if (Call->Args.size() < Fn->Args.size())
          error(Call, "incorrect number of arguments for extern function '" +
                          Call->Callee + "'");
      }
      for (size_t i = 0; i < Call->Args.size(); ++i) {
        std::string ArgType = checkExpr(Call->Args[i].get());
        if (i < Fn->Args.size()) {
          const auto &arg = Fn->Args[i];
          std::string ExpectedTy = arg.Type;
          if (arg.IsReference)
            ExpectedTy = "^" + ExpectedTy;
          else if (arg.HasPointer)
            ExpectedTy = "*" + ExpectedTy;

          if (!isTypeCompatible(ExpectedTy, ArgType)) {
            error(Call->Args[i].get(), "argument type mismatch: expected '" +
                                           ExpectedTy + "', got '" + ArgType +
                                           "'");
          }
        }
      }
      return Fn->ReturnType;
      return Fn->ReturnType;
    }
    // Check for Shape Constructor (Struct Initialization via Call)
    else if (ShapeMap.count(Call->Callee)) {
      ShapeDecl *sh = ShapeMap[Call->Callee];
      if (sh->Kind == ShapeKind::Struct || sh->Kind == ShapeKind::Tuple) {
        // Validate arguments
        // We allow named args matching fields, or positional args if Tuple.
        // Or mixed? Toka seems to use named for internal fields.

        size_t argIdx = 0;
        for (auto &arg : Call->Args) {
          // Check if argument is "Field = Value" (Named Argument)
          bool isNamed = false;
          std::string fieldName;
          Expr *valueExpr = arg.get();

          // We only support named args on the top level of the call arg
          // Parsing produces BinaryExpr(=) for this.
          if (auto *bin = dynamic_cast<BinaryExpr *>(arg.get())) {
            if (bin->Op == "=") {
              if (auto *var = dynamic_cast<VariableExpr *>(bin->LHS.get())) {
                fieldName = var->Name;
                valueExpr = bin->RHS.get();
                isNamed = true;
              }
            }
          }

          if (isNamed) {
            // Validate field name and type
            bool found = false;
            std::string fieldType;
            for (auto &mem : sh->Members) {
              if (mem.Name == fieldName) {
                found = true;
                fieldType = mem.Type;
                break;
              }
            }
            if (!found) {
              error(arg.get(), "Shape '" + Call->Callee +
                                   "' has no field named '" + fieldName + "'");
            } else {
              // Check value type
              // We invoke checkExpr on the RHS, ignoring the LHS (label) to
              // avoid undeclared var error
              std::string valType = checkExpr(valueExpr);
              if (!isTypeCompatible(fieldType, valType)) {
                error(valueExpr, "Field '" + fieldName + "' expects type '" +
                                     fieldType + "', got '" + valType + "'");
              }
            }
          } else {
            // Positional Argument
            // Only valid for Tuples or if we decide to support positional
            // struct init
            if (sh->Kind == ShapeKind::Struct) {
              // Actually, let's strictly require named args for Structs for now
              // to match user expectation, OR check if user intends positional.
              // test_shape_match.tk: Point(u8, u8...) uses positional?
              // shape Point(u8, u8, u8, u8) is Tuple-like struct?
              // If members are named "0", "1", etc. it is Tuple.
              // If named, it is Struct.
              // Parser sets Kind=Struct if it sees 'name :'.
              // Parser sets Kind=Tuple if it sees comma separated types?
              // Let's assume purely positional for Struct is risky unless
              // defined order. But wait, test_shape_match.tk uses `RawBytes[1,
              // 2, 3, 4]`. That's ArrayIndexExpr, not CallExpr. But `shape
              // Point(u8, ....)` -> `auto p = Point(1, 2, 3, 4)` ? User changed
              // `struct Point {x,y,z}` to `shape Point(x:i32...)`. They used
              // `Point(x=1...)`. This is named.

              // If Positional passed to Struct with named fields:
              if (argIdx < sh->Members.size()) {
                // Allow positional for struct too?
                // For now, assume yes if no name provided.
                std::string valType = checkExpr(valueExpr);
                if (!isTypeCompatible(sh->Members[argIdx].Type, valType)) {
                  error(valueExpr, "Field '" + sh->Members[argIdx].Name +
                                       "' (arg " + std::to_string(argIdx) +
                                       ") expects '" +
                                       sh->Members[argIdx].Type + "', got '" +
                                       valType + "'");
                }
              } else {
                error(arg.get(),
                      "Too many arguments for shape '" + Call->Callee + "'");
              }
            } else { // Tuple
              if (argIdx < sh->Members.size()) {
                std::string valType = checkExpr(valueExpr);
                if (!isTypeCompatible(sh->Members[argIdx].Type, valType)) {
                  error(valueExpr, "Tuple element " + std::to_string(argIdx) +
                                       " expects '" + sh->Members[argIdx].Type +
                                       "', got '" + valType + "'");
                }
              }
            }
          }
          argIdx++;
        }
        return Call->Callee;
      }
    }
    // Check Option Variants Name::Variant
    else if (Call->Callee.find("::") != std::string::npos) {
      size_t pos = Call->Callee.find("::");
      std::string optName = Call->Callee.substr(0, pos);
      // Verify variant exists?
      // For now assume yes or checked during codegen/parsing?
      // Sema should check.
      if (ShapeMap.count(optName)) {
        // It's a valid shape. check variant?
        // The variant name is in Call->Callee.
      }
      return optName;
    }

    error(Call, "call to undefined function or shape '" + Call->Callee + "'");
    return "unknown";
  } else if (auto *New = dynamic_cast<NewExpr *>(E)) {
    // Return the Type being created.
    // Validating the Initializer matches the Type is good (e.g.
    // InitStructExpr)
    if (New->Initializer) {
      std::string InitType = checkExpr(New->Initializer.get());
      if (!isTypeCompatible(New->Type, InitType) && InitType != "unknown") {
        // InitStruct returns "StructName".
        // checkExpr(InitStruct) returns "StructName".
        // So this should match.
        error(New, "new expression type mismatch: expected '" + New->Type +
                       "', got '" + InitType + "'");
      }
    }
    // 'new' usually returns a pointer or the type itself depending on
    // context. AST just says Type. The variable decl usually says ^Type.
    return New->Type;
  } else if (auto *Met = dynamic_cast<MethodCallExpr *>(E)) {
    std::string ObjType = resolveType(checkExpr(Met->Object.get()));
    if (MethodMap.count(ObjType) && MethodMap[ObjType].count(Met->Method)) {
      return MethodMap[ObjType][Met->Method];
    }
    // Check if it's a reference to a struct
    if (ObjType.size() > 1 && ObjType[0] == '^') {
      std::string Pointee = ObjType.substr(1);
      if (MethodMap.count(Pointee) && MethodMap[Pointee].count(Met->Method)) {
        return MethodMap[Pointee][Met->Method];
      }
    }
    error(Met,
          "method '" + Met->Method + "' not found on type '" + ObjType + "'");
    return "unknown";
  } else if (auto *Init = dynamic_cast<InitStructExpr *>(E)) {
    // Validate fields against ShapeMap
    if (ShapeMap.count(Init->ShapeName)) {
      ShapeDecl *SD = ShapeMap[Init->ShapeName];

      // Visibility Check:
      if (!SD->IsPub && SD->FileName != Init->FileName) {
        // Relaxed privacy check: allow calls within the same module, regardless
        // of file.
        bool sameModule = false;
        if (CurrentModule && !CurrentModule->Shapes.empty()) {
          for (const auto &shapeInModule : CurrentModule->Shapes) {
            if (shapeInModule->FileName == SD->FileName) {
              sameModule = true;
              break;
            }
          }
        }
        if (!sameModule) {
          error(Init, "Struct '" + Init->ShapeName + "' is private to '" +
                          SD->FileName + "'");
        }
      }

      // Check fields exist and types match
      // Simplified check: just return the name
      // TODO: Check fields
    } else {
      error(Init, "unknown struct '" + Init->ShapeName + "'");
    }
    return Init->ShapeName;
  } else if (auto *Memb = dynamic_cast<MemberExpr *>(E)) {
    std::string ObjTypeFull = checkExpr(Memb->Object.get());
    if (ObjTypeFull.find('?') != std::string::npos) {
      error(Memb, "cannot access member of nullable '" + ObjTypeFull +
                      "' without 'is' check (Rule 408)");
    }
    std::string ObjType = resolveType(ObjTypeFull);

    // Auto-dereference if it's a pointer/ref (strips ^, &, *, ~)
    if (ObjType.size() > 1 && (ObjType[0] == '^' || ObjType[0] == '&' ||
                               ObjType[0] == '*' || ObjType[0] == '~')) {
      ObjType = ObjType.substr(1);
    }
    // Strip suffix '?' if it was '^?Point' -> '?Point' -> 'Point'
    if (!ObjType.empty() && ObjType.back() == '?')
      ObjType.pop_back();

    // Secondary middle check if prefix icon remained somehow?
    if (!ObjType.empty() && ObjType[0] == '?')
      ObjType = ObjType.substr(1);

    if (ShapeMap.count(ObjType)) {
      ShapeDecl *SD = ShapeMap[ObjType];
      for (const auto &Field : SD->Members) {
        if (Field.Name == Memb->Member) {
          return Field.Type;
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
      // Let's return "unknown" to suppress error but allow it to pass checks
      // if we are lenient.
      return "unknown";
    } else if (ObjType != "unknown") {
      error(Memb, "member access on non-struct type '" + ObjType + "'");
    }
    return "unknown";
  } else if (auto *Post = dynamic_cast<PostfixExpr *>(E)) {
    std::string lhsInfo = checkExpr(Post->LHS.get());
    if (auto *Var = dynamic_cast<VariableExpr *>(Post->LHS.get())) {
      SymbolInfo Info;
      if (CurrentScope->lookup(Var->Name, Info) && !Info.IsValueMutable) {
        error(Post, "cannot modify immutable variable '" + Var->Name +
                        "' (# suffix required)");
      }
    }
    return lhsInfo;
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
            std::string ArgType = checkExpr(idx.get());
            if (!isTypeCompatible(ElemType, ArgType)) {
              error(idx.get(), "Array element type mismatch: expected '" +
                                   ElemType + "', got '" + ArgType + "'");
            }
          }
          return Var->Name;
        }
      }
    }

    // Normal Array Indexing
    std::string arrType = checkExpr(Arr->Array.get());
    if (Arr->Indices.size() != 1) {
      error(Arr, "Array indexing expects exactly 1 index");
    }
    checkExpr(Arr->Indices[0].get());
    return "unknown"; // Placeholder for element type derivation
  } else if (auto *Tup = dynamic_cast<TupleExpr *>(E)) {
    std::string s = "(";
    for (size_t i = 0; i < Tup->Elements.size(); ++i) {
      if (i > 0)
        s += ", ";
      s += checkExpr(Tup->Elements[i].get());
    }
    s += ")";
    return s;
  } else if (auto *ArrLit = dynamic_cast<ArrayExpr *>(E)) {
    // Infer from first element
    if (!ArrLit->Elements.empty()) {
      std::string ElemTy = checkExpr(ArrLit->Elements[0].get());
      return "[" + ElemTy + "; " + std::to_string(ArrLit->Elements.size()) +
             "]";
    }
    return "[i32; 0]";
  } else if (auto *me = dynamic_cast<MatchExpr *>(E)) {
    std::string targetType = checkExpr(me->Target.get());
    std::string resultType = "void";

    for (auto &arm : me->Arms) {
      enterScope();
      checkPattern(arm->Pat.get(), targetType, false);
      if (arm->Guard) {
        if (checkExpr(arm->Guard.get()) != "bool")
          error(arm->Guard.get(), "guard must be bool");
      }
      m_ControlFlowStack.push_back({"", "void", false});
      checkStmt(arm->Body.get());
      std::string armType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();

      if (resultType == "void")
        resultType = armType;
      else if (armType != "void" && !isTypeCompatible(resultType, armType)) {
        if (resultType != "unknown" && armType != "unknown")
          error(me, "Match arms have incompatible types ('" + resultType +
                        "' vs '" + armType + "')");
      }
      exitScope();
    }
    return resultType;
  }

  return "unknown";
}

std::string Sema::resolveType(const std::string &Type) {
  if (TypeAliasMap.count(Type)) {
    return resolveType(TypeAliasMap[Type]);
  }
  return Type;
}

bool Sema::isTypeCompatible(const std::string &Target,
                            const std::string &Source) {
  std::string T = resolveType(Target);
  std::string S = resolveType(Source);

  if (T == S)
    return true;
  if (T == "unknown" || S == "unknown")
    return true;

  // Integer Literal Promotion
  if ((T == "i32" || T == "u32" || T == "i64" || T == "u64" || T == "i8" ||
       T == "u8" || T == "i16" || T == "u16") &&
      (S == "i32" || S == "u32" || S == "i64" || S == "u64" || S == "i8" ||
       S == "u8" || S == "i16" || S == "u16")) {
    return true;
  }

  // String literal conversion
  if (S == "str" && (T == "*i8" || T == "^i8" || T == "str"))
    return true;

  if (S == "null") {
    // Identity nulling: Requires the morphology to be nullable (^? or ^!)
    if (T.size() >= 2 &&
        (T[0] == '^' || T[0] == '*' || T[0] == '~' || T[0] == '&')) {
      return (T[1] == '?' || T[1] == '!');
    }
    return false;
  }
  if (S == "none") {
    // Value nulling: Requires the value type to have a nullable suffix
    // But we must distinguish between ^?T (pointer nullable) and ^T? (value
    // nullable) Actually in checkExpr(VariableExpr), we return baseType + "?"
    // if either is nullable.
    // To be strict, none should target the value attribute.
    if (!T.empty() && T.back() == '?') {
      return true;
    }
    return false;
  }

  // Basic Tuple/Struct weak check: if both look like tuples, allow for now
  if (T.size() > 0 && T[0] == '(' && S.size() > 0 && S[0] == '(')
    return true;

  return false;
}

} // namespace toka
