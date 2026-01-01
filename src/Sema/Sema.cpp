#include "toka/Sema.h"
#include "llvm/Support/raw_ostream.h"

namespace toka {

bool Sema::checkModule(Module &M) {
  // 1. Register all globals (Functions, Structs, etc.)
  registerGlobals(M);

  // 2. Check function bodies
  for (auto &Fn : M.Functions) {
    checkFunction(Fn.get());
  }

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
    if (FunctionMap.count(Fn->Name)) {
      error(Fn.get(), "redefinition of function '" + Fn->Name + "'");
    } else {
      FunctionMap[Fn->Name] = Fn.get();
    }
  }

  for (auto &Ext : M.Externs) {
    if (ExternMap.count(Ext->Name)) {
      error(Ext.get(), "redefinition of extern '" + Ext->Name + "'");
    } else {
      ExternMap[Ext->Name] = Ext.get();
    }
  }

  for (auto &St : M.Structs) {
    if (StructMap.count(St->Name)) {
      error(St.get(), "redefinition of struct '" + St->Name + "'");
    } else {
      StructMap[St->Name] = St.get();
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
  } else if (auto *If = dynamic_cast<IfStmt *>(S)) {
    std::string CondType = checkExpr(If->Condition.get());
    clearStmtBorrows();
    if (CondType != "bool") {
      error(If->Condition.get(),
            "if condition must be bool, got '" + CondType + "'");
    }

    // Narrowing logic for 'is' operator
    std::string refinedVar = "";
    if (auto *Bin = dynamic_cast<BinaryExpr *>(If->Condition.get())) {
      if (Bin->Op == "is") {
        if (auto *UL = dynamic_cast<UnaryExpr *>(Bin->LHS.get())) {
          if (auto *UR = dynamic_cast<UnaryExpr *>(Bin->RHS.get())) {
            if (UL->Op == UR->Op && UL->HasNull && !UR->HasNull) {
              if (auto *VL = dynamic_cast<VariableExpr *>(UL->RHS.get())) {
                refinedVar = VL->Name;
              }
            }
          }
        } else if (auto *VL = dynamic_cast<VariableExpr *>(Bin->LHS.get())) {
          if (auto *VR = dynamic_cast<VariableExpr *>(Bin->RHS.get())) {
            if (VL->Name == VR->Name && VL->IsNullable && !VR->IsNullable) {
              refinedVar = VL->Name;
            }
          }
        }
      }
    }

    if (!refinedVar.empty()) {
      SymbolInfo *Info = nullptr;
      if (CurrentScope->findSymbol(refinedVar, Info)) {
        enterScope();
        SymbolInfo Refined = *Info;
        Refined.IsPointerNullable = false;
        Refined.IsValueNullable = false;
        CurrentScope->Symbols[refinedVar] = Refined;
        checkStmt(If->Then.get());
        exitScope();
      } else {
        checkStmt(If->Then.get());
      }
    } else {
      checkStmt(If->Then.get());
    }

    if (If->Else)
      checkStmt(If->Else.get());
  } else if (auto *Var = dynamic_cast<VariableDecl *>(S)) {
    std::string InitType = "";
    if (Var->Init) {
      InitType = checkExpr(Var->Init.get());
    }

    // If type not specified, infer from init
    if (Var->TypeName.empty()) {
      if (InitType.empty()) {
        error(Var, "variable '" + Var->Name + "' type must be specified");
        Var->TypeName = "unknown";
      } else {
        Var->TypeName = InitType;
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
  } else if (auto *While = dynamic_cast<WhileStmt *>(S)) {
    std::string CondType = checkExpr(While->Condition.get());
    clearStmtBorrows();
    if (CondType != "bool") {
      error(While->Condition.get(),
            "while condition must be bool, got '" + CondType + "'");
    }
    checkStmt(While->Body.get());
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
    if (StructMap.count(DeclType)) {
      StructDecl *SD = StructMap[DeclType];
      size_t Limit = std::min(SD->Fields.size(), Destruct->Variables.size());
      for (size_t i = 0; i < Limit; ++i) {
        SymbolInfo Info;
        Info.Type = SD->Fields[i].Type;
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
      bool IsRefAssign = false;
      if (auto *VarLHS = dynamic_cast<VariableExpr *>(Bin->LHS.get())) {
        SymbolInfo Info;
        if (CurrentScope->lookup(VarLHS->Name, Info)) {
          if (Info.IsReference()) {
            // It's a reference variable.
            // We expect RHS to match the Pointee type.
            // Type is likely "^i32". Pointee is "i32".
            if (LHS.size() > 1 && LHS[0] == '^') {
              std::string Pointee = LHS.substr(1);
              if (!isTypeCompatible(Pointee, RHS)) {
                error(Bin, "assignment type mismatch (ref): cannot assign '" +
                               RHS + "' to '" + Pointee + "'");
              }
              return Pointee; // Assignment result type? usually void or
                              // value.
            }
            IsRefAssign = true;
          }
        }
      }

      if (!IsRefAssign && !isTypeCompatible(LHS, RHS) && LHS != "unknown" &&
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
          // FOR POINTER TYPES: ANY assignment to the variable itself is a
          // Reseat. Because changing the view 'p' of a pointer variable means
          // changing what it points to.
          // EXCEPTION: References (&) cannot be reseated, so assignment is
          // value mutation.
          if (!InfoPtr->Morphology.empty() && InfoPtr->Morphology != "&") {
            if (!InfoPtr->IsRebindable) {
              error(LHSExpr,
                    "cannot reassign fixed pointer '" + Var->Name +
                        "' (requires morphology # or !, e.g. ^# or ^!)");
            } else if (!isTypeCompatible(LHS, RHS)) {
              // This handles the 'null' vs 'none' check and '?' slot check
              error(LHSExpr, "assignment type mismatch: cannot assign '" + RHS +
                                 "' to '" + LHS + "'");
            }
          } else {
            // FOR PLAIN VALUES: Assignment is a Value Mutation.
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
        // Identity reseating: *p = ..., ^p = ...
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
      return LHS;
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
    // Result type depends on Op.
    // Arithmetic
    // Strict arithmetic: only numeric types
    if (Bin->Op == "+" || Bin->Op == "-" || Bin->Op == "*" || Bin->Op == "/") {
      if (LHS != "i32" && LHS != "i64" && LHS != "f32" && LHS != "f64") {
        // Maybe allow pointer arithmetic later?
        error(Bin, "operands of '" + Bin->Op + "' must be numeric, got '" +
                       LHS + "'");
      }
    }

    return LHS;
  } else if (auto *Call = dynamic_cast<CallExpr *>(E)) {
    // Check FunctionMap
    if (FunctionMap.count(Call->Callee)) {
      FunctionDecl *Fn = FunctionMap[Call->Callee];
      if (Fn->Args.size() != Call->Args.size() && !Fn->IsVariadic) {
        error(Call, "incorrect number of arguments for function '" +
                        Call->Callee + "'");
      }
      for (size_t i = 0; i < Call->Args.size(); ++i) {
        std::string ArgType = checkExpr(Call->Args[i].get());
        if (i < Fn->Args.size()) {
          // Check Reference compatibility:
          // If Param is Reference (IsReference=true in AST), it expects
          // pointer/address? AST `FunctionDecl::Arg` has `IsReference`.
          // Parser: `fn foo(&p: Point)`. `&` sets IsReference. Type is
          // "Point". `checkExpr(&val)` returns "^Point". So we need to accept
          // "^Point" if Expected is "Point" AND IsReference is true.

          if (Fn->Args[i].IsReference) {
            if (ArgType == "&" + Fn->Args[i].Type ||
                ArgType == "^" + Fn->Args[i].Type) {
              // Compatible!
              continue;
            }
            // Also if ArgType == Type? (implicit address take? No Toka
            // usually explicit)
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
    else if (ExternMap.count(Call->Callee)) {
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
    }
    // Check Option Variants Name::Variant
    else if (Call->Callee.find("::") != std::string::npos) {
      size_t pos = Call->Callee.find("::");
      std::string optName = Call->Callee.substr(0, pos);
      return optName;
    }

    error(Call, "call to undefined function '" + Call->Callee + "'");
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
    // Validate fields against StructMap
    if (StructMap.count(Init->StructName)) {
      StructDecl *SD = StructMap[Init->StructName];
      // Check fields exist and types match
      // Simplified check: just return the name
      // TODO: Check fields
    } else {
      error(Init, "unknown struct '" + Init->StructName + "'");
    }
    return Init->StructName;
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

    if (StructMap.count(ObjType)) {
      StructDecl *SD = StructMap[ObjType];
      for (const auto &Field : SD->Fields) {
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
    // Array type usually [Type; Size] or just Type (if treated as slice/ptr).
    // currently explicit array types are not well defined in my AST string
    // representation for checks. Assuming variable type is "Type" (if it was
    // decays to pointer) or handling it is tricky without richer Type system.
    // Let's just return "i32" or something or try to strip "[]".
    // Simple hack: if type is "i32[]", return "i32".
    // But my parser/AST uses generic string types.
    return "unknown"; // Placeholder
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
    return "unknown";
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
