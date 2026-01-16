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
#include "toka/Sema.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cctype>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace toka {

bool Sema::checkModule(Module &M) {
  enterScope();       // Module-level global scope
  CurrentModule = &M; // Set context
  // 1. Register all globals (Functions, Structs, etc.)
  registerGlobals(M);
  // 2. Shape Analysis Pass (Safety Enforcement)
  analyzeShapes(M);
  checkShapeSovereignty();

  // 2b. Check function bodies (reordered)
  for (auto &Fn : M.Functions) {
    checkFunction(Fn.get());
  }
  // ...

  // Transfer ownership of synthetic anonymous record shapes to the Module
  // so CodeGen can see them as regular structs.
  for (auto &S : SyntheticShapes) {
    M.Shapes.push_back(std::move(S));
  }
  SyntheticShapes.clear();

  CurrentModule = nullptr;
  exitScope();
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

  // Initialize ModuleScope
  ModuleScope &ms = ModuleMap[M.FileName];
  ms.Name = M.FileName;
  // Simple name extraction (e.g. std/io.tk -> io)
  size_t lastSlash = ms.Name.find_last_of('/');
  if (lastSlash != std::string::npos) {
    ms.Name = ms.Name.substr(lastSlash + 1);
  }
  size_t dot = ms.Name.find_last_of('.');
  if (dot != std::string::npos) {
    ms.Name = ms.Name.substr(0, dot);
  }

  // Case A: Register local symbols in the ModuleScope
  for (auto &Fn : M.Functions) {
    ms.Functions[Fn->Name] = Fn.get();
    GlobalFunctions.push_back(
        Fn.get()); // Still keep global map for flat-checks
  }
  for (auto &Ext : M.Externs) {
    ms.Externs[Ext->Name] = Ext.get();
    ExternMap[Ext->Name] = Ext.get();
  }
  for (auto &St : M.Shapes) {
    ms.Shapes[St->Name] = St.get();
    ShapeMap[St->Name] = St.get();
  }
  for (auto &Alias : M.TypeAliases) {
    ms.TypeAliases[Alias->Name] = {Alias->TargetType, Alias->IsStrong};
    TypeAliasMap[Alias->Name] = {Alias->TargetType, Alias->IsStrong};
  }
  for (auto &Trait : M.Traits) {
    ms.Traits[Trait->Name] = Trait.get();
    TraitMap[Trait->Name] = Trait.get();

    // Register Trait methods for 'dyn' dispatch checks
    std::string traitKey = "@" + Trait->Name;
    for (auto &Method : Trait->Methods) {
      MethodMap[traitKey][Method->Name] = Method->ReturnType;
      MethodDecls[traitKey][Method->Name] = Method.get();
    }
  }
  for (auto &G : M.Globals) {
    if (auto *v = dynamic_cast<VariableDecl *>(G.get())) {

      ms.Globals[v->Name] = v;
      // In-line inference for global constants if TypeName is missing
      if (v->TypeName.empty() && v->Init) {
        if (auto *cast = dynamic_cast<CastExpr *>(v->Init.get())) {
          v->TypeName = cast->TargetType;
        } else if (dynamic_cast<NumberExpr *>(v->Init.get())) {
          v->TypeName = "i64";
        } else if (dynamic_cast<BoolExpr *>(v->Init.get())) {
          v->TypeName = "bool";
        } else if (dynamic_cast<StringExpr *>(v->Init.get())) {
          v->TypeName = "str";
        } else {
          // Last resort: run full checkExpr (e.g. for AnonymousRecordExpr)
          std::shared_ptr<toka::Type> inferredType = checkExpr(v->Init.get());
          std::string inferred = inferredType->toString();
          if (!inferredType->isUnknown() && !inferredType->isVoid()) {
            v->TypeName = inferred;
          }
        }
      }
    }
  }

  // Case B: Handle Imports
  for (auto &Imp : M.Imports) {
    ModuleScope *target = nullptr;
    // We need to resolve PhysicalPath to what's in ModuleMap
    // The ModuleMap is keyed by whatever FileName was set in main.cpp
    for (auto &[path, scope] : ModuleMap) {
      if (path == Imp->PhysicalPath ||
          (path.find(Imp->PhysicalPath) != std::string::npos &&
           path.length() > Imp->PhysicalPath.length())) {
        target = &scope;
        break;
      }
    }

    if (!target) {
      error(Imp.get(), "module '" + Imp->PhysicalPath + "' not found");
      continue;
    }

    if (Imp->Items.empty()) {
      // 1. Simple Import: import std/io
      SymbolInfo info;
      info.TypeObj = toka::Type::fromString("module");
      info.ReferencedModule = target;
      std::string modName = Imp->Alias.empty() ? target->Name : Imp->Alias;
      CurrentScope->define(modName, info);
    } else {
      // 2. Specific Import: import std/io::println
      for (auto &item : Imp->Items) {
        if (item.Symbol == "*") {
          // Import all functions
          for (auto const &[name, fn] : target->Functions)
            CurrentScope->define(item.Alias.empty() ? name : item.Alias,
                                 {toka::Type::fromString("fn")});
          // Import all shapes
          for (auto const &[name, sh] : target->Shapes) {
            ShapeMap[name] =
                sh; // Still needs to be in global maps for resolution
          }
          // Import all aliases
          for (auto const &[name, ai] : target->TypeAliases) {
            TypeAliasMap[name] = ai;
          }
          // Import all traits
          for (auto const &[name, trait] : target->Traits) {
            TraitMap[name] = trait;
          }
          // Import all externs
          for (auto const &[name, ext] : target->Externs) {
            ExternMap[name] = ext;
            CurrentScope->define(name, {toka::Type::fromString("extern")});
          }
          // Import all globals (constants)
          for (auto const &[name, v] : target->Globals) {
            std::string morph = "";
            if (v->HasPointer)
              morph = "*";
            else if (v->IsUnique)
              morph = "^";
            else if (v->IsShared)
              morph = "~";
            else if (v->IsReference)
              morph = "&";

            std::string fullType = morph + v->TypeName;
            if (v->IsRebindable)
              fullType += "!";
            if (v->IsValueMutable)
              fullType += "#";
            if (v->IsPointerNullable || v->IsValueNullable)
              fullType += "?";

            CurrentScope->define(item.Alias.empty() ? name : item.Alias,
                                 {toka::Type::fromString(fullType)});
          }
        } else {
          // Import specific
          std::string name = item.Alias.empty() ? item.Symbol : item.Alias;
          bool found = false;
          // Trait name lookup hack: if symbol is @Trait, look for Trait
          std::string lookupSym = item.Symbol;
          if (lookupSym.size() > 1 && lookupSym[0] == '@') {
            lookupSym = lookupSym.substr(1);
          }

          if (target->Functions.count(item.Symbol)) {
            CurrentScope->define(name, {toka::Type::fromString("fn")});
            found = true;
          } else if (target->Shapes.count(item.Symbol)) {
            ShapeMap[name] = target->Shapes[item.Symbol];
            found = true;
          } else if (target->TypeAliases.count(item.Symbol)) {
            TypeAliasMap[name] = target->TypeAliases[item.Symbol];
            found = true;
          } else if (target->Traits.count(lookupSym)) {
            TraitMap[name] = target->Traits[lookupSym];
            found = true;
          } else if (target->Externs.count(item.Symbol)) {
            ExternMap[name] = target->Externs[item.Symbol];
            CurrentScope->define(name, {toka::Type::fromString("extern")});
            found = true;
          } else if (target->Globals.count(item.Symbol)) {
            auto *v = target->Globals[item.Symbol];
            std::string morph = "";
            if (v->HasPointer)
              morph = "*";
            else if (v->IsUnique)
              morph = "^";
            else if (v->IsShared)
              morph = "~";
            else if (v->IsReference)
              morph = "&";

            std::string fullType = morph + v->TypeName;
            if (v->IsRebindable)
              fullType += "!";
            if (v->IsValueMutable)
              fullType += "#";
            if (v->IsPointerNullable || v->IsValueNullable)
              fullType += "?";

            CurrentScope->define(name, {toka::Type::fromString(fullType)});
            found = true;
          }

          if (!found) {
            error(Imp.get(), "symbol '" + item.Symbol +
                                 "' not found in module '" + Imp->PhysicalPath +
                                 "'");
          }
        }
      }
    }
  }

  for (auto &Impl : M.Impls) {
    if (Impl->TraitName == "encap") {
      EncapMap[Impl->TypeName] = Impl->EncapEntries;
      // removed continue to allow method registration (hybrid trait)
    }
    std::set<std::string> implemented;
    for (auto &Method : Impl->Methods) {
      MethodMap[Impl->TypeName][Method->Name] = Method->ReturnType;
      MethodDecls[Impl->TypeName][Method->Name] = Method.get();
      implemented.insert(Method->Name);
    }
    // Populate ImplMap
    if (!Impl->TraitName.empty()) {
      std::string implKey = Impl->TypeName + "@" + Impl->TraitName;
      for (auto &Method : Impl->Methods) {
        ImplMap[implKey][Method->Name] = Method.get();
      }
    }
    // Handle Trait Defaults
    if (!Impl->TraitName.empty()) {
      if (TraitMap.count(Impl->TraitName)) {
        TraitDecl *TD = TraitMap[Impl->TraitName];
        for (auto &Method : TD->Methods) {
          if (implemented.count(Method->Name)) {
            // Verify Signature Match (Pub/Priv)
            // We need to find the Impl method
            FunctionDecl *ImplMethod = nullptr;
            // Search in Impl->Methods
            for (auto &m : Impl->Methods) {
              if (m->Name == Method->Name) {
                ImplMethod = m.get();
                break;
              }
            }
            if (ImplMethod) {
              if (ImplMethod->IsPub != Method->IsPub) {
                std::string traitVis = Method->IsPub ? "pub" : "private";
                std::string implVis = ImplMethod->IsPub ? "pub" : "private";
                error(ImplMethod, "signature mismatch: trait method '" +
                                      Method->Name + "' is " + traitVis +
                                      ", but implementation is " + implVis);
              }
            }
            continue;
          }
          if (Method->Body) {
            // Trait provides a default implementation
            MethodMap[Impl->TypeName][Method->Name] = Method->ReturnType;
            MethodDecls[Impl->TypeName][Method->Name] = Method.get();
          } else {
            error(Impl.get(), "Missing implementation for method '" +
                                  Method->Name + "' of trait '" +
                                  Impl->TraitName + "'");
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
          if (foundMemb->Type.empty() && foundMemb->SubMembers.empty()) {
            error(static_cast<ASTNode *>(Pat),
                  "variant '" + variantName + "' takes no payload");
          } else {
            if (!foundMemb->SubMembers.empty()) {
              // Multi-field tuple variant
              if (Pat->SubPatterns.size() != foundMemb->SubMembers.size()) {
                error(static_cast<ASTNode *>(Pat),
                      "variant '" + variantName + "' expects " +
                          std::to_string(foundMemb->SubMembers.size()) +
                          " fields, but got " +
                          std::to_string(Pat->SubPatterns.size()));
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
                error(static_cast<ASTNode *>(Pat),
                      "variant '" + variantName +
                          "' expects 1 field, but got " +
                          std::to_string(Pat->SubPatterns.size()));
              }
              checkPattern(Pat->SubPatterns[0].get(), foundMemb->Type,
                           SourceIsMutable);
            }
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

namespace {
bool allPathsReturn(Stmt *S) {
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

bool allPathsJump(Stmt *S) {
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
} // namespace

void Sema::checkFunction(FunctionDecl *Fn) {
  CurrentFunctionReturnType = Fn->ReturnType;
  enterScope(); // Function scope

  // Register arguments
  for (auto &Arg : Fn->Args) {
    SymbolInfo Info;
    // Construct Full Type String for Type Object
    std::string fullType = "";
    if (Arg.IsReference)
      fullType += "&";
    else if (Arg.IsUnique)
      fullType += "^";
    else if (Arg.IsShared)
      fullType += "~";
    else if (Arg.HasPointer)
      fullType += "*";

    std::string baseType = Arg.Type;
    // Strip redundant sigil if present in baseType and implied by flags
    if (baseType.size() > 1) {
      char c = baseType[0];
      if ((Arg.IsReference && c == '&') || (Arg.IsUnique && c == '^') ||
          (Arg.IsShared && c == '~') || (Arg.HasPointer && c == '*')) {
        baseType = baseType.substr(1);
      }
    }

    fullType += baseType;
    if (Arg.IsRebindable)
      fullType += "!";
    if (Arg.IsValueMutable || Arg.IsMutable)
      fullType += "#";
    if (Arg.IsPointerNullable || Arg.IsValueNullable || Arg.IsNullable)
      fullType += "?";

    // [New] Annotated AST: Use resolveType (string version) to handle
    // aliases/Self, then parse
    std::string resolvedStr = resolveType(fullType);
    Info.TypeObj = toka::Type::fromString(resolvedStr);

    // Assign to AST Node for CodeGen
    Arg.ResolvedType = Info.TypeObj;

    CurrentScope->define(Arg.Name, Info);
  }

  if (Fn->Body) {
    checkStmt(Fn->Body.get());

    // Check if all paths return if return type is not void
    if (Fn->ReturnType != "void") {
      if (!allPathsReturn(Fn->Body.get())) {
        error(Fn,
              "control reaches end of non-void function '" + Fn->Name + "'");
      }
    }
  }

  exitScope();
  CurrentFunctionReturnType = "";
}

void Sema::checkShapeSovereignty() {
  for (auto const &[name, decl] : ShapeMap) {
    if (decl->Kind == ShapeKind::Struct) {
      bool needsDrop = false;

      // Check if Shape manages resources
      for (auto &memb : decl->Members) {
        // 1. Raw Pointers (*T)
        if (memb.HasPointer) {
          needsDrop = true;
          break;
        }
        // 2. Unique Pointers (^T)
        if (memb.IsUnique) {
          needsDrop = true;
          break;
        }
        // 3. Members that need drop (Recursive check)
        if (m_ShapeProps.count(memb.Type) && m_ShapeProps[memb.Type].HasDrop) {
          needsDrop = true;
          break;
        }
      }

      if (needsDrop) {
        // Must have 'drop' method in MethodMap
        // Check MethodMap[name]["drop"]
        bool hasDropImpl = false;
        if (MethodMap.count(name) && MethodMap[name].count("drop")) {
          hasDropImpl = true;
        }

        if (!hasDropImpl) {
          error(decl, "Shape '" + name +
                          "' manages resources/sovereignty but does not "
                          "implement 'drop'");
        }
      }
    }
  }
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
      error(Ret, "return type mismatch: expected '" +
                     CurrentFunctionReturnType + "', got '" + ExprType + "'");
    }
  } else if (auto *Free = dynamic_cast<FreeStmt *>(S)) {
    auto FreeTypeObj = checkExpr(Free->Expression.get());
    if (!FreeTypeObj->isRawPointer()) {
      std::string ExprType = FreeTypeObj->toString();
      if (FreeTypeObj->isSmartPointer()) {
        error(Free, "manual 'free' is forbidden for smart pointers ('" +
                        ExprType + "'). Use automatic scoping instead.");
      } else {
        error(Free,
              "can only 'free' a raw pointer ('*'), got '" + ExprType + "'");
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
        error(Var, "variable '" + Var->Name + "' type must be specified");
        Var->TypeName = "unknown";
      } else {
        std::string Inferred = InitType;
        // If the variable declaration HAS morphology, and the initializer
        // ALSO has it, we must strip it from the inferred type to avoid
        // redundancy (e.g. *i32 -> i32)
        if (Inferred == "nullptr" && Var->TypeName.empty()) {
          error(Var, "Cannot infer type from 'nullptr'. Explicit type "
                     "annotation required (e.g., 'auto *p: Data = nullptr').");
          Var->TypeName = "unknown";
          return;
        }

        if (Var->HasPointer || Var->IsUnique || Var->IsShared ||
            Var->IsReference) {
          if (!Inferred.empty() && (Inferred[0] == '*' || Inferred[0] == '^' ||
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
        error(Var, "cannot initialize variable of type '" + DeclFullTy +
                       "' with '" + InitType + "'");
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
    // Legacy behavior: If type implies morphology (e.g. ^Data) and flags imply
    // morphology (IsUnique), we often treat the type string's sigil as the
    // representation of that flag and strip it? Or if flags are set, we PREPEND
    // morphology. The legacy code checked if baseType started with sigils.

    if (baseType.size() > 1 && (baseType[0] == '^' || baseType[0] == '~' ||
                                baseType[0] == '*' || baseType[0] == '&')) {
      if (morph.empty()) {
        morph = std::string(1, baseType[0]);
      }
      // We strip the sigil from base type if we are extracting it to morph, OR
      // if morph is already set (redundancy)
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
      llvm::errs() << "DEBUG: checkVariableDecl NO_PERSIST Var=" << Var->Name
                   << " Morph=" << morph << " LastSrc=" << m_LastBorrowSource
                   << "\n";
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
    // Stage 1: Resolve Types using the new Type system
    auto initType = checkExpr(Destruct->Init.get());
    auto declType = toka::Type::fromString(Destruct->TypeName);

    // Basic check: declType should match initType
    if (initType->toString() != "unknown" && initType->toString() != "tuple" &&
        !isTypeCompatible(declType, initType)) {
      error(Destruct, "destructuring type mismatch: expected '" +
                          declType->toString() + "', got '" +
                          initType->toString() + "'");
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
  auto T = checkExprImpl(E);
  E->ResolvedType = T;
  return T;
}

std::shared_ptr<toka::Type> Sema::checkExprImpl(Expr *E) {
  if (!E)
    return toka::Type::fromString("void");

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
        error(Rec, "duplicate field '" + f.first + "' in anonymous record");
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
    error(Deref,
          "cannot dereference non-pointer type '" + innerObj->toString() + "'");
    return toka::Type::fromString("unknown");
  } else if (auto *Unary = dynamic_cast<UnaryExpr *>(E)) {
    return checkUnaryExpr(Unary);
  } else if (auto *Str = dynamic_cast<StringExpr *>(E)) {
    return toka::Type::fromString("str");
  } else if (auto *ve = dynamic_cast<VariableExpr *>(E)) {
    SymbolInfo Info;
    if (!CurrentScope->lookup(ve->Name, Info)) {
      if (ShapeMap.count(ve->Name) || TypeAliasMap.count(ve->Name)) {
        return toka::Type::fromString(ve->Name);
      }
      error(ve, "use of undeclared identifier '" + ve->Name + "'");
      return toka::Type::fromString("unknown");
    }
    if (Info.Moved) {
      error(ve, "use of moved value: '" + ve->Name + "'");
    }
    if (Info.IsMutablyBorrowed) {
      error(ve, "cannot access '" + ve->Name +
                    "' while it is mutably borrowed (Rule 406)");
    }
    return Info.TypeObj;
  } else if (auto *Null = dynamic_cast<NullExpr *>(E)) {
    return toka::Type::fromString("nullptr");
  } else if (auto *None = dynamic_cast<NoneExpr *>(E)) {
    return toka::Type::fromString("none");
  } else if (auto *Bin = dynamic_cast<BinaryExpr *>(E)) { // Legacy Dispatch
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
      m_ControlFlowStack.push_back({"", "void", false, isReceiver});
      checkStmt(ie->Else.get());
      elseType = m_ControlFlowStack.back().ExpectedType;
      m_ControlFlowStack.pop_back();
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
        error(New, "new expression type mismatch: expected '" + New->Type +
                       "', got '" + InitType + "'");
      }
    }
    // 'new' usually returns a pointer or the type itself depending on
    // context. AST just says Type. The variable decl usually says ^Type.
    return toka::Type::fromString(New->Type);
  } else if (auto *UnsafeE = dynamic_cast<UnsafeExpr *>(E)) {
    bool oldUnsafe = m_InUnsafeContext;
    m_InUnsafeContext = true;
    auto typeObj = checkExpr(UnsafeE->Expression.get());
    std::string type = typeObj->toString();
    m_InUnsafeContext = oldUnsafe;
    return typeObj;
  } else if (auto *AllocE = dynamic_cast<AllocExpr *>(E)) {
    if (!m_InUnsafeContext) {
      error(AllocE, "alloc operation requires unsafe context");
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
              if (CurrentModule && CurrentModule->FileName == TD->FileName) {
                sameModule = true; // Simplified same-file check
              }
              if (Met->FileName != TD->FileName && !sameModule) {
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
          if (CurrentModule && CurrentModule->FileName == FD->FileName) {
            sameModule = true;
          } else if (CurrentModule) {
            // Weak check for same module via paths?
            // For now, strict file check or explicitly public is enough for
            // "private" If we are in std/string and calling String methods,
            // likely ok.
          }

          if (Met->FileName != FD->FileName && !sameModule) {
            error(Met, "method '" + Met->Method + "' is private to '" +
                           FD->FileName + "'");
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
      if (!SD->IsPub && SD->FileName != Init->FileName) {
        // Relaxed privacy check: allow calls within the same module,
        // regardless of file.
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
      std::set<std::string> providedFields;
      for (const auto &pair : Init->Members) {
        if (providedFields.count(pair.first)) {
          error(Init, "Duplicate field '" + pair.first +
                          "' in struct initialization");
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
          error(Init, "Shape '" + Init->ShapeName + "' has no field named '" +
                          pair.first + "'");
          continue; // Continue to find more errors
        }

        std::shared_ptr<toka::Type> exprTypeObj = checkExpr(pair.second.get());
        std::string exprType = exprTypeObj->toString();
        if (!isTypeCompatible(expectedType, exprType)) {
          error(Init, "Field '" + pair.first + "' expects type '" +
                          expectedType + "', got '" + exprType + "'");
        }
      }

      // Check for missing fields
      for (const auto &defField : SD->Members) {
        // If field is NOT in providedFields
        if (!providedFields.count(defField.Name)) {
          error(Init, "Missing field '" + defField.Name +
                          "' in initialization of '" + Init->ShapeName + "'");
        }
      }
    } else {
      error(Init, "unknown struct '" + Init->ShapeName + "'");
    }
    return toka::Type::fromString(Init->ShapeName);
  } else if (auto *Memb = dynamic_cast<MemberExpr *>(E)) {
    auto objTypeObj = checkExpr(Memb->Object.get());
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
      error(Memb, "cannot access member of nullable '" + ObjTypeFull +
                      "' without 'is' check (Rule 408)");
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
          if (Memb->FileName != SD->FileName) {
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
                    if (Memb->FileName.find(entry.TargetPath) !=
                        std::string::npos) {
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
    }

    return toka::Type::fromString(resultType);
  } else if (auto *Cast = dynamic_cast<CastExpr *>(E)) {
    checkExpr(Cast->Expression.get());
    return toka::Type::fromString(Cast->TargetType);
  }

  return toka::Type::fromString("unknown");
}

void Sema::analyzeShapes(Module &M) {
  // Pass 2: Resolve Member Types (The "Filling" Phase)
  // This must happen after registerGlobals (Pass 1) so that all Shape names are
  // known.
  for (auto &S : M.Shapes) {
    // We only resolve members for Struct, Tuple, Union (Not Enum variants
    // purely yet? Enums have members too) Actually ShapeMember is used for all.
    for (auto &member : S->Members) {
      if (member.ResolvedType)
        continue; // Already resolved?

      // 1. Construct Morphology Prefix
      std::string prefix = "";
      if (member.IsShared)
        prefix += "~";
      else if (member.IsUnique)
        prefix += "^";
      else if (member.IsReference)
        prefix += "&";
      else if (member.HasPointer)
        prefix += "*";

      // 2. Construct Full Type String
      // Note: member.Type is the raw string from parser (e.g. "Node" or "i32")
      std::string fullTypeStr = prefix + member.Type;

      // 3. Resolve to Canonical Name (handles imports, aliases)
      // We use the full string so resolveType can handle the modifiers if it
      // needs to, but usually resolveType expects the base name if we passed
      // bools. However, type strings like "^Node" are valid for resolveType
      // lookup if "Node" is a Shape. Let's use the standard resolveType(string)
      // which handles everything.
      std::string resolvedName = resolveType(fullTypeStr);

      // 4. Create Type Object
      member.ResolvedType = toka::Type::fromString(resolvedName);

      // 5. Basic Validation (Optional but good)
      if (member.ResolvedType->isUnknown()) {
        // Maybe just log, or soft error?
        // analyzeShapes often runs before full function checking, so hard error
        // here is critical. But let's stick to just populating for now. Errors
        // catch later in ComputeProperties or CodeGen? Update: Implementation
        // Plan said "Validate". Let's rely on standard flow. If it's unknown,
        // CodeGen might crash or error. Better to error here if possible, but
        // let's keep it simple first as requested.
      }
    }
  }

  m_ShapeProps.clear();

  // First pass: Compute properties for all shapes
  for (auto &S : M.Shapes) {
    if (m_ShapeProps[S->Name].Status != ShapeAnalysisStatus::Analyzed) {
      computeShapeProperties(S->Name, M);
    }
  }

  // Second pass: Enforce Rules
  for (auto &S : M.Shapes) {
    auto &props = m_ShapeProps[S->Name];

    // Check if Shape has explicit drop
    bool hasExplicitDrop = false;
    // Look in Impl blocks for "drop"
    for (auto &I : M.Impls) {
      if (I->TypeName == S->Name) {
        for (auto &M : I->Methods) {
          if (M->Name == "drop") {
            hasExplicitDrop = true;
            break;
          }
        }
      }
      if (hasExplicitDrop)
        break;
    }

    if (props.HasRawPtr && !hasExplicitDrop) {
      error(S.get(), "Shape '" + S->Name +
                         "' contains raw pointers via members but does not "
                         "implement 'drop'. This is unsafe.");
    }

    if (props.HasDrop && !hasExplicitDrop) {
      error(S.get(),
            "Shape '" + S->Name +
                "' contains resources via members that require dropping, but "
                "does not implement 'drop'. Please implement 'drop' (can be "
                "empty) to confirm ownership.");
    }
  }
}

void Sema::computeShapeProperties(const std::string &shapeName, Module &M) {
  auto &props = m_ShapeProps[shapeName];
  if (props.Status == ShapeAnalysisStatus::Visiting)
    return; // Cycle
  if (props.Status == ShapeAnalysisStatus::Analyzed)
    return;

  props.Status = ShapeAnalysisStatus::Visiting;

  // Find Shape Decl
  const ShapeDecl *S = nullptr;
  if (ShapeMap.count(shapeName))
    S = ShapeMap[shapeName];
  // Also check ModuleScope if using full path? Assume simplified for now or
  // look in M.Shapes
  if (!S) {
    for (auto &sh : M.Shapes)
      if (sh->Name == shapeName) {
        S = sh.get();
        break;
      }
  }

  if (S) {
    for (auto &member : S->Members) {
      // Check member type
      std::string type = member.Type;
      // Primitive checks
      if (member.HasPointer) { // Raw pointer syntax *T in AST usually
                               // Wait, AST member has flags.
                               // If member.Type is bare, rely on flags?
                               // In Parser, *T -> HasPointer=true.
      }

      // We need to parse strict morphology from Type string or Member flags
      // Member flags: HasPointer, IsUnique, IsShared

      if (member.HasPointer) { // *T
        props.HasRawPtr = true;
      }

      if (member.IsUnique || member.IsShared) { // ^T or ~T
        props.HasDrop = true;                   // Smart pointers have drop
      }

      // If it's a value type (member.Type), recurse
      if (!member.HasPointer && !member.IsUnique && !member.IsShared &&
          !member.IsReference) {
        // It's a value type T. Check if T is a Shape.
        std::string baseType = member.Type;
        if (ShapeMap.count(baseType)) {
          computeShapeProperties(baseType, M);
          auto &subProps = m_ShapeProps[baseType];
          if (subProps.HasRawPtr)
            props.HasRawPtr = true;
          if (subProps.HasDrop)
            props.HasDrop = true;
        }

        // Also check if type T has 'drop' method itself (encap) even if not
        // a Shape recursion? This is covered by "if T has drop method, then
        // HasDrop=true". We check: does T have a "drop" method? Iterate
        // Impls again?
        bool memberTypeHasExplicitDrop = false;
        for (auto &I : M.Impls) {
          if (I->TypeName == baseType) {
            for (auto &M : I->Methods) {
              if (M->Name == "drop") {
                memberTypeHasExplicitDrop = true;
                break;
              }
            }
          }
        }
        if (memberTypeHasExplicitDrop)
          props.HasDrop = true;
      }
    }
  }

  props.Status = ShapeAnalysisStatus::Analyzed;
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
          bool wantMutable = Var->IsMutable;
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
          bool innerWritable = Info->IsMutable() || Var->IsMutable;
          innerType =
              innerType->withAttributes(innerWritable, innerType->IsNullable);
          auto refType = std::make_shared<toka::ReferenceType>(innerType);
          return refType;
        } else if (Unary->Op == TokenType::Caret) {
          if (Info->IsMutablyBorrowed || Info->ImmutableBorrowCount > 0) {
            error(Unary,
                  "cannot move '" + Var->Name + "' while it is borrowed");
          }
          // Move Semantics: Return value is the variable's value
          // (UniquePointerType), not wrapped again. If we are moving a
          // ^Data, the result expression type is ^Data.
          return rhsType;
        } else if (Unary->Op == TokenType::Tilde) {
          if (rhsType->isSharedPtr()) {
            return rhsType; // Idempotent
          }
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
      auto unq = std::make_shared<toka::UniquePointerType>(rhsType);
      unq->IsNullable = Unary->HasNull;
      unq->IsWritable = Unary->IsRebindable;
      return unq;
    }
    if (Unary->Op == TokenType::Tilde) {
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
  return rhsType;
}

// Stage 5: Object-Oriented Binary Expression Check
std::shared_ptr<toka::Type> Sema::checkBinaryExpr(BinaryExpr *Bin) {
  // 1. Resolve Operands using New API
  auto lhsType = checkExpr(Bin->LHS.get());
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
  }

  bool isRefAssign = false;

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
    if (dynamic_cast<NewExpr *>(Bin->RHS.get())) {
      if (lhsType->isUniquePtr() || lhsType->isSharedPtr()) {
        auto inner = lhsType->getPointeeType();
        if (inner && isTypeCompatible(inner, rhsType)) {
          isSmartNew = true;
        }
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
        if (InfoPtr->IsMutable() || Var->IsMutable)
          isLHSWritable = true;
      }
    } else if (auto *Un = dynamic_cast<UnaryExpr *>(Traverse)) {
      if (Un->Op == TokenType::Star || Un->Op == TokenType::Caret ||
          Un->Op == TokenType::Tilde) {
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

    if (!isRefAssign && !isSmartNew &&
        !isTypeCompatible(lhsCompatType, rhsType) && LHS != "unknown" &&
        RHS != "unknown") {
      error(Bin, "assignment type mismatch: cannot assign '" + RHS + "' to '" +
                     LHS + "'");
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
      if (target->Functions.count(FuncName))
        Fn = target->Functions[FuncName];
      else if (target->Externs.count(FuncName))
        Ext = target->Externs[FuncName];
      else if (target->Shapes.count(FuncName))
        Sh = target->Shapes[FuncName];
    } else {
      error(Call, "Module '" + ModName + "' not found or not imported");
      return toka::Type::fromString("unknown");
    }
  } else {
    // Local Scope Lookup (Shadowing Global)
    // Usually Functions are Global. But maybe local closure in future?
    // For now, look in ModuleMap
    // Current Module Context?
    // Legacy logic looked in GlobalFunctions and ExternMap directly if
    // local lookup failed? Actually legacy checks GlobalFunctions list.

    // Let's iterate GlobalFunctions
    for (auto *GF : GlobalFunctions) {
      if (GF->Name == CallName) {
        Fn = GF;
        break;
      }
    }
    if (!Fn) {
      // Check ExternMap
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
    if (CallName != "str" && CallName != "unknown") // checkExprStr fallbacks
      error(Call,
            "use of undeclared function or type (NEW) '" + CallName + "'");
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

        // Detect Named Arg: Field = Value
        if (auto *bin = dynamic_cast<BinaryExpr *>(arg.get())) {
          if (bin->Op == "=") {
            if (auto *var = dynamic_cast<VariableExpr *>(bin->LHS.get())) {
              fieldName = var->Name;
              valExpr = bin->RHS.get();
              isNamed = true;
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
