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
#include "toka/DiagnosticEngine.h"
#include "toka/SourceManager.h"
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
  llvm::errs() << "DEBUG: checkModule Enters. Functions: " << M.Functions.size()
               << "\n";
  enterScope();       // Module-level global scope
  CurrentModule = &M; // Set context
  // 1. Register all globals (Functions, Structs, etc.)
  registerGlobals(M);
  // 2. Shape Analysis Pass (Safety Enforcement)
  analyzeShapes(M);
  checkShapeSovereignty();

  // 2b. Check function bodies (reordered)
  llvm::errs() << "DEBUG: checkModule Checking Functions...\n";
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

static SourceLocation getLoc(ASTNode *Node) { return Node->Loc; }

void Sema::error(ASTNode *Node, const std::string &Msg) {
  HasError = true;
  // Fallback for not-yet-migrated errors
  DiagnosticEngine::report(getLoc(Node), DiagID::ERR_GENERIC_SEMA, Msg);
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
  std::string fileName =
      DiagnosticEngine::SrcMgr->getFullSourceLoc(M.Loc).FileName;
  ModuleScope &ms = ModuleMap[fileName];
  ms.Name = fileName;
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
    // [NEW] Define locally in scope for explicit lookup
    CurrentScope->define(Fn->Name, {toka::Type::fromString("fn")});
  }
  for (auto &Ext : M.Externs) {
    ms.Externs[Ext->Name] = Ext.get();
    ExternMap[Ext->Name] = Ext.get();
    // [NEW] Define locally in scope
    CurrentScope->define(Ext->Name, {toka::Type::fromString("extern")});
  }
  for (auto &St : M.Shapes) {
    ms.Shapes[St->Name] = St.get();
    ShapeMap[St->Name] = St.get();
    // [NEW] Shapes usually resolved via ShapeMap, but define in scope for
    // consistency if needed.
    CurrentScope->define(St->Name, {toka::Type::fromString(St->Name)});
  }
  for (auto &Alias : M.TypeAliases) {
    ms.TypeAliases[Alias->Name] = {Alias->TargetType, Alias->IsStrong};
    TypeAliasMap[Alias->Name] = {Alias->TargetType, Alias->IsStrong};
    // [NEW] Define locally in scope
    CurrentScope->define(Alias->Name, {toka::Type::fromString(Alias->Name)});
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
    // [NEW] Define locally in scope
    CurrentScope->define(Trait->Name, {toka::Type::fromString(Trait->Name)});
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
      // [NEW] Define local global in scope
      std::string fullT = synthesizePhysicalType(*v);
      CurrentScope->define(v->Name, {toka::Type::fromString(fullT)});
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
      DiagnosticEngine::report(getLoc(Imp.get()), DiagID::ERR_MODULE_NOT_FOUND,
                               Imp->PhysicalPath);
      HasError = true;
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
            DiagnosticEngine::report(getLoc(Imp.get()),
                                     DiagID::ERR_SYMBOL_NOT_FOUND, item.Symbol,
                                     Imp->PhysicalPath);
            HasError = true;
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
                DiagnosticEngine::report(getLoc(ImplMethod),
                                         DiagID::ERR_SIGNATURE_MISMATCH,
                                         Method->Name, traitVis, implVis);
                HasError = true;
              }
            }
            continue;
          }
          if (Method->Body) {
            // Trait provides a default implementation
            MethodMap[Impl->TypeName][Method->Name] = Method->ReturnType;
            MethodDecls[Impl->TypeName][Method->Name] = Method.get();
          } else {
            DiagnosticEngine::report(getLoc(Impl.get()),
                                     DiagID::ERR_MISSING_IMPL, Method->Name,
                                     Impl->TraitName);
            HasError = true;
          }
        }
      } else {
        DiagnosticEngine::report(getLoc(Impl.get()),
                                 DiagID::ERR_TRAIT_NOT_FOUND, Impl->TraitName,
                                 Impl->TypeName);
        HasError = true;
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

void Sema::checkFunction(FunctionDecl *Fn) {
  llvm::errs() << "DEBUG: Checking function: " << Fn->Name << "\n";
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
    if (Arg.IsValueMutable)
      fullType += "#";
    if (Arg.IsPointerNullable || Arg.IsValueNullable)
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
        DiagnosticEngine::report(getLoc(Fn), DiagID::ERR_CONTROL_REACHES_END,
                                 Fn->Name);
        HasError = true;
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
          DiagnosticEngine::report(getLoc(decl), DiagID::ERR_SHAPE_NO_DROP,
                                   name);
          HasError = true;
        }
      }
    }
  }
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

      // [New] Populate ShapeDecl pointer in ShapeType
      std::shared_ptr<toka::Type> inner = member.ResolvedType;
      // Recursively unwrap
      while (inner->isPointer() || inner->isArray()) {
        if (auto p = std::dynamic_pointer_cast<toka::PointerType>(inner))
          inner = p->PointeeType;
        else if (auto a = std::dynamic_pointer_cast<toka::ArrayType>(inner))
          inner = a->ElementType;
        else
          break;
      }
      if (auto *st = dynamic_cast<toka::ShapeType *>(inner.get())) {
        if (ShapeMap.count(st->Name)) {
          st->resolve(ShapeMap[st->Name]);
        }
      }

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
      DiagnosticEngine::report(getLoc(S.get()), DiagID::ERR_UNSAFE_RAW_PTR,
                               S->Name);
      HasError = true;
    }

    if (props.HasDrop && !hasExplicitDrop) {
      DiagnosticEngine::report(getLoc(S.get()), DiagID::ERR_UNSAFE_RESOURCE,
                               S->Name);
      HasError = true;
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

} // namespace toka
