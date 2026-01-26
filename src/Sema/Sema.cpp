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
#include <functional> // [NEW] Added for std::function
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

  // 2c. Check Impl blocks (NEW: Proper Self Injection)
  for (auto &Impl : M.Impls) {
    if (!Impl->GenericParams.empty())
      continue; // Skip templates, they are checked upon instantiation
    checkImpl(Impl.get());
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
      if (isMutable) {
        sourcePtr->IsMutablyBorrowed = false;
      } else {
        sourcePtr->ImmutableBorrowCount--;
      }
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
    if (!St->GenericParams.empty()) {
      // [NEW] Generic Template Registration
      // Do NOT generate TypeLayout or simple ShapeMap entry yet.
      // We might need a separate GenericShapeMap or flag it.
      // For now, put in ShapeMap but the key distinction is St->GenericParams
      // is not empty. The Type system will see "Box" in ShapeMap, but when it
      // resolves, it sees GenericParams.
      ms.Shapes[St->Name] = St.get();
      ShapeMap[St->Name] = St.get();
    } else {
      ms.Shapes[St->Name] = St.get();
      ShapeMap[St->Name] = St.get();
      // [NEW] Shapes usually resolved via ShapeMap, but define in scope for
      // consistency if needed.
      CurrentScope->define(St->Name, {toka::Type::fromString(St->Name)});
    }
  }
  for (auto &Alias : M.TypeAliases) {
    ms.TypeAliases[Alias->Name] = {Alias->TargetType, Alias->IsStrong,
                                   Alias->GenericParams};
    TypeAliasMap[Alias->Name] = {Alias->TargetType, Alias->IsStrong,
                                 Alias->GenericParams};
    std::cerr << "Registered Alias: " << Alias->Name
              << " Strong=" << Alias->IsStrong << "\n";
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
      SymbolInfo globalInfo;
      globalInfo.TypeObj = toka::Type::fromString(fullT);
      globalInfo.IsRebindable = v->IsRebindable;
      CurrentScope->define(v->Name, globalInfo);
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

            SymbolInfo globalInfo;
            globalInfo.TypeObj = toka::Type::fromString(fullType);
            globalInfo.IsRebindable = v->IsRebindable;
            CurrentScope->define(item.Alias.empty() ? name : item.Alias,
                                 globalInfo);
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

            SymbolInfo globalInfo;
            globalInfo.TypeObj = toka::Type::fromString(fullType);
            globalInfo.IsRebindable = v->IsRebindable;
            CurrentScope->define(name, globalInfo);
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
    // [NEW] Generic Impl Registration (Lazy)
    // If impl has generic params, OR it points to a generic shape, it's a
    // template.
    bool typeIsGeneric = false;
    std::string baseShapeName = Impl->TypeName;
    size_t lt_check = baseShapeName.find('<');
    if (lt_check != std::string::npos)
      baseShapeName = baseShapeName.substr(0, lt_check);

    if (ShapeMap.count(baseShapeName) &&
        !ShapeMap[baseShapeName]->GenericParams.empty()) {
      typeIsGeneric = true;
    }

    if (!Impl->GenericParams.empty() || typeIsGeneric) {
      if (Impl->GenericParams.empty()) {
        // [Check] Validate that we aren't using undefined types as generic args
        // e.g. impl Vec<T> {} -> Error "T is undefined"
        auto typeObj = toka::Type::fromString(Impl->TypeName);
        if (auto st = std::dynamic_pointer_cast<ShapeType>(typeObj)) {
          for (auto &Arg : st->GenericArgs) {
            // Peel pointer/ref to get base name
            std::string name = Arg->getSoulName();
            // Check if known
            bool known = false;
            if (toka::Type::fromString(name)->typeKind == toka::Type::Primitive)
              known = true;
            else if (ShapeMap.count(name))
              known = true;
            else if (TypeAliasMap.count(name))
              known = true;
            else if (ExternMap.count(name))
              known = true; // External?
            else {
              // Consult Sema lookup (CurrentScope)
              // Since we are in registerGlobals, imports might be in scope or
              // Extern declarations? But usually simple generic params are just
              // T, U, etc.
              SymbolInfo info;
              if (CurrentScope && CurrentScope->lookup(name, info))
                known = true;
            }

            if (!known) {
              // Heuristic: If name is short (1-2 chars) or clearly looks like a
              // placeholder
              DiagnosticEngine::report(Impl->Loc, DiagID::ERR_UNDEFINED_TYPE,
                                       name);
              DiagnosticEngine::report(Impl->Loc,
                                       DiagID::NOTE_GENERIC_IMPL_HINT, name);
              HasError = true;
            }
          }
        }
      }

      std::string baseName = Impl->TypeName;
      size_t lt = baseName.find('<');
      if (lt != std::string::npos)
        baseName = baseName.substr(0, lt);
      GenericImplMap[baseName].push_back(Impl.get());
      continue; // Skip standard registration for templates
    }

    if (Impl->TraitName == "encap") {
      EncapMap[Impl->TypeName] = Impl->EncapEntries;
      // removed continue to allow method registration (hybrid trait)
    }
    registerImpl(Impl.get());
  }
}

void Sema::registerImpl(ImplDecl *Impl) {
  // [New] Resolve 'Self' in Method Signatures for External Callers
  // We must replace 'Self' with the concrete (or generic) TypeName
  // so that callers (like main) typically don't fail to resolve 'Self'.
  std::string selfTy = Impl->TypeName;
  for (auto &Method : Impl->Methods) {
    if (Method->ReturnType == "Self") {
      Method->ReturnType = selfTy;
    }
    for (auto &Arg : Method->Args) {
      if (Arg.Type == "Self") {
        Arg.Type = selfTy;
      }
    }
  }

  std::set<std::string> implemented;
  std::string resolvedTypeName = resolveType(Impl->TypeName);
  for (auto &Method : Impl->Methods) {
    MethodMap[resolvedTypeName][Method->Name] = Method->ReturnType;
    MethodDecls[resolvedTypeName][Method->Name] = Method.get();
    implemented.insert(Method->Name);
  }

  // Populate ImplMap
  if (!Impl->TraitName.empty()) {
    std::string implKey = resolvedTypeName + "@" + Impl->TraitName;
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
          FunctionDecl *ImplMethod = nullptr;
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
          MethodMap[resolvedTypeName][Method->Name] = Method->ReturnType;
          MethodDecls[resolvedTypeName][Method->Name] = Method.get();
        } else {
          DiagnosticEngine::report(getLoc(Impl), DiagID::ERR_MISSING_IMPL,
                                   Method->Name, Impl->TraitName);
          HasError = true;
        }
      }
    } else {
      DiagnosticEngine::report(getLoc(Impl), DiagID::ERR_TRAIT_NOT_FOUND,
                               Impl->TraitName, Impl->TypeName);
      HasError = true;
    }
  }

  // [Toka] Resource Management: Mark type as having drop if @encap is
  // implemented
  if (Impl->TraitName == "encap") {
    if (implemented.count("drop")) {
      m_ShapeProps[resolvedTypeName].HasDrop = true;
      // [Single Source of Truth] Store the authoritative mangled name
      if (ShapeMap.count(resolvedTypeName)) {
        ShapeMap[resolvedTypeName]->MangledDestructorName =
            "encap_" + resolvedTypeName + "_drop";
      }
    }
  }
}

void Sema::checkFunction(FunctionDecl *Fn) {
  // [NEW] Skip Generic Templates
  // We cannot check them until they are instantiated with concrete types.
  if (!Fn->GenericParams.empty())
    return;

  std::string savedRet =
      CurrentFunctionReturnType; // [FIX] Save state for recursion
  FunctionDecl *savedFn = CurrentFunction;
  CurrentFunction = Fn;
  CurrentFunctionReturnType = Fn->ReturnType;

  // [New] Annotated AST: Resolve Return Type Object
  if (Fn->ReturnType != "void") {
    std::string resolvedRetStr = resolveType(Fn->ReturnType);
    Fn->ResolvedReturnType = toka::Type::fromString(resolvedRetStr);
  } else {
    Fn->ResolvedReturnType = toka::Type::fromString("void");
  }

  enterScope(); // Function scope

  // Register arguments
  for (auto &Arg : Fn->Args) {
    SymbolInfo Info;
    std::string fullType = "";
    // 1. Morphology Sigil (Constitutional 1.3 - Leading)
    if (Arg.IsUnique)
      fullType += "^";
    else if (Arg.IsShared)
      fullType += "~";
    else if (Arg.IsReference)
      fullType += "&";
    else if (Arg.HasPointer)
      fullType += "*";

    // 2. Identity Attributes (Prefix Zone)
    if (Arg.IsPointerNullable)
      fullType += "?";
    if (Arg.IsRebindable)
      fullType += "#";

    std::string baseType = toka::Type::stripMorphology(Arg.Type);
    fullType += baseType;

    // 3. Soul Attributes (Suffix Zone)
    if (Arg.IsValueNullable)
      fullType += "?";
    if (Arg.IsValueMutable)
      fullType += "#";

    // [New] Annotated AST: Use resolveType (string version) to handle
    // aliases/Self, then parse
    std::string resolvedStr = resolveType(fullType);
    Info.TypeObj = toka::Type::fromString(resolvedStr);

    // Assign to AST Node for CodeGen
    Arg.ResolvedType = Info.TypeObj;
    Info.IsRebindable = Arg.IsRebindable;

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
  CurrentFunctionReturnType = savedRet; // [FIX] Restore state
  CurrentFunction = savedFn;
}

void Sema::checkImpl(ImplDecl *Impl) {
  // [NEW] Skip Generic Templates until Instantiation
  // (Assuming Impl<T> is handled similarly to Functions, but for now we focus
  // on non-generic Impl or instantiated ones) Actually, ImplDecl doesn't have
  // GenericParams on itself usually? It refers to a Generic Type. We should
  // check if the TargetType is generic? For "impl<T> Box<T>", the ImplDecl
  // has "Box<T>" as TypeName. We need to resolve it.

  enterScope(); // Helper Scope for Self Injection

  // 1. Resolve Target Type (The "Self")
  std::shared_ptr<toka::Type> SelfType = nullptr;

  // Resolve the type name. Note: resolveType handles "Box<T>" if
  // instantiated, or "Box" if we are inside a generic context (which we
  // aren't yet for global impls). For now, let's assume we are checking
  // concrete impls OR we are just setting up the scope for "Self" to alias to
  // "TypeName".

  // Create a Type Object for the Impl's Target
  // We use Type::fromString but we might want to resolve aliases.
  SelfType = toka::Type::fromString(Impl->TypeName);

  // If we can resolve it deeper (e.g. valid shape), do so.
  SelfType = resolveType(SelfType);

  // 2. Define "Self" in the Scope
  if (SelfType) {
    SymbolInfo Sym;
    // Sym.Name = "Self"; // SymbolInfo doesn't store Name, Key does.
    Sym.IsTypeAlias = true;
    Sym.TypeObj = SelfType;
    CurrentScope->define("Self", Sym);
  } else {
    // Should we error?
  }

  // 3. Check all methods
  for (auto &Method : Impl->Methods) {
    // Methods inside Impl are FunctionDecls.
    checkFunction(Method.get());
  }

  exitScope();
}

void Sema::checkShapeSovereignty() {
  for (auto const &[name, decl] : ShapeMap) {
    if (!decl->GenericParams.empty())
      continue;

    if (decl->Kind == ShapeKind::Struct) {
      bool needsDrop = false;

      // Check if Shape manages resources
      for (auto &memb : decl->Members) {
        // 1. Raw Pointers (*T) - DO NOT force drop (views/unsafe aliases)
        // if (memb.HasPointer) { ... }
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
  // This must happen after registerGlobals (Pass 1) so that all Shape names
  // are known.
  for (auto &S : M.Shapes) {
    // [NEW] Skip analysis for Generic Templates. They are analyzed only upon
    // Instantiation.
    if (!S->GenericParams.empty())
      continue;

    // We only resolve members for Struct, Tuple, Union (Not Enum variants
    // purely yet? Enums have members too) Actually ShapeMember is used for
    // all.
    for (auto &member : S->Members) {
      if (member.ResolvedType)
        continue; // Already resolved?

      // Use member.Type directly as it already contains morphology sigils from
      // the parser
      std::string fullTypeStr = member.Type;

      // 3. Resolve to Canonical Name (handles imports, aliases)
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
        // ... (keep existing comments if any, or just ignore unknown)
      }

      // [Rule] Union Type Blacklist: Check Underlying Physics
      if (S->Kind == ShapeKind::Union) {
        auto underlying = getDeepestUnderlyingType(member.ResolvedType);
        bool invalid = false;
        std::string reason = "";

        if (underlying->isBoolean() || underlying->toString() == "bool") {
          invalid = true;
          reason = "bool";
        } else if (auto st =
                       std::dynamic_pointer_cast<toka::ShapeType>(underlying)) {
          // Check if it's a Strict Enum
          if (ShapeMap.count(st->Name)) {
            ShapeDecl *SD = ShapeMap[st->Name];
            if (SD->Kind == ShapeKind::Enum &&
                !SD->IsPacked) { // Packed is C-enum
              invalid = true;
              reason = "strict enum";
            }
          }
        }

        if (invalid) {
          DiagnosticEngine::report(
              getLoc(S.get()), DiagID::ERR_UNION_INVALID_MEMBER, member.Name,
              member.Type /* Original Name */, reason);
          HasError = true;
        }
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

    // Relaxed: Raw pointers don't force 'drop' (views/unsafe aliases)
    /*
    if (props.HasRawPtr && !hasExplicitDrop) {
      DiagnosticEngine::report(getLoc(S.get()), DiagID::ERR_UNSAFE_RAW_PTR,
                               S->Name);
      HasError = true;
    }
    */

    if (props.HasManualDrop && !hasExplicitDrop) {
      DiagnosticEngine::report(getLoc(S.get()), DiagID::ERR_UNSAFE_RESOURCE,
                               S->Name);
      HasError = true;
    }

    // [Rule] Union Safety: No Resource Types (HasDrop)
    if (S->Kind == ShapeKind::Union) {
      for (auto &memb : S->Members) {
        bool isResource = false;
        // 1. Smart Pointers
        if (memb.IsUnique || memb.IsShared) {
          isResource = true;
        }
        // 2. Value types that have Drop
        //    (Recursive structures, or types with Drop impl)
        //    We can check the computed props of the member's type if it is a
        //    Shape
        else if (!memb.HasPointer && !memb.IsReference) {
          std::string baseType = toka::Type::stripMorphology(memb.Type);
          // Unwrap arrays first?
          // Actually computeShapeProperties handles recursion if we called it
          // on baseType. But wait, computeShapeProperties is called on
          // Shapes. If 'memb.Type' is [T; 2], baseType might be [T; 2] or we
          // need to handle it. Let's use the resolved type if possible, or
          // simple recursion. 'memb.ResolvedType' should be available now.

          if (memb.ResolvedType) {
            std::shared_ptr<toka::Type> T = memb.ResolvedType;
            // Unwrap Arrays
            while (T->isArray()) {
              T = std::static_pointer_cast<toka::ArrayType>(T)->ElementType;
            }

            if (T->isSmartPointer()) {
              isResource = true;
            } else if (T->isShape()) {
              std::string shapeName =
                  std::static_pointer_cast<toka::ShapeType>(T)->Name;
              if (m_ShapeProps.count(shapeName) &&
                  m_ShapeProps[shapeName].HasDrop) {
                isResource = true;
              }
            }
          }
        }

        if (isResource) {
          DiagnosticEngine::report(getLoc(S.get()),
                                   DiagID::ERR_UNION_RESOURCE_TYPE, memb.Name,
                                   memb.Type);
          DiagnosticEngine::report(getLoc(S.get()),
                                   DiagID::NOTE_UNION_RESOURCE_TIP, memb.Type);
          HasError = true;
        }
      }
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

      std::string typeStr = member.Type;
      // Handle flag-based or string-based sigils
      if (member.IsUnique || member.IsShared || typeStr.rfind("^", 0) == 0 ||
          typeStr.rfind("~", 0) == 0) {
        props.HasDrop = true;
      }

      // Check if it's an array string "[T; N]"
      // Note: This is a hacky parse, but consistent with current AST
      if (typeStr.size() > 0 && typeStr.front() == '[') {
        size_t semi = typeStr.rfind(';');
        if (semi != std::string::npos) {
          std::string inner = typeStr.substr(1, semi - 1);
          // Recurse on inner type
          if (inner.rfind("^", 0) == 0 || inner.rfind("~", 0) == 0) {
            props.HasDrop = true;
          } else {
            if (ShapeMap.count(inner)) {
              computeShapeProperties(inner, M);
              if (m_ShapeProps[inner].HasDrop)
                props.HasDrop = true;
              if (m_ShapeProps[inner].HasRawPtr)
                props.HasRawPtr = true;
            }
            // Check explicit drop on inner type (e.g. valid struct inside
            // array)
            bool innerHasDrop = false;
            for (auto &I : M.Impls) {
              if (I->TypeName == inner) {
                for (auto &M : I->Methods) {
                  if (M->Name == "drop") {
                    innerHasDrop = true;
                    break;
                  }
                }
              }
            }
            if (innerHasDrop)
              props.HasDrop = true;
          }
        }
      } else if (!member.HasPointer && !member.IsUnique && !member.IsShared &&
                 !member.IsReference && typeStr.rfind("^", 0) != 0 &&
                 typeStr.rfind("~", 0) != 0) {
        // It's a value type T. Check if T is a Shape.
        std::string baseType = member.Type;
        if (ShapeMap.count(baseType)) {
          computeShapeProperties(baseType, M);
          auto &subProps = m_ShapeProps[baseType];
          if (subProps.HasRawPtr)
            props.HasRawPtr = true;
          if (subProps.HasDrop)
            props.HasDrop = true;
          if (subProps.HasManualDrop)
            props.HasManualDrop = true;
        }

        // Also check if type T has 'drop' method itself (encap)
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
        if (memberTypeHasExplicitDrop) {
          props.HasDrop = true;
          props.HasManualDrop = true;
        }
      }
    }
  }

  // Check if THIS shape has an explicit drop impl
  for (auto &I : M.Impls) {
    if (I->TypeName == shapeName) {
      for (auto &Met : I->Methods) {
        if (Met->Name == "drop") {
          props.HasDrop = true;
          props.HasManualDrop = true;
          break;
        }
      }
    }
  }

  props.Status = ShapeAnalysisStatus::Analyzed;
}

FunctionDecl *Sema::instantiateGenericFunction(
    FunctionDecl *Template,
    const std::vector<std::shared_ptr<toka::Type>> &Args, CallExpr *CallSite) {

  if (Template->GenericParams.size() != Args.size()) {
    DiagnosticEngine::report(getLoc(CallSite),
                             DiagID::ERR_GENERIC_ARITY_MISMATCH, Template->Name,
                             Template->GenericParams.size(), Args.size());
    HasError = true;
    return nullptr;
  }

  // Magnling: Name_M_Arg1_Arg2
  std::string mangledName = Template->Name + "_M";
  for (auto &Arg : Args) {
    if (!Arg)
      continue;
    std::string argStr = resolveType(Arg)->toString();
    for (char &c : argStr) {
      if (!std::isalnum(c) && c != '_')
        c = '_';
    }
    mangledName += "_" + argStr;
  }

  // Recursion Guard
  static int depth = 0;
  if (depth > 100) {
    DiagnosticEngine::report(
        getLoc(CallSite), DiagID::ERR_GENERIC_RECURSION_LIMIT, Template->Name);
    HasError = true;
    return nullptr;
  }

  // Check Cache
  static std::map<std::string, FunctionDecl *> InstantiationCache;
  if (InstantiationCache.count(mangledName)) {
    return InstantiationCache[mangledName];
  }

  // Instantiate
  depth++;

  // 1. Clone
  auto ClonedNode = Template->clone();
  FunctionDecl *Instance = static_cast<FunctionDecl *>(ClonedNode.release());
  std::unique_ptr<FunctionDecl> InstancePtr(Instance);

  Instance->Name = mangledName;
  Instance->GenericParams.clear(); // Mark as concrete

  // 2. Scope Injection Setup
  enterScope();
  for (size_t i = 0; i < Template->GenericParams.size(); ++i) {
    const auto &GP = Template->GenericParams[i];
    std::string SubstVal = resolveType(Args[i])->toString(); // "10" or "i32"

    if (GP.IsConst) {
      SymbolInfo constInfo;
      // We assume it's a number (usize/integer).
      // We register it as a variable so checkExpr(VariableExpr(N)) works.
      constInfo.TypeObj =
          toka::Type::fromString(GP.Type.empty() ? "usize" : GP.Type);

      // [NEW] Set Const Value for Expression Evaluation
      uint64_t val = 0;
      try {
        val = std::stoull(SubstVal);
      } catch (...) {
      }
      constInfo.HasConstValue = true;
      constInfo.ConstValue = val;

      // NOTE: We don't set IsTypeAlias. Semantically it's a value.
      // But we need CodeGen to see it.
      // Strategy: Inject a synthetic variable declaration at the start of the
      // body.
      CurrentScope->define(GP.Name, constInfo);

      // We will inject `auto N = 10;` into the body later.
    } else {
      SymbolInfo aliasInfo;
      aliasInfo.TypeObj = resolveType(Args[i]);
      aliasInfo.IsTypeAlias = true;
      CurrentScope->define(GP.Name, aliasInfo);
    }
  }

  // [NEW] 2.5 Substitute Generic Types in Signature
  // We must update Arg types and ReturnType so callers see concrete types
  // (e.g. i32 instead of T)
  std::map<std::string, std::string> substMap;
  for (size_t i = 0; i < Template->GenericParams.size(); ++i) {
    substMap[Template->GenericParams[i].Name] =
        resolveType(Args[i])->toString();
  }

  auto applySubst = [&](std::string &s) {
    for (auto const &[K, V] : substMap) {
      size_t pos = 0;
      while ((pos = s.find(K, pos)) != std::string::npos) {
        auto isWordChar = [](char c) { return std::isalnum(c) || c == '_'; };
        bool startOk = (pos == 0) || !isWordChar(s[pos - 1]);
        bool endOk =
            (pos + K.size() == s.size()) || !isWordChar(s[pos + K.size()]);
        if (startOk && endOk) {
          s.replace(pos, K.size(), V);
          pos += V.size();
        } else {
          pos += K.size();
        }
      }
    }
  };

  // Substitute types in signature
  for (auto &Arg : Instance->Args) {
    applySubst(Arg.Type);
    Arg.ResolvedType = nullptr;
  }
  applySubst(Instance->ReturnType);

  // [NEW] Substitute types in Body (Recursive Traversal)
  // We need to traverse Stmt and Expr to find nodes that store type strings:
  // VariableDecl: TypeName
  // NewExpr: Type
  // CastExpr: TargetType
  // AllocExpr: TypeName
  // InitStructExpr: ShapeName (if generic)
  // CallExpr: GenericArgs (vector<string>)
  // MethodCallExpr: N/A (args handled by Expr)
  // AnonymousRecordExpr: AssignedTypeName

  if (Instance->Body) {
    // Define Visitors
    std::function<void(Expr *)> visitExpr;
    std::function<void(Stmt *)> visitStmt;

    visitExpr = [&](Expr *e) {
      if (!e)
        return;
      // Clear ResolvedType on ALL expressions to force
      // re-type-check/resolution
      e->ResolvedType = nullptr;

      if (auto *ne = dynamic_cast<NewExpr *>(e)) {
        applySubst(ne->Type);
        if (ne->Initializer)
          visitExpr(ne->Initializer.get());
      } else if (auto *ae = dynamic_cast<AllocExpr *>(e)) {
        applySubst(ae->TypeName);
        if (ae->Initializer)
          visitExpr(ae->Initializer.get());
        if (ae->ArraySize)
          visitExpr(ae->ArraySize.get());
      } else if (auto *ce = dynamic_cast<CastExpr *>(e)) {
        applySubst(ce->TargetType);
        if (ce->Expression)
          visitExpr(ce->Expression.get());
      } else if (auto *ise = dynamic_cast<InitStructExpr *>(e)) {
        applySubst(ise->ShapeName);
        for (auto &m : ise->Members)
          visitExpr(m.second.get());
      } else if (auto *call = dynamic_cast<CallExpr *>(e)) {
        // Function Name (if it has generics embedded?) - Usually handled by
        // Parser logic putting generics in name? If so, applySubst on Callee.
        applySubst(call->Callee);
        for (auto &s : call->GenericArgs)
          applySubst(s);
        for (auto &arg : call->Args)
          visitExpr(arg.get());
      } else if (auto *mc = dynamic_cast<MethodCallExpr *>(e)) {
        // applySubst(mc->Method); // Method name usually doesn't have type
        // unless it's generic method?
        visitExpr(mc->Object.get());
        for (auto &arg : mc->Args)
          visitExpr(arg.get());
      } else if (auto *are = dynamic_cast<AnonymousRecordExpr *>(e)) {
        applySubst(are->AssignedTypeName);
        for (auto &m : are->Fields)
          if (m.second)
            visitExpr(m.second.get());
      } else if (auto *bin = dynamic_cast<BinaryExpr *>(e)) {
        visitExpr(bin->LHS.get());
        visitExpr(bin->RHS.get());
      } else if (auto *un = dynamic_cast<UnaryExpr *>(e)) {
        visitExpr(un->RHS.get());
      } else if (auto *fe = dynamic_cast<MemberExpr *>(e)) {
        visitExpr(fe->Object.get());
      } else if (auto *idx = dynamic_cast<ArrayIndexExpr *>(e)) {
        visitExpr(idx->Array.get());
        for (auto &i : idx->Indices)
          visitExpr(i.get());
      } else if (auto *me = dynamic_cast<MatchExpr *>(e)) {
        visitExpr(me->Target.get());
        for (auto &arm : me->Arms) {
          if (arm->Guard)
            visitExpr(arm->Guard.get());
          visitStmt(arm->Body.get());
        }
      } else if (auto *ifE = dynamic_cast<IfExpr *>(e)) {
        visitExpr(ifE->Condition.get());
        visitStmt(ifE->Then.get());
        if (ifE->Else)
          visitStmt(ifE->Else.get());
      } else if (auto *le = dynamic_cast<LoopExpr *>(e)) {
        visitStmt(le->Body.get());
      } else if (auto *fore = dynamic_cast<ForExpr *>(e)) {
        visitExpr(fore->Collection.get());
        visitStmt(fore->Body.get());
      } else if (auto *rep = dynamic_cast<RepeatedArrayExpr *>(e)) {
        visitExpr(rep->Value.get());
        visitExpr(rep->Count.get());
        // [FIX] Handle N_ replacement in Count if it was invalid
        if (auto *ve = dynamic_cast<VariableExpr *>(rep->Count.get())) {
          std::string name = Type::stripMorphology(ve->Name);
          if (substMap.count(name)) {
            std::string valStr = substMap[name];
            // Check if it's a number
            try {
              uint64_t val = std::stoull(valStr);
              rep->Count = std::make_unique<NumberExpr>(val);
              rep->Count->Loc = ve->Loc;
            } catch (...) {
            }
          }
        }
      } else if (auto *ve = dynamic_cast<VariableExpr *>(e)) {
        // This is tricky because we can't easily replace 'e' here as it's
        // passed by pointer The caller usually holds 'std::unique_ptr<Expr>&'
        // but we have 'Expr*'. However, RepeatedArrayExpr above handles its
        // specific child case. For general cases (e.g. 'return N'), replacing
        // VariableExpr 'N' with NumberExpr '5' is harder without access to
        // the parent's unique_ptr. BUT: 'instantiateGenericFunction' visitor
        // infrastructure is weak here. Fortunately, for 'return N',
        // 'checkExpr' handles Variable lookup into Scope. The ERROR was
        // specific to RepeatedArrayExpr which demands a Literal/Const. By
        // fixing RepeatedArrayExpr specific logic above, we solve the array
        // size issue. For other cases, Scope Injection (N_ = 5) should
        // suffice.
      }
      // Note: VariableExpr, NumberExpr, etc don't have nested Exprs or Types
      // to substitute
    };

    visitStmt = [&](Stmt *s) {
      if (!s)
        return;
      if (auto *bs = dynamic_cast<BlockStmt *>(s)) {
        for (auto &sub : bs->Statements)
          visitStmt(sub.get());
      } else if (auto *vd = dynamic_cast<VariableDecl *>(s)) {
        applySubst(vd->TypeName);
        vd->ResolvedType = nullptr; // Clear cache
        if (vd->Init)
          visitExpr(vd->Init.get());
      } else if (auto *rs = dynamic_cast<ReturnStmt *>(s)) {
        if (rs->ReturnValue)
          visitExpr(rs->ReturnValue.get());
      } else if (auto *es = dynamic_cast<ExprStmt *>(s)) {
        visitExpr(es->Expression.get());
      } else if (auto *ds = dynamic_cast<DeleteStmt *>(s)) {
        visitExpr(ds->Expression.get());
      } else if (auto *fs = dynamic_cast<FreeStmt *>(s)) {
        visitExpr(fs->Expression.get());
        if (fs->Count)
          visitExpr(fs->Count.get());
      } else if (auto *dd = dynamic_cast<DestructuringDecl *>(s)) {
        applySubst(dd->TypeName);
        if (dd->Init)
          visitExpr(dd->Init.get());
      } else if (auto *us = dynamic_cast<UnsafeStmt *>(s)) {
        visitStmt(us->Statement.get());
      }
    };

    visitStmt(Instance->Body.get());
  }

  // 3. Register in Module
  if (CurrentModule) {
    CurrentModule->Functions.push_back(std::move(InstancePtr));
    Instance = CurrentModule->Functions.back().get();

    std::string fileName =
        DiagnosticEngine::SrcMgr->getFullSourceLoc(CurrentModule->Loc).FileName;
    ModuleMap[fileName].Functions[mangledName] = Instance;

    GlobalFunctions.push_back(Instance);
  } else {
    // Create independent ownership if no module context (shouldn't happen
    // here) For safety, leak it or manage elsewhere. But Sema always has
    // CurrentModule during analysis. If we are called from checkCallExpr,
    // CurrentModule is set.
    InstancePtr.release(); // Leak if no module? No, let's assume CurrentModule.
  }

  // [NEW] Inject Const Generic Variables into Body
  // Removed dirty hack (VariableDecl injection).
  // Const values are now handled by SymbolInfo resolution in CodeGen.

  // 4. Semantic Check (Recursion)
  checkFunction(Instance);

  exitScope();
  depth--;

  InstantiationCache[mangledName] = Instance;
  return Instance;
}

Sema::ModuleScope *Sema::getModule(const std::string &Path) {
  if (ModuleMap.count(Path))
    return &ModuleMap[Path];
  return nullptr;
}

std::string Sema::getModuleName(Module *M) {
  if (!M)
    return "root";
  std::string fullPath =
      DiagnosticEngine::SrcMgr->getFullSourceLoc(M->Loc).FileName;
  size_t lastSlash = fullPath.find_last_of('/');
  std::string name = (lastSlash == std::string::npos)
                         ? fullPath
                         : fullPath.substr(lastSlash + 1);
  size_t dot = name.find_last_of('.');
  if (dot != std::string::npos)
    name = name.substr(0, dot);
  return name;
}

int Sema::getScopeDepth(const std::string &Name) {
  Scope *S = CurrentScope;
  while (S) {
    if (S->Symbols.count(Name))
      return S->Depth;
    S = S->Parent;
  }
  return 0; // Global or not found (Global is 0)
}

bool Sema::checkVisibility(ASTNode *Node, ShapeDecl *SD) {
  if (!SD)
    return true;

  // Visibility Check Logic
  std::string sdFile =
      DiagnosticEngine::SrcMgr->getFullSourceLoc(SD->Loc).FileName;
  std::string nodeFile =
      DiagnosticEngine::SrcMgr->getFullSourceLoc(Node->Loc).FileName;

  if (!SD->IsPub && sdFile != nodeFile) {
    bool sameModule = false;
    if (CurrentModule && !CurrentModule->Shapes.empty()) {
      for (const auto &shapeInModule : CurrentModule->Shapes) {
        if (DiagnosticEngine::SrcMgr->getFullSourceLoc(shapeInModule->Loc)
                .FileName == sdFile) {
          if (CurrentModule) {
            std::string modFile =
                DiagnosticEngine::SrcMgr->getFullSourceLoc(CurrentModule->Loc)
                    .FileName;
            if (modFile == sdFile) { // Simplified same-file/module check
              sameModule = true;
            }
          }
          break;
        }
      }
    }
    if (!sameModule) {
      DiagnosticEngine::report(getLoc(Node), DiagID::ERR_PRIVATE_TYPE, SD->Name,
                               sdFile);
      HasError = true;
      return false;
    }
  }
  return true;
}

} // namespace toka
