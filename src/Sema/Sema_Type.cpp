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
#include <cctype>
#include <iostream>
#include <set>
#include <string>

namespace toka {

// Helper to get location
static SourceLocation getLoc(ASTNode *Node) { return Node->Loc; }

std::string Sema::resolveType(const std::string &Type) {
  // [NEW] Local Type Alias (Generic Parameter) Lookup
  if (CurrentScope) {
    SymbolInfo Sym;
    if (CurrentScope->lookup(Type, Sym)) {
      if (Sym.IsTypeAlias && Sym.TypeObj) {
        return resolveType(Sym.TypeObj)->toString();
      }
    }
  }

  size_t scopePos = Type.find("::");
  if (scopePos != std::string::npos) {
    std::string ModName = Type.substr(0, scopePos);
    std::string TargetType = Type.substr(scopePos + 2);

    SymbolInfo modSpec;
    if (CurrentScope && CurrentScope->lookup(ModName, modSpec) &&
        modSpec.ReferencedModule) {
      ModuleScope *target = (ModuleScope *)modSpec.ReferencedModule;
      if (target->TypeAliases.count(TargetType)) {
        return resolveType(target->TypeAliases[TargetType].Target);
      }
      return TargetType; // It's a base type or shape in that module
    }
  }

  // Fallback: use the object-based resolver which handles generics
  auto typeObj = toka::Type::fromString(Type);
  auto resolved = resolveType(typeObj);
  std::string result = resolved->toString();
  return result;
}

std::shared_ptr<toka::Type>
Sema::resolveType(std::shared_ptr<toka::Type> type) {
  if (!type)
    return nullptr;

  if (auto ptr = std::dynamic_pointer_cast<toka::PointerType>(type)) {
    auto inner = resolveType(ptr->getPointeeType());
    if (inner != ptr->getPointeeType()) {
      std::shared_ptr<toka::PointerType> newPtr;
      if (ptr->typeKind == toka::Type::UniquePtr)
        newPtr = std::make_shared<toka::UniquePointerType>(inner);
      else if (ptr->typeKind == toka::Type::SharedPtr)
        newPtr = std::make_shared<toka::SharedPointerType>(inner);
      else if (ptr->typeKind == toka::Type::Reference)
        newPtr = std::make_shared<toka::ReferenceType>(inner);
      else
        newPtr = std::make_shared<toka::RawPointerType>(inner);
      newPtr->IsWritable = ptr->IsWritable;
      newPtr->IsNullable = ptr->IsNullable;
      return newPtr;
    }
  }

  if (auto arr = std::dynamic_pointer_cast<toka::ArrayType>(type)) {
    auto inner = resolveType(arr->ElementType);
    if (inner != arr->ElementType) {
      auto newArr = std::make_shared<toka::ArrayType>(inner, arr->Size,
                                                      arr->SymbolicSize);
      newArr->IsWritable = arr->IsWritable;
      newArr->IsNullable = arr->IsNullable;
      return newArr;
    }
  }

  if (auto shape = std::dynamic_pointer_cast<toka::ShapeType>(type)) {

    // [NEW] Monomorphization Trigger
    if (!shape->GenericArgs.empty()) {
      // 1. Resolve arguments first
      for (auto &Arg : shape->GenericArgs) {
        Arg = resolveType(Arg);
      }
      // 2. Instantiate
      return instantiateGenericShape(shape);
    }

    // [NEW] Local Scope Alias Lookup (for T -> i32)
    if (CurrentScope) {
      SymbolInfo Sym;
      if (CurrentScope->lookup(shape->Name, Sym)) {
        if (Sym.IsTypeAlias && Sym.TypeObj) {
          // We found T -> i32 (TypeObj).
          // We need to return TypeObj, but retain attributes (IsWritable, etc)
          // of 'shape'.
          auto resolved = resolveType(Sym.TypeObj);
          return resolved->withAttributes(type->IsWritable, type->IsNullable);
        }
      }
    }

    size_t scopePos = shape->Name.find("::");
    if (scopePos != std::string::npos) {
      std::string ModName = shape->Name.substr(0, scopePos);
      std::string TargetType = shape->Name.substr(scopePos + 2);
      SymbolInfo modSpec;
      if (CurrentScope && CurrentScope->lookup(ModName, modSpec) &&
          modSpec.ReferencedModule) {
        ModuleScope *target = (ModuleScope *)modSpec.ReferencedModule;
        if (target->TypeAliases.count(TargetType)) {
          auto resolved =
              toka::Type::fromString(target->TypeAliases[TargetType].Target);
          return resolveType(
              resolved->withAttributes(type->IsWritable, type->IsNullable));
        }
      }
    }

    if (TypeAliasMap.count(shape->Name)) {
      auto resolved = toka::Type::fromString(TypeAliasMap[shape->Name].Target);
      return resolveType(
          resolved->withAttributes(type->IsWritable, type->IsNullable));
    }

    if (ShapeMap.count(shape->Name)) {
      shape->resolve(ShapeMap[shape->Name]);
    }
  }

  if (auto prim = std::dynamic_pointer_cast<toka::PrimitiveType>(type)) {
    if (TypeAliasMap.count(prim->Name)) {
      auto resolved = toka::Type::fromString(TypeAliasMap[prim->Name].Target);
      return resolveType(
          resolved->withAttributes(type->IsWritable, type->IsNullable));
    }
  }

  // Primitives can also be aliased potentially? Or just shapes.
  // currently Type::fromString parses unknown as ShapeType so this covers
  // aliases.
  return type;
}

std::shared_ptr<toka::Type>
Sema::instantiateGenericShape(std::shared_ptr<ShapeType> GenericShape) {
  if (!GenericShape)
    return GenericShape;

  // 1. Find the Template
  std::string templateName = GenericShape->Name;
  if (!ShapeMap.count(templateName)) {
    // Maybe alias logic here, but let's assume direct lookup first
    return GenericShape;
  }
  ShapeDecl *Template = ShapeMap[templateName];
  if (Template->GenericParams.empty()) {
    // Not a generic template, why args?
    // Error or ignore? Error.
    // For now, return generic shape (unresolved) or error
    return GenericShape;
  }

  if (Template->GenericParams.size() != GenericShape->GenericArgs.size()) {
    // Error: Arity mismatch
    return GenericShape;
  }

  // 2. Mangle Name: Name_M_Arg1_Arg2
  // Simple mangling: Name_M + (Arg1.Name or Arg1.Mangling)
  // We need a robust mangler. For Proof of Concept:
  std::string mangledName = templateName + "_M";
  for (auto &Arg : GenericShape->GenericArgs) {
    std::string argStr = Arg->toString();
    // Sanitize literal values for mangled name (e.g. "10" is fine, but maybe
    // others are not)
    for (char &c : argStr) {
      if (!std::isalnum(c) && c != '_')
        c = '_';
    }
    mangledName += "_" + argStr;
  }

  // 3. Check Cache
  // We use ShapeMap as the primary cache for instantiated decls.
  // We also have a GenericShapeCache if we want to return the same ShapeType
  // object too.
  static std::map<std::string, std::shared_ptr<toka::Type>> GenericShapeCache;
  if (GenericShapeCache.count(mangledName)) {
    return std::dynamic_pointer_cast<ShapeType>(
        GenericShapeCache[mangledName]->withAttributes(
            GenericShape->IsWritable, GenericShape->IsNullable));
  }

  // 4. Instantiate (Cache-First Cycle Breaking)
  // Create partial decl first to allow recursion
  auto NewDecl = std::make_unique<ShapeDecl>(
      Template->IsPub, mangledName, std::vector<GenericParam>{}, Template->Kind,
      std::vector<ShapeMember>{}, Template->IsPacked);
  NewDecl->Loc = Template->Loc;

  ShapeDecl *storedDecl = NewDecl.get();
  // Register IMMEDIATELY in Sema's primary map for resolution
  ShapeMap[mangledName] = storedDecl;

  // Add to CurrentModule to ensure CodeGen visibility
  CurrentModule->Shapes.push_back(std::move(NewDecl));

  auto NewShapeTy = std::make_shared<toka::ShapeType>(mangledName);
  NewShapeTy->Decl = storedDecl;
  GenericShapeCache[mangledName] = NewShapeTy; // Cache base version

  auto ResultTy =
      std::dynamic_pointer_cast<ShapeType>(NewShapeTy->withAttributes(
          GenericShape->IsWritable, GenericShape->IsNullable));

  // [NEW] Synchronous Impl Instantiation
  // If this shape has generic impls, instantiate them now so that
  // m_ShapeProps (HasDrop) and MethodMap are populated before sovereignty
  // checks.
  if (GenericImplMap.count(templateName)) {
    instantiateGenericImpl(GenericImplMap[templateName], mangledName);
  }

  // Now resolve members with recursion enabled using substMap...
  // Wait, we need to return ResultTy but the members are in storedDecl.
  // storedDecl is shared by all attributes versions. Correct.
  std::vector<ShapeMember> newMembers;
  std::map<std::string, std::shared_ptr<toka::Type>> substMap;

  for (size_t i = 0; i < Template->GenericParams.size(); ++i) {
    substMap[Template->GenericParams[i].Name] = GenericShape->GenericArgs[i];
  }

  for (auto &oldMember : Template->Members) {
    ShapeMember newM = oldMember;

    // [Constitution] Hat Rule Normalization (The Double-Hat Fix)
    // Rule: Slot wins. If slot has a hat, take argument's soul and apply slot's
    // hat.
    if (substMap.count(newM.Type)) {
      auto actualTy = substMap[newM.Type];
      if (newM.IsUnique || newM.IsShared || newM.IsReference ||
          newM.HasPointer) {
        // Slot has hat. Strip argument's hat (get soul) and apply slot's hat.
        auto soulTy = actualTy->getSoulType();
        newM.ResolvedType = soulTy; // We'll re-apply hats in resolveMember
        newM.Type = soulTy->toString();
      } else {
        // Slot has NO hat. Pass-through argument's morphology.
        newM.ResolvedType = actualTy;
        newM.Type = actualTy->toString();
      }
    } else {
      // [FIX] Nested Generic Substitution Workaround (Phase 1.5)
      // If type string contains generic param, replace it.
      // E.g. "Node<T>" -> "Node<i32>"
      // Very naive, but needed for `^next: Node<T>`
      std::string memberTypeStr = newM.Type;
      // For each param, replace in string
      for (auto const &[K, V] : substMap) {
        size_t pos = 0;
        while ((pos = memberTypeStr.find(K, pos)) != std::string::npos) {
          // Check word boundaries (include underscore)
          auto isWordChar = [](char c) { return std::isalnum(c) || c == '_'; };
          bool startOk = (pos == 0) || !isWordChar(memberTypeStr[pos - 1]);
          bool endOk = (pos + K.size() == memberTypeStr.size()) ||
                       !isWordChar(memberTypeStr[pos + K.size()]);

          if (startOk && endOk) {
            std::string valStr = V->toString();
            memberTypeStr.replace(pos, K.size(), valStr);
            pos += valStr.size();
          } else {
            pos += K.size();
          }
        }
      }
      newM.Type = memberTypeStr;
    }

    newMembers.push_back(std::move(newM));
  }

  // Update members of the already-registered decl
  storedDecl->Members = std::move(newMembers);

  // Recursively analyze the new shape (resolve members fully)
  // We manually run the resolution logic that analyzeShapes does
  for (auto &member : storedDecl->Members) {
    auto resolveMember = [&](ShapeMember &m) {
      if (m.ResolvedType)
        return;

      std::string prefix = "";
      if (m.IsShared)
        prefix += "~";
      else if (m.IsUnique)
        prefix += "^";
      else if (m.IsReference)
        prefix += "&";
      else if (m.HasPointer)
        prefix += "*";

      std::string fullTypeStr = m.Type;
      if (!fullTypeStr.empty() && fullTypeStr[0] != '^' &&
          fullTypeStr[0] != '*' && fullTypeStr[0] != '&' &&
          fullTypeStr[0] != '~') {
        fullTypeStr = prefix + m.Type;
      }
      // [NEW] If it's an array with a symbolic size that's one of our generic
      // params, replace it before resolution.
      auto memberTypeObj = toka::Type::fromString(fullTypeStr);
      if (auto arr =
              std::dynamic_pointer_cast<toka::ArrayType>(memberTypeObj)) {
        if (!arr->SymbolicSize.empty() && substMap.count(arr->SymbolicSize)) {
          std::string valStr = substMap.at(arr->SymbolicSize)->toString();
          size_t semi = fullTypeStr.find(';');
          size_t close = fullTypeStr.find(']', semi);
          if (semi != std::string::npos && close != std::string::npos) {
            std::string newVal = fullTypeStr.substr(0, semi + 1) + " " +
                                 valStr + fullTypeStr.substr(close);
            fullTypeStr = newVal;
          }
        }
      }

      std::string resolvedName = resolveType(fullTypeStr);
      m.ResolvedType = toka::Type::fromString(resolvedName);
    };

    // [NEW] Handle Nested Substitution for SubMembers (Variants)
    for (auto &sub : member.SubMembers) {
      if (substMap.count(sub.Type)) {
        sub.ResolvedType = substMap[sub.Type];
        sub.Type = sub.ResolvedType->toString();
      } else {
        std::string subTypeStr = sub.Type;
        for (auto const &[K, V] : substMap) {
          size_t pos = 0;
          while ((pos = subTypeStr.find(K, pos)) != std::string::npos) {
            auto isWordChar = [](char c) {
              return std::isalnum(c) || c == '_';
            };
            bool startOk = (pos == 0) || !isWordChar(subTypeStr[pos - 1]);
            bool endOk = (pos + K.size() == subTypeStr.size()) ||
                         !isWordChar(subTypeStr[pos + K.size()]);
            if (startOk && endOk) {
              std::string valStr = V->toString();
              subTypeStr.replace(pos, K.size(), valStr);
              pos += valStr.size();
            } else {
              pos += K.size();
            }
          }
        }
        sub.Type = subTypeStr;
      }
      resolveMember(sub);
    }

    resolveMember(member);
  }

  auto instance = std::make_shared<ShapeType>(mangledName);
  instance->resolve(storedDecl);

  // [NEW] Late Validation for Generic Union Instantiation
  // We must re-run the union safety checks ("Latent Blacklist Check")
  // because T might have been substituted with a forbidden type (e.g.
  // Union<bool>).
  if (storedDecl->Kind == ShapeKind::Union) {
    for (const auto &memb : storedDecl->Members) {
      if (!memb.ResolvedType)
        continue;

      // 1. Check for Forbidden Primitive Types (bool, strict enum)
      auto underlying = getDeepestUnderlyingType(memb.ResolvedType);
      bool invalid = false;
      std::string reason = "";

      if (underlying->isBoolean() || underlying->toString() == "bool") {
        invalid = true;
        reason = "bool";
      } else if (auto st =
                     std::dynamic_pointer_cast<toka::ShapeType>(underlying)) {
        // Naive Check for Strict Enum (without full ShapeMap lookup if easy)
        // We can access ShapeMap from Sema
        if (ShapeMap.count(st->Name)) {
          ShapeDecl *SD = ShapeMap[st->Name];
          if (SD->Kind == ShapeKind::Enum && !SD->IsPacked) {
            invalid = true;
            reason = "strict enum";
          }
        }
      }

      if (invalid) {
        DiagnosticEngine::report(getLoc(storedDecl),
                                 DiagID::ERR_UNION_INVALID_MEMBER, memb.Name,
                                 memb.Type, reason);
        HasError = true;
      }

      // 2. Check for Resource Types (Limit HasDrop)
      bool isResource = false;
      if (memb.IsUnique || memb.IsShared) {
        isResource = true;
      } else if (auto st =
                     std::dynamic_pointer_cast<toka::ShapeType>(underlying)) {
        // Check if this shape is known to have drop
        // Since we are in Sema, we can access m_ShapeProps if it was computed.
        // But m_ShapeProps is local to analyzeShapes pass? No, it's a member of
        // Sema. We might need to ensure it's populated. If not found, be
        // conservative? Or assume it's fine if not smart ptr? For now, let's
        // check smart pointers (most common issue) and explicit String
        if (st->Name == "String" || st->Name == "std::string::String") {
          isResource = true;
        } else if (m_ShapeProps.count(st->Name) &&
                   m_ShapeProps[st->Name].HasDrop) {
          isResource = true;
        }
      } else if (auto ptr =
                     std::dynamic_pointer_cast<toka::PointerType>(underlying)) {
        if (ptr->isUniquePtr() || ptr->isSharedPtr())
          isResource = true;
      }

      if (isResource) {
        DiagnosticEngine::report(getLoc(storedDecl),
                                 DiagID::ERR_UNION_RESOURCE_TYPE, memb.Name,
                                 memb.Type);
        DiagnosticEngine::report(getLoc(storedDecl),
                                 DiagID::NOTE_UNION_RESOURCE_TIP);
        HasError = true;
      }
    }
  }

  auto result = std::dynamic_pointer_cast<ShapeType>(instance->withAttributes(
      GenericShape->IsWritable, GenericShape->IsNullable));
  return result;
}

bool Sema::isTypeCompatible(std::shared_ptr<toka::Type> Target,
                            std::shared_ptr<toka::Type> Source) {
  if (!Target || !Source)
    return false;

  // [NEW] Canonicalize types before comparison
  Target = resolveType(Target);
  Source = resolveType(Source);

  // Unknown/Unresolved types are compatible with everything (Error
  // Recovery)
  if (Target->isUnknown() || Source->isUnknown())
    return true;

  // Identity
  if (Target->equals(*Source))
    return true;

  // Strong Type Check (Pre-resolution)
  // We need the Name if possible.
  std::string targetName;
  if (auto s = std::dynamic_pointer_cast<toka::ShapeType>(Target))
    targetName = s->Name;
  else if (auto p = std::dynamic_pointer_cast<toka::PrimitiveType>(Target))
    targetName = p->Name;

  std::string sourceName;
  if (auto s = std::dynamic_pointer_cast<toka::ShapeType>(Source))
    sourceName = s->Name;
  else if (auto p = std::dynamic_pointer_cast<toka::PrimitiveType>(Source))
    sourceName = p->Name;

  if (!targetName.empty() && TypeAliasMap.count(targetName) &&
      TypeAliasMap[targetName].IsStrong)
    return false;
  if (!sourceName.empty() && TypeAliasMap.count(sourceName) &&
      TypeAliasMap[sourceName].IsStrong)
    return false;

  auto T = resolveType(Target);
  auto S = resolveType(Source);

  // Re-check identity after resolution
  if (T->equals(*S))
    return true;

  // Dynamic Trait Coercion (Unsizing)
  // Check if Target is "dyn @Trait"
  if (auto tShape = std::dynamic_pointer_cast<toka::ShapeType>(T)) {
    std::string tName = tShape->Name;
    if (tName.size() >= 4 && tName.substr(0, 3) == "dyn") {
      std::string traitName = "";
      if (tName.rfind("dyn @", 0) == 0)
        traitName = tName.substr(5);
      else if (tName.rfind("dyn@", 0) == 0)
        traitName = tName.substr(4);

      if (!traitName.empty()) {
        // Get Soul Type Name from Source
        std::string sName = "";
        auto inner = S;
        // Strip pointers to find soul
        while (inner->isPointer()) {
          if (auto ptr = std::dynamic_pointer_cast<toka::PointerType>(inner))
            inner = ptr->getPointeeType();
          else
            break;
        }

        if (auto sPrim = std::dynamic_pointer_cast<toka::PrimitiveType>(inner))
          sName = sPrim->Name;
        else if (auto sShape =
                     std::dynamic_pointer_cast<toka::ShapeType>(inner))
          sName = sShape->Name;

        if (!sName.empty()) {
          std::string implKey = sName + "@" + traitName;
          if (ImplMap.count(implKey))
            return true;
        }
      }
    }
  }

  // Integer Promotion
  auto primT = std::dynamic_pointer_cast<toka::PrimitiveType>(T);
  auto primS = std::dynamic_pointer_cast<toka::PrimitiveType>(S);
  if (primT && primS) {
    if (primT->isInteger() && primS->isInteger()) {
      return true;
    }
    // String Literal (str -> str)
    if (primS->Name == "str" && primT->Name == "str")
      return true;
  }

  // String literal to pointer
  if (primS && primS->Name == "str") {
    // T could be *i8 or *u8 or *char or ^...
    if (auto ptr = std::dynamic_pointer_cast<toka::PointerType>(T)) {
      if (auto pte = std::dynamic_pointer_cast<toka::PrimitiveType>(
              ptr->getPointeeType())) {
        if (pte->Name == "i8" || pte->Name == "u8" || pte->Name == "char")
          return true;
      }
    }
  }

  // 1. Array to Pointer Decay (e.g. [10]i32 -> *i32)
  if (auto ptrT = std::dynamic_pointer_cast<toka::PointerType>(T)) {
    if (auto arrS = std::dynamic_pointer_cast<toka::ArrayType>(S)) {
      if (ptrT->getPointeeType()->isCompatibleWith(
              *arrS->getArrayElementType())) {
        return true;
      }
    }
  }

  // 2. Nullability Covariance: T is compatible with T?
  // (A non-null value can be assigned to a nullable slot)
  // Check if Target is Nullable (Implicitly via name/attribute or
  // Explicitly
  // ?)
  bool targetNullable = Target->IsNullable;
  // Should check specific pointer types too, but let's look at the objects.

  // 3. Implicit Dereference (Reference -> Value)
  // If Source is Reference (&T) and Target is Value (T), allow if T is
  // compat. Note: Sema doesn't strictly track "is copyable" yet, so we
  // allow it generically. CodeGen handles the load.
  if (auto refS = std::dynamic_pointer_cast<toka::ReferenceType>(S)) {
    // Check if Target is NOT a reference
    if (!std::dynamic_pointer_cast<toka::ReferenceType>(T)) {
      // Source &T, Target T. Check compatibility of Inner(S) and T.
      if (isTypeCompatible(Target, refS->getPointeeType())) {
        return true;
      }
    }
  }

  // 4. Writability Stripping (T# compatible with T)
  // Used for passing mutable variables to immutable args.
  // S->withAttributes(false, ...) effectively strips writability logic from
  // comparison.
  // 4. Writability Stripping (T# compatible with T)
  auto cleanT = Target->withAttributes(false, Target->IsNullable);
  auto cleanS = Source->withAttributes(false, Source->IsNullable);
  if (cleanT->equals(*cleanS))
    return true;

  // 5. Pointer Nullability Subtyping (*Data compatible with *?Data)
  if (auto ptrT = std::dynamic_pointer_cast<toka::PointerType>(Target)) {
    if (auto ptrS = std::dynamic_pointer_cast<toka::PointerType>(Source)) {
      if (ptrS->getPointeeType()->equals(*ptrT->getPointeeType())) {
        // Same Pointee. Check nullability.
        // We assume subtyping: NonNullable <: Nullable
        // So if Target is Nullable, Source can be anything.
        if (ptrT->IsNullable)
          return true; // *?T accepts *T or *?T
        if (!ptrS->IsNullable)
          return true; // *T accepts *T
        // Fallback: Raw pointers allow *?T -> *T (Unsafe) if
        // Type::isCompatibleWith allows it. But we handle it there.
      }
    }
  }

  // Nullptr Logic
  bool sIsNull = false;
  if (primS && primS->Name == "nullptr")
    sIsNull = true;
  else if (auto sShape = std::dynamic_pointer_cast<toka::ShapeType>(S)) {
    if (sShape->Name == "nullptr")
      sIsNull = true;
  }

  if (sIsNull) {
    if (T->isPointer() || std::dynamic_pointer_cast<toka::PointerType>(T))
      return true;
  }

  // Symmetric Nullptr (for Comparisons like nullptr == ptr)
  bool tIsNull = false;
  if (primT && primT->Name == "nullptr")
    tIsNull = true;
  else if (auto tShape = std::dynamic_pointer_cast<toka::ShapeType>(T)) {
    if (tShape->Name == "nullptr")
      tIsNull = true;
  }

  if (tIsNull) {
    if (S->isPointer() || std::dynamic_pointer_cast<toka::PointerType>(S))
      return true;
  }

  // Weak Tuple Check (Legacy Coexistence)
  // Since Type::fromString parses tuples as ShapeType("..."), we check the
  // name.
  if (auto tShape = std::dynamic_pointer_cast<toka::ShapeType>(T)) {
    if (auto sShape = std::dynamic_pointer_cast<toka::ShapeType>(S)) {
      if (!tShape->Name.empty() && tShape->Name[0] == '(' &&
          !sShape->Name.empty() && sShape->Name[0] == '(') {
        return true;
      }
    }
  }

  // NOTE: Trait coercion (dyn) is omitted for briefness/complexity, will
  // rely upon resolveType logic or add later. The original string logic had
  // it. For Coexistence, we might skip it if not used in current tests, OR
  // add it. Original logic checked string "dyn". `Type::fromString` parses
  // "dyn Shape" as ShapeType("dyn Shape")? No, `dyn @Shape`. `fromString`
  // fallback: ShapeType("dyn @Shape"). So we can check name.

  // Use core compatibility (Source flows to Target)
  return S->isCompatibleWith(*T);
}

bool Sema::isTypeCompatible(const std::string &Target,
                            const std::string &Source) {
  if (Target == Source || Target == "unknown" || Source == "unknown" ||
      Source == "Unresolved(unknown)")
    return true;

  auto tObj = toka::Type::fromString(resolveType(Target));
  auto sObj = toka::Type::fromString(resolveType(Source));
  return isTypeCompatible(tObj, sObj);
}

std::shared_ptr<toka::Type>
Sema::getDeepestUnderlyingType(std::shared_ptr<toka::Type> type) {
  if (!type)
    return nullptr;

  auto current = type;
  // Limit recursion to avoid infinite loops
  for (int i = 0; i < 20; ++i) {
    if (auto s = std::dynamic_pointer_cast<toka::ShapeType>(current)) {
      if (TypeAliasMap.count(s->Name)) {
        std::string targetStr = TypeAliasMap[s->Name].Target;
        auto targetObj = toka::Type::fromString(resolveType(targetStr));
        if (targetObj) {
          current = targetObj;
          continue;
        }
      }
    }
    break;
  }
  return resolveType(current);
}

} // namespace toka
