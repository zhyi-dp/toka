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
#include "toka/Sema.h"
#include "toka/Type.h"
#include <set>
#include <string>

namespace toka {

std::string Sema::resolveType(const std::string &Type) {
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

  if (TypeAliasMap.count(Type)) {
    // For strong types, we only resolve if we want the UNDERLYING type
    // But for most semantic checks, we might want to keep the strong Name.
    // However, resolveType is usually used to find the physical layout.
    // Verify object creation (Coexistence Phase)
    toka::Type::fromString(TypeAliasMap[Type].Target);
    return resolveType(TypeAliasMap[Type].Target);
  }
  // Coexistence Phase
  toka::Type::fromString(Type);
  return Type;
}

std::shared_ptr<toka::Type>
Sema::resolveType(std::shared_ptr<toka::Type> type) {
  if (!type)
    return nullptr;

  if (auto shape = std::dynamic_pointer_cast<toka::ShapeType>(type)) {
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
  }
  // Primitives can also be aliased potentially? Or just shapes.
  // currently Type::fromString parses unknown as ShapeType so this covers
  // aliases.
  return type;
}

bool Sema::isTypeCompatible(std::shared_ptr<toka::Type> Target,
                            std::shared_ptr<toka::Type> Source) {
  if (!Target || !Source)
    return false;

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
    // T could be *i8 or ^i8
    if (auto ptr = std::dynamic_pointer_cast<toka::PointerType>(T)) {
      if (auto pte = std::dynamic_pointer_cast<toka::PrimitiveType>(
              ptr->getPointeeType())) {
        if (pte->Name == "i8")
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
  auto cleanT = T->withAttributes(false, T->IsNullable);
  auto cleanS = S->withAttributes(false, S->IsNullable);
  if (cleanT->equals(*cleanS))
    return true;

  // 5. Pointer Nullability Subtyping (*Data compatible with *?Data)
  if (auto ptrT = std::dynamic_pointer_cast<toka::PointerType>(T)) {
    if (auto ptrS = std::dynamic_pointer_cast<toka::PointerType>(S)) {
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
  if (Target == Source || Target == "unknown" || Source == "unknown")
    return true;

  auto tObj = toka::Type::fromString(Target);
  auto sObj = toka::Type::fromString(Source);
  return isTypeCompatible(tObj, sObj);
}

} // namespace toka
