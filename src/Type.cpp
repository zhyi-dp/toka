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
#include "toka/Type.h"
#include "toka/AST.h"
#include <memory>
#include <sstream>
#include <string>

namespace toka {

bool Type::equals(const toka::Type &other) const {
  if (typeKind != other.typeKind)
    return false;
  if (IsWritable != other.IsWritable)
    return false;
  if (IsNullable != other.IsNullable)
    return false;
  return true;
}

// Check compatibility (Permission Flow)
bool Type::isCompatibleWith(const Type &target) const {
  if (typeKind != target.typeKind)
    return false;
  // Target Writable? Source must be Writable.
  // Mutability check removed from base: T -> T# is allowed for values (copy).
  // Strict mutability is enforced in Pointer/Reference types where it matters.
  // Target Non-Nullable? Source must be Non-Nullable.
  if (!target.IsNullable && IsNullable)
    return false;
  return true;
}

// --- Attribute Helpers ---
template <typename T>
std::shared_ptr<Type> cloneWithAttrs(const T *original, bool w, bool n) {
  auto clone = std::make_shared<T>(*original);
  clone->IsWritable = w;
  clone->IsNullable = n;
  return clone;
}

// --- Implementations ---

std::shared_ptr<Type> VoidType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string PrimitiveType::toString() const {
  std::string s = Name;
  if (IsWritable && IsNullable)
    s += "!";
  else {
    if (IsNullable)
      s += "?";
    if (IsWritable)
      s += "#";
  }
  return s;
}

bool PrimitiveType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherPrim = dynamic_cast<const PrimitiveType *>(&other);
  return otherPrim && Name == otherPrim->Name;
}

std::shared_ptr<Type> PrimitiveType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

bool PrimitiveType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target)) {
    const auto *otherPrim = dynamic_cast<const PrimitiveType *>(&target);
    if (otherPrim && isInteger() && otherPrim->isInteger())
      return true; // Loose integer compatibility
    return false;
  }
  const auto *otherPrim = dynamic_cast<const PrimitiveType *>(&target);
  if (!otherPrim)
    return false;
  if (Name == otherPrim->Name)
    return true;
  return isInteger() && otherPrim->isInteger();
}

// --- Pointers ---

bool PointerType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherPtr = dynamic_cast<const PointerType *>(&other);
  if (!otherPtr)
    return false;
  return PointeeType->equals(*otherPtr->PointeeType);
}

bool PointerType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target))
    return false;
  const auto *otherPtr = dynamic_cast<const PointerType *>(&target);
  if (!otherPtr)
    return false;
  if (otherPtr->PointeeType->IsWritable && !PointeeType->IsWritable)
    return false;
  return PointeeType->isCompatibleWith(*otherPtr->PointeeType);
}

std::string RawPointerType::toString() const {
  std::string s = "*";
  if (IsWritable && IsNullable) {
    s += "!";
  } else {
    if (IsNullable)
      s += "?";
    if (IsWritable)
      s += "#";
  }
  return s + PointeeType->toString();
}

bool RawPointerType::isCompatibleWith(const Type &target) const {
  const auto *otherPtr = dynamic_cast<const RawPointerType *>(&target);
  if (!otherPtr)
    return false;
  if (target.IsWritable && !IsWritable)
    return false;
  if (otherPtr->PointeeType->IsWritable && !PointeeType->IsWritable)
    return false;
  // Loose: *?T flows to *T for Raw Pointers (Unsafe)
  return PointeeType->isCompatibleWith(*otherPtr->PointeeType);
}

std::shared_ptr<Type> RawPointerType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string UniquePointerType::toString() const {
  std::string s = "^";
  if (IsWritable && IsNullable) {
    s += "!";
  } else {
    if (IsNullable)
      s += "?";
    if (IsWritable)
      s += "#";
  }
  return s + PointeeType->toString();
}

bool UniquePointerType::isCompatibleWith(const Type &target) const {
  const auto *otherPtr = dynamic_cast<const UniquePointerType *>(&target);
  if (otherPtr)
    if (otherPtr->PointeeType->IsWritable && !PointeeType->IsWritable)
      return false;
  return Type::isCompatibleWith(target) &&
         PointeeType->isCompatibleWith(*otherPtr->PointeeType);
  return false;
}

std::shared_ptr<Type> UniquePointerType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string SharedPointerType::toString() const {
  std::string s = "~";
  if (IsWritable && IsNullable) {
    s += "!";
  } else {
    if (IsNullable)
      s += "?";
    if (IsWritable)
      s += "#";
  }
  return s + PointeeType->toString();
}

bool SharedPointerType::isCompatibleWith(const Type &target) const {
  const auto *otherPtr = dynamic_cast<const SharedPointerType *>(&target);
  if (otherPtr) {
    if (otherPtr->PointeeType->IsWritable && !PointeeType->IsWritable)
      return false;
    return Type::isCompatibleWith(target) &&
           PointeeType->isCompatibleWith(*otherPtr->PointeeType);
  }
  if (!dynamic_cast<const PointerType *>(&target)) {
    return PointeeType->isCompatibleWith(target);
  }
  return false;
}

std::shared_ptr<Type> SharedPointerType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string ReferenceType::toString() const {
  std::string s = "&";
  if (IsWritable && IsNullable) {
    s += "!";
  } else {
    if (IsNullable)
      s += "?";
    if (IsWritable)
      s += "#";
  }
  return s + PointeeType->toString();
}

bool ReferenceType::isCompatibleWith(const Type &target) const {
  const auto *otherPtr = dynamic_cast<const ReferenceType *>(&target);
  if (otherPtr)
    if (otherPtr->PointeeType->IsWritable && !PointeeType->IsWritable)
      return false;
  return Type::isCompatibleWith(target) &&
         PointeeType->isCompatibleWith(*otherPtr->PointeeType);
  return false;
}

std::shared_ptr<Type> ReferenceType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

// --- Composite ---

std::string ArrayType::toString() const {
  std::string s =
      "[" + ElementType->toString() + "; " + std::to_string(Size) + "]";
  if (IsWritable && IsNullable) {
    s += "!";
  } else {
    if (IsWritable)
      s += "#";
    if (IsNullable)
      s += "?";
  }
  return s;
}

bool ArrayType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherArr = dynamic_cast<const ArrayType *>(&other);
  return otherArr && Size == otherArr->Size &&
         ElementType->equals(*otherArr->ElementType);
}

bool ArrayType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target))
    return false;
  const auto *otherArr = dynamic_cast<const ArrayType *>(&target);
  return otherArr && Size == otherArr->Size &&
         ElementType->isCompatibleWith(*otherArr->ElementType);
}

std::shared_ptr<Type> ArrayType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string ShapeType::toString() const {
  std::string s = Name;
  if (IsWritable && IsNullable) {
    s += "!";
  } else {
    if (IsWritable)
      s += "#";
    if (IsNullable)
      s += "?";
  }
  return s;
}

bool ShapeType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherSh = dynamic_cast<const ShapeType *>(&other);
  return otherSh && Name == otherSh->Name;
}

bool ShapeType::isCompatibleWith(const Type &target) const {
  const auto *otherSh = dynamic_cast<const ShapeType *>(&target);
  if (otherSh) {
    if (otherSh->Name.rfind("dyn@", 0) == 0)
      return true;
    if (Name == otherSh->Name)
      return Type::isCompatibleWith(target);
    return false;
  }
  return false;
}

std::shared_ptr<Type> ShapeType::withAttributes(bool w, bool n) const {
  auto clone = cloneWithAttrs(this, w, n);
  if (Decl)
    std::dynamic_pointer_cast<ShapeType>(clone)->resolve(Decl);
  return clone;
}

void ShapeType::resolve(ShapeDecl *decl) {
  Decl = decl;
  if (decl) {
    Name = decl->Name;
  }
}

std::string TupleType::toString() const {
  std::string s = "(";
  for (size_t i = 0; i < Elements.size(); ++i) {
    if (i > 0)
      s += ", ";
    s += Elements[i]->toString();
  }
  s += ")";
  if (IsWritable && IsNullable) {
    s += "!";
  } else {
    if (IsWritable)
      s += "#";
    if (IsNullable)
      s += "?";
  }
  return s;
}

bool TupleType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherTup = dynamic_cast<const TupleType *>(&other);
  if (!otherTup || Elements.size() != otherTup->Elements.size())
    return false;
  for (size_t i = 0; i < Elements.size(); ++i) {
    if (!Elements[i]->equals(*otherTup->Elements[i]))
      return false;
  }
  return true;
}

bool TupleType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target))
    return false;
  const auto *otherTup = dynamic_cast<const TupleType *>(&target);
  if (!otherTup || Elements.size() != otherTup->Elements.size())
    return false;
  for (size_t i = 0; i < Elements.size(); ++i) {
    if (!Elements[i]->isCompatibleWith(*otherTup->Elements[i]))
      return false;
  }
  return true;
}

std::shared_ptr<Type> TupleType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string FunctionType::toString() const {
  std::string s = "fn(";
  for (size_t i = 0; i < ParamTypes.size(); ++i) {
    if (i > 0)
      s += ", ";
    s += ParamTypes[i]->toString();
  }
  if (IsVariadic)
    s += ", ...";
  s += ")";
  if (ReturnType && ReturnType->typeKind != Void) {
    s += " -> " + ReturnType->toString();
  }
  return s;
}

bool FunctionType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherFn = dynamic_cast<const FunctionType *>(&other);
  if (!otherFn || ParamTypes.size() != otherFn->ParamTypes.size())
    return false;
  return ReturnType->equals(*otherFn->ReturnType);
}

bool FunctionType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target))
    return false;
  const auto *otherFn = dynamic_cast<const FunctionType *>(&target);
  if (!otherFn || ParamTypes.size() != otherFn->ParamTypes.size())
    return false;
  if (!ReturnType->isCompatibleWith(*otherFn->ReturnType))
    return false;
  for (size_t i = 0; i < ParamTypes.size(); ++i) {
    if (!ParamTypes[i]->equals(*otherFn->ParamTypes[i]))
      return false;
  }
  return true;
}

std::shared_ptr<Type> FunctionType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::shared_ptr<Type> UnresolvedType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

// --- Static Factory (The Parser) ---

std::string Type::stripMorphology(const std::string &name) {
  std::string s = name;
  if (s.empty())
    return "";

  // 1. Strip Prefixes (*, ^, ~, &) and their modifiers
  size_t start = 0;
  while (start < s.size()) {
    char c = s[start];
    if (c == '*' || c == '^' || c == '~' || c == '&' || c == '#' || c == '?' ||
        c == '!' || c == '$') {
      start++;
    } else {
      break;
    }
  }
  s = s.substr(start);

  // 2. Strip Suffixes (#, ?, !)
  while (!s.empty()) {
    char c = s.back();
    if (c == '#' || c == '?' || c == '!' || c == '$') {
      s.pop_back();
    } else {
      break;
    }
  }
  return s;
}

static std::string trim(const std::string &str) {
  size_t first = str.find_first_not_of(' ');
  if (std::string::npos == first)
    return str;
  size_t last = str.find_last_not_of(' ');
  return str.substr(first, (last - first + 1));
}

std::shared_ptr<Type> Type::fromString(const std::string &rawType) {
  std::string s = trim(rawType);
  if (s.empty())
    return std::make_shared<VoidType>();

  // Parse Suffixes (applies to the OUTERMOST type being constructed)
  bool isWritable = false;
  bool isNullable = false;
  while (!s.empty()) {
    char back = s.back();
    if (back == '#') {
      isWritable = true;
      s.pop_back();
    } else if (back == '?') {
      isNullable = true;
      s.pop_back();
    } else if (back == '!') {
      isWritable = isNullable = true;
      s.pop_back();
    } else if (back == ' ') {
      s.pop_back();
    } else
      break;
  }

  if (s.empty())
    return std::make_shared<UnresolvedType>(rawType);

  char first = s[0];
  if (first == '*' || first == '^' || first == '~' || first == '&') {
    size_t offset = 1;
    bool ptrNullable = false;
    bool ptrWritable = false;
    while (offset < s.size()) {
      if (s[offset] == '?') {
        ptrNullable = true;
        offset++;
      } else if (s[offset] == '#') {
        ptrWritable = true;
        offset++;
      } else if (s[offset] == '!') {
        ptrNullable = ptrWritable = true;
        offset++;
      } else
        break;
    }
    auto pointee = Type::fromString(s.substr(offset));
    std::shared_ptr<PointerType> ptr;
    if (first == '*')
      ptr = std::make_shared<RawPointerType>(pointee);
    else if (first == '^')
      ptr = std::make_shared<UniquePointerType>(pointee);
    else if (first == '~')
      ptr = std::make_shared<SharedPointerType>(pointee);
    else
      ptr = std::make_shared<ReferenceType>(pointee);

    ptr->IsNullable = ptrNullable || isNullable;
    ptr->IsWritable = ptrWritable || isWritable;
    return ptr;
  }

  if (first == '[') {
    size_t semi = s.find(';');
    size_t close = s.find_last_of(']');
    if (semi != std::string::npos && close != std::string::npos) {
      auto elem = Type::fromString(s.substr(1, semi - 1));
      uint64_t size = 0;
      try {
        size = std::stoull(s.substr(semi + 1, close - semi - 1));
      } catch (...) {
      }
      auto arr = std::make_shared<ArrayType>(elem, size);
      arr->IsWritable = isWritable;
      arr->IsNullable = isNullable;
      return arr;
    }
  }

  if (s == "void")
    return std::make_shared<VoidType>();
  if (s == "i32" || s == "i64" || s == "u32" || s == "u64" || s == "f32" ||
      s == "f64" || s == "bool" || s == "char" || s == "str" || s == "i8" ||
      s == "u8" || s == "i16" || s == "u16" || s == "usize") {
    auto prim = std::make_shared<PrimitiveType>(s);
    prim->IsWritable = isWritable;
    prim->IsNullable = isNullable;
    return prim;
  }

  if (s == "unknown")
    return std::make_shared<UnresolvedType>(s);

  auto shape = std::make_shared<ShapeType>(s);
  shape->IsWritable = isWritable;
  shape->IsNullable = isNullable;
  return shape;
}

} // namespace toka
