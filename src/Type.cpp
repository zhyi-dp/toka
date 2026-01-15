#include "toka/Type.h"
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
  if (target.IsWritable && !IsWritable)
    return false;
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
  if (IsWritable)
    s += "#";
  if (IsNullable)
    s += "?";
  return s;
}

bool PrimitiveType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherPrim = dynamic_cast<const PrimitiveType *>(&other);
  return otherPrim && Name == otherPrim->Name;
}

bool PrimitiveType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target))
    return false;
  const auto *otherPrim = dynamic_cast<const PrimitiveType *>(&target);
  return otherPrim && Name == otherPrim->Name;
}

std::shared_ptr<Type> PrimitiveType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
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
  // Recursively check compatibility of pointee
  return PointeeType->isCompatibleWith(*otherPtr->PointeeType);
}

std::string RawPointerType::toString() const {
  std::string s = "*";
  if (IsWritable /* applied to pointer */)
    s += "#"; // This is ambiguous in current spec vs code
              // Current spec: *#p (swappable ident) vs *p# (mutable soul).
              // Type object represents the SOUL type mostly.
              // Let's stick to standard representation: *T
              // If the pointer itself implies attributes on the pointee view?
              // Let's just output *Pointee

  // Correction: The Type object for `*i32` should be
  // RawPointer(Primitive(i32)). If it is `*i32#`, it is
  // RawPointer(Primitive(i32, Writable=true)).

  return "*" + PointeeType->toString();
}

std::shared_ptr<Type> RawPointerType::withAttributes(bool w, bool n) const {
  // Attributes on the Pointer Type itself usually mean metadata about the
  // pointer SLOT, or are propagated. For now, distinct from Pointee.
  return cloneWithAttrs(this, w, n);
}

std::string UniquePointerType::toString() const {
  return "^" + PointeeType->toString();
}
std::shared_ptr<Type> UniquePointerType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string SharedPointerType::toString() const {
  return "~" + PointeeType->toString();
}
std::shared_ptr<Type> SharedPointerType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string ReferenceType::toString() const {
  return "&" + PointeeType->toString();
}
std::shared_ptr<Type> ReferenceType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

// --- Composite ---

std::string ArrayType::toString() const {
  return "[" + ElementType->toString() + "; " + std::to_string(Size) + "]";
}

bool ArrayType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherArr = dynamic_cast<const ArrayType *>(&other);
  ElementType->equals(*otherArr->ElementType);
}

bool ArrayType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target))
    return false;
  const auto *otherArr = dynamic_cast<const ArrayType *>(&target);
  // Arrays must be invariant in size and element type for safety usually?
  // Toka rules: size must match. Element type can flow?
  // Let's assume element type must be compatible.
  return otherArr && Size == otherArr->Size &&
         ElementType->isCompatibleWith(*otherArr->ElementType);
}

std::shared_ptr<Type> ArrayType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string ShapeType::toString() const {
  std::string s = Name;
  if (IsWritable)
    s += "#";
  if (IsNullable)
    s += "?";
  return s;
}

bool ShapeType::equals(const Type &other) const {
  if (!Type::equals(other))
    return false;
  const auto *otherSh = dynamic_cast<const ShapeType *>(&other);
  return otherSh && Name == otherSh->Name;
}

bool ShapeType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target))
    return false;
  const auto *otherSh = dynamic_cast<const ShapeType *>(&target);
  return otherSh && Name == otherSh->Name;
}

std::shared_ptr<Type> ShapeType::withAttributes(bool w, bool n) const {
  return cloneWithAttrs(this, w, n);
}

std::string TupleType::toString() const {
  std::string s = "(";
  for (size_t i = 0; i < Elements.size(); ++i) {
    if (i > 0)
      s += ", ";
    s += Elements[i]->toString();
  }
  s += ")";
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
  if (!ReturnType->equals(*otherFn->ReturnType))
    return false;
  return true;
}

bool FunctionType::isCompatibleWith(const Type &target) const {
  if (!Type::isCompatibleWith(target))
    return false;
  const auto *otherFn = dynamic_cast<const FunctionType *>(&target);
  if (!otherFn || ParamTypes.size() != otherFn->ParamTypes.size())
    return false;

  // Function covariance/contravariance?
  // Return type: covariant (can return subtype of target return)
  // Params: contravariant (can accept supertype of target param)
  // For now, let's stick to strict equality or simple compatibility

  if (!ReturnType->isCompatibleWith(*otherFn->ReturnType))
    return false;

  for (size_t i = 0; i < ParamTypes.size(); ++i) {
    // strict for params for now to avoid ambiguity
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

// Helper to strip outer whitespace
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

  // Parse Attributes (Suffixes)
  bool isWritable = false;
  bool isNullable = false;

  // Peel from end
  while (!s.empty()) {
    char back = s.back();
    if (back == '#') {
      isWritable = true;
      s.pop_back();
    } else if (back == '?') {
      isNullable = true;
      s.pop_back();
    } else if (back == '!') {
      isWritable = true;
      isNullable = true;
      s.pop_back();
    } else {
      break;
    }
  }

  // Pointer Prefixes
  // Need to handle recursive pointers like **char
  if (s.empty())
    return std::make_shared<UnresolvedType>(rawType);

  char first = s[0];
  if (first == '*') {
    auto pointee = Type::fromString(s.substr(1));
    auto ptr = std::make_shared<RawPointerType>(pointee);
    ptr->IsWritable = isWritable;
    ptr->IsNullable = isNullable;
    return ptr;
  }
  if (first == '^') {
    auto pointee = Type::fromString(s.substr(1));
    auto ptr = std::make_shared<UniquePointerType>(pointee);
    ptr->IsWritable = isWritable;
    ptr->IsNullable = isNullable;
    return ptr;
  }
  if (first == '~') {
    auto pointee = Type::fromString(s.substr(1));
    auto ptr = std::make_shared<SharedPointerType>(pointee);
    ptr->IsWritable = isWritable;
    ptr->IsNullable = isNullable;
    return ptr;
  }
  if (first == '&') {
    auto pointee = Type::fromString(s.substr(1));
    auto ptr = std::make_shared<ReferenceType>(pointee);
    ptr->IsWritable = isWritable;
    ptr->IsNullable = isNullable;
    return ptr;
  }

  // Arrays [T; N]
  if (first == '[') {
    size_t semi = s.find(';');
    size_t close = s.find_last_of(']');
    if (semi != std::string::npos && close != std::string::npos) {
      std::string elemStr = s.substr(1, semi - 1);
      std::string sizeStr = s.substr(semi + 1, close - semi - 1);
      auto elem = Type::fromString(elemStr);
      uint64_t size = 0;
      try {
        size = std::stoull(sizeStr);
      } catch (...) {
      }
      auto arr = std::make_shared<ArrayType>(elem, size);
      arr->IsWritable = isWritable;
      arr->IsNullable = isNullable;
      return arr;
    }
  }

  // Basic Types
  if (s == "void")
    return std::make_shared<VoidType>();
  if (s == "i32" || s == "i64" || s == "u32" || s == "u64" || s == "f32" ||
      s == "f64" || s == "bool" || s == "char" || s == "str") {
    auto prim = std::make_shared<PrimitiveType>(s);
    prim->IsWritable = isWritable;
    prim->IsNullable = isNullable;
    return prim;
  }

  // Fallback: Shape or Alias
  if (s == "unknown") {
    return std::make_shared<UnresolvedType>(s);
  }

  auto shape = std::make_shared<ShapeType>(s);
  shape->IsWritable = isWritable;
  shape->IsNullable = isNullable;
  return shape;
}

} // namespace toka
