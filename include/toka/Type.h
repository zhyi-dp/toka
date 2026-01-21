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
#pragma once
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

namespace toka {

class ShapeDecl; // Forward declaration

class Type {
public:
  enum Kind {
    Primitive,
    Void,
    RawPtr,
    UniquePtr,
    SharedPtr,
    Reference,
    Array,
    Slice, // Reserved for future
    Shape,
    Tuple,
    Function,
    Unresolved // String-based placeholder
  };

  Kind typeKind;
  bool IsWritable = false; // '#' (Content mutation)
  bool IsNullable = false; // '?' (Content nullability)

  Type(Kind k) : typeKind(k) {}
  virtual ~Type() = default;

  virtual std::string toString() const = 0;
  virtual bool equals(const Type &other) const;

  // Helpers
  // Checks if 'this' can be assigned to 'target' (handles permission flow)
  // e.g. i32# -> i32 (OK), i32 -> i32# (Error)
  virtual bool isCompatibleWith(const Type &target) const;

  bool isPointer() const {
    return typeKind == RawPtr || typeKind == UniquePtr ||
           typeKind == SharedPtr || typeKind == Reference;
  }
  bool isRawPointer() const { return typeKind == RawPtr; }
  bool isSmartPointer() const {
    return typeKind == UniquePtr || typeKind == SharedPtr;
  }
  bool isReference() const { return typeKind == Reference; }
  bool isUniquePtr() const { return typeKind == UniquePtr; }
  bool isSharedPtr() const { return typeKind == SharedPtr; }
  bool isArray() const { return typeKind == Array; }
  bool isTuple() const { return typeKind == Tuple; }
  bool isFunction() const { return typeKind == Function; }
  bool isVoid() const { return typeKind == Void; }
  bool isUnknown() const { return typeKind == Unresolved; }

  virtual bool isBoolean() const { return false; }
  virtual bool isInteger() const { return false; }
  virtual bool isSignedInteger() const { return false; }
  virtual bool isFloatingPoint() const { return false; }

  virtual std::shared_ptr<Type> getPointeeType() const { return nullptr; }
  virtual std::shared_ptr<Type> getArrayElementType() const { return nullptr; }

  // Clone with new attributes
  virtual std::shared_ptr<Type> withAttributes(bool writable,
                                               bool nullable) const = 0;

  // Static Factory for String Parsing (The Bridge)
  static std::shared_ptr<Type> fromString(const std::string &typeStr);

  // Helper: Strip morphology characters (*, ^, &, ~, #, ?, !) to get the "Soul"
  // name
  static std::string stripMorphology(const std::string &name);
  static std::string stripPrefixes(const std::string &name);

  bool isShape() const { return typeKind == Shape; }
  virtual std::string getSoulName() const { return toString(); }
};

// --- Basic Types ---

class VoidType : public Type {
public:
  VoidType() : Type(Void) {}
  std::string toString() const override { return "void"; }
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
};

class PrimitiveType : public Type {
public:
  std::string Name; // i32, f64, bool, str
  PrimitiveType(const std::string &name) : Type(Primitive), Name(name) {}
  std::string toString() const override;
  bool equals(const Type &other) const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;

  bool isBoolean() const override { return Name == "bool"; }
  bool isInteger() const override {
    return Name == "i32" || Name == "i64" || Name == "u32" || Name == "u64" ||
           Name == "i8" || Name == "u8" || Name == "i16" || Name == "u16" ||
           Name == "usize" || Name == "char";
  }
  bool isSignedInteger() const override {
    return Name == "i32" || Name == "i64" || Name == "i8" || Name == "i16" ||
           Name == "isize";
  }
  bool isFloatingPoint() const override {
    return Name == "f32" || Name == "f64";
  }
};

// --- Pointer Types ---

class PointerType : public Type {
public:
  std::shared_ptr<Type> PointeeType;

  PointerType(Kind k, std::shared_ptr<Type> pointee)
      : Type(k), PointeeType(pointee) {}

  bool equals(const Type &other) const override;
  bool isCompatibleWith(const Type &target) const override;
  std::shared_ptr<Type> getPointeeType() const override { return PointeeType; }
};

class RawPointerType : public PointerType {
public:
  RawPointerType(std::shared_ptr<Type> pointee)
      : PointerType(RawPtr, pointee) {}
  std::string toString() const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;
};

class UniquePointerType : public PointerType {
public:
  UniquePointerType(std::shared_ptr<Type> pointee)
      : PointerType(UniquePtr, pointee) {}
  std::string toString() const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;
};

class SharedPointerType : public PointerType {
public:
  SharedPointerType(std::shared_ptr<Type> pointee)
      : PointerType(SharedPtr, pointee) {}
  std::string toString() const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;
};

class ReferenceType : public PointerType {
public:
  ReferenceType(std::shared_ptr<Type> pointee)
      : PointerType(Reference, pointee) {}
  std::string toString() const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;
};

// --- Composite Types ---

class ArrayType : public Type {
public:
  std::shared_ptr<Type> ElementType;
  uint64_t Size;
  std::string SymbolicSize; // [NEW] For const generics like N_

  ArrayType(std::shared_ptr<Type> elem, uint64_t size, std::string sym = "")
      : Type(Array), ElementType(elem), Size(size),
        SymbolicSize(std::move(sym)) {}
  std::string toString() const override;
  bool equals(const Type &other) const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;
  std::shared_ptr<Type> getArrayElementType() const override {
    return ElementType;
  }
};

class ShapeType : public Type {
public:
  std::string Name;
  std::vector<std::shared_ptr<Type>> GenericArgs; // [NEW] Generic Arguments
  ShapeDecl *Decl = nullptr;
  ShapeType(const std::string &name,
            std::vector<std::shared_ptr<Type>> args = {})
      : Type(Shape), Name(name), GenericArgs(std::move(args)) {}
  void resolve(ShapeDecl *decl);
  bool isResolved() const { return Decl != nullptr; }
  std::string toString() const override;
  bool equals(const Type &other) const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;
};

class TupleType : public Type {
public:
  std::vector<std::shared_ptr<Type>> Elements;

  TupleType(std::vector<std::shared_ptr<Type>> elems)
      : Type(Tuple), Elements(std::move(elems)) {}
  std::string toString() const override;
  bool equals(const Type &other) const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;
};

class FunctionType : public Type {
public:
  std::vector<std::shared_ptr<Type>> ParamTypes;
  std::shared_ptr<Type> ReturnType;
  bool IsVariadic = false;

  FunctionType(std::vector<std::shared_ptr<Type>> params,
               std::shared_ptr<Type> ret, bool variadic = false)
      : Type(Function), ParamTypes(std::move(params)), ReturnType(ret),
        IsVariadic(variadic) {}

  std::string toString() const override;
  bool equals(const Type &other) const override;
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
  bool isCompatibleWith(const Type &target) const override;
};

// #include "toka/Type.h" -> Removed self-include

// ... (in UnresolvedType)
class UnresolvedType : public Type {
public:
  std::string Name;
  UnresolvedType(const std::string &name) : Type(Unresolved), Name(name) {}
  std::string toString() const override { return "Unresolved(" + Name + ")"; }
  bool equals(const Type &other) const override {
    return false;
  } // Should resolve first
  std::shared_ptr<Type> withAttributes(bool w, bool n) const override;
};

} // namespace toka
