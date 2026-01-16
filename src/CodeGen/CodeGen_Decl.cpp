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
#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>
#include <string>
#include <typeinfo>

namespace toka {

std::string CodeGen::stripMorphology(const std::string &name) {
  std::string s = name;
  if (!s.empty() &&
      (s[0] == '&' || s[0] == '*' || s[0] == '^' || s[0] == '~')) {
    s = s.substr(1); // is a pointer or reference

    if (!s.empty() &&
        (s[0] == '$' || s[0] == '#' || s[0] == '?' || s[0] == '!')) {
      s = s.substr(1); // pointer or reference with flags
    }
  }
  if (!s.empty() && (s.back() == '$' || s.back() == '#' || s.back() == '?' ||
                     s.back() == '!')) {
    s.pop_back(); // soul entity with flags
  }
  return s;
}

llvm::Function *CodeGen::genFunction(const FunctionDecl *func,
                                     const std::string &overrideName,
                                     bool declOnly) {
  std::string funcName = overrideName.empty() ? func->Name : overrideName;
  llvm::errs() << "DEBUG: genFunction " << funcName
               << " (AST Name: " << func->Name << ")\n";
  m_Functions[funcName] = func;
  // m_ValueElementTypes.clear(); // LEGACY REMOVED
  m_Symbols.clear();

  llvm::Function *f = m_Module->getFunction(funcName);

  if (!f) {
    std::vector<llvm::Type *> argTypes;
    for (const auto &arg : func->Args) {
      // Create Type Object from String (Temporary Bridge)
      // Ideally, FunctionDecl would store shared_ptr<Type>, but for now we
      // parse.
      std::shared_ptr<Type> typeObj = Type::fromString(arg.Type);

      // Permission Decorators (AST overrides Type string if present)
      if (arg.IsMutable)
        typeObj = typeObj->withAttributes(true, typeObj->IsNullable);
      if (arg.IsNullable || arg.IsPointerNullable)
        typeObj = typeObj->withAttributes(typeObj->IsWritable, true);

      // [Fix] Apply AST-level Morphology wrappers (Pointer, Unique, Reference,
      // Shared) The AST 'Type' string often doesn't contain *, ^, ~ if they
      // were parsed as decorators
      if (arg.IsReference) {
        typeObj = std::make_shared<ReferenceType>(typeObj);
      } else if (arg.IsUnique) {
        typeObj = std::make_shared<UniquePointerType>(typeObj);
      } else if (arg.IsShared) {
        typeObj = std::make_shared<SharedPointerType>(typeObj);
      } else if (arg.HasPointer) {
        typeObj = std::make_shared<RawPointerType>(typeObj);
      }

      // Determine LLVM Type
      llvm::Type *t = getLLVMType(typeObj);

      // [Restored Logic] Implicit Capture (ABI)
      // Structs, Arrays, and Mutable bindings are passed by pointer (Implicit
      // Reference) unless they are already explicit pointers/references.
      // SharedPtr and UniquePtr are already pointers (or struct wrappers acting
      // as pointers), so checks below mostly separate them.

      bool isAggregate = t->isStructTy() || t->isArrayTy();
      // Note: SharedPtr is a struct {T*, i32*}, but we handle Shared explicitly
      // in AST logic usually. However, old logic checked `!arg.IsReference`.
      // New logic: `typeObj` already wraps Reference/Pointer if AST had them.
      // So if `typeObj` is ALREADY a Pointer/Reference/Unique/Shared, `t` is a
      // pointer (or {ptr,ptr}). We only want to capture if it is a DIRECT value
      // (Primitive, Shape, Array, Tuple) that needs to be passed by ptr.

      bool isDirectValue = !typeObj->isPointer() && !typeObj->isReference();
      // isPointer() covers Raw, Unique, Shared, Reference in Type.h?
      // Checking Type.h: isPointer() covers Raw, Unique, Shared, Reference.

      // [Fix] Enable Capture for Unique Pointers
      bool needsCapture = (isDirectValue && (isAggregate || arg.IsMutable ||
                                             arg.IsRebindable)) ||
                          arg.IsUnique;

      if (needsCapture) {
        t = llvm::PointerType::getUnqual(t);
      }

      if (t)
        argTypes.push_back(t);
    }

    // Return Type
    std::shared_ptr<Type> retTypeObj = Type::fromString(func->ReturnType);
    llvm::Type *retType = getLLVMType(retTypeObj);

    llvm::FunctionType *ft = llvm::FunctionType::get(retType, argTypes, false);
    f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, funcName,
                               m_Module.get());
  }

  if (declOnly)
    return f;

  if (!func->Body)
    return f;

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(m_Context, "entry", f);
  m_Builder.SetInsertPoint(bb);

  m_ScopeStack.push_back({});

  size_t idx = 0;
  for (auto &arg : f->args()) {
    const auto &argDecl = func->Args[idx];
    std::string argName = argDecl.Name;

    // 1. Strip morphology to get the base symbol name
    argName = stripMorphology(argName);

    arg.setName(argName);

    // 2. Resolve Type Object
    std::shared_ptr<Type> typeObj = Type::fromString(argDecl.Type);

    // Apply Function Arg overrides (e.g. "x: i32#")
    bool isMutable = argDecl.IsMutable || argDecl.IsValueMutable;
    bool isNullable = argDecl.IsNullable || argDecl.IsPointerNullable;
    typeObj = typeObj->withAttributes(isMutable, isNullable);

    // [Fix] Apply AST-level Morphology wrappers (Pointer, Unique, Reference,
    // Shared)
    if (argDecl.IsReference) {
      typeObj = std::make_shared<ReferenceType>(typeObj);
    } else if (argDecl.IsUnique) {
      typeObj = std::make_shared<UniquePointerType>(typeObj);
    } else if (argDecl.IsShared) {
      typeObj = std::make_shared<SharedPointerType>(typeObj);
    } else if (argDecl.HasPointer) {
      typeObj = std::make_shared<RawPointerType>(typeObj);
    }

    // 3. Get LLVM Type from Object
    llvm::Type *allocaType = getLLVMType(typeObj);
    llvm::Type *pTy = allocaType; // Soul type approx (refines later)

    // [Restored Logic] Implicit Capture (ABI) - Body
    bool isAggregate = allocaType->isStructTy() || allocaType->isArrayTy();
    bool isDirectValue = !typeObj->isPointer() && !typeObj->isReference();
    bool needsCapture = (isDirectValue && (isAggregate || argDecl.IsMutable ||
                                           argDecl.IsRebindable)) ||
                        argDecl.IsUnique;

    if (needsCapture) {
      // Argument passed by pointer
      allocaType = llvm::PointerType::getUnqual(allocaType);
    }

    llvm::AllocaInst *alloca =
        m_Builder.CreateAlloca(allocaType, nullptr, argName + ".addr");
    m_Builder.CreateStore(&arg, alloca);

    // 4. Register in Symbol Table using Type Object
    TokaSymbol sym;
    sym.allocaPtr = alloca;

    // Refactored Metadata Filler
    fillSymbolMetadata(
        sym, typeObj,
        pTy); // Pass pTy (base element type) not the captured pointer type
    sym.typeName =
        argDecl.Type; // [Fix] Set legacy type string for Dynamic Dispatch

    if (needsCapture) {
      sym.mode = AddressingMode::Pointer;
      // If captured, we add a level of indirection (ptr -> ptr*)
      sym.indirectionLevel++;
    }

    // Explicit permission/flag overrides from AST if not in Type String
    sym.isRebindable = argDecl.IsRebindable;
    sym.isMutable = isMutable;

    m_Symbols[argName] = sym;

    // Legacy maps for compatibility
    // m_ValueTypeNames[argName] = argDecl.Type;
    // m_ValueElementTypes[argName] = sym.soulType; // Now comes from
    // fillSymbolMetadata m_ValueTypes[argName] = allocaType;
    m_NamedValues[argName] = alloca;
    // m_ValueIsMutable[argName] = sym.isMutable; // LEGACY REMOVED

    if (!m_ScopeStack.empty()) {
      // [Fix] Argument Lifecycle
      // Arguments passed by In-Place Capture (Unique Pointers) are effectively
      // borrowed. The Callee must NOT free them. Ownership remains with Caller.
      // So we register them as IsUnique=false for cleanup purposes.
      m_ScopeStack.back().push_back({argName, alloca, false, argDecl.IsShared});
    }

    idx++;
  }

  genStmt(func->Body.get());

  // Recursive Drop Injection
  // We assume 'drop' methods have one argument 'self' (implied or explicit)
  // and we need to drop its members.
  bool isDrop =
      func->Name == "drop" || func->Name.find("_drop") != std::string::npos;

  if (!func->Args.empty() && isDrop) {
    const auto &arg0 = func->Args[0];

    // Check if arg is "self"
    if (arg0.Name == "self") {
      std::string typeName = arg0.Type;
      // Strip morphology
      while (!typeName.empty() &&
             (typeName[0] == '^' || typeName[0] == '*' || typeName[0] == '&' ||
              typeName[0] == '~' || typeName[0] == '#')) {
        typeName = typeName.substr(1);
      }
      if (typeName == "Self" && !m_CurrentSelfType.empty()) {
        typeName = m_CurrentSelfType;
      }

      if (m_Shapes.count(typeName)) {
        const ShapeDecl *S = m_Shapes[typeName];
        // Iterate reverse
        for (auto it = S->Members.rbegin(); it != S->Members.rend(); ++it) {
          // Check if member needs drop
          // We check if a drop function exists for the member type
          std::string memberType = it->Type;
          // Strip morphology
          while (!memberType.empty() &&
                 (memberType[0] == '*' || memberType[0] == '#' ||
                  memberType[0] == '&')) {
            memberType = memberType.substr(1);
          }

          std::string baseType = it->Type;
          bool hasDrop = false;
          std::string dropFunc = "";

          if (it->IsUnique || it->IsShared) {
            hasDrop = true;
          } else if (it->HasPointer || it->IsReference) {
            hasDrop = false; // Raw pointers don't drop
          } else {
            // Value type. Check for drop method
            // Try encap_Type_drop
            std::string try1 = "encap_" + it->Type + "_drop";
            std::string try2 = it->Type + "_drop"; // Legacy

            if (m_Functions.count(try1)) {
              hasDrop = true;
              dropFunc = try1;
            } else if (m_Functions.count(try2)) {
              hasDrop = true;
              dropFunc = try2;
            }
          }

          if (hasDrop) {
            // Access member
            if (m_Symbols.count("self")) {
              TokaSymbol selfSym = m_Symbols["self"];
              llvm::Value *selfPtr = selfSym.allocaPtr;

              llvm::Value *structPtr = m_Builder.CreateLoad(
                  llvm::PointerType::getUnqual(m_Context), selfPtr, "self_ptr");

              // GEP to member
              int fieldIdx = -1;
              int i = 0;
              for (auto &m : S->Members) {
                if (m.Name == it->Name) {
                  fieldIdx = i;
                  break;
                }
                i++;
              }

              if (fieldIdx != -1) {
                // FIX: Use m_StructTypes instead of m_ValueElementTypes
                if (m_StructTypes.count(typeName)) {
                  llvm::Value *fieldEP = m_Builder.CreateStructGEP(
                      m_StructTypes[typeName], structPtr, fieldIdx,
                      it->Name + "_ptr");

                  // Register in Scope 0
                  if (!m_ScopeStack.empty()) {
                    m_ScopeStack[0].push_back({it->Name, fieldEP, it->IsUnique,
                                               it->IsShared,
                                               !dropFunc.empty(), // HasDrop
                                               dropFunc});
                  }
                } else {
                }
              }
            }
          }
        }
      }
    }
  }

  // Ensure Implicit Cleanup
  if (!m_Builder.GetInsertBlock()->getTerminator()) {
    cleanupScopes(0);

    if (func->ReturnType == "void" || func->Name == "main") {
      if (func->Name == "main" && !f->getReturnType()->isVoidTy()) {
        m_Builder.CreateRet(
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));
      } else {
        m_Builder.CreateRetVoid();
      }
    } else {
      m_Builder.CreateUnreachable();
    }
  }
  m_ScopeStack.pop_back();
  return f;
}

llvm::Value *CodeGen::genVariableDecl(const VariableDecl *var) {
  std::string varName = stripMorphology(var->Name);
  std::cerr << "DEBUG: genVariableDecl: " << varName
            << " (TypeName: " << var->TypeName << ")\n";

  llvm::Value *initVal = nullptr;
  llvm::Type *decayArrayType = nullptr;
  std::string inferredTypeName = "";
  if (var->Init) {
    m_CFStack.push_back({varName, nullptr, nullptr, nullptr});
    PhysEntity initEnt = genExpr(var->Init.get());

    // [Fix] Array-to-Pointer Decay Interception
    // Check if RHS is physically an array type that should decay to a pointer
    if (var->HasPointer || var->IsReference) {
      if (var->Init) {
        if (auto *ue = dynamic_cast<const UnaryExpr *>(var->Init.get())) {
          if (ue->Op == TokenType::Star) {
            if (auto *ve = dynamic_cast<const VariableExpr *>(ue->RHS.get())) {
              std::string veName = stripMorphology(ve->Name);
              if (m_Symbols.count(veName)) {
                llvm::Type *t = m_Symbols[veName].soulType;
                if (t && t->isArrayTy()) {
                  decayArrayType = t;
                }
              }
            }
          }
        }
      }
    }

    if (decayArrayType) {

      llvm::Value *arrPtr =
          initEnt.value; // PhysEntity.value gives address due to genUnaryExpr
      llvm::Value *zero =
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0);
      initVal = m_Builder.CreateInBoundsGEP(decayArrayType, arrPtr,
                                            {zero, zero}, "array.decay");
    } else {
      initVal = initEnt.load(m_Builder);
    }

    inferredTypeName = initEnt.typeName;
    m_CFStack.pop_back();
    if (!initVal)
      return nullptr;
  }

  llvm::Type *type = nullptr;
  if (!var->TypeName.empty()) {
    type = resolveType(var->TypeName, var->HasPointer || var->IsReference ||
                                          var->IsUnique || var->IsShared);
  } else if (initVal) {
    type = initVal->getType();
  }

  llvm::Type *elemTy = nullptr;
  std::string soulTypeName = var->TypeName;
  if (!soulTypeName.empty()) {
    // Strip ALL morphology to find the core Soul dimension
    while (!soulTypeName.empty() &&
           (soulTypeName[0] == '^' || soulTypeName[0] == '*' ||
            soulTypeName[0] == '&' || soulTypeName[0] == '~')) {
      soulTypeName = soulTypeName.substr(1);
    }
    while (!soulTypeName.empty() &&
           (soulTypeName.back() == '#' || soulTypeName.back() == '?' ||
            soulTypeName.back() == '!')) {
      soulTypeName.pop_back();
    }
    elemTy = resolveType(soulTypeName, false);
  } else if (initVal) {
    // 1. Prefer Inferred Type from PhysEntity (The "Soul" Type)
    if (!inferredTypeName.empty()) {
      std::string tn = inferredTypeName;
      // Strip morphology to find base element type
      while (!tn.empty() &&
             (tn[0] == '*' || tn[0] == '^' || tn[0] == '&' || tn[0] == '#'))
        tn = tn.substr(1);
      elemTy = resolveType(tn, false);
    }

    // 2. Fallbacks using AST inspection (Legacy/Redundant if 1 works, but kept
    // for safety)
    if (!elemTy) {
      if (auto *ve = dynamic_cast<const VariableExpr *>(var->Init.get())) {
        if (m_Symbols.count(ve->Name))
          elemTy = m_Symbols[ve->Name].soulType;
      } else if (auto *ae =
                     dynamic_cast<const AddressOfExpr *>(var->Init.get())) {
        if (auto *vae =
                dynamic_cast<const VariableExpr *>(ae->Expression.get())) {
          if (m_Symbols.count(vae->Name))
            elemTy = m_Symbols[vae->Name].soulType;
        }
      } else if (auto *allocExpr =
                     dynamic_cast<const AllocExpr *>(var->Init.get())) {
        // auto *p = alloc Point(...) -> elemTy should be Point
        elemTy = resolveType(allocExpr->TypeName, false);
      } else if (auto *newExpr =
                     dynamic_cast<const NewExpr *>(var->Init.get())) {
        elemTy = resolveType(newExpr->Type, false);
      } else if (auto *cast = dynamic_cast<const CastExpr *>(var->Init.get())) {
        std::string tn = cast->TargetType;
        while (!tn.empty() &&
               (tn[0] == '*' || tn[0] == '^' || tn[0] == '&' || tn[0] == '#'))
          tn = tn.substr(1);
        elemTy = resolveType(tn, false);
      } else if (auto *call = dynamic_cast<const CallExpr *>(var->Init.get())) {
        std::string retTypeName;
        if (m_Functions.count(call->Callee)) {
          retTypeName = m_Functions[call->Callee]->ReturnType;
        } else if (m_Externs.count(call->Callee)) {
          retTypeName = m_Externs[call->Callee]->ReturnType;
        }

        if (!retTypeName.empty()) {
          std::string tn = retTypeName;
          while (!tn.empty() &&
                 (tn[0] == '*' || tn[0] == '^' || tn[0] == '&' || tn[0] == '#'))
            tn = tn.substr(1);
          elemTy = resolveType(tn, false);
        }
      } else if (auto *ue = dynamic_cast<const UnaryExpr *>(var->Init.get())) {
        // [Fix] Handle *var for type deduction
        if (ue->Op == TokenType::Star) {
          if (auto *ve = dynamic_cast<const VariableExpr *>(ue->RHS.get())) {
            if (m_Symbols.count(ve->Name))
              elemTy = m_Symbols[ve->Name].soulType;
          }
        }
      } else if (initVal->getType()->isPointerTy()) {
        // Fallback: use the value type itself as elem
        elemTy = initVal->getType();
      }
    }
  }

  if (!elemTy) {
    if (initVal)
      elemTy = initVal->getType();
    else
      elemTy = llvm::Type::getInt32Ty(m_Context);
  }

  // Ensure m_ValueElementTypes is set early
  // m_ValueElementTypes[varName] = elemTy; // LEGACY REMOVED
  // m_ValueTypeNames[varName] = var->TypeName; // LEGACY REMOVED

  // The Form (Identity) is always what resolveType returns for the full name
  if (!type) { // Only try to resolve if type hasn't been determined yet
    if (!var->TypeName.empty()) {
      type = resolveType(var->TypeName, var->HasPointer || var->IsUnique ||
                                            var->IsShared || var->IsReference);
    } else if (elemTy && (var->HasPointer || var->IsReference)) {
      // [Fix] Auto-deduction with pointer modifiers/decorators
      // If we have 'auto *p = ...', we deduced elemTy from initVal, but we
      // need to ensure 'type' is a pointer to elemTy.
      // Additionally, if elemTy is an Array, we must decay it to Pointer to
      // Element.
      llvm::Type *innerTy = elemTy;
      if (innerTy->isArrayTy()) {
        innerTy = innerTy->getArrayElementType();
      }
      type = llvm::PointerType::getUnqual(innerTy);
    }
  }
  if (!type && initVal)
    type = initVal->getType();

  // [Fix] Update the element type map with the FINAL resolved type
  // This ensures that pointers decayed from arrays are registered as pointers
  // to the ELEMENT type (e.g. i32), not the ARRAY type ([N]i32).
  if (decayArrayType) {
    elemTy = decayArrayType->getArrayElementType();
    // m_ValueElementTypes[varName] = elemTy; // LEGACY REMOVED
  }

  // CRITICAL: For Shared variables, ALWAYS use the handle struct { ptr, ptr
  // }, regardless of what resolveType returned. This ensures all Shared
  // variables have consistent memory layout with ref counting support.
  if (var->IsShared) {
    llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
    llvm::Type *refTy =
        llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
    type = llvm::StructType::get(m_Context, {ptrTy, refTy});
  } else if (var->IsUnique && (!type || !type->isPointerTy())) {
    // Unique variables must be pointers, never raw Soul types
    type = llvm::PointerType::getUnqual(elemTy);
  } else if (!type) {
    // Regular variables use the Soul type directly
    type = elemTy;
  }

  if (!type) {
    error(var, "Cannot infer type for variable '" + varName + "'");
    return nullptr;
  }

  if (var->Init && initVal) {
    // Move Semantics for Unique
    if (var->IsUnique) {
      const VariableExpr *ve =
          dynamic_cast<const VariableExpr *>(var->Init.get());
      if (!ve) {
        if (auto *ue = dynamic_cast<const UnaryExpr *>(var->Init.get())) {
          if (ue->Op == TokenType::Caret)
            ve = dynamic_cast<const VariableExpr *>(ue->RHS.get());
        }
      }
      if (ve) {
        // Stripping logic for unique variable lookup could be added here if
        // needed, but m_NamedValues should have stripped keys now.
        std::string veName = stripMorphology(ve->Name);
        if (m_Symbols.count(veName) &&
            m_Symbols[veName].morphology == Morphology::Unique) {
          TokaSymbol &sSym = m_Symbols[veName];
          llvm::Value *s = sSym.allocaPtr;
          if (s && llvm::isa<llvm::AllocaInst>(s))
            m_Builder.CreateStore(
                llvm::Constant::getNullValue(
                    llvm::cast<llvm::AllocaInst>(s)->getAllocatedType()),
                s);
        }
      }
    } else if (var->IsShared) {
      // Shared Semantics: Incref or Promote
      if (initVal->getType()->isStructTy() &&
          initVal->getType()->getStructNumElements() == 2 &&
          initVal->getType()->getStructElementType(0)->isPointerTy() &&
          initVal->getType()->getStructElementType(1)->isPointerTy()) {
        llvm::Value *ref = m_Builder.CreateExtractValue(initVal, 1);
        llvm::Value *c = m_Builder.CreateLoad(llvm::Type::getInt32Ty(m_Context),
                                              ref, "ref_count");
        llvm::Value *inc = m_Builder.CreateAdd(
            c, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
        m_Builder.CreateStore(inc, ref);
      } else {
        // Promote to Shared Handle (Ptr or Value)
        if (initVal->getType()->isPointerTy() &&
            llvm::isa<llvm::ConstantPointerNull>(initVal)) {
          // Null shared pointer initialization
          llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
          llvm::Type *refTy =
              llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
          llvm::StructType *st =
              llvm::StructType::get(m_Context, {ptrTy, refTy});
          llvm::Value *u = llvm::UndefValue::get(st);
          initVal = m_Builder.CreateInsertValue(
              m_Builder.CreateInsertValue(
                  u,
                  llvm::ConstantPointerNull::get(
                      llvm::cast<llvm::PointerType>(ptrTy)),
                  0),
              llvm::ConstantPointerNull::get(
                  llvm::cast<llvm::PointerType>(refTy)),
              1);
          type = st;
        } else {
          llvm::Function *mallocFn = m_Module->getFunction("malloc");
          if (!mallocFn) {
            std::vector<llvm::Type *> args;
            args.push_back(llvm::Type::getInt64Ty(m_Context));
            llvm::FunctionType *ft =
                llvm::FunctionType::get(m_Builder.getPtrTy(), args, false);
            mallocFn = llvm::Function::Create(
                ft, llvm::Function::ExternalLinkage, "malloc", m_Module.get());
          }
          if (mallocFn) {
            // 1. Allocate RefCount
            llvm::Value *rcSize =
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), 4);
            llvm::Value *refPtr = m_Builder.CreateCall(mallocFn, rcSize);
            refPtr = m_Builder.CreateBitCast(
                refPtr, llvm::PointerType::getUnqual(
                            llvm::Type::getInt32Ty(m_Context)));
            m_Builder.CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1),
                refPtr);

            // 2. Prepare Data Pointer
            llvm::Value *dataPtr = nullptr;
            if (initVal->getType()->isPointerTy()) {
              // Already a pointer, use it (assume ownership transfer or raw ->
              // shared promotion)
              dataPtr = m_Builder.CreateBitCast(
                  initVal, llvm::PointerType::getUnqual(elemTy));
            } else {
              // Value Type -> Allocate and Copy
              llvm::DataLayout dl(m_Module.get());
              uint64_t dataSz = dl.getTypeAllocSize(elemTy);
              llvm::Value *valSize = llvm::ConstantInt::get(
                  llvm::Type::getInt64Ty(m_Context), dataSz);
              dataPtr = m_Builder.CreateCall(mallocFn, valSize);
              dataPtr = m_Builder.CreateBitCast(
                  dataPtr, llvm::PointerType::getUnqual(elemTy));
              m_Builder.CreateStore(initVal, dataPtr);
            }

            // 3. Create Handle
            llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
            llvm::Type *refTy =
                llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
            llvm::StructType *st =
                llvm::StructType::get(m_Context, {ptrTy, refTy});
            llvm::Value *u = llvm::UndefValue::get(st);
            initVal = m_Builder.CreateInsertValue(
                m_Builder.CreateInsertValue(u, dataPtr, 0), refPtr, 1);
            type = st;
          }
        }
      }
    }
  }

  llvm::AllocaInst *alloca = m_Builder.CreateAlloca(type, nullptr, varName);

  TokaSymbol sym;
  sym.allocaPtr = alloca;
  fillSymbolMetadata(sym, var->TypeName, var->HasPointer, var->IsUnique,
                     var->IsShared, var->IsReference, var->IsMutable,
                     var->IsNullable || var->IsPointerNullable, elemTy);
  sym.isRebindable = var->IsRebindable;
  sym.isContinuous =
      (elemTy && elemTy->isArrayTy()) ||
      (dynamic_cast<const AllocExpr *>(var->Init.get()) &&
       dynamic_cast<const AllocExpr *>(var->Init.get())->IsArray);
  m_Symbols[varName] = sym;

  m_NamedValues[varName] = alloca;
  // m_ValueTypes[varName] = type; // LEGACY REMOVED
  // LEGACY MAPS REMOVED
  // m_ValueIsUnique[varName] = var->IsUnique;
  // m_ValueIsShared[varName] = var->IsShared;
  // m_ValueIsReference[varName] = var->IsReference;
  // m_ValueIsMutable[varName] = var->IsMutable;

  if (initVal) {
    if (initVal->getType() != type) {
      if (initVal->getType()->isPointerTy() && type->isPointerTy()) {
        initVal = m_Builder.CreateBitCast(initVal, type);
      } else if (initVal->getType()->isPointerTy() && !type->isPointerTy()) {
        initVal = m_Builder.CreateLoad(type, initVal);
      } else if (initVal->getType()->isIntegerTy() && type->isIntegerTy()) {
        initVal = m_Builder.CreateIntCast(initVal, type, true);
      } else {
        if (initVal->getType() != type) {
          std::string s1 = "Unknown", s2 = "Unknown";
          if (type) {
            llvm::raw_string_ostream os1(s1);
            type->print(os1);
          }
          if (initVal) {
            llvm::raw_string_ostream os2(s2);
            initVal->getType()->print(os2);
          }
          error(var, "Internal Error: Type mismatch in VariableDecl despite "
                     "Sema: Expected " +
                         s1 + ", Got " + s2);
          return nullptr;
        }
      }

      if (initVal->getType() != type) {
        std::string s1 = "Unknown", s2 = "Unknown";
        if (type) {
          llvm::raw_string_ostream os1(s1);
          type->print(os1);
        }
        if (initVal) {
          llvm::raw_string_ostream os2(s2);
          initVal->getType()->print(os2);
        }

        error(var, "Internal Error: Type mismatch in VariableDecl despite "
                   "Sema: Expected " +
                       s1 + ", Got " + s2);
        return nullptr;
      }
    }
  }

  m_Builder.CreateStore(initVal, alloca);

  // Automatic Drop Registration
  if (!m_ScopeStack.empty()) {
    std::string typeName = m_ValueTypeNames[varName];
    // If empty (auto with no Sema update?), try m_ValueElementTypes or
    // inferred?

    std::string dropFunc = "";
    bool hasDrop = false;

    if (!typeName.empty()) {
      // Simple mangling check: Type_encap_drop
      // Need to strip morphology
      std::string base = typeName;
      while (!base.empty() && (base[0] == '^' || base[0] == '*' ||
                               base[0] == '&' || base[0] == '~'))
        base = base.substr(1);

      std::string tryName = base + "_encap_drop";
      if (m_Module->getFunction(tryName)) {
        hasDrop = true;
        dropFunc = tryName;
      } else {
        std::string altName = "encap_" + base + "_drop";
        if (m_Module->getFunction(altName)) {
          hasDrop = true;
          dropFunc = altName;
        }
      }
    }

    if (!var->IsReference &&
        (!var->HasPointer || var->IsUnique ||
         var->IsShared)) { // Do not auto-drop references or raw pointers!
      VariableScopeInfo info;
      info.Name = varName;
      info.Alloca = alloca;
      info.IsUniquePointer = var->IsUnique;
      info.IsShared = var->IsShared;
      info.HasDrop = hasDrop;
      info.DropFunc = dropFunc;
      m_ScopeStack.back().push_back(info);
    }
    // Debug Lookup
    // llvm::errs() << "DEBUG: genFreeStmt lookup varName='" << varName
    //              << "' count=" << m_ValueTypeNames.count(varName) << " val='"
    //              << m_ValueTypeNames[varName] << "'\n";
  }
  // Debug
  // llvm::errs() << "DEBUG: genVariableDecl varName=" << varName
  //              << " TypeName=" << m_ValueTypeNames[varName] << "\n";

  return nullptr;
}

llvm::Value *CodeGen::genDestructuringDecl(const DestructuringDecl *dest) {
  llvm::Value *initVal = genExpr(dest->Init.get()).load(m_Builder);
  if (!initVal)
    return nullptr;

  llvm::Type *srcTy = initVal->getType();
  if (!srcTy->isStructTy()) {
    error(dest, "Positional destructuring requires a struct or tuple type");
    return nullptr;
  }

  auto *st = llvm::cast<llvm::StructType>(srcTy);
  for (size_t i = 0; i < dest->Variables.size(); ++i) {
    if (i >= st->getNumElements()) {
      error(dest, "Too many variables in destructuring");
      break;
    }

    const auto &v = dest->Variables[i];
    std::string vName = stripMorphology(v.Name);

    llvm::Value *val = m_Builder.CreateExtractValue(initVal, i, vName);
    llvm::Type *ty = val->getType();

    llvm::AllocaInst *alloca = m_Builder.CreateAlloca(ty, nullptr, vName);
    m_Builder.CreateStore(val, alloca);

    m_NamedValues[vName] = alloca;
    // m_ValueTypes[vName] = ty; // LEGACY REMOVED
    // m_ValueElementTypes[vName] = ty; // fallback for basic types // LEGACY
    // REMOVED LEGACY MAPS REMOVED m_ValueIsMutable[vName] = v.IsMutable;
    // m_ValueIsNullable[vName] = v.IsNullable;

    // [Fix] Register Type Name for Lookup (Auto Deduction)
    // We attempt to extract the user-written type from AST (AllocExpr/NewExpr).
    std::string deducedType = "";
    Expr *rawInit = dest->Init.get();

    // Peel UnsafeExpr wrapper
    if (auto *ue = dynamic_cast<UnsafeExpr *>(rawInit)) {
      rawInit = ue->Expression.get();
    }

    if (auto *ae = dynamic_cast<AllocExpr *>(rawInit)) {
      deducedType = ae->TypeName; // "Data"
    } else if (auto *ne = dynamic_cast<NewExpr *>(rawInit)) {
      deducedType = ne->Type; // "Data"
    }

    // Strip decorators if present
    deducedType = stripMorphology(deducedType);

    llvm::errs() << "DEBUG: genDestructuringDecl vName=" << vName
                 << " deducedType='" << deducedType << "'\n";

    if (!deducedType.empty()) {
      m_ValueTypeNames[vName] = deducedType;
    }

    TokaSymbol sym;
    sym.allocaPtr = alloca;
    // For destructuring, metadata is often already flattened
    fillSymbolMetadata(sym, "", false, false, false, false, v.IsMutable,
                       v.IsNullable, ty);
    sym.isRebindable = false;
    sym.isContinuous = ty->isArrayTy();
    m_Symbols[vName] = sym;

    if (!m_ScopeStack.empty()) {
      m_ScopeStack.back().push_back({v.Name, alloca, false, false});
    }
  }
  return nullptr;
}

void CodeGen::genGlobal(const Stmt *stmt) {
  if (auto *var = dynamic_cast<const VariableDecl *>(stmt)) {
    llvm::Value *initVal = nullptr;
    if (var->Init) {
      // Try resolving type hint first
      llvm::Type *hintType = nullptr;
      if (!var->TypeName.empty()) {
        hintType = resolveType(var->TypeName, var->HasPointer);
      }

      // Try compile-time constant generation first (Critical for Anonymous
      // Records)
      if (auto *c = genConstant(var->Init.get(), hintType)) {
        initVal = c;
      } else {
        // Fallback to legacy genExpr (might crash if it uses instructions)
        initVal = genExpr(var->Init.get()).load(m_Builder);
      }
    }

    llvm::Type *type = nullptr;
    if (!var->TypeName.empty()) {
      type = resolveType(var->TypeName, var->HasPointer);
    } else if (initVal) {
      type = initVal->getType();
    }

    if (!type) {
      // Potentially resolve via initVal if TypeName is empty
      if (initVal) {
        type = initVal->getType();
      }

      if (!type) {
        std::cerr << "DEBUG: genGlobal: Could not resolve type for '"
                  << var->Name << "' (TypeName: '" << var->TypeName << "')\n";
        type = llvm::Type::getInt32Ty(m_Context);
      }
    }

    auto *globalVar = new llvm::GlobalVariable(
        *m_Module, type, false, llvm::GlobalValue::ExternalLinkage, nullptr,
        var->Name);

    if (initVal) {
      if (auto *constInit = llvm::dyn_cast<llvm::Constant>(initVal)) {
        globalVar->setInitializer(constInit);
      } else {
        std::cerr << "DEBUG: genGlobal: Non-constant initializer for '"
                  << var->Name << "'\n";
        globalVar->setInitializer(llvm::ConstantInt::get(type, 0));
      }
    } else {
      globalVar->setInitializer(llvm::ConstantInt::get(type, 0));
    }

    m_NamedValues[var->Name] = globalVar;
    // m_ValueTypes[var->Name] = type; // LEGACY REMOVED

    TokaSymbol sym;
    sym.allocaPtr = globalVar;
    fillSymbolMetadata(sym, var->TypeName, var->HasPointer, var->IsUnique,
                       var->IsShared, var->IsReference, var->IsMutable,
                       var->IsNullable || var->IsPointerNullable, type);
    sym.isRebindable = var->IsRebindable;
    sym.isContinuous = type->isArrayTy();
    m_Symbols[var->Name] = sym;
  } else {
    // We could support global destructuring here, but for now just skip or
    // error
    error(dynamic_cast<const ASTNode *>(stmt),
          "Global destructuring not yet supported");
  }
}

void CodeGen::genExtern(const ExternDecl *ext) {
  std::vector<llvm::Type *> argTypes;
  for (const auto &arg : ext->Args) {
    llvm::Type *t = resolveType(arg.Type, arg.HasPointer || arg.IsReference);
    argTypes.push_back(t);
  }
  llvm::Type *retType = resolveType(ext->ReturnType, false);
  llvm::FunctionType *ft =
      llvm::FunctionType::get(retType, argTypes, ext->IsVariadic);

  std::string llvmName = ext->Name;
  if (llvmName.size() > 5 && llvmName.substr(0, 5) == "libc_") {
    llvmName = llvmName.substr(5);
  }

  llvm::Function::Create(ft, llvm::Function::ExternalLinkage, llvmName,
                         m_Module.get());
}

void CodeGen::genShape(const ShapeDecl *sh) {
  llvm::StructType *st = llvm::StructType::create(m_Context, sh->Name);
  m_Shapes[sh->Name] = sh;
  m_StructTypes[sh->Name] = st;
  m_TypeToName[st] = sh->Name;

  std::vector<llvm::Type *> body;
  llvm::DataLayout DL(m_Module.get());

  if (sh->Kind == ShapeKind::Struct || sh->Kind == ShapeKind::Tuple) {
    std::vector<std::string> fieldNames;
    for (const auto &member : sh->Members) {
      body.push_back(
          resolveType(member.Type, member.HasPointer || member.IsUnique ||
                                       member.IsShared || member.IsReference));
      fieldNames.push_back(member.Name);
    }
    st->setBody(body, sh->IsPacked);
    m_StructFieldNames[sh->Name] = fieldNames;
  } else if (sh->Kind == ShapeKind::Array) {
    llvm::Type *elemTy = resolveType(sh->Members[0].Type, false);
    llvm::Type *arrTy = llvm::ArrayType::get(elemTy, sh->ArraySize);
    // For Array shapes, we wrap them in a struct for consistent GEP/naming if
    // needed, but usually Toka treats them as ArrayType. Let's wrap in a
    // struct so it's a named type.
    body.push_back(arrTy);
    st->setBody(body, sh->IsPacked);
  } else if (sh->Kind == ShapeKind::Union) {
    // Bare Union: find max size and alignment
    uint64_t maxSize = 0;
    uint64_t maxAlign = 1;
    for (const auto &member : sh->Members) {
      llvm::Type *t = resolveType(member.Type, false);
      if (!t)
        continue;
      maxSize =
          std::max(maxSize, (uint64_t)DL.getTypeAllocSize(t).getFixedValue());
      maxAlign = std::max(maxAlign, (uint64_t)DL.getABITypeAlign(t).value());
    }
    // Model as [maxSize x i8]
    body.push_back(
        llvm::ArrayType::get(llvm::Type::getInt8Ty(m_Context), maxSize));
    st->setBody(body, sh->IsPacked);
  } else if (sh->Kind == ShapeKind::Enum) {
    // Tagged Union: { i8 tag, [Payload] }
    uint64_t maxPayloadSize = 0;
    for (const auto &variant : sh->Members) {
      uint64_t variantSize = 0;
      if (!variant.SubMembers.empty()) {
        std::vector<llvm::Type *> fieldTypes;
        for (const auto &field : variant.SubMembers) {
          fieldTypes.push_back(resolveType(field.Type, false));
        }
        // Use packed layout to estimate consistent payload size
        llvm::StructType *st =
            llvm::StructType::get(m_Context, fieldTypes, true);
        variantSize = DL.getTypeAllocSize(st).getFixedValue();
      } else if (!variant.Type.empty()) {
        llvm::Type *t = resolveType(variant.Type, false);
        if (t)
          variantSize = DL.getTypeAllocSize(t).getFixedValue();
      }
      maxPayloadSize = std::max(maxPayloadSize, variantSize);
    }
    body.push_back(llvm::Type::getInt8Ty(m_Context)); // Tag
    if (maxPayloadSize > 0) {
      body.push_back(llvm::ArrayType::get(llvm::Type::getInt8Ty(m_Context),
                                          maxPayloadSize));
    }
    st->setBody(body, sh->IsPacked);
  }
}

void toka::CodeGen::genImpl(const toka::ImplDecl *decl, bool declOnly) {
  if (decl->TraitName == "encap") {
    // Allow generation for hybrid encap trait
  }
  m_CurrentSelfType = decl->TypeName;
  std::set<std::string> implementedMethods;

  // Methods defined in Impl block
  for (const auto &method : decl->Methods) {
    std::string mangledName;
    if (!decl->TraitName.empty()) {
      mangledName = decl->TraitName + "_" + decl->TypeName + "_" + method->Name;
    } else {
      mangledName = decl->TypeName + "_" + method->Name;
    }
    genFunction(method.get(), mangledName, declOnly);
    implementedMethods.insert(method->Name);
  }

  // Handle Trait Defaults and Missing Methods
  if (!decl->TraitName.empty()) {
    const TraitDecl *trait = nullptr;
    if (m_Traits.count(decl->TraitName)) {
      trait = m_Traits[decl->TraitName];
    }

    if (trait) {
      for (const auto &method : trait->Methods) {
        if (implementedMethods.count(method->Name))
          continue;

        if (method->Body) {
          // Generate default implementation
          std::string mangledName =
              decl->TraitName + "_" + decl->TypeName + "_" + method->Name;
          genFunction(method.get(), mangledName, declOnly);
        } else {
          error(decl, "Missing implementation for method '" + method->Name +
                          "' of trait '" + decl->TraitName + "'");
        }
      }
    } else {
      error(decl, "Trait '" + decl->TraitName + "' not found");
    }

    // Generate VTable
    if (trait && !declOnly) {
      std::vector<llvm::Constant *> vtableMethods;
      llvm::Type *voidPtrTy =
          llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
      for (const auto &method : trait->Methods) {
        std::string implFuncName =
            decl->TraitName + "_" + decl->TypeName + "_" + method->Name;
        llvm::Function *f = m_Module->getFunction(implFuncName);
        if (f) {
          vtableMethods.push_back(llvm::ConstantExpr::getBitCast(f, voidPtrTy));
        } else {
          vtableMethods.push_back(llvm::Constant::getNullValue(voidPtrTy));
        }
      }

      if (!vtableMethods.empty()) {
        llvm::ArrayType *arrTy =
            llvm::ArrayType::get(voidPtrTy, vtableMethods.size());
        llvm::Constant *init = llvm::ConstantArray::get(arrTy, vtableMethods);
        std::string vtableName =
            "_VTable_" + decl->TypeName + "_" + decl->TraitName;
        new llvm::GlobalVariable(*m_Module, arrTy, true,
                                 llvm::GlobalValue::ExternalLinkage, init,
                                 vtableName);
      }
    }
  }

  m_CurrentSelfType = "";
}

PhysEntity toka::CodeGen::genMethodCall(const toka::MethodCallExpr *expr) {
  llvm::Value *objVal = genExpr(expr->Object.get()).load(m_Builder);
  if (!objVal)
    return nullptr;

  // --- Dynamic Dispatch (dyn @Trait) ---
  std::string dynamicTypeName = "";
  if (auto *ve = dynamic_cast<const VariableExpr *>(expr->Object.get())) {
    std::string varName = stripMorphology(ve->Name);
    // [Fix] Use Symbol Table typeName instead of legacy m_ValueTypeNames
    if (m_Symbols.count(varName)) {
      std::string vType = m_Symbols[varName].typeName;
      // vType is e.g. *Data or Data
      if (!vType.empty()) {
        if (vType[0] == '*') {
          dynamicTypeName = vType.substr(1); // Peel pointer
        } else {
          dynamicTypeName = vType; // Already base type (e.g. "Data")
        }
      }
    }
  }

  if (!dynamicTypeName.empty()) {
    std::string traitName = "";
    if (dynamicTypeName.find("dyn @") == 0)
      traitName = dynamicTypeName.substr(5);
    else if (dynamicTypeName.find("dyn@") == 0)
      traitName = dynamicTypeName.substr(4);

    if (!traitName.empty() && m_Traits.count(traitName)) {
      const TraitDecl *trait = m_Traits[traitName];
      int methodIdx = -1;
      const FunctionDecl *methodDecl = nullptr;

      for (size_t i = 0; i < trait->Methods.size(); ++i) {
        if (trait->Methods[i]->Name == expr->Method) {
          methodIdx = i;
          methodDecl = trait->Methods[i].get();
          break;
        }
      }

      if (methodIdx != -1) {
        // 1. Extract Data and VTable
        llvm::Value *dataPtr =
            m_Builder.CreateExtractValue(objVal, 0, "dyn_data");
        llvm::Value *vtablePtr =
            m_Builder.CreateExtractValue(objVal, 1, "dyn_vtable");

        // 2. Load Function Pointer from VTable
        llvm::Type *voidPtrTy =
            llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
        llvm::Type *vtableArrayTy =
            llvm::PointerType::getUnqual(voidPtrTy); // i8**

        llvm::Value *vtableArray =
            m_Builder.CreateBitCast(vtablePtr, vtableArrayTy);
        llvm::Value *funcPtrAddr =
            m_Builder.CreateConstGEP1_32(voidPtrTy, vtableArray, methodIdx);
        llvm::Value *voidFuncPtr = m_Builder.CreateLoad(voidPtrTy, funcPtrAddr);

        // 3. Prepare Arguments
        std::vector<llvm::Value *> args;
        std::vector<llvm::Type *> argTypes;

        // Self (dataPtr)
        args.push_back(dataPtr); // i8* passed to opaque ptr
        argTypes.push_back(llvm::PointerType::getUnqual(m_Context));

        for (auto &arg : expr->Args) {
          llvm::Value *av = genExpr(arg.get()).load(m_Builder);
          args.push_back(av);
          argTypes.push_back(av->getType());
        }

        // 4. Determine Return Type
        llvm::Type *retTy = resolveType(methodDecl->ReturnType, false);
        llvm::FunctionType *ft =
            llvm::FunctionType::get(retTy, argTypes, false);

        // 5. Call
        return m_Builder.CreateCall(ft, voidFuncPtr, args);
      }
    }
  }
  // --- End Dynamic Dispatch ---

  llvm::Type *ty = objVal->getType();
  llvm::Type *structTy = nullptr;

  if (ty->isStructTy()) {
    structTy = ty;
  } else {
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(objVal)) {
      if (ai->getAllocatedType()->isStructTy())
        structTy = ai->getAllocatedType();
    } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objVal)) {
      if (gep->getResultElementType()->isStructTy())
        structTy = gep->getResultElementType();
    }

    if (!structTy) {
      if (auto *ve = dynamic_cast<const VariableExpr *>(expr->Object.get())) {
        if (m_Symbols.count(ve->Name)) {
          structTy = m_Symbols[ve->Name].soulType;
        }
      }
    }
  }

  if (!structTy) {
    if (auto *ne = dynamic_cast<const NewExpr *>(expr->Object.get())) {
      structTy = resolveType(ne->Type, false);
    }
  }

  std::string typeName;
  if (structTy && m_TypeToName.count(structTy)) {
    typeName = m_TypeToName[structTy];
  }

  if (typeName.empty()) {
    error(expr, "Cannot determine type for method call '" + expr->Method + "'");
    return nullptr;
  }

  std::string funcName = typeName + "_" + expr->Method;
  llvm::Function *callee = m_Module->getFunction(funcName);

  // Check Traits
  if (!callee) {
    for (auto const &[traitName, traitDecl] : m_Traits) {
      std::string traitFunc = traitName + "_" + typeName + "_" + expr->Method;
      callee = m_Module->getFunction(traitFunc);
      if (callee)
        break;
    }
  }

  // Explicit check for @encap (hybrid trait)
  if (!callee) {
    std::string encapFunc = "encap_" + typeName + "_" + expr->Method;
    callee = m_Module->getFunction(encapFunc);
  }

  if (!callee) {
    error(expr, "Method '" + expr->Method + "' not found for type '" +
                    typeName + "' (Mangled: " + funcName + ")");
    return nullptr;
  }

  // Retrieve FunctionDecl to check for Mutability (Pass-By-Reference)
  const FunctionDecl *fd = nullptr;
  if (m_Functions.count(funcName)) {
    fd = m_Functions[funcName];
  }

  std::vector<llvm::Value *> args;

  // 1. Handle Self (Argument 0)
  // Check if self is mutable (requires pointer)
  bool selfIsMutable = false;
  if (fd && !fd->Args.empty()) {
    // Arg 0 is self
    if (fd->Args[0].IsMutable)
      selfIsMutable = true;
  }
  // Fallback: Check LLVM Arg Type
  if (!fd && callee->arg_size() > 0 &&
      callee->getArg(0)->getType()->isPointerTy()) {
    selfIsMutable = true;
  }

  llvm::Value *finalObjVal = objVal;
  bool targetExpectsPtr =
      (callee->arg_size() > 0 && callee->getArg(0)->getType()->isPointerTy());

  if (selfIsMutable || targetExpectsPtr) {
    // Must pass address
    llvm::Value *addr = genAddr(expr->Object.get());
    if (addr) {
      finalObjVal = addr;
    } else {
      // Fallback for R-Values: Create temporary alloca
      // Only if objVal is not already a pointer
      if (!objVal->getType()->isPointerTy()) {
        llvm::AllocaInst *tmp = m_Builder.CreateAlloca(objVal->getType());
        m_Builder.CreateStore(objVal, tmp);
        finalObjVal = tmp;
      }
    }
  }

  // Type Check Self
  if (callee->arg_size() > 0) {
    llvm::Type *targetTy = callee->getArg(0)->getType();
    if (finalObjVal->getType() != targetTy) {
      if (finalObjVal->getType()->isPointerTy() && !targetTy->isPointerTy()) {
        // Implicit Dereference (Pass Reference as Value - Rare for self but
        // possible)
        finalObjVal = m_Builder.CreateLoad(targetTy, finalObjVal);
      }
    }
  }
  args.push_back(finalObjVal);

  // 2. Handle Arguments
  for (size_t i = 0; i < expr->Args.size(); ++i) {
    bool isMutable = false;
    // Arg i maps to fd->Args[i+1]
    if (fd && i + 1 < fd->Args.size()) {
      isMutable = fd->Args[i + 1].IsMutable;
    }

    llvm::Value *argVal = nullptr;
    if (isMutable) {
      argVal = genAddr(expr->Args[i].get());
      if (!argVal) {
        // R-Value fallback
        llvm::Value *rval = genExpr(expr->Args[i].get()).load(m_Builder);
        if (!rval)
          return nullptr;

        // Don't double-alloc if already pointer?
        // If expects mutable (Pointer) and we have Pointer R-value allow it?
        // Usually IsMutable expects L-Value.
        // For safety, store R-value in temp.
        llvm::AllocaInst *tmp = m_Builder.CreateAlloca(rval->getType());
        m_Builder.CreateStore(rval, tmp);
        argVal = tmp;
      }
    } else {
      argVal = genExpr(expr->Args[i].get()).load(m_Builder);

      // Implicit By-Ref Fix for Method Arguments
      if (argVal && callee->arg_size() > i + 1) {
        llvm::Type *paramTy = callee->getFunctionType()->getParamType(i + 1);
        if (paramTy->isPointerTy() && argVal->getType()->isStructTy()) {
          llvm::AllocaInst *tmp = m_Builder.CreateAlloca(
              argVal->getType(), nullptr, "arg_byref_tmp");
          m_Builder.CreateStore(argVal, tmp);
          argVal = tmp;
        }
      }
    }

    if (!argVal)
      return nullptr;

    // Auto-cast for primitives
    // ... (Existing cast logic could be added here if needed) ...

    args.push_back(argVal);
  }

  llvm::Value *retVal = m_Builder.CreateCall(callee, args);
  std::string retTypeName = "";
  if (fd)
    retTypeName = fd->ReturnType;

  return PhysEntity(retVal, retTypeName, retVal->getType(), false);
}

void CodeGen::fillSymbolMetadata(TokaSymbol &sym, const std::string &typeStr,
                                 bool hasPointer, bool isUnique, bool isShared,
                                 bool isReference, bool isMutable,
                                 bool isNullable, llvm::Type *allocaElemTy) {
  sym.indirectionLevel = 0;
  sym.typeName =
      typeStr; // [Fix] Store original type string for legacy/dynamic logic
  std::string ts = typeStr;

  // 1. Peel recursive indirection prefixes
  while (!ts.empty() && (ts[0] == '*' || ts[0] == '^' || ts[0] == '~')) {
    sym.indirectionLevel++;
    ts = ts.substr(1);
  }

  // 2. Determine Addressing Mode
  if (isReference) {
    sym.mode = AddressingMode::Reference;
    sym.indirectionLevel = 1;
  } else if (hasPointer || isUnique || isShared || sym.indirectionLevel > 0) {
    sym.mode = AddressingMode::Pointer;
    if (sym.indirectionLevel == 0)
      sym.indirectionLevel = 1;
  } else {
    sym.mode = AddressingMode::Direct;
  }

  // 3. Extract Elemental Soul Type (the 'Meat')
  sym.soulType = resolveType(ts, false);
  if (!sym.soulType)
    sym.soulType = allocaElemTy;

  // 4. Morphology (Ownership/Cleanup)
  if (isUnique)
    sym.morphology = Morphology::Unique;
  else if (isShared)
    sym.morphology = Morphology::Shared;
  else if (hasPointer)
    sym.morphology = Morphology::Raw;
  else
    sym.morphology = Morphology::None;

  // 5. Semantic flags
  sym.isMutable = isMutable;
  sym.isNullable = isNullable;
  // Note: isRebindable is usually set separately based on '#' token presence
  // but it's often linked to morphology in declarations.
}

llvm::Type *CodeGen::resolveType(const std::string &baseType, bool hasPointer) {
  llvm::Type *type = nullptr;
  if (baseType.empty())
    return nullptr;

  if (baseType == "Self") {
    if (m_CurrentSelfType.empty()) {
      // Should not happen if Parser checks context, but for safety in CodeGen
      return nullptr;
    }
    return resolveType(m_CurrentSelfType, hasPointer);
  }

  // Check aliases first
  if (m_TypeAliases.count(baseType)) {
    // std::cerr << "DEBUG: resolveType: Found Alias: " << baseType << " -> "
    // << m_TypeAliases[baseType] << "\n";
    return resolveType(m_TypeAliases[baseType], hasPointer);
  }

  // Handle 'shape' keyword (e.g. shape(u8, u8))
  if (baseType.size() > 5 && baseType.substr(0, 5) == "shape") {
    return resolveType(baseType.substr(5), hasPointer);
  }

  // Handle Dynamic Traits (dyn @Trait)
  if (baseType.size() >= 4 && baseType.substr(0, 3) == "dyn") {
    // Fat Pointer: { void* data, void* vtable }
    llvm::Type *voidPtr =
        llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
    return llvm::StructType::get(m_Context, {voidPtr, voidPtr});
  }

  // Handle Shared Pointers (~Type): { T*, i32* }
  if (baseType.size() > 1 && baseType[0] == '~') {
    llvm::Type *elemTy = resolveType(baseType.substr(1), false);
    llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
    llvm::Type *refCountTy =
        llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
    return llvm::StructType::get(m_Context, {ptrTy, refCountTy});
  }

  // Handle raw pointer types (e.g. *i32, *void) AND managed
  // pointers (^Type)
  if (baseType.size() > 1 && (baseType[0] == '*' || baseType[0] == '^' ||
                              baseType[0] == '~' || baseType[0] == '&')) {
    size_t offset = 1;
    if (offset < baseType.size()) {
      char next = baseType[offset];
      if (next == '#' || next == '?' || next == '!') {
        offset++;
      }
    }
    llvm::Type *elemTy = resolveType(baseType.substr(offset), false);
    if (!elemTy)
      return nullptr;
    return llvm::PointerType::getUnqual(elemTy);
  }

  if (baseType[0] == '[') {
    // Array: [T; N]
    size_t lastSemi = baseType.find_last_of(';');
    if (lastSemi != std::string::npos) {
      std::string elemTyStr = baseType.substr(1, lastSemi - 1);
      std::string countStr =
          baseType.substr(lastSemi + 1, baseType.size() - lastSemi - 2);
      llvm::Type *elemTy = resolveType(elemTyStr, false);
      if (!elemTy)
        return nullptr;
      uint64_t count = std::stoull(countStr);
      type = llvm::ArrayType::get(elemTy, count);
    }
  } else if (baseType[0] == '(') {
    // Tuple: (T1, T2, ...)
    std::vector<llvm::Type *> elemTypes;
    std::string content = baseType.substr(1, baseType.size() - 2);
    // Very simple split by comma, not perfect for nested but works
    // for now
    size_t start = 0;
    int depth = 0;
    for (size_t i = 0; i < content.size(); ++i) {
      if (content[i] == '(' || content[i] == '[')
        depth++;
      else if (content[i] == ')' || content[i] == ']')
        depth--;
      else if (content[i] == ',' && depth == 0) {
        std::string elemStr = content.substr(start, i - start);
        // Trim
        elemStr.erase(0, elemStr.find_first_not_of(" \t"));
        elemStr.erase(elemStr.find_last_not_of(" \t") + 1);
        llvm::Type *et = resolveType(elemStr, false);
        if (et)
          elemTypes.push_back(et);
        else {
          // If one element fails, the whole tuple is invalid
          return nullptr;
        }
        start = i + 1;
      }
    }
    if (start < content.size()) {
      std::string elemStr = content.substr(start);
      elemStr.erase(0, elemStr.find_first_not_of(" \t"));
      elemStr.erase(elemStr.find_last_not_of(" \t") + 1);
      llvm::Type *et = resolveType(elemStr, false);
      if (et)
        elemTypes.push_back(et);
      else
        return nullptr;
    }
    if (elemTypes.empty())
      return nullptr;
    type = llvm::StructType::get(m_Context, elemTypes);

    // Generate canonical baseType for registration (no spaces)
    std::string canonical = "(";
    for (size_t i = 0; i < elemTypes.size(); ++i) {
      if (i > 0)
        canonical += ",";
      std::string s;
      llvm::raw_string_ostream os(s);
      elemTypes[i]->print(os);
      canonical += os.str();
    }
    canonical += ")";

    // Register tuple fields
    m_TypeToName[type] = canonical;
    std::vector<std::string> fields;
    for (size_t i = 0; i < elemTypes.size(); ++i)
      fields.push_back(std::to_string(i));
    m_StructFieldNames[canonical] = fields;
    m_StructTypes[canonical] = llvm::cast<llvm::StructType>(type);
  } else if (baseType == "bool" || baseType == "i1")
    type = llvm::Type::getInt1Ty(m_Context);
  else if (baseType == "i8" || baseType == "u8" || baseType == "char" ||
           baseType == "byte")
    type = llvm::Type::getInt8Ty(m_Context);
  else if (baseType == "i16" || baseType == "u16")
    type = llvm::Type::getInt16Ty(m_Context);
  else if (baseType == "i32" || baseType == "u32" || baseType == "int")
    type = llvm::Type::getInt32Ty(m_Context);
  else if (baseType == "i64" || baseType == "u64" || baseType == "long")
    type = llvm::Type::getInt64Ty(m_Context);
  else if (baseType == "f32" || baseType == "float")
    type = llvm::Type::getFloatTy(m_Context);
  else if (baseType == "f64" || baseType == "double")
    type = llvm::Type::getDoubleTy(m_Context);
  else if (baseType == "str")
    type = llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
  else if (baseType == "void")
    type = llvm::Type::getVoidTy(m_Context);
  else if (baseType == "ptr")
    type = llvm::PointerType::getUnqual(m_Context);
  else if (m_StructTypes.count(baseType))
    type = m_StructTypes[baseType];
  else if (m_Shapes.count(baseType))
    type = m_StructTypes[baseType];
  else if (baseType == "unknown") {
    return nullptr;
  } else {
    // std::cerr << "CodeGen Debug: resolveType failed for '" << baseType <<
    // "'\n";
    return nullptr;
  }

  if (hasPointer && type)
    return llvm::PointerType::getUnqual(type);
  return type;
}

llvm::Type *CodeGen::getLLVMType(std::shared_ptr<Type> type) {
  if (!type) {
    return llvm::Type::getVoidTy(m_Context);
  }

  // Handle Primitives
  if (type->typeKind == Type::Primitive) {
    auto prim = std::static_pointer_cast<PrimitiveType>(type);
    if (prim->Name == "i32" || prim->Name == "u32" || prim->Name == "int")
      return llvm::Type::getInt32Ty(m_Context);
    if (prim->Name == "i64" || prim->Name == "u64" || prim->Name == "long" ||
        prim->Name == "usize")
      return llvm::Type::getInt64Ty(m_Context);
    if (prim->Name == "i8" || prim->Name == "u8" || prim->Name == "byte" ||
        prim->Name == "char")
      return llvm::Type::getInt8Ty(m_Context);
    if (prim->Name == "i16" || prim->Name == "u16")
      return llvm::Type::getInt16Ty(m_Context);
    if (prim->Name == "bool" || prim->Name == "i1")
      return llvm::Type::getInt1Ty(m_Context); // i1
    if (prim->Name == "f32" || prim->Name == "float")
      return llvm::Type::getFloatTy(m_Context);
    if (prim->Name == "f64" || prim->Name == "double")
      return llvm::Type::getDoubleTy(m_Context);
    if (prim->Name == "void")
      return llvm::Type::getVoidTy(m_Context);
    if (prim->Name == "str")
      return llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
  }

  // Handle Void
  if (type->typeKind == Type::Void) {
    return llvm::Type::getVoidTy(m_Context);
  }

  // Handle Pointers (Raw, Unique, Reference) -> Map to LLVM Pointer
  // Note: Reference in Toka is a pointer in LLVM.
  // Note: Unique is a pointer in LLVM.
  if (type->typeKind == Type::RawPtr || type->typeKind == Type::UniquePtr ||
      type->typeKind == Type::Reference) {

    auto ptrType = std::static_pointer_cast<PointerType>(type);

    // For opaque pointers (LLVM 17+), we could just return ptr.
    // However, if we need typed pointers for GEPs or older LLVM logic (which
    // Toka seems to rely on with resolveType returning typed ptrs):
    llvm::Type *pointeeTy = getLLVMType(ptrType->PointeeType);
    if (pointeeTy->isVoidTy()) {
      // *void -> i8* (standard convention) or just opaque ptr if supported.
      // Let's stick to i8* for void* equivalent if typed.
      pointeeTy = llvm::Type::getInt8Ty(m_Context);
    }
    return llvm::PointerType::getUnqual(pointeeTy);
  }

  // Handle Shared Pointer (~T) -> { T*, i32* }
  if (type->typeKind == Type::SharedPtr) {
    auto sharedType = std::static_pointer_cast<SharedPointerType>(type);
    llvm::Type *elemTy = getLLVMType(sharedType->PointeeType);

    llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
    llvm::Type *refCountTy =
        llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));

    return llvm::StructType::get(m_Context, {ptrTy, refCountTy});
  }

  // Handle Arrays ([T; N])
  if (type->typeKind == Type::Array) {
    auto arrType = std::static_pointer_cast<ArrayType>(type);
    llvm::Type *elemTy = getLLVMType(arrType->ElementType);
    return llvm::ArrayType::get(elemTy, arrType->Size);
  }

  // Handle Tuples ((T1, T2))
  if (type->typeKind == Type::Tuple) {
    auto tupleType = std::static_pointer_cast<TupleType>(type);
    std::vector<llvm::Type *> elemTypes;
    for (const auto &elem : tupleType->Elements) {
      elemTypes.push_back(getLLVMType(elem));
    }
    return llvm::StructType::get(m_Context, elemTypes);
  }

  // Handle Shapes (Structs) via Name Lookup
  if (type->typeKind == Type::Shape) {
    auto shapeType = std::static_pointer_cast<ShapeType>(type);
    if (m_StructTypes.count(shapeType->Name)) {
      return m_StructTypes[shapeType->Name];
    }
    // If generic lookup fails, try resolving by name (backup)
    return resolveType(shapeType->Name, false);
  }

  // Fallback to string based resolution if we have an Unresolved type wrapping
  // a string
  if (type->typeKind == Type::Unresolved) {
    auto unresolved = std::static_pointer_cast<UnresolvedType>(type);
    return resolveType(unresolved->Name, false);
  }

  // Default Void
  return llvm::Type::getVoidTy(m_Context);
}

void CodeGen::fillSymbolMetadata(TokaSymbol &sym, std::shared_ptr<Type> typeObj,
                                 llvm::Type *allocaElemTy) {
  if (!typeObj)
    return;

  // 1. Indirection and Core Type Logic is now driven by TypeObj structure
  sym.soulTypeObj = typeObj;

  // Determine Morphology and Indirection based on Type Kind
  sym.indirectionLevel = 0;
  sym.morphology = Morphology::None;
  sym.mode = AddressingMode::Direct;

  std::shared_ptr<Type> current = typeObj;

  // Unseal wrappers to find "Soul"
  // We loop to peel of layers if needed, or just switch on the top layer logic.
  // But TokaSymbol logic expects 'soulType' to be the underlying data type.

  // Handle Reference
  if (typeObj->isReference()) {
    sym.mode = AddressingMode::Reference;
    sym.indirectionLevel = 1;
    sym.morphology = Morphology::None; // Reference is an alias, not ownership
    // Peel for Soul
    auto ptr = std::static_pointer_cast<PointerType>(typeObj);
    sym.soulType = getLLVMType(ptr->PointeeType);
    current = ptr->PointeeType;
  } else if (typeObj->typeKind == Type::UniquePtr) {
    sym.mode = AddressingMode::Pointer;
    sym.indirectionLevel = 1;
    sym.morphology = Morphology::Unique;
    auto ptr = std::static_pointer_cast<PointerType>(typeObj);
    sym.soulType = getLLVMType(ptr->PointeeType);
    current = ptr->PointeeType;
  } else if (typeObj->typeKind == Type::SharedPtr) {
    sym.mode = AddressingMode::Pointer;
    sym.indirectionLevel = 1; // Effectively a pointer access to value
    sym.morphology = Morphology::Shared;
    auto ptr = std::static_pointer_cast<SharedPointerType>(typeObj);
    sym.soulType = getLLVMType(ptr->PointeeType);
    current = ptr->PointeeType;
  } else if (typeObj->typeKind == Type::RawPtr) {
    sym.mode = AddressingMode::Pointer;
    sym.indirectionLevel = 1;
    sym.morphology = Morphology::Raw;
    auto ptr = std::static_pointer_cast<PointerType>(typeObj);
    sym.soulType = getLLVMType(ptr->PointeeType);
    current = ptr->PointeeType;
  } else {
    // Direct Value
    sym.mode = AddressingMode::Direct;
    sym.morphology = Morphology::None;
    sym.soulType = getLLVMType(typeObj);
    // current remains typeObj
  }

  if (!sym.soulType)
    sym.soulType = allocaElemTy;

  // Attributes
  sym.isMutable = typeObj->IsWritable;
  sym.isNullable = typeObj->IsNullable;

  // Arrays are continuous
  if (current && current->isArray()) {
    sym.isContinuous = true;
  } else {
    sym.isContinuous = false;
  }

  // Rebindable? usually strict to the variable decl itself, not the type
  // always, but we can check if the top level pointer was rebindable if we
  // stored it in Type? Currently Type has IsWritable. IsRebindable is typically
  // a property of the binding, not the type (like `mut` binding in Rust). So
  // we'll leave sym.isRebindable to be set by the caller (Declaration).
}

} // namespace toka