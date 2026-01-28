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
#include <typeinfo>

namespace toka {

static int getTypeHatCount(std::shared_ptr<toka::Type> type) {
  if (!type)
    return 0;
  int count = 0;
  auto cur = type;
  while (cur &&
         (cur->isPointer() || cur->isReference() || cur->isSmartPointer())) {
    count++;
    cur = cur->getPointeeType();
  }
  return count;
}

PhysEntity CodeGen::genAllocExpr(const AllocExpr *ae) {
  llvm::Function *allocHook = m_Module->getFunction("__toka_alloc");
  if (!allocHook) {
    allocHook = m_Module->getFunction("malloc");
  }
  if (!allocHook) {
    // Declare malloc if neither is present
    llvm::Type *sizeTy = llvm::Type::getInt64Ty(m_Context);
    llvm::Type *retTy = m_Builder.getPtrTy();
    llvm::FunctionType *ft = llvm::FunctionType::get(retTy, {sizeTy}, false);
    allocHook = llvm::Function::Create(ft, llvm::Function::ExternalLinkage,
                                       "malloc", m_Module.get());
  }

  llvm::Type *elemTy = nullptr;
  if (ae->ResolvedType) {
    auto resTy = ae->ResolvedType;
    if (resTy->isPointer() || resTy->isSmartPointer()) {
      elemTy = getLLVMType(resTy->getPointeeType());
    } else {
      elemTy = getLLVMType(resTy);
    }
  } else {
    elemTy = resolveType(ae->TypeName, false);
  }
  llvm::DataLayout dl(m_Module.get());
  uint64_t size = dl.getTypeAllocSize(elemTy);
  llvm::Value *sizeVal =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), size);

  llvm::Value *arrayCount = nullptr;

  if (ae->IsArray && ae->ArraySize) {
    llvm::Value *count = genExpr(ae->ArraySize.get()).load(m_Builder);
    count = m_Builder.CreateIntCast(count, llvm::Type::getInt64Ty(m_Context),
                                    false);
    arrayCount = count;
    sizeVal = m_Builder.CreateMul(sizeVal, count);
  }

  llvm::Value *rawPtr = m_Builder.CreateCall(allocHook, sizeVal);
  llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
  llvm::Value *castedPtr = m_Builder.CreateBitCast(rawPtr, ptrTy);

  if (ae->Initializer) {
    // Evaluate initializer once
    llvm::Value *initVal = genExpr(ae->Initializer.get()).load(m_Builder);

    if (arrayCount) {
      // Loop to initialize all elements
      llvm::BasicBlock *preHeaderBB = m_Builder.GetInsertBlock();
      llvm::Function *F = preHeaderBB->getParent();
      llvm::BasicBlock *loopBB =
          llvm::BasicBlock::Create(m_Context, "alloc_init_loop", F);
      llvm::BasicBlock *afterBB =
          llvm::BasicBlock::Create(m_Context, "alloc_init_after", F);

      m_Builder.CreateBr(loopBB);
      m_Builder.SetInsertPoint(loopBB);

      llvm::PHINode *iVar =
          m_Builder.CreatePHI(llvm::Type::getInt64Ty(m_Context), 2, "i");
      iVar->addIncoming(
          llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), 0),
          preHeaderBB);

      // GEP to element
      llvm::Value *elemPtr =
          m_Builder.CreateInBoundsGEP(elemTy, castedPtr, iVar);
      m_Builder.CreateStore(initVal, elemPtr);

      llvm::Value *nextI = m_Builder.CreateAdd(
          iVar, llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), 1));
      llvm::Value *cond = m_Builder.CreateICmpULT(nextI, arrayCount);
      iVar->addIncoming(nextI, loopBB);

      m_Builder.CreateCondBr(cond, loopBB, afterBB);
      m_Builder.SetInsertPoint(afterBB);
    } else {
      m_Builder.CreateStore(initVal, castedPtr);
    }
  }
  return castedPtr;
}

llvm::Value *CodeGen::genFreeStmt(const FreeStmt *fs) {
  llvm::Function *freeHook = m_Module->getFunction("free");

  llvm::Value *ptrAddr = nullptr;
  if (auto *unary = dynamic_cast<const UnaryExpr *>(fs->Expression.get())) {
    if (unary->Op == TokenType::Star || unary->Op == TokenType::Caret ||
        unary->Op == TokenType::Tilde) {
      // [Fix] Freeing a pointer (*p) means freeing the Value (the Soul),
      // not the Stack Address (identity). genAddr(*p) returns Identity.
      // genExpr(*p) returns Soul.
      ptrAddr = genExpr(fs->Expression.get()).load(m_Builder);
    }
  }

  if (!ptrAddr) {
    // [Fix] Always load the pointer value. free() expects the heap address
    // (RValue), not the variable address (LValue).
    ptrAddr = genExpr(fs->Expression.get()).load(m_Builder);
  }

  if (freeHook && ptrAddr) {
    // [Feature] Drop before Free for Raw Pointers
    // Check if the type being freed has a drop method
    std::string typeName = "";
    bool isArray = false;
    uint64_t arraySize = 0;
    llvm::Value *dynamicCount = nullptr;

    // Try to deduce type from expression
    const Expr *rawExpr = fs->Expression.get();
    // Peel layers (*, ?, etc.)
    while (true) {
      if (auto *ue = dynamic_cast<const UnaryExpr *>(rawExpr)) {
        rawExpr = ue->RHS.get();
      } else if (auto *pe = dynamic_cast<const PostfixExpr *>(rawExpr)) {
        rawExpr = pe->LHS.get();
      } else {
        break;
      }
    }

    if (auto *ve = dynamic_cast<const VariableExpr *>(rawExpr)) {
      std::string varName = Type::stripMorphology(ve->Name);

      if (m_Symbols.count(varName)) {
        std::string vType = m_Symbols[varName].typeName;
        // vType is e.g. *Data or *[10]Data
        if (!vType.empty()) {
          if (vType[0] == '*') {
            typeName = vType.substr(1); // Peel pointer
          } else {
            typeName = vType;
          }
        }
      }
    }

    // Handle Array Type parsing (e.g. [10]Data)
    if (!typeName.empty() && typeName[0] == '[') {
      size_t close = typeName.find(']');
      if (close != std::string::npos) {
        std::string sizeStr = typeName.substr(1, close - 1);
        try {
          arraySize = std::stoull(sizeStr);
          typeName = typeName.substr(close + 1);
          isArray = true;
        } catch (...) {
        }
      }
    }

    std::string dropFunc = "";
    if (!typeName.empty()) {
      if (m_Shapes.count(typeName)) {
        dropFunc = m_Shapes[typeName]->MangledDestructorName;
      }
      if (dropFunc.empty()) {
        std::string try1 = "encap_" + typeName + "_drop";
        std::string try2 = typeName + "_drop"; // Legacy
        if (m_Functions.count(try1))
          dropFunc = try1;
        else if (m_Functions.count(try2))
          dropFunc = try2;
      }
    }

    // Use explicit count if provided, otherwise parsed array size
    if (fs->Count) {
      PhysEntity res = genExpr(fs->Count.get());
      dynamicCount = res.load(m_Builder);
      isArray = true;
    }

    if (!dropFunc.empty()) {
      llvm::Function *dFn = m_Module->getFunction(dropFunc);
      if (dFn) {
        if (isArray) {
          // Loop and drop
          // We need the element size
          llvm::Type *elemTy = resolveType(typeName, false);

          llvm::Value *countVal = dynamicCount;
          if (!countVal) {
            countVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context),
                                              arraySize);
          }

          if (countVal->getType() != llvm::Type::getInt64Ty(m_Context)) {
            countVal = m_Builder.CreateIntCast(
                countVal, llvm::Type::getInt64Ty(m_Context), false,
                "count_cast");
          }

          llvm::BasicBlock *preHeaderBB = m_Builder.GetInsertBlock();
          llvm::Function *F = preHeaderBB->getParent();
          llvm::BasicBlock *loopBB =
              llvm::BasicBlock::Create(m_Context, "drop_loop", F);
          llvm::BasicBlock *afterBB =
              llvm::BasicBlock::Create(m_Context, "drop_after", F);

          m_Builder.CreateBr(loopBB);
          m_Builder.SetInsertPoint(loopBB);

          llvm::PHINode *iVar =
              m_Builder.CreatePHI(llvm::Type::getInt64Ty(m_Context), 2, "i");
          iVar->addIncoming(
              llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), 0),
              preHeaderBB);

          // GEP to element
          // ptrAddr is the base pointer (void* or T*). Cast to T*
          llvm::Value *typedBase = m_Builder.CreateBitCast(
              ptrAddr, llvm::PointerType::getUnqual(elemTy));
          llvm::Value *elemPtr =
              m_Builder.CreateInBoundsGEP(elemTy, typedBase, iVar);

          m_Builder.CreateCall(dFn, {elemPtr});

          llvm::Value *nextI = m_Builder.CreateAdd(
              iVar,
              llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), 1));
          llvm::Value *cond = m_Builder.CreateICmpULT(nextI, countVal);
          iVar->addIncoming(nextI, loopBB);

          m_Builder.CreateCondBr(cond, loopBB, afterBB);
          m_Builder.SetInsertPoint(afterBB);

        } else {
          // Single drop
          // Cast ptrAddr to correct type if needed (though opaque pointers
          // handling makes it easier, drop expects T*)
          llvm::Type *elemTy = resolveType(typeName, false);
          llvm::Value *typedPtr = m_Builder.CreateBitCast(
              ptrAddr, llvm::PointerType::getUnqual(elemTy));
          m_Builder.CreateCall(dFn, {typedPtr});
        }
      }
    }

    llvm::Value *casted =
        m_Builder.CreateBitCast(ptrAddr, m_Builder.getPtrTy());
    m_Builder.CreateCall(freeHook, casted);
  }
  return nullptr;
}

PhysEntity CodeGen::genMemberExpr(const MemberExpr *mem) {
  if (mem->IsStatic) {
    // 1. Get Type Name
    std::string typeName = "";
    if (auto *ve = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
      typeName = ve->Name;
      if (ve->ResolvedType && ve->ResolvedType->isShape()) {
        typeName = ve->ResolvedType->getSoulName();
      }
    }
    // 2. Check if Enum Variant
    if (m_Shapes.count(typeName) &&
        m_Shapes[typeName]->Kind == ShapeKind::Enum) {
      int tag = -1;
      const ShapeDecl *sh = m_Shapes[typeName];
      for (size_t i = 0; i < sh->Members.size(); ++i) {
        if (sh->Members[i].Name == mem->Member) {
          tag = (sh->Members[i].TagValue != -1) ? (int)sh->Members[i].TagValue
                                                : (int)i;
          break;
        }
      }
      if (tag != -1) {
        llvm::Value *tagVal =
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), tag);
        // Enums are Shapes (Structs), so return { tag }
        if (m_StructTypes.count(typeName)) {
          llvm::StructType *st = m_StructTypes[typeName];
          llvm::Value *res = llvm::UndefValue::get(st);
          // Dynamically determine tag type (i8, i32, etc.)
          llvm::Type *tagTy = st->getElementType(0);
          llvm::Value *typedTagVal = llvm::ConstantInt::get(tagTy, tag);
          res = m_Builder.CreateInsertValue(res, typedTagVal, 0);
          return res;
        }
        return tagVal;
      }
    }
    return nullptr;
  }

  // --- Dynamic Member Access (Sovereign Logic) ---
  llvm::Value *objAddr = emitEntityAddr(mem->Object.get());
  if (!objAddr)
    return nullptr;

  // [Fix] Shared Pointer Auto-Dereference for Member Access
  // If the object is a Shared Pointer (~T), it is physically { T*, RefCount* }.
  // We must unwrap it to get T* before accessing members of T.

  // Helper to peel decorators to find the VariableExpr
  const Expr *inner = mem->Object.get();
  while (true) {
    if (auto *pe = dynamic_cast<const PostfixExpr *>(inner)) {
      inner = pe->LHS.get();
    } else if (auto *ue = dynamic_cast<const UnaryExpr *>(inner)) {
      inner = ue->RHS.get();
    } else {
      break;
    }
  }

  if (auto *ve = dynamic_cast<const VariableExpr *>(inner)) {
    std::string baseName = ve->Name;
    while (!baseName.empty() &&
           (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
            baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!'))
      baseName = baseName.substr(1);
    while (!baseName.empty() &&
           (baseName.back() == '#' || baseName.back() == '?' ||
            baseName.back() == '!'))
      baseName.pop_back();

    if (m_Symbols.count(baseName)) {
      TokaSymbol &sym = m_Symbols[baseName];
      if (sym.morphology == Morphology::Shared) {
        // Shared Pointer ~T is { T*, RC* }
        // getEntityAddr has ALREADY unwrapped this to T* (Data Pointer).
        // So objAddr is T*. We do not need to unwrap again.
      }
    }
  }

  llvm::Type *objType = nullptr;
  if (mem->Object->ResolvedType) {
    auto base = mem->Object->ResolvedType;
    // Unwrap pointers/references to get the underlying Shape/Struct type
    while (base && (base->isPointer() || base->isReference() ||
                    base->isSmartPointer())) {
      auto next = base->getPointeeType();
      if (!next)
        break;
      base = next;
    }
    if (base) {
      objType = getLLVMType(base);
    }
  }

  // Fallback for cases without ResolvedType (unlikely in modern Toka Sema)
  if (!objType) {
    if (auto *ptrTy = llvm::dyn_cast<llvm::PointerType>(objAddr->getType())) {
      if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(objAddr)) {
        objType = alloca->getAllocatedType();
      } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objAddr)) {
        objType = gep->getResultElementType();
      } else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(objAddr)) {
        objType = load->getType();
      }
    }
  }

  // Final fallback: Contextual Symbol Table (for 'self', globals, etc)
  if (!objType || !objType->isStructTy()) {
    if (auto *ve = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
      std::string baseName = ve->Name;
      while (!baseName.empty() &&
             (baseName[0] == '*' || baseName[0] == '&' || baseName[0] == '#'))
        baseName = baseName.substr(1);
      if (m_Symbols.count(baseName)) {
        objType = m_Symbols[baseName].soulType;
      }
    }
  }

  int idx = mem->Index;
  llvm::StructType *st = nullptr;
  if (objType && objType->isStructTy()) {
    st = llvm::cast<llvm::StructType>(objType);
  }

  // [Fix] Handle Auto-Dereference (Identity -> Soul) for Deref Expressions
  // (*p.x) If the object expression is a Dereference (*p), genAddr returned the
  // Identity (Handle Address). We need the Soul (Data Address) to access
  // members.
  const Expr *baseExpr = mem->Object.get();
  if (auto *ue = dynamic_cast<const UnaryExpr *>(baseExpr)) {
    if (ue->Op == TokenType::Star || ue->Op == TokenType::Caret ||
        ue->Op == TokenType::Tilde || ue->Op == TokenType::TokenNull) {

      bool isShared = false;
      if (ue->RHS->ResolvedType && ue->RHS->ResolvedType->isSharedPtr()) {
        isShared = true;
      }

      if (isShared) {
        // Shared Pointer Identity is { T*, Ref* }*
        // We want T*.
        // 1. GEP to data ptr (index 0)
        llvm::Value *dataAddr = m_Builder.CreateStructGEP(
            llvm::StructType::get(
                m_Context,
                {llvm::PointerType::getUnqual(st ? (llvm::Type *)st
                                                 : m_Builder.getInt8Ty()),
                 llvm::PointerType::getUnqual(m_Builder.getInt32Ty())}),
            objAddr, 0, "member.sh_data_gep");
        // 2. Load T*
        objAddr = m_Builder.CreateLoad(m_Builder.getPtrTy(), dataAddr,
                                       "member.sh_soul");
      } else {
        // Raw/Unique Pointer Identity is T**.
        // We want T*.
        objAddr = m_Builder.CreateLoad(m_Builder.getPtrTy(), objAddr,
                                       "member.peel_soul");
      }
    }
  }

  std::string memberName = mem->Member;
  if (memberName.substr(0, 2) == "??")
    memberName = memberName.substr(2);
  while (!memberName.empty() && (memberName[0] == '^' || memberName[0] == '*' ||
                                 memberName[0] == '&' || memberName[0] == '#' ||
                                 memberName[0] == '~' || memberName[0] == '!'))
    memberName = memberName.substr(1);
  while (!memberName.empty() &&
         (memberName.back() == '#' || memberName.back() == '?' ||
          memberName.back() == '!'))
    memberName.pop_back();

  if (!st) {
    std::string foundStruct;
    for (const auto &pair : m_StructFieldNames) {
      for (int i = 0; i < (int)pair.second.size(); ++i) {
        std::string fn = pair.second[i];
        while (!fn.empty() && (fn[0] == '^' || fn[0] == '*' || fn[0] == '&' ||
                               fn[0] == '#' || fn[0] == '~' || fn[0] == '!'))
          fn = fn.substr(1);
        while (!fn.empty() &&
               (fn.back() == '#' || fn.back() == '?' || fn.back() == '!'))
          fn.pop_back();

        if (fn == memberName) {
          foundStruct = pair.first;
          idx = i;
          break;
        }
      }
      if (!foundStruct.empty()) {
        st = m_StructTypes[foundStruct];
        break;
      }
    }
  }

  if (!st)
    return nullptr;

  std::string stName = m_TypeToName[st];
  if (stName.empty()) {
    for (const auto &pair : m_StructTypes) {
      if (pair.second == st) {
        stName = pair.first;
        break;
      }
    }
  }

  // Try to find index in st if still -1
  if (idx == -1) {
    if (!stName.empty()) {
      auto &fields = m_StructFieldNames[stName];
      for (int i = 0; i < (int)fields.size(); ++i) {
        std::string fn = fields[i];
        // scrub logic...
        while (!fn.empty() && (fn[0] == '#' || fn[0] == '*' || fn[0] == '&'))
          fn = fn.substr(1);                            // minimal scrub
        if (fn.find(memberName) != std::string::npos) { // simplistic
          idx = i;
          break;
        }
      }
      // Use stricter match if possible, matching genAddr logic
      for (int i = 0; i < (int)fields.size(); ++i) {
        if (fields[i].find(memberName) != std::string::npos) {
          idx = i;
          break;
        }
      }
    }
  }

  if (idx == -1) {
    std::string typeDesc = stName.empty() ? "anonymous struct or tuple"
                                          : ("struct '" + stName + "'");
    error(mem, "Failed to resolve member '" + mem->Member + "' in " + typeDesc);
    return nullptr;
  }

  // Resolve Metadata & IR Type safely
  std::string memberTypeName = "";
  llvm::Type *irTy = nullptr;
  bool isUnion = false;

  if (mem->Object->ResolvedType) {
    auto soul = mem->Object->ResolvedType;
    while (soul && (soul->isPointer() || soul->isReference() ||
                    soul->isSmartPointer())) {
      soul = soul->getPointeeType();
    }
    if (soul && soul->isShape()) {
      auto stType = std::dynamic_pointer_cast<ShapeType>(soul);
      if (stType->Decl && stType->Decl->Kind == ShapeKind::Union) {
        isUnion = true;
      }
    }
  }

  if (!isUnion && !stName.empty() && m_Shapes.count(stName)) {
    isUnion = (m_Shapes[stName]->Kind == ShapeKind::Union);
  }

  llvm::Value *fieldAddr = nullptr;
  if (!isUnion) {
    fieldAddr = m_Builder.CreateStructGEP(st, objAddr, idx, memberName);
  } else {
    // Union: bitcast base address to the desired member's type.
    // The physical struct 'st' has only one element: [maxSize x i8].
    llvm::Type *destTy = nullptr;
    const ShapeDecl *sh = nullptr;
    if (!stName.empty() && m_Shapes.count(stName)) {
      sh = m_Shapes[stName];
    } else if (mem->Object->ResolvedType) {
      auto soul = mem->Object->ResolvedType;
      while (soul && (soul->isPointer() || soul->isReference() ||
                      soul->isSmartPointer())) {
        soul = soul->getPointeeType();
      }
      if (soul && soul->isShape()) {
        sh = std::dynamic_pointer_cast<ShapeType>(soul)->Decl;
      }
    }

    if (sh && idx >= 0 && idx < (int)sh->Members.size()) {
      if (sh->Members[idx].ResolvedType) {
        destTy = getLLVMType(sh->Members[idx].ResolvedType);
      } else {
        destTy = resolveType(sh->Members[idx].Type, false);
      }
    }
    if (!destTy)
      destTy = llvm::Type::getInt8Ty(m_Context);

    fieldAddr =
        m_Builder.CreateBitCast(objAddr, llvm::PointerType::getUnqual(destTy));
  }

  llvm::Value *finalAddr = fieldAddr;

  if (!stName.empty() && m_Shapes.count(stName)) {
    const ShapeDecl *sh = m_Shapes[stName];
    if (idx >= 0 && idx < (int)sh->Members.size()) {
      memberTypeName = sh->Members[idx].Type;
      if (sh->Members[idx].ResolvedType)
        irTy = getLLVMType(sh->Members[idx].ResolvedType);
      else
        irTy = resolveType(memberTypeName, false);
    }
  } else if (mem->Object->ResolvedType) {
    auto soul = mem->Object->ResolvedType;
    while (soul && (soul->isPointer() || soul->isReference() ||
                    soul->isSmartPointer())) {
      soul = soul->getPointeeType();
    }
    if (soul && soul->isShape()) {
      auto sh = std::dynamic_pointer_cast<ShapeType>(soul)->Decl;
      if (sh && idx >= 0 && idx < (int)sh->Members.size()) {
        memberTypeName = sh->Members[idx].Type;
        if (sh->Members[idx].ResolvedType)
          irTy = getLLVMType(sh->Members[idx].ResolvedType);
      }
    }
  }

  if (!irTy) {
    if (isUnion && idx > 0) {
      irTy = llvm::Type::getInt8Ty(m_Context);
    } else {
      irTy = st->getElementType(idx);
    }
  }

  // [Constitution] Hat Rule: "指针必须带帽，脱帽就是解指针/解引用"
  // If the member is defined as a pointer/reference (hatted) but accessed
  // without hats, we must perform implicit dereferences.
  auto getHatCount = [](const std::string &s) {
    int count = 0;
    for (char c : s) {
      if (c == '^' || c == '*' || c == '~' || c == '&')
        count++;
      else
        break;
    }
    return count;
  };

  int defHats = 0;
  if (!stName.empty() && m_Shapes.count(stName)) {
    defHats = getHatCount(m_Shapes[stName]->Members[idx].Type);
  }
  int accessHats = getHatCount(mem->Member);

  // If definition has more hats than access, we are "Hat-Off", so dereference.
  int derefCount = 0;
  if (mem->ResolvedType) {
    derefCount = defHats - getTypeHatCount(mem->ResolvedType);
  } else {
    derefCount = defHats - accessHats;
  }
  if (derefCount < 0)
    derefCount = 0;
  for (int i = 0; i < derefCount; ++i) {
    // Current finalAddr is the address of the pointer.
    // Load it to get the target address.
    finalAddr =
        m_Builder.CreateLoad(m_Builder.getPtrTy(), finalAddr, "hat_off_deref");
  }

  llvm::Type *finalIrTy = irTy;
  if (mem->ResolvedType) {
    finalIrTy = getLLVMType(mem->ResolvedType);
  }

  if (mem->Member.size() >= 2 && mem->Member.substr(0, 2) == "??") {
    // Identity Assertion (Ch 6.1)
    llvm::Value *ptrVal = m_Builder.CreateLoad(finalIrTy, finalAddr, "nn.load");
    genNullCheck(ptrVal, mem);
    return PhysEntity(ptrVal, memberTypeName, finalIrTy, false); // R-Value
  }

  return PhysEntity(finalAddr, memberTypeName, finalIrTy, true);
}

PhysEntity CodeGen::genIndexExpr(const ArrayIndexExpr *idxExpr) {
  // Check for Array Shape Initialization
  if (auto *var = dynamic_cast<const VariableExpr *>(idxExpr->Array.get())) {
    if (m_Shapes.count(var->Name)) {
      const ShapeDecl *sh = m_Shapes[var->Name];
      if (sh->Kind == ShapeKind::Array) {
        llvm::StructType *st = m_StructTypes[var->Name];
        llvm::Value *alloca =
            m_Builder.CreateAlloca(st, nullptr, var->Name + "_init");

        for (size_t i = 0; i < idxExpr->Indices.size(); ++i) {
          llvm::Value *val = genExpr(idxExpr->Indices[i].get()).load(m_Builder);
          if (!val)
            return nullptr;
          // GEP: struct 0, array i
          llvm::Value *ptr = m_Builder.CreateInBoundsGEP(
              st, alloca,
              {m_Builder.getInt32(0), m_Builder.getInt32(0),
               m_Builder.getInt32((uint32_t)i)});
          m_Builder.CreateStore(val, ptr);
        }
        return m_Builder.CreateLoad(st, alloca);
      }
    }
  }

  // Normal Indexing
  llvm::Value *addr = genAddr(idxExpr);
  if (!addr)
    return nullptr;
  if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(addr)) {
    return m_Builder.CreateLoad(gep->getResultElementType(), addr);
  }
  return nullptr;
}

llvm::Value *CodeGen::genAddr(const Expr *expr) {
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    return getEntityAddr(var->Name);
  }

  if (auto *unary = dynamic_cast<const UnaryExpr *>(expr)) {
    if (unary->Op == TokenType::Ampersand) {
      if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
        return getIdentityAddr(v->Name); // &v -> Box address
      }
      return genAddr(unary->RHS.get());
    }
    if (unary->Op == TokenType::Star || unary->Op == TokenType::Caret ||
        unary->Op == TokenType::Tilde) {
      // [Constitution] *p, ^p, ~p refer to the Identity (the pointer handle).
      // Their "address" is the address of the handle box (the alloca).
      if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
        return getIdentityAddr(v->Name);
      }
      // For recursive unary, we'd need to go deeper, but Toka usually has 1
      // level.
      return genAddr(unary->RHS.get());
    }
    if (unary->Op == TokenType::TokenNull) {
      // Morphology is transparent to address
      return genAddr(unary->RHS.get());
    }
  }

  if (auto *idxExpr = dynamic_cast<const ArrayIndexExpr *>(expr)) {
    if (idxExpr->Indices.empty())
      return nullptr;

    // 1. Identify the Array base variable
    std::string baseName = "";
    if (auto *v = dynamic_cast<const VariableExpr *>(idxExpr->Array.get())) {
      baseName = v->Name;
    }

    // Scrub decorators
    while (!baseName.empty() &&
           (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
            baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!'))
      baseName = baseName.substr(1);
    while (!baseName.empty() &&
           (baseName.back() == '#' || baseName.back() == '?' ||
            baseName.back() == '!'))
      baseName.pop_back();

    llvm::Value *indexValue =
        genExpr(idxExpr->Indices[0].get()).load(m_Builder);
    if (!indexValue)
      return nullptr;

    auto it = m_Symbols.find(baseName);
    if (it == m_Symbols.end()) {
      // Fallback for non-variable bases using PhysEntity to recover type info
      PhysEntity arrEnt = genExpr(idxExpr->Array.get());

      // Soul-Identity Protocol for Array Indexing:
      // We need the IDENTITY (the data address) to index off.
      // - If arrEnt is Reference (Soul Address):
      //   - If underlying type is Pointer (e.g. *char): The Soul stores the
      //   Identity. We MUST LOAD the Soul to get Identity.
      //   - If underlying type is Array (e.g. [10]char): The Soul IS the
      //   Identity. We use Soul address directly.

      llvm::Value *basePtr = nullptr;
      llvm::Type *elemTy = nullptr;

      if (arrEnt.isAddress) {
        llvm::Type *memTy = arrEnt.irType;
        if (!memTy)
          memTy = arrEnt.value->getType();

        if (memTy->isPointerTy()) {
          // It's a pointer variable/member (like *char source).
          // The entity value is the address OF the pointer (Soul).
          // We must LOAD to get the actual pointer (Identity).
          basePtr = m_Builder.CreateLoad(memTy, arrEnt.value, "arr_load_ptr");

          if (idxExpr->Array->ResolvedType) {
            if (auto pt = idxExpr->Array->ResolvedType->getPointeeType()) {
              elemTy = getLLVMType(pt);
            }
          }
          if (!elemTy)
            elemTy = llvm::Type::getInt8Ty(m_Context);
        } else if (memTy->isArrayTy()) {
          // It's an array variable/member (like char buf[10]).
          // The entity value is the address of the array start.
          basePtr = arrEnt.value;
          elemTy = memTy->getArrayElementType();

          // Array GEP needs [0, index] because base is pointer to array
          return m_Builder.CreateInBoundsGEP(
              memTy, basePtr, {m_Builder.getInt32(0), indexValue},
              "arr_idx_gep");
        }
      } else {
        // R-Value (e.g. function return). It's already the Identity (pointer
        // value).
        basePtr = arrEnt.value;
      }

      if (!basePtr) {
        if (arrEnt.isAddress)
          basePtr = arrEnt.load(m_Builder);
        else
          basePtr = arrEnt.value;
      }
      if (!basePtr)
        return nullptr;

      if (!elemTy) {
        if (idxExpr->Array->ResolvedType) {
          if (auto pt = idxExpr->Array->ResolvedType->getPointeeType()) {
            elemTy = getLLVMType(pt);
          }
        }
      }
      if (!elemTy) {
        // Default stride for unknown pointer types in fallback
        elemTy = llvm::Type::getInt8Ty(m_Context);
      }

      return m_Builder.CreateInBoundsGEP(elemTy, basePtr, indexValue);
    }

    TokaSymbol &sym = it->second;
    llvm::Value *currentBase = sym.allocaPtr;

    // 2. The Radar Logic (Addressing Constitution)
    if (sym.mode == AddressingMode::Direct) {
      // Stack-allocated array [N]: Base is the Identity Slot itself
      // Requires double-GEP: [0, index]
      if (sym.soulType->isArrayTy()) {
        return m_Builder.CreateInBoundsGEP(sym.soulType, currentBase,
                                           {m_Builder.getInt32(0), indexValue});
      }
      return m_Builder.CreateInBoundsGEP(sym.soulType, currentBase, indexValue);
    } else {
      // Pointer or Reference: Peel the onion
      // For Pointer, we need to load 'indirectionLevel' times to get to the
      // Soul base.
      // For Reference, it's basically load 1 time.
      int loads = sym.indirectionLevel;
      if (sym.mode == AddressingMode::Reference && loads == 0)
        loads = 1;

      for (int i = 0; i < loads; ++i) {
        currentBase = m_Builder.CreateLoad(m_Builder.getPtrTy(), currentBase,
                                           baseName + ".deref_step");
      }

      // 3. Final GEP Calculation
      if (sym.soulType->isArrayTy()) {
        return m_Builder.CreateInBoundsGEP(sym.soulType, currentBase,
                                           {m_Builder.getInt32(0), indexValue});
      }
      return m_Builder.CreateInBoundsGEP(sym.soulType, currentBase, indexValue);
    }
  }

  if (auto *mem = dynamic_cast<const MemberExpr *>(expr)) {
    // Delegate to Sovereign genMemberExpr
    return genMemberExpr(mem).value;
  }

  if (auto *post = dynamic_cast<const PostfixExpr *>(expr)) {
    if (post->Op == TokenType::TokenWrite) {
      // ptr# address is same as ptr address (Entity)
      return genAddr(post->LHS.get());
    }
  }

  return nullptr;
}

llvm::Value *CodeGen::projectSoul(llvm::Value *handle, const TokaSymbol &sym) {
  if (!handle)
    return nullptr;

  llvm::Value *current = handle;

  // 1. Direct Mode: Box is the Soul
  if (sym.mode == AddressingMode::Direct) {
    return current;
  }

  // 2. Reference Mode: Reference is a pointer alias
  if (sym.mode == AddressingMode::Reference) {
    return m_Builder.CreateLoad(m_Builder.getPtrTy(), current,
                                "ref.alias_soul");
  }

  // 3. Pointer Modes (Raw, Unique, Shared)
  if (sym.morphology == Morphology::Shared) {
    // allocaPtr is {T*, Ref*}*. We want T*.
    llvm::Type *ptrTy = llvm::PointerType::getUnqual(sym.soulType);
    llvm::Type *refTy =
        llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
    llvm::Type *structTy = llvm::StructType::get(m_Context, {ptrTy, refTy});

    llvm::Value *dataPtrAddr =
        m_Builder.CreateStructGEP(structTy, current, 0, "shared.data_gep");
    return m_Builder.CreateLoad(m_Builder.getPtrTy(), dataPtrAddr,
                                "shared.data_ptr");
  }

  // Raw & Unique: Peeling recursive loads based on indirection level
  for (int i = 0; i < sym.indirectionLevel; ++i) {
    current =
        m_Builder.CreateLoad(m_Builder.getPtrTy(), current, "ptr.peel_soul");
  }

  return current;
}

llvm::Value *CodeGen::getEntityAddr(const std::string &name) {
  std::string baseName = name;
  while (!baseName.empty() &&
         (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
          baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!' ||
          baseName[0] == '?'))
    baseName = baseName.substr(1);
  while (!baseName.empty() &&
         (baseName.back() == '#' || baseName.back() == '?' ||
          baseName.back() == '!'))
    baseName.pop_back();

  auto it = m_Symbols.find(baseName);
  if (it == m_Symbols.end()) {
    // Try global
    if (auto *glob = m_Module->getNamedGlobal(baseName)) {
      return glob;
    }
    std::cerr << "CodeGen Internal Error: Symbol '" << baseName
              << "' not found in getEntityAddr (and not global)\n";
    return nullptr;
  }

  TokaSymbol &sym = it->second;
  if (!sym.allocaPtr) {
    std::cerr << "CodeGen Internal Error: Symbol '" << baseName
              << "' has null allocaPtr\n";
    return nullptr;
  }

  // Unified Address Layering: Project Soul from Identity Handle
  return projectSoul(sym.allocaPtr, sym);
}

llvm::Value *CodeGen::getIdentityAddr(const std::string &name) {
  std::string baseName = name;
  while (!baseName.empty() &&
         (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
          baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!' ||
          baseName[0] == '?'))
    baseName = baseName.substr(1);
  while (!baseName.empty() &&
         (baseName.back() == '#' || baseName.back() == '?' ||
          baseName.back() == '!'))
    baseName.pop_back();

  auto it = m_Symbols.find(baseName);
  if (it == m_Symbols.end())
    return nullptr;

  // The Identity is ALWAYS the allocaPtr (the box)
  return it->second.allocaPtr;
}

llvm::Value *CodeGen::emitEntityAddr(const Expr *expr) {
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    return getEntityAddr(var->Name);
  }

  // Try to get address directly (LValue)
  llvm::Value *addr = genAddr(expr);
  if (addr)
    return addr;

  // RValue Spill Fallback (Ch 6.2 Extension)
  // If we can't get an address (e.g. n.??point or pp??), we must evaluate
  // and spill to a temporary alloca if it's a value.
  PhysEntity pe = genExpr(expr);
  if (pe.isAddress)
    return pe.value;

  // Spill to temporary alloca
  if (!pe.value)
    return nullptr;
  llvm::Type *ty = pe.irType;
  if (!ty)
    ty = pe.value->getType();

  llvm::Value *spill = m_Builder.CreateAlloca(ty, nullptr, "rval.spill");
  m_Builder.CreateStore(pe.value, spill);
  return spill;
}

llvm::Value *CodeGen::emitHandleAddr(const Expr *expr) {
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    return getIdentityAddr(var->Name);
  }
  return genAddr(expr);
}

} // namespace toka