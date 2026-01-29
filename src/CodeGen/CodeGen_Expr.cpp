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
#include "toka/CodeGen.h"
#include "toka/DiagnosticEngine.h"
#include "toka/SourceManager.h"
#include "toka/Type.h"
#include <cctype>
#include <iostream>
#include <set>
#include <typeinfo>

namespace toka {

void CodeGen::emitAcquire(llvm::Value *sharedHandle) {
  if (!sharedHandle || !sharedHandle->getType()->isStructTy())
    return;

  auto *ST = llvm::cast<llvm::StructType>(sharedHandle->getType());
  if (ST->getNumElements() < 2)
    return;

  llvm::Value *refPtr =
      m_Builder.CreateExtractValue(sharedHandle, 1, "sh.acq_ref_ptr");
  llvm::Value *nn = m_Builder.CreateIsNotNull(refPtr, "sh.acq_nn");

  llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *incBB =
      llvm::BasicBlock::Create(m_Context, "sh.acq_inc", f);
  llvm::BasicBlock *contBB =
      llvm::BasicBlock::Create(m_Context, "sh.acq_cont", f);
  m_Builder.CreateCondBr(nn, incBB, contBB);

  m_Builder.SetInsertPoint(incBB);
  llvm::Value *cnt =
      m_Builder.CreateLoad(llvm::Type::getInt32Ty(m_Context), refPtr);
  llvm::Value *inc = m_Builder.CreateAdd(
      cnt, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
  m_Builder.CreateStore(inc, refPtr);
  m_Builder.CreateBr(contBB);

  m_Builder.SetInsertPoint(contBB);
}

void CodeGen::emitRelease(llvm::Value *sharedHandle, const TokaSymbol &sym) {
  if (!sharedHandle || !sharedHandle->getType()->isStructTy())
    return;
  llvm::Value *refPtr =
      m_Builder.CreateExtractValue(sharedHandle, 1, "sh.rel_ref_ptr");
  llvm::Value *nn = m_Builder.CreateIsNotNull(refPtr, "sh.rel_nn");

  llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *decBB =
      llvm::BasicBlock::Create(m_Context, "sh.rel_dec", f);
  llvm::BasicBlock *contBB =
      llvm::BasicBlock::Create(m_Context, "sh.rel_cont", f);
  m_Builder.CreateCondBr(nn, decBB, contBB);

  m_Builder.SetInsertPoint(decBB);
  llvm::Value *cnt =
      m_Builder.CreateLoad(llvm::Type::getInt32Ty(m_Context), refPtr);
  llvm::Value *dec = m_Builder.CreateSub(
      cnt, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
  m_Builder.CreateStore(dec, refPtr);

  llvm::Value *isZero = m_Builder.CreateICmpEQ(
      dec, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));
  llvm::BasicBlock *freeBB =
      llvm::BasicBlock::Create(m_Context, "sh.rel_free", f);
  m_Builder.CreateCondBr(isZero, freeBB, contBB);

  m_Builder.SetInsertPoint(freeBB);
  llvm::Value *data =
      m_Builder.CreateExtractValue(sharedHandle, 0, "sh.rel_data");

  // Call drop if exists
  if (sym.hasDrop && !sym.dropFunc.empty()) {
    if (auto *df = m_Module->getFunction(sym.dropFunc)) {
      m_Builder.CreateCall(df, {data});
    }
  }

  llvm::Function *freeFn = m_Module->getFunction("free");
  if (freeFn) {
    m_Builder.CreateCall(freeFn,
                         m_Builder.CreateBitCast(data, m_Builder.getPtrTy()));
    m_Builder.CreateCall(freeFn,
                         m_Builder.CreateBitCast(refPtr, m_Builder.getPtrTy()));
  }
  m_Builder.CreateBr(contBB);

  m_Builder.SetInsertPoint(contBB);
}

llvm::Value *CodeGen::emitPromotion(llvm::Value *rawPtr,
                                    llvm::Type *targetHandleType,
                                    const TokaSymbol &sym) {
  if (!rawPtr || !targetHandleType || !targetHandleType->isStructTy())
    return rawPtr;

  llvm::Function *mallocFn = m_Module->getFunction("malloc");
  if (!mallocFn) {
    std::vector<llvm::Type *> args = {llvm::Type::getInt64Ty(m_Context)};
    llvm::FunctionType *ft =
        llvm::FunctionType::get(m_Builder.getPtrTy(), args, false);
    mallocFn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage,
                                      "malloc", m_Module.get());
  }

  llvm::Value *rcSz =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), 4);
  llvm::Value *rawRC =
      m_Builder.CreateCall(mallocFn, rcSz, "sh.prom_rc_malloc");
  llvm::Value *refPtr = m_Builder.CreateBitCast(
      rawRC, llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context)));
  m_Builder.CreateStore(
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1), refPtr);

  llvm::Value *dataPtr = m_Builder.CreateBitCast(
      rawPtr, targetHandleType->getStructElementType(0));
  llvm::Value *u = llvm::UndefValue::get(targetHandleType);
  llvm::Value *handle = m_Builder.CreateInsertValue(u, dataPtr, 0);
  handle = m_Builder.CreateInsertValue(handle, refPtr, 1);
  return handle;
}

void CodeGen::emitSoulAssignment(llvm::Value *soulAddr, llvm::Value *rhsVal,
                                 llvm::Type *type) {
  if (!soulAddr || !rhsVal || !type)
    return;

  llvm::Value *finalRHS = rhsVal;
  if (finalRHS->getType() != type) {
    if (finalRHS->getType()->isIntegerTy() && type->isIntegerTy()) {
      finalRHS = m_Builder.CreateIntCast(finalRHS, type, false);
    } else if (finalRHS->getType()->isFloatingPointTy() &&
               type->isFloatingPointTy()) {
      finalRHS = m_Builder.CreateFPCast(finalRHS, type);
    } else {
      // For other types, try bitcast if sizes match, otherwise it might be an
      // error that Sema should have caught.
      if (m_Module->getDataLayout().getTypeStoreSize(finalRHS->getType()) ==
          m_Module->getDataLayout().getTypeStoreSize(type)) {
        finalRHS = m_Builder.CreateBitCast(finalRHS, type);
      }
    }
  }

  m_Builder.CreateStore(finalRHS, soulAddr);
}

void CodeGen::emitEnvelopeRebind(llvm::Value *handleAddr, llvm::Value *rhsVal,
                                 const TokaSymbol &sym, const Expr *lhsExpr) {
  if (!handleAddr || !rhsVal)
    return;

  if (sym.morphology == Morphology::Shared) {
    // 1. Release(Old)
    llvm::Value *oldVal =
        m_Builder.CreateLoad(rhsVal->getType(), handleAddr, "sh.old_handle");
    emitRelease(oldVal, sym);
    // 2. Update(Handle) with new owning handle from genExpr
    m_Builder.CreateStore(rhsVal, handleAddr);
  } else if (sym.morphology == Morphology::Unique) {
    // Unique: simple Release(Old) + Update
    llvm::Value *oldVal =
        m_Builder.CreateLoad(rhsVal->getType(), handleAddr, "u.old_handle");
    llvm::Value *nn = m_Builder.CreateIsNotNull(oldVal, "u.old_nn");
    llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *freeBB =
        llvm::BasicBlock::Create(m_Context, "u.rel_free", f);
    llvm::BasicBlock *contBB =
        llvm::BasicBlock::Create(m_Context, "u.rel_cont", f);
    m_Builder.CreateCondBr(nn, freeBB, contBB);

    m_Builder.SetInsertPoint(freeBB);
    if (sym.hasDrop && !sym.dropFunc.empty()) {
      if (auto *df = m_Module->getFunction(sym.dropFunc)) {
        m_Builder.CreateCall(df, {oldVal});
      }
    }
    llvm::Function *freeFn = m_Module->getFunction("free");
    if (freeFn) {
      m_Builder.CreateCall(
          freeFn, m_Builder.CreateBitCast(oldVal, m_Builder.getPtrTy()));
    }
    m_Builder.CreateBr(contBB);
    m_Builder.SetInsertPoint(contBB);

    m_Builder.CreateStore(rhsVal, handleAddr);
  } else {
    // Raw/Ref: direct store
    m_Builder.CreateStore(rhsVal, handleAddr);
  }
}

PhysEntity CodeGen::emitAssignment(const Expr *lhsExpr, const Expr *rhsExpr) {
  // 1. Resolve Intent
  bool hasRebind = false;
  const Expr *targetLHS = lhsExpr;
  while (auto *ue = dynamic_cast<const UnaryExpr *>(targetLHS)) {
    if (ue->IsRebindable || ue->Op == TokenType::TokenWrite)
      hasRebind = true;
    targetLHS = ue->RHS.get();
  }

  m_InLHS = true;
  // Physically we don't need to 'gen' the LHS here yet if using emitAssignment
  // structure. But we need to know the address.

  // 2. Resolve LHS Metadata
  TokaSymbol *symLHS = nullptr;
  llvm::Value *lhsAlloca = nullptr;
  if (auto *varLHS = dynamic_cast<const VariableExpr *>(targetLHS)) {
    std::string baseName = varLHS->Name;
    while (!baseName.empty() &&
           (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
            baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!'))
      baseName = baseName.substr(1);
    while (!baseName.empty() &&
           (baseName.back() == '#' || baseName.back() == '?' ||
            baseName.back() == '!'))
      baseName.pop_back();

    if (m_Symbols.count(baseName)) {
      symLHS = &m_Symbols[baseName];
      lhsAlloca = symLHS->allocaPtr;
    }
  }

  // 3. Resolve RHS Value
  llvm::Value *rhsVal = nullptr;

  // [Fix] Handle UnsetExpr (x = unset)
  // Generating code for 'unset' directly returns nullptr/error in genExpr.
  // We must handle it here to produce an UndefValue.
  if (dynamic_cast<const UnsetExpr *>(rhsExpr)) {
    // Generate Undef for LHS Type
    llvm::Type *destTy = nullptr;

    // 1. Try to get from Symbol (Most reliable for variables/morphology)
    if (symLHS) {
      if (hasRebind) {
        if (symLHS->morphology == Morphology::Shared) {
          llvm::Type *ptrTy = llvm::PointerType::getUnqual(symLHS->soulType);
          llvm::Type *refTy =
              llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
          destTy = llvm::StructType::get(m_Context, {ptrTy, refTy});
        } else if (symLHS->morphology == Morphology::Unique) {
          destTy = llvm::PointerType::getUnqual(symLHS->soulType);
        } else {
          destTy = symLHS->soulType;
        }
      } else {
        destTy = symLHS->soulType;
      }
    }

    // 2. Try to get from AST Type (For array index, deref, etc)
    if (!destTy && lhsExpr->ResolvedType) {
      destTy = getLLVMType(lhsExpr->ResolvedType);
    }

    // 3. Fallback: Alloca type (if available and valid)
    if (!destTy && lhsAlloca) {
      if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(lhsAlloca)) {
        destTy = AI->getAllocatedType();
      }
    }

    if (destTy) {
      if (destTy->isPointerTy()) {
        rhsVal = llvm::Constant::getNullValue(destTy);
      } else {
        rhsVal = llvm::UndefValue::get(destTy);
      }
    } else {
      // Cannot infer type for unset assignment. CodeGen error?
      // Sema should have caught this or we rely on explicit typing.
      return nullptr;
    }
  } else {
    // Normal Expr
    m_InLHS = false;
    PhysEntity rhs_ent = genExpr(rhsExpr).load(m_Builder);
    rhsVal = rhs_ent.load(m_Builder);
  }

  if (!rhsVal)
    return nullptr;

  // [Fix] Smart Pointer Assignment Ambiguity
  bool effectiveRebind = hasRebind;
  if (effectiveRebind && symLHS && rhsVal) {
    bool isHandleType = rhsVal->getType()->isStructTy(); // SharedPtr
    if (rhsVal->getType()->isPointerTy()) {
      // Could be RawPtr, UniquePtr, or promoted SharedPtr source
      isHandleType = true;
    }
    // If RHS matches Soul Type exactly, prefer Soul Assignment
    if (rhsVal->getType() == symLHS->soulType) {
      // e.g. s has soul i32. RHS is i32.
      // Even if i32 could be a pointer (no), it matches soul.
      isHandleType = false;
    }

    // Determine strict preference
    if (!isHandleType) {
      effectiveRebind = false;
    }
  }
  if (effectiveRebind && symLHS && lhsAlloca) {
    // Scene B: Envelope Rebind
    if (symLHS->morphology == Morphology::Shared &&
        rhsVal->getType()->isPointerTy()) {
      // Correctly pass the Handle Struct type
      rhsVal =
          emitPromotion(rhsVal, getLLVMType(lhsExpr->ResolvedType), *symLHS);
    }
    emitEnvelopeRebind(lhsAlloca, rhsVal, *symLHS, lhsExpr);
  } else {
    // Scene A: Soul Assignment
    llvm::Value *soulAddr = emitEntityAddr(lhsExpr);
    llvm::Type *destTy = rhsVal->getType();
    if (symLHS && symLHS->morphology == Morphology::None)
      destTy = symLHS->soulType;

    // [Chapter 6 Extension] Nullable Soul Assignment Wrapping
    if (lhsExpr->ResolvedType && lhsExpr->ResolvedType->IsNullable &&
        !lhsExpr->ResolvedType->isPointer() &&
        !lhsExpr->ResolvedType->isSmartPointer() &&
        !lhsExpr->ResolvedType->isReference() &&
        !lhsExpr->ResolvedType->isVoid()) {
      llvm::Type *targetStructTy = getLLVMType(lhsExpr->ResolvedType);
      if (rhsVal->getType() != targetStructTy) {
        // Wrapping T into { T, i1 }
        llvm::Value *wrapped = llvm::UndefValue::get(targetStructTy);
        if (rhsVal->getType() == targetStructTy->getStructElementType(0)) {
          wrapped = m_Builder.CreateInsertValue(wrapped, rhsVal, {0});
          wrapped = m_Builder.CreateInsertValue(
              wrapped,
              llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), 1), {1});
          rhsVal = wrapped;
          destTy = targetStructTy;
        } else if (dynamic_cast<const NoneExpr *>(rhsExpr) ||
                   rhsVal->getType()->isPointerTy()) { // Handle none
          wrapped = m_Builder.CreateInsertValue(
              wrapped,
              llvm::Constant::getNullValue(
                  targetStructTy->getStructElementType(0)),
              {0});
          wrapped = m_Builder.CreateInsertValue(
              wrapped,
              llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), 0), {1});
          rhsVal = wrapped;
          destTy = targetStructTy;
        }
      }
    }
    emitSoulAssignment(soulAddr, rhsVal, destTy);
  }

  m_InLHS = false;
  return PhysEntity(rhsVal, "void", rhsVal->getType(), false);
}

PhysEntity CodeGen::genBinaryExpr(const BinaryExpr *expr) {
  const BinaryExpr *bin = expr;
  if (bin->Op == "=" || bin->Op == "+=" || bin->Op == "-=" || bin->Op == "*=" ||
      bin->Op == "/=") {

    if (bin->Op == "=") {
      return emitAssignment(bin->LHS.get(), bin->RHS.get());
    }

    // Compound Assignment: LHS = LHS op RHS (Always Soul view)
    llvm::Value *soulAddr = emitEntityAddr(bin->LHS.get());
    PhysEntity rhsVal_ent = genExpr(bin->RHS.get()).load(m_Builder);
    llvm::Value *rhsVal = rhsVal_ent.load(m_Builder);
    if (!soulAddr || !rhsVal)
      return nullptr;

    // Determine destType for Load [Fix for Opaque Pointers]
    llvm::Type *destType = nullptr;
    if (auto *ve = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
      std::string baseName = ve->Name;
      while (!baseName.empty() &&
             (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&'))
        baseName = baseName.substr(1);
      while (!baseName.empty() &&
             (baseName.back() == '#' || baseName.back() == '!'))
        baseName.pop_back();

      if (m_Symbols.count(baseName)) {
        destType = m_Symbols[baseName].soulType;
      }
    }

    if (!destType && bin->LHS->ResolvedType) {
      destType = getLLVMType(bin->LHS->ResolvedType);
    }

    if (!destType) {
      destType = rhsVal->getType(); // Fallback
    }

    // Standard Compound Logic
    llvm::Value *lhsVal =
        m_Builder.CreateLoad(destType, soulAddr, "compound.lhs");

    llvm::Type *lhsTy = lhsVal->getType();
    llvm::Type *rhsTy = rhsVal->getType();

    // [Fix] Handle Pointer Arithmetic in Compound Assignment
    if (lhsTy->isPointerTy() && rhsTy->isIntegerTy()) {
      if (bin->Op == "+=" || bin->Op == "-=") {
        llvm::Type *elemTy = nullptr;
        elemTy = llvm::Type::getInt8Ty(m_Context);

        if (rhsTy->getIntegerBitWidth() < 64) {
          rhsVal = m_Builder.CreateSExt(
              rhsVal, llvm::Type::getInt64Ty(m_Context), "idx_ext");
        }
        if (bin->Op == "-=")
          rhsVal = m_Builder.CreateNeg(rhsVal);

        llvm::Value *res =
            m_Builder.CreateInBoundsGEP(elemTy, lhsVal, {rhsVal}, "ptradd");
        m_Builder.CreateStore(res, soulAddr);
        return PhysEntity(res, "void", res->getType(), false);
      }
    }

    // [Fix] Type Promotion for Compound Assignment
    if (lhsTy != rhsTy) {
      if (lhsTy->isIntegerTy() && rhsTy->isIntegerTy()) {
        rhsVal = m_Builder.CreateIntCast(rhsVal, lhsTy, false);
      } else if (lhsTy->isFloatingPointTy() && rhsTy->isFloatingPointTy()) {
        rhsVal = m_Builder.CreateFPCast(rhsVal, lhsTy);
      } else {
        error(bin, "Type mismatch in compound assignment");
        return nullptr;
      }
    }

    llvm::Value *res = nullptr;
    if (lhsTy->isFloatingPointTy()) {
      if (bin->Op == "+=")
        res = m_Builder.CreateFAdd(lhsVal, rhsVal);
      else if (bin->Op == "-=")
        res = m_Builder.CreateFSub(lhsVal, rhsVal);
      else if (bin->Op == "*=")
        res = m_Builder.CreateFMul(lhsVal, rhsVal);
      else if (bin->Op == "/=")
        res = m_Builder.CreateFDiv(lhsVal, rhsVal);
    } else {
      if (bin->Op == "+=")
        res = m_Builder.CreateAdd(lhsVal, rhsVal);
      else if (bin->Op == "-=")
        res = m_Builder.CreateSub(lhsVal, rhsVal);
      else if (bin->Op == "*=")
        res = m_Builder.CreateMul(lhsVal, rhsVal);
      else if (bin->Op == "/=")
        res = m_Builder.CreateSDiv(lhsVal, rhsVal);
    }

    m_Builder.CreateStore(res, soulAddr);
    return res;
  }

  // Logical Operators (Short-circuiting)
  if (bin->Op == "&&") {
    PhysEntity lhs_ent = genExpr(bin->LHS.get()).load(m_Builder);
    llvm::Value *lhs = lhs_ent.load(m_Builder);
    if (!lhs)
      return nullptr;
    if (!lhs->getType()->isIntegerTy(1))
      lhs = m_Builder.CreateICmpNE(
          lhs, llvm::ConstantInt::get(lhs->getType(), 0), "tobool");

    llvm::Function *TheFunction = m_Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *EntryBB = m_Builder.GetInsertBlock();
    llvm::BasicBlock *RHSBB =
        llvm::BasicBlock::Create(m_Context, "land.rhs", TheFunction);
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(m_Context, "land.end");

    m_Builder.CreateCondBr(lhs, RHSBB, MergeBB);

    // Eval RHS
    m_Builder.SetInsertPoint(RHSBB);
    PhysEntity rhs_ent = genExpr(bin->RHS.get()).load(m_Builder);
    llvm::Value *rhs = rhs_ent.load(m_Builder);
    if (!rhs)
      return nullptr;
    if (!rhs->getType()->isIntegerTy(1))
      rhs = m_Builder.CreateICmpNE(
          rhs, llvm::ConstantInt::get(rhs->getType(), 0), "tobool");

    m_Builder.CreateBr(MergeBB);
    RHSBB = m_Builder.GetInsertBlock();

    // Merge
    MergeBB->insertInto(TheFunction);
    m_Builder.SetInsertPoint(MergeBB);
    llvm::PHINode *PN =
        m_Builder.CreatePHI(llvm::Type::getInt1Ty(m_Context), 2, "land.val");
    PN->addIncoming(llvm::ConstantInt::getFalse(m_Context), EntryBB);
    PN->addIncoming(rhs, RHSBB);
    return PN;
  }

  if (bin->Op == "||") {
    PhysEntity lhs_ent = genExpr(bin->LHS.get()).load(m_Builder);
    llvm::Value *lhs = lhs_ent.load(m_Builder);
    if (!lhs)
      return nullptr;
    if (!lhs->getType()->isIntegerTy(1))
      lhs = m_Builder.CreateICmpNE(
          lhs, llvm::ConstantInt::get(lhs->getType(), 0), "tobool");

    llvm::Function *TheFunction = m_Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *EntryBB = m_Builder.GetInsertBlock();
    llvm::BasicBlock *RHSBB =
        llvm::BasicBlock::Create(m_Context, "lor.rhs", TheFunction);
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(m_Context, "lor.end");

    m_Builder.CreateCondBr(lhs, MergeBB, RHSBB);

    // Eval RHS
    m_Builder.SetInsertPoint(RHSBB);
    PhysEntity rhs_ent = genExpr(bin->RHS.get()).load(m_Builder);
    llvm::Value *rhs = rhs_ent.load(m_Builder);
    if (!rhs)
      return nullptr;
    if (!rhs->getType()->isIntegerTy(1))
      rhs = m_Builder.CreateICmpNE(
          rhs, llvm::ConstantInt::get(rhs->getType(), 0), "tobool");

    m_Builder.CreateBr(MergeBB);
    RHSBB = m_Builder.GetInsertBlock();

    // Merge
    MergeBB->insertInto(TheFunction);
    m_Builder.SetInsertPoint(MergeBB);
    llvm::PHINode *PN =
        m_Builder.CreatePHI(llvm::Type::getInt1Ty(m_Context), 2, "lor.val");
    PN->addIncoming(llvm::ConstantInt::getTrue(m_Context), EntryBB);
    PN->addIncoming(rhs, RHSBB);
    return PN;
  }

  // 'is' operator - Specialized 'peek' evaluation to avoid destructive moves
  if (bin->Op == "is") {
    // Special handling for 'var is nullptr' (or '~var is nullptr') to avoid
    // unsafe dereferencing
    const VariableExpr *targetVar = nullptr;
    const Expr *currentLHS = bin->LHS.get();

    // Peel all UnaryExpr layers (e.g. ~?p -> Unary(~) -> Unary(?) -> Var(p))
    while (true) {
      if (auto *ve = dynamic_cast<const VariableExpr *>(currentLHS)) {
        targetVar = ve;
        break;
      } else if (auto *ue = dynamic_cast<const UnaryExpr *>(currentLHS)) {
        currentLHS = ue->RHS.get();
      } else {
        break;
      }
    }

    if (targetVar) {
      if (dynamic_cast<const NullExpr *>(bin->RHS.get())) {
        // Get the base name sans morphology
        std::string baseName = targetVar->Name;
        while (!baseName.empty() && (baseName[0] == '*' || baseName[0] == '#' ||
                                     baseName[0] == '&' || baseName[0] == '^' ||
                                     baseName[0] == '~' || baseName[0] == '!'))
          baseName = baseName.substr(1);
        while (!baseName.empty() &&
               (baseName.back() == '#' || baseName.back() == '?' ||
                baseName.back() == '!'))
          baseName.pop_back();

        if (m_Symbols.count(baseName)) {
          TokaSymbol &sym = m_Symbols[baseName];
          // Shared Pointer: { ptr, ptr }
          if (sym.morphology == Morphology::Shared) {
            llvm::Value *identity = sym.allocaPtr;
            if (identity) {
              // Get pointer to the first element (data ptr)
              llvm::Value *dataPtrAddr = m_Builder.CreateStructGEP(
                  sym.soulType, identity, 0, "sh_data_ptr_addr");
              llvm::Value *dataPtr = m_Builder.CreateLoad(
                  m_Builder.getPtrTy(), dataPtrAddr, "sh_data_ptr");
              return m_Builder.CreateIsNull(dataPtr, "sh_is_null");
            }
          }
        }
      }
    }

    auto evaluatePeek = [&](const Expr *e) -> llvm::Value * {
      const Expr *target = e;
      if (auto *u = dynamic_cast<const UnaryExpr *>(e)) {
        if (u->Op == TokenType::Caret || u->Op == TokenType::Tilde ||
            u->Op == TokenType::Star) {
          target = u->RHS.get();
        }
      }
      if (auto *v = dynamic_cast<const VariableExpr *>(target)) {
        std::string baseName = v->Name;
        while (!baseName.empty() &&
               (baseName.back() == '#' || baseName.back() == '?' ||
                baseName.back() == '!'))
          baseName.pop_back();

        if (m_Symbols.count(baseName) &&
            m_Symbols[baseName].morphology == Morphology::Shared) {
          return genExpr(target).load(m_Builder);
        }
        return getEntityAddr(v->Name);
      }
      return genExpr(e).load(m_Builder);
    };

    llvm::Value *lhsVal = evaluatePeek(bin->LHS.get());
    if (!lhsVal)
      return nullptr;

    llvm::Type *lhsTy = lhsVal->getType();
    if (lhsTy->isStructTy() && lhsTy->getStructNumElements() == 2) {
      // shared pointer: extract raw pointer
      lhsVal = m_Builder.CreateExtractValue(lhsVal, 0, "shared_ptr_val");
      lhsTy = lhsVal->getType();
    } else if (lhsTy->isStructTy() && lhsTy->getStructNumElements() == 1) {
      lhsVal = m_Builder.CreateExtractValue(lhsVal, 0);
      lhsTy = lhsVal->getType();
    }

    // Special Case: 'expr is nullptr' (Null check)
    if (dynamic_cast<const NullExpr *>(bin->RHS.get())) {
      while (lhsVal->getType()->isStructTy() &&
             lhsVal->getType()->getStructNumElements() == 1) {
        lhsVal = m_Builder.CreateExtractValue(lhsVal, 0);
      }

      if (lhsVal->getType()->isIntegerTy()) {
        // ADDR0 is null?
        return m_Builder.CreateICmpEQ(
            lhsVal, llvm::ConstantInt::get(lhsVal->getType(), 0),
            "is_null_int");
      }

      return m_Builder.CreateIsNull(lhsVal, "is_null");
    }

    // Implicit Case: 'expr is Type' or 'expr is pattern' (Not-Null check)
    if (lhsTy->isPointerTy()) {
      return m_Builder.CreateIsNotNull(lhsVal, "is_not_null");
    }
    return llvm::ConstantInt::getTrue(m_Context);
  }

  // Standard Arithmetic and Comparisons
  PhysEntity lhs_ent = genExpr(bin->LHS.get()).load(m_Builder);
  llvm::Value *lhs = lhs_ent.load(m_Builder);
  if (!lhs) {
    return nullptr;
  }

  if (!m_Builder.GetInsertBlock() ||
      m_Builder.GetInsertBlock()->getTerminator()) {
    return nullptr;
  }

  PhysEntity rhs_ent = genExpr(bin->RHS.get()).load(m_Builder);
  llvm::Value *rhs = rhs_ent.load(m_Builder);
  if (!rhs) {
    return nullptr;
  }

  if (!m_Builder.GetInsertBlock() ||
      m_Builder.GetInsertBlock()->getTerminator()) {
    return nullptr;
  }

  llvm::Type *lhsType = lhs->getType();
  llvm::Type *rhsType = rhs->getType();

  // [Fix] Implicit Smart Pointer Dereference (Bridge Sema -> CodeGen)
  // If Sema authorized a Value usage (resolvedType is generic) but we
  // generated a Pointer/Handle, we must unwrap/load the Soul.

  auto unwrapSmartPtr = [&](llvm::Value *val,
                            const Expr *expr) -> llvm::Value * {
    if (!val || !expr || !expr->ResolvedType)
      return val;

    llvm::Type *currentTy = val->getType();
    bool isTargetStruct = currentTy->isStructTy();
    bool isTargetPtr = currentTy->isPointerTy();

    // If we differ from ResolvedType (e.g. Sema says i32, we have {i32*,
    // count*} or i32*) Simple heuristic: If we have a mismatch with the OTHER
    // operand that is solved by dereferencing OR if the ResolvedType itself
    // is not a SmartPointer/Pointer.

    // Check if expr->ResolvedType is NOT a pointer/smart-ptr, but we
    // represent one.
    bool semaIsValue = !expr->ResolvedType->isPointer() &&
                       !expr->ResolvedType->isReference() &&
                       !expr->ResolvedType->isSmartPointer();

    if (semaIsValue) {
      if (isTargetStruct && currentTy->getStructNumElements() == 2) {
        // Shared Pointer Handle: Extract Data Ptr then Load
        llvm::Value *dataPtr =
            m_Builder.CreateExtractValue(val, 0, "smart_deref_ptr");
        // Check if loading is valid (opaque pointers make dataPtr typeless,
        // need Element Type) We rely on ResolvedType to provide the Element
        // Type
        llvm::Type *loadTy = getLLVMType(expr->ResolvedType);
        if (loadTy) {
          return m_Builder.CreateLoad(loadTy, dataPtr, "smart_deref_val");
        }
      } else if (isTargetPtr) {
        // Unique Pointer or Raw Pointer: Load the Value
        // Verify we aren't loading a pointer-to-pointer if the target IS a
        // pointer But semaIsValue=true means target is i32, struct, etc. not
        // pointer.
        llvm::Type *loadTy = getLLVMType(expr->ResolvedType);
        if (loadTy) {
          return m_Builder.CreateLoad(loadTy, val, "ptr_deref_val");
        }
      }
    }
    return val;
  };

  lhs = unwrapSmartPtr(lhs, bin->LHS.get());
  rhs = unwrapSmartPtr(rhs, bin->RHS.get());

  // Refresh types after unwrap
  lhsType = lhs->getType();
  rhsType = rhs->getType();

  bool isPtrArith =
      (lhsType->isPointerTy() && rhsType->isIntegerTy()) ||
      (rhsType->isPointerTy() && lhsType->isIntegerTy() && bin->Op == "+");

  if (lhsType != rhsType && !isPtrArith) {
    if (lhsType->isPointerTy() && rhsType->isPointerTy()) {
      rhs = m_Builder.CreateBitCast(rhs, lhsType);
    } else if (lhsType->isPointerTy() && rhsType->isIntegerTy()) {
      rhs = m_Builder.CreateIntToPtr(rhs, lhsType);
    } else if (lhsType->isIntegerTy() && rhsType->isPointerTy()) {
      lhs = m_Builder.CreateIntToPtr(lhs, rhsType);
    } else if (lhsType->isIntegerTy() && rhsType->isIntegerTy()) {
      // Promote to widest
      if (lhsType->getIntegerBitWidth() < rhsType->getIntegerBitWidth()) {
        bool isUnsigned = false;
        if (bin->LHS->ResolvedType) {
          std::string lty = bin->LHS->ResolvedType->toString();
          if (lty.size() >= 1 && lty[0] == 'u')
            isUnsigned = true;
        }
        if (isUnsigned)
          lhs = m_Builder.CreateZExt(lhs, rhsType, "lhs_ext");
        else
          lhs = m_Builder.CreateSExt(lhs, rhsType, "lhs_ext");
        lhsType = rhsType;
      } else if (lhsType->getIntegerBitWidth() >
                 rhsType->getIntegerBitWidth()) {
        bool isUnsigned = false;
        if (bin->RHS->ResolvedType) {
          std::string rty = bin->RHS->ResolvedType->toString();
          if (rty.size() >= 1 && rty[0] == 'u')
            isUnsigned = true;
        }
        if (isUnsigned)
          rhs = m_Builder.CreateZExt(rhs, lhsType, "rhs_ext");
        else
          rhs = m_Builder.CreateSExt(rhs, lhsType, "rhs_ext");
        rhsType = lhsType;
      }
    } else {
      if (lhsType != rhsType) {
        std::string ls, rs;
        llvm::raw_string_ostream los(ls), ros(rs);
        lhsType->print(los);
        rhsType->print(ros);
        error(bin, "Type mismatch in binary expression: " + ls + " vs " + rs);
        return nullptr;
      }
    }
  }

  if (!lhsType->isIntOrIntVectorTy() && !lhsType->isPtrOrPtrVectorTy() &&
      !lhsType->isFloatingPointTy()) {
    std::string s;
    llvm::raw_string_ostream os(s);
    lhsType->print(os);
    error(bin, "Invalid type for comparison: " + os.str() +
                   ". Comparisons are only allowed for scalars "
                   "(integers/pointers).");
    return nullptr;
  }

  // Final check to avoid assertion
  auto unwrapHandle = [&](llvm::Value *v) -> llvm::Value * {
    if (!v)
      return nullptr;
    while (v->getType()->isStructTy()) {
      unsigned numElems = v->getType()->getStructNumElements();
      if (numElems == 1 || numElems == 2) {
        v = m_Builder.CreateExtractValue(v, 0);
      } else {
        break;
      }
    }
    return v;
  };

  if (lhsType->isFloatingPointTy() && rhsType->isFloatingPointTy()) {
    if (bin->Op == "+")
      return m_Builder.CreateFAdd(lhs, rhs, "addtmp");
    if (bin->Op == "-")
      return m_Builder.CreateFSub(lhs, rhs, "subtmp");
    if (bin->Op == "*")
      return m_Builder.CreateFMul(lhs, rhs, "multmp");
    if (bin->Op == "/")
      return m_Builder.CreateFDiv(lhs, rhs, "divtmp");
    if (bin->Op == "<")
      return m_Builder.CreateFCmpOLT(lhs, rhs, "lt_tmp");
    if (bin->Op == ">")
      return m_Builder.CreateFCmpOGT(lhs, rhs, "gt_tmp");
    if (bin->Op == "<=")
      return m_Builder.CreateFCmpOLE(lhs, rhs, "le_tmp");
    if (bin->Op == ">=")
      return m_Builder.CreateFCmpOGE(lhs, rhs, "ge_tmp");
    if (bin->Op == "==")
      return m_Builder.CreateFCmpOEQ(lhs, rhs, "eq_tmp");
    if (bin->Op == "!=")
      return m_Builder.CreateFCmpONE(lhs, rhs, "ne_tmp");
  }

  if (bin->Op == "+") {
    if (lhs->getType()->isPointerTy()) {
      llvm::Type *elemTy = llvm::Type::getInt8Ty(m_Context);

      // Ensure RHS is 64-bit for GEP
      if (rhs->getType()->isIntegerTy() &&
          rhs->getType()->getIntegerBitWidth() < 64) {
        rhs = m_Builder.CreateSExt(rhs, llvm::Type::getInt64Ty(m_Context),
                                   "idx_ext");
      }
      return m_Builder.CreateGEP(elemTy, lhs, {rhs}, "ptradd");
    }
    return m_Builder.CreateAdd(lhs, rhs, "addtmp");
  }
  if (bin->Op == "-") {
    if (lhs->getType()->isPointerTy()) {
      llvm::Type *elemTy = llvm::Type::getInt8Ty(m_Context);
      llvm::Value *negR = m_Builder.CreateNeg(rhs);
      return m_Builder.CreateGEP(elemTy, lhs, {negR}, "ptrsub");
    }
    return m_Builder.CreateSub(lhs, rhs, "subtmp");
  }
  if (bin->Op == "*")
    return m_Builder.CreateMul(lhs, rhs, "multmp");
  if (bin->Op == "/")
    return m_Builder.CreateSDiv(lhs, rhs, "divtmp");
  if (bin->Op == "<") {
    lhs = unwrapHandle(lhs);
    rhs = unwrapHandle(rhs);
    return m_Builder.CreateICmpSLT(lhs, rhs, "lt_tmp");
  }
  if (bin->Op == ">") {
    lhs = unwrapHandle(lhs);
    rhs = unwrapHandle(rhs);
    return m_Builder.CreateICmpSGT(lhs, rhs, "gt_tmp");
  }
  if (bin->Op == "<=") {
    return m_Builder.CreateICmpSLE(lhs, rhs, "le_tmp");
  }
  if (bin->Op == ">=") {
    return m_Builder.CreateICmpSGE(lhs, rhs, "ge_tmp");
  }
  if (bin->Op == "==" || bin->Op == "!=") {
    // 1. Unwrap Single-Element Structs (Strong Types)
    lhs = unwrapHandle(lhs);
    rhs = unwrapHandle(rhs);

    if (lhs->getType() != rhs->getType()) {
      if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
        if (lhs->getType()->getIntegerBitWidth() >
            rhs->getType()->getIntegerBitWidth())
          rhs = m_Builder.CreateZExt(rhs, lhs->getType());
        else
          lhs = m_Builder.CreateZExt(lhs, rhs->getType());
      } else if (lhs->getType()->isPointerTy() &&
                 rhs->getType()->isPointerTy()) {
        rhs = m_Builder.CreateBitCast(rhs, lhs->getType());
      } else {
        // Ptr vs Int Mismatch (e.g. ptr == ADDR0)
        if (lhs->getType()->isPointerTy() && rhs->getType()->isIntegerTy()) {
          lhs = m_Builder.CreatePtrToInt(lhs, rhs->getType());
        } else if (lhs->getType()->isIntegerTy() &&
                   rhs->getType()->isPointerTy()) {
          rhs = m_Builder.CreatePtrToInt(rhs, lhs->getType());
        }
      }
    }

    // Final check to avoid assertion
    if (!lhs->getType()->isIntOrIntVectorTy() &&
        !lhs->getType()->isPtrOrPtrVectorTy()) {
      // If still struct (e.g. multi-field shape), comparison is invalid
      // unless we implement deep comparison (not done yet)
      // error(bin, "Cannot compare compound types");
      // return llvm::ConstantInt::getFalse(m_Context);
      // For now, let it fail or default
    }
    llvm::Value *cmp = m_Builder.CreateICmpEQ(lhs, rhs, "eq_tmp");
    if (bin->Op == "!=")
      return m_Builder.CreateNot(cmp, "ne_tmp");
    return cmp;
  }
  if (bin->Op == "!=") {
    // Should have been handled above
    return nullptr;
  }
  if (bin->Op == "band")
    return m_Builder.CreateAnd(lhs, rhs, "andtmp");
  if (bin->Op == "bor")
    return m_Builder.CreateOr(lhs, rhs, "ortmp");
  if (bin->Op == "bxor")
    return m_Builder.CreateXor(lhs, rhs, "xortmp");
  if (bin->Op == "bshl")
    return m_Builder.CreateShl(lhs, rhs, "shltmp");
  if (bin->Op == "bshr") {
    // Check signedness of LHS
    if (lhs->getType()->isIntegerTy()) {
      // If type implies signedness (in Toka Types, not LLVM types which are
      // opaque) We need to look up source type. But genBinaryExpr has limited
      // Type info unless passed or resolved. Since Toka relies on Sema for
      // types, CodeGen often relies on 'isSigned' property if stored? LLVM
      // integer types don't carry sign. Does CodeGen store resolved types in
      // AST? Yes, Bin->LHS->ResolvedType.
      bool isSigned = false;
      if (bin->LHS->ResolvedType) {
        isSigned = bin->LHS->ResolvedType->isSignedInteger();
        // Or check if it starts with 'i' vs 'u'.
        // AST ResolvedType is standard way.
      } else {
        // Fallback: Default to arithmetic right shift for safety? Or logical?
        // C uses arith for signed, logical for unsigned.
        // If we don't know, AShr is usually safer for general math, LShr for
        // bitwise. Toka "bshr" is explicitly Bitwise. However, user said
        // "i8...i64 -> ashr", "u8...u64 -> lshr". We MUST check type. Let's
        // assume ResolvedType is populated by Sema. Checking ResolvedType.
      }
      if (isSigned)
        return m_Builder.CreateAShr(lhs, rhs, "ashrtmp");
      else
        return m_Builder.CreateLShr(lhs, rhs, "lshrtmp");
    }
    return m_Builder.CreateLShr(lhs, rhs, "lshrtmp"); // Default
  }
  return nullptr;
}

PhysEntity CodeGen::genUnaryExpr(const UnaryExpr *unary) {
  std::cerr << "DEBUG: genUnaryExpr op=" << (int)unary->Op << "\n";
  if (unary->Op == TokenType::PlusPlus || unary->Op == TokenType::MinusMinus) {
    llvm::Value *addr = genAddr(unary->RHS.get());
    if (!addr)
      return nullptr;
    llvm::Type *type = nullptr;
    if (auto *var = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      if (m_Symbols.count(var->Name))
        type = m_Symbols[var->Name].soulType;
    } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(addr)) {
      type = gep->getResultElementType();
    } else if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(addr)) {
      type = alloca->getAllocatedType();
    }
    if (!type)
      return nullptr;
    llvm::Value *oldVal = m_Builder.CreateLoad(type, addr, "pre_old");
    llvm::Value *newVal;
    if (unary->Op == TokenType::PlusPlus)
      newVal = m_Builder.CreateAdd(oldVal, llvm::ConstantInt::get(type, 1),
                                   "preinc_new");
    else
      newVal = m_Builder.CreateSub(oldVal, llvm::ConstantInt::get(type, 1),
                                   "predec_new");
    m_Builder.CreateStore(newVal, addr);
    return newVal;
  }

  if (unary->Op == TokenType::KwBnot) {
    PhysEntity rhs_ent = genExpr(unary->RHS.get()).load(m_Builder);
    llvm::Value *rhs = rhs_ent.load(m_Builder);
    if (!rhs)
      return nullptr;
    return m_Builder.CreateNot(rhs, "nottmp");
  }

  // [Constitution 1.3] Morphology symbols: *p (Raw Pointer Identity)
  if (unary->Op == TokenType::Star) {
    // A handle is something that contains the pointer (Unique/Shared/Reference)
    bool isHandle = false;
    if (unary->RHS->ResolvedType) {
      auto t = unary->RHS->ResolvedType;
      // Note: We only treat it as a handle if it's a Top-Level pointer
      // variable/member access. If it's an indexed access like raw[i], it's a
      // value.
      if ((t->isPointer() || t->isReference() || t->isSmartPointer()) &&
          !dynamic_cast<const ArrayIndexExpr *>(unary->RHS.get())) {
        isHandle = true;
      }
    }

    if (isHandle) {
      llvm::Value *identityAddr = emitHandleAddr(unary->RHS.get());
      if (!identityAddr)
        return nullptr;
      llvm::Type *ptrTy = m_Builder.getPtrTy();
      if (unary->RHS->ResolvedType)
        ptrTy = getLLVMType(unary->RHS->ResolvedType);
      llvm::Value *val = m_Builder.CreateLoad(ptrTy, identityAddr, "raw.ident");
      std::string typeName =
          unary->RHS->ResolvedType ? unary->RHS->ResolvedType->toString() : "";
      return PhysEntity(val, typeName, ptrTy, false);
    } else {
      // It's a value (like raw[start] or a simple local i32). *p is the
      // address.
      llvm::Value *addr = genAddr(unary->RHS.get());
      if (!addr)
        return nullptr;
      std::string typeName = "";
      if (unary->RHS->ResolvedType)
        typeName = "*" + unary->RHS->ResolvedType->toString();
      return PhysEntity(addr, typeName, addr->getType(), false);
    }
  }

  // [Constitution 1.3] Reference Sigil: &p (Static Borrow)
  if (unary->Op == TokenType::Ampersand) {
    llvm::Value *soulAddr = emitEntityAddr(unary->RHS.get());
    std::string typeName = "";
    if (unary->RHS->ResolvedType)
      typeName = "&" + unary->RHS->ResolvedType->toString();
    return PhysEntity(soulAddr, typeName, m_Builder.getPtrTy(), false);
  }

  // [Constitution 1.3] Morphology symbols: ^p, ~p
  if (unary->Op == TokenType::Caret) {
    llvm::Value *identityAddr = emitHandleAddr(unary->RHS.get());
    if (!identityAddr)
      return nullptr;

    // Favor actual symbol handle type over resolved soul type
    llvm::Type *handleTy = nullptr;
    if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      std::string baseName = v->Name;
      while (!baseName.empty() && (baseName[0] == '*' || baseName[0] == '^' ||
                                   baseName[0] == '~' || baseName[0] == '&'))
        baseName = baseName.substr(1);
      if (m_Symbols.count(baseName)) {
        TokaSymbol &sym = m_Symbols[baseName];
        if (sym.morphology == Morphology::Shared) {
          llvm::Type *ptrTy = llvm::PointerType::getUnqual(sym.soulType);
          llvm::Type *refTy =
              llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
          handleTy = llvm::StructType::get(m_Context, {ptrTy, refTy});
        } else if (sym.morphology == Morphology::Unique ||
                   sym.morphology == Morphology::Raw) {
          handleTy = m_Builder.getPtrTy();
        }
      }
    }
    if (!handleTy)
      handleTy = getLLVMType(unary->RHS->ResolvedType);

    llvm::Value *val = m_Builder.CreateLoad(handleTy, identityAddr, "move.val");

    // For Move (^): Nullify the handle
    if (handleTy->isPointerTy()) {
      m_Builder.CreateStore(llvm::ConstantPointerNull::get(
                                llvm::cast<llvm::PointerType>(handleTy)),
                            identityAddr);
    } else if (handleTy->isStructTy()) {
      m_Builder.CreateStore(llvm::ConstantAggregateZero::get(handleTy),
                            identityAddr);
    }

    std::string typeName =
        unary->RHS->ResolvedType ? unary->RHS->ResolvedType->toString() : "";
    return PhysEntity(val, typeName, handleTy, false);
  }

  if (unary->Op == TokenType::Tilde) {
    llvm::Value *identityAddr = emitHandleAddr(unary->RHS.get());
    if (!identityAddr)
      return nullptr;

    // Favor actual symbol handle type
    llvm::Type *handleTy = nullptr;
    if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      std::string baseName = v->Name;
      while (!baseName.empty() && (baseName[0] == '*' || baseName[0] == '^' ||
                                   baseName[0] == '~' || baseName[0] == '&'))
        baseName = baseName.substr(1);
      if (m_Symbols.count(baseName)) {
        TokaSymbol &sym = m_Symbols[baseName];
        if (sym.morphology == Morphology::Shared) {
          llvm::Type *ptrTy = llvm::PointerType::getUnqual(sym.soulType);
          llvm::Type *refTy =
              llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
          handleTy = llvm::StructType::get(m_Context, {ptrTy, refTy});
        }
      }
    }
    if (!handleTy)
      handleTy = getLLVMType(unary->RHS->ResolvedType);

    llvm::Value *val =
        m_Builder.CreateLoad(handleTy, identityAddr, "share.val");
    emitAcquire(val);

    std::string typeName =
        unary->RHS->ResolvedType ? unary->RHS->ResolvedType->toString() : "";
    return PhysEntity(val, typeName, handleTy, false);
  }

  PhysEntity rhs_ent = genExpr(unary->RHS.get()).load(m_Builder);
  llvm::Value *rhs = rhs_ent.load(m_Builder);
  if (!rhs)
    return nullptr;
  if (unary->Op == TokenType::Bang) {
    return m_Builder.CreateNot(rhs, "nottmp");
  } else if (unary->Op == TokenType::Minus) {
    if (rhs->getType()->isFloatingPointTy())
      return m_Builder.CreateFNeg(rhs, "negtmp");
    return m_Builder.CreateNeg(rhs, "negtmp");
  } else if (unary->Op == TokenType::TokenNull) {
    // Morphology pass-through
    return rhs_ent;
  }
  return nullptr;
}

PhysEntity CodeGen::genCastExpr(const CastExpr *cast) {
  if (!cast->Expression)
    return nullptr;

  bool targetIsOAddr = (cast->TargetType == "OAddr");
  const UnaryExpr *UE = dynamic_cast<const UnaryExpr *>(cast->Expression.get());
  bool isCaret = (UE && UE->Op == TokenType::Caret);

  PhysEntity srcEnt;
  if (targetIsOAddr && isCaret) {
    if (const VariableExpr *v =
            dynamic_cast<const VariableExpr *>(UE->RHS.get())) {
      std::string vName = v->Name;
      while (!vName.empty() && (vName.back() == '?' || vName.back() == '!'))
        vName.pop_back();
      llvm::Value *alloca = getIdentityAddr(vName);
      if (alloca) {
        llvm::Value *ptrVal =
            m_Builder.CreateLoad(m_Builder.getPtrTy(), alloca, vName + ".peek");
        srcEnt = PhysEntity(ptrVal, v->Name, m_Builder.getPtrTy(), false);
      } else {
        srcEnt = genExpr(cast->Expression.get());
      }
    } else if (const MemberExpr *m =
                   dynamic_cast<const MemberExpr *>(UE->RHS.get())) {
      PhysEntity meEnt = genAddr(m);
      if (meEnt.value && meEnt.isAddress) {
        llvm::Value *ptrVal = m_Builder.CreateLoad(
            m_Builder.getPtrTy(), meEnt.value, m->Member + ".peek");
        srcEnt = PhysEntity(ptrVal, m->Member, m_Builder.getPtrTy(), false);
      } else {
        srcEnt = genExpr(cast->Expression.get());
      }
    } else {
      srcEnt = genExpr(cast->Expression.get());
    }
  } else {
    srcEnt = genExpr(cast->Expression.get());
  }

  // Rule: Union L-Value Reinterpretation
  std::shared_ptr<Type> srcTypeObj = cast->Expression->ResolvedType;
  if (srcEnt.isAddress && srcTypeObj && srcTypeObj->isShape()) {
    auto st = std::dynamic_pointer_cast<ShapeType>(srcTypeObj);
    if (st->Decl && st->Decl->Kind == ShapeKind::Union) {
      llvm::Value *addr = srcEnt.value;
      llvm::Type *destTy = nullptr;
      if (cast->ResolvedType) {
        destTy = getLLVMType(cast->ResolvedType);
      } else {
        destTy = resolveType(cast->TargetType, false);
      }
      // [CRITICAL] bitcast address, preserving L-Value. DO NOT LOAD.
      llvm::Value *newAddr =
          m_Builder.CreateBitCast(addr, destTy->getPointerTo());
      return PhysEntity(newAddr, cast->TargetType, destTy, true);
    }
  }

  PhysEntity val_ent = srcEnt.load(m_Builder);
  llvm::Value *val = val_ent.load(m_Builder);
  if (!val)
    return nullptr;
  llvm::Type *targetType = nullptr;
  if (cast->ResolvedType) {
    targetType = getLLVMType(cast->ResolvedType);
  } else {
    targetType = resolveType(cast->TargetType, false);
  }
  if (!targetType)
    return val;

  // Rule: Unwrap Smart Pointer handles (like SharedPtr) if casting to
  // integer/pointer.
  // [Fix] Skip unwrap for Nullable Soul wrappers { T, i1 } and regular shapes.
  if (val->getType()->isStructTy() &&
      !val->getType()->isStructTy()) { // Original logic was here
    // Placeholder for removal of old aggressive logic
  }

  llvm::Type *srcType = val->getType();
  if (srcType->isIntegerTy() && targetType->isIntegerTy()) {
    bool isSigned = false;
    if (cast->ResolvedType) {
      isSigned = cast->ResolvedType->isSignedInteger();
    } else {
      // Fallback: check target type string if ResolvedType is missing
      isSigned = (cast->TargetType.size() > 0 && cast->TargetType[0] == 'i');
    }
    return PhysEntity(m_Builder.CreateIntCast(val, targetType, isSigned),
                      cast->TargetType, targetType, false);
  }

  // [Fix] Safe Nullable Soul Wrap in Cast
  if (targetType->isStructTy() && targetType->getStructNumElements() == 2 &&
      targetType->getStructElementType(1)->isIntegerTy(1)) {
    // 1. T -> T?
    if (srcType == targetType->getStructElementType(0)) {
      llvm::Value *wrapped = llvm::UndefValue::get(targetType);
      wrapped = m_Builder.CreateInsertValue(wrapped, val, {0});
      wrapped = m_Builder.CreateInsertValue(
          wrapped, llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), 1),
          {1});
      return PhysEntity(wrapped, cast->TargetType, targetType, false);
    }
    // 2. none/null -> T?
    if (dynamic_cast<const NoneExpr *>(cast->Expression.get()) ||
        (srcType->isPointerTy() && llvm::isa<llvm::ConstantPointerNull>(val))) {
      llvm::Value *wrapped = llvm::UndefValue::get(targetType);
      wrapped = m_Builder.CreateInsertValue(
          wrapped,
          llvm::Constant::getNullValue(targetType->getStructElementType(0)),
          {0});
      wrapped = m_Builder.CreateInsertValue(
          wrapped, llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), 0),
          {1});
      return PhysEntity(wrapped, cast->TargetType, targetType, false);
    }
  }

  // Floating Point Conversions
  if (srcType->isFloatingPointTy() && targetType->isFloatingPointTy()) {
    return PhysEntity(m_Builder.CreateFPCast(val, targetType, "fp_cast"),
                      cast->TargetType, targetType, false);
  }
  if (srcType->isFloatingPointTy() && targetType->isIntegerTy()) {
    return PhysEntity(m_Builder.CreateFPToSI(val, targetType, "fp_to_int"),
                      cast->TargetType, targetType, false);
  }
  if (srcType->isIntegerTy() && targetType->isFloatingPointTy()) {
    return PhysEntity(m_Builder.CreateSIToFP(val, targetType, "int_to_fp"),
                      cast->TargetType, targetType, false);
  }

  // Physical Interpretation: bitcast or int-ptr cast if types are different
  if (srcType != targetType) {
    if (srcType->isPointerTy() && targetType->isPointerTy()) {
      return PhysEntity(m_Builder.CreateBitCast(val, targetType),
                        cast->TargetType, targetType, false);
    }
    if (srcType->isPointerTy() && targetType->isIntegerTy()) {
      return PhysEntity(m_Builder.CreatePtrToInt(val, targetType),
                        cast->TargetType, targetType, false);
    }
    if (srcType->isIntegerTy() && targetType->isPointerTy()) {
      return PhysEntity(m_Builder.CreateIntToPtr(val, targetType),
                        cast->TargetType, targetType, false);
    }
    // If one is not a pointer, we need alloca/bitcast (Zero-cost
    // GEP/Address logic)
    llvm::Value *tmp = m_Builder.CreateAlloca(srcType);
    m_Builder.CreateStore(val, tmp);
    llvm::Value *castPtr =
        m_Builder.CreateBitCast(tmp, llvm::PointerType::getUnqual(targetType));
    // Propagate TargetType as the semantic type name
    return PhysEntity(m_Builder.CreateLoad(targetType, castPtr),
                      cast->TargetType, targetType, false);
  }
  // Propagate TargetType as the semantic type name
  return PhysEntity(val, cast->TargetType, targetType, false);
}

PhysEntity CodeGen::genVariableExpr(const VariableExpr *var) {
  // [Annotated AST] Constant Substitution: RValue Generation

  llvm::Value *soulAddr = nullptr;
  bool isShared = false;
  std::string varName = var->Name;
  std::string checkName = varName;
  // Strip morphology for lookup
  while (!checkName.empty() &&
         (checkName.back() == '?' || checkName.back() == '!'))
    checkName.pop_back();

  // Use getEntityAddr to get the Soul address (fully dereferenced if needed)
  soulAddr = getEntityAddr(var->Name);

  if (var->ResolvedType && var->ResolvedType->isSharedPtr()) {
    isShared = true;
    soulAddr = getIdentityAddr(var->Name); // [Fix] Handle Address for RValue
  } else if (m_Symbols.count(checkName) &&
             m_Symbols[checkName].morphology == Morphology::Shared) {
    isShared = true;
    soulAddr = getIdentityAddr(checkName); // [Fix] Handle Address for RValue
  }

  if (!soulAddr) {
    return nullptr;
  }

  // Get the base name (no morphology) for symbol lookup
  std::string baseName = var->Name;
  while (!baseName.empty() &&
         (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
          baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!'))
    baseName = baseName.substr(1);
  while (!baseName.empty() &&
         (baseName.back() == '#' || baseName.back() == '?' ||
          baseName.back() == '!'))
    baseName.pop_back();

  llvm::Type *soulType = nullptr;
  if (m_Symbols.count(baseName)) {
    soulType = m_Symbols[baseName].soulType;
  } else if (var->ResolvedType) {
    // [New] Use ResolvedType as fallback if not in symbols
    soulType = getLLVMType(var->ResolvedType);
  } else {
    // Fallback for globals/externs
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(soulAddr)) {
      soulType = ai->getAllocatedType();
    } else if (auto *li = llvm::dyn_cast<llvm::LoadInst>(soulAddr)) {
      soulType = li->getType();
    } else if (auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(soulAddr)) {
      soulType = gv->getValueType();
    }
  }

  // [Fix] Shared Pointer Handle Type Correction
  if (isShared && soulType) {
    llvm::Type *ptrTy = llvm::PointerType::getUnqual(soulType);
    llvm::Type *refTy =
        llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
    soulType = llvm::StructType::get(m_Context, {ptrTy, refTy});
  } else if (var->ResolvedType && var->ResolvedType->isUniquePtr() &&
             soulType) {
    // [Fix] Unique Pointer Handle is Ptr to Soul
    // The symbol stores the Soul type (Data), but the alloca stores Data*.
    soulType = llvm::PointerType::getUnqual(soulType);
  }

  if (!soulType) {
    // Last resort for opaque pointers if we really don't know the type
    // (though soulType should be set for all Toka-defined variables)
    soulType = m_Builder.getPtrTy();
  }

  std::string typeName = "";
  TokaSymbol *sym = nullptr;
  if (m_Symbols.count(baseName)) {
    typeName = m_Symbols[baseName].typeName;
    sym = &m_Symbols[baseName];
  } else if (m_TypeToName.count(soulType)) {
    typeName = m_TypeToName[soulType];
  }

  // [Fix] Uniform Ownership Transfer for Smart Pointers
  // If we are using a Smart Pointer as an RValue (not in LHS), we must perform
  // an ownership transfer to ensure it survives the current statement.
  bool isUnique = (var->ResolvedType && var->ResolvedType->isUniquePtr());
  if (sym && sym->morphology == Morphology::Unique)
    isUnique = true;

  // [Fix] Handle Type Adjustment for Unique Pointers
  if (isUnique && soulType && !soulType->isPointerTy()) {
    soulType = llvm::PointerType::getUnqual(soulType);
  }

  if (!m_InLHS && soulAddr && !llvm::isa<llvm::Function>(soulAddr) &&
      !llvm::isa<llvm::GlobalVariable>(soulAddr)) {

    if (var->ResolvedType && var->ResolvedType->isSharedPtr()) {
      // SharedPtr: Share (Load + Acquire)
      llvm::Value *val = m_Builder.CreateLoad(soulType, soulAddr, "share.val");
      emitAcquire(val);
      return PhysEntity(val, typeName, soulType, false); // Return RValue
    }
  }

  return PhysEntity(soulAddr, typeName, soulType, true);
}

PhysEntity CodeGen::genLiteralExpr(const Expr *expr) {
  if (auto *num = dynamic_cast<const NumberExpr *>(expr)) {
    if (num->Value > 2147483647) {
      return llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context),
                                    num->Value);
    }
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context),
                                  num->Value);
  }
  if (auto *flt = dynamic_cast<const FloatExpr *>(expr)) {
    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(m_Context),
                                 flt->Value);
  }
  if (auto *bl = dynamic_cast<const BoolExpr *>(expr)) {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), bl->Value);
  }
  if (dynamic_cast<const NullExpr *>(expr) ||
      dynamic_cast<const NoneExpr *>(expr)) {
    return llvm::ConstantPointerNull::get(m_Builder.getPtrTy());
  }
  if (auto *str = dynamic_cast<const StringExpr *>(expr)) {
    return m_Builder.CreateGlobalStringPtr(str->Value);
  }
  return nullptr;
}

llvm::Value *CodeGen::genNullCheck(llvm::Value *val, const ASTNode *node,
                                   const std::string &msg) {
  if (!val)
    return val;

  llvm::Value *nn = nullptr;
  if (val->getType()->isPointerTy()) {
    nn = m_Builder.CreateIsNotNull(val, "nn_check");
  } else if (val->getType()->isIntegerTy(1)) {
    // For Soul flags, nn is just the value itself
    nn = val;
  } else {
    return val;
  }
  llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *okBB = llvm::BasicBlock::Create(m_Context, "nn.ok", f);
  llvm::BasicBlock *panicBB =
      llvm::BasicBlock::Create(m_Context, "nn.panic", f);
  m_Builder.CreateCondBr(nn, okBB, panicBB);

  m_Builder.SetInsertPoint(panicBB);

  // Precision Panic: call __toka_panic(message, file, line)
  llvm::Function *panicFunc = m_Module->getFunction("__toka_panic");
  if (!panicFunc) {
    // Declare if not found (happens during early codegen stages or if not
    // imported)
    std::vector<llvm::Type *> panicArgs = {
        m_Builder.getPtrTy(), m_Builder.getPtrTy(), m_Builder.getInt32Ty()};
    llvm::FunctionType *panicFT =
        llvm::FunctionType::get(m_Builder.getVoidTy(), panicArgs, false);
    panicFunc = llvm::Function::Create(panicFT, llvm::Function::ExternalLinkage,
                                       "__toka_panic", m_Module.get());
  }

  // Extract location info
  std::string fileName = "";
  int line = -1;
  if (node) {
    auto fullLoc = DiagnosticEngine::SrcMgr->getFullSourceLoc(node->Loc);
    if (fullLoc.isValid()) {
      fileName = fullLoc.FileName;
      line = (int)fullLoc.Line;
    }
  }

  std::vector<llvm::Value *> args;
  args.push_back(m_Builder.CreateGlobalStringPtr(msg, "panic_msg"));
  args.push_back(m_Builder.CreateGlobalStringPtr(fileName, "panic_file"));
  args.push_back(m_Builder.getInt32(line));

  m_Builder.CreateCall(panicFunc, args);

  // Ensure execution terminates even if __toka_panic somehow returns (it
  // shouldn't)
  llvm::Function *trap =
      llvm::Intrinsic::getDeclaration(m_Module.get(), llvm::Intrinsic::trap);
  m_Builder.CreateCall(trap);
  m_Builder.CreateUnreachable();

  m_Builder.SetInsertPoint(okBB);
  return val;
}

PhysEntity CodeGen::genMatchExpr(const MatchExpr *expr) {
  PhysEntity targetVal_ent = genExpr(expr->Target.get()).load(m_Builder);
  llvm::Value *targetVal = targetVal_ent.load(m_Builder);
  llvm::Type *targetType = targetVal->getType();
  std::string shapeName;
  if (targetType->isStructTy() && m_TypeToName.count(targetType)) {
    shapeName = m_TypeToName[targetType];
  }

  // Create result alloca (Assume i32 for now, ideally get from Sema or
  // expr)
  llvm::Type *resultType = llvm::Type::getInt32Ty(m_Context);
  llvm::AllocaInst *resultAddr =
      m_Builder.CreateAlloca(resultType, nullptr, "match_result_addr");

  // Store target to temp alloca for addressing
  llvm::Value *targetAddr =
      m_Builder.CreateAlloca(targetType, nullptr, "match_target_addr");
  m_Builder.CreateStore(targetVal, targetAddr);

  llvm::Function *func = m_Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(m_Context, "match_merge", func);

  // For Enums, we use a Switch
  if (shapeName != "" && m_Shapes.count(shapeName) &&
      m_Shapes[shapeName]->Kind == ShapeKind::Enum) {
    const ShapeDecl *sh = m_Shapes[shapeName];
    llvm::Value *tagVal = m_Builder.CreateExtractValue(targetVal, 0, "tag");
    llvm::BasicBlock *defaultBB =
        llvm::BasicBlock::Create(m_Context, "match_default", func);
    llvm::SwitchInst *sw =
        m_Builder.CreateSwitch(tagVal, defaultBB, expr->Arms.size());

    for (const auto &arm : expr->Arms) {
      // Find variant index for this arm
      int tag = -1;
      const ShapeMember *variant = nullptr;
      // We look for the first variant name in the pattern
      // Simplified: Assume Decons pattern at top level
      for (size_t i = 0; i < sh->Members.size(); ++i) {
        std::string patName = arm->Pat->Name;
        if (patName.find("::") != std::string::npos) {
          patName = patName.substr(patName.find("::") + 2);
        }
        if (sh->Members[i].Name == patName) {
          tag = (sh->Members[i].TagValue == -1) ? (int)i
                                                : (int)sh->Members[i].TagValue;
          variant = &sh->Members[i];
          break;
        }
      }

      if (tag != -1) {
        llvm::BasicBlock *caseBB =
            llvm::BasicBlock::Create(m_Context, "case_" + variant->Name, func);
        sw->addCase(m_Builder.getInt8(tag), caseBB);
        m_Builder.SetInsertPoint(caseBB);

        m_ScopeStack.push_back({});
        // Bind Payload if exists
        if (!arm->Pat->SubPatterns.empty() && variant) {
          llvm::Value *payloadAddr =
              m_Builder.CreateStructGEP(targetType, targetAddr, 1);

          llvm::Type *payloadLayoutType = nullptr;
          std::vector<llvm::Type *> fieldTypes;

          if (!variant->SubMembers.empty()) {
            for (const auto &f : variant->SubMembers) {
              fieldTypes.push_back(resolveType(f.Type, false));
            }
            payloadLayoutType =
                llvm::StructType::get(m_Context, fieldTypes, true);
          } else if (!variant->Type.empty()) {
            payloadLayoutType = resolveType(variant->Type, false);
          }

          if (payloadLayoutType) {
            llvm::Value *variantAddr = m_Builder.CreateBitCast(
                payloadAddr, llvm::PointerType::getUnqual(payloadLayoutType));

            for (size_t i = 0; i < arm->Pat->SubPatterns.size(); ++i) {
              // Safety check
              if (fieldTypes.empty() && i > 0)
                break;
              if (!fieldTypes.empty() && i >= fieldTypes.size())
                break;

              llvm::Value *fieldAddr = variantAddr;
              llvm::Type *fieldTy = payloadLayoutType;

              if (!fieldTypes.empty()) {
                fieldAddr = m_Builder.CreateStructGEP(payloadLayoutType,
                                                      variantAddr, i);
                fieldTy = fieldTypes[i];
              }

              genPatternBinding(arm->Pat->SubPatterns[i].get(), fieldAddr,
                                fieldTy);
            }
          }
        }

        m_CFStack.push_back(
            {"", mergeBB, nullptr, resultAddr, m_ScopeStack.size()});
        genStmt(arm->Body.get());
        m_CFStack.pop_back();

        m_ScopeStack.pop_back();
        if (m_Builder.GetInsertBlock() &&
            !m_Builder.GetInsertBlock()->getTerminator())
          m_Builder.CreateBr(mergeBB);
      }
    }

    m_Builder.SetInsertPoint(defaultBB);
    // Find Wildcard arm if any
    for (const auto &arm : expr->Arms) {
      if (arm->Pat->PatternKind == MatchArm::Pattern::Wildcard) {
        m_ScopeStack.push_back({});
        m_CFStack.push_back(
            {"", mergeBB, nullptr, resultAddr, m_ScopeStack.size()});
        genStmt(arm->Body.get());
        m_CFStack.pop_back();
        m_ScopeStack.pop_back();
        break;
      }
    }
    if (m_Builder.GetInsertBlock() &&
        !m_Builder.GetInsertBlock()->getTerminator())
      m_Builder.CreateBr(mergeBB);
  } else {
    // General pattern matching (Sequence of Ifs)
    for (const auto &arm : expr->Arms) {
      llvm::BasicBlock *armBB =
          llvm::BasicBlock::Create(m_Context, "match_arm", func);
      llvm::BasicBlock *nextArmBB =
          llvm::BasicBlock::Create(m_Context, "match_next", func);

      // 1. Check Pattern
      llvm::Value *cond = nullptr;
      if (arm->Pat->PatternKind == MatchArm::Pattern::Literal) {
        cond = m_Builder.CreateICmpEQ(targetVal,
                                      m_Builder.getInt32(arm->Pat->LiteralVal));
      } else if (arm->Pat->PatternKind == MatchArm::Pattern::Wildcard ||
                 arm->Pat->PatternKind == MatchArm::Pattern::Variable) {
        cond = m_Builder.getInt1(true);
      } else {
        cond = m_Builder.getInt1(false);
      }

      // 2. Branch to guard-check, arm or next
      if (arm->Guard) {
        llvm::BasicBlock *guardBB =
            llvm::BasicBlock::Create(m_Context, "match_guard", func);
        m_Builder.CreateCondBr(cond, guardBB, nextArmBB);
        m_Builder.SetInsertPoint(guardBB);

        // For guard check, we might need variable bindings if it's a
        // variable pattern
        m_ScopeStack.push_back({});
        if (arm->Pat->PatternKind == MatchArm::Pattern::Variable) {
          genPatternBinding(arm->Pat.get(), targetAddr, targetType);
        }

        PhysEntity guardVal_ent = genExpr(arm->Guard.get()).load(m_Builder);
        llvm::Value *guardVal = guardVal_ent.load(m_Builder);

        m_Builder.CreateCondBr(guardVal, armBB, nextArmBB);
        m_ScopeStack.pop_back(); // Clean up guard scope
      } else {
        m_Builder.CreateCondBr(cond, armBB, nextArmBB);
      }

      // 3. ARM Body
      m_Builder.SetInsertPoint(armBB);
      m_ScopeStack.push_back({});
      if (arm->Pat->PatternKind == MatchArm::Pattern::Variable) {
        genPatternBinding(arm->Pat.get(), targetAddr, targetType);
      }

      m_CFStack.push_back(
          {"", mergeBB, nullptr, resultAddr, m_ScopeStack.size()});
      genStmt(arm->Body.get());
      m_CFStack.pop_back();

      m_ScopeStack.pop_back();
      if (m_Builder.GetInsertBlock() &&
          !m_Builder.GetInsertBlock()->getTerminator())
        m_Builder.CreateBr(mergeBB);

      m_Builder.SetInsertPoint(nextArmBB);
    }
    m_Builder.CreateBr(mergeBB);
  }

  // Check if mergeBB is reachable (i.e. has predecessors)
  // If use_empty() is true, it means no branches jump to it, so it's dead
  // code.
  if (mergeBB->use_empty()) {
    mergeBB->eraseFromParent(); // Remove dead block
    // Return dummy value since we can't be here at runtime
    // But we need to return something valid for the caller to not crash if
    // it uses the value type. However, if we removed the block, we have no
    // insert point? Actually if we return nullptr, genStmt might handle it?
    // genExpr must return a Value*. Use Undef.
    return llvm::UndefValue::get(resultType);
  }

  m_Builder.SetInsertPoint(mergeBB);
  return m_Builder.CreateLoad(resultType, resultAddr, "match_result");
}

PhysEntity CodeGen::genIfExpr(const IfExpr *ie) {
  // Track result via alloca if this if yields a value (determined by
  // PassExpr)
  llvm::AllocaInst *resultAddr =
      m_Builder.CreateAlloca(m_Builder.getInt32Ty(), nullptr, "if_result_addr");
  // Initialize with 0 or some default
  m_Builder.CreateStore(m_Builder.getInt32(0), resultAddr);

  PhysEntity cond_ent = genExpr(ie->Condition.get()).load(m_Builder);
  llvm::Value *cond = cond_ent.load(m_Builder);
  if (!cond)
    return nullptr;
  if (!cond->getType()->isIntegerTy(1)) {
    cond = m_Builder.CreateICmpNE(
        cond, llvm::ConstantInt::get(cond->getType(), 0), "ifcond");
  }

  llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(m_Context, "then", f);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(m_Context, "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(m_Context, "ifcont");

  m_Builder.CreateCondBr(cond, thenBB, elseBB);

  m_Builder.SetInsertPoint(thenBB);
  m_CFStack.push_back({"", mergeBB, nullptr, resultAddr, m_ScopeStack.size()});
  genStmt(ie->Then.get());
  m_CFStack.pop_back();
  llvm::BasicBlock *thenEndBB = m_Builder.GetInsertBlock();
  if (thenEndBB && !thenEndBB->getTerminator())
    m_Builder.CreateBr(mergeBB);

  elseBB->insertInto(f);
  m_Builder.SetInsertPoint(elseBB);
  if (ie->Else) {
    m_CFStack.push_back(
        {"", mergeBB, nullptr, resultAddr, m_ScopeStack.size()});
    genStmt(ie->Else.get());
    m_CFStack.pop_back();
  }
  llvm::BasicBlock *elseEndBB = m_Builder.GetInsertBlock();
  if (elseEndBB && !elseEndBB->getTerminator())
    m_Builder.CreateBr(mergeBB);

  mergeBB->insertInto(f);
  m_Builder.SetInsertPoint(mergeBB);
  return m_Builder.CreateLoad(m_Builder.getInt32Ty(), resultAddr, "if_result");
}

PhysEntity CodeGen::genWhileExpr(const WhileExpr *we) {
  llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *condBB =
      llvm::BasicBlock::Create(m_Context, "while_cond", f);
  llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(m_Context, "while_loop");
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(m_Context, "while_else");
  llvm::BasicBlock *afterBB =
      llvm::BasicBlock::Create(m_Context, "while_after");

  // Result via alloca
  llvm::AllocaInst *resultAddr = m_Builder.CreateAlloca(
      m_Builder.getInt32Ty(), nullptr, "while_result_addr");
  m_Builder.CreateStore(m_Builder.getInt32(0), resultAddr);

  m_Builder.CreateBr(condBB);
  m_Builder.SetInsertPoint(condBB);
  PhysEntity cond_ent = genExpr(we->Condition.get()).load(m_Builder);
  llvm::Value *cond = cond_ent.load(m_Builder);
  m_Builder.CreateCondBr(cond, loopBB, elseBB);

  loopBB->insertInto(f);
  m_Builder.SetInsertPoint(loopBB);
  std::string myLabel = "";
  if (!m_CFStack.empty() && m_CFStack.back().BreakTarget == nullptr)
    myLabel = m_CFStack.back().Label;

  m_CFStack.push_back(
      {myLabel, afterBB, condBB, resultAddr, m_ScopeStack.size()});
  genStmt(we->Body.get());
  m_CFStack.pop_back();
  if (m_Builder.GetInsertBlock() &&
      !m_Builder.GetInsertBlock()->getTerminator())
    m_Builder.CreateBr(condBB);

  elseBB->insertInto(f);
  m_Builder.SetInsertPoint(elseBB);
  if (we->ElseBody) {
    m_CFStack.push_back(
        {"", afterBB, nullptr, resultAddr, m_ScopeStack.size()});
    genStmt(we->ElseBody.get());
    m_CFStack.pop_back();
  }
  if (m_Builder.GetInsertBlock() &&
      !m_Builder.GetInsertBlock()->getTerminator())
    m_Builder.CreateBr(afterBB);

  afterBB->insertInto(f);
  m_Builder.SetInsertPoint(afterBB);
  return m_Builder.CreateLoad(m_Builder.getInt32Ty(), resultAddr,
                              "while_result");
}

PhysEntity CodeGen::genLoopExpr(const LoopExpr *le) {
  llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(m_Context, "loop", f);
  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(m_Context, "loop_after");

  // Result via alloca
  llvm::AllocaInst *resultAddr = m_Builder.CreateAlloca(
      m_Builder.getInt32Ty(), nullptr, "loop_result_addr");
  m_Builder.CreateStore(m_Builder.getInt32(0), resultAddr);

  m_Builder.CreateBr(loopBB);
  m_Builder.SetInsertPoint(loopBB);
  std::string myLabel = "";
  if (!m_CFStack.empty() && m_CFStack.back().BreakTarget == nullptr)
    myLabel = m_CFStack.back().Label;

  m_CFStack.push_back(
      {myLabel, afterBB, loopBB, resultAddr, m_ScopeStack.size()});
  genStmt(le->Body.get());
  m_CFStack.pop_back();
  if (m_Builder.GetInsertBlock() &&
      !m_Builder.GetInsertBlock()->getTerminator())
    m_Builder.CreateBr(loopBB);

  afterBB->insertInto(f);
  m_Builder.SetInsertPoint(afterBB);
  return m_Builder.CreateLoad(m_Builder.getInt32Ty(), resultAddr,
                              "loop_result");
}

PhysEntity CodeGen::genForExpr(const ForExpr *fe) {
  PhysEntity collVal_ent = genExpr(fe->Collection.get()).load(m_Builder);
  llvm::Value *collVal = collVal_ent.load(m_Builder);
  if (!collVal)
    return nullptr;

  llvm::Function *f = m_Builder.GetInsertBlock()->getParent();

  // Only support Array/Slice iteration for now
  if (!collVal->getType()->isPointerTy() && !collVal->getType()->isArrayTy()) {
    error(fe, "Only arrays and pointers can be iterated in for loops");
    return nullptr;
  }

  llvm::BasicBlock *condBB = llvm::BasicBlock::Create(m_Context, "for_cond", f);
  llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(m_Context, "for_loop");
  llvm::BasicBlock *incrBB = llvm::BasicBlock::Create(m_Context, "for_incr");
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(m_Context, "for_else");
  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(m_Context, "for_after");

  // Result via alloca
  llvm::AllocaInst *resultAddr = m_Builder.CreateAlloca(
      m_Builder.getInt32Ty(), nullptr, "for_result_addr");
  m_Builder.CreateStore(m_Builder.getInt32(0), resultAddr);

  // Loop index
  llvm::AllocaInst *idxAlloca = m_Builder.CreateAlloca(
      llvm::Type::getInt32Ty(m_Context), nullptr, "for_idx");
  m_Builder.CreateStore(
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0), idxAlloca);

  m_Builder.CreateBr(condBB);
  m_Builder.SetInsertPoint(condBB);

  llvm::Value *currIdx = m_Builder.CreateLoad(llvm::Type::getInt32Ty(m_Context),
                                              idxAlloca, "curr_idx");
  llvm::Value *limit = nullptr;

  // 1. Array Size Detection via Semantic Type
  std::string typeStr = collVal_ent.typeName;
  bool foundSize = false;
  if (typeStr.size() > 1 && typeStr[0] == '[') {
    size_t lastSemi = typeStr.find_last_of(';');
    if (lastSemi != std::string::npos) {
      std::string countStr =
          typeStr.substr(lastSemi + 1, typeStr.size() - lastSemi - 2);
      try {
        uint64_t n = std::stoull(countStr);
        limit = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), n);
        foundSize = true;
      } catch (...) {
      }
    }
  }

  if (!foundSize) {
    if (collVal->getType()->isArrayTy()) {
      limit = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context),
                                     collVal->getType()->getArrayNumElements());
    } else {
      // Fallback to hardcoded 10 for pointers if unknown
      limit = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 10);
    }
  }

  llvm::Value *cond = m_Builder.CreateICmpULT(currIdx, limit, "forcond");
  m_Builder.CreateCondBr(cond, loopBB, elseBB);

  loopBB->insertInto(f);
  m_Builder.SetInsertPoint(loopBB);

  // Define loop variable scope
  m_ScopeStack.push_back({});

  // 2. Determine Element Type
  llvm::Type *elemTy = nullptr;
  if (collVal->getType()->isArrayTy()) {
    elemTy = collVal->getType()->getArrayElementType();
  } else if (collVal->getType()->isPointerTy()) {
    // Use semantic type to resolve element type because of opaque pointers
    std::string collTypeName = collVal_ent.typeName;
    if (collTypeName.size() > 1) {
      if (collTypeName[0] == '[') {
        size_t lastSemi = collTypeName.find_last_of(';');
        if (lastSemi != std::string::npos) {
          std::string inner = collTypeName.substr(1, lastSemi - 1);
          elemTy = resolveType(inner, false);
        }
      } else if (collTypeName[0] == '*' || collTypeName[0] == '^' ||
                 collTypeName[0] == '&' || collTypeName[0] == '~') {
        size_t offset = 1;
        if (collTypeName.length() > 1 &&
            (collTypeName[1] == '?' || collTypeName[1] == '#' ||
             collTypeName[1] == '!'))
          offset++;
        elemTy = resolveType(collTypeName.substr(offset), false);
      }
    }
    if (!elemTy)
      elemTy = llvm::Type::getInt32Ty(m_Context);
  } else {
    elemTy = llvm::Type::getInt32Ty(m_Context);
  }

  // 3. Obtain Element Pointer
  llvm::Value *elemPtr = nullptr;
  if (collVal->getType()->isPointerTy()) {
    std::string collTypeName = collVal_ent.typeName;
    if (collTypeName.size() > 0 && collTypeName[0] == '[') {
      // Pointer to array literal or alloca'd array
      llvm::Type *arrTy = resolveType(collTypeName, false);
      elemPtr = m_Builder.CreateGEP(
          arrTy, collVal,
          {llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0),
           currIdx});
    } else {
      // Raw pointer iteration
      elemPtr = m_Builder.CreateGEP(elemTy, collVal, {currIdx});
    }
  } else {
    // Array R-Value (LLVM Array)
    llvm::Value *allocaColl =
        m_Builder.CreateAlloca(collVal->getType(), nullptr, "for_arr_tmp");
    m_Builder.CreateStore(collVal, allocaColl);
    elemPtr = m_Builder.CreateGEP(
        collVal->getType(), allocaColl,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0),
         currIdx});
  }

  std::string vName = fe->VarName;
  while (!vName.empty() &&
         (vName[0] == '*' || vName[0] == '#' || vName[0] == '&' ||
          vName[0] == '^' || vName[0] == '~' || vName[0] == '!'))
    vName = vName.substr(1);
  while (!vName.empty() &&
         (vName.back() == '#' || vName.back() == '?' || vName.back() == '!'))
    vName.pop_back();

  // 4. Load and Store into Loop Variable
  llvm::Value *elem = m_Builder.CreateLoad(elemTy, elemPtr, vName);
  llvm::AllocaInst *vAlloca =
      m_Builder.CreateAlloca(elem->getType(), nullptr, vName);
  m_Builder.CreateStore(elem, vAlloca);

  // Register in legacy and new symbol tables
  m_NamedValues[vName] = vAlloca;
  // m_ValueTypes[vName] = elem->getType(); // LEGACY REMOVED
  // m_ValueElementTypes[vName] = elemTy; // LEGACY REMOVED

  TokaSymbol sym;
  sym.allocaPtr = vAlloca;
  fillSymbolMetadata(sym, "", false, false, false, false, false, false,
                     elem->getType());
  sym.soulType = elem->getType();
  m_Symbols[vName] = sym;

  std::string myLabel = "";
  if (!m_CFStack.empty() && m_CFStack.back().BreakTarget == nullptr)
    myLabel = m_CFStack.back().Label;

  m_CFStack.push_back(
      {myLabel, afterBB, incrBB, resultAddr, m_ScopeStack.size()});
  genStmt(fe->Body.get());
  m_CFStack.pop_back();
  cleanupScopes(m_ScopeStack.size() - 1);
  m_ScopeStack.pop_back();

  if (m_Builder.GetInsertBlock() &&
      !m_Builder.GetInsertBlock()->getTerminator())
    m_Builder.CreateBr(incrBB);

  incrBB->insertInto(f);
  m_Builder.SetInsertPoint(incrBB);
  llvm::Value *nextIdx = m_Builder.CreateAdd(
      currIdx, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
  m_Builder.CreateStore(nextIdx, idxAlloca);
  m_Builder.CreateBr(condBB);

  elseBB->insertInto(f);
  m_Builder.SetInsertPoint(elseBB);
  if (fe->ElseBody) {
    m_CFStack.push_back(
        {"", afterBB, nullptr, resultAddr, m_ScopeStack.size()});
    genStmt(fe->ElseBody.get());
    m_CFStack.pop_back();
  }
  if (m_Builder.GetInsertBlock() &&
      !m_Builder.GetInsertBlock()->getTerminator())
    m_Builder.CreateBr(afterBB);
  afterBB->insertInto(f);
  m_Builder.SetInsertPoint(afterBB);
  return m_Builder.CreateLoad(m_Builder.getInt32Ty(), resultAddr, "for_result");
}

void CodeGen::genPatternBinding(const MatchArm::Pattern *pat,
                                llvm::Value *targetAddr,
                                llvm::Type *targetType) {
  if (pat->PatternKind == MatchArm::Pattern::Variable) {
    llvm::Value *val = targetAddr;
    std::string pName = pat->Name;
    bool isUnique = false;
    bool isShared = false;
    bool isRaw = false;

    // Detect Morphology from Name
    std::string checkName = pName;
    while (!checkName.empty()) {
      char c = checkName[0];
      if (c == '^')
        isUnique = true;
      else if (c == '~')
        isShared = true;
      else if (c == '*')
        isRaw = true;
      else if (c == '#' || c == '&' || c == '!') { /* skip */
      } else
        break;
      checkName = checkName.substr(1);
    }

    while (!pName.empty() &&
           (pName[0] == '*' || pName[0] == '#' || pName[0] == '&' ||
            pName[0] == '^' || pName[0] == '~' || pName[0] == '!'))
      pName = pName.substr(1);
    while (!pName.empty() &&
           (pName.back() == '#' || pName.back() == '?' || pName.back() == '!'))
      pName.pop_back();

    if (!pat->IsReference) {
      val = m_Builder.CreateLoad(targetType, targetAddr, pName);
    }
    // Create local alloca
    llvm::Type *allocaType = val->getType();
    llvm::AllocaInst *alloca =
        m_Builder.CreateAlloca(allocaType, nullptr, pName);
    m_Builder.CreateStore(val, alloca);

    m_NamedValues[pName] = alloca;

    TokaSymbol sym;
    sym.allocaPtr = alloca;
    // For pattern bindings, metadata is often already inferred by Sema
    fillSymbolMetadata(sym, "", false, false, false, pat->IsReference,
                       pat->IsValueMutable, false, targetType);
    sym.isRebindable = false;
    sym.isContinuous = targetType->isArrayTy();

    // [Fix] Set Morphology and Indirection explicitly for Pattern Bindings
    if (isUnique) {
      sym.morphology = Morphology::Unique;
      sym.indirectionLevel = 1;
    } else if (isShared) {
      sym.morphology = Morphology::Shared;
      // Shared is a struct value, so indirection is 0 (alloca holds the struct)
      // Unless targetType was already a pointer to shared?
      // genMatchExpr loads target. If variable was ~T, targetVal is {T*, Ref}.
      // So alloca stores {T*, Ref}. Indirection 0.
      sym.indirectionLevel = 0;
    } else if (isRaw || targetType->isPointerTy()) {
      sym.morphology = Morphology::Raw;
      sym.indirectionLevel = 1;
    }

    m_Symbols[pName] = sym;

    if (!m_ScopeStack.empty()) {
      m_ScopeStack.back().push_back(
          {pName, alloca, targetType, isUnique, isShared});
    }
  } else if (pat->PatternKind == MatchArm::Pattern::Decons) {
    if (targetType->isStructTy()) {
      for (size_t i = 0; i < pat->SubPatterns.size(); ++i) {
        llvm::Value *fieldAddr =
            m_Builder.CreateStructGEP(targetType, targetAddr, i);
        genPatternBinding(pat->SubPatterns[i].get(), fieldAddr,
                          targetType->getStructElementType(i));
      }
    } else if (!pat->SubPatterns.empty()) {
      // Single payload case (not wrapped in tuple/struct)
      genPatternBinding(pat->SubPatterns[0].get(), targetAddr, targetType);
    }
  }
}

PhysEntity CodeGen::genCallExpr(const CallExpr *call) {
  // Primitives as constructors: i32(42)
  if (call->Callee == "i32" || call->Callee == "u32" || call->Callee == "i64" ||
      call->Callee == "u64" || call->Callee == "f32" || call->Callee == "f64" ||
      call->Callee == "i16" || call->Callee == "u16" || call->Callee == "i8" ||
      call->Callee == "u8" || call->Callee == "usize" ||
      call->Callee == "isize" || call->Callee == "bool") {
    llvm::Type *targetTy = resolveType(call->Callee, false);
    if (call->Args.empty())
      return llvm::Constant::getNullValue(targetTy);
    PhysEntity val_ent = genExpr(call->Args[0].get()).load(m_Builder);
    llvm::Value *val = val_ent.load(m_Builder);
    if (!val)
      return nullptr;
    if (val->getType() != targetTy) {
      if (val->getType()->isIntegerTy() && targetTy->isIntegerTy()) {
        return m_Builder.CreateIntCast(val, targetTy, true);
      } else if (val->getType()->isPointerTy() && targetTy->isIntegerTy()) {
        return m_Builder.CreatePtrToInt(val, targetTy);
      } else if (val->getType()->isIntegerTy() && targetTy->isPointerTy()) {
        return m_Builder.CreateIntToPtr(val, targetTy);
      } else if (val->getType()->isFloatingPointTy() &&
                 targetTy->isFloatingPointTy()) {
        return m_Builder.CreateFPCast(val, targetTy);
      }
    }
    return val;
  }

  // Check if it is a Shape/Struct Constructor
  const ShapeDecl *sh = nullptr;
  if (call->ResolvedShape) {
    sh = call->ResolvedShape;
  } else if (m_Shapes.count(call->Callee)) {
    sh = m_Shapes[call->Callee];
  }

  if (sh) {
    // [Fix] On-Demand Generation (Top-Level)
    if (!m_StructTypes.count(sh->Name)) {
      genShape(sh);
    }

    if (sh->Kind == ShapeKind::Struct || sh->Kind == ShapeKind::Tuple ||
        sh->Kind == ShapeKind::Union) {
      if (!m_StructTypes.count(sh->Name)) {
        // Should have been generated by now
        return nullptr; // Avoids crashing on CreateAlloca(nullptr)
      }
      llvm::StructType *st = m_StructTypes[sh->Name];
      auto *alloca = m_Builder.CreateAlloca(st, nullptr, sh->Name + "_ctor");

      // [Fix] Union Alignment
      if (sh->Kind == ShapeKind::Union) {
        alloca->setAlignment(llvm::Align(sh->MaxAlign));
      }

      size_t argIdx = 0;
      for (const auto &arg : call->Args) {
        std::string fieldName;
        const Expr *valExpr = arg.get();
        bool isNamed = false;

        // Check for named arg (x = val)
        if (auto *bin = dynamic_cast<const BinaryExpr *>(arg.get())) {
          // Attempt auto-deref in CodeGen if Sema authorized it?
          // If LHS is ptr and RHS is int (identity/value mismatch)

          if (bin->Op == "=") {
            if (auto *var =
                    dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
              fieldName = var->Name;
              valExpr = bin->RHS.get();
              isNamed = true;
            } else if (auto *un =
                           dynamic_cast<const UnaryExpr *>(bin->LHS.get())) {
              if (auto *v = dynamic_cast<const VariableExpr *>(un->RHS.get())) {
                fieldName = v->Name;
                valExpr = bin->RHS.get();
                isNamed = true;
              }
            }
          }
        }

        int memberIdx = -1;
        if (sh->Kind == ShapeKind::Union) {
          memberIdx = call->MatchedMemberIdx;
        } else if (isNamed) {
          for (size_t i = 0; i < sh->Members.size(); ++i) {
            if (sh->Members[i].Name == fieldName) {
              memberIdx = (int)i;
              break;
            }
          }
        } else {
          memberIdx = (int)argIdx;
        }

        if (memberIdx >= 0 && memberIdx < (int)sh->Members.size()) {
          PhysEntity val_ent = genExpr(valExpr).load(m_Builder);
          llvm::Value *val = val_ent.load(m_Builder);
          if (!val)
            return nullptr;

          // Auto-cast if needed (e.g. integer promotion) - minimal support
          llvm::Type *destTy = nullptr;
          if (sh->Kind == ShapeKind::Union) {
            const ShapeMember &M = sh->Members[memberIdx];
            if (M.ResolvedType) {
              destTy = getLLVMType(M.ResolvedType);
            } else {
              destTy = resolveType(M.Type, false);
            }
          } else {
            destTy = st->getElementType(memberIdx);
          }
          if (val->getType() != destTy) {
            if (val->getType()->isIntegerTy() && destTy->isIntegerTy()) {
              val = m_Builder.CreateIntCast(val, destTy, true);
            }
          }

          llvm::Value *ptr = nullptr;
          if (sh->Kind == ShapeKind::Union) {
            // [CRITICAL] Union physically has only one element: the storage
            // array. We bitcast the base address to the actual member type we
            // matched.
            ptr = m_Builder.CreateBitCast(alloca,
                                          llvm::PointerType::getUnqual(destTy));
          } else {
            ptr = m_Builder.CreateStructGEP(st, alloca, memberIdx);
          }
          m_Builder.CreateStore(val, ptr);
        }
        argIdx++;
      }
      return m_Builder.CreateLoad(st, alloca);
    }
  }

  // Intrinsic: println (Compiler Magic)
  if (call->Callee == "println" ||
      (call->Callee.size() > 9 &&
       call->Callee.substr(call->Callee.size() - 9) == "::println")) {
    if (call->Args.empty())
      return nullptr;

    auto *fmtExpr = dynamic_cast<const StringExpr *>(call->Args[0].get());
    if (!fmtExpr) {
      error(call, "println intrinsic requires a string literal as "
                  "first argument.");
      return nullptr;
    }

    std::string fmt = fmtExpr->Value;
    size_t lastPos = 0;
    size_t pos = 0;
    int argIndex = 1;

    auto printfFunc = m_Module->getOrInsertFunction(
        "printf", llvm::FunctionType::get(m_Builder.getInt32Ty(),
                                          {m_Builder.getInt8PtrTy()}, true));

    while ((pos = fmt.find("{}", lastPos)) != std::string::npos) {
      // Print text segment
      std::string text = fmt.substr(lastPos, pos - lastPos);
      if (!text.empty()) {
        std::vector<llvm::Value *> safeArgs;
        safeArgs.push_back(m_Builder.CreateGlobalStringPtr("%s"));
        safeArgs.push_back(m_Builder.CreateGlobalStringPtr(text));
        m_Builder.CreateCall(printfFunc, safeArgs);
      }

      // Print argument
      if (argIndex < (int)call->Args.size()) {
        PhysEntity val_ent = genExpr(call->Args[argIndex].get());
        // Note: genExpr returns PhysEntity. DO NOT LOAD YET if we want Type
        // info.

        llvm::Value *val = val_ent.load(m_Builder);
        if (val) {
          llvm::Type *ty = val->getType();
          std::string spec = "";
          llvm::Value *pVal = val;

          // Use Entity Type Name if available
          std::string semanticType = val_ent.typeName;

          if (ty->isIntegerTy(1)) { // bool
            llvm::Value *trueStr = m_Builder.CreateGlobalStringPtr("true");
            llvm::Value *falseStr = m_Builder.CreateGlobalStringPtr("false");
            pVal = m_Builder.CreateSelect(val, trueStr, falseStr);
            spec = "%s";
          } else if (ty->isIntegerTy(8) || semanticType == "char" ||
                     semanticType == "u8" || semanticType == "i8") {
            spec = semanticType == "char" ? "%c" : "%d";
            if (semanticType == "u8") {
              pVal = m_Builder.CreateZExtOrBitCast(
                  val, llvm::Type::getInt32Ty(m_Context));
            } else {
              pVal = m_Builder.CreateSExtOrBitCast(
                  val, llvm::Type::getInt32Ty(m_Context));
            }
          } else if (ty->isIntegerTy(64)) {
            if (semanticType == "OAddr") {
              spec = "0x%016lX";
            } else {
              spec = (semanticType.size() > 0 && semanticType[0] == 'u')
                         ? "%llu"
                         : "%lld";
            }
          } else if (ty->isIntegerTy()) {
            spec = (semanticType.size() > 0 && semanticType[0] == 'u') ? "%u"
                                                                       : "%d";
            if (ty->getIntegerBitWidth() < 32) {
              if (semanticType.size() > 0 && semanticType[0] == 'u')
                pVal = m_Builder.CreateZExt(val, m_Builder.getInt32Ty());
              else
                pVal = m_Builder.CreateSExt(val, m_Builder.getInt32Ty());
            }
          } else if (ty->isDoubleTy()) {
            spec = "%f";
          } else if (ty->isFloatTy()) {
            spec = "%f";
            pVal = m_Builder.CreateFPExt(val, m_Builder.getDoubleTy());
          } else if (semanticType == "*char" || semanticType == "str" ||
                     semanticType == "String") {
            // Explicit check for String type (including String struct if we
            // support it)
            spec = "%s";
            // If it's *char (Pointer to i8), printf %s works.
            // If it's String struct, we might need to extract data pointer?
            // But currently String is likely passed as *char from c_str()
            // or similar. If it is String struct, pVal is struct value.
            // printf cannot take struct. But val_ent.load() loads the
            // value.
          } else if (ty->isPointerTy()) {
            // Fallback for pointers
            spec = "%p";
          } else if (ty->isStructTy()) {
            // Attempt to unwrap struct (e.g. Enum { tag }, or StrongType {
            // val
            // })
            llvm::Value *unwrapped = pVal;
            llvm::Type *innerTy = ty;
            while (innerTy->isStructTy() &&
                   innerTy->getStructNumElements() > 0) {
              unwrapped = m_Builder.CreateExtractValue(unwrapped, 0);
              innerTy = unwrapped->getType();
            }

            // Re-evaluate type
            if (innerTy->isIntegerTy()) {
              if (innerTy->getIntegerBitWidth() > 32) {
                spec = "%lld";
                pVal = unwrapped;
              } else {
                spec = "%d";
                pVal = m_Builder.CreateZExtOrBitCast(
                    unwrapped, llvm::Type::getInt32Ty(m_Context));
              }
            } else if (innerTy->isFloatTy() || innerTy->isDoubleTy()) {
              spec = "%f";
              pVal = unwrapped;
              if (innerTy->isFloatTy())
                pVal = m_Builder.CreateFPExt(pVal, m_Builder.getDoubleTy());
            } else {
              spec = "?Struct?";
              // Don't pass struct to printf, it crashes lli
              pVal = llvm::ConstantInt::get(m_Builder.getInt32Ty(), 0);
            }
          } else {
            spec = "?"; // Unknown
          }

          if (!spec.empty()) {
            std::vector<llvm::Value *> pArgs;
            pArgs.push_back(m_Builder.CreateGlobalStringPtr(spec));
            pArgs.push_back(pVal);
            m_Builder.CreateCall(printfFunc, pArgs);
          }
        }
        argIndex++;
      }
      lastPos = pos + 2; // Skip {}
    }

    // Print remaining tail
    std::string tail = fmt.substr(lastPos);
    tail += "\n"; // Auto newline
    std::vector<llvm::Value *> tailArgs;
    tailArgs.push_back(m_Builder.CreateGlobalStringPtr("%s"));
    tailArgs.push_back(m_Builder.CreateGlobalStringPtr(tail));
    m_Builder.CreateCall(printfFunc, tailArgs);

    return llvm::ConstantInt::get(m_Builder.getInt32Ty(), 0);
  }

  std::string calleeName = call->Callee;
  if (call->ResolvedFn) {
    calleeName = call->ResolvedFn->Name;
  } else if (call->ResolvedExtern) {
    calleeName = call->ResolvedExtern->Name;
    genExtern(call->ResolvedExtern);
  }

  if (calleeName.size() > 5 && calleeName.substr(0, 5) == "libc_") {
    calleeName = calleeName.substr(5);
  }

  llvm::Function *callee = m_Module->getFunction(calleeName);
  if (!callee && call->ResolvedFn) {
    genFunction(call->ResolvedFn, "", true);
    callee = m_Module->getFunction(call->ResolvedFn->Name);
  }
  if (!callee && call->ResolvedExtern) {
    genExtern(call->ResolvedExtern);
    callee = m_Module->getFunction(call->ResolvedExtern->Name);
  }
  if (!callee) {
    // Check for ADT Constructor (Type::Member)
    std::string callName = call->Callee;
    size_t delim = callName.find("::");
    if (delim != std::string::npos) {
      std::string optName = callName.substr(0, delim);
      std::string varName = callName.substr(delim + 2);

      // Try static call Type::Member -> Type_Member
      std::string mangledName = optName + "_" + varName;
      if (auto *f = m_Module->getFunction(mangledName)) {
        std::vector<llvm::Value *> args;
        for (size_t i = 0; i < call->Args.size(); ++i) {
          PhysEntity argVal_ent = genExpr(call->Args[i].get()).load(m_Builder);
          llvm::Value *argVal = argVal_ent.load(m_Builder);
          if (i < f->getFunctionType()->getNumParams()) {
            llvm::Type *paramTy = f->getFunctionType()->getParamType(i);
            if (argVal && paramTy->isPointerTy() &&
                argVal->getType()->isStructTy()) {
              // Implicit By-Ref for Structs: Pass Address of Temp
              llvm::AllocaInst *tmp = m_Builder.CreateAlloca(
                  argVal->getType(), nullptr, "arg_tmp_byref");
              m_Builder.CreateStore(argVal, tmp);
              argVal = tmp;
            }
          }
          args.push_back(argVal);
        }
        if (!args.empty() && !args.back())
          return nullptr;

        return m_Builder.CreateCall(
            f, args, f->getReturnType()->isVoidTy() ? "" : "calltmp");
      }

      if (m_Shapes.count(optName) &&
          m_Shapes[optName]->Kind == ShapeKind::Enum) {
        const ShapeDecl *sh = m_Shapes[optName];
        int tag = -1;
        const ShapeMember *targetVar = nullptr;
        for (int i = 0; i < (int)sh->Members.size(); ++i) {
          if (sh->Members[i].Name == varName) {
            tag = (sh->Members[i].TagValue == -1)
                      ? i
                      : (int)sh->Members[i].TagValue;
            targetVar = &sh->Members[i];
            break;
          }
        }

        if (tag != -1) {
          std::vector<llvm::Value *> args;
          for (auto &argExpr : call->Args) {
            args.push_back(genExpr(argExpr.get()).load(m_Builder));
          }
          if (!args.empty() && !args.back())
            return nullptr;

          llvm::StructType *st = m_StructTypes[optName];
          llvm::Value *alloca = m_Builder.CreateAlloca(st, nullptr, "opt_ctor");
          llvm::Value *tagAddr =
              m_Builder.CreateStructGEP(st, alloca, 0, "tag_addr");
          m_Builder.CreateStore(
              llvm::ConstantInt::get(llvm::Type::getInt8Ty(m_Context), tag),
              tagAddr);

          if (targetVar) {
            llvm::Type *payloadType = nullptr;
            std::vector<llvm::Type *> fieldTypes;

            if (!targetVar->SubMembers.empty()) {
              for (auto &f : targetVar->SubMembers) {
                fieldTypes.push_back(resolveType(f.Type, false));
              }
              payloadType = llvm::StructType::get(m_Context, fieldTypes, true);
            } else if (!targetVar->Type.empty()) {
              payloadType = resolveType(targetVar->Type, false);
            }

            if (payloadType) {
              llvm::Value *payloadAddr =
                  m_Builder.CreateStructGEP(st, alloca, 1, "payload_addr");
              llvm::Value *castPtr = m_Builder.CreateBitCast(
                  payloadAddr, llvm::PointerType::getUnqual(payloadType));

              if (!targetVar->SubMembers.empty()) {
                // Multi-field Tuple Payload
                for (size_t i = 0; i < args.size() && i < fieldTypes.size();
                     ++i) {
                  llvm::Value *fPtr =
                      m_Builder.CreateStructGEP(payloadType, castPtr, i);
                  m_Builder.CreateStore(args[i], fPtr);
                }
              } else {
                // Single Payload
                if (args.size() == 1) {
                  m_Builder.CreateStore(args[0], castPtr);
                }
              }
            }
          }
          return m_Builder.CreateLoad(st, alloca);
        }
      }
    }
  }

  if (!callee) {
    error(call, "Call to undefined function or shape '" + call->Callee + "'");
    return nullptr;
  }

  const FunctionDecl *funcDecl = nullptr;
  if (m_Functions.count(call->Callee))
    funcDecl = m_Functions[call->Callee];

  const ExternDecl *extDecl = nullptr;
  if (m_Externs.count(call->Callee))
    extDecl = m_Externs[call->Callee];

  std::vector<llvm::Value *> argsV;
  for (size_t i = 0; i < call->Args.size(); ++i) {
    bool isRef = false;
    if (funcDecl && i < funcDecl->Args.size()) {
      isRef = funcDecl->Args[i].IsReference;
    } else if (extDecl && i < extDecl->Args.size()) {
      isRef = extDecl->Args[i].IsReference;
    }

    llvm::Value *val = nullptr;
    bool shouldPassAddr = isRef;
    llvm::Type *pTy = nullptr;
    if (callee && i < callee->getFunctionType()->getNumParams())
      pTy = callee->getFunctionType()->getParamType(i);

    bool isCaptured = false;

    if (funcDecl && i < funcDecl->Args.size()) {
      const auto &arg = funcDecl->Args[i];
      // Force Capture for Unique Pointers to enable In-Place Move / Borrow
      // [ABI Fix] Force Capture for Shared Pointers to pass by Pointer
      // (avoiding ABI split)
      // [NEW] Lifetime Union: Force capture if param is a dependency
      for (const auto &dep : funcDecl->LifeDependencies) {
        if (dep == arg.Name) {
          isCaptured = true;
          break;
        }
      }

      // Only capture if it's a Value Type (Struct/Array/Mutable) AND NOT a
      // Pointer/Shared/Reference
      if (!isCaptured && !arg.HasPointer && !arg.IsReference) {
        if (arg.IsValueMutable) {
          isCaptured = true;
        } else {
          llvm::Type *logicalTy = resolveType(arg.Type, false);
          if (logicalTy && (logicalTy->isStructTy() || logicalTy->isArrayTy()))
            isCaptured = true;
        }
      }

      // [Fix] Unique/Shared/Rebindable Pointers MUST be passed by Reference
      // (Capture) This matches genFunction ABI where they are treated as
      // Captured Arguments. This allows the Callee to manipulate the Handle
      // (e.g. invalidating it on Move or rebinding it).
      if (arg.IsUnique || arg.IsShared || arg.IsRebindable) {
        isCaptured = true;
      }
    } else if (extDecl && i < extDecl->Args.size()) {
      const auto &arg = extDecl->Args[i];
      if (!arg.HasPointer) {
        llvm::Type *logicalTy = resolveType(arg.Type, false);
        if (logicalTy && (logicalTy->isStructTy() || logicalTy->isArrayTy()))
          isCaptured = true;
      }
    }

    // [Debug] Arg detection
    if (funcDecl && i < funcDecl->Args.size()) {
      const auto &arg = funcDecl->Args[i];
      std::cerr << "DEBUG: CallArg " << i << " Name=" << arg.Name
                << " HasRef=" << arg.IsReference << " HasPtr=" << arg.HasPointer
                << " IsUni=" << arg.IsUnique << " Cap=" << isCaptured
                << " ShouldAddr=" << shouldPassAddr << "\n";
    }

    if (isCaptured || isRef) {
      shouldPassAddr = true;
    }

    if (shouldPassAddr) {
      if (dynamic_cast<const AddressOfExpr *>(call->Args[i].get())) {
        val = genExpr(call->Args[i].get()).load(m_Builder);
      } else {
        // [Fix] Explicit Identity Capture
        // If we are capturing (Pass-By-Reference), we want the Identity
        // Address (Alloca), not the Soul Address (Heap Ptr). genAddr often
        // peels to Soul. We manually unwrap and seek Identity.
        const Expr *rawArg = call->Args[i].get();
        // Unwrap decorators to find the variable
        while (true) {
          if (auto *ue = dynamic_cast<const UnaryExpr *>(rawArg))
            rawArg = ue->RHS.get();
          else if (auto *pe = dynamic_cast<const PostfixExpr *>(rawArg))
            rawArg = pe->LHS.get();
          else
            break;
        }

        if (auto *ve = dynamic_cast<const VariableExpr *>(rawArg)) {
          if (ve->HasConstantValue) {
            // [Fix] Constants are RValues. Fall through to Temp
            // Materialization (genExpr)
            val = nullptr;
          } else {
            val = getIdentityAddr(ve->Name);
          }
        }

        if (!val) {
          val = genAddr(call->Args[i].get());
        }

        // [Fix] Handle RValues for Captured Arguments (Temp
        // Materialization)
        if (!val) {
          PhysEntity pe = genExpr(call->Args[i].get());
          // If genAddr failed, it's likely an RValue (return from call,
          // etc.) We must create a temporary alloca to pass its address.
          llvm::Value *rVal = pe.load(m_Builder);
          if (rVal) {
            llvm::AllocaInst *tmp =
                m_Builder.CreateAlloca(rVal->getType(), nullptr, "arg_tmp");
            m_Builder.CreateStore(rVal, tmp);
            val = tmp;
          }
        }
      }
    } else {
      val = genExpr(call->Args[i].get()).load(m_Builder);
    }

    if (!val) {
      error(call, "Failed to generate argument " + std::to_string(i) + " for " +
                      call->Callee);
      return nullptr;
    }

    // [ABI Fix] Shared Pointer Argument Copy (Incref) REMOVED
    // We now pass Shared Pointers by "Single Pointer" (Address of Handle).
    // This implies Borrowing semantics (no transfer of ownership, no new
    // reference). The Callee will see the Caller's handle via pointer.
    // Explicit Incref/Decref is NOT needed for this ABI strategy unless we
    // implement explicit cloning.
    if (funcDecl && i < funcDecl->Args.size() && funcDecl->Args[i].IsShared) {
      // No-op for Pass-By-Pointer
    }

    // Fallback: If we generated a Value (e.g. Struct) but Function expects
    // Pointer (Implicit ByRef), wrap it now.
    if (val && i < callee->getFunctionType()->getNumParams()) {
      llvm::Type *paramTy = callee->getFunctionType()->getParamType(i);
      if (paramTy->isPointerTy() && val->getType()->isStructTy()) {
        llvm::AllocaInst *tmp = m_Builder.CreateAlloca(val->getType(), nullptr,
                                                       "arg_fallback_byref");
        m_Builder.CreateStore(val, tmp);
        val = tmp;
      }
    }

    if (i < callee->getFunctionType()->getNumParams()) {
      llvm::Type *paramType = callee->getFunctionType()->getParamType(i);

      // Unsizing Coercion (Concrete -> dyn @Trait)
      std::string targetArgType =
          (funcDecl ? funcDecl->Args[i].Type
                    : (extDecl ? extDecl->Args[i].Type : ""));
      if (targetArgType.size() >= 4 && targetArgType.substr(0, 3) == "dyn") {
        // 1. Identify Trait Name
        std::string traitName = "";
        if (targetArgType.find("dyn @") == 0)
          traitName = targetArgType.substr(5);
        else if (targetArgType.find("dyn@") == 0)
          traitName = targetArgType.substr(4);

        // 2. Identify Concrete Type Name
        std::string concreteName = "";
        const Expr *argExpr = call->Args[i].get();

        // [New] Annotated AST: Use ResolvedType
        if (argExpr->ResolvedType) {
          auto rt = argExpr->ResolvedType;
          // Strip pointer/reference layers to get the core Shape/Value
          // implementation (Traits are usually implemented on value types)
          while (rt && (rt->isPointer() || rt->isReference() ||
                        rt->isSmartPointer())) {
            if (auto inner = rt->getPointeeType())
              rt = inner;
            else
              break;
          }
          if (rt) {
            concreteName = rt->toString();
            // Strip suffixes (#, ?, !) from the resulting name to match
            // VTable expectation
            while (!concreteName.empty() &&
                   (concreteName.back() == '#' || concreteName.back() == '?' ||
                    concreteName.back() == '!')) {
              concreteName.pop_back();
            }
          }
        }

        // Legacy Fallback / Refinement
        if (concreteName.empty() || concreteName == "void") {
          if (auto *ve = dynamic_cast<const VariableExpr *>(argExpr)) {
            if (m_Symbols.count(ve->Name)) {
              llvm::Type *ct = m_Symbols[ve->Name].soulType;
              if (m_TypeToName.count(ct))
                concreteName = m_TypeToName[ct];
            }
          } else if (auto *ne = dynamic_cast<const NewExpr *>(argExpr)) {
            concreteName = ne->Type;
          } else if (auto *ie = dynamic_cast<const InitStructExpr *>(argExpr)) {
            concreteName = ie->ShapeName;
          }
        }

        // 3. Construct Fat Pointer
        if (!concreteName.empty() && !traitName.empty()) {
          std::string vtableName = "_VTable_" + concreteName + "_" + traitName;
          llvm::GlobalVariable *vtable =
              m_Module->getGlobalVariable(vtableName);
          if (vtable) {
            llvm::Type *fatPtrTy = resolveType(targetArgType, false);
            llvm::Value *ctxPtr = m_Builder.CreateBitCast(
                val,
                llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
            llvm::Value *vtablePtr = m_Builder.CreateBitCast(
                vtable,
                llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));

            llvm::Value *fatPtr = llvm::UndefValue::get(fatPtrTy);
            fatPtr = m_Builder.CreateInsertValue(fatPtr, ctxPtr, 0);
            fatPtr = m_Builder.CreateInsertValue(fatPtr, vtablePtr, 1);

            // Pass address of Fat Pointer (Struct)
            llvm::AllocaInst *tmp =
                m_Builder.CreateAlloca(fatPtrTy, nullptr, "dyn_tmp");
            m_Builder.CreateStore(fatPtr, tmp);
            val = tmp;
            // Skip standard casting if we handled it here?
            // paramType is ptr (to dyn struct). val is ptr (to dyn struct).
            // types match.
          }
        }
      }

      if (val->getType() != paramType) {
        if (val->getType()->isIntegerTy() && paramType->isIntegerTy()) {
          val = m_Builder.CreateIntCast(val, paramType, true);
        } else if (val->getType()->isPointerTy() && paramType->isPointerTy()) {
          val = m_Builder.CreateBitCast(val, paramType);
        }
      }
    }

    argsV.push_back(val);
  }
  return m_Builder.CreateCall(callee->getFunctionType(), callee, argsV);
}

PhysEntity CodeGen::genPostfixExpr(const PostfixExpr *post) {
  if (post->Op == TokenType::TokenWrite) {
    return genExpr(post->LHS.get());
  }
  if (post->Op == TokenType::DoubleQuestion) {
    PhysEntity lhs_pe = genExpr(post->LHS.get());
    llvm::Value *lhs_val = lhs_pe.value;
    if (!lhs_val)
      return nullptr;

    bool isNullableSoul = false;
    llvm::Type *innerType = nullptr;
    if (lhs_pe.irType && lhs_pe.irType->isStructTy() &&
        lhs_pe.irType->getStructNumElements() == 2 &&
        lhs_pe.irType->getStructElementType(1)->isIntegerTy(1)) {
      isNullableSoul = true;
      innerType = lhs_pe.irType->getStructElementType(0);
    }

    if (lhs_pe.isAddress) {
      if (isNullableSoul) {
        // L-Value Propagation for Soul: { T, i1 }*
        // 1. GEP to isPresent (index 1) and check
        llvm::Value *isPresentPtr = m_Builder.CreateStructGEP(
            lhs_pe.irType, lhs_val, 1, "soul.isPresentPtr");
        llvm::Value *isPresent = m_Builder.CreateLoad(
            m_Builder.getInt1Ty(), isPresentPtr, "soul.isPresent");
        genNullCheck(isPresent, post);
        // 2. GEP to data (index 0) and return its address
        llvm::Value *dataPtr = m_Builder.CreateStructGEP(lhs_pe.irType, lhs_val,
                                                         0, "soul.dataPtr");
        return PhysEntity(dataPtr, lhs_pe.typeName, innerType, true);
      } else {
        // Raw Pointer case: T**
        llvm::Value *ptrVal =
            m_Builder.CreateLoad(lhs_pe.irType, lhs_val, "nn.load");
        genNullCheck(ptrVal, post);
        return lhs_pe; // Still an address T**
      }
    } else {
      // R-Value (already loaded / value type)
      if (isNullableSoul) {
        llvm::Value *isPresent =
            m_Builder.CreateExtractValue(lhs_val, {1}, "soul.isPresent");
        genNullCheck(isPresent, post);
        llvm::Value *data =
            m_Builder.CreateExtractValue(lhs_val, {0}, "soul.data");
        return PhysEntity(data, lhs_pe.typeName, innerType, false);
      } else {
        genNullCheck(lhs_val, post);
        return lhs_pe;
      }
    }
  }

  llvm::Value *addr = genAddr(post->LHS.get());
  if (!addr)
    return nullptr;
  llvm::Type *type = nullptr;
  if (auto *var = dynamic_cast<const VariableExpr *>(post->LHS.get())) {
    if (m_Symbols.count(var->Name))
      type = m_Symbols[var->Name].soulType;
  } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(addr)) {
    type = gep->getResultElementType();
  } else if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(addr)) {
    type = alloca->getAllocatedType();
  }
  if (!type)
    return nullptr;
  llvm::Value *oldVal = m_Builder.CreateLoad(type, addr, "post_old");
  llvm::Value *newVal;
  if (post->Op == TokenType::PlusPlus)
    newVal = m_Builder.CreateAdd(oldVal, llvm::ConstantInt::get(type, 1),
                                 "postinc_new");
  else
    newVal = m_Builder.CreateSub(oldVal, llvm::ConstantInt::get(type, 1),
                                 "postdec_new");
  m_Builder.CreateStore(newVal, addr);
  return oldVal;
}

PhysEntity CodeGen::genPassExpr(const PassExpr *pe) {
  llvm::Value *val = nullptr;
  if (pe->Value) {
    val = genExpr(pe->Value.get()).load(m_Builder);
  }

  if (!m_CFStack.empty()) {
    auto target = m_CFStack.back();
    cleanupScopes(target.ScopeDepth);
    if (val && target.ResultAddr) {
      m_Builder.CreateStore(val, target.ResultAddr);
    } else if (!pe->Value && target.ResultAddr) {
      // pass none
      llvm::Type *allocaTy =
          llvm::cast<llvm::AllocaInst>(target.ResultAddr)->getAllocatedType();
      m_Builder.CreateStore(llvm::Constant::getNullValue(allocaTy),
                            target.ResultAddr);
    }
    if (target.BreakTarget)
      m_Builder.CreateBr(target.BreakTarget);
    m_Builder.ClearInsertionPoint();
  }
  return nullptr;
}

PhysEntity CodeGen::genBreakExpr(const BreakExpr *be) {
  llvm::Value *val = nullptr;
  if (be->Value)
    val = genExpr(be->Value.get()).load(m_Builder);

  CFInfo *target = nullptr;
  if (be->TargetLabel.empty()) {
    for (auto it = m_CFStack.rbegin(); it != m_CFStack.rend(); ++it) {
      if (it->ContinueTarget != nullptr) {
        target = &(*it);
        break;
      }
    }
  } else {
    for (auto it = m_CFStack.rbegin(); it != m_CFStack.rend(); ++it) {
      if (it->Label == be->TargetLabel) {
        target = &(*it);
        break;
      }
    }
  }

  if (target) {
    cleanupScopes(target->ScopeDepth);
    if (val && target->ResultAddr)
      m_Builder.CreateStore(val, target->ResultAddr);
    if (target->BreakTarget)
      m_Builder.CreateBr(target->BreakTarget);
    m_Builder.ClearInsertionPoint();
  }
  return nullptr;
}

PhysEntity CodeGen::genContinueExpr(const ContinueExpr *ce) {
  CFInfo *target = nullptr;
  if (ce->TargetLabel.empty()) {
    for (auto it = m_CFStack.rbegin(); it != m_CFStack.rend(); ++it) {
      if (it->ContinueTarget != nullptr) {
        target = &(*it);
        break;
      }
    }
  } else {
    for (auto it = m_CFStack.rbegin(); it != m_CFStack.rend(); ++it) {
      if (it->Label == ce->TargetLabel) {
        target = &(*it);
        break;
      }
    }
  }
  if (target && target->ContinueTarget) {
    cleanupScopes(target->ScopeDepth);
    m_Builder.CreateBr(target->ContinueTarget);
    m_Builder.ClearInsertionPoint();
  }
  return nullptr;
}

PhysEntity CodeGen::genUnsafeExpr(const UnsafeExpr *ue) {
  return genExpr(ue->Expression.get());
}

PhysEntity CodeGen::genInitStructExpr(const InitStructExpr *init) {
  std::string shapeName = init->ShapeName;
  if (init->ResolvedType && init->ResolvedType->isShape()) {
    shapeName = init->ResolvedType->getSoulName();
  }

  llvm::StructType *st = m_StructTypes[shapeName];
  if (!st) {
    error(init, "Unknown struct type " + shapeName);
    return nullptr;
  }

  llvm::Value *alloca =
      m_Builder.CreateAlloca(st, nullptr, init->ShapeName + "_init");
  auto &fields = m_StructFieldNames[shapeName];

  for (const auto &f : init->Members) {
    int idx = -1;
    for (int i = 0; i < (int)fields.size(); ++i) {
      std::string fn = fields[i];
      while (!fn.empty() && (fn[0] == '^' || fn[0] == '*' || fn[0] == '&' ||
                             fn[0] == '#' || fn[0] == '~' || fn[0] == '!'))
        fn = fn.substr(1);
      while (!fn.empty() &&
             (fn.back() == '#' || fn.back() == '?' || fn.back() == '!'))
        fn.pop_back();

      if (fn == f.first) {
        idx = i;
        break;
      }

      // Try stripping the initializer name as well
      std::string initName = f.first;
      while (!initName.empty() &&
             (initName[0] == '^' || initName[0] == '*' || initName[0] == '&' ||
              initName[0] == '#' || initName[0] == '~' || initName[0] == '!' ||
              initName[0] == '?'))
        initName = initName.substr(1);
      while (!initName.empty() &&
             (initName.back() == '#' || initName.back() == '?' ||
              initName.back() == '!'))
        initName.pop_back();

      if (fn == initName) {
        idx = i;
        break;
      }
    }
    if (idx == -1) {
      error(init, "Unknown field " + f.first);
      return nullptr;
    }

    llvm::Value *fieldVal = nullptr;

    // [Fix] ShapeKind Aware Type Lookup
    llvm::Type *elemTy = nullptr;
    auto kind = ShapeKind::Struct;
    if (m_Shapes.count(shapeName)) {
      kind = m_Shapes[shapeName]->Kind;
      if (kind == ShapeKind::Union) {
        auto sh = m_Shapes[shapeName];
        if (idx >= 0 && idx < (int)sh->Members.size()) {
          if (sh->Members[idx].ResolvedType)
            elemTy = getLLVMType(sh->Members[idx].ResolvedType);
          else
            elemTy = resolveType(sh->Members[idx].Type, false);
        }
      } else {
        elemTy = st->getElementType(idx);
      }
    } else {
      elemTy = st->getElementType(idx);
    }
    if (!elemTy)
      elemTy = llvm::Type::getInt8Ty(m_Context);

    if (dynamic_cast<const UnsetExpr *>(f.second.get())) {
      fieldVal = elemTy->isPointerTy() ? llvm::Constant::getNullValue(elemTy)
                                       : llvm::UndefValue::get(elemTy);
    } else {
      fieldVal = genExpr(f.second.get()).load(m_Builder);
      // [Chapter 6 Extension] Nullable Soul Wrap for Init
      if (fieldVal && fieldVal->getType() != elemTy && elemTy->isStructTy() &&
          elemTy->getStructNumElements() == 2 &&
          elemTy->getStructElementType(1)->isIntegerTy(1)) {
        if (fieldVal->getType() == elemTy->getStructElementType(0)) {
          llvm::Value *wrapped = llvm::UndefValue::get(elemTy);
          wrapped = m_Builder.CreateInsertValue(wrapped, fieldVal, {0});
          wrapped = m_Builder.CreateInsertValue(
              wrapped,
              llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), 1), {1});
          fieldVal = wrapped;
        } else if (dynamic_cast<const NoneExpr *>(f.second.get()) ||
                   fieldVal->getType()->isPointerTy()) {
          llvm::Value *wrapped = llvm::UndefValue::get(elemTy);
          wrapped = m_Builder.CreateInsertValue(
              wrapped,
              llvm::Constant::getNullValue(elemTy->getStructElementType(0)),
              {0});
          wrapped = m_Builder.CreateInsertValue(
              wrapped,
              llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), 0), {1});
          fieldVal = wrapped;
        }
      }
    }

    if (!fieldVal)
      return nullptr;

    llvm::Value *fieldAddr = nullptr;
    if (m_Shapes.count(shapeName)) {
      auto kind = m_Shapes[shapeName]->Kind;
      if (kind == ShapeKind::Union) {
        // Bare Union: bitcast base to member pointer type
        fieldAddr = m_Builder.CreateBitCast(
            alloca, llvm::PointerType::getUnqual(fieldVal->getType()));
      } else if (kind == ShapeKind::Enum) {
        // Tagged Union: Store tag and payload
        // 1. Tag
        llvm::Value *tagAddr =
            m_Builder.CreateStructGEP(st, alloca, 0, "tag_addr");
        m_Builder.CreateStore(m_Builder.getInt8(idx), tagAddr);
        // 2. Payload
        if (st->getNumElements() > 1) {
          llvm::Value *payloadAddr =
              m_Builder.CreateStructGEP(st, alloca, 1, "payload_addr");
          fieldAddr = m_Builder.CreateBitCast(
              payloadAddr, llvm::PointerType::getUnqual(fieldVal->getType()));
        }
      } else {
        fieldAddr =
            m_Builder.CreateStructGEP(st, alloca, idx, "field_" + f.first);
      }
    } else {
      fieldAddr =
          m_Builder.CreateStructGEP(st, alloca, idx, "field_" + f.first);
    }

    if (f.second && f.second->ResolvedType &&
        f.second->ResolvedType->isSharedPtr()) {
      emitAcquire(fieldVal);
    }

    m_Builder.CreateStore(fieldVal, fieldAddr);
  }

  return m_Builder.CreateLoad(st, alloca);
}

PhysEntity CodeGen::genNewExpr(const NewExpr *newExpr) {
  llvm::Type *type = nullptr;
  if (newExpr->ResolvedType) {
    auto rt = newExpr->ResolvedType;
    if (rt->isPointer() || rt->isSmartPointer()) {
      if (auto ptr = std::dynamic_pointer_cast<PointerType>(rt)) {
        rt = ptr->PointeeType;
      }
    }
    type = getLLVMType(rt);
  } else {
    type = resolveType(newExpr->Type, false);
  }

  if (!type)
    return nullptr;

  // Size of type
  llvm::DataLayout dl(m_Module.get());
  uint64_t size = dl.getTypeAllocSize(type);

  // Call malloc
  llvm::Function *mallocFn = m_Module->getFunction("malloc");
  if (!mallocFn) {
    // Attempt lib_malloc or just declare malloc
    llvm::Type *sizeTy = llvm::Type::getInt64Ty(m_Context);
    llvm::Type *ptrTy = m_Builder.getPtrTy();
    mallocFn = llvm::Function::Create(
        llvm::FunctionType::get(ptrTy, {sizeTy}, false),
        llvm::Function::ExternalLinkage, "malloc", m_Module.get());
  }

  llvm::Value *sizeVal =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), size);
  llvm::Value *voidPtr = m_Builder.CreateCall(mallocFn, sizeVal, "new_alloc");

  // In LLVM 17 with opaque pointers, we just use the pointer.
  // But we need to handle initialization.
  llvm::Value *heapPtr = voidPtr;

  if (newExpr->Initializer) {
    llvm::Value *initVal = genExpr(newExpr->Initializer.get()).load(m_Builder);
    if (initVal) {
      if (initVal->getType() != type) {
        // Attempt cast
        if (initVal->getType()->isIntegerTy() && type->isIntegerTy()) {
          initVal = m_Builder.CreateIntCast(initVal, type, true);
        }
      }
      m_Builder.CreateStore(initVal, heapPtr);
    }
  }

  return heapPtr;
}

PhysEntity CodeGen::genTupleExpr(const TupleExpr *expr) {
  std::vector<llvm::Constant *> consts;
  std::vector<llvm::Value *> values;
  bool allConst = true;

  for (auto &e : expr->Elements) {
    PhysEntity v_ent = genExpr(e.get()).load(m_Builder);
    llvm::Value *v = v_ent.load(m_Builder);
    if (!v)
      return nullptr;
    values.push_back(v);

    // [Constitution] Shared Pointer RC Acquisition for Tuples
    if (e->ResolvedType && e->ResolvedType->isSharedPtr()) {
      emitAcquire(v);
    }

    if (auto *c = llvm::dyn_cast<llvm::Constant>(v)) {
      consts.push_back(c);
    } else {
      allConst = false;
    }
  }

  if (allConst) {
    return llvm::ConstantStruct::getAnon(m_Context, consts);
  }

  std::vector<llvm::Type *> types;
  for (auto *v : values)
    types.push_back(v->getType());
  llvm::StructType *st = llvm::StructType::get(m_Context, types);
  llvm::Value *val = llvm::UndefValue::get(st);
  for (size_t i = 0; i < values.size(); ++i) {
    val = m_Builder.CreateInsertValue(val, values[i], i);
  }
  return val;
}

PhysEntity CodeGen::genArrayExpr(const ArrayExpr *expr) {
  if (expr->Elements.empty())
    return nullptr;

  std::vector<llvm::Constant *> consts;
  std::vector<llvm::Value *> values;
  bool allConst = true;

  for (auto &e : expr->Elements) {
    PhysEntity v_ent = genExpr(e.get()).load(m_Builder);
    llvm::Value *v = v_ent.load(m_Builder);
    if (!v)
      return nullptr;
    values.push_back(v);
    if (auto *c = llvm::dyn_cast<llvm::Constant>(v)) {
      consts.push_back(c);
    } else {
      allConst = false;
    }
  }

  llvm::Type *elemTy = values[0]->getType();
  llvm::ArrayType *arrTy = llvm::ArrayType::get(elemTy, values.size());

  std::string elemTypeName = "i32"; // default
  if (!values.empty()) {
    if (m_TypeToName.count(elemTy)) {
      elemTypeName = m_TypeToName[elemTy];
    }
  }
  std::string arrayTypeName =
      "[" + elemTypeName + "; " + std::to_string(values.size()) + "]";

  if (allConst) {
    return PhysEntity(llvm::ConstantArray::get(arrTy, consts), arrayTypeName,
                      arrTy, false);
  }

  llvm::Value *val = llvm::UndefValue::get(arrTy);
  for (size_t i = 0; i < values.size(); ++i) {
    llvm::Value *elt = values[i];
    if (elt->getType() != elemTy) {
      // Minimal cast attempt for safety
      if (elt->getType()->isIntegerTy() && elemTy->isIntegerTy())
        elt = m_Builder.CreateIntCast(elt, elemTy, true);
      else if (elt->getType()->isPointerTy() && elemTy->isPointerTy())
        elt = m_Builder.CreateBitCast(elt, elemTy);
    }
    val = m_Builder.CreateInsertValue(val, elt, i);
  }
  return PhysEntity(val, arrayTypeName, arrTy, false);
}

PhysEntity CodeGen::genAnonymousRecordExpr(const AnonymousRecordExpr *expr) {
  std::string uniqueName = expr->AssignedTypeName;
  if (uniqueName.empty()) {
    error(expr, "Anonymous record missing type name");
    return nullptr;
  }

  llvm::Type *recType = nullptr;
  if (expr->ResolvedType) {
    recType = getLLVMType(expr->ResolvedType);
  } else {
    recType = resolveType(uniqueName, false);
    if (!recType && m_StructTypes.count(uniqueName)) {
      recType = m_StructTypes[uniqueName];
    }
  }

  if (!recType) {
    error(expr, "Anonymous record type '" + uniqueName + "' not found");
    return nullptr;
  }

  llvm::Value *alloca = m_Builder.CreateAlloca(recType, nullptr, "anon_rec");

  for (size_t i = 0; i < expr->Fields.size(); ++i) {
    PhysEntity val_ent = genExpr(expr->Fields[i].second.get()).load(m_Builder);
    llvm::Value *val = val_ent.load(m_Builder);
    if (!val)
      return nullptr;

    // GEP to element i (Struct layout matches Fields order)
    llvm::Value *ptr = m_Builder.CreateStructGEP(recType, alloca, i);

    // [Constitution] Shared Pointer RC Acquisition for Anonymous Records
    if (expr->Fields[i].second->ResolvedType &&
        expr->Fields[i].second->ResolvedType->isSharedPtr()) {
      emitAcquire(val);
    }

    m_Builder.CreateStore(val, ptr);
  }

  return m_Builder.CreateLoad(recType, alloca);
}

llvm::Constant *CodeGen::genConstant(const Expr *expr, llvm::Type *targetType) {
  if (auto *num = dynamic_cast<const NumberExpr *>(expr)) {
    if (targetType && targetType->isIntegerTy()) {
      return llvm::ConstantInt::get(targetType, num->Value);
    }
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context),
                                  num->Value);
  }

  if (auto *b = dynamic_cast<const BoolExpr *>(expr)) {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context),
                                  b->Value ? 1 : 0);
  }

  if (auto *flt = dynamic_cast<const FloatExpr *>(expr)) {
    if (targetType && targetType->isFloatingPointTy())
      return llvm::ConstantFP::get(targetType, flt->Value);
    return llvm::ConstantFP::get(llvm::Type::getFloatTy(m_Context), flt->Value);
  }

  if (dynamic_cast<const NoneExpr *>(expr) ||
      dynamic_cast<const NullExpr *>(expr)) {
    if (targetType && targetType->isStructTy() &&
        targetType->getStructNumElements() == 2 &&
        targetType->getStructElementType(1)->isIntegerTy(1)) {
      // Nullable Soul Wrapper constant for none/nullptr
      llvm::Type *baseTy = targetType->getStructElementType(0);
      return llvm::ConstantStruct::get(
          (llvm::StructType *)targetType,
          {llvm::Constant::getNullValue(baseTy),
           llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), 0)});
    }
    return llvm::ConstantPointerNull::get(m_Builder.getPtrTy());
  }

  if (auto *rec = dynamic_cast<const AnonymousRecordExpr *>(expr)) {
    std::string uniqueName = rec->AssignedTypeName;
    if (uniqueName.empty())
      return nullptr;

    if (!m_StructTypes.count(uniqueName))
      return nullptr;

    llvm::StructType *st = m_StructTypes[uniqueName];
    std::vector<llvm::Constant *> fields;
    for (size_t i = 0; i < rec->Fields.size(); ++i) {
      llvm::Type *elemTy = st->getElementType(i);
      llvm::Constant *c = genConstant(rec->Fields[i].second.get(), elemTy);
      if (!c) {
        // Fallback: If we can't generate constant, maybe return null (error
        // upstream)
        return nullptr;
      }
      fields.push_back(c);
    }
    return llvm::ConstantStruct::get(st, fields);
  }

  if (auto *unary = dynamic_cast<const UnaryExpr *>(expr)) {
    if (unary->Op == TokenType::Minus) {
      llvm::Constant *rhs = genConstant(unary->RHS.get(), targetType);
      if (rhs)
        return llvm::ConstantExpr::getNeg(rhs);
    }
  }

  if (auto *cast = dynamic_cast<const CastExpr *>(expr)) {
    llvm::Type *destTy = resolveType(cast->TargetType, false);
    if (!destTy)
      return nullptr;

    llvm::Constant *rhs = genConstant(cast->Expression.get());
    if (!rhs)
      return nullptr;

    if (rhs->getType() == destTy)
      return rhs;

    if (destTy->isIntegerTy() && rhs->getType()->isIntegerTy()) {
      return llvm::ConstantExpr::getIntegerCast(rhs, destTy, true);
    } else if (destTy->isFloatingPointTy() && rhs->getType()->isIntegerTy()) {
      return llvm::ConstantExpr::getSIToFP(rhs, destTy);
    } else if (destTy->isIntegerTy() && rhs->getType()->isFloatingPointTy()) {
      return llvm::ConstantExpr::getFPToSI(rhs, destTy);
    } else {
      return llvm::ConstantExpr::getBitCast(rhs, destTy);
    }
  }

  return nullptr;
}

PhysEntity CodeGen::genRepeatedArrayExpr(const RepeatedArrayExpr *expr) {
  PhysEntity val_ent = genExpr(expr->Value.get());
  llvm::Value *val = val_ent.load(m_Builder);
  if (!val)
    return nullptr;
  uint64_t count = 0;

  if (auto *num = dynamic_cast<const NumberExpr *>(expr->Count.get())) {
    count = num->Value;
  } else if (auto *var =
                 dynamic_cast<const VariableExpr *>(expr->Count.get())) {
    if (var->HasConstantValue) {
      count = var->ConstantValue;
    } else {
      error(expr, "Repeat count variable must be a compile-time constant");
      return nullptr;
    }
  } else {
    error(expr, "Repeat count must be a numeric literal or const generic "
                "parameter");
    return nullptr;
  }
  llvm::Type *elemTy = val->getType();
  llvm::ArrayType *arrTy = llvm::ArrayType::get(elemTy, count);
  if (auto *c = llvm::dyn_cast<llvm::Constant>(val)) {
    std::vector<llvm::Constant *> elements(count, c);
    llvm::Value *arrVal = llvm::ConstantArray::get(arrTy, elements);
    std::string arrayTypeName =
        "[" + val_ent.typeName + "; " + std::to_string(count) + "]";
    return PhysEntity(arrVal, arrayTypeName, arrTy, false);
  }
  llvm::Value *alloca =
      m_Builder.CreateAlloca(arrTy, nullptr, "repeated_array");
  for (uint64_t i = 0; i < count; ++i) {
    llvm::Value *ptr = m_Builder.CreateInBoundsGEP(
        arrTy, alloca,
        {m_Builder.getInt32(0),
         llvm::ConstantInt::get(m_Builder.getInt64Ty(), i)});
    m_Builder.CreateStore(val, ptr);
  }
  std::string arrayTypeName =
      "[" + val_ent.typeName + "; " + std::to_string(count) + "]";
  return PhysEntity(alloca, arrayTypeName, arrTy, true);
}

} // namespace toka
