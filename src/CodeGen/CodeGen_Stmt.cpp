#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>
#include <typeinfo>

namespace toka {

llvm::Value *CodeGen::genReturnStmt(const ReturnStmt *ret) {
  llvm::Value *retVal = nullptr;
  if (ret->ReturnValue) {
    retVal = genExpr(ret->ReturnValue.get()).load(m_Builder);

    // [Fix] Premature Drop in Return
    // If we are returning a local Shared Pointer variable, we are creating a
    // copy (the return value). We must increment the RefCount because the local
    // variable itself is about to be dropped (DecRef) by cleanupScopes. Logic:
    // Return(Copy) -> RC++ -> ScopeExit(Drop) -> RC--. Net change 0 (Object
    // survives).

    // 1. Identify if it's a shared variable
    const Expr *inner = ret->ReturnValue.get();
    while (true) {
      if (auto *pe = dynamic_cast<const PostfixExpr *>(inner))
        inner = pe->LHS.get();
      else if (auto *ue = dynamic_cast<const UnaryExpr *>(inner))
        inner = ue->RHS.get();
      else
        break;
    }

    if (auto *ve = dynamic_cast<const VariableExpr *>(inner)) {
      std::string baseName = ve->Name;
      // Scrub decorators
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
        if (sym.morphology == Morphology::Shared &&
            retVal->getType()->isStructTy()) {
          // Confirm it is { T*, RC* }
          llvm::Type *stTy = retVal->getType();
          if (stTy->getStructNumElements() == 2) {
            // Manual IncRef
            llvm::Value *refPtr =
                m_Builder.CreateExtractValue(retVal, 1, "ret_inc_refptr");
            llvm::Value *refNN =
                m_Builder.CreateIsNotNull(refPtr, "ret_inc_nn");

            llvm::Function *F = m_Builder.GetInsertBlock()->getParent();
            llvm::BasicBlock *doIncBB =
                llvm::BasicBlock::Create(m_Context, "ret_inc", F);
            llvm::BasicBlock *contBB =
                llvm::BasicBlock::Create(m_Context, "ret_cont", F);

            m_Builder.CreateCondBr(refNN, doIncBB, contBB);
            m_Builder.SetInsertPoint(doIncBB);

            llvm::Value *cnt =
                m_Builder.CreateLoad(llvm::Type::getInt32Ty(m_Context), refPtr);
            llvm::Value *inc = m_Builder.CreateAdd(
                cnt,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
            m_Builder.CreateStore(inc, refPtr);

            m_Builder.CreateBr(contBB);
            m_Builder.SetInsertPoint(contBB);
          }
        }
      }
    }

    if (auto *varExpr =
            dynamic_cast<const VariableExpr *>(ret->ReturnValue.get())) {
      if (varExpr->IsUnique) {
        if (m_Symbols.count(varExpr->Name)) {
          llvm::Value *alloca = m_Symbols[varExpr->Name].allocaPtr;
          if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(alloca)) {
            m_Builder.CreateStore(
                llvm::Constant::getNullValue(ai->getAllocatedType()), alloca);
          }
        }
      }
    }
  }

  cleanupScopes(0);
  if (retVal)
    return m_Builder.CreateRet(retVal);
  return m_Builder.CreateRetVoid();
}

llvm::Value *CodeGen::genBlockStmt(const BlockStmt *bs) {
  m_ScopeStack.push_back({});
  llvm::Value *lastVal = nullptr;
  for (const auto &s : bs->Statements) {
    lastVal = genStmt(s.get());
    // Liveness check: stop if terminator was generated
    if (m_Builder.GetInsertBlock() &&
        m_Builder.GetInsertBlock()->getTerminator())
      break;
  }

  if (m_ScopeStack.empty())
    return lastVal;

  cleanupScopes(m_ScopeStack.size() - 1);
  m_ScopeStack.pop_back();
  return lastVal;
}

llvm::Value *CodeGen::genDeleteStmt(const DeleteStmt *del) {
  llvm::Function *freeFunc = m_Module->getFunction("free");
  if (freeFunc) {
    llvm::Value *val = genExpr(del->Expression.get()).load(m_Builder);
    if (val && val->getType()->isPointerTy()) {
      llvm::Value *casted = m_Builder.CreateBitCast(
          val, llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
      m_Builder.CreateCall(freeFunc, casted);
    }
  }
  return nullptr;
}

void CodeGen::cleanupScopes(size_t targetDepth) {
  llvm::BasicBlock *currBB = m_Builder.GetInsertBlock();
  if (!currBB || currBB->getTerminator())
    return;

  // Cleanup scopes from high to low (up to but not including targetDepth)
  for (int i = (int)m_ScopeStack.size() - 1; i >= (int)targetDepth; --i) {
    auto &scope = m_ScopeStack[i];
    for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
      if (it->IsShared && it->Alloca) {
        llvm::Type *shTy =
            llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType();
        if (shTy->isStructTy()) {
          llvm::Value *sh = m_Builder.CreateLoad(shTy, it->Alloca, "sh_pop");
          llvm::Value *refPtr = m_Builder.CreateExtractValue(sh, 1, "ref_ptr");
          llvm::Value *refIsNotNull =
              m_Builder.CreateIsNotNull(refPtr, "ref_not_null");

          llvm::Function *f = currBB->getParent();
          if (f) {
            llvm::BasicBlock *decBB =
                llvm::BasicBlock::Create(m_Context, "sh_dec", f);
            llvm::BasicBlock *afterDecBB =
                llvm::BasicBlock::Create(m_Context, "sh_after_dec", f);

            m_Builder.CreateCondBr(refIsNotNull, decBB, afterDecBB);
            m_Builder.SetInsertPoint(decBB);

            llvm::Value *count = m_Builder.CreateLoad(
                llvm::Type::getInt32Ty(m_Context), refPtr, "ref_count");
            llvm::Value *dec = m_Builder.CreateSub(
                count,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
            m_Builder.CreateStore(dec, refPtr);
            llvm::Value *isZero = m_Builder.CreateICmpEQ(
                dec,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));

            llvm::BasicBlock *freeBB =
                llvm::BasicBlock::Create(m_Context, "sh_free", f);
            // Reuse afterDecBB as continuation
            m_Builder.CreateCondBr(isZero, freeBB, afterDecBB);

            m_Builder.SetInsertPoint(freeBB);
            llvm::Function *freeFunc = m_Module->getFunction("free");
            if (freeFunc) {
              llvm::Value *data =
                  m_Builder.CreateExtractValue(sh, 0, "data_ptr");

              if (it->HasDrop) {
                llvm::Function *dropFn = m_Module->getFunction(it->DropFunc);
                if (dropFn) {
                  m_Builder.CreateCall(dropFn, {data});
                }
              }
              // Check data for null not strictly needed if refPtr valid?
              // But refPtr controls lifecycle.
              m_Builder.CreateCall(
                  freeFunc, m_Builder.CreateBitCast(
                                data, llvm::PointerType::getUnqual(
                                          llvm::Type::getInt8Ty(m_Context))));
              m_Builder.CreateCall(
                  freeFunc, m_Builder.CreateBitCast(
                                refPtr, llvm::PointerType::getUnqual(
                                            llvm::Type::getInt8Ty(m_Context))));
            }
            m_Builder.CreateBr(afterDecBB);

            m_Builder.SetInsertPoint(afterDecBB);
            currBB = afterDecBB;
          }
        }
      } else if (it->IsUniquePointer && it->Alloca) {
        llvm::Value *ptr = m_Builder.CreateLoad(
            llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType(),
            it->Alloca);
        llvm::Value *notNull = m_Builder.CreateIsNotNull(ptr, "not_null");
        llvm::Function *f = currBB->getParent();
        if (f) {
          llvm::BasicBlock *freeBB =
              llvm::BasicBlock::Create(m_Context, "un_free", f);
          llvm::BasicBlock *contBB =
              llvm::BasicBlock::Create(m_Context, "un_cont", f);
          m_Builder.CreateCondBr(notNull, freeBB, contBB);
          m_Builder.SetInsertPoint(freeBB);

          if (it->HasDrop) {
            llvm::Function *dropFn = m_Module->getFunction(it->DropFunc);
            if (dropFn) {
              // ptr is the T* loaded from alloca
              m_Builder.CreateCall(dropFn, {ptr});
            }
          }

          llvm::Function *freeFunc = m_Module->getFunction("free");
          if (freeFunc) {
            m_Builder.CreateCall(
                freeFunc, m_Builder.CreateBitCast(
                              ptr, llvm::PointerType::getUnqual(
                                       llvm::Type::getInt8Ty(m_Context))));
          }
          m_Builder.CreateBr(contBB);
          m_Builder.SetInsertPoint(contBB);
          currBB = contBB;
        }
      } else if (it->HasDrop && it->Alloca) {
        llvm::Function *dropFn = m_Module->getFunction(it->DropFunc);
        if (dropFn) {
          m_Builder.CreateCall(dropFn, {it->Alloca});
        }
      }
    }
  }
}

llvm::Value *CodeGen::genUnsafeStmt(const UnsafeStmt *us) {
  return genStmt(us->Statement.get());
}

llvm::Value *CodeGen::genExprStmt(const ExprStmt *es) {
  return genExpr(es->Expression.get()).load(m_Builder);
}

} // namespace toka