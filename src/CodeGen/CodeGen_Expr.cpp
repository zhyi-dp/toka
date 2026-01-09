#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>
#include <typeinfo>

namespace toka {

llvm::Value *CodeGen::genBinaryExpr(const BinaryExpr *expr) {
  const BinaryExpr *bin = expr;
  if (bin->Op == "=" || bin->Op == "+=" || bin->Op == "-=" || bin->Op == "*=" ||
      bin->Op == "/=") {
    llvm::Value *ptr = nullptr;
    llvm::Type *destType = nullptr;
    TokaSymbol *symLHS = nullptr;

    if (auto *varLHS = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
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
      }

      // Variable on LHS is always a MUTATION of the soul
      ptr = getEntityAddr(varLHS->Name);
      if (symLHS) {
        destType = symLHS->soulType;
      }

      // Semantic Check (LHS must be mutable)
      if (symLHS && !symLHS->isMutable) {
        error(bin, "Cannot modify immutable variable '" + varLHS->Name + "'");
        return nullptr;
      }
    } else if (auto *unaryLHS =
                   dynamic_cast<const UnaryExpr *>(bin->LHS.get())) {
      if (unaryLHS->Op == TokenType::Star || unaryLHS->Op == TokenType::Caret ||
          unaryLHS->Op == TokenType::Tilde) {
        // Unary morphology on LHS is a REBIND of the identity
        if (auto *v = dynamic_cast<const VariableExpr *>(unaryLHS->RHS.get())) {
          ptr = getIdentityAddr(v->Name);
          destType = m_Builder.getPtrTy();
        }
      }
    }

    if (!ptr) {
      ptr = genAddr(bin->LHS.get());
    }

    if (!ptr) {
      return nullptr;
    }

    llvm::Value *rhsVal = genExpr(bin->RHS.get());
    if (!rhsVal)
      return nullptr;

    // Determine destType if not already found (for MemberExpr, ArrayIndexExpr,
    // etc.)
    if (!destType) {
      if (auto *memLHS = dynamic_cast<const MemberExpr *>(bin->LHS.get())) {
        llvm::Type *objType = nullptr;
        std::function<std::string(const Expr *)> getBaseName =
            [&](const Expr *e) -> std::string {
          if (auto *v = dynamic_cast<const VariableExpr *>(e))
            return v->Name;
          if (auto *p = dynamic_cast<const PostfixExpr *>(e))
            return getBaseName(p->LHS.get());
          if (auto *u = dynamic_cast<const UnaryExpr *>(e))
            return getBaseName(u->RHS.get());
          return "";
        };

        std::string baseName = getBaseName(memLHS->Object.get());
        while (!baseName.empty() && (baseName[0] == '*' || baseName[0] == '#' ||
                                     baseName[0] == '&' || baseName[0] == '^' ||
                                     baseName[0] == '~' || baseName[0] == '!'))
          baseName = baseName.substr(1);
        while (!baseName.empty() &&
               (baseName.back() == '#' || baseName.back() == '?' ||
                baseName.back() == '!'))
          baseName.pop_back();

        if (m_Symbols.count(baseName))
          objType = m_Symbols[baseName].soulType;
        else
          objType = genExpr(memLHS->Object.get())->getType();

        if (objType && objType->isStructTy()) {
          llvm::StructType *st = llvm::cast<llvm::StructType>(objType);
          std::string memberName = memLHS->Member;
          while (!memberName.empty() &&
                 (memberName[0] == '^' || memberName[0] == '*' ||
                  memberName[0] == '&' || memberName[0] == '#' ||
                  memberName[0] == '~' || memberName[0] == '!'))
            memberName = memberName.substr(1);

          std::string stName = m_TypeToName[st];
          if (!stName.empty()) {
            auto &fields = m_StructFieldNames[stName];
            for (int i = 0; i < (int)fields.size(); ++i) {
              std::string fn = fields[i];
              while (!fn.empty() &&
                     (fn[0] == '^' || fn[0] == '*' || fn[0] == '&' ||
                      fn[0] == '#' || fn[0] == '~' || fn[0] == '!'))
                fn = fn.substr(1);
              while (!fn.empty() &&
                     (fn.back() == '#' || fn.back() == '?' || fn.back() == '!'))
                fn.pop_back();

              if (fn == memberName) {
                destType = st->getElementType(i);
                break;
              }
            }
          }
        }
      } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(ptr)) {
        destType = gep->getResultElementType();
      } else if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(ptr)) {
        destType = ai->getAllocatedType();
      } else if (auto *unary =
                     dynamic_cast<const UnaryExpr *>(bin->LHS.get())) {
        if (unary->Op == TokenType::Star) {
          // Dereference assignment: *ptr = val
          // For LLVM 17+, we fallback to rhs type if we can't find it
          destType = rhsVal->getType();
        }
      }
    }

    if (!destType) {
      destType = rhsVal->getType(); // Last resort
    }

    if (bin->Op != "=") {
      // Compound: LHS = LHS op RHS
      llvm::Value *lhsVal = m_Builder.CreateLoad(destType, ptr);
      if (lhsVal->getType() != rhsVal->getType()) {
        error(bin, "Type mismatch in compound assignment");
        return nullptr;
      }

      llvm::Value *res = nullptr;
      if (lhsVal->getType()->isFloatingPointTy()) {
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

      rhsVal = res;
    }

    bool typesMatch = (destType == rhsVal->getType());
    if (!typesMatch) {
      // Fallback 1: Integer width matching (i32 vs i32)
      if (destType->isIntegerTy() && rhsVal->getType()->isIntegerTy()) {
        if (destType->getIntegerBitWidth() ==
            rhsVal->getType()->getIntegerBitWidth()) {
          typesMatch = true;
        }
      }
      // Fallback 2: Pointer matching
      if (destType->isPointerTy() && rhsVal->getType()->isPointerTy()) {
        typesMatch = true;
      }
    }

    if (!typesMatch) {
      error(bin, "Type mismatch in assignment");
      return nullptr;
    }

    m_Builder.CreateStore(rhsVal, ptr);
    return rhsVal;
  }

  // Logical Operators (Short-circuiting)
  if (bin->Op == "&&") {
    llvm::Value *lhs = genExpr(bin->LHS.get());
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
    llvm::Value *rhs = genExpr(bin->RHS.get());
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
    llvm::Value *lhs = genExpr(bin->LHS.get());
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
    llvm::Value *rhs = genExpr(bin->RHS.get());
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

        if (m_ValueIsShared.count(baseName) && m_ValueIsShared[baseName]) {
          return genExpr(target);
        }
        return getEntityAddr(v->Name);
      }
      return genExpr(e);
    };

    llvm::Value *lhsVal = evaluatePeek(bin->LHS.get());
    if (!lhsVal)
      return nullptr;

    llvm::Type *lhsTy = lhsVal->getType();
    if (lhsTy->isStructTy() && lhsTy->getStructNumElements() == 2) {
      // shared pointer: extract raw pointer
      lhsVal = m_Builder.CreateExtractValue(lhsVal, 0, "shared_ptr_val");
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
  llvm::Value *lhs = genExpr(bin->LHS.get());
  if (!lhs) {
    return nullptr;
  }

  if (!m_Builder.GetInsertBlock() ||
      m_Builder.GetInsertBlock()->getTerminator()) {
    return nullptr;
  }

  llvm::Value *rhs = genExpr(bin->RHS.get());
  if (!rhs) {
    return nullptr;
  }

  if (!m_Builder.GetInsertBlock() ||
      m_Builder.GetInsertBlock()->getTerminator()) {
    return nullptr;
  }

  llvm::Type *lhsType = lhs->getType();
  llvm::Type *rhsType = rhs->getType();

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
        lhs = m_Builder.CreateSExt(lhs, rhsType, "lhs_ext");
        lhsType = rhsType;
      } else if (lhsType->getIntegerBitWidth() >
                 rhsType->getIntegerBitWidth()) {
        rhs = m_Builder.CreateSExt(rhs, lhsType, "rhs_ext");
        rhsType = lhsType;
      }
    } else {
      if (lhsType != rhsType) {
        error(bin, "Type mismatch in binary expression");
        return nullptr;
      }
    }
  }

  if (!lhsType->isIntOrIntVectorTy() && !lhsType->isPtrOrPtrVectorTy()) {
    std::string s;
    llvm::raw_string_ostream os(s);
    lhsType->print(os);
    error(bin, "Invalid type for comparison: " + os.str() +
                   ". Comparisons are only allowed for scalars "
                   "(integers/pointers).");
    return nullptr;
  }

  if (bin->Op == "+") {
    if (lhs->getType()->isPointerTy()) {
      // Find elemTy
      llvm::Type *elemTy = nullptr;
      if (auto *ve = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
        elemTy = m_ValueElementTypes[ve->Name];
      } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(lhs)) {
        elemTy = gep->getResultElementType();
      }
      if (!elemTy)
        elemTy = llvm::Type::getInt8Ty(m_Context);

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
      llvm::Type *elemTy = nullptr;
      if (auto *ve = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
        elemTy = m_ValueElementTypes[ve->Name];
      }
      if (!elemTy)
        elemTy = llvm::Type::getInt8Ty(m_Context);
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
    return m_Builder.CreateICmpSLT(lhs, rhs, "lt_tmp");
  }
  if (bin->Op == ">") {
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
    auto unwrap = [&](llvm::Value *v) -> llvm::Value * {
      while (v->getType()->isStructTy() &&
             v->getType()->getStructNumElements() == 1) {
        v = m_Builder.CreateExtractValue(v, 0);
      }
      return v;
    };

    lhs = unwrap(lhs);
    rhs = unwrap(rhs);

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
  return nullptr;
}

llvm::Value *CodeGen::genUnaryExpr(const UnaryExpr *unary) {
  if (unary->Op == TokenType::PlusPlus || unary->Op == TokenType::MinusMinus) {
    llvm::Value *addr = genAddr(unary->RHS.get());
    if (!addr)
      return nullptr;
    llvm::Type *type = nullptr;
    if (auto *var = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      type = m_ValueElementTypes[var->Name];
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

  // Identity and address-of: *p, &p
  if (unary->Op == TokenType::Ampersand) {
    if (auto *var = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      return getEntityAddr(var->Name);
    }
    return genAddr(unary->RHS.get());
  }

  if (unary->Op == TokenType::Star) {
    return emitEntityAddr(unary->RHS.get());
  }

  // Morphology symbols: ^p, ~p
  if (unary->Op == TokenType::Caret) {
    if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      llvm::Value *alloca = getIdentityAddr(v->Name);
      if (alloca) {
        // Get the base name (no morphology) for symbol lookup
        std::string baseName = v->Name;
        while (!baseName.empty() &&
               (baseName[0] == '*' || baseName[0] == '#' ||
                baseName[0] == '&' || baseName[0] == '^' || baseName[0] == '~'))
          baseName = baseName.substr(1);
        while (!baseName.empty() &&
               (baseName.back() == '#' || baseName.back() == '?' ||
                baseName.back() == '!'))
          baseName.pop_back();

        if (m_Symbols.count(baseName)) {
          TokaSymbol &sym = m_Symbols[baseName];
          llvm::Value *val = m_Builder.CreateLoad(m_Builder.getPtrTy(), alloca,
                                                  v->Name + ".move");
          // Move Semantics: Null out the source (Destructive Move)
          m_Builder.CreateStore(
              llvm::ConstantPointerNull::get(m_Builder.getPtrTy()), alloca);
          return val;
        }
      }
    }
    return genExpr(unary->RHS.get());
  }
  if (unary->Op == TokenType::Tilde) {
    if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      llvm::Value *alloca = m_NamedValues[v->Name];
      if (alloca) {
        return m_Builder.CreateLoad(m_ValueTypes[v->Name], alloca,
                                    v->Name + "_shared");
      }
    }
    return genExpr(unary->RHS.get());
  }

  llvm::Value *rhs = genExpr(unary->RHS.get());
  if (!rhs)
    return nullptr;
  if (unary->Op == TokenType::Bang) {
    return m_Builder.CreateNot(rhs, "nottmp");
  } else if (unary->Op == TokenType::Minus) {
    if (rhs->getType()->isFloatingPointTy())
      return m_Builder.CreateFNeg(rhs, "negtmp");
    return m_Builder.CreateNeg(rhs, "negtmp");
  }
  return nullptr;
}

llvm::Value *CodeGen::genCastExpr(const CastExpr *cast) {
  llvm::Value *val = genExpr(cast->Expression.get());
  if (!val)
    return nullptr;
  llvm::Type *targetType = resolveType(cast->TargetType, false);
  if (!targetType)
    return val;

  llvm::Type *srcType = val->getType();
  if (srcType->isIntegerTy() && targetType->isIntegerTy())
    return m_Builder.CreateIntCast(val, targetType, true);

  // Physical Interpretation: bitcast or int-ptr cast if types are different
  if (srcType != targetType) {
    if (srcType->isPointerTy() && targetType->isPointerTy()) {
      return m_Builder.CreateBitCast(val, targetType);
    }
    if (srcType->isPointerTy() && targetType->isIntegerTy()) {
      return m_Builder.CreatePtrToInt(val, targetType);
    }
    if (srcType->isIntegerTy() && targetType->isPointerTy()) {
      return m_Builder.CreateIntToPtr(val, targetType);
    }
    // If one is not a pointer, we need alloca/bitcast (Zero-cost
    // GEP/Address logic)
    llvm::Value *tmp = m_Builder.CreateAlloca(srcType);
    m_Builder.CreateStore(val, tmp);
    llvm::Value *castPtr =
        m_Builder.CreateBitCast(tmp, llvm::PointerType::getUnqual(targetType));
    return m_Builder.CreateLoad(targetType, castPtr);
  }
  return val;
}

llvm::Value *CodeGen::genVariableExpr(const VariableExpr *var) {
  llvm::Value *soulAddr = getEntityAddr(var->Name);
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

  if (!soulType) {
    // Last resort for opaque pointers if we really don't know the type
    // (though soulType should be set for all Toka-defined variables)
    soulType = m_Builder.getPtrTy();
  }

  return m_Builder.CreateLoad(soulType, soulAddr, var->Name);
}

llvm::Value *CodeGen::genLiteralExpr(const Expr *expr) {
  if (auto *num = dynamic_cast<const NumberExpr *>(expr)) {
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

llvm::Value *CodeGen::genMatchExpr(const MatchExpr *expr) {
  llvm::Value *targetVal = genExpr(expr->Target.get());
  if (!targetVal)
    return nullptr;

  llvm::Type *targetType = targetVal->getType();
  std::string shapeName;
  if (targetType->isStructTy() && m_TypeToName.count(targetType)) {
    shapeName = m_TypeToName[targetType];
  }

  // Create result alloca (Assume i32 for now, ideally get from Sema or expr)
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
          // Payload is often modeled as bytes, cast it
          llvm::Type *payloadType = resolveType(variant->Type, false);
          if (payloadType) {
            llvm::Value *castAddr = m_Builder.CreateBitCast(
                payloadAddr, llvm::PointerType::getUnqual(payloadType));
            // Bind subpatterns
            for (size_t i = 0; i < arm->Pat->SubPatterns.size(); ++i) {
              // If reference, pass address directly, not value.
              // CodeGen::genPatternBinding handles 'IsReference' logic but
              // needs correct address. castAddr IS the address of payload.
              // For multi-field payloads, we would GEP. For now assume single
              // payload if not tuple.
              genPatternBinding(arm->Pat->SubPatterns[i].get(), castAddr,
                                payloadType);
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
    // General pattern matching (Sequence of Ifs) - simplified
    for (const auto &arm : expr->Arms) {
      // TODO: Implement general pattern matching logic
    }
    m_Builder.CreateBr(mergeBB);
  }

  // Check if mergeBB is reachable (i.e. has predecessors)
  // If use_empty() is true, it means no branches jump to it, so it's dead
  // code.
  if (mergeBB->use_empty()) {
    mergeBB->eraseFromParent(); // Remove dead block
    // Return dummy value since we can't be here at runtime
    // But we need to return something valid for the caller to not crash if it
    // uses the value type. However, if we removed the block, we have no
    // insert point? Actually if we return nullptr, genStmt might handle it?
    // genExpr must return a Value*. Use Undef.
    return llvm::UndefValue::get(resultType);
  }

  m_Builder.SetInsertPoint(mergeBB);
  return m_Builder.CreateLoad(resultType, resultAddr, "match_result");
}

llvm::Value *CodeGen::genIfExpr(const IfExpr *ie) {
  // Track result via alloca if this if yields a value (determined by
  // PassExpr)
  llvm::AllocaInst *resultAddr =
      m_Builder.CreateAlloca(m_Builder.getInt32Ty(), nullptr, "if_result_addr");
  // Initialize with 0 or some default
  m_Builder.CreateStore(m_Builder.getInt32(0), resultAddr);

  llvm::Value *cond = genExpr(ie->Condition.get());
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

llvm::Value *CodeGen::genWhileExpr(const WhileExpr *we) {
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
  llvm::Value *cond = genExpr(we->Condition.get());
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

llvm::Value *CodeGen::genLoopExpr(const LoopExpr *le) {
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

llvm::Value *CodeGen::genForExpr(const ForExpr *fe) {
  llvm::Value *collVal = genExpr(fe->Collection.get());
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
  if (collVal->getType()->isArrayTy()) {
    limit = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context),
                                   collVal->getType()->getArrayNumElements());
  } else {
    // TODO: handle dynamic slices/vectors
    limit = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 10);
  }
  llvm::Value *cond = m_Builder.CreateICmpULT(currIdx, limit, "forcond");
  m_Builder.CreateCondBr(cond, loopBB, elseBB);

  loopBB->insertInto(f);
  m_Builder.SetInsertPoint(loopBB);

  // Define loop variable
  m_ScopeStack.push_back({});
  llvm::Type *elemTy = collVal->getType()->isArrayTy()
                           ? collVal->getType()->getArrayElementType()
                           : llvm::Type::getInt32Ty(m_Context);
  llvm::Value *elemPtr = nullptr;
  if (collVal->getType()->isPointerTy()) {
    elemPtr = m_Builder.CreateGEP(elemTy, collVal, {currIdx});
  } else {
    llvm::Value *allocaColl = m_Builder.CreateAlloca(collVal->getType());
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

  llvm::Value *elem = m_Builder.CreateLoad(elemTy, elemPtr, vName);
  llvm::AllocaInst *vAlloca =
      m_Builder.CreateAlloca(elem->getType(), nullptr, vName);
  m_Builder.CreateStore(elem, vAlloca);
  m_NamedValues[vName] = vAlloca;
  m_ValueTypes[vName] = elem->getType();
  m_ValueElementTypes[vName] = elemTy; // Store elem type for reference

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
    m_ValueTypes[pName] = allocaType;
    m_ValueElementTypes[pName] = targetType;
    m_ValueIsReference[pName] = pat->IsReference;
    // Patterns are immutable by default unless 'mut'
    m_ValueIsMutable[pName] = pat->IsMutable;
    m_ValueIsUnique[pName] =
        false; // IsUnique needs pat support? Assume false for now or infer
    m_ValueIsShared[pName] = false;

    TokaSymbol sym;
    sym.allocaPtr = alloca;
    // For pattern bindings, metadata is often already inferred by Sema
    fillSymbolMetadata(sym, "", false, false, false, pat->IsReference,
                       pat->IsMutable, false, targetType);
    sym.isRebindable = false;
    sym.isContinuous = targetType->isArrayTy();
    m_Symbols[pName] = sym;

    if (!m_ScopeStack.empty()) {
      m_ScopeStack.back().push_back({pName, alloca, false, false});
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

llvm::Value *CodeGen::genCallExpr(const CallExpr *call) {
  // Primitives as constructors: i32(42)
  if (call->Callee == "i32" || call->Callee == "u32" || call->Callee == "i64" ||
      call->Callee == "u64" || call->Callee == "f32" || call->Callee == "f64" ||
      call->Callee == "i16" || call->Callee == "u16" || call->Callee == "i8" ||
      call->Callee == "u8" || call->Callee == "usize" ||
      call->Callee == "isize" || call->Callee == "bool") {
    llvm::Type *targetTy = resolveType(call->Callee, false);
    if (call->Args.empty())
      return llvm::Constant::getNullValue(targetTy);
    llvm::Value *val = genExpr(call->Args[0].get());
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
  if (m_Shapes.count(call->Callee)) {
    const ShapeDecl *sh = m_Shapes[call->Callee];
    if (sh->Kind == ShapeKind::Struct || sh->Kind == ShapeKind::Tuple) {
      llvm::StructType *st = m_StructTypes[call->Callee];
      llvm::Value *alloca =
          m_Builder.CreateAlloca(st, nullptr, call->Callee + "_ctor");

      size_t argIdx = 0;
      for (const auto &arg : call->Args) {
        std::string fieldName;
        const Expr *valExpr = arg.get();
        bool isNamed = false;

        // Check for named arg (x = val)
        if (auto *bin = dynamic_cast<const BinaryExpr *>(arg.get())) {
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
        if (isNamed) {
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
          llvm::Value *val = genExpr(valExpr);
          if (!val)
            return nullptr;

          // Auto-cast if needed (e.g. integer promotion) - minimal support
          llvm::Type *destTy = st->getElementType(memberIdx);
          if (val->getType() != destTy) {
            if (val->getType()->isIntegerTy() && destTy->isIntegerTy()) {
              val = m_Builder.CreateIntCast(val, destTy, true);
            }
          }

          llvm::Value *ptr = m_Builder.CreateStructGEP(st, alloca, memberIdx);
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
        llvm::Value *val = genExpr(call->Args[argIndex].get());
        if (val) {
          llvm::Type *ty = val->getType();
          std::string spec = "";
          llvm::Value *pVal = val;

          if (ty->isIntegerTy(1)) { // bool
            llvm::Value *trueStr = m_Builder.CreateGlobalStringPtr("true");
            llvm::Value *falseStr = m_Builder.CreateGlobalStringPtr("false");
            pVal = m_Builder.CreateSelect(val, trueStr, falseStr);
            spec = "%s";
          } else if (ty->isIntegerTy(64)) {
            spec = "%lld";
          } else if (ty->isIntegerTy()) {
            spec = "%d";
          } else if (ty->isDoubleTy()) {
            spec = "%f";
          } else if (ty->isFloatTy()) {
            spec = "%f";
            pVal = m_Builder.CreateFPExt(val, m_Builder.getDoubleTy());
          } else if (ty->isPointerTy()) {
            // Enhanced pointer detection for println
            std::string semanticType = "";
            const Expr *argExpr = call->Args[argIndex].get();

            if (auto *ve = dynamic_cast<const VariableExpr *>(argExpr)) {
              semanticType = m_ValueTypeNames[ve->Name];
            } else if (auto *ce = dynamic_cast<const CallExpr *>(argExpr)) {
              if (m_Functions.count(ce->Callee))
                semanticType = m_Functions[ce->Callee]->ReturnType;
              else if (m_Externs.count(ce->Callee))
                semanticType = m_Externs[ce->Callee]->ReturnType;
            } else if (dynamic_cast<const StringExpr *>(argExpr)) {
              semanticType = "str";
            }

            if (semanticType == "str" || semanticType == "*i8" ||
                semanticType == "^i8") {
              spec = "%s";
            } else {
              spec = "%p"; // Default to address
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
  if (calleeName.size() > 5 && calleeName.substr(0, 5) == "libc_") {
    calleeName = calleeName.substr(5);
  }
  llvm::Function *callee = m_Module->getFunction(calleeName);
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
        for (auto &arg : call->Args) {
          args.push_back(genExpr(arg.get()));
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
            args.push_back(genExpr(argExpr.get()));
          }
          if (!args.empty() && !args.back())
            return nullptr;

          llvm::StructType *st = m_StructTypes[optName];
          llvm::Value *alloca = m_Builder.CreateAlloca(st, nullptr, "opt_ctor");
          llvm::Value *tagAddr =
              m_Builder.CreateStructGEP(st, alloca, 0, "tag_addr");
          m_Builder.CreateStore(
              llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), tag),
              tagAddr);

          if (targetVar && !targetVar->Type.empty()) {
            llvm::Value *payloadAddr =
                m_Builder.CreateStructGEP(st, alloca, 1, "payload_addr");
            llvm::Type *payloadType = resolveType(targetVar->Type, false);
            if (payloadType) {
              llvm::Value *castPtr = m_Builder.CreateBitCast(
                  payloadAddr, llvm::PointerType::getUnqual(payloadType));
              if (args.size() == 1) {
                m_Builder.CreateStore(args[0], castPtr);
              } else if (payloadType->isStructTy()) {
                for (size_t i = 0;
                     i < args.size() && i < payloadType->getStructNumElements();
                     ++i) {
                  llvm::Value *fPtr =
                      m_Builder.CreateStructGEP(payloadType, castPtr, i);
                  m_Builder.CreateStore(args[i], fPtr);
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
      llvm::Type *logicalTy = resolveType(funcDecl->Args[i].Type, false);
      if (logicalTy && (logicalTy->isStructTy() || logicalTy->isArrayTy()))
        isCaptured = true;
    } else if (extDecl && i < extDecl->Args.size()) {
      llvm::Type *logicalTy = resolveType(extDecl->Args[i].Type, false);
      if (logicalTy && (logicalTy->isStructTy() || logicalTy->isArrayTy()))
        isCaptured = true;
    }

    if (isCaptured || isRef) {
      shouldPassAddr = true;
    }

    if (shouldPassAddr) {
      if (dynamic_cast<const AddressOfExpr *>(call->Args[i].get())) {
        val = genExpr(call->Args[i].get());
      } else {
        val = genAddr(call->Args[i].get());
      }
    } else {
      val = genExpr(call->Args[i].get());
    }

    if (!val) {
      error(call, "Failed to generate argument " + std::to_string(i) + " for " +
                      call->Callee);
      return nullptr;
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
        if (auto *ve = dynamic_cast<const VariableExpr *>(argExpr)) {
          if (m_ValueElementTypes.count(ve->Name)) {
            llvm::Type *ct = m_ValueElementTypes[ve->Name];
            if (m_TypeToName.count(ct))
              concreteName = m_TypeToName[ct];
          }
        } else if (auto *ne = dynamic_cast<const NewExpr *>(argExpr)) {
          concreteName = ne->Type;
        } else if (auto *ie = dynamic_cast<const InitStructExpr *>(argExpr)) {
          concreteName = ie->ShapeName;
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

llvm::Value *CodeGen::genPostfixExpr(const PostfixExpr *post) {
  if (post->Op == TokenType::TokenWrite) {
    return genExpr(post->LHS.get());
  }

  llvm::Value *addr = genAddr(post->LHS.get());
  if (!addr)
    return nullptr;
  llvm::Type *type = nullptr;
  if (auto *var = dynamic_cast<const VariableExpr *>(post->LHS.get())) {
    type = m_ValueElementTypes[var->Name];
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

llvm::Value *CodeGen::genPassExpr(const PassExpr *pe) {
  llvm::Value *val = nullptr;
  if (pe->Value) {
    val = genExpr(pe->Value.get());
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

llvm::Value *CodeGen::genBreakExpr(const BreakExpr *be) {
  llvm::Value *val = nullptr;
  if (be->Value)
    val = genExpr(be->Value.get());

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

llvm::Value *CodeGen::genContinueExpr(const ContinueExpr *ce) {
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

llvm::Value *CodeGen::genUnsafeExpr(const UnsafeExpr *ue) {
  return genExpr(ue->Expression.get());
}

llvm::Value *CodeGen::genInitStructExpr(const InitStructExpr *init) {
  llvm::StructType *st = m_StructTypes[init->ShapeName];
  if (!st) {
    error(init, "Unknown struct type " + init->ShapeName);
    return nullptr;
  }

  llvm::Value *alloca =
      m_Builder.CreateAlloca(st, nullptr, init->ShapeName + "_init");
  auto &fields = m_StructFieldNames[init->ShapeName];

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
    }
    if (idx == -1) {
      error(init, "Unknown field " + f.first);
      return nullptr;
    }

    llvm::Value *fieldVal = genExpr(f.second.get());
    if (!fieldVal)
      return nullptr;

    llvm::Value *fieldAddr =
        m_Builder.CreateStructGEP(st, alloca, idx, "field_" + f.first);
    m_Builder.CreateStore(fieldVal, fieldAddr);
  }

  return m_Builder.CreateLoad(st, alloca);
}

llvm::Value *CodeGen::genNewExpr(const NewExpr *newExpr) {
  llvm::Type *type = resolveType(newExpr->Type, false);
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
    llvm::Value *initVal = genExpr(newExpr->Initializer.get());
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

llvm::Value *CodeGen::genTupleExpr(const TupleExpr *expr) {
  std::vector<llvm::Constant *> consts;
  std::vector<llvm::Value *> values;
  bool allConst = true;

  for (auto &e : expr->Elements) {
    llvm::Value *v = genExpr(e.get());
    if (!v)
      return nullptr;
    values.push_back(v);
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

llvm::Value *CodeGen::genArrayExpr(const ArrayExpr *expr) {
  if (expr->Elements.empty())
    return nullptr;

  std::vector<llvm::Constant *> consts;
  std::vector<llvm::Value *> values;
  bool allConst = true;

  for (auto &e : expr->Elements) {
    llvm::Value *v = genExpr(e.get());
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

  if (allConst) {
    return llvm::ConstantArray::get(arrTy, consts);
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
  return val;
}

} // namespace toka