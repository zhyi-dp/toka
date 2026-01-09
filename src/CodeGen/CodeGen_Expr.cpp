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
    llvm::Value *ptr = genAddr(bin->LHS.get());
    if (!ptr) {
      return nullptr;
    }

    if (auto *varLHS = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
      if (!m_ValueIsMutable[varLHS->Name]) {
        error(bin, "Cannot mutate immutable variable '" + varLHS->Name + "'");
        return nullptr;
      }
      if (!varLHS->IsMutable) {
        error(bin, "Missing '#' token for mutation of '" + varLHS->Name + "'");
        return nullptr;
      }
    }

    llvm::Value *rhsVal = genExpr(bin->RHS.get());
    if (!rhsVal)
      return nullptr;

    llvm::Type *destType = nullptr;
    if (auto *varLHS = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
      std::string baseName = varLHS->Name;
      while (!baseName.empty() &&
             (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
              baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!')) {
        baseName = baseName.substr(1);
      }
      while (!baseName.empty() &&
             (baseName.back() == '#' || baseName.back() == '?' ||
              baseName.back() == '!')) {
        baseName.pop_back();
      }

      if (m_ValueIsReference[baseName])
        destType = m_ValueElementTypes[baseName];
      else
        destType = m_ValueTypes[baseName];
    } else if (auto *memLHS =
                   dynamic_cast<const MemberExpr *>(bin->LHS.get())) {
      // Find struct type and field index to get destType
      llvm::Value *objAddr = nullptr;
      llvm::Type *objType = nullptr;

      // Helper to get base variable name from expression
      std::function<std::string(const Expr *)> getBaseName =
          [&](const Expr *e) -> std::string {
        if (auto *v = dynamic_cast<const VariableExpr *>(e)) {
          return v->Name;
        } else if (auto *p = dynamic_cast<const PostfixExpr *>(e)) {
          return getBaseName(p->LHS.get());
        } else if (auto *u = dynamic_cast<const UnaryExpr *>(e)) {
          // Handle *ptr or &ptr if needed, though usually MemberExpr object
          // is simple
          return getBaseName(u->RHS.get());
        }
        return "";
      };

      std::string baseName = getBaseName(memLHS->Object.get());

      // Clean up morphology decorators
      while (!baseName.empty() &&
             (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
              baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!')) {
        baseName = baseName.substr(1);
      }
      while (!baseName.empty() &&
             (baseName.back() == '#' || baseName.back() == '?' ||
              baseName.back() == '!')) {
        baseName.pop_back();
      }

      if (!baseName.empty() && m_ValueElementTypes.count(baseName)) {
        objType = m_ValueElementTypes[baseName];
      } else {
        objType = genExpr(memLHS->Object.get())->getType();
      }

      if (objType && objType->isStructTy()) {
        llvm::StructType *st = llvm::cast<llvm::StructType>(objType);
        std::string memberName = memLHS->Member;
        while (!memberName.empty() &&
               (memberName[0] == '^' || memberName[0] == '*' ||
                memberName[0] == '&' || memberName[0] == '#' ||
                memberName[0] == '~' || memberName[0] == '!')) {
          memberName = memberName.substr(1);
        }

        std::string stName = m_TypeToName[st];

        if (!stName.empty()) {
          auto &fields = m_StructFieldNames[stName];

          for (int i = 0; i < (int)fields.size(); ++i) {
            std::string fn = fields[i];

            while (!fn.empty() &&
                   (fn[0] == '^' || fn[0] == '*' || fn[0] == '&' ||
                    fn[0] == '#' || fn[0] == '~' || fn[0] == '!')) {
              fn = fn.substr(1);
            }
            while (!fn.empty() &&
                   (fn.back() == '#' || fn.back() == '?' || fn.back() == '!')) {
              fn.pop_back();
            }

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
    } else if (auto *unary = dynamic_cast<const UnaryExpr *>(bin->LHS.get())) {
      if (unary->Op == TokenType::Star) {
        // Dereference assignment: *ptr = val
        // Use RHS of UnaryExpr to find the element type
        std::string baseName = "";
        if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
          baseName = v->Name;
        } else if (auto *m =
                       dynamic_cast<const MemberExpr *>(unary->RHS.get())) {
          baseName = m->Member;
        }

        if (!baseName.empty()) {
          while (!baseName.empty() &&
                 (baseName[0] == '*' || baseName[0] == '#' ||
                  baseName[0] == '&' || baseName[0] == '^' ||
                  baseName[0] == '~' || baseName[0] == '!'))
            baseName = baseName.substr(1);
          while (!baseName.empty() &&
                 (baseName.back() == '#' || baseName.back() == '?' ||
                  baseName.back() == '!'))
            baseName.pop_back();

          if (m_ValueElementTypes.count(baseName)) {
            destType = m_ValueElementTypes[baseName];
          }
        }

        if (!destType) {
          // Fallback: use LLVM type of ptr if possible
          if (ptr->getType()->isPointerTy()) {
            // In LLVM 17+, we can't easily get element type from ptr type
            // So we might need to rely on the AST type if we had it.
            // For now, try to find in m_ValueTypes
            if (!baseName.empty() && m_ValueTypes.count(baseName)) {
              destType = m_ValueTypes[baseName];
            }
          }
        }
      }
    } else if (auto *post = dynamic_cast<const PostfixExpr *>(bin->LHS.get())) {
      if (post->Op == TokenType::TokenWrite) {
        if (auto *var = dynamic_cast<const VariableExpr *>(post->LHS.get())) {
          std::string baseName = var->Name;
          while (!baseName.empty() &&
                 (baseName[0] == '*' || baseName[0] == '#' ||
                  baseName[0] == '&' || baseName[0] == '^' ||
                  baseName[0] == '~' || baseName[0] == '!'))
            baseName = baseName.substr(1);
          while (!baseName.empty() &&
                 (baseName.back() == '#' || baseName.back() == '?' ||
                  baseName.back() == '!'))
            baseName.pop_back();

          if (m_ValueElementTypes.count(baseName)) {
            destType = m_ValueElementTypes[baseName];
          }
        }
      }
    }

    if (!destType) {
      error(bin, "Could not determine destination type for assignment");
      return nullptr;
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

    if (destType != rhsVal->getType()) {
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

  // Standard Arithmetic and Comparisons
  llvm::Value *lhs = genExpr(bin->LHS.get());
  if (!lhs) {
    return nullptr;
  }

  // Dead Instruction Probes: If the block was terminated during LHS
  // evaluation, we must not attempt to emit more instructions.
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
      error(bin, "Type mismatch in binary expression");
      return nullptr;
    }
  }

  // 'is' operator (Null Check)
  if (bin->Op == "is") {
    if (lhsType->isPointerTy()) {
      return m_Builder.CreateIsNotNull(lhs, "is_not_null");
    }
    return llvm::ConstantInt::getTrue(m_Context);
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
  if (bin->Op == "==") {
    if (lhs->getType() != rhs->getType()) {
      if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
        if (lhs->getType()->getIntegerBitWidth() >
            rhs->getType()->getIntegerBitWidth())
          rhs = m_Builder.CreateZExt(rhs, lhs->getType());
        else
          lhs = m_Builder.CreateZExt(lhs, rhs->getType());
      }
    }
    return m_Builder.CreateICmpEQ(lhs, rhs, "eq_tmp");
  }
  if (bin->Op == "!=")
    return m_Builder.CreateICmpNE(lhs, rhs, "ne_tmp");
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
    if (dynamic_cast<const VariableExpr *>(unary->RHS.get()) ||
        dynamic_cast<const MemberExpr *>(unary->RHS.get())) {
      return genAddr(unary->RHS.get());
    }
    return genExpr(unary->RHS.get());
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
          baseName[0] == '^' || baseName[0] == '~'))
    baseName = baseName.substr(1);
  while (!baseName.empty() &&
         (baseName.back() == '#' || baseName.back() == '?' ||
          baseName.back() == '!'))
    baseName.pop_back();

  llvm::Type *soulType = nullptr;
  if (m_Symbols.count(baseName)) {
    soulType = m_Symbols[baseName].llvmType;
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

  if (!soulType)
    return nullptr;

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
    return llvm::ConstantPointerNull::get(llvm::PointerType::get(m_Context, 0));
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

} // namespace toka