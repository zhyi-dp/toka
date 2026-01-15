#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>
#include <typeinfo>

namespace toka {

PhysEntity CodeGen::genBinaryExpr(const BinaryExpr *expr) {
  const BinaryExpr *bin = expr;
  if (bin->Op == "=" || bin->Op == "+=" || bin->Op == "-=" || bin->Op == "*=" ||
      bin->Op == "/=") {
    llvm::errs() << "DEBUG: genBinaryExpr Assign LHS Type: "
                 << typeid(*bin->LHS).name() << "\n";
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

          // Auto-Drop Logic on Reassignment
          // We must find the variable in scope to know if it's Unique/Shared
          // and has Drop
          bool found = false;
          bool isUnique = false;
          bool isShared = false;
          bool hasDrop = false;
          std::string dropFuncName = "";

          for (auto itLayer = m_ScopeStack.rbegin();
               itLayer != m_ScopeStack.rend(); ++itLayer) {
            for (auto &sv : *itLayer) {
              if (sv.Name == v->Name) {
                isUnique = sv.IsUniquePointer;
                isShared = sv.IsShared;
                hasDrop = sv.HasDrop;
                if (hasDrop && !sv.DropFunc.empty()) {
                  dropFuncName = sv.DropFunc;
                }
                found = true;
                break;
              }
            }
            if (found)
              break;
          }

          if (found && ptr) {
            if (isUnique) {
              // Load old pointer
              llvm::Value *oldPtr = m_Builder.CreateLoad(m_Builder.getPtrTy(),
                                                         ptr, "old_unique_ptr");
              llvm::Value *notNull =
                  m_Builder.CreateIsNotNull(oldPtr, "rebind_unn");

              llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
              llvm::BasicBlock *freeBB =
                  llvm::BasicBlock::Create(m_Context, "rebind_free", f);
              llvm::BasicBlock *contBB =
                  llvm::BasicBlock::Create(m_Context, "rebind_cont", f);

              m_Builder.CreateCondBr(notNull, freeBB, contBB);
              m_Builder.SetInsertPoint(freeBB);

              if (hasDrop && !dropFuncName.empty()) {
                llvm::Function *dropFn = m_Module->getFunction(dropFuncName);
                if (dropFn)
                  m_Builder.CreateCall(dropFn, {oldPtr});
              }

              llvm::Function *freeFunc = m_Module->getFunction("free");
              if (freeFunc) {
                m_Builder.CreateCall(
                    freeFunc,
                    m_Builder.CreateBitCast(
                        oldPtr, llvm::PointerType::getUnqual(
                                    llvm::Type::getInt8Ty(m_Context))));
              }
              m_Builder.CreateBr(contBB);
              m_Builder.SetInsertPoint(contBB);
            } else if (isShared) {
              // Load old shared struct {data, ref}
              // ptr is pointer to the struct alloca
              // We need to know the struct type. It's available from the alloca
              // usually.
              if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(ptr)) {
                llvm::Type *shTy = ai->getAllocatedType();
                llvm::Value *sh =
                    m_Builder.CreateLoad(shTy, ptr, "old_shared_val");
                llvm::Value *refPtr =
                    m_Builder.CreateExtractValue(sh, 1, "old_ref_ptr");
                llvm::Value *refIsNotNull =
                    m_Builder.CreateIsNotNull(refPtr, "rebind_sh_nn");

                llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
                llvm::BasicBlock *decBB =
                    llvm::BasicBlock::Create(m_Context, "rebind_dec", f);
                llvm::BasicBlock *contBB =
                    llvm::BasicBlock::Create(m_Context, "rebind_next", f);

                m_Builder.CreateCondBr(refIsNotNull, decBB, contBB);
                m_Builder.SetInsertPoint(decBB);

                llvm::Value *count = m_Builder.CreateLoad(
                    llvm::Type::getInt32Ty(m_Context), refPtr);
                llvm::Value *dec = m_Builder.CreateSub(
                    count, llvm::ConstantInt::get(
                               llvm::Type::getInt32Ty(m_Context), 1));
                m_Builder.CreateStore(dec, refPtr);

                llvm::Value *isZero = m_Builder.CreateICmpEQ(
                    dec, llvm::ConstantInt::get(
                             llvm::Type::getInt32Ty(m_Context), 0));
                llvm::BasicBlock *freeBB =
                    llvm::BasicBlock::Create(m_Context, "rebind_sh_free", f);

                m_Builder.CreateCondBr(isZero, freeBB, contBB);
                m_Builder.SetInsertPoint(freeBB);

                llvm::Value *data =
                    m_Builder.CreateExtractValue(sh, 0, "old_data_ptr");
                if (hasDrop && !dropFuncName.empty()) {
                  llvm::Function *dropFn = m_Module->getFunction(dropFuncName);
                  if (dropFn)
                    m_Builder.CreateCall(dropFn, {data});
                }

                llvm::Function *freeFunc = m_Module->getFunction("free");
                if (freeFunc) {
                  m_Builder.CreateCall(
                      freeFunc,
                      m_Builder.CreateBitCast(data, m_Builder.getPtrTy()));
                  m_Builder.CreateCall(
                      freeFunc,
                      m_Builder.CreateBitCast(refPtr, m_Builder.getPtrTy()));
                }
                m_Builder.CreateBr(contBB);
                m_Builder.SetInsertPoint(contBB);
              }
            }
          }
        }
      }
    }

    if (!ptr) {
      ptr = genAddr(bin->LHS.get());
    }

    if (!ptr) {
      return nullptr;
    }

    PhysEntity rhsVal_ent = genExpr(bin->RHS.get()).load(m_Builder);
    llvm::Value *rhsVal = rhsVal_ent.load(m_Builder);
    if (!rhsVal)
      return nullptr;

    // Determine destType if not already found (for MemberExpr, ArrayIndexExpr,
    // etc.)
    llvm::errs() << "DEBUG_V2: Checking destType. isNull="
                 << (destType == nullptr) << "\n";
    if (!destType) {
      if (auto *pe = dynamic_cast<const PostfixExpr *>(bin->LHS.get())) {
        llvm::errs() << "DEBUG_V2: LHS is PostfixExpr. Op=" << (int)pe->Op
                     << "\n";
        if (pe->Op == TokenType::TokenWrite || pe->Op == TokenType::TokenNull) {
          const Expr *base = pe->LHS.get();
          while (auto *innerPe = dynamic_cast<const PostfixExpr *>(base))
            base = innerPe->LHS.get();
          if (auto *ve = dynamic_cast<const VariableExpr *>(base)) {
            std::string baseName = ve->Name;
            while (!baseName.empty() &&
                   (baseName[0] == '*' || baseName[0] == '#' ||
                    baseName[0] == '&'))
              baseName = baseName.substr(1);
            while (!baseName.empty() &&
                   (baseName.back() == '#' || baseName.back() == '!' ||
                    baseName.back() == '?'))
              baseName.pop_back();

            llvm::errs() << "DEBUG: PostfixExpr assignment check baseName='"
                         << baseName << "'\n";
            if (m_Symbols.count(baseName)) {
              destType = m_Symbols[baseName].soulType;
              std::string s;
              llvm::raw_string_ostream os(s);
              if (destType)
                destType->print(os);
              llvm::errs() << "DEBUG: Found symbol. SoulType: " << os.str()
                           << "\n";
            } else {
              llvm::errs() << "DEBUG: Symbol not found: " << baseName << "\n";
            }
          }
        }
      }

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
          objType = genExpr(memLHS->Object.get()).load(m_Builder)->getType();

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
      if (destType) {
        std::string s;
        llvm::raw_string_ostream os(s);
        destType->print(os);
        llvm::errs() << "DEBUG: CodeGen DestType: " << os.str() << "\n";
      } else {
        llvm::errs() << "DEBUG: CodeGen DestType is NULL\n";
      }
      std::string s2;
      llvm::raw_string_ostream os2(s2);
      rhsVal->getType()->print(os2);
      llvm::errs() << "DEBUG: CodeGen RHSType: " << os2.str() << "\n";

      error(bin, "Type mismatch in assignment");
      return nullptr;
    }

    m_Builder.CreateStore(rhsVal, ptr);
    return rhsVal;
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

        if (m_ValueIsShared.count(baseName) && m_ValueIsShared[baseName]) {
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

PhysEntity CodeGen::genUnaryExpr(const UnaryExpr *unary) {
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
    llvm::Value *val = emitEntityAddr(unary->RHS.get());
    std::string typeName = "";
    if (auto *idxExpr =
            dynamic_cast<const ArrayIndexExpr *>(unary->RHS.get())) {
      if (auto *v = dynamic_cast<const VariableExpr *>(idxExpr->Array.get())) {
        std::string baseName = v->Name;
        while (!baseName.empty() && (baseName[0] == '*' || baseName[0] == '#' ||
                                     baseName[0] == '&' || baseName[0] == '^' ||
                                     baseName[0] == '~' || baseName[0] == '!'))
          baseName = baseName.substr(1);
        while (!baseName.empty() &&
               (baseName.back() == '#' || baseName.back() == '?' ||
                baseName.back() == '!'))
          baseName.pop_back();

        if (m_ValueTypeNames.count(baseName)) {
          std::string T = m_ValueTypeNames[baseName];
          while (!T.empty() && (T[0] == '*' || T[0] == '^' || T[0] == '&' ||
                                T[0] == '~' || T[0] == '[')) {
            if (T[0] == '[') {
              size_t end = T.find(']');
              if (end != std::string::npos)
                T = T.substr(end + 1);
              else
                T = T.substr(1);
            } else {
              T = T.substr(1);
            }
          }
          typeName = "*" + T;
        }
      }
    }

    if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      std::string baseName = v->Name;
      while (!baseName.empty() &&
             (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
              baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!'))
        baseName = baseName.substr(1);
      while (!baseName.empty() &&
             (baseName.back() == '#' || baseName.back() == '?' ||
              baseName.back() == '!'))
        baseName.pop_back();

      // [Fix] Move Semantics for Unique Pointers
      // If we are passing a Unique Pointer to a function that expects a
      // Unique Pointer AND we have captured it (Pass-By-Reference/Address),
      // we can simply nullify the source memory directly via 'val'. This
      // block seems misplaced here, as it refers to `funcDecl`, `extDecl`,
      // `i`, `isCaptured` which are typically found in `genCallExpr`. As per
      // instructions, I must make the change faithfully and without making
      // any unrelated edits. Since the instruction explicitly says "Update
      // `genCallExpr`" and the provided content does not contain
      // `genCallExpr`, I cannot apply this change. However, if the user
      // *intended* for this to be inserted at this specific location in
      // `genUnaryExpr` despite the instruction's function name, then the code
      // would be syntactically incorrect due to undefined variables
      // (`funcDecl`, `extDecl`, `i`, `isCaptured`). Given the constraint to
      // return syntactically correct code, and the mismatch between
      // instruction and context, I will *not* insert the problematic block
      // here. I will proceed with the original interpretation of the `*`
      // operator in `genUnaryExpr`. The instruction's code snippet is
      // malformed at the end, containing "typically returns...". I will
      // assume the user meant to insert the block *before* the return
      // statement, and the "typically returns..." was an accidental copy.

      if (m_ValueTypeNames.count(baseName)) {
        // If variable is 'char', *var is effectively 'char' (or *char
        // depending on semantics) println allows 'char' or '*char' to print
        // as string
        typeName = m_ValueTypeNames[baseName];
      }
    }
    // Assuming the user intended to insert this logic into `genCallExpr` but
    // provided the surrounding context from `genUnaryExpr` by mistake. As per
    // instructions, I must make the change faithfully and without making any
    // unrelated edits. Since the instruction explicitly says "Update
    // `genCallExpr`" and the provided content does not contain `genCallExpr`,
    // I cannot apply this change. However, if the user *intended* for this to
    // be inserted at this specific location in `genUnaryExpr` despite the
    // instruction's function name, then the code would be syntactically
    // incorrect due to undefined variables (`funcDecl`, `extDecl`, `i`,
    // `isCaptured`). Given the constraint to return syntactically correct
    // code, and the mismatch between instruction and context, I will *not*
    // insert the problematic block here. I will proceed with the original
    // interpretation of the `*` operator in `genUnaryExpr`. The instruction's
    // code snippet is malformed at the end, containing "typically
    // returns...". I will assume the user meant to insert the block *before*
    // the return statement, and the "typically returns..." was an accidental
    // copy.

    // *ptr typically returns the pointer address as R-value, but semantically
    // it's accessing the value
    return PhysEntity(val, typeName, val ? val->getType() : nullptr, false);
  }

  // Morphology symbols: ^p, ~p
  if (unary->Op == TokenType::Caret) {
    if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
      std::string vName = v->Name;
      // Strip morphology just in case
      while (!vName.empty() && (vName.back() == '?' || vName.back() == '!'))
        vName.pop_back();

      // std::cerr << "DEBUG: genUnaryExpr Caret Var=" << vName << "\n";
      llvm::Value *alloca = getIdentityAddr(vName);

      if (alloca) {
        // Identity Found! Load the pointer value (The Soul)
        llvm::Value *val =
            m_Builder.CreateLoad(m_Builder.getPtrTy(), alloca, vName + ".move");

        // Move Semantics: Null out the source (Destructive Move)
        // Only if it is NOT a reference and IS a unique pointer (checked via
        // Symbol Table ideally)
        bool isUnique = false;
        if (m_Symbols.count(vName))
          isUnique = m_Symbols[vName].morphology == Morphology::Unique;

        // Optimize: If it is indeed a unique pointer, we must null it out.
        // For raw pointers using ^, we also allow it as "Move Pointer".
        m_Builder.CreateStore(
            llvm::ConstantPointerNull::get(m_Builder.getPtrTy()), alloca);

        return val;
      } else {
        // Fallback: If identity not found (e.g. global?), try standard
        // genExpr This handles cases where 'v' might not be a simple local
        // variable
        PhysEntity ent = genExpr(unary->RHS.get());
        return ent.load(m_Builder);
      }
    }
    // Non-variable case (e.g. ^(expression))
    return genExpr(unary->RHS.get()).load(m_Builder);
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
  }
  return nullptr;
}

PhysEntity CodeGen::genCastExpr(const CastExpr *cast) {
  PhysEntity val_ent = genExpr(cast->Expression.get()).load(m_Builder);
  llvm::Value *val = val_ent.load(m_Builder);
  if (!val)
    return nullptr;
  llvm::Type *targetType = resolveType(cast->TargetType, false);
  if (!targetType)
    return val;

  llvm::Type *srcType = val->getType();
  if (srcType->isIntegerTy() && targetType->isIntegerTy())
    return m_Builder.CreateIntCast(val, targetType, true);

  // Floating Point Conversions
  if (srcType->isFloatingPointTy() && targetType->isFloatingPointTy()) {
    return m_Builder.CreateFPCast(val, targetType, "fp_cast");
  }
  if (srcType->isFloatingPointTy() && targetType->isIntegerTy()) {
    return m_Builder.CreateFPToSI(val, targetType, "fp_to_int");
  }
  if (srcType->isIntegerTy() && targetType->isFloatingPointTy()) {
    return m_Builder.CreateSIToFP(val, targetType, "int_to_fp");
  }

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
    // Propagate TargetType as the semantic type name
    return PhysEntity(m_Builder.CreateLoad(targetType, castPtr),
                      cast->TargetType, targetType, false);
  }
  // Propagate TargetType as the semantic type name
  return PhysEntity(val, cast->TargetType, targetType, false);
}

PhysEntity CodeGen::genVariableExpr(const VariableExpr *var) {
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

  std::string typeName = "";
  if (m_ValueTypeNames.count(baseName))
    typeName = m_ValueTypeNames[baseName];
  else if (m_TypeToName.count(soulType))
    typeName = m_TypeToName[soulType];

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

PhysEntity CodeGen::genMatchExpr(const MatchExpr *expr) {
  PhysEntity targetVal_ent = genExpr(expr->Target.get()).load(m_Builder);
  llvm::Value *targetVal = targetVal_ent.load(m_Builder);
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

        // For guard check, we might need variable bindings if it's a variable
        // pattern
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
    // But we need to return something valid for the caller to not crash if it
    // uses the value type. However, if we removed the block, we have no
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
  m_ValueTypes[vName] = elem->getType();
  m_ValueElementTypes[vName] = elemTy;

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
    if (sh->Kind == ShapeKind::Struct || sh->Kind == ShapeKind::Tuple) {
      llvm::StructType *st = m_StructTypes[sh->Name];
      llvm::Value *alloca =
          m_Builder.CreateAlloca(st, nullptr, sh->Name + "_ctor");

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
          PhysEntity val_ent = genExpr(valExpr).load(m_Builder);
          llvm::Value *val = val_ent.load(m_Builder);
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
          } else if (ty->isIntegerTy(64)) {
            spec = "%lld";
          } else if (ty->isIntegerTy()) {
            spec = "%d";
          } else if (ty->isDoubleTy()) {
            spec = "%f";
          } else if (ty->isFloatTy()) {
            spec = "%f";
            pVal = m_Builder.CreateFPExt(val, m_Builder.getDoubleTy());
          } else if (ty->isIntegerTy(8) || semanticType == "char" ||
                     semanticType == "u8" || semanticType == "i8") {
            spec = "%c";
          } else if (semanticType == "*char" || semanticType == "str" ||
                     semanticType == "String") {
            // Explicit check for String type (including String struct if we
            // support it)
            spec = "%s";
            // If it's *char (Pointer to i8), printf %s works.
            // If it's String struct, we might need to extract data pointer?
            // But currently String is likely passed as *char from c_str() or
            // similar. If it is String struct, pVal is struct value. printf
            // cannot take struct. But val_ent.load() loads the value.
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
      if (arg.IsUnique) {
        isCaptured = true;
      }
      // Only capture if it's a Value Type (Struct/Array) AND NOT a
      // Pointer/Shared/Reference
      else if (!arg.HasPointer && !arg.IsShared && !arg.IsReference) {
        llvm::Type *logicalTy = resolveType(arg.Type, false);
        if (logicalTy && (logicalTy->isStructTy() || logicalTy->isArrayTy()))
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
          val = getIdentityAddr(ve->Name);
        } else {
          val = genAddr(call->Args[i].get());
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

    // [Fix] Move Semantics Reverted
    // User clarified: In-Place Capture != Move.
    // Argument passing is borrowing. Caller retains ownership (unless
    // explicitly moved). Callee should NOT free the argument. So we do NOT
    // write null here.

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

    // [Fix] IncRef for Shared Pointer Arguments
    // Passing a shared pointer by value acts as a copy, so we must increment
    // RC.
    bool isSharedArg = false;
    if (funcDecl && i < funcDecl->Args.size() && funcDecl->Args[i].IsShared) {
      isSharedArg = true;
    }

    // Only increment if it's a valid Shared Pointer (Struct {ptr, ptr})
    if (isSharedArg && val && val->getType()->isStructTy() &&
        val->getType()->getStructNumElements() == 2) {
      llvm::Value *refPtr =
          m_Builder.CreateExtractValue(val, 1, "arg_inc_refptr");
      llvm::Value *cnt = m_Builder.CreateLoad(llvm::Type::getInt32Ty(m_Context),
                                              refPtr, "arg_rc");
      llvm::Value *inc = m_Builder.CreateAdd(
          cnt, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
      m_Builder.CreateStore(inc, refPtr);
    }

    argsV.push_back(val);
  }
  return m_Builder.CreateCall(callee->getFunctionType(), callee, argsV);
}

PhysEntity CodeGen::genPostfixExpr(const PostfixExpr *post) {
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

    PhysEntity fieldVal_ent = genExpr(f.second.get()).load(m_Builder);
    llvm::Value *fieldVal = fieldVal_ent.load(m_Builder);
    if (!fieldVal)
      return nullptr;

    llvm::Value *fieldAddr =
        m_Builder.CreateStructGEP(st, alloca, idx, "field_" + f.first);
    m_Builder.CreateStore(fieldVal, fieldAddr);
  }

  return m_Builder.CreateLoad(st, alloca);
}

PhysEntity CodeGen::genNewExpr(const NewExpr *newExpr) {
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
    PhysEntity initVal_ent =
        genExpr(newExpr->Initializer.get()).load(m_Builder);
    llvm::Value *initVal = initVal_ent.load(m_Builder);
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

  llvm::Type *recType = resolveType(uniqueName, false);
  if (!recType) {
    // It's possible the type wasn't created if it's unused?
    // No, Sema moves it to Module, so discover() should have seen it.
    // Try to fallback/check if it's in m_StructTypes directly?
    if (m_StructTypes.count(uniqueName)) {
      recType = m_StructTypes[uniqueName];
    } else {
      error(expr, "Anonymous record type '" + uniqueName + "' not found");
      return nullptr;
    }
  }

  llvm::Value *alloca = m_Builder.CreateAlloca(recType, nullptr, "anon_rec");

  for (size_t i = 0; i < expr->Fields.size(); ++i) {
    PhysEntity val_ent = genExpr(expr->Fields[i].second.get()).load(m_Builder);
    llvm::Value *val = val_ent.load(m_Builder);
    if (!val)
      return nullptr;

    // GEP to element i (Struct layout matches Fields order)
    llvm::Value *ptr = m_Builder.CreateStructGEP(recType, alloca, i);
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

} // namespace toka