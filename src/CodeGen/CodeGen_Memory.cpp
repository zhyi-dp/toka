#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>
#include <typeinfo>

namespace toka {

llvm::Value *CodeGen::genAllocExpr(const AllocExpr *ae) {
  llvm::Function *allocHook = m_Module->getFunction("__toka_alloc");
  if (!allocHook) {
    // Declare it if not present
    llvm::Type *sizeTy = llvm::Type::getInt64Ty(m_Context);
    llvm::Type *retTy =
        llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
    llvm::FunctionType *ft = llvm::FunctionType::get(retTy, {sizeTy}, false);
    allocHook = llvm::Function::Create(ft, llvm::Function::ExternalLinkage,
                                       "__toka_alloc", m_Module.get());
  }

  llvm::Type *elemTy = resolveType(ae->TypeName, false);
  llvm::DataLayout dl(m_Module.get());
  uint64_t size = dl.getTypeAllocSize(elemTy);
  llvm::Value *sizeVal =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), size);

  if (ae->IsArray && ae->ArraySize) {
    llvm::Value *count = genExpr(ae->ArraySize.get());
    count = m_Builder.CreateIntCast(count, llvm::Type::getInt64Ty(m_Context),
                                    false);
    sizeVal = m_Builder.CreateMul(sizeVal, count);
  }

  llvm::Value *rawPtr = m_Builder.CreateCall(allocHook, sizeVal);
  llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
  llvm::Value *castedPtr = m_Builder.CreateBitCast(rawPtr, ptrTy);

  if (ae->Initializer) {
    // For now ignore initializer logic for malloc or handle struct init
    // We assume InitStructExpr is handled separately but if we have new
    // Struct { ... } We need to store it to malloc'd memory
    llvm::Value *initVal = genExpr(ae->Initializer.get());
    m_Builder.CreateStore(initVal, castedPtr);
  }
  return castedPtr;
}

llvm::Value *CodeGen::genFreeStmt(const FreeStmt *fs) {
  llvm::Function *freeHook = m_Module->getFunction("__toka_free");
  if (!freeHook)
    std::cerr << "DEBUG: __toka_free NOT FOUND in module\n";
  else
    std::cerr << "DEBUG: __toka_free FOUND: " << freeHook->getName().str()
              << "\n";
  llvm::Value *ptrAddr = genAddr(fs->Expression.get());
  if (!ptrAddr)
    std::cerr << "DEBUG: genAddr returned NULL for free expression\n";
  else
    std::cerr << "DEBUG: genAddr returned value for free expression\n";
  if (freeHook && ptrAddr) {
    std::cerr << "DEBUG: Generating call to __toka_free\n";
    llvm::Value *casted =
        m_Builder.CreateBitCast(ptrAddr, m_Builder.getPtrTy());
    m_Builder.CreateCall(freeHook, casted);
  }
  return nullptr;
}

llvm::Value *CodeGen::genMemberExpr(const MemberExpr *mem) {
  llvm::Value *addr = genAddr(mem);
  if (!addr)
    return nullptr;

  // Resolve element type for Load.
  llvm::Type *resultTy = nullptr;
  if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(addr)) {
    resultTy = gep->getResultElementType();
  } else if (addr->getType()->isPointerTy()) {
    // For Soul access (where genAddr returns the pointer value),
    // the load should yield the pointed-to element (e.g. i8).
    resultTy = llvm::Type::getInt8Ty(m_Context);
  }

  if (!resultTy)
    return addr;

  return m_Builder.CreateLoad(resultTy, addr, mem->Member);
}

llvm::Value *CodeGen::genIndexExpr(const ArrayIndexExpr *idxExpr) {
  // Check for Array Shape Initialization
  if (auto *var = dynamic_cast<const VariableExpr *>(idxExpr->Array.get())) {
    if (m_Shapes.count(var->Name)) {
      const ShapeDecl *sh = m_Shapes[var->Name];
      if (sh->Kind == ShapeKind::Array) {
        llvm::StructType *st = m_StructTypes[var->Name];
        llvm::Value *alloca =
            m_Builder.CreateAlloca(st, nullptr, var->Name + "_init");

        for (size_t i = 0; i < idxExpr->Indices.size(); ++i) {
          llvm::Value *val = genExpr(idxExpr->Indices[i].get());
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
    if (unary->Op == TokenType::Star || unary->Op == TokenType::Ampersand) {
      if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
        return getIdentityAddr(v->Name); // Identity
      }
      if (auto *mem = dynamic_cast<const MemberExpr *>(unary->RHS.get())) {
        // We want the field address (Identity target).
        // For raw pointers, genAddr(mem) already returns the field address
        // if it's NOT a pointer field, and it returns Slot A if it IS a
        // pointer field. Wait, for Identity (*buf), we ALWAYS want the slot
        // address. I have logic in genAddr(MemberExpr) for this if member
        // starts with '*'. Temporary hack: use a modified member name without
        // copying the whole object.
        std::string originalMember = mem->Member;
        const_cast<MemberExpr *>(mem)->Member = "*" + originalMember;
        llvm::Value *res = genAddr(mem);
        const_cast<MemberExpr *>(mem)->Member = originalMember;
        return res;
      }
      return genAddr(unary->RHS.get());
    } else if (unary->Op == TokenType::PlusPlus ||
               unary->Op == TokenType::MinusMinus) {
      // handled in genExpr? No, getting address of post increment result?
      // Usually invalid lvalue unless reference.
    }
  }

  if (auto *idxExpr = dynamic_cast<const ArrayIndexExpr *>(expr)) {
    if (idxExpr->Indices.empty())
      return nullptr;

    llvm::Value *arrAddr = emitEntityAddr(idxExpr->Array.get());
    if (!arrAddr)
      return nullptr;

    llvm::Value *index = genExpr(idxExpr->Indices[0].get());
    if (!index)
      return nullptr;

    // Get soul type for GEP
    llvm::Type *soulType = nullptr;
    if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(arrAddr)) {
      soulType = alloca->getAllocatedType();
    } else {
      soulType = arrAddr->getType();
    }

    if (soulType && soulType->isArrayTy()) {
      return m_Builder.CreateInBoundsGEP(soulType, arrAddr,
                                         {m_Builder.getInt32(0), index});
    }
    return m_Builder.CreateInBoundsGEP(soulType, arrAddr, index);
  }

  if (auto *mem = dynamic_cast<const MemberExpr *>(expr)) {
    llvm::Value *objAddr = nullptr;
    llvm::Type *objType = nullptr;

    objAddr = emitEntityAddr(mem->Object.get());

    if (auto *objVar = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
      std::string baseName = objVar->Name;
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
      if (m_Symbols.count(baseName)) {
        objType = m_Symbols[baseName].llvmType;
      } else {
        objType = m_ValueElementTypes[baseName];
      }
    } else {
      if (objAddr) {
        if (auto *ptrTy =
                llvm::dyn_cast<llvm::PointerType>(objAddr->getType())) {
          if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objAddr)) {
            objType = gep->getResultElementType();
          } else if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(objAddr)) {
            objType = alloca->getAllocatedType();
          } else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(objAddr)) {
            objType = load->getType();
          }
        }
      }
    }

    if (!objAddr)
      return nullptr;

    int idx = -1;
    llvm::StructType *st = nullptr;
    if (objType && objType->isStructTy()) {
      st = llvm::cast<llvm::StructType>(objType);
    }

    std::string memberName = mem->Member;
    while (!memberName.empty() &&
           (memberName[0] == '^' || memberName[0] == '*' ||
            memberName[0] == '&' || memberName[0] == '#' ||
            memberName[0] == '~' || memberName[0] == '!')) {
      memberName = memberName.substr(1);
    }
    while (!memberName.empty() &&
           (memberName.back() == '#' || memberName.back() == '?' ||
            memberName.back() == '!')) {
      memberName.pop_back();
    }

    if (!st) {
      std::string foundStruct;
      for (const auto &pair : m_StructFieldNames) {
        for (int i = 0; i < (int)pair.second.size(); ++i) {
          std::string fn = pair.second[i];
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
            foundStruct = pair.first;
            idx = i;
            break;
          }
        }
        if (!foundStruct.empty())
          break;
      }
      if (!foundStruct.empty()) {
        st = m_StructTypes[foundStruct];
      }
    }

    if (!st) {
      return nullptr;
    }
    if (idx == -1) {
      std::string stName = m_TypeToName[st];
      if (stName.empty()) {
        for (const auto &pair : m_StructTypes) {
          if (pair.second == st) {
            stName = pair.first;
            break;
          }
        }
      }
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
            idx = i;
            break;
          }
        }
      }
    }

    if (idx == -1)
      return nullptr;

    llvm::Value *fieldAddr =
        m_Builder.CreateStructGEP(st, objAddr, idx, memberName);

    // Morphology handling:
    // Identity (*buf): return the address of the pointer field (GEP).
    if (!mem->Member.empty() && mem->Member[0] == '*') {
      return fieldAddr;
    }

    // Soul (buf): if field is a pointer, return the pointer value (Slot A).
    if (st->getElementType(idx)->isPointerTy()) {
      return m_Builder.CreateLoad(st->getElementType(idx), fieldAddr,
                                  memberName + "_ptr");
    }

    return fieldAddr;
  }

  if (auto *post = dynamic_cast<const PostfixExpr *>(expr)) {
    if (post->Op == TokenType::TokenWrite) {
      // ptr# address is same as ptr address (Entity)
      return genAddr(post->LHS.get());
    }
  }

  return nullptr;
}

llvm::Value *CodeGen::getEntityAddr(const std::string &name) {
  std::string baseName = name;
  while (!baseName.empty() &&
         (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
          baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!'))
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
    return nullptr;
  }

  TokaSymbol &sym = it->second;
  if (sym.isImplicitPtr || sym.isExplicitPtr) {
    // Soul access: load the data pointer from the stack slot (Identity)
    m_Builder.CreateIntrinsic(llvm::Intrinsic::donothing, {}, {}); // IR Marker
    return m_Builder.CreateLoad(m_Builder.getPtrTy(), sym.allocaPtr,
                                baseName + ".soul");
  }
  // Regular variable: the alloca is the data address
  return sym.allocaPtr;
}

llvm::Value *CodeGen::getIdentityAddr(const std::string &name) {
  std::string baseName = name;
  while (!baseName.empty() &&
         (baseName[0] == '*' || baseName[0] == '#' || baseName[0] == '&' ||
          baseName[0] == '^' || baseName[0] == '~' || baseName[0] == '!'))
    baseName = baseName.substr(1);
  while (!baseName.empty() &&
         (baseName.back() == '#' || baseName.back() == '?' ||
          baseName.back() == '!'))
    baseName.pop_back();

  auto it = m_Symbols.find(baseName);
  if (it == m_Symbols.end())
    return nullptr;

  return it->second.allocaPtr;
}

llvm::Value *CodeGen::emitEntityAddr(const Expr *expr) {
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    return getEntityAddr(var->Name);
  }
  return genAddr(expr);
}

llvm::Value *CodeGen::emitHandleAddr(const Expr *expr) {
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    return getIdentityAddr(var->Name);
  }
  return genAddr(expr);
}


} // namespace toka