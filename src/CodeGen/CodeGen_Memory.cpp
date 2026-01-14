#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>
#include <typeinfo>

namespace toka {

PhysEntity CodeGen::genAllocExpr(const AllocExpr *ae) {
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
    llvm::Value *count = genExpr(ae->ArraySize.get()).load(m_Builder);
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
    llvm::Value *initVal = genExpr(ae->Initializer.get()).load(m_Builder);
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
  llvm::Value *ptrAddr = nullptr;
  if (auto *unary = dynamic_cast<const UnaryExpr *>(fs->Expression.get())) {
    // TokenType::Star is explicitly the Soul (Pointer Value), so we let it fall
    // through to genAddr which correctly resolves to the Soul address (Heap
    // Address).
  }

  if (!ptrAddr)
    ptrAddr = genAddr(fs->Expression.get());

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
      std::string varName = stripMorphology(ve->Name);

      // Debug Lookup
      // llvm::errs() << "DEBUG: genFreeStmt lookup varName='" << varName
      //              << "' count=" << m_ValueTypeNames.count(varName) << "
      //              val='"
      //              << m_ValueTypeNames[varName] << "'\n";

      if (m_ValueTypeNames.count(varName)) {
        std::string vType = m_ValueTypeNames[varName];
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
      std::string try1 = "encap_" + typeName + "_drop";
      std::string try2 = typeName + "_drop"; // Legacy
      if (m_Functions.count(try1))
        dropFunc = try1;
      else if (m_Functions.count(try2))
        dropFunc = try2;
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
        // We need T* (Data Pointer) to access members.

        // Check if objAddr is already unwrapped.
        // If emitEntityAddr returned a LoadInst from the symbol's alloca,
        // it effectively loaded the first element (Data*) of the wrapper.
        bool alreadyUnwrapped = false;
        if (auto *li = llvm::dyn_cast<llvm::LoadInst>(objAddr)) {
          if (li->getPointerOperand() == sym.allocaPtr) {
            alreadyUnwrapped = true;
          }
        }

        if (!alreadyUnwrapped) {
          // Unwrap: Load T* from { T*, RC* }
          // objAddr is the address of the wrapper.
          // Since Data* is at offset 0, we can just load from objAddr.
          objAddr = m_Builder.CreateLoad(m_Builder.getPtrTy(), objAddr,
                                         "sh_unwrap_load");
        }
      }
    }
  }

  llvm::Type *objType = nullptr;
  if (auto *ptrTy = llvm::dyn_cast<llvm::PointerType>(objAddr->getType())) {
    if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(objAddr)) {
      objType = alloca->getAllocatedType();
    } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objAddr)) {
      objType = gep->getResultElementType();
    } else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(objAddr)) {
      objType = load->getType();
    } else {
      // Fallback: try element type discovery
      if (auto *ve = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
        std::string baseName = ve->Name;
        while (!baseName.empty() &&
               (baseName[0] == '*' || baseName[0] == '&' || baseName[0] == '#'))
          baseName = baseName.substr(1);
        if (m_ValueElementTypes.count(baseName))
          objType = m_ValueElementTypes[baseName];
      }
    }
  }

  int idx = -1;
  llvm::StructType *st = nullptr;
  if (objType && objType->isStructTy()) {
    st = llvm::cast<llvm::StructType>(objType);
  }

  std::string memberName = mem->Member;
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

  // Try to find index in st if still -1
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

  if (idx == -1)
    return nullptr;

  llvm::Value *fieldAddr =
      m_Builder.CreateStructGEP(st, objAddr, idx, memberName);

  llvm::Value *finalAddr = fieldAddr;
  bool isPointerField = st->getElementType(idx)->isPointerTy();

  if (!mem->Member.empty() && mem->Member[0] == '*') {
    finalAddr = fieldAddr;
  }

  // Resolve Metadata
  std::string memberTypeName = "";
  llvm::Type *irTy = st->getElementType(idx); // Base type from struct def

  std::string stName = m_TypeToName[st];
  if (stName.empty()) { // Reverse lookup fallback
    for (const auto &pair : m_StructTypes) {
      if (pair.second == st) {
        stName = pair.first;
        break;
      }
    }
  }
  if (!stName.empty() && m_Shapes.count(stName)) {
    const ShapeDecl *sh = m_Shapes[stName];
    // Need correct index relative to Shape Members (usually matches)
    if (idx < (int)sh->Members.size()) {
      memberTypeName = sh->Members[idx].Type;
    }
  }

  return PhysEntity(finalAddr, memberTypeName, irTy, true);
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
    if (unary->Op == TokenType::Star) {
      // *p -> The Soul address of p.
      // genAddr(p) already peels the onion to return the Soul address.
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

          // For opaque pointers, we need to know the stride.
          // Logic: Pointers are usually byte-addressed buffers (char*, u8*) or
          // specific types. HACK: Default to i8 for *T indexing if we can't
          // deduce T. In Toka std, *char is the most common case for this
          // fallback.
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

      // 3. Final GEP Calculation (Single-level for pointer-based arrays)
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

  // std::cerr << "DEBUG: getEntityAddr: " << baseName << " mode=" <<
  // (int)sym.mode << " level=" << sym.indirectionLevel << "\n";

  llvm::Value *current = sym.allocaPtr; // Identity (Box)

  // 1. Direct Mode: Box is the Soul
  if (sym.mode == AddressingMode::Direct) {
    return current;
  }

  // 2. Pointer Mode: Peeling Recursive loads
  // LLVM 17 requires explicit type for loads.
  for (int i = 0; i < sym.indirectionLevel; ++i) {
    current = m_Builder.CreateLoad(m_Builder.getPtrTy(), current,
                                   baseName + ".peel_layer");
  }

  return current;
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
  return genAddr(expr);
}

llvm::Value *CodeGen::emitHandleAddr(const Expr *expr) {
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    return getIdentityAddr(var->Name);
  }
  return genAddr(expr);
}

} // namespace toka