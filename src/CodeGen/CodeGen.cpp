#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>

namespace toka {

void CodeGen::generate(const Module &ast) {
  // Generate Type Aliases
  for (const auto &alias : ast.TypeAliases) {
    m_TypeAliases[alias->Name] = alias->TargetType;
  }

  // Generate Structs
  for (const auto &str : ast.Structs) {
    genStruct(str.get());
    if (hasErrors())
      return;
  }

  // Generate Externs
  for (const auto &ext : ast.Externs) {
    m_Externs[ext->Name] = ext.get();
    genExtern(ext.get());
    if (hasErrors())
      return;
  }

  // Generate Globals
  for (const auto &glob : ast.Globals) {
    genGlobal(glob.get());
    if (hasErrors())
      return;
  }

  // Generate Functions (decl phase)
  for (const auto &func : ast.Functions) {
    m_Functions[func->Name] = func.get();
  }

  // Generate Functions (body phase)
  for (const auto &func : ast.Functions) {
    genFunction(func.get());
    if (hasErrors())
      return;
  }
}

void CodeGen::print(llvm::raw_ostream &os) { m_Module->print(os, nullptr); }

void CodeGen::genGlobal(const Stmt *stmt) {
  if (auto *var = dynamic_cast<const VariableDecl *>(stmt)) {
    llvm::Type *type = resolveType(var->TypeName, var->HasPointer);

    auto *globalVar = new llvm::GlobalVariable(
        *m_Module, type, false, llvm::GlobalValue::ExternalLinkage, nullptr,
        var->Name);

    if (var->Init) {
      if (auto *num = dynamic_cast<const NumberExpr *>(var->Init.get())) {
        globalVar->setInitializer(llvm::ConstantInt::get(type, num->Value));
      } else {
        globalVar->setInitializer(llvm::ConstantInt::get(type, 0));
      }
    } else {
      globalVar->setInitializer(llvm::ConstantInt::get(type, 0));
    }

    m_NamedValues[var->Name] = globalVar;
    m_ValueTypes[var->Name] = type;
  } else {
    // We could support global destructuring here, but for now just skip or
    // error
    error(dynamic_cast<const ASTNode *>(stmt),
          "Global destructuring not yet supported");
  }
}

llvm::Function *CodeGen::genFunction(const FunctionDecl *func) {
  m_NamedValues.clear();
  m_ValueTypes.clear();
  m_ValueElementTypes.clear();
  m_ValueIsReference.clear();
  m_ValueIsMutable.clear();

  llvm::Function *f = m_Module->getFunction(func->Name);

  if (!f) {
    std::vector<llvm::Type *> argTypes;
    for (const auto &arg : func->Args) {
      llvm::Type *t = resolveType(arg.Type, arg.HasPointer || arg.IsReference);
      // Capture Passing: Structs, Arrays, Tuples or Mutable params are passed
      // as pointers
      bool isCaptured = (t->isStructTy() || t->isArrayTy() || arg.IsMutable) &&
                        !arg.IsReference && !arg.HasPointer;
      if (isCaptured)
        t = llvm::PointerType::getUnqual(t);
      argTypes.push_back(t);
    }

    llvm::Type *retType = resolveType(func->ReturnType, false);

    llvm::FunctionType *ft = llvm::FunctionType::get(retType, argTypes, false);
    f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, func->Name,
                               m_Module.get());
  }

  if (!func->Body)
    return f;

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(m_Context, "entry", f);
  m_Builder.SetInsertPoint(bb);

  m_ScopeStack.push_back({});

  size_t idx = 0;
  for (auto &arg : f->args()) {
    arg.setName(func->Args[idx].Name);
    llvm::Type *targetType =
        resolveType(func->Args[idx].Type, func->Args[idx].HasPointer);
    llvm::Type *baseType = func->Args[idx].IsReference
                               ? llvm::PointerType::getUnqual(targetType)
                               : targetType;

    llvm::Type *t =
        resolveType(func->Args[idx].Type,
                    func->Args[idx].HasPointer || func->Args[idx].IsReference);
    bool isCaptured =
        (t->isStructTy() || t->isArrayTy() || func->Args[idx].IsMutable);
    bool isPointerOrRef = func->Args[idx].IsReference ||
                          func->Args[idx].HasPointer ||
                          func->Args[idx].IsUnique || func->Args[idx].IsShared;

    if (isPointerOrRef || isCaptured) {
      // Passed as a pointer (explicit ref, pointer, or captured struct/array)
      m_NamedValues[func->Args[idx].Name] = &arg;
      m_ValueIsReference[func->Args[idx].Name] = func->Args[idx].IsReference;
    } else {
      // Primitive pass by value
      llvm::AllocaInst *alloca =
          m_Builder.CreateAlloca(baseType, nullptr, func->Args[idx].Name);
      m_Builder.CreateStore(&arg, alloca);
      m_NamedValues[func->Args[idx].Name] = alloca;
      m_ValueIsReference[func->Args[idx].Name] = false;
      m_ScopeStack.back().push_back({func->Args[idx].Name, alloca,
                                     func->Args[idx].IsUnique,
                                     func->Args[idx].IsShared});
    }
    m_ValueTypes[func->Args[idx].Name] = baseType;
    m_ValueIsMutable[func->Args[idx].Name] = func->Args[idx].IsMutable;
    m_ValueIsUnique[func->Args[idx].Name] = func->Args[idx].IsUnique;
    m_ValueIsShared[func->Args[idx].Name] = func->Args[idx].IsShared;

    // Set Element Type for indexing or member access
    if (targetType->isArrayTy()) {
      m_ValueElementTypes[func->Args[idx].Name] =
          targetType->getArrayElementType();
    } else if (func->Args[idx].Type == "str") {
      m_ValueElementTypes[func->Args[idx].Name] =
          llvm::Type::getInt8Ty(m_Context);
    } else {
      m_ValueElementTypes[func->Args[idx].Name] = targetType;
    }

    idx++;
  }

  genStmt(func->Body.get());

  if (!m_Builder.GetInsertBlock()->getTerminator()) {
    // Cleanup Arguments (Implicit Return)
    llvm::Function *freeFunc = m_Module->getFunction("free");
    if (freeFunc) {
      auto &scope = m_ScopeStack.back();
      for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
        if (it->IsUniquePointer && it->Alloca) {
          llvm::Value *val = m_Builder.CreateLoad(
              llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType(),
              it->Alloca);
          llvm::BasicBlock *curBB = m_Builder.GetInsertBlock();
          llvm::Function *func = curBB->getParent();
          llvm::BasicBlock *freeBB =
              llvm::BasicBlock::Create(m_Context, "free_block", func);
          llvm::BasicBlock *contBB =
              llvm::BasicBlock::Create(m_Context, "free_cont", func);

          llvm::Value *notNull = m_Builder.CreateIsNotNull(val, "not_null");
          m_Builder.CreateCondBr(notNull, freeBB, contBB);

          m_Builder.SetInsertPoint(freeBB);
          llvm::Value *casted = m_Builder.CreateBitCast(
              val,
              llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
          m_Builder.CreateCall(freeFunc, casted);
          m_Builder.CreateBr(contBB);

          m_Builder.SetInsertPoint(contBB);
        } else if (it->IsShared && it->Alloca) {
          // Shared Cleanup
          llvm::Value *shVal = m_Builder.CreateLoad(
              llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType(),
              it->Alloca);
          llvm::Value *refPtr =
              m_Builder.CreateExtractValue(shVal, 1, "ref_ptr");
          llvm::Value *count = m_Builder.CreateLoad(
              llvm::Type::getInt32Ty(m_Context), refPtr, "ref_count");
          llvm::Value *dec = m_Builder.CreateSub(
              count,
              llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
          m_Builder.CreateStore(dec, refPtr);

          llvm::BasicBlock *curBB = m_Builder.GetInsertBlock();
          llvm::Function *func = curBB->getParent();
          llvm::BasicBlock *freeBB =
              llvm::BasicBlock::Create(m_Context, "sh_free", func);
          llvm::BasicBlock *contBB =
              llvm::BasicBlock::Create(m_Context, "sh_cont", func);

          llvm::Value *isZero = m_Builder.CreateICmpEQ(
              dec,
              llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));
          m_Builder.CreateCondBr(isZero, freeBB, contBB);

          m_Builder.SetInsertPoint(freeBB);
          // Free Data
          llvm::Value *dataPtr =
              m_Builder.CreateExtractValue(shVal, 0, "data_ptr");
          llvm::Value *castData = m_Builder.CreateBitCast(
              dataPtr,
              llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
          m_Builder.CreateCall(freeFunc, castData);

          // Free RefCount
          llvm::Value *castRef = m_Builder.CreateBitCast(
              refPtr,
              llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
          m_Builder.CreateCall(freeFunc, castRef);

          m_Builder.CreateBr(contBB);
          m_Builder.SetInsertPoint(contBB);
        }
      }
    }

    if (func->Name == "main" || func->ReturnType == "void") {
      m_Builder.CreateRetVoid();
    }
  }

  m_ScopeStack.pop_back();

  llvm::verifyFunction(*f);
  return f;
}

llvm::Value *CodeGen::genStmt(const Stmt *stmt) {
  if (auto *ret = dynamic_cast<const ReturnStmt *>(stmt)) {
    llvm::Value *retVal = nullptr;
    if (ret->ReturnValue) {
      retVal = genExpr(ret->ReturnValue.get());
      if (auto *varExpr =
              dynamic_cast<const VariableExpr *>(ret->ReturnValue.get())) {
        if (varExpr->IsUnique) {
          if (auto *alloca = m_NamedValues[varExpr->Name]) {
            if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(alloca)) {
              m_Builder.CreateStore(
                  llvm::Constant::getNullValue(ai->getAllocatedType()), alloca);
            }
          }
        }
      }
    }

    llvm::Function *freeFunc = m_Module->getFunction("free");
    if (freeFunc) {
      for (auto scopeIt = m_ScopeStack.rbegin(); scopeIt != m_ScopeStack.rend();
           ++scopeIt) {
        for (auto it = scopeIt->rbegin(); it != scopeIt->rend(); ++it) {
          if (it->IsUniquePointer && it->Alloca) {
            llvm::Value *val = m_Builder.CreateLoad(
                llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType(),
                it->Alloca);
            llvm::BasicBlock *curBB = m_Builder.GetInsertBlock();
            llvm::Function *func = curBB->getParent();
            llvm::BasicBlock *freeBB =
                llvm::BasicBlock::Create(m_Context, "free_block", func);
            llvm::BasicBlock *contBB =
                llvm::BasicBlock::Create(m_Context, "free_cont", func);

            llvm::Value *notNull = m_Builder.CreateIsNotNull(val, "not_null");
            m_Builder.CreateCondBr(notNull, freeBB, contBB);

            m_Builder.SetInsertPoint(freeBB);
            llvm::Value *casted = m_Builder.CreateBitCast(
                val,
                llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
            m_Builder.CreateCall(freeFunc, casted);
            m_Builder.CreateBr(contBB);

            m_Builder.SetInsertPoint(contBB);
          } else if (it->IsShared && it->Alloca) {
            // Shared Cleanup
            llvm::Value *shVal = m_Builder.CreateLoad(
                llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType(),
                it->Alloca);
            llvm::Value *refPtr =
                m_Builder.CreateExtractValue(shVal, 1, "ref_ptr");
            llvm::Value *count = m_Builder.CreateLoad(
                llvm::Type::getInt32Ty(m_Context), refPtr, "ref_count");
            llvm::Value *dec = m_Builder.CreateSub(
                count,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
            m_Builder.CreateStore(dec, refPtr);

            llvm::BasicBlock *curBB = m_Builder.GetInsertBlock();
            llvm::Function *func = curBB->getParent();
            llvm::BasicBlock *freeBB =
                llvm::BasicBlock::Create(m_Context, "sh_free", func);
            llvm::BasicBlock *contBB =
                llvm::BasicBlock::Create(m_Context, "sh_cont", func);

            llvm::Value *isZero = m_Builder.CreateICmpEQ(
                dec,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));
            m_Builder.CreateCondBr(isZero, freeBB, contBB);

            m_Builder.SetInsertPoint(freeBB);
            // Free Data
            llvm::Value *dataPtr =
                m_Builder.CreateExtractValue(shVal, 0, "data_ptr");
            llvm::Value *castData = m_Builder.CreateBitCast(
                dataPtr,
                llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
            m_Builder.CreateCall(freeFunc, castData);

            // Free RefCount
            llvm::Value *castRef = m_Builder.CreateBitCast(
                refPtr,
                llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
            m_Builder.CreateCall(freeFunc, castRef);

            m_Builder.CreateBr(contBB);
            m_Builder.SetInsertPoint(contBB);
          }
        }
      }
    }

    if (retVal)
      return m_Builder.CreateRet(retVal);
    return m_Builder.CreateRetVoid();
  } else if (auto *dest = dynamic_cast<const DestructuringDecl *>(stmt)) {
    llvm::Value *initVal = genExpr(dest->Init.get());
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
      llvm::Value *val = m_Builder.CreateExtractValue(initVal, i, v.Name);
      llvm::Type *ty = val->getType();

      llvm::AllocaInst *alloca = m_Builder.CreateAlloca(ty, nullptr, v.Name);
      m_Builder.CreateStore(val, alloca);

      m_NamedValues[v.Name] = alloca;
      m_ValueTypes[v.Name] = ty;
      m_ValueElementTypes[v.Name] = ty; // fallback for basic types
      m_ValueIsMutable[v.Name] = v.IsMutable;
      m_ValueIsNullable[v.Name] = v.IsNullable;

      if (!m_ScopeStack.empty()) {
        m_ScopeStack.back().push_back({v.Name, alloca, false, false});
      }
    }
    return nullptr;
  } else if (auto *var = dynamic_cast<const VariableDecl *>(stmt)) {
    llvm::Value *initVal = nullptr;
    if (var->Init) {
      initVal = genExpr(var->Init.get());
      if (!initVal)
        return nullptr;
    }

    llvm::Type *type = nullptr;
    if (!var->TypeName.empty()) {
      type = resolveType(var->TypeName, var->HasPointer || var->IsReference);
    } else if (initVal) {
      type = initVal->getType();
    }

    llvm::Type *elemTy = nullptr;
    if (!var->TypeName.empty()) {
      elemTy = resolveType(var->TypeName, false);
      if (var->TypeName == "str")
        elemTy = llvm::Type::getInt8Ty(m_Context);
    } else if (initVal) {
      if (auto *ne = dynamic_cast<const NewExpr *>(var->Init.get())) {
        elemTy = resolveType(ne->Type, false);
      } else if (auto *ve =
                     dynamic_cast<const VariableExpr *>(var->Init.get())) {
        elemTy = m_ValueElementTypes[ve->Name];
      } else if (auto *ae =
                     dynamic_cast<const AddressOfExpr *>(var->Init.get())) {
        if (auto *ve =
                dynamic_cast<const VariableExpr *>(ae->Expression.get())) {
          elemTy = m_ValueTypes[ve->Name];
        }
      } else if (var->IsShared && type && type->isStructTy() &&
                 type->getStructNumElements() == 2) {
        // Fallback: Try to get from first element of existing shared
        llvm::Type *ptrTy = type->getStructElementType(0);
        if (ptrTy->isPointerTy()) {
          // Opaque pointer fallback, usually handled by VariableExpr above
        }
      }
      if (!elemTy) {
        if (type->isArrayTy())
          elemTy = type->getArrayElementType();
        else if (type->isPointerTy())
          elemTy = llvm::Type::getInt8Ty(m_Context);
        else
          elemTy = type;
      }
    }

    if (!type) {
      error(var, "Cannot infer type for variable '" + var->Name + "'");
      return nullptr;
    }

    if (var->Init && initVal) {
      // Move Semantics for Unique
      if (var->IsUnique) {
        if (auto *ve = dynamic_cast<const VariableExpr *>(var->Init.get())) {
          if (m_ValueIsUnique[ve->Name]) {
            llvm::Value *s = m_NamedValues[ve->Name];
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
            initVal->getType()->getStructNumElements() == 2) {
          llvm::Value *ref = m_Builder.CreateExtractValue(initVal, 1);
          llvm::Value *c = m_Builder.CreateLoad(
              llvm::Type::getInt32Ty(m_Context), ref, "ref_count");
          llvm::Value *inc = m_Builder.CreateAdd(
              c, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
          m_Builder.CreateStore(inc, ref);
        } else if (initVal->getType()->isPointerTy()) {
          llvm::Function *mallocFn = m_Module->getFunction("malloc");
          if (mallocFn) {
            llvm::Value *size =
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), 4);
            llvm::Value *refPtr = m_Builder.CreateCall(mallocFn, size);
            refPtr = m_Builder.CreateBitCast(
                refPtr, llvm::PointerType::getUnqual(
                            llvm::Type::getInt32Ty(m_Context)));
            m_Builder.CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1),
                refPtr);
            llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
            llvm::Type *refTy =
                llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
            llvm::StructType *st =
                llvm::StructType::get(m_Context, {ptrTy, refTy});
            llvm::Value *u = llvm::UndefValue::get(st);
            llvm::Value *ci = m_Builder.CreateBitCast(initVal, ptrTy);
            initVal = m_Builder.CreateInsertValue(
                m_Builder.CreateInsertValue(u, ci, 0), refPtr, 1);
            type = st;
          }
        }
      }
    }

    llvm::AllocaInst *alloca = m_Builder.CreateAlloca(type, nullptr, var->Name);
    m_NamedValues[var->Name] = alloca;
    m_ValueTypes[var->Name] = type;
    m_ValueElementTypes[var->Name] = elemTy;
    m_ValueIsReference[var->Name] = var->IsReference;
    m_ValueIsMutable[var->Name] = var->IsMutable;
    m_ValueIsUnique[var->Name] = var->IsUnique;
    m_ValueIsShared[var->Name] = var->IsShared;

    if (!m_ScopeStack.empty()) {
      m_ScopeStack.back().push_back(
          {var->Name, alloca, var->IsUnique, var->IsShared});
    }

    if (var->Init && initVal) {
      if (initVal->getType() != type)
        initVal = m_Builder.CreateBitCast(initVal, type);
      m_Builder.CreateStore(initVal, alloca);
    }
    return nullptr;
  } else if (auto *ifs = dynamic_cast<const IfStmt *>(stmt)) {
    llvm::Value *cond = genExpr(ifs->Condition.get());
    if (!cond)
      return nullptr;
    cond = m_Builder.CreateICmpNE(
        cond, llvm::ConstantInt::get(cond->getType(), 0), "ifcond");
    llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(m_Context, "then", f);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(m_Context, "else");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(m_Context, "ifcont");
    m_Builder.CreateCondBr(cond, thenBB, elseBB);
    m_Builder.SetInsertPoint(thenBB);
    genStmt(ifs->Then.get());
    if (!thenBB->getTerminator())
      m_Builder.CreateBr(mergeBB);
    elseBB->insertInto(f);
    m_Builder.SetInsertPoint(elseBB);
    if (ifs->Else)
      genStmt(ifs->Else.get());
    if (!elseBB->getTerminator())
      m_Builder.CreateBr(mergeBB);
    mergeBB->insertInto(f);
    m_Builder.SetInsertPoint(mergeBB);
    return nullptr;
  } else if (auto *ws = dynamic_cast<const WhileStmt *>(stmt)) {
    llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *condBB =
        llvm::BasicBlock::Create(m_Context, "whilecond", f);
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(m_Context, "whileloop");
    llvm::BasicBlock *afterBB =
        llvm::BasicBlock::Create(m_Context, "whileafter");
    m_Builder.CreateBr(condBB);
    m_Builder.SetInsertPoint(condBB);
    llvm::Value *cond = genExpr(ws->Condition.get());
    if (!cond)
      return nullptr;
    cond = m_Builder.CreateICmpNE(
        cond, llvm::ConstantInt::get(cond->getType(), 0), "whilecond");
    m_Builder.CreateCondBr(cond, loopBB, afterBB);
    loopBB->insertInto(f);
    m_Builder.SetInsertPoint(loopBB);
    genStmt(ws->Body.get());
    if (!loopBB->getTerminator())
      m_Builder.CreateBr(condBB);
    afterBB->insertInto(f);
    m_Builder.SetInsertPoint(afterBB);
    return nullptr;
  } else if (auto *bs = dynamic_cast<const BlockStmt *>(stmt)) {
    m_ScopeStack.push_back({});
    for (const auto &s : bs->Statements)
      genStmt(s.get());
    auto &scope = m_ScopeStack.back();
    if (!m_Builder.GetInsertBlock()->getTerminator()) {
      for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
        if (it->IsUniquePointer && it->Alloca) {
          llvm::Value *val = m_Builder.CreateLoad(
              llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType(),
              it->Alloca);
          llvm::Function *freeFunc = m_Module->getFunction("free");
          if (freeFunc) {
            llvm::BasicBlock *currBB = m_Builder.GetInsertBlock();
            llvm::Function *func = currBB->getParent();
            llvm::BasicBlock *freeBB =
                llvm::BasicBlock::Create(m_Context, "free_block", func);
            llvm::BasicBlock *contBB =
                llvm::BasicBlock::Create(m_Context, "free_cont", func);
            llvm::Value *notNull = m_Builder.CreateIsNotNull(val);
            m_Builder.CreateCondBr(notNull, freeBB, contBB);
            m_Builder.SetInsertPoint(freeBB);
            m_Builder.CreateCall(
                freeFunc, m_Builder.CreateBitCast(
                              val, llvm::PointerType::getUnqual(
                                       llvm::Type::getInt8Ty(m_Context))));
            m_Builder.CreateBr(contBB);
            m_Builder.SetInsertPoint(contBB);
          }
        } else if (it->IsShared && it->Alloca) {
          llvm::Value *sh = m_Builder.CreateLoad(
              llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType(),
              it->Alloca);
          llvm::Value *ref = m_Builder.CreateExtractValue(sh, 1);
          llvm::Value *c = m_Builder.CreateLoad(
              llvm::Type::getInt32Ty(m_Context), ref, "ref_count");
          llvm::Value *dec = m_Builder.CreateSub(
              c, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
          m_Builder.CreateStore(dec, ref);
          llvm::Function *freeFunc = m_Module->getFunction("free");
          if (freeFunc) {
            llvm::BasicBlock *currBB = m_Builder.GetInsertBlock();
            llvm::Function *func = currBB->getParent();
            llvm::BasicBlock *freeBB =
                llvm::BasicBlock::Create(m_Context, "sh_free", func);
            llvm::BasicBlock *contBB =
                llvm::BasicBlock::Create(m_Context, "sh_cont", func);
            llvm::Value *isZero = m_Builder.CreateICmpEQ(
                dec,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));
            m_Builder.CreateCondBr(isZero, freeBB, contBB);
            m_Builder.SetInsertPoint(freeBB);
            m_Builder.CreateCall(
                freeFunc,
                m_Builder.CreateBitCast(m_Builder.CreateExtractValue(sh, 0),
                                        llvm::PointerType::getUnqual(
                                            llvm::Type::getInt8Ty(m_Context))));
            m_Builder.CreateCall(
                freeFunc, m_Builder.CreateBitCast(
                              ref, llvm::PointerType::getUnqual(
                                       llvm::Type::getInt8Ty(m_Context))));
            m_Builder.CreateBr(contBB);
            m_Builder.SetInsertPoint(contBB);
          }
        }
      }
    }
    m_ScopeStack.pop_back();
    return nullptr;
  } else if (auto *es = dynamic_cast<const ExprStmt *>(stmt)) {
    return genExpr(es->Expression.get());
  }
  return nullptr;
}

llvm::Value *CodeGen::genExpr(const Expr *expr) {
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
  if (auto *str = dynamic_cast<const StringExpr *>(expr)) {
    return m_Builder.CreateGlobalStringPtr(str->Value);
  }
  if (auto *addrOf = dynamic_cast<const AddressOfExpr *>(expr)) {
    return genAddr(addrOf->Expression.get());
  }
  if (auto *unary = dynamic_cast<const UnaryExpr *>(expr)) {
    if (unary->Op == TokenType::PlusPlus ||
        unary->Op == TokenType::MinusMinus) {
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
  if (auto *post = dynamic_cast<const PostfixExpr *>(expr)) {
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
  if (auto *call = dynamic_cast<const CallExpr *>(expr)) {
    llvm::Function *callee = m_Module->getFunction(call->Callee);
    if (!callee)
      return nullptr;

    const FunctionDecl *funcDecl = nullptr;
    if (m_Functions.count(call->Callee))
      funcDecl = m_Functions[call->Callee];

    const ExternDecl *extDecl = nullptr;
    if (m_Externs.count(call->Callee))
      extDecl = m_Externs[call->Callee];

    std::vector<llvm::Value *> argsV;
    for (size_t i = 0; i < call->Args.size(); ++i) {
      bool isMut = false;
      bool isRef = false;
      if (funcDecl && i < funcDecl->Args.size()) {
        isMut = funcDecl->Args[i].IsMutable;
        isRef = funcDecl->Args[i].IsReference;
      } else if (extDecl && i < extDecl->Args.size()) {
        isMut = extDecl->Args[i].IsMutable;
        isRef = extDecl->Args[i].IsReference;
      }

      llvm::Value *val = nullptr;
      bool shouldPassAddr = isRef;
      if (funcDecl && i < funcDecl->Args.size()) {
        llvm::Type *baseT =
            resolveType(funcDecl->Args[i].Type, funcDecl->Args[i].HasPointer);
        if (baseT->isStructTy() || baseT->isArrayTy() ||
            funcDecl->Args[i].IsMutable) {
          shouldPassAddr = true;
        }
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
        std::cerr << "CodeGen Error: Failed to generate argument " << i
                  << " for " << call->Callee << "\n";
        return nullptr;
      }

      if (i < callee->getFunctionType()->getNumParams()) {
        llvm::Type *paramType = callee->getFunctionType()->getParamType(i);
        if (val->getType() != paramType) {
          std::string expected, actual;
          llvm::raw_string_ostream os_exp(expected), os_act(actual);
          paramType->print(os_exp);
          val->getType()->print(os_act);
          error(call, "Type mismatch for argument " + std::to_string(i) +
                          " in call to " + call->Callee + ". Expected " +
                          expected + ", got " + actual);
          return nullptr;
        }
      } else if (callee->isVarArg()) {
        // Varargs promotion
        if (val->getType()->isIntegerTy()) {
          if (val->getType()->getIntegerBitWidth() < 32)
            val = m_Builder.CreateIntCast(
                val, llvm::Type::getInt32Ty(m_Context), true);
        } else if (val->getType()->isFloatTy()) {
          val = m_Builder.CreateFPExt(val, llvm::Type::getDoubleTy(m_Context));
        }
      }
      argsV.push_back(val);
    }
    return m_Builder.CreateCall(callee, argsV);
  }
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    llvm::Value *v = m_NamedValues[var->Name];
    if (!v)
      return nullptr;
    llvm::Value *val = v;
    if (llvm::isa<llvm::AllocaInst>(v) || llvm::isa<llvm::GlobalVariable>(v)) {
      val = m_Builder.CreateLoad(m_ValueTypes[var->Name], v, var->Name);
    }
    if (m_ValueIsReference[var->Name]) {
      val = m_Builder.CreateLoad(m_ValueElementTypes[var->Name], val,
                                 var->Name + "_val");
    }
    return val;
  }
  if (auto *bin = dynamic_cast<const BinaryExpr *>(expr)) {
    if (bin->Op == "=" || bin->Op == "+=" || bin->Op == "-=" ||
        bin->Op == "*=" || bin->Op == "/=") {
      llvm::Value *ptr = genAddr(bin->LHS.get());
      if (!ptr)
        return nullptr;

      if (auto *varLHS = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
        if (!m_ValueIsMutable[varLHS->Name]) {
          error(bin, "Cannot mutate immutable variable '" + varLHS->Name + "'");
          return nullptr;
        }
        if (!varLHS->IsMutable) {
          error(bin,
                "Missing '#' token for mutation of '" + varLHS->Name + "'");
          return nullptr;
        }
      }

      llvm::Value *rhsVal = genExpr(bin->RHS.get());
      if (!rhsVal)
        return nullptr;

      llvm::Type *destType = nullptr;
      if (auto *varLHS = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
        if (m_ValueIsReference[varLHS->Name])
          destType = m_ValueElementTypes[varLHS->Name];
        else
          destType = m_ValueTypes[varLHS->Name];
      } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(ptr)) {
        destType = gep->getResultElementType();
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
        if (bin->Op == "+=")
          res = m_Builder.CreateAdd(lhsVal, rhsVal);
        else if (bin->Op == "-=")
          res = m_Builder.CreateSub(lhsVal, rhsVal);
        else if (bin->Op == "*=")
          res = m_Builder.CreateMul(lhsVal, rhsVal);
        else if (bin->Op == "/=")
          res = m_Builder.CreateSDiv(lhsVal, rhsVal);

        rhsVal = res;
      }

      if (destType != rhsVal->getType()) {
        error(bin, "Type mismatch in assignment");
        return nullptr;
      }

      if (bin->Op == "=") {
        if (auto *varLHS = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
          if (m_ValueIsUnique[varLHS->Name]) {
            // 1. Free old value
            if (destType) {
              llvm::Value *oldVal = m_Builder.CreateLoad(destType, ptr);
              llvm::Function *freeFunc = m_Module->getFunction("free");
              if (freeFunc) {
                llvm::BasicBlock *currBB = m_Builder.GetInsertBlock();
                llvm::Function *func = currBB->getParent();
                llvm::BasicBlock *freeBB =
                    llvm::BasicBlock::Create(m_Context, "assign_free", func);
                llvm::BasicBlock *contBB =
                    llvm::BasicBlock::Create(m_Context, "assign_cont", func);

                llvm::Value *notNull = m_Builder.CreateIsNotNull(oldVal);
                m_Builder.CreateCondBr(notNull, freeBB, contBB);
                m_Builder.SetInsertPoint(freeBB);
                llvm::Value *casted = m_Builder.CreateBitCast(
                    oldVal, llvm::PointerType::getUnqual(
                                llvm::Type::getInt8Ty(m_Context)));
                m_Builder.CreateCall(freeFunc, casted);
                m_Builder.CreateBr(contBB);
                m_Builder.SetInsertPoint(contBB);
              }
            }
            // 2. Nullify RHS if variable
            if (auto *varRHS =
                    dynamic_cast<const VariableExpr *>(bin->RHS.get())) {
              if (m_ValueIsUnique[varRHS->Name]) {
                llvm::Value *rhsPtr = m_NamedValues[varRHS->Name];
                if (rhsPtr) {
                  if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(rhsPtr))
                    m_Builder.CreateStore(
                        llvm::Constant::getNullValue(ai->getAllocatedType()),
                        rhsPtr);
                }
              }
            }
          } else if (m_ValueIsShared[varLHS->Name]) {
            // Shared Assignment
            // 1. Inc RHS
            if (rhsVal->getType()->isStructTy()) {
              llvm::Value *newRefPtr =
                  m_Builder.CreateExtractValue(rhsVal, 1, "new_ref_ptr");
              llvm::Value *count = m_Builder.CreateLoad(
                  llvm::Type::getInt32Ty(m_Context), newRefPtr, "new_count");
              llvm::Value *inc = m_Builder.CreateAdd(
                  count,
                  llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
              m_Builder.CreateStore(inc, newRefPtr);
            }

            // 2. Dec LHS
            llvm::Value *oldVal = m_Builder.CreateLoad(destType, ptr);
            llvm::Value *oldRefPtr =
                m_Builder.CreateExtractValue(oldVal, 1, "old_ref_ptr");
            llvm::Value *oldCount = m_Builder.CreateLoad(
                llvm::Type::getInt32Ty(m_Context), oldRefPtr, "old_count");
            llvm::Value *dec = m_Builder.CreateSub(
                oldCount,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
            m_Builder.CreateStore(dec, oldRefPtr);

            llvm::Function *freeFunc = m_Module->getFunction("free");
            if (freeFunc) {
              llvm::BasicBlock *curBB = m_Builder.GetInsertBlock();
              llvm::Function *func = curBB->getParent();
              llvm::BasicBlock *freeBB =
                  llvm::BasicBlock::Create(m_Context, "assign_sh_free", func);
              llvm::BasicBlock *contBB =
                  llvm::BasicBlock::Create(m_Context, "assign_sh_cont", func);

              llvm::Value *isZero = m_Builder.CreateICmpEQ(
                  dec,
                  llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));
              m_Builder.CreateCondBr(isZero, freeBB, contBB);

              m_Builder.SetInsertPoint(freeBB);
              // Free Old Data & Ref
              llvm::Value *oldData =
                  m_Builder.CreateExtractValue(oldVal, 0, "old_data");
              llvm::Value *castData = m_Builder.CreateBitCast(
                  oldData, llvm::PointerType::getUnqual(
                               llvm::Type::getInt8Ty(m_Context)));
              m_Builder.CreateCall(freeFunc, castData);
              llvm::Value *castRef = m_Builder.CreateBitCast(
                  oldRefPtr, llvm::PointerType::getUnqual(
                                 llvm::Type::getInt8Ty(m_Context)));
              m_Builder.CreateCall(freeFunc, castRef);

              m_Builder.CreateBr(contBB);
              m_Builder.SetInsertPoint(contBB);
            }
          }
        }
      }
      m_Builder.CreateStore(rhsVal, ptr);
      return rhsVal;
    }

    // Standard Arithmetic and Comparisons
    llvm::Value *lhs = genExpr(bin->LHS.get());
    llvm::Value *rhs = genExpr(bin->RHS.get());
    if (!lhs || !rhs)
      return nullptr;

    if (lhs->getType() != rhs->getType()) {
      error(bin, "Type mismatch in binary expression");
      return nullptr;
    }

    if (bin->Op == "+")
      return m_Builder.CreateAdd(lhs, rhs, "addtmp");
    if (bin->Op == "-")
      return m_Builder.CreateSub(lhs, rhs, "subtmp");
    if (bin->Op == "*")
      return m_Builder.CreateMul(lhs, rhs, "multmp");
    if (bin->Op == "/")
      return m_Builder.CreateSDiv(lhs, rhs, "divtmp");
    if (bin->Op == "<")
      return m_Builder.CreateICmpSLT(lhs, rhs, "lt_tmp");
    if (bin->Op == ">")
      return m_Builder.CreateICmpSGT(lhs, rhs, "gt_tmp");
    if (bin->Op == "==")
      return m_Builder.CreateICmpEQ(lhs, rhs, "eq_tmp");
    if (bin->Op == "!=")
      return m_Builder.CreateICmpNE(lhs, rhs, "ne_tmp");
    return nullptr;
  }
  if (auto *cast = dynamic_cast<const CastExpr *>(expr)) {
    llvm::Value *val = genExpr(cast->Expression.get());
    if (!val)
      return nullptr;
    llvm::Type *targetType = resolveType(cast->TargetType, false);
    if (val->getType()->isIntegerTy() && targetType->isIntegerTy())
      return m_Builder.CreateIntCast(val, targetType, true);
    return val;
  }
  if (auto *newExpr = dynamic_cast<const NewExpr *>(expr)) {
    llvm::Type *type = resolveType(newExpr->Type, false);
    if (!type)
      return nullptr;

    // Size of type
    uint64_t size = m_Module->getDataLayout().getTypeAllocSize(type);

    // Call malloc
    llvm::Function *mallocFn = m_Module->getFunction("malloc");
    if (!mallocFn) {
      error(newExpr, "malloc function not found (missing import "
                     "'core/memory'?)");
      return nullptr;
    }

    llvm::Value *sizeVal =
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(m_Context), size);
    llvm::Value *voidPtr = m_Builder.CreateCall(mallocFn, sizeVal, "new_alloc");

    // Cast to T*
    llvm::Value *heapPtr = m_Builder.CreateBitCast(
        voidPtr, llvm::PointerType::getUnqual(type), "new_ptr");

    if (newExpr->Initializer) {
      if (auto *strInit = dynamic_cast<const InitStructExpr *>(
              newExpr->Initializer.get())) {
        // Struct initialization on heap
        // We need to resolve struct type to get field indices
        llvm::StructType *st = m_StructTypes[newExpr->Type];
        if (!st) {
          error(newExpr, "Unknown struct type " + newExpr->Type);
          return nullptr;
        }

        auto &fields = m_StructFieldNames[newExpr->Type];
        for (const auto &f : strInit->Fields) {
          int idx = -1;
          for (int i = 0; i < fields.size(); ++i) {
            if (fields[i] == f.first) {
              idx = i;
              break;
            }
          }
          if (idx == -1) {
            error(newExpr, "Unknown field " + f.first);
            return nullptr;
          }

          llvm::Value *fieldVal = genExpr(f.second.get());
          if (!fieldVal)
            return nullptr;

          // GEP
          llvm::Value *fieldAddr =
              m_Builder.CreateStructGEP(st, heapPtr, idx, "field_" + f.first);
          m_Builder.CreateStore(fieldVal, fieldAddr);
        }
      } else {
        // Primitive initialization or copy
        llvm::Value *initVal = genExpr(newExpr->Initializer.get());
        if (initVal) {
          if (initVal->getType() != type) {
            error(newExpr, "Type mismatch in new initialization");
            return nullptr;
          }
          m_Builder.CreateStore(initVal, heapPtr);
        }
      }
    }

    return heapPtr;
  }
  if (auto *mem = dynamic_cast<const MemberExpr *>(expr)) {
    llvm::Value *addr = genAddr(mem);
    if (!addr)
      return nullptr;
    // We need the element type to perform a Load.
    // Manually resolve the struct and field type to be robust.
    llvm::Type *objType = nullptr;
    if (auto *objVar = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
      objType = m_ValueElementTypes[objVar->Name];
    } else if (auto *objMem =
                   dynamic_cast<const MemberExpr *>(mem->Object.get())) {
      // For a.b.c, we need to resolve recursively or catch it from
      // GEP
      if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(addr)) {
        return m_Builder.CreateLoad(gep->getResultElementType(), addr,
                                    mem->Member);
      }
    }

    llvm::StructType *st = nullptr;
    if (objType && objType->isStructTy()) {
      st = llvm::cast<llvm::StructType>(objType);
    } else {
      // Try to find struct by field name
      for (const auto &pair : m_StructFieldNames) {
        for (const auto &f : pair.second) {
          if (f == mem->Member) {
            st = m_StructTypes[pair.first];
            break;
          }
        }
        if (st)
          break;
      }
    }

    if (st) {
      int idx = -1;
      std::string stName = m_TypeToName[st];
      if (stName.empty()) {
        // Fallback to name search
        for (const auto &pair : m_StructTypes) {
          if (pair.second == st) {
            stName = pair.first;
            break;
          }
        }
      }

      if (!stName.empty()) {
        auto &fields = m_StructFieldNames[stName];
        for (size_t i = 0; i < fields.size(); ++i) {
          if (fields[i] == mem->Member) {
            idx = (int)i;
            break;
          }
        }
      }

      if (idx != -1) {
        llvm::Type *fieldType = st->getElementType(idx);
        return m_Builder.CreateLoad(fieldType, addr, mem->Member);
      }
    }

    // Fallback to fragile GEP cast if struct resolution failed
    if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(addr)) {
      return m_Builder.CreateLoad(gep->getResultElementType(), addr,
                                  mem->Member);
    }

    return nullptr;
  }
  if (auto *init = dynamic_cast<const InitStructExpr *>(expr)) {
    llvm::StructType *st = m_StructTypes[init->StructName];
    if (!st)
      return nullptr;
    llvm::Value *alloca = m_Builder.CreateAlloca(st, nullptr, "structinit");
    auto &fieldNames = m_StructFieldNames[init->StructName];
    for (const auto &f : init->Fields) {
      int idx = -1;
      for (size_t i = 0; i < fieldNames.size(); ++i)
        if (fieldNames[i] == f.first) {
          idx = i;
          break;
        }
      if (idx == -1)
        continue;
      llvm::Value *fieldAddr = m_Builder.CreateStructGEP(st, alloca, idx);
      llvm::Value *fieldVal = genExpr(f.second.get());
      if (fieldVal->getType() != st->getElementType(idx)) {
        error(init,
              "Type mismatch for field " + f.first + " in " + init->StructName);
        return nullptr;
      }
      m_Builder.CreateStore(fieldVal, fieldAddr);
    }
    return m_Builder.CreateLoad(st, alloca);
  }
  if (auto *arr = dynamic_cast<const ArrayExpr *>(expr)) {
    if (arr->Elements.empty())
      return nullptr;
    std::vector<llvm::Value *> vals;
    for (auto &e : arr->Elements)
      vals.push_back(genExpr(e.get()));
    llvm::ArrayType *at = llvm::ArrayType::get(vals[0]->getType(), vals.size());
    llvm::Value *alloca = m_Builder.CreateAlloca(at, nullptr, "arrayliteral");
    for (size_t i = 0; i < vals.size(); ++i) {
      llvm::Value *ptr = m_Builder.CreateInBoundsGEP(
          at, alloca, {m_Builder.getInt32(0), m_Builder.getInt32((uint32_t)i)});
      m_Builder.CreateStore(vals[i], ptr);
    }
    return m_Builder.CreateLoad(at, alloca);
  }
  if (auto *tuple = dynamic_cast<const TupleExpr *>(expr)) {
    std::vector<llvm::Value *> vals;
    std::vector<llvm::Type *> types;
    for (auto &e : tuple->Elements) {
      llvm::Value *v = genExpr(e.get());
      vals.push_back(v);
      types.push_back(v->getType());
    }
    llvm::StructType *st = llvm::StructType::get(m_Context, types);
    // Register tuple
    std::string typeStr = "(";
    for (size_t i = 0; i < types.size(); ++i) {
      if (i > 0)
        typeStr += ",";
      std::string s;
      llvm::raw_string_ostream os(s);
      types[i]->print(os);
      typeStr += os.str();
    }
    typeStr += ")";
    m_TypeToName[st] = typeStr;
    std::vector<std::string> fields;
    for (size_t i = 0; i < types.size(); ++i)
      fields.push_back(std::to_string(i));
    m_StructFieldNames[typeStr] = fields;
    m_StructTypes[typeStr] = st;

    llvm::Value *alloca = m_Builder.CreateAlloca(st, nullptr, "tupleliteral");
    for (size_t i = 0; i < vals.size(); ++i) {
      llvm::Value *ptr = m_Builder.CreateStructGEP(st, alloca, i);
      m_Builder.CreateStore(vals[i], ptr);
    }
    return m_Builder.CreateLoad(st, alloca);
  }
  if (auto *idxExpr = dynamic_cast<const ArrayIndexExpr *>(expr)) {
    llvm::Value *addr = genAddr(idxExpr);
    if (!addr)
      return nullptr;
    // We need the element type.
    // Try to get it from the GEP result if addr is GEP.
    if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(addr)) {
      return m_Builder.CreateLoad(gep->getResultElementType(), addr);
    }
    return nullptr;
  }
  return nullptr;
}

llvm::Value *CodeGen::genAddr(const Expr *expr) {
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    llvm::Value *v = m_NamedValues[var->Name];
    if (m_ValueIsReference[var->Name] && v && llvm::isa<llvm::AllocaInst>(v)) {
      return m_Builder.CreateLoad(m_ValueTypes[var->Name], v,
                                  var->Name + "_ref_addr");
    }
    return v;
  }
  if (auto *idxExpr = dynamic_cast<const ArrayIndexExpr *>(expr)) {
    llvm::Value *arrAddr = genAddr(idxExpr->Array.get());
    if (!arrAddr) {
      llvm::Value *arrVal = genExpr(idxExpr->Array.get());
      arrAddr = m_Builder.CreateAlloca(arrVal->getType(), nullptr, "arrtmp");
      m_Builder.CreateStore(arrVal, arrAddr);
    }
    llvm::Value *index = genExpr(idxExpr->Index.get());
    llvm::Type *elemTy = nullptr;
    llvm::Type *arrTy = nullptr;
    if (auto *var = dynamic_cast<const VariableExpr *>(idxExpr->Array.get())) {
      arrTy = m_ValueTypes[var->Name];
      elemTy = m_ValueElementTypes[var->Name];
    } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(arrAddr)) {
      arrTy = gep->getResultElementType();
      if (arrTy->isArrayTy())
        elemTy = arrTy->getArrayElementType();
    }

    if (arrTy && arrTy->isArrayTy()) {
      return m_Builder.CreateInBoundsGEP(arrTy, arrAddr,
                                         {m_Builder.getInt32(0), index});
    }
    if (elemTy && arrTy && arrTy->isPointerTy()) {
      return m_Builder.CreateInBoundsGEP(elemTy, arrAddr, index);
    }
    return nullptr;
  }
  if (auto *mem = dynamic_cast<const MemberExpr *>(expr)) {
    llvm::Value *objAddr = genAddr(mem->Object.get());

    if (objAddr) {
      if (auto *var = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
        if (m_ValueIsShared[var->Name]) {
          if (llvm::isa<llvm::AllocaInst>(objAddr)) {
            llvm::Value *structVal = m_Builder.CreateLoad(
                llvm::cast<llvm::AllocaInst>(objAddr)->getAllocatedType(),
                objAddr, "shared_vars_load");
            objAddr =
                m_Builder.CreateExtractValue(structVal, 0, "shared_data_ptr");
          }
        }
      }
    }

    if (!objAddr) {
      llvm::Value *objVal = genExpr(mem->Object.get());
      if (!objVal)
        return nullptr;
      objAddr = m_Builder.CreateAlloca(objVal->getType());
      m_Builder.CreateStore(objVal, objAddr);
    }

    llvm::Type *objType = nullptr;
    if (auto *var = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
      objType = m_ValueElementTypes[var->Name];
    } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objAddr)) {
      objType = gep->getResultElementType();
    }

    if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(objAddr)) {
      llvm::Type *allocTy = alloca->getAllocatedType();
      if (allocTy->isStructTy()) {
        objType = allocTy;
      } else if (allocTy->isPointerTy()) {
        // It's a pointer variable (Struct**), load to get Struct*
        objAddr = m_Builder.CreateLoad(allocTy, objAddr, "deref");
        // We don't know the struct type from opaque ptr, will find
        // by field
        objType = nullptr;
      }
    } else if (objAddr->getType()->isPointerTy()) {
      // Result of a GEP or Load, likely Struct* (if typed) or ptr
      // (opaque) Assume it is the address of the struct. We can't
      // verify type easily.
    }

    int idx = -1;
    llvm::StructType *st = nullptr;
    if (objType && objType->isStructTy()) {
      st = llvm::cast<llvm::StructType>(objType);
    } else {
      // Try to find struct by field name
      std::string foundStruct;
      for (const auto &pair : m_StructFieldNames) {
        const auto &fields = pair.second;
        for (int i = 0; i < (int)fields.size(); ++i) {
          if (fields[i] == mem->Member) {
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

    if (!st)
      return nullptr;

    if (idx == -1) {
      // Recalculate index from st using our map
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
          if (fields[i] == mem->Member) {
            idx = i;
            break;
          }
        }
      } else {
        std::string objName = "unknown";
        if (auto *var = dynamic_cast<const VariableExpr *>(mem->Object.get()))
          objName = var->Name;
      }
    }

    if (idx == -1)
      return nullptr;

    return m_Builder.CreateStructGEP(st, objAddr, idx);
  }
  return nullptr;
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
  llvm::Function::Create(ft, llvm::Function::ExternalLinkage, ext->Name,
                         m_Module.get());
}

llvm::Type *CodeGen::resolveType(const std::string &baseType, bool hasPointer) {
  llvm::Type *type = nullptr;
  if (baseType.empty())
    return nullptr;

  // Check aliases first
  if (m_TypeAliases.count(baseType)) {
    return resolveType(m_TypeAliases[baseType], hasPointer);
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
  if (baseType.size() > 1 && (baseType[0] == '*' || baseType[0] == '^')) {
    return llvm::PointerType::getUnqual(resolveType(baseType.substr(1), false));
  }

  if (baseType[0] == '[') {
    // Array: [T; N]
    size_t lastSemi = baseType.find_last_of(';');
    if (lastSemi != std::string::npos) {
      std::string elemTyStr = baseType.substr(1, lastSemi - 1);
      std::string countStr =
          baseType.substr(lastSemi + 1, baseType.size() - lastSemi - 2);
      llvm::Type *elemTy = resolveType(elemTyStr, false);
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
        elemTypes.push_back(resolveType(elemStr, false));
        start = i + 1;
      }
    }
    if (start < content.size()) {
      std::string elemStr = content.substr(start);
      elemStr.erase(0, elemStr.find_first_not_of(" \t"));
      elemStr.erase(elemStr.find_last_not_of(" \t") + 1);
      elemTypes.push_back(resolveType(elemStr, false));
    }
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
  } else if (baseType == "bool")
    type = llvm::Type::getInt1Ty(m_Context);
  else if (baseType == "i8" || baseType == "u8" || baseType == "char")
    type = llvm::Type::getInt8Ty(m_Context);
  else if (baseType == "i16" || baseType == "u16")
    type = llvm::Type::getInt16Ty(m_Context);
  else if (baseType == "i32" || baseType == "u32")
    type = llvm::Type::getInt32Ty(m_Context);
  else if (baseType == "i64" || baseType == "u64")
    type = llvm::Type::getInt64Ty(m_Context);
  else if (baseType == "f32")
    type = llvm::Type::getFloatTy(m_Context);
  else if (baseType == "f64")
    type = llvm::Type::getDoubleTy(m_Context);
  else if (baseType == "str")
    type = llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
  else if (baseType == "void")
    type = llvm::Type::getVoidTy(m_Context);
  else if (m_StructTypes.count(baseType))
    type = m_StructTypes[baseType];
  else
    type = llvm::Type::getInt64Ty(m_Context);

  if (hasPointer && type)
    return llvm::PointerType::getUnqual(type);
  return type;
}

void CodeGen::genStruct(const StructDecl *str) {
  std::vector<llvm::Type *> fieldTypes;
  std::vector<std::string> fieldNames;
  for (const auto &field : str->Fields) {
    fieldTypes.push_back(resolveType(field.Type, field.HasPointer));
    fieldNames.push_back(field.Name);
  }
  llvm::StructType *st = llvm::StructType::create(m_Context, str->Name);
  st->setBody(fieldTypes);
  m_StructTypes[str->Name] = st;
  m_TypeToName[st] = str->Name;
  m_StructFieldNames[str->Name] = fieldNames;
}

void CodeGen::error(const ASTNode *node, const std::string &message) {
  m_ErrorCount++;
  if (node && !node->FileName.empty()) {
    std::cerr << node->FileName << ":" << node->Line << ":" << node->Column
              << ": error: " << message << "\n";
  } else if (node) {
    std::cerr << "error: " << message << " (at line " << node->Line << ")\n";
  } else {
    std::cerr << "error: " << message << "\n";
  }
}

} // namespace toka
