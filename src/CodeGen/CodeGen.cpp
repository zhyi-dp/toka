#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>

namespace toka {

void CodeGen::generate(const Module &ast) {
  m_AST = &ast;
  if (!m_Module) {
    m_Module = std::make_unique<llvm::Module>("toka_module", m_Context);
  }
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

  // Generate Options
  for (const auto &opt : ast.Options) {
    genOption(opt.get());
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

  // Generate Impls (Decl Phase)
  for (const auto &impl : ast.Impls) {
    genImpl(impl.get(), true);
    if (hasErrors())
      return;
  }

  // Generate Functions (Decl Phase)
  for (const auto &func : ast.Functions) {
    genFunction(func.get(), "", true);
    if (hasErrors())
      return;
  }

  // Generate Impls (Body Phase)
  for (const auto &impl : ast.Impls) {
    genImpl(impl.get(), false);
    if (hasErrors())
      return;
  }

  // Generate Functions (Body Phase)
  for (const auto &func : ast.Functions) {
    genFunction(func.get(), "", false);
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

llvm::Function *CodeGen::genFunction(const FunctionDecl *func,
                                     const std::string &overrideName,
                                     bool declOnly) {
  std::string funcName = overrideName.empty() ? func->Name : overrideName;
  m_NamedValues.clear();
  m_ValueTypes.clear();
  m_ValueElementTypes.clear();
  m_ValueIsReference.clear();
  m_ValueIsMutable.clear();

  llvm::Function *f = m_Module->getFunction(funcName);

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
    arg.setName(func->Args[idx].Name);
    llvm::Type *targetType =
        resolveType(func->Args[idx].Type, func->Args[idx].HasPointer);
    if (!targetType) {
      error(func, "Internal Error: Could not resolve type '" +
                      func->Args[idx].Type + "' for argument '" +
                      func->Args[idx].Name + "'");
      return nullptr;
    }
    llvm::Type *baseType = func->Args[idx].IsReference
                               ? llvm::PointerType::getUnqual(targetType)
                               : targetType;

    llvm::Type *t = targetType; // use existing resolved type
    if (func->Args[idx].IsReference)
      t = llvm::PointerType::getUnqual(t);

    bool isCaptured =
        (t->isStructTy() || t->isArrayTy() || func->Args[idx].IsMutable);
    bool isPointerOrRef = func->Args[idx].IsReference ||
                          func->Args[idx].HasPointer ||
                          func->Args[idx].IsUnique || func->Args[idx].IsShared;

    // Every argument gets an alloca for consistency and mutability
    llvm::Type *allocaType = baseType;
    if (isPointerOrRef || isCaptured) {
      allocaType = llvm::PointerType::getUnqual(targetType);
    }
    llvm::AllocaInst *alloca =
        m_Builder.CreateAlloca(allocaType, nullptr, func->Args[idx].Name);
    m_Builder.CreateStore(&arg, alloca);
    m_NamedValues[func->Args[idx].Name] = alloca;
    m_ValueTypes[func->Args[idx].Name] = allocaType;
    m_ValueElementTypes[func->Args[idx].Name] = targetType;
    m_ValueIsReference[func->Args[idx].Name] =
        func->Args[idx].IsReference || isCaptured;
    m_ValueIsRawPointer[func->Args[idx].Name] =
        (func->Args[idx].HasPointer && !func->Args[idx].IsUnique &&
         !func->Args[idx].IsShared);
    if (!m_ScopeStack.empty()) {
      m_ScopeStack.back().push_back({func->Args[idx].Name, alloca,
                                     func->Args[idx].IsUnique,
                                     func->Args[idx].IsShared});
    }
    m_ValueTypes[func->Args[idx].Name] = allocaType;
    m_ValueIsMutable[func->Args[idx].Name] = func->Args[idx].IsMutable;
    m_ValueIsUnique[func->Args[idx].Name] = func->Args[idx].IsUnique;
    m_ValueIsShared[func->Args[idx].Name] = func->Args[idx].IsShared;

    // Set Element Type for indexing or member access
    if (targetType->isArrayTy()) {
      m_ValueElementTypes[func->Args[idx].Name] =
          targetType->getArrayElementType();
    } else if (func->Args[idx].Type == "str") {
      m_ValueElementTypes[func->Args[idx].Name] =
          llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
    } else {
      m_ValueElementTypes[func->Args[idx].Name] = targetType;
    }

    idx++;
  }

  genStmt(func->Body.get());

  if (!m_Builder.GetInsertBlock()->getTerminator()) {
    if (func->ReturnType == "void" || func->Name == "main") {
      m_Builder.CreateRetVoid();
    } else {
      llvm::Type *retTy = resolveType(func->ReturnType, false);
      if (retTy) {
        m_Builder.CreateRet(llvm::Constant::getNullValue(retTy));
      } else {
        m_Builder.CreateRetVoid();
      }
    }
  }
  m_ScopeStack.pop_back();
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
  } else if (auto *del = dynamic_cast<const DeleteStmt *>(stmt)) {
    llvm::Function *freeFunc = m_Module->getFunction("free");
    if (freeFunc) {
      llvm::Value *val = genExpr(del->Expression.get());
      if (val && val->getType()->isPointerTy()) {
        llvm::Value *casted = m_Builder.CreateBitCast(
            val,
            llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context)));
        m_Builder.CreateCall(freeFunc, casted);
      }
    }
    return nullptr;
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
      type = resolveType(var->TypeName, var->HasPointer || var->IsReference ||
                                            var->IsUnique || var->IsShared);
      std::cerr << "[DEBUG] VariableDecl " << var->Name
                << ": Type from TypeName (Unique=" << var->IsUnique
                << ", Shared=" << var->IsShared << "), type=" << type << "\n";
    } else if (initVal) {
      type = initVal->getType();
      std::string typeStr;
      llvm::raw_string_ostream typeOS(typeStr);
      type->print(typeOS);
      std::cerr << "[DEBUG] VariableDecl " << var->Name
                << ": Type from initVal, initVal=" << initVal
                << ", type=" << typeOS.str() << "\n";
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
      elemTy = resolveType(soulTypeName, false);
    } else if (initVal) {
      if (auto *ve = dynamic_cast<const VariableExpr *>(var->Init.get())) {
        elemTy = m_ValueElementTypes[ve->Name];
      } else if (auto *ae =
                     dynamic_cast<const AddressOfExpr *>(var->Init.get())) {
        if (auto *vae =
                dynamic_cast<const VariableExpr *>(ae->Expression.get()))
          elemTy = m_ValueElementTypes[vae->Name];
      } else if (auto *newExpr =
                     dynamic_cast<const NewExpr *>(var->Init.get())) {
        elemTy = resolveType(newExpr->Type, false);
      } else if (initVal->getType()->isPointerTy()) {
        // Fallback: use the value type itself as elem
        elemTy = initVal->getType();
      }
    }

    if (!elemTy)
      elemTy = llvm::Type::getInt32Ty(m_Context);

    // Ensure m_ValueElementTypes is set early
    m_ValueElementTypes[var->Name] = elemTy;

    // The Form (Identity) is always what resolveType returns for the full name
    if (!type) { // Only try to resolve if type hasn't been determined yet
      type = resolveType(var->TypeName, var->HasPointer || var->IsUnique ||
                                            var->IsShared || var->IsReference);
    }
    if (!type && initVal)
      type = initVal->getType();

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
      error(var, "Cannot infer type for variable '" + var->Name + "'");
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

    std::cerr << "[DEBUG] VariableDecl: var=" << var->Name
              << ", IsUnique=" << var->IsUnique
              << ", IsShared=" << var->IsShared << ", type=";
    if (type) {
      std::string typeStr;
      llvm::raw_string_ostream typeOS(typeStr);
      type->print(typeOS);
      std::cerr << typeOS.str();
    } else {
      std::cerr << "nullptr";
    }
    std::cerr << "\n";

    llvm::AllocaInst *alloca = m_Builder.CreateAlloca(type, nullptr, var->Name);

    m_NamedValues[var->Name] = alloca;
    m_ValueTypes[var->Name] = type;
    m_ValueElementTypes[var->Name] = elemTy;
    m_ValueIsReference[var->Name] = var->IsReference;
    m_ValueIsRawPointer[var->Name] =
        (var->HasPointer && !var->IsUnique && !var->IsShared);
    m_ValueIsMutable[var->Name] = var->IsMutable;
    m_ValueIsUnique[var->Name] = var->IsUnique;
    m_ValueIsShared[var->Name] = var->IsShared;

    if (!m_ScopeStack.empty()) {
      m_ScopeStack.back().push_back(
          {var->Name, alloca, var->IsUnique, var->IsShared});
    }

    if (var->Init && initVal) {
      if (initVal->getType() != type) {
        if (initVal->getType()->isPointerTy() && type->isPointerTy()) {
          initVal = m_Builder.CreateBitCast(initVal, type);
        } else if (initVal->getType()->isPointerTy() && !type->isPointerTy()) {
          initVal = m_Builder.CreateLoad(type, initVal);
        } else {
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
      m_Builder.CreateStore(initVal, alloca);
    }
  } else if (auto *ifs = dynamic_cast<const IfStmt *>(stmt)) {
    llvm::Value *cond = genExpr(ifs->Condition.get());
    if (!cond)
      return nullptr;
    if (cond->getType()->isIntegerTy(1)) {
      // Already a boolean i1, no need to compare with 0
    } else if (cond->getType()->isIntegerTy() ||
               cond->getType()->isPointerTy()) {
      cond = m_Builder.CreateICmpNE(
          cond, llvm::ConstantInt::get(cond->getType(), 0), "ifcond");
    } else {
      error(ifs, "If condition must be a scalar value");
      return nullptr;
    }
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
    if (cond->getType()->isIntegerTy(1)) {
      // Already a boolean
    } else if (cond->getType()->isIntegerTy() ||
               cond->getType()->isPointerTy()) {
      cond = m_Builder.CreateICmpNE(
          cond, llvm::ConstantInt::get(cond->getType(), 0), "whilecond");
    } else {
      error(ws, "While condition must be a scalar value");
      return nullptr;
    }
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
      std::cerr
          << "[DEBUG] BlockStmt cleanup: Entering cleanup loop. Scope size="
          << scope.size() << "\n";
      for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
        std::cerr << "[DEBUG] Cleanup iteration: Name=" << it->Name
                  << ", IsShared=" << it->IsShared
                  << ", IsUnique=" << it->IsUniquePointer << "\n";
        if (it->IsShared && it->Alloca) {
          llvm::Type *shTy =
              llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType();
          if (shTy->isStructTy()) {
            llvm::Value *sh = m_Builder.CreateLoad(shTy, it->Alloca, "sh_pop");
            llvm::Value *refPtr =
                m_Builder.CreateExtractValue(sh, 1, "ref_ptr");
            llvm::Value *count = m_Builder.CreateLoad(
                llvm::Type::getInt32Ty(m_Context), refPtr, "ref_count");
            llvm::Value *dec = m_Builder.CreateSub(
                count,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
            m_Builder.CreateStore(dec, refPtr);

            std::cerr << "[DEBUG] Shared cleanup: About to CreateICmpEQ for "
                         "ref count. dec=";
            dec->print(llvm::errs());
            std::cerr << ", constant=i32 0\n";
            llvm::Value *isZero = m_Builder.CreateICmpEQ(
                dec,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));
            llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
            llvm::BasicBlock *freeBB =
                llvm::BasicBlock::Create(m_Context, "sh_free", f);
            llvm::BasicBlock *contBB =
                llvm::BasicBlock::Create(m_Context, "sh_cont", f);

            m_Builder.CreateCondBr(isZero, freeBB, contBB);
            m_Builder.SetInsertPoint(freeBB);
            llvm::Function *freeFunc = m_Module->getFunction("free");
            if (freeFunc) {
              llvm::Value *data =
                  m_Builder.CreateExtractValue(sh, 0, "data_ptr");
              m_Builder.CreateCall(
                  freeFunc, m_Builder.CreateBitCast(
                                data, llvm::PointerType::getUnqual(
                                          llvm::Type::getInt8Ty(m_Context))));
              m_Builder.CreateCall(
                  freeFunc, m_Builder.CreateBitCast(
                                refPtr, llvm::PointerType::getUnqual(
                                            llvm::Type::getInt8Ty(m_Context))));
            }
            m_Builder.CreateBr(contBB);
            m_Builder.SetInsertPoint(contBB);
          }
        } else if (it->IsUniquePointer && it->Alloca) {
          llvm::Value *ptr = m_Builder.CreateLoad(
              llvm::cast<llvm::AllocaInst>(it->Alloca)->getAllocatedType(),
              it->Alloca);
          llvm::Value *notNull = m_Builder.CreateIsNotNull(ptr, "not_null");
          llvm::Function *f = m_Builder.GetInsertBlock()->getParent();
          llvm::BasicBlock *freeBB =
              llvm::BasicBlock::Create(m_Context, "un_free", f);
          llvm::BasicBlock *contBB =
              llvm::BasicBlock::Create(m_Context, "un_cont", f);
          m_Builder.CreateCondBr(notNull, freeBB, contBB);
          m_Builder.SetInsertPoint(freeBB);
          llvm::Function *freeFunc = m_Module->getFunction("free");
          if (freeFunc) {
            m_Builder.CreateCall(
                freeFunc, m_Builder.CreateBitCast(
                              ptr, llvm::PointerType::getUnqual(
                                       llvm::Type::getInt8Ty(m_Context))));
          }
          m_Builder.CreateBr(contBB);
          m_Builder.SetInsertPoint(contBB);
        }
      }
    }
    m_ScopeStack.pop_back();
    return nullptr;

  } else if (auto *ms = dynamic_cast<const MatchStmt *>(stmt)) {
    return genMatch(ms);
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
  if (dynamic_cast<const NullExpr *>(expr)) {
    // Return a null pointer. Since LLVM 15+ uses opaque pointers, a single
    // "ptr" type handles all. If using older LLVM, we might need i8* null. Toka
    // uses opaque pointers where possible? Step 272 view shows
    // m_Builder.CreateIsNotNull. Let's assume PointerType::get(m_Context, 0) is
    // valid.
    return llvm::ConstantPointerNull::get(llvm::PointerType::get(m_Context, 0));
  }
  if (auto *deref = dynamic_cast<const DereferenceExpr *>(expr)) {
    // Toka Morphology: *expr returns the handle/pointer identity (the
    // address)
    return genAddr(deref->Expression.get());
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
    } else if (unary->Op == TokenType::Caret || unary->Op == TokenType::Tilde ||
               unary->Op == TokenType::Star) {
      if (auto *v = dynamic_cast<const VariableExpr *>(unary->RHS.get())) {
        llvm::Value *alloca = m_NamedValues[v->Name];
        if (alloca) {
          llvm::Value *handle = m_Builder.CreateLoad(
              m_ValueTypes[v->Name], alloca, v->Name + "_handle");
          // If the variable is Shared/Unique and we ask for Star (*), extract
          // only the data pointer.
          if (unary->Op == TokenType::Star &&
              (m_ValueIsShared[v->Name] || m_ValueIsUnique[v->Name])) {
            return m_Builder.CreateExtractValue(handle, 0,
                                                v->Name + "_raw_ptr");
          }
          return handle;
        }
      }
      return rhs;
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
  if (auto *newExpr = dynamic_cast<const NewExpr *>(expr)) {
    llvm::Type *ty = resolveType(newExpr->Type, false);
    if (!ty) {
      error(newExpr, "Unknown type in new expression");
      return nullptr;
    }
    llvm::Value *mallocCall = llvm::CallInst::CreateMalloc(
        m_Builder.GetInsertBlock(), llvm::Type::getInt64Ty(m_Context), ty,
        llvm::ConstantExpr::getSizeOf(ty), nullptr, nullptr, "");
    m_Builder.Insert(mallocCall);
    if (newExpr->Initializer) {
      // For now ignore initializer logic for malloc or handle struct init
      // We assume InitStructExpr is handled separately but if we have new
      // Struct { ... } We need to store it to malloc'd memory
      llvm::Value *initVal = genExpr(newExpr->Initializer.get());
      m_Builder.CreateStore(initVal, mallocCall);
    }
    return mallocCall;
  }

  if (auto *mc = dynamic_cast<const MethodCallExpr *>(expr)) {
    return genMethodCall(mc);
  }

  if (auto *call = dynamic_cast<const CallExpr *>(expr)) {
    llvm::Function *callee = m_Module->getFunction(call->Callee);
    if (!callee) {
      // Check for ADT Constructor (Type::Variant)
      // Format: OptionName::VariantName
      std::string callName = call->Callee;
      size_t delim = callName.find("::");
      if (delim != std::string::npos) {
        std::string optName = callName.substr(0, delim);
        std::string varName = callName.substr(delim + 2);

        if (m_Options.count(optName)) {
          const OptionDecl *opt = m_Options[optName];
          int tag = -1;
          const OptionVariant *targetVar = nullptr;
          for (int i = 0; i < opt->Variants.size(); ++i) {
            if (opt->Variants[i].Name == varName) {
              tag = i;
              targetVar = &opt->Variants[i];
              break;
            }
          }

          if (tag != -1) {
            // Generate Option Construction
            llvm::StructType *st = m_StructTypes[optName];
            llvm::Value *alloca =
                m_Builder.CreateAlloca(st, nullptr, "opt_ctor");

            // 1. Store Tag
            llvm::Value *tagAddr =
                m_Builder.CreateStructGEP(st, alloca, 0, "tag_addr");
            m_Builder.CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), tag),
                tagAddr);

            // 2. Store Payload Fields
            if (!targetVar->Fields.empty()) {
              // Get payload array address (index 1)
              llvm::Value *payloadAddr =
                  m_Builder.CreateStructGEP(st, alloca, 1, "payload_addr");

              // Bitcast payload addr to first field pointer?
              // Current simple implementation: assume sequential packing in
              // payload memory But since payload is [N x i8], we need to cast
              // it for each field. Actually, we should recalculate offsets
              // manually or cast payload* to a packed struct of fields?
              // Easiest is to cast payloadAddr to FieldType* and store.

              // NOTE: This assumes standard alignment padding is manually
              // handled or sufficient? Since we treat payload as opaque
              // bytes, we should ideally construct a "Variant Type" on the
              // fly and cast the payload pointer to that.

              std::vector<llvm::Type *> varFieldTypes;
              for (const auto &f : targetVar->Fields) {
                varFieldTypes.push_back(
                    resolveType(f.Type, f.HasPointer || f.IsReference));
              }
              llvm::StructType *varStructTy = llvm::StructType::get(
                  m_Context, varFieldTypes,
                  false); // Packed? No, use natural alignment

              // Cast payload address to VariantStruct*
              llvm::Value *varPtr = m_Builder.CreateBitCast(
                  payloadAddr, llvm::PointerType::getUnqual(varStructTy),
                  "var_ptr");

              for (size_t i = 0; i < call->Args.size(); ++i) {
                if (i >= targetVar->Fields.size())
                  break;

                llvm::Value *argVal = genExpr(call->Args[i].get());
                if (!argVal)
                  return nullptr;

                llvm::Value *dstPtr =
                    m_Builder.CreateStructGEP(varStructTy, varPtr, i);
                m_Builder.CreateStore(argVal, dstPtr);
              }
            }

            return m_Builder.CreateLoad(st, alloca);
          }
        }
      }

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
    llvm::Value *addr = genAddr(var);
    if (!addr)
      return nullptr;
    return m_Builder.CreateLoad(m_ValueElementTypes[var->Name], addr,
                                var->Name);
  }
  if (auto *bin = dynamic_cast<const BinaryExpr *>(expr)) {
    std::cerr << "[DEBUG] BinaryExpr Op: " << bin->Op << "\n";
    if (bin->Op == "=" || bin->Op == "+=" || bin->Op == "-=" ||
        bin->Op == "*=" || bin->Op == "/=") {
      std::cerr << "[DEBUG] Assignment detected\n";
      llvm::Value *ptr = genAddr(bin->LHS.get());
      if (!ptr) {
        std::cerr << "[DEBUG] genAddr(LHS) failed\n";
        return nullptr;
      }
      std::string s;
      llvm::raw_string_ostream os(s);
      ptr->getType()->print(os);
      std::cerr << "[DEBUG] LHS Addr Type: " << os.str() << "\n";

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
      } else if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(ptr)) {
        destType = ai->getAllocatedType();
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

      if (bin->Op == "=") {
        // Direct assignment to Soul. Auto-dereferencing is handled by
        // genAddr. No ownership/reference counting logic belongs here.
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
      llvm::BasicBlock *MergeBB =
          llvm::BasicBlock::Create(m_Context, "land.end");

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
      llvm::BasicBlock *MergeBB =
          llvm::BasicBlock::Create(m_Context, "lor.end");

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
    std::cerr << "[DEBUG] After genExpr(LHS), lhs=" << lhs << "\n";
    llvm::Value *rhs = genExpr(bin->RHS.get());
    std::cerr << "[DEBUG] After genExpr(RHS), rhs=" << rhs << "\n";
    if (!lhs || !rhs)
      return nullptr;

    std::string s1, s2;
    llvm::raw_string_ostream os1(s1), os2(s2);
    std::cerr << "[DEBUG] About to call lhs->getType()...\n";
    llvm::Type *lhsType = lhs->getType();
    std::cerr << "[DEBUG] Got lhsType=" << lhsType << "\n";
    lhsType->print(os1);
    std::cerr << "[DEBUG] About to call rhs->getType()...\n";
    llvm::Type *rhsType = rhs->getType();
    std::cerr << "[DEBUG] Got rhsType=" << rhsType << "\n";
    rhsType->print(os2);
    std::cerr << "[DEBUG] ICmp Operands: LHS=" << os1.str()
              << ", RHS=" << os2.str() << "\n";

    if (lhs->getType() != rhs->getType()) {
      if (lhs->getType()->isPointerTy() && rhs->getType()->isPointerTy()) {
        rhs = m_Builder.CreateBitCast(rhs, lhs->getType());
      } else {
        error(bin, "Type mismatch in binary expression: " + os1.str() + " vs " +
                       os2.str());
        return nullptr;
      }
    }

    // Temporarily commented out - crash happens before this
    /*
    std::cerr << "[DEBUG] Pre-type-check. LHS: ";
    lhs->print(llvm::errs());
    std::cerr << "\n[DEBUG] Getting  LHS type pointer...\n";
    llvm::Type* lhsType2 = lhs->getType();
    std::cerr << "[DEBUG] LHS type pointer: " << lhsType2 << "\n";
    std::cerr << "[DEBUG] Calling isIntegerTy...\n";
    bool isInt = lhsType2->isIntegerTy();
    std::cerr << "[DEBUG] isIntegerTy=" << isInt << "\n";
    std::cerr << "[DEBUG] Calling isIntOrIntVectorTy...\n";
    bool isIntOrVec = lhsType2->isIntOrIntVectorTy();
    std::cerr << "[DEBUG] isIntOrIntVectorTy=" << isIntOrVec << "\n";
    if (!isIntOrVec && !lhsType2->isPtrOrPtrVectorTy()) {
      std::string s;
      llvm::raw_string_ostream os(s);
      lhsType2->print(os);
      error(bin, "Invalid type for comparison: " + os.str() +
                     ". Comparisons are only allowed for scalars "
                     "(integers/pointers).");
      return nullptr;
    }
    */

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

    if (bin->Op == "+")
      return m_Builder.CreateAdd(lhs, rhs, "addtmp");
    if (bin->Op == "-")
      return m_Builder.CreateSub(lhs, rhs, "subtmp");
    if (bin->Op == "*")
      return m_Builder.CreateMul(lhs, rhs, "multmp");
    if (bin->Op == "/")
      return m_Builder.CreateSDiv(lhs, rhs, "divtmp");
    if (bin->Op == "<") {
      std::cerr << "[DEBUG] About to create ICmpSLT. LHS: ";
      lhs->print(llvm::errs());
      std::cerr << ", RHS: ";
      rhs->print(llvm::errs());
      std::cerr << "\n";
      return m_Builder.CreateICmpSLT(lhs, rhs, "lt_tmp");
    }
    if (bin->Op == ">") {
      std::cerr << "[DEBUG] About to create ICmpSGT. LHS: ";
      lhs->print(llvm::errs());
      std::cerr << ", RHS: ";
      rhs->print(llvm::errs());
      std::cerr << "\n";
      return m_Builder.CreateICmpSGT(lhs, rhs, "gt_tmp");
    }
    if (bin->Op == "==") {
      // Check if current BasicBlock already has a terminator
      llvm::BasicBlock *curBB = m_Builder.GetInsertBlock();
      if (curBB && curBB->getTerminator()) {
        std::cerr << "[DEBUG] CRITICAL: BasicBlock already has terminator "
                     "before ICmp!\n";
        // Create a new unreachable block for the orphaned comparison
        llvm::Function *f = curBB->getParent();
        llvm::BasicBlock *newBB =
            llvm::BasicBlock::Create(m_Context, "unreachable_cmp", f);
        m_Builder.SetInsertPoint(newBB);
      }
      if (lhs->getType() != rhs->getType()) {
        std::cerr << "[DEBUG] Type mismatch detected. Attempting cast...\n";
        if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
          if (lhs->getType()->getIntegerBitWidth() >
              rhs->getType()->getIntegerBitWidth())
            rhs = m_Builder.CreateZExt(rhs, lhs->getType());
          else
            lhs = m_Builder.CreateZExt(lhs, rhs->getType());
        }
      }
      std::cerr << "[DEBUG] About to create ICmp. Dumping operands:\n";
      std::cerr << "[DEBUG] LHS: ";
      lhs->print(llvm::errs());
      std::cerr << "\n[DEBUG] RHS: ";
      rhs->print(llvm::errs());
      std::cerr << "\n";
      return m_Builder.CreateICmpEQ(lhs, rhs, "eq_tmp");
    }
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
        std::string typeStr;
        llvm::raw_string_ostream typeOS(typeStr);
        fieldType->print(typeOS);
        std::cerr << "[DEBUG] MemberExpr Load: field=" << mem->Member
                  << ", type=" << typeOS.str() << "\n";
        llvm::Value *loaded =
            m_Builder.CreateLoad(fieldType, addr, mem->Member);
        std::cerr << "[DEBUG] Load result: " << loaded
                  << ", type=" << loaded->getType() << "\n";
        std::cerr << "[DEBUG] Type check: isIntegerTy="
                  << loaded->getType()->isIntegerTy() << ", isIntOrIntVectorTy="
                  << loaded->getType()->isIntOrIntVectorTy() << "\n";
        return loaded;
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
    llvm::Value *alloca = m_NamedValues[var->Name];
    if (alloca) {
      if (m_ValueIsShared[var->Name]) {
        llvm::Value *sh = m_Builder.CreateLoad(m_ValueTypes[var->Name], alloca,
                                               var->Name + "_handle");
        // Extract data pointer (Index 0)
        llvm::Value *soulPtr =
            m_Builder.CreateExtractValue(sh, 0, var->Name + "_ptr");
        // Re-cast to the actual soul type pointer to be safe for GEP
        return m_Builder.CreateBitCast(
            soulPtr,
            llvm::PointerType::getUnqual(m_ValueElementTypes[var->Name]));
      } else if (m_ValueIsUnique[var->Name] || m_ValueIsReference[var->Name] ||
                 m_ValueIsRawPointer[var->Name]) {
        return m_Builder.CreateLoad(m_ValueTypes[var->Name], alloca,
                                    var->Name + "_val");
      }
      return alloca;
    }
  }
  if (auto *deref = dynamic_cast<const DereferenceExpr *>(expr)) {
    // Toka Morphology: *ptr is the pointer storage
    if (auto *v = dynamic_cast<const VariableExpr *>(deref->Expression.get())) {
      return m_NamedValues[v->Name];
    }
    return genAddr(deref->Expression.get());
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
    llvm::Value *objAddr = nullptr;
    llvm::Type *objType = nullptr;

    if (auto *objVar = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
      objAddr = genAddr(objVar);
      objType = m_ValueElementTypes[objVar->Name];
    } else {
      objAddr = mem->IsArrow ? genExpr(mem->Object.get())
                             : genAddr(mem->Object.get());
      if (objAddr) {
        // Try to infer type from objAddr
        if (auto *ptrTy =
                llvm::dyn_cast<llvm::PointerType>(objAddr->getType())) {
          // In Opaque Pointers, we often need the type from somewhere else.
          // But if it's a GEP, we can get it.
          if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objAddr)) {
            objType = gep->getResultElementType();
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

    if (!st) {
      // Fallback: Find struct by field name
      std::string foundStruct;
      for (const auto &pair : m_StructFieldNames) {
        for (int i = 0; i < (int)pair.second.size(); ++i) {
          if (pair.second[i] == mem->Member) {
            foundStruct = pair.first;
            idx = i;
            break;
          }
        }
        if (!foundStruct.empty())
          break;
      }
      if (!foundStruct.empty())
        st = m_StructTypes[foundStruct];
    }

    if (!st)
      return nullptr;

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
          if (fields[i] == mem->Member) {
            idx = i;
            break;
          }
        }
      }
    }

    if (idx == -1)
      return nullptr;

    return m_Builder.CreateStructGEP(st, objAddr, idx, mem->Member);
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

  if (baseType == "Self") {
    if (m_CurrentSelfType.empty()) {
      // Should not happen if Parser checks context, but for safety in CodeGen
      return nullptr;
    }
    return resolveType(m_CurrentSelfType, hasPointer);
  }

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
  else if (m_Options.count(baseType))
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

void CodeGen::genOption(const OptionDecl *opt) {
  uint64_t maxPayloadSize = 0;
  uint64_t maxAlignment = 1;

  llvm::DataLayout DL(m_Module.get());

  for (const auto &variant : opt->Variants) {
    uint64_t currentSize = 0;
    uint64_t currentAlign = 1;
    for (const auto &field : variant.Fields) {
      llvm::Type *t =
          resolveType(field.Type, field.HasPointer || field.IsUnique ||
                                      field.IsShared || field.IsReference);
      if (!t) {
        error(opt, "Internal Error: Could not resolve type '" + field.Type +
                       "' for option variant field");
        continue;
      }
      currentSize += DL.getTypeAllocSize(t);
      currentAlign =
          std::max(currentAlign, (uint64_t)DL.getABITypeAlign(t).value());
    }
    maxPayloadSize = std::max(maxPayloadSize, currentSize);
    maxAlignment = std::max(maxAlignment, currentAlign);
  }

  // Tag is i32, 4 bytes
  std::vector<llvm::Type *> body;
  body.push_back(llvm::Type::getInt32Ty(m_Context)); // Tag
  if (maxPayloadSize > 0) {
    // We use a byte array for the payload to be safe about alignment/size
    body.push_back(
        llvm::ArrayType::get(llvm::Type::getInt8Ty(m_Context), maxPayloadSize));
  }

  llvm::StructType *st = llvm::StructType::create(m_Context, opt->Name);
  st->setBody(body, /*isPacked=*/false);

  m_StructTypes[opt->Name] = st;
  m_TypeToName[st] = opt->Name;
  m_Options[opt->Name] = opt;
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

llvm::Value *CodeGen::genMatch(const MatchStmt *stmt) {
  if (!stmt->Target)
    return nullptr;

  llvm::Value *targetVal = genExpr(stmt->Target.get());
  if (!targetVal)
    return nullptr;

  std::string optName;
  llvm::Type *targetType = targetVal->getType();

  // Try to determine option name from type
  if (targetType->isStructTy()) {
    if (m_TypeToName.count(targetType)) {
      optName = m_TypeToName[targetType];
    }
  }

  if (optName.empty() || m_Options.find(optName) == m_Options.end()) {
    // If unresolved, it might be an opaque pointer or similar.
    // For now, strict check.
    error(stmt, "Match target is not a resolvable Option type");
    return nullptr;
  }

  const OptionDecl *opt = m_Options[optName];

  // 1. Extract Tag (Index 0)
  llvm::Value *tagVal = m_Builder.CreateExtractValue(targetVal, 0, "tag");

  // 2. Store target to temp alloca for payload addressing
  llvm::Value *targetAlloca =
      m_Builder.CreateAlloca(targetType, nullptr, "match_tmp");
  m_Builder.CreateStore(targetVal, targetAlloca);

  llvm::Function *func = m_Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(m_Context, "match_merge", func);
  llvm::BasicBlock *defaultBB =
      llvm::BasicBlock::Create(m_Context, "match_default", func);

  llvm::SwitchInst *sw =
      m_Builder.CreateSwitch(tagVal, defaultBB, stmt->Cases.size());

  // Track handled tags to avoid duplicates or logic errors (simplified)
  bool wildcardHandled = false;

  for (const auto &c : stmt->Cases) {
    if (c.IsWildcard) {
      wildcardHandled = true;
      continue;
    }

    int tag = -1;
    const OptionVariant *variant = nullptr;
    for (size_t i = 0; i < opt->Variants.size(); ++i) {
      if (opt->Variants[i].Name == c.VariantName) {
        tag = i;
        variant = &opt->Variants[i];
        break;
      }
    }

    if (tag == -1) {
      error(stmt, "Unknown variant " + c.VariantName);
      continue;
    }

    llvm::BasicBlock *caseBB =
        llvm::BasicBlock::Create(m_Context, "case_" + c.VariantName, func);
    sw->addCase(m_Builder.getInt32(tag), caseBB);

    m_Builder.SetInsertPoint(caseBB);
    m_ScopeStack.push_back({});

    // Destructuring Bindings
    if (!c.BindingNames.empty() && variant) {
      llvm::Value *payloadAddr =
          m_Builder.CreateStructGEP(targetType, targetAlloca, 1);

      // Recreate Variant Struct Type
      std::vector<llvm::Type *> fieldTypes;
      llvm::DataLayout DL(m_Module.get());
      for (const auto &f : variant->Fields) {
        llvm::Type *t = resolveType(f.Type, f.HasPointer || f.IsUnique ||
                                                f.IsShared || f.IsReference);
        if (t)
          fieldTypes.push_back(t);
        else {
          error(stmt, "Internal Error: Could not resolve type '" + f.Type +
                          "' in match binding");
        }
      }

      if (!fieldTypes.empty()) {
        llvm::StructType *varSt =
            llvm::StructType::get(m_Context, fieldTypes, false);
        // BitCast Payload Addr (i8*) to VariantStruct*
        llvm::Value *castPtr = m_Builder.CreateBitCast(
            payloadAddr, llvm::PointerType::getUnqual(varSt));

        for (size_t i = 0; i < c.BindingNames.size() && i < fieldTypes.size();
             ++i) {
          // GEP to field
          llvm::Value *fieldPtr = m_Builder.CreateStructGEP(varSt, castPtr, i);
          llvm::Value *val =
              m_Builder.CreateLoad(fieldTypes[i], fieldPtr, c.BindingNames[i]);

          // Local var
          llvm::Value *alloc =
              m_Builder.CreateAlloca(fieldTypes[i], nullptr, c.BindingNames[i]);
          m_Builder.CreateStore(val, alloc);

          m_NamedValues[c.BindingNames[i]] = alloc;
          m_ValueTypes[c.BindingNames[i]] = fieldTypes[i];
          m_ValueElementTypes[c.BindingNames[i]] = fieldTypes[i];

          VariableScopeInfo info;
          info.Name = c.BindingNames[i];
          info.Alloca = alloc;
          m_ScopeStack.back().push_back(info);
        }
      }
    }

    genStmt(c.Body.get());
    m_ScopeStack.pop_back(); // Simple scope pop

    if (!m_Builder.GetInsertBlock()->getTerminator())
      m_Builder.CreateBr(mergeBB);
  }

  // Default / Wildcard Logic
  m_Builder.SetInsertPoint(defaultBB);
  const MatchCase *wild = nullptr;
  for (const auto &c : stmt->Cases)
    if (c.IsWildcard) {
      wild = &c;
      break;
    }

  if (wild) {
    m_ScopeStack.push_back({});
    genStmt(wild->Body.get());
    m_ScopeStack.pop_back();
    if (!m_Builder.GetInsertBlock()->getTerminator())
      m_Builder.CreateBr(mergeBB);
  } else {
    // If no wildcard, assume it's exhaustive or do nothing?
    // Jumping to mergeBB if unhandled (silent failure/nop currently)
    m_Builder.CreateBr(mergeBB);
  }

  m_Builder.SetInsertPoint(mergeBB);
  return nullptr;
}

void toka::CodeGen::genImpl(const toka::ImplDecl *decl, bool declOnly) {
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
  if (!decl->TraitName.empty() && m_AST) {
    const TraitDecl *trait = nullptr;
    for (const auto &t : m_AST->Traits) {
      if (t->Name == decl->TraitName) {
        trait = t.get();
        break;
      }
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
  }

  m_CurrentSelfType = "";
}

llvm::Value *toka::CodeGen::genMethodCall(const toka::MethodCallExpr *expr) {
  llvm::Value *objVal = genExpr(expr->Object.get());
  if (!objVal)
    return nullptr;

  llvm::Type *ty = objVal->getType();
  llvm::Type *structTy = nullptr;

  if (ty->isStructTy()) {
    structTy = ty;
  } else {
    // Try to identify if it is a pointer to struct
    // 1. Check if it's Alloca
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(objVal)) {
      if (ai->getAllocatedType()->isStructTy())
        structTy = ai->getAllocatedType();
    }
    // 2. Check GEP
    else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objVal)) {
      if (gep->getResultElementType()->isStructTy())
        structTy = gep->getResultElementType();
    }

    // 3. VariableExpr lookup
    if (!structTy) {
      if (auto *ve = dynamic_cast<const VariableExpr *>(expr->Object.get())) {
        if (m_ValueElementTypes.count(ve->Name)) {
          structTy = m_ValueElementTypes[ve->Name];
        }
      }
    }
  }

  // Fallback: Check if it is a NewExpr
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

  // Check Traits if inherent not found
  if (!callee && m_AST) {
    for (const auto &trait : m_AST->Traits) {
      std::string traitFunc = trait->Name + "_" + typeName + "_" + expr->Method;
      callee = m_Module->getFunction(traitFunc);
      if (callee)
        break;
    }
  }

  if (!callee) {
    error(expr, "Method '" + expr->Method + "' not found for type '" +
                    typeName + "' (Mangled: " + funcName + ")");
    return nullptr;
  }

  std::vector<llvm::Value *> args;
  // Pass self (Pointer)
  if (ty->isStructTy()) {
    // Value -> Stack -> Pointer
    llvm::Value *tmp = m_Builder.CreateAlloca(ty);
    m_Builder.CreateStore(objVal, tmp);
    args.push_back(tmp);
  } else {
    args.push_back(objVal);
  }

  for (const auto &arg : expr->Args) {
    args.push_back(genExpr(arg.get()));
  }

  return m_Builder.CreateCall(callee, args);
}
} // namespace toka
