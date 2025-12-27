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
  }

  // Generate Externs
  for (const auto &ext : ast.Externs) {
    m_Externs[ext->Name] = ext.get();
    genExtern(ext.get());
  }

  // Generate Globals
  for (const auto &glob : ast.Globals) {
    genGlobal(glob.get());
  }

  // Generate Functions (decl phase)
  for (const auto &func : ast.Functions) {
    m_Functions[func->Name] = func.get();
  }

  // Generate Functions (body phase)
  for (const auto &func : ast.Functions) {
    genFunction(func.get());
  }
}

void CodeGen::print(llvm::raw_ostream &os) { m_Module->print(os, nullptr); }

void CodeGen::genGlobal(const VariableDecl *var) {
  llvm::Type *type = resolveType(var->TypeName, var->HasPointer);

  auto *globalVar = new llvm::GlobalVariable(*m_Module, type, false,
                                             llvm::GlobalValue::ExternalLinkage,
                                             nullptr, var->Name);

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
}

llvm::Function *CodeGen::genFunction(const FunctionDecl *func) {
  llvm::Function *f = m_Module->getFunction(func->Name);

  if (!f) {
    std::vector<llvm::Type *> argTypes;
    for (const auto &arg : func->Args) {
      llvm::Type *t = resolveType(arg.Type, arg.HasPointer);
      if (arg.IsMutable)
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

  size_t idx = 0;
  for (auto &arg : f->args()) {
    arg.setName(func->Args[idx].Name);
    llvm::Type *baseType =
        resolveType(func->Args[idx].Type, func->Args[idx].HasPointer);

    if (func->Args[idx].IsMutable) {
      // Already a pointer (implicit reference)
      m_NamedValues[func->Args[idx].Name] = &arg;
    } else {
      // Pass by value, create local copy
      llvm::AllocaInst *alloca =
          m_Builder.CreateAlloca(baseType, nullptr, func->Args[idx].Name);
      m_Builder.CreateStore(&arg, alloca);
      m_NamedValues[func->Args[idx].Name] = alloca;
    }
    m_ValueTypes[func->Args[idx].Name] = baseType;

    // Set Element Type
    if (baseType->isArrayTy())
      m_ValueElementTypes[func->Args[idx].Name] =
          baseType->getArrayElementType();
    else if (baseType->isPointerTy()) {
      m_ValueElementTypes[func->Args[idx].Name] =
          llvm::Type::getInt8Ty(m_Context); // Fallback
    } else if (func->Args[idx].Type == "str") {
      m_ValueElementTypes[func->Args[idx].Name] =
          llvm::Type::getInt8Ty(m_Context);
    }

    idx++;
  }

  for (const auto &stmt : func->Body->Statements) {
    genStmt(stmt.get());
  }

  if (func->Name == "main" || func->ReturnType == "void") {
    if (!m_Builder.GetInsertBlock()->getTerminator())
      m_Builder.CreateRetVoid();
  }

  llvm::verifyFunction(*f);
  return f;
}

llvm::Value *CodeGen::genStmt(const Stmt *stmt) {
  if (auto *ret = dynamic_cast<const ReturnStmt *>(stmt)) {
    if (ret->ReturnValue) {
      llvm::Value *val = genExpr(ret->ReturnValue.get());
      if (val) {
        llvm::Type *retType =
            m_Builder.GetInsertBlock()->getParent()->getReturnType();
        if (val->getType()->isIntegerTy() && retType->isIntegerTy())
          val = m_Builder.CreateIntCast(val, retType, true);
        return m_Builder.CreateRet(val);
      }
    } else {
      return m_Builder.CreateRetVoid();
    }
  } else if (auto *var = dynamic_cast<const VariableDecl *>(stmt)) {
    llvm::Value *initVal = nullptr;
    if (var->Init) {
      initVal = genExpr(var->Init.get());
    }

    llvm::Type *type = nullptr;
    llvm::Type *elemTy = nullptr;
    if (!var->TypeName.empty()) {
      type = resolveType(var->TypeName, var->HasPointer);
      elemTy = resolveType(var->TypeName, false);
      if (var->TypeName == "str")
        elemTy = llvm::Type::getInt8Ty(m_Context);
    } else if (initVal) {
      type = initVal->getType();
      if (type->isArrayTy())
        elemTy = type->getArrayElementType();
      else if (type->isPointerTy()) {
        // Fallback for strings
        elemTy = llvm::Type::getInt8Ty(m_Context);
      }
    } else {
      type = llvm::Type::getInt64Ty(m_Context);
    }

    if (!type) {
      type = llvm::Type::getInt64Ty(m_Context);
    }

    llvm::AllocaInst *alloca = m_Builder.CreateAlloca(type, nullptr, var->Name);
    m_NamedValues[var->Name] = alloca;
    m_ValueTypes[var->Name] = type;
    m_ValueElementTypes[var->Name] = elemTy;

    if (initVal) {
      if (initVal->getType()->isIntegerTy() && type->isIntegerTy())
        initVal = m_Builder.CreateIntCast(initVal, type, true);
      m_Builder.CreateStore(initVal, alloca);
    }

    m_NamedValues[var->Name] = alloca;
    m_ValueTypes[var->Name] = type;
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
    for (const auto &s : bs->Statements)
      genStmt(s.get());
    return nullptr;
  } else if (auto *es = dynamic_cast<const ExprStmt *>(stmt)) {
    return genExpr(es->Expression.get());
  }
  return nullptr;
}

llvm::Value *CodeGen::genExpr(const Expr *expr) {
  if (auto *num = dynamic_cast<const NumberExpr *>(expr)) {
    return llvm::ConstantInt::get(m_Context, llvm::APInt(64, num->Value));
  }
  if (auto *flt = dynamic_cast<const FloatExpr *>(expr)) {
    return llvm::ConstantFP::get(m_Context, llvm::APFloat(flt->Value));
  }
  if (auto *bl = dynamic_cast<const BoolExpr *>(expr)) {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_Context), bl->Value);
  }
  if (auto *str = dynamic_cast<const StringExpr *>(expr)) {
    return m_Builder.CreateGlobalStringPtr(str->Value);
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
      if (funcDecl && i < funcDecl->Args.size())
        isMut = funcDecl->Args[i].IsMutable;
      else if (extDecl && i < extDecl->Args.size())
        isMut = extDecl->Args[i].IsMutable;

      if (isMut) {
        llvm::Value *addr = genAddr(call->Args[i].get());
        if (!addr)
          return nullptr;
        argsV.push_back(addr);
      } else {
        llvm::Value *val = genExpr(call->Args[i].get());
        if (!val)
          return nullptr;

        // Auto cast to i64 if needed for variadic or simple calls
        if (callee->isVarArg() &&
            i >= callee->getFunctionType()->getNumParams()) {
          if (val->getType()->isIntegerTy())
            val = m_Builder.CreateIntCast(
                val, llvm::Type::getInt64Ty(m_Context), true);
        } else if (i < callee->getFunctionType()->getNumParams()) {
          llvm::Type *paramType = callee->getFunctionType()->getParamType(i);
          if (val->getType()->isIntegerTy() && paramType->isIntegerTy())
            val = m_Builder.CreateIntCast(val, paramType, true);
        }

        argsV.push_back(val);
      }
    }
    return m_Builder.CreateCall(callee, argsV);
  }
  if (auto *var = dynamic_cast<const VariableExpr *>(expr)) {
    llvm::Value *v = m_NamedValues[var->Name];
    if (!v)
      return nullptr;
    if (llvm::isa<llvm::AllocaInst>(v) || llvm::isa<llvm::GlobalVariable>(v)) {
      return m_Builder.CreateLoad(m_ValueTypes[var->Name], v, var->Name);
    }
    return v;
  }
  if (auto *bin = dynamic_cast<const BinaryExpr *>(expr)) {
    if (bin->Op == "=" || bin->Op == "+=" || bin->Op == "-=" ||
        bin->Op == "*=" || bin->Op == "/=") {
      llvm::Value *ptr = genAddr(bin->LHS.get());
      if (!ptr)
        return nullptr;
      if (auto *varLHS = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
        if (!varLHS->IsMutable)
          return nullptr;
      }
      llvm::Value *rhsVal = genExpr(bin->RHS.get());
      if (!rhsVal)
        return nullptr;

      llvm::Type *destType = nullptr;
      if (auto *varLHS = dynamic_cast<const VariableExpr *>(bin->LHS.get())) {
        destType = m_ValueTypes[varLHS->Name];
      } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(ptr)) {
        destType = gep->getResultElementType();
      }

      if (bin->Op != "=") {
        // Compound: LHS = LHS op RHS
        llvm::Value *lhsVal = m_Builder.CreateLoad(destType, ptr);
        // Cast to i64 for calculation if needed
        if (lhsVal->getType()->isIntegerTy() &&
            rhsVal->getType()->isIntegerTy()) {
          lhsVal = m_Builder.CreateIntCast(
              lhsVal, llvm::Type::getInt64Ty(m_Context), true);
          rhsVal = m_Builder.CreateIntCast(
              rhsVal, llvm::Type::getInt64Ty(m_Context), true);
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

      if (destType && rhsVal->getType()->isIntegerTy() &&
          destType->isIntegerTy())
        rhsVal = m_Builder.CreateIntCast(rhsVal, destType, true);

      m_Builder.CreateStore(rhsVal, ptr);
      return rhsVal;
    }

    llvm::Value *lhs = genExpr(bin->LHS.get());
    llvm::Value *rhs = genExpr(bin->RHS.get());
    if (!lhs || !rhs)
      return nullptr;

    if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
      lhs =
          m_Builder.CreateIntCast(lhs, llvm::Type::getInt64Ty(m_Context), true);
      rhs =
          m_Builder.CreateIntCast(rhs, llvm::Type::getInt64Ty(m_Context), true);
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
      lhs = m_Builder.CreateICmpSLT(lhs, rhs, "cmptmp");
      return m_Builder.CreateIntCast(lhs, llvm::Type::getInt64Ty(m_Context),
                                     true);
    }
    if (bin->Op == ">") {
      lhs = m_Builder.CreateICmpSGT(lhs, rhs, "cmptmp");
      return m_Builder.CreateIntCast(lhs, llvm::Type::getInt64Ty(m_Context),
                                     true);
    }
    if (bin->Op == "==") {
      lhs = m_Builder.CreateICmpEQ(lhs, rhs, "eqtmp");
      return m_Builder.CreateIntCast(lhs, llvm::Type::getInt64Ty(m_Context),
                                     true);
    }
    if (bin->Op == "!=") {
      lhs = m_Builder.CreateICmpNE(lhs, rhs, "netmp");
      return m_Builder.CreateIntCast(lhs, llvm::Type::getInt64Ty(m_Context),
                                     true);
    }
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
  if (auto *mem = dynamic_cast<const MemberExpr *>(expr)) {
    llvm::Value *addr = genAddr(mem);
    if (!addr)
      return nullptr;
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
      if (fieldVal->getType()->isIntegerTy() &&
          st->getElementType(idx)->isIntegerTy())
        fieldVal =
            m_Builder.CreateIntCast(fieldVal, st->getElementType(idx), true);
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
    return m_NamedValues[var->Name];
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
    if (!objAddr) {
      llvm::Value *objVal = genExpr(mem->Object.get());
      if (!objVal)
        return nullptr;
      objAddr = m_Builder.CreateAlloca(objVal->getType());
      m_Builder.CreateStore(objVal, objAddr);
    }

    llvm::Type *objType = nullptr;
    if (auto *var = dynamic_cast<const VariableExpr *>(mem->Object.get())) {
      objType = m_ValueTypes[var->Name];
    } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objAddr)) {
      objType = gep->getResultElementType();
    }

    if (!objType || !objType->isStructTy())
      return nullptr;

    llvm::StructType *st = llvm::cast<llvm::StructType>(objType);
    int idx = -1;
    if (!mem->Member.empty() && isdigit(mem->Member[0])) {
      idx = std::stoi(mem->Member);
    } else {
      auto &fields = m_StructFieldNames[st->getName().str()];
      for (size_t i = 0; i < fields.size(); ++i)
        if (fields[i] == mem->Member) {
          idx = i;
          break;
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
    llvm::Type *t = resolveType(arg.Type, arg.HasPointer);
    if (arg.IsMutable)
      t = llvm::PointerType::getUnqual(t);
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
    // Very simple split by comma, not perfect for nested but works for now
    size_t start = 0;
    int depth = 0;
    for (size_t i = 0; i < content.size(); ++i) {
      if (content[i] == '(' || content[i] == '[')
        depth++;
      else if (content[i] == ')' || content[i] == ']')
        depth--;
      else if (content[i] == ',' && depth == 0) {
        elemTypes.push_back(
            resolveType(content.substr(start, i - start), false));
        start = i + 1;
      }
    }
    if (start < content.size())
      elemTypes.push_back(resolveType(content.substr(start), false));
    type = llvm::StructType::get(m_Context, elemTypes);
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
  m_StructFieldNames[str->Name] = fieldNames;
}

} // namespace toka
