#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>
#include <typeinfo>

namespace toka {

llvm::Function *CodeGen::genFunction(const FunctionDecl *func,
                                     const std::string &overrideName,
                                     bool declOnly) {
  std::string funcName = overrideName.empty() ? func->Name : overrideName;
  m_Functions[funcName] = func;
  m_ValueElementTypes.clear();
  m_Symbols.clear();

  llvm::Function *f = m_Module->getFunction(funcName);

  if (!f) {
    std::vector<llvm::Type *> argTypes;
    for (const auto &arg : func->Args) {
      llvm::Type *pTy = resolveType(arg.Type, false); // Logic Type
      bool isCaptured = (pTy && (pTy->isStructTy() || pTy->isArrayTy() ||
                                 arg.IsMutable || arg.IsRebindable)) &&
                        !arg.IsReference;
      llvm::Type *t = resolveType(arg.Type, arg.HasPointer || arg.IsReference);
      if (isCaptured)
        t = llvm::PointerType::getUnqual(pTy);
      if (t)
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
    const auto &argDecl = func->Args[idx];
    std::string argName = argDecl.Name;

    // 1. Strip morphology to get the base symbol name
    while (!argName.empty() &&
           (argName[0] == '*' || argName[0] == '#' || argName[0] == '&' ||
            argName[0] == '^' || argName[0] == '~' || argName[0] == '!'))
      argName = argName.substr(1);
    while (!argName.empty() && (argName.back() == '#' ||
                                argName.back() == '?' || argName.back() == '!'))
      argName.pop_back();

    arg.setName(argName);

    // 2. Determine types
    llvm::Type *pTy = resolveType(argDecl.Type, false); // Logic Type (Soul)
    if (!pTy) {
      error(func, "Internal Error: Could not resolve type '" + argDecl.Type +
                      "' for argument '" + argDecl.Name + "'");
      return nullptr;
    }

    // 3. Address Layering Flags
    bool isCaptured = (pTy->isStructTy() || pTy->isArrayTy() ||
                       argDecl.IsMutable || argDecl.IsRebindable) &&
                      !argDecl.IsReference;
    bool isExplicit = (argDecl.HasPointer || argDecl.IsUnique ||
                       argDecl.IsShared || argDecl.IsReference);

    // Identity slot: always a pointer to the value or pointer to the pointer
    llvm::Type *allocaType = llvm::PointerType::getUnqual(pTy);

    llvm::AllocaInst *alloca =
        m_Builder.CreateAlloca(allocaType, nullptr, argName + ".addr");
    m_Builder.CreateStore(&arg, alloca);

    // 4. Register in Symbol Table (Soul/Identity)
    TokaSymbol sym;
    sym.allocaPtr = alloca;
    fillSymbolMetadata(sym, argDecl.Type, argDecl.HasPointer, argDecl.IsUnique,
                       argDecl.IsShared, argDecl.IsReference,
                       argDecl.IsMutable || argDecl.IsValueMutable,
                       argDecl.IsNullable || argDecl.IsPointerNullable, pTy);
    m_ValueTypeNames[argName] = argDecl.Type;

    // CRITICAL: Captured arguments (Implicit Pointers) must use Pointer mode
    // so getEntityAddr loads the real address from the alloca.
    if (isCaptured) {
      sym.mode = AddressingMode::Pointer;
      if (sym.indirectionLevel == 0)
        sym.indirectionLevel = 1;
    }

    sym.isRebindable = argDecl.IsRebindable;
    sym.isContinuous = pTy->isArrayTy();
    m_Symbols[argName] = sym;

    // Backward compatibility maps
    m_ValueElementTypes[argName] = pTy;
    m_ValueTypes[argName] = allocaType;
    m_NamedValues[argName] = alloca;
    m_ValueIsMutable[argName] = sym.isMutable;

    if (!m_ScopeStack.empty()) {
      m_ScopeStack.back().push_back(
          {argName, alloca, argDecl.IsUnique, argDecl.IsShared});
    }

    idx++;
  }

  genStmt(func->Body.get());

  if (!m_Builder.GetInsertBlock()->getTerminator()) {
    if (func->ReturnType == "void" || func->Name == "main") {
      if (func->Name == "main" && !f->getReturnType()->isVoidTy()) {
        m_Builder.CreateRet(
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 0));
      } else {
        m_Builder.CreateRetVoid();
      }
    } else {
      m_Builder.CreateUnreachable();
    }
  }
  m_ScopeStack.pop_back();
  return f;
}

llvm::Value *CodeGen::genVariableDecl(const VariableDecl *var) {
  std::string varName = var->Name;
  while (!varName.empty() &&
         (varName[0] == '*' || varName[0] == '#' || varName[0] == '&' ||
          varName[0] == '^' || varName[0] == '~' || varName[0] == '!'))
    varName = varName.substr(1);
  while (!varName.empty() && (varName.back() == '#' || varName.back() == '?' ||
                              varName.back() == '!'))
    varName.pop_back();

  llvm::Value *initVal = nullptr;
  if (var->Init) {
    m_CFStack.push_back({varName, nullptr, nullptr, nullptr});
    initVal = genExpr(var->Init.get());
    m_CFStack.pop_back();
    if (!initVal)
      return nullptr;
  }

  llvm::Type *type = nullptr;
  if (!var->TypeName.empty()) {
    type = resolveType(var->TypeName, var->HasPointer || var->IsReference ||
                                          var->IsUnique || var->IsShared);
  } else if (initVal) {
    type = initVal->getType();
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
      if (auto *vae = dynamic_cast<const VariableExpr *>(ae->Expression.get()))
        elemTy = m_ValueElementTypes[vae->Name];
    } else if (auto *allocExpr =
                   dynamic_cast<const AllocExpr *>(var->Init.get())) {
      // auto *p = alloc Point(...) -> elemTy should be Point
      elemTy = resolveType(allocExpr->TypeName, false);
    } else if (auto *newExpr = dynamic_cast<const NewExpr *>(var->Init.get())) {
      elemTy = resolveType(newExpr->Type, false);
    } else if (auto *cast = dynamic_cast<const CastExpr *>(var->Init.get())) {
      std::string tn = cast->TargetType;
      while (!tn.empty() &&
             (tn[0] == '*' || tn[0] == '^' || tn[0] == '&' || tn[0] == '#'))
        tn = tn.substr(1);
      elemTy = resolveType(tn, false);
    } else if (auto *call = dynamic_cast<const CallExpr *>(var->Init.get())) {
      std::string retTypeName;
      if (m_Functions.count(call->Callee)) {
        retTypeName = m_Functions[call->Callee]->ReturnType;
      } else if (m_Externs.count(call->Callee)) {
        retTypeName = m_Externs[call->Callee]->ReturnType;
      }

      if (!retTypeName.empty()) {
        std::string tn = retTypeName;
        while (!tn.empty() &&
               (tn[0] == '*' || tn[0] == '^' || tn[0] == '&' || tn[0] == '#'))
          tn = tn.substr(1);
        elemTy = resolveType(tn, false);
      }
    } else if (initVal->getType()->isPointerTy()) {
      // Fallback: use the value type itself as elem
      elemTy = initVal->getType();
    }
  }

  if (!elemTy) {
    if (initVal)
      elemTy = initVal->getType();
    else
      elemTy = llvm::Type::getInt32Ty(m_Context);
  }

  // Ensure m_ValueElementTypes is set early
  m_ValueElementTypes[varName] = elemTy;
  m_ValueTypeNames[varName] = var->TypeName;

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
    error(var, "Cannot infer type for variable '" + varName + "'");
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
        // Stripping logic for unique variable lookup could be added here if
        // needed, but m_NamedValues should have stripped keys now.
        std::string veName = ve->Name;
        while (!veName.empty() &&
               (veName[0] == '*' || veName[0] == '#' || veName[0] == '&' ||
                veName[0] == '^' || veName[0] == '~' || veName[0] == '!'))
          veName = veName.substr(1);
        while (!veName.empty() &&
               (veName.back() == '#' || veName.back() == '?' ||
                veName.back() == '!'))
          veName.pop_back();

        if (m_ValueIsUnique[veName]) {
          llvm::Value *s = m_NamedValues[veName];
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
        llvm::Value *c = m_Builder.CreateLoad(llvm::Type::getInt32Ty(m_Context),
                                              ref, "ref_count");
        llvm::Value *inc = m_Builder.CreateAdd(
            c, llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_Context), 1));
        m_Builder.CreateStore(inc, ref);
      } else if (initVal->getType()->isPointerTy()) {
        if (llvm::isa<llvm::ConstantPointerNull>(initVal)) {
          // Null shared pointer initialization
          llvm::Type *ptrTy = llvm::PointerType::getUnqual(elemTy);
          llvm::Type *refTy =
              llvm::PointerType::getUnqual(llvm::Type::getInt32Ty(m_Context));
          llvm::StructType *st =
              llvm::StructType::get(m_Context, {ptrTy, refTy});
          llvm::Value *u = llvm::UndefValue::get(st);
          initVal = m_Builder.CreateInsertValue(
              m_Builder.CreateInsertValue(
                  u,
                  llvm::ConstantPointerNull::get(
                      llvm::cast<llvm::PointerType>(ptrTy)),
                  0),
              llvm::ConstantPointerNull::get(
                  llvm::cast<llvm::PointerType>(refTy)),
              1);
          type = st;
        } else {
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
  }

  llvm::AllocaInst *alloca = m_Builder.CreateAlloca(type, nullptr, varName);

  TokaSymbol sym;
  sym.allocaPtr = alloca;
  fillSymbolMetadata(sym, var->TypeName, var->HasPointer, var->IsUnique,
                     var->IsShared, var->IsReference, var->IsMutable,
                     var->IsNullable || var->IsPointerNullable, elemTy);
  sym.isRebindable = var->IsRebindable;
  sym.isContinuous =
      (elemTy && elemTy->isArrayTy()) ||
      (dynamic_cast<const AllocExpr *>(var->Init.get()) &&
       dynamic_cast<const AllocExpr *>(var->Init.get())->IsArray);
  m_Symbols[varName] = sym;

  m_NamedValues[varName] = alloca;
  m_ValueTypes[varName] = type;
  m_ValueIsUnique[varName] = var->IsUnique;
  m_ValueIsShared[varName] = var->IsShared;
  m_ValueIsReference[varName] = var->IsReference;
  m_ValueIsMutable[varName] = var->IsMutable;

  if (initVal) {
    if (initVal->getType() != type) {
      if (initVal->getType()->isPointerTy() && type->isPointerTy()) {
        initVal = m_Builder.CreateBitCast(initVal, type);
      } else if (initVal->getType()->isPointerTy() && !type->isPointerTy()) {
        initVal = m_Builder.CreateLoad(type, initVal);
      } else if (initVal->getType()->isIntegerTy() && type->isIntegerTy()) {
        initVal = m_Builder.CreateIntCast(initVal, type, true);
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
  return nullptr;
}

llvm::Value *CodeGen::genDestructuringDecl(const DestructuringDecl *dest) {
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
    std::string vName = v.Name;
    while (!vName.empty() &&
           (vName[0] == '*' || vName[0] == '#' || vName[0] == '&' ||
            vName[0] == '^' || vName[0] == '~' || vName[0] == '!'))
      vName = vName.substr(1);
    while (!vName.empty() &&
           (vName.back() == '#' || vName.back() == '?' || vName.back() == '!'))
      vName.pop_back();

    llvm::Value *val = m_Builder.CreateExtractValue(initVal, i, vName);
    llvm::Type *ty = val->getType();

    llvm::AllocaInst *alloca = m_Builder.CreateAlloca(ty, nullptr, vName);
    m_Builder.CreateStore(val, alloca);

    m_NamedValues[vName] = alloca;
    m_ValueTypes[vName] = ty;
    m_ValueElementTypes[vName] = ty; // fallback for basic types
    m_ValueIsMutable[vName] = v.IsMutable;
    m_ValueIsNullable[vName] = v.IsNullable;

    TokaSymbol sym;
    sym.allocaPtr = alloca;
    // For destructuring, metadata is often already flattened
    fillSymbolMetadata(sym, "", false, false, false, false, v.IsMutable,
                       v.IsNullable, ty);
    sym.isRebindable = false;
    sym.isContinuous = ty->isArrayTy();
    m_Symbols[vName] = sym;

    if (!m_ScopeStack.empty()) {
      m_ScopeStack.back().push_back({v.Name, alloca, false, false});
    }
  }
  return nullptr;
}

void CodeGen::genGlobal(const Stmt *stmt) {
  if (auto *var = dynamic_cast<const VariableDecl *>(stmt)) {
    llvm::Value *initVal = nullptr;
    if (var->Init) {
      // Try resolving type hint first
      llvm::Type *hintType = nullptr;
      if (!var->TypeName.empty()) {
        hintType = resolveType(var->TypeName, var->HasPointer);
      }

      // Try compile-time constant generation first (Critical for Anonymous
      // Records)
      if (auto *c = genConstant(var->Init.get(), hintType)) {
        initVal = c;
      } else {
        // Fallback to legacy genExpr (might crash if it uses instructions)
        initVal = genExpr(var->Init.get());
      }
    }

    llvm::Type *type = nullptr;
    if (!var->TypeName.empty()) {
      type = resolveType(var->TypeName, var->HasPointer);
    } else if (initVal) {
      type = initVal->getType();
    }

    if (!type) {
      // Potentially resolve via initVal if TypeName is empty
      if (initVal) {
        type = initVal->getType();
      }

      if (!type) {
        std::cerr << "DEBUG: genGlobal: Could not resolve type for '"
                  << var->Name << "' (TypeName: '" << var->TypeName << "')\n";
        type = llvm::Type::getInt32Ty(m_Context);
      }
    }

    auto *globalVar = new llvm::GlobalVariable(
        *m_Module, type, false, llvm::GlobalValue::ExternalLinkage, nullptr,
        var->Name);

    if (initVal) {
      if (auto *constInit = llvm::dyn_cast<llvm::Constant>(initVal)) {
        globalVar->setInitializer(constInit);
      } else {
        std::cerr << "DEBUG: genGlobal: Non-constant initializer for '"
                  << var->Name << "'\n";
        globalVar->setInitializer(llvm::ConstantInt::get(type, 0));
      }
    } else {
      globalVar->setInitializer(llvm::ConstantInt::get(type, 0));
    }

    m_NamedValues[var->Name] = globalVar;
    m_ValueTypes[var->Name] = type;

    TokaSymbol sym;
    sym.allocaPtr = globalVar;
    fillSymbolMetadata(sym, var->TypeName, var->HasPointer, var->IsUnique,
                       var->IsShared, var->IsReference, var->IsMutable,
                       var->IsNullable || var->IsPointerNullable, type);
    sym.isRebindable = var->IsRebindable;
    sym.isContinuous = type->isArrayTy();
    m_Symbols[var->Name] = sym;
  } else {
    // We could support global destructuring here, but for now just skip or
    // error
    error(dynamic_cast<const ASTNode *>(stmt),
          "Global destructuring not yet supported");
  }
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

  std::string llvmName = ext->Name;
  if (llvmName.size() > 5 && llvmName.substr(0, 5) == "libc_") {
    llvmName = llvmName.substr(5);
  }

  llvm::Function::Create(ft, llvm::Function::ExternalLinkage, llvmName,
                         m_Module.get());
}

void CodeGen::genShape(const ShapeDecl *sh) {
  llvm::StructType *st = llvm::StructType::create(m_Context, sh->Name);
  m_Shapes[sh->Name] = sh;
  m_StructTypes[sh->Name] = st;
  m_TypeToName[st] = sh->Name;

  std::vector<llvm::Type *> body;
  llvm::DataLayout DL(m_Module.get());

  if (sh->Kind == ShapeKind::Struct || sh->Kind == ShapeKind::Tuple) {
    std::vector<std::string> fieldNames;
    for (const auto &member : sh->Members) {
      body.push_back(
          resolveType(member.Type, member.HasPointer || member.IsUnique ||
                                       member.IsShared || member.IsReference));
      fieldNames.push_back(member.Name);
    }
    st->setBody(body, sh->IsPacked);
    m_StructFieldNames[sh->Name] = fieldNames;
  } else if (sh->Kind == ShapeKind::Array) {
    llvm::Type *elemTy = resolveType(sh->Members[0].Type, false);
    llvm::Type *arrTy = llvm::ArrayType::get(elemTy, sh->ArraySize);
    // For Array shapes, we wrap them in a struct for consistent GEP/naming if
    // needed, but usually Toka treats them as ArrayType. Let's wrap in a
    // struct so it's a named type.
    body.push_back(arrTy);
    st->setBody(body, sh->IsPacked);
  } else if (sh->Kind == ShapeKind::Union) {
    // Bare Union: find max size and alignment
    uint64_t maxSize = 0;
    uint64_t maxAlign = 1;
    for (const auto &member : sh->Members) {
      llvm::Type *t = resolveType(member.Type, false);
      if (!t)
        continue;
      maxSize =
          std::max(maxSize, (uint64_t)DL.getTypeAllocSize(t).getFixedValue());
      maxAlign = std::max(maxAlign, (uint64_t)DL.getABITypeAlign(t).value());
    }
    // Model as [maxSize x i8]
    body.push_back(
        llvm::ArrayType::get(llvm::Type::getInt8Ty(m_Context), maxSize));
    st->setBody(body, sh->IsPacked);
  } else if (sh->Kind == ShapeKind::Enum) {
    // Tagged Union: { i8 tag, [Payload] }
    uint64_t maxPayloadSize = 0;
    for (const auto &variant : sh->Members) {
      if (variant.Type.empty())
        continue;
      llvm::Type *t = resolveType(variant.Type, false);
      if (!t)
        continue;
      maxPayloadSize = std::max(
          maxPayloadSize, (uint64_t)DL.getTypeAllocSize(t).getFixedValue());
    }
    body.push_back(llvm::Type::getInt8Ty(m_Context)); // Tag
    if (maxPayloadSize > 0) {
      body.push_back(llvm::ArrayType::get(llvm::Type::getInt8Ty(m_Context),
                                          maxPayloadSize));
    }
    st->setBody(body, sh->IsPacked);
  }
}

void toka::CodeGen::genImpl(const toka::ImplDecl *decl, bool declOnly) {
  if (decl->TraitName == "encap")
    return;
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
  if (!decl->TraitName.empty()) {
    const TraitDecl *trait = nullptr;
    if (m_Traits.count(decl->TraitName)) {
      trait = m_Traits[decl->TraitName];
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

    // Generate VTable
    if (trait && !declOnly) {
      std::vector<llvm::Constant *> vtableMethods;
      llvm::Type *voidPtrTy =
          llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
      for (const auto &method : trait->Methods) {
        std::string implFuncName =
            decl->TraitName + "_" + decl->TypeName + "_" + method->Name;
        llvm::Function *f = m_Module->getFunction(implFuncName);
        if (f) {
          vtableMethods.push_back(llvm::ConstantExpr::getBitCast(f, voidPtrTy));
        } else {
          vtableMethods.push_back(llvm::Constant::getNullValue(voidPtrTy));
        }
      }

      if (!vtableMethods.empty()) {
        llvm::ArrayType *arrTy =
            llvm::ArrayType::get(voidPtrTy, vtableMethods.size());
        llvm::Constant *init = llvm::ConstantArray::get(arrTy, vtableMethods);
        std::string vtableName =
            "_VTable_" + decl->TypeName + "_" + decl->TraitName;
        new llvm::GlobalVariable(*m_Module, arrTy, true,
                                 llvm::GlobalValue::ExternalLinkage, init,
                                 vtableName);
      }
    }
  }

  m_CurrentSelfType = "";
}

llvm::Value *toka::CodeGen::genMethodCall(const toka::MethodCallExpr *expr) {
  llvm::Value *objVal = genExpr(expr->Object.get());
  if (!objVal)
    return nullptr;

  // --- Dynamic Dispatch (dyn @Trait) ---
  std::string dynamicTypeName = "";
  if (auto *ve = dynamic_cast<const VariableExpr *>(expr->Object.get())) {
    if (m_ValueTypeNames.count(ve->Name)) {
      std::string t = m_ValueTypeNames[ve->Name];
      if (t.size() >= 4 && t.substr(0, 3) == "dyn") {
        dynamicTypeName = t;
      }
    }
  }

  if (!dynamicTypeName.empty()) {
    std::string traitName = "";
    if (dynamicTypeName.find("dyn @") == 0)
      traitName = dynamicTypeName.substr(5);
    else if (dynamicTypeName.find("dyn@") == 0)
      traitName = dynamicTypeName.substr(4);

    if (!traitName.empty() && m_Traits.count(traitName)) {
      const TraitDecl *trait = m_Traits[traitName];
      int methodIdx = -1;
      const FunctionDecl *methodDecl = nullptr;

      for (size_t i = 0; i < trait->Methods.size(); ++i) {
        if (trait->Methods[i]->Name == expr->Method) {
          methodIdx = i;
          methodDecl = trait->Methods[i].get();
          break;
        }
      }

      if (methodIdx != -1) {
        // 1. Extract Data and VTable
        llvm::Value *dataPtr =
            m_Builder.CreateExtractValue(objVal, 0, "dyn_data");
        llvm::Value *vtablePtr =
            m_Builder.CreateExtractValue(objVal, 1, "dyn_vtable");

        // 2. Load Function Pointer from VTable
        llvm::Type *voidPtrTy =
            llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
        llvm::Type *vtableArrayTy =
            llvm::PointerType::getUnqual(voidPtrTy); // i8**

        llvm::Value *vtableArray =
            m_Builder.CreateBitCast(vtablePtr, vtableArrayTy);
        llvm::Value *funcPtrAddr =
            m_Builder.CreateConstGEP1_32(voidPtrTy, vtableArray, methodIdx);
        llvm::Value *voidFuncPtr = m_Builder.CreateLoad(voidPtrTy, funcPtrAddr);

        // 3. Prepare Arguments
        std::vector<llvm::Value *> args;
        std::vector<llvm::Type *> argTypes;

        // Self (dataPtr)
        args.push_back(dataPtr); // i8* passed to opaque ptr
        argTypes.push_back(llvm::PointerType::getUnqual(m_Context));

        for (auto &arg : expr->Args) {
          llvm::Value *av = genExpr(arg.get());
          args.push_back(av);
          argTypes.push_back(av->getType());
        }

        // 4. Determine Return Type
        llvm::Type *retTy = resolveType(methodDecl->ReturnType, false);
        llvm::FunctionType *ft =
            llvm::FunctionType::get(retTy, argTypes, false);

        // 5. Call
        return m_Builder.CreateCall(ft, voidFuncPtr, args);
      }
    }
  }
  // --- End Dynamic Dispatch ---

  llvm::Type *ty = objVal->getType();
  llvm::Type *structTy = nullptr;

  if (ty->isStructTy()) {
    structTy = ty;
  } else {
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(objVal)) {
      if (ai->getAllocatedType()->isStructTy())
        structTy = ai->getAllocatedType();
    } else if (auto *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(objVal)) {
      if (gep->getResultElementType()->isStructTy())
        structTy = gep->getResultElementType();
    }

    if (!structTy) {
      if (auto *ve = dynamic_cast<const VariableExpr *>(expr->Object.get())) {
        if (m_ValueElementTypes.count(ve->Name)) {
          structTy = m_ValueElementTypes[ve->Name];
        }
      }
    }
  }

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

  // Check Traits
  if (!callee) {
    for (auto const &[traitName, traitDecl] : m_Traits) {
      std::string traitFunc = traitName + "_" + typeName + "_" + expr->Method;
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

  // Retrieve FunctionDecl to check for Mutability (Pass-By-Reference)
  const FunctionDecl *fd = nullptr;
  if (m_Functions.count(funcName)) {
    fd = m_Functions[funcName];
  }

  std::vector<llvm::Value *> args;

  // 1. Handle Self (Argument 0)
  // Check if self is mutable (requires pointer)
  bool selfIsMutable = false;
  if (fd && !fd->Args.empty()) {
    // Arg 0 is self
    if (fd->Args[0].IsMutable)
      selfIsMutable = true;
  }
  // Fallback: Check LLVM Arg Type
  if (!fd && callee->arg_size() > 0 &&
      callee->getArg(0)->getType()->isPointerTy()) {
    selfIsMutable = true;
  }

  llvm::Value *finalObjVal = objVal;
  bool targetExpectsPtr =
      (callee->arg_size() > 0 && callee->getArg(0)->getType()->isPointerTy());

  if (selfIsMutable || targetExpectsPtr) {
    // Must pass address
    llvm::Value *addr = genAddr(expr->Object.get());
    if (addr) {
      finalObjVal = addr;
    } else {
      // Fallback for R-Values: Create temporary alloca
      // Only if objVal is not already a pointer
      if (!objVal->getType()->isPointerTy()) {
        llvm::AllocaInst *tmp = m_Builder.CreateAlloca(objVal->getType());
        m_Builder.CreateStore(objVal, tmp);
        finalObjVal = tmp;
      }
    }
  }

  // Type Check Self
  if (callee->arg_size() > 0) {
    llvm::Type *targetTy = callee->getArg(0)->getType();
    if (finalObjVal->getType() != targetTy) {
      if (finalObjVal->getType()->isPointerTy() && !targetTy->isPointerTy()) {
        // Implicit Dereference (Pass Reference as Value - Rare for self but
        // possible)
        finalObjVal = m_Builder.CreateLoad(targetTy, finalObjVal);
      }
    }
  }
  args.push_back(finalObjVal);

  // 2. Handle Arguments
  for (size_t i = 0; i < expr->Args.size(); ++i) {
    bool isMutable = false;
    // Arg i maps to fd->Args[i+1]
    if (fd && i + 1 < fd->Args.size()) {
      isMutable = fd->Args[i + 1].IsMutable;
    }

    llvm::Value *argVal = nullptr;
    if (isMutable) {
      argVal = genAddr(expr->Args[i].get());
      if (!argVal) {
        // R-Value fallback
        llvm::Value *rval = genExpr(expr->Args[i].get());
        if (!rval)
          return nullptr;

        // Don't double-alloc if already pointer?
        // If expects mutable (Pointer) and we have Pointer R-value allow it?
        // Usually IsMutable expects L-Value.
        // For safety, store R-value in temp.
        llvm::AllocaInst *tmp = m_Builder.CreateAlloca(rval->getType());
        m_Builder.CreateStore(rval, tmp);
        argVal = tmp;
      }
    } else {
      argVal = genExpr(expr->Args[i].get());
    }

    if (!argVal)
      return nullptr;

    // Auto-cast for primitives
    // ... (Existing cast logic could be added here if needed) ...

    args.push_back(argVal);
  }

  return m_Builder.CreateCall(callee, args);
}

void CodeGen::fillSymbolMetadata(TokaSymbol &sym, const std::string &typeStr,
                                 bool hasPointer, bool isUnique, bool isShared,
                                 bool isReference, bool isMutable,
                                 bool isNullable, llvm::Type *allocaElemTy) {
  sym.indirectionLevel = 0;
  std::string ts = typeStr;

  // 1. Peel recursive indirection prefixes
  while (!ts.empty() && (ts[0] == '*' || ts[0] == '^' || ts[0] == '~')) {
    sym.indirectionLevel++;
    ts = ts.substr(1);
  }

  // 2. Determine Addressing Mode
  if (isReference) {
    sym.mode = AddressingMode::Reference;
    sym.indirectionLevel = 1;
  } else if (hasPointer || isUnique || isShared || sym.indirectionLevel > 0) {
    sym.mode = AddressingMode::Pointer;
    if (sym.indirectionLevel == 0)
      sym.indirectionLevel = 1;
  } else {
    sym.mode = AddressingMode::Direct;
  }

  // 3. Extract Elemental Soul Type (the 'Meat')
  sym.soulType = resolveType(ts, false);
  if (!sym.soulType)
    sym.soulType = allocaElemTy;

  // 4. Morphology (Ownership/Cleanup)
  if (isUnique)
    sym.morphology = Morphology::Unique;
  else if (isShared)
    sym.morphology = Morphology::Shared;
  else if (hasPointer)
    sym.morphology = Morphology::Raw;
  else
    sym.morphology = Morphology::None;

  // 5. Semantic flags
  sym.isMutable = isMutable;
  sym.isNullable = isNullable;
  // Note: isRebindable is usually set separately based on '#' token presence
  // but it's often linked to morphology in declarations.
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
    // std::cerr << "DEBUG: resolveType: Found Alias: " << baseType << " -> "
    // << m_TypeAliases[baseType] << "\n";
    return resolveType(m_TypeAliases[baseType], hasPointer);
  }

  // Handle 'shape' keyword (e.g. shape(u8, u8))
  if (baseType.size() > 5 && baseType.substr(0, 5) == "shape") {
    return resolveType(baseType.substr(5), hasPointer);
  }

  // Handle Dynamic Traits (dyn @Trait)
  if (baseType.size() >= 4 && baseType.substr(0, 3) == "dyn") {
    // Fat Pointer: { void* data, void* vtable }
    llvm::Type *voidPtr =
        llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(m_Context));
    return llvm::StructType::get(m_Context, {voidPtr, voidPtr});
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
  if (baseType.size() > 1 && (baseType[0] == '*' || baseType[0] == '^' ||
                              baseType[0] == '~' || baseType[0] == '&')) {
    size_t offset = 1;
    if (offset < baseType.size()) {
      char next = baseType[offset];
      if (next == '#' || next == '?' || next == '!') {
        offset++;
      }
    }
    llvm::Type *elemTy = resolveType(baseType.substr(offset), false);
    if (!elemTy)
      return nullptr;
    return llvm::PointerType::getUnqual(elemTy);
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
  else if (m_Shapes.count(baseType))
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

} // namespace toka