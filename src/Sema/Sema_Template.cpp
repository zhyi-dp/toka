
// Copyright (c) 2025 YiZhonghua<zhyi@dpai.com>. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
#include "toka/AST.h"
#include "toka/DiagnosticEngine.h"
#include "toka/Sema.h"
#include "toka/Type.h"
#include <iostream>
#include <sstream>

namespace toka {

// --- Helper: String-based Type Substitution ---
// Replaces generic params (e.g. "T") with concrete types (e.g. "i32") in a type
// string.
static std::string
substituteTypeString(const std::string &Input,
                     const std::map<std::string, std::string> &Map) {
  std::string Output = Input;
  for (auto const &[K, V] : Map) {
    size_t pos = 0;
    while ((pos = Output.find(K, pos)) != std::string::npos) {
      // Check boundaries: must not be preceded or followed by alphanumeric
      // chars EXCEPT '_' which IS allowed as a boundary for mangled names.
      bool startOk =
          (pos == 0) || !std::isalnum((unsigned char)Output[pos - 1]);
      bool endOk = (pos + K.size() == Output.size()) ||
                   !std::isalnum((unsigned char)Output[pos + K.size()]);

      if (startOk && endOk) {
        Output.replace(pos, K.size(), V);
        pos += V.size();
      } else {
        pos += K.size();
      }
    }
  }
  if (Input != Output) {
    llvm::errs() << "DEBUG: sub [" << Input << "] -> [" << Output << "]\n";
  }
  return Output;
}

// --- Helper: Generic Instantiator Visitor ---
// Traverses the AST and applies substitution to all Type Strings.
// Since we don't have a central AST Visitor, we implement specific traversals
// here.
class GenericInstantiator {
  const std::map<std::string, std::string> &Replacements;

public:
  GenericInstantiator(const std::map<std::string, std::string> &map)
      : Replacements(map) {}

  std::string sub(const std::string &s) {
    if (s.empty())
      return "";
    return substituteTypeString(s, Replacements);
  }

  void visitFunction(FunctionDecl *Fn) {
    Fn->ReturnType = sub(Fn->ReturnType);
    for (auto &Arg : Fn->Args) {
      Arg.Type = sub(Arg.Type);
      // Reset ResolvedType to allow Sema to re-resolve it
      Arg.ResolvedType = nullptr;
    }
    if (Fn->Body) {
      visitStmt(Fn->Body.get());
    }
    // GenericParams of the function itself?
    // If impl<T> fn foo<U>(), T is substituted, U remains.
    // We only substitute Impl params.
  }

  void visitStmt(Stmt *S) {
    if (!S)
      return;

    if (auto *Block = dynamic_cast<BlockStmt *>(S)) {
      for (auto &Sub : Block->Statements) {
        visitStmt(Sub.get());
      }
    } else if (auto *Ret = dynamic_cast<ReturnStmt *>(S)) {
      visitExpr(Ret->ReturnValue.get());
    } else if (auto *ExprS = dynamic_cast<ExprStmt *>(S)) {
      visitExpr(ExprS->Expression.get());
    } else if (auto *Var = dynamic_cast<VariableDecl *>(S)) {
      if (!Var->TypeName.empty()) {
        Var->TypeName = sub(Var->TypeName);
      }
      Var->ResolvedType = nullptr;
      visitExpr(Var->Init.get());
    } else if (auto *Del = dynamic_cast<DeleteStmt *>(S)) {
      visitExpr(Del->Expression.get());
    } else if (auto *Uns = dynamic_cast<UnsafeStmt *>(S)) {
      if (Uns->Statement)
        visitStmt(Uns->Statement.get());
    }
  }

  void visitExpr(Expr *E) {
    if (!E)
      return;

    if (auto *If = dynamic_cast<IfExpr *>(E)) {
      visitExpr(If->Condition.get());
      visitStmt(If->Then.get());
      visitStmt(If->Else.get());
    } else if (auto *While = dynamic_cast<WhileExpr *>(E)) {
      visitExpr(While->Condition.get());
      visitStmt(While->Body.get());
      visitStmt(While->ElseBody.get());
    } else if (auto *For = dynamic_cast<ForExpr *>(E)) {
      visitExpr(For->Collection.get());
      visitStmt(For->Body.get());
      visitStmt(For->ElseBody.get());
    } else if (auto *Loop = dynamic_cast<LoopExpr *>(E)) {
      visitStmt(Loop->Body.get());
    } else if (auto *Bin = dynamic_cast<BinaryExpr *>(E)) {
      visitExpr(Bin->LHS.get());
      visitExpr(Bin->RHS.get());
    } else if (auto *Un = dynamic_cast<UnaryExpr *>(E)) {
      visitExpr(Un->RHS.get());
    } else if (auto *Post = dynamic_cast<PostfixExpr *>(E)) {
      visitExpr(Post->LHS.get());
    } else if (auto *Cast = dynamic_cast<CastExpr *>(E)) {
      Cast->TargetType = sub(Cast->TargetType);
      visitExpr(Cast->Expression.get());
    } else if (auto *Addr = dynamic_cast<AddressOfExpr *>(E)) {
      visitExpr(Addr->Expression.get());
    } else if (auto *Mem = dynamic_cast<MemberExpr *>(E)) {
      visitExpr(Mem->Object.get());
    } else if (auto *VarE = dynamic_cast<VariableExpr *>(E)) {
      VarE->Name = sub(VarE->Name);
    } else if (auto *Call = dynamic_cast<CallExpr *>(E)) {
      // Call->Callee could rely on T? e.g. T::new() -> i32::new()
      // T::new is parsed as "T::new" string in Callee.
      Call->Callee = sub(Call->Callee);
      for (auto &Arg : Call->Args)
        visitExpr(Arg.get());
      for (auto &G : Call->GenericArgs)
        G = sub(G);
    } else if (auto *New = dynamic_cast<NewExpr *>(E)) {
      New->Type = sub(New->Type);
      visitExpr(New->Initializer.get());
    } else if (auto *Alloc = dynamic_cast<AllocExpr *>(E)) {
      Alloc->TypeName = sub(Alloc->TypeName);
      visitExpr(Alloc->Initializer.get());
      visitExpr(Alloc->ArraySize.get());
    } else if (auto *Arr = dynamic_cast<ArrayExpr *>(E)) {
      for (auto &El : Arr->Elements)
        visitExpr(El.get());
    } else if (auto *Idx = dynamic_cast<ArrayIndexExpr *>(E)) {
      visitExpr(Idx->Array.get());
      for (auto &I : Idx->Indices)
        visitExpr(I.get());
    } else if (auto *Rec = dynamic_cast<AnonymousRecordExpr *>(E)) {
      for (auto &F : Rec->Fields)
        visitExpr(F.second.get());
      if (!Rec->AssignedTypeName.empty())
        Rec->AssignedTypeName = sub(Rec->AssignedTypeName);
    } else if (auto *MetCall = dynamic_cast<MethodCallExpr *>(E)) {
      visitExpr(MetCall->Object.get());
      for (auto &Arg : MetCall->Args)
        visitExpr(Arg.get());
    } else if (auto *Init = dynamic_cast<InitStructExpr *>(E)) {
      Init->ShapeName = sub(Init->ShapeName);
      for (auto &F : Init->Members)
        visitExpr(F.second.get());
    } else if (auto *Tup = dynamic_cast<TupleExpr *>(E)) {
      for (auto &El : Tup->Elements)
        visitExpr(El.get());
    } else if (auto *Rep = dynamic_cast<RepeatedArrayExpr *>(E)) {
      visitExpr(Rep->Value.get());
      visitExpr(Rep->Count.get());
    } else if (auto *Match = dynamic_cast<MatchExpr *>(E)) {
      visitExpr(Match->Target.get());
      for (auto &Arm : Match->Arms) {
        visitStmt(Arm->Body.get());
        if (Arm->Guard)
          visitExpr(Arm->Guard.get());
      }
    } else if (auto *Pass = dynamic_cast<PassExpr *>(E)) {
      visitExpr(Pass->Value.get());
    } else if (auto *UnsE = dynamic_cast<UnsafeExpr *>(E)) {
      visitExpr(UnsE->Expression.get());
    } else if (auto *Brk = dynamic_cast<BreakExpr *>(E)) {
      if (Brk->Value)
        visitExpr(Brk->Value.get());
    } else if (auto *Cont = dynamic_cast<ContinueExpr *>(E)) {
      // Nothing to substitute in labels usually
    }

    // Reset ResolvedType
    E->ResolvedType = nullptr;
  }
};

void Sema::instantiateGenericImpl(
    ImplDecl *Template, const std::string &ConcreteTypeName,
    const std::vector<std::shared_ptr<toka::Type>> &GenericArgs) {
  // 1. Verify generic args count

  if (GenericArgs.size() != Template->GenericParams.size()) {
    // Mismatch or non-generic concrete type?
    // If implicit instantiation of "Box" without args? Error.
    return;
  }

  // 2. Build Substitution Map
  std::map<std::string, std::string> Replacements;
  for (size_t i = 0; i < Template->GenericParams.size(); ++i) {
    Replacements[Template->GenericParams[i].Name] = GenericArgs[i]->toString();
  }

  // 3. Clone and Substitute
  GenericInstantiator Instantiator(Replacements);

  std::vector<std::unique_ptr<FunctionDecl>> NewMethods;
  for (auto &Method : Template->Methods) {
    // Clone Method
    // We can use the clone() method from ASTNode if FunctionDecl supports it
    // correctly? FunctionDecl::clone() exists.
    auto ClonedAST = Method->clone();
    // clone() returns unique_ptr<ASTNode>. cast to FunctionDecl.
    // The AST clone implementation returns base unique_ptr.
    // We need to static_cast via release/reset or dynamic_cast.

    // Wait, FunctionDecl::clone() returns unique_ptr<ASTNode>.
    std::unique_ptr<FunctionDecl> ClonedFn(
        static_cast<FunctionDecl *>(ClonedAST.release()));

    // Apply Substitution
    Instantiator.visitFunction(ClonedFn.get());

    NewMethods.push_back(std::move(ClonedFn));
  }

  // 4. Create New ImplDecl
  // TypeName MUST be the mangled name (e.g. "Box_M_i32") for CodeGen lookup
  std::string MangledName = resolveType(ConcreteTypeName);

  auto NewImpl = std::make_unique<ImplDecl>(
      MangledName, std::move(NewMethods), Template->TraitName
      // No GenericParams for the instance!
  );

  // Copy encapsulation entries if any (generics might affect them?)
  NewImpl->EncapEntries = Template->EncapEntries;
  NewImpl->Loc = Template->Loc; // rough loc

  // 5. Register and Check
  ImplDecl *RawPtr = NewImpl.get();

  // We must add to M.Impls to own it.
  CurrentModule->Impls.push_back(std::move(NewImpl));

  // Now Register it!
  registerImpl(RawPtr);

  // Now Check it!
  // This will check method bodies
  checkImpl(RawPtr);

  // Done.
}

} // namespace toka
