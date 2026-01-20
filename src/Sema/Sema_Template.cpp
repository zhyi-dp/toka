
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
  std::string Output;
  Output.reserve(Input.size());

  size_t i = 0;
  while (i < Input.size()) {
    char c = Input[i];
    if (isalnum(c) || c == '_') {
      size_t start = i;
      while (i < Input.size() && (isalnum(Input[i]) || Input[i] == '_')) {
        i++;
      }
      std::string word = Input.substr(start, i - start);
      if (Map.count(word)) {
        Output += Map.at(word);
      } else {
        Output += word;
      }
    } else {
      Output += c;
      i++;
    }
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
      // Member can be generic? No, member name is ident.
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
    }

    // Reset ResolvedType
    E->ResolvedType = nullptr;
  }
};

void Sema::instantiateGenericImpl(ImplDecl *Template,
                                  const std::string &ConcreteTypeName) {
  // 1. Extract Concrete Generic Args from ConcreteTypeName
  // e.g. "Box<i32>" -> Generic Args: ["i32"]
  // Helper to parse the top-level generics

  std::vector<std::string> ConcreteArgs;
  auto TypeObj = Type::fromString(ConcreteTypeName);
  if (auto ShapeTy = std::dynamic_pointer_cast<ShapeType>(TypeObj)) {
    for (auto &Arg : ShapeTy->GenericArgs) {
      ConcreteArgs.push_back(Arg->toString());
    }
  } else if (auto ArrTy = std::dynamic_pointer_cast<ArrayType>(TypeObj)) {
    // Array generic? Not implicit impl generally, but Impl Array<T, N>
    // possible? For now assume ShapeType.
  }

  if (ConcreteArgs.size() != Template->GenericParams.size()) {
    // Mismatch or non-generic concrete type?
    // If implicit instantiation of "Box" without args? Error.
    return;
  }

  // 2. Build Substitution Map
  std::map<std::string, std::string> Replacements;
  for (size_t i = 0; i < Template->GenericParams.size(); ++i) {
    Replacements[Template->GenericParams[i].Name] = ConcreteArgs[i];
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
