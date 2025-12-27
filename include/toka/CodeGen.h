#pragma once

#include "toka/AST.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include <map>
#include <memory>

namespace toka {

class CodeGen {
public:
  CodeGen(llvm::LLVMContext &context, const std::string &moduleName)
      : m_Context(context), m_Builder(context) {
    m_Module = std::make_unique<llvm::Module>(moduleName, context);
  }

  void generate(const Module &ast);
  void print(llvm::raw_ostream &os);

private:
  llvm::LLVMContext &m_Context;
  llvm::IRBuilder<> m_Builder;
  std::unique_ptr<llvm::Module> m_Module;

  std::map<std::string, const FunctionDecl *> m_Functions;
  std::map<std::string, const ExternDecl *> m_Externs;
  std::map<std::string, llvm::Value *> m_NamedValues;
  std::map<std::string, llvm::Type *> m_ValueTypes;
  std::map<std::string, llvm::Type *> m_ValueElementTypes;
  std::map<std::string, llvm::StructType *> m_StructTypes;
  std::map<std::string, std::vector<std::string>> m_StructFieldNames;
  std::map<std::string, std::string> m_TypeAliases;

  llvm::Type *resolveType(const std::string &baseType, bool hasPointer);

  llvm::Value *genExpr(const Expr *expr);
  llvm::Value *genAddr(const Expr *expr);
  llvm::Value *genStmt(const Stmt *stmt);
  llvm::Function *genFunction(const FunctionDecl *func);
  void genGlobal(const VariableDecl *var);
  void genExtern(const ExternDecl *ext);
  void genStruct(const StructDecl *str);
};

} // namespace toka
