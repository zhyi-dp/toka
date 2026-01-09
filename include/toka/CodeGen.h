#pragma once

#include "toka/AST.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace toka {
class ASTNode;
class Module;
class Stmt;
class Expr;
class FunctionDecl;
class ExternDecl;
class VariableDecl;
class ShapeDecl;
class ImplDecl;
class MethodCallExpr;

struct TokaSymbol {
  llvm::Value *allocaPtr; // Identity (alloca address)
  llvm::Type *llvmType;   // Real logic type (e.g. Point struct)
  bool isImplicitPtr;     // In-place capture / Captured Shape
  bool isExplicitPtr;     // Unique/Shared/Raw pointer handled by user
  bool isMutable;
};

class CodeGen {
public:
  CodeGen(llvm::LLVMContext &context, const std::string &moduleName)
      : m_Context(context), m_Builder(context) {
    m_Module = std::make_unique<llvm::Module>(moduleName, context);
  }

  void discover(const Module &ast);
  void resolveSignatures(const Module &ast);
  void generate(const Module &ast);
  bool hasErrors() const { return m_ErrorCount > 0; }
  void print(llvm::raw_ostream &os);

private:
  int m_ErrorCount = 0;
  void error(const ASTNode *node, const std::string &message);
  llvm::LLVMContext &m_Context;
  llvm::IRBuilder<> m_Builder;
  std::unique_ptr<llvm::Module> m_Module;
  const Module *m_AST = nullptr;

  std::map<std::string, const FunctionDecl *> m_Functions;
  std::map<std::string, const ExternDecl *> m_Externs;
  std::map<std::string, TokaSymbol> m_Symbols;
  std::string m_CurrentSelfType;
  std::map<std::string, std::string> m_ValueTypeNames;
  std::map<std::string, llvm::Type *> m_ValueElementTypes;
  std::map<std::string, llvm::StructType *> m_StructTypes;
  std::map<std::string, std::vector<std::string>> m_StructFieldNames;
  std::map<std::string, std::string> m_TypeAliases;
  std::map<std::string, const ShapeDecl *> m_Shapes;
  std::map<std::string, const TraitDecl *> m_Traits;
  std::map<llvm::Type *, std::string> m_TypeToName;

  // Legacy Mapping (Sema/CodeGen interoperability)
  std::map<std::string, llvm::Value *> m_NamedValues;
  std::map<std::string, llvm::Type *> m_ValueTypes;
  std::map<std::string, bool> m_ValueIsReference;
  std::map<std::string, bool> m_ValueIsMutable;
  std::map<std::string, bool> m_ValueIsUnique;
  std::map<std::string, bool> m_ValueIsShared;
  std::map<std::string, bool> m_ValueIsNullable;

  struct CFInfo {
    std::string Label;
    llvm::BasicBlock *BreakTarget;
    llvm::BasicBlock *ContinueTarget;
    llvm::Value *ResultAddr; // Alloca for storing results
    size_t ScopeDepth;
  };
  std::vector<CFInfo> m_CFStack;

  struct VariableScopeInfo {
    std::string Name;
    llvm::Value *Alloca;
    bool IsUniquePointer; // ^Type
    bool IsShared;        // ~Type
  };
  std::vector<std::vector<VariableScopeInfo>> m_ScopeStack;

  llvm::Type *resolveType(const std::string &baseType, bool hasPointer);

  void cleanupScopes(size_t targetDepth);
  llvm::Value *genExpr(const Expr *expr);
  llvm::Value *genAddr(const Expr *expr);
  llvm::Value *getVarAddr(const std::string &name);

  // Address Layering Protocol
  llvm::Value *getEntityAddr(const std::string &name);
  llvm::Value *getIdentityAddr(const std::string &name);
  llvm::Value *emitEntityAddr(const Expr *expr); // "Soul" - actual data address
  llvm::Value *
  emitHandleAddr(const Expr *expr); // "Handle" - identity/sleeve (alloca)

  llvm::Value *genStmt(const Stmt *stmt);
  llvm::Function *genFunction(const FunctionDecl *func,
                              const std::string &overrideName = "",
                              bool declOnly = false);
  void genGlobal(const Stmt *stmt);
  void genExtern(const ExternDecl *ext);
  void genShape(const ShapeDecl *sh);
  void genImpl(const ImplDecl *impl, bool declOnly = false);
  llvm::Value *genMatchExpr(const MatchExpr *expr);
  llvm::Value *genBinaryExpr(const BinaryExpr *expr);
  llvm::Value *genAllocExpr(const AllocExpr *expr);
  llvm::Value *genMemberExpr(const MemberExpr *expr);
  llvm::Value *genIndexExpr(const ArrayIndexExpr *expr);
  llvm::Value *genVariableExpr(const VariableExpr *expr);
  llvm::Value *genLiteralExpr(const Expr *expr);
  llvm::Value *genCastExpr(const CastExpr *expr);
  llvm::Value *genUnaryExpr(const UnaryExpr *expr);
  llvm::Value *genIfExpr(const IfExpr *expr);
  llvm::Value *genWhileExpr(const WhileExpr *expr);
  llvm::Value *genLoopExpr(const LoopExpr *expr);
  llvm::Value *genForExpr(const ForExpr *expr);
  void genPatternBinding(const MatchArm::Pattern *pat, llvm::Value *targetAddr,
                         llvm::Type *targetType);
  llvm::Value *genMethodCall(const MethodCallExpr *expr);
};

} // namespace toka
