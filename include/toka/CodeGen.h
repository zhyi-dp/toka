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

enum class AddressingMode {
  Direct,   // Stack allocated value (Box is Soul): i32, [5]i32
  Pointer,  // Pointer orbit (Address in Box): *i32, ^Point, ~Point
  Reference // Implicit alias: &v
};

enum class Morphology {
  None,   // Scalar/Value
  Raw,    // * (Manual)
  Unique, // ^ (Auto-Owned)
  Shared  // ~ (Ref-counted)
};

struct TokaSymbol {
  llvm::Value *allocaPtr; // Identity (The stationary "Box" address)
  llvm::Type *soulType;   // Soul Type (Explicit layout for LLVM 17 GEP/Load)
  AddressingMode mode;    // Path type
  Morphology morphology;  // Ownership/Cleanup logic
  int indirectionLevel;   // Depth (1 for *p, 2 for **p)
  bool isRebindable;      // # on identity (Swappable address)
  bool isMutable;         // # on entity (Writable data)
  bool isContinuous;      // Sequence marker (alloc [N])
  bool isNullable;        // ?/! marker
};

struct PhysEntity {
  llvm::Value *value = nullptr;
  std::string typeName;
  llvm::Type *irType = nullptr; // For Load instructions
  bool isAddress = false;

  PhysEntity(llvm::Value *v = nullptr) : value(v) {
    if (v)
      irType = v->getType();
  }
  PhysEntity(llvm::Value *v, std::string t, llvm::Type *it, bool addr)
      : value(v), typeName(t), irType(it), isAddress(addr) {
    if (!irType && value)
      irType = value->getType();
  }

  llvm::Value *load(llvm::IRBuilder<> &b) const {
    if (!value)
      return nullptr;
    if (isAddress && irType) {
      return b.CreateLoad(irType, value);
    }
    return value;
  }
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
    bool HasDrop;
    std::string DropFunc;
  };
  std::vector<std::vector<VariableScopeInfo>> m_ScopeStack;

  llvm::Type *resolveType(const std::string &baseType, bool hasPointer);
  void fillSymbolMetadata(TokaSymbol &sym, const std::string &typeStr,
                          bool hasPointer, bool isUnique, bool isShared,
                          bool isReference, bool isMutable, bool isNullable,
                          llvm::Type *allocaElemTy);

  void cleanupScopes(size_t targetDepth);
  PhysEntity genExpr(const Expr *expr);
  llvm::Constant *genConstant(const Expr *expr,
                              llvm::Type *targetType = nullptr);
  llvm::Value *genAddr(const Expr *expr);
  // llvm::Value *getVarAddr(const std::string &name);

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
  PhysEntity genMatchExpr(const MatchExpr *expr);
  PhysEntity genBinaryExpr(const BinaryExpr *expr);
  PhysEntity genAllocExpr(const AllocExpr *expr);
  PhysEntity genMemberExpr(const MemberExpr *expr);
  PhysEntity genIndexExpr(const ArrayIndexExpr *expr);
  PhysEntity genVariableExpr(const VariableExpr *expr);
  PhysEntity genLiteralExpr(const Expr *expr);
  PhysEntity genTupleExpr(const TupleExpr *expr);
  PhysEntity genArrayExpr(const ArrayExpr *expr);
  PhysEntity genCastExpr(const CastExpr *expr);
  PhysEntity genUnaryExpr(const UnaryExpr *expr);
  PhysEntity genIfExpr(const IfExpr *expr);
  PhysEntity genWhileExpr(const WhileExpr *expr);
  PhysEntity genLoopExpr(const LoopExpr *expr);
  PhysEntity genForExpr(const ForExpr *expr);
  void genPatternBinding(const MatchArm::Pattern *pat, llvm::Value *targetAddr,
                         llvm::Type *targetType);
  PhysEntity genInitStructExpr(const InitStructExpr *expr);
  PhysEntity genAnonymousRecordExpr(const AnonymousRecordExpr *expr);
  PhysEntity genMethodCall(const MethodCallExpr *expr);
  PhysEntity genCallExpr(const CallExpr *expr);
  PhysEntity genPostfixExpr(const PostfixExpr *expr);
  PhysEntity genPassExpr(const PassExpr *expr);
  PhysEntity genBreakExpr(const BreakExpr *expr);
  PhysEntity genContinueExpr(const ContinueExpr *expr);
  PhysEntity genUnsafeExpr(const UnsafeExpr *expr);
  PhysEntity genNewExpr(const NewExpr *expr);
  llvm::Value *genReturnStmt(const ReturnStmt *stmt);
  llvm::Value *genBlockStmt(const BlockStmt *stmt);
  llvm::Value *genVariableDecl(const VariableDecl *stmt);
  llvm::Value *genDestructuringDecl(const DestructuringDecl *stmt);
  llvm::Value *genDeleteStmt(const DeleteStmt *stmt);
  llvm::Value *genFreeStmt(const FreeStmt *stmt);

  // Helpers
  std::string stripMorphology(const std::string &name);
  llvm::Value *genUnsafeStmt(const UnsafeStmt *stmt);
  llvm::Value *genExprStmt(const ExprStmt *stmt);
};

} // namespace toka
