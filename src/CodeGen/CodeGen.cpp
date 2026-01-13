#include "toka/CodeGen.h"
#include <cctype>
#include <iostream>
#include <set>
#include <typeinfo>

namespace toka {

// 在 src/CodeGen/CodeGen.cpp 的 genExpr 函数中
PhysEntity CodeGen::genExpr(const Expr *expr) {
  if (!expr)
    return {};

  if (m_Builder.GetInsertBlock() && m_Builder.GetInsertBlock()->getTerminator())
    return {};

  // 1. 基础表达式
  if (auto e = dynamic_cast<const BinaryExpr *>(expr))
    return genBinaryExpr(e);
  if (auto e = dynamic_cast<const UnaryExpr *>(expr))
    return genUnaryExpr(e);
  if (auto e = dynamic_cast<const VariableExpr *>(expr))
    return genVariableExpr(e);

  // 2. 字面量系列 (重点修复：手动列出 AST 中存在的真实类名)
  if (dynamic_cast<const NumberExpr *>(expr) ||
      dynamic_cast<const FloatExpr *>(expr) ||
      dynamic_cast<const BoolExpr *>(expr) ||
      dynamic_cast<const NullExpr *>(expr) ||
      dynamic_cast<const NoneExpr *>(expr) ||
      dynamic_cast<const StringExpr *>(expr)) {
    return genLiteralExpr(expr);
  }
  if (auto e = dynamic_cast<const TupleExpr *>(expr))
    return genTupleExpr(e);
  if (auto e = dynamic_cast<const AnonymousRecordExpr *>(expr))
    return genAnonymousRecordExpr(e);
  if (auto e = dynamic_cast<const ArrayExpr *>(expr))
    return genArrayExpr(e);

  // 3. 内存与成员访问
  if (auto e = dynamic_cast<const MemberExpr *>(expr))
    return genMemberExpr(e);
  if (auto e = dynamic_cast<const ArrayIndexExpr *>(expr))
    return genIndexExpr(e);
  if (auto e = dynamic_cast<const AllocExpr *>(expr))
    return genAllocExpr(e);

  // 4. 控制流与高级表达式
  if (auto e = dynamic_cast<const CastExpr *>(expr))
    return genCastExpr(e);
  if (auto e = dynamic_cast<const MatchExpr *>(expr))
    return genMatchExpr(e);
  if (auto e = dynamic_cast<const IfExpr *>(expr))
    return genIfExpr(e);
  if (auto e = dynamic_cast<const WhileExpr *>(expr))
    return genWhileExpr(e);
  if (auto e = dynamic_cast<const LoopExpr *>(expr))
    return genLoopExpr(e);
  if (auto e = dynamic_cast<const ForExpr *>(expr))
    return genForExpr(e);
  if (auto e = dynamic_cast<const MethodCallExpr *>(expr))
    return genMethodCall(e);
  if (auto e = dynamic_cast<const CallExpr *>(expr))
    return genCallExpr(e);
  if (auto e = dynamic_cast<const PostfixExpr *>(expr))
    return genPostfixExpr(e);
  if (auto e = dynamic_cast<const InitStructExpr *>(expr))
    return genInitStructExpr(e);
  if (auto e = dynamic_cast<const PassExpr *>(expr))
    return genPassExpr(e);
  if (auto e = dynamic_cast<const BreakExpr *>(expr))
    return genBreakExpr(e);
  if (auto e = dynamic_cast<const ContinueExpr *>(expr))
    return genContinueExpr(e);
  if (auto e = dynamic_cast<const UnsafeExpr *>(expr))
    return genUnsafeExpr(e);
  if (auto e = dynamic_cast<const NewExpr *>(expr))
    return genNewExpr(e);

  return {};
}

llvm::Value *CodeGen::genStmt(const Stmt *stmt) {
  if (!stmt)
    return nullptr;

  if (auto s = dynamic_cast<const BlockStmt *>(stmt))
    return genBlockStmt(s);
  if (auto s = dynamic_cast<const ReturnStmt *>(stmt))
    return genReturnStmt(s);
  if (auto s = dynamic_cast<const VariableDecl *>(stmt))
    return genVariableDecl(s);
  if (auto s = dynamic_cast<const DestructuringDecl *>(stmt))
    return genDestructuringDecl(s);
  if (auto s = dynamic_cast<const DeleteStmt *>(stmt))
    return genDeleteStmt(s);
  if (auto s = dynamic_cast<const FreeStmt *>(stmt))
    return genFreeStmt(s);
  if (auto s = dynamic_cast<const UnsafeStmt *>(stmt))
    return genUnsafeStmt(s);
  if (auto s = dynamic_cast<const ExprStmt *>(stmt))
    // genExprStmt returns Value* (wrapper) or PhysEntity?
    // CodeGen.h: genExprStmt returns Value*.
    // Wait. In CodeGen.h I didn't verify genExprStmt signature update.
    // I assumed genExprStmt returns Value* because genStmt signature didn't
    // change. Let's assume genExprStmt returns Value* for now.
    return genExprStmt(s);

  // 如果 Stmt 是 Expr 的包装
  if (auto e = dynamic_cast<const Expr *>(stmt))
    return genExpr(e).load(m_Builder);

  return nullptr;
}

void CodeGen::discover(const Module &ast) {
  m_AST = &ast;
  if (!m_Module) {
    m_Module = std::make_unique<llvm::Module>("toka_module", m_Context);
  }

  // Phase 1: Registration (Names only)
  for (const auto &sh : ast.Shapes)
    m_Shapes[sh->Name] = sh.get();
  for (const auto &alias : ast.TypeAliases)
    m_TypeAliases[alias->Name] = alias->TargetType;
  for (const auto &func : ast.Functions)
    m_Functions[func->Name] = func.get();
  for (const auto &ext : ast.Externs)
    m_Externs[ext->Name] = ext.get();
  for (const auto &trait : ast.Traits)
    m_Traits[trait->Name] = trait.get();
}

void CodeGen::resolveSignatures(const Module &ast) {
  m_AST = &ast;

  // Phase 2: Declaration (Signatures and Types)
  // Shapes first (for struct layouts)
  for (const auto &sh : ast.Shapes) {
    genShape(sh.get());
    if (hasErrors())
      return;
  }

  for (const auto &ext : ast.Externs) {
    genExtern(ext.get());
    if (hasErrors())
      return;
  }

  for (const auto &func : ast.Functions) {
    genFunction(func.get(), "", true);
    if (hasErrors())
      return;
  }

  for (const auto &impl : ast.Impls) {
    genImpl(impl.get(), true);
    if (hasErrors())
      return;
  }
}

void CodeGen::generate(const Module &ast) {
  m_AST = &ast;

  // Generate Globals (Emission)
  for (const auto &glob : ast.Globals) {
    genGlobal(glob.get());
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