#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "ez_ir.pb.h"

#include "scoped_environment.hpp"

using namespace llvm;

struct CodegenVisitor {
  CodegenVisitor(std::shared_ptr<LLVMContext>, std::shared_ptr<Module>);

  Value *generate_unit();
  Type *generate_type(const ez_proto::EzType &);
  FunctionType *generate_function_type(const ez_proto::Definition &);
  AllocaInst *create_entry_block_alloca(Function *, const std::string &,
                                        Type *);

  Value *codegen(const ez_proto::Value &);
  Value *codegen(const ez_proto::Expr &);
  Value *codegen(const ez_proto::Expr_Binary &);
  void codegen(const ez_proto::Statement &);
  Function *codegen(const ez_proto::Definition &);
  void sanitize_function(Function &);

  std::shared_ptr<LLVMContext> context;
  std::shared_ptr<Module> module;
  std::unique_ptr<IRBuilder<>> builder;
  std::unique_ptr<scoped_environment> env;
};
