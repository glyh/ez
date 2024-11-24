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

  Type *generate_type(const ez_proto::EzType &);
  Type *generate_extern_type(const ez_proto::EzType &);
  Value *generate_unit();

  FunctionType *generate_function_type(const ez_proto::Function &);
  FunctionType *generate_function_type(const ez_proto::Extern &);
  AllocaInst *create_entry_block_alloca(Function *, const std::string &,
                                        Type *);

  Value *codegen(const ez_proto::Value &);
  Value *codegen(const ez_proto::Expr &);
  Value *codegen(const ez_proto::Expr_Binary &);
  void codegen(const ez_proto::Statement &);

  Function *codegen(const ez_proto::Function &);
  Function *codegen(const ez_proto::Extern &);

  void sanitize_function(Function &);

  std::shared_ptr<LLVMContext> context;
  std::shared_ptr<Module> module;
  std::unique_ptr<IRBuilder<>> builder;
  std::unique_ptr<scoped_environment> env;
};
