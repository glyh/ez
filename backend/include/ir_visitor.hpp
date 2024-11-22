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

#include "alias.hpp"

using namespace llvm;

struct CodegenVisitor {
  CodegenVisitor(const char *module_name);

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

  box<LLVMContext> context;
  box<IRBuilder<>> builder;
  box<Module> module;
  box<scoped_environment> env;
};
