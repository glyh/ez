#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <vector>

#include "alias.hpp"

using namespace llvm;

struct scoped_environment {
  std::vector<std::unordered_map<str, AllocaInst *>> scopes_stack;
  scoped_environment();
  AllocaInst *find(const std::string &);
  void add(const std::string &, AllocaInst *);
  void enter_scope();
  void exit_scope();
};
