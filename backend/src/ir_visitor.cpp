// NOTE:
// 1. we assign names to all tmps so they don't colide with potential unnamed
// blocks

#include <cstdio>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>
#include <unordered_set>

#include "extra_exceptions.hpp"
#include "ez_ir.pb.h"
#include "ezdebug.hpp"
#include "ir_visitor.hpp"

using namespace llvm;

CodegenVisitor::CodegenVisitor(std::shared_ptr<LLVMContext> ctx,
                               std::shared_ptr<Module> mod) {
  context = ctx;
  builder = std::unique_ptr<IRBuilder<>>(new IRBuilder<>(*context));
  module = mod;
  env = std::make_unique<scoped_environment>();
}

Type *CodegenVisitor::generate_type(const ez_proto::EzType &ty) {
  switch (ty.kind_case()) {
  case ez_proto::EzType::kNonPtr:
    switch (ty.non_ptr()) {
    case ez_proto::EzType_NonPtr_UNIT:
    case ez_proto::EzType_NonPtr_BOOL:
      return Type::getInt1Ty(*context);

    case ez_proto::EzType_NonPtr_I64:
      return Type::getInt64Ty(*context);

    case ez_proto::EzType_NonPtr_F64:
      return Type::getDoubleTy(*context);

    case ez_proto::EzType_NonPtr_STR:
      return Type::getInt8Ty(*context)->getPointerTo();
    }
  case ez_proto::EzType::kPtr:
    return PointerType::get(*context, 0);
  default:
    throw std::domain_error("type has invalid type");
  }
}

Type *CodegenVisitor::generate_extern_type(const ez_proto::EzType &ty) {
  switch (ty.kind_case()) {
  case ez_proto::EzType::kNonPtr:
    switch (ty.non_ptr()) {
    case ez_proto::EzType_NonPtr_UNIT:
      return Type::getVoidTy(*context);

    case ez_proto::EzType_NonPtr_BOOL:
      return Type::getInt1Ty(*context);

    case ez_proto::EzType_NonPtr_I64:
      return Type::getInt64Ty(*context);

    case ez_proto::EzType_NonPtr_F64:
      return Type::getDoubleTy(*context);

    case ez_proto::EzType_NonPtr_STR:
      return Type::getInt8Ty(*context)->getPointerTo();
    }
  case ez_proto::EzType::kPtr:
    return PointerType::get(*context, 0);
  default:
    throw std::domain_error("type has invalid type");
  }
}

FunctionType *
CodegenVisitor::generate_function_type(const ez_proto::Function &func) {
  Type *return_type = generate_type(func.return_type());
  std::vector<Type *> param_types;
  for (auto param : func.params()) {
    param_types.push_back(generate_type(param.param_type()));
  }
  return FunctionType::get(return_type, param_types, false);
}

FunctionType *
CodegenVisitor::generate_function_type(const ez_proto::Extern &extern_func) {
  Type *return_type = generate_type(extern_func.return_type());
  std::vector<Type *> param_types;
  for (auto param : extern_func.params()) {
    param_types.push_back(generate_extern_type(param.param_type()));
  }
  return FunctionType::get(return_type, param_types, false);
}

AllocaInst *CodegenVisitor::create_entry_block_alloca(
    Function *TheFunction, const std::string &var_name, Type *ty) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(ty, nullptr, var_name);
}

Value *CodegenVisitor::codegen(const ez_proto::Value &value) {
  switch (value.kind_case()) {
  case ez_proto::Value::kUnitVal:
    return ConstantInt::get(Type::getInt1Ty(*context), 0);

  case ez_proto::Value::kI64Val:
    return ConstantInt::getSigned(Type::getInt64Ty(*context), value.i64_val());

  case ez_proto::Value::kF64Val:
    return ConstantFP::get(Type::getDoubleTy(*context),
                           APFloat(value.f64_val()));

  case ez_proto::Value::kBoolVal:
    return ConstantInt::get(Type::getInt1Ty(*context), value.bool_val());

  case ez_proto::Value::kStrVal:
    return builder->CreateGlobalStringPtr(value.str_val());

  default:
    throw std::domain_error("value has invalid type");
  }
}

Value *CodegenVisitor::codegen(const ez_proto::Expr &expr) {
  switch (expr.kind_case()) {
  case ez_proto::Expr::kValue:
    return codegen(expr.value());

  case ez_proto::Expr::kVariable: {
    const std::string variable_name = expr.variable();
    auto alloc_inst = env->find(variable_name);
    if (alloc_inst == nullptr) {
      throw UndefinedVariable(variable_name);
    } else {
      return builder->CreateLoad(alloc_inst->getAllocatedType(), alloc_inst,
                                 variable_name);
    }
  }

  case ez_proto::Expr::kBinary:
    return codegen(expr.binary());

  case ez_proto::Expr::kCall: {
    auto call = expr.call();
    Function *function_to_call = module->getFunction(call.callee());
    std::vector<Value *> args;
    for (auto arg : call.args()) {
      args.push_back(codegen(arg));
    }
    return builder->CreateCall(function_to_call, args, "calltmp");
  }

  default:
    throw std::domain_error("expr has invalid type");
  }
}

Value *CodegenVisitor::codegen(const ez_proto::Expr_Binary &expr) {
  Value *lhs = codegen(expr.lhs());
  Value *rhs = codegen(expr.rhs());
  ez_proto::EzType left_type = expr.lhs().expr_type();
  ez_proto::Expr_Binary_BinOp bop = expr.op();
  switch (bop) {
  case ez_proto::Expr_Binary_BinOp_ADD:
    if (left_type.non_ptr() == ez_proto::EzType_NonPtr_I64) {
      return builder->CreateAdd(lhs, rhs, "i64_add_tmp");
    } else {
      return builder->CreateFAdd(lhs, rhs, "f64_add_tmp");
    }

  case ez_proto::Expr_Binary_BinOp_SUB:
    if (left_type.non_ptr() == ez_proto::EzType_NonPtr_I64) {
      return builder->CreateSub(lhs, rhs, "i64_sub_tmp");
    } else {
      return builder->CreateFSub(lhs, rhs, "f64_sub_tmp");
    }

  case ez_proto::Expr_Binary_BinOp_MUL:
    if (left_type.non_ptr() == ez_proto::EzType_NonPtr_I64) {
      return builder->CreateMul(lhs, rhs, "i64_mul_tmp");
    } else {
      return builder->CreateFMul(lhs, rhs, "f64_mul_tmp");
    }

  case ez_proto::Expr_Binary_BinOp_DIV:
    if (left_type.non_ptr() == ez_proto::EzType_NonPtr_I64) {
      return builder->CreateSDiv(lhs, rhs, "i64_div_tmp");
    } else {
      return builder->CreateFDiv(lhs, rhs, "f64_div_tmp");
    }

  case ez_proto::Expr_Binary_BinOp_MOD:
    if (left_type.non_ptr() == ez_proto::EzType_NonPtr_I64) {
      return builder->CreateSRem(lhs, rhs, "i64_mod_tmp");
    } else {
      return builder->CreateFRem(lhs, rhs, "f64_mod_tmp");
    }
  case ez_proto::Expr_Binary_BinOp_EQ:
    if (left_type.kind_case() == ez_proto::EzType::kNonPtr) {
      switch (left_type.non_ptr()) {
      case ez_proto::EzType_NonPtr_I64:
        return builder->CreateICmpEQ(lhs, rhs, "i64_cmp_eq_tmp");
      case ez_proto::EzType_NonPtr_F64:
        return builder->CreateFCmpOEQ(lhs, rhs, "f64_cmp_oeq_tmp");
      default:
        throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                         ez_type_to_string(left_type)));
      }
    } else {
      throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                       ez_type_to_string(left_type)));
    }
  case ez_proto::Expr_Binary_BinOp_NOTEQ:
    if (left_type.kind_case() == ez_proto::EzType::kNonPtr) {
      switch (left_type.non_ptr()) {
      case ez_proto::EzType_NonPtr_I64:
        return builder->CreateICmpNE(lhs, rhs, "i64_cmp_ne_tmp");
      case ez_proto::EzType_NonPtr_F64:
        return builder->CreateFCmpONE(lhs, rhs, "f64_cmp_one_tmp");
      default:
        throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                         ez_type_to_string(left_type)));
      }
    } else {
      throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                       ez_type_to_string(left_type)));
    }
  case ez_proto::Expr_Binary_BinOp_LESSTHAN:
    if (left_type.kind_case() == ez_proto::EzType::kNonPtr) {
      switch (left_type.non_ptr()) {
      case ez_proto::EzType_NonPtr_I64:
        return builder->CreateICmpSLT(lhs, rhs, "i64_cmp_slt_tmp");
      case ez_proto::EzType_NonPtr_F64:
        return builder->CreateFCmpOLT(lhs, rhs, "f64_cmp_olt_tmp");
      default:
        throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                         ez_type_to_string(left_type)));
      }
    } else {
      throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                       ez_type_to_string(left_type)));
    }
  case ez_proto::Expr_Binary_BinOp_LESSEQ:
    if (left_type.kind_case() == ez_proto::EzType::kNonPtr) {
      switch (left_type.non_ptr()) {
      case ez_proto::EzType_NonPtr_I64:
        return builder->CreateICmpSLE(lhs, rhs, "f64_cmp_sle_tmp");
      case ez_proto::EzType_NonPtr_F64:
        return builder->CreateFCmpOLE(lhs, rhs, "f64_cmp_ole_tmp");
      default:
        throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                         ez_type_to_string(left_type)));
      }
    } else {
      throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                       ez_type_to_string(left_type)));
    }
  case ez_proto::Expr_Binary_BinOp_GREATERTHAN:
    if (left_type.kind_case() == ez_proto::EzType::kNonPtr) {
      switch (left_type.non_ptr()) {
      case ez_proto::EzType_NonPtr_I64:
        return builder->CreateICmpSGT(lhs, rhs, "f64_cmp_sgt_tmp");
      case ez_proto::EzType_NonPtr_F64:
        return builder->CreateFCmpOGT(lhs, rhs, "f64_cmp_ogt_tmp");
      default:
        throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                         ez_type_to_string(left_type)));
      }
    } else {
      throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                       ez_type_to_string(left_type)));
    }
  case ez_proto::Expr_Binary_BinOp_GREATEREQ:
    if (left_type.kind_case() == ez_proto::EzType::kNonPtr) {
      switch (left_type.non_ptr()) {
      case ez_proto::EzType_NonPtr_I64:
        return builder->CreateICmpSGE(lhs, rhs, "i64_cmp_sge_tmp");
      case ez_proto::EzType_NonPtr_F64:
        return builder->CreateFCmpOGE(lhs, rhs, "f64_cmp_oge_tmp");
      default:
        throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                         ez_type_to_string(left_type)));
      }
    } else {
      throw NotImplemented(std::format("`{}` of {}", ez_binop_to_string(bop),
                                       ez_type_to_string(left_type)));
    }
  case ez_proto::Expr_Binary_BinOp_LAND:
    return builder->CreateAnd(lhs, rhs, "land_tmp");
  case ez_proto::Expr_Binary_BinOp_LOR:
    return builder->CreateOr(lhs, rhs, "lor_tmp");
  default:
    throw std::domain_error("invalid bin op");
  }
}

void CodegenVisitor::codegen(const ez_proto::Statement &stmt) {

  switch (stmt.kind_case()) {
  case ez_proto::Statement::kReturn: {
    auto ret_stmt = stmt.return_();
    builder->CreateRet(codegen(ret_stmt));
    break;
  }

  case ez_proto::Statement::kBlock: {
    env->enter_scope();
    auto block_stmt = stmt.block();
    for (auto stmt : block_stmt.statements()) {
      codegen(stmt);

      // if we already exit the block, stop code generation
      if (builder->GetInsertBlock()->getTerminator()) {
        break;
      }
    }
    env->exit_scope();
    break;
  }

  case ez_proto::Statement::kIf: {
    // 1. Prepare blocks and branches
    auto if_stmt = stmt.if_();

    Value *cond = codegen(if_stmt.condition());

    Function *TheFunction = builder->GetInsertBlock()->getParent();

    BasicBlock *ThenBB = BasicBlock::Create(*context, "if_then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*context, "if_else");
    BasicBlock *MergeBB = BasicBlock::Create(*context, "if_cont");
    builder->CreateCondBr(cond, ThenBB, ElseBB);
    // 2. Generating code for then branch
    builder->SetInsertPoint(ThenBB);
    codegen(if_stmt.then_branch());
    // Only branch back to merge bb if we're not returned yet.
    if (!builder->GetInsertBlock()->getTerminator()) {
      builder->CreateBr(MergeBB);
    }

    // Codegen of 'Then' can change the current block, update ThenBB for the
    // PHI.
    ThenBB = builder->GetInsertBlock();
    // 3. Generating code for else branch
    TheFunction->insert(TheFunction->end(), ElseBB);
    builder->SetInsertPoint(ElseBB);
    codegen(if_stmt.else_branch());
    // Only branch back to merge bb if we're not returned yet.
    if (!builder->GetInsertBlock()->getTerminator()) {
      builder->CreateBr(MergeBB);
    }
    // codegen of 'Else' can change the current block, update ElseBB for the
    // PHI.
    ElseBB = builder->GetInsertBlock();
    TheFunction->insert(TheFunction->end(), MergeBB);
    builder->SetInsertPoint(MergeBB);
    break;
  }

  case ez_proto::Statement::kWhile: {
    auto while_stmt = stmt.while_();
    Function *TheFunction = builder->GetInsertBlock()->getParent();
    BasicBlock *TestBB =
        BasicBlock::Create(*context, "while_test", TheFunction);
    BasicBlock *BodyBB = BasicBlock::Create(*context, "while_body");
    BasicBlock *MergeBB = BasicBlock::Create(*context, "while_cont");
    // 1. Generating code for test condition
    builder->SetInsertPoint(TestBB);
    Value *condition = codegen(while_stmt.condition());
    builder->CreateCondBr(condition, BodyBB, MergeBB);
    // 2. Generating code for loop body
    TheFunction->insert(TheFunction->end(), BodyBB);
    builder->SetInsertPoint(BodyBB);
    codegen(while_stmt.body());
    // Only branch back to merge bb if we're not returned yet.
    if (!builder->GetInsertBlock()->getTerminator()) {
      builder->CreateBr(TestBB);
    }
    // Codegen of 'Body' can change the current block, update BodyBB for the
    // PHI.
    BodyBB = builder->GetInsertBlock();
    // 3. Generate MergeBB
    TheFunction->insert(TheFunction->end(), MergeBB);
    builder->SetInsertPoint(MergeBB);
    break;
  }
  case ez_proto::Statement::kExpr: {
    auto expr_stmt = stmt.expr();
    codegen(expr_stmt);
    break;
  }

  case ez_proto::Statement::kDeclaration: {
    auto declare_stmt = stmt.declaration();
    Function *TheFunction = builder->GetInsertBlock()->getParent();
    Value *init_val = codegen(declare_stmt.rhs());
    Type *var_type = generate_type(declare_stmt.type());
    const std::string &name = declare_stmt.name();
    AllocaInst *alloc = create_entry_block_alloca(TheFunction, name, var_type);
    builder->CreateStore(init_val, alloc);
    env->add(name, alloc);
    break;
  }

  case ez_proto::Statement::kAssign: {
    auto assign_stmt = stmt.assign();
    const std::string &var_name = assign_stmt.name();
    Value *rhs = codegen(assign_stmt.rhs());
    auto lhs = env->find(var_name);
    if (lhs == nullptr) {
      throw UndefinedVariable(var_name);
    } else {
      builder->CreateStore(rhs, lhs);
    }
    break;
  }

  default:
    throw std::domain_error("statement has invalid type");
  }
}

Function *CodegenVisitor::codegen(const ez_proto::Function &func) {
  FunctionType *cur_fn_type = generate_function_type(func);
  const std::string &name = func.name();
  Function *cur_fn = Function::Create(cur_fn_type, Function::ExternalLinkage,
                                      name, module.get());
  auto params = func.params();
  int idx = 0;
  for (auto &arg : cur_fn->args()) {
    arg.setName(params.at(idx).name());
    idx += 1;
  }
  BasicBlock *BB =
      BasicBlock::Create(*context, std::format("{}_entry", name), cur_fn);
  builder->SetInsertPoint(BB);
  env->enter_scope();

  for (auto &arg : cur_fn->args()) {
    std::string name(arg.getName());

    AllocaInst *alloc_inst =
        create_entry_block_alloca(cur_fn, name, arg.getType());
    // Store the initial value into the alloca.
    builder->CreateStore(&arg, alloc_inst);
    env->add(name, alloc_inst);
  }
  codegen(func.body());

  // Add a return instruction if the user neglected to do so.
  if (!builder->GetInsertBlock()->getTerminator()) {
    Type *return_type = generate_type(func.return_type());
    builder->CreateRet(Constant::getNullValue(return_type));
  }

  verifyFunction(*cur_fn, &errs());

  env->exit_scope();
  return cur_fn;
}

Function *CodegenVisitor::codegen(const ez_proto::Extern &extern_fn) {
  FunctionType *cur_fn_type = generate_function_type(extern_fn);
  return Function::Create(cur_fn_type, Function::ExternalLinkage,
                          extern_fn.name(), module.get());
}
