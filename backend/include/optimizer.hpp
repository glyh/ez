#pragma once

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassInstrumentation.h>
#include <llvm/IR/PassManager.h>

#include <llvm/Passes/StandardInstrumentations.h>

using namespace llvm;

struct Optimizer {
  Optimizer(std::shared_ptr<LLVMContext>, std::shared_ptr<Module>);

  void run_on_function(Function *);

  std::unique_ptr<FunctionPassManager> FPM;
  std::unique_ptr<LoopAnalysisManager> LAM;
  std::unique_ptr<FunctionAnalysisManager> FAM;
  std::unique_ptr<CGSCCAnalysisManager> CGAM;
  std::unique_ptr<ModuleAnalysisManager> MAM;
  std::unique_ptr<PassInstrumentationCallbacks> PIC;
  std::unique_ptr<StandardInstrumentations> SI;
};
