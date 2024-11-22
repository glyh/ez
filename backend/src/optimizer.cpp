#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/IR/PassInstrumentation.h>
#include <llvm/IR/PassManager.h>

#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <memory>

#include "optimizer.hpp"

Optimizer::Optimizer(std::shared_ptr<LLVMContext> context,
                     std::shared_ptr<Module> module)
    : FPM(std::make_unique<FunctionPassManager>()),
      LAM(std::make_unique<LoopAnalysisManager>()),
      FAM(std::make_unique<FunctionAnalysisManager>()),
      CGAM(std::make_unique<CGSCCAnalysisManager>()),
      MAM(std::make_unique<ModuleAnalysisManager>()),
      PIC(std::make_unique<PassInstrumentationCallbacks>()),
      SI(std::make_unique<StandardInstrumentations>(*context, true))

{
  SI->registerCallbacks(*PIC, MAM.get());

  // Do simple "peephole" optimizations and bit-twiddling optzns.
  FPM->addPass(InstCombinePass());
  // Reassociate expressions.
  FPM->addPass(ReassociatePass());
  // Eliminate Common SubExpressions.
  FPM->addPass(GVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  FPM->addPass(SimplifyCFGPass());

  // Register analysis passes used in these transform passes.
  PassBuilder PB;
  PB.registerModuleAnalyses(*MAM);
  PB.registerFunctionAnalyses(*FAM);
  PB.crossRegisterProxies(*LAM, *FAM, *CGAM, *MAM);
}

void Optimizer::run_on_function(Function *fn) { FPM->run(*fn, *FAM); }
