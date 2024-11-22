#include <stdexcept>
#include <system_error>

#include "objgen.hpp"

using namespace llvm;

Objgen::Objgen(std::shared_ptr<Module> module) : module(module) {

  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  auto TargetTriple = sys::getDefaultTargetTriple();
  module->setTargetTriple(TargetTriple);

  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    errs() << Error;
    throw std::logic_error("no target available");
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  target = Target->createTargetMachine(TargetTriple, CPU, Features, opt,
                                       Reloc::PIC_);

  module->setDataLayout(target->createDataLayout());
}

int Objgen::generate_object(const char *file_name) {
  std::error_code EC;
  raw_fd_ostream dest(file_name, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Couldn't find open file: " << EC.message();
    return 1;
  }

  legacy::PassManager pass;
  auto FileType = CodeGenFileType::ObjectFile;

  if (target->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    errs() << "TargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(*module);
  dest.flush();

  printf("Worte %s\n", file_name);
  return 0;
}
