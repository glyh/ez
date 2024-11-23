#include <cstdio>
#include <fstream>
#include <ios>
#include <memory>
#include <stdexcept>

#include "ez_ir.pb.h"
#include "ir_visitor.hpp"
#include "objgen.hpp"
#include "optimizer.hpp"

int main(int argc, char **argv) {
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  if (argc != 3) {
    printf("Usage: %s [*.irpb] [*.o]", argv[0]);
    return -1;
  }

  std::shared_ptr<LLVMContext> ctx = std::make_shared<LLVMContext>();
  std::shared_ptr<Module> mod = std::make_shared<Module>("JIT", *ctx);

  ez_proto::Program program;
  {
    std::fstream input(argv[1], std::ios::in | std::ios::binary);
    if (!program.ParseFromIstream(&input)) {
      printf("Failed to parse IRPB file.");
      return -1;
    }

    CodegenVisitor visitor(ctx, mod);
    for (auto def : program.definitions()) {
      switch (def.kind_case()) {
      case ez_proto::Definition::kFunc: {
        auto func = def.func();
        Function *cur_fn = visitor.codegen(func);
        // Recreate an optimizer is needed
        // https://discourse.llvm.org/t/segmentation-faults-running-the-new-llvm-modulepassmanager-with-default-pipeline
        Optimizer optimizer(ctx, mod);
        optimizer.run_on_function(cur_fn);
        break;
      }
      case ez_proto::Definition::kExtern: {
        auto extern_fn = def.extern_();
        Function *cur_fn = visitor.codegen(extern_fn);
        break;
      }
      default:
        throw std::domain_error("invalid definition");
      }
    }
    Objgen objgen(mod);
    return objgen.generate_object(argv[2]);
  }
}
