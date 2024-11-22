#include "ez_ir.pb.h"
#include "ir_visitor.hpp"
#include <cstdio>
#include <fstream>
#include <ios>

int main(int argc, char **argv) {
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  if (argc != 2) {
    printf("Usage: %s [*.irpb]", argv[0]);
    return -1;
  }

  ez_proto::Program program;
  {
    std::fstream input(argv[1], std::ios::in | std::ios::binary);
    if (!program.ParseFromIstream(&input)) {
      printf("Failed to parse IRPB file.");
      return -1;
    }
    CodegenVisitor visitor("JIT");
    for (auto def : program.definitions()) {
      Function *cur_fn = visitor.codegen(def);
      cur_fn->print(errs());
      fprintf(stderr, "\n");
    }
  }

  printf("Hello!");
}
