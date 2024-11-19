.PHONY: build

default: build

build: frontend _build_cmake/ezback
	
./lib/ez_ir.ml ./lib/ez_ir.mli: ./proto/ez_ir.proto
	ocaml-protoc --binary --ml_out ./lib/ ./proto/ez_ir.proto 

OCAML_DIRS := bin lib test
OCAML_FILES := $(foreach dir,$(OCAML_DIRS),$(wildcard $(dir)/*.ml) $(wildcard $(dir)/*.mli)  $(wildcard $(dir)/dune))

frontend: $(OCAML_FILES)
	dune build

./backend/proto_gen/ez_ir.pb.h ./backend/proto_gen/ez_ir.pb.cc: ./proto/ez_ir.proto
	protoc -I=./proto/ --cpp_out=./backend/proto_gen/ ./proto/ez_ir.proto 

CPP_PROTO_GEN := $(wildcard backend/proto_gen/*)
CPP_HEADER := $(wildcard backend/include/*.hpp)
CPP_SRC := $(wildcard backend/src/*.cpp)

_build_cmake/ezback: $(CPP_PROTO_GEN) $(CPP_HEADER) $(CPP_SRC) ./backend/CMakeLists.txt
	cmake -S backend -B _build_cmake
	cmake --build _build_cmake
