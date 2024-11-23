.PHONY: build

default: build

build: _build/default/bin/ezfront.exe _build_back/ezback ezstd/zig-out/lib/libezstd.so
	
./lib/ez_ir.ml ./lib/ez_ir.mli: ./proto/ez_ir.proto
	ocaml-protoc --binary --ml_out ./lib/ ./proto/ez_ir.proto 

OCAML_DIRS := bin lib test
OCAML_FILES := $(foreach dir,$(OCAML_DIRS),$(wildcard $(dir)/*.ml) $(wildcard $(dir)/*.mli)  $(wildcard $(dir)/dune))


_build/default/bin/ezfront.exe: $(OCAML_FILES)
	dune build

./backend/proto_gen/ez_ir.pb.h ./backend/proto_gen/ez_ir.pb.cc: ./proto/ez_ir.proto
	protoc -I=./proto/ --cpp_out=./backend/proto_gen/ ./proto/ez_ir.proto 

BACK_PROTO_GEN := $(wildcard backend/proto_gen/*)
BACK_HEADER := $(wildcard backend/include/*.hpp)
BACK_SRC := $(wildcard backend/src/*.cpp)

_build_back/ezback: $(BACK_PROTO_GEN) $(BACK_HEADER) $(BACK_SRC) ./backend/CMakeLists.txt
	cmake -S backend -B _build_back
	cmake --build _build_back

STD_SRC := $(wildcard ezstd/src/*.zig)

ezstd/zig-out/lib/libezstd.so: $(STD_SRC) ./ezstd/build.zig ./ezstd/build.zig.zon
	cd ezstd && zig build
