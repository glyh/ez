# Ez

An ez language implemented with OCaml, Cpp and LLVM.

### Requirements
- LLVM 17
- opam
- protoc

### Building

- Clone the repo and init the switch
```sh
git clone https://github.com/glyh/ez
cd ez
opam switch create . --deps-only --with-test -y
```
- For developing, you may want to have LSP and other stuffs available
```sh
opam install --switch=. -y ocamlformat ocaml-lsp-server utop ocaml-protoc
```
- Update the environment, for example if you're on bash: 
```bash
eval $(opam env)
```
- Build both frontend and backend with
```sh
make
```

### Running

```sh
# Generate protobuf IR with frontend
./_build/default/bin/main.exe ./examples/fib.ez /tmp/fib.irpb
# Generate LLVM IR with backend
./_build_cmake/ezback /tmp/fib.irpb > /tmp/fib.ll
```
