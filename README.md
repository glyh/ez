# Ez

An ez language implemented with OCaml, Cpp and LLVM.

### Requirements
- LLVM 18.1
- opam 2.2
- libprotoc 28.3

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
./compile ./examples/fib.ez
```

### References 
#### Code Generation
- [A Complete Guide to LLVM for Programming Language Creators](https://mukulrathi.com/create-your-own-programming-language/llvm-ir-cpp-api-tutorial/)
- [My First Language Frontend with LLVM Tutorial — LLVM 20.0.0git documentation](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)
- [MarkLeone/WeekendCompiler: An example LLVM-based compiler for a subset of C.](https://github.com/MarkLeone/WeekendCompiler)
