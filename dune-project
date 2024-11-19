(lang dune 3.15)

(name ez)

(using menhir 2.0)
(using ctypes 0.3)

(generate_opam_files true)

(source
 (github glyh/ez))

(authors "Yihang Liu")

(maintainers "Yihang Liu")

(license GPL-3.0-or-later)

(documentation https://github.com/glyh/ez)

(package
 (name ez)
 (synopsis "An ez language implemented with OCaml, Cpp and LLVM")
 (description "An easy language implemented with OCaml, Cpp and LLVM")
 (depends
   ocaml
   dune
   menhir
   (core (>= v0.17.1))
   (nice_parser (>= 1.0.0))
   (ctypes-foreign (>= 0.22.0))
   (alcotest :with-test)
   (pbrt (>= 3.1.1)))
   
 (tags
  (topics compiler LLVM)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
