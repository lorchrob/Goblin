# Goblin Ubuntu/macOs build instructions

## Dependencies

* OCaml **version >= 5.1.1**

  Install Opam and create a switch for version 5.1.1 in the `core` (this) directory:
  ```
  cd core
  bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
  opam init
  opam switch create 5.1.1
  eval $(opam env)
  ```

* cvc5 version **version >= 1.3.0** (https://github.com/cvc5/cvc5/releases/tag/cvc5-1.3.0). You must make the cvc5 binary discoverable from `$PATH` (update `$PATH` environment variable so that `which cvc5` returns a valid path to cvc5)

* Required opam packages:
  `opam install menhirLib cmdliner ocamlgraph bitstring yojson lwt batteries ppx_bitstring alcotest lwt_ppx menhir`

## Building and running

* To build, run `make`
* To (build and) execute, run `./sbf` 
* To run tests, run `make test`

Command-line args:

* Use `--help` for command-line arg documentation that's guaranteed to be up to date (e.g., `sbf --help`)
* Use `--file <filename>` to specify the input file (**required**) (e.g., `sbf --file ./my_files/input`)
* Use `--debug` for debug output
* Use `--only-parse` to run the front end (type checking, syntactic checks, various AST transformations, etc.) without invoking SyGuS
* Use `--no-warnings` to disable warnings
