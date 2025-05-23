# WiFi SAE Packet Generator

## Dependencies

OCaml **version >= 5**

Required opam packages:

* `opam install menhirLib cmdliner ocamlgraph bitstring yojson lwt batteries ppx_bitstring alcotest lwt_ppx`


Other dependencies:

* macOs: `brew install coreutils` (TODO: `timeout` vs `gtimeout` based on OS)
* `cvc5`
  * You must make cvc5 discoverable from `$PATH` (update `$PATH` environment variable so that `which cvc5` returns a valid path to cvc5)
  * If you want to run a portfolio with a second version of `cvc5`, optionally set `$PATH_TO_SECOND_CVC5` to the path to the second `cvc5` executable (including "`cvc5`", not just the folder containing the binary). If you don't want to run a portfolio, no further action is required.

How to run the tool:

* To build, run `make`
* To (build and) execute, run `sbf` (SBF stands for SyGuS-based fuzzing)
* To run tests, run `make test`

Command-line args:

* Use `--help` for command-line arg documentation that's guaranteed to be up to date (e.g., `sbf --help`)
* Use `--file <filename>` to specify the input file (**required**) (e.g., `sbf --file ./my_files/input`)
* Use `--debug` for debug output
* Use `--only-parse` to run the front end (type checking, syntactic checks, various AST transformations, etc.) without invoking SyGuS
* Use `--no-warnings` to disable warnings
