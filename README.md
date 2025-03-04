# WiFi SAE Packet Generator

Required opam packages:

* `opam install menhir alcotest ppx_bitstring yojson ocamlgraph lwt lwt_ppx`

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

* Use `--filename <path>` to specify the input file (**required**)
* Use `--debug` for debug output (`e.g., sbf --debug`)
* Use `--only_parse` to run the front end (type checking, syntactic checks, various AST transformations, etc.) without invoking SyGuS
