# WiFi SAE Packet Generator

Required opam packages:

* `opam install menhir alcotest ppx_bitstring yojson ocamlgraph lwt lwt_ppx`

Other dependencies:

* macOs: `brew install coreutils` (TODO: `timeout` vs `gtimeout` based on OS)
* `cvc5`
  * You must add `cvc5` to `$PATH`
  * If you want to run a portfolio with a second version of `cvc5`, optionally set `$PATH_TO_SECOND_CVC5` to the path to the second `cvc5` executable (including "`cvc5`", not just the folder containing the binary).

How to run the tool:

* To build, run `dune build`
* To (build and) execute, run `dune exec sbf` (SBF stands for SyGuS-based fuzzing)
* To run tests, run `dune test`

Command-line args:
* All command-line args are passed as such: `dune exec sbf -- <args here>`
* Use `--debug` for debug output (`dune exec sbf -- --debug`)
