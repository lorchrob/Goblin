# WiFi SAE Packet Generator

Required opam packages:

* `opam install menhir alcotest ppx_bitstring yojson ocamlgraph lwt lwt_ppx`

Other dependencies:

* macOs: `brew install coreutils` (TODO: `timeout` vs `gtimeout` based on OS)
* `cvc5`! (TODO: Make it so you don't have to manually update file path)

How to run the tool:

* To build, run `dune build`
* To (build and) execute, run `dune exec sbf` (SBF stands for SyGuS-based fuzzing)
* To run tests, run `dune test`

Other prerequisites to running:

* There are currently a few ugly hard-coded file paths that only work on Daniyal's machine (e.g., paths to cvc5 executables).
  So those need to be manually updated to work on your machine. One way to find them is by doing a project-wide search for "pirwani".

Command-line args:
* All command-line args are passed as such: `dune exec sbf -- <args here>`
* Use `--debug` for debug output (`dune exec sbf -- --debug`)
