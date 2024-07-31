# WiFi SAE Packet Generator

Required opam packages:

* Alcotest (`opam install alcotest`)

How to run the tool:

* To build, run `dune build`
* To (build and) execute, run `dune exec sbf` (SBF stands for SyGuS-based fuzzing)
* To run tests, run `dune test`

Command-line args:
* All command-line args are passed as such: `dune exec sbf -- <args here>`
* Use `--debug` for debug output (`dune exec sbf -- --debug`)