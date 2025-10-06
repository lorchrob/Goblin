# Goblin input generator
Hello! 👋

To get started with Goblin, build using the instructions below and read through [the Goblin tutorial](https://github.com/lorchrob/Goblin/blob/master/doc/tutorial.md).

### Goblin dependencies

* Note: Goblin is supported on **Ubuntu** and **macOs**. Other versions of Linux may work but have not been tested. Windows is not supported (but you can try using WSL).

* OCaml **version ≥ 5.1.1**

  Install Opam and create a switch for version 5.1.1 in the top-level directory:
  ```
  bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
  opam init
  opam switch create 5.1.1
  eval $(opam env)
  ```

* cvc5 **version ≥ 1.3.0** (https://github.com/cvc5/cvc5/releases/tag/cvc5-1.3.0). You must make the cvc5 binary discoverable from `$PATH` (update `$PATH` environment variable so that `which cvc5` returns a valid path to cvc5)

* To download the required OPAM packages, run
  ```
  opam install menhirLib cmdliner ocamlgraph bitstring yojson lwt batteries ppx_bitstring alcotest lwt_ppx menhir
  ```

### Building Goblin

Run `make` from the top-level directory 

### Running Goblin

* To execute, run `goblin` (after building)
* To run tests, run `make test`
* Use `goblin --help` for command-line arg documentation 
* Example invocation: `goblin --file ./test/test_cases/test2`

### User documentation

See the files within [the `doc` directory](https://github.com/lorchrob/Goblin/blob/master/doc). In particular, we have a [Goblin tutorial](https://github.com/lorchrob/Goblin/blob/master/doc/tutorial.md) and [an additional file describing Goblin syntax](https://github.com/lorchrob/Goblin/blob/master/doc/syntax.md).

### Citing Goblin

Details to come :)
