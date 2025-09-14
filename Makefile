.PHONY: all test clean

all:
	@dune build
	@dune install

test:
	OCAMLRUNPARAM=b dune test -j 1 --verbose

clean:
	@dune clean