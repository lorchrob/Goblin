.PHONY: all test clean

all:
	@dune build

test:
	@dune test -j 1

clean:
	@dune clean