.PHONY: all test clean

all:
	@dune build

test:
	@dune test

clean:
	@dune clean