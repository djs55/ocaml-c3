.PHONY: all clean example

all:
	dune build

clean:
	dune clean

example:
	dune build @example
