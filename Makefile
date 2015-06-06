.PHONY: all clean distclean setup build doc install test
all: build

J ?= 2

setup.data: setup.bin
	./setup.bin -configure

setup.ml: _oasis
	oasis setup

distclean: setup.data setup.bin
	./setup.bin -distclean $(OFLAGS)
	$(RM) setup.bin

setup: setup.data

build: setup.data  setup.bin
	./setup.bin -build -j $(J) $(OFLAGS)

clean:
	ocamlbuild -clean
	rm -f setup.data setup.bin main.js

doc: setup.data setup.bin
	./setup.bin -doc -j $(J) $(OFLAGS)

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	$(RM) setup.cmx setup.cmi setup.o setup.cmo
