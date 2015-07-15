#!/bin/sh -e

cd _build/example
js_of_ocaml --pretty --noinline --debug-info +weak.js main.byte
cd ../../example
ln -sf ../_build/example/main.js
