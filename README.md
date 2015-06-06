# ocaml-c3
OCaml bindings for the Javascript c3 charting library.

If you want to write a client-side web application in OCaml with
`js_of_ocaml` and display some charts, this library is for you.

- The [C3 library](http://c3js.org/): to see the kinds of things
  that are possible with the Javascript C3 library
- The [OCaml demo](http://djs55.github.io/ocaml-c3/index.html):
  to see the example from this repo.

# Getting started

* install all the dependencies, for example by:

```
opam pin add c3 .
```

* build the library and example site:

```
make
```

* serve the example site:

```
make run
```

View the example on http://localhost:8080/
