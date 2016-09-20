MIX prototype
===

A prototype OCaml implementation of the hybrid symbolic execution/type checking system [MIX](http://www.cs.colorado.edu/~bec/papers/pldi10-mix.pdf).

Building MIX
---
MIX has the following build dependencies
- mparser
- mparser.re
- ppx_deriving.std
- oasis

MIX uses the ```oasis``` build system, so after cloning the repo with dependencies installed, you can simply run
```make build```.