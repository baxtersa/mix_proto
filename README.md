MIX prototype
===

A prototype OCaml implementation of the hybrid symbolic execution/type checking system [MIX](http://www.cs.colorado.edu/~bec/papers/pldi10-mix.pdf).

Building MIX
---
The easiest method of installating `mix_proto` is using [`opam`](https://opam.ocaml.org/doc/Install.html). With `opam` installed on your system, the following commands should install build dependencies, compile the `mix_proto` executable, and install the executable in your PATH:
- `opam pin add mix_proto https://github.com/baxtersa/mix_proto.git`
  
  This adds the `mix_proto` repository to `opam`'s search path for OPAM packages. It may automatically prompt you to install the package.
- If pinning the repository does not install the package automatically, simply run

  `opam install mix_proto`
  
Running MIX
---
`mix_proto` has a runtime dependency on a `z3` executable being installed on your system. `z3` can easily be installed using `brew` on OSX or `apt-get` on Ubuntu.

With `z3` and the `mix_proto` executable installed, you can run `mix_proto` from the command line to perform its analysis on a file.

Command Line Options
---
For information on command line usage:
- `-h` lists command line options.
- `-z3 </path/to/z3>` (optional) uses the installation of `z3` at the path provided. By default, `mix_proto` searches for z3 in your system PATH. This can be passed the `z3` shell script provided in the root of this repository to log `mix_proto`'s communication with `z3` to a file.

One of the following two options is required:
- `-sym <path/to/code>` begins the analysis on the file provided under symbolic evaluation.
- `-typ <path/to/code>` begins the analysis on the file provided under the type checker.

MIX Syntax
---
```
expr ::= x | val
  | expr op expr
  | not expr
  | if expr then expr else expr
  | let x = expr in expr
  | fun (x : t) -> expr
  | expr expr
  | MIX(typed) { expr }
  | MIX(symbolic) { expr }
  
val ::= n | true | false

op ::= + | / | == | && | ||
```

Known Issues
---
There are a few quirks with my implementation, as well as issues with the formalism/examples given in the paper.
- Symbolic execution of division doesn't perform arithmetic, so expressions like `7 / (1 / 2)` aren't recognized as division by 0.
- I left out mutable and null-valued references from my implementation because it leads to additional complications I don't have time to implement, and they alone don't produce very useful examples.
- Functions and Application are left out of the formalism of symbolic execution in the paper -.-

  So I'm a little grumpy about this because all of their meaningful examples use both language features. I assumed I would be able to do something reasonable and come up with my own semantics without deciphering their prototype implementation for much more complex functions/application in C, and I think I did reasonably well with it, but it's certainly not perfect.
- There's some issues with a naive implementation of their mixing rules that are a little complicated, but I can go into detail if you're interested.
