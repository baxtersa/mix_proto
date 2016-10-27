Examples
===

Division by 0
---

Consider the following example in `div_by_0.mix`:
```ocaml
MIX(symbolic) {
let div =
  fun (x:int) ->
  fun (y:int) ->
    if y == 0
    then false
    else x / y in
MIX(typed) { 10 + MIX(symbolic) { div 7 0 } } }
```
You can run an analysis on this code by executing:
`mix -<sym|typ> div_by_0.mix`

I'm going to list a few things to note:
- `div` obviously doesn't typecheck, because then/else branches differ in their type, so we wrap the definition in a symbolic block
- The definition of `div` contains an `if` statement, so we expect the symbolic evaluator to fork whenever the function is applied.
- We apply `div 7 0`, which should yield the symbolic expression `false:bool`.
- Since we typecheck the block containing an addition, and symbolic evaluation yields a fresh variable of type `bool` for one of the operands, the typechecker fails as expected.
- Try modifying the application of `div` to `div 7 4`, and watch `mix`'s analysis pass now that we don't divide by 0.
- Unfortunately, modifying the application of `div` to `div 7 (div 1 2)` passes the analysis, because symbolic execution doesn't perform arithmetic. This could probably be solved with a smarter symbolic evaluator.
- Try removing the outermost `MIX(symbolic) { ... }` annotation. Beginning analysis with symbolic execution from the command line will succeed, just as it did originally. But beginning the analysis under the type checker will fail now attempting to typecheck the body of `div`.
- So it should be clear that the most important part of the analysis is the placement of `MIX(...)` annotations. Feel free to move the annotations around, add more, delete some

Unreachable Code
---

Consider the following example in `unreachable_code.mix`:
```ocaml
let val =
MIX(symbolic) {
    if true
    then
        MIX(typed) { 0 }
    else
        MIX(typed) { 10 + false }
} in
val
```
You can run an analysis on this code by executing:
`mix -<sym|typ> unreachable_code.mix`

I'm going to list a few things to note again:
- Since the if-guard is `true`, then `else` branch is clearly unreachable
- Running this under a traditional type checker will fail because of differing branch types
- Under `mix`, symbolic execution performs a feasibility on each fork of its evaluation, and recognizes the `else` condition is infeasible. So, it can escape early, without performing unecessary type analysis on unreachable code.
- This is clearly bad practice, right? Even in untyped languages, you want then/else branches to have compatible types. So is it even a good thing to allow this in a static analysis? My thoughts are that despite bad practice, code such as this exists and is in use. There could be some subtle things like putting elements of different types into the same collection, and later failing trying to iterate over it. Since symbolic execution accumulates conditions on execution state that are used to generate constraints for `z3`, testing the satisfiability of the negation of those conditions yields a model that witnesses whatever error you might be experiencing.
