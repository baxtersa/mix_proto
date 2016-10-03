open Symbolic_ast
open Symbolic_environment

type state

val initial_state : state

val guard_of : state -> sym_exp
val memory_of : state -> sym_memory

val sym_eval : sigma -> state -> Ast.exp -> state * sym_exp
