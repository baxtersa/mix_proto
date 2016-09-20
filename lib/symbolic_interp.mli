open Symbolic_ast
open Symbolic_environment

type state

val sym_eval : sigma ref -> state -> Ast.exp -> state * sym_exp
