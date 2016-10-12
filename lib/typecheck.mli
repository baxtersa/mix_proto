open Ast
open Environment

module type TYPECHECK =
sig
  val typecheck : gamma -> exp -> typ
end

module type SYM =
sig
  type state

  val initial_state : state

  val guard_of : state -> Symbolic_ast.sym_exp
  val memory_of : state -> Symbolic_ast.sym_memory

  val sym_eval : Symbolic_environment.sigma -> state -> exp -> state * Symbolic_ast.sym_exp
end

module type MAKE =
  functor (Sym:SYM) -> TYPECHECK

module Make : MAKE
