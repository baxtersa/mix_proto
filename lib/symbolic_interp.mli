open Symbolic_ast
open Symbolic_environment

module type SYM_INTERP = sig
  type state

  val initial_state : state

  val guard_of : state -> sym_exp
  val memory_of : state -> sym_memory

  val sym_eval : sigma -> state -> Ast.exp -> state * sym_exp
end

module type TYP = sig
  val typecheck : Environment.gamma -> Ast.exp -> Ast.typ
end

module type MAKE =
  functor (Typecheck:TYP) -> SYM_INTERP

module Make : MAKE
