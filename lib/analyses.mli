module type SYM =
sig
  type state

  val initial_state : state

  val guard_of : state -> Symbolic_ast.sym_exp
  val memory_of : state -> Symbolic_ast.sym_memory

  val sym_eval : Symbolic_environment.sigma -> state -> Ast.exp -> state * Symbolic_ast.sym_exp
end

module type TYP = sig
  val typecheck : Environment.gamma -> Ast.exp -> Ast.typ
end
