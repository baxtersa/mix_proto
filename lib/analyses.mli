module type SYM =
sig
  type state

  val initial_state : state

  val guard_of : state -> Symbolic_ast.sym_exp
  val memory_of : state -> Symbolic_ast.sym_memory

  val sym_eval : Symbolic_environment.sigma -> state -> Ast.exp -> (state * Symbolic_ast.sym_exp) list
end

module type TYP = sig
  (* Takes a symbolic predicate and calls out to z3 to determine
     its satisfiability *)
  val is_feasible : Symbolic_ast.sym_exp -> bool

  val typecheck : Environment.gamma -> Ast.exp -> Ast.typ
end
