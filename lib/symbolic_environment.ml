open Symbolic_ast

type sigma = (sym_id * sym_exp) list

let lookup_exp (x:sym_id) (ctx:sigma) :sym_exp =
  List.assoc x ctx

let extend (x:sym_id) (s:sym_exp) (ctx:sigma ref) : unit =
  ctx := (x, s) :: !ctx
