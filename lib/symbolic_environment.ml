open Symbolic_ast

type sigma = (sym_id * sym_exp) list

let lookup_exp (x:sym_id) (ctx:sigma) : sym_exp =
  try
    List.assoc x ctx
  with Not_found ->
    failwith ("Identifier '" ^ x ^ "' not found in symbolic environment.")
