open Ast
open Symbolic_ast


type gamma = (id * typ) list

let lookup_typ (x:id) (ctx:gamma) : typ =
  try
    List.assoc x ctx
  with Not_found ->
    failwith ("Identifier '" ^ x ^ "' not found in type environment.")

let generate_sym_env (ctx:gamma) =
  List.map
    (fun (x, t) ->
       let sym_x = fresh_sym () in
       x, Typed (SymId sym_x, t))
    ctx
