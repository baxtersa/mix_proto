open Ast

type gamma = (id * typ) list

let lookup_typ (x:id) (ctx:gamma) : typ =
  try
    List.assoc x ctx
  with Not_found ->
    failwith ("Identifier '" ^ x ^ "' not found in type environment.")
