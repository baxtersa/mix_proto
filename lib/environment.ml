open Ast

type gamma = (id * typ) list

let lookup_typ (x:id) (ctx:gamma) : typ =
  List.assoc x ctx
