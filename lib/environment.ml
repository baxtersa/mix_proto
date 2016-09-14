open Ast

type gamma = (id * typ) list

let lookup_typ (x:id) (ctx:gamma) : typ =
  List.assoc x ctx

let extend (x:id) (t:typ) (ctx:gamma ref) : unit =
  ctx := (x, t) :: !ctx
