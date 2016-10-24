type sym_id = string [@@deriving show]

type sym_exp =
  | Typed of sym_exp * Ast.typ
  | SymId of sym_id
  | SymConst of Ast.const
  | SymFun of sym_id * Ast.typ * Ast.typ *  (sym_id * sym_exp) list * Ast.exp
  | SymBinop of Ast.binop * sym_exp * sym_exp
  | SymUnop of Ast.unop * sym_exp
  | MemSelect of sym_memory * sym_exp [@@deriving show]

and sym_memory =
  | Arbitrary
  | Update of sym_memory * sym_exp * sym_exp
  | Alloc of sym_memory * sym_exp * sym_exp [@@deriving show]

let count = ref 0
let fresh_sym () : sym_id =
  incr count;
  "_a" ^ (string_of_int !count)

let mem_ok (m : sym_memory) : bool =
  let rec ok (m : sym_memory) (u : sym_memory) : bool =
    match m, u with
    | Arbitrary, _ ->
      true
    | Alloc (m', Typed (SymId x, TRef t), Typed (sym_e, t')), _ ->
      ok m u
    | Update (m', Typed (sym_e, TRef t), Typed (sym_e', t')), u ->
      if ok m' u
      then true
      else failwith "Memory does not preserve type consistency."
    | _ ->
      false in
  ok m Arbitrary
